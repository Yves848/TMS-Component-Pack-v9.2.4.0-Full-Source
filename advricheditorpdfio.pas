{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2016	                                            }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvRichEditorPDFIO;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvPDFLib, AdvRichEditorBase, AdvPDFIO, AdvGraphicsTypes;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed: Issue with displaying images correct width/height

resourcestring
  sAdvRichEditorPDFIORichEditorNotAssigned = 'RichEditor Not Assigned';

type
  TAdvRichEditorPDFIOOptions = class(TAdvPDFIOOptions)
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
  end;

  TAdvCustomRichEditorPDFIO = class(TAdvCustomPDFIO)
  private
    function GetOptions: TAdvRichEditorPDFIOOptions;
    procedure SetOptions(const Value: TAdvRichEditorPDFIOOptions);
    function GetRichEditor: TAdvRichEditorBase;
    procedure SetRichEditor(const Value: TAdvRichEditorBase);
  protected
    function GetVersion: String; override;
    function CreateOptions: TAdvPDFIOOptions; override;
    procedure DoPDFExport(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject); override;
    procedure DoPDFExportRichEditor(const APDFLib: TAdvPDFLib; const ARichEditor: TAdvRichEditorBase); virtual;
    property RichEditor: TAdvRichEditorBase read GetRichEditor write SetRichEditor;
    property Version: String read GetVersion;
    property Options: TAdvRichEditorPDFIOOptions read GetOptions write SetOptions;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvRichEditorPDFIO = class(TAdvCustomRichEditorPDFIO)
  published
    property RichEditor;
    property Version;
    property Options;
    property Information;
    property OnGetHeader;
    property OnGetFooter;
    property OnBeforeDrawHeader;
    property OnAfterDrawHeader;
    property OnBeforeDrawFooter;
    property OnAfterDrawFooter;
    property OnBeforeDrawContent;
    property OnAfterDrawContent;
  end;

implementation

uses
  AdvUtils, PictureContainer,
  Types, Forms, SysUtils, AdvGraphics, AdvTypes, Graphics
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  ;

{ TAdvCustomRichEditorPDFIO }

function TAdvCustomRichEditorPDFIO.GetOptions: TAdvRichEditorPDFIOOptions;
begin
  Result := TAdvRichEditorPDFIOOptions(inherited Options);
end;

function TAdvCustomRichEditorPDFIO.GetRichEditor: TAdvRichEditorBase;
begin
  Result := TAdvRichEditorBase(inherited ExportObject);
end;

function TAdvCustomRichEditorPDFIO.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

constructor TAdvCustomRichEditorPDFIO.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  if Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for I := 0 to AOwner.ComponentCount - 1 do
    begin
      if (AOwner.Components[i] is TAdvRichEditorBase) then
      begin
        RichEditor := AOwner.Components[i] as TAdvRichEditorBase;
        Break;
      end;
    end;
  end;
end;

function TAdvCustomRichEditorPDFIO.CreateOptions: TAdvPDFIOOptions;
begin
  Result := TAdvRichEditorPDFIOOptions.Create;
end;

procedure TAdvCustomRichEditorPDFIO.DoPDFExport(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject);
begin
  if not Assigned(AExportObject) or (Assigned(AExportObject) and not (AExportObject is TAdvRichEditorBase)) then
    raise Exception.Create(sAdvRichEditorPDFIORichEditorNotAssigned);

  DoPDFExportRichEditor(APDFLib, AExportObject as TAdvRichEditorBase);
end;

procedure TAdvCustomRichEditorPDFIO.DOPDFExportRichEditor(const APDFLib: TAdvPDFLib; const ARichEditor: TAdvRichEditorBase);
var
  x, y, w, h: Single;
  i: Integer;
  el: TREElement;
  picel: TPictureElement;
  r: TRectF;
  s: String;
  bmpc: TPictureContainer;
  bmpci: TPictureItem;
  pici: String;
  sHTML, img: String;
  ft: String;
  bl: TBulletElement;
begin
  bmpc := TPictureContainer.Create(nil);
  try
    NewPage(APDFLib, ARichEditor);

    w := APDFLib.MediaBox.width;
    h := APDFLib.MediaBox.height;
    x := Options.Margins.Left;
    y := Options.Margins.Top;
    r := RectF(x, y, w - Options.Margins.Right, h - Options.Margins.Bottom);

    i := 0;
    sHTML := '';

    while i < ARichEditor.Context.Content.Count do
    begin
      el := ARichEditor.Context.Content.Items[i];
      if el is TLineBreakElement then
      begin
        if el is TLineElement then
        begin
          if (el.Color <> gcNull) and (el.Color <> gcWhite) and (el.Color <> clNone) then
            sHTML := sHTML + '<hr color="'+TAdvGraphics.ColorToHtml(el.Color)+'">'
          else
            sHTML := sHTML + '<hr>'
        end
        else
          sHTML := sHTML + '<br/>';
      end
      else if el is TTextElement then
      begin
        s := '';
        if el.URL <> '' then
          s := '<a href="' + el.URL + '">' + el.Text + '</a>'
        else
          s := el.Text;

        if TFontStyle.fsBold in el.Font.Style then
          s := '<b>' + s + '</b>';
        if TFontStyle.fsItalic in el.Font.Style then
          s := '<i>' + s + '</i>';
        if TFontStyle.fsUnderline in el.Font.Style then
          s := '<u>' + s + '</u>';
        if TFontStyle.fsStrikeOut in el.Font.Style then
          s := '<s>' + s + '</s>';

        if el.Baseline = tbSubscript then
          s := '<sub>' + s + '</sub>'
        else if el.Baseline = tbSuperScript then
          s := '<sup>' + s + '</sup>';

        ft := '<font indent="'+IntToStr(el.Indent)+'"';

        case el.Alignment of
          taLeftJustify: ft := ft + ' align="left"';
          taRightJustify: ft := ft + ' align="right"';
          taCenter: ft := ft + ' align="center"';
        end;

        if el.FontName <> '' then
          ft := ft + ' face="'+el.FontName+'"';
        if el.FontSize > 0 then
          ft := ft + ' size="'+IntToStr(el.FontSize)+'"';
        if (el.TextColor <> gcNull) and (el.TextColor <> gcWhite) and (el.TextColor <> clNone) then
          ft := ft + ' color="'+TAdvGraphics.ColorToHtml(el.TextColor)+'"';
        if (el.Color <> gcNull) and (el.Color <> gcWhite) and (el.Color <> clNone) then
          ft := ft + ' bgcolor="'+TAdvGraphics.ColorToHtml(el.Color)+'"';
        ft := ft + '>' + s + '</font>';

        sHTML := sHTML + ft;
      end
      else if (el is TPictureElement) then
      begin
        if Options.ExportImages then
        begin
          picel := el as TPictureElement;
          pici := IntToStr(bmpc.Items.Count);
          img := '<img src="' + pici + '" width="' + IntToStr(picel.PictureWidth) + '" height="' + IntToStr(picel.PictureHeight) + '"';
          case el.Alignment of
            taLeftJustify: img := img + ' align="left"';
            taRightJustify: img := img + ' align="right"';
            taCenter: img := img + ' align="center"';
          end;
          img := img + '</img>';
          sHTML := sHTML + img;
          bmpci := bmpc.Items.Add;
          bmpci.Name := pici;
          bmpci.Picture.Assign(picel.Picture);
        end;
      end
      else if (el is TBulletElement) then
      begin
        bl := (el as TBulletElement);

        case bl.&Type of
          btCircle: s := #$2022;
          btSquare: s := #$25AA;
          btStar: s := #$2605;
          btArrow: s := #$2192;
          btTick: s := #$2713;
          btNumber: s := Format(bl.BulletFormat, [bl.Index + 1]);
          btChar: s := Chr(ord('a') + bl.Index);
          btCustom: s := bl.Bullet;
        end;

        s := s + ' ';

        ft := '<font bullet="" indent="'+IntToStr(el.Indent)+'"';
        case el.Alignment of
          taLeftJustify: ft := ft + ' align="left"';
          taRightJustify: ft := ft + ' align="right"';
          taCenter: ft := ft + ' align="center"';
        end;

        if el.FontSize > 0 then
          ft := ft + ' size="'+IntToStr(el.FontSize)+'"';
        if el.TextColor <> gcNull then
          ft := ft + ' color="'+TAdvGraphics.ColorToHtml(el.TextColor)+'"';
        if (el.Color <> gcNull) and (el.Color <> gcWhite) and (el.Color <> clNone) then
          ft := ft + ' bgcolor="'+TAdvGraphics.ColorToHtml(el.Color)+'"';
        ft := ft + '>' + s + '</font>';

        sHTML := sHTML + ft;
      end;
      inc(i);
    end;

    APDFLib.PictureContainer := bmpc;
    APDFLib.Graphics.DrawHTMLText(sHTML, r, True);
  finally
    bmpc.Free;
  end;
end;

procedure TAdvCustomRichEditorPDFIO.SetOptions(
  const Value: TAdvRichEditorPDFIOOptions);
begin
  Options.Assign(Value);
end;

procedure TAdvCustomRichEditorPDFIO.SetRichEditor(
  const Value: TAdvRichEditorBase);
begin
  inherited ExportObject := Value;
end;

{ TAdvRichEditorPDFIOOptions }

procedure TAdvRichEditorPDFIOOptions.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvRichEditorPDFIOOptions then
  begin
  end;
end;

constructor TAdvRichEditorPDFIOOptions.Create;
begin
  inherited;
end;

end.


