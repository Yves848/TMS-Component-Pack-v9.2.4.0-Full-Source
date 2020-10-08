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

unit AdvMemoPDFIO;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvPDFLib, AdvMemo, AdvPDFIO, AdvGraphicsTypes;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release

resourcestring
  sAdvMemoPDFIOMemoNotAssigned = 'Memo Not Assigned';

type
  TAdvMemoPDFIOOptions = class(TAdvPDFIOOptions)
  private
    FWordWrapping: Boolean;
    procedure SetWordWrapping(const Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
  published
    property WordWrapping: Boolean read FWordWrapping write SetWordWrapping default True;
  end;

  TAdvCustomMemoPDFIO = class(TAdvCustomPDFIO)
  private
    function GetOptions: TAdvMemoPDFIOOptions;
    procedure SetOptions(const Value: TAdvMemoPDFIOOptions);
    function GetMemo: TAdvCustomMemo;
    procedure SetMemo(const Value: TAdvCustomMemo);
  protected
    function GetVersion: String; override;
    function CreateOptions: TAdvPDFIOOptions; override;
    procedure DoPDFExport(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject); override;
    procedure DoPDFExportMemo(const APDFLib: TAdvPDFLib; const AMemo: TAdvCustomMemo); virtual;
    property Memo: TAdvCustomMemo read GetMemo write SetMemo;
    property Version: String read GetVersion;
    property Options: TAdvMemoPDFIOOptions read GetOptions write SetOptions;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMemoPDFIO = class(TAdvCustomMemoPDFIO)
  published
    property Memo;
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
  AdvUtils, Types, Forms, SysUtils, AdvGraphics,
  AdvTypes, Graphics
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  ;

type
  TAdvCustomMemoOpen = class(TAdvCustomMemo);

{ TAdvCustomMemoPDFIO }

function TAdvCustomMemoPDFIO.GetMemo: TAdvCustomMemo;
begin
  Result := TAdvCustomMemo(inherited ExportObject);
end;

function TAdvCustomMemoPDFIO.GetOptions: TAdvMemoPDFIOOptions;
begin
  Result := TAdvMemoPDFIOOptions(inherited Options);
end;

function TAdvCustomMemoPDFIO.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

constructor TAdvCustomMemoPDFIO.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  if Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for I := 0 to AOwner.ComponentCount - 1 do
    begin
      if (AOwner.Components[i] is TAdvMemo) then
      begin
        Memo := AOwner.Components[i] as TAdvMemo;
        Break;
      end;
    end;
  end;
end;

function TAdvCustomMemoPDFIO.CreateOptions: TAdvPDFIOOptions;
begin
  Result := TAdvMemoPDFIOOptions.Create;
end;

procedure TAdvCustomMemoPDFIO.DoPDFExport(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject);
begin
  if not Assigned(AExportObject) or (Assigned(AExportObject) and not (AExportObject is TAdvCustomMemo)) then
    raise Exception.Create(sAdvMemoPDFIOMemoNotAssigned);

  DoPDFExportMemo(APDFLib, AExportObject as TAdvCustomMemo);
end;

procedure TAdvCustomMemoPDFIO.DOPDFExportMemo(const APDFLib: TAdvPDFLib; const AMemo: TAdvCustomMemo);
var
  x, y, w, h: Single;
  i, K: Integer;
  el: TMemoLineItem;
  pt: TMemoPartItem;
  r: TRectF;
  s: String;
  sHTML: String;
  ft: String;
begin
  try
    NewPage(APDFLib, AMemo);

    w := APDFLib.MediaBox.width;
    h := APDFLib.MediaBox.height;
    x := Options.Margins.Left;
    y := Options.Margins.Top;
    r := RectF(x, y, w - Options.Margins.Right, h - Options.Margins.Bottom);

    APDFLib.Graphics.Font.Name := TAdvCustomMemoOpen(AMemo).Font.Name;
    APDFLib.Graphics.Font.Size := TAdvCustomMemoOpen(AMemo).Font.Size;
    APDFLib.Graphics.Font.Style := TAdvCustomMemoOpen(AMemo).Font.Style;

    i := 0;
    sHTML := '';
    TAdvCustomMemoOpen(AMemo).GetLines;

    while i < AMemo.MemoLines.Count do
    begin
      el := AMemo.MemoLines[i];
      for K := 0 to el.Parts.Count - 1 do
      begin
        pt := el.Parts[K];
        try
          s := pt.Text;
          s := StringReplace(s, '<', '&lt;',[rfReplaceAll]);
          s := StringReplace(s, '>', '&gt;',[rfReplaceAll]);

          if TFontStyle.fsBold in pt.FontStyle then
            s := '<b>' + s + '</b>';
          if TFontStyle.fsItalic in pt.FontStyle then
            s := '<i>' + s + '</i>';
          if TFontStyle.fsUnderline in pt.FontStyle then
            s := '<u>' + s + '</u>';
          if TFontStyle.fsStrikeOut in pt.FontStyle then
            s := '<s>' + s + '</s>';

          ft := '<font';

          if (pt.Color <> gcNull) and (pt.Color <> gcWhite) and (pt.Color <> clNone) then
            ft := ft + ' color="' + TAdvGraphics.ColorToHTML(pt.Color)+'"';
          if (pt.BkgColor <> gcNull) and (pt.BkgColor <> gcWhite) and (pt.BkgColor <> clNone) then
            ft := ft + ' bgcolor="' + TAdvGraphics.ColorToHtml(pt.BkgColor)+'"';
          ft := ft + '>' + s + '</font>';

          sHTML := sHTML + ft;
        finally
        end;
      end;

      sHTML := sHTML + '<br/>';
      inc(i);
    end;

    if Options.WordWrapping then
      APDFLib.Graphics.DrawHTMLText(sHTML, r, True)
    else
      APDFLib.Graphics.DrawHTMLText(sHTML, RectF(r.Left, r.Top, 10000, r.Bottom), True);
  finally
  end;
end;

procedure TAdvCustomMemoPDFIO.SetMemo(const Value: TAdvCustomMemo);
begin
  inherited ExportObject := Value;
end;

procedure TAdvCustomMemoPDFIO.SetOptions(
  const Value: TAdvMemoPDFIOOptions);
begin
  Options.Assign(Value);
end;

{ TAdvMemoPDFIOOptions }

procedure TAdvMemoPDFIOOptions.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvMemoPDFIOOptions then
    FWordWrapping := (Source as TAdvMemoPDFIOOptions).WordWrapping;
end;

constructor TAdvMemoPDFIOOptions.Create;
begin
  inherited;
  FWordWrapping := True;
end;

procedure TAdvMemoPDFIOOptions.SetWordWrapping(const Value: Boolean);
begin
  if FWordWrapping <> Value then
    FWordWrapping := Value;
end;

end.


