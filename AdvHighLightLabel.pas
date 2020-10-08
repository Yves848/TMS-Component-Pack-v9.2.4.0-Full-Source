{**************************************************************************}
{ TAdvHighLightLabel component                                             }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2017 - 2018                                       }
{            Email : info@tmssoftware.com                                  }
{            Website : http://www.tmssoftware.com/                         }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvHighlightLabel;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Controls, Forms, Types, Graphics, Messages
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.0.0.0 : First release
  // 1.0.0.1 : Improved : HTML engine drawing in high DPI mode with form.Scaled = false

type
  TAutoSizeType = (asVertical,asHorizontal,asBoth);

  THighlightItem = class(TCollectionItem)
  private
    FEnabled: boolean;
    FTextColor: TColor;
    FColor: TColor;
    FTag: NativeInt;
    FText: string;
    FCaseSensitive: boolean;
    procedure SetColor(const Value: TColor);
    procedure SetEnabled(const Value: boolean);
    procedure SetTextColor(const Value: TColor);
    procedure SetText(const Value: string);
    procedure SetCaseSensitive(const Value: boolean);

  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
  published
    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive default true;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Text: string read FText write SetText;
    property Color: TColor read FColor write SetColor default clHighlight;
    property TextColor: TColor read FTextColor write SetTextColor default clHighlightText;
    property Tag: NativeInt read FTag write FTag;
  end;

  THighlightItems = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: integer): THighlightItem;
    procedure SetItem(Index: integer; const Value: THighlightItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    function Add: THighlightItem;
    function Insert(Index: integer): THighlightItem;
    property Items[Index: integer]: THighlightItem read GetItem write SetItem;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvHighlightLabel = class(TGraphicControl)
  private
    FText: TStrings;
    FRenderText: string;
    FHighlighting: THighlightItems;
    FAutoSizeType: TAutoSizeType;
    FFormScaled: boolean;
    procedure SetText(const Value: TStrings);
    procedure SetHighlighting(const Value: THighlightItems);

    procedure UpdateHighlight;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetAutoSizeType(const Value: TAutoSizeType);

  protected
    procedure AdjustSize; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure HighlightChanged(Sender: TObject); virtual;
    procedure TextChanged(Sender: TObject); virtual;
    function GetVersionNr: integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Highlight(const AWord: string; Color: TColor = clHighlight; TextColor: TColor = clHighlightText);
  published
    property AutoSize;
    property AutoSizeType: TAutoSizeType read FAutoSizeType write SetAutoSizeType default asVertical;
    property Highlighting: THighlightItems read FHighlighting write SetHighlighting;
    property Text: TStrings read FText write SetText;
    property Version: string read GetVersion write SetVersion;

    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property ParentShowHint;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnMouseLeave;
    property OnMouseEnter;
  end;


implementation

uses
  ShellAPI, SysUtils, ImgList, ComCtrls, CommCtrl, StrUtils, PictureContainer;

{$I HTMLENGO.PAS}


function ColorToHtml(const Value: TColor): string;
type
  TColorRecord = record
    RedValue: Byte;    //  clRed = TColor($0000FF);   Low byte
    GreenValue: Byte;  //  clLime = TColor($00FF00);  Middle byte
    BlueValue: Byte;   //  clBlue = TColor($FF0000);  High byte
    SystemValue: Byte; //  becomes zero when calling ColorToRgb
  end;
const
  HtmlHexColor = '#RRGGBB';
  HexDigit: array[0..$F] of Char = '0123456789ABCDEF';
begin
 //  HTML Color output as: #RRGGBB

  with TColorRecord(ColorToRGb(Value)) do
  begin
    Result := HtmlHexColor;
    Result[2] := HexDigit[RedValue shr 4];
    Result[3] := HexDigit[RedValue and $F];
    Result[4] := HexDigit[GreenValue shr 4];
    Result[5] := HexDigit[GreenValue and $F];
    Result[6] := HexDigit[BlueValue shr 4];
    Result[7] := HexDigit[BlueValue and $F];
  end;
end;


{ TAdvHighlightLabel }

constructor TAdvHighlightLabel.Create(AOwner: TComponent);
var
  FDesignTime: boolean;

begin
  inherited;
  FText := TStringList.Create;
  (FText as TStringList).OnChange := TextChanged;

  FHighlighting := THighlightItems.Create(Self);
  FHighlighting.OnChange := HighlightChanged;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    FText.Add('Sample highlighted text in TAdvHighLightLabel');
    FHighlighting.Add.Text := 'highlighted';
  end;

  Width := 300;
  Height := 300;
end;

destructor TAdvHighlightLabel.Destroy;
begin
  FHighlighting.Free;
  FText.Free;
  inherited;
end;

procedure TAdvHighlightLabel.AdjustSize;
var
  r: TRect;
  av,sv,fa: string;
  xs, ys, hl, ml: integer;
  hr: TRect;
begin
  if (csLoading in ComponentState) then
    Exit;

  r := ClientRect;

  case AutoSizeType of
    asHorizontal, asBoth: r := Rect(0,0,4096, 4096);
  end;

  Canvas.Font.Assign(Font);

  HTMLDrawEx(Canvas, FRenderText, r, nil, 0,0,0,0,0, false,true,false,false,false,false,true,FFormScaled,
             1.0,
             clBlue, clNone, clNone, clSilver,
             av, sv, fa,
             xs, ys, hl, ml,
             hr, nil, nil, 0);

  Width := xs;
  Height := ys;

end;

function TAdvHighlightLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvHighlightLabel.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvHighlightLabel.Highlight(const AWord: string; Color,
  TextColor: TColor);
var
  hi: THighlightItem;
begin
  Highlighting.Clear;
  hi := Highlighting.Add;
  hi.Text := AWord;
  hi.Color := Color;
  hi.TextColor := TextColor;
end;

procedure TAdvHighlightLabel.HighlightChanged(Sender: TObject);
begin
  UpdateHighlight;
end;

procedure TAdvHighlightLabel.Loaded;
begin
  inherited;
  if AutoSize then
    AdjustSize;
end;

procedure TAdvHighlightLabel.Paint;
var
  r: TRect;
  av,sv,fa: string;
  xs, ys, hl, ml: integer;
  hr: TRect;
  frm: TCustomForm;
begin
  inherited;

  FFormScaled := true;
  frm := GetParentForm(Self);
  if Assigned(frm) and (frm is TForm) then
    FFormScaled := (frm as TForm).Scaled;

  r := ClientRect;

  Canvas.Font.Assign(Font);

  HTMLDrawEx(Canvas, FRenderText, r, nil, 0,0,0,0,0, false,false,false,false,false,false,true,FFormScaled,
             1.0,
             clBlue, clNone, clNone, clSilver,
             av, sv, fa,
             xs, ys, hl, ml,
             hr, nil, nil, 0);
end;

procedure TAdvHighlightLabel.SetAutoSizeType(const Value: TAutoSizeType);
begin
  FAutoSizeType := Value;
end;

procedure TAdvHighlightLabel.SetHighlighting(const Value: THighlightItems);
begin
  FHighlighting.Assign(Value);
end;

procedure TAdvHighlightLabel.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
end;

procedure TAdvHighlightLabel.SetVersion(const Value: string);
begin
  // readonly
end;

procedure TAdvHighlightLabel.TextChanged(Sender: TObject);
begin
  UpdateHighlight;
  if AutoSize then
    AdjustSize;
end;

procedure TAdvHighlightLabel.UpdateHighlight;
var
  i: integer;
  ftag: string;
  flags: TReplaceFlags;
begin
  FRenderText := FText.Text;


  for i := 0 to FHighlighting.Count - 1 do
  begin
    if FHighlighting.Items[i].Enabled and (FHighlighting.Items[i].Text <> '') then
    begin
      flags := [rfReplaceAll];

      if not FHighlighting.Items[i].CaseSensitive then
        flags := flags + [rfIgnoreCase];

      ftag := '<FONT bgcolor="' + ColorToHTML(FHighlighting.Items[i].Color) + '" color="' + ColorToHTML(FHighlighting.Items[i].TextColor)+'">' + FHighlighting.Items[i].Text + '</FONT>';
      FRenderText := StringReplace(FRenderText, FHighlighting.Items[i].Text, ftag, flags);
    end;
  end;

  Invalidate;
end;

{ THighlightItems }

function THighlightItems.Add: THighlightItem;
begin
  Result := THighlightItem(inherited Add);
end;

constructor THighlightItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, THighlightItem);
end;

function THighlightItems.GetItem(Index: integer): THighlightItem;
begin
  Result := THighlightItem(inherited Items[Index]);
end;

function THighlightItems.Insert(Index: integer): THighlightItem;
begin
  Result := THighlightItem(inherited Insert(Index));
end;

procedure THighlightItems.SetItem(Index: integer; const Value: THighlightItem);
begin
  inherited Items[Index] := Value;
end;

procedure THighlightItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ THighlightItem }

procedure THighlightItem.Assign(Source: TPersistent);
begin
  if (Source is THighlightItem) then
  begin
    FTag := (Source as THighlightItem).Tag;
    FColor := (Source as THighlightItem).Color;
    FTextColor := (Source as THighlightItem).TextColor;
    FText := (Source as THighlightItem).Text;
    FCaseSensitive := (Source as THighlightItem).CaseSensitive;
    FEnabled := (Source as THighlightItem).Enabled;
  end;
end;

constructor THighlightItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := true;
  FTag := 0;
  FColor := clHighlight;
  FTextColor := clHighlightText;
  FCaseSensitive := false;
end;

procedure THighlightItem.SetCaseSensitive(const Value: boolean);
begin
  if (FCaseSensitive <> Value) then
  begin
    FCaseSensitive := Value;
    Changed(false);
  end;
end;

procedure THighlightItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(false);
  end;
end;

procedure THighlightItem.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Changed(false);
  end;
end;

procedure THighlightItem.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Changed(false);
  end;
end;

procedure THighlightItem.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed(false);
  end;
end;

end.
