{***************************************************************************}
{ TAdvGauge component                                                       }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014 - 2019                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                              }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}
unit AdvGauge;

{$I TMSDEFS.INC}

interface

uses
  Windows, Forms, Math, Classes, Graphics, Controls, SysUtils, ExtCtrls, AdvGaugeControl,
  AdvStyleIF, Messages, GDIPFill, AdvGDIP
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.1.0.0 : New : ExtraNeedles collection added to add any nr. of needles on the gauge
  // v1.2.0.0 : New : TopPosition, UseTopPosition, TopPositionColor added to have the possibility for a 3rd arc range
  //          : New : MultiRanges to define any number of value ranges in the gauge
  // v1.2.1.0 : Improved : Support for High DPI

type
  TGaugeKind = (gkNone, gkMinmax, gkNightday, gkBattery, gkText);

  TTextPosition = (tpNone, tpInNeedle, tpTop, tpBottom);

  TAdvGauge = class;

  TGetCustomTextEvent = procedure(Sender: TObject; var Text: string) of object;

  TAdvGaugeNeedle = class(TPersistent)
  private
    FColor: TColor;
    FOpacity: Integer;
    FOwner: TAdvGauge;
    FWidth: Integer;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    procedure Draw(g: TGPGraphics; R: TGPRectF; TextHeight: double; Innermargin, Widest: Integer; Deg: Double; TextPosition: TTextPosition);
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TAdvGauge);
    procedure Assign(Sender: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clGray;
    property Opacity: Integer read FOpacity write SetOpacity default 255;
    property Width: Integer read FWidth write SetWidth default 20;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TNeedleItem = class(TCollectionItem)
  private
    FOpacity: Integer;
    FWidth: Integer;
    FColor: TColor;
    FPosition: Integer;
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetPosition(const Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clRed;
    property Opacity: Integer read FOpacity write SetOpacity default 255;
    property Position: Integer read FPosition write SetPosition default 0;
    property Width: Integer read FWidth write SetWidth default 10;
  end;

  TNeedleItems = class(TCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TNeedleItem;
    procedure SetItem(Index: Integer; const Value: TNeedleItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create;
    function Add: TNeedleItem;
    function Insert(Index: integer): TNeedleItem;
    property Items[Index: Integer]: TNeedleItem read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TRangeItem = class(TCollectionItem)
  private
    FOpacity: Integer;
    FColor: TColor;
    FPosition: Integer;
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: Integer);
    procedure SetPosition(const Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clGreen;
    property Opacity: Integer read FOpacity write SetOpacity default 255;
    property Position: Integer read FPosition write SetPosition;
  end;

  TRangeItems = class(TCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TRangeItem;
    procedure SetItem(Index: Integer; const Value: TRangeItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create;
    function Add: TRangeItem;
    function Insert(Index: integer): TRangeItem;
    property Items[Index: Integer]: TRangeItem read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvGauge = class(TCustomControl, IGaugeControl)
  private
    { Private declarations }
    FArcColor: TColor;
    FArcOpacity: Integer;
    FArcWidth: Integer;
    FBackground: TGDIPFill;
    FDegFirstPos: Double;
    FDegSecondPos: Double;
    FDegThirdPos: Double;
    FDegSplit: Double;
    FEndPosition: Double;
    FTopPosition: Double;
    FFormat: string;
    FInnerMargin: Integer;
    FIsRange: Boolean;
    FKind: TGaugeKind;
    FMinimum: Double;
    FMinText: string;
    FMaximum: Double;
    FMaxText: string;
    FNeedle: TAdvGaugeNeedle;
    FPosition: Double;
    FPictureMaximum: TAdvGdipPicture;
    FPictureMinimum: TAdvGdipPicture;
    FShowIcons: Boolean;
    FSplitArcColor: TColor;
    FSplitArcOpacity: Integer;
    FSplitPosition: Double;
    FTextPosition: TTextPosition;
    FUseSplitPosition: Boolean;
    FUseTopPosition: Boolean;
    FEndNeedle: TAdvGaugeNeedle;
    FExtraNeedles: TNeedleItems;
    FMultiRanges: TRangeItems;
    FOnGetCustomText: TGetCustomTextEvent;
    FTopArcColor: TColor;
    FTopArcOpacity: Integer;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    function DrawArc(var gpa: TGPGraphicsPath; var ba, bb, bc: TGPBrush; g: TGPGraphics; r, tr: TGPRectF): single;
    procedure DrawIcons(g: TGPGraphics; r, tr: TGPRectF; IconLeft, IconRight: TAdvGdipPicture);
    function GetValueDegrees(MinVal, MaxVal, Value, StartDeg, EndDeg: Double): Double;
    function GetIsRange: Boolean;
    function GetPosition: Double;
    function GetMinimum: Double;
    function GetMaximum: Double;
    function GetEndPosition: Double;
    function GetFormat: string;                           
    function GetVersion: string;
    function GetVersionNr: Integer;
    function Loading: Boolean;
    procedure SetArcColor(const Value: TColor);
    procedure SetArcWidth(const Value: Integer);
    procedure SetBackground(const Value: TGDIPFill);
    procedure SetEndPosition(const Value: Double);
    procedure SetFormat(const Value: string);
    procedure SetInnerMargin(const Value: Integer);
    procedure SetIsRange(const Value: Boolean);
    procedure SetKind(const Value: TGaugeKind);
    procedure SetMaximum(const Value: Double);
    procedure SetMaxText(const Value: string);
    procedure SetMinimum(const Value: Double);
    procedure SetMinText(const Value: string);
    procedure SetNeedle(const Value: TAdvGaugeNeedle);
    procedure SetPosition(const Value: Double);
    procedure SetPictureMaximum(const Value: TAdvGdipPicture);
    procedure SetPictureMinimum(const Value: TAdvGdipPicture);
    procedure SetShowIcons(const Value: Boolean);
    procedure SetTextPosition(const Value: TTextPosition);
    procedure SetVersion(const Value: string);
    procedure SetArcOpacity(const Value: Integer);
    procedure SetSplitArcColor(const Value: TColor);
    procedure SetSplitArcOpacity(const Value: Integer);
    procedure SetsplitPosition(const Value: Double);
    procedure SetUseSplitPosition(const Value: Boolean);
    procedure SetEndNeedle(const Value: TAdvGaugeNeedle);
    procedure SetExtraNeedles(const Value: TNeedleItems);
    procedure SetMultiRanges(const Value: TRangeItems);
    procedure SetTopPosition(const Value: Double);
    procedure SetTopArcColor(const Value: TColor);
    procedure SetTopArcOpacity(const Value: Integer);
    procedure SetUseTopPosition(const Value: Boolean);
  protected
    { Protected declarations }
    procedure Changed(Sender: TObject);
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure NeedleChanged(Sender: TObject); virtual;
    class function GetRadian(theta: Double): Double;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property ArcColor: TColor read FArcColor write SetArcColor default clGreen;
    property ArcWidth: Integer read FArcWidth write SetArcWidth default 40;
    property ArcOpacity: Integer read FArcOpacity write SetArcOpacity default 255;
    property Background: TGDIPFill read FBackground write SetBackground;
    property Constraints;
    property DoubleBuffered default True;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;                                                         
    property EndNeedle: TAdvGaugeNeedle read FEndNeedle write SetEndNeedle;
    property EndPosition: Double read GetEndPosition write SetEndPosition;
    property ExtraNeedles: TNeedleItems read FExtraNeedles write SetExtraNeedles;
    property MultiRanges: TRangeItems read FMultiRanges write SetMultiRanges;
    property Format: string read GetFormat write SetFormat;
    property Font;
    property Height default 190;
    property InnerMargin: Integer read FInnerMargin write SetInnerMargin default 10;
    property IsRange: Boolean read GetIsRange write SetIsRange default False;
    property Kind: TGaugeKind read FKind write SetKind default gkMinmax;
    property Needle: TAdvGaugeNeedle read FNeedle write SetNeedle;
    property Minimum: Double read GetMinimum write SetMinimum;
    property MinText: string read FMinText write SetMinText;
    property Maximum: Double read GetMaximum write SetMaximum;
    property MaxText: string read FMaxText write SetMaxText;
    property PictureMinimum: TAdvGdipPicture read FPictureMinimum write SetPictureMinimum;
    property PictureMaximum: TAdvGdipPicture read FPictureMaximum write SetPictureMaximum;
    property Position: Double read GetPosition write SetPosition;
    property PopupMenu;
    property ShowHint;
    property ShowIcons: Boolean read FShowIcons write SetShowIcons default True;
    property SplitArcColor: TColor read FSplitArcColor write SetSplitArcColor default clRed;
    property SplitArcOpacity: Integer read FSplitArcOpacity write SetSplitArcOpacity default 255;
    property SplitPosition: Double read FSplitPosition write SetSplitPosition;
    property Tag;
    property TextPosition: TTextPosition read FTextPosition write SetTextPosition default tpInNeedle;
    property Top;
    property TopArcColor: TColor read FTopArcColor write SetTopArcColor default clYellow;
    property TopArcOpacity: Integer read FTopArcOpacity write SetTopArcOpacity default 255;
    property TopPosition: Double read FTopPosition write SetTopPosition;
    property Touch;
    property UseSplitPosition: Boolean read FUseSplitPosition write SetUseSplitPosition default true;
    property UseTopPosition: Boolean read FUseTopPosition write SetUseTopPosition default false;
    property Visible;
    property Version: string read GetVersion write SetVersion;
    property Width default 300;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnGetCustomText: TGetCustomTextEvent read FOnGetCustomText write FOnGetCustomText;
  end;

implementation

{$R AdvGauge.res}

{ TAdvGauge }

procedure TAdvGauge.Changed(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvGauge.CMFontChanged(var Msg: TMessage);
begin
  Invalidate;
end;

constructor TAdvGauge.Create(AOwner: TComponent);
begin
  inherited;

  Width := 300;
  Height := 190;
  FPosition := 50;
  FEndPosition := 100;
  FTopPosition := 100;
  FIsRange := False;
  FIsRange := False;
  FMinimum := 0;
  FMaximum := 100;
  FArcWidth := 40;
  FArcColor := clGreen;
  FArcOpacity := 255;
  FSplitArcColor := clRed;
  FSplitArcOpacity := 255;
  FTopArcColor := clYellow;
  FTopArcOpacity := 255;
  FInnerMargin := 10;
  FShowIcons := True;
  DoubleBuffered := True;
  FUseSplitPosition := True;
  FSplitPosition := 75;

  FNeedle := TAdvGaugeNeedle.Create(Self);
  FNeedle.OnChange := NeedleChanged;

  FEndNeedle := TAdvGaugeNeedle.Create(Self);
  FEndNeedle.OnChange := NeedleChanged;

  FBackground := TGDIPFill.Create;
  FBackground.Color := clWhite;
  FBackground.ColorTo := clWhite;
  FBackground.OnChange := Changed;

  FPictureMaximum := TAdvGdipPicture.Create;
  FPictureMaximum.OnChange := Changed;
  FPictureMinimum := TAdvGdipPicture.Create;
  FPictureMinimum.OnChange := Changed;

  Kind := gkMinmax;
  TextPosition := tpInNeedle;

  FMinText := 'Left';
  FMaxText := 'Right';

  FDegSplit := GetValueDegrees(FMinimum, FMaximum, FSplitPosition, 210, 330);
  FDegFirstPos := GetValueDegrees(FMinimum, FMaximum, FPosition, 210, 330);
  FDegSecondPos := GetValueDegrees(FMinimum, FMaximum, FEndPosition, 210, 330);
  FDegThirdPos := GetValueDegrees(FMinimum, FMaximum, FTopPosition, 210, 330);
  FExtraNeedles := TNeedleItems.Create;
  FExtraNeedles.OnChange := NeedleChanged;
  FMultiRanges := TRangeItems.Create;
  FMultiRanges.OnChange := NeedleChanged;
end;

destructor TAdvGauge.Destroy;
begin
  FMultiRanges.Free;
  FExtraNeedles.Free;
  FBackground.Free;
  FEndNeedle.Free;
  FNeedle.Free;
  FPictureMaximum.Free;
  FPictureMinimum.Free;

  inherited;
end;

function TAdvGauge.GetEndPosition: Double;
begin
  Result := FEndPosition;
end;

function TAdvGauge.GetFormat: string;
begin
  Result := FFormat;
end;

function TAdvGauge.GetIsRange: Boolean;
begin
  Result := FIsRange;
end;

function TAdvGauge.GetMaximum: Double;
begin
  Result := FMaximum;
end;

function TAdvGauge.GetMinimum: Double;
begin
  Result := FMinimum
end;

function TAdvGauge.GetPosition: Double;
begin
  Result := FPosition;
end;

class function TAdvGauge.GetRadian(theta: Double): Double;
begin
  Result := theta * (PI / 180);
end;

function TAdvGauge.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvGauge.Loaded;
begin
  inherited;
end;

function TAdvGauge.Loading: Boolean;
begin
  Result := (csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState);
end;

procedure TAdvGauge.NeedleChanged(Sender: TObject);
begin
  Invalidate;
end;

function TAdvGauge.GetValueDegrees(MinVal, MaxVal, Value, StartDeg, EndDeg: Double): Double;
begin
  Result := (MaxVal - MinVal);
  if Result <> 0 then
    Result := ((Value - MinVal)) / Result;

  Result := (EndDeg - StartDeg) * Result;
  Result := Max(Result + StartDeg, StartDeg);
  Result := Min(Result, EndDeg);
end;

function TAdvGauge.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

procedure TAdvGauge.Paint;
var
  g: TGPGraphics;
  r, tr, ir, nr: TGPRectF;
  gpa: TGPGraphicsPath;
  ba, bb, bc: TGPBrush;
  gpfont: TGPFont;
  gpbrush: TGPBrush;
  PosText: string;
  pt: TGPPointF;
  widest: integer;
  h,w,th,i: integer;
  arcdelta: single;
  ExtraNeedle: TAdvGaugeNeedle;
  FDegNeedle: double;
  DPIScale: single;

begin
  gpa := nil;
  ba := nil;
  bb := nil;
  bc := nil;
  gpfont := nil;
  gpbrush := nil;
  DPIScale := GetDPIScale(Self, Canvas);

  //Create the TGPGraphics
  g := TGPGraphics.Create(Canvas.Handle);

  try
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

    //create the font and brush
    gpfont := g.MakeFont(Font);

    //Fill the background
    r := MakeRect(0, 0, ClientRect.Right - ClientRect.Left, ClientRect.Bottom - ClientRect.Top);

    th := 0;
    if TextPosition = tpBottom then
    begin
      g.MeasureString('gh',2,gpfont,r,ir);
      th := Round(ir.Height) + 2;
    end;

    FBackground.Fill(g, r);

    if FIsRange then
      widest := Max(Round(DPIScale * FNeedle.Width), Round(DPIScale * FEndNeedle.Width))
    else
      widest := Round(DPIScale * FNeedle.Width);

    for i := 0 to Pred(ExtraNeedles.Count) do
      widest := Max(widest, Round(DPIScale * ExtraNeedles[i].Width));
    w := Width;
    h := Width div 2 + (widest div 2) + th;

    if h > Height then
    begin
      w := (Height - widest div 2 - th) * 2;
      h := Height;
    end;

    r := MakeRect(0, 0, w, h);

    gpbrush := TGPSolidBrush.Create(MakeColor(255, Font.Color));

    //calculate the Position rectangle
    if TextPosition <> tpNone then
    begin
      PosText := FloatToStr(Position);

      if FFormat <> '' then
      begin
        PosText := SysUtils.Format(FFormat, [Position])
      end;
      if Assigned(FOnGetCustomText) then
        FOnGetCustomText(Self, PosText);

      g.MeasureString(PosText,Length(PosText), gpfont ,r, tr);

      case TextPosition of
        tpInNeedle:
        begin
          pt.X := (w - tr.Width) / 2;
          pt.Y := (w - tr.Height) / 2;
        end;
        tpTop:
        begin
          pt.X := (w - tr.Width) / 2;
          pt.Y := Max(0, (Round(DPIScale * InnerMargin) - tr.Height)/2);
        end;
        tpBottom:
        begin
          pt.X := (w - tr.Width) / 2;
          pt.Y := h - tr.Height;
        end;
      end;
    end;

    //Draw the Arc
    gpa := TGPGraphicsPath.Create;
    ba := TGPSolidBrush.Create(MakeColor(FArcOpacity, FArcColor));
    bb := TGPSolidBrush.Create(MakeColor(FSplitArcOpacity, FSplitArcColor));
    bc := TGPSolidBrush.Create(MakeColor(FTopArcOpacity, FTopArcColor));

    arcdelta := DrawArc(gpa, ba, bb, bc, g, r, tr);

    //Draw the needle
    if Assigned(FNeedle) then
    begin
      nr := r;
      nr.Height := r.Height - Round(DPIScale * Needle.Width) - th;

      FNeedle.Draw(g, nr, tr.Height, InnerMargin, widest, FDegFirstPos, TextPosition);

      if FIsRange then
      begin
        nr := r;
        nr.Height := r.Height - Round(DPIScale * EndNeedle.Width) - th;
        FEndNeedle.Draw(g, nr, tr.Height, FInnerMargin, widest, FDegSecondPos, TextPosition);
      end;
    end;

    if ExtraNeedles.Count > 0 then
    begin
      ExtraNeedle := TAdvGaugeNeedle.Create(Self);

      for i := 0 to ExtraNeedles.Count - 1 do
      begin
        ExtraNeedle.Color := ExtraNeedles[i].Color;
        ExtraNeedle.Width := ExtraNeedles[i].Width;
        ExtraNeedle.Opacity := ExtraNeedles[i].Opacity;

        nr := r;
        nr.Height := r.Height - Round(DPIScale * ExtraNeedle.Width) - th;

        FDegNeedle := GetValueDegrees(FMinimum, FMaximum, ExtraNeedles[i].Position, 210, 330);
        ExtraNeedle.Draw(g, nr, tr.Height, InnerMargin, widest, FDegNeedle, TextPosition);
      end;

      ExtraNeedle.Free;
    end;


    //Draw PosText
    if TextPosition <> tpNone then
      g.DrawString(PosText,Length(PosText),gpFont,pt, gpbrush);

    //Draw the Icons/Text
    if not PictureMinimum.Empty then
    begin
      nr := r;
      nr.Height := r.Height - th - (arcdelta - PictureMinimum.Height) /2;
      DrawIcons(g, nr, tr, PictureMinimum, PictureMaximum);
    end;

    if FKind = gkText then
    begin
      nr := r;

      g.MeasureString(FMinText, Length(FMinText), gpfont, r, ir);

      nr.Height := r.Height - th - (arcdelta - ir.Height) /2;

      pt.X := FInnerMargin;
      pt.Y := nr.Height - ir.Height;

      if FTextPosition = tpTop then
        pt.X := pt.X + Max((tr.Height - FInnerMargin) / 2, 0);

      g.DrawString(FMinText, Length(FMinText), gpfont, pt, gpbrush);

      g.MeasureString(FMaxText, Length(FMaxText), gpfont, r, ir);

      pt.X := r.Width - FInnerMargin - ir.Width;
      pt.Y := nr.Height - ir.Height;

      if FTextPosition = tpTop then
        pt.X := pt.X - Max((tr.Height - FInnerMargin) / 2, 0) / 2;

      g.DrawString(FMaxText, Length(FMaxText), gpfont, pt, gpbrush);
    end;
  finally
    if Assigned(gpbrush) then
      gpbrush.Free;
    if Assigned(gpfont) then
      gpfont.Free;
    if Assigned(bc) then
      bc.Free;
    if Assigned(bb) then
      bb.Free;
    if Assigned(ba) then
      ba.Free;
    if Assigned(gpa) then
      gpa.Free;

    g.Free;
  end;
end;

procedure TAdvGauge.Resize;
begin
  inherited;
  SetArcWidth(FArcWidth);
end;

procedure TAdvGauge.SetArcColor(const Value: TColor);
begin
  if Value <> FArcColor then
  begin
    FArcColor := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetArcOpacity(const Value: Integer);
begin
  if (Value <> FArcOpacity) and (Value <= 255) and (Value >= 0) then
  begin
    FArcOpacity := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetArcWidth(const Value: Integer);
begin
  if (Value <> FArcWidth) and (Value > 0) then
  begin
    FArcWidth := Value;
    if FArcWidth > (Width div 2) - FInnerMargin then
      FArcWidth := (Width div 2) - FInnerMargin - 2;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetBackground(const Value: TGDIPFill);
begin
  if Value <> FBackground then
  begin
    FBackground.Assign(Value);
    Invalidate;
  end;
end;

procedure TAdvGauge.DrawIcons(g: TGPGraphics; r, tr: TGPRectF; IconLeft, IconRight: TAdvGdipPicture);
var
  TextOffset: Double;
  dr: TGPRectF;
  DPIScale: single;
  cIM, cAW: integer;
begin
  if FShowIcons and Assigned(IconLeft) and Assigned(IconRight) then
  begin
    TextOffset := 0;
    DPIScale := GetDPIScale(Self, Canvas);
    cIM := Round(DPIScale * FInnerMargin);
    cAW := Round(DPIScale * FArcWidth);


    dr.Y := r.Height - IconLeft.Height;

    if FTextPosition = tpTop then
      TextOffset := Max(0, tr.Height - cIM);

    dr.X := FInnerMargin + TextOffset / 2 + Max(0, cAW - IconLeft.Width) / 2;

    dr.Width := IconLeft.Width;
    dr.Height := IconLeft.Height;
    IconLeft.GDIPDraw(g, dr);

    dr.X := r.Width - IconRight.Width - cIM - TextOffset - Max(0, cAW - IconRight.Width) / 2;
    dr.Y := dr.Y + IconLeft.Height - IconRight.Height;
    dr.Width := IconRight.Width;
    dr.Height := IconRight.Height;
    IconRight.GDIPDraw(g, dr);
  end;
end;

function TAdvGauge.DrawArc(var gpa: TGPGraphicsPath; var ba, bb, bc: TGPBrush; g: TGPGraphics; r, tr: TGPRectF): single;
var
  TextOffset: Double;
  dr,rb: TGPRectF;
  bm: TGPSolidBrush;
  dStart, dEnd: Double;
  i, iOpacity: Integer;
  cl: TColor;
  DPIScale: single;
  cAW, cIM: integer;
begin
  // outer rect of circle
//  if FIsRange then
//    el := Max(FNeedle.Width, FEndNeedle.Width)
//  else
//    el := FNeedle.Width;
  DPIScale := GetDPIScale(Self,Canvas);
  cAW := Round( FArcWidth * DPIScale);
  cIM := Round( FInnerMargin * DPIScale);

  TextOffset := 0;

  if (FTextPosition = tpTop) and (tr.Height > FInnerMargin) then
    TextOffset := tr.Height;

  dr.Y := r.Y + cIM + TextOffset / 2;
  dr.X := r.x + cIM + TextOffset / 2;
  dr.Width := r.Width - cIM * 2 - TextOffset;
  dr.Height := dr.Width;

  Result := (dr.Width / 2) * sin(PI/6);

  // inner rect of circle
  rb.X := dr.X + cAW;
  rb.Y := dr.y + cAW;
  rb.Width := dr.Width - (2 * cAW);
  rb.Height := rb.Width;


  if FMultiRanges.Count > 0 then
  begin
    dStart := 210;
    cl := ArcColor;
    iOpacity := ArcOpacity;
    for i := 0 to Pred(FMultiRanges.Count) do
    begin
      dEnd := GetValueDegrees(FMinimum, FMaximum, FMultiRanges[i].Position, 210, 330);
      bm := TGPSolidBrush.Create(MakeColor(iOpacity, cl));
      try
        if (rb.Width > 0) and (rb.Height > 0) then
        begin
          gpa.AddArc(dr, dStart, dEnd - dStart);
          gpa.AddArc(rb, dEnd, dStart - dEnd);
        end
        else
          gpa.AddPie(dr, dStart, dEnd - dStart);
        dStart := dEnd;
        cl := FMultiRanges[i].Color;
        iOpacity := FMultiRanges[i].Opacity;
        gpa.CloseFigure;
        g.FillPath(bm, gpa);
      finally
        bm.Free;
      end;
      gpa.Reset;
    end;
    dEnd := 330;
    bm := TGPSolidBrush.Create(MakeColor(iOpacity, cl));
    try
      if (rb.Width > 0) and (rb.Height > 0) then
      begin
        gpa.AddArc(dr, dStart, dEnd - dStart);
        gpa.AddArc(rb, dEnd, dStart - dEnd);
      end
      else
        gpa.AddPie(dr, dStart, dEnd - dStart);
      gpa.CloseFigure;
      g.FillPath(bm, gpa);
    finally
      bm.Free;
    end;
  end
  else
  if FSplitArcColor = clNone then
  begin
    if (rb.Width > 0) and (rb.Height > 0) then
    begin
      gpa.AddArc(dr, 210, 120);
      gpa.AddArc(rb, 330, -120);
    end
    else
      gpa.AddPie(dr, 210, 120);

    gpa.CloseFigure;
    g.FillPath(ba, gpa);
  end
  else
  if FUseSplitPosition then
  begin
    //Draw first Arc
    if (rb.Width > 0) and (rb.Height > 0) then
    begin
      gpa.AddArc(dr, 210, 120 - 330 + FDegSplit);
      gpa.AddArc(rb, FDegSplit, -120 + 330 - FDegSplit);
    end
    else
      gpa.AddPie(dr, 210, 120 - 330 + FDegSplit);

    gpa.CloseFigure;
    g.FillPath(ba, gpa);
    gpa.Reset;

    if FUseTopPosition then
    begin
      //Draw the second Arc
      if (rb.Width > 0) and (rb.Height > 0) then
      begin
        gpa.AddArc(dr, FDegSplit, FDegThirdPos - FDegSplit);
        gpa.AddArc(rb, FDegThirdPos, FDegSplit - FDegThirdPos);
      end
      else
        gpa.AddPie(dr, FDegSplit, FDegThirdPos - FDegSplit);

      gpa.CloseFigure;
      g.FillPath(bb, gpa);
      gpa.Reset;

      //Draw the third Arc
      if (rb.Width > 0) and (rb.Height > 0) then
      begin
        gpa.AddArc(dr, FDegThirdPos, 330 - FDegThirdPos);
        gpa.AddArc(rb, 330, -330 + FDegThirdPos);
      end
      else
        gpa.AddPie(dr, FDegThirdPos, 330 - FDegThirdPos);

      gpa.CloseFigure;
      g.FillPath(bc, gpa);
    end
    else
    begin
      //Draw the second arc
      if (rb.Width > 0) and (rb.Height > 0) then
      begin
        gpa.AddArc(dr, FDegSplit, 330 - FDegSplit);
        gpa.AddArc(rb, 330, -330 + FDegSplit);
      end
      else
        gpa.AddPie(dr, FDegSplit, 330 - FDegSplit);

      gpa.CloseFigure;
      g.FillPath(bb, gpa);
    end;
  end
  else
  if FIsRange then
  begin
    //Draw the first Arc
    if (rb.Width > 0) and (rb.Height > 0) then
    begin
      gpa.AddArc(dr, 210, 120 - 330 + Min(FDegFirstPos, FDegSecondPos));
      gpa.AddArc(rb, Min(FDegFirstPos, FDegSecondPos), -120 + 330 - Min(FDegFirstPos, FDegSecondPos));
    end
    else
      gpa.AddPie(dr, 210, 120 - 330 + Min(FDegFirstPos, FDegSecondPos));

    gpa.CloseFigure;
    g.FillPath(ba, gpa);
    gpa.Reset;

    //Draw the second Arc
    if (rb.Width > 0) and (rb.Height > 0) then
    begin
      gpa.AddArc(dr, Min(FDegFirstPos, FDegSecondPos), Max(FDegFirstPos, FDegSecondPos) - Min(FDegFirstPos, FDegSecondPos));
      gpa.AddArc(rb, Max(FDegFirstPos, FDegSecondPos), Min(FDegFirstPos, FDegSecondPos) - Max(FDegFirstPos, FDegSecondPos));
    end
    else
      gpa.AddPie(dr, Min(FDegFirstPos, FDegSecondPos), Max(FDegFirstPos, FDegSecondPos) - Min(FDegFirstPos, FDegSecondPos));

    gpa.CloseFigure;
    g.FillPath(bb, gpa);
    gpa.Reset;

    //Draw the third Arc
    if (rb.Width > 0) and (rb.Height > 0) then
    begin
      gpa.AddArc(dr, Max(FDegFirstPos, FDegSecondPos), 330 - Max(FDegFirstPos, FDegSecondPos));
      gpa.AddArc(rb, 330, -330 + Max(FDegFirstPos, FDegSecondPos));
    end
    else
      gpa.AddPie(dr, Max(FDegFirstPos, FDegSecondPos), 330 - Max(FDegFirstPos, FDegSecondPos));

    gpa.CloseFigure;
    g.FillPath(ba, gpa);
  end
  else
  begin
    //Draw first Arc
    if (rb.Width > 0) and (rb.Height > 0) then
    begin
      gpa.AddArc(dr, 210, 120 - 330 + FDegFirstPos);
      gpa.AddArc(rb, FDegFirstPos, -120 + 330 - FDegFirstPos);
    end
    else
      gpa.AddPie(dr, 210, 120 - 330 + FDegFirstPos);

    gpa.CloseFigure;
    g.FillPath(ba, gpa);
    gpa.Reset;

    //Draw the second arc
    if (rb.Width > 0) and (rb.Height > 0) then
    begin
      gpa.AddArc(dr, FDegFirstPos, 330 - FDegFirstPos);
      gpa.AddArc(rb, 330, -330 + FDegFirstPos);
    end
    else
      gpa.AddPie(dr, FDegFirstPos, 330 - FDegFirstPos);

    gpa.CloseFigure;
    g.FillPath(bb, gpa);
  end;
end;

procedure TAdvGauge.SetPictureMaximum(const Value: TAdvGdipPicture);
begin
  if (Value <> FPictureMaximum) and (FKind <> gkNone) and (FKind <> gkText) then
  begin
    FPictureMaximum.Assign(Value);
    Invalidate;
  end;
end;

procedure TAdvGauge.SetPictureMinimum(const Value: TAdvGdipPicture);
begin
  if (Value <> FPictureMinimum) and (FKind <> gkNone) and (FKind <> gkText) then
  begin
    FPictureMinimum.Assign(Value);
    Invalidate;
  end;
end;

procedure TAdvGauge.SetEndPosition(const Value: Double);
begin
  if (Value <> FEndPosition) then
  begin
    FEndPosition := Value;
    FDegSecondPos := GetValueDegrees(FMinimum, FMaximum, Value, 210, 330);
    Invalidate;
  end;
end;

procedure TAdvGauge.SetExtraNeedles(const Value: TNeedleItems);
begin
  FExtraNeedles.Assign(Value);
  Invalidate;
end;

procedure TAdvGauge.SetFormat(const Value: string);
begin
  if Value <> FFormat then
  begin
    FFormat := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetInnerMargin(const Value: Integer);
begin
  if Value <> FInnerMargin then
  begin
    FInnerMargin := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetIsRange(const Value: Boolean);
begin
  if Value <> FIsRange then
  begin
    FIsRange := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetKind(const Value: TGaugeKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;

    case FKind of
      gkNone:
        begin
          FPictureMinimum.Assign(nil);
          FPictureMaximum.Assign(nil);
        end;
      gkMinmax:
        begin
          FPictureMinimum.LoadFromResourceName(HInstance, 'TMSGMIN');
          FPictureMaximum.LoadFromResourceName(HInstance, 'TMSGMAX');
        end;
      gkNightday:
        begin
          FPictureMinimum.LoadFromResourceName(HInstance, 'TMSGNIGHT');
          FPictureMaximum.LoadFromResourceName(HInstance, 'TMSGDAY');
        end;
      gkBattery:
        begin
          FPictureMinimum.LoadFromResourceName(HInstance, 'TMSGEMPTY');
          FPictureMaximum.LoadFromResourceName(HInstance, 'TMSGFULL');
        end;
      gkText:
        begin
          FPictureMaximum.Assign(nil);
          FPictureMinimum.Assign(nil);
        end;
    end;

    Invalidate;
  end;
end;

procedure TAdvGauge.SetMaximum(const Value: Double);
begin
  if (Value <> FMaximum) and (Loading or (Value > FMinimum)) then
  begin
    FMaximum := Value;

    FDegFirstPos := GetValueDegrees(FMinimum, Value, Min(FPosition, Value), 210, 330);
    FDegSecondPos := GetValueDegrees(FMinimum, Value, Min(FEndPosition, Value), 210, 330);
    FDegThirdPos := GetValueDegrees(FMinimum, Value, Min(FTopPosition, Value), 210, 330);
    Invalidate;
  end;
end;

procedure TAdvGauge.SetMaxText(const Value: string);
begin
  if Value <> FMaxText then
  begin
    FMaxText := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetMinimum(const Value: Double);
begin
  if (Value <> FMinimum) and (Loading or (Value < FMaximum)) then
  begin
    FMinimum := Value;
    FDegFirstPos := GetValueDegrees(Value, FMaximum, Max(FPosition, Value), 210, 330);
    FDegSecondPos := GetValueDegrees(Value, FMaximum, Max(FEndPosition, Value), 210, 330);
    FDegThirdPos := GetValueDegrees(FMinimum, Value, Min(FTopPosition, Value), 210, 330);
    Invalidate;
  end;
end;

procedure TAdvGauge.SetMinText(const Value: string);
begin
  if Value <> FMinText then
  begin
    FMinText := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetNeedle(const Value: TAdvGaugeNeedle);
begin
  FNeedle.Assign(Value);
  Invalidate;
end;

procedure TAdvGauge.SetPosition(const Value: Double);
begin
  if (Value <> FPosition) and (Loading or ((Value <= FMaximum) and (Value >= FMinimum))) then
  begin
    FPosition := Value;
    FDegFirstPos := GetValueDegrees(FMinimum, FMaximum, Value, 210, 330);
    Invalidate;
  end;
end;

procedure TAdvGauge.SetShowIcons(const Value: Boolean);
begin
  if Value <> FShowIcons then
  begin
    FShowIcons := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetSplitArcColor(const Value: TColor);
begin
  if Value <> FSplitArcColor then
  begin
    FSplitArcColor := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetSplitArcOpacity(const Value: Integer);
begin
  if Value <> FSplitArcOpacity then
  begin
    FSplitArcOpacity := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetEndNeedle(const Value: TAdvGaugeNeedle);
begin
  FEndNeedle.Assign(Value);
  Invalidate
end;

procedure TAdvGauge.SetMultiRanges(const Value: TRangeItems);
begin
  FMultiRanges.Assign(Value);
  Invalidate;
end;

procedure TAdvGauge.SetTopPosition(const Value: Double);
begin
  if (Value <> FTopPosition) then
  begin
    FTopPosition := Value;
    FDegThirdPos := GetValueDegrees(FMinimum, FMaximum, Value, 210, 330);
    Invalidate;
  end;
end;

procedure TAdvGauge.SetTopArcColor(const Value: TColor);
begin
  if Value <> FTopArcColor then
  begin
    FTopArcColor := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetTopArcOpacity(const Value: Integer);
begin
  if Value <> FTopArcOpacity then
  begin
    FTopArcOpacity := Value;
    Invalidate;
  end;
end;
procedure TAdvGauge.SetsplitPosition(const Value: Double);
begin
  if Value <> FSplitPosition then
  begin
    FSplitPosition := Value;
    FDegSplit := GetValueDegrees(FMinimum, FMaximum, Value, 210, 330);
    Invalidate;
  end;
end;

procedure TAdvGauge.SetTextPosition(const Value: TTextPosition);
begin
  if Value <> FTextPosition then
  begin
    FTextPosition := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetUseSplitPosition(const Value: Boolean);
begin
  if Value <> FUseSplitPosition then
  begin
    FUseSplitPosition := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetUseTopPosition(const Value: Boolean);
begin
  if (FUseTopPosition <> Value) then
  begin
    FUseTopPosition := Value;
    Invalidate;
  end;
end;

procedure TAdvGauge.SetVersion(const Value: string);
begin

end;

{ TAdvGaugeNeedle }

procedure TAdvGaugeNeedle.Assign(Sender: TPersistent);
var
  needle: TAdvGaugeNeedle;
begin
  if Sender is TAdvGaugeNeedle then
  begin
    needle := Sender as TAdvGaugeNeedle;
    FColor := needle.FColor;
    FWidth := needle.FWidth;
    FOpacity := needle.FOpacity;
  end;
end;

procedure TAdvGaugeNeedle.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvGaugeNeedle.Create(AOwner: TAdvGauge);
begin
  FOwner := AOwner;
  Color := clRed;
  Width := 20;
  Opacity := 255;
end;

procedure TAdvGaugeNeedle.Draw(g: TGPGraphics; R: TGPRectF; TextHeight: double; InnerMargin, Widest: integer; Deg: double; TextPosition: TTextPosition);
var
  gpa: TGPGraphicsPath;
  dr: TGPRectF;
  a, ar, al, rd, cx, cy, TextOffset: Double;
  ba: TGPBrush;
  DPIScale: single;
  cW, cIM: integer;
begin
  TextOffset := 0;
  DPIScale := GetDPIScale(FOwner, FOwner.Canvas);
  cW := Round(Width * DPIScale);
  cIM := Round(Innermargin * DPIScale);

  //needle circle
  dr.X := (r.Width - cW) / 2;
  dr.Y := r.height - (Widest - cW) / 2; //dr.X;

  dr.Width := cW;
  dr.Height := dr.Width;

  //needle triangle
  cx := r.Width / 2;
  cy := r.Height + cW / 2 - (Widest - cW) / 2;

  if (TextPosition = tpTop) and (TextHeight > cIM) then
    TextOffset := TextHeight;

  rd := r.Width / 2 - cIM - TextOffset / 2;

  a := TAdvGauge.GetRadian(Deg);
  ar := TAdvGauge.GetRadian(Deg + 90);
  al := TAdvGauge.GetRadian(Deg - 90);

  gpa := TGPGraphicsPath.Create;
  ba := TGPSolidBrush.Create(MakeColor(Opacity, Color));
  try
    gpa.AddLine(cx + rd * Cos(a), cy + rd * Sin(a), cx + cW / 2 * Cos(ar), cy + cW / 2 * Sin(ar));
    gpa.AddLine(cx + cW / 2 * Cos(ar), cy + cW / 2 * Sin(ar), cx + cW / 2 * Cos(al), cy + cW / 2 * Sin(al));
    gpa.AddArc(dr, Deg + 90, 180);
    gpa.CloseFigure;
    g.FillPath(ba, gpa);
  finally
    gpa.Free;
    ba.Free;
  end;
end;

procedure TAdvGaugeNeedle.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvGaugeNeedle.SetOpacity(const Value: Integer);
begin
  if (Value <> Opacity) and (Value <= 255) and (Value >= 0) then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvGaugeNeedle.SetWidth(const Value: Integer);
begin
  if Value <> Width then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TNeedleItem }

constructor TNeedleItem.Create(Collection: TCollection);
begin
  inherited;
  FColor := clRed;
  FOpacity := 255;
  FWidth := 10;
end;

procedure TNeedleItem.Assign(Source: TPersistent);
begin
 if Source is TNeedleItem then
 begin
   FOpacity := TNeedleItem(Source).FOpacity;
   FColor := TNeedleItem(Source).FColor;
   FPosition := TNeedleItem(Source).FPosition;
   FWidth := TNeedleItem(Source).FWidth;
 end
 else
   inherited Assign(Source);
end;

procedure TNeedleItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(false);
  end;
end;

procedure TNeedleItem.SetOpacity(const Value: Integer);
begin
  if (FOpacity <> Value) then
  begin
    FOpacity := Value;
    Changed(false);
  end;
end;

procedure TNeedleItem.SetPosition(const Value: Integer);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed(False);
  end;
end;

procedure TNeedleItem.SetWidth(const Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

{ TNeedleItems }

function TNeedleItems.Add: TNeedleItem;
begin
  Result := TNeedleItem(inherited Add);
end;

constructor TNeedleItems.Create;
begin
  inherited Create(TNeedleItem);
end;

function TNeedleItems.GetItem(Index: Integer): TNeedleItem;
begin
  Result := TNeedleItem(inherited Items[Index]);
end;

function TNeedleItems.Insert(Index: integer): TNeedleItem;
begin
  Result := TNeedleItem(inherited Insert(Index));
end;

procedure TNeedleItems.SetItem(Index: Integer; const Value: TNeedleItem);
begin
  inherited Items[Index] := Value;
end;

procedure TNeedleItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);

end;

{ TRangeItems }

function TRangeItems.Add: TRangeItem;
begin
  Result := TRangeItem(inherited Add);
end;

constructor TRangeItems.Create;
begin
  inherited Create(TRangeItem);
end;

function TRangeItems.GetItem(Index: Integer): TRangeItem;
begin
  Result := TRangeItem(inherited Items[Index]);
end;

function TRangeItems.Insert(Index: integer): TRangeItem;
begin
  Result := TRangeItem(inherited Insert(Index));
end;

procedure TRangeItems.SetItem(Index: Integer; const Value: TRangeItem);
begin
  inherited Items[Index] := Value;
end;

procedure TRangeItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ TRangeItem }

constructor TRangeItem.Create(Collection: TCollection);
begin
  inherited;
  FColor := clGreen;
  FOpacity := 255;
end;

procedure TRangeItem.Assign(Source: TPersistent);
begin
 if Source is TRangeItem then
 begin
   FOpacity := TRangeItem(Source).FOpacity;
   FColor := TRangeItem(Source).FColor;
   FPosition := TRangeItem(Source).FPosition;
 end
 else
   inherited Assign(Source);
end;

procedure TRangeItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(false);
  end;
end;

procedure TRangeItem.SetOpacity(const Value: Integer);
begin
  if (FOpacity <> Value) then
  begin
    FOpacity := Value;
    Changed(false);
  end;
end;

procedure TRangeItem.SetPosition(const Value: Integer);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed(False);
  end;
end;

end.
