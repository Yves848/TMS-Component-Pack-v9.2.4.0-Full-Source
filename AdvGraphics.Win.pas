{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2017                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvGraphics.Win;

{$I TMSDEFS.INC}

interface

{$IFDEF MSWINDOWS}

uses
  Classes, Windows, ActiveX, AdvGDIPlusClasses, AdvGDIPlusApi, AdvGraphicsTypes,
  {%H-}Types, AdvGraphics, Graphics, AdvUtils, AdvTypes
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;
{$ENDIF}

{$IFNDEF LCLLIB}
const
  FPC_FULLVERSION = 0;
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  TAdvGraphicsContextWin = class(TAdvGraphicsContext)
  private
    FShowAcceleratorChar: Boolean;
    FTextMatrix: TGPMatrix;
    FSmoothingMode: SmoothingMode;
    FSaveGP: Cardinal;
    FActivePath: TAdvGraphicsPath;
    FScale: Single;
    FMovePoint: TPointF;
    FNeedsRendering: Boolean;
    FFont: TAdvGraphicsFont;
    FFill: TAdvGraphicsFill;
    FStroke: TAdvGraphicsStroke;
    FGPFont: TGPFont;
    FContextSize: TSizeF;
    FGPStringFormat: TGPStringFormat;
    FGPBrush: TGPBrush;
    FGPPen: TGPPen;
    FGPGraphics: TGPGraphics;
    FGPBitmap: TGPBitmap;
    FBitmap: TBitmap;
    {$IFDEF FMXLIB}
    FMapping: Boolean;
    FBitmapData: TBitmapData;
    {$ENDIF}
  protected
    function GetNativeCanvas: Pointer; override;
    procedure DestroyResources;
    procedure ApplyFill(ARect: TRectF);
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure SetSmoothingMode(ASmoothingMode: SmoothingMode);
    procedure RestoreSmoothingMode;
  public
    constructor Create(const AGraphics: TAdvGraphics); override;
    destructor Destroy; override;
    function GetFillColor: TAdvGraphicsColor; override;
    function CalculateText(AText: string; ARect: TRectF; AWordWrapping: Boolean): TRectF; override;
    function SetTextAngle(ARect: TRectF; {%H-}AAngle: Single): TRectF; override;
    function CreatePath: Pointer; override;
    procedure Render; override;
    procedure PathOpen(APath: Pointer); override;
    procedure PathMoveTo({%H-}APath: Pointer; APoint: TPointF); override;
    procedure PathLineTo(APath: Pointer; APoint: TPointF); override;
    procedure PathClose(APath: Pointer); override;
    procedure ResetClip; override;
    procedure ResetTransform; override;
    procedure ScaleTransform(AX, AY: Single); override;
    procedure RotateTransform(AAngle: Single); override;
    procedure TranslateTransform(AX, AY: Single); override;
    procedure SetTextQuality({%H-}ATextQuality: TAdvGraphicsTextQuality); override;
    procedure SetAntiAliasing(AAntiAliasing: Boolean); override;
    procedure SetShowAcceleratorChar({%H-}AShowAcceleratorChar: Boolean); override;
    procedure SetSize(AWidth, AHeight: Single); override;
    procedure ResetTextAngle({%H-}AAngle: Single); override;
    procedure BeginScene; override;
    procedure EndScene; override;
    procedure StartSpecialPen; override;
    procedure StopSpecialPen; override;
    procedure RestoreState(AState: TAdvGraphicsSaveState); override;
    procedure SaveState({%H-}AState: TAdvGraphicsSaveState); override;
    procedure SetFontSize(ASize: Integer); override;
    procedure SetFontColor(AColor: TAdvGraphicsColor); override;
    procedure SetFontName(AName: string); override;
    procedure SetFont(AFont: TAdvGraphicsFont); override;
    procedure SetFontStyles(AStyle: TFontStyles); override;
    procedure SetFill(AFill: TAdvGraphicsFill); override;
    procedure SetFillKind(AKind: TAdvGraphicsFillKind); override;
    procedure SetFillColor(AColor: TAdvGraphicsColor); override;
    procedure SetStroke(AStroke: TAdvGraphicsStroke); override;
    procedure SetStrokeKind(AKind: TAdvGraphicsStrokeKind); override;
    procedure SetStrokeColor(AColor: TAdvGraphicsColor); override;
    procedure SetStrokeWidth(AWidth: Single); override;
    procedure DrawLine({%H-}AStroke: TAdvGraphicsStroke; AFromPoint: TPointF; AToPoint: TPointF; {%H-}AModifyPointModeFrom: TAdvGraphicsModifyPointMode = gcpmRightDown; {%H-}AModifyPointModeTo: TAdvGraphicsModifyPointMode = gcpmRightDown); override;
    procedure DrawPolygon({%H-}AStroke: TAdvGraphicsStroke; APolygon: TAdvGraphicsPathPolygon); override;
    procedure FillPolygon({%H-}AFill: TAdvGraphicsFill; APolygon: TAdvGraphicsPathPolygon); override;
    procedure DrawPolyline({%H-}AStroke: TAdvGraphicsStroke; APolyline: TAdvGraphicsPathPolygon); override;
    procedure FillPolyline({%H-}AFill: TAdvGraphicsFill; APolyline: TAdvGraphicsPathPolygon); override;
    procedure FillArc({%H-}AFill: TAdvGraphicsFill; {%H-}ACenter, {%H-}ARadius: TPointF; {%H-}AStartAngle, {%H-}ASweepAngle: Single); override;
    procedure DrawArc({%H-}AStroke: TAdvGraphicsStroke; ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single); override;
    procedure FillRect({%H-}AFill: TAdvGraphicsFill; ARect: TRectF; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawRect({%H-}AStroke: TAdvGraphicsStroke; ARect: TRectF; ASides: TAdvGraphicsSides; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure FillRoundRect({%H-}AFill: TAdvGraphicsFill; ARect: TRectF; ARounding: Single; ACorners: TAdvGraphicsCorners; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawRoundRect({%H-}AStroke: TAdvGraphicsStroke; ARect: TRectF; ARounding: Single; ACorners: TAdvGraphicsCorners; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure FillEllipse({%H-}AFill: TAdvGraphicsFill; ARect: TRectF; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawEllipse({%H-}AStroke: TAdvGraphicsStroke; ARect: TRectF; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawBitmap(ABitmap: TAdvDrawBitmap; {%H-}ASrcRect, ADstRect: TRectF; {%H-}AOpacity: Single); override;
    procedure ClipRect(ARect: TRectF); override;
    procedure ClipPath({%H-}APath: TAdvGraphicsPath); override;
    procedure DrawFocusPath({%H-}AStroke: TAdvGraphicsStroke; {%H-}APath: TAdvGraphicsPath; {%H-}AColor: TAdvGraphicsColor); override;
    procedure DrawFocusRectangle({%H-}AStroke: TAdvGraphicsStroke; ARect: TRectF; AColor: TAdvGraphicsColor; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawText(AText: string; ARect: TRectF; AWordWrapping: Boolean; AHorizontalAlign, AVerticalAlign: TAdvGraphicsTextAlign; ATrimming: TAdvGraphicsTextTrimming; {%H-}AAngle: Single); override;
    procedure DrawPath({%H-}AStroke: TAdvGraphicsStroke; APath: TAdvGraphicsPath; APathMode: TAdvGraphicsPathDrawMode = pdmPolygon); override;
    procedure FillPath({%H-}AFill: TAdvGraphicsFill; APath: TAdvGraphicsPath; APathMode: TAdvGraphicsPathDrawMode = pdmPolygon); override;
  end;

function GetNativeContextClass: TAdvGraphicsContextClass;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  SysUtils;

function GetNativeContextClass: TAdvGraphicsContextClass;
begin
  Result := TAdvGraphicsContextWin;
end;

function ConvertToGDIPColor(AColor: TAdvGraphicsColor; AOpacity: Single): Cardinal;
begin
  Result := MakeColor(Round(AOpacity * 255), TAdvGraphics.GetColorRed(AColor), TAdvGraphics.GetColorGreen(AColor), TAdvGraphics.GetColorBlue(AColor));
end;

function CreateGDIPFontFill(AFont: TAdvGraphicsFont): TGPBrush;
begin
  Result := TGPSolidBrush.Create(ConvertToGDIPColor(AFont.Color, 1));
end;

function CreateGDIPFont(AFont: TAdvGraphicsFont): TGPFont;
var
  style: integer;
  fn: string;
  sz: Single;
begin
  fn := AFont.Name;
  if (fn = '') or (UpperCase(fn) = 'DEFAULT') then
    fn := 'Arial';

  sz := AFont.Size;
  if sz = 0 then
    sz := 8;

  style := FontStyleRegular;
  if TFontStyle.fsBold in AFont.Style then
    style := style + FontStyleBold;
  if TFontStyle.fsItalic in AFont.Style then
    style := style + FontStyleItalic;
  if TFontStyle.fsUnderline in AFont.Style then
    style := style + FontStyleUnderline;
  if TFontStyle.fsStrikeOut in AFont.Style then
    style := style + FontStyleStrikeout;

  {$IFDEF FMXLIB}
  Result := TGPFont.Create(PChar(fn), sz, style, UnitPixel);
  {$ENDIF}
  {$IFDEF CMNLIB}
  Result := TGPFont.Create(PChar(fn), sz, style, UnitPoint);
  {$ENDIF}
end;

function CreateGDIPPen(AStroke: TAdvGraphicsStroke): TGPPen;
begin
  Result := TGPPen.Create(ConvertToGDIPColor(AStroke.Color, AStroke.Opacity), AStroke.Width);
  case AStroke.Kind of
    gskSolid: Result.SetDashStyle(DashStyleSolid);
    gskDash: Result.SetDashStyle(DashStyleDash);
    gskDot: Result.SetDashStyle(DashStyleDot);
    gskDashDot: Result.SetDashStyle(DashStyleDashDot);
    gskDashDotDot: Result.SetDashStyle(DashStyleDashDotDot);
  end;
end;

function CreateGPStringFormat(AHAlign: TAdvGraphicsTextAlign; AVAlign: TAdvGraphicsTextAlign; AWrap: Boolean; ATrimming: TAdvGraphicsTextTrimming; AShowAcceleratorChar: Boolean): TGPStringFormat;
var
  flags: integer;
begin
  Result := TGPStringFormat.Create;
  case AHAlign of
    gtaLeading:
      Result.SetAlignment(StringAlignmentNear);
    gtaTrailing:
      Result.SetAlignment(StringAlignmentFar);
    gtaCenter:
      Result.SetAlignment(StringAlignmentCenter);
  end;
  case AValign of
    gtaLeading:
      Result.SetLineAlignment(StringAlignmentNear);
    gtaTrailing:
      Result.SetLineAlignment(StringAlignmentFar);
    gtaCenter:
      Result.SetLineAlignment(StringAlignmentCenter);
  end;

  flags := StringFormatFlagsNoClip;
  if not AWrap then
    flags := flags or StringFormatFlagsNoWrap;

  Result.SetFormatFlags(flags);

  case ATrimming of
    gttCharacter: Result.SetTrimming(StringTrimmingCharacter);
    gttWord: Result.SetTrimming(StringTrimmingWord);
  end;

  if AShowAcceleratorChar then
    Result.SetHotkeyPrefix(HotkeyPrefixShow)
  else
    Result.SetHotkeyPrefix(HotkeyPrefixNone);
end;

function CreateGDIPBrush(AFill: TAdvGraphicsFill; ARect: TGPRectF): TGPBrush;
begin
  Result := nil;
  case AFill.Kind of
    gfkSolid: Result := TGPSolidBrush.Create(ConvertToGDIPColor(AFill.Color, AFill.Opacity));
    gfkTexture, gfkNone: Result := TGPSolidBrush.Create(ConvertToGDIPColor(gcNull, 0));
    gfkGradient:
    begin
      case AFill.Orientation of
        gfoHorizontal: Result := TGPLinearGradientBrush.Create(MakeRect(ARect.X - 1, ARect.Y - 1, ARect.Width + 2, ARect.Height + 2), ConvertToGDIPColor(AFill.Color, AFill.Opacity), ConvertToGDIPColor(AFill.ColorTo, AFill.Opacity), LinearGradientModeHorizontal);
        gfoVertical: Result := TGPLinearGradientBrush.Create(MakeRect(ARect.X - 1, ARect.Y - 1, ARect.Width + 2, ARect.Height + 2), ConvertToGDIPColor(AFill.Color, AFill.Opacity), ConvertToGDIPColor(AFill.ColorTo, AFill.Opacity), LinearGradientModeVertical);
      end;
    end;
  end;
end;

{ TAdvGraphicsContextWin }

procedure TAdvGraphicsContextWin.BeginScene;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  {$IFDEF FMXLIB}
  Canvas.BeginScene;
  Canvas.Clear(gcNull);
  {$ENDIF}
end;

function TAdvGraphicsContextWin.CalculateText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean): TRectF;
var
  b: TGPBrush;
  sf: TGPStringFormat;
  rt, bb: TGPRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(FGPGraphics) then
    Exit;

  b := CreateGDIPFontFill(FFont);
  sf := CreateGPStringFormat(gtaLeading, gtaLeading, AWordWrapping, gttNone, FShowAcceleratorChar);
  try
    rt := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
    FGPGraphics.MeasureString(PChar(AText), Length(AText), FGPFont, rt, sf, bb);
    Result := RectF(bb.X, bb.Y, bb.X + bb.Width, bb.Y + bb.Height);
  finally
    sf.Free;
    b.Free;
  end;
end;

procedure TAdvGraphicsContextWin.ClipPath(APath: TAdvGraphicsPath);
var
  pth: TGPGraphicsPath;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  pth := TGPGraphicsPath(ConvertToPath(APath));
  try
    FGPGraphics.SetClip(pth);
  finally
    pth.Free;
  end;
end;

procedure TAdvGraphicsContextWin.ClipRect(ARect: TRectF);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.SetClip(MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height));
end;

constructor TAdvGraphicsContextWin.Create(const AGraphics: TAdvGraphics);
begin
  inherited;
  FScale := TAdvUtils.GetDPIScale;
  FNeedsRendering := True;
  FContextSize.cx := 0;
  FContextSize.cy := 0;
  FFont := TAdvGraphicsFont.Create;
  FFont.OnChanged := FontChanged;
  FStroke := TAdvGraphicsStroke.Create;
  FStroke.OnChanged := StrokeChanged;
  FFill := TAdvGraphicsFill.Create;
  FFill.OnChanged := FillChanged;
end;

function TAdvGraphicsContextWin.CreatePath: Pointer;
begin
  Result := TGPGraphicsPath.Create;
end;

destructor TAdvGraphicsContextWin.Destroy;
begin
  Render;

  if Assigned(FFont) then
  begin
    FFont.Free;
    FFont := nil;
  end;

  if Assigned(FFill) then
  begin
    FFill.Free;
    FFill := nil;
  end;

  if Assigned(FStroke) then
  begin
    FStroke.Free;
    FStroke := nil;
  end;

  if Assigned(FGPPen) then
  begin
    FGPPen.Free;
    FGPPen := nil;
  end;

  if Assigned(FGPBrush) then
  begin
    FGPBrush.Free;
    FGPBrush := nil;
  end;

  if Assigned(FGPFont) then
  begin
    FGPFont.Free;
    FGPFont := nil;
  end;

  if Assigned(FGPStringFormat) then
  begin
    FGPStringFormat.Free;
    FGPStringFormat := nil;
  end;

  DestroyResources;
  inherited;
end;

procedure TAdvGraphicsContextWin.DestroyResources;
begin
  if Assigned(FGPBitmap) then
  begin
    FGPBitmap.Free;
    FGPBitmap := nil;
  end;

  if Assigned(FGPGraphics) then
  begin
    FGPGraphics.Restore(FSaveGP);
    FGPGraphics.Free;
    FGPGraphics := nil;
  end;

  if Assigned(FBitmap) then
  begin
    {$IFDEF FMXLIB}
    if FMapping then
    begin
      FBitmap.UnMap(FBitmapData);
      FMapping := False;
    end;
    {$ENDIF}
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TAdvGraphicsContextWin.DrawArc(AStroke: TAdvGraphicsStroke;
  ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if FGPPen.GetDashStyle <> DashStyleSolid then
    SetSmoothingMode(SmoothingModeDefault);

  FGPGraphics.DrawArc(FGPPen, ACenter.X - ARadius.X, ACenter.Y - ARadius.Y, ARadius.X * 2, ARadius.Y * 2, AStartAngle, ASweepAngle);

  if FGPPen.GetDashStyle <> DashStyleSolid then
    RestoreSmoothingMode;
end;

procedure TAdvGraphicsContextWin.DrawBitmap(ABitmap: TAdvDrawBitmap;
  ASrcRect, ADstRect: TRectF; AOpacity: Single);
var
  img: TGPImage;

  function CreateGPImage: TGPImage;
  var
    pstm: IStream;
    pcbWrite: Longint;
    {$HINTS OFF}
    {$WARNINGS OFF}
    {$IFNDEF LCLLIB}
    {$IF COMPILERVERSION > 28}
    aSize: LargeUint;
    {$IFEND}
    {$IF COMPILERVERSION <= 28}
    aSize: Largeint;
    {$IFEND}
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IF FPC_FULLVERSION < 30002}
    aSize: Int64;
    {$IFEND}
    {$IF FPC_FULLVERSION >= 30002}
    aSize: QWord;
    {$IFEND}
    {$ENDIF}
    {$HINTS ON}
    {$WARNINGS ON}
    TempImage: TGPImage;
    TempGraphics: TGPGraphics;
    hglobal: THandle;
    DataStream: TMemoryStream;
  begin
    Result := nil;
    if Assigned(ABitmap) then
    begin
      DataStream := TMemoryStream.Create;
      ABitmap.SaveToStream(DataStream);
      try
        DataStream.Position := 0;

        hglobal := GlobalAlloc(GMEM_MOVEABLE, DataStream.Size);
        if (hglobal = 0) then
          raise Exception.Create('Could not allocate memory for image');
        try

          pstm := nil;
          // Create IStream* from global memory
          CreateStreamOnHGlobal(hglobal, TRUE, pstm);
      {$WARNINGS OFF}
          pstm.Write(DataStream.Memory, DataStream.Size, @pcbWrite);
      {$WARNINGS ON}
          pstm.Seek(0, STREAM_SEEK_SET, aSize);

          TempImage := TGPImage.Create(pstm);
          if TempImage.GetType = ImageTypeBitmap then
          begin
            Result := TGPBitmap.Create(TempImage.GetWidth, TempImage.GetHeight,
              PixelFormat32bppARGB);
            TempGraphics := TGPGraphics.Create(Result);
            TempGraphics.DrawImage(TempImage, 0, 0, TempImage.GetWidth,
              TempImage.GetHeight);
            TempGraphics.Free;
            TempImage.Free;
          end
          else
            Result := TempImage;

        finally
          GlobalFree(hglobal);
        end;
      finally
        DataStream.Free;
      end;
    end;
  end;
begin
  if not Assigned(ABitmap) or not Assigned(FGPGraphics) then
    Exit;

  img := CreateGPImage;
  try
    FGPGraphics.DrawImage(img, MakeRect(ADstRect.Left, ADstRect.Top, ADstRect.Width, ADstRect.Height));
  finally
    img.Free;
  end;
end;

procedure TAdvGraphicsContextWin.DrawEllipse(AStroke: TAdvGraphicsStroke;
  ARect: TRectF; AModifyRectMode: TAdvGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  r := RectF(ARect.Left, ARect.Top, ARect.Right - 1, ARect.Bottom - 1);
  if FGPPen.GetDashStyle <> DashStyleSolid then
    SetSmoothingMode(SmoothingModeDefault);

  FGPGraphics.DrawEllipse(FGPPen, r.Left, r.Top, r.Width, r.Height);

  if FGPPen.GetDashStyle <> DashStyleSolid then
    RestoreSmoothingMode;
end;

procedure TAdvGraphicsContextWin.DrawFocusPath(
  AStroke: TAdvGraphicsStroke; APath: TAdvGraphicsPath;
  AColor: TAdvGraphicsColor);
var
  ds: DashStyle;
  c: TGPColor;
  smt: SmoothingMode;
  w: Single;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  smt := FGPGraphics.GetSmoothingMode;
  FGPGraphics.SetSmoothingMode(SmoothingModeDefault);
  ds := FGPPen.GetDashStyle;
  w := FGPPen.GetWidth;
  FGPPen.GetColor(c);
  FGPPen.SetDashStyle(DashStyleDot);
  FGPPen.SetWidth(1);
  FGPPen.SetColor(ConvertToGDIPColor(AColor, 1));
  DrawPath(AStroke, APath);
  FGPPen.SetWidth(w);
  FGPPen.SetDashStyle(ds);
  FGPPen.SetColor(c);
  FGPGraphics.SetSmoothingMode(smt);
end;

procedure TAdvGraphicsContextWin.DrawFocusRectangle(
  AStroke: TAdvGraphicsStroke; ARect: TRectF; AColor: TAdvGraphicsColor;
  AModifyRectMode: TAdvGraphicsModifyRectMode);
var
  ds: DashStyle;
  c: TGPColor;
  smt: SmoothingMode;
  w: Single;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  smt := FGPGraphics.GetSmoothingMode;
  FGPGraphics.SetSmoothingMode(SmoothingModeDefault);
  ds := FGPPen.GetDashStyle;
  w := FGPPen.GetWidth;
  FGPPen.GetColor(c);
  FGPPen.SetDashStyle(DashStyleDot);
  FGPPen.SetWidth(1);
  FGPPen.SetColor(ConvertToGDIPColor(AColor, 1));
  DrawRect(AStroke, ARect, AllSides, AModifyRectMode);
  FGPPen.SetWidth(w);
  FGPPen.SetDashStyle(ds);
  FGPPen.SetColor(c);
  FGPGraphics.SetSmoothingMode(smt);
end;

procedure TAdvGraphicsContextWin.DrawLine(AStroke: TAdvGraphicsStroke;
  AFromPoint, AToPoint: TPointF; AModifyPointModeFrom,
  AModifyPointModeTo: TAdvGraphicsModifyPointMode);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if FGPPen.GetDashStyle <> DashStyleSolid then
    SetSmoothingMode(SmoothingModeDefault);

  FGPGraphics.DrawLine(FGPPen, AFromPoint.X, AFromPoint.Y, AToPoint.X, AToPoint.Y);

  if FGPPen.GetDashStyle <> DashStyleSolid then
    RestoreSmoothingMode;
end;

procedure TAdvGraphicsContextWin.DrawPath(AStroke: TAdvGraphicsStroke;
  APath: TAdvGraphicsPath; APathMode: TAdvGraphicsPathDrawMode);
var
  p: TAdvGraphicsPathPolygon;
begin
  if Assigned(APath) then
  begin
    FActivePath := APath;
    SetLength(p, 0);
    APath.FlattenToPolygon(p);
    case APathMode of
      pdmPolygon: DrawPolygon(AStroke, p);
      pdmPolyline: DrawPolyline(AStroke, p);
    end;
    FActivePath := nil;
  end;
end;

procedure TAdvGraphicsContextWin.DrawPolygon(AStroke: TAdvGraphicsStroke;
  APolygon: TAdvGraphicsPathPolygon);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  if FGPPen.GetDashStyle <> DashStyleSolid then
    SetSmoothingMode(SmoothingModeDefault);

  FGPGraphics.DrawPolygon(FGPPen, PGPPointF(APolygon), Length(APolygon));

  if FGPPen.GetDashStyle <> DashStyleSolid then
    RestoreSmoothingMode;
end;

procedure TAdvGraphicsContextWin.DrawPolyline(AStroke: TAdvGraphicsStroke;
  APolyline: TAdvGraphicsPathPolygon);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  if FGPPen.GetDashStyle <> DashStyleSolid then
    SetSmoothingMode(SmoothingModeDefault);

  FGPGraphics.DrawLines(FGPPen, PGPPointF(APolyline), Length(APolyline));

  if FGPPen.GetDashStyle <> DashStyleSolid then
    RestoreSmoothingMode;
end;

procedure TAdvGraphicsContextWin.DrawRect(AStroke: TAdvGraphicsStroke;
  ARect: TRectF; ASides: TAdvGraphicsSides;
  AModifyRectMode: TAdvGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  r := RectF(ARect.Left, ARect.Top, ARect.Right - 1, ARect.Bottom - 1);
  if gsTop in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Right, r.Top));
  if gsLeft in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Left, r.Bottom));
  if gsBottom in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Bottom), PointF(r.Right, r.Bottom));
  if gsRight in ASides then
    DrawLine(AStroke, PointF(r.Right, r.Top), PointF(r.Right, r.Bottom));
end;

procedure TAdvGraphicsContextWin.DrawRoundRect(
  AStroke: TAdvGraphicsStroke; ARect: TRectF; ARounding: Single;
  ACorners: TAdvGraphicsCorners;
  AModifyRectMode: TAdvGraphicsModifyRectMode);
var
  pth: TAdvGraphicsPath;
  r: TRectF;
  rc: Single;
begin
  r := RectF(ARect.Left, ARect.Top, ARect.Right - 1, ARect.Bottom - 1);
  rc := ARounding;

  pth := TAdvGraphicsPath.Create;
  try
    if gcBottomLeft in ACorners then
    begin
      pth.MoveTo(PointF(r.Left + rc, r.Bottom));
      pth.AddArc(PointF(r.Left + rc, r.Bottom - rc), PointF(rc, rc), -270, 90);
      pth.LineTo(PointF(r.Left, r.Bottom - rc));
    end
    else
    begin
      pth.MoveTo(PointF(r.Left, r.Bottom));
    end;

    if gcTopLeft in ACorners then
    begin
      pth.LineTo(PointF(r.Left, r.Top + rc));
      pth.AddArc(PointF(r.Left + rc, r.Top + rc), PointF(rc, rc), -180, 90);
      pth.LineTo(PointF(r.Left + rc, r.Top));
    end
    else
      pth.LineTo(PointF(r.Left, r.Top));

    if gcTopRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right - rc, r.Top));
      pth.AddArc(PointF(r.Right - rc, r.Top + rc), PointF(rc, rc), -90, 90);
      pth.LineTo(PointF(r.Right, r.Top + rc));
    end
    else
      pth.LineTo(PointF(r.Right, r.Top));

    if gcBottomRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right, r.Bottom - rc));
      pth.AddArc(PointF(r.Right - rc, r.Bottom - rc), PointF(rc, rc), 0, 90);
      pth.LineTo(PointF(r.Right - rc, r.Bottom));
    end
    else
      pth.LineTo(PointF(r.Right, r.Bottom));

    if gcBottomLeft in ACorners then
      pth.LineTo(PointF(r.Left + rc, r.Bottom))
    else
      pth.LineTo(PointF(r.Left, r.Bottom));

    pth.ClosePath;

    DrawPath(FStroke, pth);
  finally
    pth.Free;
  end;
end;

procedure TAdvGraphicsContextWin.DrawText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean; AHorizontalAlign,
  AVerticalAlign: TAdvGraphicsTextAlign;
  ATrimming: TAdvGraphicsTextTrimming; AAngle: Single);
var
  b: TGPBrush;
  sf: TGPStringFormat;
  rt: TGPRectF;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  b := CreateGDIPFontFill(FFont);
  sf := CreateGPStringFormat(AHorizontalAlign, AVerticalAlign, AWordWrapping, ATrimming, FShowAcceleratorChar);
  try
    rt := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
    FGPGraphics.DrawString(AText, FGPFont, rt, sf, b);
  finally
    sf.Free;
    b.Free;
  end;
end;

procedure TAdvGraphicsContextWin.EndScene;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  Render;
  {$IFDEF FMXLIB}
  Canvas.EndScene;
  {$ENDIF}
end;

procedure TAdvGraphicsContextWin.FillArc(AFill: TAdvGraphicsFill; ACenter,
  ARadius: TPointF; AStartAngle, ASweepAngle: Single);
var
  p: TGPGraphicsPath;
  r: TGPRectF;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  p := TGPGraphicsPath.Create;
  try
    p.AddArc(ACenter.X - ARadius.X, ACenter.Y - ARadius.Y, ARadius.X * 2, ARadius.Y * 2, AStartAngle, ASweepAngle);
    p.GetBounds(r);
    ApplyFill(RectF(r.X, r.Y, r.X + r.Width, r.Y + r.Height));
    FGPGraphics.FillPath(FGPBrush, p);
  finally
    p.Free;
  end;
end;

procedure TAdvGraphicsContextWin.FillChanged(Sender: TObject);
begin
  ApplyFill(RectF(0, 0, FContextSize.Width, FContextSize.Height));
end;

procedure TAdvGraphicsContextWin.FillEllipse(AFill: TAdvGraphicsFill;
  ARect: TRectF; AModifyRectMode: TAdvGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  ApplyFill(r);
  FGPGraphics.FillEllipse(FGPBrush, r.Left, r.Top, r.Width, r.Height);
end;

procedure TAdvGraphicsContextWin.FillPath(AFill: TAdvGraphicsFill;
  APath: TAdvGraphicsPath; APathMode: TAdvGraphicsPathDrawMode);
var
  p: TAdvGraphicsPathPolygon;
begin
  if Assigned(APath) then
  begin
    FActivePath := APath;
    SetLength(p, 0);
    APath.FlattenToPolygon(p);
    case APathMode of
      pdmPolygon: FillPolygon(AFill, p);
      pdmPolyline: FillPolyline(AFill, p);
    end;
    FActivePath := nil;
  end;
end;

procedure TAdvGraphicsContextWin.FillPolygon(AFill: TAdvGraphicsFill;
  APolygon: TAdvGraphicsPathPolygon);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  if Assigned(FActivePath) then
    ApplyFill(FActivePath.GetBounds);
  FGPGraphics.FillPolygon(FGPBrush, PGPPointF(APolygon), Length(APolygon));
end;

procedure TAdvGraphicsContextWin.FillPolyline(AFill: TAdvGraphicsFill;
  APolyline: TAdvGraphicsPathPolygon);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  if Assigned(FActivePath) then
    ApplyFill(FActivePath.GetBounds);
  FGPGraphics.FillPolygon(FGPBrush, PGPPointF(APolyline), Length(APolyline));
end;

procedure TAdvGraphicsContextWin.FillRect(AFill: TAdvGraphicsFill;
  ARect: TRectF; AModifyRectMode: TAdvGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  ApplyFill(r);
  FGPGraphics.FillRectangle(FGPBrush, r.Left, r.Top, r.Width, r.Height);
end;

procedure TAdvGraphicsContextWin.FillRoundRect(AFill: TAdvGraphicsFill;
  ARect: TRectF; ARounding: Single; ACorners: TAdvGraphicsCorners;
  AModifyRectMode: TAdvGraphicsModifyRectMode);
var
  pth: TAdvGraphicsPath;
  r: TRectF;
  rc: Single;
begin
  r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  rc := ARounding;

  pth := TAdvGraphicsPath.Create;
  try
    if gcBottomLeft in ACorners then
    begin
      pth.MoveTo(PointF(r.Left + rc, r.Bottom));
      pth.AddArc(PointF(r.Left + rc, r.Bottom - rc), PointF(rc, rc), -270, 90);
      pth.LineTo(PointF(r.Left, r.Bottom - rc));
    end
    else
    begin
      pth.MoveTo(PointF(r.Left, r.Bottom));
    end;

    if gcTopLeft in ACorners then
    begin
      pth.LineTo(PointF(r.Left, r.Top + rc));
      pth.AddArc(PointF(r.Left + rc, r.Top + rc), PointF(rc, rc), -180, 90);
      pth.LineTo(PointF(r.Left + rc, r.Top));
    end
    else
      pth.LineTo(PointF(r.Left, r.Top));

    if gcTopRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right - rc, r.Top));
      pth.AddArc(PointF(r.Right - rc, r.Top + rc), PointF(rc, rc), -90, 90);
      pth.LineTo(PointF(r.Right, r.Top + rc));
    end
    else
      pth.LineTo(PointF(r.Right, r.Top));

    if gcBottomRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right, r.Bottom - rc));
      pth.AddArc(PointF(r.Right - rc, r.Bottom - rc), PointF(rc, rc), 0, 90);
      pth.LineTo(PointF(r.Right - rc, r.Bottom));
    end
    else
      pth.LineTo(PointF(r.Right, r.Bottom));

    if gcBottomLeft in ACorners then
      pth.LineTo(PointF(r.Left + rc, r.Bottom))
    else
      pth.LineTo(PointF(r.Left, r.Bottom));

    pth.ClosePath;

    FillPath(FFill, pth);
  finally
    pth.Free;
  end;
end;

procedure TAdvGraphicsContextWin.ApplyFill(ARect: TRectF);
begin
  if Assigned(FGPBrush) then
  begin
    FGPBrush.Free;
    FGPBrush := nil;
  end;

  FGPBrush := CreateGDIPBrush(FFill, MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height));
end;

procedure TAdvGraphicsContextWin.FontChanged(Sender: TObject);
begin
  if Assigned(FGPFont) then
  begin
    FGPFont.Free;
    FGPFont := nil;
  end;

  FGPFont := CreateGDIPFont(FFont);
end;

function TAdvGraphicsContextWin.GetFillColor: TAdvGraphicsColor;
begin
  Result := Graphics.Fill.Color;
end;

function TAdvGraphicsContextWin.GetNativeCanvas: Pointer;
begin
  Result := FGPGraphics;
end;

procedure TAdvGraphicsContextWin.PathClose(APath: Pointer);
begin
  TGPGraphicsPath(APath).CloseFigure;
end;

procedure TAdvGraphicsContextWin.PathLineTo(APath: Pointer; APoint: TPointF);
begin
  TGPGraphicsPath(APath).AddLine(FMovePoint.X, FMovePoint.Y, APoint.X, APoint.Y);
  FMovePoint := APoint;
end;

procedure TAdvGraphicsContextWin.PathMoveTo(APath: Pointer; APoint: TPointF);
begin
  FMovePoint := APoint;
end;

procedure TAdvGraphicsContextWin.PathOpen(APath: Pointer);
begin
  TGPGraphicsPath(APath).StartFigure;
end;

procedure TAdvGraphicsContextWin.Render;
begin
  if not FNeedsRendering then
    Exit;

  {$IFDEF FMXLIB}
  if Assigned(FGPGraphics) then
  begin
    if FMapping then
    begin
      FBitmap.Unmap(FBitmapData);
      FMapping := False;
    end;

    Canvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), RectF(0, 0, FContextSize.Width, FContextSize.Height), 1)
  end;
  {$ENDIF}

  FNeedsRendering := False;
  DestroyResources;
end;

procedure TAdvGraphicsContextWin.ResetClip;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.ResetClip;
end;

procedure TAdvGraphicsContextWin.ResetTextAngle(AAngle: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  if (AAngle <> 0) and Assigned(FTextMatrix) then
  begin
    FGPGraphics.SetTransform(FTextMatrix);
    FTextMatrix.Free;
    FTextMatrix := nil;
  end;
end;

procedure TAdvGraphicsContextWin.ResetTransform;
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.Restore(FSaveGP);
  FSaveGP := FGPGraphics.Save;
end;

procedure TAdvGraphicsContextWin.RestoreState(
  AState: TAdvGraphicsSaveState);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.Restore(AState.CustomSaveDC);
end;

procedure TAdvGraphicsContextWin.RotateTransform(AAngle: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.RotateTransform(AAngle, MatrixOrderPrepend);
end;

procedure TAdvGraphicsContextWin.SaveState(AState: TAdvGraphicsSaveState);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  AState.CustomSaveDC := FGPGraphics.Save;
end;

procedure TAdvGraphicsContextWin.ScaleTransform(AX, AY: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.ScaleTransform(AX, AY, MatrixOrderPrepend);
end;

procedure TAdvGraphicsContextWin.SetTextQuality(ATextQuality: TAdvGraphicsTextQuality);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.Restore(FSaveGP);
  case ATextQuality of
    gtqDefault: FGPGraphics.SetTextRenderingHint(TextRenderingHintSystemDefault);
    gtqAntiAliasing: FGPGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    gtqClearType: FGPGraphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  end;
  FSaveGP := FGPGraphics.Save;
end;

procedure TAdvGraphicsContextWin.SetSmoothingMode(ASmoothingMode: SmoothingMode);
begin
  FSmoothingMode := FGPGraphics.GetSmoothingMode;
  FGPGraphics.SetSmoothingMode(ASmoothingMode)
end;

procedure TAdvGraphicsContextWin.RestoreSmoothingMode;
begin
  FGPGraphics.SetSmoothingMode(FSmoothingMode);
end;

procedure TAdvGraphicsContextWin.SetSize(AWidth, AHeight: Single);
begin
  FContextSize.cx := AWidth;
  FContextSize.cy := AHeight;

  DestroyResources;

  {$IFDEF FMXLIB}
  FBitmap := TBitmap.Create(Round(AWidth * FScale), Round(AHeight * FScale));
  FBitmap.BitmapScale := FScale;
  if FBitmap.Map(TMapAccess.Write, FBitmapData) then
  begin
    FMapping := True;
    FGPBitmap := TGPBitmap.Create(FBitmap.Width, FBitmap.Height, FBitmapData.Pitch, PixelFormat32bppPARGB, FBitmapData.Data);
    FGPGraphics := TGPGraphics.Create(FGPBitmap);
    FGPGraphics.Clear(ConvertToGDIPColor(gcNull, 0));
    ScaleTransform(FScale, FScale);
  {$ENDIF}
  {$IFDEF CMNLIB}
  begin
    FGPGraphics := TGPGraphics.Create(Canvas.Handle);
  {$ENDIF}
  end;

  FSaveGP := FGPGraphics.Save;
end;

procedure TAdvGraphicsContextWin.SetShowAcceleratorChar(AShowAcceleratorChar: Boolean);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FShowAcceleratorChar := AShowAcceleratorChar;
end;

procedure TAdvGraphicsContextWin.SetAntiAliasing(AAntiAliasing: Boolean);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.Restore(FSaveGP);
  if AAntiAliasing then
    FGPGraphics.SetSmoothingMode(SmoothingModeAntiAlias)
  else
    FGPGraphics.SetSmoothingMode(SmoothingModeDefault);
  FSaveGP := FGPGraphics.Save;
end;

procedure TAdvGraphicsContextWin.SetFill(AFill: TAdvGraphicsFill);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFill.Assign(AFill);
end;

procedure TAdvGraphicsContextWin.SetFillColor(AColor: TAdvGraphicsColor);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFill.Color := AColor;
end;

procedure TAdvGraphicsContextWin.SetFillKind(AKind: TAdvGraphicsFillKind);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFill.Kind := AKind;
end;

procedure TAdvGraphicsContextWin.SetFont(AFont: TAdvGraphicsFont);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFont.Assign(AFont);
end;

procedure TAdvGraphicsContextWin.SetFontColor(AColor: TAdvGraphicsColor);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFont.Color := AColor;
end;

procedure TAdvGraphicsContextWin.SetFontName(AName: string);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFont.Name := AName;
end;

procedure TAdvGraphicsContextWin.SetFontSize(ASize: Integer);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFont.Size := ASize;
end;

procedure TAdvGraphicsContextWin.SetFontStyles(AStyle: TFontStyles);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FFont.Style := AStyle;
end;

procedure TAdvGraphicsContextWin.SetStroke(AStroke: TAdvGraphicsStroke);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FStroke.Assign(AStroke);
end;

procedure TAdvGraphicsContextWin.SetStrokeColor(
  AColor: TAdvGraphicsColor);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FStroke.Color := AColor;
end;

procedure TAdvGraphicsContextWin.SetStrokeKind(
  AKind: TAdvGraphicsStrokeKind);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FStroke.Kind := AKind;
end;

procedure TAdvGraphicsContextWin.SetStrokeWidth(AWidth: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FStroke.Width := AWidth;
end;

function TAdvGraphicsContextWin.SetTextAngle(ARect: TRectF;
  AAngle: Single): TRectF;
var
  ar: Single;
  cx: TPointF;
  rm: TGPMatrix;
begin
  Result := ARect;
  if not Assigned(FGPGraphics) then
    Exit;

  if AAngle <> 0 then
  begin
    ar := AAngle;
    cx.X := Result.Left + Result.Width / 2;
    cx.Y := Result.Top + Result.Height / 2;
    FTextMatrix := TGPMatrix.Create;
    FGPGraphics.GetTransform(FTextMatrix);
    rm := TGPMatrix.Create;
    rm.Translate(cx.X, cx.Y);
    rm.Rotate(ar);
    FGPGraphics.MultiplyTransform(rm, MatrixOrderPrepend);
    rm.Free;
    if Result.Width < Result.Height then
      Result := RectF(-Result.Height / 2, -Result.Width / 2, Result.Height / 2, Result.Width / 2)
    else
      Result := RectF(-Result.Width / 2, -Result.Height / 2, Result.Width / 2, Result.Height / 2);
  end;
end;

procedure TAdvGraphicsContextWin.StartSpecialPen;
begin
  if not Assigned(FGPGraphics) then
    Exit;
end;

procedure TAdvGraphicsContextWin.StopSpecialPen;
begin
  if not Assigned(FGPGraphics) then
    Exit;
end;

procedure TAdvGraphicsContextWin.StrokeChanged(Sender: TObject);
begin
  if Assigned(FGPPen) then
  begin
    FGPPen.Free;
    FGPPen := nil;
  end;

  FGPPen := CreateGDIPPen(FStroke);
end;

procedure TAdvGraphicsContextWin.TranslateTransform(AX, AY: Single);
begin
  if not Assigned(FGPGraphics) then
    Exit;

  FGPGraphics.TranslateTransform(AX, AY, MatrixOrderPrepend);
end;

{$ENDIF}

end.
