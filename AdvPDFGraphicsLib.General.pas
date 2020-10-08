{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2016                                        }
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

unit AdvPDFGraphicsLib.General;

{$I TMSDEFS.INC}

interface

procedure RegisterPDFGraphicsLibGeneralService;
procedure UnRegisterPDFGraphicsLibGeneralService;

implementation

uses
  Classes, AdvPDFGraphicsLib, SysUtils, AdvTypes, Graphics, AdvPDFLib
  ,AdvPDFRichTextLib, AdvPDFCoreLibBase, AdvPDFGraphicsLibHTMLEngine, AdvUtils,
  PictureContainer, AdvGraphicsTypes
  {$IFNDEF LCLLIB}
  ,Types
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  TAdvGeneralPDFGraphicsLibService = class(TAdvPDFGraphicsLibFactoryService)
  protected
    function DoCreatePDFGraphicsLib: TObject; override;
  end;

  TAdvGeneralPDFGraphicsLib = class(TAdvPDFGraphicsLibBase, IAdvCustomPDFGraphicsLib, IAdvCustomPDFInitializationLib, IAdvCustomPDFGraphicsExLib)
  private
    FPathRect: TRectF;
    FOutput: TAdvPDFGraphicsLibOutputWriter;
    FTextRect: TRectF;
    FPDFRichTextLib: IAdvCustomPDFRichTextLib;
    FPDFLib: IAdvCustomPDFLib;
    FPageWidth: Single;
    FPageHeight: Single;
    FOnNotifyNewPage: TNotifyEvent;
    function GetOnNotifyNewPage: TNotifyEvent;
    procedure SetOnNotifyNewPage(const Value: TNotifyEvent);
  protected
    procedure SetFill(const Value: TAdvPDFGraphicsFill);
    procedure SetStroke(const Value: TAdvPDFGraphicsStroke);
    procedure SetAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetURLFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetLineBreakMode(const Value: TAdvPDFGraphicsLibLineBreakMode);
    procedure CreateClasses; override;
    procedure InitializeAppearance; override;
    procedure UpdateFill; override;
    procedure UpdateStroke; override;
    procedure UpdateFont; override;
    procedure UpdatePathRect(APoint: TPointF);
    procedure SetCanvas(ACanvas: Pointer);
    procedure SetPDFLib(APDFLib: IInterface);
    procedure SetPageWidth(APageWidth: Single);
    procedure SetPageHeight(APageHeight: Single);
    procedure DrawAddShadow({%H-}Offset: TPointF; {%H-}Blur: Single); overload;
    procedure DrawAddShadow({%H-}Offset: TPointF; {%H-}Blur: Single; {%H-}Color: TAdvGraphicsColor); overload;
    procedure NotifyNewPage;
    procedure DrawPathEndRadialGradient({%H-}StartCenter, {%H-}EndCenter: TPointF; {%H-}StartRadius, {%H-}EndRadius: Single);
    procedure SetPDFRichTextLib(const Value: IAdvCustomPDFRichTextLib);
    procedure DrawRectangle(Rect: TRectF);
    procedure DrawRoundedRectangle({%H-}Rect: TRectF; {%H-}Rounding: Single);
    procedure DrawEllipse({%H-}Rect: TRectF);
    procedure DrawLine(StartPoint, EndPoint: TPointF);
    procedure AddURL(AText: UnicodeString; AURL: UnicodeString; ARect: TRectF);
    procedure DrawSaveState;
    procedure DrawClear({%H-}Rect: TRectF); overload;
    procedure DrawRestoreState;
    procedure DrawPathBegin;
    procedure DrawPathBeginClip;
    procedure DrawPathEndClip;
    procedure DrawPathEndLinearGradient({%H-}StartPoint, {%H-}EndPoint: TPointF);
    procedure DrawPathMoveToPoint(Point: TPointF);
    procedure DrawPathAddLineToPoint(Point: TPointF);
    procedure DrawPathAddArc({%H-}CenterPoint: TPointF; {%H-}Radius: Single; {%H-}StartAngle, {%H-}EndAngle: Single; {%H-}Clockwise: Boolean = False);
    procedure DrawPathAddArcToPoint({%H-}FirstPoint, {%H-}SecondPoint: TPointF; {%H-}Radius: Single);
    procedure DrawPathAddRectangle({%H-}Rect: TRectF);
    procedure DrawPathAddEllipse({%H-}Rect: TRectF);
    procedure DrawPathAddCurveToPoint({%H-}FirstControlPoint, {%H-}SecondControlPoint, {%H-}EndPoint: TPointF);
    procedure DrawPathAddLines({%H-}Points: TAdvPDFGraphicsLibPointArray);
    procedure DrawPathAddQuadCurveToPoint({%H-}ControlPoint: TPointF; {%H-}EndPoint: TPointF);
    procedure DrawPathClose;
    procedure DrawPathEnd(DrawingMode: TAdvPDFGraphicsLibPathDrawingMode = dmPathFillStroke);
    function CalculateHTMLText(Text: UnicodeString; Scale: Single = 1.0): TRectF; overload;
    function CalculateHTMLText(Text: UnicodeString; Rect: TRectF; Scale: Single = 1.0): TRectF; overload;
    function CalculateText(Text: UnicodeString): TRectF; overload;
    function CalculateText(Text: UnicodeString; Rect: TRectF): TRectF; overload;
    function CalculateTextOverflow(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0): Integer; overload;
    function CalculateTextOverflow(Text: UnicodeString; Rects: TAdvPDFGraphicsLibRectArray; Padding: Single = 5.0): Integer; overload;
    function DrawImageWithName(ABitmapName: string; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImage(ABitmap: TAdvBitmap; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageFromFile(AFileName: String; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageWithName(ABitmapName: string; ABackgroundColor: TAdvGraphicsColor; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImage(ABitmap: TAdvBitmap; ABackgroundColor: TAdvGraphicsColor; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageFromFile(AFileName: String; ABackgroundColor: TAdvGraphicsColor; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageWithName(ABitmapName: string; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImage(ABitmap: TAdvBitmap; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageFromFile(AFileName: String; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageWithName(ABitmapName: string; ABackgroundColor: TAdvGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImage(ABitmap: TAdvBitmap; ABackgroundColor: TAdvGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageFromFile(AFileName: String; ABackgroundColor: TAdvGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawHTMLText(Text: UnicodeString; Point: TPointF; AScale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    function DrawHTMLText(Text: UnicodeString; Rect: TRectF; Paging: Boolean = False; AScale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    function DrawText(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawText(Text: UnicodeString; Rects: TAdvPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawText(Text: UnicodeString; Point: TPointF; Calculate: Boolean = False): TRectF; overload;
    function DrawText(Text: UnicodeString; Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function GetPageWidth: Single;
    function GetPageHeight: Single;
    function DrawRichText(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawRichText(Rects: TAdvPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawRichText(Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function FindNextWord(AText: UnicodeString; var APos: Integer): UnicodeString;
    function MeasureTextWidth(AText: UnicodeString): Single;
    function GetCharacterCount(AText: UnicodeString; AWidth: single): Integer;
    function IsUnicodeText(AText: UnicodeString): Boolean;
    function RichText: IAdvCustomPDFRichTextLib;
    function GetTextRect: TRectF;
    function GetFill: TAdvPDFGraphicsFill;
    function GetStroke: TAdvPDFGraphicsStroke;
    function GetAlignment: TAdvGraphicsTextAlign;
    function GetFont: TAdvPDFGraphicsLibFont;
    function GetURLFont: TAdvPDFGraphicsLibFont;
    function GetPictureContainer: TPictureContainer;
    function GetLineBreakMode: TAdvPDFGraphicsLibLineBreakMode;
    property OnNotifyNewPage: TNotifyEvent read GetOnNotifyNewPage write SetOnNotifyNewPage;
  public
    destructor Destroy; override;
  end;

var
  PDFGraphicsLibService: IAdvPDFGraphicsLibGeneralService;

procedure RegisterPDFGraphicsLibGeneralService;
begin
  if not TAdvPDFPlatformServices.Current.SupportsPlatformService(IAdvPDFGraphicsLibGeneralService, IInterface(PDFGraphicsLibService)) then
  begin
    PDFGraphicsLibService := TAdvGeneralPDFGraphicsLibService.Create;
    TAdvPDFPlatformServices.Current.AddPlatformService(IAdvPDFGraphicsLibGeneralService, PDFGraphicsLibService);
  end;
end;

procedure UnregisterPDFGraphicsLibGeneralService;
begin
  TAdvPDFPlatformServices.Current.RemovePlatformService(IAdvPDFGraphicsLibGeneralService);
end;

{ TAdvGeneralPDFGraphicsLibService }

function TAdvGeneralPDFGraphicsLibService.DoCreatePDFGraphicsLib: TObject;
begin
  Result := TAdvGeneralPDFGraphicsLib.Create;
end;

function TAdvGeneralPDFGraphicsLib.DrawText(Text: UnicodeString; Point: TPointF; Calculate: Boolean = False): TRectF;
var
  pt: TPointF;
  tw: Single;
  th: Single;
  c: TAdvGraphicsColor;
  lw: Single;
//  ar: Single;
//  cx: TPointF;
//  m: TAdvGraphicsMatrix;
begin
  if (Pos(#13, Text) > 0) or (Pos(#10, Text) > 0) then
    Result := DrawText(Text, RectF(Point.X, Point.Y, Point.X + 10000, Point.Y + 10000), Calculate)
  else
  begin
    if Assigned(FOutput) then
    begin
      if IsUnicodeText(Text) then
        FOutput.NotifyUnicode(Text)
      else
        FOutput.NotifyText(Text);

      tw := MeasureTextWidth(Text);
      th := FOutput.FontSize;

      if not Calculate then
      begin
        pt.X := Point.X;
        pt.Y := FPageHeight - Point.Y - th;
        FOutput.BeginText;
        FOutput.WriteFont;
        FOutput.WriteFontColor;
//        ar := DegToRad(90);
//        cx.X := pt.X + tw / 2;
//        cx.Y := pt.Y + th / 2;
//        m := TAdvGraphicsMatrix.CreateRotation(ar) * TAdvGraphicsMatrix.CreateTranslation(cx.X, cx.Y);
//        FOutput.WriteTextMatrix(m.m11, m.m12, m.m21, m.m22, m.m31 + (th / 2), m.m32 - (tw / 2));
        FOutput.MoveTextTo(pt);
        FOutput.AddText(Text);
        FOutput.EndText;
        c := Stroke.Color;
        lw := Stroke.Width;
        Stroke.Color := Font.Color;
        Stroke.Width := Font.Size * PDFULLWFACTOR;
        if TFontStyle.fsUnderline in Font.Style then
          DrawLine(PointF(Point.X, Point.Y + th * PDFLHFACTOR * PDFULFACTOR), PointF(Point.X + tw, Point.Y + th * PDFLHFACTOR * PDFULFACTOR));

        if TFontStyle.fsStrikeOut in Font.Style then
          DrawLine(PointF(Point.X, Point.Y + th * PDFLHFACTOR * PDFSTFACTOR), PointF(Point.X + tw, Point.Y + th * PDFLHFACTOR * PDFSTFACTOR));
        Stroke.Color := c;
        Stroke.Width := lw;
      end;

      FTextRect := RectF(Point.X, Point.Y, Point.X + tw, Point.Y + th * PDFLHFACTOR);
      Result := FTextRect;
    end;
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathBeginClip;
begin
  DrawSaveState;
  if Assigned(FOutput) then
    FOutput.WriteString('W' + PDFLB);
end;

procedure TAdvGeneralPDFGraphicsLib.DrawAddShadow(Offset: TPointF; Blur: Single);
begin
end;

procedure TAdvGeneralPDFGraphicsLib.AddURL(AText, AURL: UnicodeString;
  ARect: TRectF);
var
  tr, trr: TRectF;
  fts: TAdvPDFGraphicsLibFont;
begin
  if Assigned(FOutput) then
  begin
    fts := TAdvPDFGraphicsLibFont.Create;
    try
      fts.Assign(Font);
      Font.Assign(URLFont);
      tr := DrawText(AText, ARect);
      case Alignment of
        gtaLeading: trr := RectF(ARect.Left, FPageHeight - ARect.Top - tr.Height, ARect.Left + tr.Width, FPageHeight - ARect.Top);
        gtaCenter: trr := RectF(ARect.Left + (ARect.Width - tr.Width) / 2, FPageHeight - ARect.Top - tr.Height, ARect.Left + (ARect.Width - tr.Width) / 2 + tr.Width, FPageHeight - ARect.Top);
        gtaTrailing: trr := RectF(ARect.Right - tr.Width, FPageHeight - ARect.Top - tr.Height, ARect.Right, FPageHeight - ARect.Top);
      end;
      FOutput.NotifyURL(trr, AURL);
      Font.Assign(fts);
    finally
      fts.Free;
    end;
  end;
end;

function TAdvGeneralPDFGraphicsLib.CalculateHTMLText(Text: UnicodeString;
  Rect: TRectF; Scale: Single): TRectF;
begin
  Result := DrawHTMLText(Text, Rect, False, Scale, True);
end;

function TAdvGeneralPDFGraphicsLib.CalculateHTMLText(Text: UnicodeString;
  Scale: Single): TRectF;
begin
  Result := DrawHTMLText(Text, PointF(0, 0), Scale, True);
end;

function TAdvGeneralPDFGraphicsLib.CalculateText(
  Text: UnicodeString): TRectF;
begin
  Result := DrawText(Text, PointF(0, 0), True);
end;

function TAdvGeneralPDFGraphicsLib.CalculateText(Text: UnicodeString;
  Rect: TRectF): TRectF;
begin
  Result := DrawText(Text, Rect, True);
end;

function TAdvGeneralPDFGraphicsLib.CalculateTextOverflow(Text: UnicodeString;
  Rects: TAdvPDFGraphicsLibRectArray; Padding: Single): Integer;
begin
  Result := DrawText(Text, Rects, Padding, True);
end;

function TAdvGeneralPDFGraphicsLib.CalculateTextOverflow(Text: UnicodeString;
  Rect: TRectF; Columns: Integer; Padding: Single): Integer;
begin
  Result := DrawText(Text, Rect, Columns, Padding, True);
end;

procedure TAdvGeneralPDFGraphicsLib.CreateClasses;
begin
  inherited;
end;

destructor TAdvGeneralPDFGraphicsLib.Destroy;
begin
  FOutput := nil;
  inherited;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawAddShadow(Offset: TPointF; Blur: Single;
  Color: TAdvGraphicsColor);
begin
end;

procedure TAdvGeneralPDFGraphicsLib.DrawClear(Rect: TRectF);
begin
end;

procedure TAdvGeneralPDFGraphicsLib.DrawEllipse(Rect: TRectF);
begin
  DrawSaveState;
  DrawPathBegin;
  DrawPathMoveToPoint(PointF(Rect.Left, Rect.Top + Rect.Height / 2));
  DrawPathAddEllipse(Rect);
  DrawPathClose;

  if (Fill.Color <> gcNull) and (Stroke.Color <> gcNull) and (Fill.Kind <> gfkNone) and (Stroke.Kind <> gskNone) then
    DrawPathEnd(dmPathFillStroke)
  else if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
    DrawPathEnd(dmPathFill)
  else if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
    DrawPathEnd(dmPathStroke);

  DrawRestoreState;
end;

function TAdvGeneralPDFGraphicsLib.DrawHTMLText(Text: UnicodeString; Rect: TRectF; Paging: Boolean = False; AScale: Single = 1.0; Calculate: Boolean = False): TRectF;
begin
  FTextRect := TAdvPDFGraphicsLibHTMLEngine.DrawHTMLText(FPDFLib, Text, Rect, PictureContainer, Paging, AScale, Calculate);
  Result := FTextRect;
end;

function TAdvGeneralPDFGraphicsLib.DrawHTMLText(Text: UnicodeString; Point: TPointF; AScale: Single = 1.0; Calculate: Boolean = False): TRectF;
begin
  FTextRect := TAdvPDFGraphicsLibHTMLEngine.DrawHTMLText(FPDFLib, Text, Point, PictureContainer, AScale, Calculate);
  Result := FTextRect;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathEndClip;
begin
  if Assigned(FOutput) then
    FOutput.WriteString('W*' + PDFLB);

  DrawRestoreState;
end;

function TAdvGeneralPDFGraphicsLib.DrawImageFromFile(AFileName: String; ABackgroundColor: TAdvGraphicsColor; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF;
var
  bmp: TAdvBitmap;
begin
  bmp := TAdvBitmap.Create;
  try
    bmp.LoadFromFile(AFileName);
    Result := DrawImage(bmp, ABackgroundColor, Point, ImageType, Quality);
  finally
    bmp.Free;
  end;
end;

function TAdvGeneralPDFGraphicsLib.DrawImage(ABitmap: TAdvBitmap; ABackgroundColor: TAdvGraphicsColor; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType; Quality: Single): TRectF;
var
  br: String;
  pt: TPointF;
  sz: TSizeF;
begin
  if Assigned(FOutput) and Assigned(ABitmap) then
  begin
    br := '';
    FOutput.NotifyBitmap(ABitmap, ImageType, Quality, ABackgroundColor, br);
    pt.X := Point.X;
    pt.Y := FPageHeight - Point.Y - ABitmap.Height;
    sz.cx := ABitmap.Width;
    sz.cy := ABitmap.Height;
    DrawSaveState;
    FOutput.WriteMatrix(pt, sz);
    FOutput.WriteString(br + ' Do' + PDFLB);
    DrawRestoreState;
    Result := RectF(Point.X, Point.Y, Point.X + ABitmap.Width, Point.Y + ABitmap.Height);
  end;
end;

function TAdvGeneralPDFGraphicsLib.DrawImage(ABitmap: TAdvBitmap; ABackgroundColor: TAdvGraphicsColor; Rect: TRectF; Stretch, AspectRatio: Boolean; ImageType: TAdvPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
var
  br: String;
  pt: TPointF;
  sz: TSizeF;
  x, y, w, h: Single;
  rdest: TRectF;
begin
  if Assigned(FOutput) and Assigned(ABitmap) then
  begin
    br := '';
    FOutput.NotifyBitmap(ABitmap, ImageType, Quality, ABackgroundColor, br);
    x := 0;
    y := 0;
    w := 0;
    h := 0;
    GetAspectSize(x, y, w, h, ABitmap.Width, ABitmap.height, Rect.Width, Rect.Height, AspectRatio, Stretch, False);

    if Center then
    begin
      x := Rect.Left + (Rect.Width - w) / 2;
      y := Rect.Top + (Rect.Height - h) / 2;
    end
    else
    begin
      x := Rect.Left;
      y := Rect.Top;
    end;

    rdest := RectF(x, y, x + w, y + h);

    pt.X := rdest.Left;
    pt.Y := FPageHeight - rdest.Top - rdest.Height;
    sz.cx := rdest.Width;
    sz.cy := rdest.Height;
    DrawSaveState;
    FOutput.WriteMatrix(pt, sz);
    FOutput.WriteString(br + ' Do' + PDFLB);
    DrawRestoreState;

    Result := rdest;
  end;
end;

function TAdvGeneralPDFGraphicsLib.DrawImage(ABitmap: TAdvBitmap;
  Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImage(ABitmap, gcWhite, Point, ImageType, Quality);
end;

function TAdvGeneralPDFGraphicsLib.DrawImage(ABitmap: TAdvBitmap;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TAdvPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImage(ABitmap, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TAdvGeneralPDFGraphicsLib.DrawImageFromFile(AFileName: String;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TAdvPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImageFromFile(AFileName, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TAdvGeneralPDFGraphicsLib.DrawImageFromFile(AFileName: String;
  Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImageFromFile(AFileName, gcWhite, Point, ImageType, Quality);
end;

function TAdvGeneralPDFGraphicsLib.DrawImageFromFile(AFileName: String; ABackgroundColor: TAdvGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF;
var
  bmp: TAdvBitmap;
begin
  bmp := TAdvBitmap.Create;
  try
    bmp.LoadFromFile(AFileName);
    Result := DrawImage(bmp, ABackgroundColor, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
  finally
    bmp.Free;
  end;
end;

function TAdvGeneralPDFGraphicsLib.DrawImageWithName(ABitmapName: string; ABackgroundColor: TAdvGraphicsColor; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF;
var
  bmp: TAdvBitmap;
begin
  if not Assigned(PictureContainer) then
    Exit;

  bmp := PictureContainer.FindBitmap(ABitmapName);
  if Assigned(bmp) and not IsBitmapEmpty(bmp) then
    Result := DrawImage(bmp, ABackgroundColor, Point, ImageType, Quality);
end;

function TAdvGeneralPDFGraphicsLib.DrawImageWithName(ABitmapName: string; ABackgroundColor: TAdvGraphicsColor; Rect: TRectF; Stretch, AspectRatio: Boolean; ImageType: TAdvPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
var
  bmp: TAdvBitmap;
begin
  if not Assigned(PictureContainer) then
    Exit;

  bmp := PictureContainer.FindBitmap(ABitmapName);
  if Assigned(bmp) and not IsBitmapEmpty(bmp) then
    Result := DrawImage(bmp, ABackgroundColor, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TAdvGeneralPDFGraphicsLib.DrawImageWithName(ABitmapName: string;
  Rect: TRectF; Stretch, AspectRatio: Boolean;
  ImageType: TAdvPDFGraphicsLibImageType; Quality: Single; Center: Boolean): TRectF;
begin
  Result := DrawImageWithName(ABitmapName, gcWhite, Rect, Stretch, AspectRatio, ImageType, Quality, Center);
end;

function TAdvGeneralPDFGraphicsLib.DrawImageWithName(ABitmapName: string;
  Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType; Quality: Single): TRectF;
begin
  Result := DrawImageWithName(ABitmapName, gcWhite, Point, ImageType, Quality);
end;

procedure TAdvGeneralPDFGraphicsLib.DrawLine(StartPoint, EndPoint: TPointF);
begin
  if Stroke.Kind <> gskNone then
  begin
    DrawPathBegin;
    DrawPathMoveToPoint(StartPoint);
    DrawPathAddLineToPoint(EndPoint);
    DrawPathClose;
    DrawPathEnd(dmPathStroke);
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathEndLinearGradient(StartPoint,
  EndPoint: TPointF);
begin
  if Assigned(FOutput) then
  begin
    FOutput.WriteString('f' + PDFLB);
    if (Fill.ColorTo <> Fill.Color) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) then
      FOutput.NotifyShadingRect(RectF(StartPoint.X, FPageHeight - EndPoint.Y, EndPoint.X, FPageHeight - StartPoint.Y));
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathAddArc(CenterPoint: TPointF; Radius,
  StartAngle, EndAngle: Single; Clockwise: Boolean = False);
begin
  if Assigned(FOutput) then
  begin
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathAddArcToPoint(FirstPoint,
  SecondPoint: TPointF; Radius: Single);
begin
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathAddCurveToPoint(FirstControlPoint, SecondControlPoint, EndPoint: TPointF);
var
  pt1, pt2, pt3: TPointF;
begin
  if Assigned(FOutput) then
  begin
    pt1.X := FirstControlPoint.X;
    pt1.Y := FPageHeight - FirstControlPoint.Y;
    pt2.X := SecondControlPoint.X;
    pt2.Y := FPageHeight - SecondControlPoint.Y;
    pt3.X := EndPoint.X;
    pt3.Y := FPageHeight - EndPoint.Y;
    FOutput.CurveTo(pt1.X, pt1.Y, pt2.X, pt2.Y, pt3.X, pt3.Y);
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathAddEllipse(Rect: TRectF);
var
  x, y, w2, h2, xw2, yh2, w, h: single;
  bz: Single;
begin
  if Assigned(FOutput) then
  begin
    w := Rect.Width;
    h := Rect.Height;

    x := Rect.Left;
    y := FPageHeight - Rect.Top - h;

    bz := 4/3 * (sqrt(2) - 1);
    w2 := w / 2;
    h2 := h / 2;
    xw2 := x + w2;
    yh2 := y + h2;

    UpdatePathRect(PointF(x, y));
    UpdatePathRect(PointF(x + w, y + h));

    FOutput.CurveTo(x, yh2 - h2 * bz, xw2 - w2 * bz, y, xw2, y);
    FOutput.CurveTo(xw2 + w2 * bz, y, x + w, yh2 - h2 * bz, x + w, yh2);
    FOutput.CurveTo(x + w, yh2 + h2 * bz, xw2 + w2 * bz, y + h, xw2, y + h);
    FOutput.CurveTo(xw2 - w2 * bz, y + h, x, yh2 + h2 * bz, x, yh2);
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathAddLines(Points: TAdvPDFGraphicsLibPointArray);
var
  I: Integer;
begin
  if Length(Points) > 1 then
  begin
    DrawPathMoveToPoint(Points[0]);
    for I := 1 to Length(Points) - 1 do
      DrawPathAddLineToPoint(Points[I]);
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathAddLineToPoint(Point: TPointF);
var
  pt: TPointF;
begin
  if Assigned(FOutput) then
  begin
    pt.X := Point.X;
    pt.Y := FPageHeight - Point.Y;
    UpdatePathRect(pt);
    FOutput.LineTo(pt);
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathAddQuadCurveToPoint(ControlPoint,
  EndPoint: TPointF);
var
  pt1, pt2: TPointF;
begin
  if Assigned(FOutput) then
  begin
    pt1.X := ControlPoint.X;
    pt1.Y := FPageHeight - ControlPoint.Y;
    pt2.X := EndPoint.X;
    pt2.Y := FPageHeight - EndPoint.Y;
    FOutput.CurveTo2(pt1.X, pt1.Y, pt2.X, pt2.Y);
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathAddRectangle(Rect: TRectF);
var
  pt: TPointF;
begin
  if Assigned(FOutput) then
  begin
    pt.X := Rect.Left;
    pt.Y := FPageHeight - Rect.Top - Rect.Height;
    UpdatePathRect(pt);
    UpdatePathRect(PointF(pt.X + Rect.Width, pt.Y + Rect.Height));
    FOutput.WriteRectangle(RectF(pt.X, pt.Y, Rect.Width, Rect.Height));
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathBegin;
var
  p: string;
begin
  if Assigned(FOutput) then
  begin
    FPathRect := RectF(-1, -1, -1, -1);
    FOutput.WriteString('n' + PDFLB);
    if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
      FOutput.WriteStrokeColor(Stroke.Color);

    if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
    begin
      if (Fill.ColorTo <> Fill.Color) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) then
      begin
        p := '';
        FOutput.NotifyShading(Fill.Color, Fill.ColorTo, Fill.Orientation, p);
        FOutput.WriteString('/Pattern cs' + PDFLB);
        FOutput.WriteString(p + ' scn' + PDFLB);
      end
      else
        FOutput.WriteFillColor(Fill.Color);
    end;

    if Stroke.Width > 0 then
      FOutput.WriteStrokeWidth(Stroke.Width);

    FOutput.WriteStrokeKind(Stroke.Kind);

  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathClose;
begin
  if Assigned(FOutput) then
    FOutput.WriteString('h' + PDFLB);
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathEnd(DrawingMode: TAdvPDFGraphicsLibPathDrawingMode = dmPathFillStroke);
begin
  if Assigned(FOutput) then
  begin
    case DrawingMode of
      dmPathFill, dmPathEOFill:
      begin
        if DrawingMode = dmPathEOFill then
          DrawPathClose;
        FOutput.WriteString('f' + PDFLB);
      end;
      dmPathStroke, dmPathEOStroke:
      begin
        if DrawingMode = dmPathEOStroke then
          DrawPathClose;
        FOutput.WriteString('S' + PDFLB);
      end;
      dmPathFillStroke, dmPathEOFillStroke:
      begin
        if DrawingMode = dmPathEOFillStroke then
          DrawPathClose;
        FOutput.WriteString('B' + PDFLB);
      end;
    end;

    if (Fill.ColorTo <> Fill.Color) and (Fill.ColorTo <> gcNull) and (Fill.Kind = gfkGradient) then
      FOutput.NotifyShadingRect(FPathRect);
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathMoveToPoint(Point: TPointF);
var
  pt: TPointF;
begin
  if Assigned(FOutput) then
  begin
    pt.X := Point.X;
    pt.Y := FPageHeight - Point.Y;
    UpdatePathRect(pt);
    FOutput.MoveTo(pt);
  end;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawPathEndRadialGradient(StartCenter,
  EndCenter: TPointF; StartRadius, EndRadius: Single);
begin
end;

procedure TAdvGeneralPDFGraphicsLib.DrawRectangle(Rect: TRectF);
begin
  DrawSaveState;
  DrawPathBegin;
  DrawPathMoveToPoint(PointF(Rect.Left, Rect.Top));
  DrawPathAddLineToPoint(PointF(Rect.Right, Rect.Top));
  DrawPathAddLineToPoint(PointF(Rect.Right, Rect.Bottom));
  DrawPathAddLineToPoint(PointF(Rect.Left, Rect.Bottom));
  DrawPathClose;

  if (Fill.Color <> gcNull) and (Stroke.Color <> gcNull) and (Fill.Kind <> gfkNone) and (Stroke.Kind <> gskNone) then
    DrawPathEnd(dmPathFillStroke)
  else if (Fill.Color <> gcNull) and (Fill.Kind <> gfkNone) then
    DrawPathEnd(dmPathFill)
  else if (Stroke.Color <> gcNull) and (Stroke.Kind <> gskNone) then
    DrawPathEnd(dmPathStroke);

  DrawRestoreState;
end;

procedure TAdvGeneralPDFGraphicsLib.DrawRestoreState;
begin
  if Assigned(FOutput) then
    FOutput.WriteString('Q' + PDFLB);
end;

function TAdvGeneralPDFGraphicsLib.DrawRichText(Rects: TAdvPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := RichText.Draw(Rects, Padding, DetectOverflow);
end;

function TAdvGeneralPDFGraphicsLib.DrawRichText(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := RichText.Draw(Rect, Columns, Padding, DetectOverflow);
end;

function TAdvGeneralPDFGraphicsLib.DrawRichText(Rect: TRectF;
  Calculate: Boolean): TRectF;
begin
  Result := RichText.Draw(Rect, Calculate);
end;

procedure TAdvGeneralPDFGraphicsLib.DrawRoundedRectangle(Rect: TRectF;
  Rounding: Single);
begin
end;

procedure TAdvGeneralPDFGraphicsLib.DrawSaveState;
begin
  if Assigned(FOutput) then
    FOutput.WriteString('q' + PDFLB);
end;

function TAdvGeneralPDFGraphicsLib.DrawText(Text: UnicodeString; Rect: TRectF; Calculate: Boolean = False): TRectF;
var
  i: Integer;
  s, sn, st: UnicodeString;
  l: Integer;
  w, mw: Single;
  f: Boolean;
  p: Integer;
  tw: Single;
  th: Single;
  lcnt: Integer;
  rs: TRectF;
  c: TAdvGraphicsColor;
  lw: Single;
begin
  rs := Rect;
  if Assigned(FOutput) then
  begin
    if IsUnicodeText(Text) then
      FOutput.NotifyUnicode(Text)
    else
      FOutput.NotifyText(Text);

    Rect.Top := FPageHeight - Rect.Top - FOutput.FontSize;
    Rect.Bottom := FPageHeight - Rect.Bottom - FOutput.FontSize;

    mw := 0;
    {$IFDEF DELPHI_LLVM}
    i := 0;
    {$ENDIF}
    {$IFNDEF DELPHI_LLVM}
    i := 1;
    {$ENDIF}
    lcnt := 0;
    tw := 0;
    s := FindNextWord(Text, i);
    w := MeasureTextWidth(s);
    th := FOutput.FontSize;

    mw := mw + w;
    if (Length(s) > 0) and (s[Length(s)] = ' ') then
      mw := mw + FOutput.FontWordSpacing;

    while i <= Length(Text) do
    begin
      l := Length(s);
      if (l >= 2) and (((s[l] = #10) and (s[l - 1] = #13)) or ((s[l] = #13) and (s[l - 1] = #10))) then
      begin
        s := Copy(s, 1, l - 2);
        f := True;
      end
      else if (l >= 1) and ((s[l] = #10) or (s[l] = #13)) then
      begin
        s := Copy(s, 1, l - 1);
        f := True;
      end
      else
        f := False;

      sn := FindNextWord(Text, i);
      w := MeasureTextWidth(sn);

      if (Rect.Left + mw + w > Rect.Right) or f then
      begin
        if s <> '' then
        begin
          p := GetCharacterCount(s, Rect.Width);
          st := Copy(s, 1, p);

          Inc(lcnt);
          if mw > tw then
            tw := mw;

          if not Calculate then
          begin
            FOutput.BeginText;
            FOutput.WriteFont;
            FOutput.WriteFontColor;
            FOutput.WriteFontLeading;
            case Alignment of
              gtaLeading: FOutput.MoveTextTo(PointF(Rect.Left, Rect.Top));
              gtaCenter: FOutput.MoveTextTo(PointF(Rect.Left + (Rect.Width - mw) / 2, Rect.Top));
              gtaTrailing: FOutput.MoveTextTo(PointF(Rect.Right - mw, Rect.Top));
            end;
            FOutput.AddText(st);
            FOutput.EndText;

            c := Stroke.Color;
            lw := Stroke.Width;
            Stroke.Color := Font.Color;
            Stroke.Width := Font.Size * PDFULLWFACTOR;
            if TFontStyle.fsUnderline in Font.Style then
            begin
              case Alignment of
                gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
                gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Width - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + (Rect.Width - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
                gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
              end;
            end;

            if TFontStyle.fsStrikeOut in Font.Style then
            begin
              case Alignment of
                gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
                gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Width - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + (Rect.Width - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
                gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
              end;
            end;
            Stroke.Color := c;
            Stroke.Width := lw;
          end;

          mw := 0;
        end;

        s := '';

        Rect.Top := Rect.Top - FOutput.FontSize * PDFLHFACTOR;
        if (Int(Rect.Top) < Int(Rect.Bottom + FOutput.FontSize * PDFLHFACTOR)) and not Calculate then
          Break;
      end;

      mw := mw + w;
      if (Length(sn) > 0) and (sn[Length(sn)] = ' ') then
        mw := mw + FOutput.FontWordSpacing;
      s := s + sn;
    end;

    if s <> '' then
    begin
      p := GetCharacterCount(s, Rect.Width);
      st := Copy(s, 1, p);
      Inc(lcnt);
      if mw > tw then
        tw := mw;

      if not Calculate then
      begin
        FOutput.BeginText;
        FOutput.WriteFont;
        FOutput.WriteFontColor;
        FOutput.WriteFontLeading;
        case Alignment of
          gtaLeading: FOutput.MoveTextTo(PointF(Rect.Left, Rect.Top));
          gtaCenter: FOutput.MoveTextTo(PointF(Rect.Left + (Rect.Width - mw) / 2, Rect.Top));
          gtaTrailing: FOutput.MoveTextTo(PointF(Rect.Right - mw, Rect.Top));
        end;
        FOutput.AddText(st);
        FOutput.EndText;
        c := Stroke.Color;
        lw := Stroke.Width;
        Stroke.Color := Font.Color;
        Stroke.Width := Font.Size * PDFULLWFACTOR;
        if TFontStyle.fsUnderline in Font.Style then
        begin
          case Alignment of
            gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
            gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Width - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + (Rect.Width - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
            gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
          end;
        end;

        if TFontStyle.fsStrikeOut in Font.Style then
        begin
          case Alignment of
            gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
            gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Width - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + (Rect.Width - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
            gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
          end;
        end;
        Stroke.Color := c;
        Stroke.Width := lw;
      end;
    end;

    FTextRect := RectF(rs.Left, rs.Top, rs.Left + tw, rs.Top + lcnt * FOutput.FontSize * PDFLHFACTOR);
    Result := FTextRect;
  end;
end;

function TAdvGeneralPDFGraphicsLib.FindNextWord(AText: UnicodeString;
  var APos: Integer): UnicodeString;
var
  l: integer;
  i: integer;
begin
  Result := '';

  l := Length(AText);
  if APos > l then
    Exit;

  i := APos;
  while True do
  begin
    if ((AText[i] = #10) and (AText[i - 1] = #13)) or ((AText[i] = #13) and (AText[i - 1] = #10)) or (AText[i] = ' ') then
    begin
      if AText[i] = ' ' then
        Result := Copy(AText, APos, i - (APos - 1))
      else
        Result := Copy(AText, APos, i - APos);

      Break;
    end
    else if (AText[i] = #10) or (AText[i] = #13) or (AText[i] = ' ') then
    begin
      result := Copy(AText, APos, i - (APos - 1));
      Break;
    end
    else if i >= l then
    begin
      result := Copy(AText, APos, i - (APos - 1));
      Break;
    end
    else
      inc(i);
  end;

  APos := i + 1;
end;

function TAdvGeneralPDFGraphicsLib.DrawText(Text: UnicodeString; Rects: TAdvPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  K, i: Integer;
  s, sn, st: UnicodeString;
  l: Integer;
  w, mw: Single;
  f: Boolean;
  p: Integer;
  Rect: TRectF;
  th: Single;
  c: TAdvGraphicsColor;
  lw: Single;
begin
  Result := 0;
  if Assigned(FOutput) then
  begin
    if IsUnicodeText(Text) then
      FOutput.NotifyUnicode(Text)
    else
      FOutput.NotifyText(Text);

    for K := 0 to Length(Rects) - 1 do
    begin
      Rect := Rects[K];
      InflateRectEx(Rect, -Padding, 0);
      Rect.Top := FPageHeight - Rect.Top - FOutput.FontSize;
      Rect.Bottom := FPageHeight - Rect.Bottom - FOutput.FontSize;

      mw := 0;
      {$IFDEF DELPHI_LLVM}
      i := 0;
      {$ENDIF}
      {$IFNDEF DELPHI_LLVM}
      i := 1;
      {$ENDIF}
      s := FindNextWord(Text, i);
      Delete(Text, 1, i - 1);
      w := MeasureTextWidth(s);
      th := FOutput.FontSize;
      mw := mw + w;
      if (Length(s) > 0) and (s[Length(s)] = ' ') then
        mw := mw + FOutput.FontWordSpacing;

      while Length(Text) > 0 do
      begin
        l := Length(s);
        if (l >= 2) and (((s[l] = #10) and (s[l - 1] = #13)) or ((s[l] = #13) and (s[l - 1] = #10))) then
        begin
          s := Copy(s, 1, l - 2);
          f := True;
        end
        else if (l >= 1) and (s[l] = #10) or (s[l] = #13) then
        begin
          s := Copy(s, 1, l - 1);
          f := True;
        end
        else
          f := False;

        {$IFDEF DELPHI_LLVM}
        i := 0;
        {$ENDIF}
        {$IFNDEF DELPHI_LLVM}
        i := 1;
        {$ENDIF}
        sn := FindNextWord(Text, i);
        Delete(Text, 1, i - 1);
        w := MeasureTextWidth(sn);

        if (Rect.Left + mw + w > Rect.Right) or f then
        begin
          if s <> '' then
          begin
            p := GetCharacterCount(s, Rect.Width);
            st := Copy(s, 1, p);

            if not DetectOverflow then
            begin
              FOutput.BeginText;
              FOutput.WriteFont;
              FOutput.WriteFontColor;
              FOutput.WriteFontLeading;
              case Alignment of
                gtaLeading: FOutput.MoveTextTo(PointF(Rect.Left, Rect.Top));
                gtaCenter: FOutput.MoveTextTo(PointF(Rect.Left + (Rect.Width - mw) / 2, Rect.Top));
                gtaTrailing: FOutput.MoveTextTo(PointF(Rect.Right - mw, Rect.Top));
              end;
              FOutput.AddText(st);
              FOutput.EndText;

              c := Stroke.Color;
              lw := Stroke.Width;
              Stroke.Color := Font.Color;
              Stroke.Width := Font.Size * PDFULLWFACTOR;
              if TFontStyle.fsUnderline in Font.Style then
              begin
                case Alignment of
                  gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
                  gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Width - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + (Rect.Width - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
                  gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
                end;
              end;

              if TFontStyle.fsStrikeOut in Font.Style then
              begin
                case Alignment of
                  gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
                  gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Width - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + (Rect.Width - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
                  gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
                end;
              end;
              Stroke.Color := c;
              Stroke.Width := lw;
            end;

            mw := 0;
          end;

          s := '';

          Rect.Top := Rect.Top - FOutput.FontSize * PDFLHFACTOR;
          if Int(Rect.Top) < Int(Rect.Bottom + FOutput.FontSize * PDFLHFACTOR) then
          begin
            Text := sn + Text;
            Break;
          end;
        end;

        mw := mw + w;
        if (Length(sn) > 0) and (sn[Length(sn)] = ' ') then
          mw := mw + FOutput.FontWordSpacing;
        s := s + sn;
      end;

      if s <> '' then
      begin
        p := GetCharacterCount(s, Rect.Width);
        st := Copy(s, 1, p);

        if not DetectOverflow then
        begin
          FOutput.BeginText;
          FOutput.WriteFont;
          FOutput.WriteFontColor;
          FOutput.WriteFontLeading;
          case Alignment of
            gtaLeading: FOutput.MoveTextTo(PointF(Rect.Left, Rect.Top));
            gtaCenter: FOutput.MoveTextTo(PointF(Rect.Left + (Rect.Width - mw) / 2, Rect.Top));
            gtaTrailing: FOutput.MoveTextTo(PointF(Rect.Right - mw, Rect.Top));
          end;
          FOutput.AddText(st);
          FOutput.EndText;

          c := Stroke.Color;
          lw := Stroke.Width;
          Stroke.Color := Font.Color;
          Stroke.Width := Font.Size * PDFULLWFACTOR;
          if TFontStyle.fsUnderline in Font.Style then
          begin
            case Alignment of
              gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
              gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Width - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Left + (Rect.Width - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
              gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFULFACTOR)));
            end;
          end;

          if TFontStyle.fsStrikeOut in Font.Style then
          begin
            case Alignment of
              gtaLeading: DrawLine(PointF(Rect.Left, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
              gtaCenter: DrawLine(PointF(Rect.Left + (Rect.Width - mw) / 2, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Left + (Rect.Width - mw) / 2 + mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
              gtaTrailing: DrawLine(PointF(Rect.Right - mw, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)), PointF(Rect.Right, FPageHeight - Rect.Top - th + (th * PDFLHFACTOR * PDFSTFACTOR)));
            end;
          end;
          Stroke.Color := c;
          Stroke.Width := lw;
        end;
      end;
    end;

    Result := Length(Text);
  end;
end;

function TAdvGeneralPDFGraphicsLib.DrawText(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  arr: TAdvPDFGraphicsLibRectArray;
  I: Integer;
begin
  SetLength(arr, Columns);
  for I := 0 to Columns - 1 do
    arr[I] := RectF(Rect.Left + Rect.Width / Columns * I, Rect.Top, Rect.Left + Rect.Width / Columns * (I + 1), Rect.Bottom);

  Result := DrawText(Text, arr, Padding, DetectOverflow);
end;

function TAdvGeneralPDFGraphicsLib.GetAlignment: TAdvGraphicsTextAlign;
begin
  Result := Alignment;
end;

function TAdvGeneralPDFGraphicsLib.GetPictureContainer: TPictureContainer;
begin
  Result := PictureContainer;
end;

function TAdvGeneralPDFGraphicsLib.GetCharacterCount(AText: UnicodeString;
  AWidth: single): Integer;
var
  i: Integer;
  c: Char;
  w, tw: Single;
begin
  Result := 0;
  tw := 0;

  {$IFDEF DELPHI_LLVM}
  for i := 0 to Length(AText) - 1 do
  {$ENDIF}
  {$IFNDEF DELPHI_LLVM}
  for i := 1 to Length(AText) do
  {$ENDIF}
  begin
    c := AText[i];
    w := FOutput.GetFontCharWidth(AText, i) * FOutput.FontSize / 1000;

    if w > 0 then
      w := w + FOutput.FontCharSpacing
    else
      w := 0;

    if (c = ' ') and (FOutput.FontWordSpacing > 0) and (i <> Length(AText)) then
      w := w + FOutput.FontWordSpacing;

    tw := tw + w;
    if tw > AWidth then
      Break;

    Inc(Result);
  end;
end;

function TAdvGeneralPDFGraphicsLib.GetFill: TAdvPDFGraphicsFill;
begin
  Result := Fill;
end;

function TAdvGeneralPDFGraphicsLib.GetFont: TAdvPDFGraphicsLibFont;
begin
  Result := Font;
end;

function TAdvGeneralPDFGraphicsLib.GetLineBreakMode: TAdvPDFGraphicsLibLineBreakMode;
begin
  Result := LineBreakMode;
end;

function TAdvGeneralPDFGraphicsLib.GetOnNotifyNewPage: TNotifyEvent;
begin
  Result := FOnNotifyNewPage;
end;

function TAdvGeneralPDFGraphicsLib.GetPageHeight: Single;
begin
  Result := FPageHeight;
end;

function TAdvGeneralPDFGraphicsLib.GetPageWidth: Single;
begin
  Result := FPageWidth;
end;

function TAdvGeneralPDFGraphicsLib.GetStroke: TAdvPDFGraphicsStroke;
begin
  Result := Stroke;
end;

function TAdvGeneralPDFGraphicsLib.GetTextRect: TRectF;
begin
  Result := FTextRect;
end;

function TAdvGeneralPDFGraphicsLib.GetURLFont: TAdvPDFGraphicsLibFont;
begin
  Result := URLFont;
end;

procedure TAdvGeneralPDFGraphicsLib.InitializeAppearance;
begin
end;

function TAdvGeneralPDFGraphicsLib.IsUnicodeText(AText: UnicodeString): Boolean;
begin
  Result := False;
  if Assigned(FOutput) then
    Result := FOutput.IsUnicodeString(AText);
end;

function TAdvGeneralPDFGraphicsLib.MeasureTextWidth(AText: UnicodeString): Single;
var
  i: integer;
  c: char;
  w: Single;
begin
  Result := 0;
  {$IFDEF DELPHI_LLVM}
  for i := 0 to Length(AText) - 1 do
  {$ENDIF}
  {$IFNDEF DELPHI_LLVM}
  for i := 1 to Length(AText) do
  {$ENDIF}
  begin
    if (AText[i] = #10) or (AText[i] = #13) then
      Continue;

    c := AText[i];
    w := FOutput.GetFontCharWidth(AText, i) * FOutput.FontSize / 1000;

    if w > 0 then
      w := w + FOutput.FontCharSpacing
    else
      w := 0;

    if (c = ' ') and (FOutput.FontWordSpacing > 0) and (i <> Length(AText)) then
      w := w + FOutput.FontWordSpacing;

    Result := Result + w;
  end;

  Result := Result - FOutput.FontCharSpacing;
end;

procedure TAdvGeneralPDFGraphicsLib.NotifyNewPage;
begin
  if Assigned(OnNotifyNewPage) then
    OnNotifyNewPage(Self);
end;

function TAdvGeneralPDFGraphicsLib.RichText: IAdvCustomPDFRichTextLib;
begin
  Result := FPDFRichTextLib;
end;

procedure TAdvGeneralPDFGraphicsLib.SetAlignment(const Value: TAdvGraphicsTextAlign);
begin
  Alignment := Value;
end;

procedure TAdvGeneralPDFGraphicsLib.SetPictureContainer(
  const Value: TPictureContainer);
begin
  PictureContainer := Value;
end;

procedure TAdvGeneralPDFGraphicsLib.SetCanvas(ACanvas: Pointer);
begin
  FOutput := TAdvPDFGraphicsLibOutputWriter(ACanvas);
end;

procedure TAdvGeneralPDFGraphicsLib.SetFill(
  const Value: TAdvPDFGraphicsFill);
begin
  Fill.Assign(Value);
end;

procedure TAdvGeneralPDFGraphicsLib.SetFont(const Value: TAdvPDFGraphicsLibFont);
begin
  Font.Assign(Value);
end;

procedure TAdvGeneralPDFGraphicsLib.SetLineBreakMode(
  const Value: TAdvPDFGraphicsLibLineBreakMode);
begin
  LineBreakMode := Value;
end;

procedure TAdvGeneralPDFGraphicsLib.SetOnNotifyNewPage(
  const Value: TNotifyEvent);
begin
  FOnNotifyNewPage := Value;
end;

procedure TAdvGeneralPDFGraphicsLib.SetPageHeight(APageHeight: Single);
begin
  FPageHeight := APageHeight;
end;

procedure TAdvGeneralPDFGraphicsLib.SetPageWidth(APageWidth: Single);
begin
  FPageWidth := APageWidth;
end;

procedure TAdvGeneralPDFGraphicsLib.SetPDFLib(APDFLib: IInterface);
begin
  FPDFLib := APDFLib as IAdvCustomPDFLib;
end;

procedure TAdvGeneralPDFGraphicsLib.SetPDFRichTextLib(
  const Value: IAdvCustomPDFRichTextLib);
begin
  FPDFRichTextLib := Value;
end;

procedure TAdvGeneralPDFGraphicsLib.SetStroke(
  const Value: TAdvPDFGraphicsStroke);
begin
  Stroke.Assign(Value);
end;

procedure TAdvGeneralPDFGraphicsLib.SetURLFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  URLFont.Assign(Value);
end;

procedure TAdvGeneralPDFGraphicsLib.UpdateFill;
begin
  inherited;

end;

procedure TAdvGeneralPDFGraphicsLib.UpdateFont;
begin
  inherited;
  if Assigned(FOutput) and Assigned(FOutput.OnFontChanged) then
    FOutput.OnFontChanged(Self);
end;

procedure TAdvGeneralPDFGraphicsLib.UpdatePathRect(APoint: TPointF);
begin
  if (APoint.X < FPathRect.Left) or (FPathRect.Left = -1) then
    FPathRect.Left := APoint.X;

  if (APoint.Y < FPathRect.Top) or (FPathRect.Top = -1) then
    FPathRect.Top := APoint.Y;

  if (APoint.X > FPathRect.Right) or (FPathRect.Right = -1) then
    FPathRect.Right := APoint.X;

  if (APoint.Y > FPathRect.Bottom) or (FPathRect.Bottom = -1) then
    FPathRect.Bottom := APoint.Y;
end;

procedure TAdvGeneralPDFGraphicsLib.UpdateStroke;
begin
  inherited;

end;

end.


