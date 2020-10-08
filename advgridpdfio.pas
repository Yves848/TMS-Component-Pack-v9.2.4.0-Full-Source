{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2016 - 2018                                      }
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

unit AdvGridPDFIO;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvUtil, AdvPDFLib, AdvGrid, AdvPDFIO, Graphics,
  AdvTypes, Types, BaseGrid, PictureContainer, AdvObj, AdvGraphicsTypes;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 9; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : Issue with displaying images under certain circumstances
  //          : Fixed : Issue rendering HTML without correct tags
  //          : Fixed : Issue rendering images from image list.
  // v1.0.0.2 : Fixed : Issue with hidden columns still displayed
  // v1.0.0.3 : Improved : Support for goFixedHorzLine, goFixedVertLine, goHorzLine & goVertLine
  // v1.0.0.4 : Fixed : Issue with cell properties being reverted to default
  // v1.0.0.5 : Fixed: Issue with single row grids
  // v1.0.0.6 : Fixed: Issue with wordwrapping initialization
  // v1.0.0.7 : Fixed: Issue with row page break causing access violation in certain situations
  // v1.0.0.8 : Fixed: Issue with export from grid with hidden columns
  // v1.0.0.9 : Fixed: Issue with export hidden columns with fixed row repeat

resourcestring
  sTMSGridPDFIOGridNotAssigned = 'Grid Not Assigned';

type
  TAdvGridPDFIO = class;

  TAdvGridPDFIOCellLayout = (gclFull, gclColor, gclNone);

  TAdvGridPDFIOOptions = class(TAdvPDFIOOptions)
  private
    FFitToPage: Boolean;
    FRepeatFixedRows: Boolean;
    FRepeatFixedColumns: Boolean;
    FCellLayout: TAdvGridPDFIOCellLayout;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
  published
    property FitToPage: Boolean read FFitToPage write FFitToPage default True;
    property RepeatFixedRows: Boolean read FRepeatFixedRows write FRepeatFixedRows default False;
    property RepeatFixedColumns: Boolean read FRepeatFixedColumns write FRepeatFixedColumns default False;
    property CellLayout: TAdvGridPDFIOCellLayout read FCellLayout write FCellLayout default gclFull;
  end;

  TAdvGridPDFIORowIsPageBreakEvent = procedure(Sender: TObject; ARow: Integer; var IsPageBreak: Boolean) of object;

  TAdvGridPDFIOCellRange = record
    StartCol, StartRow, EndCol, EndRow: Integer;
  end;

  TAdvCustomGridPDFIO = class(TAdvCustomPDFIO)
  private
    FOnRowIsPageBreak: TAdvGridPDFIORowIsPageBreakEvent;
    function GetOptions: TAdvGridPDFIOOptions;
    procedure SetOptions(const Value: TAdvGridPDFIOOptions);
    function GetGrid: TAdvStringGrid;
    procedure SetGrid(const Value: TAdvStringGrid);
  protected
    function GetVersion: String; override;
    function CreateOptions: TAdvPDFIOOptions; override;
    procedure DrawCell(APDFIO: TAdvCustomGridPDFIO; APDFLib: TAdvPDFLib; AGrid: TAdvStringGrid; AText: String;
      ACol, ARow: Integer; ARect: TRectF; ADrawText, ADrawBackground: Boolean; AScale: Single);
    procedure DoPDFExport(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject); override;
    procedure DoPDFGridExport(const APDFLib: TAdvPDFLib; const AGrid: TAdvStringGrid; const ASelection: TAdvGridPDFIOCellRange); virtual;
    procedure DoRowIsPageBreak(ARow: Integer; var IsPageBreak: Boolean); virtual;
    property Grid: TAdvStringGrid read GetGrid write SetGrid;
    property Version: String read GetVersion;
    property Options: TAdvGridPDFIOOptions read GetOptions write SetOptions;
    property OnRowIsPageBreak: TAdvGridPDFIORowIsPageBreakEvent read FOnRowIsPageBreak write FOnRowIsPageBreak;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridPDFIO = class(TAdvCustomGridPDFIO)
  published
    property Grid;
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
    property OnRowIsPageBreak;
  end;

implementation

uses
  SysUtils, ImgList, AdvUtils, Forms, Math, AdvGraphics,
  Grids, AdvPDFCoreLibBase
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  ;

type
  TAdvStringGridOpen = class(TAdvStringGrid);
  TCellPropertiesOpen = class(TCellProperties);

function CellRange(AStartCol, AStartRow, AEndCol,
  AEndRow: Integer): TAdvGridPDFIOCellRange;
begin
  Result.StartCol := AStartCol;
  Result.StartRow := AStartRow;
  Result.EndCol := AEndCol;
  Result.EndRow := AEndRow;
end;

{ TAdvCustomGridPDFIO }

function TAdvCustomGridPDFIO.GetGrid: TAdvStringGrid;
begin
  Result := TAdvStringGrid(inherited ExportObject);
end;

function TAdvCustomGridPDFIO.GetOptions: TAdvGridPDFIOOptions;
begin
  Result := TAdvGridPDFIOOptions(inherited Options);
end;

function TAdvCustomGridPDFIO.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomGridPDFIO.DrawCell(APDFIO: TAdvCustomGridPDFIO; APDFLib: TAdvPDFLib; AGrid: TAdvStringGrid; AText: String;
  ACol, ARow: Integer; ARect: TRectF; ADrawText, ADrawBackground: Boolean; AScale: Single);

procedure DrawCheckBox(ARect: TRectF; AChecked: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True);
var
  c: TAdvGraphicsColor;
  r: TRectF;
begin
  r := ARect;
  InflateRectEx(r, -1, -1);

  if AEnabled then
  begin
    if AFocused then
      c := gcSteelBlue
    else
      c := gcBlack;
  end
  else
    c := gcDarkgray;

  APDFLib.Graphics.Fill.ColorTo := gcNull;
  if AEnabled then
    APDFLib.Graphics.Fill.Color := Lighter(gcLightgray, 85)
  else
    APDFLib.Graphics.Fill.Color := gcLightgray;

  APDFLib.Graphics.Stroke.Width := 1 * AScale;
  APDFLib.Graphics.Stroke.Kind := gskSolid;
  APDFLib.Graphics.Stroke.Color := c;
  APDFLib.Graphics.DrawRectangle(r);
  InflateRectEx(r, -r.Width / 5, -r.Height / 5);
  APDFLib.Graphics.Stroke.Width := 2 * AScale;
  APDFLib.Graphics.Stroke.Color := c;
  if AChecked then
  begin
    APDFLib.Graphics.DrawLine(PointF(r.Left + 1, r.Top + 1), PointF(r.Right - 1, r.Bottom - 1));
    APDFLib.Graphics.DrawLine(PointF(r.Right - 1, r.Top + 1), PointF(r.Left + 1, r.Bottom - 1));
  end;
end;

procedure DrawRadioButton(ARect: TRectF; AChecked: Boolean = False; AFocused: Boolean = False; AEnabled: Boolean = True);
var
  c: TAdvGraphicsColor;
  r: TRectF;
begin
  r := ARect;
  InflateRectEx(r, -1, -1);

  if AEnabled then
  begin
    if AFocused then
      c := gcSteelBlue
    else
      c := gcBlack;
  end
  else
    c := gcDarkgray;

  APDFLib.Graphics.Fill.ColorTo := gcNull;
  if AEnabled then
    APDFLib.Graphics.Fill.Color := Lighter(gcLightgray, 85)
  else
    APDFLib.Graphics.Fill.Color := gcLightgray;

  APDFLib.Graphics.Stroke.Width := 1 * AScale;
  APDFLib.Graphics.Stroke.Kind := gskSolid;
  APDFLib.Graphics.Stroke.Color := c;
  APDFLib.Graphics.DrawEllipse(r);
  InflateRectEx(r, -r.Width / 5, -r.Height / 5);
  APDFLib.Graphics.Fill.Color := c;
  if AChecked then
  begin
    APDFLib.Graphics.DrawEllipse(r);
  end;
end;

  procedure DrawProgressBar(AProgressRect: TRectF; AValue: Single; AFormat: string = '%.0f%%'; AMax: Single = 100; AColor: TAdvGraphicsColor = gcYellowgreen; ATextColor: TAdvGraphicsColor = gcBlack; AShowText: Boolean = True; AEnabled: Boolean = True);
  var
    r, rp: TRectF;
    v: Single;
    trt: TRectF;
    spr: String;
  begin
    r := AProgressRect;
    rp := r;
    if AEnabled then
      APDFLib.Graphics.Fill.Color := Lighter(gcLightGray, 75)
    else
      APDFLib.Graphics.Fill.Color := gcLightGray;

    APDFLib.Graphics.Stroke.Kind := gskSolid;
    APDFLib.Graphics.Stroke.Color := gcDarkgray;

    APDFLib.Graphics.DrawRectangle(r);

    v := Max(0, Min(AValue, AMax));

    if (v >= 0) and (v <= AMax) and (AMax > 0) then
    begin
      InflateRectEx(rp, -1, -1);
      rp.Width := rp.Width * v / AMax;

      APDFLib.Graphics.Fill.Color := AColor;
      APDFLib.Graphics.Stroke.Color := APDFLib.Graphics.Fill.Color;

      APDFLib.Graphics.DrawRectangle(rp);

      if AShowText then
      begin
        spr := Format(AFormat, [v / AMax * 100]);
        APDFLib.Graphics.Font.Color := ATextColor;
        trt := APDFLib.Graphics.DrawText(spr, r, True);
        trt.Width := trt.Width + (2 * AScale);
        trt := RectF(AProgressRect.Left + (AProgressRect.Width - trt.Width) / 2, AProgressRect.Top + (AProgressRect.Height - trt.Height) / 2, AProgressRect.Left  + (AProgressRect.Width - trt.Width) / 2 + trt.Width, AProgressRect.Top + (AProgressRect.Height - trt.Height) / 2 + trt.Height);
        trt := RectF(Max(AProgressRect.Left, trt.Left), Max(AProgressRect.Top, trt.Top), Min(AProgressRect.Right, trt.Right), Min(AProgressRect.Bottom, trt.Bottom));
        APDFLib.Graphics.DrawText(spr, trt);
      end;
    end;
  end;

  procedure DrawPolygon(APoints: array of TPointF);
  var
    I: Integer;
  begin
    APDFLib.Graphics.DrawPathBegin;
      APDFLib.Graphics.DrawPathMoveToPoint(APoints[0]);
    for I := 1 to Length(APoints) - 1 do
      APDFLib.Graphics.DrawPathAddLineToPoint(APoints[I]);
    APDFLib.Graphics.DrawPathClose;
    APDFLib.Graphics.DrawPathEnd;
  end;

  procedure DrawBitmapTranspPDF(bmp:TBitmap;bkcolor:TColor;r:TRectF);
  var
    tmpbmp: TBitmap;
    srcColor: TColor;
    tgtrect: TRect;
    pic: TPicture;
  begin
    TmpBmp := TBitmap.Create;
    TmpBmp.Height := bmp.Height;
    TmpBmp.Width := bmp.Width;
    tgtrect.left :=0;
    tgtrect.top :=0;
    tgtrect.right := bmp.width;
    tgtrect.bottom := bmp.Height;
    r.bottom := r.top + bmp.height;
    r.Right := r.Left + bmp.width;
    TmpBmp.Canvas.Brush.Color := bkcolor;
    srcColor := bmp.canvas.pixels[0,0];
    TmpBmp.Canvas.BrushCopy(tgtrect,bmp,tgtrect,srcColor);
    pic := TPicture.Create;
    pic.Assign(tmpbmp);
    APDFLib.Graphics.DrawImage(pic, r);
    pic.Free;
    TmpBmp.Free;
  end;

  procedure DrawBitmapResourceTranspPDF(bkColor: TColor; r: TRectF; ResName:string);
  var
    bmp: TBitmap;
  begin
    bmp := TBitmap.Create;
    try
      bmp.LoadFromResourceName(hinstance,ResName);
      DrawBitmapTranspPDF(bmp,bkColor,r);
    finally
      bmp.Free;
    end;
  end;

  procedure DrawRangeIndicator(r: TRectF; Value, Range: Integer; ShowValue: boolean; NegColor, PosColor: TColor);
  var
    pw,mid: Single;
    txt: string;
    al: TAdvGraphicsTextAlign;
  begin
    InflateRectEx(r,-2,-2);

    mid := (R.Right - R.Left) / 2;

    if Value > Range then
      Value := Range;
    if Value < -Range then
      Value := -Range;

    pw := Round(Value * mid / Range);

    if Value < 0 then
    begin
      APDFLib.Graphics.Fill.Color := NegColor;
      APDFLib.Graphics.Stroke.Color := NegColor;
      APDFLib.Graphics.DrawRectangle(RectF(r.Left + mid + pw, r.Top, r.Left + mid, r.Bottom));
    end
    else
    begin
      APDFLib.Graphics.Fill.Color := PosColor;
      APDFLib.Graphics.Stroke.Color := PosColor;
      APDFLib.Graphics.DrawRectangle(RectF(r.Left + mid, r.Top, r.Left + mid + pw, r.Bottom));
    end;

    if ShowValue then
    begin
      txt := inttostr(value);
      al := APDFLib.Graphics.Alignment;
      APDFLib.Graphics.Alignment := gtaCenter;
      APDFLib.Graphics.DrawText(txt, r);
      APDFLib.Graphics.Alignment := al;
    end;
  end;

  procedure DrawProgressPie(r: TRectF; Color: TColor; p: Integer);
  var
    x,y: Integer;
    //dy: Integer;
  begin
    APDFLib.Graphics.Stroke.Color := clGray;
    APDFLib.Graphics.Stroke.Width := 1 * AScale;
    APDFLib.Graphics.DrawEllipse(r);

    APDFLib.Graphics.DrawLine(PointF(r.Left + (r.Right - r.Left) / 2,r.Top), PointF(r.Left + (r.Right - r.Left) / 2,r.Top + (r.Bottom - r.Top) / 2));

    x := round(0.5 * succ(Round(r.Right - r.Left)) * sin( p/100*2*PI ));
    y := round(0.5 * succ(Round(r.Bottom - r.Top)) * cos( p/100*2*PI ));

    APDFLib.Graphics.DrawLine(PointF(r.Left + (r.Right - r.Left) / 2,r.Top + (r.Bottom - r.Top) / 2), PointF(r.Left + x + (r.Right - r.Left) / 2,r.Top - y + (r.Bottom - r.Top) / 2));

    APDFLib.Graphics.Fill.Color := Color;
//    dy := 2;
//    if p <> 0 then
//      Canvas.FloodFill(r.Left + 1 + (r.Right - r.Left) / 2,r.Top + dy,clGray,fsBorder);
  end;

  procedure DrawShape(X,Y: Single; Width, Height: Single; Shape: TCellShape; FillColor:TColor; LineColor:TColor);
  var
    szx,szy: Single;
    cntrx,cntry: Single;
    wx,wy: Single;
    wxs,wys: double;
  begin
    APDFLib.Graphics.Fill.Kind := gfkSolid;
    APDFLib.Graphics.Fill.Color := FillColor;
    APDFLib.Graphics.Stroke.Kind := gskSolid;
    APDFLib.Graphics.Stroke.Color := LineColor;

    szx := Width;
    szy := Height;

    if (Shape in [csCircle, csSquare, csRoundSquare]) then
    begin
      if szx <> szy then
      begin
        if szx > szy then
          szx := szy
        else
          szy := szx;
      end;
    end;

    case Shape of
    csRectangle,csSquare: APDFLib.Graphics.DrawRectangle(RectF(X + 1 * AScale, Y + 1 * AScale, X + szx - 1 * AScale, Y + szy - 1 * AScale));
    csCircle,csEllips: APDFLib.Graphics.DrawEllipse(RectF(X, Y, X + szx, Y + szy));
    csTriangleLeft: DrawPolygon([PointF(X+szx,Y),PointF(X+szx,Y+szy),PointF(X,Y+szy / 2)]);
    csTriangleRight: DrawPolygon([PointF(X,Y),PointF(X,Y+szy),PointF(X+szx,Y+szy / 2)]);
    csTriangleUp: DrawPolygon([PointF(X,Y+szy),PointF(X+szx,Y+szy),PointF(X+szx / 2,Y)]);
    csTriangleDown: DrawPolygon([PointF(X,Y),PointF(X+szx,Y),PointF(X+szx / 2,Y+szy)]);
    csDiamond:
      begin
        szx := szx / 2;
        szy := szy / 2;
        DrawPolygon([PointF(X,Y+szy),PointF(X + szx,Y),PointF(X+ 2 * szx,Y + szy),PointF(X + szx,Y+ 2 * szy)]);
      end;
    csLineVert: APDFLib.Graphics.DrawRectangle(RectF(X+szx / 2, Y, X+ szx / 2 + 1, Y + szy));
    csLineHorz: APDFLib.Graphics.DrawRectangle(RectF(X,Y+szy / 2,X+szx, Y+szy / 2 + 1));
    csRoundRect,csRoundSquare: ;
    csHalfStar:
      begin
        wx := szx / 2;
        wy := szy / 2;
        wxs := wx / 3;
        wys := wy / 3;
        cntrx := X + wx;
        cntry := Y + wy;

        DrawPolygon([
                        PointF(cntrx + (wx * sin(0)),cntry - (wy * cos(0))),

                        PointF(cntrx + (wxs  * sin(5 * 2*PI/10)),cntry - (wys  * cos(5*2*PI/10))),

                        PointF(cntrx + (wx * sin(2*3*PI/5)),cntry - (wy * cos(2*3*PI/5))),

                        PointF(cntrx + (wxs  * sin(7 * 2*PI/10)),cntry - (wys  * cos(7*2*PI/10))),

                        PointF(cntrx + (wx * sin(2*4*PI/5)),cntry - (wy * cos(2*4*PI/5))),

                        PointF(cntrx + (wxs  * sin(9 * 2*PI/10)),cntry - (wys  * cos(9*2*PI/10)))
                       ]);
      end;
    csStar:
      begin
        wx := szx / 2;
        wy := szy / 2;
        wxs := wx / 3;
        wys := wy / 3;
        cntrx := X + wx;
        cntry := Y + wy;

        DrawPolygon([
                        PointF(cntrx + (wx * sin(0)),cntry - (wy * cos(0))),

                        PointF(cntrx + (wxs  * sin(2*PI/10)),cntry - (wys  * cos(2*PI/10))),

                        PointF(cntrx + (wx * sin(2*PI/5)),cntry - (wy * cos(2*PI/5))),

                        PointF(cntrx + (wxs  * sin(3 * 2*PI/10)),cntry - (wys  * cos(3*2*PI/10))),

                        PointF(cntrx + (wx * sin(2*2*PI/5)),cntry - (wy * cos(2*2*PI/5))),

                        PointF(cntrx + (wxs  * sin(5 * 2*PI/10)),cntry - (wys  * cos(5*2*PI/10))),

                        PointF(cntrx + (wx * sin(2*3*PI/5)),cntry - (wy * cos(2*3*PI/5))),

                        PointF(cntrx + (wxs  * sin(7 * 2*PI/10)),cntry - (wys  * cos(7*2*PI/10))),

                        PointF(cntrx + (wx * sin(2*4*PI/5)),cntry - (wy * cos(2*4*PI/5))),

                        PointF(cntrx + (wxs  * sin(9 * 2*PI/10)),cntry - (wys  * cos(9*2*PI/10)))
                       ]);

      end;
    csArrowUp:
      begin
        if odd(Round(szx)) then
          szx := szx - 1;

        DrawPolygon([
                         PointF(X,Y + szy / 2), PointF(X + szx / 2, Y), PointF(X + szx, Y + szy / 2),
                         PointF(X + 3 * (szx / 4), Y + szy / 2),
                         PointF(X + 3 * (szx / 4), Y + szy),
                         PointF(X + (szx / 4), Y + szy),
                         PointF(X + (szx / 4), Y + szy / 2)]);
      end;
    csArrowDown:
      begin
        if odd(Round(szx)) then
          szx := szx - 1;
        DrawPolygon([
                         PointF(X,Y + szy / 2), PointF(X + szx / 2, Y + szy), PointF(X + szx, Y + szy / 2),
                         PointF(X + 3 * (szx / 4), Y + szy / 2),
                         PointF(X + 3 * (szx / 4), Y),
                         PointF(X + (szx / 4), Y),
                         PointF(X + (szx / 4), Y + szy / 2)]);
      end;
    csArrowLeft:
      begin
        if odd(Round(szy)) then
          szy := szy - 1;

        DrawPolygon([
                         PointF(X,Y + szy / 2), PointF(X + szx / 2, Y),
                         PointF(X + szx / 2, Y + szy / 4),
                         PointF(X + szx, Y + szy / 4),
                         PointF(X + szx, Y + 3 * (szy / 4)),
                         PointF(X + szx / 2, Y + 3 * (szy / 4)),
                         PointF(X + szx / 2, Y + szy) ]);
      end;
    csArrowRight:
      begin
        if odd(Round(szy)) then
          szy := szy - 1;

        DrawPolygon([
                         PointF(X + szx,Y + szy / 2), PointF(X + szx / 2, Y),
                         PointF(X + szx / 2, Y + szy / 4),
                         PointF(X, Y + szy / 4),
                         PointF(X, Y + 3 * (szy / 4)),
                         PointF(X + szx / 2, Y + 3 * (szy / 4)),
                         PointF(X + szx / 2, Y + szy) ]);

      end;
    end;
  end;

  function ConvertToPlainText(AText: string): string;
  begin
    Result := AText;
    if (Pos('{\rtf', LowerCase(AText)) > 0) then
    begin
      AGrid.CellToRich(ACol, ARow, AGrid.RichEdit);
      Result := AGrid.RichEdit.Text;
    end;
  end;

var
  s, surl: string;
  tr: TRectF;
  ATextRect, ABitmapRect: TRectF;
  st: TGridDrawState;
  b: TBrush;
  cto, cm, cmto: TColor;
  ft: TFont;
  ha: TAlignment;
  va: TVAlignment;
  ww: Boolean;
  gd: TCellGradientDirection;
  bw: Single;
  bc: TColor;
  bmp: TPictureContainer;
  img: TCustomImageList;
  ico: TIcon;
  sz: TPointF;
  szs: TPoint;
  pic: TPicture;
  I: Integer;
  ARectB: TRectF;
  r, dr, cr: TRectF;
  brshColor: TColor;
  cl: TCellGraphic;
  rv: Single;
  idx: Integer;
begin
  b := TBrush.Create;
  ft := TFont.Create;
  bmp := TPictureContainer.Create(nil);
  try
    bw := 1;
    bc := clBlack;
    ww := AGrid.WordWrap;
    AGrid.GetVisualProperties(ACol, ARow, st, False, False, True, b, cto, cm, cmto, ft, ha, va, ww, gd);

    ATextRect := ARect;
    ARectB := ARect;
    InflateRectEx(ATextRect, -2, -2);
    InflateRectEx(ARectB, -1, -1);
    s := AText;
    s := ConvertToPlainText(s);

    if AGrid.URLShow and IsURL(s) and not TAdvUtils.IsHTML(s) then
    begin
      surl := s;
      if not AGrid.URLFull then
        StripURLProtocol(surl);

      s := '<a href="'+s+'">'+surl+'</a>';
    end;

    tr := RectF(0, 0, 0, 0);
    if s <> '' then
    begin
      APDFLib.Graphics.Font.Name := ft.Name;
      APDFLib.Graphics.Font.Color := ft.Color;
      if ft.Size > 0 then
        APDFLib.Graphics.Font.Size := ft.Size * AScale
      else
        APDFLib.Graphics.Font.Size := 12 * AScale;
      APDFLib.Graphics.Font.Style := ft.Style;

      if ww then
      begin
        if TAdvUtils.IsHTMLUnicode(s) then
          tr := APDFLib.Graphics.DrawHTMLText(s, ATextRect, False, AScale, True)
        else
          tr := APDFLib.Graphics.DrawText(s, ATextRect, True);
      end
      else
      begin
        if TAdvUtils.IsHTMLUnicode(s) then
          tr := APDFLib.Graphics.DrawHTMLText(s, RectF(0, 0, 10000, 10000), False, AScale, True)
        else
          tr := APDFLib.Graphics.DrawText(s, RectF(0, 0, 10000, 10000), True);
      end;

      tr.Width := tr.Width + (2 * AScale);
    end;

    szs := TAdvStringGridOpen(AGrid).GetPrintGraphicSize(ACol, ARow, Round(ARect.Width), Round(ARect.Height), 1.0);
    sz := PointF(szs.X * AScale, szs.Y * AScale);

    cl := AGrid.CellGraphics[ACol, ARow];

    case AGrid.CellTypes[ACol, ARow] of
      ctBitmap, ctPicture, ctFilePicture:
      begin
        if Assigned(cl) then
          bmp.Items.Add.Picture.Assign(cl.CellBitmap);
      end;
      ctIcon:
      begin
        if Assigned(cl) then
          bmp.Items.Add.Picture.Assign(cl.CellIcon);
      end;
      ctImages:
      begin
        if Assigned(cl) then
        begin
          img := TAdvStringGridOpen(AGrid).GetCellImageList(ACol, ARow);
          if Assigned(img) then
          begin
            sz.X := img.Width;
            sz.Y := img.Height;
            for I := 0 to TIntList(cl.CellBitmap).Count - 1 do
            begin
              ico := TIcon.Create;
              AGrid.GridImages.GetIcon(TIntList(cl.CellBitmap)[I], ico);
              if Assigned(ico) then
              begin
                if not ico.Empty then
                  bmp.Items.Add.Picture.Assign(ico);
                ico.Free;
              end;
            end;
          end;
        end;
      end;
      ctDataImage:
      begin
        ico := TIcon.Create;
        AGrid.GridImages.GetIcon(AGrid.Ints[ACol, ARow], ico);
        if Assigned(ico) then
        begin
          if not ico.Empty then
            bmp.Items.Add.Picture.Assign(ico);
          ico.Free;
        end;
      end;
      ctImageList:
      begin
        if Assigned(cl) then
        begin
          ico := TIcon.Create;
          AGrid.GridImages.GetIcon(cl.CellIndex, ico);
          if Assigned(ico) then
          begin
            if not ico.Empty then
              bmp.Items.Add.Picture.Assign(ico);
            ico.Free;
          end;
        end;
      end;
    end;

    if Assigned(cl) then
    begin
      case cl.CellHAlign of
        haLeft:
        begin
          ABitmapRect.Left := ARectB.Left;
          ABitmapRect.Right := ABitmapRect.Left + sz.X;
        end;
        haRight:
        begin
          ABitmapRect.Right := ARectB.Right;
          ABitmapRect.Left := ABitmapRect.Right - sz.X;
        end;
        haCenter:
        begin
          if (sz.X < ARectB.Width) then
          begin
            ABitmapRect.Left := ARectB.Left + (Round((ARectB.Width - sz.X)) shr 1);
            ABitmapRect.Right := ABitmapRect.Left + sz.X;
          end
          else
          begin
            ABitmapRect.Left := ARectB.Left;
            ABitmapRect.Right := ABitmapRect.Left + sz.X;
          end;
        end;
        haBeforeText:
        begin
          case ha of
            taLeftJustify:
            begin
              ABitmapRect.Left := ARectB.Left;
              ABitmapRect.Right := ABitmapRect.Left + sz.X;
            end;
            taRightJustify:
            begin
              ABitmapRect.Left := ARectB.Right - tr.Width - sz.X;
              ABitmapRect.Right := ABitmapRect.Left + sz.X;
              if ABitmapRect.Left < ARectB.Left then
                ABitmapRect.Left := ARectB.Left;
            end;
            taCenter:
            begin
              ABitmapRect.Left := ARectB.Left + ((ARectB.Width - tr.Width - sz.X) / 2);
              ABitmapRect.Right := ARectB.Left + sz.X;
              if ABitmapRect.Left < ARectB.Left then
                ABitmapRect.Left := ARectB.Left;
            end;
          end;
        end;
        haAfterText:
        begin
          case ha of
            taLeftJustify:
            begin
              ABitmapRect.Left := ARectB.Left + tr.Width;
              ABitmapRect.Right := ABitmapRect.Left + sz.X;
            end;
            taRightJustify:
            begin
              ABitmapRect.Right := ARectB.Right;
              ABitmapRect.Left := ABitmapRect.Right - sz.X;
            end;
            taCenter:
            begin
              ABitmapRect.Left := ARectB.Left + tr.Width + ((ARectB.Width - tr.Width - sz.X) / 2);
              ABitmapRect.Right := ABitmapRect.Left + sz.X;
              if ABitmapRect.Left < ARectB.Left then
                ABitmapRect.Left := ARectB.Left;
            end;
          end;
        end;
        haFull:
        begin
          ABitmapRect.Right := ARectB.Right;
          ABitmapRect.Left := ARectB.Left;
        end;
      end;

      case cl.CellVAlign of
        vaTop, vaAboveText:
        begin
          ABitmapRect.Top := ARectB.Top;
          ABitmapRect.Bottom := ABitmapRect.Top + sz.Y;
        end;
        vaBottom:
        begin
          ABitmapRect.Bottom := ARectB.Bottom;
          ABitmapRect.Top := ABitmapRect.Bottom - sz.Y;
        end;
        vaCenter:
        begin
          if sz.Y < ARectB.Height then
          begin
            ABitmapRect.Top := ARectB.Top + (Round(ARectB.Height - sz.Y) shr 1);
            ABitmapRect.Bottom := ABitmapRect.Top + sz.Y;
          end
          else
          begin
            ABitmapRect.Top := ARectB.Top;
            ABitmapRect.Bottom := ABitmapRect.Top + sz.Y;
          end;
        end;
        vaUnderText:
        begin
          ABitmapRect.Top := ARectB.Bottom - sz.Y;
          ABitmapRect.Bottom := ARectB.Bottom;
        end;
        vaFull:
        begin
          ABitmapRect.Top := ARectB.Top;
          ABitmapRect.Bottom := ARectB.Bottom;
        end;
      end;
    end;

    case AGrid.CellTypes[ACol, ARow] of
      ctCheckBox,ctDataCheckBox,ctTriStateCheckBox,ctVirtCheckBox,ctRowCheckBox,ctRadioButton:
      begin
        if Assigned(AGrid.OnGetAlignment) or TAdvStringGridOpen(AGrid).HasColumnsProp then
        begin
          case ha of
            taLeftJustify:
            begin
              ABitmapRect.Left := ARectB.Left;
              ABitmapRect.Right := ABitmapRect.Left + sz.X;
            end;
            taRightJustify:
            begin
              if AGrid.CellTypes[ACol, ARow] in [ctDataCheckBox,ctVirtCheckBox,ctRowCheckBox] then
                ABitmapRect.Left := ARectB.Right - sz.X
              else
                ABitmapRect.Left := ARectB.Right - tr.Width - sz.X;

              if ABitmapRect.Left < ARectB.Left then
                ABitmapRect.Left := ARectB.Left;
              ABitmapRect.Right := ABitmapRect.Left + sz.X;
            end;
            taCenter:
            begin
              if AGrid.CellTypes[ACol, ARow] in [ctDataCheckBox,ctVirtCheckBox,ctRowCheckBox] then
                ABitmapRect.Left := ARectB.Left + Max(0, Round((ARectB.Right - sz.X - ARectB.Left)) shr 1)
              else
              begin
                if tr.Width > 0 then
                  ABitmapRect.Left := ARectB.Left - sz.X + Max(0, Round((ARectB.Right - tr.Width - ARectB.Left)) shr 1)
                else
                  ABitmapRect.Left := ARectB.Left + Max(0, Round((ARectB.Right - sz.X - ARectB.Left)) shr 1);
              end;

              if ABitmapRect.Left < ARectB.Left then ABitmapRect.Left := ARectB.Left + 1;
                ABitmapRect.Right := ABitmapRect.Left + sz.X;
            end;
          end;
        end;
      end;
    end;

    if (ha = taLeftJustify) and (Assigned(cl) and (cl.CellHAlign = haBeforeText)) then
      ATextRect.Left := ATextRect.Left + sz.X;

    if (ha in [taLeftJustify,taCenter]) and (Assigned(cl) and (cl.CellHAlign = haAfterText)) then
      ATextRect.Right := ATextRect.Right - sz.X;

    if (ha = taRightJustify) and (Assigned(cl) and (cl.CellHAlign = haAfterText)) then
      ATextRect.Right := ATextRect.Right - sz.X;

    if (ha in [taRightJustify,taCenter]) and (Assigned(cl) and (cl.CellHAlign = haBeforeText)) then
      ATextRect.Left := ATextRect.Left + sz.X;

    if Assigned(cl) and (cl.CellVAlign = vaAboveText) then
      ATextRect.Top := ATextRect.Top + sz.Y;

    if Assigned(cl) and (cl.CellVAlign = vaUnderText) then
      ATextRect.Bottom := ATextRect.Bottom - sz.Y;

    APDFLib.Graphics.Stroke.Kind := gskNone;
    if ADrawBackGround then
    begin
      if (cto <> clNone) then
      begin
        APDFLib.Graphics.Fill.Kind := gfkGradient;
        APDFLib.Graphics.Fill.Color := b.Color;
        APDFLib.Graphics.Fill.ColorTo := cto;
      end
      else
      begin
        APDFLib.Graphics.Fill.Kind := gfkSolid;
        APDFLib.Graphics.Fill.Color := b.Color;
        APDFLib.Graphics.Fill.ColorTo := gcNull;
      end;

      APDFLib.Graphics.DrawRectangle(ARect);
    end;

    APDFLib.Graphics.Stroke.Width := bw * AScale;
    APDFLib.Graphics.Stroke.Kind := gskSolid;
    APDFLib.Graphics.Stroke.Color := bc;

    if (s <> '') and ADrawText and not (AGrid.CellTypes[ACol, ARow] in [ctRating, ctProgress, ctRangeIndicator, ctXPProgress,
      ctDataCheckBox, ctVirtCheckBox, ctRowCheckBox, ctRadio, ctDataPicture, ctDataImage]) then
    begin
      case ha of
        taCenter:
        begin
          case va of
            vtaCenter: tr := RectF(ATextRect.Left + (ATextRect.Width - tr.Width) / 2, ATextRect.Top + (ATextRect.Height - tr.Height) / 2, ATextRect.Left  + (ATextRect.Width - tr.Width) / 2 + tr.Width, ATextRect.Top + (ATextRect.Height - tr.Height) / 2 + tr.Height);
            vtaTop: tr := RectF(ATextRect.Left + (ATextRect.Width - tr.Width) / 2, ATextRect.Top, ATextRect.Left + (ATextRect.Width - tr.Width) / 2 + tr.Width, ATextRect.Top + tr.Height);
            vtaBottom: tr := RectF(ATextRect.Left + (ATextRect.Width - tr.Width) / 2, ATextRect.Bottom - tr.Height, ATextRect.Left + (ATextRect.Width - tr.Width) / 2 + tr.Width, ATextRect.Bottom);
          end;
        end;
        taLeftJustify:
        begin
          case va of
            vtaCenter: tr := RectF(ATextRect.Left, ATextRect.Top + (ATextRect.Height - tr.Height) / 2, ATextRect.Left + tr.Width, ATextRect.Top + (ATextRect.Height - tr.Height) / 2 + tr.Height);
            vtaTop: tr := RectF(ATextRect.Left, ATextRect.Top, ATextRect.Left + tr.Width, ATextRect.Top + tr.Height);
            vtaBottom: tr := RectF(ATextRect.Left, ATextRect.Bottom - tr.Height, ATextRect.Left + tr.Width, ATextRect.Bottom);
          end;
        end;
        taRightJustify:
        begin
          case va of
            vtaCenter: tr := RectF(ATextRect.Right - tr.Width, ATextRect.Top + (ATextRect.Height - tr.Height) / 2, ATextRect.Right, ATextRect.Top + (ATextRect.Height - tr.Height) / 2 + tr.Height);
            vtaTop: tr := RectF(ATextRect.Right - tr.Width, ATextRect.Top, ATextRect.Right, ATextRect.Top + tr.Height);
            vtaBottom: tr := RectF(ATextRect.Right - tr.Width, ATextRect.Bottom - tr.Height, ATextRect.Right, ATextRect.Bottom);
          end;
        end;
      end;

      tr := RectF(Max(ARect.Left, tr.Left), Max(ARect.Top, tr.Top), Min(ARect.Right, tr.Right), Min(ARect.Bottom, tr.Bottom));

      if TAdvUtils.IsHTMLUnicode(s) then
        APDFLib.Graphics.DrawHTMLText(s, PointF(tr.Left, tr.Top), AScale)
      else
        APDFLib.Graphics.DrawText(s, tr)
    end;

    APDFLib.Graphics.Stroke.Kind := gskSolid;
    APDFLib.Graphics.Stroke.Color := bc;
    APDFLib.Graphics.Fill.Color := gcNull;
    APDFLib.Graphics.Fill.ColorTo := gcNull;

    if AGrid.IsFixed(ACol, ARow) then
    begin
      if (goFixedVertLine in AGrid.Options) and (goFixedHorzLine in AGrid.Options) then
        APDFLib.Graphics.DrawRectangle(ARect)
      else if goFixedVertLine in AGrid.Options then
      begin
        APDFLib.Graphics.DrawLine(PointF(ARect.Left, ARect.Top), PointF(ARect.Left, ARect.Bottom));
        APDFLib.Graphics.DrawLine(PointF(ARect.Right, ARect.Top), PointF(ARect.Right, ARect.Bottom));
      end
      else if goFixedHorzLine in AGrid.Options then
      begin
        APDFLib.Graphics.DrawLine(PointF(ARect.Left, ARect.Top), PointF(ARect.Right, ARect.Top));
        APDFLib.Graphics.DrawLine(PointF(ARect.Left, ARect.Bottom), PointF(ARect.Right, ARect.Bottom));
      end;
    end
    else
    begin
      if (goVertLine in AGrid.Options) and (goHorzLine in AGrid.Options) then
        APDFLib.Graphics.DrawRectangle(ARect)
      else if goVertLine in AGrid.Options then
      begin
        APDFLib.Graphics.DrawLine(PointF(ARect.Left, ARect.Top), PointF(ARect.Left, ARect.Bottom));
        APDFLib.Graphics.DrawLine(PointF(ARect.Right, ARect.Top), PointF(ARect.Right, ARect.Bottom));
      end
      else if goHorzLine in AGrid.Options then
      begin
        APDFLib.Graphics.DrawLine(PointF(ARect.Left, ARect.Top), PointF(ARect.Right, ARect.Top));
        APDFLib.Graphics.DrawLine(PointF(ARect.Left, ARect.Bottom), PointF(ARect.Right, ARect.Bottom));
      end;
    end;

    if APDFIO.Options.ExportImages then
    begin
      pic := TPicture.Create;
      try
        if bmp.Items.Count = 1 then
        begin
          pic.Assign(bmp.Items[0].Picture);
          if Assigned(cl) and (TStretchMode(cl.CellAngle) = Shrink) then
            APDFLib.Graphics.DrawImage(pic, b.Color, ABitmapRect, True, False)
          else if Assigned(cl) and (TStretchMode(cl.CellAngle) = ShrinkWithAspectRatio) then
            APDFLib.Graphics.DrawImage(pic, b.Color, ABitmapRect, True, True)
          else
            APDFLib.Graphics.DrawImage(pic, b.Color, ABitmapRect);
        end
        else if bmp.Items.Count > 0 then
        begin
          for I := 0 to bmp.Items.Count -  1 do
          begin
            pic.Assign(bmp.Items[I].Picture);
            if Assigned(cl) and (TStretchMode(cl.CellAngle) = Shrink) then
              APDFLib.Graphics.DrawImage(pic, b.Color, ABitmapRect, True, False)
            else if Assigned(cl) and (TStretchMode(cl.CellAngle) = ShrinkWithAspectRatio) then
              APDFLib.Graphics.DrawImage(pic, b.Color, ABitmapRect, True, True)
            else
              APDFLib.Graphics.DrawImage(pic, b.Color, ABitmapRect);

            ABitmapRect.Left := ABitmapRect.Left + pic.Width;
            ABitmapRect.Right := ABitmapRect.Right + pic.Width;
          end;
        end;
      finally
        pic.free;
      end;
    end;

    case AGrid.CellTypes[ACol, ARow] of
      ctDataCheckBox, ctCheckBox, ctVirtCheckBox, ctRowCheckBox, ctTriStateCheckBox:
      begin
        if Assigned(cl) then
          DrawCheckBox(ABitmapRect, cl.CellBoolean);
      end;
      ctRadio, ctRadioButton:
      begin
        if Assigned(cl) then
          DrawRadioButton(ABitmapRect, cl.CellBoolean);
      end;
      ctXPProgress, ctProgress:
      begin
        if Assigned(cl) then
        begin
          if cl.CellText = '' then
            DrawProgressBar(ABitmapRect, AGrid.Floats[ACol, ARow], '%.0f%%', cl.CellErrLen)
          else
            DrawProgressBar(ABitmapRect, AGrid.Floats[ACol, ARow], cl.CellText, cl.CellErrLen);
        end;
      end;
      ctShape:
      begin
        if Assigned(cl) then
          DrawShape(ABitmapRect.Left, ABitmapRect.Top, ABitmapRect.Right - ABitmapRect.Left - 1 * AScale,
            ABitmapRect.Bottom - ABitmapRect.Top - 1 * AScale, TCellShape(cl.CellAngle), TColor(cl.CellIndex), TColor(cl.CellBitmap));
      end;
      ctRating:
      begin
        if Assigned(cl) then
        begin
          rv := Trunc(AGrid.Floats[ACol,ARow] * 2) / 2;

          for idx := 1 to cl.CellAngle do
          begin
            if idx <= rv then
              DrawShape(ABitmapRect.Left, ABitmapRect.Top, 16 * AScale, 16 * AScale, csStar, TColor(cl.CellIndex), TColor(cl.CellIndex))
            else
              DrawShape(ABitmapRect.Left, ABitmapRect.Top, 16 * AScale, 16 * AScale, csStar, TColor(cl.CellBitmap), TColor(cl.CellBitmap));

            if (idx > rv) and (idx - 1 < rv) then
              DrawShape(ABitmapRect.Left, ABitmapRect.Top, 16 * AScale, 16 * AScale, csHalfStar, TColor(cl.CellIndex), TColor(cl.CellIndex));

            ABitmapRect.Left := ABitmapRect.Left + 18 * AScale;
          end;
        end;
      end;
      ctNode:
      begin
        if Assigned(cl) then
        begin
          r := ARect;
          dr := r;
          r.Left := r.Left + TAdvStringGridOpen(AGrid).NodeIndent(ARow) - AGrid.CellNode.NodeIndent;

          if AGrid.CellNode.ShowTree and (ARow >= AGrid.FixedRows) then
          begin
            APDFLib.Graphics.Stroke.Color := AGrid.CellNode.TreeColor;
            APDFLib.Graphics.Stroke.Width := 1 * AScale;

            if (TAdvStringGridOpen(AGrid).NodeIndent(ARow + 1) > 0) and (ARow + 1 < AGrid.RowCount) and not (AGrid.NodeState[ARow] and (TAdvStringGridOpen(AGrid).NodeIndent(ARow + 1) < TAdvStringGridOpen(AGrid).NodeIndent(ARow))) then
              APDFLib.Graphics.DrawLine(PointF(r.Left + 2 * AScale + AGrid.CellNode.NodeIndent / 2,r.Top + (r.Bottom - r.Top) / 2), PointF(r.Left + 2 + AGrid.CellNode.NodeIndent / 2,r.Bottom));

            if (TAdvStringGridOpen(AGrid).NodeIndent(ARow - 1) >= TAdvStringGridOpen(AGrid).NodeIndent(ARow)) then
              APDFLib.Graphics.DrawLine(PointF(r.Left + 2 * AScale + AGrid.CellNode.NodeIndent / 2,r.Top), PointF(r.Left + 2 * AScale + AGrid.CellNode.NodeIndent / 2,r.Top + (r.Bottom - r.Top) / 2));

            if (ACol < AGrid.FixedCols) and not AGrid.Flat and (AGrid.Look in [glTMS,glXP,glListView,glSoft]) then
              r.Left := r.Left + 1 * AScale;
          end;

          r := dr;

          r.Left := r.Left + TAdvStringGridOpen(AGrid).NodeIndent(ARow) - AGrid.CellNode.NodeIndent / 2 - 4 * AScale;

          if Canvas.Brush.Color <> clNone then
            brshColor := Canvas.Brush.Color
          else
            if AGrid.FixedCols > 0 then
              brshColor := AGrid.FixedColor
            else
              brshColor := AGrid.Color;

          if TAdvStringGridOpen(AGrid).CanShowSelection and (((gdSelected in st) and not AGrid.MouseActions.DisjunctRowSelect) or (AGrid.MouseActions.DisjunctRowSelect and
             AGrid.RowSelect[ARow] and (goRowSelect in AGrid.Options) and (ACol >= AGrid.FixedCols)) ) then
             brshColor := AGrid.SelectionColor;

          if AGrid.CellNode.NodeType = cn3D then
          begin
            APDFLib.Graphics.Fill.Color := AGrid.CellNode.Color;
            APDFLib.Graphics.DrawRectangle(r);
          end;

          if (AGrid.CellNode.NodeType = cnLeaf) then
          begin
            OffsetRectEx(r,0,(r.Bottom - r.Top - 12 * AScale) / 2);

            if cl.CellBoolean then
              DrawBitmapResourceTranspPDF(brshColor,r,'ASGLEAFCLOSE')
            else
              DrawBitmapResourceTranspPDF(brshColor,r,'ASGLEAFOPEN');
            Exit;
          end;

          if (AGrid.CellNode.NodeType = cnXP) then
          begin
            OffsetRectEx(r,4,(r.Bottom - r.Top - 10 * AScale) / 2);

            if cl.CellBoolean then
              DrawBitmapResourceTranspPDF(brshColor,r,'XPNODEC')
            else
              DrawBitmapResourceTranspPDF(brshColor,r,'XPNODEO');
            Exit;
          end;

          if (AGrid.CellNode.NodeType = cnGlyph) and
             (not AGrid.CellNode.ExpandGlyph.Empty) and
             (not AGrid.CellNode.ContractGlyph.Empty) then
          begin
            cr := r;
            cr.Top := cr.Top + (r.Bottom - r.Top - AGrid.CellNode.ContractGlyph.Height) / 2;

            if cl.CellBoolean then
              DrawBitmapTranspPDF(AGrid.CellNode.ContractGlyph, brshColor, cr)
            else
              DrawBitmapTranspPDF(AGrid.CellNode.ExpandGlyph, brshColor, cr);

            Exit;
          end;

          APDFLib.Graphics.Fill.Color := AGrid.CellNode.Color;
          r.Left := r.Left + 4 * AScale;
          r.Right := r.Left + 8 * AScale;
          r.Top := r.Top + (MaxI(0, Round(r.Bottom - r.Top - 8 * AScale)) shr 1);
          r.Bottom := r.Top + 8 * AScale;

          if AGrid.CellNode.NodeType = cnFlat then
          begin
            APDFLib.Graphics.Stroke.Color := AGrid.CellNode.NodeColor;
            APDFLib.Graphics.DrawRectangle(RectF(r.Left,r.Top,r.Right + 1 * AScale,r.Bottom + 1 * AScale));
            if cl.CellBoolean then
            begin
              APDFLib.Graphics.DrawLine(PointF(r.Left + 2 * AScale,r.Top+4 * AScale), PointF(r.Left + 7 * AScale,r.Top+4 * AScale));
              APDFLib.Graphics.DrawLine(PointF(r.Left + 4 * AScale,r.Top+2 * AScale), PointF(r.Left + 4 * AScale,r.Top+7 * AScale));
            end
            else
              APDFLib.Graphics.DrawLine(PointF(r.Left + 2 * AScale,r.Top + 4 * AScale), PointF(r.Left + 7 * AScale,r.Top + 4 * AScale));
          end
          else
          begin
          end;
        end;
      end;
      ctButton, ctBitButton, ctExpand:;
      ctProgressPie:
      begin
        if Assigned(cl) then
          DrawProgressPie(RectF(ABitmapRect.left, ABitmapRect.Top, ABitmapRect.left + 20 * AScale, ABitmapRect.Top + 20 * AScale),TColor(cl.CellBitmap),cl.CellAngle);
      end;
      ctRangeIndicator:
      begin
        if Assigned(cl) then
        begin
          r := ARect;
          InflateRectEx(r,- AGrid.ControlLook.ProgressMarginX * AScale,-AGrid.ControlLook.ProgressMarginY * AScale);
          DrawRangeIndicator(r,AGrid.Ints[ACol,ARow],cl.CellIndex,cl.CellBoolean,TColor(cl.CellBitmap),TColor(cl.CellIcon));
        end;
      end;
    end;

    if Assigned(cl) and (cl.CellComment <> '') then
    begin
      r := ABitmapRect;
      APDFLib.Graphics.Stroke.Kind := gskSolid;
      APDFLib.Graphics.Stroke.Color := AGrid.ControlLook.CommentColor;
      if TColor(cl.CellIndex) <> clNone then
        APDFLib.Graphics.Stroke.Color := cl.CommentColor;

      if APDFLib.Graphics.Stroke.Color <> clNone then
      begin
        APDFLib.Graphics.Fill.Color := APDFLib.Graphics.Stroke.Color;
        APDFLib.Graphics.DrawPathBegin;
        APDFLib.Graphics.DrawPathMoveToPoint(PointF(r.Right - 8 * AScale, r.Top + 1 * AScale));
        APDFLib.Graphics.DrawPathAddLineToPoint(PointF(r.Right - 3 * AScale, r.Top + 1 * AScale));
        APDFLib.Graphics.DrawPathAddLineToPoint(PointF(r.Right - 3 * AScale, r.Top + 6 * AScale));
        APDFLib.Graphics.DrawPathEnd;
      end;
    end;
  finally
    b.Free;
    ft.Free;
    bmp.Free;
  end;
end;

constructor TAdvCustomGridPDFIO.Create(AOwner: TComponent);
var
  I: Integer;
  FDesignTime: boolean;
begin
  inherited;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime and Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for I := 0 to AOwner.ComponentCount - 1 do
    begin
      if (AOwner.Components[i] is TAdvStringGrid) then
      begin
        Grid := AOwner.Components[i] as TAdvStringGrid;
        Break;
      end;
    end;
  end;
end;

function TAdvCustomGridPDFIO.CreateOptions: TAdvPDFIOOptions;
begin
  Result := TAdvGridPDFIOOptions.Create;
end;

procedure TAdvCustomGridPDFIO.DoRowIsPageBreak(ARow: integer;
  var IsPageBreak: boolean);
begin
  if Assigned(OnRowIsPageBreak) then
    OnRowIsPageBreak(Self, Arow, IsPageBreak);
end;

procedure TAdvCustomGridPDFIO.DoPDFExport(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject);
var
  pgb: array of TAdvGridPDFIOCellRange;
  I: Integer;
  pb: Boolean;
  pi: integer;
  g: TAdvStringGrid;
begin
  if not Assigned(AExportObject) or (Assigned(AExportObject) and not (AExportObject is TAdvStringGrid)) then
    raise Exception.Create(sTMSGridPDFIOGridNotAssigned);


  g := AExportObject as TAdvStringGrid;
  pi := 0;
  pgb := nil;
  for I := 0 to g.RowCount - 1 do
  begin
    pb := False;
    DoRowIsPageBreak(I, pb);
    if pb then
    begin
      SetLength(pgb, Length(pgb) + 1);
      pgb[Length(pgb) - 1] := CellRange(0, pi, g.AllColCount - 1, I - 1);
      pi := I;
    end;
  end;

  if (pi < g.RowCount - 1) or (pi = 0) then
  begin
    SetLength(pgb, Length(pgb) + 1);
    pgb[Length(pgb) - 1] := CellRange(0, pi, g.AllColCount - 1, g.RowCount - 1);
  end;

  for I := 0 to Length(pgb) - 1 do
    DoPDFGridExport(APDFLib, g, pgb[I]);
end;

procedure TAdvCustomGridPDFIO.DOPDFGridExport(const APDFLib: TAdvPDFLib; const AGrid: TAdvStringGrid; const ASelection: TAdvGridPDFIOCellRange);
var
  c, r: Integer;
  cellstr: String;
  cwr, rhr, cw, rh, cwrt, rhrt: Single;
  I, J: Integer;
  xr, yr: Single;
  bc, br: Integer;
  cs, rs: Integer;
  cellr: TRectF;
  x, y: Single;
  rSt, csave, cSt: Integer;
  chk: Boolean;
  FFixedRowsSave, FFixedColumnsSave: Integer;
  FCSave, FRSave: Integer;
  FFixedRowGet, FFixedColGet: Boolean;
  FSetRSave, FSetCSave: Boolean;
  xs, ys: Single;
  ph, pw: Single;
  xts, yts: Single;
  FSaveXBounds: Single;
  FSaveYBounds: Single;
  w, h: Single;
  sc: Single;
  CurrentGrid: TAdvStringGridOpen;
  AMergeCellPrintPageNr: array of array of Integer;

  procedure GetCellMergeInfo(ACol, ARow: Integer; var ABaseCol, ABaseRow, AColSpan, ARowSpan: Integer);
  var
    cp: TCellPropertiesOpen;
    bc: TPoint;
  begin
    if CurrentGrid.HasCellProperties(ACol, ARow) then
    begin
      cp := TCellPropertiesOpen(CurrentGrid.GetCellProperties(ACol, ARow));
      if Assigned(cp) then
      begin
        bc := cp.BaseCell[ACol, ARow];
        ABaseCol := bc.X;
        ABaseRow := bc.Y;
        AColSpan := cp.CellSpanX + 1;
        ARowSpan := cp.CellSpanY + 1;
      end;
    end;
  end;

  function GetFixedPrintWidth: Single;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to CurrentGrid.FixedCols - 1 do
      Result := Result + CurrentGrid.AllColWidths[I] * sc;
  end;

  function GetFixedPrintHeight: Single;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to CurrentGrid.FixedRows - 1 do
      Result := Result + CurrentGrid.RowHeights[I] * sc;
  end;

  function IsHiddenColumn(Colindex: Integer): Boolean;
  begin
    if (ColIndex < CurrentGrid.AllColCount) then
      Result := not CurrentGrid.VisibleCol[Colindex]
    else
      Result := False;
  end;

  function GetScale: Single;
  var
    K: Integer;
  begin
    Result := 1;
    if Options.FitToPage then
    begin
      w := 0;
      for K := 0 to CurrentGrid.AllColCount - 1 do
      begin
        if not IsHiddenColumn(K) then
          w := w + Round(CurrentGrid.AllColWidths[K]);
      end;

      if w > 0 then
        Result := (APDFLib.MediaBox.Width - Options.Margins.Left - Options.Margins.Right) / w;
    end;
  end;

begin
  CurrentGrid := TAdvStringGridOpen(AGrid);
  APDFLib.PictureContainer := CurrentGrid.PictureContainer;
  sc := GetScale;
  NewPage(APDFLib, CurrentGrid);

  w := APDFLib.MediaBox.width;
  h := APDFLib.MediaBox.height;

  x := Options.Margins.Left;
  y := Options.Margins.Top;

  rSt := ASelection.StartRow;
  FFixedRowsSave := 0;
  FFixedColumnsSave := 0;
  cSave := -1;
  cSt := -1;
  FFixedRowGet := False;
  FFixedColGet := False;
  FSetRSave := True;
  FSetCSave := True;

  pw := GetFixedPrintWidth;
  ph := GetFixedPrintHeight;

  FSaveXBounds := Options.Margins.Left;

  for I := ASelection.StartCol to ASelection.EndCol do
  begin
    if not IsHiddenColumn(I) then
    begin
      if (CompareValue(Floor(FSaveXBounds + CurrentGrid.AllColWidths[I] * sc), w - Options.Margins.Right) = GreaterThanValue) then
        Break;

      FSaveXBounds := FSaveXBounds + CurrentGrid.AllColWidths[I] * sc;
    end;
  end;

  FSaveYBounds := Options.Margins.Top;

  for I := ASelection.StartRow to ASelection.EndRow do
  begin
    if (CompareValue(Floor(FSaveYBounds + CurrentGrid.RowHeights[I] * sc), h - Options.Margins.Bottom) = GreaterThanValue) then
      Break;

    FSaveYBounds := FSaveYBounds + CurrentGrid.RowHeights[I] * sc;
  end;

  c := ASelection.StartCol;

  SetLength(AMergeCellPrintPageNr, ASelection.EndCol - ASelection.StartCol + 1, ASelection.EndRow - ASelection.StartRow + 1);

  for i := ASelection.StartCol to ASelection.EndCol do
    for j := ASelection.StartRow to ASelection.EndRow do
      AMergeCellPrintPageNr[i - ASelection.StartCol, j - ASelection.StartRow] := -1;

  CurrentGrid.ExportNotification(esExportStart,-1);
  FCSave := -1;
  r := 0;
  while c <= ASelection.EndCol do
  begin
    if not IsHiddenColumn(c) then
    begin
      if cSt = -1 then
        cSt := c;

      if Options.RepeatFixedColumns and (CurrentGrid.FixedCols > 0) then
      begin
        if (FFixedColumnsSave < CurrentGrid.FixedCols) then
        begin
          FFixedColGet := True;
          if FSetCSave then
          begin
            FSetCSave := False;
            FCSave := cSt;
          end;

          cst := FFixedColumnsSave;
          c := cst;
          Inc(FFixedColumnsSave);
        end
        else if FFixedColGet then
        begin
          FFixedColGet := False;
          cSt := FCSave;
          c := FCSave;

          if cst < CurrentGrid.FixedCols then
          begin
            cst := CurrentGrid.FixedCols;
            c := cst;
            while IsHiddenColumn(c) do
              Inc(c);
          end;
        end;
      end;

      cw := CurrentGrid.AllColWidths[c] * sc;

      r := rst;
      FRSave := rst;
      while r <= ASelection.EndRow do
      begin
        if Options.RepeatFixedRows and (CurrentGrid.FixedRows > 0) then
        begin
          if (FFixedRowsSave < CurrentGrid.FixedRows) then
          begin
            FFixedRowGet := True;
            if FSetRSave then
            begin
              FSetRSave := False;
              FRSave := rSt;
            end;

            rSt := FFixedRowsSave;
            r := rst;
            Inc(FFixedRowsSave);
          end
          else if FFixedRowGet then
          begin
            FFixedRowGet := False;
            rSt := FRSave;
            r := FRSave;

            if rst < CurrentGrid.FixedRows then
            begin
              rst := CurrentGrid.FixedRows;
              r := rst;
            end;
          end;
        end;

        CurrentGrid.ExportNotification(esExportNewRow, r);

        bc := c;
        br := r;
        cs := 0;
        rs := 0;

        rh := CurrentGrid.RowHeights[r] * sc;

        GetCellMergeInfo(c, r, bc, br, cs, rs);

        chk := (c = bc) and (r = br);
        if chk then
          AMergeCellPrintPageNr[bc - ASelection.StartCol, br - ASelection.StartRow] := APDFLib.GetPageIndex
        else if (bc <> c) and (br <> r) then
          chk := AMergeCellPrintPageNr[bc - ASelection.StartCol, br - ASelection.StartRow] <> APDFLib.GetPageIndex;

        if chk then
        begin
          cellstr := CurrentGrid.Cells[bc, br];

          cwr := cw;
          rhr := rh;

          cwrt := 0;
          for I := bc to bc + cs - 1 do
            cwrt := cwrt + CurrentGrid.AllColWidths[I] * sc;

          if cwrt > 0 then
            cwr := cwrt;

          rhrt := 0;
          for I := br to br + rs - 1 do
            rhrt := rhrt + CurrentGrid.RowHeights[I] * sc;

          if rhrt > 0 then
            rhr := rhrt;

          xr := 0;
          for I := bc to c - 1 do
            xr := xr + CurrentGrid.AllColWidths[I] * sc;

          yr := 0;
          for I := br to r - 1 do
            yr := yr + CurrentGrid.RowHeights[I] * sc;

          xts := x - xr;
          yts := y - yr;
          xs := xts + cwr;
          ys := yts + rhr;

          if FSaveXBounds > -1 then
            xs := Min(FSaveXBounds, xs);

          if ((APDFLib.GetPageIndex = 0) or Options.RepeatFixedColumns) and (c > CurrentGrid.FixedCols) then
            xts := Max(Options.Margins.Left + pw, xts)
          else
            xts := Max(Options.Margins.Left, xts);

          if FSaveYBounds > -1 then
            ys := Min(FSaveYBounds, ys);

          if ((APDFLib.GetPageIndex = 0) or Options.RepeatFixedRows) and (r > CurrentGrid.FixedRows) then
            yts := Max(Options.Margins.Top + ph, yts)
          else
            yts := Max(Options.Margins.Top, yts);

          cellr := RectF(xts, yts, xs, ys);
          DrawCell(Self, APDFLib, CurrentGrid, cellstr, bc, br, cellr, True, Options.CellLayout <> gclNone, sc);
        end;

        y := y + rh;
        if CompareValue(y + CurrentGrid.RowHeights[r + 1] * sc, h - Options.Margins.Bottom) = GreaterThanValue then
        begin
          FSaveYBounds := Y;
          Break;
        end;

        Inc(r);
      end;
      y := Options.Margins.Top;
      x := x + cw;
    end;

    Inc(c);

    FFixedRowsSave := 0;

    if (bc + cs = ASelection.EndCol) and (br + rs = ASelection.EndRow) then
      Break;

    if (CompareValue(Floor(x + CurrentGrid.AllColWidths[c] * sc) , w - Options.Margins.Right) = GreaterThanValue) or (c = ASelection.EndCol + 1) then
    begin
      FSaveXBounds := x;

      rst := r + 1;

      if cSave = -1 then
        cSave := c;

      if r = ASelection.EndRow + 1 then
      begin
        c := cSave;
        cst := -1;
        rst := ASelection.StartRow;
        cSave := -1;
      end
      else
        c := cst;

      FFixedRowsSave := 0;
      FFixedColumnsSave := 0;
      FSetRSave := True;
      FSetCSave := True;

      if (c < ASelection.EndCol + 1) then
      begin
        NewPage(APDFLib, CurrentGrid);
        x := Options.Margins.Left;
        y := Options.Margins.Top;
      end;
    end;
  end;
  CurrentGrid.ExportNotification(esExportDone,-1);
end;

procedure TAdvCustomGridPDFIO.SetGrid(const Value: TAdvStringGrid);
begin
  inherited ExportObject := Value;
end;

procedure TAdvCustomGridPDFIO.SetOptions(const Value: TAdvGridPDFIOOptions);
begin
  Options.Assign(Value);
end;

{ TAdvGridPDFIOOptions }

procedure TAdvGridPDFIOOptions.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvGridPDFIOOptions then
  begin
    FFitToPage := (Source as TAdvGridPDFIOOptions).FitToPage;
    FRepeatFixedRows := (Source as TAdvGridPDFIOOptions).RepeatFixedRows;
    FRepeatFixedColumns := (Source as TAdvGridPDFIOOptions).RepeatFixedColumns;
    FCellLayout := (Source as TAdvGridPDFIOOptions).CellLayout;
  end;
end;

constructor TAdvGridPDFIOOptions.Create;
begin
  inherited;
  FFitToPage := True;
  FRepeatFixedRows := False;
  FRepeatFixedColumns := False;
  FCellLayout := gclFull;
end;

end.

