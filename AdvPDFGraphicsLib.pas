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

unit AdvPDFGraphicsLib;

{$I TMSDEFS.INC}

{$IFDEF FMXLIB}
{$IFNDEF MSWINDOWS}
{$IFNDEF LINUX}
{$DEFINE USENATIVE}
{$ENDIF}
{$ENDIF}
{$ENDIF}

interface

uses
  Classes, AdvPDFCoreLibBase, AdvPDFRichTextLib, AdvTypes, PictureContainer,
  AdvGraphicsTypes
  {$IFNDEF LCLLIB}
  ,Types, Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ;

type
  IAdvCustomPDFGraphicsLib = interface(IInterface)
  ['{EDB1C5AC-6E2B-4B0C-BA5C-884837A6DFF2}']
    procedure SetFill(const Value: TAdvPDFGraphicsFill);
    procedure SetStroke(const Value: TAdvPDFGraphicsStroke);
    procedure SetFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetURLFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetLineBreakMode(const Value: TAdvPDFGraphicsLibLineBreakMode);
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure DrawRectangle(Rect: TRectF);
    procedure DrawEllipse(Rect: TRectF);
    procedure DrawLine(StartPoint, EndPoint: TPointF);
    procedure AddURL(AText: UnicodeString; AURL: UnicodeString; ARect: TRectF);
    procedure DrawSaveState;
    procedure DrawClear(Rect: TRectF); overload;
    procedure DrawRestoreState;
    procedure DrawPathBegin;
    procedure DrawPathBeginClip;
    procedure DrawPathEndClip;
    procedure DrawPathEndLinearGradient(StartPoint, EndPoint: TPointF);
    procedure DrawPathMoveToPoint(Point: TPointF);
    procedure DrawPathAddLineToPoint(Point: TPointF);
    procedure DrawPathAddRectangle(Rect: TRectF);
    procedure DrawPathAddEllipse(Rect: TRectF);
    procedure DrawPathAddCurveToPoint(FirstControlPoint, SecondControlPoint, EndPoint: TPointF);
    procedure DrawPathAddLines(Points: TAdvPDFGraphicsLibPointArray);
    procedure DrawPathAddQuadCurveToPoint(ControlPoint: TPointF; EndPoint: TPointF);
    procedure DrawPathClose;
    procedure DrawPathEnd(DrawingMode: TAdvPDFGraphicsLibPathDrawingMode = dmPathFillStroke);
    procedure SetAlignment(const Value: TAdvGraphicsTextAlign);
    function CalculateHTMLText(Text: UnicodeString; Scale: Single = 1.0): TRectF; overload;
    function CalculateHTMLText(Text: UnicodeString; Rect: TRectF; Scale: Single = 1.0): TRectF; overload;
    function CalculateText(Text: UnicodeString): TRectF; overload;
    function CalculateText(Text: UnicodeString; Rect: TRectF): TRectF; overload;
    function CalculateTextOverflow(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0): Integer; overload;
    function CalculateTextOverflow(Text: UnicodeString; Rects: TAdvPDFGraphicsLibRectArray; Padding: Single = 5.0): Integer; overload;
    function DrawImageWithName(AName: string; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImage(ABitmap: TAdvBitmap; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageFromFile(AFileName: String; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageWithName(AName: string; ABackgroundColor: TAdvGraphicsColor; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImage(ABitmap: TAdvBitmap; ABackgroundColor: TAdvGraphicsColor; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageFromFile(AFileName: String; ABackgroundColor: TAdvGraphicsColor; Point: TPointF; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0): TRectF; overload;
    function DrawImageWithName(AName: string; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImage(ABitmap: TAdvBitmap; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageFromFile(AFileName: String; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageWithName(AName: string; ABackgroundColor: TAdvGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImage(ABitmap: TAdvBitmap; ABackgroundColor: TAdvGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawImageFromFile(AFileName: String; ABackgroundColor: TAdvGraphicsColor; Rect: TRectF; Stretch: Boolean = True; AspectRatio: Boolean = True; ImageType: TAdvPDFGraphicsLibImageType = itOriginal; Quality: Single = 1.0; Center: Boolean = True): TRectF; overload;
    function DrawHTMLText(Text: UnicodeString; Point: TPointF; Scale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    function DrawHTMLText(Text: UnicodeString; Rect: TRectF; Paging: Boolean = False; Scale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    function DrawText(Text: UnicodeString; Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawText(Text: UnicodeString; Rects: TAdvPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawText(Text: UnicodeString; Point: TPointF; Calculate: Boolean = False): TRectF; overload;
    function DrawText(Text: UnicodeString; Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function GetPictureContainer: TPictureContainer;
    function GetTextRect: TRectF;
    function GetAlignment: TAdvGraphicsTextAlign;
    function GetFill: TAdvPDFGraphicsFill;
    function GetStroke: TAdvPDFGraphicsStroke;
    function GetFont: TAdvPDFGraphicsLibFont;
    function GetURLFont: TAdvPDFGraphicsLibFont;
    function GetLineBreakMode: TAdvPDFGraphicsLibLineBreakMode;
    property Alignment: TAdvGraphicsTextAlign read GetAlignment write SetAlignment;
    property Fill: TAdvPDFGraphicsFill read GetFill write SetFill;
    property Stroke: TAdvPDFGraphicsStroke read GetStroke write SetStroke;
    property Font: TAdvPDFGraphicsLibFont read GetFont write SetFont;
    property URLFont: TAdvPDFGraphicsLibFont read GetURLFont write SetURLFont;
    property LineBreakMode: TAdvPDFGraphicsLibLineBreakMode read GetLineBreakMode write SetLineBreakMode;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
  end;

  IAdvCustomPDFInitializationLib = interface(IInterface)
  ['{AAC3B710-EEBB-46BE-8450-3D70123B49CA}']
    procedure SetPDFLib(APDFLib: IInterface);
    procedure SetCanvas(ACanvas: Pointer);
    procedure SetPageWidth(APageWidth: Single);
    procedure SetPageHeight(APageHeight: Single);
    procedure SetOnNotifyNewPage(const Value: TNotifyEvent);
    procedure InitializeAppearance;
    procedure NotifyNewPage;
    function GetPageWidth: Single;
    function GetPageHeight: Single;
    function GetOnNotifyNewPage: TNotifyEvent;
    property OnNotifyNewPage: TNotifyEvent read GetOnNotifyNewPage write SetOnNotifyNewPage;
  end;

  IAdvCustomPDFGraphicsExLib = interface(IInterface)
  ['{4BA4B9BC-CBD2-4852-BE93-573753A37365}']
    procedure SetPDFRichTextLib(const Value: IAdvCustomPDFRichTextLib);
    procedure DrawAddShadow(Offset: TPointF; Blur: Single); overload;
    procedure DrawAddShadow(Offset: TPointF; Blur: Single; Color: TAdvGraphicsColor); overload;
    procedure DrawRoundedRectangle(Rect: TRectF; Rounding: Single);
    procedure DrawPathAddArc(CenterPoint: TPointF; Radius: Single; StartAngle, EndAngle: Single; Clockwise: Boolean = False);
    procedure DrawPathAddArcToPoint(FirstPoint, SecondPoint: TPointF; Radius: Single);
    procedure DrawPathEndRadialGradient(StartCenter, EndCenter: TPointF; StartRadius, EndRadius: Single);
    function DrawRichText(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawRichText(Rects: TAdvPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function DrawRichText(Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function RichText: IAdvCustomPDFRichTextLib;
  end;

  IAdvPDFGraphicsLibService = interface(IInterface)
  ['{033FCA68-4F8F-4916-9B59-67FF54E18BCC}']
    function CreatePDFGraphicsLib: TObject;
  end;

  IAdvPDFGraphicsLibGeneralService = interface(IInterface)
  ['{0F974932-4204-489D-95DE-DBEFF4DEF5D7}']
    function CreatePDFGraphicsLib: TObject;
  end;

  { TAdvCustomPDFGraphicsLib }

  TAdvCustomPDFGraphicsLib = class(TPersistent)
  private
    FPDFGraphicsLib: IAdvCustomPDFGraphicsLib;
    FPDFGraphicsExLib: IAdvCustomPDFGraphicsExLib;
    FPDFRichTextLib: TAdvPDFRichTextLib;
    FPDFInitializationLib: IAdvCustomPDFInitializationLib;
  public
    constructor Create; overload; virtual;
    constructor Create({%H-}AUseNativePDFImplementation: Boolean); reintroduce; overload; virtual;
    destructor Destroy; override;
    function GetPDFGraphicsLib: IAdvCustomPDFGraphicsLib;
    function GetPDFGraphicsExLib: IAdvCustomPDFGraphicsExLib;
    function GetPDFInitializationLib: IAdvCustomPDFInitializationLib;
  end;

  TAdvPDFGraphicsLibList = TList<TObject>;

  TAdvPDFGraphicsLibFactoryService = class abstract(TInterfacedObject, IAdvPDFGraphicsLibService, IAdvPDFGraphicsLibGeneralService)
  private
    FPDFGraphicsLibs: TAdvPDFGraphicsLibList;
  protected
    function DoCreatePDFGraphicsLib: TObject; virtual; abstract;
    function CreatePDFGraphicsLib: TObject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAdvPDFGraphicsLib = class(TAdvCustomPDFGraphicsLib)
  end;

implementation

uses
{$IFDEF MACOS}
{$IFDEF IOS}
  AdvPDFGraphicsLib.iOS,
{$ELSE}
  AdvPDFGraphicsLib.Mac,
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  AdvPDFGraphicsLib.Android,
{$ENDIF}
  AdvPDFGraphicsLib.General,
  SysUtils
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  ;

{ TAdvPDFGraphicsLibFactoryService }

constructor TAdvPDFGraphicsLibFactoryService.Create;
begin
  inherited Create;
  FPDFGraphicsLibs := TAdvPDFGraphicsLibList.Create;
end;

function TAdvPDFGraphicsLibFactoryService.CreatePDFGraphicsLib: TObject;
begin
  Result := DoCreatePDFGraphicsLib;
  FPDFGraphicsLibs.Add(Result);
end;

destructor TAdvPDFGraphicsLibFactoryService.Destroy;
begin
  FreeAndNil(FPDFGraphicsLibs);
  inherited Destroy;
end;

{ TAdvCustomPDFGraphicsLib }

constructor TAdvCustomPDFGraphicsLib.Create;
begin
  Create(True);
end;

constructor TAdvCustomPDFGraphicsLib.Create(
  AUseNativePDFImplementation: Boolean);
var
  PDFGraphicsLibServiceGeneral: IAdvPDFGraphicsLibGeneralService;
  {$IFDEF USENATIVE}
  PDFGraphicsLibService: IAdvPDFGraphicsLibService;
  {$ENDIF}
  o: TObject;
begin
  {$IFDEF USENATIVE}
  if AUseNativePDFImplementation then
  begin
    if TAdvPDFPlatformServices.Current.SupportsPlatformService(IAdvPDFGraphicsLibService, IInterface(PDFGraphicsLibService)) then
    begin
      o := PDFGraphicsLibService.CreatePDFGraphicsLib;
      Supports(o, IAdvCustomPDFGraphicsLib, FPDFGraphicsLib);
      Supports(o, IAdvCustomPDFGraphicsExLib, FPDFGraphicsExLib);
      Supports(o, IAdvCustomPDFInitializationLib, FPDFInitializationLib);
    end;
  end
  else
  {$ENDIF}
  begin
    if TAdvPDFPlatformServices.Current.SupportsPlatformService(IAdvPDFGraphicsLibGeneralService, IInterface(PDFGraphicsLibServiceGeneral)) then
    begin
      o := PDFGraphicsLibServiceGeneral.CreatePDFGraphicsLib;
      Supports(o, IAdvCustomPDFGraphicsLib, FPDFGraphicsLib);
      Supports(o, IAdvCustomPDFGraphicsExLib, FPDFGraphicsExLib);
      Supports(o, IAdvCustomPDFInitializationLib, FPDFInitializationLib);
    end;
  end;

  FPDFRichTextLib := TAdvPDFRichTextLib.Create;
  FPDFGraphicsExLib.SetPDFRichTextLib(FPDFRichTextLib.GetPDFRichTextLib);
end;

destructor TAdvCustomPDFGraphicsLib.Destroy;
begin
  if Assigned(FPDFRichTextLib) then
  begin
    FPDFRichTextLib.Free;
    FPDFRichTextLib := nil;
  end;

  if Assigned(FPDFGraphicsLib) then
    FPDFGraphicsLib := nil;

  inherited;
end;

function TAdvCustomPDFGraphicsLib.GetPDFGraphicsExLib: IAdvCustomPDFGraphicsExLib;
begin
  Result := FPDFGraphicsExLib;
end;

function TAdvCustomPDFGraphicsLib.GetPDFGraphicsLib: IAdvCustomPDFGraphicsLib;
begin
  Result := FPDFGraphicsLib;
end;

function TAdvCustomPDFGraphicsLib.GetPDFInitializationLib: IAdvCustomPDFInitializationLib;
begin
  Result := FPDFInitializationLib;
end;

initialization
begin
  {$IFDEF USENATIVE}
  RegisterPDFGraphicsLibService;
  {$ENDIF}
  RegisterPDFGraphicsLibGeneralService;
end;

finalization
begin
  UnRegisterPDFGraphicsLibGeneralService;
  {$IFDEF USENATIVE}
  UnRegisterPDFGraphicsLibService;
  {$ENDIF}
end;

end.
