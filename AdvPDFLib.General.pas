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

unit AdvPDFLib.General;

{$I TMSDEFS.INC}

interface

procedure RegisterPDFLibGeneralService;
procedure UnRegisterPDFLibGeneralService;

implementation

uses
  Classes, Math, DateUtils, Types, SysUtils, AdvPDFGraphicsLib, AdvGraphicsTypes
  ,AdvPDFLib, AdvUtils, AdvPDFCoreLibBase, AdvTypes, Graphics,
  PictureContainer
  {$IFNDEF LCLLIB}
  ,Generics.Collections, Generics.Defaults
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  ,AdvPDFLib.General.iOS
  {$ELSE}
  ,AdvPDFLib.General.Mac
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  ,AdvPDFLib.General.Android
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ,AdvPDFLib.General.Win
  {$ENDIF}
  {$IFDEF UNIX}
  ,AdvPDFLib.General.Unix
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  ,AdvPDFLib.General.Unix
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFNDEF MSWINDOWS}
  {$IFNDEF UNIX}
  ,AdvPDFLib.General.Default
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  ;

type
  TAdvGeneralPDFLibService = class;

  TAdvGeneralPDFLibService = class(TAdvPDFLibFactoryService)
  protected
    function DoCreatePDFLib: IAdvCustomPDFLib; override;
  end;

const
  DefaultMediaBox: TRectF = (Left: 0; Top: 0; Right: 612; Bottom: 792);
  {%H-}PDFStart17 = '%PDF-1.7'#13#10'%'#249#250#251#252;
  {%H-}PDFStart16 = '%PDF-1.6'#13#10'%'#245#246#247#248;
  {%H-}PDFStart15 = '%PDF-1.5'#13#10'%'#241#242#243#244;
  {%H-}PDFStart14 = '%PDF-1.4'#13#10'%'#228#229#230#240;
  {%H-}PDFStart13 = '%PDF-1.3';
  PDFStart = PDFStart17;
  PDFEnd = '%%EOF';
  PDFCR = #13;
  PDFLF = #10;
  PDFLB = PDFCR + PDFLF;
  PDFMaxXref = 65535;
  PDFDestOutputProfileRef = '{PDFDESTOUTPUTPROFILEREFERENCE}';
  PDFDestOutputProfileLengthRef = '{PDFDESTOUTPUTPROFILELENGTHREFERENCE}';
  PDFPageFontTextRef = '{PDFPAGEFONTTEXTREFERENCE%s}';
  PDFPageRef = '{PDFPAGEREFERENCE}';
  PDFPageChildRef = '{PDFPAGECHILDREFERENCE}';
  PDFPageAcroFormRef = '{PDFPAGEACROFORMREFERENCE}';
  PDFPageAcroFormFieldsRef = '{PDFPAGEACROFORMFIELDSREFERENCE}';
  PDFPageAcroFormFieldContentRef = '{PDFPAGEACROFORMFIELDCONTENTREFERENCE}';
  PDFPageContentRef = '{PDFPAGECONTENTREFERENCE}';
  PDFPageContentLengthRef = '{PDFPAGECONTENTLENGTHREFERENCE}';
  PDFPageCountRef = '{PDFPAGECOUNTREFERENCE}';
  PDFPageFontRef = '{PDFPAGEFONTREFERENCE}';
  PDFPagePatternRef = '{PDFPAGEPATTERNREFERENCE}';
  PDFPageAnnotationsRef = '{PDFPAGEANNOTATIONSREFERENCE}';
  PDFPageShadingObjectRef = '{PDFPAGESHADINGOBJECTREFERENCE%s}';
  PDFPageBitmapObjectRef = '{PDFPAGEBITMAPOBJECTREFERENCE%s}';
  PDFPageXObjectRef = '{PDFPAGEXOBJECTREFERENCE}';

  ICC: array[0..139] of UInt32 = (
    805437440,1161970753,4098,1920233069,541214546,542792024,134270983,318769920,989868800,
    1886610273,1280331841,0,1701736302,0,0,0,0,3606446080,256,768802816,1161970753,
    0,0,0,0,0,0,0,0,0,0,0,167772160,1953656931,4227858432,838860800,1668506980,805371904,
    1795162112,1953526903,2617311232,335544320,1953524578,2952855552,335544320,1129469042,
    3288399872,234881024,1129469031,3556835328,234881024,1129469026,3825270784,234881024,
    1515804786,4093706240,335544320,1515804775,134348800,335544320,1515804770,469893120,
    335544320,1954047348,0,2037411651,1751607666,808591476,1092628528,1700949860,1937330976,
    1936549236,1668172064,1869640303,1702125938,100,1668506980,0,285212672,1651467329,
    1196564581,824713282,691550521,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,542792024,0,
    1374879744,256,3423994112,542792024,0,0,0,0,1987212643,0,16777216,13058,1987212643,0,
    16777216,13058,1987212643,0,16777216,13058,542792024,0,412876800,2773417984,4228120576,
    542792024,0,2368995328,748683264,2500788224,542792024,0,824573952,789577728,2629697536);

type
  TAdvGeneralPDFLib = class;

  TAdvGeneralPDFLibObjectType = (lotDirect, lotIndirect);

  TAdvGeneralPDFLibXRefObject = class;

  TAdvGeneralPDFLibObject = class
  private
    FPDFLib: TAdvGeneralPDFLib;
    FXRefNumber: integer;
    FNumber: integer;
    FType: TAdvGeneralPDFLibObjectType;
    FXRefObject: TAdvGeneralPDFLibXRefObject;
    FRefNameOriginal: string;
    FInactive: Boolean;
    procedure SetNumber(const Value: integer);
    function GetRef: string;
    function GetRefName: string;
    function GetRefNameNotEscaped: string;
    function GetRefNameBase: String; virtual;
    function GetRefIndex(ACheckClassType: Boolean): Integer; virtual;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const ANumber: Integer = -1; const AXRefNumber: Integer = 0); virtual;
    destructor Destroy; override;
    property Inactive: Boolean read FInactive write FInactive;
    property Number: integer read FNumber write SetNumber;
    property &Type: TAdvGeneralPDFLibObjectType read FType;
    property XRefNumber: integer read FXRefNumber write FXRefNumber;
    property XRefObject: TAdvGeneralPDFLibXRefObject read FXRefObject write FXRefObject;
    property PDFLib: TAdvGeneralPDFLib read FPDFLib;
    property Ref: string read GetRef;
    property RefName: string read GetRefName;
    property RefNameNotEscaped: string read GetRefNameNotEscaped;
    property RefNameBase: String read GetRefNameBase;
    property RefNameOriginal: string read FRefNameOriginal write FRefNameOriginal;
  end;

  TAdvGeneralPDFLibObjectClass = class of TAdvGeneralPDFLibObject;

  TAdvGeneralPDFLibDestOutputProfile = class(TAdvGeneralPDFLibObject);
  TAdvGeneralPDFLibInfo = class(TAdvGeneralPDFLibObject);
  TAdvGeneralPDFLibCatalog = class(TAdvGeneralPDFLibObject);
  TAdvGeneralPDFLibPages = class(TAdvGeneralPDFLibObject);
  TAdvGeneralPDFLibOutputIntent = class(TAdvGeneralPDFLibObject);
  TAdvGeneralPDFLibFontDescriptor = class;
  TAdvGeneralPDFLibFontFile = class;
  TAdvGeneralPDFLibFontDescendant = class;
  TAdvGeneralPDFLibFontUnicode = class;
  TAdvGeneralPDFLibFont = class;
  TAdvGeneralPDFLibXObject = class;
  TAdvGeneralPDFLibShading = class;
  TAdvGeneralPDFLibPattern = class;
  TAdvGeneralPDFLibBitmap = class;
  TAdvGeneralPDFLibContent = class;
  TAdvGeneralPDFLibAcroForm = class;
  TAdvGeneralPDFLibAcroFormField = class;
  TAdvGeneralPDFLibAcroFormFieldContent = class;
  TAdvGeneralPDFLibAnnotation = class;
  TAdvGeneralPDFLibPage = class;

  TAdvGeneralPDFLibPageList = class(TList<TAdvGeneralPDFLibPage>);
  TAdvGeneralPDFLibFonts = TList<TAdvGeneralPDFLibFont>;
  TAdvGeneralPDFLibXObjects = TList<TAdvGeneralPDFLibXObject>;
  TAdvGeneralPDFLibShadings = TList<TAdvGeneralPDFLibShading>;
  TAdvGeneralPDFLibPatterns = TList<TAdvGeneralPDFLibPattern>;
  TAdvGeneralPDFLibAnnotations = TList<TAdvGeneralPDFLibAnnotation>;
  TAdvGeneralPDFLibBitmaps = TList<TAdvGeneralPDFLibBitmap>;
  TAdvGeneralPDFLibAcroFormFields = TList<TAdvGeneralPDFLibAcroFormField>;

  TAdvGeneralPDFLibPage = class(TAdvGeneralPDFLibObject)
  private
    FPageNumber: Integer;
    FFontList: TAdvGeneralPDFLibFonts;
    FXObjectList: TAdvGeneralPDFLibXObjects;
    FShadings: TAdvGeneralPDFLibShadings;
    FPatterns: TAdvGeneralPDFLibPatterns;
    FBitmaps: TAdvGeneralPDFLibBitmaps;
    FContent: TAdvGeneralPDFLibContent;
    FAnnotations: TAdvGeneralPDFLibAnnotations;
    FInsertPageNumber: Integer;
  protected
    property InsertPageNumber: Integer read FInsertPageNumber write FInsertPageNumber;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const APageNumber: Integer); reintroduce; virtual;
    destructor Destroy; override;
    property PageNumber: Integer read FPageNumber write FPageNumber;
    property FontList: TAdvGeneralPDFLibFonts read FFontList;
    property XObjectList: TAdvGeneralPDFLibXObjects read FXObjectList;
    property Shadings: TAdvGeneralPDFLibShadings read FShadings;
    property Bitmaps: TAdvGeneralPDFLibBitmaps read FBitmaps;
    property Patterns: TAdvGeneralPDFLibPatterns read FPatterns;
    property Annotations: TAdvGeneralPDFLibAnnotations read FAnnotations;
    property Content: TAdvGeneralPDFLibContent read FContent write FContent;
  end;

  TAdvGeneralPDFLibAcroFormField = class(TAdvGeneralPDFLibObject)
  private
    FContent: TAdvGeneralPDFLibAcroFormFieldContent;
  public
    destructor Destroy; override;
    property Content: TAdvGeneralPDFLibAcroFormFieldContent read FContent write FContent;
  end;

  TAdvGeneralPDFLibContent = class(TAdvGeneralPDFLibObject)
  private
    FPage: TAdvGeneralPDFLibPage;
  public
    destructor Destroy; override;
    property Page: TAdvGeneralPDFLibPage read FPage write FPage;
  end;

  TAdvGeneralPDFLibAcroForm = class(TAdvGeneralPDFLibObject);

  TAdvGeneralPDFLibFont = class(TAdvGeneralPDFLibObject)
  private
    FInitializer: TAdvGeneralPDFLibFontInitializer;
    FSize: Single;
    FName: string;
    FStyle: TFontStyles;
    FBase: string;
    FUnicode: Boolean;
    FDescent: Integer;
    FAscent: Integer;
    FBox: TRect;
    FItalicAngle: Integer;
    FFontFile: TAdvGeneralPDFLibFontFile;
    FFontDescriptor: TAdvGeneralPDFLibFontDescriptor;
    FFontDescendant: TAdvGeneralPDFLibFontDescendant;
    FFontUnicode: TAdvGeneralPDFLibFontUnicode;
    FTrueType: Boolean;
    FCapHeight: Integer;
    FUnitsPerEm: Integer;
    function GetCharArray: TAdvPDFGraphicsLibFontCharArray;
    function GetCharWidths: TAdvPDFGraphicsLibFontCharWidths;
    function GetUsedCharArray: TAdvPDFGraphicsLibUsedFontCharArray;
    function GetRefNameBase: string; override;
  protected
    function BoxAsString: string;
    function WidthsAsString: string;
    function GlyphsAndWidthsAsString: string;
    function FirstGlyphAsString: string;
    function LastGlyphAsString: string;
    function FirstGlyph: Integer;
    function LastGlyph: Integer;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const AMainInitializer: TAdvGeneralPDFLibInitializer; const ABase: string; const AName: String; const ASize: Single; const AStyle: TFontStyles); reintroduce; virtual;
    destructor Destroy; override;
    property Base: string read FBase write FBase;
    property Name: string read FName write FName;
    property Size: Single read FSize write FSize;
    property Style: TFontStyles read FStyle write FStyle;
    property Ascent: Integer read FAscent write FAscent;
    property CapHeight: Integer read FCapHeight write FCapHeight;
    property UnitsPerEm: Integer read FUnitsPerEm write FUnitsPerEm;
    property Descent: Integer read FDescent write FDescent;
    property ItalicAngle: Integer read FItalicAngle write FItalicAngle;
    property Box: TRect read FBox write FBox;
    property CharWidths: TAdvPDFGraphicsLibFontCharWidths read GetCharWidths;
    property FontFile: TAdvGeneralPDFLibFontFile read FFontFile write FFontFile;
    property FontDescriptor: TAdvGeneralPDFLibFontDescriptor read FFontDescriptor write FFontDescriptor;
    property FontDescendant: TAdvGeneralPDFLibFontDescendant read FFontDescendant write FFontDescendant;
    property FontUnicode: TAdvGeneralPDFLibFontUnicode read FFontUnicode write FFontUnicode;
    property CharArray: TAdvPDFGraphicsLibFontCharArray read GetCharArray;
    property UsedCharArray: TAdvPDFGraphicsLibUsedFontCharArray read GetUsedCharArray;
    property Unicode: Boolean read FUnicode write FUnicode;
    property TrueType: Boolean read FTrueType write FTrueType;
    property Initializer: TAdvGeneralPDFLibFontInitializer read FInitializer;
  end;

//  TAdvGeneralPDFLibTextField = class(TAdvGeneralPDFLibAcroFormField);

  TAdvGeneralPDFLibFontDescriptor = class(TAdvGeneralPDFLibObject)
  private
    FFont: TAdvGeneralPDFLibFont;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const AFont: TAdvGeneralPDFLibFont); reintroduce; virtual;
    destructor Destroy; override;
    property Font: TAdvGeneralPDFLibFont read FFont write FFont;
  end;

  TAdvGeneralPDFLibFontFile = class(TAdvGeneralPDFLibObject)
  private
    FFont: TAdvGeneralPDFLibFont;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const AFont: TAdvGeneralPDFLibFont); reintroduce; virtual;
    destructor Destroy; override;
    property Font: TAdvGeneralPDFLibFont read FFont write FFont;
    procedure InitializeFontFile;
  end;

  TAdvGeneralPDFLibFontDescendant = class(TAdvGeneralPDFLibObject)
  private
    FFont: TAdvGeneralPDFLibFont;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const AFont: TAdvGeneralPDFLibFont); reintroduce; virtual;
    destructor Destroy; override;
    property Font: TAdvGeneralPDFLibFont read FFont write FFont;
  end;

  TAdvGeneralPDFLibFontUnicode = class(TAdvGeneralPDFLibObject)
  private
    FFont: TAdvGeneralPDFLibFont;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const AFont: TAdvGeneralPDFLibFont); reintroduce; virtual;
    destructor Destroy; override;
    property Font: TAdvGeneralPDFLibFont read FFont write FFont;
  end;

  TAdvGeneralPDFLibXObject = class(TAdvGeneralPDFLibObject);

  TAdvGeneralPDFLibBitmap = class(TAdvGeneralPDFLibXObject)
  private
    FStream: TMemoryStream;
    FBitmap: TAdvBitmap;
    FImageType: TAdvPDFGraphicsLibImageType;
    FQuality: Single;
    FBackgroundColor: TAdvGraphicsColor;
    procedure SetBitmap(const Value: TAdvBitmap);
    function GetRefNameBase: String; override;
    procedure SetStream(const Value: TMemoryStream);
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const ABitmap: TAdvBitmap; const AStream: TMemoryStream; const AImageType: TAdvPDFGraphicsLibImageType; const AQuality: Single; const ABackgroundColor: TAdvGraphicsColor); reintroduce; virtual;
    destructor Destroy; override;
    property Stream: TMemoryStream read FStream write SetStream;
    property Bitmap: TAdvBitmap read FBitmap write SetBitmap;
    property ImageType: TAdvPDFGraphicsLibImageType read FImageType write FImageType;
    property Quality: Single read FQuality write FQuality;
    property BackgroundColor: TAdvGraphicsColor read FBackgroundColor write FBackgroundColor;
  end;

  TAdvGeneralPDFLibShadingSubFunction = class(TAdvGeneralPDFLibObject)
  private
    FFillColor: TAdvGraphicsColor;
    FFillColorTo: TAdvGraphicsColor;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const AFillColor: TAdvGraphicsColor; const AFillColorTo: TAdvGraphicsColor); reintroduce; virtual;
    property FillColor: TAdvGraphicsColor read FFillColor write FFillColor;
    property FillColorTo: TAdvGraphicsColor read FFillColorTo write FFillColorTo;
  end;

  TAdvGeneralPDFLibShadingSubFunctions = TList<TAdvGeneralPDFLibShadingSubFunction>;

  TAdvGeneralPDFLibShadingFunction = class(TAdvGeneralPDFLibObject)
  private
    FSubFunctions: TAdvGeneralPDFLibShadingSubFunctions;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib); reintroduce; virtual;
    destructor Destroy; override;
    property SubFunctions: TAdvGeneralPDFLibShadingSubFunctions read FSubFunctions;
    function SubFunctionsRef: String;
  end;

  TAdvGeneralPDFLibShading = class(TAdvGeneralPDFLibObject)
  private
    FFunction: TAdvGeneralPDFLibShadingFunction;
    FRect: TRectF;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib); reintroduce; virtual;
    destructor Destroy; override;
    function RectAsString: string; virtual;
    property &Function: TAdvGeneralPDFLibShadingFunction read FFunction write FFunction;
    property Rect: TRectF read FRect write FRect;
  end;

  TAdvGeneralPDFLibAnnotation = class(TAdvGeneralPDFLibObject)
  private
    FAcroFormField: TAdvGeneralPDFLibAcroFormField;
    FRect: TRectF;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const ARect: TRectF); reintroduce; virtual;
    destructor Destroy; override;
    function RectAsString: String;
    property AcroFormField: TAdvGeneralPDFLibAcroFormField read FAcroFormField write FAcroFormField;
    property Rect: TRectF read FRect write FRect;
  end;

  TAdvGeneralPDFLibURL = class(TAdvGeneralPDFLibAnnotation)
  private
    FURL: UnicodeString;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const ARect: TRectF; const AURL: UnicodeString); reintroduce; virtual;
    property URL: UnicodeString read FURL write FURL;
  end;

  TAdvGeneralPDFLibAcroFormFieldContent = class(TAdvGeneralPDFLibObject);

  TAdvGeneralPDFLibPattern = class(TAdvGeneralPDFLibObject)
  private
    FShading: TAdvGeneralPDFLibShading;
    function GetRefNameBase: string; override;
  public
    destructor Destroy; override;
    property Shading: TAdvGeneralPDFLibShading read FShading write FShading;
  end;

  TAdvGeneralPDFLibLinearGradient = class(TAdvGeneralPDFLibShading)
  private
    FFillOrientation: TAdvGraphicsFillOrientation;
  public
    constructor Create(const APDFLib: TAdvGeneralPDFLib; const AFillOrientation: TAdvGraphicsFillOrientation); reintroduce; virtual;
    property FillOrientation: TAdvGraphicsFillOrientation read FFillOrientation write FFillOrientation;
  end;

  TAdvGeneralPDFLibXRefObject = class
  private
    FXRefOffset: integer;
    FXRefNumber: integer;
    FXRefType: string;
    FXRefObject: TAdvGeneralPDFLibObject;
    FInXRefList: Boolean;
  public
    constructor Create(const AValue: TAdvGeneralPDFLibObject = nil);
    destructor Destroy; override;
    class function FormatValue(AValue: Integer; ALength: Integer): string;
    function GenerateXRefValue: string;
    property XRefOffset: integer read FXRefOffset write FXRefOffset;
    property XRefNumber: integer read FXRefNumber write FXRefNumber;
    property XRefType: string read FXRefType write FXRefType;
    property XRefObject: TAdvGeneralPDFLibObject read FXRefObject write FXRefObject;
    property InXRefList: Boolean read FInXRefList write FInXRefList;
  end;

  TAdvGeneralPDFLibXRefObjects = class(TObjectList<TAdvGeneralPDFLibXRefObject>)
  public
    function AddObjectClass(const AObjectClass: TAdvGeneralPDFLibObjectClass = nil): TAdvGeneralPDFLibXRefObject;
    function AddObject(const AObject: TAdvGeneralPDFLibObject = nil): TAdvGeneralPDFLibXRefObject;
    function IndexOfObject(const AObject: TAdvGeneralPDFLibObject; const ACheckClassType: Boolean = False): Integer;
  end;

  TAdvGeneralPDFLib = class(TInterfacedObject, IAdvCustomPDFLib)
  private
    {$IFNDEF LCLLIB}
    FComparePageNumbers: IComparer<TAdvGeneralPDFLibPage>;
    {$ENDIF}
    FOnBeforeDrawHeader: TAdvPDFLibBeforeDrawHeaderEvent;
    FOnBeforeDrawFooter: TAdvPDFLibBeforeDrawFooterEvent;
    FOnBeforeDrawPageNumber: TAdvPDFLibBeforeDrawPageNumberEvent;
    FOnAfterDrawHeader: TAdvPDFLibAfterDrawHeaderEvent;
    FOnAfterDrawFooter: TAdvPDFLibAfterDrawFooterEvent;
    FOnAfterDrawPageNumber: TAdvPDFLibAfterDrawPageNumberEvent;
    FOnNewPageStarted: TAdvPDFLibNewPageStartedEvent;
    FPageHeight, FPageWidth: Single;
    FBlockFontUpdate: Boolean;
    FOSFontList: TStringList;
    FInfo: TAdvGeneralPDFLibInfo;
    FDestOutputProfile: TAdvGeneralPDFLibDestOutputProfile;
    FInitializer: TAdvGeneralPDFLibInitializer;
    FAcroForm: TAdvGeneralPDFLibAcroForm;
    FOutputIntent: TAdvGeneralPDFLibOutputIntent;
    FActivePage: TAdvGeneralPDFLibPage;
    FActiveFont: TAdvGeneralPDFLibFont;
    FActiveShading: TAdvGeneralPDFLibShading;
    XRefAddress: Int64;
    FXRefObjects: TAdvGeneralPDFLibXRefObjects;
    FFontList: TAdvGeneralPDFLibFonts;
    FAcroFormFields: TAdvGeneralPDFLibAcroFormFields;
    FPageCount: Integer;
    FOutput: TAdvPDFGraphicsLibOutputWriter;
    FPDFFileName: string;
    FDocumentStarted, FPageStarted: Boolean;
    FPDFGraphicsLib: IAdvCustomPDFGraphicsLib;
    FPDFGraphicsExLib: IAdvCustomPDFGraphicsExLib;
    FPDFInitializationLib: IAdvCustomPDFInitializationLib;
    FMediaBox: TRectF;
    FPageOrientation: TAdvPDFLibPageOrientation;
    FAuthor: String;
    FSubject: String;
    FAllowsPrinting: Boolean;
    FTitle: String;
    FCreator: String;
    FUserPassword: String;
    FAllowsCopying: Boolean;
    FKeywords: TStrings;
    FOwnerPassword: String;
    FBleedBox: TRectF;
    FCropBox: TRectF;
    FArtBox: TRectF;
    FTrimBox: TRectF;
    FPageSize: TAdvPDFLibPageSize;
    FPageNumberFormat: UnicodeString;
    FPageNumber: TAdvPDFLibPageNumber;
    FPageNumberSize: Single;
    FPageNumberMargins: TAdvMargins;
    FHeader: UnicodeString;
    FFooter: UnicodeString;
    FHeaderSize: Single;
    FHeaderMargins: TAdvMargins;
    FFooterMargins: TAdvMargins;
    FFooterSize: Single;
    FFooterAlignment: TAdvGraphicsTextAlign;
    FHeaderAlignment: TAdvGraphicsTextAlign;
    FPageNumberAlignment: TAdvGraphicsTextAlign;
    FModificationDate: string;
    FProducer: String;
    FCreationDate: String;
    FEmbedFonts: Boolean;
    FFontFallBackList: TStrings;
    FFooterFont: TAdvPDFGraphicsLibFont;
    FHeaderFont: TAdvPDFGraphicsLibFont;
    FPageNumberFont: TAdvPDFGraphicsLibFont;
    FPDFStandard: TAdvPDFLibStandard;
    procedure SetPageSize(const Value: TAdvPDFLibPageSize);
    procedure SetPageOrientation(
      const Value: TAdvPDFLibPageOrientation);
    procedure SetHeaderMargins(const Value: TAdvMargins);
    procedure SetPageNumberMargins(const Value: TAdvMargins);
    procedure SetFooterMargins(const Value: TAdvMargins);
    procedure SetAllowsCopying(const Value: Boolean);
    procedure SetAllowsPrinting(const Value: Boolean);
    procedure SetAuthor(const Value: String);
    procedure SetCreator(const Value: String);
    procedure SetFooter(const Value: UnicodeString);
    procedure SetFooterAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetFooterSize(const Value: Single);
    procedure SetHeader(const Value: UnicodeString);
    procedure SetHeaderAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetHeaderSize(const Value: Single);
    procedure SetPageNumber(const Value: TAdvPDFLibPageNumber);
    procedure SetPageNumberFormat(const Value: UnicodeString);
    procedure SetPageNumberAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetPageNumberSize(const Value: Single);
    procedure SetKeywords(const Value: TStrings);
    procedure SetFontFallBackList(const Value: TStrings);
    procedure SetOwnerPassword(const Value: String);
    procedure SetSubject(const Value: String);
    procedure SetTitle(const Value: String);
    procedure SetUserPassword(const Value: String);
    procedure SetArtBox(const Value: TRectF);
    procedure SetBleedBox(const Value: TRectF);
    procedure SetCropBox(const Value: TRectF);
    procedure SetMediaBox(const Value: TRectF);
    procedure SetTrimBox(const Value: TRectF);
    procedure SetEmbedFonts(const Value: Boolean);
    procedure SetFooterFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetHeaderFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetPageNumberFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetPageHeight(const Value: Single);
    procedure SetPageWidth(const Value: Single);
    procedure SetOnAfterDrawFooter(const Value: TAdvPDFLibAfterDrawFooterEvent);
    procedure SetOnAfterDrawHeader(const Value: TAdvPDFLibAfterDrawHeaderEvent);
    procedure SetOnAfterDrawPageNumber(const Value: TAdvPDFLibAfterDrawPageNumberEvent);
    procedure SetOnBeforeDrawFooter(const Value: TAdvPDFLibBeforeDrawFooterEvent);
    procedure SetOnBeforeDrawHeader(const Value: TAdvPDFLibBeforeDrawHeaderEvent);
    procedure SetOnBeforeDrawPageNumber(const Value: TAdvPDFLibBeforeDrawPageNumberEvent);
    procedure SetOnNewPageStarted(const Value: TAdvPDFLibNewPageStartedEvent);
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetPDFStandard(const Value: TAdvPDFLibStandard);
    function GetPDFStandard: TAdvPDFLibStandard;
    function GetAllowsCopying: Boolean;
    function GetAllowsPrinting: Boolean;
    function GetAuthor: String;
    function GetCreator: String;
    function GetFooter: UnicodeString;
    function GetFooterAlignment: TAdvGraphicsTextAlign;
    function GetFooterMargins: TAdvMargins;
    function GetFooterSize: Single;
    function GetHeader: UnicodeString;
    function GetHeaderAlignment: TAdvGraphicsTextAlign;
    function GetHeaderMargins: TAdvMargins;
    function GetHeaderSize: Single;
    function GetPageNumber: TAdvPDFLibPageNumber;
    function GetPageNumberFormat: UnicodeString;
    function GetPageNumberAlignment: TAdvGraphicsTextAlign;
    function GetPageNumberMargins: TAdvMargins;
    function GetPageNumberSize: Single;
    function GetKeywords: TStrings;
    function GetFontFallBackList: TStrings;
    function GetOwnerPassword: String;
    function GetPageOrientation: TAdvPDFLibPageOrientation;
    function GetPageSize: TAdvPDFLibPageSize;
    function GetSubject: String;
    function GetTitle: String;
    function GetUserPassword: String;
    function GetArtBox: TRectF;
    function GetBleedBox: TRectF;
    function GetCreationDate: String;
    function GetCropBox: TRectF;
    function GetMediaBox: TRectF;
    function GetModificationDate: string;
    function GetProducer: String;
    function GetTrimBox: TRectF;
    function GetEmbedFonts: Boolean;
    function GetFooterFont: TAdvPDFGraphicsLibFont;
    function GetHeaderFont: TAdvPDFGraphicsLibFont;
    function GetPageNumberFont: TAdvPDFGraphicsLibFont;
    function GetPageHeight: Single;
    function GetPageWidth: Single;
    function GetOnAfterDrawFooter: TAdvPDFLibAfterDrawFooterEvent;
    function GetOnAfterDrawHeader: TAdvPDFLibAfterDrawHeaderEvent;
    function GetOnAfterDrawPageNumber: TAdvPDFLibAfterDrawPageNumberEvent;
    function GetOnBeforeDrawFooter: TAdvPDFLibBeforeDrawFooterEvent;
    function GetOnBeforeDrawHeader: TAdvPDFLibBeforeDrawHeaderEvent;
    function GetOnBeforeDrawPageNumber: TAdvPDFLibBeforeDrawPageNumberEvent;
    function GetOnNewPageStarted: TAdvPDFLibNewPageStartedEvent;
    function GetPictureContainer: TPictureContainer;
  protected
    procedure DoNewPageStarted(APageIndex: Integer);
    procedure DoBeforeDrawHeader(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawPageNumber(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawFooter(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawHeader(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib); virtual;
    procedure DoAfterDrawPageNumber(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib); virtual;
    procedure DoAfterDrawFooter(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib); virtual;
    procedure FontChanged(Sender: TObject);
    procedure NotifyURL(Sender: TObject; ARect: TRectF; AURL: UnicodeString);
    procedure NotifyUnicode(Sender: TObject; AValue: UnicodeString);
    procedure NotifyText(Sender: TObject; {%H-}AValue: UnicodeString);
    procedure NotifyShading(Sender: TObject; AFillColor: TAdvGraphicsColor; AFillColorTo: TAdvGraphicsColor; AFillOrientation: TAdvGraphicsFillOrientation; var AShadingReference: string);
    procedure NotifyShadingRect(Sender: TObject; ARect: TRectF);
    procedure NotifyBitmap(Sender: TObject; AValue: TAdvBitmap; AImageType: TAdvPDFGraphicsLibImageType; AQuality: Single; ABackgroundColor: TAdvGraphicsColor; var ABitmapReference: string);
    procedure DestroyFont(AFont: TAdvGeneralPDFLibFont);
    procedure UpdateBoxRect;
    procedure FinishPage;
    procedure CleanUpUnusedFonts;
    procedure UpdateFontList(ASearchForUnicodeFont: Boolean = False);
    procedure WriteFontList;
    procedure WriteBitmapList;
    procedure WriteAcroFormFieldList;
    procedure WriteAnnotationList;
    procedure WritePatternList;
    procedure WriteShadingList;
    procedure WriteXRefTable;
    procedure SetPDFGraphicsLib(const Value: IAdvCustomPDFGraphicsLib);
    procedure SetPDFGraphicsExLib(const Value: IAdvCustomPDFGraphicsExLib);
    procedure SetPDFInitializationLib(const Value: IAdvCustomPDFInitializationLib);
    procedure BeginDocument(FileName: String = '');
    procedure OpenDocument({%H-}FileName: String); overload;
    procedure OpenDocument({%H-}FileStream: TMemoryStream); overload;
    procedure SaveDocumentFromStream({%H-}FileStream: TMemoryStream; {%H-}FileName: String);
    procedure GetDocumentInfo;
    procedure GetPageInfo({%H-}PageIndex: Integer);
    procedure CloseDocument;
    procedure NewPage;
    procedure InsertPage(PageIndex: Integer);
    procedure DrawPage({%H-}PageIndex: Integer);
    procedure DrawHeader;
    procedure DrawFooter;
    procedure DrawPageNumber;
    function GetPDFGraphicsLib: IAdvCustomPDFGraphicsLib;
    function GetPDFGraphicsExLib: IAdvCustomPDFGraphicsExLib;
    function GetPDFInitializationLib: IAdvCustomPDFInitializationLib;
    function CompareStreams(AStream1, AStream2: TMemoryStream): Boolean;
    function VerifyFontName(AFontName: string): string;
    function SearchForFont(ABaseFontName: string; AUnicodeFont: Boolean): TAdvGeneralPDFLibFont;
    function GetPageReferences: String;
    function GetAcroFormFieldReferences: string;
    function GetKeywordsString: string;
    function GetFontRefs(APage: TAdvGeneralPDFLibPage): String;
    function GetXObjectRefs(APage: TAdvGeneralPDFLibPage): string;
    function GetShadingRef(APage: TAdvGeneralPDFLibPage): string;
    function GetAnnotationsRef(APage: TAdvGeneralPDFLibPage): string;
    function GetPatternRef(APage: TAdvGeneralPDFLibPage): string;
    function GetContentRef(APage: TAdvGeneralPDFLibPage): string;
    function GetAcroFormRef: string;
    function GetDestOutputProfileRef: String;
    function GetPageRef(APage: TAdvGeneralPDFLibPage): string;
    function UnlockWithPassword({%H-}Password: String): Boolean;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    function IsDocumentOpened: Boolean;
    function GetHeaderRect: TRectF;
    function GetPageNumberRect: TRectF;
    function GetFooterRect: TRectF;
    function EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
    property MediaBox: TRectF read FMediaBox write FMediaBox;
    property TrimBox: TRectF read FTrimBox write FTrimBox;
    property ArtBox: TRectF read FArtBox write FArtBox;
    property BleedBox: TRectF read FBleedBox write FBleedBox;
    property CropBox: TRectF read FCropBox write FCropBox;
    property EmbedFonts: Boolean read FEmbedFonts write FEmbedFonts;
    property ModificationDate: string read FModificationDate;
    property Producer: String read FProducer;
    property CreationDate: String read FCreationDate;
    property Initializer: TAdvGeneralPDFLibInitializer read FInitializer;
    property PageWidth: Single read GetPageWidth write SetPageWidth;
    property PageHeight: Single read GetPageHeight write SetPageHeight;
    property PageSize: TAdvPDFLibPageSize read FPageSize write FPageSize default psLetter;
    property Orientation: TAdvPDFLibPageOrientation read FPageOrientation write FPageOrientation default poPortrait;
    property Author: String read FAuthor write FAuthor;
    property Creator: String read FCreator write FCreator;
    property Header: UnicodeString read FHeader write FHeader;
    property HeaderSize: Single read FHeaderSize write FHeaderSize;
    property HeaderMargins: TAdvMargins read FHeaderMargins write FHeaderMargins;
    property HeaderAlignment: TAdvGraphicsTextAlign read FHeaderAlignment write FHeaderAlignment default gtaCenter;
    property HeaderFont: TAdvPDFGraphicsLibFont read GetHeaderFont write SetHeaderFont;
    property PageNumberFormat: UnicodeString read FPageNumberFormat write FPageNumberFormat;
    property PageNumber: TAdvPDFLibPageNumber read FPageNumber write FPageNumber;
    property PageNumberSize: Single read FPageNumberSize write FPageNumberSize;
    property PageNumberMargins: TAdvMargins read FPageNumberMargins write FPageNumberMargins;
    property PageNumberAlignment: TAdvGraphicsTextAlign read FPageNumberAlignment write FPageNumberAlignment default gtaTrailing;
    property PageNumberFont: TAdvPDFGraphicsLibFont read GetPageNumberFont write SetPageNumberFont;
    property PDFStandard: TAdvPDFLibStandard read FPDFStandard write FPDFStandard default pdfNone;
    property Footer: UnicodeString read FFooter write FFooter;
    property FooterSize: Single read FFooterSize write FFooterSize;
    property FooterMargins: TAdvMargins read FFooterMargins write FFooterMargins;
    property FooterAlignment: TAdvGraphicsTextAlign read FFooterAlignment write FFooterAlignment default gtaCenter;
    property FooterFont: TAdvPDFGraphicsLibFont read GetFooterFont write SetFooterFont;
    property Title: String read FTitle write FTitle;
    property OwnerPassword: String read FOwnerPassword write FOwnerPassword;
    property UserPassword: String read FUserPassword write FUserPassword;
    property AllowsPrinting: Boolean read FAllowsPrinting write FAllowsPrinting default True;
    property AllowsCopying: Boolean read FAllowsCopying write FAllowsCopying default True;
    property Subject: String read FSubject write FSubject;
    property Keywords: TStrings read FKeywords write FKeywords;
    property FontFallBackList: TStrings read FFontFallBackList write FFontFallBackList;
    property OnBeforeDrawHeader: TAdvPDFLibBeforeDrawHeaderEvent read GetOnBeforeDrawHeader write SetOnBeforeDrawHeader;
    property OnAfterDrawHeader: TAdvPDFLibAfterDrawHeaderEvent read GetOnAfterDrawHeader write SetOnAfterDrawHeader;
    property OnBeforeDrawPageNumber: TAdvPDFLibBeforeDrawPageNumberEvent read GetOnBeforeDrawPageNumber write SetOnBeforeDrawPageNumber;
    property OnAfterDrawPageNumber: TAdvPDFLibAfterDrawPageNumberEvent read GetOnAfterDrawPageNumber write SetOnAfterDrawPageNumber;
    property OnBeforeDrawFooter: TAdvPDFLibBeforeDrawFooterEvent read GetOnBeforeDrawFooter write SetOnBeforeDrawFooter;
    property OnAfterDrawFooter: TAdvPDFLibAfterDrawFooterEvent read GetOnAfterDrawFooter write SetOnAfterDrawFooter;
    property OnNewPageStarted: TAdvPDFLibNewPageStartedEvent read GetOnNewPageStarted write SetOnNewPageStarted;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  PDFLibService: IAdvPDFLibGeneralService;

procedure RegisterPDFLibGeneralService;
begin
  if not TAdvPDFPlatformServices.Current.SupportsPlatformService(IAdvPDFLibGeneralService, IInterface(PDFLibService)) then
  begin
    PDFLibService := TAdvGeneralPDFLibService.Create;
    TAdvPDFPlatformServices.Current.AddPlatformService(IAdvPDFLibGeneralService, PDFLibService);
  end;
end;

procedure UnregisterPDFLibGeneralService;
begin
  TAdvPDFPlatformServices.Current.RemovePlatformService(IAdvPDFLibGeneralService);
end;

{ TAdvGeneralPDFLibService }

function TAdvGeneralPDFLibService.DoCreatePDFLib: IAdvCustomPDFLib;
begin
  Result := TAdvGeneralPDFLib.Create;
end;

procedure TAdvGeneralPDFLib.BeginDocument(FileName: String = '');
var
  r: TAdvGeneralPDFLibXRefObject;
  d: TDateTime;
  dt, dtiso: string;
  ms: TStringStream;
  s: String;
begin
  FPDFFileName := FileName;
  FDocumentStarted := True;

  r := FXRefObjects.AddObject;
  r.XRefNumber := PDFMaxXref;
  FOutput.StartNewStream;

  case PDFStandard of
    pdfNone: FOutput.WriteString(PDFStart + PDFLB);
    pdfA1: FOutput.WriteString(PDFStart14 + PDFLB);
  end;

  r := FXRefObjects.AddObjectClass(TAdvGeneralPDFLibCatalog);
  FOutput.StartNewStream(r);
  FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) + ' 0 obj' + PDFLB);
  FOutput.WriteString('<<');
  FOutput.WriteString('/Type/Catalog');
  FOutput.WriteString('/Pages 2 0 R');
  case PDFStandard of
    pdfA1:
    begin
      FOutput.WriteString('/OutputIntents[3 0 R]');
      FOutput.WriteString('/MarkInfo <</Marked true>>/Metadata 5 0 R/StructTreeRoot<<>>');
    end;
  end;

//  FOutput.WriteString('/AcroForm ' + PDFPageAcroFormRef + ' 0 R');
  FOutput.WriteString('>>' + PDFLB);
  FOutput.WriteString('endobj' + PDFLB);

  r := FXRefObjects.AddObjectClass(TAdvGeneralPDFLibPages);
  FOutput.StartNewStream(r);
  FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) +' 0 obj' + PDFLB);
  FOutput.WriteString('<<');
  FOutput.WriteString('/Type/Pages');
  FOutput.WriteString('/Kids['+PDFPageChildRef+']');
  FOutput.WriteString('/Count ' + PDFPageCountRef);
  FOutput.WriteString('>>' + PDFLB);
  FOutput.WriteString('endobj' + PDFLB);

  d := Now;
  dt := ConvertToPDFDate(d);
  dtiso := DateTimeToIso(d);

  case PDFStandard of
    pdfA1:
    begin
      FOutputIntent := TAdvGeneralPDFLibOutputIntent.Create(Self);
      r := FXRefObjects.AddObject(FOutputIntent);
      FOutput.StartNewStream(r);
      FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) +' 0 obj' + PDFLB);
      FOutput.WriteString('<<');
      FOutput.WriteString('/Type/OutputIntent');
      FOutput.WriteString('/OutputCondition()');
      FOutput.WriteString('/OutputConditionIdentifier(sRGB)');
      FOutput.WriteString('/RegistryName(http://www.color.org)');
      FOutput.WriteString('/Info(sRGB IEC61966-2.1)');
      FOutput.WriteString('/S/GTS_PDFA1');
      FOutput.WriteString('/DestOutputProfile ' + PDFDestOutputProfileRef + ' 0 R');
      FOutput.WriteString('>>' + PDFLB);
      FOutput.WriteString('endobj' + PDFLB);

      FDestOutputProfile := TAdvGeneralPDFLibDestOutputProfile.Create(Self);
      r := FXRefObjects.AddObject(FDestOutputProfile);
      FOutput.StartNewStream(r);
      FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) +' 0 obj' + PDFLB);
      FOutput.WriteString('<<');
      FOutput.WriteString(PDFDestOutputProfileLengthRef);
      FOutput.WriteString('>>' + PDFLB);
      FOutput.WriteString('stream' + PDFLB);
      FOutput.StartContentStream(r);

      FOutput.Write(@ICC, SizeOf(ICC));

      ms := FOutput.FinishContentStream(PDFDestOutputProfileLengthRef, '/N 3');
      if Assigned(ms) then
        FOutput.Streams.Add(ms);

      FOutput.StartNewStream;
      FOutput.WriteString(PDFLB);
      FOutput.WriteString('endstream' + PDFLB);
      FOutput.WriteString('endobj' + PDFLB);

      FOutputIntent := TAdvGeneralPDFLibOutputIntent.Create(Self);
      r := FXRefObjects.AddObject(FOutputIntent);
      FOutput.StartNewStream(r);
      FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) +' 0 obj' + PDFLB);
      FOutput.WriteString('<<');

      s := Format(PDFMetaData, [Producer, dtiso, dtiso, Creator, Title, Author, Subject, Keywords.Text, Producer]);

      FOutput.WriteString('/Length '+IntToStr(Length(s))+'/Subtype/XML/Type/Metadata');
      FOutput.WriteString('>>' + PDFLB);
      FOutput.WriteString('stream' + PDFLB);
      FOutput.WriteString(s + PDFLB);
      FOutput.WriteString('endstream' + PDFLB);
      FOutput.WriteString('endobj' + PDFLB);
    end;
  end;

  FInfo := TAdvGeneralPDFLibInfo.Create(Self);
  r := FXRefObjects.AddObject(FInfo);
  FOutput.StartNewStream(r);
  FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(r)) +' 0 obj' + PDFLB);
  FOutput.WriteString('<<');
  FOutput.WriteString('/Producer <>');
  FOutput.WriteString('/Author ('+Author+')');
  FOutput.WriteString('/CreationDate (D:'+dt+')');
  FOutput.WriteString('/Creator ('+Creator+')');
  FOutput.WriteString('/Keywords ('+GetKeywordsString+')');
  FOutput.WriteString('/Subject ('+Subject+')');
  FOutput.WriteString('/Title ('+Title+')');
  FOutput.WriteString('/ModDate (D:'+dt+')');
  FOutput.WriteString('>>' + PDFLB);
  FOutput.WriteString('endobj' + PDFLB);
end;

procedure TAdvGeneralPDFLib.NewPage;
var
  ab, mb, bb, cb, tb: TRectF;
  r: TAdvGeneralPDFLibXRefObject;
  cnt: TAdvGeneralPDFLibContent;
begin
  mb := MediaBox;
  ab := ArtBox;
  bb := BleedBox;
  cb := CropBox;
  tb := TrimBox;

  FinishPage;
  FActivePage := TAdvGeneralPDFLibPage.Create(Self, FPageCount);
  r := FXRefObjects.AddObject(FActivePage);
  FOutput.StartNewStream(r);
  FOutput.WriteString(PDFPageRef + ' 0 obj' + PDFLB);
  FOutput.WriteString('<<');
  FOutput.WriteString('/Type/Page');
  FOutput.WriteString('/Parent 2 0 R');
  FOutput.WriteString('/MediaBox[' + IntToStr(Round(mb.Left))+ ' '+IntToStr(Round(mb.Top)) + ' ' + IntToStr(Round(mb.Right)) + ' ' + IntToStr(Round(mb.Bottom)) + ']');
  FOutput.WriteString('/ArtBox[' + IntToStr(Round(ab.Left))+ ' '+IntToStr(Round(ab.Top)) + ' ' + IntToStr(Round(ab.Right)) + ' ' + IntToStr(Round(ab.Bottom)) + ']');
  FOutput.WriteString('/BleedBox[' + IntToStr(Round(bb.Left))+ ' '+IntToStr(Round(bb.Top)) + ' ' + IntToStr(Round(bb.Right)) + ' ' + IntToStr(Round(bb.Bottom)) + ']');
  FOutput.WriteString('/CropBox[' + IntToStr(Round(cb.Left))+ ' '+IntToStr(Round(cb.Top)) + ' ' + IntToStr(Round(cb.Right)) + ' ' + IntToStr(Round(cb.Bottom)) + ']');
  FOutput.WriteString('/Trimbox[' + IntToStr(Round(tb.Left))+ ' '+IntToStr(Round(tb.Top)) + ' ' + IntToStr(Round(tb.Right)) + ' ' + IntToStr(Round(tb.Bottom)) + ']');
  FOutput.WriteString('/Contents ' + PDFPageContentRef +' 0 R');
  FOutput.WriteString('/ProcSet[/PDF/Text/ImageC]');
  FOutput.WriteString('/Annots['+PDFPageAnnotationsRef+']');
  FOutput.WriteString('/Resources');
  FOutput.WriteString('<<');
  FOutput.WriteString('/XObject');
  FOutput.WriteString('<<');
  FOutput.WriteString(PDFPageXObjectRef);
  FOutput.WriteString('>>');
  FOutput.WriteString('/Font');
  FOutput.WriteString('<<');
  FOutput.WriteString(PDFPageFontRef);
  FOutput.WriteString('>>');
  FOutput.WriteString('/Pattern');
  FOutput.WriteString('<<');
  FOutput.WriteString(PDFPagePatternRef);
  FOutput.WriteString('>>');
  FOutput.WriteString('>>');
  FOutput.WriteString('>>' + PDFLB);
  FOutput.WriteString('endobj' + PDFLB);

  UpdateFontList;

//  FAcroForm := TAdvGeneralPDFLibAcroForm.Create(Self);
//  r := FXRefObjects.AddObject(FAcroForm);
//  FOutput.StartNewStream(r);
//  FOutput.WriteString(PDFPageAcroFormRef + ' 0 obj' + PDFLB);
//  FOutput.WriteString('<<');
//  FOutput.WriteString('/Fields['+PDFPageAcroFormFieldsRef+']');
//  FOutput.WriteString('>>' + PDFLB);
//  FOutput.WriteString('endobj' + PDFLB);

  cnt := TAdvGeneralPDFLibContent.Create(Self);
  cnt.Page := FActivePage;
  r := FXRefObjects.AddObject(cnt);
  FActivePage.Content := cnt;
  FOutput.StartNewStream(r);
  FOutput.WriteString(PDFPageContentRef + ' 0 obj' + PDFLB);
  FOutput.WriteString('<<');
  FOutput.WriteString(PDFPageContentLengthRef);
  FOutput.WriteString('>>' + PDFLB);
  FOutput.WriteString('stream' + PDFLB);
  FOutput.StartContentStream(r);

  if Assigned(FPDFInitializationLib) then
  begin
    FPDFInitializationLib.SetCanvas(FOutput);
    FPDFInitializationLib.SetPageWidth(mb.Width);
    FPDFInitializationLib.SetPageHeight(mb.Height);
    FPDFInitializationLib.InitializeAppearance;
  end;

  DoNewPageStarted(GetPageIndex);

  DrawHeader;
  DrawFooter;
  DrawPageNumber;

  Inc(FPageCount);
  FPageStarted := True;
end;

procedure TAdvGeneralPDFLib.NotifyBitmap(Sender: TObject; AValue: TAdvBitmap; AImageType: TAdvPDFGraphicsLibImageType; AQuality: Single; ABackgroundColor: TAdvGraphicsColor; var ABitmapReference: string);
var
  bmp, bmpc: TAdvGeneralPDFLibBitmap;
  ms: TMemoryStream;
  x: TAdvGeneralPDFLibXRefObject;
begin
  if Assigned(FActivePage) and Assigned(AValue) then
  begin
    ms := TMemoryStream.Create;
    try
      AValue.SaveToStream(ms);
      bmp := nil;
      for x in FXRefObjects do
      begin
        if Assigned(x.XRefObject) and (x.XRefObject is TAdvGeneralPDFLibBitmap) then
        begin
          bmpc := x.XRefObject as TAdvGeneralPDFLibBitmap;
          if Assigned(bmpc.Stream) and (AQuality = bmpc.Quality) and (ABackgroundColor = bmpc.BackgroundColor) and CompareStreams(bmpc.Stream, ms) then
          begin
            bmp := bmpc;
            Break;
          end;
        end;
      end;

      if not Assigned(bmp) then
      begin
        bmp := TAdvGeneralPDFLibBitmap.Create(Self, AValue, ms, AImageType, AQuality, ABackgroundColor);
        FXRefObjects.AddObject(bmp);
      end;

      if FActivePage.Bitmaps.IndexOf(bmp) < 0 then
        FActivePage.Bitmaps.Add(bmp);

      if FActivePage.XObjectList.IndexOf(bmp) < 0 then
        FActivePage.XObjectList.Add(bmp);

      ABitmapReference := Format(PDFPageBitmapObjectRef, [bmp.RefNameNotEscaped]);
      bmp.RefNameOriginal := ABitmapReference;
    finally
      ms.Free;
    end;
  end;
end;

procedure TAdvGeneralPDFLib.NotifyShading(Sender: TObject; AFillColor: TAdvGraphicsColor; AFillColorTo: TAdvGraphicsColor; AFillOrientation: TAdvGraphicsFillOrientation; var AShadingReference: string);
var
  lg: TAdvGeneralPDFLibLinearGradient;
  f: TAdvGeneralPDFLibShadingFunction;
  sf: TAdvGeneralPDFLibShadingSubFunction;
  p: TAdvGeneralPDFLibPattern;
begin
  if Assigned(FActivePage) then
  begin
    lg := TAdvGeneralPDFLibLinearGradient.Create(Self, AFillOrientation);
    FXRefObjects.AddObject(lg);
    f := TAdvGeneralPDFLibShadingFunction.Create(Self);
    FXRefObjects.AddObject(f);
    lg.&Function := f;

    sf := TAdvGeneralPDFLibShadingSubFunction.Create(Self, AFillColor, AFillColorTo);
    FXRefObjects.AddObject(sf);
    f.SubFunctions.Add(sf);

    sf := TAdvGeneralPDFLibShadingSubFunction.Create(Self, AFillColor, AFillColorTo);
    FXRefObjects.AddObject(sf);
    f.SubFunctions.Add(sf);

    FActivePage.Shadings.Add(lg);

    p := TAdvGeneralPDFLibPattern.Create(Self);
    p.Shading := lg;
    FXRefObjects.AddObject(p);
    FActivePage.Patterns.Add(p);

    AShadingReference := Format(PDFPageShadingObjectRef, [p.RefNameNotEscaped]);
    p.RefNameOriginal := AShadingReference;

    FActiveShading := lg;
  end;
end;

procedure TAdvGeneralPDFLib.NotifyShadingRect(Sender: TObject;
  ARect: TRectF);
begin
  if Assigned(FActiveShading) then
    FActiveShading.Rect := ARect;
end;

procedure TAdvGeneralPDFLib.NotifyText(Sender: TObject;
  AValue: UnicodeString);
begin
  UpdateFontList(False);
end;

procedure TAdvGeneralPDFLib.NotifyUnicode(Sender: TObject; AValue: UnicodeString);
var
  I, v: Integer;
  vu: Boolean;
  fnff, fns, fn: string;
  cr: Boolean;
begin
  if Assigned(FOutput) and Assigned(FActiveFont) then
  begin
    fnff := FActiveFont.Base;
    for fnff in FontFallBackList do
    begin
      cr := (FActiveFont.UsedCharArray.Count = 0);
      vu := True;
      {$IFDEF DELPHI_LLVM}
      for I := 0 to Length(AValue) - 1 do
      {$ENDIF}
      {$IFNDEF DELPHI_LLVM}
      for I := 1 to Length(AValue) do
      {$ENDIF}
      begin
        v := Ord(AValue[I]);
        if (v >= 32) and (FActiveFont.CharArray.IndexOf(v) < 0) then
        begin
          vu := False;
          Break;
        end;
      end;

      if not vu then
      begin
        if cr then
        begin
          FActiveFont.Inactive := True;
          FActiveFont := nil;
        end;

        fns := VerifyFontName(fnff);
        FBlockFontUpdate := True;
        FPDFGraphicsLib.Font.Name := fns;
        FBlockFontUpdate := False;
        UpdateFontList(True);
      end
      else
        Break;
    end;

    if Assigned(FActiveFont) and not FActiveFont.Unicode then
    begin
      fn := FActiveFont.Base;
      cr := (FActiveFont.UsedCharArray.Count = 0);
      if cr then
      begin
        FActiveFont.Inactive := True;
        FActiveFont := nil;
      end;

      FBlockFontUpdate := True;
      FPDFGraphicsLib.Font.Name := VerifyFontName(fn);
      FBlockFontUpdate := False;
      UpdateFontList(True);
      FActiveFont.Unicode := True;
      UpdateFontList(True);
    end;
  end;
end;

procedure TAdvGeneralPDFLib.NotifyURL(Sender: TObject; ARect: TRectF; AURL: UnicodeString);
var
  u: TAdvGeneralPDFLibURL;
begin
  if Assigned(FActivePage) then
  begin
    u := TAdvGeneralPDFLibURL.Create(Self, ARect, AURL);
    FXRefObjects.AddObject(u);
    FActivePage.Annotations.Add(u);
  end;
end;

procedure TAdvGeneralPDFLib.OpenDocument(FileStream: TMemoryStream);
begin
  if IsDocumentOpened then
    CloseDocument;
end;

procedure TAdvGeneralPDFLib.OpenDocument(FileName: String);
begin
  if IsDocumentOpened then
    CloseDocument;
end;

procedure TAdvGeneralPDFLib.CleanUpUnusedFonts;
var
  I: Integer;
  ft: TAdvGeneralPDFLibFont;
  cr: Boolean;
begin
  for I := FFontList.Count - 1 downto 0 do
  begin
    ft := FFontList[I];
    cr := (ft.UsedCharArray.Count = 0);
    if cr then
      DestroyFont(ft);
  end;
end;

procedure TAdvGeneralPDFLib.CloseDocument;
begin
end;

function TAdvGeneralPDFLib.CompareStreams(AStream1, AStream2: TMemoryStream): Boolean;
const
  Block_Size = 4096;
var
  Buffer_1: array[0..Block_Size-1] of byte;
  Buffer_2: array[0..Block_Size-1] of byte;
  Buffer_Length: integer;
begin
  Result := False;

  AStream1.Position := 0;
  AStream2.Position := 0;

  if AStream1.Size <> AStream2.Size then
    Exit;

  while AStream1.Position < AStream1.Size do
  begin
    Buffer_Length := AStream1.Read({%H-}Buffer_1, Block_Size);
    AStream2.Read({%H-}Buffer_2, Block_Size);

    if not CompareMem(@Buffer_1, @Buffer_2, Buffer_Length) then
      Exit;
  end;

  Result := True;
end;

constructor TAdvGeneralPDFLib.Create;
var
  r: TRectF;
begin
  inherited;
  {$IFNDEF LCLLIB}
  FComparePageNumbers := TDelegatedComparer<TAdvGeneralPDFLibPage>.Create(
    function(const Item1, Item2: TAdvGeneralPDFLibPage): Integer
    begin
      Result := CompareValue(Item1.InsertPageNumber, Item2.InsertPageNumber);
    end
    );
  {$ENDIF}

  FPageHeight := 0;
  FPageWidth := 0;
  FFontFallBackList := TStringList.Create;
  FOSFontList := TStringList.Create;
  FOSFontList.CaseSensitive := False;
  TAdvUtils.GetFonts(FOSFontList);
  FEmbedFonts := True;
  FInitializer := TAdvGeneralPDFLibInitializer.Create;
  FInitializer.InitializeFontFallBackList(FFontFallBackList);
  FPageCount := 0;
  FOutput := TAdvPDFGraphicsLibOutputWriter.Create;
  FOutput.OnFontChanged := FontChanged;
  FOutput.OnNotifyURL := NotifyURL;
  FOutput.OnNotifyText := NotifyText;
  FOutput.OnNotifyUnicode := NotifyUnicode;
  FOutput.OnNotifyBitmap := NotifyBitmap;
  FOutput.OnNotifyShading := NotifyShading;
  FOutput.OnNotifyShadingRect := NotifyShadingRect;
  FXRefObjects := TAdvGeneralPDFLibXRefObjects.Create;
  FFontList := TAdvGeneralPDFLibFonts.Create;
  FAcroFormFields := TAdvGeneralPDFLibAcroFormFields.Create;
  FMediaBox := DefaultMediaBox;
  FCropBox := DefaultMediaBox;
  FTrimBox := DefaultMediaBox;
  FArtBox := DefaultMediaBox;
  FBleedBox := DefaultMediaBox;
  FPageOrientation := poPortrait;
  FPageSize := psLetter;
  FKeywords := TStringList.Create;
  FAllowsPrinting := True;
  FAllowsCopying := True;
  FPDFStandard := pdfNone;

  FHeader := 'Header';
  FHeaderSize := 30;
  r := RectF(5, 5, 5, 5);
  FHeaderMargins := TAdvMargins.Create(r);
  FHeaderAlignment := gtaCenter;
  FHeaderFont := TAdvPDFGraphicsLibFont.Create;

  FFooter := 'Footer';
  FFooterSize := 30;
  r := RectF(5, 5, 5, 5);
  FFooterMargins := TAdvMargins.Create(r);
  FFooterAlignment := gtaCenter;
  FFooterFont := TAdvPDFGraphicsLibFont.Create;

  FPageNumber := pnNone;
  FPageNumberFormat := '%d';
  FPageNumberSize := 30;
  r := RectF(5, 5, 5, 5);
  FPageNumberMargins := TAdvMargins.Create(r);
  FPageNumberAlignment := gtaTrailing;
  FPageNumberFont := TAdvPDFGraphicsLibFont.Create;
end;

destructor TAdvGeneralPDFLib.Destroy;
begin
  FAcroForm := nil;
  FOutputIntent := nil;

  if Assigned(FFontFallBackList) then
  begin
    FFontFallBackList.Free;
    FFontFallBackList := nil;
  end;

  if Assigned(FOSFontList) then
  begin
    FOSFontList.Free;
    FOSFontList := nil;
  end;

  if Assigned(FInitializer) then
  begin
    FInitializer.Free;
    FInitializer := nil;
  end;

  if Assigned(FFontList) then
  begin
    FFontList.Free;
    FFontList := nil;
  end;

  if Assigned(FAcroFormFields) then
  begin
    FAcroFormFields.Free;
    FAcroFormFields := nil;
  end;

  if Assigned(FXRefObjects) then
  begin
    FXRefObjects.Free;
    FXRefObjects := nil;
  end;

  if Assigned(FOutput) then
  begin
    FOutput.Free;
    FXRefObjects := nil;
  end;

  if Assigned(FFooterMargins) then
  begin
    FFooterMargins.Free;
    FFooterMargins := nil;
  end;

  if Assigned(FHeaderMargins) then
  begin
    FHeaderMargins.Free;
    FHeaderMargins := nil;
  end;

  if Assigned(FPageNumberMargins) then
  begin
    FPageNumberMargins.Free;
    FPageNumberMargins := nil;
  end;

  if Assigned(FKeywords) then
  begin
    FKeywords.Free;
    FKeywords := nil;
  end;

  if Assigned(FHeaderFont) then
  begin
    FHeaderFont.Free;
    FHeaderFont := nil;
  end;

  if Assigned(FPageNumberFont) then
  begin
    FPageNumberFont.Free;
    FPageNumberFont := nil;
  end;

  if Assigned(FFooterFont) then
  begin
    FFooterFont.Free;
    FFooterFont := nil;
  end;

  inherited;
end;

procedure TAdvGeneralPDFLib.DestroyFont(AFont: TAdvGeneralPDFLibFont);
var
  I: Integer;
  ftd: TAdvGeneralPDFLibFontDescriptor;
  ftds: TAdvGeneralPDFLibFontDescendant;
  ftff: TAdvGeneralPDFLibFontFile;
  ftu: TAdvGeneralPDFLibFontUnicode;
  x: TAdvGeneralPDFLibXRefObject;
begin
  if Assigned(AFont) then
  begin
    FFontList.Remove(AFont);
    ftd := AFont.FontDescriptor;
    ftds := AFont.FontDescendant;
    ftff := AFont.FontFile;
    ftu := AFont.FontUnicode;

    for I := FXRefObjects.Count - 1 downto 0 do
    begin
      x := FXRefObjects[I];
      if Assigned(x.XRefObject) and (x.XRefObject = ftd) then
        FXRefObjects.Delete(I)
      else if Assigned(x.XRefObject) and (x.XRefObject = ftds) then
        FXRefObjects.Delete(I)
      else if Assigned(x.XRefObject) and (x.XRefObject = ftff) then
        FXRefObjects.Delete(I)
      else if Assigned(x.XRefObject) and (x.XRefObject = ftu) then
        FXRefObjects.Delete(I)
      else if Assigned(x.XRefObject) and (x.XRefObject = AFont) then
        FXRefObjects.Delete(I)
    end;
  end;
end;

procedure TAdvGeneralPDFLib.DoAfterDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawFooter) then
    OnAfterDrawFooter(Self, APageIndex, AFooter, ARect, AGraphics);
end;

procedure TAdvGeneralPDFLib.DoAfterDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawHeader) then
    OnAfterDrawHeader(Self, APageIndex, AHeader, ARect, AGraphics);
end;

procedure TAdvGeneralPDFLib.DoAfterDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawPageNumber) then
    OnAfterDrawPageNumber(Self, APageIndex, APageNumber, ARect, AGraphics);
end;

procedure TAdvGeneralPDFLib.DoBeforeDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawFooter) then
    OnBeforeDrawFooter(Self, APageIndex, AFooter, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvGeneralPDFLib.DoBeforeDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawHeader) then
    OnBeforeDrawHeader(Self, APageIndex, AHeader, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvGeneralPDFLib.DoBeforeDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawPageNumber) then
    OnBeforeDrawPageNumber(Self, APageIndex, APageNumber, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvGeneralPDFLib.DoNewPageStarted(APageIndex: Integer);
begin
  if Assigned(OnNewPageStarted) then
    OnNewPageStarted(Self, APageIndex);
end;

procedure TAdvGeneralPDFLib.DrawFooter;
var
  al: TAdvGraphicsTextAlign;
  ft: TAdvPDFGraphicsLibFont;
  sz: TSizeF;
  r: TRectF;
  df: Boolean;
begin
  if not Assigned(FPDFGraphicsLib) then
    Exit;

  al := FPDFGraphicsLib.Alignment;
  ft := TAdvPDFGraphicsLibFont.Create;
  ft.Assign(FPDFGraphicsLib.Font);
  FPDFGraphicsLib.Alignment := FooterAlignment;
  FPDFGraphicsLib.Font.Assign(FooterFont);
  r := GetFooterRect;

  df := True;
  DoBeforeDrawFooter(Self, GetPageIndex, Footer, r, FPDFGraphicsLib, df);
  if df then
  begin
    if TAdvUtils.IsHTMLUnicode(Footer) then
    begin
      sz := FPDFGraphicsLib.DrawHTMLText(Footer, r, False, 1, True).Size;
      FPDFGraphicsLib.DrawHTMLText(Footer, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end
    else
    begin
      sz := FPDFGraphicsLib.DrawText(Footer, r, True).Size;
      FPDFGraphicsLib.DrawText(Footer, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end;

    DoAfterDrawFooter(Self, GetPageIndex, Footer, r, FPDFGraphicsLib);
  end;

  FPDFGraphicsLib.Alignment := al;
  FPDFGraphicsLib.Font.Assign(ft);
  ft.Free;
end;

procedure TAdvGeneralPDFLib.DrawHeader;
var
  al: TAdvGraphicsTextAlign;
  ft: TAdvPDFGraphicsLibFont;
  sz: TSizeF;
  r: TRectF;
  df: Boolean;
begin
  if not Assigned(FPDFGraphicsLib) then
    Exit;

  al := FPDFGraphicsLib.Alignment;
  ft := TAdvPDFGraphicsLibFont.Create;
  ft.Assign(FPDFGraphicsLib.Font);
  FPDFGraphicsLib.Alignment := HeaderAlignment;
  FPDFGraphicsLib.Font.Assign(HeaderFont);
  r := GetHeaderRect;

  df := True;
  DoBeforeDrawHeader(Self, GetPageIndex, Header, r, FPDFGraphicsLib, df);
  if df then
  begin
    if TAdvUtils.IsHTMLUnicode(Header) then
    begin
      sz := FPDFGraphicsLib.DrawHTMLText(Header, r, False, 1, True).Size;
      FPDFGraphicsLib.DrawHTMLText(Header, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end
    else
    begin
      sz := FPDFGraphicsLib.DrawText(Header, r, True).Size;
      FPDFGraphicsLib.DrawText(Header, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end;

    DoAfterDrawHeader(Self, GetPageIndex, Header, r, FPDFGraphicsLib);
  end;

  FPDFGraphicsLib.Alignment := al;
  FPDFGraphicsLib.Font.Assign(ft);
  ft.Free;
end;

procedure TAdvGeneralPDFLib.DrawPageNumber;
var
  al: TAdvGraphicsTextAlign;
  ft: TAdvPDFGraphicsLibFont;
  sz: TSizeF;
  r: TRectF;
  df: Boolean;
  s: UnicodeString;
begin
  if not Assigned(FPDFGraphicsLib) or (PageNumber = pnNone) then
    Exit;

  al := FPDFGraphicsLib.Alignment;
  ft := TAdvPDFGraphicsLibFont.Create;
  ft.Assign(FPDFGraphicsLib.Font);
  FPDFGraphicsLib.Alignment := PageNumberAlignment;
  FPDFGraphicsLib.Font.Assign(PageNumberFont);
  r := GetPageNumberRect;

  df := True;
  {$IFDEF LCLLIB}
  s := UTF8Decode(Format(UTF8Encode(PageNumberFormat), [GetPageIndex + 1]));
  {$ENDIF}
  {$IFNDEF LCLLIB}
  s := Format(PageNumberFormat, [GetPageIndex + 1]);
  {$ENDIF}
  DoBeforeDrawPageNumber(Self, GetPageIndex, s, r, FPDFGraphicsLib, df);
  if df then
  begin
    if TAdvUtils.IsHTMLUnicode(s) then
    begin
      sz := FPDFGraphicsLib.DrawHTMLText(s, r, False, 1, True).Size;
      FPDFGraphicsLib.DrawHTMLText(s, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end
    else
    begin
      sz := FPDFGraphicsLib.DrawText(s, r, True).Size;
      FPDFGraphicsLib.DrawText(s, RectF(R.Left, R.Top + (r.Height - sz.Height) / 2, r.Left + r.Width, (R.Top + (r.Height - sz.Height) / 2) + sz.Height));
    end;

    DoAfterDrawPageNumber(Self, GetPageIndex, s, r, FPDFGraphicsLib);
  end;

  FPDFGraphicsLib.Alignment := al;
  FPDFGraphicsLib.Font.Assign(ft);
  ft.Free;
end;

procedure TAdvGeneralPDFLib.DrawPage(PageIndex: Integer);
begin
end;

function TAdvGeneralPDFLib.EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
var
  ms: TMemoryStream;
  I: Integer;
  st: TStream;
  sts: TAdvPDFGraphicsLibOutputWriterStream;
  x: TAdvGeneralPDFLibXRefObject;
  pg: TAdvGeneralPDFLibPage;
  acf: TAdvGeneralPDFLibAcroFormField;
  fid: array[0..3] of UInt32;
  IDs: UnicodeString;
  P: PChar;
begin
  Result := nil;
  if FDocumentStarted then
  begin
    FinishPage;
    WriteFontList;
    WriteBitmapList;
    WriteAcroFormFieldList;
    WriteAnnotationList;
    WriteShadingList;
    WritePatternList;

    for I := 0 to FOutput.Streams.Count - 1 do
    begin
      st := FOutput.Streams[I];
      st.Position := 0;
      if st is TAdvPDFGraphicsLibOutputWriterStream then
      begin
        sts := st as TAdvPDFGraphicsLibOutputWriterStream;
        if Assigned(sts.Reference) and (sts.Reference is TAdvGeneralPDFLibXRefObject) then
        begin
          x := sts.Reference as TAdvGeneralPDFLibXRefObject;
          if x.XRefObject is TAdvGeneralPDFLibPages then
          begin
            FOutput.Stream := st as TAdvPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFPageChildRef, PDFPageCountRef], [GetPageReferences, IntToStr(FPageCount)]);
          end
          else if x.XRefObject is TAdvGeneralPDFLibOutputIntent then
          begin
            FOutput.Stream := st as TAdvPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFDestOutputProfileRef], [GetDestOutputProfileRef]);
          end
          else if x.XRefObject is TAdvGeneralPDFLibCatalog then
          begin
            FOutput.Stream := st as TAdvPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFPageAcroFormRef], [GetAcroFormRef]);
          end
          else if x.XRefObject is TAdvGeneralPDFLibPage then
          begin
            pg := x.XRefObject as TAdvGeneralPDFLibPage;
            FOutput.Stream := st as TAdvPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFPageFontRef, PDFPageXObjectRef, PDFPageContentRef, PDFPagePatternRef, PDFPageRef, PDFPageAnnotationsRef],
              [GetFontRefs(pg), GetXObjectRefs(pg), GetContentRef(pg), GetPatternRef(pg), GetPageRef(pg), GetAnnotationsRef(pg)]);
          end
          else if x.XRefObject is TAdvGeneralPDFLibContent then
          begin
            pg := (x.XRefObject as TAdvGeneralPDFLibContent).Page;
            FOutput.Stream := st as TAdvPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFPageContentRef], [GetContentRef(pg)]);
          end
          else if x.XRefObject is TAdvGeneralPDFLibAcroForm then
          begin
            FOutput.Stream := st as TAdvPDFGraphicsLibOutputWriterStream;
            FOutput.ReplaceStrings([PDFPageAcroFormRef, PDFPageAcroFormFieldsRef], [GetAcroFormRef, GetAcroFormFieldReferences]);
          end
          else if x.XRefObject is TAdvGeneralPDFLibAcroFormField then
          begin
            acf := (x.XRefObject as TAdvGeneralPDFLibAcroFormField);
            FOutput.Stream := st as TAdvPDFGraphicsLibOutputWriterStream;
            if Assigned(acf.Content) then
              FOutput.ReplaceStrings([PDFPageAcroFormFieldContentRef], [acf.Content.Ref]);
          end;
        end;
      end;
    end;

    FOutput.Stream := CreateStringStream;
    for I := 0 to FOutput.Streams.Count - 1 do
    begin
      st := FOutput.Streams[I];
      st.Position := 0;
      if st is TAdvPDFGraphicsLibOutputWriterStream then
      begin
        sts := st as TAdvPDFGraphicsLibOutputWriterStream;
        if Assigned(sts.Reference) and (sts.Reference is TAdvGeneralPDFLibXRefObject) then
        begin
          x := sts.Reference as TAdvGeneralPDFLibXRefObject;
          x.XRefOffset := FOutput.Stream.Position;
        end;
      end;

      FOutput.Stream.CopyFrom(st, st.Size);
    end;

    FOutput.Streams.Add(FOutput.Stream);
    FOutput.StreamPosition := FOutput.Stream.Size;
    XRefAddress := FOutput.StreamPosition;
    WriteXRefTable;
    FOutput.WriteString('trailer' + PDFLB);
    FOutput.WriteString('<<');
    FOutput.WriteString('/Root 1 0 R');
    FOutput.WriteString('/Info ' + IntToStr(FXRefObjects.IndexOf(FInfo.XRefObject)) + ' 0 R');
    FOutput.WriteString('/Size ' + IntToStr(FXRefObjects.Count));
    case PDFStandard of
      pdfA1:
      begin
        Randomize;
        for i := 0 to high(fid) do
          fid[i] := UInt32(Random(MaxInt));

        Inc(fid[0], GetTickCountX);
        SetLength(IDs,34);
        P := pointer(IDs);
        P[0] := '<';
        {$IFNDEF DELPHI_LLVM}
	{$IFNDEF LINUX}
        BinToHex(@fid[0], p + 1, 16);
	{$ENDIF}
        {$ENDIF}
        P[33] := '>';
        FOutput.WriteString('/ID[' + p + ' ' + p +']');
      end;
    end;
    FOutput.WriteString('>>'+ PDFLB);
    FOutput.WriteString('startxref' + PDFLB);
    FOutput.WriteString(IntToStr(XRefAddress) + PDFLB);
    FOutput.WriteString(PDFEnd + PDFLB);

    FOutput.StreamPosition := 0;
    ms := TMemoryStream.Create;
    ms.CopyFrom(FOutput.Stream, FOutput.Stream.Size);
    if FPDFFileName <> '' then
    begin
      try
        ms.SaveToFile(FPDFFileName);
        if AOpenInPDFReader then
          TAdvUtils.OpenFile(FPDFFileName);        
      finally
        ms.Free;
      end;
    end
    else
      Result := ms;

    FFontList.Clear;
    FActivePage := nil;
    FXRefObjects.Clear;
    FOutput.ClearProperties;
    FOutput.Streams.Clear;
    FDocumentStarted := False;
    FPageCount := 0;
  end;
end;

procedure TAdvGeneralPDFLib.WriteFontList;
var
  x: TAdvGeneralPDFLibXRefObject;
  ft: TAdvGeneralPDFLibFont;
  ftd: TAdvGeneralPDFLibFontDescriptor;
  ftds: TAdvGeneralPDFLibFontDescendant;
  ftff: TAdvGeneralPDFLibFontFile;
  ftu: TAdvGeneralPDFLibFontUnicode;
  os: integer;
  s: String;
  ms: TStringStream;
  ls: Integer;
  I: Integer;
  idx: Integer;
begin
  if not Assigned(FOutput) then
    Exit;

  for x in FXRefObjects do
  begin
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TAdvGeneralPDFLibFont then
      begin
        ft := x.XRefObject as TAdvGeneralPDFLibFont;
        if not ft.Inactive then
        begin
          FOutput.StartNewStream(x);
          FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
          FOutput.WriteString('<<');
          FOutput.WriteString('/Type/Font');
          FOutput.WriteString('/BaseFont/' + ft.Name);
          if ft.Unicode then
          begin
            FOutput.WriteString('/Subtype/Type0');
            FOutput.WriteString('/Encoding/Identity-H');
          end
          else
          begin
            FOutput.WriteString('/Subtype/TrueType');
            FOutput.WriteString('/Encoding/WinAnsiEncoding');
          end;

          if Assigned(ft.FontDescriptor) then
            FOutput.WriteString('/FontDescriptor ' + IntToStr(FXRefObjects.IndexOf(ft.FontDescriptor.XRefObject)) + ' 0 R');

          FOutput.WriteString('/Name' + ft.RefName);

          if Assigned(ft.FontDescendant) then
            FOutput.WriteString('/DescendantFonts[' + IntToStr(FXRefObjects.IndexOf(ft.FontDescendant.XRefObject)) + ' 0 R]');

          if ft.Unicode then
          begin
            if Assigned(ft.FontUnicode) and EmbedFonts then
              FOutput.WriteString('/ToUnicode ' + IntToStr(FXRefObjects.IndexOf(ft.FontUnicode.XRefObject)) + ' 0 R')
          end
          else
          begin
            FOutput.WriteString('/FirstChar ' + ft.FirstGlyphAsString);
            FOutput.WriteString('/LastChar ' + ft.LastGlyphAsString);
            FOutput.WriteString('/Widths['+ft.WidthsAsString+']');
          end;

          FOutput.WriteString('>>' + PDFLB);
          FOutput.WriteString('endobj' + PDFLB);
        end;
      end
      else if x.XRefObject is TAdvGeneralPDFLibFontDescriptor then
      begin
        ftd := (x.XRefObject as TAdvGeneralPDFLibFontDescriptor);
        if not ftd.Font.Inactive then
        begin
          FOutput.StartNewStream(x);
          FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
          FOutput.WriteString('<<');
          FOutput.WriteString('/Type/FontDescriptor');
          FOutput.WriteString('/FontName/' + ftd.Font.Name);
          FOutput.WriteString('/Ascent ' + IntToStr(ftd.Font.Ascent));
          FOutput.WriteString('/CapHeight ' + IntToStr(ftd.Font.CapHeight));
          FOutput.WriteString('/Descent ' + IntToStr(ftd.Font.Descent));
          FOutput.WriteString('/ItalicAngle ' + IntToStr(ftd.Font.ItalicAngle));
          FOutput.WriteString('/StemV 87');
          FOutput.WriteString('/Flags 32');
          FOutput.WriteString('/MissingWidth 600');
          FOutput.WriteString('/FontBBox[' + ftd.Font.BoxAsString+']');
          if Assigned(ftd.Font) and Assigned(ftd.Font.FontFile) and EmbedFonts then
            FOutput.WriteString('/FontFile2 ' + IntToStr(FXRefObjects.IndexOf(ftd.Font.FontFile.XRefObject)) + ' 0 R');
          FOutput.WriteString('>>' + PDFLB);
          FOutput.WriteString('endobj' + PDFLB);
        end;
      end
      else if x.XRefObject is TAdvGeneralPDFLibFontFile then
      begin
        ftff := (x.XRefObject as TAdvGeneralPDFLibFontFile);
        if not ftff.Font.Inactive then
        begin
          ftff.InitializeFontFile;
          os := ftff.Font.Initializer.GetTTFDataLength;
          ftff.Font.Initializer.CompressTTFData;
          ls := ftff.Font.Initializer.GetTTFDataCompressedLength;

          FOutput.StartNewStream(x);
          FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
          FOutput.WriteString('<<');
          FOutput.WriteString('/Length ' + IntToStr(ls));
          FOutput.WriteString('/Length1 ' + IntToStr(os));
          FOutput.WriteString('/Filter');
          FOutput.WriteString('/FlateDecode');
          FOutput.WriteString('>>' + PDFLB);
          FOutput.WriteString('stream' + PDFLB);
          ms := ftff.Font.Initializer.GetTTFDataCompressed;
          if Assigned(ms) then
            FOutput.Streams.Add(ms);
          FOutput.StartNewStream;
          FOutput.WriteString(PDFLB);
          FOutput.WriteString('endstream' + PDFLB);
          FOutput.WriteString('endobj' + PDFLB);
        end;
      end
      else if x.XRefObject is TAdvGeneralPDFLibFontDescendant then
      begin
        ftds := (x.XRefObject as TAdvGeneralPDFLibFontDescendant);
        if not ftds.Font.Inactive then
        begin
          FOutput.StartNewStream(x);
          FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
          FOutput.WriteString('<<');
          FOutput.WriteString('/Type/Font');
          FOutput.WriteString('/Subtype/CIDFontType2');
          case PDFStandard of
            pdfA1: FOutput.WriteString('/CIDToGIDMap/Identity');
          end;
          FOutput.WriteString('/BaseFont/' + ftds.Font.Name);
          FOutput.WriteString('/CIDSystemInfo<</Supplement 0/Ordering(Identity)/Registry(Adobe)>>');
          FOutput.WriteString('/DW 600');
          FOutput.WriteString('/W[' + ftds.Font.GlyphsAndWidthsAsString + ']');

          if Assigned(ftds.Font) and Assigned(ftds.Font.FontDescriptor) then
            FOutput.WriteString('/FontDescriptor ' + IntToStr(FXRefObjects.IndexOf(ftds.Font.FontDescriptor.XRefObject)) + ' 0 R');

          FOutput.WriteString('>>' + PDFLB);
          FOutput.WriteString('endobj' + PDFLB);
        end;
      end
      else if x.XRefObject is TAdvGeneralPDFLibFontUnicode then
      begin
        ftu := (x.XRefObject as TAdvGeneralPDFLibFontUnicode);
        if not ftu.Font.Inactive then
        begin
          s := '/CIDInit /ProcSet findresource begin'+ PDFLF+
          '12 dict begin'+ PDFLF + 'begincmap'+ PDFLF + '/CIDSystemInfo'+ PDFLF + '<<'+ PDFLF + '/Registry ('+
          ftu.Font.RefNameNotEscaped+'+0)'+ PDFLF + '/Ordering (UCS)'+ PDFLF + '/Supplement 0'+ PDFLF + '>> def'+ PDFLF+
          '/CMapName /'+ftu.font.RefNameNotEscaped+'+0 def'+ PDFLF + '/CMapType 2 def'+ PDFLF+
          '1 begincodespacerange'+ PDFLF + '<'+ FOutput.AddHex4(ftu.Font.FirstGlyph)+'> <'+
          FOutput.AddHex4(ftu.Font.LastGlyph)+'>'+ PDFLF + 'endcodespacerange'+ PDFLF;

          s := s + IntToStr(ftu.Font.UsedCharArray.Count) + ' beginbfchar' + PDFLF;
          for i := 0 to ftu.Font.UsedCharArray.Count - 1 do
          begin
            idx := ftu.Font.CharArray.IndexOf(ftu.Font.UsedCharArray[I]);
            if (idx > -1) then
              s := s + '<' + FOutput.AddHex4(ftu.Font.CharWidths[idx].g)+'><'+ FOutput.AddHex4(ftu.Font.CharArray.v[idx])+ '>'+PDFLF;
          end;
          s := s + 'endbfchar' + PDFLF;
          s := s + 'endcmap' + PDFLF + 'CMapName currentdict /CMap defineresource pop'+PDFLF+'end'+PDFLF+'end';

          ms := FOutput.CompressString(s);
          ls := 0;
          if Assigned(ms) then
            ls := ms.Size;

          FOutput.StartNewStream(x);
          FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
          FOutput.WriteString('<<');
          FOutput.WriteString('/Length ' + IntToStr(ls));
          FOutput.WriteString('/Filter');
          FOutput.WriteString('/FlateDecode');
          FOutput.WriteString('>>' + PDFLB);
          FOutput.WriteString('stream' + PDFLB);
          if Assigned(ms) then
            FOutput.Streams.Add(ms);
          FOutput.StartNewStream;
          FOutput.WriteString(PDFLB);
          FOutput.WriteString('endstream' + PDFLB);
          FOutput.WriteString('endobj' + PDFLB);
        end;
      end;
    end;
  end;
end;

procedure TAdvGeneralPDFLib.WriteAcroFormFieldList;
var
  x: TAdvGeneralPDFLibXRefObject;
begin
  if not Assigned(FOutput) then
    Exit;

  for x in FXRefObjects do
  begin
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TAdvGeneralPDFLibAcroFormField then
      begin
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
//        FOutput.WriteString('/DA (/Helv 12 Tf 0 g)');
//        FOutput.WriteString('/F 4');
//        FOutput.WriteString('/FT Tx');
//        FOutput.WriteString('/Rect [9, 680, 297, 702]');
//        FOutput.WriteString('/Subtype /Widget');
//        FOutput.WriteString('/Type /Annot');
//        FOutput.WriteString('/T (SimpleText)');
//        FOutput.WriteString('/V (A Single line of text in one style)');
//        FOutput.WriteString('/AP <</N '+PDFPageAcroFormFieldContentRef+'>>');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end
      else if x.XRefObject is TAdvGeneralPDFLibAcroFormFieldContent then
      begin
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('stream' + PDFLB);
//        FOutput.WriteString('/Tx BMC' + PDFLB);
//        FOutput.WriteString('q' + PDFLB);
//        FOutput.WriteString('BT' + PDFLB);
//        FOutput.WriteString('0 0 1 rg' + PDFLB);
//        FOutput.WriteString('/Ti 12 Tf' + PDFLB);
//        FOutput.WriteString('1 0 0 1 100 100 Tm' + PDFLB);
//        FOutput.WriteString('0 0 Td' + PDFLB);
//        FOutput.WriteString('( The quick brown fox ) Tj' + PDFLB);
//        FOutput.WriteString('0 −13 Td' + PDFLB);
//        FOutput.WriteString('( ate the lazy mouse. ) Tj' + PDFLB);
//        FOutput.WriteString('ET' + PDFLB);
//        FOutput.WriteString('Q' + PDFLB);
//        FOutput.WriteString('EMC' + PDFLB);
        FOutput.WriteString('endstream' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end;
    end;
  end;
end;

procedure TAdvGeneralPDFLib.WriteAnnotationList;
var
  x: TAdvGeneralPDFLibXRefObject;
  an: TAdvGeneralPDFLibAnnotation;
  u: TAdvGeneralPDFLibURL;
begin
  if not Assigned(FOutput) then
    Exit;

  for x in FXRefObjects do
  begin
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TAdvGeneralPDFLibAnnotation then
      begin
        an := x.XRefObject as TAdvGeneralPDFLibAnnotation;
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('/Type/Annot');
        if an is TAdvGeneralPDFLibURL then
        begin
          u := an as TAdvGeneralPDFLibURL;
          FOutput.WriteString('/Subtype/Link');
          FOutput.WriteString('/A');
          FOutput.WriteString('<<');
          {$IFDEF LCLLIB}
          FOutput.WriteString('/Type/Action/S/URI/URI('+UTF8Encode(u.URL) + ')');
          {$ENDIF}
          {$IFNDEF LCLLIB}
          FOutput.WriteString('/Type/Action/S/URI/URI('+u.URL + ')');
          {$ENDIF}
          FOutput.WriteString('>>');
          FOutput.WriteString('/BS');
          FOutput.WriteString('<<');
          FOutput.WriteString('/W 0/S/S');
          FOutput.WriteString('>>');
        end;
        FOutput.WriteString('/Rect['+ an.RectAsString +']');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end;
    end;
  end;
end;

procedure TAdvGeneralPDFLib.WriteBitmapList;
var
  x: TAdvGeneralPDFLibXRefObject;
  bmp: TAdvGeneralPDFLibBitmap;
  ms: TMemoryStream;
  ls: Integer;
begin
  if not Assigned(FOutput) then
    Exit;

  for x in FXRefObjects do
  begin
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TAdvGeneralPDFLibBitmap then
      begin
        bmp := x.XRefObject as TAdvGeneralPDFLibBitmap;

        ms := TAdvUtils.ConvertBitmapToJPEGStream(bmp.Bitmap, bmp.Quality, bmp.BackgroundColor);
        ls := 0;
        if Assigned(ms) then
          ls := ms.Size;

        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('/Type/XObject');
        FOutput.WriteString('/Subtype/Image');
        FOutput.WriteString('/Filter/DCTDecode');
        FOutput.WriteString('/Width ' + IntToStr(bmp.Bitmap.Width));
        FOutput.WriteString('/Height ' + IntToStr(bmp.Bitmap.Height));
        FOutput.WriteString('/ColorSpace/DeviceRGB');
        FOutput.WriteString('/BitsPerComponent 8');
        FOutput.WriteString('/Length ' + IntToStr(ls));
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('stream' + PDFLB);
        if Assigned(ms) then
          FOutput.Streams.Add(ms);
        FOutput.StartNewStream;
        FOutput.WriteString(PDFLB);
        FOutput.WriteString('endstream' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end
    end;
  end;
end;

procedure TAdvGeneralPDFLib.WritePatternList;
var
  x: TAdvGeneralPDFLibXRefObject;
  p: TAdvGeneralPDFLibPattern;
begin
  if not Assigned(FOutput) then
    Exit;

  for x in FXRefObjects do
  begin
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TAdvGeneralPDFLibPattern then
      begin
        p := x.XRefObject as TAdvGeneralPDFLibPattern;
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('/Type/Pattern');
        FOutput.WriteString('/PatternType 2');
        if Assigned(p.Shading) then
          FOutput.WriteString('/Shading ' + IntToStr(FXRefObjects.IndexOf(p.Shading.XRefObject)) + ' 0 R');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end
    end;
  end;
end;

procedure TAdvGeneralPDFLib.WriteShadingList;
var
  x: TAdvGeneralPDFLibXRefObject;
  f: TAdvGeneralPDFLibShadingFunction;
  sf: TAdvGeneralPDFLibShadingSubFunction;
  s: TAdvGeneralPDFLibShading;
begin
  if not Assigned(FOutput) then
    Exit;

  for x in FXRefObjects do
  begin
    if Assigned(x.XRefObject) then
    begin
      if x.XRefObject is TAdvGeneralPDFLibShading then
      begin
        s := x.XRefObject as TAdvGeneralPDFLibShading;
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        if s is TAdvGeneralPDFLibLinearGradient then
          FOutput.WriteString('/ShadingType 2');

        FOutput.WriteString('/Extend[false false]');
        FOutput.WriteString('/ColorSpace/DeviceRGB');
        FOutput.WriteString('/Coords['+s.RectAsString+']');
        if Assigned(s.&Function) then
          FOutput.WriteString('/Function ' + IntToStr(FXRefObjects.IndexOf(s.&Function.XRefObject)) + ' 0 R');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end
      else if x.XRefObject is TAdvGeneralPDFLibShadingFunction then
      begin
        f := x.XRefObject as TAdvGeneralPDFLibShadingFunction;
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('/FunctionType 3');
        FOutput.WriteString('/Domain[0 1]');
        FOutput.WriteString('/Encode[0 1 0 1]');
        FOutput.WriteString('/Bounds[1]');
        FOutput.WriteString('/Functions['+ f.SubFunctionsRef +']');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end
      else if x.XRefObject is TAdvGeneralPDFLibShadingSubFunction then
      begin
        sf := x.XRefObject as TAdvGeneralPDFLibShadingSubFunction;
        FOutput.StartNewStream(x);
        FOutput.WriteString(IntToStr(FXRefObjects.IndexOf(x)) + ' 0 obj' + PDFLB);
        FOutput.WriteString('<<');
        FOutput.WriteString('/FunctionType 2');
        FOutput.WriteString('/Domain[0 1]');
        FOutput.WriteString('/C0['+FOutput.ConvertColorToString(sf.FillColor)+']');
        FOutput.WriteString('/C1['+FOutput.ConvertColorToString(sf.FillColorTo)+']');
        FOutput.WriteString('/N 1');
        FOutput.WriteString('>>' + PDFLB);
        FOutput.WriteString('endobj' + PDFLB);
      end;
    end;
  end;
end;

procedure TAdvGeneralPDFLib.WriteXRefTable;
var
  s, sref: string;
  cnt: integer;
  x: TAdvGeneralPDFLibXRefObject;
begin
  sref := '';
  cnt := 0;
  for x in FXRefObjects do
  begin
    if x.InXRefList then
    begin
      Inc(cnt);
      sref := sref + x.GenerateXRefValue + PDFLB;
    end;
  end;

  s := 'xref' + PDFLB + '0 ' + IntToStr(cnt) + PDFLB;
  s := s + sref;
  FOutput.WriteString(S);
end;

procedure TAdvGeneralPDFLib.FinishPage;
var
  I: Integer;
  ft: TAdvGeneralPDFLibFont;
  p: TAdvGeneralPDFLibPattern;
  bmp: TAdvGeneralPDFLibBitmap;
  ms: TStringStream;
  a: TAdvGeneralPDFLibAnnotation;
begin
  if FPageStarted then
  begin
    FPageStarted := False;

    for I := 0 to FActivePage.FontList.Count - 1 do
    begin
      ft := FActivePage.FontList[I];
      FOutput.ReplaceString(ft.RefNameOriginal, ft.RefName);
    end;

    for I := 0 to FActivePage.Patterns.Count - 1 do
    begin
      p := FActivePage.Patterns[I];
      FOutput.ReplaceString(p.RefNameOriginal, p.RefName);
    end;

    for I := 0 to FActivePage.Bitmaps.Count - 1 do
    begin
      bmp := FActivePage.Bitmaps[I];
      FOutput.ReplaceString(bmp.RefNameOriginal, bmp.RefName);
    end;

    for I := 0 to FActivePage.Annotations.Count - 1 do
    begin
      a := FActivePage.Annotations[I];
      FOutput.ReplaceString(a.RefNameOriginal, a.RefName);
    end;

    ms := FOutput.FinishContentStream(PDFPageContentLengthRef);
    if Assigned(ms) then
      FOutput.Streams.Add(ms);
    FOutput.StartNewStream;
    FOutput.WriteString(PDFLB);
    FOutput.WriteString('endstream' + PDFLB);
    FOutput.WriteString('endobj' + PDFLB);
    FOutput.ClearProperties;
  end;
end;

procedure TAdvGeneralPDFLib.FontChanged(Sender: TObject);
begin
  UpdateFontList;
end;

function  TAdvGeneralPDFLib.UnlockWithPassword(Password: String): Boolean;
begin
  Result := False;
end;

procedure TAdvGeneralPDFLib.InsertPage(PageIndex: Integer);
begin
  NewPage;
  if Assigned(FActivePage) then
    FActivePage.InsertPageNumber := PageIndex;
end;

function TAdvGeneralPDFLib.IsDocumentOpened: Boolean;
begin
  Result := False;
end;

function TAdvGeneralPDFLib.GetAcroFormFieldReferences: string;
var
  x: TAdvGeneralPDFLibXRefObject;
begin
  Result := '';
  for x in FXRefObjects do
    if Assigned(x.XRefObject) then
      if x.XRefObject is TAdvGeneralPDFLibAcroFormField then
        Result := Result + IntToStr(FXRefObjects.IndexOf(x)) + ' 0 R ';

  Result := Trim(Result);
end;

function TAdvGeneralPDFLib.GetAcroFormRef: string;
begin
  Result := '';
  if Assigned(FAcroForm) and Assigned(FAcroForm.XRefObject) then
    Result := IntToStr(FXRefObjects.IndexOf(FAcroForm.XRefObject));

  Result := Trim(Result);
end;

function TAdvGeneralPDFLib.GetAllowsCopying: Boolean;
begin
  Result := AllowsCopying;
end;

function TAdvGeneralPDFLib.GetAllowsPrinting: Boolean;
begin
  Result := AllowsPrinting;
end;

function TAdvGeneralPDFLib.GetArtBox: TRectF;
begin
  Result := ArtBox;
end;

function TAdvGeneralPDFLib.GetAuthor: String;
begin
  Result := Author;
end;

function TAdvGeneralPDFLib.GetPictureContainer: TPictureContainer;
begin
  Result := nil;
  if Assigned(FPDFGraphicsLib) then
    Result := FPDFGraphicsLib.PictureContainer;
end;

function TAdvGeneralPDFLib.GetBleedBox: TRectF;
begin
  Result := BleedBox;
end;

function TAdvGeneralPDFLib.GetContentRef(APage: TAdvGeneralPDFLibPage): string;
begin
  Result := '';
  if Assigned(APage.Content) and Assigned(APage.Content.XRefObject) then
    Result := IntToStr(FXRefObjects.IndexOf(APage.Content.XRefObject));

  Result := Trim(Result);
end;

function TAdvGeneralPDFLib.GetCreationDate: String;
begin
  Result := CreationDate;
end;

function TAdvGeneralPDFLib.GetCreator: String;
begin
  Result := Creator;
end;

function TAdvGeneralPDFLib.GetCropBox: TRectF;
begin
  Result := CropBox;
end;

procedure TAdvGeneralPDFLib.GetDocumentInfo;
begin
end;

function TAdvGeneralPDFLib.GetEmbedFonts: Boolean;
begin
  Result := EmbedFonts;
end;

function TAdvGeneralPDFLib.GetFontFallBackList: TStrings;
begin
  Result := FontFallBackList;
end;

function TAdvGeneralPDFLib.GetFontRefs(APage: TAdvGeneralPDFLibPage): String;
var
  I: Integer;
begin
  Result := '';
  if Assigned(APage) then
  begin
    for I := 0 to APage.FontList.Count - 1 do
    begin
      if not APage.FontList[I].Inactive then
        Result := Result + APage.FontList[I].Ref;
    end;
  end;

  Result := Trim(Result);
end;

function TAdvGeneralPDFLib.GetFooter: UnicodeString;
begin
  Result := Footer;
end;

function TAdvGeneralPDFLib.GetFooterAlignment: TAdvGraphicsTextAlign;
begin
  Result := FooterAlignment;
end;

function TAdvGeneralPDFLib.GetFooterFont: TAdvPDFGraphicsLibFont;
begin
  Result := FFooterFont;
end;

function TAdvGeneralPDFLib.GetFooterMargins: TAdvMargins;
begin
  Result := FooterMargins;
end;

function TAdvGeneralPDFLib.GetFooterRect: TRectF;
begin
  Result := RectF(FMediaBox.Left + FFooterMargins.Left, FMediaBox.Bottom - FFooterSize - FFooterMargins.Bottom, FMediaBox.Width - FFooterMargins.Right, FMediaBox.Bottom - FFooterMargins.Bottom);
end;

function TAdvGeneralPDFLib.GetFooterSize: Single;
begin
  Result := FooterSize;
end;

function TAdvGeneralPDFLib.GetHeader: UnicodeString;
begin
  Result := Header;
end;

function TAdvGeneralPDFLib.GetHeaderAlignment: TAdvGraphicsTextAlign;
begin
  Result := HeaderAlignment;
end;

function TAdvGeneralPDFLib.GetHeaderFont: TAdvPDFGraphicsLibFont;
begin
  Result := FHeaderFont;
end;

function TAdvGeneralPDFLib.GetHeaderMargins: TAdvMargins;
begin
  Result := HeaderMargins;
end;

function TAdvGeneralPDFLib.GetHeaderRect: TRectF;
begin
  Result := RectF(FMediaBox.Left + FHeaderMargins.Left, FMediaBox.Top + FHeaderMargins.Top, FMediaBox.Width - FHeaderMargins.Right, FMediaBox.Top + FHeaderMargins.Top + FHeaderSize);
end;

function TAdvGeneralPDFLib.GetHeaderSize: Single;
begin
  Result := HeaderSize;
end;

function TAdvGeneralPDFLib.GetPageNumberFormat: UnicodeString;
begin
  Result := PageNumberFormat;
end;

function TAdvGeneralPDFLib.GetPageNumber: TAdvPDFLibPageNumber;
begin
  Result := PageNumber;
end;

function TAdvGeneralPDFLib.GetPageNumberAlignment: TAdvGraphicsTextAlign;
begin
  Result := PageNumberAlignment;
end;

function TAdvGeneralPDFLib.GetPageNumberFont: TAdvPDFGraphicsLibFont;
begin
  Result := FPageNumberFont;
end;

function TAdvGeneralPDFLib.GetPageNumberMargins: TAdvMargins;
begin
  Result := PageNumberMargins;
end;

function TAdvGeneralPDFLib.GetPageNumberRect: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  case PageNumber of
    pnHeader: Result := RectF(FMediaBox.Left + FPageNumberMargins.Left, FMediaBox.Top + FPageNumberMargins.Top, FMediaBox.Width - FPageNumberMargins.Right, FMediaBox.Top + FPageNumberMargins.Top + FPageNumberSize);
    pnFooter: Result := RectF(FMediaBox.Left + FPageNumberMargins.Left, FMediaBox.Bottom - FPageNumberSize - FPageNumberMargins.Bottom, FMediaBox.Width - FPageNumberMargins.Right, FMediaBox.Bottom - FPageNumberMargins.Bottom);
  end;
end;

function TAdvGeneralPDFLib.GetPageNumberSize: Single;
begin
  Result := PageNumberSize;
end;

function TAdvGeneralPDFLib.GetKeywordsString: string;
var
  s: string;
begin
  Result := '';
  for s in FKeywords do
    Result := Result + s + ' ';

  Result := Trim(Result);
end;

function TAdvGeneralPDFLib.GetKeywords: TStrings;
begin
  Result := Keywords;
end;

function TAdvGeneralPDFLib.GetMediaBox: TRectF;
begin
  Result := MediaBox;
end;

function TAdvGeneralPDFLib.GetModificationDate: string;
begin
  Result := ModificationDate;
end;

function TAdvGeneralPDFLib.GetOnAfterDrawFooter: TAdvPDFLibAfterDrawFooterEvent;
begin
  Result := FOnAfterDrawFooter;
end;

function TAdvGeneralPDFLib.GetOnAfterDrawHeader: TAdvPDFLibAfterDrawHeaderEvent;
begin
  Result := FOnAfterDrawHeader;
end;

function TAdvGeneralPDFLib.GetOnAfterDrawPageNumber: TAdvPDFLibAfterDrawPageNumberEvent;
begin
  Result := FOnAfterDrawPageNumber;
end;

function TAdvGeneralPDFLib.GetOnBeforeDrawFooter: TAdvPDFLibBeforeDrawFooterEvent;
begin
  Result := FOnBeforeDrawFooter;
end;

function TAdvGeneralPDFLib.GetOnBeforeDrawHeader: TAdvPDFLibBeforeDrawHeaderEvent;
begin
  Result := FOnBeforeDrawHeader
end;

function TAdvGeneralPDFLib.GetOnBeforeDrawPageNumber: TAdvPDFLibBeforeDrawPageNumberEvent;
begin
  Result := FOnBeforeDrawPageNumber
end;

function TAdvGeneralPDFLib.GetOnNewPageStarted: TAdvPDFLibNewPageStartedEvent;
begin
  Result := FOnNewPageStarted;
end;

function TAdvGeneralPDFLib.GetDestOutputProfileRef: String;
begin
  Result := '';
  if Assigned(FDestOutputProfile) and Assigned(FDestOutputProfile.XRefObject) then
    Result := IntToStr(FXRefObjects.IndexOf(FDestOutputProfile.XRefObject));

  Result := Trim(Result);
end;

function TAdvGeneralPDFLib.GetOwnerPassword: String;
begin
  Result := OwnerPassword;
end;

function TAdvGeneralPDFLib.GetPageCount: Integer;
begin
  Result := FPageCount;
end;

function TAdvGeneralPDFLib.GetPageHeight: Single;
begin
  Result := FPageHeight;
end;

function TAdvGeneralPDFLib.GetPageIndex: Integer;
begin
  Result := 0;
  if Assigned(FActivePage) then
    Result := FActivePage.PageNumber;
end;

procedure TAdvGeneralPDFLib.GetPageInfo(PageIndex: Integer);
begin
end;

function TAdvGeneralPDFLib.GetPageOrientation: TAdvPDFLibPageOrientation;
begin
  Result := Orientation;
end;

function TAdvGeneralPDFLib.GetPageRef(APage: TAdvGeneralPDFLibPage): string;
begin
  Result := '';
  if Assigned(APage) and Assigned(APage.XRefObject) then
    Result := IntToStr(FXRefObjects.IndexOf(APage.XRefObject));

  Result := Trim(Result);
end;

function ComparePageNumbers(const Item1, Item2: TAdvGeneralPDFLibPage): Integer;
begin
  Result := CompareValue(Item1.InsertPageNumber, Item2.InsertPageNumber);
end;

function TAdvGeneralPDFLib.GetPageReferences: String;
var
  x: TAdvGeneralPDFLibXRefObject;
  l: TAdvGeneralPDFLibPageList;
  I: Integer;
begin
  Result := '';

  l := TAdvGeneralPDFLibPageList.Create;
  try
    for x in FXRefObjects do
      if Assigned(x.XRefObject) then
        if x.XRefObject is TAdvGeneralPDFLibPage then
          l.Add(x.XRefObject as TAdvGeneralPDFLibPage);

    {$IFNDEF LCLLIB}
    l.Sort(FComparePageNumbers);
    {$ENDIF}
    {$IFDEF LCLLIB}
    l.Sort(@ComparePageNumbers);
    {$ENDIF}

    for I := 0 to l.Count - 1 do
      Result := Result + IntToStr(FXRefObjects.IndexOf(l[I].XRefObject)) + ' 0 R ';

    Result := Trim(Result);
  finally
    l.Free;
  end;
end;

function TAdvGeneralPDFLib.GetPageSize: TAdvPDFLibPageSize;
begin
  Result := PageSize;
end;

function TAdvGeneralPDFLib.GetPageWidth: Single;
begin
  Result := FPageWidth;
end;

function TAdvGeneralPDFLib.GetShadingRef(APage: TAdvGeneralPDFLibPage): string;
var
  I: Integer;
begin
  Result := '';
  if Assigned(APage) then
    for I := 0 to APage.Shadings.Count - 1 do
      Result := Result + APage.Shadings[I].Ref;

  Result := Trim(Result);
end;

function TAdvGeneralPDFLib.GetAnnotationsRef(APage: TAdvGeneralPDFLibPage): string;
var
  I: Integer;
  ft: TAdvGeneralPDFLibAcroFormField;
  an: TAdvGeneralPDFLibAnnotation;
begin
  Result := '';
  if Assigned(APage) then
  begin
    for I := 0 to APage.Annotations.Count - 1 do
    begin
      an := APage.Annotations[I];
      ft := an.AcroFormField;
      if Assigned(ft) then
        Result := Result + ft.Ref + ' '
      else
        Result := Result + an.Ref + ' ';
    end;
  end;

  Result := Trim(Result);
end;

function TAdvGeneralPDFLib.GetPatternRef(APage: TAdvGeneralPDFLibPage): string;
var
  I: Integer;
begin
  Result := '';
  if Assigned(APage) then
    for I := 0 to APage.Patterns.Count - 1 do
      Result := Result + APage.Patterns[I].Ref;

  Result := Trim(Result);
end;

function TAdvGeneralPDFLib.GetPDFGraphicsExLib: IAdvCustomPDFGraphicsExLib;
begin
  Result := FPDFGraphicsExLib;
end;

function TAdvGeneralPDFLib.GetPDFGraphicsLib: IAdvCustomPDFGraphicsLib;
begin
  Result := FPDFGraphicsLib;
end;

function TAdvGeneralPDFLib.GetPDFInitializationLib: IAdvCustomPDFInitializationLib;
begin
  Result := FPDFInitializationLib;
end;

function TAdvGeneralPDFLib.GetPDFStandard: TAdvPDFLibStandard;
begin
  Result := PDFStandard;
end;

function TAdvGeneralPDFLib.GetProducer: String;
begin
  Result := Producer;
end;

function TAdvGeneralPDFLib.GetSubject: String;
begin
  Result := Subject;
end;

function TAdvGeneralPDFLib.GetTitle: String;
begin
  Result := Title;
end;

function TAdvGeneralPDFLib.GetTrimBox: TRectF;
begin
  Result := TrimBox;
end;

function TAdvGeneralPDFLib.GetUserPassword: String;
begin
  Result := UserPassword;
end;

function TAdvGeneralPDFLib.GetXObjectRefs(APage: TAdvGeneralPDFLibPage): string;
var
  I: Integer;
begin
  Result := '';
  if Assigned(APage) then
    for I := 0 to APage.XObjectList.Count - 1 do
      Result := Result + APage.XObjectList[I].Ref;

  Result := Trim(Result);
end;

procedure TAdvGeneralPDFLib.SaveDocumentFromStream(FileStream: TMemoryStream;
  FileName: String);
begin
end;

function TAdvGeneralPDFLib.SearchForFont(ABaseFontName: string; AUnicodeFont: Boolean): TAdvGeneralPDFLibFont;
var
  ft: TAdvGeneralPDFLibFont;
begin
  Result := nil;
  for ft in FFontList do
  begin
    if not ft.Inactive and (ft.Name = ABaseFontName) and (ft.Unicode = AUnicodeFont) then
    begin
      Result := ft;
      Break;
    end;
  end;
end;

procedure TAdvGeneralPDFLib.SetAllowsCopying(const Value: Boolean);
begin
  AllowsCopying := Value;
end;

procedure TAdvGeneralPDFLib.SetAllowsPrinting(const Value: Boolean);
begin
  AllowsPrinting := Value;
end;

procedure TAdvGeneralPDFLib.SetArtBox(const Value: TRectF);
begin
  ArtBox := Value;
end;

procedure TAdvGeneralPDFLib.SetAuthor(const Value: String);
begin
  Author := Value;
end;

procedure TAdvGeneralPDFLib.SetPictureContainer(
  const Value: TPictureContainer);
begin
  if Assigned(FPDFGraphicsLib) then
    FPDFGraphicsLib.PictureContainer := Value;
end;

procedure TAdvGeneralPDFLib.SetBleedBox(const Value: TRectF);
begin
  BleedBox := Value;
end;

procedure TAdvGeneralPDFLib.SetCreator(const Value: String);
begin
  Creator := Value;
end;

procedure TAdvGeneralPDFLib.SetCropBox(const Value: TRectF);
begin
  CropBox := Value;
end;

procedure TAdvGeneralPDFLib.SetEmbedFonts(const Value: Boolean);
begin
  EmbedFonts := Value;
end;

procedure TAdvGeneralPDFLib.SetFontFallBackList(const Value: TStrings);
begin
  FontFallBackList.Assign(Value);
end;

procedure TAdvGeneralPDFLib.SetFooter(const Value: UnicodeString);
begin
  Footer := Value;
end;

procedure TAdvGeneralPDFLib.SetFooterAlignment(const Value: TAdvGraphicsTextAlign);
begin
  FooterAlignment := Value;
end;

procedure TAdvGeneralPDFLib.SetFooterFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TAdvGeneralPDFLib.SetFooterMargins(const Value: TAdvMargins);
begin
  FooterMargins.Assign(Value);
end;

procedure TAdvGeneralPDFLib.SetFooterSize(const Value: Single);
begin
  FooterSize := Value;
end;

procedure TAdvGeneralPDFLib.SetPDFGraphicsExLib(
  const Value: IAdvCustomPDFGraphicsExLib);
begin
  FPDFGraphicsExLib := Value;
end;

procedure TAdvGeneralPDFLib.SetPDFGraphicsLib(
  const Value: IAdvCustomPDFGraphicsLib);
begin
  FPDFGraphicsLib := Value;
end;

procedure TAdvGeneralPDFLib.SetPDFInitializationLib(
  const Value: IAdvCustomPDFInitializationLib);
begin
  FPDFInitializationLib := Value;
end;

procedure TAdvGeneralPDFLib.SetPDFStandard(
  const Value: TAdvPDFLibStandard);
begin
  PDFStandard := Value;
end;

procedure TAdvGeneralPDFLib.SetHeader(const Value: UnicodeString);
begin
  FHeader := Value;
end;

procedure TAdvGeneralPDFLib.SetHeaderAlignment(const Value: TAdvGraphicsTextAlign);
begin
  HeaderAlignment := Value;
end;

procedure TAdvGeneralPDFLib.SetHeaderFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TAdvGeneralPDFLib.SetHeaderMargins(const Value: TAdvMargins);
begin
  HeaderMargins.Assign(Value);
end;

procedure TAdvGeneralPDFLib.SetHeaderSize(const Value: Single);
begin
  HeaderSize := Value;
end;

procedure TAdvGeneralPDFLib.SetPageNumberFormat(const Value: UnicodeString);
begin
  FPageNumberFormat := Value;
end;

procedure TAdvGeneralPDFLib.SetPageNumber(const Value: TAdvPDFLibPageNumber);
begin
  FPageNumber := Value;
end;

procedure TAdvGeneralPDFLib.SetPageNumberAlignment(const Value: TAdvGraphicsTextAlign);
begin
  FPageNumberAlignment := Value;
end;

procedure TAdvGeneralPDFLib.SetPageNumberFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  FPageNumberFont.Assign(Value);
end;

procedure TAdvGeneralPDFLib.SetPageNumberMargins(const Value: TAdvMargins);
begin
  FPageNumberMargins.Assign(Value);
end;

procedure TAdvGeneralPDFLib.SetPageNumberSize(const Value: Single);
begin
  FPageNumberSize := Value;
end;

procedure TAdvGeneralPDFLib.SetKeywords(const Value: TStrings);
begin
  Keywords.Assign(Value);
end;

procedure TAdvGeneralPDFLib.SetMediaBox(const Value: TRectF);
begin
  MediaBox := Value;
end;

procedure TAdvGeneralPDFLib.SetOnAfterDrawFooter(
  const Value: TAdvPDFLibAfterDrawFooterEvent);
begin
  FOnAfterDrawFooter := Value;
end;

procedure TAdvGeneralPDFLib.SetOnAfterDrawHeader(
  const Value: TAdvPDFLibAfterDrawHeaderEvent);
begin
  FOnAfterDrawHeader := Value;
end;

procedure TAdvGeneralPDFLib.SetOnAfterDrawPageNumber(
  const Value: TAdvPDFLibAfterDrawPageNumberEvent);
begin
  FOnAfterDrawPageNumber := Value;
end;

procedure TAdvGeneralPDFLib.SetOnBeforeDrawFooter(
  const Value: TAdvPDFLibBeforeDrawFooterEvent);
begin
  FOnBeforeDrawFooter := Value;
end;

procedure TAdvGeneralPDFLib.SetOnBeforeDrawHeader(
  const Value: TAdvPDFLibBeforeDrawHeaderEvent);
begin
  FOnBeforeDrawHeader := Value;
end;

procedure TAdvGeneralPDFLib.SetOnBeforeDrawPageNumber(
  const Value: TAdvPDFLibBeforeDrawPageNumberEvent);
begin
  FOnBeforeDrawPageNumber := Value;
end;

procedure TAdvGeneralPDFLib.SetOnNewPageStarted(
  const Value: TAdvPDFLibNewPageStartedEvent);
begin
  FOnNewPageStarted := Value;
end;

procedure TAdvGeneralPDFLib.SetOwnerPassword(const Value: String);
begin
  OwnerPassword := Value;
end;

procedure TAdvGeneralPDFLib.SetPageHeight(const Value: Single);
begin
  FPageHeight := Value;
  UpdateBoxRect;
end;

procedure TAdvGeneralPDFLib.SetPageOrientation(
  const Value: TAdvPDFLibPageOrientation);
begin
  Orientation := Value;
  UpdateBoxRect;
end;

procedure TAdvGeneralPDFLib.SetPageSize(
  const Value: TAdvPDFLibPageSize);
begin
  PageSize := Value;
  UpdateBoxRect;
end;

procedure TAdvGeneralPDFLib.SetPageWidth(const Value: Single);
begin
  FPageWidth := Value;
  UpdateBoxRect;
end;

procedure TAdvGeneralPDFLib.SetSubject(const Value: String);
begin
  Subject := Value;
end;

procedure TAdvGeneralPDFLib.SetTitle(const Value: String);
begin
  Title := Value;
end;

procedure TAdvGeneralPDFLib.SetTrimBox(const Value: TRectF);
begin
  TrimBox := Value;
end;

procedure TAdvGeneralPDFLib.SetUserPassword(const Value: String);
begin
  UserPassword := Value;
end;

procedure TAdvGeneralPDFLib.UpdateBoxRect;
var
  w, h: Single;
  g: IAdvCustomPDFInitializationLib;
begin
  case PageSize of
    psA0: FMediaBox := RectF(0, 0, 2384, 3370);
    psA1: FMediaBox := RectF(0, 0, 1684, 2384);
    psA2: FMediaBox := RectF(0, 0, 1190, 1684);
    psA3: FMediaBox := RectF(0, 0, 842, 1190);
    psA4: FMediaBox := RectF(0, 0, 595, 842);
    psA5: FMediaBox := RectF(0, 0, 420, 595);
    psA6: FMediaBox := RectF(0, 0, 298, 420);
    psA7: FMediaBox := RectF(0, 0, 210, 298);
    psA8: FMediaBox := RectF(0, 0, 148, 210);
    psB0: FMediaBox := RectF(0, 0, 2835, 4008);
    psB1: FMediaBox := RectF(0, 0, 2004, 2835);
    psB2: FMediaBox := RectF(0, 0, 1417, 2004);
    psB3: FMediaBox := RectF(0, 0, 1001, 1417);
    psB4: FMediaBox := RectF(0, 0, 709, 1001);
    psB5: FMediaBox := RectF(0, 0, 499, 709);
    psB6: FMediaBox := RectF(0, 0, 354, 499);
    psB7: FMediaBox := RectF(0, 0, 249, 354);
    psB8: FMediaBox := RectF(0, 0, 176, 249);
    psB9: FMediaBox := RectF(0, 0, 125, 176);
    psB10: FMediaBox := RectF(0, 0, 88, 125);
    psC2: FMediaBox := RectF(0, 0, 1837, 578);
    psC3: FMediaBox := RectF(0, 0, 578, 919);
    psC4: FMediaBox := RectF(0, 0, 919, 649);
    psC5: FMediaBox := RectF(0, 0, 649, 459);
    psC6: FMediaBox := RectF(0, 0, 459, 323);
    psD0: FMediaBox := RectF(0, 0, 3090, 2186);
    psSRA0: FMediaBox := RectF(0, 0, 3628, 2551);
    psSRA1: FMediaBox := RectF(0, 0, 2551, 1814);
    psSRA2: FMediaBox := RectF(0, 0, 1814, 1276);
    psSRA3: FMediaBox := RectF(0, 0, 1276, 907);
    psSRA4: FMediaBox := RectF(0, 0, 907, 638);
    psRA0: FMediaBox := RectF(0, 0, 3458, 2438);
    psRA1: FMediaBox := RectF(0, 0, 2438, 1729);
    psRA2: FMediaBox := RectF(0, 0, 1729, 1219);
    psLetter: FMediaBox := RectF(0, 0, 612, 792);
    psLegal: FMediaBox := RectF(0, 0, 612, 1008);
    psLedger: FMediaBox := RectF(0, 0, 792, 1224);
    psTabloid: FMediaBox := RectF(0, 0, 1224, 792);
    psExecutive: FMediaBox := RectF(0, 0, 522, 756);
    psANSIC: FMediaBox := RectF(0, 0, 1584, 1224);
    psANSID: FMediaBox := RectF(0, 0, 2448, 1584);
    psANSIE: FMediaBox := RectF(0, 0, 3168, 2448);
    psFoolscap: FMediaBox := RectF(0, 0, 954, 1188);
    psSmallPost: FMediaBox := RectF(0, 0, 1044, 1332);
    psSheetAnd13Cap: FMediaBox := RectF(0, 0, 954, 1584);
    psSheetAnd12Cap: FMediaBox := RectF(0, 0, 954, 1782);
    psDemy: FMediaBox := RectF(0, 0, 1116, 1440);
    psLargePost: FMediaBox := RectF(0, 0, 1188, 1512);
    psSmallmedium: FMediaBox := RectF(0, 0, 1260, 1584);
    psMedium: FMediaBox := RectF(0, 0, 1296, 1656);
    psSmallRoyal: FMediaBox := RectF(0, 0, 1368, 1728);
    psRoyal: FMediaBox := RectF(0, 0, 1440, 1800);
    psImperial: FMediaBox := RectF(0, 0, 1584, 2160);
    psMetricCrownQuarto: FMediaBox := RectF(0, 0, 536, 697);
    psMetricCrownOctavo: FMediaBox := RectF(0, 0, 349, 527);
    psMetricLargeCrownQuarto: FMediaBox := RectF(0, 0, 570, 731);
    psMetricLargeCrownOctavo: FMediaBox := RectF(0, 0, 366, 561);
    psMetricDemyQuarto: FMediaBox := RectF(0, 0, 621, 782);
    psMetricDemyOctavo: FMediaBox := RectF(0, 0, 391, 612);
    psMetricRoyalQuarto: FMediaBox := RectF(0, 0, 672, 884);
    psMetricRoyalOctavo: FMediaBox := RectF(0, 0, 366, 561);
    psCustom: FMediaBox := RectF(0, 0, PageWidth, PageHeight);
  end;

  if FPageOrientation = poLandscape then
  begin
    w := FMediaBox.Width;
    h := FMediaBox.Height;
    FMediaBox.Height := w;
    FMediaBox.Width := h;
  end;

  FCropBox := FMediaBox;
  FBleedBox := FMediaBox;
  FArtBox := FMediaBox;
  FTrimBox := FMediaBox;

  g := FPDFInitializationLib;
  if Assigned(g) then
  begin
    g.SetPageWidth(MediaBox.Width);
    g.SetPageHeight(MediaBox.Height);
  end;
end;

procedure TAdvGeneralPDFLib.UpdateFontList(ASearchForUnicodeFont: Boolean = False);
var
  g: IAdvCustomPDFGraphicsLib;
  r: TAdvGeneralPDFLibXRefObject;
  ft: TAdvGeneralPDFLibFont;
  ftd: TAdvGeneralPDFLibFontDescriptor;
  ftff: TAdvGeneralPDFLibFontFile;
  ftu: TAdvGeneralPDFLibFontUnicode;
  ftds: TAdvGeneralPDFLibFontDescendant;
  fontn: string;
  tm: TAdvGeneralPDFLibFontMetrics;
begin
  if FBlockFontUpdate then
    Exit;

  g := FPDFGraphicsLib;
  if Assigned(g) and Assigned(FOutput) then
  begin
    fontn := VerifyFontName(g.Font.Name);
    FBlockFontUpdate := True;
    g.Font.Name := fontn;
    FBlockFontUpdate := False;

    if (FOutput.FontBase <> fontn) or (FOutput.FontStyle <> g.Font.Style) or ASearchForUnicodeFont or (FOutput.FontUnicode and not ASearchForUnicodeFont) then
    begin
      FOutput.FontBase := StringReplace(fontn, ' ', '', [rfReplaceAll]);
      FOutput.FontName := FOutput.FontBase;
      if (TFontStyle.fsBold in g.Font.Style) and (TFontStyle.fsItalic in g.Font.Style) then
        FOutput.FontName := FOutput.FontName +',BoldItalic'
      else if TFontStyle.fsBold in g.Font.Style then
        FOutput.FontName := FOutput.FontName +',Bold'
      else if TFontStyle.fsItalic in g.Font.Style then
        FOutput.FontName := FOutput.FontName +',Italic';

      ft := SearchForFont(FOutput.FontName, ASearchForUnicodeFont);

      if not Assigned(ft) then
      begin
        ft := TAdvGeneralPDFLibFont.Create(Self, FInitializer, fontn, FOutput.FontName, g.Font.Size, g.Font.Style);
        FXRefObjects.AddObject(ft);
        FFontList.Add(ft);
        tm := ft.Initializer.GetFontMetrics;
        ft.Ascent := tm.Ascent;
        ft.CapHeight := tm.CapHeight;
        ft.ItalicAngle := tm.ItalicAngle;
        ft.Descent := tm.Descent;
        ft.Box := tm.FontBox;
        ft.TrueType := tm.TrueType;
        ft.Initializer.IsFixedWidth := tm.Fixed;
        ft.Initializer.InitializeCharWidths;
        ft.UnitsPerEm := ft.Initializer.GetUnitsPerEm;
        ft.RefNameOriginal := Format(PDFPageFontTextRef, [ft.RefNameNotEscaped]);
      end;

      if not Assigned(ft.FontDescriptor) then
      begin
        ftd := TAdvGeneralPDFLibFontDescriptor.Create(Self, ft);
        r := FXRefObjects.AddObject(ftd);
        r.XRefType := 'n';
        ft.FontDescriptor := ftd;
      end;

      if not Assigned(ft.FontUnicode) and ft.Unicode then
      begin        
        ftu := TAdvGeneralPDFLibFontUnicode.Create(Self, ft);
        r := FXRefObjects.AddObject(ftu);
        r.XRefType := 'n';
        ft.FontUnicode := ftu;
      end;

      if not Assigned(ft.FontDescendant) and ft.Unicode then
      begin        
        ftds := TAdvGeneralPDFLibFontDescendant.Create(Self, ft);
        r := FXRefObjects.AddObject(ftds);
        r.XRefType := 'n';
        ft.FontDescendant := ftds;
      end;

      if not Assigned(ft.FontFile) and EmbedFonts and ft.TrueType then
      begin
        ftff := TAdvGeneralPDFLibFontFile.Create(Self, ft);
        r := FXRefObjects.AddObject(ftff);
        r.XRefType := 'n';
        ft.FontFile := ftff;
      end;
    end
    else
      ft := SearchForFont(FOutput.FontName, ASearchForUnicodeFont);

    FActiveFont := ft;

    if Assigned(ft) and Assigned(FActivePage) and (FActivePage.FontList.IndexOf(ft) = -1) then
      FActivePage.FontList.Add(ft);

    if Assigned(ft) then
    begin
      FOutput.FontCapHeight := ft.CapHeight;
      FOutput.FontUnitsPerEm := ft.UnitsPerEm;
    end
    else
    begin
      FOutput.FontCapHeight := 0;
      FOutput.FontUnitsPerEm := 0;
    end;

    FOutput.FontSize := g.Font.Size;
    FOutput.FontColor := g.Font.Color;
    FOutput.FontStyle := g.Font.Style;
    FOutput.FontLeading := g.Font.Size;
    FOutput.FontWordSpacing := 0;
    FOutput.FontCharSpacing := 0;
    FOutput.FontUnicode := Assigned(ft) and ft.Unicode;
    if Assigned(ft) then
    begin
      FOutput.FontRefName := ft.RefNameOriginal;
      FOutput.FontCharWidths := ft.CharWidths;
      FOutput.FontCharArray := ft.CharArray;
      FOutput.FontUsedCharArray := ft.UsedCharArray;
    end;
  end;
end;

function TAdvGeneralPDFLib.VerifyFontName(AFontName: string): string;
var
  idx: Integer;
  fbi: Integer;
begin
  Result := AFontName;
  idx := FOSFontList.IndexOf(Result);
  fbi := 0;
  while (idx = - 1) and (fbi >= 0) and (fbi <= FontFallBackList.Count - 1) do
  begin
    Result := FontFallBackList[fbi];
    idx := FOSFontList.IndexOf(Result);
    if idx = -1 then
      Inc(fbi);
  end;
end;

{ TAdvGeneralPDFLibXRefObject }

constructor TAdvGeneralPDFLibXRefObject.Create(const AValue: TAdvGeneralPDFLibObject = nil);
begin
  if Assigned(AValue) then
  begin
    FXRefType := 'n';
    FXRefNumber := AValue.Number;
    FXRefObject := AValue;
  end
  else
  begin
    FXRefType := 'f';
    FXRefNumber := 0;
  end;
  FXRefOffset := -1;
  FInXRefList := True;
end;

destructor TAdvGeneralPDFLibXRefObject.Destroy;
begin
  if Assigned(FXRefObject) then
  begin
    FXRefObject.Free;
    FXRefObject := nil;
  end;

  inherited;
end;

class function TAdvGeneralPDFLibXRefObject.FormatValue(AValue,
  ALength: Integer): string;
var
  i, K: integer;
  S: string;
begin
  Result := '';
  if AValue > 0 then
    S := IntToStr(AValue)
  else
    S := '0';

  i := ALength - Length(S);

  for K := 0 to i - 1 do
    Result := Result + '0';

  Result := Result + S;
end;

function TAdvGeneralPDFLibXRefObject.GenerateXRefValue: string;
begin
  Result := FormatValue(FXRefOffset, 10) + ' ' + FormatValue(FXRefNumber, 5) + ' ' + FXRefType;
end;

{ TAdvGeneralPDFLibObject }

constructor TAdvGeneralPDFLibObject.Create(const APDFLib: TAdvGeneralPDFLib; const ANumber: Integer = -1; const AXRefNumber: Integer = 0);
begin
  FPDFLib := APDFLib;
  FXRefNumber := AXRefNumber;
  FNumber := ANumber;
  FXRefObject := nil;
end;

destructor TAdvGeneralPDFLibObject.Destroy;
begin
  FXRefObject := nil;
  inherited;
end;

function TAdvGeneralPDFLibObject.GetRefNameBase: String;
begin
  Result := '';
end;

function TAdvGeneralPDFLibObject.GetRef: string;
var
  r: String;
begin
  r := RefName;
  if r <> '' then
    Result := r + ' ' + IntToStr(GetRefIndex(False)) + ' 0 R'
  else
    Result := IntToStr(GetRefIndex(False)) + ' 0 R';
end;

function TAdvGeneralPDFLibObject.GetRefIndex(ACheckClassType: Boolean): Integer;
begin
  Result := -1;
  if Assigned(PDFLib) and Assigned(PDFLib.FXRefObjects) then
    Result := PDFLib.FXRefObjects.IndexOfObject(Self, ACheckClassType);
end;

function TAdvGeneralPDFLibObject.GetRefName: string;
var
  r: String;
begin
  Result := '';
  r := RefNameNotEscaped;
  if r <> '' then
    Result := '/' + r;
end;

function TAdvGeneralPDFLibObject.GetRefNameNotEscaped: string;
var
  r: String;
begin
  Result := '';
  r := RefNameBase;
  if r <> '' then
    Result := r + IntToStr(GetRefIndex(True));
end;

procedure TAdvGeneralPDFLibObject.SetNumber(const Value: integer);
begin
  FNumber := Value;
  if Value < 0 then
    FType := lotIndirect
  else
    FType := lotDirect;
end;

{ TAdvGeneralPDFLibXRefObjects }

function TAdvGeneralPDFLibXRefObjects.AddObject(const AObject: TAdvGeneralPDFLibObject = nil): TAdvGeneralPDFLibXRefObject;
var
  i: Integer;
begin
  Result := TAdvGeneralPDFLibXRefObject.Create(AObject);
  i := Add(Result);
  if Assigned(AObject) then
  begin
    AObject.Number := i;
    AObject.XRefObject := Result;
  end;
end;

function TAdvGeneralPDFLibXRefObjects.AddObjectClass(
  const AObjectClass: TAdvGeneralPDFLibObjectClass): TAdvGeneralPDFLibXRefObject;
var
  i: Integer;
  AObject: TAdvGeneralPDFLibObject;
begin
  AObject := AObjectClass.Create(nil);
  Result := TAdvGeneralPDFLibXRefObject.Create(AObject);
  i := Add(Result);
  if Assigned(AObject) then
  begin
    AObject.Number := i;
    AObject.XRefObject := Result;
  end;
end;

function TAdvGeneralPDFLibXRefObjects.IndexOfObject(
  const AObject: TAdvGeneralPDFLibObject; const ACheckClassType: Boolean = False): Integer;
var
  I, K: Integer;
  x: TAdvGeneralPDFLibObject;
begin
  Result := -1;
  K := -1;
  if Assigned(AObject) then
  begin
    for I := 0 to Count - 1 do
    begin
      x := Items[I].XRefObject;
      if Assigned(x) then
      begin
        if (not ACheckClassType) or (ACheckClassType and (x.ClassType = AObject.ClassType)) then
          Inc(K);

        if Items[I].XRefObject = AObject then
        begin
          Result := K;
          Break;
        end;
      end
      else if not ACheckClassType then
        Inc(K);
    end;
  end;
end;

{ TAdvGeneralPDFLibPage }

constructor TAdvGeneralPDFLibPage.Create(const APDFLib: TAdvGeneralPDFLib; const APageNumber: Integer);
begin
  inherited Create(APDFLib);
  FPageNumber := APageNumber;
  FInsertPageNumber := APageNumber;
  FFontList := TAdvGeneralPDFLibFonts.Create;
  FXObjectList := TAdvGeneralPDFLibXObjects.Create;
  FShadings := TAdvGeneralPDFLibShadings.Create;
  FBitmaps := TAdvGeneralPDFLibBitmaps.Create;
  FAnnotations := TAdvGeneralPDFLibAnnotations.Create;
  FPatterns := TAdvGeneralPDFLibPatterns.Create;
end;

destructor TAdvGeneralPDFLibPage.Destroy;
begin
  FContent := nil;

  if Assigned(FAnnotations) then
  begin
    FAnnotations.Free;
    FAnnotations := nil;
  end;

  if Assigned(FBitmaps) then
  begin
    FBitmaps.Free;
    FBitmaps := nil;
  end;

  if Assigned(FShadings) then
  begin
    FShadings.Free;
    FShadings := nil;
  end;

  if Assigned(FPatterns) then
  begin
    FPatterns.Free;
    FPatterns := nil;
  end;

  if Assigned(FXObjectList) then
  begin
    FXObjectList.Free;
    FXObjectList := nil;
  end;

  if Assigned(FFontList) then
  begin
    FFontList.Free;
    FFontList := nil;
  end;

  inherited;
end;

{ TAdvGeneralPDFLibFont }

constructor TAdvGeneralPDFLibFont.Create(const APDFLib: TAdvGeneralPDFLib; const AMainInitializer: TAdvGeneralPDFLibInitializer; const ABase: String; const AName: String; const ASize: Single; const AStyle: TFontStyles);
begin
  inherited Create(APDFLib);
  FInitializer := TAdvGeneralPDFLibFontInitializer.Create(AMainInitializer, ABase, AStyle, ASize);
  FFontFile := nil;
  FFontDescriptor := nil;
  FFontDescendant := nil;
  FFontUnicode := nil;
  FBase := ABase;
  FSize := ASize;
  FName := AName;
  FStyle := AStyle;
end;

destructor TAdvGeneralPDFLibFont.Destroy;
begin
  if Assigned(FInitializer) then
  begin
    FInitializer.Free;
    FInitializer := nil;
  end;

  FFontFile := nil;
  FFontDescriptor := nil;
  FFontDescendant := nil;
  FFontUnicode := nil;
  inherited;
end;

function TAdvGeneralPDFLibFont.BoxAsString: string;
begin
  Result := IntToStr(Box.Left) + ' ' + IntToStr(Box.Bottom) + ' ' + IntToStr(Box.Right) + ' ' + IntToStr(Box.Top);
end;

function TAdvGeneralPDFLibFont.FirstGlyph: Integer;
begin
  Result := 0;
  if (UsedCharArray.Count > 0) then
    Result := UsedCharArray[0];
end;

function TAdvGeneralPDFLibFont.FirstGlyphAsString: string;
begin
  Result := IntToStr(FirstGlyph);
end;

function TAdvGeneralPDFLibFont.GlyphsAndWidthsAsString: string;
var
  I: Integer;
  v: Integer;
  idx: Integer;
begin
  Result := '';
  for I := 0 to UsedCharArray.Count - 1 do
  begin
    v := UsedCharArray[I];
    idx := CharArray.IndexOf(v);
    if idx > -1 then
      Result := Result + IntToStr(CharWidths[idx].g) + '['+ IntToStr(CharWidths[idx].w)  + '] '
  end;

  Result := Trim(Result);
end;

function TAdvGeneralPDFLibFont.LastGlyph: Integer;
begin
  Result := 0;
  if (UsedCharArray.Count > 0) then
    Result := UsedCharArray[UsedCharArray.Count - 1];
end;

function TAdvGeneralPDFLibFont.LastGlyphAsString: string;
begin
  Result := IntToStr(LastGlyph);
end;

function TAdvGeneralPDFLibFont.WidthsAsString: string;
var
  I: Integer;
  idx, idxc: Integer;
begin
  Result := '';
  for I := FirstGlyph to LastGlyph do
  begin
    idx := UsedCharArray.IndexOf(I);
    idxc := CharArray.IndexOf(I);
    if (idx > -1) and (idxc > -1) then
      Result := Result + IntToStr(CharWidths[idxc].w)  + ' '
    else
      Result := Result + '0 '
  end;

  Result := Trim(Result);
end;

function TAdvGeneralPDFLibFont.GetCharArray: TAdvPDFGraphicsLibFontCharArray;
begin
  Result := Initializer.GetCharArray;
end;

function TAdvGeneralPDFLibFont.GetCharWidths: TAdvPDFGraphicsLibFontCharWidths;
begin
  Result := Initializer.GetCharWidths;
end;

function TAdvGeneralPDFLibFont.GetRefNameBase: string;
begin
  Result := 'FT';
end;

function TAdvGeneralPDFLibFont.GetUsedCharArray: TAdvPDFGraphicsLibUsedFontCharArray;
begin
  Result := Initializer.GetUsedCharArray;
end;

{ TAdvGeneralPDFLibFontDescriptor }

constructor TAdvGeneralPDFLibFontDescriptor.Create(const APDFLib: TAdvGeneralPDFLib; const AFont: TAdvGeneralPDFLibFont);
begin
  inherited Create(APDFLib);
  FFont := AFont;
end;

destructor TAdvGeneralPDFLibFontDescriptor.Destroy;
begin
  FFont := nil;
  inherited;
end;

{ TAdvGeneralPDFLibFontDescendant }

constructor TAdvGeneralPDFLibFontDescendant.Create(const APDFLib: TAdvGeneralPDFLib; const AFont: TAdvGeneralPDFLibFont);
begin
  inherited Create(APDFLib);
  FFont := AFont;
end;

destructor TAdvGeneralPDFLibFontDescendant.Destroy;
begin
  FFont := nil;
  inherited;
end;

{ TAdvGeneralPDFLibFontFile }

constructor TAdvGeneralPDFLibFontFile.Create(const APDFLib: TAdvGeneralPDFLib; const AFont: TAdvGeneralPDFLibFont);
begin
  inherited Create(APDFLib);
  FFont := AFont;
end;

procedure TAdvGeneralPDFLibFontFile.InitializeFontFile;
begin
  if Assigned(FFont) then
    FFont.Initializer.InitializeFontFile;
end;

destructor TAdvGeneralPDFLibFontFile.Destroy;
begin
  FFont := nil;
  inherited;
end;

{ TAdvGeneralPDFLibFontUnicode }

constructor TAdvGeneralPDFLibFontUnicode.Create(const APDFLib: TAdvGeneralPDFLib; const AFont: TAdvGeneralPDFLibFont);
begin
  inherited Create(APDFLib);
  FFont := AFont;
end;

destructor TAdvGeneralPDFLibFontUnicode.Destroy;
begin
  FFont := nil;
  inherited;
end;

{ TAdvGeneralPDFLibShading }

function TAdvGeneralPDFLibShading.RectAsString: string;
begin
  Result := '0 0 0 0';
  if Self is TAdvGeneralPDFLibLinearGradient then
  begin
    case (Self as TAdvGeneralPDFLibLinearGradient).FillOrientation of
      gfoHorizontal: Result := IntToStr(Round(Rect.Left)) + ' ' + IntToStr(Round(Rect.Top)) + ' ' + IntToStr(Round(Rect.Right)) + ' ' + IntToStr(Round(Rect.Bottom - Rect.Height));
      gfoVertical: Result := IntToStr(Round(Rect.Left)) + ' ' + IntToStr(Round(Rect.Bottom)) + ' ' + IntToStr(Round(Rect.Left)) + ' ' + IntToStr(Round(Rect.Bottom - Rect.Height));
    end;
  end;
end;

constructor TAdvGeneralPDFLibShading.Create(const APDFLib: TAdvGeneralPDFLib);
begin
  inherited Create(APDFLib);
end;

destructor TAdvGeneralPDFLibShading.Destroy;
begin
  FFunction := nil;
  inherited;
end;

{ TAdvGeneralPDFLibShadingFunction }

constructor TAdvGeneralPDFLibShadingFunction.Create(const APDFLib: TAdvGeneralPDFLib);
begin
  inherited Create(APDFLib);
  FSubFunctions := TAdvGeneralPDFLibShadingSubFunctions.Create;
end;

destructor TAdvGeneralPDFLibShadingFunction.Destroy;
begin
  if Assigned(FSubFunctions) then
  begin
    FSubFunctions.Free;
    FSubFunctions := nil;
  end;
  inherited;
end;

function TAdvGeneralPDFLibShadingFunction.SubFunctionsRef: String;
var
  x: TAdvGeneralPDFLibShadingSubFunction;
begin
  Result := '';
  if Assigned(FSubFunctions) then
  begin
    for x in FSubFunctions do
      Result := Result + IntToStr(PDFLib.FXRefObjects.IndexOf(x.XRefObject)) + ' 0 R ';

    Result := Trim(Result);
  end;
end;

{ TAdvGeneralPDFLibShadingSubFunction }

constructor TAdvGeneralPDFLibShadingSubFunction.Create(const APDFLib: TAdvGeneralPDFLib; const AFillColor: TAdvGraphicsColor; const AFillColorTo: TAdvGraphicsColor);
begin
  inherited Create(APDFLib);
  FFillColor := AFillColor;
  FFillColorTo := AFillColorTo;
end;

{ TAdvGeneralPDFLibPattern }

destructor TAdvGeneralPDFLibPattern.Destroy;
begin
  FShading := nil;
  inherited;
end;

function TAdvGeneralPDFLibPattern.GetRefNameBase: string;
begin
  Result := 'PT';
end;

{ TAdvGeneralPDFLibBitmap }

constructor TAdvGeneralPDFLibBitmap.Create(const APDFLib: TAdvGeneralPDFLib; const ABitmap: TAdvBitmap; const AStream: TMemoryStream; const AImageType: TAdvPDFGraphicsLibImageType; const AQuality: Single; const ABackgroundColor: TAdvGraphicsColor);
begin
  inherited Create(APDFLib);
  FBitmap := TAdvBitmap.Create;
  FBitmap.Assign(ABitmap);
  FImageType := AImageType;
  FQuality := AQuality;
  FBackgroundColor := ABackgroundColor;
  FStream := TMemoryStream.Create;
  FStream.LoadFromStream(AStream);
end;

destructor TAdvGeneralPDFLibBitmap.Destroy;
begin
  if Assigned(FStream) then
  begin
    FStream.Free;
    FStream := nil;
  end;

  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
  inherited;
end;

function TAdvGeneralPDFLibBitmap.GetRefNameBase: String;
begin
  Result := 'IM';
end;

procedure TAdvGeneralPDFLibBitmap.SetBitmap(const Value: TAdvBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TAdvGeneralPDFLibBitmap.SetStream(const Value: TMemoryStream);
begin
  FStream.LoadFromStream(Value);
end;

{ TAdvGeneralPDFLibContent }

destructor TAdvGeneralPDFLibContent.Destroy;
begin
  FPage := nil;
  inherited;
end;

{ TAdvGeneralPDFLibAnnotation }

function TAdvGeneralPDFLibAnnotation.RectAsString: String;
begin
  Result := IntToStr(Round(Rect.Left)) + ' ' + IntToStr(Round(Rect.Top)) + ' ' + IntToStr(Round(Rect.Right)) + ' ' + IntToStr(Round(Rect.Bottom))
end;

constructor TAdvGeneralPDFLibAnnotation.Create(
  const APDFLib: TAdvGeneralPDFLib; const ARect: TRectF);
begin
  inherited Create(APDFLib);
  FRect := ARect;
end;

destructor TAdvGeneralPDFLibAnnotation.Destroy;
begin
  FAcroFormField := nil;
  inherited;
end;

{ TAdvGeneralPDFLibAcroFormField }

destructor TAdvGeneralPDFLibAcroFormField.Destroy;
begin
  FContent := nil;
  inherited;
end;

{ TAdvGeneralPDFLibURL }

constructor TAdvGeneralPDFLibURL.Create(const APDFLib: TAdvGeneralPDFLib;
  const ARect: TRectF; const AURL: UnicodeString);
begin
  inherited Create(APDFLib, ARect);
  FURL := AURL;
end;

{ TAdvGeneralPDFLibLinearGradient }

constructor TAdvGeneralPDFLibLinearGradient.Create(
  const APDFLib: TAdvGeneralPDFLib;
  const AFillOrientation: TAdvGraphicsFillOrientation);
begin
  inherited Create(APDFLib);
  FFillOrientation := AFillOrientation;
end;

end.

