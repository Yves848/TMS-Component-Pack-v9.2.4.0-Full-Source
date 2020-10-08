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

unit AdvPDFLib;

{$I TMSDEFS.INC}

{$IFDEF FMXLIB}
{$IFNDEF MSWINDOWS}
{$IFNDEF LINUX}
{$DEFINE USENATIVE}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFNDEF LCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 23}
{$DEFINE UNREGISTER}
{$IFEND}
{$HINTS ON}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE UNREGISTER}
{$ENDIF}

interface

uses
  Classes, Types
  {$IFNDEF LCLLIB}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ,AdvPDFGraphicsLib, AdvPDFCoreLibBase, AdvGraphicsTypes,
  AdvPDFRichTextLib, AdvTypes, PictureContainer;

const
  TAdvPDFLibDocURL = 'http://www.tmssoftware.biz/download/manuals/AdvPDFLibraryDevGuide.pdf';
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  //v1.0.0.0: First release
  //v1.0.0.1: Improved: Image drawing in combination with HTML text
  //        : Fixed: Issue with images being displayed while calculating HTML text
  //        : Fixed: Issue applying URLFont in combination with HTML text
  //v1.0.0.2: Fixed: Issue encoding text with accented characters
  //v1.0.0.3: Fixed: Issue with text break on Android
  //v1.0.0.4: Fixed: Issue with text break on Android with single-line text
  //v1.0.1.0: New: Conversion routines Millimeter <-> Pixel and Inc <-> Pixel
  //        : Improved: Center parameter to draw images centered or top-left
  //v1.0.1.1: Fixed: Issue with displaying unicode text in specific circumstances
  //v1.0.1.2: Fixed: Issue with applying bold and italic text in specific circumstances
  //v1.0.1.3: Fixed: Issues searching for correct font in specific circumstances
  //v1.0.1.4: Fixed: Issue with C++Builder XE2 finalization section
  //v1.0.1.5: Fixed: Issue with Unix PDF generation and font handling
  //v1.0.1.6: Fixed: Issue with referencing correct font after destroying unused fonts
  //v1.0.1.7: Fixed: Issue with fonts referencing in multiple pages
  //v1.0.1.8: Fixed: Issue with special characters in filename on macOS
  //v1.0.2.0: New: Added external TTF file support on Android (via Font.FileName)

const
  DefaultMediaBox: TRectF = (Left: 0; Top: 0; Right: 612; Bottom: 792);

resourcestring
  sAdvPDFLibNoPages = 'There are no pages in this document, please add a minimum of one page to create a valid PDF document. Pages can be added by using "%s.NewPage".';

type
  TAdvPDFLibPageSize = (psA0, psA1, psA2, psA3, psA4, psA5, psA6, psA7, psA8,
  psB0, psB1, psB2, psB3, psB4, psB5, psB6, psB7, psB8, psB9, psB10, psC2, psC3, psC4,
  psC5, psC6, psD0,  psSRA0, psSRA1, psSRA2, psSRA3, psSRA4, psRA0, psRA1, psRA2, psLetter,
  psLegal, psLedger, psTabloid, psExecutive, psANSIC, psANSID, psANSIE, psFoolscap,
  psSmallPost, psSheetAnd13Cap, psSheetAnd12Cap, psDemy, psLargePost, psSmallmedium,
  psMedium, psSmallRoyal, psRoyal, psImperial, psMetricCrownQuarto, psMetricCrownOctavo,
  psMetricLargeCrownQuarto, psMetricLargeCrownOctavo, psMetricDemyQuarto, psMetricDemyOctavo,
  psMetricRoyalQuarto, psMetricRoyalOctavo, psCustom);

  TAdvPDFLibStandard = (pdfNone, pdfA1);

  TAdvPDFLibPageOrientation = (poPortrait, poLandscape);
  TAdvPDFLibPageNumber = (pnNone, pnHeader, pnFooter);

  TAdvCustomPDFLib = class;

  TAdvPDFLibBeforeDrawHeaderEvent = procedure(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TAdvPDFLibBeforeDrawFooterEvent = procedure(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TAdvPDFLibBeforeDrawPageNumberEvent = procedure(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TAdvPDFLibAfterDrawHeaderEvent = procedure(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib) of object;
  TAdvPDFLibAfterDrawFooterEvent = procedure(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib) of object;
  TAdvPDFLibAfterDrawPageNumberEvent = procedure(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib) of object;
  TAdvPDFLibNewPageStartedEvent = procedure(Sender: TObject; APageIndex: Integer) of object;

  IAdvCustomPDFLib = interface(IInterface)
  ['{69FD5F00-62C7-48D8-878A-B19F31C9537B}']
    procedure BeginDocument(FileName: String = '');
    procedure OpenDocument(FileName: String); overload;
    procedure OpenDocument({%H-}FileStream: TMemoryStream); overload;
    procedure SaveDocumentFromStream({%H-}FileStream: TMemoryStream; {%H-}FileName: String);
    procedure GetDocumentInfo;
    procedure GetPageInfo({%H-}PageIndex: Integer);
    procedure CloseDocument;
    procedure NewPage;
    procedure InsertPage(PageIndex: Integer);
    procedure DrawPage({%H-}PageIndex: Integer);
    procedure SetPageSize(const Value: TAdvPDFLibPageSize);
    procedure SetPageOrientation(
      const Value: TAdvPDFLibPageOrientation);
    procedure SetHeaderMargins(const Value: TAdvMargins);
    procedure SetFooterMargins(const Value: TAdvMargins);
    procedure SetPageNumberMargins(const Value: TAdvMargins);
    procedure SetArtBox(const Value: TRectF);
    procedure SetBleedBox(const Value: TRectF);
    procedure SetCropBox(const Value: TRectF);
    procedure SetMediaBox(const Value: TRectF);
    procedure SetTrimBox(const Value: TRectF);
    procedure SetPageWidth(const Value: Single);
    procedure SetPageHeight(const Value: Single);
    procedure SetAllowsCopying(const Value: Boolean);
    procedure SetAllowsPrinting(const Value: Boolean);
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
    procedure SetAuthor(const Value: String);
    procedure SetCreator(const Value: String);
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetEmbedFonts(const Value: Boolean);
    procedure SetFooterFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetHeaderFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetOnBeforeDrawHeader(const Value: TAdvPDFLibBeforeDrawHeaderEvent);
    procedure SetOnAfterDrawHeader(const Value: TAdvPDFLibAfterDrawHeaderEvent);
    procedure SetPageNumberFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetOnBeforeDrawPageNumber(const Value: TAdvPDFLibBeforeDrawPageNumberEvent);
    procedure SetOnAfterDrawPageNumber(const Value: TAdvPDFLibAfterDrawPageNumberEvent);
    procedure SetOnBeforeDrawFooter(const Value: TAdvPDFLibBeforeDrawFooterEvent);
    procedure SetOnAfterDrawFooter(const Value: TAdvPDFLibAfterDrawFooterEvent);
    procedure SetOnNewPageStarted(const Value: TAdvPDFLibNewPageStartedEvent);
    procedure SetPDFStandard(const Value: TAdvPDFLibStandard);
    procedure DrawHeader;
    procedure DrawFooter;
    procedure DrawPageNumber;
    procedure SetPDFGraphicsLib(const Value: IAdvCustomPDFGraphicsLib);
    procedure SetPDFGraphicsExLib(const Value: IAdvCustomPDFGraphicsExLib);
    procedure SetPDFInitializationLib(const Value: IAdvCustomPDFInitializationLib);
    function GetPDFGraphicsLib: IAdvCustomPDFGraphicsLib;
    function GetPDFGraphicsExLib: IAdvCustomPDFGraphicsExLib;
    function GetPDFInitializationLib: IAdvCustomPDFInitializationLib;
    function GetPDFStandard: TAdvPDFLibStandard;
    function GetHeaderRect: TRectF;
    function GetPageNumberRect: TRectF;
    function GetFooterRect: TRectF;
    function GetOnBeforeDrawHeader: TAdvPDFLibBeforeDrawHeaderEvent;
    function GetOnAfterDrawHeader: TAdvPDFLibAfterDrawHeaderEvent;
    function GetOnBeforeDrawPageNumber: TAdvPDFLibBeforeDrawPageNumberEvent;
    function GetOnAfterDrawPageNumber: TAdvPDFLibAfterDrawPageNumberEvent;
    function GetOnBeforeDrawFooter: TAdvPDFLibBeforeDrawFooterEvent;
    function GetOnAfterDrawFooter: TAdvPDFLibAfterDrawFooterEvent;
    function GetOnNewPageStarted: TAdvPDFLibNewPageStartedEvent;
    function GetArtBox: TRectF;
    function GetBleedBox: TRectF;
    function GetCreationDate: String;
    function GetCropBox: TRectF;
    function GetMediaBox: TRectF;
    function GetModificationDate: string;
    function GetProducer: String;
    function GetTrimBox: TRectF;
    function UnlockWithPassword({%H-}Password: String): Boolean;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    function IsDocumentOpened: Boolean;
    function EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
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
    function GetEmbedFonts: Boolean;
    function GetFooterFont: TAdvPDFGraphicsLibFont;
    function GetHeaderFont: TAdvPDFGraphicsLibFont;
    function GetPageNumberFont: TAdvPDFGraphicsLibFont;
    function GetPageHeight: Single;
    function GetPageWidth: Single;
    function GetPictureContainer: TPictureContainer;
    property MediaBox: TRectF read GetMediaBox write SetMediaBox;
    property TrimBox: TRectF read GetTrimBox write SetTrimBox;
    property ArtBox: TRectF read GetArtBox write SetArtBox;
    property BleedBox: TRectF read GetBleedBox write SetBleedBox;
    property CropBox: TRectF read GetCropBox write SetCropBox;
    property ModificationDate: string read GetModificationDate;
    property Producer: String read GetProducer;
    property CreationDate: String read GetCreationDate;
    property EmbedFonts: Boolean read GetEmbedFonts write SetEmbedFonts;
    property PageSize: TAdvPDFLibPageSize read GetPageSize write SetPageSize;
    property PageWidth: Single read GetPageWidth write SetPageWidth;
    property PageHeight: Single read GetPageHeight write SetPageHeight;
    property PageOrientation: TAdvPDFLibPageOrientation read GetPageOrientation write SetPageOrientation;
    property Author: String read GetAuthor write SetAuthor;
    property Creator: String read GetCreator write SetCreator;
    property PageNumber: TAdvPDFLibPageNumber read GetPageNumber write SetPageNumber;
    property PageNumberFormat: UnicodeString read GetPageNumberFormat write SetPageNumberFormat;
    property PageNumberSize: Single read GetPageNumberSize write SetPageNumberSize;
    property PageNumberMargins: TAdvMargins read GetPageNumberMargins write SetPageNumberMargins;
    property PageNumberAlignment: TAdvGraphicsTextAlign read GetPageNumberAlignment write SetPageNumberAlignment;
    property PageNumberFont: TAdvPDFGraphicsLibFont read GetPageNumberFont write SetPageNumberFont;
    property Header: UnicodeString read GetHeader write SetHeader;
    property HeaderSize: Single read GetHeaderSize write SetHeaderSize;
    property HeaderMargins: TAdvMargins read GetHeaderMargins write SetHeaderMargins;
    property HeaderAlignment: TAdvGraphicsTextAlign read GetHeaderAlignment write SetHeaderAlignment;
    property HeaderFont: TAdvPDFGraphicsLibFont read GetHeaderFont write SetHeaderFont;
    property Footer: UnicodeString read GetFooter write SetFooter;
    property FooterSize: Single read GetFooterSize write SetFooterSize;
    property FooterMargins: TAdvMargins read GetFooterMargins write SetFooterMargins;
    property FooterAlignment: TAdvGraphicsTextAlign read GetFooterAlignment write SetFooterAlignment;
    property FooterFont: TAdvPDFGraphicsLibFont read GetFooterFont write SetFooterFont;
    property Title: String read GetTitle write SetTitle;
    property OwnerPassword: String read GetOwnerPassword write SetOwnerPassword;
    property UserPassword: String read GetUserPassword write SetUserPassword;
    property AllowsPrinting: Boolean read GetAllowsPrinting write SetAllowsPrinting;
    property AllowsCopying: Boolean read GetAllowsCopying write SetAllowsCopying;
    property Subject: String read GetSubject write SetSubject;
    property Keywords: TStrings read GetKeywords write SetKeywords;
    property FontFallBackList: TStrings read GetFontFallBackList write SetFontFallBackList;
    property OnBeforeDrawHeader: TAdvPDFLibBeforeDrawHeaderEvent read GetOnBeforeDrawHeader write SetOnBeforeDrawHeader;
    property OnAfterDrawHeader: TAdvPDFLibAfterDrawHeaderEvent read GetOnAfterDrawHeader write SetOnAfterDrawHeader;
    property OnBeforeDrawPageNumber: TAdvPDFLibBeforeDrawPageNumberEvent read GetOnBeforeDrawPageNumber write SetOnBeforeDrawPageNumber;
    property OnAfterDrawPageNumber: TAdvPDFLibAfterDrawPageNumberEvent read GetOnAfterDrawPageNumber write SetOnAfterDrawPageNumber;
    property OnBeforeDrawFooter: TAdvPDFLibBeforeDrawFooterEvent read GetOnBeforeDrawFooter write SetOnBeforeDrawFooter;
    property OnAfterDrawFooter: TAdvPDFLibAfterDrawFooterEvent read GetOnAfterDrawFooter write SetOnAfterDrawFooter;
    property OnNewPageStarted: TAdvPDFLibNewPageStartedEvent read GetOnNewPageStarted write SetOnNewPageStarted;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
    property Graphics: IAdvCustomPDFGraphicsLib read GetPDFGraphicsLib write SetPDFGraphicsLib;
    property GraphicsEx: IAdvCustomPDFGraphicsExLib read GetPDFGraphicsExLib write SetPDFGraphicsExLib;
    property &Initialization: IAdvCustomPDFInitializationLib read GetPDFInitializationLib write SetPDFInitializationLib;
    property PDFStandard: TAdvPDFLibStandard read GetPDFStandard write SetPDFStandard;
  end;

  IAdvPDFLibService = interface(IInterface)
  ['{017EC71B-91BA-4A92-B3B2-67724061A21F}']
    function CreatePDFLib: IAdvCustomPDFLib;
  end;

  IAdvPDFLibGeneralService = interface(IInterface)
  ['{7434C5E1-A00F-4592-B34F-46756818C122}']
    function CreatePDFLib: IAdvCustomPDFLib;
  end;

  { TAdvCustomPDFLib }

  TAdvCustomPDFLib = class
  private
    FPDFLib: IAdvCustomPDFLib;
    FPDFGraphicsLib: TAdvPDFGraphicsLib;
    procedure SetPageSize(const Value: TAdvPDFLibPageSize);
    procedure SetPageOrientation(const Value: TAdvPDFLibPageOrientation);
    procedure SetHeaderMargins(const Value: TAdvMargins);
    procedure SetFooterMargins(const Value: TAdvMargins);
    procedure SetArtBox(const Value: TRectF);
    procedure SetBleedBox(const Value: TRectF);
    procedure SetCropBox(const Value: TRectF);
    procedure SetMediaBox(const Value: TRectF);
    procedure SetTrimBox(const Value: TRectF);
    procedure SetAllowsCopying(const Value: Boolean);
    procedure SetAllowsPrinting(const Value: Boolean);
    procedure SetFooter(const Value: UnicodeString);
    procedure SetFooterAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetFooterSize(const Value: Single);
    procedure SetHeader(const Value: UnicodeString);
    procedure SetHeaderAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetHeaderSize(const Value: Single);
    procedure SetKeywords(const Value: TStrings);
    procedure SetOwnerPassword(const Value: String);
    procedure SetSubject(const Value: String);
    procedure SetTitle(const Value: String);
    procedure SetUserPassword(const Value: String);
    procedure SetAuthor(const Value: String);
    procedure SetCreator(const Value: String);
    procedure SetEmbedFonts(const Value: Boolean);
    procedure SetFontFallBackList(const Value: TStrings);
    procedure SetFooterFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetHeaderFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetPageHeight(const Value: Single);
    procedure SetPageWidth(const Value: Single);
    procedure SetOnAfterDrawFooter(const Value: TAdvPDFLibAfterDrawFooterEvent);
    procedure SetOnAfterDrawHeader(const Value: TAdvPDFLibAfterDrawHeaderEvent);
    procedure SetOnAfterDrawPageNumber(const Value: TAdvPDFLibAfterDrawPageNumberEvent);
    procedure SetOnBeforeDrawFooter(const Value: TAdvPDFLibBeforeDrawFooterEvent);
    procedure SetOnBeforeDrawHeader(const Value: TAdvPDFLibBeforeDrawHeaderEvent);
    procedure SetOnBeforeDrawPageNumber(const Value: TAdvPDFLibBeforeDrawPageNumberEvent);
    procedure SetOnNewPageStarted(const Value: TAdvPDFLibNewPageStartedEvent);
    function GetVersion: string;
    function GetVersionNr: Integer;
    function GetArtBox: TRectF;
    function GetBleedBox: TRectF;
    function GetCreationDate: String;
    function GetCropBox: TRectF;
    function GetMediaBox: TRectF;
    function GetModificationDate: string;
    function GetProducer: String;
    function GetTrimBox: TRectF;
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
    function GetKeywords: TStrings;
    function GetOwnerPassword: String;
    function GetPageOrientation: TAdvPDFLibPageOrientation;
    function GetPageSize: TAdvPDFLibPageSize;
    function GetSubject: String;
    function GetTitle: String;
    function GetUserPassword: String;
    function GetEmbedFonts: Boolean;
    function GetFontFallBackList: TStrings;
    function GetFooterFont: TAdvPDFGraphicsLibFont;
    function GetHeaderFont: TAdvPDFGraphicsLibFont;
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
    procedure SetPictureContainer(const Value: TPictureContainer);
    function GetPageNumber: TAdvPDFLibPageNumber;
    function GetPageNumberAlignment: TAdvGraphicsTextAlign;
    function GetPageNumberFont: TAdvPDFGraphicsLibFont;
    function GetPageNumberFormat: UnicodeString;
    function GetPageNumberMargins: TAdvMargins;
    function GetPageNumberSize: Single;
    procedure SetPageNumber(const Value: TAdvPDFLibPageNumber);
    procedure SetPageNumberAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetPageNumberFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetPageNumberFormat(const Value: UnicodeString);
    procedure SetPageNumberMargins(const Value: TAdvMargins);
    procedure SetPageNumberSize(const Value: Single);
    function GetPDFStandard: TAdvPDFLibStandard;
    procedure SetPDFStandard(const Value: TAdvPDFLibStandard);
  protected
    {%H-}constructor Create(AUseNativePDFImplementation: Boolean); overload; virtual;
    procedure DoNotifyNewPage(Sender: TObject);
    procedure SaveDocumentFromStream(FileStream: TMemoryStream; FileName: String);
    procedure GetDocumentInfo;
    procedure InsertPage(PageIndex: Integer);
    procedure OpenDocument(FileName: String); overload;
    procedure OpenDocument(FileStream: TMemoryStream); overload;
    procedure GetPageInfo(PageIndex: Integer);
    procedure CloseDocument;
    procedure DrawPage(PageIndex: Integer);
    procedure DrawHeader;
    procedure DrawFooter;
    {$IFDEF FREEWARE}
    procedure DrawTrial;
    {$ENDIF}
    function RichText: IAdvCustomPDFRichTextLib;
    function UnlockWithPassword(Password: String): Boolean;
    function IsDocumentOpened: Boolean;
    function &Initialization: IAdvCustomPDFInitializationLib;
    function PDFLib: IAdvCustomPDFLib;
    property ModificationDate: string read GetModificationDate;
    property Producer: String read GetProducer;
    property CreationDate: String read GetCreationDate;
    property OwnerPassword: String read GetOwnerPassword write SetOwnerPassword;
    property UserPassword: String read GetUserPassword write SetUserPassword;
    property AllowsPrinting: Boolean read GetAllowsPrinting write SetAllowsPrinting default True;
    property AllowsCopying: Boolean read GetAllowsCopying write SetAllowsCopying default True;
    property PDFStandard: TAdvPDFLibStandard read GetPDFStandard write SetPDFStandard default pdfNone;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure BeginDocument(FileName: String = '');
    procedure NewPage;
    function Graphics: IAdvCustomPDFGraphicsLib;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    function EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
    function GetHeaderRect: TRectF;
    function GetFooterRect: TRectF;
    function GetPageNumberRect: TRectF;
    property MediaBox: TRectF read GetMediaBox write SetMediaBox;
    property TrimBox: TRectF read GetTrimBox write SetTrimBox;
    property ArtBox: TRectF read GetArtBox write SetArtBox;
    property BleedBox: TRectF read GetBleedBox write SetBleedBox;
    property CropBox: TRectF read GetCropBox write SetCropBox;
    property EmbedFonts: Boolean read GetEmbedFonts write SetEmbedFonts;
    property Version: String read GetVersion;
    property PageWidth: Single read GetPageWidth write SetPageWidth;
    property PageHeight: Single read GetPageHeight write SetPageHeight;
    property PageSize: TAdvPDFLibPageSize read GetPageSize write SetPageSize default psLetter;
    property PageOrientation: TAdvPDFLibPageOrientation read GetPageOrientation write SetPageOrientation default poPortrait;
    property Author: String read GetAuthor write SetAuthor;
    property Creator: String read GetCreator write SetCreator;
    property Header: UnicodeString read GetHeader write SetHeader;
    property HeaderSize: Single read GetHeaderSize write SetHeaderSize;
    property HeaderMargins: TAdvMargins read GetHeaderMargins write SetHeaderMargins;
    property HeaderAlignment: TAdvGraphicsTextAlign read GetHeaderAlignment write SetHeaderAlignment default gtaCenter;
    property HeaderFont: TAdvPDFGraphicsLibFont read GetHeaderFont write SetHeaderFont;
    property PageNumber: TAdvPDFLibPageNumber read GetPageNumber write SetPageNumber;
    property PageNumberFormat: UnicodeString read GetPageNumberFormat write SetPageNumberFormat;
    property PageNumberSize: Single read GetPageNumberSize write SetPageNumberSize;
    property PageNumberMargins: TAdvMargins read GetPageNumberMargins write SetPageNumberMargins;
    property PageNumberAlignment: TAdvGraphicsTextAlign read GetPageNumberAlignment write SetPageNumberAlignment default gtaCenter;
    property PageNumberFont: TAdvPDFGraphicsLibFont read GetPageNumberFont write SetPageNumberFont;
    property Footer: UnicodeString read GetFooter write SetFooter;
    property FooterSize: Single read GetFooterSize write SetFooterSize;
    property FooterMargins: TAdvMargins read GetFooterMargins write SetFooterMargins;
    property FooterAlignment: TAdvGraphicsTextAlign read GetFooterAlignment write SetFooterAlignment default gtaCenter;
    property FooterFont: TAdvPDFGraphicsLibFont read GetFooterFont write SetFooterFont;
    property Title: String read GetTitle write SetTitle;
    property Subject: String read GetSubject write SetSubject;
    property Keywords: TStrings read GetKeywords write SetKeywords;
    property FontFallBackList: TStrings read GetFontFallBackList write SetFontFallBackList;
    property OnBeforeDrawHeader: TAdvPDFLibBeforeDrawHeaderEvent read GetOnBeforeDrawHeader write SetOnBeforeDrawHeader;
    property OnAfterDrawHeader: TAdvPDFLibAfterDrawHeaderEvent read GetOnAfterDrawHeader write SetOnAfterDrawHeader;
    property OnBeforeDrawPageNumber: TAdvPDFLibBeforeDrawPageNumberEvent read GetOnBeforeDrawPageNumber write SetOnBeforeDrawPageNumber;
    property OnAfterDrawPageNumber: TAdvPDFLibAfterDrawPageNumberEvent read GetOnAfterDrawPageNumber write SetOnAfterDrawPageNumber;
    property OnBeforeDrawFooter: TAdvPDFLibBeforeDrawFooterEvent read GetOnBeforeDrawFooter write SetOnBeforeDrawFooter;
    property OnAfterDrawFooter: TAdvPDFLibAfterDrawFooterEvent read GetOnAfterDrawFooter write SetOnAfterDrawFooter;
    property OnNewPageStarted: TAdvPDFLibNewPageStartedEvent read GetOnNewPageStarted write SetOnNewPageStarted;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
  end;

  TAdvPDFLibList = TList<IAdvCustomPDFLib>;

  TAdvPDFLibFactoryService = class abstract(TInterfacedObject, IAdvPDFLibService, IAdvPDFLibGeneralService)
  private
    FPDFLibs: TAdvPDFLibList;
  protected
    function DoCreatePDFLib: IAdvCustomPDFLib; virtual; abstract;
    function CreatePDFLib: IAdvCustomPDFLib;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  TAdvPDFLib = class(TAdvCustomPDFLib)
  public
    property Version;
  end;

function InchToPixel(AInch: Single; ADPI: Single = 72.0): Single;
function InchPoint(AX, AY: Single; ADPI: Single = 72.0): TPointF;
function InchRect(AX, AY, AWidth, AHeight: Single; ADPI: Single = 72.0): TRectF;
function MillimeterToPixel(AMillimeter: Single; ADPI: Single = 72.0): Single;
function MillimeterPoint(AX, AY: Single; ADPI: Single = 72.0): TPointF;
function MillimeterRect(AX, AY, AWidth, AHeight: Single; ADPI: Single = 72.0): TRectF;
function PixelToMillimeter(APixel: Single; ADPI: Single = 72.0): Single;
function PixelToInch(APixel: Single; ADPI: Single = 72.0): Single;
function PointToMillimeterPoint(APoint: TPointF; ADPI: Single = 72.0): TPointF;
function PointToInchPoint(APoint: TPointF; ADPI: Single = 72.0): TPointF;
function RectToMillimeterRect(ARect: TRectF; ADPI: Single = 72.0): TRectF;
function RectToInchRect(ARect: TRectF; ADPI: Single = 72.0): TRectF;

implementation

uses
  {%H-}Math,
{$IFDEF MACOS}
{$IFDEF IOS}
  AdvPDFLib.iOS,
{$ELSE}
  AdvPDFLib.Mac,
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  AdvPDFLib.Android,
{$ENDIF}
  AdvPDFLib.General,
  SysUtils
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  ;

function MillimeterToPixel(AMillimeter: Single; ADPI: Single = 72.0): Single;
begin
  Result := (AMillimeter * ADPI) / 25.4;
end;

function PixelToMillimeter(APixel: Single; ADPI: Single = 72.0): Single;
begin
  Result := (APixel * 25.4) / ADPI;
end;

function InchToPixel(AInch: Single; ADPI: Single = 72.0): Single;
begin
  Result := AInch * ADPI;
end;

function PixelToInch(APixel: Single; ADPI: Single = 72.0): Single;
begin
  Result := APixel / ADPI;
end;

function MillimeterPoint(AX, AY: Single; ADPI: Single = 72.0): TPointF;
begin
  Result := PointF(MillimeterToPixel(AX, ADPI), MillimeterToPixel(AY, ADPI));
end;

function MillimeterRect(AX, AY, AWidth, AHeight: Single; ADPI: Single = 72.0): TRectF;
begin
  Result := RectF(MillimeterToPixel(AX, ADPI), MillimeterToPixel(AY, ADPI), MillimeterToPixel(AX + AWidth, ADPI), MillimeterToPixel(AY + AHeight, ADPI));
end;

function InchPoint(AX, AY: Single; ADPI: Single = 72.0): TPointF;
begin
  Result := PointF(InchToPixel(AX, ADPI), InchToPixel(AY, ADPI));
end;

function InchRect(AX, AY, AWidth, AHeight: Single; ADPI: Single = 72.0): TRectF;
begin
  Result := RectF(InchToPixel(AX, ADPI), InchToPixel(AY, ADPI), InchToPixel(AX + AWidth, ADPI), InchToPixel(AY + AHeight, ADPI));
end;

function RectToMillimeterRect(ARect: TRectF; ADPI: Single = 72.0): TRectF;
begin
  Result := RectF(PixelToMillimeter(ARect.Left, ADPI), PixelToMillimeter(ARect.Top, ADPI), PixelToMillimeter(ARect.Width, ADPI), PixelToMillimeter(ARect.Height, ADPI));
end;

function RectToInchRect(ARect: TRectF; ADPI: Single = 72.0): TRectF;
begin
  Result := RectF(PixelToInch(ARect.Left, ADPI), PixelToInch(ARect.Top, ADPI), PixelToInch(ARect.Width, ADPI), PixelToInch(ARect.Height, ADPI));
end;

function PointToMillimeterPoint(APoint: TPointF; ADPI: Single = 72.0): TPointF;
begin
  Result := PointF(PixelToMillimeter(APoint.X, ADPI), PixelToMillimeter(APoint.X, ADPI));
end;

function PointToInchPoint(APoint: TPointF; ADPI: Single = 72.0): TPointF;
begin
  Result := InchPoint(APoint.X, APoint.Y, ADPI);
end;

function Hiword(L: DWORD): integer;
begin
  Result := L shr 16;
end;

function LoWord(L: DWORD): Integer;
begin
  Result := L AND $FFFF;
end;

function MakeWord(b1,b2: integer): integer;
begin
  Result := b1 or b2 shl 8;
end;

function MakeLong(i1,i2: integer): integer;
begin
  Result := i1 or i2 shl 16;
end;

{ TAdvCustomPDFLib }

function TAdvCustomPDFLib.GetAllowsCopying: Boolean;
begin
  Result := False;
  if Assigned(FPDFLib) then
    Result := FPDFLib.AllowsCopying;
end;

function TAdvCustomPDFLib.GetAllowsPrinting: Boolean;
begin
  Result := False;
  if Assigned(FPDFLib) then
    Result := FPDFLib.AllowsPrinting;
end;

function TAdvCustomPDFLib.GetArtBox: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.ArtBox;
end;

function TAdvCustomPDFLib.GetAuthor: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.Author;
end;

function TAdvCustomPDFLib.GetPictureContainer: TPictureContainer;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PictureContainer;
end;

function TAdvCustomPDFLib.GetBleedBox: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.BleedBox;
end;

function TAdvCustomPDFLib.GetCreationDate: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.CreationDate;
end;

function TAdvCustomPDFLib.GetCreator: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.Creator;
end;

function TAdvCustomPDFLib.GetCropBox: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.CropBox;
end;

function TAdvCustomPDFLib.GetPageHeight: Single;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.MediaBox.Height;
end;

function TAdvCustomPDFLib.GetPageWidth: Single;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.MediaBox.Width;
end;

function TAdvCustomPDFLib.GetPDFStandard: TAdvPDFLibStandard;
begin
  Result := pdfNone;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PDFStandard;
end;

procedure TAdvCustomPDFLib.GetDocumentInfo;
begin
  if Assigned(FPDFLib) then
    FPDFLib.GetDocumentInfo;
end;

function TAdvCustomPDFLib.GetEmbedFonts: Boolean;
begin
  Result := True;
  if Assigned(FPDFLib) then
    Result := FPDFLib.EmbedFonts;
end;

function TAdvCustomPDFLib.GetFontFallBackList: TStrings;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FontFallBackList;
end;

function TAdvCustomPDFLib.GetFooter: UnicodeString;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.Footer;
end;

function TAdvCustomPDFLib.GetFooterAlignment: TAdvGraphicsTextAlign;
begin
  Result := gtaCenter;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FooterAlignment;
end;

function TAdvCustomPDFLib.GetFooterFont: TAdvPDFGraphicsLibFont;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FooterFont;
end;

function TAdvCustomPDFLib.GetFooterMargins: TAdvMargins;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FooterMargins;
end;

function TAdvCustomPDFLib.GetFooterRect: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.GetFooterRect;
end;

function TAdvCustomPDFLib.GetFooterSize: Single;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.FooterSize;
end;

function TAdvCustomPDFLib.GetHeader: UnicodeString;
begin
  Result := '';
  if Assigned(FPDFLib) then
    Result := FPDFLib.Header;
end;

function TAdvCustomPDFLib.GetHeaderAlignment: TAdvGraphicsTextAlign;
begin
  Result := gtaCenter;
  if Assigned(FPDFLib) then
    Result := FPDFLib.HeaderAlignment;
end;

function TAdvCustomPDFLib.GetHeaderFont: TAdvPDFGraphicsLibFont;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.HeaderFont;
end;

function TAdvCustomPDFLib.GetHeaderMargins: TAdvMargins;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.HeaderMargins;
end;

function TAdvCustomPDFLib.GetHeaderRect: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.GetHeaderRect;
end;

function TAdvCustomPDFLib.GetHeaderSize: Single;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.HeaderSize;
end;

function TAdvCustomPDFLib.GetPageNumber: TAdvPDFLibPageNumber;
begin
  Result := pnNone;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumber;
end;

function TAdvCustomPDFLib.GetPageNumberFormat: UnicodeString;
begin
  Result := '';
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumberFormat;
end;

function TAdvCustomPDFLib.GetPageNumberAlignment: TAdvGraphicsTextAlign;
begin
  Result := gtaCenter;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumberAlignment;
end;

function TAdvCustomPDFLib.GetPageNumberFont: TAdvPDFGraphicsLibFont;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumberFont;
end;

function TAdvCustomPDFLib.GetPageNumberMargins: TAdvMargins;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumberMargins;
end;

function TAdvCustomPDFLib.GetPageNumberRect: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.GetPageNumberRect;
end;

function TAdvCustomPDFLib.GetPageNumberSize: Single;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageNumberSize;
end;

function TAdvCustomPDFLib.GetKeywords: TStrings;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.Keywords;
end;

function TAdvCustomPDFLib.GetMediaBox: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.MediaBox;
end;

function TAdvCustomPDFLib.GetModificationDate: string;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.ModificationDate;
end;

function TAdvCustomPDFLib.GetOnAfterDrawFooter: TAdvPDFLibAfterDrawFooterEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnAfterDrawFooter;
end;

function TAdvCustomPDFLib.GetOnAfterDrawHeader: TAdvPDFLibAfterDrawHeaderEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnAfterDrawHeader;
end;

function TAdvCustomPDFLib.GetOnAfterDrawPageNumber: TAdvPDFLibAfterDrawPageNumberEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnAfterDrawPageNumber;
end;

function TAdvCustomPDFLib.GetOnBeforeDrawFooter: TAdvPDFLibBeforeDrawFooterEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnBeforeDrawFooter;
end;

function TAdvCustomPDFLib.GetOnBeforeDrawHeader: TAdvPDFLibBeforeDrawHeaderEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnBeforeDrawHeader;
end;

function TAdvCustomPDFLib.GetOnBeforeDrawPageNumber: TAdvPDFLibBeforeDrawPageNumberEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnBeforeDrawPageNumber;
end;

function TAdvCustomPDFLib.GetOnNewPageStarted: TAdvPDFLibNewPageStartedEvent;
begin
  Result := nil;
  if Assigned(FPDFLib) then
    Result := FPDFLib.OnNewPageStarted;
end;

function TAdvCustomPDFLib.GetOwnerPassword: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.OwnerPassword;
end;

function TAdvCustomPDFLib.GetPageCount: Integer;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.GetPageCount;
end;

function TAdvCustomPDFLib.GetPageIndex: Integer;
begin
  Result := 0;
  if Assigned(FPDFLib) then
    Result := FPDFLib.GetPageIndex;
end;

procedure TAdvCustomPDFLib.GetPageInfo(PageIndex: Integer);
begin
  if Assigned(FPDFLib) then
    FPDFLib.GetPageInfo(PageIndex);
end;

function TAdvCustomPDFLib.GetPageOrientation: TAdvPDFLibPageOrientation;
begin
  Result := poPortrait;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageOrientation;
end;

function TAdvCustomPDFLib.GetPageSize: TAdvPDFLibPageSize;
begin
  Result := psA4;
  if Assigned(FPDFLib) then
    Result := FPDFLib.PageSize;
end;

function TAdvCustomPDFLib.GetProducer: String;
begin
  Result := '';
  if Assigned(FPDFLib) then
    Result := FPDFLib.Producer;
end;

function TAdvCustomPDFLib.GetSubject: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.Subject;
end;

function TAdvCustomPDFLib.GetTitle: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.Title;
end;

function TAdvCustomPDFLib.GetTrimBox: TRectF;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.TrimBox;
end;

function TAdvCustomPDFLib.GetUserPassword: String;
begin
  if Assigned(FPDFLib) then
    Result := FPDFLib.UserPassword;
end;

function TAdvCustomPDFLib.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomPDFLib.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvCustomPDFLib.&Initialization: IAdvCustomPDFInitializationLib;
begin
  Result := nil;
  if Assigned(FPDFGraphicsLib) then
    Result := FPDFGraphicsLib.GetPDFInitializationLib;
end;

procedure TAdvCustomPDFLib.BeginDocument(FileName: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.BeginDocument(FileName);
end;

procedure TAdvCustomPDFLib.CloseDocument;
begin
  if Assigned(FPDFLib) then
    FPDFLib.CloseDocument;
end;

constructor TAdvCustomPDFLib.Create;
begin
  Create(True);
end;

constructor TAdvCustomPDFLib.Create(AUseNativePDFImplementation: Boolean);
var
  PDFLibServiceGeneral: IAdvPDFLibGeneralService;
  {$IFDEF USENATIVE}
  PDFLibService: IAdvPDFLibService;
  {$ENDIF}
  g: IAdvCustomPDFInitializationLib;
begin
  {$IFDEF USENATIVE}
  if AUseNativePDFImplementation then
  begin
    if TAdvPDFPlatformServices.Current.SupportsPlatformService(IAdvPDFLibService, IInterface(PDFLibService)) then
      FPDFLib := PDFLibService.CreatePDFLib;
  end
  else
  {$ENDIF}
  begin
    if TAdvPDFPlatformServices.Current.SupportsPlatformService(IAdvPDFLibGeneralService, IInterface(PDFLibServiceGeneral)) then
      FPDFLib := PDFLibServiceGeneral.CreatePDFLib;
  end;

  FPDFGraphicsLib := TAdvPDFGraphicsLib.Create(AUseNativePDFImplementation);
  FPDFLib.SetPDFGraphicsLib(FPDFGraphicsLib.GetPDFGraphicsLib);
  FPDFLib.SetPDFGraphicsExLib(FPDFGraphicsLib.GetPDFGraphicsExLib);
  FPDFLib.SetPDFInitializationLib(FPDFGraphicsLib.GetPDFInitializationLib);
  FPDFLib.&Initialization.SetPDFLib(FPDFLib);
  FPDFLib._Release;

  g := &Initialization;
  if Assigned(g) then
    g.OnNotifyNewPage := DoNotifyNewPage;
end;

function TAdvCustomPDFLib.Graphics: IAdvCustomPDFGraphicsLib;
begin
  Result := nil;
  if Assigned(FPDFGraphicsLib) then
    Result := FPDFGraphicsLib.GetPDFGraphicsLib;
end;

function TAdvCustomPDFLib.IsDocumentOpened: Boolean;
begin
  Result := False;
  if Assigned(FPDFLib) then
    Result := FPDFLib.IsDocumentOpened;
end;

procedure TAdvCustomPDFLib.InsertPage(PageIndex: Integer);
begin
  if Assigned(FPDFLib) then
  begin
    {$IFDEF FREEWARE}
    DrawTrial;
    {$ENDIF}
    FPDFLib.InsertPage(PageIndex);
  end;
end;

procedure TAdvCustomPDFLib.NewPage;
begin
  if Assigned(FPDFLib) then
  begin
    {$IFDEF FREEWARE}
    DrawTrial;
    {$ENDIF}
    FPDFLib.NewPage;
  end;
end;

procedure TAdvCustomPDFLib.OpenDocument(FileName: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OpenDocument(FileName);
end;

procedure TAdvCustomPDFLib.OpenDocument(FileStream: TMemoryStream);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OpenDocument(FileStream);
end;

function TAdvCustomPDFLib.PDFLib: IAdvCustomPDFLib;
begin
  Result := FPDFLib;
end;

function TAdvCustomPDFLib.RichText: IAdvCustomPDFRichTextLib;
begin
  Result := nil;
  if Assigned(FPDFGraphicsLib) then
    Result := FPDFGraphicsLib.GetPDFGraphicsExLib.RichText;
end;

destructor TAdvCustomPDFLib.Destroy;
begin
  if Assigned(FPDFLib) then
    FPDFLib := nil;

  if Assigned(FPDFGraphicsLib) then
  begin
    FPDFGraphicsLib.Free;
    FPDFGraphicsLib := nil;
  end;
  inherited;
end;

procedure TAdvCustomPDFLib.DoNotifyNewPage(Sender: TObject);
begin
  NewPage;
end;

procedure TAdvCustomPDFLib.DrawFooter;
begin
  if Assigned(FPDFLib) then
    FPDFLib.DrawFooter;
end;

procedure TAdvCustomPDFLib.DrawHeader;
begin
  if Assigned(FPDFLib) then
    FPDFLib.DrawHeader;
end;

procedure TAdvCustomPDFLib.DrawPage(PageIndex: Integer);
begin
  if Assigned(FPDFLib) then
    FPDFLib.DrawPage(PageIndex);
end;

{$IFDEF FREEWARE}
procedure TAdvCustomPDFLib.DrawTrial;
var
  f: TAdvPDFGraphicsLibFont;
begin
  if GetPageCount > 0 then
  begin
    f := TAdvPDFGraphicsLibFont.Create;
    try
      f.Assign(Graphics.Font);
      Graphics.Font.BeginUpdate;
      Graphics.Font.Name := DefaultFontName;
      Graphics.Font.Size := 8;
      Graphics.Font.Style := [];
      Graphics.Font.Color := gcRed;
      Graphics.Font.EndUpdate;
      Graphics.DrawText(ClassName + ' TRIAL VERSION ' + Version, PointF(0, 0));
    finally
      f.Free;
    end;
  end;
end;
{$ENDIF}

function TAdvCustomPDFLib.EndDocument(AOpenInPDFReader: Boolean = False): TMemoryStream;
begin
  Result := nil;
  if Assigned(FPDFLib) then
  begin
    if FPDFLib.GetPageCount = 0 then
      raise Exception.Create(Format(sAdvPDFLibNoPages, [ClassName]));

    {$IFDEF FREEWARE}
    DrawTrial;
    {$ENDIF}
    Result := FPDFLib.EndDocument(AOpenInPDFReader);
  end;
end;

procedure TAdvCustomPDFLib.SaveDocumentFromStream(FileStream: TMemoryStream;
  FileName: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.SaveDocumentFromStream(FileStream, FileName);
end;

procedure TAdvCustomPDFLib.SetAllowsCopying(const Value: Boolean);
begin
  if Assigned(FPDFLib) then
    FPDFLib.AllowsCopying := Value;
end;

procedure TAdvCustomPDFLib.SetAllowsPrinting(const Value: Boolean);
begin
  if Assigned(FPDFLib) then
    FPDFLib.AllowsPrinting := Value;
end;

procedure TAdvCustomPDFLib.SetArtBox(const Value: TRectF);
begin
  if Assigned(FPDFLib) then
    FPDFLib.ArtBox := Value;
end;

procedure TAdvCustomPDFLib.SetAuthor(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Author := Value;
end;

procedure TAdvCustomPDFLib.SetPictureContainer(
  const Value: TPictureContainer);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PictureContainer := Value;
end;

procedure TAdvCustomPDFLib.SetBleedBox(const Value: TRectF);
begin
  if Assigned(FPDFLib) then
    FPDFLib.BleedBox := Value;
end;

procedure TAdvCustomPDFLib.SetCreator(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Creator := Value;
end;

procedure TAdvCustomPDFLib.SetCropBox(const Value: TRectF);
begin
  if Assigned(FPDFLib) then
    FPDFLib.CropBox := Value;
end;

procedure TAdvCustomPDFLib.SetPageHeight(const Value: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageHeight := Value;
end;

procedure TAdvCustomPDFLib.SetPageNumber(const Value: TAdvPDFLibPageNumber);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumber := Value;
end;

procedure TAdvCustomPDFLib.SetPageNumberFormat(const Value: UnicodeString);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumberFormat := Value;
end;

procedure TAdvCustomPDFLib.SetPageNumberAlignment(const Value: TAdvGraphicsTextAlign);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumberAlignment := Value;
end;

procedure TAdvCustomPDFLib.SetPageNumberFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumberFont.Assign(Value);
end;

procedure TAdvCustomPDFLib.SetPageNumberMargins(const Value: TAdvMargins);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumberMargins.Assign(Value);
end;

procedure TAdvCustomPDFLib.SetPageNumberSize(const Value: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageNumberSize := Value;
end;

procedure TAdvCustomPDFLib.SetPageWidth(const Value: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageWidth := Value;
end;

procedure TAdvCustomPDFLib.SetPDFStandard(const Value: TAdvPDFLibStandard);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PDFStandard := Value;
end;

procedure TAdvCustomPDFLib.SetEmbedFonts(const Value: Boolean);
begin
  if Assigned(FPDFLib) then
    FPDFLib.EmbedFonts := Value;
end;

procedure TAdvCustomPDFLib.SetFontFallBackList(const Value: TStrings);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FontFallBackList.Assign(Value);
end;

procedure TAdvCustomPDFLib.SetFooter(const Value: UnicodeString);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Footer := Value;
end;

procedure TAdvCustomPDFLib.SetFooterAlignment(const Value: TAdvGraphicsTextAlign);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FooterAlignment := Value;
end;

procedure TAdvCustomPDFLib.SetFooterFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FooterFont.Assign(Value);
end;

procedure TAdvCustomPDFLib.SetFooterMargins(const Value: TAdvMargins);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FooterMargins.Assign(Value);
end;

procedure TAdvCustomPDFLib.SetFooterSize(const Value: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.FooterSize := Value;
end;

procedure TAdvCustomPDFLib.SetHeader(const Value: UnicodeString);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Header := Value;
end;

procedure TAdvCustomPDFLib.SetHeaderAlignment(const Value: TAdvGraphicsTextAlign);
begin
  if Assigned(FPDFLib) then
    FPDFLib.HeaderAlignment := Value;
end;

procedure TAdvCustomPDFLib.SetHeaderFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  if Assigned(FPDFLib) then
    FPDFLib.HeaderFont.Assign(Value);
end;

procedure TAdvCustomPDFLib.SetHeaderMargins(const Value: TAdvMargins);
begin
  if Assigned(FPDFLib) then
    FPDFLib.HeaderMargins.Assign(Value);
end;

procedure TAdvCustomPDFLib.SetHeaderSize(const Value: Single);
begin
  if Assigned(FPDFLib) then
    FPDFLib.HeaderSize := Value;
end;

procedure TAdvCustomPDFLib.SetKeywords(const Value: TStrings);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Keywords.Assign(Value);
end;

procedure TAdvCustomPDFLib.SetMediaBox(const Value: TRectF);
var
  g: IAdvCustomPDFInitializationLib;
begin
  if Assigned(FPDFLib) then
    FPDFLib.MediaBox := Value;

  g := &Initialization;
  if Assigned(g) then
  begin
    g.SetPageWidth(MediaBox.Width);
    g.SetPageHeight(MediaBox.Height);
  end;
end;

procedure TAdvCustomPDFLib.SetOnAfterDrawFooter(
  const Value: TAdvPDFLibAfterDrawFooterEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnAfterDrawFooter := Value;
end;

procedure TAdvCustomPDFLib.SetOnAfterDrawHeader(
  const Value: TAdvPDFLibAfterDrawHeaderEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnAfterDrawHeader := Value;
end;

procedure TAdvCustomPDFLib.SetOnAfterDrawPageNumber(
  const Value: TAdvPDFLibAfterDrawPageNumberEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnAfterDrawPageNumber := Value;
end;

procedure TAdvCustomPDFLib.SetOnBeforeDrawFooter(
  const Value: TAdvPDFLibBeforeDrawFooterEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnBeforeDrawFooter := Value;
end;

procedure TAdvCustomPDFLib.SetOnBeforeDrawHeader(
  const Value: TAdvPDFLibBeforeDrawHeaderEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnBeforeDrawHeader := Value;
end;

procedure TAdvCustomPDFLib.SetOnBeforeDrawPageNumber(
  const Value: TAdvPDFLibBeforeDrawPageNumberEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnBeforeDrawPageNumber := Value;
end;

procedure TAdvCustomPDFLib.SetOnNewPageStarted(
  const Value: TAdvPDFLibNewPageStartedEvent);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OnNewPageStarted := Value;
end;

procedure TAdvCustomPDFLib.SetOwnerPassword(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.OwnerPassword := Value;
end;

procedure TAdvCustomPDFLib.SetPageOrientation(
  const Value: TAdvPDFLibPageOrientation);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageOrientation := Value;
end;

procedure TAdvCustomPDFLib.SetPageSize(const Value: TAdvPDFLibPageSize);
begin
  if Assigned(FPDFLib) then
    FPDFLib.PageSize := Value;
end;

procedure TAdvCustomPDFLib.SetSubject(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Subject := Value;
end;

procedure TAdvCustomPDFLib.SetTitle(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.Title := Value;
end;

procedure TAdvCustomPDFLib.SetTrimBox(const Value: TRectF);
begin
  if Assigned(FPDFLib) then
    FPDFLib.TrimBox := Value;
end;

procedure TAdvCustomPDFLib.SetUserPassword(const Value: String);
begin
  if Assigned(FPDFLib) then
    FPDFLib.UserPassword := Value;
end;

function TAdvCustomPDFLib.UnlockWithPassword(Password: String): Boolean;
begin
  Result := FPDFLib.UnlockWithPassword(Password);
end;

{ TAdvPDFLibFactoryService }

constructor TAdvPDFLibFactoryService.Create;
begin
  inherited Create;
  FPDFLibs := TAdvPDFLibList.Create;
end;

function TAdvPDFLibFactoryService.CreatePDFLib: IAdvCustomPDFLib;
begin
  Result := DoCreatePDFLib;
  FPDFLibs.Add(Result);
end;

destructor TAdvPDFLibFactoryService.Destroy;
begin
  FreeAndNil(FPDFLibs);
  inherited Destroy;
end;

initialization
begin
  {$IFDEF USENATIVE}
  RegisterPDFLibService;
  {$ENDIF}
  RegisterPDFLibGeneralService;
end;

finalization
begin
  {$IFDEF UNREGISTER}
  UnRegisterPDFLibGeneralService;
  {$IFDEF USENATIVE}
  UnRegisterPDFLibService;
  {$ENDIF}
  {$ENDIF}
end;

end.
