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

unit AdvPDFCoreLibBase;

{$I TMSDEFS.INC}

interface

uses
  Classes, Types, AdvGraphics, PictureContainer, AdvGraphicsTypes, AdvTypes, Graphics
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
  ,MacApi.ObjectiveC, MacApi.CoreFoundation
  {$IFDEF IOS}
  ,iOSApi.UIKit, iOSApi.CoreGraphics, iOSApi.CocoaTypes, iOSApi.Foundation
  ,iOSApi.CoreText
  {$ELSE}
  ,MacApi.CocoaTypes, MacApi.AppKit, MacApi.Foundation, MacApi.QuartzCore,
  MacApi.CoreGraphics, MacApi.CoreText
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  ,AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNIBridge, AndroidApi.JNI.JavaTypes, AndroidApi.Helpers
  {$ENDIF}
  ;

const
  PDFCR = #13;
  PDFLF = #10;
  PDFLB = PDFCR + PDFLF;
  PDFEC: array[0..7] of Char = (#12, #08, #10, #13, #09, '(', ')', '\');
  PDFRC: array[0..7] of Char = ('f', 'b', 'n', 'r', 't', '(', ')', '\');
  PDFLHFACTOR = 1.4;
  PDFULFACTOR = 0.8;
  PDFSTFACTOR = 0.5;
  PDFULLWFACTOR = 0.05;
  {$IFDEF ANDROID}
  DefaultFontName = 'Roboto';
  {$ENDIF}
  {$IFDEF MACOS}
  DefaultFontName = 'Arial';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  DefaultFontName = 'Arial';
  {$ENDIF}
  {$IFDEF UNIX}
  {$IFDEF LINUX}
  DefaultFontName = 'DejaVu Sans';
  {$ENDIF}
  {$IFDEF DARWIN}
  DefaultFontName = 'Arial';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  DefaultFontName = 'DejaVu Sans';
  {$ENDIF}
  {$ENDIF}

  PDFMetaData =
      '<?xpacket begin="'#$EF#$BB#$BF'" id="W5M0MpCehiHzreSzNTczkc9d"?>'+
      '<x:xmpmeta xmlns:x="adobe:ns:meta/" x:xmptk="%s">'+
      '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">'+
      '<rdf:Description rdf:about="" xmlns:xmp="http://ns.adobe.com/xap/1.0/">'+
      '<xmp:CreateDate>%s</xmp:CreateDate>'+
      '<xmp:ModifyDate>%s</xmp:ModifyDate>'+
      '<xmp:CreatorTool>%s</xmp:CreatorTool></rdf:Description>'+
      '<rdf:Description rdf:about="" xmlns:dc="http://purl.org/dc/elements/1.1/">'+
      '<dc:title><rdf:Alt><rdf:li xml:lang="x-default">%s</rdf:li></rdf:Alt></dc:title>'+
      '<dc:creator><rdf:Seq><rdf:li xml:lang="x-default">%s</rdf:li></rdf:Seq></dc:creator>'+
      '<dc:description><rdf:Alt><rdf:li xml:lang="x-default">%s</rdf:li></rdf:Alt></dc:description>'+
      '</rdf:Description><rdf:Description rdf:about="" xmlns:pdf="http://ns.adobe.com/pdf/1.3/">'+
      '<pdf:Keywords>%s</pdf:Keywords>'+
      '<pdf:Producer>%s</pdf:Producer></rdf:Description>'+
      '<rdf:Description rdf:about="" xmlns:pdfaid="http://www.aiim.org/pdfa/ns/id/">'+
      '<pdfaid:part>1</pdfaid:part><pdfaid:conformance>A</pdfaid:conformance>'+
      '</rdf:Description></rdf:RDF></x:xmpmeta><?xpacket end="w"?>'
      ;

type
  TAdvPDFGraphicsLibPathDrawingMode = (dmPathFill, dmPathEOFill, dmPathStroke, dmPathEOStroke, dmPathFillStroke, dmPathEOFillStroke);
  TAdvPDFGraphicsLibImageType = (itOriginal, itPNG, itJPG);
  TAdvPDFGraphicsLibLineBreakMode = (bmLineBreakModeWordWrap, bmLineBreakModeCharacterWrap, bmLineBreakModeClip
    , bmLineBreakModeHeadTruncation, bmLineBreakModeMiddleTruncation, bmLineBreakModeTailTruncation);

  TAdvPDFGraphicsLibPointArray = array of TPointF;
  TAdvPDFGraphicsLibRectArray = array of TRectF;

  TAdvPDFGraphicsLibTextRange = record
    location: Integer;
    length: Integer;
  end;

  TAdvPDFGraphicsLibFont = class(TPersistent)
  private
    FUpdateCount: Integer;
    FName: String;
    FSize: Single;
    FColor: TAdvGraphicsColor;
    FOnChanged: TNotifyEvent;
    FStyle: TFontStyles;
    FFileName: String;
    procedure SetName(const Value: String);
    procedure SetSize(const Value: Single);
    procedure SetColor(const Value: TAdvGraphicsColor);
    function IsSizeStored: Boolean;
    procedure SetStyle(const Value: TFontStyles);
    procedure SetSizeNoScale(const Value: Single);
    procedure SetFileName(const Value: String);
  protected
    procedure DoChanged; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  published
    property Name: String read FName write SetName;
    property FileName: String read FFileName write SetFileName;
    property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
    property SizeNoScale: Single read FSize write SetSizeNoScale stored IsSizeStored nodefault;
    property Color: TAdvGraphicsColor read FColor write SetColor default gcBlack;
    property Style: TFontStyles read FStyle write SetStyle default [];
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TAdvPDFGraphicsFill = class(TAdvCustomGraphicsFill)
  published
    property Kind;
    property Orientation;
    property Color;
    property ColorTo;
  end;

  TAdvPDFGraphicsStroke = class(TAdvCustomGraphicsStroke)
  published
    property Kind;
    property Color;
    property Width;
  end;

  TAdvPDFGraphicsLibBase = class(TInterfacedObject)
  private
    FFont: TAdvPDFGraphicsLibFont;
    FLineBreakMode: TAdvPDFGraphicsLibLineBreakMode;
    FAlignment: TAdvGraphicsTextAlign;
    FURLFont: TAdvPDFGraphicsLibFont;
    FFill: TAdvPDFGraphicsFill;
    FStroke: TAdvPDFGraphicsStroke;
    FPictureContainer: TPictureContainer;
    procedure SetAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetLineBreakMode(const Value: TAdvPDFGraphicsLibLineBreakMode);
    procedure SetURLFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetFill(const Value: TAdvPDFGraphicsFill);
    procedure SetStroke(const Value: TAdvPDFGraphicsStroke);
    procedure SetPictureContainer(const Value: TPictureContainer);
  protected
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
    procedure InitializeAppearance; virtual;
    procedure UpdateFont; virtual;
    procedure UpdateAlignment; virtual;
    procedure UpdateFill; virtual;
    procedure UpdateLineBreakMode; virtual;
    procedure UpdateStroke; virtual;
    procedure CreateClasses; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Fill: TAdvPDFGraphicsFill read FFill write SetFill;
    property Stroke: TAdvPDFGraphicsStroke read FStroke write SetStroke;
    property Font: TAdvPDFGraphicsLibFont read FFont write SetFont;
    property URLFont: TAdvPDFGraphicsLibFont read FURLFont write SetURLFont;
    property Alignment: TAdvGraphicsTextAlign read FAlignment write SetAlignment default gtaLeading;
    property LineBreakMode: TAdvPDFGraphicsLibLineBreakMode read FLineBreakMode write SetLineBreakMode default bmLineBreakModeWordWrap;
    property PictureContainer: TPictureContainer read FPictureContainer write SetPictureContainer;
  end;

  TAdvPlatformServicesList = TDictionary<string, IInterface>;
  TAdvPlatformServicesGlobalFlags = TDictionary<string, Boolean>;

  TAdvPDFPlatformServices = class
  private
    FServicesList: TAdvPlatformServicesList;
    FGlobalFlags: TAdvPlatformServicesGlobalFlags;
    class var FCurrent: TAdvPDFPlatformServices;
    class var FCurrentReleased: Boolean;
{$IFNDEF AUTOREFCOUNT}
    class procedure ReleaseCurrent;
{$ENDIF}
    class function GetCurrent: TAdvPDFPlatformServices; static;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPlatformService(const AServiceGUID: TGUID; const AService: IInterface);
    procedure RemovePlatformService(const AServiceGUID: TGUID);
    function GetPlatformService(const AServiceGUID: TGUID): IInterface;
    function SupportsPlatformService(const AServiceGUID: TGUID): Boolean; overload;
    function SupportsPlatformService(const AServiceGUID: TGUID; out AService): Boolean; overload;
    property GlobalFlags: TAdvPlatformServicesGlobalFlags read FGlobalFlags;
    class property Current: TAdvPDFPlatformServices read GetCurrent;
  end;

  TAdvPDFGraphicsLibUsedFontCharArray = TList<Integer>;

  TAdvPDFGraphicsLibFontCharArray = record
  public
    v: TWordDynArray;
    c: Integer;
    function Add(AValue: Word): Integer;
    function IndexOf(AValue: Word): Integer;
    function FirstValue: Integer;
    function LastValue: Integer;
  end;

  TAdvPDFGraphicsLibFontCharWidths = array of packed record
      case Byte of
      0: (
        w: word;
        g: word;
      );
      1: (
        i: Integer;
      );
    end;

  TAdvPDFGraphicsLibOutputWriterNotifyTextEvent = procedure(Sender: TObject; AValue: UnicodeString) of object;
  TAdvPDFGraphicsLibOutputWriterNotifyURLEvent = procedure(Sender: TObject; ARect: TRectF; AURL: UnicodeString) of object;
  TAdvPDFGraphicsLibOutputWriterNotifyUnicodeEvent = procedure(Sender: TObject; AValue: UnicodeString) of object;
  TAdvPDFGraphicsLibOutputWriterNotifyBitmapEvent = procedure(Sender: TObject; AValue: TAdvBitmap; AImageType: TAdvPDFGraphicsLibImageType; AQuality: Single; ABackgroundColor: TAdvGraphicsColor; var ABitmapReference: string) of object;
  TAdvPDFGraphicsLibOutputWriterNotifyShadingEvent = procedure(Sender: TObject; AFillColor: TAdvGraphicsColor; AFillColorTo: TAdvGraphicsColor; AFillOrientation: TAdvGraphicsFillOrientation; var AShadingReference: String) of object;
  TAdvPDFGraphicsLibOutputWriterNotifyShadingRectEvent = procedure(Sender: TObject; ARect: TRectF) of object;

  TAdvPDFGraphicsLibOutputWriterStream = class(TStringStream)
  private
    FReference: TObject;
  public
    destructor Destroy; override;
    property Reference: TObject read FReference write FReference;
  end;

  TAdvPDFGraphicsLibOutputWriterStreams = TObjectList<TStream>;

  TAdvPDFGraphicsLibOutputWriter = class
  private
    {$IFNDEF LCLLIB}
    FCompareValues: IComparer<Integer>;
    {$ENDIF}
    FStream, FContentStream: TAdvPDFGraphicsLibOutputWriterStream;
    FStreams: TAdvPDFGraphicsLibOutputWriterStreams;
    FFontRefName: String;
    FFontSize: Single;
    FFontName: String;
    FOnFontChanged: TNotifyEvent;
    FFontColor: TAdvGraphicsColor;
    FFontStyle: TFontStyles;
    FFontBase: String;
    FFontWordSpacing: Integer;
    FFontLeading: Single;
    FFontCharWidths: TAdvPDFGraphicsLibFontCharWidths;
    FFontCharSpacing: Integer;
    FOnNotifyUnicode: TAdvPDFGraphicsLibOutputWriterNotifyUnicodeEvent;
    FFontCharArray: TAdvPDFGraphicsLibFontCharArray;
    FFontUsedCharArray: TAdvPDFGraphicsLibUsedFontCharArray;
    FOnNotifyBitmap: TAdvPDFGraphicsLibOutputWriterNotifyBitmapEvent;
    FOnNotifyShading: TAdvPDFGraphicsLibOutputWriterNotifyShadingEvent;
    FOnNotifyShadingRect: TAdvPDFGraphicsLibOutputWriterNotifyShadingRectEvent;
    FOnNotifyText: TAdvPDFGraphicsLibOutputWriterNotifyTextEvent;
    FOnNotifyURL: TAdvPDFGraphicsLibOutputWriterNotifyURLEvent;
    FFontUnicode: Boolean;
    FFontCapHeight: Integer;
    FFontUnitsPerEm: Integer;
    function GetStreamPosition: Int64;
    procedure SetStreamPosition(const Value: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    function Streams: TAdvPDFGraphicsLibOutputWriterStreams;
    function GetFontCharWidth(AText: UnicodeString; APos: Integer): Integer;
    function ConvertColorToString(AValue: TAdvGraphicsColor): String;
    function ConvertFloatToString(AValue: Extended): String;
    function IsUnicodeString(AValue: UnicodeString): Boolean;
    function ConvertStringToHex(AValue: UnicodeString): String;
    function AddHex4(AWordValue: Cardinal): String;
    function EscapeString(AValue: UnicodeString): String;
    function FontCharArrayContainsValue(ACharValue: Integer): Boolean;
    function CompressString(AValue: string): TStringStream;
    function CompressStream(AStream: TStringStream): TStringStream;
    function FinishContentStream(AReference: String; AAdditionalFlags: String = ''): TStringStream;
    function TextHeight: Single;
    procedure FontCharArrayAddValue(ACharValue: Integer);
    procedure BeginText;
    procedure WriteMatrix(APoint: TPointF; ASize: TSizeF);
    procedure WriteTextMatrix(AX1, AX2, AX3, AX4, AX5, AX6: Single);
    procedure WriteRectangle(ARect: TRectF);
    procedure WriteFontColor;
    procedure WriteFontLeading;
    procedure WriteFont;
    procedure WriteFillColor(AColor: TAdvGraphicsColor);
    procedure WriteStrokeColor(AColor: TAdvGraphicsColor);
    procedure WriteStrokeKind(AKind: TAdvGraphicsStrokeKind);
    procedure WriteStrokeWidth(AWidth: Single);
    procedure EndText;
    procedure MoveTo(APoint: TPointF);
    procedure LineTo(APoint: TPointF);
    procedure CurveTo(AX1, AX2, AX3, AX4, AX5, AX6: Single);
    procedure CurveTo2(AX1, AX2, AX3, AX4: Single);
    procedure MoveTextTo(APoint: TPointF);
    procedure MoveTextToNextLine;
    procedure AddText(AValue: UnicodeString);
    procedure StartContentStream(AReference: TObject = nil);
    procedure StartNewStream(AReference: TObject = nil);
    procedure WriteString(AValue: string);
    procedure Write(Buffer: Pointer; Count: Integer);
    procedure ReplaceString(ASearchValue, AValue: String);
    procedure ClearProperties;
    procedure NotifyURL(ARect: TRectF; AURL: UnicodeString);
    procedure NotifyShading(AFillColor: TAdvGraphicsColor; AFillColorTo: TAdvGraphicsColor; AFillOrientation: TAdvGraphicsFillOrientation; var AShadingReference: String);
    procedure NotifyShadingRect(ARect: TRectF);
    procedure NotifyUnicode(AValue: UnicodeString);
    procedure NotifyText(AValue: UnicodeString);
    procedure NotifyBitmap(AValue: TAdvBitmap; AImageType: TAdvPDFGraphicsLibImageType; AQuality: Single; ABackgroundColor: TAdvGraphicsColor; var ABitmapReference: string);
    procedure ReplaceStrings(ASearchValues, AValues: array of String);
    property Stream: TAdvPDFGraphicsLibOutputWriterStream read FStream write FStream;
    property ContentStream: TAdvPDFGraphicsLibOutputWriterStream read FContentStream write FContentStream;
    property FontBase: String read FFontBase write FFontBase;
    property FontName: String read FFontName write FFontName;
    property FontRefName: String read FFontRefName write FFontRefName;
    property FontSize: Single read FFontSize write FFontSize;
    property FontCapHeight: Integer read FFontCapHeight write FFontCapHeight;
    property FontUnitsPerEm: Integer read FFontUnitsPerEm write FFontUnitsPerEm;
    property FontColor: TAdvGraphicsColor read FFontColor write FFontColor;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property FontWordSpacing: Integer read FFontWordSpacing write FFontWordSpacing;
    property FontCharSpacing: Integer read FFontCharSpacing write FFontCharSpacing;
    property FontLeading: Single read FFontLeading write FFontLeading;
    property FontCharWidths: TAdvPDFGraphicsLibFontCharWidths read FFontCharWidths write FFontCharWidths;
    property FontCharArray: TAdvPDFGraphicsLibFontCharArray read FFontCharArray write FFontCharArray;
    property FontUsedCharArray: TAdvPDFGraphicsLibUsedFontCharArray read FFontUsedCharArray write FFontUsedCharArray;
    property FontUnicode: Boolean read FFontUnicode write FFontUnicode;
    property OnFontChanged: TNotifyEvent read FOnFontChanged write FOnFontChanged;
    property OnNotifyText: TAdvPDFGraphicsLibOutputWriterNotifyTextEvent read FOnNotifyText write FOnNotifyText;
    property OnNotifyUnicode: TAdvPDFGraphicsLibOutputWriterNotifyUnicodeEvent read FOnNotifyUnicode write FOnNotifyUnicode;
    property OnNotifyURL: TAdvPDFGraphicsLibOutputWriterNotifyURLEvent read FOnNotifyURL write FOnNotifyURL;
    property OnNotifyBitmap: TAdvPDFGraphicsLibOutputWriterNotifyBitmapEvent read FOnNotifyBitmap write FOnNotifyBitmap;
    property OnNotifyShading: TAdvPDFGraphicsLibOutputWriterNotifyShadingEvent read FOnNotifyShading write FOnNotifyShading;
    property OnNotifyShadingRect: TAdvPDFGraphicsLibOutputWriterNotifyShadingRectEvent read FOnNotifyShadingRect write FOnNotifyShadingRect;
    property StreamPosition: Int64 read GetStreamPosition write SetStreamPosition;
  end;

{$IFDEF MACOS}
const
  ImageIOFwk = '/System/Library/Frameworks/ImageIO.framework/ImageIO';
  NSUnderlineByWord = $8000;

{$IFDEF IOS}
const
  NSUnderlineStyleNone = $00;
  NSUnderlineStyleSingle = $01;
  NSUnderlineStyleThick = $02;
  NSUnderlineStyleDouble = $09;
  NSUnderlineShadingSolid = $0000;
  NSUnderlineShadingDot = $0100;
  NSUnderlineShadingDash = $0200;
  NSUnderlineShadingDashDot = $0300;
  NSUnderlineShadingDashDotDot = $0400;
  UIFontDescriptorTraitItalic = 1 shl 0;
  UIFontDescriptorTraitBold = 1 shl 1;
  NSUnderlinePatternSolid = 0;
  NSUnderlinePatternDot = 256;
  NSUnderlinePatternDash = 512;
  NSUnderlinePatternDashDot = 768;
  NSUnderlinePatternDashDotDot = 1024;

type
  PNSDictionary = Pointer;
  NSLineBreakMode = cardinal;
  NSStringDrawingOptions = cardinal;
  NSTextAlignment = cardinal;
  NSWritingDirection = cardinal;
  UIFontDescriptor = interface;
  UIFontDescriptorSymbolicTraits = UInt32;

  NSTextAttachmentClass = interface(NSObjectClass)
    ['{47E479D1-9140-4E7C-8261-177249AE4BDF}']
  end;
  NSTextAttachment = interface(NSObject)
    ['{E7C893FE-D13D-4875-ADAA-0AA79D0125F4}']
    function initWithData(contentData: NSData; uti: NSString): Pointer; cdecl;
    function image: UIImage; cdecl;
    procedure setImage(image: UIImage); cdecl;
    function contents: NSData; cdecl;
    procedure setContents(contents: NSData); cdecl;
    function bounds: CGRect; cdecl;
    procedure setBounds(bounds: CGRect); cdecl;
  end;
  TNSTextAttachment = class(TOCGenericImport<NSTextAttachmentClass, NSTextAttachment>)  end;

  NSStringDrawingContextClass = interface(NSObjectClass)
  ['{A15F7DDC-F8A8-4BF8-A40D-CB2BE4CBE6AD}']
  end;
  NSStringDrawingContext = interface(NSObject)
  ['{E5D6C8AB-2C60-432D-8F78-674A58941B38}']
  end;
  TNSStringDrawingContext = class(TOCGenericImport<NSStringDrawingContextClass, NSStringDrawingContext>)  end;

  NSAttributedStringExClass = interface(NSAttributedStringClass)
  ['{E03D8900-A2B1-4B19-BA3C-4FCD5AC4AD04}']
    function attributedStringWithAttachment(attachment: NSTextAttachment): Pointer; cdecl;
  end;

  NSAttributedStringEx = interface(NSAttributedString)
  ['{552373B5-4E9B-4598-AEEC-F42D1F28C1C3}']
    procedure drawInRect(aRect: CGRect); cdecl;
    function boundingRectWithSize(size: CGSize; options: NSStringDrawingOptions; context: NSStringDrawingContext): CGRect; cdecl;
    function dataFromRange(range: NSRange; documentAttributes: NSDictionary; error: NSError): NSData; cdecl;
    function initWithData(data: NSData; options: NSDictionary; documentAttributes: NSDictionary; error: NSError): Pointer; cdecl;
  end;
  TNSAttributedStringEx = class(TOCGenericImport<NSAttributedStringExClass, NSAttributedStringEx>)  end;

  UIFontDescriptorClass = interface(NSObjectClass)
    ['{CB4BF3A8-9509-47D8-8A44-C96903303564}']
    {class} function fontDescriptorWithFontAttributes(attributes: NSDictionary): UIFontDescriptor; cdecl;
    {class} [MethodName('fontDescriptorWithName:size:')]
    function fontDescriptorWithNameSize(fontName: NSString; size: CGFloat): UIFontDescriptor; cdecl;
    {class} [MethodName('fontDescriptorWithName:matrix:')]
    function fontDescriptorWithNameMatrix(fontName: NSString; matrix: CGAffineTransform): UIFontDescriptor; cdecl;
    {class} function preferredFontDescriptorWithTextStyle(style: NSString): UIFontDescriptor; cdecl;
  end;

  UIFontDescriptor = interface(NSObject)
    ['{4E4A3072-20EE-468F-B5DD-897E00D7A3AE}']
    function postscriptName: NSString; cdecl;
    function pointSize: CGFloat; cdecl;
    function matrix: CGAffineTransform; cdecl;
    function symbolicTraits: UIFontDescriptorSymbolicTraits; cdecl;
    function objectForKey(anAttribute: NSString): Pointer; cdecl;
    function fontAttributes: NSDictionary; cdecl;
    function matchingFontDescriptorsWithMandatoryKeys(mandatoryKeys: NSSet): NSArray; cdecl;
    function initWithFontAttributes(attributes: NSDictionary): Pointer{instancetype}; cdecl;
    function fontDescriptorByAddingAttributes(attributes: NSDictionary): UIFontDescriptor; cdecl;
    function fontDescriptorWithSymbolicTraits(symbolicTraits: UIFontDescriptorSymbolicTraits): UIFontDescriptor; cdecl;
    function fontDescriptorWithSize(newPointSize: CGFloat): UIFontDescriptor; cdecl;
    function fontDescriptorWithMatrix(matrix: CGAffineTransform): UIFontDescriptor; cdecl;
    function fontDescriptorWithFace(newFace: NSString): UIFontDescriptor; cdecl;
    function fontDescriptorWithFamily(newFamily: NSString): UIFontDescriptor; cdecl;
  end;

  TUIFontDescriptor = class(TOCGenericImport<UIFontDescriptorClass, UIFontDescriptor>)
  end;

  UIFontClass = interface(NSObjectClass)
    ['{F21CAA74-9F23-42C5-A0F3-CECA57AFB3BC}']
    {class} function boldSystemFontOfSize(fontSize: CGFloat): Pointer; cdecl;
    {class} function buttonFontSize: CGFloat; cdecl;
    {class} function familyNames: NSArray; cdecl;
    {class} function fontNamesForFamilyName(familyName: NSString): NSArray; cdecl;
    {class} function fontWithName(fontName: NSString; size: CGFloat): Pointer; cdecl;
    {class} function italicSystemFontOfSize(fontSize: CGFloat): Pointer; cdecl;
    {class} function labelFontSize: CGFloat; cdecl;
    {class} function smallSystemFontSize: CGFloat; cdecl;
    {class} function systemFontOfSize(fontSize: CGFloat): Pointer; cdecl;
    {class} function systemFontSize: CGFloat; cdecl;
    {class} function fontWithDescriptor(descriptor: UIFontDescriptor; size: CGFloat): Pointer; cdecl;
  end;
  UIFont = interface(NSObject)
    ['{026495EC-177F-4517-9B25-C2F2371A110D}']
    function ascender: CGFloat; cdecl;
    function capHeight: CGFloat; cdecl;
    function descender: CGFloat; cdecl;
    function familyName: NSString; cdecl;
    function fontName: NSString; cdecl;
    function fontWithSize(fontSize: CGFloat): UIFont; cdecl;
    function leading: CGFloat; cdecl;
    function lineHeight: CGFloat; cdecl;
    function pointSize: CGFloat; cdecl;
    function xHeight: CGFloat; cdecl;
    function fontDescriptor: UIFontDescriptor; cdecl;
  end;
  TUIFont = class(TOCGenericImport<UIFontClass, UIFont>)  end;

  NSStringExClass = interface(NSStringClass)
  ['{DC6AFF07-2E8E-40A3-9EB3-3AE908AC8AE3}']
  end;
  NSStringEx = interface(NSString)
  ['{C5696015-1BB2-4649-AA1F-A8274FAD0603}']
    function cString: MarshaledAString; cdecl;
    function sizeWithFont(font: UIFont; forWidth: CGFloat; lineBreakMode: NSLineBreakMode): CGSize; cdecl; overload;
    function sizeWithFont(font: UIFont): CGSize; cdecl; overload;
    function drawAtPoint(aPoint: NSPoint; withFont: UIFont): CGSize; cdecl; overload;
    procedure drawAtPoint(aPoint: NSPoint; withAttributes: NSDictionary); cdecl; overload;
    procedure drawInRect(aRect: NSRect; withAttributes: NSDictionary); cdecl; overload;
    function drawInRect(aRect: NSRect; withFont: UIFont): CGSize; cdecl; overload;
    function drawInRect(aRect: NSRect; withFont: UIFont; lineBreakMode: NSLineBreakMode; alignment: NSTextAlignment): CGSize; cdecl; overload;
    function sizeWithAttributes(aAttributes: NSDictionary): NSSize; cdecl;
    function boundingRectWithSize(size: CGSize; options: NSStringDrawingOptions; attributes: NSDictionary; context: NSStringDrawingContext): CGRect; cdecl;
  end;
  TNSStringEx = class(TOCGenericImport<NSStringExClass, NSStringEx>)  end;

  NSParagraphStyle = interface;

  NSParagraphStyleClass = interface(NSObjectClass)
  ['{C4DFBBF6-EDE3-4E34-9816-826F7F445585}']
    function defaultParagraphStyle: NSParagraphStyle; cdecl;
    function defaultWritingDirectionForLanguage(languageName: NSString): NSWritingDirection; cdecl;
  end;
  NSParagraphStyle = interface(NSObject)
  ['{BDAA7FD2-9A2A-4C6D-A77A-75315F406540}']
    function alignment: NSTextAlignment; cdecl;
    function firstLineHeadIndent: CGFloat; cdecl;
    function headIndent: CGFloat; cdecl;
    function tailIndent: CGFloat; cdecl;
    function lineHeightMultiple: CGFloat; cdecl;
    function maximumLineHeight: CGFloat; cdecl;
    function minimumLineHeight: CGFloat; cdecl;
    function lineSpacing: CGFloat; cdecl;
    function paragraphSpacing: CGFloat; cdecl;
    function paragraphSpacingBefore: CGFloat; cdecl;
    function tabStops: NSArray; cdecl;
    function defaultTabInterval: CGFloat; cdecl;
    function lineBreakMode: NSLineBreakMode; cdecl;
    function hyphenationFactor: CGFloat; cdecl;
    function baseWritingDirection: NSWritingDirection; cdecl;
  end;
  TNSParagraphStyle = class(TOCGenericImport<NSParagraphStyleClass, NSParagraphStyle>)  end;

  NSMutableParagraphStyleClass = interface(NSObjectClass)
  ['{C4DFBBF6-EDE3-4E34-9816-826F7F445585}']
    function defaultWritingDirectionForLanguage(languageName: NSString): NSWritingDirection; cdecl;
  end;
  NSMutableParagraphStyle = interface(NSObject)
  ['{BDAA7FD2-9A2A-4C6D-A77A-75315F406540}']
    function alignment: NSTextAlignment; cdecl;
    procedure setAlignment(alignment: NSTextAlignment); cdecl;
    function firstLineHeadIndent: CGFloat; cdecl;
    procedure setFirstLineHeadIndent(firstLineHeadIndent: CGFloat); cdecl;
    function headIndent: CGFloat; cdecl;
    procedure setHeadIndent(headIndent: CGFloat); cdecl;
    function tailIndent: CGFloat; cdecl;
    procedure setTailIndent(tailIndent: CGFloat); cdecl;
    function lineHeightMultiple: CGFloat; cdecl;
    procedure setLineHeightMultiple(lineHeightMultiple: CGFloat); cdecl;
    function maximumLineHeight: CGFloat; cdecl;
    procedure setMaximumLineHeight(maximumLineHeight: CGFloat); cdecl;
    function minimumLineHeight: CGFloat; cdecl;
    procedure setMinimumLineHeight(minimumLineHeight: CGFloat); cdecl;
    function lineSpacing: CGFloat; cdecl;
    procedure setLineSpacing(lineSpacing: CGFloat); cdecl;
    function paragraphSpacing: CGFloat; cdecl;
    procedure setParagraphSpacing(paragraphSpacing: CGFloat); cdecl;
    function paragraphSpacingBefore: CGFloat; cdecl;
    procedure setParagraphSpacingBefore(paragraphSpacingBefore: CGFloat); cdecl;
    function tabStops: NSArray; cdecl;
    function defaultTabInterval: CGFloat; cdecl;
    function lineBreakMode: NSLineBreakMode; cdecl;
    procedure setLineBreakMode(lineBreakMode: NSLineBreakMode); cdecl;
    function hyphenationFactor: CGFloat; cdecl;
    procedure setHyphenationFactor(hyphenationFactor: CGFloat); cdecl;
    function style: NSParagraphStyle; cdecl;
    procedure setStyle(style: NSParagraphStyle); cdecl;
  end;
  TNSMutableParagraphStyle = class(TOCGenericImport<NSMutableParagraphStyleClass, NSMutableParagraphStyle>)  end;

  NSLayoutManager = interface;

  NSTextContainerClass = interface(NSObjectClass)
    ['{76FC0807-D297-4A5C-A439-C9D881EC83B5}']
  end;
  NSTextContainer = interface(NSObject)
    ['{B2A0C26E-E652-405C-984D-0445CCA979FD}']
    function heightTracksTextView: Boolean; cdecl;
    function initWithSize(size: CGSize): Pointer; cdecl;
    function isSimpleRectangularTextContainer: Boolean; cdecl;
    function layoutManager: NSLayoutManager; cdecl;
    function lineFragmentPadding: Single; cdecl;
    procedure replaceLayoutManager(newLayoutManager: NSLayoutManager); cdecl;
    procedure setContainerSize(size: CGSize); cdecl;
    procedure setHeightTracksTextView(flag: Boolean); cdecl;
    procedure setLayoutManager(layoutManager: NSLayoutManager); cdecl;
    procedure setLineFragmentPadding(pad: Single); cdecl;
    procedure setWidthTracksTextView(flag: Boolean); cdecl;
    function widthTracksTextView: Boolean; cdecl;
  end;
  TNSTextContainer = class(TOCGenericImport<NSTextContainerClass, NSTextContainer>)  end;

  NSLayoutManagerDelegate = interface
  ['{EFA4EB76-4617-4953-AF61-F6DB3FCB233F}']
  end;

  NSTextStorageDelegate = interface
  ['{882A0DD8-C627-4191-BCE5-7F1CF7780209}']
  end;

  NSLayoutManagerClass = interface(NSObjectClass)
    ['{6BA8AC5B-7B26-42B5-90C5-7FB194F2F306}']
  end;
  NSLayoutManager = interface(NSObject)
    ['{4084A21D-7F14-4732-A33E-D0ADA49B4D59}']
    procedure addTextContainer(container: NSTextContainer); cdecl;
    function delegate: NSLayoutManagerDelegate; cdecl;
    procedure setDelegate(delegate: NSLayoutManagerDelegate); cdecl;
    function glyphRangeForBoundingRect(bounds: NSRect; inTextContainer: NSTextContainer): NSRange; cdecl;
    function characterRangeForGlyphRange(glyphRange: NSRange; actualGlyphRange: PNSRange): NSRange; cdecl;
    function glyphRangeForTextContainer(container: NSTextContainer): NSRange; cdecl;
    procedure drawGlyphsForGlyphRange(glyphsToShow: NSRange; atPoint: CGPoint); cdecl;
    procedure drawBackgroundForGlyphRange(glyphsToShow: NSRange; atPoint: CGPoint); cdecl;
    function textContainers: NSArray; cdecl;
    function numberOfGlyphs: NSUInteger; cdecl;
  end;
  TNSLayoutManager = class(TOCGenericImport<NSLayoutManagerClass, NSLayoutManager>)  end;

  NSTextStorageClass = interface(NSMutableAttributedStringClass)
    ['{EB0E07CA-3010-498B-9307-49744A89849C}']
  end;
  NSTextStorage = interface(NSMutableAttributedString)
    ['{53CEA2E6-F675-44D8-8320-F0305E8F0E86}']
    procedure addLayoutManager(obj: NSLayoutManager); cdecl;
    function changeInLength: NSInteger; cdecl;
    function delegate: NSTextStorageDelegate; cdecl;
    procedure edited(editedMask: NSUInteger; range: NSRange; changeInLength: NSInteger); cdecl;
    function editedMask: NSUInteger; cdecl;
    function editedRange: NSRange; cdecl;
    procedure ensureAttributesAreFixedInRange(range: NSRange); cdecl;
    function fixesAttributesLazily: Boolean; cdecl;
    procedure invalidateAttributesInRange(range: NSRange); cdecl;
    function layoutManagers: NSArray; cdecl;
    procedure processEditing; cdecl;
    procedure removeLayoutManager(obj: NSLayoutManager); cdecl;
    procedure setDelegate(delegate: NSTextStorageDelegate); cdecl;
  end;
  TNSTextStorage = class(TOCGenericImport<NSTextStorageClass, NSTextStorage>)  end;

function ImageFromBitmap(ABitmap: TAdvBitmap): UIImage; overload;
function ImageFromBitmap(ABitmap: TAdvBitmap; ASize: Single): UIImage; overload;
function BitmapFromImage(AImage: UIImage): TAdvBitmap;
function ImageFromBitmapFile(ABitmapFile: String; ASize: Single = -1): UIImage;
function AlphaColorToUIColor(AColor: TAdvGraphicsColor): UIColor;
function UIColorToAlphaColor(AColor: UIColor): TAdvGraphicsColor;
procedure UIGraphicsSetPDFContextURLForRect(url: Pointer {NSURL}; rect: CGRect); cdecl; external libUIKit name _PU + 'UIGraphicsSetPDFContextURLForRect';
procedure UIGraphicsEndPDFContext; cdecl; external libUIKit name _PU + 'UIGraphicsEndPDFContext';
procedure UIGraphicsBeginPDFPageWithInfo(bounds: CGRect; pageInfo: PNSDictionary); cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFPageWithInfo';
function UIGraphicsBeginPDFContextToFile(path: PNSString; bounds: CGRect; documentInfo: PNSDictionary): Boolean; cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFContextToFile';
procedure UIGraphicsBeginPDFContextToData(data: PNSData; bounds: CGRect; documentInfo: PNSDictionary); cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFContextToData';
{$ELSE}
const
  AppKitFwk = '/System/Library/Frameworks/AppKit.framework/AppKit';

type
  NSAttributedStringExClass = interface(NSAttributedStringClass)
  ['{E03D8900-A2B1-4B19-BA3C-4FCD5AC4AD04}']
    function attributedStringWithAttachment(attachment: NSTextAttachment): Pointer; cdecl;
  end;
  NSAttributedStringEx = interface(NSAttributedString)
  ['{A57AB088-DBE3-4987-BCC7-F8EA35013B55}']
    procedure drawInRect(aRect: NSRect); cdecl;
    function boundingRectWithSize(size: CGSize; options: NSStringDrawingOptions; context: Pointer): CGRect; cdecl;
    function RTFFromRange(range: NSRange; documentAttributes: NSDictionary): NSData; cdecl;
    function RTFDFromRange(range: NSRange; documentAttributes: NSDictionary): NSData; cdecl;
    function docFormatFromRange(range: NSRange; documentAttributes: NSDictionary): NSData; cdecl;
    function dataFromRange(range: NSRange; documentAttributes: NSDictionary; error: NSError): NSData; cdecl;
    function initWithData(data: NSData; options: NSDictionary; documentAttributes: NSDictionary; error: NSError): Pointer; cdecl;
    function initWithHTML(data: NSData; documentAttributes: NSDictionary): Pointer; cdecl;
    function initWithRTF(data: NSData; documentAttributes: NSDictionary): Pointer; cdecl;
    function initWithRTFD(data: NSData; documentAttributes: NSDictionary): Pointer; cdecl;
    function initWithDocFormat(data: NSData; documentAttributes: NSDictionary): Pointer; cdecl;
  end;
  TNSAttributedStringEx = class(TOCGenericImport<NSAttributedStringExClass, NSAttributedStringEx>)  end;

  NSStringExClass = interface(NSStringClass)
  ['{E45DC84C-40C8-4F6E-8285-1147E2056364}']
  end;
  NSStringEx = interface(NSString)
  ['{62CF57D3-08BC-4C5B-8B8C-860A5F997CBC}']
    procedure drawAtPoint(aPoint: NSPoint; withAttributes: NSDictionary); cdecl;
    procedure drawInRect(aRect: NSRect; withAttributes: NSDictionary); cdecl;
    function boundingRectWithSize(size: NSSize; options: NSStringDrawingOptions; attributes: NSDictionary): NSRect; cdecl;
  end;
  TNSStringEx = class(TOCGenericImport<NSStringExClass, NSStringEx>)  end;

  NSColorExClass = interface(NSColorClass)
  ['{249C8486-D101-485F-90CF-7B3A6AC42910}']
  end;
  NSColorEx = interface(NSColor)
  ['{9F6D9977-BC7A-4D98-814C-4A3AE9DECE05}']
    function CGColor: CGColorRef; cdecl;
  end;
  TNSColorEx = class(TOCGenericImport<NSColorExClass, NSColorEx>)  end;

  NSTextAttachmentCellClass = interface(NSCellClass)
    ['{6EAB6432-EBCC-49D8-BCEA-3BC269CC72F5}']
  end;
  NSTextAttachmentCell = interface(NSCell)
    ['{F5058137-6B49-4C38-BBD2-40A50C039F9D}']
    function action: Pointer; cdecl;
    procedure setAction(aSelector: Pointer); cdecl;
    procedure setTag(anInt: NSInteger); cdecl;
    procedure setTarget(anObject: Pointer); cdecl;
    function tag: NSInteger; cdecl;
    function target: Pointer; cdecl;
  end;
  TNSTextAttachmentCell = class(TOCGenericImport<NSTextAttachmentCellClass, NSTextAttachmentCell>)  end;

function NSMaxRange(range: NSRange): NSUInteger;
function NSMakeRange(loc: NSUInteger; len: NSUInteger): NSRange;
function ImageFromBitmap(ABitmap: TAdvBitmap): NSImage; overload;
function ImageFromBitmap(ABitmap: TAdvBitmap; ASize: Single): NSImage; overload;
function BitmapFromImage(AImage: NSImage): TAdvBitmap;
function ImageFromBitmapFile(ABitmapFile: String; ASize: Single = -1): NSImage;
function AlphaColorToNSColor(AColor: TAdvGraphicsColor): NSColor;
function NSColorToAlphaColor(AColor: NSColor): TAdvGraphicsColor;
{$ENDIF}
procedure CGPDFContextAddDocumentMetadata(context: CGContextRef; metadata: CFDataRef); cdecl; external libCoreGraphics name _PU + 'CGPDFContextAddDocumentMetadata';
function CGImageSourceCreateWithData(data: CFDataRef; options: CFDictionaryRef): CGImageSourceRef; cdecl; external ImageIOFwk name _PU + 'CGImageSourceCreateWithData';
function CGImageSourceCreateThumbnailAtIndex(isrc: CGImageSourceRef; index: Longword; options: CFDictionaryRef): CGImageRef; cdecl; external ImageIOFwk name _PU + 'CGImageSourceCreateThumbnailAtIndex';
function CGImageSourceCreateWithURL(url: CFURLRef; options: CFDictionaryRef): CGImageSourceRef; cdecl; external ImageIOFwk name _PU + 'CGImageSourceCreateWithURL';
function CGImageSourceCreateWithDataProvider(dataProvider: CGDataProviderRef; options: CFDictionaryRef): CGImageSourceRef; cdecl; external ImageIOFwk name _PU + 'CGImageSourceCreateWithDataProvider';
function CGImageSourceGetType(isrc: CGImageSourceRef): CFStringRef; cdecl; external ImageIOFwk name _PU + 'CGImageSourceGetType';
function CGImageSourceCreateImageAtIndex(isrc: CGImageSourceRef; index: NSInteger; options: CFDictionaryRef): CGImageRef; cdecl; external ImageIOFwk name _PU + 'CGImageSourceCreateImageAtIndex';
function NSStrEx(AString: String): NSString;
function kCTFontFormatAttribute: Pointer;
function kCTFontFixedAdvanceAttribute: Pointer;
{$ENDIF}

{$IFDEF ANDROID}
JStaticLayout = interface;
JSpannableString = interface;
JForegroundColorSpan = interface;
JImageSpan = interface;
JTypefaceSpan = interface;
JBackgroundColorSpan = interface;
JAbsoluteSizeSpan = interface;
JStrikethroughSpan = interface;
JUnderlineSpan = interface;
JMetricAffectingSpan = interface;
JStyleSpan = interface;
JPdfDocument = interface;
JPdfDocument_Page = interface;
JPdfDocument_PageInfo = interface;
JPrintedPdfDocument = interface;
JPageInfo_Builder = interface;
JPrintAttributes = interface;
JPrintAttributes_Builder = interface;
JPrintAttributes_MediaSize = interface;
JEditable = interface;

JStaticLayoutClass = interface(JLayoutClass)
['{D76230AF-C65C-4582-8E10-DD8600B593B7}']
  function init(source: JCharSequence; paint: JTextPaint; width: Integer; align: JLayout_Alignment; spacingmult: Single; spacingadd: Single; includepad: Boolean): JStaticLayout; cdecl; overload;
  function init(source: JCharSequence; bufstart: Integer; bufend: Integer; paint: JTextPaint; outerwidth: Integer; align: JLayout_Alignment; spacingmult: Single; spacingadd: Single; includepad: Boolean): JStaticLayout; cdecl; overload;
  function init(source: JCharSequence; bufstart: Integer; bufend: Integer; paint: JTextPaint; outerwidth: Integer; align: JLayout_Alignment; spacingmult: Single; spacingadd: Single; includepad: Boolean; ellipsize: JTextUtils_TruncateAt; ellipsizedWidth: Integer): JStaticLayout; cdecl; overload;
end;
[JavaSignature('android/text/StaticLayout')]
JStaticLayout = interface(JLayout)
['{722D0BA8-FF91-4E5A-BEA4-1DCC12AC3DAF}']
end;
TJStaticLayout = class(TJavaGenericImport<JStaticLayoutClass, JStaticLayout>) end;

JSpannableStringClass = interface(JObjectClass)
['{E59603E0-6F1F-4D07-B426-D2CF52328FC7}']
  function init(source: JCharSequence): JSpannableString; cdecl;
end;

[JavaSignature('android/text/SpannableString')]
JSpannableString = interface(JObject)
['{0B3C6D0A-37B5-49C0-8AE0-941402C624BA}']
  procedure setSpan(what: JObject; start: Integer; end_: Integer; flags: Integer); cdecl;
end;
TJSpannableString = class(TJavaGenericImport<JSpannableStringClass, JSpannableString>) end;

JMetricAffectingSpanClass = interface(JCharacterStyleClass)
  ['{BD72E80E-B67D-4CB8-B287-2C7B32758572}']
  {class} function init: JMetricAffectingSpan; cdecl;
end;

[JavaSignature('android/text/style/MetricAffectingSpan')]
JMetricAffectingSpan = interface(JCharacterStyle)
  ['{771C211E-1713-4166-820C-BFDA592A5C8A}']
end;
TJMetricAffectingSpan = class(TJavaGenericImport<JMetricAffectingSpanClass, JMetricAffectingSpan>) end;

JTypefaceSpanClass = interface(JMetricAffectingSpanClass)
  ['{BD72E80E-B67D-4CB8-B287-2C7B32758572}']
  {class} function init(family: JString): JTypefaceSpan; cdecl;
end;

[JavaSignature('android/text/style/TypefaceSpan')]
JTypefaceSpan = interface(JMetricAffectingSpan)
  ['{771C211E-1713-4166-820C-BFDA592A5C8A}']
  function getFamily: JString; cdecl;
end;
TJTypefaceSpan = class(TJavaGenericImport<JTypefaceSpanClass, JTypefaceSpan>) end;

JBackgroundColorSpanClass = interface(JCharacterStyleClass)
['{5A23B6BF-C6F8-4803-8EA7-2F5792B73B26}']
  function init(color: Integer): JBackgroundColorSpan; cdecl;
end;
[JavaSignature('android/text/style/BackgroundColorSpan')]
JBackgroundColorSpan = interface(JCharacterStyle)
['{6FBBB5C2-4A14-4A11-A746-BE7852F684C8}']
function getBackgroundColor: Integer; cdecl;
end;
TJBackgroundColorSpan = class(TJavaGenericImport<JBackgroundColorSpanClass, JBackgroundColorSpan>) end;

JForegroundColorSpanClass = interface(JCharacterStyleClass)
['{A23B8427-0AB5-46AB-B9F8-4081950EFBB0}']
  function init(color: Integer): JForegroundColorSpan; cdecl;
end;
[JavaSignature('android/text/style/ForegroundColorSpan')]
JForegroundColorSpan = interface(JCharacterStyle)
['{2D552A1B-385B-49DF-B82A-5728AF42C366}']
function getForegroundColor: Integer; cdecl;
end;
TJForegroundColorSpan = class(TJavaGenericImport<JForegroundColorSpanClass, JForegroundColorSpan>) end;

JStyleSpanClass = interface(JCharacterStyleClass)
['{5A23B6BF-C6F8-4803-8EA7-2F5792B73B26}']
  function init(style: Integer): JStyleSpan; cdecl;
end;
[JavaSignature('android/text/style/StyleSpan')]
JStyleSpan = interface(JCharacterStyle)
['{6FBBB5C2-4A14-4A11-A746-BE7852F684C8}']
function getStyle: Integer; cdecl;
function isBold: Boolean; cdecl;
function isItalic: Boolean; cdecl;
end;
TJStyleSpan = class(TJavaGenericImport<JStyleSpanClass, JStyleSpan>) end;

JStrikethroughSpanClass = interface(JCharacterStyleClass)
['{5A23B6BF-C6F8-4803-8EA7-2F5792B73B26}']
  function init: JStrikethroughSpan; cdecl;
end;
[JavaSignature('android/text/style/StrikethroughSpan')]
JStrikethroughSpan = interface(JCharacterStyle)
['{6FBBB5C2-4A14-4A11-A746-BE7852F684C8}']
end;
TJStrikethroughSpan = class(TJavaGenericImport<JStrikethroughSpanClass, JStrikethroughSpan>) end;

JUnderlineSpanClass = interface(JCharacterStyleClass)
['{5A23B6BF-C6F8-4803-8EA7-2F5792B73B26}']
  function init: JUnderlineSpan; cdecl;
end;
[JavaSignature('android/text/style/UnderlineSpan')]
JUnderlineSpan = interface(JCharacterStyle)
['{6FBBB5C2-4A14-4A11-A746-BE7852F684C8}']
end;
TJUnderlineSpan = class(TJavaGenericImport<JUnderlineSpanClass, JUnderlineSpan>) end;

JAbsoluteSizeSpanClass = interface(JMetricAffectingSpanClass)
['{463312A2-C1C8-480E-967E-77215E32CE7F}']
  function init(size: Integer): JAbsoluteSizeSpan; cdecl; overload;
  function init(size: Integer; dpi: Boolean): JAbsoluteSizeSpan; cdecl; overload;
end;
[JavaSignature('android/text/style/AbsoluteSizeSpan')]
JAbsoluteSizeSpan = interface(JMetricAffectingSpan)
['{E8C2D48E-656F-46F2-BEAD-1A3DC2972B09}']
function getSize: Integer; cdecl;
end;
TJAbsoluteSizeSpan = class(TJavaGenericImport<JAbsoluteSizeSpanClass, JAbsoluteSizeSpan>) end;

JImageSpanClass = interface(JObjectClass)
['{743E868A-27D3-479A-BA8B-5C52C4BC1ACB}']
  function init(d: JDrawable): JImageSpan; cdecl; overload;
  function init(d: JDrawable; verticalAlignment: Integer): JImageSpan; cdecl; overload;
end;
[JavaSignature('android/text/style/ImageSpan')]
JImageSpan = interface(JObject)
['{6DC63921-BDFA-422B-9E3C-6B5886E4E13D}']
  function getDrawable: JDrawable; cdecl;
  function getSource: JString; cdecl;
end;
TJImageSpan = class(TJavaGenericImport<JImageSpanClass, JImageSpan>) end;

JXMLReaderClass = interface(JObjectClass)
['{1A7A6E4B-A3C2-4DC7-85A8-8F28B22D0978}']
end;
[JavaSignature('org/xml/sax/XMLReader')]
JXMLReader = interface(JObject)
['{4DA90053-53F4-4EB2-9520-BB1DF8BFC81A}']
  function getProperty(name: JString): JObject; cdecl;
end;

TJXMLReader = class(TJavaGenericImport<JXMLReaderClass, JXMLReader>)
end;

JHTML_ImageGetterClass = interface(IJavaClass)
['{58259454-6717-4B3C-B641-6E9E3622B282}']
end;
[JavaSignature('android/text/HTML$ImageGetter')]
JHTML_ImageGetter = interface(IJavaInstance)
['{14179BA1-4E2C-458D-9A5C-210C5AE57A85}']
  function getDrawable(source: JString): JDrawable; cdecl;
end;
TJHTML_ImageGetter = class(TJavaGenericImport<JHTML_ImageGetterClass, JHTML_ImageGetter>) end;

JHTML_TagHandlerClass = interface(IJavaClass)
['{58259454-6717-4B3C-B641-6E9E3622B282}']
end;
[JavaSignature('android/text/HTML$TagHandler')]
JHTML_TagHandler = interface(IJavaInstance)
['{14179BA1-4E2C-458D-9A5C-210C5AE57A85}']
  procedure handleTag(opening: Boolean; tag: JString; output: JEditable; xmlReader: JXMLReader); cdecl;
end;
TJHTML_TagHandler = class(TJavaGenericImport<JHTML_TagHandlerClass, JHTML_TagHandler>) end;

JHTMLClass = interface(JObjectClass)
['{6D624AA1-FBF4-45E8-A07F-EB4745BB66FC}']
  function fromHTML(source: JString): JSpanned; cdecl; overload;
  function fromHTML(source: JString; imageGetter: JHTML_ImageGetter; tagHandler: JHTML_TagHandler): JSpanned; cdecl; overload;
  function toHTML(text: JSpanned): JString; cdecl;
  function escapeHTML(text: JCharSequence): JString; cdecl;
end;
[JavaSignature('android/text/HTML')]
JHTML = interface(JObject)
['{F9406242-90DB-411D-8770-756662247A5C}']
end;
TJHTML = class(TJavaGenericImport<JHTMLClass, JHTML>) end;

JPdfDocumentClass = interface(JObjectClass)
['{62D77F93-F2EA-4484-9D7E-7C904EBE8947}']
  {class} function init: JPdfDocument; cdecl;
end;

[JavaSignature('android/graphics/pdf/PdfDocument')]
JPdfDocument = interface(JObject)
['{E4A8CF83-046F-453A-AEB6-6446FF689939}']
  procedure close; cdecl;
  procedure finishPage(page: JPdfDocument_Page); cdecl;
  function getPages: JList; cdecl;
  function startPage(pageInfo: JPdfDocument_PageInfo): JPdfDocument_Page; cdecl;
  procedure writeTo(out_: JOutputStream); cdecl;
end;
TJPdfDocument = class(TJavaGenericImport<JPdfDocumentClass, JPdfDocument>) end;

JPdfDocument_PageClass = interface(JObjectClass)
  ['{4A418EEE-47BF-41CC-A70F-5C08BE3E1CF4}']
end;

[JavaSignature('android/graphics/pdf/PdfDocument$Page')]
JPdfDocument_Page = interface(JObject)
  ['{E6347FD3-7D0F-4388-A527-BFA12B297B34}']
  function getCanvas: JCanvas; cdecl;
  function getInfo: JPdfDocument_PageInfo; cdecl;
end;
TJPdfDocument_Page = class(TJavaGenericImport<JPdfDocument_PageClass, JPdfDocument_Page>) end;

JPdfDocument_PageInfoClass = interface(JObjectClass)
  ['{8B3EA9E0-F8C6-4249-BBFB-9412F998BA64}']
end;

[JavaSignature('android/graphics/pdf/PdfDocument$PageInfo')]
JPdfDocument_PageInfo = interface(JObject)
  ['{5338321F-FDD1-403A-8E0D-78BF3E75D7A8}']
  function getContentRect: JRect; cdecl;
  function getPageHeight: Integer; cdecl;
  function getPageNumber: Integer; cdecl;
  function getPageWidth: Integer; cdecl;
end;
TJPdfDocument_PageInfo = class(TJavaGenericImport<JPdfDocument_PageInfoClass, JPdfDocument_PageInfo>) end;

JPageInfo_BuilderClass = interface(JObjectClass)
  ['{60378CED-24D6-4298-97D3-13245AD3C230}']
  {class} function init(pageWidth: Integer; pageHeight: Integer; pageNumber: Integer): JPageInfo_Builder; cdecl;
end;

[JavaSignature('android/graphics/pdf/PdfDocument$PageInfo$Builder')]
JPageInfo_Builder = interface(JObject)
  ['{9B2C8A40-031A-4065-BF4B-809DFC7EF396}']
  function create: JPdfDocument_PageInfo; cdecl;
  function setContentRect(contentRect: JRect): JPageInfo_Builder; cdecl;
end;
TJPageInfo_Builder = class(TJavaGenericImport<JPageInfo_BuilderClass, JPageInfo_Builder>) end;

JPrintAttributes_MediaSizeClass = interface(JObjectClass)
  ['{E463B724-D4A7-4CE3-B265-4A6B1D661C53}']
  function _GetISO_A0 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_A1 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_A10 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_A2 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_A3 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_A4 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_A5 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_A6 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_A7 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_A8 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_A9 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B0 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B1 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B10 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B2 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B3 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B4 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B5 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B6 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B7 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B8 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_B9 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C0 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C1 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C10 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C2 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C3 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C4 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C5 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C6 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C7 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C8 : JPrintAttributes_MediaSize; cdecl;
  function _GetISO_C9 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B0 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B1 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B10 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B2 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B3 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B4 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B5 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B6 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B7 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B8 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_B9 : JPrintAttributes_MediaSize; cdecl;
  function _GetJIS_EXEC : JPrintAttributes_MediaSize; cdecl;
  function _GetJPN_CHOU2 : JPrintAttributes_MediaSize; cdecl;
  function _GetJPN_CHOU3 : JPrintAttributes_MediaSize; cdecl;
  function _GetJPN_CHOU4 : JPrintAttributes_MediaSize; cdecl;
  function _GetJPN_HAGAKI : JPrintAttributes_MediaSize; cdecl;
  function _GetJPN_KAHU : JPrintAttributes_MediaSize; cdecl;
  function _GetJPN_KAKU2 : JPrintAttributes_MediaSize; cdecl;
  function _GetJPN_OUFUKU : JPrintAttributes_MediaSize; cdecl;
  function _GetJPN_YOU4 : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_FOOLSCAP : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_GOVT_LETTER : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_INDEX_3X5 : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_INDEX_4X6 : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_INDEX_5X8 : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_JUNIOR_LEGAL : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_LEDGER : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_LEGAL : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_LETTER : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_MONARCH : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_QUARTO : JPrintAttributes_MediaSize; cdecl;
  function _GetNA_TABLOID : JPrintAttributes_MediaSize; cdecl;
  function _GetOM_DAI_PA_KAI : JPrintAttributes_MediaSize; cdecl;
  function _GetOM_JUURO_KU_KAI : JPrintAttributes_MediaSize; cdecl;
  function _GetOM_PA_KAI : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_1 : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_10 : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_16K : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_2 : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_3 : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_4 : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_5 : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_6 : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_7 : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_8 : JPrintAttributes_MediaSize; cdecl;
  function _GetPRC_9 : JPrintAttributes_MediaSize; cdecl;
  function _GetROC_16K : JPrintAttributes_MediaSize; cdecl;
  function _GetROC_8K : JPrintAttributes_MediaSize; cdecl;
  function _GetUNKNOWN_LANDSCAPE : JPrintAttributes_MediaSize; cdecl;
  function _GetUNKNOWN_PORTRAIT : JPrintAttributes_MediaSize; cdecl;
  function asLandscape : JPrintAttributes_MediaSize; cdecl;
  function asPortrait : JPrintAttributes_MediaSize; cdecl;
  function equals(obj : JObject) : boolean; cdecl;
  function getHeightMils : Integer; cdecl;
  function getId : JString; cdecl;
  function getLabel(packageManager : JPackageManager) : JString; cdecl;
  function getWidthMils : Integer; cdecl;
  function hashCode : Integer; cdecl;
  function init(id : JString; &label : JString; widthMils : Integer; heightMils : Integer) : JPrintAttributes_MediaSize; cdecl;
  function isPortrait : boolean; cdecl;
  function toString : JString; cdecl;
  property ISO_A0 : JPrintAttributes_MediaSize read _GetISO_A0;
  property ISO_A1 : JPrintAttributes_MediaSize read _GetISO_A1;
  property ISO_A10 : JPrintAttributes_MediaSize read _GetISO_A10;
  property ISO_A2 : JPrintAttributes_MediaSize read _GetISO_A2;
  property ISO_A3 : JPrintAttributes_MediaSize read _GetISO_A3;
  property ISO_A4 : JPrintAttributes_MediaSize read _GetISO_A4;
  property ISO_A5 : JPrintAttributes_MediaSize read _GetISO_A5;
  property ISO_A6 : JPrintAttributes_MediaSize read _GetISO_A6;
  property ISO_A7 : JPrintAttributes_MediaSize read _GetISO_A7;
  property ISO_A8 : JPrintAttributes_MediaSize read _GetISO_A8;
  property ISO_A9 : JPrintAttributes_MediaSize read _GetISO_A9;
  property ISO_B0 : JPrintAttributes_MediaSize read _GetISO_B0;
  property ISO_B1 : JPrintAttributes_MediaSize read _GetISO_B1;
  property ISO_B10 : JPrintAttributes_MediaSize read _GetISO_B10;
  property ISO_B2 : JPrintAttributes_MediaSize read _GetISO_B2;
  property ISO_B3 : JPrintAttributes_MediaSize read _GetISO_B3;
  property ISO_B4 : JPrintAttributes_MediaSize read _GetISO_B4;
  property ISO_B5 : JPrintAttributes_MediaSize read _GetISO_B5;
  property ISO_B6 : JPrintAttributes_MediaSize read _GetISO_B6;
  property ISO_B7 : JPrintAttributes_MediaSize read _GetISO_B7;
  property ISO_B8 : JPrintAttributes_MediaSize read _GetISO_B8;
  property ISO_B9 : JPrintAttributes_MediaSize read _GetISO_B9;
  property ISO_C0 : JPrintAttributes_MediaSize read _GetISO_C0;
  property ISO_C1 : JPrintAttributes_MediaSize read _GetISO_C1;
  property ISO_C10 : JPrintAttributes_MediaSize read _GetISO_C10;
  property ISO_C2 : JPrintAttributes_MediaSize read _GetISO_C2;
  property ISO_C3 : JPrintAttributes_MediaSize read _GetISO_C3;
  property ISO_C4 : JPrintAttributes_MediaSize read _GetISO_C4;
  property ISO_C5 : JPrintAttributes_MediaSize read _GetISO_C5;
  property ISO_C6 : JPrintAttributes_MediaSize read _GetISO_C6;
  property ISO_C7 : JPrintAttributes_MediaSize read _GetISO_C7;
  property ISO_C8 : JPrintAttributes_MediaSize read _GetISO_C8;
  property ISO_C9 : JPrintAttributes_MediaSize read _GetISO_C9;
  property JIS_B0 : JPrintAttributes_MediaSize read _GetJIS_B0;
  property JIS_B1 : JPrintAttributes_MediaSize read _GetJIS_B1;
  property JIS_B10 : JPrintAttributes_MediaSize read _GetJIS_B10;
  property JIS_B2 : JPrintAttributes_MediaSize read _GetJIS_B2;
  property JIS_B3 : JPrintAttributes_MediaSize read _GetJIS_B3;
  property JIS_B4 : JPrintAttributes_MediaSize read _GetJIS_B4;
  property JIS_B5 : JPrintAttributes_MediaSize read _GetJIS_B5;
  property JIS_B6 : JPrintAttributes_MediaSize read _GetJIS_B6;
  property JIS_B7 : JPrintAttributes_MediaSize read _GetJIS_B7;
  property JIS_B8 : JPrintAttributes_MediaSize read _GetJIS_B8;
  property JIS_B9 : JPrintAttributes_MediaSize read _GetJIS_B9;
  property JIS_EXEC : JPrintAttributes_MediaSize read _GetJIS_EXEC;
  property JPN_CHOU2 : JPrintAttributes_MediaSize read _GetJPN_CHOU2;
  property JPN_CHOU3 : JPrintAttributes_MediaSize read _GetJPN_CHOU3;
  property JPN_CHOU4 : JPrintAttributes_MediaSize read _GetJPN_CHOU4;
  property JPN_HAGAKI : JPrintAttributes_MediaSize read _GetJPN_HAGAKI;
  property JPN_KAHU : JPrintAttributes_MediaSize read _GetJPN_KAHU;
  property JPN_KAKU2 : JPrintAttributes_MediaSize read _GetJPN_KAKU2;
  property JPN_OUFUKU : JPrintAttributes_MediaSize read _GetJPN_OUFUKU;
  property JPN_YOU4 : JPrintAttributes_MediaSize read _GetJPN_YOU4;
  property NA_FOOLSCAP : JPrintAttributes_MediaSize read _GetNA_FOOLSCAP;
  property NA_GOVT_LETTER : JPrintAttributes_MediaSize read _GetNA_GOVT_LETTER;
  property NA_INDEX_3X5 : JPrintAttributes_MediaSize read _GetNA_INDEX_3X5;
  property NA_INDEX_4X6 : JPrintAttributes_MediaSize read _GetNA_INDEX_4X6;
  property NA_INDEX_5X8 : JPrintAttributes_MediaSize read _GetNA_INDEX_5X8;
  property NA_JUNIOR_LEGAL : JPrintAttributes_MediaSize read _GetNA_JUNIOR_LEGAL;
  property NA_LEDGER : JPrintAttributes_MediaSize read _GetNA_LEDGER;
  property NA_LEGAL : JPrintAttributes_MediaSize read _GetNA_LEGAL;
  property NA_LETTER : JPrintAttributes_MediaSize read _GetNA_LETTER;
  property NA_MONARCH : JPrintAttributes_MediaSize read _GetNA_MONARCH;
  property NA_QUARTO : JPrintAttributes_MediaSize read _GetNA_QUARTO;
  property NA_TABLOID : JPrintAttributes_MediaSize read _GetNA_TABLOID;
  property OM_DAI_PA_KAI : JPrintAttributes_MediaSize read _GetOM_DAI_PA_KAI;
  property OM_JUURO_KU_KAI : JPrintAttributes_MediaSize read _GetOM_JUURO_KU_KAI;
  property OM_PA_KAI : JPrintAttributes_MediaSize read _GetOM_PA_KAI;
  property PRC_1 : JPrintAttributes_MediaSize read _GetPRC_1;
  property PRC_10 : JPrintAttributes_MediaSize read _GetPRC_10;
  property PRC_16K : JPrintAttributes_MediaSize read _GetPRC_16K;
  property PRC_2 : JPrintAttributes_MediaSize read _GetPRC_2;
  property PRC_3 : JPrintAttributes_MediaSize read _GetPRC_3;
  property PRC_4 : JPrintAttributes_MediaSize read _GetPRC_4;
  property PRC_5 : JPrintAttributes_MediaSize read _GetPRC_5;
  property PRC_6 : JPrintAttributes_MediaSize read _GetPRC_6;
  property PRC_7 : JPrintAttributes_MediaSize read _GetPRC_7;
  property PRC_8 : JPrintAttributes_MediaSize read _GetPRC_8;
  property PRC_9 : JPrintAttributes_MediaSize read _GetPRC_9;
  property ROC_16K : JPrintAttributes_MediaSize read _GetROC_16K;
  property ROC_8K : JPrintAttributes_MediaSize read _GetROC_8K;
  property UNKNOWN_LANDSCAPE : JPrintAttributes_MediaSize read _GetUNKNOWN_LANDSCAPE;
  property UNKNOWN_PORTRAIT : JPrintAttributes_MediaSize read _GetUNKNOWN_PORTRAIT;
end;

[JavaSignature('android/print/PrintAttributes$MediaSize')]
JPrintAttributes_MediaSize = interface(JObject)
  ['{3D89217B-93F4-4DB3-9E92-F99E06B9EFD1}']
  function asLandscape : JPrintAttributes_MediaSize; cdecl;
  function asPortrait : JPrintAttributes_MediaSize; cdecl;
  function equals(obj : JObject) : boolean; cdecl;
  function getHeightMils : Integer; cdecl;
  function getId : JString; cdecl;
  function getLabel(packageManager : JPackageManager) : JString; cdecl;
  function getWidthMils : Integer; cdecl;
  function hashCode : Integer; cdecl;
  function isPortrait : boolean; cdecl;
  function toString : JString; cdecl;
end;

TJPrintAttributes_MediaSize = class(TJavaGenericImport<JPrintAttributes_MediaSizeClass, JPrintAttributes_MediaSize>)
end;

JPrintAttributes_ResolutionClass = interface(JObjectClass)
['{890A9602-792D-417F-B413-41CD6576A178}']
end;
[JavaSignature('android/print/PrintAttribute$Resolution')]
JPrintAttributes_Resolution = interface(JObject)
['{0E4C6F96-FB83-4187-B26C-332F9DBDEBD3}']
end;
TJPrintAttributes_Resolution = class(TJavaGenericImport<JPrintAttributes_ResolutionClass, JPrintAttributes_Resolution>) end;

JPrintAttributes_MarginsClass = interface(JObjectClass)
['{CB38E8EF-9112-4F1F-B713-0B2E03A8EDF3}']
end;
[JavaSignature('android/print/PrintAttribute$Margins')]
JPrintAttributes_Margins = interface(JObject)
['{21BF97D5-0C39-4F71-BA25-1B1C168BF23E}']
end;
TJPrintAttributes_Margins = class(TJavaGenericImport<JPrintAttributes_MarginsClass, JPrintAttributes_Margins>) end;

JPrintAttributesClass = interface(JObjectClass)
['{16CFEB84-F40A-45E2-8220-4AF3682B7FD7}']
  function _GetCOLOR_MODE_COLOR : Integer; cdecl;
  function _GetCOLOR_MODE_MONOCHROME : Integer; cdecl;
  property COLOR_MODE_COLOR : Integer read _GetCOLOR_MODE_COLOR;
  property COLOR_MODE_MONOCHROME : Integer read _GetCOLOR_MODE_MONOCHROME;
end;
[JavaSignature('android/print/PrintAttributes')]
JPrintAttributes = interface(JObject)
['{899388B0-B3B2-4259-B5A3-147D43392C60}']
end;
TJPrintAttributes = class(TJavaGenericImport<JPrintAttributesClass, JPrintAttributes>) end;

JPrintAttributes_BuilderClass = interface(JObjectClass)
['{62D77F93-F2EA-4484-9D7E-7C904EBE8947}']
  function init: JPrintAttributes_Builder; cdecl;
end;
[JavaSignature('android/print/PrintAttributes$Builder')]
JPrintAttributes_Builder = interface(JObject)
['{E4A8CF83-046F-453A-AEB6-6446FF689939}']
  function build: JPrintAttributes; cdecl;
  function setColorMode(colorMode: Integer): JPrintAttributes_Builder; cdecl;
  function setMediaSize(mediaSize: JPrintAttributes_MediaSize): JPrintAttributes_Builder; cdecl;
  function setMargins(Margins: JPrintAttributes_Margins): JPrintAttributes_Builder; cdecl;
  function setResolution(Resolution: JPrintAttributes_Resolution): JPrintAttributes_Builder; cdecl;
end;
TJPrintAttributes_Builder = class(TJavaGenericImport<JPrintAttributes_BuilderClass, JPrintAttributes_Builder>) end;

JPrintedPdfDocumentClass = interface(JPdfDocumentClass)
['{62D77F93-F2EA-4484-9D7E-7C904EBE8947}']
  {class} function init(context: JContext; attributes: JPrintAttributes): JPrintedPdfDocument; cdecl;
end;

[JavaSignature('android/print/pdf/PrintedPdfDocument')]
JPrintedPdfDocument = interface(JPdfDocument)
['{E4A8CF83-046F-453A-AEB6-6446FF689939}']
end;
TJPrintedPdfDocument = class(TJavaGenericImport<JPrintedPdfDocumentClass, JPrintedPdfDocument>) end;

JEditableClass = interface(JCharSequenceClass)
['{84A38124-9D41-4471-A616-31BCB6E2F798}']
end;

[JavaSignature('android/text/Editable')]
JEditable = interface(JCharSequence)
['{5FA14BBC-C3C3-492C-BF3F-5770374929C9}']
  function append(text: JCharSequence): JEditable; cdecl; overload;
  function append(text: JCharSequence; start: Integer; end_: Integer): JEditable; cdecl; overload;
  function append(text: Char): JEditable; cdecl; overload;
  procedure clear; cdecl;
  procedure clearSpans; cdecl;
  function delete(st: Integer; en: Integer): JEditable; cdecl;
  function getFilters: TJavaObjectArray<JInputFilter>; cdecl;
  function insert(where: Integer; text: JCharSequence; start: Integer; end_: Integer): JEditable; cdecl; overload;
  function insert(where: Integer; text: JCharSequence): JEditable; cdecl; overload;
  function replace(st: Integer; en: Integer; source: JCharSequence; start: Integer; end_: Integer): JEditable; cdecl; overload;
  function replace(st: Integer; en: Integer; text: JCharSequence): JEditable; cdecl; overload;
  procedure setFilters(filters: TJavaObjectArray<JInputFilter>); cdecl;
  procedure setSpan(what: JObject; start: Integer; end_: Integer; flags: Integer); cdecl;
end;
TJEditable = class(TJavaGenericImport<JEditableClass, JEditable>) end;

procedure FontToJTextPaint(AFont: TAdvPDFGraphicsLibFont; ATextPaint: JTextPaint);
function AlphaColorToJColor(AColor: TAdvGraphicsColor): Integer;
function JColorToAlphaColor(AColor: Integer): TAdvGraphicsColor;
function ImageFromBitmap(ABitmap: TAdvBitmap): JBitmap;
{$ENDIF}

function GetBValue(rgb: DWORD): Byte;
{$EXTERNALSYM GetBValue}
function GetGValue(rgb: DWORD): Byte;
{$EXTERNALSYM GetGValue}
function GetRValue(rgb: DWORD): Byte;
{$EXTERNALSYM GetRValue}
function Hiword(L: DWORD): integer;
{$EXTERNALSYM Hiword}
function LoWord(L: DWORD): Integer;
{$EXTERNALSYM LoWord}
function MakeWord(b1,b2: integer): integer;
{$EXTERNALSYM MakeWord}
function MakeLong(i1,i2: integer): integer;
{$EXTERNALSYM MakeLong}
procedure GetAspectSize(var {%H-}x, {%H-}y, w, h: Single; ow, oh, nw, nh: Single; AspectRatio: Boolean; Stretch: Boolean; Cropping: Boolean);
function MakeTextRange(ALocation, ALength: Integer): TAdvPDFGraphicsLibTextRange;
function MaxRange(ARange: TAdvPDFGraphicsLibTextRange): Integer;
function CreateStringStream(AValue: string = ''): TAdvPDFGraphicsLibOutputWriterStream;
function GetTickCountX: DWORD;
function DateTimeToIso(ADateTime: TDateTime): string;
function ConvertToPDFDate(ADateTime: TDateTime): string;

implementation

uses
  SysUtils, AdvUtils, Math, DateUtils
  {$IFNDEF LCLLIB}
  ,zlib
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,zstream
  {$ENDIF}
  ;

{$IFDEF MACOS}
var
  kCTFontFormatAttribute_: Pointer;
  kCTFontFixedAdvanceAttribute_: Pointer;
{$ENDIF}

function CreateStringStream(AValue: string = ''): TAdvPDFGraphicsLibOutputWriterStream;
begin
  Result := TAdvPDFGraphicsLibOutputWriterStream.Create(AValue);
end;

function MaxRange(ARange: TAdvPDFGraphicsLibTextRange): Integer;
begin
  Result := ARange.location + ARange.length;
end;

function MakeTextRange(ALocation, ALength: Integer): TAdvPDFGraphicsLibTextRange;
begin
  Result.location := ALocation;
  Result.length := ALength;
end;

procedure GetAspectSize(var x, y, w, h: Single; ow, oh, nw, nh: Single;
  AspectRatio: Boolean; Stretch: Boolean; Cropping: Boolean);
var
  arc, ar: Single;
begin
  if AspectRatio then
  begin
    if (ow > 0) and (oh > 0) and (nw > 0) and (nh > 0) then
    begin
      if (ow < nw) and (oh < nh) and (not Stretch) then
      begin
        w := ow;
        h := oh;
      end
      else
      begin
        if ow / oh < nw / nh then
        begin
          h := nh;
          w := nh * ow / oh;
        end
        else
        begin
          w := nw;
          h := nw * oh / ow;
        end;
      end;
    end
    else
    begin
      w := 0;
      h := 0;
    end;
  end
  else
  begin
    if Stretch then
    begin
      w := nw;
      h := nh;
    end
    else
    begin
      w := ow;
      h := oh;

      if Cropping then
      begin
        if (w >= h) and (w > 0) then
        begin
          h := nw / w * h;
          w := nh;
        end
        else
        if (h >= w) and (h > 0) then
        begin
          w := nh / h * w;
          h := nh;
        end;

        if h = 0 then
          ar := 1
        else
          ar := w / h;

        if nh = 0 then
          arc := 1
        else
          arc := nw / nh;

        if (ar < 1) or (arc > ar) then
        begin
          h := nw / ar;
          w := nw;
        end
        else
        begin
          w := ar * nh;
          h := nh;
        end;
      end;
    end;
  end;
end;

function IntToZStr(i,l: Integer):string;
var
  Res: string;
begin
  Res := IntToStr(i);
  while Length(Res)<l do
    Res := '0' + Res;

  Result := Res;
end;

function DateTimeToIso(ADateTime: TDateTime): string;
var
  da,mo,ye,ho,mi,se,ms:word;
begin
  DecodeDate(ADateTime, ye, mo, da);
  DecodeTime(ADateTime, ho, mi, se, ms);
  Result := IntToZStr(ye,4) + '-' + IntToZStr(mo,2) + '-' + IntToZStr(da,2) + 'T' +
            IntToZStr(ho,2) + ':' + IntToZStr(mi,2) + ':' + IntToZStr(se,2) + 'Z';
end;

function ConvertToPDFDate(ADateTime: TDateTime): string;
begin
  Result := FormatFloat('0000', YearOf(ADateTime)) + FormatFloat('00', MonthOf(ADateTime)) + FormatFloat('00', DayOf(ADateTime)) +
    FormatFloat('00', HourOf(ADateTime)) + FormatFloat('00', MinuteOf(ADateTime)) + FormatFloat('00', SecondOf(ADateTime)) + 'Z';
end;

function GetTickCountX: DWORD;
var
  h, m, s, ms: Word;
begin
  DecodeTime(Now, h, m, s, ms);
  Result := ms + s * 1000 + m * 60 * 1000 + h * 60 * 60 * 1000;
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

function GetBValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb);
end;

function GetGValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 8);
end;

function GetRValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 16);
end;

{$IFDEF MACOS}
function NSStrEx(AString: String): NSString;
begin
  Result := TNSString.Wrap(TNSString.OCClass.stringWithCharacters(PChar(AString), AString.Length));
end;
{$IFDEF IOS}

function AlphaColorToUIColor(AColor: TAdvGraphicsColor): UIColor;
var
  r, g, b, a: Byte;
begin
  if AColor = gcNull then
  begin
    Result := TUIColor.Wrap(TUIColor.OCClass.clearColor);
    Exit;
  end;

  r := TAlphaColorRec(AColor).R;
  g := TAlphaColorRec(AColor).G;
  b := TAlphaColorRec(AColor).B;
  a := TAlphaColorRec(AColor).A;
  Result := TUIColor.Wrap(TUIColor.OCClass.colorWithRed(r / 255, g / 255, b / 255, a / 255));
end;

function UIColorToAlphaColor(AColor: UIColor): TAdvGraphicsColor;
var
  r, g, b, a: Single;
begin
  if AColor = TUIColor.Wrap(TUIColor.OCClass.clearColor) then
  begin
    Result := gcNull;
    Exit;
  end;

  AColor.getRed(@r, @g, @b, @a);

  TAlphaColorRec(Result).R := Round(r * 255);
  TAlphaColorRec(Result).g := Round(g * 255);
  TAlphaColorRec(Result).b := Round(b * 255);
  TAlphaColorRec(Result).a := Round(a * 255);
end;

function ImageFromBitmap(ABitmap: TAdvBitmap; ASize: Single): UIImage;
  type
    TBArray = TArray<Byte>;
  procedure BitmapToByteArray(var ByteArray: TBArray);
  var
    MS: TMemoryStream;
  begin
    MS := TMemoryStream.Create;
    try
      ABitmap.SaveToStream(MS);
      MS.Seek(0, TSeekOrigin.soBeginning);
      SetLength(ByteArray, MS.size);
      MS.ReadBuffer(ByteArray[0], Length(ByteArray));
    finally
      MS.free;
    end;
  end;
var
  imageSource: CGImageSourceRef;
  options: CFDictionaryRef;
  opt: NSMutableDictionary;
  imgref: CGImageRef;
  scaled: UIImage;
  data: Pointer;
  btarr: TBArray;
begin
  Result := nil;
  if not Assigned(ABitmap) then
    Exit;

  if ABitmap.isEmpty then
    Exit;

  BitmapToByteArray(btarr);
  data := TNSData.OCClass.dataWithBytes(@btarr[0], Length(btarr));
  imageSource := CGImageSourceCreateWithData(data, nil);

  if not Assigned(imageSource) then
    Exit;

  if ASize > -1 then
    opt := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).initWithCapacity(2))
  else
    opt := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).initWithCapacity(1));

  opt.setValue(Pointer(kCFBooleanTrue), NSSTREx('kCGImageSourceCreateThumbnailFromImageAlways'));
  if ASize > -1 then
    opt.setValue(TNSNumber.OCClass.numberWithFloat(ASize), NSSTREx('kCGImageSourceThumbnailMaxPixelSize'));

  options := TNSDictionary.OCClass.dictionaryWithDictionary(opt);

  imgref := CGImageSourceCreateThumbnailAtIndex(imageSource, 0, options);

  scaled := TUIImage.Wrap(TUIImage.OCClass.imageWithCGImage(imgref));
  CGImageRelease(imgref);
  CFRelease(imageSource);
  opt.release;
  Result := scaled;
end;

function ImageFromBitmapFile(ABitmapFile: String; ASize: Single = -1): UIImage;
var
  imageSource: CGImageSourceRef;
  url: Pointer;
  options: CFDictionaryRef;
  opt: NSMutableDictionary;
  imgref: CGImageRef;
  scaled: UIImage;
begin
  Result := nil;
  if not FileExists(ABitmapFile) then
    Exit;

  url := TNSURL.OCClass.fileURLWithPath(NSSTREx(ABitmapFile));
  imageSource := CGImageSourceCreateWithURL(CFURLRef(url), nil);

  if not Assigned(imageSource) then
    Exit;

  if ASize > -1 then
    opt := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).initWithCapacity(2))
  else
    opt := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).initWithCapacity(1));

  opt.setValue(Pointer(kCFBooleanTrue), NSSTREx('kCGImageSourceCreateThumbnailFromImageAlways'));
  if ASize > -1 then
    opt.setValue(TNSNumber.OCClass.numberWithFloat(ASize), NSSTREx('kCGImageSourceThumbnailMaxPixelSize'));

  options := TNSDictionary.OCClass.dictionaryWithDictionary(opt);

  imgref := CGImageSourceCreateThumbnailAtIndex(imageSource, 0, options);

  scaled := TUIImage.Wrap(TUIImage.OCClass.imageWithCGImage(imgref));
  CGImageRelease(imgref);
  CFRelease(imageSource);
  opt.release;
  Result := scaled;
end;

function BitmapFromImage(AImage: UIImage): TAdvBitmap;
var
  dt: NSData;
  ms: TMemoryStream;
begin
  Result := nil;
  if not Assigned(AImage) then
   Exit;

  dt := TNSData.Wrap(UIImagePNGRepresentation((AImage as ILocalObject).GetObjectID));
  if Assigned(dt) then
  begin
    Result := TAdvBitmap.Create(0, 0);
    ms := TMemoryStream.Create;
    ms.Write(dt.bytes^, dt.length);
    Result.LoadFromStream(ms);
    ms.Free;
  end;
end;

function ImageFromBitmap(ABitmap: TAdvBitmap): UIImage;
var
  ms: TMemoryStream;
  dt: NSData;
begin
  Result := nil;
  if not Assigned(ABitmap) then
    Exit;

  if ABitmap.IsEmpty then
    Exit;

  ms := TMemoryStream.Create;
  ABitmap.SaveToStream(ms);
  ms.Position := 0;
  dt := TNSData.Wrap(TNSData.OCClass.dataWithBytes(ms.Memory, ms.Size));
  ms.Free;
  Result := TUIImage.Wrap(TUIImage.OCClass.imageWithData(dt));
end;

{$ELSE}

function NSMaxRange(range: NSRange): NSUInteger;
begin
  Result := range.location + range.length;
end;

function NSColorToAlphaColor(AColor: NSColor): TAdvGraphicsColor;
var
  r, g, b, a: Single;
begin
  if AColor = TNSColor.Wrap(TNSColor.OCClass.clearColor) then
  begin
    Result := gcNull;
    Exit;
  end;

  AColor.getRed(@r, @g, @b, @a);

  TAlphaColorRec(Result).R := Round(r * 255);
  TAlphaColorRec(Result).g := Round(g * 255);
  TAlphaColorRec(Result).b := Round(b * 255);
  TAlphaColorRec(Result).a := Round(a * 255);
end;

function AlphaColorToNSColor(AColor: TAdvGraphicsColor): NSColor;
var
  r, g, b: Byte;
begin
  if AColor = gcNull then
  begin
    Result := TNSColor.Wrap(TNSColor.OCClass.clearColor);
    Exit;
  end;

  r := GetRValue(AColor);
  g := GetGValue(AColor);
  b := GetBValue(AColor);
  Result := TNSColor.Wrap(TNSColor.OCClass.colorWithDeviceRed(r / 255, g / 255, b / 255, 1.0));
end;

function NSMakeRange(loc: NSUInteger; len: NSUInteger): NSRange;
begin
  Result.location := loc;
  Result.length := len;
end;

function ImageFromBitmap(ABitmap: TAdvBitmap; ASize: Single): NSImage;
  type
    TBArray = Array of Byte;
  procedure BitmapToByteArray(var ByteArray: TBArray);
  var
    MS: TMemoryStream;
  begin
    MS := TMemoryStream.Create;
    try
      ABitmap.SaveToStream(MS);
      MS.Seek(0, TSeekOrigin.soBeginning);
      SetLength(ByteArray, MS.size);
      MS.ReadBuffer(ByteArray[0], Length(ByteArray));
    finally
      MS.free;
    end;
  end;
var
  imageSource: CGImageSourceRef;
  options: CFDictionaryRef;
  opt: NSMutableDictionary;
  imgref: CGImageRef;
  scaled: NSImage;
  data: Pointer;
  btarr: TBArray;
  sz: NSSize;
begin
  Result := nil;
  if not Assigned(ABitmap) then
    Exit;

  if ABitmap.isEmpty then
    Exit;

  BitmapToByteArray(btarr);
  data := TNSData.OCClass.dataWithBytes(@btarr[0], Length(btarr));
  imageSource := CGImageSourceCreateWithData(data, nil);

  if not Assigned(imageSource) then
    Exit;

  if ASize > -1 then
    opt := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).initWithCapacity(2))
  else
    opt := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).initWithCapacity(1));

  opt.setValue(Pointer(kCFBooleanTrue), NSStrEx('kCGImageSourceCreateThumbnailFromImageAlways'));
  if ASize > -1 then
    opt.setValue(TNSNumber.OCClass.numberWithFloat(ASize), NSStrEx('kCGImageSourceThumbnailMaxPixelSize'));

  options := TNSDictionary.OCClass.dictionaryWithDictionary(opt);

  imgref := CGImageSourceCreateThumbnailAtIndex(imageSource, 0, options);

  sz.width := 0;
  sz.height := 0;
  scaled := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithCGImage(imgref, sz));
  CGImageRelease(imgref);
  CFRelease(imageSource);
  Result := scaled;
end;

function ImageFromBitmapFile(ABitmapFile: String; ASize: Single = -1): NSImage;
var
  imageSource: CGImageSourceRef;
  url: Pointer;
  options: CFDictionaryRef;
  opt: NSMutableDictionary;
  imgref: CGImageRef;
  scaled: NSImage;
  sz: NSSize;
begin
  Result := nil;
  if not FileExists(ABitmapFile) then
    Exit;

  url := TNSURL.OCClass.fileURLWithPath(NSStrEx(ABitmapFile));
  imageSource := CGImageSourceCreateWithURL(CFURLRef(url), nil);

  if not Assigned(imageSource) then
    Exit;

  if ASize > -1 then
    opt := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).initWithCapacity(2))
  else
    opt := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).initWithCapacity(1));

  opt.setValue(Pointer(kCFBooleanTrue), NSStrEx('kCGImageSourceCreateThumbnailFromImageAlways'));
  if ASize > -1 then
    opt.setValue(TNSNumber.OCClass.numberWithFloat(ASize), NSStrEx('kCGImageSourceThumbnailMaxPixelSize'));

  options := TNSDictionary.OCClass.dictionaryWithDictionary(opt);

  imgref := CGImageSourceCreateThumbnailAtIndex(imageSource, 0, options);

  sz.width := 0;
  sz.height := 0;
  scaled := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithCGImage(imgref, sz));
  CGImageRelease(imgref);
  CFRelease(imageSource);
  Result := scaled;
end;

function BitmapFromImage(AImage: NSImage): TAdvBitmap;
var
  dt: NSData;
  ms: TMemoryStream;
  imgrep: NSBitmapImageRep;
begin
  Result := nil;
  if not Assigned(AImage) then
   Exit;

  imgrep := TNSBitmapImageRep.Wrap(AImage.representations.objectAtIndex(0));
  dt := imgrep.representationUsingType(NSPNGFileType, nil);
  if Assigned(dt) then
  begin
    Result := TAdvBitmap.Create(0, 0);
    ms := TMemoryStream.Create;
    ms.Write(dt.bytes^, dt.length);
    Result.LoadFromStream(ms);
    ms.Free;
  end;
end;

function ImageFromBitmap(ABitmap: TAdvBitmap): NSImage;
var
  ms: TMemoryStream;
  dt: NSData;
begin
  Result := nil;
  if not Assigned(ABitmap) then
    Exit;

  if ABitmap.IsEmpty then
    Exit;

  ms := TMemoryStream.Create;
  ABitmap.SaveToStream(ms);
  ms.Position := 0;
  dt := TNSData.Wrap(TNSData.OCClass.dataWithBytes(ms.Memory, ms.Size));
  ms.Free;
  Result := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithData(dt));
end;

{$ENDIF}
function GetCTAttributeObject(Attr: string): Pointer;
var
  CTLib: HMODULE;
begin
  CTLib := LoadLibrary(libCoreText);
  Result := Pointer(GetProcAddress(CTLib, PWideChar(Attr))^);
  FreeLibrary(CTLib);
end;

function kCTFontFormatAttribute: Pointer;
begin
  if kCTFontFormatAttribute_ = nil then
    kCTFontFormatAttribute_ := GetCTAttributeObject('kCTFontFormatAttribute');
  Result := kCTFontFormatAttribute_;
end;

function kCTFontFixedAdvanceAttribute: Pointer;
begin
  if kCTFontFixedAdvanceAttribute_ = nil then
    kCTFontFixedAdvanceAttribute_ := GetCTAttributeObject('kCTFontFixedAdvanceAttribute');
  Result := kCTFontFixedAdvanceAttribute_;
end;
{$ENDIF}

{$IFDEF ANDROID}
procedure FontToJTextPaint(AFont: TAdvPDFGraphicsLibFont; ATextPaint: JTextPaint);
var
  flg: Integer;
begin
  ATextPaint.setColor(AlphaColorToJColor(AFont.Color));

  if FileExists(AFont.FileName) then
    ATextPaint.setTypeface(TJTypeface.JavaClass.createFromFile(StringToJString(AFont.FileName)))
  else
  begin
    if (TFontStyle.fsBold in AFont.Style) and (TFontStyle.fsItalic in AFont.Style) then
      ATextPaint.setTypeface(TJTypeface.JavaClass.create(StringToJString(AFont.Name), TJTypeface.JavaClass.BOLD_ITALIC))
    else if TFontStyle.fsBold in AFont.Style then
      ATextPaint.setTypeface(TJTypeface.JavaClass.create(StringToJString(AFont.Name), TJTypeface.JavaClass.BOLD))
    else if TFontStyle.fsBold in AFont.Style then
      ATextPaint.setTypeface(TJTypeface.JavaClass.create(StringToJString(AFont.Name), TJTypeface.JavaClass.ITALIC))
    else
      ATextPaint.setTypeface(TJTypeface.JavaClass.create(StringToJString(AFont.Name), TJTypeface.JavaClass.NORMAL));
  end;

  ATextPaint.setTextSize(AFont.Size);

  flg := 0;
  if TFontStyle.fsUnderline in AFont.Style then
    flg := flg or TJPaint.JavaClass.UNDERLINE_TEXT_FLAG;

  if TFontStyle.fsStrikeOut in AFont.Style then
    flg := flg or TJPaint.JavaClass.STRIKE_THRU_TEXT_FLAG;

    ATextPaint.setFlags(flg);
end;

function AlphaColorToJColor(AColor: TAdvGraphicsColor): Integer;
var
  r, g, b, a: Byte;
begin
  if AColor = gcNull then
  begin
    Result := TJColor.JavaClass.TRANSPARENT;
    Exit;
  end;

  r := TAlphaColorRec(AColor).R;
  g := TAlphaColorRec(AColor).G;
  b := TAlphaColorRec(AColor).B;
  a := TAlphaColorRec(AColor).A;
  Result := TJColor.JavaClass.argb(a, r, g, b);
end;

function JColorToAlphaColor(AColor: Integer): TAdvGraphicsColor;
var
  r, g, b, a: Integer;
begin
  if AColor = TJColor.JavaClass.TRANSPARENT then
  begin
    Result := gcNull;
    Exit;
  end;

  r := (AColor shr 16) and $FF;
  g := (AColor shr 8) and $FF;
  b := (AColor shr 0) and $FF;
  a := (AColor shr 24) and $FF;

  TAlphaColorRec(Result).R := r;
  TAlphaColorRec(Result).g := g;
  TAlphaColorRec(Result).b := b;
  TAlphaColorRec(Result).a := a;
end;

function ImageFromBitmap(ABitmap: TAdvBitmap): JBitmap;
var
  ms: TMemoryStream;
  ba: TJavaArray<Byte>;
  o: JBitmapFactory_Options;
begin
  if Assigned(ABitmap) then
  begin
    o := TJBitmapFactory_Options.JavaClass.init;
    ms := TMemoryStream.Create;
    try
      ABitmap.SaveToStream(ms);
      ms.Position := 0;
      ba := TJavaArray<Byte>.Create(ms.Size);
      Move(ms.Memory^, ba.Data^, ms.Size);
      Result := TJBitmapFactory.JavaClass.decodeByteArray(ba, 0, ms.Size, o);
    finally
      ms.Free;
    end;
  end;
end;
{$ENDIF}

{ TAdvPDFGraphicsLibFont }

procedure TAdvPDFGraphicsLibFont.Assign(Source: TPersistent);
begin
  if Source is TAdvPDFGraphicsLibFont then
  begin
    BeginUpdate;
    FName := (Source as TAdvPDFGraphicsLibFont).Name;
    FSize := (Source as TAdvPDFGraphicsLibFont).Size;
    FColor := (Source as TAdvPDFGraphicsLibFont).Color;
    FStyle := (Source as TAdvPDFGraphicsLibFont).Style;
    FFileName := (Source as TAdvPDFGraphicsLibFont).FileName;
    EndUpdate;
  end;
end;

procedure TAdvPDFGraphicsLibFont.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TAdvPDFGraphicsLibFont.Create;
begin
  inherited;
  FName := DefaultFontName;
  FSize := 12;
  FColor := gcBlack;
  FStyle := [];
  FFileName := '';
end;

procedure TAdvPDFGraphicsLibFont.DoChanged;
begin
  if FUpdateCount > 0 then
    Exit;

  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TAdvPDFGraphicsLibFont.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    DoChanged;
end;

function TAdvPDFGraphicsLibFont.IsSizeStored: Boolean;
begin
  Result := Size <> 12;
end;

procedure TAdvPDFGraphicsLibFont.SetName(const Value: String);
begin
  FName := Value;
  DoChanged;
end;

procedure TAdvPDFGraphicsLibFont.SetColor(const Value: TAdvGraphicsColor);
begin
  FColor := Value;
  DoChanged;
end;

procedure TAdvPDFGraphicsLibFont.SetFileName(const Value: String);
begin
  FFileName := Value;
  DoChanged;
end;

procedure TAdvPDFGraphicsLibFont.SetSize(const Value: Single);
var
  v: Single;
begin
  v := Value;
  if v = 0 then
    v := 12;

  {$IFDEF CMNLIB}
  FSize := v * 96 / 72;
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFNDEF MSWINDOWS}
  FSize := v * 72 / 96;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FSize := v;
  {$ENDIF}
  {$ENDIF}

  FSize := Floor(FSize);
  DoChanged;
end;

procedure TAdvPDFGraphicsLibFont.SetSizeNoScale(const Value: Single);
begin
  FSize := Value;
  DoChanged;
end;

procedure TAdvPDFGraphicsLibFont.SetStyle(const Value: TFontStyles);
begin
  FStyle := Value;
  DoChanged;
end;

{ TAdvPDFGraphicsLibBase }

constructor TAdvPDFGraphicsLibBase.Create;
begin
  CreateClasses;
  FFont := TAdvPDFGraphicsLibFont.Create;
  FFont.OnChanged := FontChanged;
  FURLFont := TAdvPDFGraphicsLibFont.Create;
  FURLFont.Color := gcBlue;
  FURLFont.Style := [TFontStyle.fsUnderline];
  FLineBreakMode := bmLineBreakModeWordWrap;
  FAlignment := gtaLeading;
  FFill := TAdvPDFGraphicsFill.Create;
  FFill.OnChanged := FillChanged;
  FStroke := TAdvPDFGraphicsStroke.Create;
  FStroke.OnChanged := StrokeChanged;
  InitializeAppearance;
end;

procedure TAdvPDFGraphicsLibBase.CreateClasses;
begin

end;

destructor TAdvPDFGraphicsLibBase.Destroy;
begin
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

  if Assigned(FURLFont) then
  begin
    FURLFont.Free;
    FURLFont := nil;
  end;

  if Assigned(FFont) then
  begin
    FFont.Free;
    FFont := nil;
  end;
  inherited;
end;

procedure TAdvPDFGraphicsLibBase.FillChanged(Sender: TObject);
begin
  UpdateFill;
end;

procedure TAdvPDFGraphicsLibBase.FontChanged(Sender: TObject);
begin
  UpdateFont;
end;

procedure TAdvPDFGraphicsLibBase.InitializeAppearance;
begin

end;

procedure TAdvPDFGraphicsLibBase.SetAlignment(const Value: TAdvGraphicsTextAlign);
begin
  FAlignment := Value;
  UpdateAlignment;
end;

procedure TAdvPDFGraphicsLibBase.SetPictureContainer(
  const Value: TPictureContainer);
begin
  FPictureContainer := Value;
end;

procedure TAdvPDFGraphicsLibBase.SetFill(
  const Value: TAdvPDFGraphicsFill);
begin
  FFill.Assign(Value);
end;

procedure TAdvPDFGraphicsLibBase.SetFont(const Value: TAdvPDFGraphicsLibFont);
begin
  FFont.Assign(Value);
end;

procedure TAdvPDFGraphicsLibBase.SetLineBreakMode(
  const Value: TAdvPDFGraphicsLibLineBreakMode);
begin
  FLineBreakMode := Value;
  UpdateLineBreakMode;
end;

procedure TAdvPDFGraphicsLibBase.SetStroke(
  const Value: TAdvPDFGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

procedure TAdvPDFGraphicsLibBase.SetURLFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  FURLFont.Assign(Value);
end;

procedure TAdvPDFGraphicsLibBase.StrokeChanged(Sender: TObject);
begin
  UpdateStroke;
end;

procedure TAdvPDFGraphicsLibBase.UpdateAlignment;
begin

end;

procedure TAdvPDFGraphicsLibBase.UpdateFill;
begin

end;

procedure TAdvPDFGraphicsLibBase.UpdateFont;
begin

end;

procedure TAdvPDFGraphicsLibBase.UpdateLineBreakMode;
begin

end;

procedure TAdvPDFGraphicsLibBase.UpdateStroke;
begin

end;

{ TAdvPDFPlatformServices }

procedure TAdvPDFPlatformServices.AddPlatformService(const AServiceGUID: TGUID; const AService: IInterface);
var
  LService: IInterface;
begin
  {$IFDEF LCLLIB}
  if FServicesList.IndexOf(GUIDToString(AServiceGUID)) = -1 then
  {$ENDIF}
  {$IFNDEF LCLLIB}
  if not FServicesList.ContainsKey(GUIDToString(AServiceGUID)) then
  {$ENDIF}
  begin
    if Supports(AService, AServiceGUID, LService) then
      FServicesList.Add(GUIDToString(AServiceGUID), AService);
  end;
end;

constructor TAdvPDFPlatformServices.Create;
begin
  inherited;
  FServicesList := TAdvPlatformServicesList.Create;
  FGlobalFlags := TAdvPlatformServicesGlobalFlags.Create;
end;

destructor TAdvPDFPlatformServices.Destroy;
begin
  FServicesList.Free;
  FGlobalFlags.Free;
  inherited;
end;

{$IFNDEF AUTOREFCOUNT}
class procedure TAdvPDFPlatformServices.ReleaseCurrent;
begin
  FreeAndNil(FCurrent);
  FCurrentReleased := True;
end;
{$ENDIF}

class function TAdvPDFPlatformServices.GetCurrent: TAdvPDFPlatformServices;
begin
  if (FCurrent = nil) and not FCurrentReleased then
    FCurrent := TAdvPDFPlatformServices.Create;
  Result := FCurrent;
end;

function TAdvPDFPlatformServices.GetPlatformService(const AServiceGUID: TGUID): IInterface;
var
  k: IInterface;
  {$IFDEF LCLLIB}
  I: Integer;
  {$ENDIF}
begin
  {$IFNDEF LCLLIB}
  k := FServicesList.Items[GUIDToString(AServiceGUID)];
  {$ENDIF}
  {$IFDEF LCLLIB}
  i := FServicesList.IndexOf(GUIDToString(AServiceGUID));
  k := nil;
  if (i >= 0) and (i <= FServicesList.Count - 1) then
    k := IInterface(FServicesList.Data[I]);
  {$ENDIF}
  Supports(k, AServiceGUID, Result);
end;

procedure TAdvPDFPlatformServices.RemovePlatformService(const AServiceGUID: TGUID);
begin
  FServicesList.Remove(GUIDToString(AServiceGUID));
end;

function TAdvPDFPlatformServices.SupportsPlatformService(const AServiceGUID: TGUID;
  out AService): Boolean;
{$IFDEF LCLLIB}
var
  i: Integer;
  k: IInterface;
{$ENDIF}
begin
  {$IFDEF LCLLIB}
  i := FServicesList.IndexOf(GUIDToString(AServiceGUID));
  if (i >= 0) and (i <= FServicesList.Count - 1) then
  begin
    k := IInterface(FServicesList.Data[I]);
    Result := Supports(k, AServiceGUID, AService);
  end
  {$ENDIF}
  {$IFNDEF LCLLIB}
  if FServicesList.ContainsKey(GUIDToString(AServiceGUID)) then
    Result := Supports(FServicesList.Items[GUIDToString(AServiceGUID)], AServiceGUID, AService)
  {$ENDIF}
  else
  begin
    Pointer(AService) := nil;
    Result := False;
  end;
end;

function TAdvPDFPlatformServices.SupportsPlatformService(const AServiceGUID: TGUID): Boolean;
begin
  {$IFDEF LCLLIB}
  Result := FServicesList.IndexOf(GUIDToString(AServiceGUID)) > -1;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  Result := FServicesList.ContainsKey(GUIDToString(AServiceGUID));
  {$ENDIF}
end;

{ TAdvPDFGraphicsLibOutputWriter }

procedure TAdvPDFGraphicsLibOutputWriter.ClearProperties;
begin
  FFontBase := '';
  FFontName := '';
  FFontUnicode := False;
  FFontRefName := '';
  FFontSize := 0;
  FFontColor := gcNull;
  FFontStyle := [];
  FFontWordSpacing := 0;
  FFontCharSpacing := 0;
  FFontLeading := 0;
  SetLength(FFontCharWidths, 0);
  FFontCharWidths := nil;
  FFontUsedCharArray := nil;
end;

function TAdvPDFGraphicsLibOutputWriter.IsUnicodeString(AValue: UnicodeString): Boolean;
var
  I: Integer;
begin
  Result := False;
  {$IFDEF DELPHI_LLVM}
  for I := 0 to Length(AValue) - 1 do
  {$ENDIF}
  {$IFNDEF DELPHI_LLVM}
  for I := 1 to Length(AValue) do
  {$ENDIF}
  begin
    if Ord(AValue[I]) > 127 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TAdvPDFGraphicsLibOutputWriter.ConvertColorToString(AValue: TAdvGraphicsColor): String;
begin
  Result := ConvertFloatToString(TAdvGraphics.GetColorRed(AValue) / 255) + ' ' +
            ConvertFloatToString(TAdvGraphics.GetColorGreen(AValue) / 255) + ' ' +
            ConvertFloatToString(TAdvGraphics.GetColorBlue(AValue) / 255);
end;

function TAdvPDFGraphicsLibOutputWriter.ConvertFloatToString(AValue: Extended): String;
var
  s: Char;
begin
  Result := FloatToStr(Trunc(AValue * 100 + 0.5) / 100);
  s := FormatSettings.DecimalSeparator;
  if s <> '.' then
    Result := StringReplace(Result, s, '.', [rfReplaceAll]);
end;

function TAdvPDFGraphicsLibOutputWriter.ConvertStringToHex(AValue: UnicodeString): String;
var
  I: integer;
  v: Integer;
  idx: integer;
begin
  Result := '';
  {$IFDEF DELPHI_LLVM}
  for I := 0 to Length(AValue) - 1 do
  {$ENDIF}
  {$IFNDEF DELPHI_LLVM}
  for I := 1 to Length(AValue) do
  {$ENDIF}
  begin
    v := Ord(AValue[I]);
    if (v >= 32) then
    begin
      if not FontCharArrayContainsValue(v) then
        FontCharArrayAddValue(v);

      idx := FontCharArray.IndexOf(v);
      if idx > -1 then
        Result := Result + AddHex4(FontCharWidths[idx].g);
    end;
  end;
end;

function TAdvPDFGraphicsLibOutputWriter.AddHex4(
  AWordValue: Cardinal): String;
const
  HexChars: array[0..15] of Char = '0123456789ABCDEF';
var
  v: Cardinal;
  B: array[0..3] of Char;
begin
  v := aWordValue shr 8;
  aWordValue := aWordValue and $ff;
  B[0] := HexChars[v shr 4];
  B[1] := HexChars[v and $F];
  B[2] := HexChars[aWordValue shr 4];
  B[3] := HexChars[aWordValue and $F];
  Result := B;
end;

procedure TAdvPDFGraphicsLibOutputWriter.AddText(AValue: UnicodeString);
var
  s: string;
begin
  if FontUnicode or IsUnicodeString(AValue) then
    s := '<' + ConvertStringToHex(AValue) + '>'
  else
    s := '(' + EscapeString(AValue) + ')';

  WriteString(s + ' Tj' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.BeginText;
begin
  WriteString('BT' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.EndText;
begin
  WriteString('ET' + PDFLB);
end;

function TAdvPDFGraphicsLibOutputWriter.EscapeString(AValue: UnicodeString): string;
var
  I: Integer;
  K: Integer;
  f: Boolean;
  v: Integer;
begin
  Result := '';
  {$IFDEF DELPHI_LLVM}
  for I := 0 to Length(AValue) - 1 do
  {$ENDIF}
  {$IFNDEF DELPHI_LLVM}
  for I := 1 to Length(AValue) do
  {$ENDIF}
  begin
    v := Ord(AValue[I]);
    if not FontCharArrayContainsValue(v) then
      FontCharArrayAddValue(v);

    f := False;
    for K := 0 to Length(PDFEC) - 1 do
    begin
      if AValue[I] = PDFEC[K] then
      begin
        Result := Result + '\' + PDFRC[K];
        f := True;
        Break;
      end;
    end;

    if not f then
      Result := Result + String(AValue[I]);
  end;
end;

function TAdvPDFGraphicsLibOutputWriter.CompressStream(
  AStream: TStringStream): TStringStream;
var
  vDest: TStringStream;
  vSource: TStringStream;
  vCompressor: TCompressionStream;
begin
  Result := nil;
  if AStream.Size = 0 then
    Exit;

  vDest := CreateStringStream;
  try
  	vCompressor := TCompressionStream.Create(clMax, vDest);
    try
      vSource := AStream;
      vCompressor.CopyFrom(vSource, 0);
    finally
      vCompressor.Free;
    end;

    vDest.Position := 0;
    Result := vDest;
  finally
  end;
end;

function TAdvPDFGraphicsLibOutputWriter.CompressString(AValue: string): TStringStream;
var
  vDest: TStringStream;
  vSource: TStream;
  vCompressor: TCompressionStream;
begin
  Result := nil;
  if AValue = '' then
    Exit;

  vDest := CreateStringStream;
  try
  	vCompressor := TCompressionStream.Create(clMax, vDest);
    try
      vSource := CreateStringStream(AValue);
      try
        vCompressor.CopyFrom(vSource, 0);
      finally
        vSource.Free;
      end;
    finally
      vCompressor.Free;
    end;

    vDest.Position := 0;
    Result := vDest;
  finally
  end;
end;


function TAdvPDFGraphicsLibOutputWriter.FontCharArrayContainsValue(ACharValue: Integer): Boolean;
begin
  Result := False;
  if Assigned(FFontUsedCharArray) and (ACharValue >= 32) then
    Result := FFontUsedCharArray.IndexOf(ACharValue) > -1;
end;

{$IFDEF LCLLIB}
function CompareVal(const Item1, Item2: Integer): Integer;
begin
  Result := Item1 - Item2;
end;
{$ENDIF}

function TAdvPDFGraphicsLibOutputWriter.FinishContentStream(AReference: String; AAdditionalFlags: String = ''): TStringStream;
var
  ls: Integer;
begin
  Result := nil;
  if Assigned(FContentStream) then
  begin
    Result := CompressStream(FContentStream);
    try
      ls := 0;
      if Assigned(Result) then
        ls := Result.Size;

      FContentStream.Free;
      FContentStream := nil;
      ReplaceString(AReference, '/Length ' + IntToStr(ls) + '/Filter/FlateDecode' + AAdditionalFlags);
    finally
    end;
  end
  else if Assigned(FStream) then
    ReplaceString(AReference, '');
end;

procedure TAdvPDFGraphicsLibOutputWriter.FontCharArrayAddValue(ACharValue: Integer);
begin
  if Assigned(FontUsedCharArray) and (ACharValue >= 32) then
  begin
    FFontUsedCharArray.Add(ACharValue);
    {$IFNDEF LCLLIB}
    FFontUsedCharArray.Sort(FCompareValues);
    {$ENDIF}
    {$IFDEF LCLLIB}
    FFontUsedCharArray.Sort(@CompareVal);
    {$ENDIF}
  end;
end;

function TAdvPDFGraphicsLibOutputWriter.GetFontCharWidth(AText: UnicodeString;
  APos: Integer): Integer;
var
  i: Integer;
begin
  i := FontCharArray.IndexOf(Ord(AText[APos]));
  if (i >= 0) and (i <= Length(FontCharWidths) - 1) then
    Result := FontCharWidths[i].w
  else
    Result := 600;
end;

function TAdvPDFGraphicsLibOutputWriter.GetStreamPosition: Int64;
begin
  Result := FStream.Position;
end;

procedure TAdvPDFGraphicsLibOutputWriter.LineTo(APoint: TPointF);
begin
  WriteString(ConvertFloatToString(APoint.X) + ' ' + ConvertFloatToString(APoint.Y) + ' l' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.MoveTextTo(APoint: TPointF);
begin
  WriteString(ConvertFloatToString(APoint.X) + ' ' + ConvertFloatToString(APoint.Y) + ' Td' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.MoveTextToNextLine;
begin
  WriteString('T*'+PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.MoveTo(APoint: TPointF);
begin
  WriteString(ConvertFloatToString(APoint.X) + ' ' + ConvertFloatToString(APoint.Y) + ' m' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.NotifyBitmap(AValue: TAdvBitmap; AImageType: TAdvPDFGraphicsLibImageType; AQuality: Single; ABackgroundColor: TAdvGraphicsColor; var ABitmapReference: string);
begin
  if Assigned(OnNotifyBitmap) then
    OnNotifyBitmap(Self, AValue, AImageType, AQuality, ABackgroundColor, ABitmapReference);
end;

procedure TAdvPDFGraphicsLibOutputWriter.NotifyShading(AFillColor: TAdvGraphicsColor; AFillColorTo: TAdvGraphicsColor; AFillOrientation: TAdvGraphicsFillOrientation; var AShadingReference: String);
begin
  if Assigned(OnNotifyShading) then
    OnNotifyShading(Self, AFillColor, AFillColorTo, AFillOrientation, AShadingReference);
end;

procedure TAdvPDFGraphicsLibOutputWriter.NotifyShadingRect(
  ARect: TRectF);
begin
  if Assigned(OnNotifyShadingRect) then
    OnNotifyShadingRect(Self, ARect);
end;

procedure TAdvPDFGraphicsLibOutputWriter.NotifyText(AValue: UnicodeString);
begin
  if Assigned(OnNotifyText) then
    OnNotifyText(Self, AValue);
end;

procedure TAdvPDFGraphicsLibOutputWriter.NotifyUnicode(AValue: UnicodeString);
begin
  if Assigned(OnNotifyUnicode) then
    OnNotifyUnicode(Self, AValue);
end;

procedure TAdvPDFGraphicsLibOutputWriter.NotifyURL(ARect: TRectF; AURL: UnicodeString);
begin
  if Assigned(OnNotifyURL) then
    OnNotifyURL(Self, ARect, AURL);
end;

procedure TAdvPDFGraphicsLibOutputWriter.ReplaceString(ASearchValue, AValue: String);
begin
  ReplaceStrings([ASearchValue], [AValue]);
end;

procedure TAdvPDFGraphicsLibOutputWriter.ReplaceStrings(ASearchValues,
  AValues: array of String);
var
  s: TStringStream;
  sd: String;
  I: Integer;
begin
  if (Length(ASearchValues) > 0) and (Length(AValues) > 0) then
  begin
    if Assigned(FContentStream) then
    begin
      s := CreateStringStream;
      try
        FContentStream.Position := 0;
        s.CopyFrom(FContentStream, FContentStream.Size);
        sd := s.DataString;
        for I := 0 to Length(ASearchValues) - 1 do
          sd := StringReplace(sd, ASearchValues[I], AValues[I], [rfReplaceAll]);
        FContentStream.Size := 0;
        WriteString(sd);
      finally
        s.Free;
      end;
    end
    else if Assigned(FStream) then
    begin
      s := CreateStringStream;
      try
        FStream.Position := 0;
        s.CopyFrom(FStream, FStream.Size);
        sd := s.DataString;
        for I := 0 to Length(ASearchValues) - 1 do
          sd := StringReplace(sd, ASearchValues[I], AValues[I], [rfReplaceAll]);
        FStream.Size := 0;
        WriteString(sd);
      finally
        s.Free;
      end;
    end;
  end;
end;

procedure TAdvPDFGraphicsLibOutputWriter.Write(Buffer: Pointer; Count: Integer);
begin
  if Assigned(FContentStream) then
    FContentStream.Write(Buffer^, Count)
  else
    FStream.Write(Buffer^, Count);
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteFillColor(
  AColor: TAdvGraphicsColor);
begin
  WriteString(ConvertColorToString(AColor) + ' rg' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteFont;
begin
  WriteString(FontRefName + ' ' + IntToStr(Round(FontSize)) + ' Tf' + PDFLB)
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteFontColor;
begin
  WriteString(ConvertColorToString(FontColor) + ' rg' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteFontLeading;
begin
  WriteString(ConvertFloatToString(FontLeading) + ' TL' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteMatrix(APoint: TPointF;
  ASize: TSizeF);
begin
  WriteString(ConvertFloatToString(ASize.cx) + ' 0 0 ' + ConvertFloatToString(ASize.cy) + ' ' + ConvertFloatToString(APoint.X) + ' ' + ConvertFloatToString(APoint.Y) + ' cm' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteRectangle(ARect: TRectF);
begin
  WriteString(ConvertFloatToString(ARect.Left) + ' ' + ConvertFloatToString(ARect.Top) + ' ' + ConvertFloatToString(ARect.Width) + ' ' + ConvertFloatToString(ARect.Height) + ' re' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteString(AValue: String);
begin
  if Assigned(FContentStream) then
    FContentStream.WriteString(AValue)
  else
    FStream.WriteString(AValue);
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteStrokeColor(
  AColor: TAdvGraphicsColor);
begin
  WriteString(ConvertColorToString(AColor) + ' RG' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteStrokeKind(
  AKind: TAdvGraphicsStrokeKind);
begin
  case AKind of
    gskDash: WriteString('[4] 1 d' + PDFLB);
    gskDot: WriteString('[2] 1 d' + PDFLB);
    gskDashDot: WriteString('[4 2 2 2] 2 d' + PDFLB);
    gskDashDotDot: WriteString('[4 2 1 2 1 2] 2 d' + PDFLB);
  else
    WriteString('[] 0 d' + PDFLB);
  end;
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteStrokeWidth(AWidth: Single);
begin
  WriteString(ConvertFloatToString(AWidth) + ' w' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.WriteTextMatrix(AX1, AX2, AX3, AX4, AX5, AX6: Single);
begin
  WriteString(ConvertFloatToString(AX1) + ' ' +
              ConvertFloatToString(AX2) + ' ' +
              ConvertFloatToString(AX3) + ' ' +
              ConvertFloatToString(AX4) + ' ' +
              ConvertFloatToString(AX5) + ' ' +
              ConvertFloatToString(AX6) + ' Tm' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.SetStreamPosition(
  const Value: Int64);
begin
  FStream.Position := Value;
end;

procedure TAdvPDFGraphicsLibOutputWriter.StartContentStream(AReference: TObject = nil);
begin
  FContentStream := CreateStringStream;
  FContentStream.Reference := AReference;
end;

procedure TAdvPDFGraphicsLibOutputWriter.StartNewStream(AReference: TObject = nil);
begin
  FStream := CreateStringStream;
  FStream.Reference := AReference;
  if Assigned(FStream) then
    FStreams.Add(FStream);
end;

function TAdvPDFGraphicsLibOutputWriter.Streams: TAdvPDFGraphicsLibOutputWriterStreams;
begin
  Result := FStreams;
end;

function TAdvPDFGraphicsLibOutputWriter.TextHeight: Single;
begin
  Result := FontCapHeight * FontSize * 96 / (72 * FontUnitsPerEm);
end;

constructor TAdvPDFGraphicsLibOutputWriter.Create;
begin
  {$IFNDEF LCLLIB}
  FCompareValues := TDelegatedComparer<Integer>.Create(
    function(const Item1, Item2: Integer): Integer
    begin
      Result := Item1 - Item2;
    end
    );
  {$ENDIF}
  FStreams := TAdvPDFGraphicsLibOutputWriterStreams.Create;
  ClearProperties;
end;

procedure TAdvPDFGraphicsLibOutputWriter.CurveTo(AX1, AX2, AX3, AX4, AX5,
  AX6: Single);
begin
  WriteString(ConvertFloatToString(AX1) + ' ' +
              ConvertFloatToString(AX2) + ' ' +
              ConvertFloatToString(AX3) + ' ' +
              ConvertFloatToString(AX4) + ' ' +
              ConvertFloatToString(AX5) + ' ' +
              ConvertFloatToString(AX6) + ' c' + PDFLB);
end;

procedure TAdvPDFGraphicsLibOutputWriter.CurveTo2(AX1, AX2, AX3,
  AX4: Single);
begin
  WriteString(ConvertFloatToString(AX1) + ' ' +
              ConvertFloatToString(AX2) + ' ' +
              ConvertFloatToString(AX3) + ' ' +
              ConvertFloatToString(AX4) + ' v' + PDFLB);
end;

destructor TAdvPDFGraphicsLibOutputWriter.Destroy;
begin
  if Assigned(FStreams) then
  begin
    FStreams.Free;
    FStreams := nil;
  end;
  inherited;
end;

function FastLocateWordSorted(P: PWordArray; R: Integer; V: word): Integer;
var
  L, cmp: Integer;
begin
  if R < 0 then
    Result := 0
  else
  begin
    L := 0;
    repeat
    begin
      Result := (L + R) shr 1;
      cmp := P^[Result] - v;
      if cmp = 0 then
      begin
        Result := -Result - 1;
        Exit;
      end;

      if cmp < 0 then
        L := Result + 1
      else
        R := Result - 1;

    end;
    until (L > R);

    while (Result >= 0) and (P^[Result] >= v) do
      Dec(Result);

    Result := Result + 1;
  end;
end;

function TAdvPDFGraphicsLibFontCharArray.Add(AValue: Word): Integer;
begin
  Result := FastLocateWordSorted(Pointer(v), c - 1, aValue);
  if Result < 0 then
    Exit;

  if c = Length(v) then
    SetLength(v, c + 100);

  if Result < c then
    Move(v[result], v[Result + 1],(c - Result) * 2)
  else
    Result := c;

  v[Result] := AValue;
  inc(c);
end;

function TAdvPDFGraphicsLibFontCharArray.FirstValue: Integer;
begin
  Result := v[0];
end;

function TAdvPDFGraphicsLibFontCharArray.IndexOf(AValue: Word): Integer;
var
  L, R: Integer;
  cmp: integer;
begin
  L := 0;
  R := c - 1;
  if 0 <= R then
  repeat
    Result := (L + R) shr 1;
    cmp := v[Result] - aValue;
    if cmp = 0 then
      Exit
    else if cmp < 0 then
      L := Result + 1
    else
      R := Result - 1;
  until (L > R);
  Result := -1;
end;

function TAdvPDFGraphicsLibFontCharArray.LastValue: Integer;
begin
  Result := v[Length(v) - 1];
end;

{ TAdvPDFGraphicsLibOutputWriterStream }

destructor TAdvPDFGraphicsLibOutputWriterStream.Destroy;
begin
  FReference := nil;
  inherited;
end;

initialization
begin
  TAdvPDFPlatformServices.FCurrentReleased := False;
end;

finalization
begin
{$IFNDEF AUTOREFCOUNT}
  TAdvPDFPlatformServices.ReleaseCurrent;
{$ENDIF}
end;

end.
