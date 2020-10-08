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

unit AdvPDFRichTextLib;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvPDFCoreLibBase, AdvGraphicsTypes, AdvTypes
  {$IFNDEF LCLLIB}
  ,Types, Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ;

type
  TAdvPDFRichTextLibRange = record
    location: Integer;
    length: Integer;
  end;

  TAdvPDFRichTextLibDataType = (
    dtArchivedXMLDocumentType,
    dtPlainTextDocumentType,
    dtRTFTextDocumentType,
    dtRTFDTextDocumentType,
    dtHTMLTextDocumentType,
    dtSimpleTextDocumentType,
    dtDocFormatTextDocumentType,
    dtWordMLTextDocumentType,
    dtOfficeOpenXMLTextDocumentType,
    dtWebArchiveTextDocumentType,
    dtOpenDocumentTextDocumentType
  );

  TAdvPDFRichTextLibAttributeName = (
     anFontAttributeName,
     anParagraphStyleAttributeName,
     anForegroundColorAttributeName,
     anBackgroundColorAttributeName,
     anLigatureAttributeName,
     anKernAttributeName,
     anStrikethroughStyleAttributeName,
     anUnderlineStyleAttributeName,
     anStrokeColorAttributeName,
     anStrokeWidthAttributeName,
     anShadowAttributeName,
     anTextEffectAttributeName,
     anAttachmentAttributeName,
     anLinkAttributeName,
     anToolTipAttributeName,
     anBaselineOffsetAttributeName,
     anUnderlineColorAttributeName,
     anStrikethroughColorAttributeName,
     anObliquenessAttributeName,
     anExpansionAttributeName,
     anWritingDirectionAttributeName,
     anVerticalGlyphFormAttributeName
  );

  TAdvPDFRichTextLibUnderlineStyle = (
    usUnderlineStyleNone,
    usUnderlineStyleSingle,
    usUnderlineStyleThick,
    usUnderlineStyleDouble,
    usUnderlinePatternSolid,
    usUnderlinePatternDot,
    usUnderlinePatternDash,
    usUnderlinePatternDashDot,
    usUnderlinePatternDashDotDot,
    usUnderlineByWord
  );

  TAdvPDFRichTextLibUnderlineStyles = set of TAdvPDFRichTextLibUnderlineStyle;

  TAdvPDFRichTextLibFontValue = record
    FontFamily: String;
    FontName: String;
    FontSize: Single;
  end;

  TAdvPDFRichTextLibTabStop = record
    Location: Single;
    Alignment: TAdvGraphicsTextAlign;
    {$IFDEF LCLLIB}
    class operator = (z1, z2 : TAdvPDFRichTextLibTabStop) b : Boolean;
    {$ENDIF}
  end;

  TAdvPDFRichTextLibTabStops = TList<TAdvPDFRichTextLibTabStop>;

  TAdvPDFRichTextLibParagraphStyle = record
    Alignment: TAdvGraphicsTextAlign;
    FirstLineHeadIndent: Single;
    HeadIndent: Single;
    TailIndent: Single;
    LineHeightMultiple: Single;
    MaximumLineHeight: Single;
    MinimumLineHeight: Single;
    LineSpacing: Single;
    ParagraphSpacing: Single;
    ParagraphSpacingBefore: Single;
    TabStops: TAdvPDFRichTextLibTabStops;
    LineBreakMode: TAdvPDFGraphicsLibLineBreakMode;
    HyphenationFactor: Single;
  end;

  TAdvPDFRichTextLibAttributeValue = record
    ParagraphStyle: TAdvPDFRichTextLibParagraphStyle;
    Bitmap: TAdvBitmap;
    BitmapFile: String;
    ForegroundColor: TAdvGraphicsColor;
    BackgroundColor: TAdvGraphicsColor;
    UnderlineColor: TAdvGraphicsColor;
    StrikethroughColor: TAdvGraphicsColor;
    BaselineOffset: Single;
    StrokeColor: TAdvGraphicsColor;
    StrokeWidth: Single;
    URL: String;
    ToolTip: String;
    Underline: TAdvPDFRichTextLibUnderlineStyles;
    Strikethrough: TAdvPDFRichTextLibUnderlineStyles;
    ApplyBold: Boolean;
    Bold: Boolean;
    ApplyItalic: Boolean;
    Italic: Boolean;
    ApplyFontName: Boolean;
    FontName, FontFamily: String;
    ApplyFontSize: Boolean;
    FontSize: Single;
  end;

  TAdvPDFRichTextLibDataDetectorType = (
   dtDataDetectorTypePhoneNumber,
   dtDataDetectorTypeLink,
   dtDataDetectorTypeAddress,
   dtDataDetectorTypeCalendarEvent,
   dtDataDetectorTypeNone,
   dtDataDetectorTypeAll
  );

  TAdvPDFRichTextLibDataDetectorTypes = set of TAdvPDFRichTextLibDataDetectorType;

  IAdvCustomPDFRichTextLib = interface(IInterface)
  ['{E4E9AE27-E487-4D46-BA6C-33B6BA6E54E4}']
    procedure SetCanvas(ACanvas: Pointer);
    function GetSelection: TAdvPDFRichTextLibRange;
    function Draw(Rect: TRectF; Calculate: Boolean = False): TRectF; overload;
    function Draw(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function Draw(Rects: TAdvPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer; overload;
    function GetValues(AStart: Integer = -1; ALength: Integer = -1): TAdvPDFRichTextLibAttributeValue;
    procedure InitializeValues(var AValues: TAdvPDFRichTextLibAttributeValue);
    function GetURL(AStart: Integer = -1; ALength: Integer = -1): String;
    procedure SetURL(AValue: String; AStart: Integer = -1; ALength: Integer = -1);
    procedure AddBitmap(AValue: TAdvBitmap; ALineHeight: Single = -1; ALocation: Integer = -1);
    procedure AddBitmapFromFile(AValue: String; ALineHeight: Single = -1; ALocation: Integer = -1);
    function GetSubscript(AStart: Integer = -1; ALength: Integer = -1): Single;
    procedure SetSubscript(AValue: Single = -2; AStart: Integer = -1; ALength: Integer = -1);
    function GetBaselineOffset(AStart: Integer = -1; ALength: Integer = -1): Single;
    procedure SetBaselineOffset(AValue: Single; AStart: Integer = -1; ALength: Integer = -1);
    function GetSuperscript(AStart: Integer = -1; ALength: Integer = -1): Single;
    procedure SetSuperscript(AValue: Single = 2; AStart: Integer = -1; ALength: Integer = -1);
    function GetStrokeColor(AStart: Integer = -1; ALength: Integer = -1): TAdvGraphicsColor;
    procedure SetStrokeColor(AValue: TAdvGraphicsColor; AStart: Integer = -1; ALength: Integer = -1);
    function GetStrokeWidth(AStart: Integer = -1; ALength: Integer = -1): Single;
    procedure SetStrokeWidth(AWidth: Single; AStart: Integer = -1; ALength: Integer = -1);
    function GetUnderline(AStart: Integer = -1; ALength: Integer = -1): TAdvPDFRichTextLibUnderlineStyles;
    procedure SetUnderline(AValue: TAdvPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
    function GetUnderlineColor(AStart: Integer = -1; ALength: Integer = -1): TAdvGraphicsColor;
    procedure SetUnderlineColor(AValue: TAdvGraphicsColor; AStart: Integer = -1; ALength: Integer = -1);
    function GetStrikethrough(AStart: Integer = -1; ALength: Integer = -1): TAdvPDFRichTextLibUnderlineStyles;
    procedure SetStrikethrough(AValue: TAdvPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
    function GetStrikethroughColor(AStart: Integer = -1; ALength: Integer = -1): TAdvGraphicsColor;
    procedure SetStrikethroughColor(AValue: TAdvGraphicsColor; AStart: Integer = -1; ALength: Integer = -1);
    function GetFont(AStart: Integer = -1; ALength: Integer = -1): TAdvPDFRichTextLibFontValue;
    procedure SetFont(AName: String; ASize: Single = -1; AStart: Integer = -1; ALength: Integer = -1);
    function GetToolTip(AStart: Integer = -1; ALength: Integer = -1): String;
    procedure SetToolTip(AValue: String; AStart: Integer = -1; ALength: Integer = -1);
    function GetFontSize(AStart: Integer = -1; ALength: Integer = -1): Single;
    procedure SetFontSize(ASize: Single; AStart: Integer = -1; ALength: Integer = -1);
    function GetBackgroundColor(AStart: Integer = -1; ALength: Integer = -1): TAdvGraphicsColor;
    procedure SetBackgroundColor(AColor: TAdvGraphicsColor; AStart: Integer = -1; ALength: Integer = -1);
    function GetForegroundColor(AStart: Integer = -1; ALength: Integer = -1): TAdvGraphicsColor;
    procedure SetForegroundColor(AColor: TAdvGraphicsColor; AStart: Integer = -1; ALength: Integer = -1);
    function GetBold(AStart: Integer = -1; ALength: Integer = -1): Boolean;
    procedure SetBold(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);
    function GetItalic(AStart: Integer = -1; ALength: Integer = -1): Boolean;
    procedure SetItalic(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);

    procedure SetParagraphStyle(AValue: TAdvPDFRichTextLibParagraphStyle; AStart: Integer = -1; ALength: Integer = -1);
    function GetParagraphStyle(AStart: Integer = -1; ALength: Integer = -1): TAdvPDFRichTextLibParagraphStyle;
    function ExportToStream: TMemoryStream;
    procedure ImportFromStream(AStream: TMemoryStream);
    procedure ExportData(AFileName: String; ARange: TAdvPDFRichTextLibRange; AType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType); overload;
    procedure ExportData(AFileName: String; AType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType); overload;
    procedure ImportData(AFileName: String; AType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType);

    procedure Clear;

    procedure ReplaceText(AText: String; AStart: Integer = -1; ALength: Integer = -1);
    procedure DeleteText(AStart: Integer = -1; ALength: Integer = -1);
    function GetPlainTextRange(AStart: Integer = -1; ALength: Integer = -1): String; overload;
    function GetPlainText: String; overload;
    function GetTextLength: Integer;
    function GetRichTextRange(ADataType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType; AStart: Integer = -1; ALength: Integer = -1): String; overload;
    function GetRichText(ADataType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType): String; overload;
    procedure SetRichText(ARichText: String; ADataType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType);
    function GetDataText: String;
    procedure SetDataText(const Value: String);
    function GetText: String;
    procedure SetText(const Value: String);
    property DataText: String read GetDataText write SetDataText;
    property Text: String read GetText write SetText;
  end;

  IAdvPDFRichTextLibService = interface(IInterface)
  ['{6E174355-3C72-46C3-AC61-454E9711B9BD}']
    function CreatePDFRichTextLib: IAdvCustomPDFRichTextLib;
  end;

  { TAdvCustomPDFRichTextLib }

  TAdvCustomPDFRichTextLib = class(TPersistent)
  private
    FPDFRichTextLib: IAdvCustomPDFRichTextLib;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetPDFRichTextLib: IAdvCustomPDFRichTextLib;
  end;

  TAdvPDFRichTextLibList = TList<IAdvCustomPDFRichTextLib>;

  TAdvPDFRichTextLibFactoryService = class abstract(TInterfacedObject, IAdvPDFRichTextLibService)
  private
    FPDFRichTextLibs: TAdvPDFRichTextLibList;
  protected
    function DoCreatePDFRichTextLib: IAdvCustomPDFRichTextLib; virtual; abstract;
    function CreatePDFRichTextLib: IAdvCustomPDFRichTextLib;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAdvPDFRichTextLib = class(TAdvCustomPDFRichTextLib)
  end;

implementation

uses
{$IFDEF MACOS}
{$IFDEF IOS}
  AdvPDFRichTextLib.iOS,
{$ELSE}
  AdvPDFRichTextLib.Mac,
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  AdvPDFRichTextLib.Android,
{$ENDIF}
{$IFNDEF MACOS}
{$IFNDEF ANDROID}
  AdvPDFRichTextLib.General,
{$ENDIF}
{$ENDIF}
  SysUtils
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  ;

{ TAdvPDFRichTextLibFactoryService }

constructor TAdvPDFRichTextLibFactoryService.Create;
begin
  inherited Create;
  FPDFRichTextLibs := TAdvPDFRichTextLibList.Create;
end;

function TAdvPDFRichTextLibFactoryService.CreatePDFRichTextLib: IAdvCustomPDFRichTextLib;
begin
  Result := DoCreatePDFRichTextLib;
  FPDFRichTextLibs.Add(Result);
end;

destructor TAdvPDFRichTextLibFactoryService.Destroy;
begin
  FreeAndNil(FPDFRichTextLibs);
  inherited Destroy;
end;

{ TAdvCustomPDFRichTextLib }

constructor TAdvCustomPDFRichTextLib.Create;
var
  PDFRichTextLibService: IAdvPDFRichTextLibService;
begin
  if TAdvPDFPlatformServices.Current.SupportsPlatformService(IAdvPDFRichTextLibService, IInterface(PDFRichTextLibService)) then
    FPDFRichTextLib := PDFRichTextLibService.CreatePDFRichTextLib;
end;

destructor TAdvCustomPDFRichTextLib.Destroy;
begin
  if Assigned(FPDFRichTextLib) then
    FPDFRichTextLib := nil;

  inherited;
end;

function TAdvCustomPDFRichTextLib.GetPDFRichTextLib: IAdvCustomPDFRichTextLib;
begin
  Result := FPDFRichTextLib;
end;

{$IFDEF LCLLIB}
class operator TAdvPDFRichTextLibTabStop.=(z1, z2: TAdvPDFRichTextLibTabStop)b: boolean;
begin
  Result := z1 = z2;
end;
{$ENDIF}

initialization
  RegisterPDFRichTextLibService;

finalization
  UnRegisterPDFRichTextLibService;

end.
