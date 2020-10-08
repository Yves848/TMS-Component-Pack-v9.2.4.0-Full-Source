{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2016	                                            }
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

unit AdvPDFIO;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvCustomComponent, AdvPDFLib, AdvPDFCoreLibBase, AdvTypes,
  AdvPDFGraphicsLib, AdvGraphicsTypes
  {$IFNDEF LCLLIB}
  , Types
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release

type
  TAdvCustomPDFIO = class;

  TAdvPDFIOInformation = class(TPersistent)
  private
    FAuthor: String;
    FCreator: String;
    FSubject: String;
    FTitle: String;
    FKeywords: TStringList;
    procedure SetKeywords(const Value: TStringList);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Author: String read FAuthor write FAuthor;
    property Title: String read FTitle write FTitle;
    property Subject: String read FSubject write FSubject;
    property Keywords: TStringList read FKeywords write SetKeywords;
    property Creator: String read FCreator write FCreator;
  end;

  TAdvPDFIOOptions = class(TPersistent)
  private
    FOpenInPDFReader: Boolean;
    FDefaultFont: TAdvPDFGraphicsLibFont;
    FFooter: UnicodeString;
    FHeader: UnicodeString;
    FMargins: TAdvMargins;
    FHeaderFont: TAdvPDFGraphicsLibFont;
    FFooterFont: TAdvPDFGraphicsLibFont;
    FHeaderSize: Single;
    FFooterMargins: TAdvMargins;
    FFooterAlignment: TAdvGraphicsTextAlign;
    FFooterSize: Single;
    FHeaderMargins: TAdvMargins;
    FHeaderAlignment: TAdvGraphicsTextAlign;
    FPageSize: TAdvPDFLibPageSize;
    FPageOrientation: TAdvPDFLibPageOrientation;
    FPageWidth: Single;
    FPageHeight: Single;
    FExportImages: Boolean;
    FFontFallBackList: TStrings;
    FEmbedFonts: Boolean;
    FPageNumberFormat: UnicodeString;
    FPageNumberAlignment: TAdvGraphicsTextAlign;
    FPageNumber: TAdvPDFLibPageNumber;
    FPageNumberFont: TAdvPDFGraphicsLibFont;
    FPageNumberMargins: TAdvMargins;
    FPageNumberSize: Single;
    procedure SetDefaultFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetMargins(const Value: TAdvMargins);
    procedure SetFooterFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetHeaderFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetFooterAlignment(
      const Value: TAdvGraphicsTextAlign);
    procedure SetFooterMargins(const Value: TAdvMargins);
    procedure SetFooterSize(const Value: Single);
    procedure SetHeaderAlignment(
      const Value: TAdvGraphicsTextAlign);
    procedure SetHeaderMargins(const Value: TAdvMargins);
    procedure SetHeaderSize(const Value: Single);
    function IsFooterSizeStored: Boolean;
    function IsHeaderSizeStored: Boolean;
    procedure SetPageOrientation(const Value: TAdvPDFLibPageOrientation);
    procedure SetPageSize(const Value: TAdvPDFLibPageSize);
    function IsPageHeightStored: Boolean;
    function IsPageWidthStored: Boolean;
    procedure SetPageHeight(const Value: Single);
    procedure SetPageWidth(const Value: Single);
    procedure SetExportImages(const Value: Boolean);
    procedure SetEmbedFonts(const Value: Boolean);
    procedure SetFontFallBackList(const Value: TStrings);
    procedure SetPageNumber(const Value: TAdvPDFLibPageNumber);
    procedure SetPageNumberAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetPageNumberFont(const Value: TAdvPDFGraphicsLibFont);
    procedure SetPageNumberFormat(const Value: UnicodeString);
    procedure SetPageNumberMargins(const Value: TAdvMargins);
    function IsPageNumberSizeStored: Boolean;
    procedure SetPageNumberSize(const Value: Single);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property OpenInPDFReader: Boolean read FOpenInPDFReader write FOpenInPDFReader default False;
    property DefaultFont: TAdvPDFGraphicsLibFont read FDefaultFont write SetDefaultFont;
    property Header: UnicodeString read FHeader write FHeader;
    property Footer: UnicodeString read FFooter write FFooter;
    property Margins: TAdvMargins read FMargins write SetMargins;
    property HeaderFont: TAdvPDFGraphicsLibFont read FHeaderFont write SetHeaderFont;
    property FooterFont: TAdvPDFGraphicsLibFont read FFooterFont write SetFooterFont;
    property HeaderSize: Single read FHeaderSize write SetHeaderSize stored IsHeaderSizeStored nodefault;
    property HeaderMargins: TAdvMargins read FHeaderMargins write SetHeaderMargins;
    property HeaderAlignment: TAdvGraphicsTextAlign read FHeaderAlignment write SetHeaderAlignment default gtaCenter;
    property FooterSize: Single read FFooterSize write SetFooterSize stored IsFooterSizeStored nodefault;
    property FooterMargins: TAdvMargins read FFooterMargins write SetFooterMargins;
    property FooterAlignment: TAdvGraphicsTextAlign read FFooterAlignment write SetFooterAlignment default gtaCenter;
    property PageWidth: Single read FPageWidth write SetPageWidth stored IsPageWidthStored nodefault;
    property PageHeight: Single read FPageHeight write SetPageHeight stored IsPageHeightStored nodefault;
    property PageSize: TAdvPDFLibPageSize read FPageSize write SetPageSize default psLetter;
    property PageOrientation: TAdvPDFLibPageOrientation read FPageOrientation write SetPageOrientation default poPortrait;
    property ExportImages: Boolean read FExportImages write SetExportImages default True;
    property FontFallBackList: TStrings read FFontFallBackList write SetFontFallBackList;
    property EmbedFonts: Boolean read FEmbedFonts write SetEmbedFonts default True;
    property PageNumber: TAdvPDFLibPageNumber read FPageNumber write SetPageNumber default pnHeader;
    property PageNumberMargins: TAdvMargins read FPageNumberMargins write SetPageNumberMargins;
    property PageNumberFormat: UnicodeString read FPageNumberFormat write SetPageNumberFormat;
    property PageNumberAlignment: TAdvGraphicsTextAlign read FPageNumberAlignment write SetPageNumberAlignment default gtaTrailing;
    property PageNumberFont: TAdvPDFGraphicsLibFont read FPageNumberFont write SetPageNumberFont;
    property PageNumberSize: Single read FPageNumberSize write SetPageNumberSize stored IsPageNumberSizeStored nodefault;
  end;

  TAdvPDFIOExportObject = TObject;

  TAdvPDFIOGetHeaderEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; var AHeader: UnicodeString) of object;
  TAdvPDFIOGetFooterEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; var AFooter: UnicodeString) of object;
  TAdvPDFIOBeforeDrawContentEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TAdvPDFIOAfterDrawContentEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib) of object;
  TAdvPDFIOBeforeDrawHeaderEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TAdvPDFIOBeforeDrawPageNumberEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TAdvPDFIOBeforeDrawFooterEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean) of object;
  TAdvPDFIOAfterDrawHeaderEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib) of object;
  TAdvPDFIOAfterDrawPageNumberEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib) of object;
  TAdvPDFIOAfterDrawFooterEvent = procedure(Sender: TObject; AExportObject: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib) of object;

  TAdvPDFIOExportObjectArray = array of TAdvPDFIOExportObject;

  TAdvCustomPDFIO = class(TAdvCustomComponent)
  private
    FNewPageStarted: Boolean;
    FInformation: TAdvPDFIOInformation;
    FOptions: TAdvPDFIOOptions;
    FOnGetHeader: TAdvPDFIOGetHeaderEvent;
    FOnGetFooter: TAdvPDFIOGetFooterEvent;
    FOnBeforeDrawHeader: TAdvPDFIOBeforeDrawHeaderEvent;
    FOnBeforeDrawPageNumber: TAdvPDFIOBeforeDrawPageNumberEvent;
    FOnBeforeDrawContent: TAdvPDFIOBeforeDrawContentEvent;
    FOnBeforeDrawFooter: TAdvPDFIOBeforeDrawFooterEvent;
    FOnAfterDrawHeader: TAdvPDFIOAfterDrawHeaderEvent;
    FOnAfterDrawPageNumber: TAdvPDFIOAfterDrawPageNumberEvent;
    FOnAfterDrawFooter: TAdvPDFIOAfterDrawFooterEvent;
    FOnAfterDrawContent: TAdvPDFIOAfterDrawContentEvent;
    FActiveExportObject: TAdvPDFIOExportObject;
    FExportObject: TAdvPDFIOExportObject;
    procedure SetInformation(const Value: TAdvPDFIOInformation);
    procedure SetOptions(const Value: TAdvPDFIOOptions);
  protected
    function GetDocURL: string; override;
    function GetInstance: NativeUInt; override;
    function CreateOptions: TAdvPDFIOOptions; virtual;
    function NewPage(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject): Boolean; virtual;
    function GetContentRect(const APDFLib: TAdvPDFLib): TRectF; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EndPage(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject); virtual;
    procedure DoNewPageStarted(Sender: TObject; {%H-}APageIndex: Integer); virtual;
    procedure DoGetHeader(AExportObject: TAdvPDFIOExportObject; APageIndex: Integer; var AHeader: UnicodeString); virtual;
    procedure DoGetFooter(AExportObject: TAdvPDFIOExportObject; APageIndex: Integer; var AFooter: UnicodeString); virtual;
    procedure DoPDFExport(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject); virtual; abstract;
    procedure DoBeforeDrawHeader(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawPageNumber(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawFooter(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawHeader(Sender: TObject; APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib); virtual;
    procedure DoAfterDrawPageNumber(Sender: TObject; APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib); virtual;
    procedure DoAfterDrawFooter(Sender: TObject; APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib); virtual;
    procedure DoBeforeDrawContent(AExportObject: TAdvPDFIOExportObject; APageIndex: Integer; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawContent(AExportObject: TAdvPDFIOExportObject; APageIndex: Integer; ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib); virtual;
    property Information: TAdvPDFIOInformation read FInformation write SetInformation;
    property Options: TAdvPDFIOOptions read FOptions write SetOptions;
    property OnGetHeader: TAdvPDFIOGetHeaderEvent read FOnGetHeader write FOnGetHeader;
    property OnGetFooter: TAdvPDFIOGetFooterEvent read FOnGetFooter write FOnGetFooter;
    property OnBeforeDrawHeader: TAdvPDFIOBeforeDrawHeaderEvent read FOnBeforeDrawHeader write FOnBeforeDrawHeader;
    property OnBeforeDrawPageNumber: TAdvPDFIOBeforeDrawPageNumberEvent read FOnBeforeDrawPageNumber write FOnBeforeDrawPageNumber;
    property OnAfterDrawHeader: TAdvPDFIOAfterDrawHeaderEvent read FOnAfterDrawHeader write FOnAfterDrawHeader;
    property OnAfterDrawPageNumber: TAdvPDFIOAfterDrawPageNumberEvent read FOnAfterDrawPageNumber write FOnAfterDrawPageNumber;
    property OnBeforeDrawFooter: TAdvPDFIOBeforeDrawFooterEvent read FOnBeforeDrawFooter write FOnBeforeDrawFooter;
    property OnAfterDrawFooter: TAdvPDFIOAfterDrawFooterEvent read FOnAfterDrawFooter write FOnAfterDrawFooter;
    property OnBeforeDrawContent: TAdvPDFIOBeforeDrawContentEvent read FOnBeforeDrawContent write FOnBeforeDrawContent;
    property OnAfterDrawContent: TAdvPDFIOAfterDrawContentEvent read FOnAfterDrawContent write FOnAfterDrawContent;
    property ExportObject: TAdvPDFIOExportObject read FExportObject write FExportObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Save(const AFileName: String); overload;
    procedure Save(const AStream: TStream); overload;
    procedure Save(const AFileName: String; AExportObjects: TAdvPDFIOExportObjectArray); overload;
    procedure Save(const AStream: TStream; AExportObjects: TAdvPDFIOExportObjectArray); overload;
  end;

implementation

uses
  SysUtils, AdvUtils;

{$IFNDEF FNCLIB}
{$R 'AdvPDFIO.res'}
{$ENDIF}

{ TAdvCustomPDFIO }

procedure TAdvCustomPDFIO.Save(const AFileName: String; AExportObjects: TAdvPDFIOExportObjectArray);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    Save(ms, AExportObjects);
    ms.SaveToFile(AFileName);
    if Options.OpenInPDFReader then
      TAdvUtils.OpenFile(AFileName);
  finally
    ms.Free;
  end;
end;

procedure TAdvCustomPDFIO.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomPDFIO then
  begin
    FInformation.Assign((Source as TAdvCustomPDFIO).Information);
    FOptions.Assign((Source as TAdvCustomPDFIO).Options);
  end;
end;

constructor TAdvCustomPDFIO.Create(AOwner: TComponent);
begin
  inherited;
  FInformation := TAdvPDFIOInformation.Create;
  FOptions := CreateOptions;
  if IsDesignTime then
  begin
    FOptions.Header := 'TMS PDF Header';
    FOptions.Footer := 'TMS PDF Footer';
    FOptions.PageNumberFormat := '%d';
  end;
end;

function TAdvCustomPDFIO.CreateOptions: TAdvPDFIOOptions;
begin
  Result := TAdvPDFIOOptions.Create;
end;

destructor TAdvCustomPDFIO.Destroy;
begin
  FOptions.Free;
  FInformation.Free;
  inherited;
end;

procedure TAdvCustomPDFIO.DoAfterDrawContent(AExportObject: TAdvPDFIOExportObject; APageIndex: Integer;
  ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawContent) then
    OnAfterDrawContent(Self, AExportObject, APageIndex, ARect, AGraphics);
end;

procedure TAdvCustomPDFIO.DoAfterDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawFooter) then
    OnAfterDrawFooter(Self, FActiveExportObject, APageIndex, AFooter, ARect, AGraphics);
end;

procedure TAdvCustomPDFIO.DoAfterDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawHeader) then
    OnAfterDrawHeader(Self, FActiveExportObject, APageIndex, AHeader, ARect, AGraphics);
end;

procedure TAdvCustomPDFIO.DoAfterDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterDrawPageNumber) then
    OnAfterDrawPageNumber(Self, FActiveExportObject, APageIndex, APageNumber, ARect, AGraphics);
end;

procedure TAdvCustomPDFIO.DoBeforeDrawContent(AExportObject: TAdvPDFIOExportObject; APageIndex: Integer;
  ARect: TRectF; AGraphics: IAdvCustomPDFGraphicsLib;
  var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawContent) then
    OnBeforeDrawContent(Self, AExportObject, APageIndex, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvCustomPDFIO.DoBeforeDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawFooter) then
    OnBeforeDrawFooter(Self, FActiveExportObject, APageIndex, AFooter, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvCustomPDFIO.DoBeforeDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawHeader) then
    OnBeforeDrawHeader(Self, FActiveExportObject, APageIndex, AHeader, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvCustomPDFIO.DoBeforeDrawPageNumber(Sender: TObject;
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: IAdvCustomPDFGraphicsLib; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawPageNumber) then
    OnBeforeDrawPageNumber(Self, FActiveExportObject, APageIndex, APageNumber, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvCustomPDFIO.DoGetFooter(AExportObject: TAdvPDFIOExportObject; APageIndex: Integer;
  var AFooter: UnicodeString);
begin
  if Assigned(OnGetFooter) then
    OnGetFooter(Self, AExportObject, APageIndex, AFooter);
end;

procedure TAdvCustomPDFIO.DoGetHeader(AExportObject: TAdvPDFIOExportObject; APageIndex: Integer;
  var AHeader: UnicodeString);
begin
  if Assigned(OnGetHeader) then
    OnGetHeader(Self, AExportObject, APageIndex, AHeader);
end;

procedure TAdvCustomPDFIO.DoNewPageStarted(Sender: TObject; APageIndex: Integer);
var
  h, f: UnicodeString;
  pdflib: IAdvCustomPDFLib;
begin
  if Assigned(Sender) and Supports(Sender, IAdvCustomPDFLib, pdflib) then
  begin
    h := Options.Header;
    f := Options.Footer;
    DoGetHeader(FActiveExportObject, pdflib.GetPageIndex, h);
    DoGetFooter(FActiveExportObject, pdflib.GetPageIndex, f);
    pdflib.Header := h;
    pdflib.Footer := f;
  end;
end;

procedure TAdvCustomPDFIO.EndPage(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject);
var
  r: TRectF;
begin
  if FNewPageStarted then
  begin
    FNewPageStarted := False;
    r := GetContentRect(APDFLib);
    DoAfterDrawContent(AExportObject, APDFLib.GetPageIndex, r, APDFLib.Graphics);
  end;
end;

function TAdvCustomPDFIO.GetContentRect(const APDFLib: TAdvPDFLib): TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if Assigned(APDFLib) then
  begin
    Result := APDFLib.MediaBox;
    Result := RectF(Result.Left + Options.Margins.Left, Result.Top + Options.Margins.Top,
      Result.Right - Options.Margins.Right, Result.Bottom - Options.Margins.Bottom);
  end;
end;

function TAdvCustomPDFIO.GetDocURL: string;
begin
  Result := TAdvPDFLibDocURL;
end;

function TAdvCustomPDFIO.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TAdvCustomPDFIO.NewPage(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject): Boolean;
var
  r: TRectF;
begin
  Result := False;
  if Assigned(APDFLib) then
  begin
    EndPage(APDFLib, AExportObject);
    APDFLib.Graphics.Font.Assign(Options.DefaultFont);
    APDFLib.NewPage;
    Result := True;
    r := GetContentRect(APDFLib);
    DoBeforeDrawContent(AExportObject, APDFLib.GetPageIndex, r, APDFLib.Graphics, Result);
    FNewPageStarted := Result;
  end;
end;

procedure TAdvCustomPDFIO.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FExportObject then
      FExportObject := nil;
  end;
end;

procedure TAdvCustomPDFIO.Save(const AStream: TStream; AExportObjects: TAdvPDFIOExportObjectArray);
var
  pdflib: TAdvPDFLib;
  I: Integer;
  ms: TMemoryStream;
begin
  pdflib := TAdvPDFLib.Create;
  try
    pdflib.EmbedFonts := Options.EmbedFonts;
    pdflib.OnNewPageStarted := DoNewPageStarted;
    pdflib.OnBeforeDrawHeader := DoBeforeDrawHeader;
    pdflib.OnAfterDrawHeader := DoAfterDrawHeader;
    pdflib.OnBeforeDrawPageNumber := DoBeforeDrawPageNumber;
    pdflib.OnAfterDrawPageNumber := DoAfterDrawPageNumber;
    pdflib.OnBeforeDrawFooter := DoBeforeDrawFooter;
    pdflib.OnAfterDrawFooter := DoAfterDrawFooter;
    pdflib.Header := Options.Header;
    pdflib.PageNumber := Options.PageNumber;
    pdflib.PageNumberFormat := Options.PageNumberFormat;
    pdflib.Footer := Options.Footer;
    pdflib.Author := Information.Author;
    pdflib.Title := Information.Title;
    pdflib.Subject := Information.Subject;
    for I := 0 to Options.FontFallBackList.Count - 1 do
      pdflib.FontFallBackList.Add(Options.FontFallBackList[I]);
    pdflib.Keywords.Assign(Information.Keywords);
    pdflib.Creator := Information.Creator;
    pdflib.PageWidth := Options.PageWidth;
    pdflib.PageHeight := Options.PageHeight;
    pdflib.PageSize := Options.PageSize;
    pdflib.PageOrientation := Options.PageOrientation;
    pdflib.PageNumberFont.Assign(Options.PageNumberFont);
    pdflib.HeaderFont.Assign(Options.HeaderFont);
    pdflib.FooterFont.Assign(Options.FooterFont);
    pdflib.FooterMargins.Assign(Options.FooterMargins);
    pdflib.PageNumberMargins.Assign(Options.PageNumberMargins);
    pdflib.HeaderMargins.Assign(Options.HeaderMargins);
    pdflib.PageNumberSize := Options.PageNumberSize;
    pdflib.HeaderSize := Options.HeaderSize;
    pdflib.FooterSize := Options.FooterSize;
    pdflib.HeaderAlignment := Options.HeaderAlignment;
    pdflib.PageNumberAlignment := Options.PageNumberAlignment;
    pdflib.FooterAlignment := Options.FooterAlignment;
    pdflib.Graphics.LineBreakMode := bmLineBreakModeClip;

    pdflib.BeginDocument;

    if Length(AExportObjects) = 0 then
    begin
      SetLength(AExportObjects, 1);
      AExportObjects[0] := ExportObject;
    end;

    for I := 0 to Length(AExportObjects) - 1 do
    begin
      FActiveExportObject := AExportObjects[I];
      DoPDFExport(pdflib, AExportObjects[I]);
      EndPage(pdflib, AExportObjects[I]);
    end;

    ms := pdflib.EndDocument;
    if Assigned(ms) then
    begin
      ms.SaveToStream(AStream);
      ms.Free;
    end;
  finally
    pdflib.Free;
  end;
end;

procedure TAdvCustomPDFIO.Save(const AFileName: String);
var
  l: TAdvPDFIOExportObjectArray;
begin
  SetLength(l, 0);
  Save(AFileName, l);
end;

procedure TAdvCustomPDFIO.Save(const AStream: TStream);
var
  l: TAdvPDFIOExportObjectArray;
begin
  SetLength(l, 0);
  Save(AStream, l);
end;

procedure TAdvCustomPDFIO.SetInformation(
  const Value: TAdvPDFIOInformation);
begin
  FInformation.Assign(Value);
end;

procedure TAdvCustomPDFIO.SetOptions(const Value: TAdvPDFIOOptions);
begin
  FOptions.Assign(Value);
end;

{ TAdvPDFIOInformation }

procedure TAdvPDFIOInformation.Assign(Source: TPersistent);
begin
  if Source is TAdvPDFIOInformation then
  begin
    FAuthor := (Source as TAdvPDFIOInformation).Author;
    FCreator := (Source as TAdvPDFIOInformation).Creator;
    FSubject := (Source as TAdvPDFIOInformation).Subject;
    FTitle := (Source as TAdvPDFIOInformation).Title;
    FKeywords.Assign((Source as TAdvPDFIOInformation).Keywords);
  end;
end;

constructor TAdvPDFIOInformation.Create;
begin
  FKeywords := TStringList.Create;
end;

destructor TAdvPDFIOInformation.Destroy;
begin
  FKeywords.Free;
  inherited;
end;

procedure TAdvPDFIOInformation.SetKeywords(const Value: TStringList);
begin
  FKeywords.Assign(Value);
end;

{ TAdvPDFIOOptions }

procedure TAdvPDFIOOptions.Assign(Source: TPersistent);
begin
  if Source is TAdvPDFIOOptions then
  begin
    FMargins.Assign((Source as TAdvPDFIOOptions).Margins);
    FOpenInPDFReader := (Source as TAdvPDFIOOptions).OpenInPDFReader;
    FDefaultFont.Assign((Source as TAdvPDFIOOptions).DefaultFont);
    FFooter := (Source as TAdvPDFIOOptions).Footer;
    FHeader := (Source as TAdvPDFIOOptions).Header;
    FHeaderFont.Assign((Source as TAdvPDFIOOptions).HeaderFont);
    FFooterFont.Assign((Source as TAdvPDFIOOptions).FooterFont);
    FHeaderMargins.Assign((Source as TAdvPDFIOOptions).HeaderMargins);
    FFooterMargins.Assign((Source as TAdvPDFIOOptions).FooterMargins);
    FFooterSize := (Source as TAdvPDFIOOptions).FooterSize;
    FHeaderSize := (Source as TAdvPDFIOOptions).HeaderSize;
    FFooterAlignment := (Source as TAdvPDFIOOptions).FooterAlignment;
    FHeaderAlignment := (Source as TAdvPDFIOOptions).HeaderAlignment;
    FEmbedFonts := (Source as TAdvPDFIOOptions).EmbedFonts;
    FFontFallBackList.Assign((Source as TAdvPDFIOOptions).FontFallBackList);
    FPageNumberFormat := (Source as TAdvPDFIOOptions).PageNumberFormat;
    FPageNumber := (Source as TAdvPDFIOOptions).PageNumber;
    FPageNumberFont.Assign((Source as TAdvPDFIOOptions).PageNumberFont);
    FPageNumberAlignment := (Source as TAdvPDFIOOptions).PageNumberAlignment;
    FPageNumberSize := (Source as TAdvPDFIOOptions).PageNumberSize;
    FPageNumberMargins.Assign((Source as TAdvPDFIOOptions).PageNumberMargins);
  end;
end;

constructor TAdvPDFIOOptions.Create;
var
  r: TRectF;
begin
  r := RectF(5, 5, 5, 5);
  FEmbedFonts := True;
  FFontFallBackList := TStringList.Create;
  FOpenInPDFReader := False;
  FMargins := TAdvMargins.Create(RectF(20, 50, 20, 50));
  FDefaultFont := TAdvPDFGraphicsLibFont.Create;
  FExportImages := True;
  FHeaderFont := TAdvPDFGraphicsLibFont.Create;
  FFooterFont :=  TAdvPDFGraphicsLibFont.Create;
  FHeaderSize := 30;
  FFooterSize := 30;
  FPageNumberSize := 30;
  FHeaderMargins := TAdvMargins.Create(r);
  FFooterMargins := TAdvMargins.Create(r);
  FFooterAlignment := gtaCenter;
  FHeaderAlignment := gtaCenter;
  FPageSize := psLetter;
  FPageOrientation := poPortrait;
  FPageWidth := 0;
  FPageHeight := 0;
  r := RectF(10, 5, 10, 5);
  FPageNumberMargins := TAdvMargins.Create(r);
  FPageNumber := pnHeader;
  FPageNumberFont := TAdvPDFGraphicsLibFont.Create;
  FPageNumberAlignment := gtaTrailing;
end;

destructor TAdvPDFIOOptions.Destroy;
begin
  FPageNumberFont.Free;
  FFontFallBackList.Free;
  FDefaultFont.Free;
  FMargins.Free;
  FFooterFont.Free;
  FHeaderFont.Free;
  FFooterMargins.Free;
  FHeaderMargins.Free;
  FPageNumberMargins.Free;
  inherited;
end;

function TAdvPDFIOOptions.IsFooterSizeStored: Boolean;
begin
  Result := FooterSize <> 30;
end;

function TAdvPDFIOOptions.IsHeaderSizeStored: Boolean;
begin
  Result := HeaderSize <> 30;
end;

function TAdvPDFIOOptions.IsPageHeightStored: Boolean;
begin
  Result := PageHeight <> 0;
end;

function TAdvPDFIOOptions.IsPageNumberSizeStored: Boolean;
begin
  Result := PageNumberSize <> 30;
end;

function TAdvPDFIOOptions.IsPageWidthStored: Boolean;
begin
  Result := PageWidth <> 0;
end;

procedure TAdvPDFIOOptions.SetDefaultFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  FDefaultFont.Assign(Value);
end;

procedure TAdvPDFIOOptions.SetEmbedFonts(const Value: Boolean);
begin
  FEmbedFonts := Value;
end;

procedure TAdvPDFIOOptions.SetExportImages(const Value: Boolean);
begin
  FExportImages := Value;
end;

procedure TAdvPDFIOOptions.SetFontFallBackList(const Value: TStrings);
begin
  FFontFallBackList.Assign(Value);
end;

procedure TAdvPDFIOOptions.SetFooterAlignment(
  const Value: TAdvGraphicsTextAlign);
begin
  FFooterAlignment := Value;
end;

procedure TAdvPDFIOOptions.SetFooterFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TAdvPDFIOOptions.SetFooterMargins(const Value: TAdvMargins);
begin
  FFooterMargins.Assign(Value);
end;

procedure TAdvPDFIOOptions.SetFooterSize(const Value: Single);
begin
  FFooterSize := Value;
end;

procedure TAdvPDFIOOptions.SetHeaderAlignment(
  const Value: TAdvGraphicsTextAlign);
begin
  FHeaderAlignment := Value;
end;

procedure TAdvPDFIOOptions.SetHeaderFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TAdvPDFIOOptions.SetHeaderMargins(const Value: TAdvMargins);
begin
  FHeaderMargins.Assign(Value);
end;

procedure TAdvPDFIOOptions.SetHeaderSize(const Value: Single);
begin
  FHeaderSize := Value;
end;

procedure TAdvPDFIOOptions.SetMargins(const Value: TAdvMargins);
begin
  FMargins.Assign(Value);
end;

procedure TAdvPDFIOOptions.SetPageHeight(const Value: Single);
begin
  FPageHeight := Value;
end;

procedure TAdvPDFIOOptions.SetPageNumber(
  const Value: TAdvPDFLibPageNumber);
begin
  FPageNumber := Value;
end;

procedure TAdvPDFIOOptions.SetPageNumberAlignment(
  const Value: TAdvGraphicsTextAlign);
begin
  FPageNumberAlignment := Value;
end;

procedure TAdvPDFIOOptions.SetPageNumberFont(
  const Value: TAdvPDFGraphicsLibFont);
begin
  FPageNumberFont.Assign(Value);
end;

procedure TAdvPDFIOOptions.SetPageNumberFormat(const Value: UnicodeString);
begin
  FPageNumberFormat := Value;
end;

procedure TAdvPDFIOOptions.SetPageNumberMargins(const Value: TAdvMargins);
begin
  FPageNumberMargins.Assign(Value);
end;

procedure TAdvPDFIOOptions.SetPageNumberSize(const Value: Single);
begin
  FPageNumberSize := Value;
end;

procedure TAdvPDFIOOptions.SetPageOrientation(
  const Value: TAdvPDFLibPageOrientation);
begin
  FPageOrientation := Value;
end;

procedure TAdvPDFIOOptions.SetPageSize(const Value: TAdvPDFLibPageSize);
begin
  FPageSize := Value;
end;

procedure TAdvPDFIOOptions.SetPageWidth(const Value: Single);
begin
  FPageWidth := Value;
end;

end.
