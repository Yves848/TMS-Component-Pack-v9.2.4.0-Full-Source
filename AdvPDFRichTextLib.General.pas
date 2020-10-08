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

unit AdvPDFRichTextLib.General;

{$I TMSDEFS.INC}

interface

procedure RegisterPDFRichTextLibService;
procedure UnRegisterPDFRichTextLibService;

implementation

uses
  Classes, AdvPDFRichTextLib, SysUtils, AdvTypes
  ,AdvPDFCoreLibBase, AdvGraphicsTypes
  {$IFNDEF LCLLIB}
  ,Types
  {$ENDIF}
  ;

type
  TAdvGeneralPDFRichTextLibService = class;
  TAdvGeneralPDFRichTextLib = class;

  TAdvGeneralPDFRichTextLibService = class(TAdvPDFRichTextLibFactoryService)
  protected
    function DoCreatePDFRichTextLib: IAdvCustomPDFRichTextLib; override;
  end;

  TAdvGeneralPDFRichTextLib = class(TInterfacedObject, IAdvCustomPDFRichTextLib)
  private
    FText: String;
    procedure SetText(const Value: String);
    function GetText: String;
    function GetDataText: String;
    procedure SetDataText(const Value: String);
  protected
    procedure UpdateText;
    procedure ProcessAllAttributes(var {%H-}AValues: TAdvPDFRichTextLibAttributeValue; {%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1; {%H-}ARetrieve: Boolean = False);
    procedure ProcessAttribute({%H-}AAtributeName: TAdvPDFRichTextLibAttributeName; var {%H-}AValues: TAdvPDFRichTextLibAttributeValue; {%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1; {%H-}ARetrieve: Boolean = False);
    procedure SetCanvas({%H-}ACanvas: Pointer);
    function GetSelection: TAdvPDFRichTextLibRange;
    function Draw({%H-}Rect: TRectF; {%H-}Calculate: Boolean = False): TRectF; overload;
    function Draw({%H-}Rect: TRectF; {%H-}Columns: Integer; {%H-}Padding: Single = 5.0; {%H-}DetectOverflow: Boolean = False): Integer; overload;
    function Draw({%H-}Rects: TAdvPDFGraphicsLibRectArray; {%H-}Padding: Single = 5.0; {%H-}DetectOverflow: Boolean = False): Integer; overload;
    function GetValues(AStart: Integer = -1; ALength: Integer = -1): TAdvPDFRichTextLibAttributeValue;
    procedure InitializeValues(var AValues: TAdvPDFRichTextLibAttributeValue);
    function GetURL(AStart: Integer = -1; ALength: Integer = -1): String; virtual;
    procedure SetURL(AValue: String; AStart: Integer = -1; ALength: Integer = -1); virtual;
    procedure AddBitmap(AValue: TAdvBitmap; ALineHeight: Single = -1; ALocation: Integer = -1); virtual;
    procedure AddBitmapFromFile(AValue: String; ALineHeight: Single = -1; ALocation: Integer = -1); virtual;
    function GetSubscript(AStart: Integer = -1; ALength: Integer = -1): Single; virtual;
    procedure SetSubscript(AValue: Single = -2; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetBaselineOffset(AStart: Integer = -1; ALength: Integer = -1): Single; virtual;
    procedure SetBaselineOffset(AValue: Single; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetSuperscript(AStart: Integer = -1; ALength: Integer = -1): Single; virtual;
    procedure SetSuperscript(AValue: Single = 2; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetStrokeColor(AStart: Integer = -1; ALength: Integer = -1): TAdvGraphicsColor; virtual;
    procedure SetStrokeColor(AValue: TAdvGraphicsColor; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetStrokeWidth(AStart: Integer = -1; ALength: Integer = -1): Single; virtual;
    procedure SetStrokeWidth(AWidth: Single; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetUnderline(AStart: Integer = -1; ALength: Integer = -1): TAdvPDFRichTextLibUnderlineStyles; virtual;
    procedure SetUnderline(AValue: TAdvPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetUnderlineColor(AStart: Integer = -1; ALength: Integer = -1): TAdvGraphicsColor; virtual;
    procedure SetUnderlineColor(AValue: TAdvGraphicsColor; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetStrikethrough(AStart: Integer = -1; ALength: Integer = -1): TAdvPDFRichTextLibUnderlineStyles; virtual;
    procedure SetStrikethrough(AValue: TAdvPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetStrikethroughColor(AStart: Integer = -1; ALength: Integer = -1): TAdvGraphicsColor; virtual;
    procedure SetStrikethroughColor(AValue: TAdvGraphicsColor; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetFont(AStart: Integer = -1; ALength: Integer = -1): TAdvPDFRichTextLibFontValue; virtual;
    procedure SetFont(AName: String; ASize: Single = -1; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetToolTip(AStart: Integer = -1; ALength: Integer = -1): String; virtual;
    procedure SetToolTip(AValue: String; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetFontSize(AStart: Integer = -1; ALength: Integer = -1): Single; virtual;
    procedure SetFontSize(ASize: Single; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetBackgroundColor(AStart: Integer = -1; ALength: Integer = -1): TAdvGraphicsColor; virtual;
    procedure SetBackgroundColor(AColor: TAdvGraphicsColor; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetForegroundColor(AStart: Integer = -1; ALength: Integer = -1): TAdvGraphicsColor; virtual;
    procedure SetForegroundColor(AColor: TAdvGraphicsColor; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetBold(AStart: Integer = -1; ALength: Integer = -1): Boolean; virtual;
    procedure SetBold(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetItalic(AStart: Integer = -1; ALength: Integer = -1): Boolean; virtual;
    procedure SetItalic(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1); virtual;

    procedure SetParagraphStyle(AValue: TAdvPDFRichTextLibParagraphStyle; AStart: Integer = -1; ALength: Integer = -1); virtual;
    function GetParagraphStyle(AStart: Integer = -1; ALength: Integer = -1): TAdvPDFRichTextLibParagraphStyle; virtual;
    function ExportToStream: TMemoryStream;
    procedure ImportFromStream({%H-}AStream: TMemoryStream);
    procedure ExportData({%H-}AFileName: String; {%H-}ARange: TAdvPDFRichTextLibRange; {%H-}AType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType); overload;
    procedure ExportData({%H-}AFileName: String; {%H-}AType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType); overload;
    procedure ImportData({%H-}AFileName: String; {%H-}AType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType);

    procedure Clear;

    procedure ReplaceText({%H-}AText: String; {%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1);
    procedure DeleteText({%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1);
    function GetPlainTextRange({%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1): String; overload;
    function GetPlainText: String; overload;
    function GetTextLength: Integer;
    function GetRichTextRange({%H-}ADataType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType; {%H-}AStart: Integer = -1; {%H-}ALength: Integer = -1): String; overload;
    function GetRichText(ADataType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType): String; overload;
    procedure SetRichText({%H-}ARichText: String; {%H-}ADataType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType);
    property DataText: String read GetDataText write SetDataText;
    property Text: String read GetText write SetText;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  PDFRichTextLibService: IAdvPDFRichTextLibService;

procedure RegisterPDFRichTextLibService;
begin
  if not TAdvPDFPlatformServices.Current.SupportsPlatformService(IAdvPDFRichTextLibService, IInterface(PDFRichTextLibService)) then
  begin
    PDFRichTextLibService := TAdvGeneralPDFRichTextLibService.Create;
    TAdvPDFPlatformServices.Current.AddPlatformService(IAdvPDFRichTextLibService, PDFRichTextLibService);
  end;
end;

procedure UnregisterPDFRichTextLibService;
begin
  TAdvPDFPlatformServices.Current.RemovePlatformService(IAdvPDFRichTextLibService);
end;

{ TAdvGeneralPDFRichTextLibService }

function TAdvGeneralPDFRichTextLibService.DoCreatePDFRichTextLib: IAdvCustomPDFRichTextLib;
begin
  Result := TAdvGeneralPDFRichTextLib.Create;
end;

{ TAdvGeneralPDFRichTextLib }

procedure TAdvGeneralPDFRichTextLib.SetUnderline(AValue: TAdvPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.Underline := AValue;
  ProcessAttribute(anUnderlineStyleAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetUnderlineColor(AValue: TAdvGraphicsColor;
  AStart, ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.UnderlineColor := AValue;
  ProcessAttribute(anUnderlineColorAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetURL(AValue: String; AStart,
  ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.URL := AValue;
  ProcessAttribute(anLinkAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetBold(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.Bold := AValue;
  val.ApplyBold := True;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetCanvas(ACanvas: Pointer);
begin
end;

procedure TAdvGeneralPDFRichTextLib.SetDataText(const Value: String);
begin
  SetRichText(Value);
end;

procedure TAdvGeneralPDFRichTextLib.AddBitmapFromFile(AValue: String; ALineHeight: Single = -1; ALocation: Integer = -1);
var
  bmp: TAdvBitmap;
begin
  bmp := TAdvBitmap.Create;
  bmp.LoadFromFile(AValue);
  AddBitmap(bmp, ALineHeight, ALocation);
  bmp.Free;
end;

procedure TAdvGeneralPDFRichTextLib.Clear;
begin
  Text := '';
end;

procedure TAdvGeneralPDFRichTextLib.SetBackgroundColor(AColor: TAdvGraphicsColor; AStart, ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.BackgroundColor := AColor;
  ProcessAttribute(anBackgroundColorAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetBaselineOffset(AValue: Single; AStart,
  ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.AddBitmap(AValue: TAdvBitmap; ALineHeight: Single = -1; ALocation: Integer = -1);
var
  val: TAdvPDFRichTextLibAttributeValue;
  h: Single;
begin
  InitializeValues({%H-}val);
  val.Bitmap := AValue;
  h := -1;
  if ALineHeight = -1 then
  begin
    if Assigned(val.Bitmap) then
        h := val.Bitmap.Height;
  end
  else
    h := ALineHeight;

  ProcessAttribute(anAttachmentAttributeName, val, ALocation, -1);
  if h > -1 then
  begin
    val.ParagraphStyle.LineHeightMultiple := h;
    val.ParagraphStyle.MinimumLineHeight := h;
    val.ParagraphStyle.MaximumLineHeight := h;
    ProcessAttribute(anParagraphStyleAttributeName, val, ALocation, 1);
  end;
end;

procedure TAdvGeneralPDFRichTextLib.SetFont(AName: String; ASize: Single; AStart,
  ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.ApplyFontName := AName <> '';
  val.ApplyFontSize := ASize <> -1;
  val.FontSize := ASize;
  val.FontName := AName;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetFontSize(ASize: Single; AStart,
  ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.ApplyFontSize := ASize <> -1;
  val.FontSize := ASize;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetForegroundColor(AColor: TAdvGraphicsColor; AStart, ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.ForegroundColor := AColor;
  ProcessAttribute(anForegroundColorAttributeName, val, AStart, ALength);
end;

constructor TAdvGeneralPDFRichTextLib.Create;
begin
end;

procedure TAdvGeneralPDFRichTextLib.DeleteText(AStart, ALength: Integer);
begin
end;

destructor TAdvGeneralPDFRichTextLib.Destroy;
begin
  inherited;
end;

function TAdvGeneralPDFRichTextLib.Draw(Rect: TRectF; Columns: Integer; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
var
  arr: TAdvPDFGraphicsLibRectArray;
  I: Integer;
begin
  SetLength(arr, Columns);
  for I := 0 to Columns - 1 do
    arr[I] := RectF(Rect.Left + Rect.Width / Columns * I, Rect.Top, Rect.Left + Rect.Width / Columns * (I + 1), Rect.Bottom);

  Result := Draw(arr, Padding, DetectOverflow);
end;

function TAdvGeneralPDFRichTextLib.Draw(Rects: TAdvPDFGraphicsLibRectArray; Padding: Single = 5.0; DetectOverflow: Boolean = False): Integer;
begin
  Result := 0;
end;

function TAdvGeneralPDFRichTextLib.Draw(Rect: TRectF; Calculate: Boolean = False): TRectF;
begin
  Result := RectF(0, 0, 0, 0);
end;

procedure TAdvGeneralPDFRichTextLib.ExportData(AFileName: String; AType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType);
begin
end;

function TAdvGeneralPDFRichTextLib.ExportToStream: TMemoryStream;
begin
  Result := nil;
end;

procedure TAdvGeneralPDFRichTextLib.ExportData(AFileName: String; ARange: TAdvPDFRichTextLibRange; AType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType);
begin
end;

function TAdvGeneralPDFRichTextLib.GetBackgroundColor(AStart,
  ALength: Integer): TAdvGraphicsColor;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anBackgroundColorAttributeName, val, AStart, ALength, True);
  Result := val.BackgroundColor;
end;

function TAdvGeneralPDFRichTextLib.GetBaselineOffset(AStart,
  ALength: Integer): Single;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TAdvGeneralPDFRichTextLib.GetBold(AStart, ALength: Integer): Boolean;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.Bold;
end;

function TAdvGeneralPDFRichTextLib.GetDataText: String;
begin
  Result := GetRichText;
end;

function TAdvGeneralPDFRichTextLib.GetFont(AStart,
  ALength: Integer): TAdvPDFRichTextLibFontValue;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result.FontFamily := val.FontFamily;
  Result.FontName := val.FontName;
  Result.FontSize := val.FontSize
end;

function TAdvGeneralPDFRichTextLib.GetFontSize(AStart,
  ALength: Integer): Single;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.FontSize;
end;

function TAdvGeneralPDFRichTextLib.GetForegroundColor(AStart,
  ALength: Integer): TAdvGraphicsColor;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anForegroundColorAttributeName, val, AStart, ALength, True);
  Result := val.ForegroundColor;
end;

function TAdvGeneralPDFRichTextLib.GetRichText(ADataType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType): String;
begin
  Result := GetRichTextRange(ADataType, 0, GetTextLength);
end;

function TAdvGeneralPDFRichTextLib.GetRichTextRange(ADataType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType; AStart: Integer = -1; ALength: Integer = -1): String;
begin
  Result := '';
end;

function TAdvGeneralPDFRichTextLib.GetItalic(AStart,
  ALength: Integer): Boolean;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anFontAttributeName, val, AStart, ALength, True);
  Result := val.Italic;
end;

function TAdvGeneralPDFRichTextLib.GetSelection: TAdvPDFRichTextLibRange;
begin
  Result.location := 0;
  Result.length := 0;
end;

function TAdvGeneralPDFRichTextLib.GetStrikethrough(AStart,
  ALength: Integer): TAdvPDFRichTextLibUnderlineStyles;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anStrikethroughStyleAttributeName, val, AStart, ALength, True);
  Result := val.Strikethrough;
end;

function TAdvGeneralPDFRichTextLib.GetStrikethroughColor(AStart,
  ALength: Integer): TAdvGraphicsColor;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anStrikethroughColorAttributeName, val, AStart, ALength, True);
  Result := val.StrikethroughColor;
end;

function TAdvGeneralPDFRichTextLib.GetStrokeColor(AStart,
  ALength: Integer): TAdvGraphicsColor;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anStrokeColorAttributeName, val, AStart, ALength, True);
  Result := val.StrokeColor;
end;

function TAdvGeneralPDFRichTextLib.GetStrokeWidth(AStart,
  ALength: Integer): Single;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anStrokeWidthAttributeName, val, AStart, ALength, True);
  Result := val.StrokeWidth;
end;

function TAdvGeneralPDFRichTextLib.GetSubscript(AStart,
  ALength: Integer): Single;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TAdvGeneralPDFRichTextLib.GetSuperscript(AStart,
  ALength: Integer): Single;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength, True);
  Result := val.BaselineOffset;
end;

function TAdvGeneralPDFRichTextLib.GetText: String;
begin
  Result := FText;
end;

function TAdvGeneralPDFRichTextLib.GetTextLength: Integer;
begin
  Result := 0;
end;

function TAdvGeneralPDFRichTextLib.GetToolTip(AStart,
  ALength: Integer): String;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anToolTipAttributeName, val, AStart, ALength, True);
  Result := val.ToolTip;
end;

function TAdvGeneralPDFRichTextLib.GetUnderline(AStart,
  ALength: Integer): TAdvPDFRichTextLibUnderlineStyles;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anUnderlineStyleAttributeName, val, AStart, ALength, True);
  Result := val.Underline;
end;

function TAdvGeneralPDFRichTextLib.GetUnderlineColor(AStart,
  ALength: Integer): TAdvGraphicsColor;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anUnderlineColorAttributeName, val, AStart, ALength, True);
  Result := val.UnderlineColor;
end;

function TAdvGeneralPDFRichTextLib.GetURL(AStart, ALength: Integer): String;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anLinkAttributeName, val, AStart, ALength, True);
  Result := val.URL;
end;

{$HINTS OFF}
function TAdvGeneralPDFRichTextLib.GetValues(AStart,
  ALength: Integer): TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues(Result);
  ProcessAllAttributes(Result, AStart, ALength, True);
end;
{$HINTS ON}

procedure TAdvGeneralPDFRichTextLib.ImportData(AFileName: String; AType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType);
begin
end;

procedure TAdvGeneralPDFRichTextLib.ImportFromStream(AStream: TMemoryStream);
begin
end;

procedure TAdvGeneralPDFRichTextLib.InitializeValues(
  var AValues: TAdvPDFRichTextLibAttributeValue);
begin
  AValues.Bitmap := nil;
  AValues.ForegroundColor := 0;
  AValues.BackgroundColor := 0;
  AValues.StrokeColor := 0;
  AValues.UnderlineColor := 0;
  AValues.BaselineOffset := 0;
  AValues.URL := '';
  AValues.BaselineOffset := 0;
  AValues.StrikethroughColor := 0;
  AValues.Strikethrough := [];
  AValues.StrokeWidth := 0;
  AValues.Underline := [];
  AValues.ApplyBold := False;
  AValues.ApplyItalic := False;
  AValues.ApplyFontName := False;
  AValues.ApplyFontSize := False;
  AValues.FontSize := 12;
  AValues.FontName := 'Roboto';
  AValues.Italic := False;
  AValues.Bold := False;

  AValues.ParagraphStyle.Alignment := gtaLeading;
  AValues.ParagraphStyle.FirstLineHeadIndent := 0;
  AValues.ParagraphStyle.HeadIndent := 0;
  AValues.ParagraphStyle.TailIndent := 0;
  AValues.ParagraphStyle.LineHeightMultiple := 0;
  AValues.ParagraphStyle.MaximumLineHeight := 0;
  AValues.ParagraphStyle.MinimumLineHeight := 0;
  AValues.ParagraphStyle.LineSpacing := 0;
  AValues.ParagraphStyle.ParagraphSpacing := 0;
  AValues.ParagraphStyle.ParagraphSpacingBefore := 0;
  AValues.ParagraphStyle.TabStops := nil;
  AValues.ParagraphStyle.LineBreakMode := bmLineBreakModeWordWrap;
  AValues.ParagraphStyle.HyphenationFactor := 0;
end;

procedure TAdvGeneralPDFRichTextLib.SetItalic(AValue: Boolean = True; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.Italic := AValue;
  val.ApplyItalic := True;
  ProcessAttribute(anFontAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetParagraphStyle(
  AValue: TAdvPDFRichTextLibParagraphStyle; AStart, ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.ParagraphStyle := AValue;
  ProcessAttribute(anParagraphStyleAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetRichText(ARichText: String;
  ADataType: TAdvPDFRichTextLibDataType = dtArchivedXMLDocumentType);
begin
end;

procedure TAdvGeneralPDFRichTextLib.ProcessAllAttributes(var AValues: TAdvPDFRichTextLibAttributeValue; AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
begin
end;

procedure TAdvGeneralPDFRichTextLib.ProcessAttribute(
  AAtributeName: TAdvPDFRichTextLibAttributeName;
  var AValues: TAdvPDFRichTextLibAttributeValue;
  AStart: Integer = -1; ALength: Integer = -1; ARetrieve: Boolean = False);
begin
end;

procedure TAdvGeneralPDFRichTextLib.ReplaceText(AText: String; AStart: Integer = -1; ALength: Integer = -1);
begin
end;

function TAdvGeneralPDFRichTextLib.GetParagraphStyle(AStart,
  ALength: Integer): TAdvPDFRichTextLibParagraphStyle;
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  ProcessAttribute(anParagraphStyleAttributeName, val, AStart, ALength, True);
  Result := val.ParagraphStyle;
end;

function TAdvGeneralPDFRichTextLib.GetPlainText: String;
begin
  Result := GetPlainTextRange(0, GetTextLength)
end;

function TAdvGeneralPDFRichTextLib.GetPlainTextRange(AStart: Integer = -1; ALength: Integer = -1): String;
begin
  Result := '';
end;

procedure TAdvGeneralPDFRichTextLib.SetStrikethrough(AValue: TAdvPDFRichTextLibUnderlineStyles = [usUnderlineStyleSingle]; AStart: Integer = -1; ALength: Integer = -1);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.Strikethrough := AValue;
  ProcessAttribute(anStrikethroughStyleAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetStrikethroughColor(AValue: TAdvGraphicsColor;
  AStart, ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.StrikethroughColor := AValue;
  ProcessAttribute(anStrikethroughColorAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetStrokeColor(AValue: TAdvGraphicsColor;
  AStart, ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.StrokeColor := AValue;
  ProcessAttribute(anStrokeColorAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetStrokeWidth(AWidth: Single; AStart,
  ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.StrokeWidth := AWidth;
  ProcessAttribute(anStrokeWidthAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetSubscript(AValue: Single; AStart,
  ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetSuperscript(AValue: Single; AStart,
  ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.BaselineOffset := AValue;
  ProcessAttribute(anBaselineOffsetAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.SetText(const Value: String);
begin
  FText := Value;
  UpdateText;
end;

procedure TAdvGeneralPDFRichTextLib.SetToolTip(AValue: String; AStart,
  ALength: Integer);
var
  val: TAdvPDFRichTextLibAttributeValue;
begin
  InitializeValues({%H-}val);
  val.ToolTip := AValue;
  ProcessAttribute(anToolTipAttributeName, val, AStart, ALength);
end;

procedure TAdvGeneralPDFRichTextLib.UpdateText;
begin
end;

end.

