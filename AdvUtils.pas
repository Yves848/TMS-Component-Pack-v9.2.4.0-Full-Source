{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2016 - 2019                                 }
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

unit AdvUtils;

{$I TMSDEFS.INC}

{$IFDEF WIN64}
{$HPPEMIT ''}
{$HPPEMIT '#pragma link "wininet.a"'}
{$HPPEMIT ''}
{$ENDIF}

{$IFDEF WIN32}
{$HPPEMIT ''}
{$HPPEMIT '#pragma link "wininet.lib"'}
{$HPPEMIT '#pragma link "urlmon.lib"'}
{$HPPEMIT ''}
{$ENDIF}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, Types, Graphics, AdvTypes, Controls, Forms
  {$IFDEF FMXLIB}
  ,FMX.Types, VCL.Dialogs
  {$IFDEF MACOS}
  ,MacApi.ObjectiveC, MacApi.ObjcRuntime
  {$IFDEF IOS}
  ,iOSApi.Foundation
  {$ELSE}
  ,MacApi.Foundation
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  ,Rtti, Generics.Collections
  {$ENDIF}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,JPEG, PngImage, GifImg
  {$ENDIF}
  {$IFDEF CMNLIB}
  ,Dialogs
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,JS, WEBLib.Dialogs, WEBLib.JSON, web
  {$ENDIF}
  {$IFDEF FNCLIB}
  {$IFNDEF WEBLIB}
  {$IFNDEF LCLLIB}
  ,JSON
  {$ELSE}
  ,fpjson, jsparser
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  ;

const
  PthDel = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF}

type
  TAdvUtilsCharSet = array of char;

  {$IFDEF FNCLIB}
  {$IFDEF WEBLIB}
  TAdvUtilsFile = record
    Data: TJSUint8Array;
    Name: string;
  end;
  TAdvUtilsLoadFile = TJSBlob;
  {$ELSE}
  TAdvUtilsFile = string;
  TAdvUtilsLoadFile = string;
  {$ENDIF}

  TAdvUtilsLoadFileCompleteEvent = {$IFNDEF LCLWEBLIB}reference to {$ENDIF}procedure(const AFile: TAdvUtilsFile){$IFDEF LCLWEBLIB} of object{$ENDIF};
  {$ENDIF}

  TAdvUtilsFileCallBackEvent = {$IFNDEF LCLWEBLIB}reference to {$ENDIF}procedure(const AFile: string; const AResult: Boolean){$IFDEF LCLWEBLIB} of object{$ENDIF};

  TAdvUtils = class
  {$IFDEF FNCLIB}
  private
    class procedure GetTimeZoneInfo(var ABias: Integer; var ABiasDay: Integer; var AIsDaylight: Boolean);
  {$ENDIF}
  public
    class procedure OpenURL(AURL: string);
    class procedure OpenFile(AFile: string; {%H-}AControlReference: TControl = nil);
    class procedure GetFonts(FontList: TStringList);
    class procedure SetFontSize(AFont: TFont; ASize: Single);
    class function GetHInstance: NativeUInt;
    class function IsHTMLUnicode(AValue: UnicodeString): Boolean;
    class function IsHTML(AValue: String): Boolean;
    class function GetDocumentsPath: String;
    class function GetAppPath: String;
    class function GetMousePos: TPointF;
    class function IsHighDPIScale: Boolean;
    class function GetDPIScale({%H-}AOwner: TComponent = nil): Single;
    class function AddBackslash(const AValue: string): string;
    class function AddForwardslash(const AValue: string): string;
    class function GetResourceStream(AResourceName: string): TResourceStream; overload;
    class function GetResourceStream(AResourceName: string; AInstance: NativeUInt): TResourceStream; overload;
    class function Clone(AComponent: TComponent): TComponent;
    class function HTMLStrip(AHTML: string): string;
    class function VarPos(ASubValue, AValue: string; var AResult: Integer): Integer;
    class function VarPosNoCase(ASubValue, AValue: string; var AResult: Integer): Integer;
    class function CharInStr(s: string; Index: Integer): Char;
    class function HexStrToBytes(const AValue: string): TBytes;
    class function URLDecode(const AURL: string): string;
    class function URLEncode(const AURL: string): string;
    class function EnDeCrypt(const AValue: String): String;
    class function SaveBitmapToHexStr(const ABitmap: TAdvBitmap): string;
    class function SaveStreamToHexStr(const AStream: TStream): string;
    class function CreateBitmapFromHexStr(const AHexStr: string): TAdvBitmap;
    class function MulDivInt(nNumber, nNumerator, nDenominator: Integer): Integer; overload;
    class function MulDivSingle(nNumber, nNumerator, nDenominator: Single): Single; overload;
    class function FormatBytesAsString(ASize: Extended): String;
    class procedure LoadBitmapFromHexStr(const AHexStr: string; const ABitmap: TAdvBitmap);
    class procedure LoadStreamFromHexStr(const AHexStr: string; const AStream: TStream);
    {$IFDEF CMNLIB}
    class function ConvertBitmapToJPEGStream(ABitmap: TAdvBitmap; AQuality: Single = 1.0; ABackgroundColor: TColor = clWhite): TMemoryStream;
    class function FindGraphicClass(const Buffer; const {%H-}BufferSize: Int64; out GraphicClass: TGraphicClass): Boolean;
    {$ENDIF}
    {$IFDEF FMXLIB}
    class function ConvertBitmapToJPEGStream(ABitmap: TAdvBitmap; AQuality: Single = 1.0; ABackgroundColor: TAlphaColor = TAlphaColorRec.White): TMemoryStream;
    {$ENDIF}
    {$IFDEF WEBLIB}
    class function ConvertBitmapToJPEGStream(ABitmap: TAdvBitmap; AQuality: Single = 1.0; ABackgroundColor: TColor = clWhite): TMemoryStream;
    {$ENDIF}
    class procedure Split(const ADelimiter: Char; AInput: string; const AStrings: TStrings);
    class procedure Log(const AMessage: string);
    class function EpochToDateTime(const AEpoch: string): TDateTime;
    {$IFDEF FNCLIB}
    class function DateTimeToISO(const ADateTime: TDateTime; AAddDelimiters: Boolean = False; AAddMilliSeconds: Boolean = False): string;
    class function ISOToDateTime(const AISOString: string): TDateTime; overload;
    class function ISOToDateTime(const AISOString: string; AIsUTC: Boolean): TDateTime; overload;
    class function ISOToDate(const AISOString: string): TDate;
    class function DateTimeToUTC(const ADateTime: TDateTime): string;
    class function DateTimeToMilliSeconds(const ADateTime: TDateTime): Int64;
    class function GetMimeType(AFile: string): string;
    class function FileToBase64(const AFile: TAdvUtilsFile): string;
    class procedure LoadFile(const AFile: TAdvUtilsLoadFile; const ALoadFileComplete: TAdvUtilsLoadFileCompleteEvent);
    class function Decode64(const AValue: string; const AURL: Boolean = False): string;
    class function Encode64(const AValue: string; const AURL: Boolean = False): string; overload;
    class function Encode64(const AValue: TBytes; const AURL: Boolean = False): string; overload;    
    class function GetJSONProp(AJSONValue: TJSONValue; APropertyName: string): string;
    class function GetJSONValueAsString(AJSONValue: TJSONValue): string;
    class function GetJSONValue(AJSONValue: TJSONValue; APropertyName: string): TJSONValue;
    class function GetJSONDoubleValue(AJSONValue: TJSONValue; APropertyName: string): Double;
    class function GetJSONIntegerValue(AJSONValue: TJSONValue; APropertyName: string): Integer;
    class function IsJSONTrue(AJSONValue: TJSONValue): Boolean;
    class function IsJSONFalse(AJSONValue: TJSONValue): Boolean;
    class function GetJSONArraySize(AJSONArray: TJSONArray): Integer;
    class function GetJSONArrayItem(AJSONArray: TJSONArray; AIndex: Integer): TJSONValue;
    class function GetJSONObjectSize(AJSONObject: TJSONObject): Integer;
    class function GetJSONObjectItem(AJSONObject: TJSONObject; AIndex: Integer): TJSONValue;
    class function GetJSONObjectName(AJSONObject: TJSONObject; AIndex: Integer): string;
    class function ParseJSON(AJSON: string): TJSONValue;
    {$ENDIF}
    class function MatchStrEx(AValue1, AValue2: string; ACaseSensitive: Boolean): Boolean;
    class function MatchStr(AValue1, AValue2:string; ACaseSensitive: Boolean): Boolean;
    class function StripLogicSpaces(AValue: string): string;
    class function FirstChar(ACharset: TAdvUtilsCharSet; AValue: string; var spos: Integer): char;
    class function CharInSet(ch: Char; ACharSet: TAdvUtilsCharSet): boolean; overload;
    class function AddCharSet(ACharSet1, ACharSet2: TAdvUtilsCharSet): TAdvUtilsCharSet;
    class function SubCharSet(ACharSet1, ACharSet2: TAdvUtilsCharSet): TAdvUtilsCharSet;
    class function AlphaCharSet: TAdvUtilsCharSet;
    class function NumericCharSet: TAdvUtilsCharSet;
    class function CreateCharSet(AValue: string): TAdvUtilsCharSet;
    class function IsDate(AValue: string; var ADate: TDateTime): boolean;
    class function IPos(su,s:string):Integer;
    class function UnFixMarkup(su:string; SpecialChars: boolean = true): string;
    class function FixMarkup(su:string;SpecialChars: boolean = true): string;
    class function TagReplaceString(const Srch,Repl:string;var Dest:string):Boolean;
    class function Matches(s0a, s1a: PChar): Boolean; overload;
    {$IFNDEF WEBLIB}
    class function Matches(s0a, s1a: string): Boolean; overload;
    {$ENDIF}
    class function StripThousandSep(ps: string): String;
    class function ClosingParenthesis(s1: string): Integer;
    class function GetMarkupIndex(const Markup: string): integer;
    class function GetSpecialChar(const Value: integer): UnicodeString;
    class function IndexOfTextInArray(const AText: string; const AValues: array of string): Integer;
    {$IFDEF MACOS}
    class function NSStrEx(AString: String): NSString;
    {$ENDIF}
    {$IFDEF FMXLIB}
    class function GetParentForm(AControl: TFMXObject): TCustomForm;
    {$ENDIF}
    {$IFDEF CMNLIB}
    class function GetParentForm(AControl: TControl): TCustomForm;
    {$ENDIF}
    {$IFDEF WEBLIB}
    class function GetParentForm(AControl: TControl): TCustomForm;
    {$ENDIF}
    class function GetOwnerForm(AComponent: TComponent): TCustomForm;
    class function &Message(const AMessage: string): Integer; overload;
    class function &Message(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons; const AHelpContext: LongInt): Integer; overload;
    class function SelectFile(var AFileName: string; const ADefaultFolder: string = ''; const AFilter: string = ''; const ACallBack: TAdvUtilsFileCallBackEvent = nil): Boolean;
    class function SaveFile(var AFileName: string; const AFilter: string = ''; const ACallBack: TAdvUtilsFileCallBackEvent = nil): Boolean;
    class function EscapeString(const AValue: string): string;
    class function UnescapeString(const AValue: string): string;
    class function DownloadImage(AURL: string): TMemoryStream;
  end;

  {$IFNDEF WEBLIB}
  TAdvComponentHelper = class helper for TComponent
    function GetVersionNumber(AMaj, AMin, ARel, ABld: ShortInt): string;
  end;
  {$ENDIF}

  {$IFDEF CMNLIB}
  TAdvControlHelper = class helper for TControl
    function MakeScreenshot: TAdvBitmap;
  end;
  {$ENDIF}

  TAdvClipBoardFormat = (cfText, cfRTF, cfHTML, cfBitmap, cfBitmapWin, cfStream, cfRichTextStream);

  TAdvClipBoard = class
  protected
    {$IFDEF FMXLIB}
    {$IFDEF MSWINDOWS}
    class function GetFormat(AFormat: TAdvClipBoardFormat): Cardinal; virtual;
    class function isTextFormat(AFormat: Cardinal): Boolean; virtual;
    class function isStreamFormat(AFormat: Cardinal): Boolean; virtual;
    {$ELSE}
    class function GetFormat(AFormat: TAdvClipBoardFormat): string; virtual;
    class function isTextFormat(AFormat: String): Boolean; virtual;
    class function isStreamFormat(AFormat: String): Boolean; virtual;
    {$ENDIF}
    {$ENDIF}
  public
    {$IFDEF FMXLIB}
    {$IFDEF MSWINDOWS}
    class procedure SetValue(AValue: TValue; AFormat: Cardinal); virtual;
    class function GetValue(AFormat: Cardinal): TValue; virtual;
    class function GetCustomValue(AFormat: Cardinal; AData: Pointer): TValue; virtual;
    class procedure SetCustomValue(AFormat: Cardinal; AValue: TValue; var AData: Pointer); virtual;
    class function AllocateCustomValue(AFormat: Cardinal; AValue: TValue): THandle; virtual;
    {$ELSE}
    class procedure SetValue(AValue: TValue; AFormat: String); virtual;
    class function GetValue(AFormat: String): TValue; virtual;
    {$IFDEF MACOS}
    class function GetCustomValue(AFormat: String; AData: NSData): TValue; virtual;
    class function SetCustomValue(AFormat: String; AValue: TValue): NSData; virtual;
    {$ELSE}
    class function GetCustomValue(AFormat: String; AData: Pointer): TValue; virtual;
    class function SetCustomValue(AFormat: String; AValue: TValue): Pointer; virtual;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    class procedure SetBitmapWin(ABitmap: TAdvBitmap); virtual;
    class function GetBitmapWin: TAdvBitmap; virtual;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    class function HasFormat(AFormat: Cardinal): Boolean; overload; virtual;
    {$ELSE}
    class function HasFormat(AFormat: String): Boolean; overload; virtual;
    {$ENDIF}
    {$ENDIF}

    {$IFDEF CMNLIB}
    class function GetFormat(AFormat: TAdvClipBoardFormat): string; virtual;
    {$ENDIF}
    class function HasFormat(AFormat: TAdvClipBoardFormat): Boolean; overload; virtual;
    class function HasContent: Boolean; virtual;
    class procedure Clear; virtual;
    class procedure SetText(AText: String); virtual;
    class function GetText: String; virtual;
    class procedure SetStream(AStream: TMemoryStream); virtual;
    class function GetStream: TMemoryStream; virtual;
    class procedure SetBitmap(ABitmap: TAdvBitmap); virtual;
    class function GetBitmap: TAdvBitmap; virtual;
    class procedure SetRichTextStream(AStream: TMemoryStream); virtual;
    class function GetRichTextStream: TMemoryStream; virtual;
    class procedure SetRTF(ARTF: String); virtual;
    class function GetRTF: String; virtual;
    class procedure SetHTML(AHTML: String); virtual;
    class function GetHTML: String; virtual;
  end;

{$EXTERNALSYM Hiword}
function Hiword(L: DWORD): Word;
{$EXTERNALSYM LoWord}
function LoWord(L: DWORD): Word;
{$EXTERNALSYM MakeWord}
function MakeWord(b1,b2: Integer): Integer;
{$EXTERNALSYM MakeLong}
function MakeLong(i1,i2: Integer): Integer;
{$IFDEF WEBLIB}
function CreateUploadFile(AName: string; AData: TJSUint8Array): TAdvUtilsFile;
{$ENDIF}

{$IFDEF FNCLIB}
function ExtractFileNameEx(AFile: TAdvUtilsFile): string;
function GetMimeTypeEx(AFile: TAdvUtilsFile): string;
{$ENDIF}

var
  CF_FNCSTREAM: Word;
  CF_FNCRICHTEXTSTREAM: Word;
  CF_FNCRTF: Word;
  CF_FNCBITMAP: Word;
  {$IFDEF FMXLIB}
  CF_FNCBITMAPWIN: Word;
  {$ENDIF}
  CF_FNCHTML: Word;

implementation

{$IFDEF WEBLIB}
uses
  DateUtils;
{$ENDIF}
{$IFNDEF WEBLIB}
uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF LCLLIB}
  WinInet, ShellApi, AnsiStrings
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,ShlObj, ComObj
  {$ENDIF}
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  iOSApi.UIKit, iOSApi.CocoaTypes, FMX.Platform.iOS
  {$ELSE}
  MacApi.AppKit
  {$ENDIF}
  ,MacApi.Helpers
  {$ENDIF}
  {$IFDEF ANDROID}
  AndroidApi.JNI.App, AndroidApi.IOUtils, AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNIBridge,
  AndroidApi.JNI.Net, AndroidApi.Helpers, FMX.Helpers.Android, AndroidApi.JNI.JavaTypes, AndroidApi.Log
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  StrUtils, FMUX.Api
  {$ENDIF}
  ,FMX.Platform, IOUtils, FMX.Surfaces
  {$HINTS OFF}
  {$IF COMPILERVERSION > 28}
  ,FMX.Utils
  {$IFEND}
  {$IF COMPILERVERSION > 30}
  {$IFDEF DELPHI_LLVM}
  ,FMX.DialogService.Async
  {$ELSE}
  ,FMX.DialogService.Sync
  {$ENDIF}
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  LCLIntF, LCLType
  {$ENDIF}
  {$IFDEF CMNLIB}
  ,ClipBrd
  {$ENDIF}
  {$IFNDEF LINUX}
  {$IFNDEF LCLLIB}
  ,StrUtils
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,FPHTTPClient, Base64
  {$IFDEF UNIX}
  ,unixutil
  {$ENDIF}
  {$ENDIF}
  ,DateUtils
  {$IFDEF FNCLIB}
  {$IFNDEF LCLWEBLIB}
  ,System.NetEncoding, System.TimeSpan
  {$ENDIF}
  {$ENDIF}
  ;
{$ENDIF}


const
  HTMLNumSpecialChar = 86;
  JSONNumSpecialChar = 7;

var
  HTMLEncodedChar : array[1..HTMLNumSpecialChar] of unicodestring = ('&','<','>','"',' ',
                                                     'é','è','ë','ê',
                                                     'ó','ò','ö','ô',
                                                     'í','ì','ï','î',
                                                     'ú','ù','ü','û',
                                                     'á','à','ä','â',
                                                     'É','È','Ë','Ê',
                                                     'Ó','Ò','Ö','Ô',
                                                     'Í','Ì','Ï','Î',
                                                     'Ú','Ù','Ü','Û',
                                                     'Á','À','Ä','Â',
                                                     'ç','Ç','ø','Ø',
                                                     'å','Å','©','®',
                                                     '€','«','»','ã',
                                                     'Ã','õ','Õ','™',
                                                     '§','¶','ß','£',
                                                     '$','§','‰','¶',
                                                     '''','¥','¤','¢',
                                                     '±','¡','°','ý','Ý',
                                                     '¼','½','¾','Æ',
                                                     'æ','Ñ','ñ','Ð');

  HTMLSpecialChar : array[1..HTMLNumSpecialChar] of string = ('amp;','lt;','gt;','quot;','&nbsp;',
                                             '&eacute;','&egrave;','&euml;','&ecirc;',
                                             '&oacute;','&ograve;','&ouml;','&ocirc;',
                                             '&iacute;','&igrave;','&iuml;','&icirc;',
                                             '&uacute;','&ugrave;','&uuml;','&ucirc;',
                                             '&aacute;','&agrave;','&auml;','&acirc;',
                                             '&Eacute;','&Egrave;','&Euml;','&Ecirc;',
                                             '&Oacute;','&Ograve;','&Ouml;','&Ocirc;',
                                             '&Iacute;','&Igrave;','&Iuml;','&Icirc;',
                                             '&Uacute;','&Ugrave;','&Uuml;','&Ucirc;',
                                             '&Aacute;','&Agrave;','&Auml;','&Acirc;',
                                             '&ccedil;','&Ccedil;','&oslash;','&Oslash;',
                                             '&aring;','&Aring;','&copy;','&reg;',
                                             '&euro;','&laquo;','&raquo;','&atilde;',
                                             '&Atilde;','&otilde;','&Otilde',
                                             '&trade;','&sect;','&para;','&szlig;',
                                             '&pound;','&dollar;','&sect;',
                                             '&permil;','&para;','&#39;',
                                             '&yen;','&curren;','&cent;','&plusmn;',
                                             '&iexcl;','&deg;','&#x00FD;','&#221;',
                                             '&frac14;', '&frac12;', '&frac34;','&AElig;',
                                             '&aelig;','&Ntilde;','&ntilde;','&ETH;'
                                             );

  JSONSpecialChar: array[1..JSONNumSpecialChar] of char = ('"','\',#8,#12,#10,#13,#9);

  JSONEncodedSpecialChar: array[1..JSONNumSpecialChar] of string = ('\u0022','\u005C','\u0008','\u000C','\u000A','\u000D','\u0009');

{$IFDEF FNCLIB}
  FMimeTypes: TStringList;

function ExtractFileNameEx(AFile: TAdvUtilsFile): string;
begin
  {$IFNDEF WEBLIB}
  Result := ExtractFileName(AFile);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := AFile.Name;
  {$ENDIF}
end;

function GetMimeTypeEx(AFile: TAdvUtilsFile): string;
begin
  {$IFNDEF WEBLIB}
  Result := TAdvUtils.GetMimeType(AFile)
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := TAdvUtils.GetMimeType(AFile.Name);
  {$ENDIF}
end;
{$ENDIF}

function Hiword(L: DWORD): Word;
begin
  Result := L shr 16;
end;

function LoWord(L: DWORD): Word;
begin
  Result := L AND $FFFF;
end;

function MakeWord(b1,b2: Integer): Integer;
begin
  Result := b1 or b2 shl 8;
end;

function MakeLong(i1,i2: Integer): Integer;
begin
  Result := i1 or i2 shl 16;
end;

{$IFDEF WEBLIB}
function CreateUploadFile(AName: string; AData: TJSUint8Array): TAdvUtilsFile;
begin
  Result.Data := AData;
  Result.Name := AName;
end;
{$ENDIF}

{$IFDEF ANDROID}
type
  JHttpURLConnection = interface;
  JURLConnection = interface;
  JURL = interface;

  JURLClass = interface(JObjectClass)
    ['{3E1A363B-FA2A-4399-A0C7-24DFC255E133}']
    {class} function init(spec: JString): JURL; cdecl; overload;
  end;

  [JavaSignature('java/net/URL')]
  JURL = interface(JObject)
    ['{BCC8860A-E48B-4894-95A9-26B3AFDF3EDC}']
    function openConnection: JURLConnection; cdecl; overload;
  end;
  TJURL = class(TJavaGenericImport<JURLClass, JURL>) end;

  JURLConnectionClass = interface(JObjectClass)
    ['{969FF7F8-7B73-491C-A621-855E197B0B6B}']
  end;

  [JavaSignature('java/net/URLConnection')]
  JURLConnection = interface(JObject)
    ['{F50D6528-AE5E-4862-89A1-CAE44274AED9}']
    function getHeaderField(key: JString): JString; cdecl; overload;
  end;
  TJURLConnection = class(TJavaGenericImport<JURLConnectionClass, JURLConnection>) end;

  JHttpURLConnectionClass = interface(JURLConnectionClass)
    ['{D80190E4-FBCC-4286-9E39-42F45C58E7D9}']
  end;
  [JavaSignature('java/net/HttpURLConnection')]
  JHttpURLConnection = interface(JURLConnection)
  ['{57A25834-F65A-4078-8B73-1C566DF70536}']
    procedure setRequestMethod(method: JString); cdecl;
    procedure setDoInput(newValue: Boolean); cdecl;
    procedure setDoOutput(newValue: Boolean); cdecl;
    function getOutputStream: JOutputStream; cdecl;
    function getInputStream: JInputStream; cdecl;
    function getResponseCode: Integer; cdecl;
  end;
  TJHttpURLConnection = class(TJavaGenericImport<JHttpURLConnectionClass, JHttpURLConnection>) end;

type
  TAdvUtilsFileMode = (ufmOpen, ufmSave);

  TAdvUtilsDialogDismissListener = class(TJavaLocal, JDialogInterface_OnDismissListener)
  public
    procedure onDismiss(dialog: JDialogInterface); cdecl;
  end;

  TAdvUtilsDialogSelectDirectoryListener = class(TJavaLocal, JDialogInterface_OnClickListener)
  public
    procedure onClick(dialog: JDialogInterface; which: Integer); cdecl;
  end;

  TAdvUtilsDialogSelectFileListener = class(TJavaLocal, JDialogInterface_OnClickListener)
  public
    procedure onClick(dialog: JDialogInterface; which: Integer); cdecl;
  end;

var
  FFileExt: string;
  FFileMode: TAdvUtilsFileMode;
  FSelectedFile: String;
  FCallBack: TAdvUtilsFileCallBackEvent;
  FDialogBuilder: JAlertDialog_Builder;
  FDialog: JDialog;
  FCurrentPath: JFile;
  FFileList: TJavaObjectArray<JCharSequence>;
  FDirectoryListener: TAdvUtilsDialogSelectDirectoryListener;
  FSelectFileListener: TAdvUtilsDialogSelectFileListener;
  FFilter: string;
  FDismissListener: TAdvUtilsDialogDismissListener;

function SharedActivityEx: JActivity;
begin
  {$HINTS OFF}
  {$IF COMPILERVERSION > 29}
  Result := TAndroidHelper.Activity;
  {$ELSE}
  Result := SharedActivity;
  {$IFEND}
  {$HINTS ON}
end;

procedure LoadFileList(APath: JFile; AFilter: string);
var
  l, lf: TList<string>;
  a, c: TArray<string>;
  I, K: Integer;
  s: string;
  SR: TSearchRec;
  fn: string;
  chk: Boolean;
begin
  FCurrentPath := APath;
  l := TList<string>.Create;
  if APath.exists then
  begin
    lf := TList<string>.Create;

    a := AFilter.Split(['|']);

    for I := 0 to Length(a) - 1 do
    begin
      if Odd(I) then
      begin
        c := a[I].Split([';']);
        for K := 0 to Length(c) - 1 do
        begin
          s := c[K].Replace('*', '');
          if not lf.Contains(s) then
            lf.Add(s);
        end;
      end;
    end;

    if APath.getParentFile <> nil then
      l.Add('..');

    fn := TAdvUtils.AddBackslash(JStringToString(APath.getPath)) + '*';
    k := FindFirst(fn, faAnyFile, SR);
    while K = 0 do
    begin
      chk := (sr.Name <> '.') and (sr.Name <> '..');
      chk := chk and (lf.Contains(ExtractFileExt(sr.Name)) or (lf.Count = 0) or ((lf.Count = 1) and (lf[0] = '.')) or (sr.Attr and 16 = 16));

      if chk then
      begin
        l.Add(ExtractFilePath(fn) + SR.Name);
      end;

      K := FindNext(SR);
    end;
    FindClose(SR);

    lf.Free;
  end;

  FFileList := TJavaObjectArray<JCharSequence>.Create(l.Count);
  for I := 0 to l.Count - 1 do
    FFileList.Items[I] := StrToJCharSequence(l[I]);

  l.Free;
end;

procedure ShowDialog(AFile: JFile);
begin
  FDialogBuilder := TJAlertDialog_Builder.JavaClass.init(SharedActivityEx);
  FDialogBuilder.setTitle(StrToJCharSequence(JStringToString(AFile.getPath)));
  if FFileMode = ufmSave then
    FDialogBuilder.setPositiveButton(StrToJCharSequence('Save'), FDirectoryListener);
  FDialogBuilder.setItems(FFileList, FSelectFileListener);
  FDialog := FDialogBuilder.create;
  FDialog.setOnDismissListener(FDismissListener);
  FDialog.show;
end;
{$ENDIF}

{$IFDEF MACOS}
{$IFDEF IOS}
const
  UIDocumentPickerModeImport = 0;
  UIDocumentPickerModeOpen = 1;
  UIDocumentPickerModeExportToService = 2;
  UIDocumentPickerModeMoveToService = 3;

type
  UIDocumentPickerViewController = interface;

  UIDocumentPickerDelegate = interface(IObjectiveC)
    ['{B978C4EB-8C88-4D80-B5B6-3AB33B7831DC}']
    procedure documentPicker(controller: UIDocumentPickerViewController; didPickDocumentsAtURLs: NSArray); cdecl;
    procedure documentPickerWasCancelled(controller: UIDocumentPickerViewController); cdecl;
  end;

  UIDocumentPickerMode = NSUInteger;

  UIDocumentPickerViewControllerClass = interface(UIViewControllerClass)
    ['{C67E60F0-F8AB-49BB-B264-9F20D14276DA}']
  end;
  UIDocumentPickerViewController = interface(UIViewController)
    ['{548F924F-2E33-4F77-8947-1B9D3F49A794}']
    function initWithDocumentTypes(allowedUTIs: NSArray; inMode: UIDocumentPickerMode): Pointer; cdecl;
    function initWithURL(url: NSURL; inMode: UIDocumentPickerMode): Pointer; cdecl;
    function initWithURLs(urls: NSArray; inMode: UIDocumentPickerMode): Pointer; cdecl;
    procedure setAllowsMultipleSelection(allowsMultipleSelection: Boolean); cdecl;
    function allowsMultipleSelection: Boolean; cdecl;
    procedure setDocumentPickerMode(documentPickerMode: UIDocumentPickerMode); cdecl;
    function documentPickerMode: UIDocumentPickerMode; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;
  end;
  TUIDocumentPickerViewController = class(TOCGenericImport<UIDocumentPickerViewControllerClass, UIDocumentPickerViewController>)  end;

  TAdvUtilsFileMode = (ufmOpen, ufmSave);

  TUIDocumentPickerDelegate = class(TOCLocal, UIDocumentPickerDelegate)
  public
    procedure documentPicker(controller: UIDocumentPickerViewController; didPickDocumentsAtURLs: NSArray); cdecl;
    procedure documentPickerWasCancelled(controller: UIDocumentPickerViewController); cdecl;
  end;

var
  FFileExt: string;
  FFileMode: TAdvUtilsFileMode;
  FSelectedFile: String;
  FCallBack: TAdvUtilsFileCallBackEvent;
  FDocumentPickerDelegate: TUIDocumentPickerDelegate;
  FDocumentPicker: UIDocumentPickerViewController;

function GetSharedApplication: UIApplication;
begin
  Result := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
end;

{$ENDIF}
class function TAdvUtils.NSStrEx(AString: String): NSString;
begin
  Result := StrToNSStr(AString);
end;

const
  libFoundation = '/System/Library/Frameworks/Foundation.framework/Foundation';
  libMobileCoreServices = '/System/Library/Frameworks/MobileCoreServices.framework/MobileCoreServices';

procedure NSLog(format: Pointer); cdecl; varargs; external libFoundation name _PU + 'NSLog';
{$ENDIF}

{ TAdvUtils }

class function TAdvUtils.IPos(su,s:string):Integer;
begin
  Result := Pos(UpperCase(su),UpperCase(s));
end;

class function TAdvUtils.TagReplaceString(const Srch,Repl:string;var Dest:string):Boolean;
var
  i: Integer;
begin
  i := IPos(srch,dest);
  if i > 0 then
  begin
    Result := True;
    Delete(Dest,i,Length(Srch));
    Dest := Copy(Dest,1,i-1) + Repl + Copy(Dest,i,Length(Dest));
  end
  else
    Result := False;
end;

class function TAdvUtils.SaveFile(var AFileName: string; const AFilter: string = ''; const ACallBack: TAdvUtilsFileCallBackEvent = nil): Boolean;
{$IFNDEF WEBLIB}
{$IFNDEF DELPHI_LLVM}
var
  s: TSaveDialog;
  ext: string;
{$ENDIF}
{$IFDEF DELPHI_LLVM}
{$IFDEF IOS}
var
  f: string;
  u: NSURL;
{$ENDIF}
{$IFDEF ANDROID}
var
  f: string;
{$ENDIF}
{$ENDIF}
begin
  Result := False;
  {$IFDEF DELPHI_LLVM}
  {$IFDEF IOS}
  FCallBack := ACallBack;
  f := AFileName;

  if ExtractFilePath(f) = '' then
    f := TAdvUtils.GetDocumentsPath + PthDel + f;

  FFileExt := ExtractFileName(AFileName);
  FFileMode := ufmSave;
  if Assigned(FDocumentPickerDelegate) then
    FDocumentPickerDelegate.Free;

  FDocumentPickerDelegate := TUIDocumentPickerDelegate.Create;

  if not FileExists(f) then
    FileCreate(f);

  u := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(TAdvUtils.NSStrEx(f)));
  FDocumentPicker := TUIDocumentPickerViewController.Wrap(TUIDocumentPickerViewController.Wrap(TUIDocumentPickerViewController.OCClass.alloc).initWithURL(u, UIDocumentPickerModeExportToService));
  FDocumentPicker.setDelegate(FDocumentPickerDelegate.GetObjectID);
  GetSharedApplication.keyWindow.rootViewController.presentViewController(FDocumentPicker, False, nil);
  FDocumentPicker.release;
  {$ENDIF}
  {$IFDEF ANDROID}
  FCallBack := ACallBack;
  FFileExt := ExtractFileName(AFileName);

  FFileMode := ufmSave;

  FDirectoryListener := TAdvUtilsDialogSelectDirectoryListener.Create;
  FSelectFileListener := TAdvUtilsDialogSelectFileListener.Create;
  FDismissListener := TAdvUtilsDialogDismissListener.Create;

  FFilter := AFilter;
  if ExtractFilePath(AFileName) = '' then
    f := AddBackslash(TAdvUtils.GetDocumentsPath)
  else
    f := AddBackslash(ExtractFilePath(AFileName));

  FSelectedFile := f + FFileExt;

  FCurrentPath := TJFile.JavaClass.init(StringToJString(f));
  LoadFileList(FCurrentPath, FFilter);
  CallInUIThreadAndWaitFinishing(
  procedure
  begin
    ShowDialog(FCurrentPath);
  end
  );
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF DELPHI_LLVM}
  s := TSaveDialog.Create(Application);
  try
    ext := ExtractFileExt(AFileName);
    s.Filter := AFilter;
    if ExtractFilePath(AFileName) = '' then
      s.InitialDir := AddBackslash(TAdvUtils.GetDocumentsPath)
    else
      s.InitialDir := AddBackslash(ExtractFilePath(AFileName));
    s.Options := s.Options + [TOpenOption.ofOverwritePrompt];
    s.FileName := ExtractFileName(AFileName);
    if s.Execute then
    begin
      AFileName := s.FileName;
      if ExtractFileExt(s.FileName) = '' then
        AFileName := AFileName + ext;
      Result := True;
    end;

    if Assigned(ACallBack) then
      ACallBack(AFileName, Result);
  finally
    s.Free;
  end;
  {$ENDIF}
{$ENDIF}
{$IFDEF WEBLIB}
begin
  Result := True;
{$ENDIF}
end;

class function TAdvUtils.EnDeCrypt(const AValue: String): String;
var
  r: string;
  i: Integer;
  c: Char;
  b: Byte;
begin
  r := '';
  {$IFDEF DELPHI_LLVM}
  for i := 0 to Length(AValue) - 1 do
  {$ELSE}
  for i := 1 to Length(AValue) do
  {$ENDIF}
  begin
    b := Ord(AValue[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;

  Result := r;
end;

class function TAdvUtils.EscapeString(const AValue: string): string;
var
  res: string;
  ch: char;
  i: integer;

  function EscapeSpecialChar(ch: char; var s: string): boolean;
  var
    i: integer;
  begin
    Result := false;
    s := '';

    for i := 1 to JSONNumSpecialChar do
    begin
      if ch = JSONSpecialChar[i] then
      begin
        s := JSONEncodedSpecialChar[i];
        Result := True;
        Break;
      end;
    end;
  end;

begin
  Result := '';
  for i := 1 to length(AValue) do
  begin
    ch := CharInStr(AValue, i);

    if ch > #127 then
      Result := Result + '\u' + IntToHex(ord(ch),4)
    else
    begin
      res := '';
      if EscapeSpecialChar(ch, res) then
        Result := Result + res
      else
        Result := Result + ch
    end;
  end;
end;

class function TAdvUtils.UnescapeString(const AValue: string): string;
var
  i: Integer;
  j: Integer;
  c: Integer;
  o: Integer;
  s: string;
  x: Integer;
begin
  SetLength(s, Length(AValue));
  {$IFDEF DELPHI_LLVM}
  o := 1;
  {$ENDIF}
  {$IFNDEF DELPHI_LLVM}
  o := 0;
  {$ENDIF}
  i := 1;
  j := 1;
  while i <= Length(AValue) do
  begin
    if AValue[i - o] = '\' then
    begin
      if i < Length(AValue) then
      begin
        if AValue[i + 1 - o] = '\' then
        begin
          x := j - o;
          s[x] := '\';
          inc(i, 2);
        end
        else if (AValue[i + 1 - o] = 'u') and (i + 1 + 4 <= Length(AValue)) and TryStrToInt('$' + string(Copy(AValue, i + 2, 4)), c) then
        begin
          inc(i, 6);
          x := j - o;
          {$IFDEF WEBLIB}
          s[x] := Chr(c);
          {$ENDIF}
          {$IFNDEF WEBLIB}
          s[x] := Char(c);
          {$ENDIF}
        end
        else
          raise Exception.CreateFmt('Invalid code at position %s', [IntToStr(i)]);
      end
      else
        raise Exception.Create('Unexpected end of string');
    end
    else
    begin
      x := j - o;
      s[x] := AValue[i - o];
      inc(i);
    end;
    inc(j);
  end;
  SetLength(s, j - 1);

  Result := s;
end;

class function TAdvUtils.SelectFile(var AFileName: string; const ADefaultFolder: string = ''; const AFilter: string = ''; const ACallBack: TAdvUtilsFileCallBackEvent = nil): Boolean;
{$IFNDEF WEBLIB}
{$IFNDEF DELPHI_LLVM}
var
  o: TOpenDialog;
{$ENDIF}
{$IFDEF DELPHI_LLVM}
{$IFDEF IOS}
var
  arr: NSArray;
  arritems: array of Pointer;
{$ENDIF}
{$IFDEF ANDROID}
var
  f: string;
{$ENDIF}
{$ENDIF}
begin
  Result := False;
  {$IFDEF DELPHI_LLVM}
  {$IFDEF IOS}
  FCallBack := ACallBack;
  FFileMode := ufmOpen;
  if Assigned(FDocumentPickerDelegate) then
    FDocumentPickerDelegate.Free;

  FDocumentPickerDelegate := TUIDocumentPickerDelegate.Create;
  SetLength(arrItems, 3);
  arrItems[0] := (CocoaNSStringConst(libMobileCoreServices, 'kUTTypeContent') as ILocalObject).GetObjectID;
  arrItems[1] := (CocoaNSStringConst(libMobileCoreServices, 'kUTTypeData') as ILocalObject).GetObjectID;
  arrItems[2] := (CocoaNSStringConst(libMobileCoreServices, 'kUTTypeItem') as ILocalObject).GetObjectID;
  arr := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@arritems[0], Length(arritems)));
  FDocumentPicker := TUIDocumentPickerViewController.Wrap(TUIDocumentPickerViewController.Wrap(TUIDocumentPickerViewController.OCClass.alloc).initWithDocumentTypes(arr, UIDocumentPickerModeImport));
  FDocumentPicker.setDocumentPickerMode(UIDocumentPickerModeImport);
  FDocumentPicker.setDelegate(FDocumentPickerDelegate.GetObjectID);
  GetSharedApplication.keyWindow.rootViewController.presentViewController(FDocumentPicker, False, nil);
  FDocumentPicker.release;
  {$ENDIF}
  {$IFDEF ANDROID}
  FCallBack := ACallBack;
  FFileMode := ufmOpen;

  FSelectFileListener := TAdvUtilsDialogSelectFileListener.Create;
  FDismissListener := TAdvUtilsDialogDismissListener.Create;

  FFilter := AFilter;
  if ADefaultFolder = '' then
    f := AddBackslash(TAdvUtils.GetDocumentsPath)
  else
    f := AddBackslash(ADefaultFolder);

  FCurrentPath := TJFile.JavaClass.init(StringToJString(f));
  LoadFileList(FCurrentPath, FFilter);
  CallInUIThreadAndWaitFinishing(
  procedure
  begin
    ShowDialog(FCurrentPath);
  end
  );
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF DELPHI_LLVM}
  o := TOpenDialog.Create(Application);
  try
    o.Options := [TOpenOption.ofFileMustExist];
    if ADefaultFolder = '' then
      o.InitialDir := AddBackslash(TAdvUtils.GetDocumentsPath)
    else
      o.InitialDir := AddBackslash(ADefaultFolder);
    o.Filter := AFilter;
    if o.Execute then
    begin
      AFileName := o.FileName;
      Result := True;
    end;

    if Assigned(ACallBack) then
      ACallBack(AFileName, Result);
  finally
    o.Free;
  end;
  {$ENDIF}
{$ENDIF}
{$IFDEF WEBLIB}
begin
  Result := True;
  if Assigned(ACallBack) then
    ACallBack(AFileName, Result);
{$ENDIF}
end;

class function TAdvUtils.&Message(const AMessage: string): Integer;
begin
  Result := TAdvUtils.&Message(AMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

class function TAdvUtils.&Message(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons; const AHelpContext: LongInt): Integer;
begin
{$IFNDEF WEBLIB}
  {$IFDEF DELPHI_LLVM}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 30}
  TDialogServiceAsync.MessageDialog(AMessage, ADialogType, AButtons, TMsgDlgBtn.mbYes, AHelpContext);
  Result := mrOk;
  {$ELSE}
  Result := mrOk;
  {$IFEND}
  {$HINTS ON}
  {$ELSE}
  {$IFDEF LCLLIB}
  Result := MessageDlg(AMessage, ADialogType, AButtons, AHelpContext);
  {$ELSE}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 30}
  {$IFDEF FMXLIB}
  Result := TDialogServiceSync.MessageDialog(AMessage, ADialogType, AButtons, TMsgDlgBtn.mbYes, AHelpContext);
  {$ELSE}
  Result := MessageDlg(AMessage, ADialogType, AButtons, AHelpContext);
  {$ENDIF}
  {$ELSE}
  Result := MessageDlg(AMessage, ADialogType, AButtons, AHelpContext);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF WEBLIB}
  if ((TMsgDlgBtn.mbOK in AButtons) and (TMsgDlgBtn.mbCancel in AButtons)) or ((TMsgDlgBtn.mbYes in AButtons) and (TMsgDlgBtn.mbNo in AButtons)) then
  begin
    asm
      if (window.confirm(AMessage))
      {
        return 1
      }
      else
      {
        return 2
      }
    end;
  end
  else
  begin
    asm
      window.alert(AMessage);
      return 1
    end;
  end;
{$ENDIF}
end;

class function TAdvUtils.NumericCharSet: TAdvUtilsCharSet;
var
  ch: char;
  i: integer;
begin
  SetLength(Result,10);
  i := 0;

  for ch := '0' to '9' do
  begin
    Result[i] := ch;
    inc(i);
  end;
end;

class function TAdvUtils.CreateCharSet(AValue: string): TAdvUtilsCharSet;
var
  i: integer;
begin
  SetLength(Result, Length(AValue));

  for i := 1 to Length(AValue) do
  begin
    Result[i - 1] := CharInStr(AValue, i);
  end;
end;

{$IFDEF FNCLIB}

class procedure TAdvUtils.GetTimeZoneInfo(var ABias: Integer; var ABiasDay: Integer; var AIsDaylight: Boolean);
{$IFNDEF UNIX}
var
{$ENDIF}
  {$IFDEF MACOS}
  lTimeZone: NSTimeZone;
  {$ENDIF}
  {$IFDEF ANDROID}
  lTimeZone: JTimeZone;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  lTimeZone: TTimeZoneInformation;
  {$ENDIF}
  {$IFDEF WEBLIB}
  bias, biasday: Integer;
  isDaylight: Boolean;
  {$ENDIF}
begin
  {$IFDEF MACOS}
  lTimeZone := TNSTimeZone.Wrap(TNSTimeZone.OCClass.systemTimeZone);
  AIsDaylight := lTimeZone.isDaylightSavingTime;
  ABias := Round(lTimeZone.secondsFromGMT / 3600);
  ABiasday := Round(lTimeZone.daylightSavingTimeOffset / 3600);
  {$ENDIF}
  {$IFDEF ANDROID}
  lTimeZone := TJTimeZone.JavaClass.getDefault;
  AIsDaylight := lTimeZone.useDaylightTime;
  ABias := Round(lTimeZone.getRawOffset / 3600 / 1000);
  ABiasday := Round(lTimeZone.getDSTSavings / 3600 / 1000);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  AIsDayLight := (GetTimeZoneInformation(lTimeZone) = TIME_ZONE_ID_DAYLIGHT);
  ABias := lTimeZone.Bias;
  ABiasday := lTimeZone.DaylightBias;
  {$ENDIF}
  {$IFDEF UNIX}
  AIsDayLight := False;
  ABias := -Tzseconds div 60;
  ABiasday := 0;
  {$ENDIF}
  {$IFDEF WEBLIB}
  asm
    Date.prototype.stdTimezoneOffset = function () {
        var jan = new Date(this.getFullYear(), 0, 1);
        var jul = new Date(this.getFullYear(), 6, 1);
        return Math.max(jan.getTimezoneOffset(), jul.getTimezoneOffset());
    }

    Date.prototype.isDstObserved = function () {
        return this.getTimezoneOffset() < this.stdTimezoneOffset();
    }
    var dt = new Date();
    bias = dt.getTimezoneOffset();
    biasday = dt.stdTimezoneOffset();
    isDaylight = dt.isDstObserved();
  end;

  ABias := bias;
  ABiasDay := biasday;
  AIsDaylight := isDaylight;
  {$ENDIF}
end;

class function TAdvUtils.DateTimeToMilliSeconds(const ADateTime: TDateTime): Int64;
var
  bias: Integer;
  biasday: Integer;
  isDayLight: Boolean;
const
  UnixStartDate: TDateTime = 25569;
begin
  bias := 0;
  biasday := 0;
  isDayLight := False;
  GetTimeZoneInfo(bias, biasday, isDayLight);

  if isDayLight then
    Result := MilliSecondsBetween(IncHour(ADateTime, (bias + biasday) div 60), UnixStartDate)
  else
    Result := MilliSecondsBetween(IncHour(ADateTime, bias div 60), UnixStartDate)
end;

class function TAdvUtils.DateTimeToUTC(const ADateTime: TDateTime): string;
var
  da,mo,ye,ho,mi,se,ms:word;
  bias: Integer;
  biasday: Integer;
  isDayLight: Boolean;

  function IntToZStr(i,l: Integer):string;
  var
    Res: string;
  begin
    Res := IntToStr(i);
    while Length(Res)<l do
      Res := '0' + Res;

    Result := Res;
  end;
begin
  bias := 0;
  biasday := 0;
  isDayLight := False;
  GetTimeZoneInfo(bias, biasday, isDayLight);

  DecodeDate(ADateTime,ye,mo,da);
  DecodeTime(ADateTime,ho,mi,se,ms);
  Result := IntToStr(ye) + '-' + IntToZStr(mo,2) + '-' + IntToZStr(da,2) + 'T' +
            IntToZStr(ho,2) + ':' + IntToZStr(mi,2) + ':' + IntToZStr(se,2);

  if isDayLight then
  begin
    if (bias) <= 0 then
      Result := Result + '+' + IntToZStr((-bias - biasday) div 60,2) + ':00'
    else
      Result := Result + '-' + IntToZStr((bias + biasday) div 60,2) + ':00';
  end
  else
  begin
    if Bias <> 0 then
    begin
      if Bias <= 0 then
        Result := Result + '+' + IntToZStr((-bias) div 60,2) + ':00'
      else
        Result := Result + '-' + IntToZStr((bias) div 60,2) + ':00';
    end
    else
      Result := Result + '+00:00';
  end;
end;

class function TAdvUtils.ISOToDate(const AISOString: string): TDate;
var
  da,mo,ye: Word;
  err: Integer;
  s: string;
begin
  s := AISOString;
  Val(Copy(s,1,4),ye,err);
  Val(Copy(s,6,2),mo,err);
  Val(Copy(s,9,2),da,err);

  if ye < 1 then
    ye := 1;
  if mo < 1 then
    mo := 1;
  if da < 1 then
    da := 1;

  Result := EncodeDate(ye,mo,da) + EncodeTime(0,0,0,0);
end;

class function TAdvUtils.ISOToDateTime(const AISOString: string): TDateTime;
var
  da,mo,ye,ho,mi,se: Word;
  err: Integer;
  s: string;
begin
  s := AISOString;
  Val(Copy(s,1,4),ye,err);
  Val(Copy(s,6,2),mo,err);
  Val(Copy(s,9,2),da,err);
  Val(Copy(s,12,2),ho,err);
  Val(Copy(s,15,2),mi,err);
  Val(Copy(s,18,2),se,err);

  if ye < 1 then
    ye := 1;
  if mo < 1 then
    mo := 1;
  if da < 1 then
    da := 1;

  Result := EncodeDate(ye,mo,da) + EncodeTime(ho,mi,se,0);
end;

class function TAdvUtils.ISOToDateTime(const AISOString: string; AIsUTC: Boolean): TDateTime;
const
  STimePrefix: Char = 'T';
var
  TimeString, DateString: string;
  TimePosition: Integer;
  da,mo,ye,ho,mi,se: Word;
  HourOffset, MinuteOffset: Integer;
  err: Integer;
  sign: string;
  s: string;

  function AdjustDateTime(const ADate: TDateTime; AHourOffset, AMinuteOffset:Integer): TDateTime;
  var
    AdjustDT: TDateTime;
    BiasLocal: Int64;
    BiasTime: Int64;
    BiasHour: Integer;
    BiasMins: Integer;
    BiasDT: TDateTime;
    {$IFNDEF LCLWEBLIB}
    TZ: TTimeZone;
    {$ENDIF}
    {$IFDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    TZ: TTimeZoneInformation;
    {$ENDIF}
    {$ENDIF}
  begin
    Result := ADate;
    if AIsUTC then
    begin
      { If we have an offset, adjust time to go back to UTC }
      if (AHourOffset <> 0) or (AMinuteOffset <> 0) then
      begin
        AdjustDT := EncodeTime(Abs(AHourOffset), Abs(AMinuteOffset), 0, 0);
        if ((AHourOffset * MinsPerHour) + AMinuteOffset) > 0 then
          Result := Result - AdjustDT
        else
          Result := Result + AdjustDT;
      end;
    end
    else
    begin
      {$IFNDEF LCLWEBLIB}
      TZ := TTimeZone.Local;
      BiasLocal := Trunc(TZ.GetUTCOffset(Result).Negate.TotalMinutes);
      {$ENDIF}
      {$IFDEF LCLLIB}
      {$IFDEF MSWINDOWS}
      GetTimeZoneInformation(TZ);
      BiasLocal := TZ.Bias;
      {$ENDIF}
      {$IFDEF UNIX}
      BiasLocal := -Tzseconds div 60;
      {$ENDIF}
      {$ENDIF}
      {$IFDEF WEBLIB}
      asm
        var dt = new Date();
        BiasLocal = dt.getTimezoneOffset();
      end;
      {$ENDIF}

      BiasTime  := (AHourOffset * MinsPerHour) + AMinuteOffset;
      if (BiasLocal + BiasTime) = 0 then
        Exit;

      BiasLocal := BiasLocal + BiasTime;
      BiasHour := Abs(BiasLocal) div MinsPerHour;
      BiasMins := Abs(BiasLocal) mod MinsPerHour;
      BiasDT := EncodeTime(BiasHour, BiasMins, 0, 0);
      if (BiasLocal > 0) then
        Result := Result - BiasDT
      else
        Result := Result + BiasDT;
    end;
  end;
begin
  s := AISOString;

  HourOffset := 0;
  MinuteOffset := 0;
  TimePosition := Pos(STimePrefix, s);
  if TimePosition >= 0 then
  begin
    DateString := Copy(s, 0, TimePosition);
    TimeString := Copy(s, TimePosition + 1, Length(s));
  end;

  Val(Copy(s,1,4),ye,err);
  Val(Copy(s,6,2),mo,err);
  Val(Copy(s,9,2),da,err);
  Val(Copy(s,12,2),ho,err);
  Val(Copy(s,15,2),mi,err);
  Val(Copy(s,18,2),se,err);

  if ye < 1 then
    ye := 1;
  if mo < 1 then
    mo := 1;
  if da < 1 then
    da := 1;

  //Get TimeOffset from Iso string
  if Length(s) > 20 then
  begin
    sign := Copy(s,20,1);
    Val(Copy(s,21,2), HourOffset, err);
    Val(Copy(s,24,2), MinuteOffset, err);

    if sign = '-' then
    begin
      HourOffset := HourOffset * -1;
      MinuteOffset := MinuteOffset * -1;
    end;
  end;

  Result := EncodeDate(ye,mo,da) + EncodeTime(ho,mi,se,0);
  Result := AdjustDateTime(Result, HourOffset, MinuteOffset);
end;

class function TAdvUtils.DateTimeToISO(const ADateTime: TDateTime; AAddDelimiters: Boolean = False; AAddMilliSeconds: Boolean = False): string;
var
  da, mo, ye, ho, mi, se, ms: Word;
  dd, td, msec: string;

  function IntToZStr(i, l: Integer): string;
  var
    Res: string;
  begin
    Res := IntToStr(i);
    while Length(Res) < l do
      Res := '0' + Res;

    Result := Res;
  end;
begin
  if AAddDelimiters then
  begin
    dd := '-';
    td := ':';
  end;

  DecodeDate(ADateTime, ye, mo, da);
  DecodeTime(ADateTime, ho, mi, se, ms);

  if AAddMilliSeconds then
    msec := '.' + IntToZStr(ms, 3) + 'Z';

  Result := IntToZStr(ye,4) + dd + IntToZStr(mo,2) + dd + IntToZStr(da,2) + 'T' +
            IntToZStr(ho,2) + td + IntToZStr(mi,2) + td + IntToZStr(se,2) + msec;
end;
{$ENDIF}

class function TAdvUtils.DownloadImage(AURL: string): TMemoryStream;
{$IFDEF LCLLIB}
begin
  try
    Result := TMemoryStream.Create;
    TFPHTTPClient.SimpleGet(AURL, Result);
  finally
  end;
{$ENDIF}
{$IFNDEF LCLLIB}
{$IFDEF MACOS}
var
  req: NSMutableURLRequest;
  nurl: NSURL;
  res: Pointer;
  resdata: NSData;
begin
  Result := TMemoryStream.Create;
  try
    nurl := TNSURL.Wrap(TNSURL.OCClass.URLWithString(TAdvUtils.NSStrEx(AURL)));
    req := TNSMutableURLRequest.Wrap(TNSMutableURLRequest.OCClass.requestWithURL(nurl, NSURLRequestUseProtocolCachePolicy, 60.0));
    req.setHTTPMethod(TAdvUtils.NSStrEx('GET'));

    res := nil;
    resdata := TNSURLConnection.OCClass.sendSynchronousRequest(req, @res, nil);
    if Assigned(resdata) and Assigned(res) then
    begin
      if not UTF8ToString(TNSString.Wrap(TNSHTTPURLResponse.Wrap(res).allHeaderFields.valueForKey(TAdvUtils.NSStrEx('Content-Type'))).UTF8String).StartsWith('image/') then
        Exit;

      {$HINTS OFF}
      {$IF COMPILERVERSION > 31}
      Result.Write(resdata.bytes^, resdata.length);
      {$ELSE}
      Result.Write(resdata.bytes, resdata.length);
      {$IFEND}
      {$HINTS ON}
    end;
  finally
  end;
{$ELSE}
{$IFDEF ANDROID}
  var
    urlCon: JHttpURLConnection;
    u: JURL;
    r: Integer;
    data: TJavaArray<Byte>;
    bs: JByteArrayOutputStream;
    ip: JInputStream;
  begin
    Result := TMemoryStream.Create;
    try
      u := TJURL.JavaClass.init(StringToJString(AURL));
      urlCon := TJHttpURLConnection.Wrap((u.openConnection as ILocalObject).GetObjectID);
      urlCon.setDoInput(True);
      urlCon.setDoOutput(False);
      urlCon.setRequestMethod(StringToJString('GET'));

      if not urlCon.getHeaderField(StringToJString('Content-Type')).startsWith(StringToJString('image/')) then
        Exit;

      if (urlCon.getResponseCode in [200, 201]) then
      begin
        bs := TJByteArrayOutputStream.JavaClass.init;
        ip := urlCon.getInputStream;
        r := ip.read();
        while r <> -1 do
        begin
          bs.write(r);
          r := ip.read();
        end;

        data := bs.toByteArray;
        Result :=  TMemoryStream.Create;
        Result.WriteBuffer(data.Data^, bs.size);
        bs.close;
      end;
    finally
    end;
{$ELSE}
{$IFDEF LINUX}
begin
{$ELSE}
{$IFNDEF WEBLIB}
var
  UrlHandle: HINTERNET;
  Buffer: array[0..1024] of AnsiChar;
  BytesRead: DWORD;
  lpdwlen,lpdwidx,lpdword: DWORD;
  cbuf: array[0..255] of char;
  p: Pointer;
begin
  Result := TMemoryStream.Create;

  p := InternetOpen('TAdvUtils', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(p) then
  begin
    UrlHandle := InternetOpenUrl(p, PChar(AURL), nil, 0, INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_RELOAD, 0);

    if Assigned(UrlHandle) then
    begin
      lpdwlen := SizeOf(cbuf);
      lpdwidx := 0;
      HttpQueryInfo(UrlHandle, HTTP_QUERY_CONTENT_TYPE,@cbuf,lpdwlen,lpdwidx);

      if Pos('IMAGE',UpperCase(StrPas(cbuf))) = 0 then
      begin
        InternetCloseHandle(UrlHandle);
        Exit;
      end;

      lpdword := 0;
      lpdwlen := 4;
      lpdwidx := 0;

      HttpQueryInfo(URLHandle,HTTP_QUERY_CONTENT_LENGTH,@lpdword,lpdwlen,lpdwidx);

      try
        FillChar(Buffer, SizeOf(Buffer), 0);
        repeat
          FillChar(Buffer, SizeOf(Buffer), 0);
          InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesRead);
          Result.Write(buffer, bytesread);
        until BytesRead = 0;
      finally
      end;
      InternetCloseHandle(UrlHandle);
    end
    else
      raise Exception.CreateFmt('Cannot open URL %s', [AUrl]);
  end
  else
    raise Exception.Create('Unable to initialize Wininet');
{$ENDIF}
{$IFDEF WEBLIB}
begin
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

class function TAdvUtils.IndexOfTextInArray(const AText: string; const AValues: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(AValues) to High(AValues) do
    if SameText(AText, AValues[I]) then
    begin
      if (pos('cute', AText) > 0) or (pos('grave', AText) > 0) or (pos('uml', AText) > 0) or (pos('circ', AText) > 0) or (pos('tilde', AText) > 0)  then
      begin
        if (AText = AValues[I]) then
        begin
          Result := I;
          Break;
        end;
      end
      else
      begin
        Result := I;
        Break;
      end;
    end;
end;

class function TAdvUtils.GetMarkupIndex(const Markup: string): integer;
begin
  Result := IndexOfTextInArray(Markup, HTMLSpecialChar);
end;

class function TAdvUtils.GetSpecialChar(const Value: integer): UnicodeString;
begin
  Result := HTMLEncodedChar[Value];
end;

class function TAdvUtils.UnFixMarkup(su:string;SpecialChars: boolean = true):string;
var
  i: integer;
begin
  while Pos('&lt;',su) > 0 do
  begin
    TagReplacestring('&lt;','<',su);
  end;

  while Pos('&gt;',su) > 0 do
  begin
    TagReplacestring('&gt;','>',su);
  end;

  while Pos('&amp;',su) > 0 do
  begin
    TagReplacestring('&amp;','&',su);
  end;

  while Pos('&quot;',su) > 0 do
  begin
    TagReplacestring('&quot;','"',su);
  end;

  if SpecialChars then
  begin
    for i := 4 to HTMLNumSpecialChar do
    begin
      while Pos(string(HTMLSpecialChar[i]),su) > 0 do
      begin
        TagReplacestring(string(HTMLSpecialChar[i]),string(HTMLEncodedChar[i]),su);
      end;
    end;
  end;

  Result := su;
end;

class function TAdvUtils.FixMarkup(su:string;SpecialChars: boolean = true): string;
var
  i: integer;
  res: string;
  ch: char;
begin
  while Pos('&',su) > 0 do
  begin
    TagReplacestring('&','*amp;',su);
  end;

  while Pos('*amp;',su) > 0 do
  begin
    TagReplacestring('*amp;','&amp;',su);
  end;

  while Pos('"',su) > 0 do
  begin
    TagReplacestring('"','&quot;',su);
  end;

  while Pos('<',su) > 0 do
  begin
    TagReplacestring('<','&lt;',su);
  end;

  while Pos('>',su) > 0 do
  begin
    TagReplacestring('>','&gt;',su);
  end;

  if SpecialChars then
  begin
    for i := 4 to HTMLNumSpecialChar do
    begin
      while Pos(string(HTMLEncodedChar[i]),su) > 0 do
      begin
        TagReplacestring(string(HTMLEncodedChar[i]),string(HTMLSpecialChar[i]),su);
      end;
    end;

    res := '';
    for i := 1 to Length(su) do
    begin
      ch := CharInStr(su, i);
      if ord(ch) > 256 then
        res := res + '&#'+IntToStr(ord(ch))+';'
      else
        res := res + ch;
    end;
    su := res;
  end;

  Result := su;
end;

class function TAdvUtils.FormatBytesAsString(ASize: Extended): String;
const
  Measure: Array[0..4] of String = ('Bytes', 'KB', 'MB', 'GB', 'TB');
var
  NewSize: Extended;
  i: Integer;
begin
  Result := '';

  if ASize > -1 then
  begin
    i := 0;
    NewSize := ASize;

    while (NewSize >= 1024) do
    begin
      NewSize := NewSize / 1024;
      Inc(i);
    end;

    case i of
      0: Result := FloatToStr(NewSize);
      1, 2, 3, 4: Result := FormatFloat('0.00', NewSize);
    else
      Result := FloatToStr(NewSize);
    end;

    Result := Result + #32 + Measure[i];
  end;
end;

class function TAdvUtils.GetOwnerForm(AComponent: TComponent): TCustomForm;
var
  c: TComponent;
begin
  Result := nil;
  c := AComponent;
  if not Assigned(c) then
    Exit;

  if c.Owner is TCustomForm then
    Result := c.Owner as TCustomForm
  else
    Result := GetOwnerForm(c.Owner);
end;

{$IFDEF FMXLIB}
class function TAdvUtils.GetParentForm(AControl: TFMXObject): TCustomForm;
var
  c: TFMXObject;
{$ENDIF}
{$IFDEF CMNLIB}
class function TAdvUtils.GetParentForm(AControl: TControl): TCustomForm;
{$ENDIF}
{$IFDEF WEBLIB}
class function TAdvUtils.GetParentForm(AControl: TControl): TCustomForm;
var
  c: TControl;
{$ENDIF}
begin
{$IFDEF FMXLIB}
  Result := nil;
  c := AControl;
  if not Assigned(c) then
    Exit;

  if c.Parent is TCustomForm then
    Result := c.Parent as TCustomForm
  else
    Result := GetParentForm(c.Parent);
{$ENDIF}
{$IFDEF CMNLIB}
  Result := Forms.GetParentForm(AControl);
{$ENDIF}
{$IFDEF WEBLIB}
  Result := nil;
  c := AControl;
  if not Assigned(c) then
    Exit;

  if c.Parent is TCustomForm then
    Result := c.Parent as TCustomForm
  else
    Result := GetParentForm(c.Parent);
{$ENDIF}
end;

{$IFDEF VCLLIB}
function CalculateDPIScale(Scaled: boolean; AHDC: HDC): single; overload;
var
  FDPI: integer;
begin
  Result:= 1.0;
  if Scaled then
  begin
    if AHDC = 0 then
      AHDC:= GetDC(0);
    try
      FDPI := GetDeviceCaps(AHDC, LOGPIXELSX);
      if FDPI <> 96 then
        Result:= FDPI / 96;
    finally
      if AHDC = 0 then
        ReleaseDC(0, AHDC);
    end;
  end;
end;

function CalculateDPIScale(AForm: TCustomForm; AHDC: HDC): single; overload;
begin
  Result := 1.0;
  if Assigned(AForm) and (AForm is TForm) then
    Result := CalculateDPIScale((AForm as TForm).Scaled, AHDC);
end;
{$ENDIF}

class function TAdvUtils.CharInStr(s: string; Index: Integer): Char;
begin
  Result := #0;
  if (Index > 0) and (Index <= Length(s)) then
  begin
    {$IFDEF DELPHI_LLVM}
    dec(Index);
    {$ENDIF}
    Result := s[Index]
  end;
end;

class function TAdvUtils.Clone(AComponent: TComponent): TComponent;
{$IFDEF WEBLIB}
begin
  raise Exception.Create('Implement Clone');
{$ENDIF}
{$IFNDEF WEBLIB}
var
  S: TStream;
  SaveName: string;
  Reader: TReader;
begin
  S := TMemoryStream.Create;
  try
    { store }
    SaveName := AComponent.Name;
    AComponent.Name := '';
    S.WriteComponent(AComponent);
    AComponent.Name := SaveName;
    S.Position := 0;
    { load }
    try
      Reader := TReader.Create(S, 4096);
      try
        Result := Reader.ReadRootComponent(nil);
      finally
        Reader.Free;
      end;
    finally
    end;
  finally
    S.Free;
  end;
{$ENDIF}
end;

class function TAdvUtils.GetAppPath: String;
begin
  {$IFDEF WEBLIB}
  Result := 'file://'
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := ExtractFilePath(ParamStr(0));
  {$ENDIF}
end;

class function TAdvUtils.GetDocumentsPath: string;
{$IFDEF VCLLIB}
var
  szBuffer: array [0..MAX_PATH] of Char;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  Result := TPath.GetDocumentsPath;
  {$ENDIF}
  {$IFDEF LCLLIB}
  Result := GetUserDir + PathDelim + 'Documents';
  {$ENDIF}
  {$IFDEF VCLLIB}
  OleCheck(SHGetFolderPath(0, $0005, 0, 0, szBuffer));
  Result := szBuffer;
  {$ENDIF}
end;

class function TAdvUtils.GetMousePos: TPointF;
{$IFDEF FMXLIB}
var
  m: IFMXMouseService;
{$ENDIF}
{$IFDEF WEBLIB}
var
  p: TPoint;
{$ENDIF}
begin
  Result := PointF(-1, -1);
  {$IFDEF FMXLIB}
  if TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, IInterface(m)) then
    Result := m.GetMousePos;
  {$ENDIF}
  {$IFDEF CMNLIB}
  Result := PointF(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  {$ENDIF}
  {$IFDEF WEBLIB}
  p := WEBLib.Controls.GetMousePos;
  Result := PointF(p.X, p.Y);
  {$ENDIF}
end;

class function TAdvUtils.AddForwardslash(const AValue: string): string;
begin
  {$IFDEF DELPHI_LLVM}
  if (Length(AValue) >= 1) and (AValue[Length(AValue) - 1] <> '/') then
  {$ELSE}
  if (Length(AValue) >= 1) and (AValue[Length(AValue)] <> '/') then
  {$ENDIF}
    Result := AValue + '/'
  else
    Result := AValue;
end;

class function TAdvUtils.AddBackslash(const AValue: string): string;
begin
  {$IFDEF DELPHI_LLVM}
  if (Length(AValue) >= 1) and (AValue[Length(AValue) - 1] <> PthDel) then
  {$ELSE}
  if (Length(AValue) >= 1) and (AValue[Length(AValue)] <> PthDel) then
  {$ENDIF}
    Result := AValue + PthDel
  else
    Result := AValue;
end;

class function TAdvUtils.GetDPIScale(AOwner: TComponent = nil): Single;
{$IFDEF FMXLIB}
var
  ScreenSvc: IFMXScreenService;
{$ENDIF}
{$IFDEF VCLLIB}
var
  frm: TCustomform;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  Result := 1.0;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSvc)) then
    Result := ScreenSvc.GetScreenScale;
  {$ENDIF}
  {$IFDEF VCLLIB}
  Result := 1.0;
  if Assigned(Application.MainForm) and Assigned(Application.MainForm.Canvas) then
    Result := CalculateDPIScale(Application.MainForm, Application.MainForm.Canvas.Handle)
  else if Assigned(AOwner) then
  begin
    frm := GetOwnerForm(AOwner);
    if Assigned(frm) and Assigned(frm.Canvas) then
      Result := CalculateDPIScale(frm, frm.Canvas.Handle);
  end;

  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  Result := Screen.PixelsPerInch / 96;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := Screen.PixelsPerInch / 72;
  {$ENDIF}
  {$IFDEF LINUX}
  Result := Screen.PixelsPerInch / 96;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := 1.0;
  {$ENDIF}
end;

class function TAdvUtils.IsHighDPIScale: Boolean;
begin
  Result := GetDPIScale > 1;
end;

class function TAdvUtils.IsHTMLUnicode(AValue: UnicodeString): Boolean;
begin
  {$IFDEF LCLLIB}
  Result := IsHTML(UTF8Encode(AValue));
  {$ENDIF}
  {$IFNDEF LCLLIB}
  Result := IsHTML(AValue);
  {$ENDIF}
end;

class function TAdvUtils.IsHTML(AValue: string): Boolean;
begin
  Result := (Pos('</', AValue) > 0) or (Pos('/>', AValue)  > 0) or (Pos('<BR>', UpperCase(AValue)) > 0);
end;

class procedure TAdvUtils.OpenFile(AFile: string; AControlReference: TControl = nil);
{$IFDEF ANDROID}
var
  i: JIntent;
  t: string;
  ext: string;
{$ENDIF}
{$IFDEF IOS}
var
  docController: UIDocumentInteractionController;
  url: NSURL;
  r: NSRect;
  vw: UIView;
  frm: TCommonCustomForm;
  pt: TPointF;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  FmuxOpenFile(PChar(AFile));
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  frm := Application.MainForm;
  if Assigned(frm) then
  begin
    url := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(NSStrEx(AFile)));
    docController := TUIDocumentInteractionController.Wrap(TUIDocumentInteractionController.OCClass.interactionControllerWithURL(url));
    docController.retain;
    vw := WindowHandleToPlatform(frm.Handle).View;
    if Assigned(AControlReference) then
    begin
      pt := AControlReference.LocalToAbsolute(AControlReference.Position.Point);
      r.origin.x := pt.X;
      r.origin.Y := pt.Y;
      r.size.Width := AControlReference.Width;
      r.size.Height := AControlReference.Height;
    end
    else
    begin
      r := vw.frame;
      r.origin.x := (r.size.width - 1) / 2;
      r.origin.y := (r.size.height - 1) / 2;
      r.size.width := 1;
      r.size.height := 1;
    end;

    docController.presentOpenInMenuFromRect(r, vw, TRUE);
  end;
  {$ELSE}
  TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace).openFile(NSStrEx(AFile));
  {$ENDIF}
  {$ENDIF}

  {$IFDEF ANDROID}
  i := TJIntent.Create;
  i.setAction(TJIntent.JavaClass.ACTION_VIEW);
  if not AFile.ToLower.StartsWith('file://') then
    AFile := 'file://' + AFile;

  ext := ExtractFileExt(AFile).ToLower;
  if ext = '.pdf' then
    t := 'application/pdf'
  else if ext = '.html' then
    t := 'text/html'
  else if (ext = '.jpeg') or (ext = '.jpg') then
    t := 'image/jpeg'
  else if (ext = '.gif') then
    t := 'image/gif'
  else if (ext = '.png') then
    t := 'image/png'
  else if (ext = '.bmp') then
    t := 'image/bmp'
  else
    t := 'text/plain';

  i.setDataAndType(StrToJURI(AFile), StringToJString(t));
  SharedActivityEx.startActivity(i);
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  ShellExecute(0,PChar('open'),PChar(AFile),PChar(''),PChar(''), SW_NORMAL);
  {$ENDIF}
  {$ENDIF}

  {$IFDEF LCLLIB}
  OpenDocument(AFile);
  {$ENDIF}
end;

class procedure TAdvUtils.OpenURL(AURL: string);
{$IFDEF ANDROID}
var
  i: JIntent;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  FmuxOpenUrl(PChar(AURL));
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication).openURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSStrEx(AUrl))));
  {$ELSE}
  TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace).openURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSStrEx(AUrl))));
  {$ENDIF}
  {$ENDIF}

  {$IFDEF ANDROID}
  i := TJIntent.Create;
  i.setAction(TJIntent.JavaClass.ACTION_VIEW);
  i.setData(StrToJURI(AURL));
  SharedActivityEx.startActivity(i);
  {$ENDIF}
  {$ENDIF}

  {$IFDEF LCLLIB}
  LCLIntF.OpenURL(AURL);
  {$ENDIF}

  {$IFNDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  ShellExecute(0,PChar('open'),PChar(AURL),PChar(''),PChar(''), SW_NORMAL);
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WEBLIB}
  asm
    window.open(AURL, '_blank');
  end;
  {$ENDIF}
end;

{$IFDEF FNCLIB}
class function TAdvUtils.ParseJSON(AJSON: string): TJSONValue;
begin
  {$IFDEF LCLLIB}
  Result := GetJSON(AJSON);
  {$ELSE}
  Result := TJSONObject.ParseJSONValue(AJSON);
  {$ENDIF}
end;

class function TAdvUtils.GetJSONDoubleValue(AJSONValue: TJSONValue; APropertyName: string): Double;
var
  v: TJSONValue;
begin
  Result := 0;
  v := GetJSONValue(AJSONValue, APropertyName);
  if Assigned(v) and (v is TJSONNumber) then
  begin
    {$IFDEF LCLLIB}
    Result := (v as TJSONNumber).AsFloat;
    {$ELSE}
    Result := (v as TJSONNumber).AsDouble;
    {$ENDIF}
  end;
end;

class function TAdvUtils.IsJSONTrue(AJSONValue: TJSONValue): Boolean;
begin
  Result := False;
  if Assigned(AJSONValue) then
    Result := AJSONValue.Value = 'true';
end;

class function TAdvUtils.IsJSONFalse(AJSONValue: TJSONValue): Boolean;
begin
  Result := False;
  if Assigned(AJSONValue) then
    Result := AJSONValue.Value = 'false';
end;

class function TAdvUtils.GetJSONIntegerValue(AJSONValue: TJSONValue; APropertyName: string): Integer;
var
  v: TJSONValue;
begin
  Result := 0;
  v := GetJSONValue(AJSONValue, APropertyName);
  if Assigned(v) and (v is TJSONNumber) then
  begin
    {$IFDEF LCLLIB}
    Result := (v as TJSONNumber).AsInteger;
    {$ELSE}
    Result := (v as TJSONNumber).AsInt;
    {$ENDIF}
  end;
end;

class function TAdvUtils.GetJSONValueAsString(AJSONValue: TJSONValue): string;
begin
  Result := '';
  if Assigned(AJSONValue) then
  begin
    {$IFDEF LCLLIB}
    Result := AJSONValue.FormatJSON(AsCompressedJSON);
    {$ENDIF}
    {$IFNDEF LCLLIB}
    Result := AJSONValue.ToString
    {$ENDIF}
  end;
end;

class function TAdvUtils.GetJSONValue(AJSONValue: TJSONValue;
  APropertyName: string): TJSONValue;
begin
  Result := nil;
  try
    if AJSONValue is TJSONObject then
    begin
      {$IFDEF LCLLIB}
      Result := (AJSONValue as TJSONObject).Find(APropertyName);
      {$ELSE}
      Result := (AJSONValue as TJSONObject).Values[APropertyName];
      {$ENDIF}
    end;
  except
  end;
end;

class function TAdvUtils.GetJSONArraySize(AJSONArray: TJSONArray): Integer;
begin
  Result := AJSONArray.Count;
end;

class function TAdvUtils.GetJSONArrayItem(AJSONArray: TJSONArray; AIndex: Integer): TJSONValue;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex <= GetJSONArraySize(AJSONArray) - 1) then
    Result := AJSONArray.Items[AIndex];
end;

class function TAdvUtils.GetJSONObjectSize(AJSONObject: TJSONObject): Integer;
begin
  Result := AJSONObject.Count;
end;

class function TAdvUtils.GetJSONObjectName(AJSONObject: TJSONObject; AIndex: Integer): string;
{$IFNDEF LCLLIB}
var
  p: TJSONPair;
{$ENDIF}
begin
  Result := '';
  if (AIndex >= 0) and (AIndex <= GetJSONObjectSize(AJSONObject) - 1) then
  begin
    {$IFDEF LCLLIB}
    Result := AJSONObject.Names[AIndex];
    {$ELSE}
    {$IFDEF WEBLIB}
    p := AJSONObject.Get(AIndex);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    p := AJSONObject.Pairs[AIndex];
    {$ENDIF}
    if Assigned(p) then
      Result := p.JsonString.Value;
    {$ENDIF}
  end;
end;

class function TAdvUtils.GetJSONObjectItem(AJSONObject: TJSONObject; AIndex: Integer): TJSONValue;
{$IFNDEF LCLWEBLIB}
var
  p: TJSONPair;
{$ENDIF}
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex <= GetJSONObjectSize(AJSONObject) - 1) then
  begin
    {$IFDEF LCLWEBLIB}
    Result := AJSONObject.Items[AIndex];
    {$ELSE}
    p := AJSONObject.Pairs[AIndex];
    if Assigned(p) then
      Result := p.JsonValue;
    {$ENDIF}
  end;
end;

class function TAdvUtils.GetJSONProp(AJSONValue: TJSONValue;
  APropertyName: string): string;
var
  v: TJSONValue;
begin
  Result := '';
  v := GetJSONValue(AJSONValue, APropertyName);
  if Assigned(v) then
    Result := v.Value;
end;
{$ENDIF}

class function TAdvUtils.GetHInstance: NativeUInt;
begin
  Result := HInstance;
end;

{$IFDEF WEBLIB}
class procedure TAdvUtils.GetFonts(FontList: TStringList);
begin
  FontList.BeginUpdate;
  FontList.Add('Arial');
  FontList.Add('Tahoma');
  FontList.Add('SimSun');
  FontList.Add('Arial Unicode MS');
  FontList.Add('Helvetica');
  FontList.Add('Times New Roman');
  FontList.Add('Times');
  FontList.Add('Courier New');
  FontList.Add('Courier');
  FontList.Add('Verdana');
  FontList.Add('Georgia');
  FontList.Add('Palatino');
  FontList.Add('Garamond');
  FontList.Add('Bookman');
  FontList.Add('Comic Sans MS');
  FontList.Add('Trebuchet MS');
  FontList.Add('Arial Black');
  FontList.Add('Impact');
  FontList.Sort;
  FontList.EndUpdate;
end;
{$ENDIF}

{$IFNDEF LCLLIB}
{$IFDEF MSWINDOWS}
function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  S := TStrings(Data);
  Temp := LogFont.lfFaceName;
  if (S.Count = 0) or (AnsiCompareText(S[S.Count-1], Temp) <> 0) then
    S.Add(Temp);
  Result := 1;
end;

class procedure TAdvUtils.GetFonts(FontList: TStringList);
var
  DC: HDC;
  LFont: TLogFont;
begin
  FontList.BeginUpdate;
  FontList.Clear;
  DC := GetDC(0);
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, LPARAM(FontList), 0);
  ReleaseDC(0, DC);
  FontList.Sort;
  FontList.EndUpdate;
end;
{$ENDIF}

{$IFDEF LINUX}
class procedure TAdvUtils.GetFonts(FontList: TStringList);
var
  I: Integer;
begin
  FontList.BeginUpdate;
  FontList.Clear;
  for I := 0 to FmuxGetFontCount - 1 do
    FontList.Add(FmuxGetFontName(I));
  FontList.Sort;
  FontList.EndUpdate;
end;
{$ENDIF}

{$IFDEF MACOS}
class procedure TAdvUtils.GetFonts(FontList: TStringList);
{$IFDEF IOS}
var
  arr: NSArray;
  I: Integer;
  lItem: NSString;
begin
  FontList.BeginUpdate;
  FontList.Clear;
  arr := TUIFont.OCClass.familyNames;
  arr := arr.sortedArrayUsingSelector(sel_getUid('localizedCaseInsensitiveCompare:'));

  for I := 0 to arr.count - 1 do
  begin
    lItem := TNSString.Wrap(arr.objectAtIndex(i));
    FontList.Add(String(lItem.UTF8String));
  end;
  FontList.Sort;
  FontList.EndUpdate;
end;
{$ELSE}
var
  fManager: NsFontManager;
  list: NSArray;
  lItem: NSString;
  i: Integer;
begin
  FontList.BeginUpdate;
  FontList.Clear;
  fManager := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
  list := fManager.availableFontFamilies;
  if (List <> nil) and (List.count > 0) then
  begin
    for i := 0 to List.Count-1 do
    begin
      lItem := TNSString.Wrap(List.objectAtIndex(i));
      FontList.Add(String(lItem.UTF8String));
    end;
  end;
  FontList.Sort;
  FontList.EndUpdate;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
class procedure TAdvUtils.GetFonts(FontList: TStringList);
begin
  FontList.BeginUpdate;
  FontList.Clear;
  FontList.Add('Roboto');
  FontList.Add('Droid Sans');
  FontList.Add('Droid Serif');
  FontList.Add('Droid Sans Mono');
  FontList.Sort;
  FontList.EndUpdate;
end;
{$ENDIF}
{$ELSE}
class procedure TAdvUtils.GetFonts(FontList: TStringList);
begin
  FontList.Assign(Screen.Fonts);
end;
{$ENDIF}

class procedure TAdvUtils.SetFontSize(AFont: TFont; ASize: Single);
begin
  {$IFDEF FMXLIB}
  AFont.Size := ASize;
  {$ENDIF}
  {$IFDEF WEBLIB}
  AFont.Size := Round(ASize / 96 * 72);
  {$ENDIF}
  {$IFDEF CMNLIB}
  AFont.Size := Round(ASize / 96 * 72);
  {$ENDIF}
end;

{$IFDEF CMNLIB}
class function TAdvUtils.FindGraphicClass(const Buffer; const BufferSize: Int64;
  out GraphicClass: TGraphicClass): Boolean;
var
  {$IFDEF VCLLIB}
  LongWords: array[Byte] of LongWord absolute Buffer;
  {$ENDIF}
  Words: array[Byte] of Word absolute Buffer;
begin
  GraphicClass := nil;

  if BufferSize = 0 then
  begin
    Result := False;
    Exit;
  end;

  {$IFDEF VCLLIB}
  case Words[0] of
    $4D42: GraphicClass := TBitmap;
    $D8FF: GraphicClass := TJPEGImage;
    $4949: if Words[1] = $002A then GraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if Words[1] = $2A00 then GraphicClass := TWicImage; //i.e., TIFF
  else
    if Int64(Buffer) = $A1A0A0D474E5089 then
      GraphicClass := TPNGImage
    else if LongWords[0] = $9AC6CDD7 then
      GraphicClass := TMetafile
    else if (LongWords[0] = 1) and (LongWords[10] = $464D4520) then
      GraphicClass := TMetafile
    {$HINTS OFF}
    {$IF COMPILERVERSION > 24}
    else if AnsiStrings.StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
    {$ELSE}
    else if StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
    {$IFEND}
    GraphicClass := TGIFImage
    {$HINTS ON}
    else if Words[1] = 1 then
      GraphicClass := TIcon;
  end;
  {$ENDIF}
  {$IFDEF LCLLIB}
  case Words[0] of
    $4D42: GraphicClass := GetGraphicClassForFileExtension('bmp');
    $D8FF: GraphicClass := GetGraphicClassForFileExtension('jpeg');
    $4949: if Words[1] = $002A then GraphicClass := GetGraphicClassForFileExtension('tiff');
    $4D4D: if Words[1] = $2A00 then GraphicClass := GetGraphicClassForFileExtension('tiff');
  else
    if Int64(Buffer) = $A1A0A0D474E5089 then
      GraphicClass := GetGraphicClassForFileExtension('png')
    else if StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
      GraphicClass := GetGraphicClassForFileExtension('gif')
    else if Words[1] = 1 then
      GraphicClass := GetGraphicClassForFileExtension('ico');
  end;
  {$ENDIF}
  Result := (GraphicClass <> nil);
end;
{$ENDIF}

class function TAdvUtils.SaveStreamToHexStr(const AStream: TStream): string;
var
  s: string;
  K: Integer;
  bt: TBytes;
begin
  s := '';
  AStream.Position := 0;
  for K := 1 to AStream.Size do
  begin
    SetLength(bt, 1);
    AStream.Read(bt, 1);
    s := s + IntToHex(bt[0], 2);
  end;
  Result := s;
end;

class function TAdvUtils.SaveBitmapToHexStr(const ABitmap: TAdvBitmap): string;
var
  ms: TMemoryStream;
  s: string;
  {$IFDEF WEBLIB}
  img: TJSObject;
  ss: TStringStream;
  {$ENDIF}
begin
  ms := TMemoryStream.Create;
  try
    s := '';
    {$IFDEF CMNLIB}
    if Assigned(ABitmap.Graphic) and not ABitmap.Graphic.Empty then
    begin
      ABitmap.Graphic.SaveToStream(ms);
    {$ENDIF}
    {$IFDEF FMXLIB}
    if not ABitmap.IsEmpty then
    begin
      ABitmap.SaveToStream(ms);
    {$ENDIF}
    {$IFDEF WEBLIB}
    if not ABitmap.Empty then
    begin
      img := ABitmap.Image;
      asm
        var canvas = document.createElement("canvas");
        canvas.width = img.width;
        canvas.height = img.height;
        var ctx = canvas.getContext("2d");
        ctx.drawImage(img, 0, 0);
        var dataURL = canvas.toDataURL("image/png");
        s = dataURL;
      end;

      ss := TStringStream.Create(s);
      try
        s := SaveStreamToHexStr(ss);
      finally
        ss.Free;
      end;
    {$ENDIF}
    {$IFNDEF WEBLIB}
      s := SaveStreamToHexStr(ms);
    {$ENDIF}
    end;
    Result := s;
  finally
    ms.Free;
  end;
end;

class function TAdvUtils.MulDivSingle(nNumber, nNumerator, nDenominator: Single): Single;
begin
  Result := MulDivInt(Round(nNumber), Round(nNumerator), Round(nDenominator));
end;

class function TAdvUtils.MulDivInt(nNumber, nNumerator, nDenominator: Integer): Integer;
  function MathRound(AValue: Extended): Int64;
  begin
    if AValue >= 0 then
      Result := Trunc(AValue + 0.5)
    else
      Result := Trunc(AValue - 0.5);
  end;
begin
  if nDenominator = 0 then
    Result := -1
  else
    Result := MathRound(Int64(nNumber) * Int64(nNumerator) / nDenominator);
end;

class function TAdvUtils.CreateBitmapFromHexStr(const AHexStr: string): TAdvBitmap;
begin
  Result := TAdvBitmap.Create;
  LoadBitmapFromHexStr(AHexStr, Result);
end;

class procedure TAdvUtils.LoadBitmapFromHexStr(const AHexStr: string; const ABitmap: TAdvBitmap);
var
  sst, sb: TStringStream;
  bst: TBytesStream;
  M: Integer;
  bt: TBytes;
  {$IFDEF VCLLIB}
  p: TGraphic;
  gcc: TGraphicClass;
  {$ENDIF}
  {$IFDEF LCLLIB}
  I: Integer;
  {$ENDIF}
  hx: string;
begin
  sst := TStringStream.Create(AHexStr);
  bst := TBytesStream.Create;
  try
    for M := 1 to sst.Size div 2 do
    begin
      bt := HexStrToBytes(sst.ReadString(2));
      {$IFDEF LCLLIB}
      for I := 0 to Length(bt) - 1 do
        bst.WriteByte(bt[I]);
      {$ENDIF}
      {$IFNDEF LCLLIB}
      bst.Write(bt, Length(bt));
      {$ENDIF}
    end;

    bst.Position := 0;

    hx := '';
    sb := TStringStream.Create('');
    try
      sb.CopyFrom(bst, bst.Size);
      hx := sb.DataString;
    finally
      sb.Free;
    end;

    if not (Pos(';base64,', hx) > 0) then
    begin
      {$IFDEF VCLLIB}
      if TAdvUtils.FindGraphicClass(bst.Memory^, bst.Size, gcc) then
      begin
        p := gcc.Create;
        try
          bst.Position := 0;
          p.LoadFromStream(bst);
          ABitmap.Assign(p)
      {$ENDIF}

      {$IFNDEF VCLLIB}
        ABitmap.LoadFromStream(bst);
      {$ENDIF}

      {$IFDEF VCLLIB}
        finally
          p.Free;
        end;
      end;
      {$ENDIF}
    end
    else
    begin
      {$IFDEF WEBLIB}
      ABitmap.LoadFromResource(hx);
      {$ENDIF}
      {$IFNDEF WEBLIB}
      //
      {$ENDIF}
    end;

  finally
    sst.Free;
    bst.Free;
  end;
end;

class procedure TAdvUtils.LoadStreamFromHexStr(const AHexStr: string;
  const AStream: TStream);
var
  sst: TStringStream;
  bst: TBytesStream;
  M: Integer;
  bt: TBytes;
  {$IFDEF LCLLIB}
  I: Integer;
  {$ENDIF}
begin
  sst := TStringStream.Create(AHexStr);
  bst := TBytesStream.Create;
  try
    for M := 1 to sst.Size div 2 do
    begin
      bt := HexStrToBytes(sst.ReadString(2));
      {$IFDEF LCLLIB}
      for I := 0 to Length(bt) - 1 do
        bst.WriteByte(bt[I]);
      {$ENDIF}
      {$IFNDEF LCLLIB}
      bst.Write(bt, Length(bt));
      {$ENDIF}
    end;

    bst.Position := 0;
    AStream.CopyFrom(bst, bst.Size);
  finally
    sst.Free;
    bst.Free;
  end;
end;

class function TAdvUtils.EpochToDateTime(const AEpoch: string): TDateTime;
var
  dw: integer;
begin
  dw := StrToInt(AEpoch);
  Result := UnixToDateTime(dw);
end;

class procedure TAdvUtils.Log(const AMessage: string);
{$IFDEF ANDROID}
var
  msh: TMarshaller;
{$ENDIF}
{$IFDEF WEBLIB}
var
  m: string;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AMessage));
  {$ENDIF}
  {$IFDEF ANDROID}
  LOGI(msh.AsAnsi(AMessage).ToPointer);
  {$ENDIF}
  {$IFDEF MACOS}
  NSLog((NSStrEx(AMessage) as ILocalObject).GetObjectID);
  {$ENDIF}
  {$IFDEF WEBLIB}
  m := AMessage;
  asm
    console.log(m);
  end;
  {$ENDIF}
end;

{$IFDEF FNCLIB}
class function TAdvUtils.GetMimeType(AFile: string): string;
var
  e: string;
begin
  e := ExtractFileExt(AFile);
  e := LowerCase(Trim(e));
  if (e <> '') and (e[{$IFDEF DELPHI_LLVM}0{$ELSE}1{$ENDIF}] = '.') then
    e := Copy(e, 2, Length(e) - 1);

  Result := '';
  if Assigned(FMimeTypes) then
    Result := FMimeTypes.Values[e];

  if Result = '' then
    Result := 'application/octet-stream';
end;

class function TAdvUtils.Encode64(const AValue: TBytes; const AURL: Boolean = False): string;
var
  s: string;
  v: TBytes;
  {$IFNDEF LCLWEBLIB}
  b: TBase64Encoding;
  {$ENDIF}
  {$IFDEF LCLLIB}
  Outstream: TStringStream;
  Encoder: TBase64EncodingStream;
  {$ENDIF}
begin
  Result := '';
  if Length(AValue) = 0 then
    Exit;

  v := AValue;
  {$IFNDEF LCLWEBLIB}
  b := TBase64Encoding.Create(0);
  try
    s := b.EncodeBytesToString(v);
  finally
    b.Free;
  end;
  {$ELSE}
  {$IFDEF LCLLIB}
  Outstream := TStringStream.Create('');
  try
    Encoder := TBase64EncodingStream.create(outstream);
    try
      Encoder.Write(v[0], Length(v));
    finally
      Encoder.Free;
    end;
    s := Outstream.DataString;
  finally
    Outstream.free;
  end;
  {$ENDIF}
  {$IFDEF WEBLIB}
  s := '';
  asm
    function _arrayBufferToBase64( buffer ) {
        var binary = '';
        var bytes = new Uint8Array( buffer );
        var len = bytes.byteLength;
        for (var i = 0; i < len; i++) {
            binary += String.fromCharCode( bytes[ i ] );
        }
        return window.btoa( binary );
    }

    s = _arrayBufferToBase64(v);
  end;
  {$ENDIF}
  {$ENDIF}
  if AURL then
  begin
    s := StringReplace(s, '+', '-', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '/', '_', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '=', '', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := s;
end;

class function TAdvUtils.Encode64(const AValue: string; const AURL: Boolean = False): string;
var
  s, v: string;
  {$IFNDEF LCLWEBLIB}
  b: TBase64Encoding;
  {$ENDIF}
begin
  Result := '';
  if Length(AValue) = 0 then
    Exit;

  v := AValue;
  {$IFNDEF LCLWEBLIB}
  b := TBase64Encoding.Create(0);
  try
    s := b.Encode(v);
  finally
    b.Free;
  end;
  {$ELSE}
  {$IFDEF LCLLIB}
  s := EncodeStringBase64(v);
  {$ENDIF}
  {$IFDEF WEBLIB}
  asm
    function b64EncodeUnicode(str) {
      return btoa(encodeURIComponent(str).replace(/%([0-9A-F]{2})/g,
          function toSolidBytes(match, p1) {
              return String.fromCharCode('0x' + p1);
      }));
    }
    s = b64EncodeUnicode(v);
  end;
  {$ENDIF}
  {$ENDIF}
  if AURL then
  begin
    s := StringReplace(s, '+', '-', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '/', '_', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '=', '', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := s;
end;

class function TAdvUtils.FileToBase64(const AFile: TAdvUtilsFile): string;
var
  s: string;
{$IFNDEF WEBLIB}
  b: TBytesStream;
  bf: TBytes;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  asm
    function uint8ToString(buf) {
      var i, length, out = '';
      for (i = 0, length = buf.length; i < length; i += 1) {
          out += String.fromCharCode(buf[i]);
      }
      return out;
    }
    s = btoa(uint8ToString(AFile.Data));
  end;

  {$ENDIF}
  {$IFNDEF WEBLIB}
  b := TBytesStream.Create;
  try
    b.LoadFromFile(AFile);
    bf := b.Bytes;
    SetLength(bf, b.Size);
    s := TAdvUtils.Encode64(bf, False);
  finally
    b.Free;
  end;
  {$ENDIF}

  Result := s;
end;

class procedure TAdvUtils.LoadFile(const AFile: TAdvUtilsLoadFile; const ALoadFileComplete: TAdvUtilsLoadFileCompleteEvent);
{$IFDEF WEBLIB}
var
  f: TJSBlob;
  c: TAdvUtilsLoadFileCompleteEvent;
{$ENDIF}
begin
  {$IFNDEF WEBLIB}
  if Assigned(ALoadFileComplete) then
    ALoadFileComplete(AFile);
  {$ENDIF}
  {$IFDEF WEBLIB}
  f := AFile;
  c := ALoadFileComplete;
  asm
    var reader = new FileReader();

    reader.onload = function(e) {
      var s = new Uint8Array(reader.result);
      var fn = pas["WEBLib.AdvUtils"].CreateUploadFile(f.name, s);
      if (c){
        c(fn);
      }
    }

    reader.readAsArrayBuffer(f);
  end;
  {$ENDIF}
end;

class function TAdvUtils.Decode64(const AValue: string; const AURL: Boolean = False): string;
var
  s: string;
  {$IFNDEF LCLWEBLIB}
  b: TBase64Encoding;
  {$ENDIF}
begin
  Result := '';
  if Length(AValue) = 0 then
    Exit;

  s := AValue;
  if AURL then
  begin
    s := s + StringOfChar('=', Length (s) mod 4);
    s := StringReplace(s, '-', '+', [rfReplaceAll]);
    s := StringReplace(s, '_', '/', [rfReplaceAll]);
  end;
  {$IFNDEF LCLWEBLIB}
  b := TBase64Encoding.Create(0);
  try
    Result := b.Decode(s);
  finally
    b.Free;
  end;
  {$ELSE}
  {$IFDEF LCLLIB}
  Result := DecodeStringBase64(s);
  {$ENDIF}
  {$IFDEF WEBLIB}
  asm
    function b64DecodeUnicode(str) {
      // Going backwards: from bytestream, to percent-encoding, to original string.
      return decodeURIComponent(atob(str).split('').map(function(c) {
          return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
      }).join(''));
    }
    return b64DecodeUnicode(s);
  end;
  {$ENDIF}
  {$ENDIF}
end;
{$ENDIF}

class function TAdvUtils.URLDecode(const AURL: string): string;
{$IFNDEF WEBLIB}
const
  HexDigit = '0123456789ABCDEF';
var
  i: Integer;
  res: string;
  ch1,ch2: integer;
  sab: TBytes;
  sabc: integer;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  Result := decodeURIComponent(AURL);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  {$IFDEF DELPHI_LLVM}
  i := 0;
  {$ELSE}
  i := 1;
  {$ENDIF}

  sabc := 0;

  SetLength(sab,length(AURL) * 2);

  while i <= Length(AURL) do
  begin
    if AURL[i] = '%' then
    begin
      if i + 2 <= Length(AURL) then
      begin
        ch1 := Pos(AURL[i + 1], HexDigit);
        ch2 := Pos(AURL[i + 2], HexDigit);

        if (ch1 > 0) and (ch2 > 0)  then
        begin
          dec(ch1);
          dec(ch2);
          ch1 := (ch1 shl 4) + ch2;

          sab[sabc] := ch1;
          inc(sabc);
        end;
        inc(i,2);
      end;
    end
    else
    begin
      sab[sabc] := ord(AURL[i]);
      inc(sabc);
    end;
    inc(i);
  end;

  sab[sabc] := 0;
  sab[sabc + 1] := 0;

  {$IFDEF LCLLIB}
  res := Trim(UTF8Encode(TEncoding.UTF8.GetString(sab)));
  {$ENDIF}
  {$IFNDEF LCLLIB}
  res := Trim(TEncoding.UTF8.GetString(sab));
  {$ENDIF}
  Result := res;
  {$ENDIF}
end;

class function TAdvUtils.URLEncode(const AURL: string): string;
{$IFNDEF WEBLIB}
var
  I: Integer;
  B: TBytes;
  c: Char;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  Result := encodeURIComponent(AURL);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := '';
  {$IFDEF LCLLIB}
  B := TEncoding.UTF8.GetBytes(UTF8Decode(AURL));
  {$ENDIF}
  {$IFNDEF LCLLIB}
  B := TEncoding.UTF8.GetBytes(AURL);
  {$ENDIF}

  for I := Low(B) to High(B) do
  begin
    c := Chr(B[I]);
    case c of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
        Result := Result + c
      else
        Result := Result + '%' + IntToHex(Ord(B[I]),2);
    end;
  end;
  {$ENDIF}
end;

class function TAdvUtils.HexStrToBytes(const AValue: string): TBytes;
var
  il, idx: Integer;
  b: TBytes;
begin
  Result := nil;
  SetLength(b, Length(AValue) div 2);
  il := 1;
  idx := 0;
  while il <= Length(AValue) do
  begin
    b[idx] := StrToInt('$' + AValue[il{$IFDEF DELPHI_LLVM}-1{$ENDIF}] + AValue[il{$IFNDEF DELPHI_LLVM} + 1{$ENDIF}]);
    il := il + 2;
    idx := idx + 1;
  end;
  Result := b;
end;

class function TAdvUtils.AddCharSet(ACharSet1,
  ACharSet2: TAdvUtilsCharSet): TAdvUtilsCharSet;
var
  l,j: integer;
begin
  l := Length(ACharSet1)+Length(ACharSet2);
  SetLength(Result,l);
  l := 0;

  for j := 0 to High(ACharSet1) do
  begin
    Result[l] := ACharSet1[j];
    inc(l);
  end;

  for j := 0 to High(ACharSet2) do
  begin
    Result[l] := ACharSet2[j];
    inc(l);
  end;
end;

class function TAdvUtils.AlphaCharSet: TAdvUtilsCharSet;
var
  i: integer;
  ch: char;
begin
  SetLength(Result,52);
  i := 0;

  for ch := 'a' to 'z' do
  begin
    Result[i] := ch;
    inc(i);
  end;

  for ch := 'A' to 'Z' do
  begin
    Result[i] := ch;
    inc(i);
  end;

end;

class function TAdvUtils.CharInSet(ch: Char; ACharset: TAdvUtilsCharSet): boolean;
var
  j: integer;
begin
  Result := false;
  for j := 0 to High(ACharSet) do
  begin
    if ch = ACharSet[j] then
    begin
      result := true;
      break;
    end;
  end;
end;

class function TAdvUtils.FirstChar(ACharset: TAdvUtilsCharSet; AValue: string; var spos: Integer): char;
var
  i:Integer;
  q: Integer;

begin
  {$IFDEF DELPHI_LLVM}
  i := 0;
  {$ELSE}
  i := 1;
  {$ENDIF}
  q := 0;
  spos := -1;
  Result := #0;

  while i <= Length(AValue) do
  begin
    if AValue[i] = '"' then
      inc(q);

    if (CharInSet(AValue[i], ACharSet)) and not Odd(q) then
    begin
      spos := i;
      Result := AValue[i];
      Break;
    end;
    Inc(i);
  end;
end;

class function TAdvUtils.StripLogicSpaces(AValue: string): string;
var
  i, k: integer;
  q: integer;
begin
  q := 0;
  {$IFDEF DELPHI_LLVM}
  i := 0;
  k := length(AValue) - 1;
  {$ELSE}
  i := 1;
  k := length(AValue);
  {$ENDIF}
  Result := '';

  while (i <= k) do
  begin
    if AValue[i] = '"' then
      inc(q);

    if (AValue[i] = ' ') then
    begin
      if Odd(q) then
        Result := Result + AValue[i];
    end
    else
      Result := Result + AValue[i];

    inc(i);
  end;
end;

class function TAdvUtils.ClosingParenthesis(s1: string): integer;
var
  i,j,k,r: integer;
begin
  r := 0;
  j := 0;
  k := 0;
  {$IFDEF DELPHI_LLVM}
  i := 0;
  {$ELSE}
  i := 1;
  {$ENDIF}

  while (i <= length(s1)) do
  begin
    if (s1[i] = ')') then
      inc(k);

    if (s1[i] = '(') then
      inc(j);

    if (s1[i] = ')') and (j = k) then
    begin
      r := i;
      break;
    end;

    inc(i);
  end;

  Result := r;
end;

{$IFDEF FMXLIB}
class function TAdvUtils.ConvertBitmapToJPEGStream(ABitmap: TAdvBitmap; AQuality: Single = 1.0; ABackgroundColor: TAlphaColor = TAlphaColorRec.White): TMemoryStream;
{$ENDIF}
{$IFDEF CMNLIB}
class function TAdvUtils.ConvertBitmapToJPEGStream(ABitmap: TAdvBitmap; AQuality: Single = 1.0; ABackgroundColor: TColor = clWhite): TMemoryStream;
{$ENDIF}
{$IFDEF WEBLIB}
class function TAdvUtils.ConvertBitmapToJPEGStream(ABitmap: TAdvBitmap; AQuality: Single = 1.0; ABackgroundColor: TColor = clWhite): TMemoryStream;
begin
  raise Exception.Create('ConvertBitmapToJPEGStream');
{$ENDIF}
{$IFNDEF WEBLIB}
var
  ms: TMemoryStream;
  bmp: TBitmap;
{$IFDEF FMXLIB}
  s: TBitmapSurface;
  sp: TBitmapCodecSaveParams;
{$ENDIF}
{$IFDEF CMNLIB}
  gcc: TGraphicClass;
  bms: TMemoryStream;
  p: TGraphic;
  img: TJPEGImage;
{$ENDIF}
begin
  Result := nil;
  {$IFDEF FMXLIB}
  s := TBitmapSurface.Create;
  bmp := TBitmap.Create;
  try
    bmp.Width := ABitmap.Width;
    bmp.Height := ABitmap.Height;
    if (ABackgroundColor <> TAlphaColorRec.Null) and (Integer(ABackgroundColor) <> -1) then
      bmp.Clear(ABackgroundColor)
    else
      bmp.Clear(TAlphaColorRec.White);
    bmp.Canvas.BeginScene;
    bmp.Canvas.DrawBitmap(ABitmap, RectF(0, 0, bmp.Width, bmp.Height), RectF(0, 0, bmp.Width, bmp.Height), 1);
    bmp.Canvas.EndScene;
    s.Assign(bmp);
    sp.Quality := Round(AQuality * 100);
    ms := TMemoryStream.Create;
    if TBitmapCodecManager.SaveToStream(ms, s, '.jpeg', @sp) then
      Result := ms;
  finally
    bmp.Free;
    s.Free;
  end;
  {$ENDIF}
  {$IFDEF CMNLIB}
  bms := TMemoryStream.Create;
  ABitmap.SaveToStream(bms);
  try
    if FindGraphicClass(bms.Memory^, bms.Size, gcc) then
    begin
      if gcc = TJPEGImage then
      begin
        ms := TMemoryStream.Create;
        ms.LoadFromStream(bms);
        Result := ms;
      end
      else
      begin
        p := gcc.Create;
        img := TJPEGImage.Create;
        img.CompressionQuality := Round(AQuality * 100);
        bmp := TBitmap.Create;
        try
          bms.Position := 0;
          p.LoadFromStream(bms);
          bmp.Width := p.Width;
          bmp.Height := p.Height;
          if (ABackgroundColor <> clNone) and (Integer(ABackgroundColor) <> -1) then
            bmp.Canvas.Brush.Color := ABackgroundColor
          else
            bmp.Canvas.Brush.Color := clWhite;
          bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
          bmp.Canvas.Draw(0, 0, p);
          img.Assign(bmp);
          ms := TMemoryStream.Create;
          img.SaveToStream(ms);
          Result := ms;
        finally
          bmp.Free;
          img.Free;
          p.Free;
        end;
      end;
    end;
  finally
    bms.Free;
  end;
  {$ENDIF}
{$ENDIF}
end;

class function TAdvUtils.StripThousandSep(ps: string): string;
begin
  {$IFDEF WEBLIB}
  while (Pos(ThousandSeparator, ps) > 0) do
    Delete(ps, Pos(ThousandSeparator, ps), 1);
  Result := ps;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  while (Pos(FormatSettings.ThousandSeparator, ps) > 0) do
    Delete(ps, Pos(FormatSettings.ThousandSeparator, ps), 1);
  Result := ps;
  {$ENDIF}
end;

class function TAdvUtils.SubCharSet(ACharSet1,
  ACharSet2: TAdvUtilsCharSet): TAdvUtilsCharSet;
var
  i,j,l: integer;
  found: boolean;
begin
  l := 0;
  SetLength(Result,l);

  for i := 0 to Length(ACharSet1) - 1 do
  begin
    found := false;
    for j := 0 to Length(ACharSet2) - 1 do
      begin
        if ACharSet1[i] = ACharSet2[j] then
        begin
          found := true;
          break;
        end;
      end;

    if not found then
    begin
      inc(l);
      SetLength(Result,l);
      Result[l - 1] := ACharSet1[i];
    end;
  end;
end;

class function TAdvUtils.IsDate(AValue: string; var ADate: TDateTime):boolean;
var
  su, ts: string;
  da,mo,ye,ho,mi,se: Word;
  err: Integer;
  dp,mp,yp,vp: Integer;
begin
  Result := False;

  ts := '';

  {$IFDEF WEBLIB}
  su := UpperCase(ShortDateFormat);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  su := UpperCase(FormatSettings.ShortDateFormat);
  {$ENDIF}
  dp := pos('D',su);
  mp := pos('M',su);
  yp := pos('Y',su);

  da := 0;
  mo := 0;
  ye := 0;
  ho := 0;
  mi := 0;
  se := 0;

  vp := -1;
  {$IFDEF WEBLIB}
  if TAdvUtils.VarPos(DateSeparator,AValue,vp)>0 then
  {$ENDIF}
  {$IFNDEF WEBLIB}
  if TAdvUtils.VarPos(FormatSettings.DateSeparator,AValue,vp)>0 then
  {$ENDIF}
  begin
    su := Copy(AValue,1,vp - 1);

    if (dp<mp) and
       (dp<yp) then
       val(su,da,err)
    else
    if (mp<dp) and
       (mp<yp) then
       val(su,mo,err)
    else
    if (yp<mp) and
       (yp<dp) then
       val(su,ye,err);

    if err<>0 then Exit;
    Delete(AValue,1,vp);

    {$IFDEF WEBLIB}
    if TAdvUtils.VarPos(DateSeparator,AValue,vp)>0 then
    {$ENDIF}
    {$IFNDEF WEBLIB}
    if TAdvUtils.VarPos(FormatSettings.DateSeparator,AValue,vp)>0 then
    {$ENDIF}
    begin
      su := Copy(AValue,1,vp - 1);

      if ((dp>mp) and (dp<yp)) or
         ((dp>yp) and (dp<mp)) then
         val(su,da,err)
      else
      if ((mp>dp) and (mp<yp)) or
         ((mp>yp) and (mp<dp)) then
         val(su,mo,err)
      else
      if ((yp>mp) and (yp<dp)) or
         ((yp>dp) and (yp<mp)) then
         val(su,ye,err);

      if err<>0 then Exit;
      Delete(AValue,1,vp);

      AValue := Trim(AValue);

      if TAdvUtils.VarPos(' ',AValue, vp) > 0 then  // there is space to separate date & time
      begin
        ts := copy(AValue, vp, length(AValue));
        AValue := copy(AValue, 1, vp - 1);
      end;

      if (dp>mp) and
         (dp>yp) then
         val(AValue,da,err)
      else
      if (mp>dp) and
         (mp>yp) then
         val(AValue,mo,err)
      else
      if (yp>mp) and
         (yp>dp) then
         val(AValue,ye,err);

      if err<>0 then Exit;
      if (da>31) then Exit;
      if (mo>12) then Exit;

      if (ts <> '') then  // there is a time part
      begin
        {$IFDEF WEBLIB}
        if TAdvUtils.VarPos(TimeSeparator,ts,vp)>0 then
        {$ENDIF}
        {$IFNDEF WEBLIB}
        if TAdvUtils.VarPos(FormatSettings.TimeSeparator,ts,vp)>0 then
        {$ENDIF}
        begin
          su := Copy(ts,1,vp - 1); // hour part
          val(su,ho,err);

          if (err <> 0) then Exit;
          if (ho > 23) then Exit;

          Delete(ts,1,vp);

          {$IFDEF WEBLIB}
          if TAdvUtils.VarPos(TimeSeparator,ts,vp)>0 then // there is a second part
          {$ENDIF}
          {$IFNDEF WEBLIB}
          if TAdvUtils.VarPos(FormatSettings.TimeSeparator,ts,vp)>0 then // there is a second part
          {$ENDIF}
          begin
            su := Copy(ts,1,vp - 1); // minute part
            val(su,mi,err);

            if err <> 0 then Exit;
            Delete(ts,1,vp);

            val(ts,se,err);  // second part
            if (err <> 0) then Exit;
            if (se > 60) then Exit;
          end
          else
          begin
            val(su,mi,err); // minute part
            if (err <> 0) then Exit;
          end;

          if (mi > 59) then Exit;

          Result := true;
        end;
      end
      else
        Result := True;

      try
        ADate := EncodeDate(ye,mo,da) + EncodeTime(ho,mi,se,0);
      except
        Result := False;
      end;
    end;
  end;
end;

class function TAdvUtils.Matches(s0a, s1a: PChar): Boolean;
{$IFNDEF WEBLIB}
begin
  Result := Matches(string(s0a), string(s1a));
end;

class function TAdvUtils.Matches(s0a, s1a: string): Boolean;
{$ENDIF}
const
  larger = '>';
  smaller = '<';
  logand  = '&';
  logor   = '^';
  asterix = '*';
  qmark = '?';
  negation = '!';

var
  matching: boolean;
  done: boolean;
  len: longint;
  lastchar:  char;
  s0,s1,s2,s3: integer;
  oksmaller,oklarger,negflag: boolean;
  compstr: string;
  flag1,flag2,flag3: boolean;
  equal: boolean;
  n1,n2: double;
  code1, code2: Integer;
  dt1,dt2: TDateTime;
  q, i: integer;
begin
  {$IFDEF DELPHI_LLVM}
  i := 1;
  {$ENDIF}
  {$IFNDEF DELPHI_LLVM}
  i := 0;
  {$ENDIF}

  oksmaller := True;
  oklarger := True;
  flag1 := False;
  flag2 := False;
  flag3 := False;
  negflag := False;
  equal := False;

  { [<>] string [&|] [<>] string }

  // do larger than or larger than or equal

  s2 := Pos(larger,s0a);

  if s2 <> 0 then
  begin
    inc(s2);
    if (s0a[s2 - i] = '=') then
    begin
      Equal := True;
      inc(s2);
    end;

    while (s0a[s2 - i] = ' ') do
      inc(s2);

    s3 := s2;
    len := 0;

    lastchar := #0;

    q := 0;

    while (s0a[s2 - i] <> ' ') and (s2 <= Length(s0a)) and (odd(q) or ((s0a[s2 - i] <> '&') and (s0a[s2 - i] <> '|')))  do
    begin
      if (s0a[s2 - i] = '"') then
        inc(q);

      if (len = 0) and (s0a[s2 - i] = '"') then
        inc(s3)
      else
        inc(len);

      lastchar := s0a[s2 - i];
      inc(s2);

      if (s0a[s2 - i] = ' ') and odd(q) then // skip space if between quotes
      begin
        lastchar := s0a[s2 - i];
        inc(s2);
      end;
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    compstr := Copy(s0a,s3,len);

    //StrLCopy(compstr,s3,len);

    Val(TAdvUtils.StripThousandSep(s1a),n1,code1);
    Val(TAdvUtils.StripThousandSep(compstr),n2,code2);

    dt2 := 0;
    if TAdvUtils.IsDate(compstr,dt2) then
      code2 := 1;

    dt1 := 0;
    if TAdvUtils.IsDate(s1a,dt1) then
      code1 := 1;

    if (code1 = 0) and (code2 = 0) then {both are numeric types}
    begin
      if equal then
        oklarger := n1 >= n2
      else
        oklarger := n1 > n2;
    end
    else
    begin
      if TAdvUtils.IsDate(compstr,dt2) and TAdvUtils.IsDate(s1a,dt1) then
      begin
        if equal then
         oklarger := dt1 >= dt2
        else
         oklarger := dt1 > dt2;
      end
      else
      begin
        if equal then
         oklarger := (CompareStr(compstr,s1a)<=0)
        else
         oklarger := (CompareStr(compstr,s1a)<0);
      end;
    end;
    flag1 := True;
  end;

  equal := False;

  // do smaller than or smaller than or equal
  s2 := Pos(smaller, s0a);
  if (s2 <> 0) then
  begin
    inc(s2);
    if (s0a[s2 - i] = '=') then
      begin
       equal := True;
       inc(s2);
      end;

    lastchar := #0;

    while (s0a[s2 - i] =' ') do inc(s2);
    s3 := s2;
    len := 0;
    q := 0;

    while (s0a[s2 - i] <> ' ') and (s2 <= Length(s0a)) and (odd(q) or ((s0a[s2 - i] <> '&') and (s0a[s2 - i] <> '|'))) do
    begin
      if s0a[s2 - i] = '"' then
        inc(q);

      if (len = 0) and (s0a[s2 - i] = '"') then
        inc(s3)
      else
        inc(len);

      lastchar := s0a[s2 - i];
      inc(s2);
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    compstr := copy(s0a, s3, len);

    Val(TAdvUtils.StripThousandSep(s1a),n1,code1);
    Val(TAdvUtils.StripThousandSep(compstr),n2,code2);
    if TAdvUtils.IsDate(compstr,dt2) then code2 := 1;
    if TAdvUtils.IsDate(s1a,dt1) then code1 := 1;

    if (code1 = 0) and (code2 = 0) then // both are numeric types
     begin
      if equal then
       oksmaller := n1 <= n2
      else
       oksmaller := n1 < n2;
     end
    else
     begin
      // check for dates here ?
      if TAdvUtils.IsDate(compstr, dt2) and TAdvUtils.IsDate(s1a, dt1) then
       begin
        if equal then
         oksmaller := dt1 <= dt2
        else
         oksmaller := dt1 < dt2;
       end
      else
       begin
        if equal then
          oksmaller := (CompareStr(compstr,s1a)>=0)
        else
          oksmaller := (CompareStr(compstr,s1a)>0);
       end;
     end;

    flag2 := True;
  end;

  s2 := Pos(negation, s0a);

  if (s2 <> 0) then
  begin
    inc(s2);
    while (s0a[s2 - i] = ' ') do
      inc(s2);
    s3 := s2;
    len := 0;

    lastchar := #0;
    q := 0;

    while (s0a[s2 - i] <> ' ') and (s2 <= length(s0a)) and (odd(q) or ((s0a[s2 - i] <> '&') and (s0a[s2 - i] <> '|'))) do
    begin
      if (s0a[s2 - i] = '"') then
        inc(q);

      if (len = 0) and (s0a[s2 - i] = '"') then
        inc(s3)
      else
        inc(len);

      lastchar := s0a[s2 - i];
      inc(s2);
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    compstr := copy(s0a, s3, len);
    flag3 := True;
  end;

  if (flag3) then
  begin
    if Pos(larger, s0a) = 0 then
      flag1 := flag3;
    if Pos(smaller, s0a) = 0 then
      flag2 := flag3;
  end;

  if (Pos(logor, s0a) <> 0) then
    if flag1 or flag2 then
    begin
      matches := oksmaller or oklarger;
      Exit;
    end;

  if (Pos(logand, s0a) <> 0) then
    if flag1 and flag2 then
    begin
      matches := oksmaller and oklarger;
      Exit;
    end;

  if ((Pos(larger, s0a) <> 0) and (oklarger)) or
     ((Pos(smaller, s0a) <> 0) and (oksmaller)) then
  begin
    matches := True;
    Exit;
  end;

  s0 := 1;
  s1 := 1;

  done := (s0a = '') or (s1a = '');

  Matching := True;

  while not done and matching do
  begin
    case s0a[s0 - i] of
    qmark:
      begin
        matching := s1 <= Length(s1a);
        if matching then
        begin
          inc(s0);
          inc(s1);
        end;
      end;
    negation:
      begin
        negflag:=True;
        inc(s0);
      end;
    '"':
      begin
        inc(s0);
      end;
    asterix:
      begin
        repeat
          inc(s0)
        until (s0a[s0 - i] <> asterix);
        len := Length(s1a) - s1;
        inc(s1,len);
        matching := matches(copy(s0a,s0,Length(s0a)),Copy(s1a, s1, Length(s1a)));

        while (len >= 0) and not matching do
        begin
         dec(s1);
         dec(len);
         matching := matches(copy(s0a,s0,Length(s0a)),Copy(s1a, s1, Length(s1a)));
       end;
       if matching then
       begin
         s0 := Length(s0a)+1;
         s1 := Length(s1a)+1;
       end;
    end;
   else
     begin
       if (s0 <= Length(s0a)) and (s1 <= Length(s1a)) then
         matching := s0a[s0 - i] = s1a[s1 - i]
       else
         matching := false;

       if matching then
       begin
         inc(s0);
         inc(s1);
       end;
     end;
   end;

   Done := (s0 > Length(s0a)) and (s1 > Length(s1a));
  end;

  if negflag then
    Matches := not matching
  else
    Matches := matching;
end;

class function TAdvUtils.MatchStr(AValue1, AValue2:string; ACaseSensitive:Boolean):Boolean;
begin
  if ACaseSensitive then
    Result := Matches(PChar(AValue1),PChar(AValue2))
  else
    Result := Matches(PChar(AnsiUpperCase(AValue1)),PChar(AnsiUpperCase(AValue2)));
end;

class function TAdvUtils.MatchStrEx(AValue1, AValue2: string; ACaseSensitive: Boolean): Boolean;
var
  ch,lastop: Char;
  sep,cp: Integer;
  res,newres: Boolean;
  CharArray: TAdvUtilsCharSet;
begin
  AValue1 := Trim(AValue1);
  AValue1 := StripLogicSpaces(AValue1);

  sep := -1;
  if TAdvUtils.VarPos('=', AValue1, sep) = 1 then
    Delete(AValue1, sep, 1);

  LastOp := #0;
  Res := True;

  SetLength(CharArray,5);
  CharArray[0] := '(';
  CharArray[1] := ';';
  CharArray[2] := '^';
  CharArray[3] := '&';
  CharArray[4] := '|';

  if (AValue1 = '') then
  begin
    Result := (AValue2 = '');
    Exit;
  end;

  if (AValue1 <> '*') and (AValue2 = '') then
  begin
    Result := False;
    Exit;
  end;

  repeat
    ch := FirstChar(CharArray, AValue1, sep);
    if ch <> #0 then
    begin
      {$IFDEF DELPHI_LLVM}
      if (length(AValue1) > 0) and (AValue1[0] = '(') and (pos('(',AValue1) > 0) then
      {$ELSE}
      if (length(AValue1) > 0) and (AValue1[1] = '(') and (pos('(',AValue1) > 0) then
      {$ENDIF}
      begin
        cp := ClosingParenthesis(AValue1);
        NewRes := MatchStrEx(copy(AValue1,2,cp - 2),AValue2,ACaseSensitive);
        delete(AValue1,1,cp);
      end
      else
      begin
        NewRes := MatchStr(Copy(AValue1,1,sep - 1),AValue2,ACaseSensitive);
        Delete(AValue1,1,sep);
      end;

      if LastOp = #0 then
        Res := NewRes
      else
        case LastOp of
        ';','^','|':Res := Res or NewRes;
        '&':Res := Res and NewRes;
        end;

      LastOp := ch;
     end;

  until (ch = #0);

  NewRes := MatchStr(AValue1, AValue2, ACaseSensitive);

  if LastOp = #0 then
    Res := NewRes
  else
    case LastOp of
    ';','^','|':Res := Res or NewRes;
    '&':Res := Res and NewRes;
    end;

  Result := Res;
end;

class procedure TAdvUtils.Split(const ADelimiter: Char; AInput: string; const AStrings: TStrings);
begin
  AStrings.Clear;
  AStrings.Delimiter := ADelimiter;
  AStrings.StrictDelimiter := True;
  AStrings.DelimitedText := AInput;
end;

class function TAdvUtils.VarPos(ASubValue, AValue: string; var AResult: Integer): Integer;
begin
  AResult := Pos(ASubValue, AValue);
  Result := AResult;
end;

class function TAdvUtils.VarPosNoCase(ASubValue, AValue: string;
  var AResult: Integer): Integer;
begin
  AResult := Pos(Uppercase(ASubValue), Uppercase(AValue));
  Result := AResult;
end;

class function TAdvUtils.HTMLStrip(AHTML: string): string;
var
  TagPos: integer;
begin
  Result := '';
  // replace line breaks by linefeeds
  while (pos('<BR>',uppercase(AHTML))>0) do
    AHTML := StringReplace(AHTML,'<BR>',chr(13)+chr(10),[rfIgnoreCase]);

  while (pos('<HR>',uppercase(AHTML))>0) do
    AHTML := StringReplace(AHTML,'<HR>',chr(13)+chr(10),[rfIgnoreCase]);

  TagPos := 0;
  {remove all other tags}
  while (VarPos('<',AHTML,TagPos) > 0) do
  begin
    Result := Result + Copy(AHTML,1,TagPos-1);
    if (VarPos('>',AHTML,TagPos)>0) then
      Delete(AHTML,1,TagPos)
    else
      Break;
  end;
  Result := Result + AHTML;
end;

class function TAdvUtils.GetResourceStream(AResourceName: string): TResourceStream;
begin
  Result := GetResourceStream(AResourceName, GetHInstance);
end;

class function TAdvUtils.GetResourceStream(AResourceName: string; AInstance: NativeUInt): TResourceStream;
{$IFDEF WEBLIB}
begin
  Result := nil;
{$ENDIF}
{$IFNDEF WEBLIB}
var
  hst: NativeUInt;
  function FindRCData(ModuleHandle: HMODULE; Name: string): boolean;
  begin
    Result := FindResource(ModuleHandle, PChar(Name), PChar(RT_RCDATA)) <> 0;
  end;
begin
  Result := nil;
  hst := AInstance;
  if FindRCData(hst, AResourceName) then
    Result := TResourceStream.Create(hst, AResourceName, RT_RCDATA);
{$ENDIF}
end;

{$IFNDEF WEBLIB}

{ TAdvComponentHelper }

function TAdvComponentHelper.GetVersionNumber(AMaj, AMin, ARel,
  ABld: ShortInt): string;
var
  vn: Integer;
begin
  vn := MakeLong(MakeWord(ABld, ARel),MakeWord(AMin, AMaj));
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;
{$ENDIF}

{$IFDEF CMNLIB}

{ TAdvControlHelper }

function TAdvControlHelper.MakeScreenshot: TAdvBitmap;
var
  bmp: Graphics.TBitmap;
begin
  bmp := Graphics.TBitmap.Create;
  bmp.SetSize(Width, Height);
  Result := TAdvBitmap.Create;
  try
    if (Self is TWinControl) and (Self as TWinControl).HandleAllocated then
      TWinControl(Self).PaintTo(bmp.Canvas.Handle, 0, 0);

    Result.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

{$ENDIF}

{ TAdvClipBoard }

{$IFDEF FMXLIB}

class procedure TAdvClipBoard.Clear;
{$IFDEF MACOS}
var
{$IFDEF IOS}
  pb: UIPasteboard;
{$ELSE}
  pb: NSPasteboard;
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
var
  clip: IFMXClipboardService;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  OpenClipboard(0);
  try
    EmptyClipboard;
  finally
    CloseClipBoard;
  end;
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  pb := TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard);
  pb.setData(nil, CocoaNSStringConst(libUIKit, 'UIPasteboardNameGeneral'));
  {$ELSE}
  pb := TNSPasteboard.Wrap(TNSPasteboard.OCClass.generalPasteboard);
  pb.clearContents;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(clip)) then
    clip.SetClipboard('');
  {$ENDIF}
end;

class function TAdvClipBoard.GetHTML: String;
var
  val: TValue;
begin
  Result := '';
  val := GetValue(GetFormat(TAdvClipBoardFormat.cfHTML));
  if not val.IsEmpty and val.IsType<String> then
    Result := val.AsType<String>;
end;

{$IFDEF MSWINDOWS}
class function TAdvClipBoard.IsTextFormat(AFormat: Cardinal): Boolean;
{$ELSE}
class function TAdvClipBoard.IsTextFormat(AFormat: String): Boolean;
{$ENDIF}
begin
  Result := (AFormat = GetFormat(TAdvClipBoardFormat.cfText))
         or (AFormat = GetFormat(TAdvClipBoardFormat.cfRTF))
         or (AFormat = GetFormat(TAdvClipBoardFormat.cfHTML));
end;

{$IFDEF MSWINDOWS}
class function TAdvClipBoard.IsStreamFormat(AFormat: Cardinal): Boolean;
{$ELSE}
class function TAdvClipBoard.IsStreamFormat(AFormat: String): Boolean;
{$ENDIF}
begin
  Result := (AFormat = GetFormat(TAdvClipBoardFormat.cfStream)) or (AFormat = GetFormat(TAdvClipBoardFormat.cfBitmap))
         or (AFormat = GetFormat(TAdvClipBoardFormat.cfRichTextStream));
end;

{$IFDEF MSWINDOWS}
class function TAdvClipBoard.GetFormat(AFormat: TAdvClipBoardFormat): Cardinal;
begin
  case AFormat of
    TAdvClipBoardFormat.cfText: Result := CF_TEXT;
    TAdvClipBoardFormat.cfRTF: Result := CF_FNCRTF;
    TAdvClipBoardFormat.cfHTML: Result := CF_FNCHTML;
    TAdvClipBoardFormat.cfStream: Result := CF_FNCSTREAM;
    TAdvClipBoardFormat.cfBitmap: Result := CF_FNCBITMAP;
    TAdvClipBoardFormat.cfBitmapWin: Result := CF_FNCBITMAPWIN;
    TAdvClipBoardFormat.cfRichTextStream: Result := CF_FNCRICHTEXTSTREAM;
    else
      Result := 0;
  end;
end;
{$ELSE}
class function TAdvClipBoard.GetFormat(AFormat: TAdvClipBoardFormat): string;
begin
  Result := '';
  case AFormat of
    TAdvClipBoardFormat.cfText: Result := 'public.utf8-plain-text';
    TAdvClipBoardFormat.cfRTF: Result := 'public.rtf';
    TAdvClipBoardFormat.cfHTML: Result := 'public.html';
    TAdvClipBoardFormat.cfStream: Result := 'TAdvClipBoard.StreamFormat';
    {$IFDEF IOS}
    TAdvClipBoardFormat.cfBitmap: Result := 'public.png';
    {$ELSE}
    TAdvClipBoardFormat.cfBitmap: Result := 'public.image';
    {$ENDIF}
    TAdvClipBoardFormat.cfRichTextStream: Result := 'RichEditorText';
  end;
end;
{$ENDIF}

class function TAdvClipBoard.GetRTF: String;
var
  val: TValue;
begin
  Result := '';
  val := GetValue(GetFormat(TAdvClipBoardFormat.cfRTF));
  if not val.IsEmpty and val.IsType<String> then
    Result := val.AsType<String>;
end;

class function TAdvClipBoard.GetBitmap: TAdvBitmap;
var
  val: TValue;
  ms: TMemoryStream;
begin
  Result := nil;
  val := GetValue(GetFormat(TAdvClipBoardFormat.cfBitmap));
  if not val.IsEmpty and val.IsType<TMemoryStream> then
  begin
    Result := TAdvBitmap.Create;
    Result.LoadFromStream(val.AsType<TMemoryStream>);
    ms := val.AsType<TMemoryStream>;
    ms.Free;
  end;
end;

class function TAdvClipBoard.GetStream: TMemoryStream;
var
  val: TValue;
begin
  Result := nil;
  val := GetValue(GetFormat(TAdvClipBoardFormat.cfStream));
  if not val.IsEmpty and val.IsType<TMemoryStream> then
    Result := val.AsType<TMemoryStream>;
end;

class function TAdvClipBoard.GetRichTextStream: TMemoryStream;
var
  val: TValue;
begin
  Result := nil;
  val := GetValue(GetFormat(TAdvClipBoardFormat.cfRichTextStream));
  if not val.IsEmpty and val.IsType<TMemoryStream> then
    Result := val.AsType<TMemoryStream>;
end;

class function TAdvClipBoard.GetText: String;
var
  val: TValue;
begin
  Result := '';
  val := GetValue(GetFormat(TAdvClipBoardFormat.cfText));
  if not val.IsEmpty and val.IsType<String> then
    Result := val.AsType<String>;
end;

{$IFDEF MSWINDOWS}
class function TAdvClipBoard.AllocateCustomValue(AFormat: Cardinal; AValue: TValue): THandle;
begin
  Result := 0;
end;

class procedure TAdvClipBoard.SetCustomValue(AFormat: Cardinal; AValue: TValue; var AData: Pointer);
{$ELSE}
{$IFDEF MACOS}
class function TAdvClipBoard.SetCustomValue(AFormat: String; AValue: TValue): NSData;
{$ELSE}
class function TAdvClipBoard.SetCustomValue(AFormat: String; AValue: TValue): Pointer;
{$ENDIF}
{$ENDIF}
begin
  {$IFNDEF MSWINDOWS}
  Result := nil;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
class function TAdvClipBoard.GetCustomValue(AFormat: Cardinal; AData: Pointer): TValue;
{$ELSE}
{$IFDEF MACOS}
class function TAdvClipBoard.GetCustomValue(AFormat: String; AData: NSData): TValue;
{$ELSE}
class function TAdvClipBoard.GetCustomValue(AFormat: String; AData: Pointer): TValue;
{$ENDIF}
{$ENDIF}
begin
  Result := nil;
end;

{$IFDEF MSWINDOWS}
class function TAdvClipBoard.GetValue(AFormat: Cardinal): TValue;
{$ELSE}
class function TAdvClipBoard.GetValue(AFormat: String): TValue;
{$ENDIF}
{$IFDEF MACOS}
var
  dt: NSData;
  str: NSString;
  I: Integer;
  arr: NSArray;
{$IFDEF IOS}
  dtObj: NSObject;
  dtp: Pointer;
  pb: UIPasteboard;
  dic: NSDictionary;
  K: Integer;
  kp: Pointer;
  key: String;
{$ELSE}
  pb: NSPasteboard;
  pbit: NSPasteboardItem;
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  hMem: THandle;
  pMem: Pointer;
  txt: string;
{$ENDIF}
{$IFDEF ANDROID}
var
  clip: IFMXClipboardService;
{$ENDIF}
begin
  Result := nil;
{$IFDEF MACOS}
  {$IFDEF IOS}
  pb := TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard);
  arr := pb.items;
  for I := 0 to arr.count - 1 do
  begin
    dic := TNSDictionary.Wrap(arr.objectAtIndex(I));
    for K := 0 to dic.allKeys.count - 1 do
    begin
      kp := dic.allKeys.objectAtIndex(K);
      key := UTF8ToString(TNSString.Wrap(kp).UTF8String);
      if key = AFormat then
      begin
        dtp := dic.objectForKey(kp);
        dtobj := TNSObject.Wrap(dtp);
        if isStreamFormat(AFormat) then
        begin
          dt := TNSData.Wrap(dtp);
          if Assigned(dt) then
          begin
            Result := TMemoryStream.Create;
            Result.AsType<TMemoryStream>.Write(dt.bytes^, dt.length);
            Result.AsType<TMemoryStream>.Position := 0;
            Exit;
          end;
        end
        else if isTextFormat(AFormat) then
        begin
          str := TNSString.Wrap(dtp);
          Result := UTF8ToString(str.UTF8String);
          Exit;
        end
        else
          Result := GetCustomValue(AFormat, TNSData.Wrap(dtp));
      end;
    end;
  end;
  {$ELSE}
  pb := TNSPasteboard.Wrap(TNSPasteboard.OCClass.generalPasteboard);
  arr := pb.pasteboardItems;
  for I := 0 to arr.count - 1 do
  begin
    pbit := TNSPasteboardItem.Wrap(arr.objectAtIndex(I));
    if isTextFormat(AFormat) then
    begin
      str := pbit.stringForType(TAdvUtils.NSStrEx(AFormat));
      if Assigned(str) then
      begin
        Result := UTF8ToString(str.UTF8String);
        Exit;
      end;
    end
    else if isStreamFormat(AFormat) then
    begin
      if AFormat = 'public.image' then
      begin
        dt := pbit.dataForType(TAdvUtils.NSStrEx('public.png'));
        if not Assigned(dt) then
        begin
          dt := pbit.dataForType(TAdvUtils.NSStrEx('public.tiff'));
          if not Assigned(dt) then
            dt := pbit.dataForType(TAdvUtils.NSStrEx('public.jpeg'));
        end;
      end
      else
        dt := pbit.dataForType(TAdvUtils.NSStrEx(AFormat));

      if Assigned(dt) then
      begin
        Result := TMemoryStream.Create;
        Result.AsType<TMemoryStream>.Write(dt.bytes^, dt.length);
        Result.AsType<TMemoryStream>.Position := 0;
        Exit;
      end;
    end
    else
      Result := GetCustomValue(AFormat, pbit.dataForType(TAdvUtils.NSStrEx(AFormat)));
  end;
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  if AFormat = CF_TEXT then
  begin
    if HasFormat(CF_UNICODETEXT) then
      AFormat := CF_UNICODETEXT;
  end;

  OpenClipboard(0);
  try
    hMem := GetClipboardData(AFormat);
  finally
    CloseClipboard;
  end;

  if hMem <> 0 then
  begin
    pMem := GlobalLock(hMem);
    if pMem <> nil then
    begin
      try
        if isStreamFormat(AFormat) then
        begin
          Result := TMemoryStream.Create;
          Result.AsType<TMemoryStream>.Write(pMem^, GlobalSize(hMem));
          Result.AsType<TMemoryStream>.Position := 0;
        end
        else if isTextFormat(AFormat) or (AFormat = CF_UNICODETEXT) then
        begin
          if AFormat = CF_UNICODETEXT then
            txt := PChar(pMem)
          else
            txt := UTF8ToString(pMem);

          Result := txt;
        end
        else
          Result := GetCustomValue(AFormat, pMem);
      finally
        GlobalUnlock(hMem);
      end;
    end;
  end;
{$ENDIF}

{$IFDEF ANDROID}
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(clip)) then
    Result := clip.GetClipboard;
{$ENDIF}
end;

class function TAdvClipBoard.HasFormat(AFormat: TAdvClipBoardFormat): Boolean;
begin
  Result := HasFormat(GetFormat(AFormat));
end;

{$IFDEF MSWINDOWS}
class function TAdvClipBoard.HasFormat(AFormat: Cardinal): Boolean;
begin
  Result := False;
  try
    OpenClipboard(0);
    Result := Boolean(IsClipboardFormatAvailable(AFormat));
    CloseClipboard;
  except
  end;
end;
{$ELSE}
class function TAdvClipBoard.HasFormat(AFormat: String): Boolean;
var
  {$IFDEF MACOS}
  {$IFDEF IOS}
  I, K: Integer;
  kp: Pointer;
  key: String;
  pb: UIPasteBoard;
  arr: NSArray;
  dic: NSDictionary;
  {$ELSE}
  I: Integer;
  pb: NSPasteBoard;
  pbi: NSPasteboardItem;
  str: NSString;
  {$ENDIF}
  {$ELSE}
  clip: IFMXClipboardService;
  {$ENDIF}
begin
  Result := False;
  {$IFDEF MACOS}
  {$IFDEF IOS}
  pb := TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard);
  arr := pb.items;
  for I := 0 to arr.count - 1 do
  begin
    dic := TNSDictionary.Wrap(arr.objectAtIndex(I));
    for K := 0 to dic.allKeys.count - 1 do
    begin
      kp := dic.allKeys.objectAtIndex(K);
      key := UTF8ToString(TNSString.Wrap(kp).UTF8String);
      if key = AFormat then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
  {$ELSE}
  pb := TNSPasteboard.Wrap(TNSPasteboard.OCClass.generalPasteboard);
  for I := 0 to pb.pasteboardItems.count - 1 do
  begin
    pbi := TNSPasteboardItem.Wrap(pb.pasteboardItems.objectAtIndex(I));
    str := pbi.availableTypeFromArray(TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((TAdvUtils.NSStrEx(AFormat) as ILocalObject).GetObjectID)));
    if Assigned(str) then
    begin
      Result := True;
      Exit;
    end;
  end;
  {$ENDIF}
  {$ELSE}
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(clip)) then
    Result := not clip.GetClipboard.IsEmpty;
  {$ENDIF}
end;
{$ENDIF}

class procedure TAdvClipBoard.SetHTML(AHTML: String);
begin
  SetValue(AHTML, GetFormat(TAdvClipBoardFormat.cfHTML));
end;

class procedure TAdvClipBoard.SetRTF(ARTF: String);
begin
  SetValue(ARTF, GetFormat(TAdvClipBoardFormat.cfRTF));
end;

class procedure TAdvClipBoard.SetStream(AStream: TMemoryStream);
begin
  SetValue(AStream, GetFormat(TAdvClipBoardFormat.cfStream));
end;

{$IFDEF MSWINDOWS}
class function TAdvClipBoard.GetBitmapWin: TAdvBitmap;
var
  Header: TBitmapFileHeader;
  DataHandle: HGLOBAL;
  BitmapInfoPtr: PBitmapV5Header;
  Stream: TMemoryStream;
begin
  Result := nil;
  OpenClipboard(0);
  DataHandle := GetClipboardData(CF_DIB);
  BitmapInfoPtr := nil;
  if DataHandle <> 0 then
    BitmapInfoPtr := GlobalLock(DataHandle);

  if Assigned(BitmapInfoPtr) then
  begin
    Stream := TMemoryStream.Create;
    try
      FillChar(Header, SizeOf(Header), 0);
      Header.bfType := $4D42;
      Header.bfSize := SizeOf(Header) + GlobalSize(DataHandle);
      Header.bfOffBits := SizeOf(Header) + BitmapInfoPtr.bV5Size;
      Stream.WriteBuffer(Header, SizeOf(Header));
      Stream.WriteBuffer(BitmapInfoPtr^, Header.bfSize - SizeOf(Header));
      Stream.Position := 0;
      Result := TAdvBitmap.Create;
      Result.LoadFromStream(Stream);
    finally
      GlobalUnlock(DataHandle);
      Stream.Free;
      CloseClipboard;
    end;
  end;
end;

class procedure TAdvClipBoard.SetBitmapWin(ABitmap: TAdvBitmap);
var
  Data: THandle;
  DataPtr: Pointer;
  BitmapHeader: TBitmapInfoHeader;
  BitmapData: TBitmapData;
  I, J: Integer;
  DIBDataPtr: Pointer;
begin
  OpenClipboard(0);
  try
    FillChar(BitmapHeader, SizeOf(BitmapHeader), 0);
    BitmapHeader.biSize := SizeOf(TBitmapInfoHeader);
    BitmapHeader.biPlanes := 1;
    BitmapHeader.biBitCount := 32;
    BitmapHeader.biCompression := BI_RGB;

    BitmapHeader.biWidth := ABitmap.Width;
    BitmapHeader.biHeight := ABitmap.Height;
    BitmapHeader.biSizeImage := ABitmap.BytesPerPixel * ABitmap.Width * ABitmap.Height;

    if BitmapHeader.biWidth <= 0 then
      BitmapHeader.biWidth := 1;
    if BitmapHeader.biHeight <= 0 then
      BitmapHeader.biHeight := 1;

    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, BitmapHeader.biWidth * Abs(BitmapHeader.biHeight) * 4 + SizeOf(BitmapHeader));
    try
      DataPtr := GlobalLock(Data);
      try
        Move(BitmapHeader, DataPtr^, SizeOf(BitmapHeader));
        DIBDataPtr := @(PByteArray(DataPtr)[SizeOf(BitmapHeader)]);

        if ABitmap.Map(TMapAccess.Read, BitmapData) then
        try
          if BitmapData.PixelFormat = TPixelFormat.BGRA then
            for I := 0 to ABitmap.Height - 1 do
              Move(PAlphaColorArray(BitmapData.Data)[I * (BitmapData.Pitch div 4)],
                PAlphaColorArray(DIBDataPtr)[(ABitmap.Height - 1 - I) * ABitmap.Width], ABitmap.Width * 4)
          else
            for I := 0 to ABitmap.Height - 1 do
              for J := 0 to ABitmap.Width - 1 do
                PAlphaColorArray(DIBDataPtr)[(ABitmap.Height - 1 - I) * ABitmap.Width + J] :=
                  BitmapData.GetPixel(J, I);
        finally
          ABitmap.Unmap(BitmapData);
        end;
      finally
        GlobalUnlock(Data);
      end;
      SetClipboardData(CF_DIB, Data);
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    CloseClipboard;
  end;
end;

{$ENDIF}

class procedure TAdvClipBoard.SetBitmap(ABitmap: TAdvBitmap);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(ms);
    SetValue(ms, GetFormat(TAdvClipBoardFormat.cfBitmap));
  finally
    ms.Free;
  end;
end;

class procedure TAdvClipBoard.SetRichTextStream(AStream: TMemoryStream);
begin
  SetValue(AStream, GetFormat(TAdvClipBoardFormat.cfRichTextStream));
end;

class procedure TAdvClipBoard.SetText(AText: String);
begin
  SetValue(AText, GetFormat(TAdvClipBoardFormat.cfText));
end;

{$IFDEF MSWINDOWS}
class procedure TAdvClipBoard.SetValue(AValue: TValue; AFormat: Cardinal);
{$ELSE}
class procedure TAdvClipBoard.SetValue(AValue: TValue; AFormat: String);
{$ENDIF}
{$IFDEF MACOS}
var
{$IFDEF IOS}
  pb: UIPasteboard;
  dic: NSDictionary;
  pValue: Pointer;
{$ELSE}
  pb: NSPasteboard;
  pbit: NSPasteboardItem;
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  hMem: THandle;
  pMem: Pointer;
  txt: string;
  l: integer;
{$ENDIF}
{$IFDEF ANDROID}
var
  clip: IFMXClipboardService;
{$ENDIF}
begin
  if AValue.IsObject then
  begin
    if AValue.IsType<TMemoryStream> then
      AValue.AsType<TMemoryStream>.Position := 0;
  end;

  {$IFDEF MACOS}
  {$IFDEF IOS}
  pb := TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard);
  if AValue.IsType<TMemoryStream> then
    pValue := TNSData.OCClass.dataWithBytes(AValue.AsType<TMemoryStream>.Memory, AValue.AsType<TMemoryStream>.Size)
  else if AValue.IsType<String> then
    pValue := (TAdvUtils.NSStrEx(AValue.AsType<String>) as ILocalObject).GetObjectID
  else
    pValue := (SetCustomValue(AFormat, AValue) as ILocalObject).GetObjectID;

  if Assigned(pValue) then
  begin
    dic := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(pValue, (TAdvUtils.NSStrEx(AFormat) as ILocalObject).GetObjectID));
    pb.addItems(TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((dic as ILocalObject).GetObjectID)));
  end;
  {$ELSE}
  pb := TNSPasteboard.Wrap(TNSPasteboard.OCClass.generalPasteboard);
  pb.declareTypes(TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((TAdvUtils.NSStrEx(AFormat) as ILocalObject).GetObjectID)), nil);
  pbit := TNSPasteboardItem.Wrap(TNSPasteboardItem.Wrap(TNSPasteboardItem.OCClass.alloc).init);
  if AValue.IsType<TMemoryStream> then
    pbit.setData(TNSData.Wrap(TNSData.OCClass.dataWithBytes(AValue.AsType<TMemoryStream>.Memory, AValue.AsType<TMemoryStream>.Size)), TAdvUtils.NSStrEx(AFormat))
  else if AValue.IsType<String> then
    pbit.setData(TAdvUtils.NSStrEx(AValue.AsType<String>).dataUsingEncoding(NSUTF8StringEncoding), TAdvUtils.NSStrEx(AFormat))
  else
    pbit.setData(SetCustomValue(AFormat, AValue), TAdvUtils.NSStrEx(AFormat));

  pb.writeObjects(TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((pbit as ILocalObject).GetObjectID)));
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  if AValue.IsObject and AValue.IsType<TMemoryStream> then
    hMem := GlobalAlloc(GHND or GMEM_DDESHARE, AValue.AsType<TMemoryStream>.Size)
  else if AValue.IsType<String> then
  begin
    txt := AValue.AsString;
    if AFormat = CF_FNCRTF then
    begin
      l := Length(txt) + 1;
      hMem := GlobalAlloc(GHND or GMEM_DDESHARE, l)
    end
    else
    begin
      l := ByteLength(txt) + SizeOf(Char);
      hMem := GlobalAlloc(GHND or GMEM_DDESHARE, l)
    end;
  end
  else
    hMem := AllocateCustomValue(AFormat, AValue);

  if hMem <> 0 then
  begin
    pMem := GlobalLock(hMem);
    if pMem <> nil then
    begin
      try
        if AValue.IsObject and AValue.IsType<TMemoryStream> then
        begin
          AValue.AsType<TMemoryStream>.Read(pMem^, AValue.AsType<TMemoryStream>.Size);
          AValue.AsType<TMemoryStream>.Position := 0;
        end
        else if AValue.IsType<string> then
        begin
          if AFormat = CF_FNCRTF then
          begin
            {$HINTS OFF}
            {$IF COMPILERVERSION > 24}
            AnsiStrings.StrCopy(pMem, PAnsiChar(AnsiString(txt)));
            {$ELSE}
            StrCopy(PAnsiChar(pMem), PAnsiChar(AnsiString(txt)));
            {$IFEND}
            {$HINTS ON}
          end
          else
          begin
            if AFormat = CF_TEXT then
              AFormat := CF_UNICODETEXT;

            Move(PChar(txt)^, pMem^, ByteLength(txt) + SizeOf(Char));
          end;
        end
        else
          SetCustomValue(AFormat, AValue, pMem);
      finally
        GlobalUnlock(hMem);
      end;
      OpenClipboard(0);
      try
        SetClipboardData(AFormat, hmem);
      finally
        CloseClipboard;
      end;
    end
    else
    begin
      GlobalFree(hMem);
    end;
  end;
{$ENDIF}

{$IFDEF ANDROID}
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(clip)) then
    clip.SetClipboard(AValue);
{$ENDIF}
end;
{$ENDIF}

{$IFDEF WEBLIB}
class function TAdvClipBoard.GetBitmap: TAdvBitmap;
begin
  raise Exception.Create('GetBitmap not supported');
end;

class procedure TAdvClipBoard.SetBitmap(ABitmap: TAdvBitmap);
begin
  raise Exception.Create('SetBitmap not supported');
end;

class function TAdvClipBoard.GetRichTextStream: TMemoryStream;
begin
  raise Exception.Create('GetRichTextStream not supported');
end;

class procedure TAdvClipBoard.SetRichTextStream(AStream: TMemoryStream);
begin
  raise Exception.Create('SetRichTextStream not supported');
end;

class function TAdvClipBoard.GetStream: TMemoryStream;
begin
  raise Exception.Create('GetStream not supported');
end;

class procedure TAdvClipBoard.SetStream(AStream: TMemoryStream);
begin
  raise Exception.Create('SetStream not supported');
end;

class function TAdvClipBoard.GetRTF: string;
begin
  raise Exception.Create('GetRTF not supported');
end;

class procedure TAdvClipBoard.SetRTF(ARTF: String);
begin
  raise Exception.Create('SetRTF not supported');
end;

class function TAdvClipBoard.GetHTML: string;
begin
  raise Exception.Create('GetHTML not supported');
end;

class procedure TAdvClipBoard.SetHTML(AHTML: String);
begin
  raise Exception.Create('SetHTML not supported');
end;

class function TAdvClipBoard.GetText: string;
begin
  raise Exception.Create('GetText not supported');
end;

class procedure TAdvClipBoard.SetText(AText: String);
begin
  asm
    const element = document.createElement('textarea');
    element.value = AText;
    document.body.appendChild(element);
    element.focus();
    element.setSelectionRange(0, element.value.length);
    document.execCommand('copy');
    document.body.removeChild(element);
  end;
end;

class procedure TAdvClipBoard.Clear;
begin
end;

class function TAdvClipBoard.HasFormat(AFormat: TAdvClipBoardFormat): Boolean;
begin
  Result := True;
end;
{$ENDIF}

{$IFDEF CMNLIB}
class function TAdvClipBoard.HasFormat(AFormat: TAdvClipBoardFormat): Boolean;
begin
  Result := False;
  case AFormat of
    TAdvClipBoardFormat.cfText: Result := Clipboard.HasFormat(CF_TEXT);
    TAdvClipBoardFormat.cfRTF: Result := Clipboard.HasFormat(CF_FNCRTF);
    TAdvClipBoardFormat.cfHTML: Result := Clipboard.HasFormat(CF_FNCHTML);
    TAdvClipBoardFormat.cfStream: Result := Clipboard.HasFormat(CF_FNCSTREAM);
    TAdvClipBoardFormat.cfBitmap: Result := Clipboard.HasFormat(CF_FNCBITMAP);
    TAdvClipBoardFormat.cfRichTextStream: Result := Clipboard.HasFormat(CF_FNCRICHTEXTSTREAM);
  end;
end;

class function TAdvClipBoard.GetFormat(AFormat: TAdvClipBoardFormat): string;
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  case AFormat of
    TAdvClipBoardFormat.cfText: Result := 'text/plain';
    TAdvClipBoardFormat.cfRTF: Result := 'Rich Text Format';
    TAdvClipBoardFormat.cfHTML: Result := 'HTML Format';
    TAdvClipBoardFormat.cfStream: Result := 'TAdvClipBoard.StreamFormat';
    TAdvClipBoardFormat.cfBitmap: Result := 'Delphi Picture';
    TAdvClipBoardFormat.cfRichTextStream: Result := 'RichEditorText';
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  case AFormat of
    TAdvClipBoardFormat.cfText: Result := 'text/plain';
    TAdvClipBoardFormat.cfRTF: Result := 'text/richtext';
    TAdvClipBoardFormat.cfHTML: Result := 'text/html';
    TAdvClipBoardFormat.cfStream: Result := 'TAdvClipBoard.StreamFormat';
    TAdvClipBoardFormat.cfBitmap: Result := 'Delphi Picture';
    TAdvClipBoardFormat.cfRichTextStream: Result := 'RichEditorText';
  end;
  {$ENDIF}
end;

class procedure TAdvClipBoard.Clear;
begin
  Clipboard.Clear;
end;

class procedure TAdvClipBoard.SetText(AText: String);
begin
  {$IFDEF VCLLIB}
  Clipboard.AsText := AText;
  {$ENDIF}
  {$IFDEF LCLLIB}
  Clipboard.SetTextBuf(PChar(AText));
  {$ENDIF}
end;

class function TAdvClipBoard.GetText: String;
begin
  Result := Clipboard.AsText;
end;

class procedure TAdvClipBoard.SetStream(AStream: TMemoryStream);
{$IFDEF VCLLIB}
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Clipboard.Open;
  try
    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, AStream.Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(AStream.Memory^, DataPtr^, AStream.Size);
        SetClipboardData(CF_FNCSTREAM, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
{$ENDIF}
{$IFDEF LCLLIB}
begin
  Clipboard.Open;
  try
    Clipboard.AddFormat(CF_FNCSTREAM, AStream);
  finally
    Clipboard.Close;
  end;
{$ENDIF}
end;

class function TAdvClipBoard.GetStream: TMemoryStream;
{$IFDEF VCLLIB}
var
  DataPtr: Pointer;
  Data: THandle;
begin
  Result := nil;
  Clipboard.Open;
  try
    Data := GetClipboardData(CF_FNCSTREAM);
    if Data <> 0 then
    begin
      DataPtr := GlobalLock(Data);
      if Assigned(DataPtr) then
      begin
        Result := TMemoryStream.Create;
        Result.WriteBuffer(DataPtr^, GlobalSize(Data));
        Result.Position := 0;
      end;
    end;
  finally
    Clipboard.Close;
  end;
{$ENDIF}
{$IFDEF LCLLIB}
var
  fmt: TClipboardFormat;
begin
  Result := nil;
  fmt := Clipboard.FindFormatID(GetFormat(cfStream));
  if fmt <> 0 then
  begin
    Result := TMemoryStream.Create;
    Clipboard.GetFormat(fmt, Result);
    Result.Position := 0;
  end;
{$ENDIF}
end;

class procedure TAdvClipBoard.SetBitmap(ABitmap: TAdvBitmap);
{$IFDEF VCLLIB}
var
  pic: TGraphic;
  ms, ds: TMemoryStream;
  gcc: TGraphicClass;
begin
  pic := nil;

  if IsBitmapEmpty(ABitmap) then
    Exit;

  ds := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(ds);
    ds.Position := 0;
    if TAdvUtils.FindGraphicClass(ds.Memory^, ds.Size, gcc) then
      pic := gcc.Create;
  finally
    ds.Free;
  end;

  if Assigned(pic) then
  begin
    try
      ms := TMemoryStream.Create;
      try
        ABitmap.SaveToStream(ms);
        ms.Position := 0;
        pic.LoadFromStream(ms);
      finally
        ms.Free;
      end;

      Clipboard.Open;
      try
        Clipboard.Assign(pic);
      finally
        Clipboard.Close;
      end;

    finally
      pic.Free;
    end;
  end;
{$ENDIF}
{$IFDEF LCLLIB}
begin
  if IsBitmapEmpty(ABitmap) then
    Exit;

  ClipBoard.Open;
  try
    ClipBoard.Assign(ABitmap.Graphic);
  finally
    ClipBoard.Close;
  end;
{$ENDIF}
end;

class function TAdvClipBoard.GetBitmap: TAdvBitmap;
begin
  Result := TAdvBitmap.Create;
  Result.Assign(Clipboard);
end;

class procedure TAdvClipBoard.SetRichTextStream(AStream: TMemoryStream);
{$IFDEF VCLLIB}
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Clipboard.Open;
  try
    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, AStream.Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(AStream.Memory^, DataPtr^, AStream.Size);
        SetClipboardData(CF_FNCRICHTEXTSTREAM, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
{$ENDIF}
{$IFDEF LCLLIB}
begin
  Clipboard.Open;
  try
    Clipboard.AddFormat(CF_FNCRICHTEXTSTREAM, AStream);
  finally
    Clipboard.Close;
  end;
{$ENDIF}
end;

class function TAdvClipBoard.GetRichTextStream: TMemoryStream;
{$IFDEF VCLLIB}
var
  DataPtr: Pointer;
  Data: THandle;
begin
  Result := nil;
  Clipboard.Open;
  try
    Data := GetClipboardData(CF_FNCRICHTEXTSTREAM);
    if Data <> 0 then
    begin
      DataPtr := GlobalLock(Data);
      if Assigned(DataPtr) then
      begin
        Result := TMemoryStream.Create;
        Result.WriteBuffer(DataPtr^, GlobalSize(Data));
        Result.Position := 0;
      end;
    end;
  finally
    Clipboard.Close;
  end;
{$ENDIF}
{$IFDEF LCLLIB}
var
  fmt: TClipboardFormat;
begin
  Result := nil;
  fmt := Clipboard.FindFormatID(GetFormat(cfRichTextStream));
  if fmt <> 0 then
  begin
    Result := TMemoryStream.Create;
    Clipboard.GetFormat(fmt, Result);
    Result.Position := 0;
  end;
{$ENDIF}
end;

class procedure TAdvClipBoard.SetRTF(ARTF: String);
{$IFDEF VCLLIB}
var
  rtfstr: ansistring;
  MemHandleRTF: THandle;
begin
  Clipboard.Open;
  try
    rtfstr := ansistring(ARTF);

    MemHandleRTF := GlobalAlloc(GHND or GMEM_SHARE, Length(rtfstr) + 1);

    if (MemHandleRTF <> 0) then
    begin
      {$HINTS OFF}
      {$IF COMPILERVERSION > 24}
      AnsiStrings.StrCopy(GlobalLock(MemHandleRTF), PAnsiChar(rtfstr));
      {$ELSE}
      StrCopy(PAnsiChar(GlobalLock(MemHandleRTF)), PAnsiChar(rtfstr));
      {$IFEND}
      {$HINTS ON}
      GlobalUnlock(MemHandleRTF);
      SetClipboardData(CF_FNCRTF, MemHandleRTF);
    end;
  finally
    Clipboard.Close;
  end;
{$ENDIF}
{$IFDEF LCLLIB}
begin
  Clipboard.Open;
  try
    Clipboard.AddFormat(CF_FNCRTF, ARTF[1], Length(ARTF));
  finally
    Clipboard.Close;
  end;
{$ENDIF}
end;

class function TAdvClipBoard.GetRTF: String;
{$IFDEF VCLLIB}
var
  hClip: THandle;
  bufptr: Pointer;
  mstream: TStringStream;
begin
  Result := '';
  Clipboard.Open;
  try
    HClip := Clipboard.GetAsHandle(CF_FNCRTF);
    bufptr := GlobalLock(HClip);
    if bufptr <> nil then
    begin
      try
        mstream := TStringStream.Create('');
        try
          mstream.WriteBuffer(bufptr^, GlobalSize(HClip));
          Result := mstream.DataString;
        finally
          mstream.Free;
        end;
      finally
        GlobalUnlock(HClip);
      end;
    end;
  finally
    Clipboard.Close;
  end;
{$ENDIF}
{$IFDEF LCLLIB}
var
  fmt: TClipboardFormat;
  ms: TMemoryStream;
  sz: int64;
begin
  Result := '';
  fmt := Clipboard.FindFormatID(GetFormat(cfRTF));
  if fmt <> 0 then
  begin
    ms := TMemoryStream.Create;
    try
      if Clipboard.GetFormat(fmt, ms) then
      begin
        sz := ms.Size;
        if (sz > 0) and (PChar(ms.Memory)[sz - 1]=#0) then
          Dec(sz);

        ms.Position := 0;
        SetLength(Result, sz);
        if sz > 0 then
          ms.Read(Result[1], sz);
      end;
    finally
      ms.Free;
    end;
  end;
{$ENDIF}
end;

class procedure TAdvClipBoard.SetHTML(AHTML: String);
{$IFDEF VCLLIB}
var
  htmlstr: ansistring;
  MemHandleHTML: THandle;
begin
  htmlstr := AnsiString(AHTML);
  htmlstr := AnsiString('Version:0.9'#13#10
    +'StartHTML:71'#13#10
    +'EndHTML:'+inttostr(length(htmlstr) + 71)+#13#10
    +'StartFragment:140'#13#10
    +'EndFragment:'+inttostr(length(htmlstr)+ 142)+#13#10
    +'<!DOCTYPE>' +#13#10
    +'<HTML>'#13#10
    +'<HEAD>'#13#10
    +'<TITLE>The HTML Clipboard</TITLE>'#13#10
    +'<BASE HREF="http://sample/specs">'#13#10
    +'</HEAD>'#13#10
    +'<BODY>'#13#10
    +'<!--StartFragment -->'#13#10)
    + htmlstr + AnsiString(#13#10
    +'<!--StartFragment -->'#13#10
    +'</BODY>'#13#10
    +'</HTML>'#13#10);

  Clipboard.Open;
  try
    MemHandleHTML := GlobalAlloc(GHND or GMEM_SHARE, Length(htmlstr) + 1);

    if (MemHandleHTML <> 0) then
    begin
      {$HINTS OFF}
      {$IF COMPILERVERSION > 24}
      AnsiStrings.StrCopy(GlobalLock(MemHandleHTML), PAnsiChar(htmlstr));
      {$ELSE}
      StrCopy(PAnsiChar(GlobalLock(MemHandleHTML)), PAnsiChar(htmlstr));
      {$IFEND}
      {$HINTS ON}
      GlobalUnlock(MemHandleHTML);
      SetClipboardData(CF_FNCHTML, MemHandleHTML);
    end;
  finally
    Clipboard.Close;
  end;
{$ENDIF}
{$IFDEF LCLLIB}
{$IFDEF MSWINDOWS}
const
  nhdr = 'Version:0.9' + #13#10 +
                 'StartHTML:%.10d' + #13#10+
                 'EndHTML:%.10d' + #13#10+
                 'StartFragment:%.10d' + #13#10+
                 'EndFragment:%.10d' + #13#10;
  hdr = '<html><head></head><body><!--StartFragment-->';
  ftr1 = '<!--EndFragment-->';
  ftr2 = '</body></html>';
{$ENDIF}
var
  HTMLSource : String;
  {$IFDEF MSWINDOWS}
  iStartHTML: Integer;
  iStartFragment: Integer;
  iEndFragment: Integer;
  iEndHTML: Integer;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  iStartHTML := 105;
  iStartFragment := iStartHTML + Length(nhdr);
  iEndFragment := iStartFragment + Length(AHTML) + Length(ftr1);
  iEndHTML := iEndFragment + Length(ftr2);
  HTMLSource := Format(nhdr, [iStartHTML, iEndHTML, iStartFragment,
    iEndFragment]) + hdr + AHTML + ftr1 + ftr2;
  {$ELSE}
  HTMLSource := AHTML;
  {$ENDIF}

  Clipboard.Open;
  try
    Clipboard.AddFormat(CF_FNCHTML, HTMLSource[1], Length(HTMLSource));
  finally
    Clipboard.Close;
  end;
{$ENDIF}
end;

class function TAdvClipBoard.GetHTML: String;
{$IFDEF VCLLIB}
var
  hclip: THandle;
  bufptr: Pointer;
  mstream: TStringStream;
begin
  Result := '';
  Clipboard.Open;
  try
    HClip := Clipboard.GetAsHandle(CF_FNCHTML);
    bufptr := GlobalLock(HClip);
    if bufptr <> nil then
    begin
      try
        mstream := TStringStream.Create('');
        try
          mstream.WriteBuffer(bufptr^, GlobalSize(HClip));
          Result := UTF8ToString(RawByteString(mstream.DataString));
        finally
          mstream.Free;
        end;
      finally
        GlobalUnlock(HClip);
      end;
    end;
  finally
    Clipboard.Close;
  end;
{$ENDIF}
{$IFDEF LCLLIB}
var
  fmt: TClipboardFormat;
  ms: TMemoryStream;
  sz: int64;
begin
  Result := '';
  fmt := Clipboard.FindFormatID(GetFormat(cfHTML));
  if fmt <> 0 then
  begin
    ms := TMemoryStream.Create;
    try
      if Clipboard.GetFormat(fmt, ms) then
      begin
        sz := ms.Size;
        if (sz > 0) and (PChar(ms.Memory)[sz-1]=#0) then
          Dec(sz);

        ms.Position := 0;
        SetLength(Result, sz);
        if sz > 0 then
          ms.Read(Result[1], sz);
      end;
    finally
      ms.Free;
    end;
  end;
{$ENDIF}
end;

{$ENDIF}

class function TAdvClipBoard.HasContent: Boolean;
begin
  Result := HasFormat(TAdvClipBoardFormat.cfText)
  or HasFormat(TAdvClipBoardFormat.cfRTF)
  or HasFormat(TAdvClipBoardFormat.cfBitmap)
  or HasFormat(TAdvClipBoardFormat.cfBitmapWin)
  or HasFormat(TAdvClipBoardFormat.cfRichTextStream)
  or HasFormat(TAdvClipBoardFormat.cfStream);
end;

{$IFDEF ANDROID}
procedure TAdvUtilsDialogSelectFileListener.onClick(dialog: JDialogInterface; which: Integer);
var
  f: JFile;
  s: JCharSequence;
  fn: string;
begin
  s := FFileList.Items[which];
  if JCharSequenceToStr(s) = '..' then
    f := FCurrentPath.getParentFile
  else
    f := TJFile.JavaClass.init(s.toString);

  if f.isDirectory then
  begin
    if FFileMode = ufmSave then
      FSelectedFile := JStringToString(f.toString);

    LoadFileList(f, FFilter);
    FDialog.cancel;
    FDialog.dismiss;
    CallInUIThreadAndWaitFinishing(
    procedure
    begin
      ShowDialog(f);
    end
    );
  end
  else
  begin
    FSelectedFile := JStringToString(f.toString);
    fn := FSelectedFile;
    if FFileMode = ufmSave then
    begin
      if ExtractFileExt(FSelectedFile) = '' then
        fn := TAdvUtils.AddBackslash(fn) + FFileExt;
    end;

    FSelectedFile := fn;

    if Assigned(FCallBack) then
    begin
      FCallBack(fn, True);
      FCallBack := nil;
    end;
  end;
end;

procedure TAdvUtilsDialogDismissListener.onDismiss(dialog: JDialogInterface);
begin
  if Assigned(FCallBack) then
  begin
    FCallBack('', False);
    FCallBack := nil;
  end;
end;

procedure TAdvUtilsDialogSelectDirectoryListener.onClick(dialog: JDialogInterface; which: Integer);
begin
  if Assigned(FCallBack) then
  begin
    FCallBack(FSelectedFile, true);
    FCallBack := nil;
  end;
end;
{$ENDIF}

{$IFDEF IOS}

{ TUIDocumentPickerDelegate }

procedure TUIDocumentPickerDelegate.documentPicker(
  controller: UIDocumentPickerViewController; didPickDocumentsAtURLs: NSArray);
var
  fn: string;
begin
  if didPickDocumentsAtURLs.count > 0 then
  begin
    FSelectedFile := UTF8ToString(TNSURL.Wrap(didPickDocumentsAtURLs.objectAtIndex(0)).path.UTF8String);
    fn := FSelectedFile;
    if FFileMode = ufmSave then
    begin
      if ExtractFileExt(FSelectedFile) = '' then
        fn := TAdvUtils.AddBackslash(fn) + FFileExt;
    end;

    if Assigned(FCallBack) then
    begin
      FCallBack(fn, True);
      FCallBack := nil;
    end;
  end;
end;

procedure TUIDocumentPickerDelegate.documentPickerWasCancelled(
  controller: UIDocumentPickerViewController);
begin
  if Assigned(FCallBack) then
  begin
    FCallBack('', False);
    FCallBack := nil;
  end;
end;
{$ENDIF}

{$IFDEF FNCLIB}
procedure RegisterMimeTypes;
  procedure AddType(AExt, AMimeType: string);
  begin
    FMimeTypes.Add(AExt + FMimeTypes.NameValueSeparator + AMimeType);
  end;
begin
  AddType('ez', 'application/andrew-inset');
  AddType('aw', 'application/applixware');
  AddType('atom', 'application/atom+xml');
  AddType('atomcat', 'application/atomcat+xml');
  AddType('atomsvc', 'application/atomsvc+xml');
  AddType('bson', 'application/bson');
  AddType('ccxml', 'application/ccxml+xml');
  AddType('cdmia', 'application/cdmi-capability');
  AddType('cdmic', 'application/cdmi-container');
  AddType('cdmid', 'application/cdmi-domain');
  AddType('cdmio', 'application/cdmi-object');
  AddType('cdmiq', 'application/cdmi-queue');
  AddType('cu', 'application/cu-seeme');
  AddType('davmount', 'application/davmount+xml');
  AddType('dbk', 'application/docbook+xml');
  AddType('dssc', 'application/dssc+der');
  AddType('xdssc', 'application/dssc+xml');
  AddType('ecma', 'application/ecmascript');
  AddType('emma', 'application/emma+xml');
  AddType('epub', 'application/epub+zip');
  AddType('exi', 'application/exi');
  AddType('pfr', 'application/font-tdpfr');
  AddType('gml', 'application/gml+xml');
  AddType('gpx', 'application/gpx+xml');
  AddType('gxf', 'application/gxf');
  AddType('stk', 'application/hyperstudio');
  AddType('ink', 'application/inkml+xml');
  AddType('inkml', 'application/inkml+xml');
  AddType('ipfix', 'application/ipfix');
  AddType('jar', 'application/java-archive');
  AddType('ser', 'application/java-serialized-object');
  AddType('class', 'application/java-vm');
  AddType('js', 'application/javascript');
  AddType('json', 'application/json');
  AddType('jsonml', 'application/jsonml+json');
  AddType('lostxml', 'application/lost+xml');
  AddType('hqx', 'application/mac-binhex40');
  AddType('cpt', 'application/mac-compactpro');
  AddType('mads', 'application/mads+xml');
  AddType('mrc', 'application/marc');
  AddType('mrcx', 'application/marcxml+xml');
  AddType('ma', 'application/mathematica');
  AddType('nb', 'application/mathematica');
  AddType('mb', 'application/mathematica');
  AddType('mathml', 'application/mathml+xml');
  AddType('mbox', 'application/mbox');
  AddType('mscml', 'application/mediaservercontrol+xml');
  AddType('metalink', 'application/metalink+xml');
  AddType('meta4', 'application/metalink4+xml');
  AddType('mets', 'application/mets+xml');
  AddType('mods', 'application/mods+xml');
  AddType('m21', 'application/mp21');
  AddType('mp21', 'application/mp21');
  AddType('mp4s', 'application/mp4');
  AddType('doc', 'application/msword');
  AddType('dot', 'application/msword');
  AddType('mxf', 'application/mxf');
  AddType('bin', 'application/octet-stream');
  AddType('bpk', 'application/octet-stream');
  AddType('class', 'application/octet-stream');
  AddType('deploy', 'application/octet-stream');
  AddType('dist', 'application/octet-stream');
  AddType('distz', 'application/octet-stream');
  AddType('dmg', 'application/octet-stream');
  AddType('dms', 'application/octet-stream');
  AddType('dump', 'application/octet-stream');
  AddType('elc', 'application/octet-stream');
  AddType('iso', 'application/octet-stream');
  AddType('lha', 'application/octet-stream');
  AddType('lrf', 'application/octet-stream');
  AddType('lzh', 'application/octet-stream');
  AddType('mar', 'application/octet-stream');
  AddType('pkg', 'application/octet-stream');
  AddType('so', 'application/octet-stream');
  AddType('oda', 'application/oda');
  AddType('opf', 'application/oebps-package+xml');
  AddType('ogx', 'application/ogg');
  AddType('omdoc', 'application/omdoc+xml');
  AddType('onetoc', 'application/onenote');
  AddType('onetoc2', 'application/onenote');
  AddType('onetmp', 'application/onenote');
  AddType('onepkg', 'application/onenote');
  AddType('oxps', 'application/oxps');
  AddType('xer', 'application/patch-ops-error+xml');
  AddType('pdf', 'application/pdf');
  AddType('pgp', 'application/pgp-encrypted');
  AddType('asc', 'application/pgp-signature');
  AddType('sig', 'application/pgp-signature');
  AddType('prf', 'application/pics-rules');
  AddType('p10', 'application/pkcs10');
  AddType('p7m', 'application/pkcs7-mime');
  AddType('p7c', 'application/pkcs7-mime');
  AddType('p7s', 'application/pkcs7-signature');
  AddType('p8', 'application/pkcs8');
  AddType('ac', 'application/pkix-attr-cert');
  AddType('cer', 'application/pkix-cert');
  AddType('crl', 'application/pkix-crl');
  AddType('pkipath', 'application/pkix-pkipath');
  AddType('pki', 'application/pkixcmp');
  AddType('pls', 'application/pls+xml');
  AddType('ai', 'application/postscript');
  AddType('eps', 'application/postscript');
  AddType('ps', 'application/postscript');
  AddType('cww', 'application/prs.cww');
  AddType('pskcxml', 'application/pskc+xml');
  AddType('rdf', 'application/rdf+xml');
  AddType('rif', 'application/reginfo+xml');
  AddType('rnc', 'application/relax-ng-compact-syntax');
  AddType('rl', 'application/resource-lists+xml');
  AddType('rld', 'application/resource-lists-diff+xml');
  AddType('rs', 'application/rls-services+xml');
  AddType('gbr', 'application/rpki-ghostbusters');
  AddType('mft', 'application/rpki-manifest');
  AddType('roa', 'application/rpki-roa');
  AddType('rsd', 'application/rsd+xml');
  AddType('rss', 'application/rss+xml');
  AddType('rtf', 'application/rtf');
  AddType('sbml', 'application/sbml+xml');
  AddType('scq', 'application/scvp-cv-request');
  AddType('scs', 'application/scvp-cv-response');
  AddType('spq', 'application/scvp-vp-request');
  AddType('spp', 'application/scvp-vp-response');
  AddType('sdp', 'application/sdp');
  AddType('setpay', 'application/set-payment-initiation');
  AddType('setreg', 'application/set-registration-initiation');
  AddType('shf', 'application/shf+xml');
  AddType('smi', 'application/smil+xml');
  AddType('smil', 'application/smil+xml');
  AddType('soap', 'application/soap+xml');
  AddType('rq', 'application/sparql-query');
  AddType('srx', 'application/sparql-results+xml');
  AddType('gram', 'application/srgs');
  AddType('grxml', 'application/srgs+xml');
  AddType('sru', 'application/sru+xml');
  AddType('ssdl', 'application/ssdl+xml');
  AddType('ssml', 'application/ssml+xml');
  AddType('tei', 'application/tei+xml');
  AddType('teicorpus', 'application/tei+xml');
  AddType('tfi', 'application/thraud+xml');
  AddType('tsd', 'application/timestamped-data');
  AddType('plb', 'application/vnd.3gpp.pic-bw-large');
  AddType('psb', 'application/vnd.3gpp.pic-bw-small');
  AddType('pvb', 'application/vnd.3gpp.pic-bw-var');
  AddType('tcap', 'application/vnd.3gpp2.tcap');
  AddType('pwn', 'application/vnd.3m.post-it-notes');
  AddType('aso', 'application/vnd.accpac.simply.aso');
  AddType('imp', 'application/vnd.accpac.simply.imp');
  AddType('acu', 'application/vnd.acucobol');
  AddType('atc', 'application/vnd.acucorp');
  AddType('acutc', 'application/vnd.acucorp');
  AddType('air', 'application/vnd.adobe.air-application-installer-package+zip');
  AddType('fcdt', 'application/vnd.adobe.formscentral.fcdt');
  AddType('fxp', 'application/vnd.adobe.fxp');
  AddType('fxpl', 'application/vnd.adobe.fxp');
  AddType('xdp', 'application/vnd.adobe.xdp+xml');
  AddType('xfdf', 'application/vnd.adobe.xfdf');
  AddType('ahead', 'application/vnd.ahead.space');
  AddType('azf', 'application/vnd.airzip.filesecure.azf');
  AddType('azs', 'application/vnd.airzip.filesecure.azs');
  AddType('azw', 'application/vnd.amazon.ebook');
  AddType('acc', 'application/vnd.americandynamics.acc');
  AddType('ami', 'application/vnd.amiga.ami');
  AddType('apk', 'application/vnd.android.package-archive');
  AddType('cii', 'application/vnd.anser-web-certificate-issue-initiation');
  AddType('fti', 'application/vnd.anser-web-funds-transfer-initiation');
  AddType('atx', 'application/vnd.antix.game-component');
  AddType('mpkg', 'application/vnd.apple.installer+xml');
  AddType('m3u8', 'application/vnd.apple.mpegurl');
  AddType('swi', 'application/vnd.aristanetworks.swi');
  AddType('iota', 'application/vnd.astraea-software.iota');
  AddType('aep', 'application/vnd.audiograph');
  AddType('mpm', 'application/vnd.blueice.multipass');
  AddType('bmi', 'application/vnd.bmi');
  AddType('rep', 'application/vnd.businessobjects');
  AddType('cdxml', 'application/vnd.chemdraw+xml');
  AddType('mmd', 'application/vnd.chipnuts.karaoke-mmd');
  AddType('cdy', 'application/vnd.cinderella');
  AddType('cla', 'application/vnd.claymore');
  AddType('rp9', 'application/vnd.cloanto.rp9');
  AddType('c4g', 'application/vnd.clonk.c4group');
  AddType('c4d', 'application/vnd.clonk.c4group');
  AddType('c4f', 'application/vnd.clonk.c4group');
  AddType('c4p', 'application/vnd.clonk.c4group');
  AddType('c4u', 'application/vnd.clonk.c4group');
  AddType('c11amc', 'application/vnd.cluetrust.cartomobile-config');
  AddType('c11amz', 'application/vnd.cluetrust.cartomobile-config-pkg');
  AddType('csp', 'application/vnd.commonspace');
  AddType('cdbcmsg', 'application/vnd.contact.cmsg');
  AddType('cmc', 'application/vnd.cosmocaller');
  AddType('clkx', 'application/vnd.crick.clicker');
  AddType('clkk', 'application/vnd.crick.clicker.keyboard');
  AddType('clkp', 'application/vnd.crick.clicker.palette');
  AddType('clkt', 'application/vnd.crick.clicker.template');
  AddType('clkw', 'application/vnd.crick.clicker.wordbank');
  AddType('wbs', 'application/vnd.criticaltools.wbs+xml');
  AddType('pml', 'application/vnd.ctc-posml');
  AddType('ppd', 'application/vnd.cups-ppd');
  AddType('car', 'application/vnd.curl.car');
  AddType('pcurl', 'application/vnd.curl.pcurl');
  AddType('dart', 'application/vnd.dart');
  AddType('rdz', 'application/vnd.data-vision.rdz');
  AddType('uvf', 'application/vnd.dece.data');
  AddType('uvvf', 'application/vnd.dece.data');
  AddType('uvd', 'application/vnd.dece.data');
  AddType('uvvd', 'application/vnd.dece.data');
  AddType('uvt', 'application/vnd.dece.ttml+xml');
  AddType('uvvt', 'application/vnd.dece.ttml+xml');
  AddType('uvx', 'application/vnd.dece.unspecified');
  AddType('uvvx', 'application/vnd.dece.unspecified');
  AddType('uvz', 'application/vnd.dece.zip');
  AddType('uvvz', 'application/vnd.dece.zip');
  AddType('fe_launch', 'application/vnd.denovo.fcselayout-link');
  AddType('dna', 'application/vnd.dna');
  AddType('mlp', 'application/vnd.dolby.mlp');
  AddType('dpg', 'application/vnd.dpgraph');
  AddType('dfac', 'application/vnd.dreamfactory');
  AddType('kpxx', 'application/vnd.ds-keypoint');
  AddType('ait', 'application/vnd.dvb.ait');
  AddType('svc', 'application/vnd.dvb.service');
  AddType('geo', 'application/vnd.dynageo');
  AddType('mag', 'application/vnd.ecowin.chart');
  AddType('nml', 'application/vnd.enliven');
  AddType('esf', 'application/vnd.epson.esf');
  AddType('msf', 'application/vnd.epson.msf');
  AddType('qam', 'application/vnd.epson.quickanime');
  AddType('slt', 'application/vnd.epson.salt');
  AddType('ssf', 'application/vnd.epson.ssf');
  AddType('es3', 'application/vnd.eszigno3+xml');
  AddType('et3', 'application/vnd.eszigno3+xml');
  AddType('ez2', 'application/vnd.ezpix-album');
  AddType('ez3', 'application/vnd.ezpix-package');
  AddType('fdf', 'application/vnd.fdf');
  AddType('mseed', 'application/vnd.fdsn.mseed');
  AddType('seed', 'application/vnd.fdsn.seed');
  AddType('dataless', 'application/vnd.fdsn.seed');
  AddType('json', 'application/vnd.firedac.json');
  AddType('xml', 'application/vnd.firedac.xml');
  AddType('bin', 'application/vnd.firedac.bin');
  AddType('gph', 'application/vnd.flographit');
  AddType('ftc', 'application/vnd.fluxtime.clip');
  AddType('fm', 'application/vnd.framemaker');
  AddType('frame', 'application/vnd.framemaker');
  AddType('maker', 'application/vnd.framemaker');
  AddType('book', 'application/vnd.framemaker');
  AddType('fnc', 'application/vnd.frogans.fnc');
  AddType('ltf', 'application/vnd.frogans.ltf');
  AddType('fsc', 'application/vnd.fsc.weblaunch');
  AddType('oas', 'application/vnd.fujitsu.oasys');
  AddType('oa2', 'application/vnd.fujitsu.oasys2');
  AddType('oa3', 'application/vnd.fujitsu.oasys3');
  AddType('fg5', 'application/vnd.fujitsu.oasysgp');
  AddType('bh2', 'application/vnd.fujitsu.oasysprs');
  AddType('ddd', 'application/vnd.fujixerox.ddd');
  AddType('xdw', 'application/vnd.fujixerox.docuworks');
  AddType('xbd', 'application/vnd.fujixerox.docuworks.binder');
  AddType('fzs', 'application/vnd.fuzzysheet');
  AddType('txd', 'application/vnd.genomatix.tuxedo');
  AddType('ggb', 'application/vnd.geogebra.file');
  AddType('ggt', 'application/vnd.geogebra.tool');
  AddType('gex', 'application/vnd.geometry-explorer');
  AddType('gre', 'application/vnd.geometry-explorer');
  AddType('gxt', 'application/vnd.geonext');
  AddType('g2w', 'application/vnd.geoplan');
  AddType('g3w', 'application/vnd.geospace');
  AddType('gmx', 'application/vnd.gmx');
  AddType('kml', 'application/vnd.google-earth.kml+xml');
  AddType('kmz', 'application/vnd.google-earth.kmz');
  AddType('gqf', 'application/vnd.grafeq');
  AddType('gqs', 'application/vnd.grafeq');
  AddType('gac', 'application/vnd.groove-account');
  AddType('ghf', 'application/vnd.groove-help');
  AddType('gim', 'application/vnd.groove-identity-message');
  AddType('grv', 'application/vnd.groove-injector');
  AddType('gtm', 'application/vnd.groove-tool-message');
  AddType('tpl', 'application/vnd.groove-tool-template');
  AddType('vcg', 'application/vnd.groove-vcard');
  AddType('hal', 'application/vnd.hal+xml');
  AddType('zmm', 'application/vnd.handheld-entertainment+xml');
  AddType('hbci', 'application/vnd.hbci');
  AddType('les', 'application/vnd.hhe.lesson-player');
  AddType('hpgl', 'application/vnd.hp-hpgl');
  AddType('hpid', 'application/vnd.hp-hpid');
  AddType('hps', 'application/vnd.hp-hps');
  AddType('jlt', 'application/vnd.hp-jlyt');
  AddType('pcl', 'application/vnd.hp-pcl');
  AddType('pclxl', 'application/vnd.hp-pclxl');
  AddType('sfd-hdstx', 'application/vnd.hydrostatix.sof-data');
  AddType('mpy', 'application/vnd.ibm.minipay');
  AddType('afp', 'application/vnd.ibm.modcap');
  AddType('listafp', 'application/vnd.ibm.modcap');
  AddType('list3820', 'application/vnd.ibm.modcap');
  AddType('irm', 'application/vnd.ibm.rights-management');
  AddType('sc', 'application/vnd.ibm.secure-container');
  AddType('icc', 'application/vnd.iccprofile');
  AddType('icm', 'application/vnd.iccprofile');
  AddType('igl', 'application/vnd.igloader');
  AddType('ivp', 'application/vnd.immervision-ivp');
  AddType('ivu', 'application/vnd.immervision-ivu');
  AddType('igm', 'application/vnd.insors.igm');
  AddType('xpw', 'application/vnd.intercon.formnet');
  AddType('xpx', 'application/vnd.intercon.formnet');
  AddType('i2g', 'application/vnd.intergeo');
  AddType('qbo', 'application/vnd.intu.qbo');
  AddType('qfx', 'application/vnd.intu.qfx');
  AddType('rcprofile', 'application/vnd.ipunplugged.rcprofile');
  AddType('irp', 'application/vnd.irepository.package+xml');
  AddType('xpr', 'application/vnd.is-xpr');
  AddType('fcs', 'application/vnd.isac.fcs');
  AddType('jam', 'application/vnd.jam');
  AddType('rms', 'application/vnd.jcp.javame.midlet-rms');
  AddType('jisp', 'application/vnd.jisp');
  AddType('joda', 'application/vnd.joost.joda-archive');
  AddType('ktz', 'application/vnd.kahootz');
  AddType('ktr', 'application/vnd.kahootz');
  AddType('karbon', 'application/vnd.kde.karbon');
  AddType('chrt', 'application/vnd.kde.kchart');
  AddType('kfo', 'application/vnd.kde.kformula');
  AddType('flw', 'application/vnd.kde.kivio');
  AddType('kon', 'application/vnd.kde.kontour');
  AddType('kpr', 'application/vnd.kde.kpresenter');
  AddType('kpt', 'application/vnd.kde.kpresenter');
  AddType('ksp', 'application/vnd.kde.kspread');
  AddType('kwd', 'application/vnd.kde.kword');
  AddType('kwt', 'application/vnd.kde.kword');
  AddType('htke', 'application/vnd.kenameaapp');
  AddType('kia', 'application/vnd.kidspiration');
  AddType('kne', 'application/vnd.kinar');
  AddType('knp', 'application/vnd.kinar');
  AddType('skp', 'application/vnd.koan');
  AddType('skd', 'application/vnd.koan');
  AddType('skt', 'application/vnd.koan');
  AddType('skm', 'application/vnd.koan');
  AddType('sse', 'application/vnd.kodak-descriptor');
  AddType('lasxml', 'application/vnd.las.las+xml');
  AddType('lbd', 'application/vnd.llamagraphics.life-balance.desktop');
  AddType('lbe', 'application/vnd.llamagraphics.life-balance.exchange+xml');
  AddType('123', 'application/vnd.lotus-1-2-3');
  AddType('apr', 'application/vnd.lotus-approach');
  AddType('pre', 'application/vnd.lotus-freelance');
  AddType('nsf', 'application/vnd.lotus-notes');
  AddType('org', 'application/vnd.lotus-organizer');
  AddType('scm', 'application/vnd.lotus-screencam');
  AddType('lwp', 'application/vnd.lotus-wordpro');
  AddType('portpkg', 'application/vnd.macports.portpkg');
  AddType('mcd', 'application/vnd.mcd');
  AddType('mc1', 'application/vnd.medcalcdata');
  AddType('cdkey', 'application/vnd.mediastation.cdkey');
  AddType('mwf', 'application/vnd.mfer');
  AddType('mfm', 'application/vnd.mfmp');
  AddType('flo', 'application/vnd.micrografx.flo');
  AddType('igx', 'application/vnd.micrografx.igx');
  AddType('mif', 'application/vnd.mif');
  AddType('daf', 'application/vnd.mobius.daf');
  AddType('dis', 'application/vnd.mobius.dis');
  AddType('mbk', 'application/vnd.mobius.mbk');
  AddType('mqy', 'application/vnd.mobius.mqy');
  AddType('msl', 'application/vnd.mobius.msl');
  AddType('plc', 'application/vnd.mobius.plc');
  AddType('txf', 'application/vnd.mobius.txf');
  AddType('mpn', 'application/vnd.mophun.application');
  AddType('mpc', 'application/vnd.mophun.certificate');
  AddType('xul', 'application/vnd.mozilla.xul+xml');
  AddType('cil', 'application/vnd.ms-artgalry');
  AddType('cab', 'application/vnd.ms-cab-compressed');
  AddType('xls', 'application/vnd.ms-excel');
  AddType('xlm', 'application/vnd.ms-excel');
  AddType('xla', 'application/vnd.ms-excel');
  AddType('xlc', 'application/vnd.ms-excel');
  AddType('xlt', 'application/vnd.ms-excel');
  AddType('xlw', 'application/vnd.ms-excel');
  AddType('xlam', 'application/vnd.ms-excel.addin.macroenabled.12');
  AddType('xlsb', 'application/vnd.ms-excel.sheet.binary.macroenabled.12');
  AddType('xlsm', 'application/vnd.ms-excel.sheet.macroenabled.12');
  AddType('xltm', 'application/vnd.ms-excel.template.macroenabled.12');
  AddType('eot', 'application/vnd.ms-fontobject');
  AddType('chm', 'application/vnd.ms-htmlhelp');
  AddType('ims', 'application/vnd.ms-ims');
  AddType('lrm', 'application/vnd.ms-lrm');
  AddType('thmx', 'application/vnd.ms-officetheme');
  AddType('cat', 'application/vnd.ms-pki.seccat');
  AddType('stl', 'application/vnd.ms-pki.stl');
  AddType('ppt', 'application/vnd.ms-powerpoint');
  AddType('pps', 'application/vnd.ms-powerpoint');
  AddType('pot', 'application/vnd.ms-powerpoint');
  AddType('ppam', 'application/vnd.ms-powerpoint.addin.macroenabled.12');
  AddType('pptm', 'application/vnd.ms-powerpoint.presentation.macroenabled.12');
  AddType('sldm', 'application/vnd.ms-powerpoint.slide.macroenabled.12');
  AddType('ppsm', 'application/vnd.ms-powerpoint.slideshow.macroenabled.12');
  AddType('potm', 'application/vnd.ms-powerpoint.template.macroenabled.12');
  AddType('mpp', 'application/vnd.ms-project');
  AddType('mpt', 'application/vnd.ms-project');
  AddType('docm', 'application/vnd.ms-word.document.macroenabled.12');
  AddType('dotm', 'application/vnd.ms-word.template.macroenabled.12');
  AddType('wps', 'application/vnd.ms-works');
  AddType('wks', 'application/vnd.ms-works');
  AddType('wcm', 'application/vnd.ms-works');
  AddType('wdb', 'application/vnd.ms-works');
  AddType('wpl', 'application/vnd.ms-wpl');
  AddType('xps', 'application/vnd.ms-xpsdocument');
  AddType('mseq', 'application/vnd.mseq');
  AddType('mus', 'application/vnd.musician');
  AddType('msty', 'application/vnd.muvee.style');
  AddType('taglet', 'application/vnd.mynfc');
  AddType('nlu', 'application/vnd.neurolanguage.nlu');
  AddType('ntf', 'application/vnd.nitf');
  AddType('nitf', 'application/vnd.nitf');
  AddType('nnd', 'application/vnd.noblenet-directory');
  AddType('nns', 'application/vnd.noblenet-sealer');
  AddType('nnw', 'application/vnd.noblenet-web');
  AddType('ngdat', 'application/vnd.nokia.n-gage.data');
  AddType('n-gage', 'application/vnd.nokia.n-gage.symbian.install');
  AddType('rpst', 'application/vnd.nokia.radio-preset');
  AddType('rpss', 'application/vnd.nokia.radio-presets');
  AddType('edm', 'application/vnd.novadigm.edm');
  AddType('edx', 'application/vnd.novadigm.edx');
  AddType('FExt', 'application/vnd.novadigm.FExt');
  AddType('odc', 'application/vnd.oasis.opendocument.chart');
  AddType('otc', 'application/vnd.oasis.opendocument.chart-template');
  AddType('odb', 'application/vnd.oasis.opendocument.database');
  AddType('odf', 'application/vnd.oasis.opendocument.formula');
  AddType('odft', 'application/vnd.oasis.opendocument.formula-template');
  AddType('odg', 'application/vnd.oasis.opendocument.graphics');
  AddType('otg', 'application/vnd.oasis.opendocument.graphics-template');
  AddType('odi', 'application/vnd.oasis.opendocument.image');
  AddType('oti', 'application/vnd.oasis.opendocument.image-template');
  AddType('odp', 'application/vnd.oasis.opendocument.presentation');
  AddType('otp', 'application/vnd.oasis.opendocument.presentation-template');
  AddType('ods', 'application/vnd.oasis.opendocument.spreadsheet');
  AddType('ots', 'application/vnd.oasis.opendocument.spreadsheet-template');
  AddType('odt', 'application/vnd.oasis.opendocument.text');
  AddType('odm', 'application/vnd.oasis.opendocument.text-master');
  AddType('ott', 'application/vnd.oasis.opendocument.text-template');
  AddType('oth', 'application/vnd.oasis.opendocument.text-web');
  AddType('xo', 'application/vnd.olpc-sugar');
  AddType('dd2', 'application/vnd.oma.dd2+xml');
  AddType('oxt', 'application/vnd.openofficeorg.extension');
  AddType('pptx', 'application/vnd.openxmlformats-officedocument.presentationml.presentation');
  AddType('sldx', 'application/vnd.openxmlformats-officedocument.presentationml.slide');
  AddType('ppsx', 'application/vnd.openxmlformats-officedocument.presentationml.slideshow');
  AddType('potx', 'application/vnd.openxmlformats-officedocument.presentationml.template');
  AddType('xlsx', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet');
  AddType('xltx', 'application/vnd.openxmlformats-officedocument.spreadsheetml.template');
  AddType('docx', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document');
  AddType('dotx', 'application/vnd.openxmlformats-officedocument.wordprocessingml.template');
  AddType('mgp', 'application/vnd.osgeo.mapguide.package');
  AddType('dp', 'application/vnd.osgi.dp');
  AddType('esa', 'application/vnd.osgi.subsystem');
  AddType('pdb', 'application/vnd.palm');
  AddType('pqa', 'application/vnd.palm');
  AddType('oprc', 'application/vnd.palm');
  AddType('paw', 'application/vnd.pawaafile');
  AddType('str', 'application/vnd.pg.format');
  AddType('ei6', 'application/vnd.pg.osasli');
  AddType('efif', 'application/vnd.picsel');
  AddType('wg', 'application/vnd.pmi.widget');
  AddType('plf', 'application/vnd.pocketlearn');
  AddType('pbd', 'application/vnd.powerbuilder6');
  AddType('box', 'application/vnd.previewsystems.box');
  AddType('mgz', 'application/vnd.proteus.magazine');
  AddType('qps', 'application/vnd.publishare-delta-tree');
  AddType('ptid', 'application/vnd.pvi.ptid1');
  AddType('qxd', 'application/vnd.quark.quarkxpress');
  AddType('qxt', 'application/vnd.quark.quarkxpress');
  AddType('qwd', 'application/vnd.quark.quarkxpress');
  AddType('qwt', 'application/vnd.quark.quarkxpress');
  AddType('qxl', 'application/vnd.quark.quarkxpress');
  AddType('qxb', 'application/vnd.quark.quarkxpress');
  AddType('bed', 'application/vnd.realvnc.bed');
  AddType('mxl', 'application/vnd.recordare.musicxml');
  AddType('musicxml', 'application/vnd.recordare.musicxml+xml');
  AddType('cryptonote', 'application/vnd.rig.cryptonote');
  AddType('cod', 'application/vnd.rim.cod');
  AddType('rm', 'application/vnd.rn-realmedia');
  AddType('rmvb', 'application/vnd.rn-realmedia-vbr');
  AddType('link66', 'application/vnd.route66.link66+xml');
  AddType('st', 'application/vnd.sailingtracker.track');
  AddType('see', 'application/vnd.seemail');
  AddType('sema', 'application/vnd.sema');
  AddType('semd', 'application/vnd.semd');
  AddType('semf', 'application/vnd.semf');
  AddType('ifm', 'application/vnd.shana.informed.formdata');
  AddType('itp', 'application/vnd.shana.informed.formtemplate');
  AddType('iif', 'application/vnd.shana.informed.interchange');
  AddType('ipk', 'application/vnd.shana.informed.package');
  AddType('twd', 'application/vnd.simtech-mindmapper');
  AddType('twds', 'application/vnd.simtech-mindmapper');
  AddType('mmf', 'application/vnd.smaf');
  AddType('teacher', 'application/vnd.smart.teacher');
  AddType('sdkm', 'application/vnd.solent.sdkm+xml');
  AddType('sdkd', 'application/vnd.solent.sdkm+xml');
  AddType('dxp', 'application/vnd.spotfire.dxp');
  AddType('sfs', 'application/vnd.spotfire.sfs');
  AddType('sdc', 'application/vnd.stardivision.calc');
  AddType('sda', 'application/vnd.stardivision.draw');
  AddType('sdd', 'application/vnd.stardivision.impress');
  AddType('smf', 'application/vnd.stardivision.math');
  AddType('sdw', 'application/vnd.stardivision.writer');
  AddType('vor', 'application/vnd.stardivision.writer');
  AddType('sgl', 'application/vnd.stardivision.writer-global');
  AddType('smzip', 'application/vnd.stepmania.package');
  AddType('sm', 'application/vnd.stepmania.stepchart');
  AddType('sxc', 'application/vnd.sun.xml.calc');
  AddType('stc', 'application/vnd.sun.xml.calc.template');
  AddType('sxd', 'application/vnd.sun.xml.draw');
  AddType('std', 'application/vnd.sun.xml.draw.template');
  AddType('sxi', 'application/vnd.sun.xml.impress');
  AddType('sti', 'application/vnd.sun.xml.impress.template');
  AddType('sxm', 'application/vnd.sun.xml.math');
  AddType('sxw', 'application/vnd.sun.xml.writer');
  AddType('sxg', 'application/vnd.sun.xml.writer.global');
  AddType('stw', 'application/vnd.sun.xml.writer.template');
  AddType('sus', 'application/vnd.sus-calendar');
  AddType('susp', 'application/vnd.sus-calendar');
  AddType('svd', 'application/vnd.svd');
  AddType('sis', 'application/vnd.symbian.install');
  AddType('sisx', 'application/vnd.symbian.install');
  AddType('xsm', 'application/vnd.syncml+xml');
  AddType('bdm', 'application/vnd.syncml.dm+wbxml');
  AddType('xdm', 'application/vnd.syncml.dm+xml');
  AddType('tao', 'application/vnd.tao.intent-module-archive');
  AddType('pcap', 'application/vnd.tcpdump.pcap');
  AddType('cap', 'application/vnd.tcpdump.pcap');
  AddType('dmp', 'application/vnd.tcpdump.pcap');
  AddType('tmo', 'application/vnd.tmobile-livetv');
  AddType('tpt', 'application/vnd.trid.tpt');
  AddType('mxs', 'application/vnd.triscape.mxs');
  AddType('tra', 'application/vnd.trueapp');
  AddType('ufd', 'application/vnd.ufdl');
  AddType('ufdl', 'application/vnd.ufdl');
  AddType('utz', 'application/vnd.uiq.theme');
  AddType('umj', 'application/vnd.umajin');
  AddType('unityweb', 'application/vnd.unity');
  AddType('uoml', 'application/vnd.uoml+xml');
  AddType('vcx', 'application/vnd.vcx');
  AddType('vsd', 'application/vnd.visio');
  AddType('vst', 'application/vnd.visio');
  AddType('vss', 'application/vnd.visio');
  AddType('vsw', 'application/vnd.visio');
  AddType('vis', 'application/vnd.visionary');
  AddType('vsf', 'application/vnd.vsf');
  AddType('wbxml', 'application/vnd.wap.wbxml');
  AddType('wmlc', 'application/vnd.wap.wmlc');
  AddType('wmlsc', 'application/vnd.wap.wmlscriptc');
  AddType('wtb', 'application/vnd.webturbo');
  AddType('nbp', 'application/vnd.wolfram.player');
  AddType('wpd', 'application/vnd.wordperfect');
  AddType('wqd', 'application/vnd.wqd');
  AddType('stf', 'application/vnd.wt.stf');
  AddType('xar', 'application/vnd.xara');
  AddType('xfdl', 'application/vnd.xfdl');
  AddType('hvd', 'application/vnd.yamaha.hv-dic');
  AddType('hvs', 'application/vnd.yamaha.hv-script');
  AddType('hvp', 'application/vnd.yamaha.hv-voice');
  AddType('osf', 'application/vnd.yamaha.openscoreformat');
  AddType('osfpvg', 'application/vnd.yamaha.openscoreformat.osfpvg+xml');
  AddType('saf', 'application/vnd.yamaha.smaf-audio');
  AddType('spf', 'application/vnd.yamaha.smaf-phrase');
  AddType('cmp', 'application/vnd.yellowriver-custom-menu');
  AddType('zir', 'application/vnd.zul');
  AddType('zirz', 'application/vnd.zul');
  AddType('zaz', 'application/vnd.zzazz.deck+xml');
  AddType('vxml', 'application/voicexml+xml');
  AddType('wgt', 'application/widget');
  AddType('hlp', 'application/winhlp');
  AddType('wsdl', 'application/wsdl+xml');
  AddType('wspolicy', 'application/wspolicy+xml');
  AddType('7z', 'application/x-7z-compressed');
  AddType('abw', 'application/x-abiword');
  AddType('ace', 'application/x-ace-compressed');
  AddType('dmg', 'application/x-apple-diskimage');
  AddType('aab', 'application/x-authorware-bin');
  AddType('x32', 'application/x-authorware-bin');
  AddType('u32', 'application/x-authorware-bin');
  AddType('vox', 'application/x-authorware-bin');
  AddType('aam', 'application/x-authorware-map');
  AddType('aas', 'application/x-authorware-seg');
  AddType('bcpio', 'application/x-bcpio');
  AddType('torrent', 'application/x-bittorrent');
  AddType('blb', 'application/x-blorb');
  AddType('blorb', 'application/x-blorb');
  AddType('bz', 'application/x-bzip');
  AddType('bz2', 'application/x-bzip2');
  AddType('boz', 'application/x-bzip2');
  AddType('cbr', 'application/x-cbr');
  AddType('cba', 'application/x-cbr');
  AddType('cbt', 'application/x-cbr');
  AddType('cbz', 'application/x-cbr');
  AddType('cb7', 'application/x-cbr');
  AddType('vcd', 'application/x-cdlink');
  AddType('cfs', 'application/x-cfs-compressed');
  AddType('chat', 'application/x-chat');
  AddType('pgn', 'application/x-chess-pgn');
  AddType('nsc', 'application/x-conference');
  AddType('cpio', 'application/x-cpio');
  AddType('csh', 'application/x-csh');
  AddType('deb', 'application/x-debian-package');
  AddType('udeb', 'application/x-debian-package');
  AddType('dgc', 'application/x-dgc-compressed');
  AddType('dir', 'application/x-director');
  AddType('dcr', 'application/x-director');
  AddType('dxr', 'application/x-director');
  AddType('cst', 'application/x-director');
  AddType('cct', 'application/x-director');
  AddType('cxt', 'application/x-director');
  AddType('w3d', 'application/x-director');
  AddType('fgd', 'application/x-director');
  AddType('swa', 'application/x-director');
  AddType('wad', 'application/x-doom');
  AddType('ncx', 'application/x-dtbncx+xml');
  AddType('dtb', 'application/x-dtbook+xml');
  AddType('res', 'application/x-dtbresource+xml');
  AddType('dvi', 'application/x-dvi');
  AddType('evy', 'application/x-envoy');
  AddType('eva', 'application/x-eva');
  AddType('bdf', 'application/x-font-bdf');
  AddType('gsf', 'application/x-font-ghostscript');
  AddType('psf', 'application/x-font-linux-psf');
  AddType('otf', 'application/x-font-otf');
  AddType('pcf', 'application/x-font-pcf');
  AddType('snf', 'application/x-font-snf');
  AddType('ttf', 'application/x-font-ttf');
  AddType('ttc', 'application/x-font-ttf');
  AddType('pfa', 'application/x-font-type1');
  AddType('pfb', 'application/x-font-type1');
  AddType('pfm', 'application/x-font-type1');
  AddType('afm', 'application/x-font-type1');
  AddType('woff', 'application/x-font-woff');
  AddType('arc', 'application/x-freearc');
  AddType('spl', 'application/x-futuresplash');
  AddType('gca', 'application/x-gca-compressed');
  AddType('ulx', 'application/x-glulx');
  AddType('gnumeric', 'application/x-gnumeric');
  AddType('gramps', 'application/x-gramps-xml');
  AddType('gtar', 'application/x-gtar');
  AddType('hdf', 'application/x-hdf');
  AddType('install', 'application/x-install-instructions');
  AddType('iso', 'application/x-iso9660-image');
  AddType('jnlp', 'application/x-java-jnlp-file');
  AddType('latex', 'application/x-latex');
  AddType('lzh', 'application/x-lzh-compressed');
  AddType('lha', 'application/x-lzh-compressed');
  AddType('mie', 'application/x-mie');
  AddType('prc', 'application/x-mobipocket-ebook');
  AddType('mobi', 'application/x-mobipocket-ebook');
  AddType('application', 'application/x-ms-application');
  AddType('lnk', 'application/x-ms-shortcut');
  AddType('wmd', 'application/x-ms-wmd');
  AddType('wmz', 'application/x-ms-wmz');
  AddType('xbap', 'application/x-ms-xbap');
  AddType('mdb', 'application/x-msaccess');
  AddType('obd', 'application/x-msbinder');
  AddType('crd', 'application/x-mscardfile');
  AddType('clp', 'application/x-msclip');
  AddType('exe', 'application/x-msdownload');
  AddType('dll', 'application/x-msdownload');
  AddType('com', 'application/x-msdownload');
  AddType('bat', 'application/x-msdownload');
  AddType('msi', 'application/x-msdownload');
  AddType('mvb', 'application/x-msmediaview');
  AddType('m13', 'application/x-msmediaview');
  AddType('m14', 'application/x-msmediaview');
  AddType('wmf', 'application/x-msmetafile');
  AddType('wmz', 'application/x-msmetafile');
  AddType('emf', 'application/x-msmetafile');
  AddType('emz', 'application/x-msmetafile');
  AddType('mny', 'application/x-msmoney');
  AddType('pub', 'application/x-mspublisher');
  AddType('scd', 'application/x-msschedule');
  AddType('trm', 'application/x-msterminal');
  AddType('wri', 'application/x-mswrite');
  AddType('nc', 'application/x-netcdf');
  AddType('cdf', 'application/x-netcdf');
  AddType('nzb', 'application/x-nzb');
  AddType('p12', 'application/x-pkcs12');
  AddType('pfx', 'application/x-pkcs12');
  AddType('p7b', 'application/x-pkcs7-certificates');
  AddType('spc', 'application/x-pkcs7-certificates');
  AddType('p7r', 'application/x-pkcs7-certreqresp');
  AddType('rar', 'application/x-rar-compressed');
  AddType('ris', 'application/x-research-info-systems');
  AddType('sh', 'application/x-sh');
  AddType('shar', 'application/x-shar');
  AddType('swf', 'application/x-shockwave-flash');
  AddType('xap', 'application/x-silverlight-app');
  AddType('sql', 'application/x-sql');
  AddType('sit', 'application/x-stuffit');
  AddType('sitx', 'application/x-stuffitx');
  AddType('srt', 'application/x-subrip');
  AddType('sv4cpio', 'application/x-sv4cpio');
  AddType('sv4crc', 'application/x-sv4crc');
  AddType('t3', 'application/x-t3vm-image');
  AddType('gam', 'application/x-tads');
  AddType('tar', 'application/x-tar');
  AddType('tcl', 'application/x-tcl');
  AddType('tex', 'application/x-tex');
  AddType('tfm', 'application/x-tex-tfm');
  AddType('texinfo', 'application/x-texinfo');
  AddType('texi', 'application/x-texinfo');
  AddType('obj', 'application/x-tgif');
  AddType('ustar', 'application/x-ustar');
  AddType('src', 'application/x-wais-source');
  AddType('der', 'application/x-x509-ca-cert');
  AddType('crt', 'application/x-x509-ca-cert');
  AddType('fig', 'application/x-xfig');
  AddType('xlf', 'application/x-xliff+xml');
  AddType('xpi', 'application/x-xpinstall');
  AddType('xz', 'application/x-xz');
  AddType('z1', 'application/x-zmachine');
  AddType('z2', 'application/x-zmachine');
  AddType('z3', 'application/x-zmachine');
  AddType('z4', 'application/x-zmachine');
  AddType('z5', 'application/x-zmachine');
  AddType('z6', 'application/x-zmachine');
  AddType('z7', 'application/x-zmachine');
  AddType('z8', 'application/x-zmachine');
  AddType('xaml', 'application/xaml+xml');
  AddType('xdf', 'application/xcap-diff+xml');
  AddType('xenc', 'application/xenc+xml');
  AddType('xhtml', 'application/xhtml+xml');
  AddType('xht', 'application/xhtml+xml');
  AddType('xml', 'application/xml');
  AddType('xsl', 'application/xml');
  AddType('dtd', 'application/xml-dtd');
  AddType('xop', 'application/xop+xml');
  AddType('xpl', 'application/xproc+xml');
  AddType('xslt', 'application/xslt+xml');
  AddType('xspf', 'application/xspf+xml');
  AddType('mxml', 'application/xv+xml');
  AddType('xhvml', 'application/xv+xml');
  AddType('xvml', 'application/xv+xml');
  AddType('xvm', 'application/xv+xml');
  AddType('yang', 'application/yang');
  AddType('yin', 'application/yin+xml');
  AddType('zip', 'application/zip');
  AddType('adp', 'audio/adpcm');
  AddType('au', 'audio/basic');
  AddType('snd', 'audio/basic');
  AddType('mid', 'audio/midi');
  AddType('midi', 'audio/midi');
  AddType('kar', 'audio/midi');
  AddType('rmi', 'audio/midi');
  AddType('mp4a', 'audio/mp4');
  AddType('mpga', 'audio/mpeg');
  AddType('mp2', 'audio/mpeg');
  AddType('mp2a', 'audio/mpeg');
  AddType('mp3', 'audio/mpeg');
  AddType('m2a', 'audio/mpeg');
  AddType('m3a', 'audio/mpeg');
  AddType('oga', 'audio/ogg');
  AddType('ogg', 'audio/ogg');
  AddType('spx', 'audio/ogg');
  AddType('s3m', 'audio/s3m');
  AddType('sil', 'audio/silk');
  AddType('uva', 'audio/vnd.dece.audio');
  AddType('uvva', 'audio/vnd.dece.audio');
  AddType('eol', 'audio/vnd.digital-winds');
  AddType('dra', 'audio/vnd.dra');
  AddType('dts', 'audio/vnd.dts');
  AddType('dtshd', 'audio/vnd.dts.hd');
  AddType('lvp', 'audio/vnd.lucent.voice');
  AddType('pya', 'audio/vnd.ms-playready.media.pya');
  AddType('ecelp4800', 'audio/vnd.nuera.ecelp4800');
  AddType('ecelp7470', 'audio/vnd.nuera.ecelp7470');
  AddType('ecelp9600', 'audio/vnd.nuera.ecelp9600');
  AddType('rip', 'audio/vnd.rip');
  AddType('weba', 'audio/webm');
  AddType('aac', 'audio/x-aac');
  AddType('aif', 'audio/x-aiff');
  AddType('aiff', 'audio/x-aiff');
  AddType('aifc', 'audio/x-aiff');
  AddType('caf', 'audio/x-caf');
  AddType('flac', 'audio/x-flac');
  AddType('mka', 'audio/x-matroska');
  AddType('m3u', 'audio/x-mpegurl');
  AddType('wax', 'audio/x-ms-wax');
  AddType('wma', 'audio/x-ms-wma');
  AddType('ram', 'audio/x-pn-realaudio');
  AddType('ra', 'audio/x-pn-realaudio');
  AddType('rmp', 'audio/x-pn-realaudio-plugin');
  AddType('wav', 'audio/x-wav');
  AddType('xm', 'audio/xm');
  AddType('cdx', 'chemical/x-cdx');
  AddType('cif', 'chemical/x-cif');
  AddType('cmdf', 'chemical/x-cmdf');
  AddType('cml', 'chemical/x-cml');
  AddType('csml', 'chemical/x-csml');
  AddType('xyz', 'chemical/x-xyz');
  AddType('bmp', 'image/bmp');
  AddType('cgm', 'image/cgm');
  AddType('g3', 'image/g3fax');
  AddType('gif', 'image/gif');
  AddType('ief', 'image/ief');
  AddType('jpeg', 'image/jpeg');
  AddType('jpg', 'image/jpeg');
  AddType('jpe', 'image/jpeg');
  AddType('ktx', 'image/ktx');
  AddType('png', 'image/png');
  AddType('btif', 'image/prs.btif');
  AddType('sgi', 'image/sgi');
  AddType('svg', 'image/svg+xml');
  AddType('svgz', 'image/svg+xml');
  AddType('tiff', 'image/tiff');
  AddType('tif', 'image/tiff');
  AddType('psd', 'image/vnd.adobe.photoshop');
  AddType('uvi', 'image/vnd.dece.graphic');
  AddType('uvvi', 'image/vnd.dece.graphic');
  AddType('uvg', 'image/vnd.dece.graphic');
  AddType('uvvg', 'image/vnd.dece.graphic');
  AddType('sub', 'image/vnd.dvb.subtitle');
  AddType('djvu', 'image/vnd.djvu');
  AddType('djv', 'image/vnd.djvu');
  AddType('dwg', 'image/vnd.dwg');
  AddType('dxf', 'image/vnd.dxf');
  AddType('fbs', 'image/vnd.fastbidsheet');
  AddType('fpx', 'image/vnd.fpx');
  AddType('fst', 'image/vnd.fst');
  AddType('mmr', 'image/vnd.fujixerox.edmics-mmr');
  AddType('rlc', 'image/vnd.fujixerox.edmics-rlc');
  AddType('mdi', 'image/vnd.ms-modi');
  AddType('wdp', 'image/vnd.ms-photo');
  AddType('npx', 'image/vnd.net-fpx');
  AddType('wbmp', 'image/vnd.wap.wbmp');
  AddType('xif', 'image/vnd.xiff');
  AddType('webp', 'image/webp');
  AddType('3ds', 'image/x-3ds');
  AddType('ras', 'image/x-cmu-raster');
  AddType('cmx', 'image/x-cmx');
  AddType('fh', 'image/x-freehand');
  AddType('fhc', 'image/x-freehand');
  AddType('fh4', 'image/x-freehand');
  AddType('fh5', 'image/x-freehand');
  AddType('fh7', 'image/x-freehand');
  AddType('ico', 'image/x-icon');
  AddType('sid', 'image/x-mrsid-image');
  AddType('pcx', 'image/x-pcx');
  AddType('pic', 'image/x-pict');
  AddType('pct', 'image/x-pict');
  AddType('pnm', 'image/x-portable-anymap');
  AddType('pbm', 'image/x-portable-bitmap');
  AddType('pgm', 'image/x-portable-graymap');
  AddType('ppm', 'image/x-portable-pixmap');
  AddType('rgb', 'image/x-rgb');
  AddType('tga', 'image/x-tga');
  AddType('xbm', 'image/x-xbitmap');
  AddType('xpm', 'image/x-xpixmap');
  AddType('xwd', 'image/x-xwindowdump');
  AddType('eml', 'message/rfc822');
  AddType('mime', 'message/rfc822');
  AddType('igs', 'model/iges');
  AddType('iges', 'model/iges');
  AddType('msh', 'model/mesh');
  AddType('mesh', 'model/mesh');
  AddType('silo', 'model/mesh');
  AddType('dae', 'model/vnd.collada+xml');
  AddType('dwf', 'model/vnd.dwf');
  AddType('gdl', 'model/vnd.gdl');
  AddType('gtw', 'model/vnd.gtw');
  AddType('mts', 'model/vnd.mts');
  AddType('vtu', 'model/vnd.vtu');
  AddType('wrl', 'model/vrml');
  AddType('vrml', 'model/vrml');
  AddType('x3db', 'model/x3d+binary');
  AddType('x3dbz', 'model/x3d+binary');
  AddType('x3dv', 'model/x3d+vrml');
  AddType('x3dvz', 'model/x3d+vrml');
  AddType('x3d', 'model/x3d+xml');
  AddType('x3dz', 'model/x3d+xml');
  AddType('appcache', 'text/cache-manifest');
  AddType('ics', 'text/calendar');
  AddType('ifb', 'text/calendar');
  AddType('cmd', 'text/cmd');
  AddType('css', 'text/css');
  AddType('csv', 'text/csv');
  AddType('html', 'text/html');
  AddType('htm', 'text/html');
  AddType('n3', 'text/n3');
  AddType('txt', 'text/plain');
  AddType('text', 'text/plain');
  AddType('conf', 'text/plain');
  AddType('def', 'text/plain');
  AddType('list', 'text/plain');
  AddType('log', 'text/plain');
  AddType('in', 'text/plain');
  AddType('js', 'text/plain');
  AddType('dsc', 'text/prs.lines.tag');
  AddType('rtx', 'text/richtext');
  AddType('sgml', 'text/sgml');
  AddType('sgm', 'text/sgml');
  AddType('tsv', 'text/tab-separated-values');
  AddType('t', 'text/troff');
  AddType('tr', 'text/troff');
  AddType('roff', 'text/troff');
  AddType('man', 'text/troff');
  AddType('me', 'text/troff');
  AddType('ms', 'text/troff');
  AddType('ttl', 'text/turtle');
  AddType('uri', 'text/uri-list');
  AddType('uris', 'text/uri-list');
  AddType('urls', 'text/uri-list');
  AddType('vcard', 'text/vcard');
  AddType('curl', 'text/vnd.curl');
  AddType('dcurl', 'text/vnd.curl.dcurl');
  AddType('scurl', 'text/vnd.curl.scurl');
  AddType('mcurl', 'text/vnd.curl.mcurl');
  AddType('sub', 'text/vnd.dvb.subtitle');
  AddType('fly', 'text/vnd.fly');
  AddType('flx', 'text/vnd.fmi.flexstor');
  AddType('gv', 'text/vnd.graphviz');
  AddType('3dml', 'text/vnd.in3d.3dml');
  AddType('spot', 'text/vnd.in3d.spot');
  AddType('jad', 'text/vnd.sun.j2me.app-descriptor');
  AddType('wml', 'text/vnd.wap.wml');
  AddType('wmls', 'text/vnd.wap.wmlscript');
  AddType('s', 'text/x-asm');
  AddType('asm', 'text/x-asm');
  AddType('c', 'text/x-c');
  AddType('cc', 'text/x-c');
  AddType('cxx', 'text/x-c');
  AddType('cpp', 'text/x-c');
  AddType('h', 'text/x-c');
  AddType('hh', 'text/x-c');
  AddType('dic', 'text/x-c');
  AddType('f', 'text/x-fortran');
  AddType('for', 'text/x-fortran');
  AddType('f77', 'text/x-fortran');
  AddType('f90', 'text/x-fortran');
  AddType('java', 'text/x-java-source');
  AddType('opml', 'text/x-opml');
  AddType('p', 'text/x-pascal');
  AddType('pas', 'text/x-pascal');
  AddType('nfo', 'text/x-nfo');
  AddType('etx', 'text/x-setext');
  AddType('sfv', 'text/x-sfv');
  AddType('uu', 'text/x-uuencode');
  AddType('vcs', 'text/x-vcalendar');
  AddType('vcf', 'text/x-vcard');
  AddType('xml', 'text/xml');
  AddType('xsl', 'text/xml');
  AddType('dtd', 'text/xml-dtd');
  AddType('3gp', 'video/3gpp');
  AddType('3g2', 'video/3gpp2');
  AddType('h261', 'video/h261');
  AddType('h263', 'video/h263');
  AddType('h264', 'video/h264');
  AddType('jpgv', 'video/jpeg');
  AddType('jpm', 'video/jpm');
  AddType('jpgm', 'video/jpm');
  AddType('mj2', 'video/mj2');
  AddType('mjp2', 'video/mj2');
  AddType('mp4', 'video/mp4');
  AddType('mp4v', 'video/mp4');
  AddType('mpg4', 'video/mp4');
  AddType('mpeg', 'video/mpeg');
  AddType('mpg', 'video/mpeg');
  AddType('mpe', 'video/mpeg');
  AddType('m1v', 'video/mpeg');
  AddType('m2v', 'video/mpeg');
  AddType('ogv', 'video/ogg');
  AddType('qt', 'video/quicktime');
  AddType('mov', 'video/quicktime');
  AddType('uvh', 'video/vnd.dece.hd');
  AddType('uvvh', 'video/vnd.dece.hd');
  AddType('uvm', 'video/vnd.dece.mobile');
  AddType('uvvm', 'video/vnd.dece.mobile');
  AddType('uvp', 'video/vnd.dece.pd');
  AddType('uvvp', 'video/vnd.dece.pd');
  AddType('uvs', 'video/vnd.dece.sd');
  AddType('uvvs', 'video/vnd.dece.sd');
  AddType('uvv', 'video/vnd.dece.video');
  AddType('uvvv', 'video/vnd.dece.video');
  AddType('dvb', 'video/vnd.dvb.file');
  AddType('fvt', 'video/vnd.fvt');
  AddType('mxu', 'video/vnd.mpegurl');
  AddType('m4u', 'video/vnd.mpegurl');
  AddType('pyv', 'video/vnd.ms-playready.media.pyv');
  AddType('uvu', 'video/vnd.uvvu.mp4');
  AddType('uvvu', 'video/vnd.uvvu.mp4');
  AddType('viv', 'video/vnd.vivo');
  AddType('webm', 'video/webm');
  AddType('f4v', 'video/x-f4v');
  AddType('fli', 'video/x-fli');
  AddType('flv', 'video/x-flv');
  AddType('m4v', 'video/x-m4v');
  AddType('mkv', 'video/x-matroska');
  AddType('mk3d', 'video/x-matroska');
  AddType('mks', 'video/x-matroska');
  AddType('mng', 'video/x-mng');
  AddType('asf', 'video/x-ms-asf');
  AddType('asx', 'video/x-ms-asf');
  AddType('vob', 'video/x-ms-vob');
  AddType('wm', 'video/x-ms-wm');
  AddType('wmv', 'video/x-ms-wmv');
  AddType('wmx', 'video/x-ms-wmx');
  AddType('wvx', 'video/x-ms-wvx');
  AddType('avi', 'video/x-msvideo');
  AddType('movie', 'video/x-sgi-movie');
  AddType('smv', 'video/x-smv');
  AddType('ice', 'x-conference/x-cooltalk');
end;
{$ENDIF}

initialization
begin
  {$IFDEF FNCLIB}
  FMimeTypes := TStringList.Create;
  RegisterMimeTypes;
  {$ENDIF}

  {$IFNDEF WEBLIB}
  if not IsConsole then
  begin
    {$IFNDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    CF_FNCRICHTEXTSTREAM := RegisterClipboardFormat('RichEditorText');
    CF_FNCSTREAM := RegisterClipboardFormat('TAdvClipBoard.StreamFormat');
    {$IFDEF FMXLIB}
    CF_FNCBITMAP := RegisterClipboardFormat('PNG');
    CF_FNCBITMAPWIN := CF_BITMAP;
    {$ENDIF}
    {$IFDEF VCLLIB}
    CF_FNCBITMAP := CF_BITMAP;
    {$ENDIF}
    CF_FNCRTF := RegisterClipboardFormat('Rich Text Format');
    CF_FNCHTML := RegisterClipboardFormat('HTML Format');
    {$ENDIF}
    {$ENDIF}

    {$IFDEF LCLLIB}
    CF_FNCRICHTEXTSTREAM := RegisterClipboardFormat('RichEditorText');
    CF_FNCSTREAM := RegisterClipboardFormat('TAdvClipBoard.StreamFormat');
    CF_FNCBITMAP := CF_PICTURE;
    CF_FNCRTF := RegisterClipboardFormat(TAdvClipBoard.GetFormat(TAdvClipBoardFormat.cfRTF));
    CF_FNCHTML := RegisterClipboardFormat(TAdvClipBoard.GetFormat(TAdvClipBoardFormat.cfHTML));
    {$ENDIF}
  end;
  {$ENDIF}
end;

{$IFNDEF WEBLIB}
{$IFDEF FNCLIB}
finalization
begin
  FMimeTypes.Free;
end;
{$ENDIF}
{$ENDIF}

end.


