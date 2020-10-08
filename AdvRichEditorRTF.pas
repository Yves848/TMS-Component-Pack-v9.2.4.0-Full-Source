{**************************************************************************}
{ RTF rendering engine                                                     }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2014 - 2019                                       }
{            Email : info@tmssoftware.com                                  }
{            Website : http://www.tmssoftware.com/                         }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvRichEditorRTF;

{$I TMSDEFS.INC}
{$IFDEF FMXMOBILE}
{$DEFINE DELPHI_LLVM}
{$ENDIF}


interface

uses
  SysUtils, Classes, Generics.Collections,
  AdvRichEditorPic, AdvGraphicsTypes, Types
  {$IFNDEF FNCLIB}
  , GDIPicture
  {$ENDIF}
  {$IFDEF VCLLIB}
  , Windows, Graphics, Controls, Forms, ImgList, Character
  , JPEG, PNGImage
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.Graphics, FMX.Controls, FMX.Forms, Character, System.UITypes, System.UIConsts
  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  , Windows, Win32Proc, Win32Extra, JwaWinUser
  {$ENDIF}
  , LCLType, Graphics, Forms, lazutf8, lconvencoding
  {$ENDIF}
  ;

{$IFDEF FMXLIB}
const
  clBlack = claBlack;
{$ENDIF}

type
  TRTFParseState = (psNone, psObject, psType, psAttr, psData, psField, psComment);

  TRTFDocState = (dsNone, dsFont, dsColor, dsText);

  {$IFDEF LCLLIB}
  TPNGImage =  TPortableNetworkGraphic;
  {$ENDIF}

  {$IFDEF FMXLIB}
  TAnsiString = MarshaledAString;
  PTAnsiChar = MarshaledAString;
  TColor = TAlphaColor;
  {$ENDIF}

  {$IFNDEF FMXLIB}
  TAnsiString = AnsiString;
  PTAnsiChar = PAnsiChar;
  {$ENDIF}

  {$IFDEF FNCLIB}
  TCustomImageList = class(TPersistent);
  {$ENDIF}

  TRTFObject = class(TPersistent)
  private
    FRTFType: string;
    FRTFAttr: string;
    FRTFData: string;
    FDataStr: TStringStream;
  protected
    function GetRTFData: string;
  public
    constructor Create;
    destructor Destroy; override;
    property RTFType: string read FRTFType write FRTFType;
    property RTFAttr: string read FRTFAttr write FRTFAttr;
    property RTFData: string read GetRTFData write FRTFData;
  end;

  TRTFColor = class(TPersistent)
  private
    FID: string;
    FColor: TColor;
  public
    property ID: string read FID write FID;
    property Color: TColor read FColor write FColor;
  end;

  TRTFFont = class(TPersistent)
  private
    FID: string;
    FName: string;
    FCodePage: integer;
  public
    property ID: string read FID write FID;
    property Name: string read FName write FName;
    property CodePage: integer read FCodePage write FCodePage;
  end;

  TRTFBaseLine = (bNormal, bSubScript, bSuperScript);

  TRTFBulletType = (rbtNone, rbtStart, rbtItem, rbtEnd);

  TRTFElementAttribute = record
    rec: TColor;
    rebc: TColor;
    ref: string;
    res: TFontStyles;
    refs: integer;
    real: TAlignment;
    rebl: TRTFBaseLine;
    reind: integer;
    rebul: boolean;
    rebuldepth: integer;
    retab: boolean;
  end;

  TRTFElement = class(TPersistent)
  private
    FAlignment: TAlignment;
    FBaseLine: TRTFBaseLine;
    FText: string;
    FBkColor: TColor;
    FColor: TColor;
    FFontName: string;
    FFontSize: integer;
    FFontStyle: TFontStyles;
    FBitmap: TBitmap;
    FIndent: integer;
    FBulletType: TRTFBulletType;
    FTab: boolean;
    FURL: string;
  public
    constructor Create;
    destructor Destroy; override;
    property BulletType: TRTFBulletType read FBulletType write FBulletType;
    property Bitmap: TBitmap read FBitmap write FBitmap;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property BaseLine: TRTFBaseLine read FBaseLine write FBaseLine;
    property BkColor: TColor read FBkColor write FBkColor;
    property Color: TColor read FColor write FColor;
    property FontName: string read FFontName write FFontName;
    property FontSize: integer read FFontSize write FFontSize;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property Indent: integer read FIndent write FIndent;
    property Text: string read FText write FText;
    property Tab: boolean read FTab write FTab;
    property URL: string read FURL write FURL;
  end;

  TRTFColorTable = TList<TRTFColor>;
  TRTFFontTable = TList<TRTFFont>;
  TRTFElementTable = TList<TRTFElement>;
  TRTFObjects = TList<TRTFObject>;

  TRTFDocument = class(TPersistent)
  private
    FColorTable: TRTFColorTable;
    FFontTable: TRTFFontTable;
    FElementTable: TRTFElementTable;
    FRTFObjects: TRTFObjects;
    FAttr: TRTFElementAttribute;
    FParseDepth: integer;
    FBullets: integer;
    FPara: boolean;
    FDefCP: integer;
    FDefUC: integer;
    FFontDPI: integer;
    FURL: string;
  protected
    function FindFont(ID: string): integer;
    property DefCodePage: integer  read FDefCP write FDefCP;
  public
    constructor Create;
    destructor Destroy; override;

    function IsUnicodeChar(s: string; var len: integer): boolean;
    function ParseUnicodeChar(s: string; var nextrep: char): string;
    function ParseRTF(ts: TStringStream): TRTFParseState;
    function ParseColor(s: string): TColor;
    function ParseFont(attr, s: string; var cp: integer): string;
    procedure ParseColors(s: string; ColorTable: TRTFColorTable);
    procedure ParseElements(s: string; isBul: boolean = false);
    procedure ParsePicture(Attr, Data: string);
    procedure ParseObjects;
    procedure AddBulletStart;
    procedure AddBulletEnd;
    procedure AddBullet;

    property ElementTable: TRTFElementTable read FElementTable;
    property FontTable: TRTFFontTable read FFontTable;
    property ColorTable: TRTFColorTable read FColorTable;
  end;

  TMetaFileHeader = record
    metatype: word;
    headsize: word;
    versionnr: word;
    filesize: cardinal;
    objects: word;
    maxsize: cardinal;
    params: word;
  end;

  TMetaFileRecord = record
    recordlen: cardinal;
    func: word;
    data: word;
  end;

  TMetaFileStretchBlt = record
    ROP: cardinal;
    srcHeight: smallint;
    srcWidth: smallint;
    ysrc: smallint;
    xsrc: smallint;
    destheight: smallint;
    destwidth: smallint;
    ydest: smallint;
    xdest: smallint;
  end;


  TFontItem = class(TCollectionItem)
  private
    FFaceName: String;
    procedure SetFaceName(const Value: String);
    function GetCode: String;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function GetText: String;
  published
    property FaceName: String read FFaceName write SetFaceName;
    property Code: String read GetCode;
  end;

  TFontCollection = class(TCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TFontItem;
    procedure SetItem(Index: Integer; const Value: TFontItem);
  public
    constructor Create({%H-}AOwner: TComponent);
    property Items[Index: Integer]: TFontItem read GetItem write SetItem; default;
    function Add: TFontItem;
    function Insert(Index: Integer): TFontItem;
    function IndexOf(FaceName: String): Integer;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFontTable = class(TObject)
  private
    FFonts: TFontCollection;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddFont(AFont: TFont): string; overload;// returns code/index, add it if not exist
    function AddFont(AFaceName: TFontName): string; overload;// returns code/index, add it if not exist
    function GetText: String;
    function IndexOf(FaceName: string): Integer;
    procedure LoadFonts(RTFString: string);
    property Fonts: TFontCollection read FFonts;
  end;

  TListTable = class(TObject)
  private
    FLists: TStringList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddList(ABullet: string): integer; overload;// returns code/index, add it if not exist
    function GetText: String;
  end;

  TColorTable = class(TObject)
  private
    FColorList: TStringList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddColor(Clr: TColor): integer; // returns code/index, add it if not exist
    function GetText: String;
    procedure LoadColors(RTFString: String);
    property Colors: TStringList read FColorList;
  end;

  TRTFHeader = class(TObject)
  private
    FFontTable: TFontTable;
    FColorTable: TColorTable;
    FListTable: TListTable;
    FViewKind: Integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function GetText: String;
    property FontTable: TFontTable read FFontTable;
    property ListTable: TListTable read FListTable;
    property ColorTable: TColorTable read FColorTable;
  end;

  TRTFPageSize = (A4, A5, Letter);

  TRTFEngine = class(TObject)
  private
    FRTFHeader: TRTFHeader;
    FNewLine: Boolean;
    FIsPreviouseKW: Boolean;
    FFont: TFont;
    FText: String;
    FStartTable: Boolean;
    FTableWidth: Integer;
    FStartRow: Boolean;
    FBold: Boolean;
    FItalic: Boolean;
    FUnderLine: Boolean;
    FHAlignment: TAlignment;
    FFontSize: Integer;
    FForeColor: TColor;
    FBackColor: TColor;
    FStrikeOut: Boolean;
    FStartBullet: Boolean;
    FBulletFont: string;
    FBulletChar: string;
    FTableAlignment: TAlignment;
    FImages: TCustomImageList;
    FCurrentCol: Integer;
    FTotalCol: Integer;
    FColColorList: TStringList;
    FHighLightColor: TColor;
    FWithBorders: Boolean;
    FLI: integer;
    FFontDPI: integer;
    FPageSize: TRTFPageSize;
    function ReplaceCR(s:string; dobreak:boolean):string;
  protected
    function RefreshPara({%H-}S: String; KWCode: Integer): Boolean;
    procedure AddInternal(S: String; KWCode: Integer = 0);
    function PicAsString(Pic: TPicture): string; overload;
    function PicAsString(Pic: TGDIPPicture; PictureWidth, PictureHeight: integer): string; overload;
    function MaxPageWidth: integer;
    function MaxPageHeight: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddFont(AFont: TFont);
    procedure AddFontName(FontName: TFontName);
    procedure AddBold(Value: Boolean);
    procedure AddItalic(Value: Boolean);
    procedure AddUnderLine(Value: Boolean);
    procedure AddStrikeOut(Value: Boolean);
    procedure AddFontSize(Value: Integer); overload;
    procedure AddFontSize(Value: Single); overload;
    procedure AddText(T: String);
    procedure AddNewLine;
    procedure AddLine;
    procedure AddTab;
    procedure AddParagraph(LeftInd: Integer = 720; RightInd: Integer = 0);
    procedure AddIndent(Indent: integer);
    procedure AddForeColor(Clr: TColor);
    procedure AddHighLightColor(Clr: TColor);
    procedure AddBackGroundColor(Clr: TColor);
    procedure AddHAlignment(Align: TAlignment);
    procedure AddPageBreak;

    procedure AddHTML(S: string);
    procedure AddRTF(S: string);

    procedure AddHyperLink(Link, Text: string; AFont: TFont); overload;
    procedure AddHyperLink(Link, Text: string; AFont: TFont; URLColor: TColor); overload;

    {$IFDEF FMXLIB}
    function AddBitmap(ABmp: TBitmap; Bkg: TAlphaColor = claWhite): string;
    {$ENDIF}
    function AddPicture(Pic: TPicture): string; overload;
    function AddPicture(Pic: TGDIPPicture;  PictureWidth, PictureHeight: integer): string; overload;

    procedure AddHyperLinkPic(Link: string; Pic: TPicture);

    procedure AddSuperScript;
    procedure AddSubScript;
    procedure AddNormalScript;

    // Table
    procedure StartTable(Align: TAlignment; WithBorders: Boolean = True);
    procedure EndTable;
    procedure AddColumn(ColWidth: Integer);
    procedure StartRow;
    procedure EndRow;
    procedure AddRow;
    procedure NextCell;
    procedure ReDefColWidth;
    procedure AddCellColor(Clr: TColor);

    procedure StartBullet(Number: boolean; FontName: TFontName = 'Symbol'; CharNo: Integer = 7; Color: TColor = clBlack); overload;
    procedure NextBullet(Number: boolean; Index: integer = -1); overload;
    procedure EndBullet;

    procedure SaveToFile(FileName: String);
    function GetText: string;
    procedure GetBuffer(var Buffer, FontTable,ColorTable: string);
    procedure SaveToStream(st: TStream);
    property FontDPI: integer read FFontDPI write FFontDPI;
    property Bold: Boolean read FBold;
    property Italic: Boolean read FItalic;
    property UnderLine: Boolean read FUnderLine;
    property StrikeOut: Boolean read FStrikeOut;
    property FontSize: Integer read FFontSize;
    property HAlignment: TAlignment read FHAlignment;
    property ForeColor: TColor read FForeColor;
    property BackColor: TColor read FBackColor;
    property HighLightColor: TColor read FHighLightColor;
    property Images: TCustomImageList read FImages write FImages;
    class function PixelsToTwips(pixels: integer): integer;
    class function TwipsToPixels(twips: integer): integer;
  end;

function HexVal(s: string): Integer;
function RTFHeader: string;
function IsNumChar(ch: char): boolean;
function IsAlphaChar(ch: char): boolean;
function IsCharInStr(ch: char; s: string): boolean;

implementation

uses
  Math, StrUtils;

const
  RTF_DEF = '\rtf1\ansi\ansicpg1252\deff0\deflang1033';

  TwipsPerInch = 1440;

{$IFDEF FMXLIB}
  clRed = claRed;
  clGreen = claGreen;
  clBlue = claBlue;
  clWhite = claWhite;
  clYellow = claYellow;
  clFuchsia = claFuchsia;
  clLime = claLime;
  clAqua = claAqua;
  clSilver = claSilver;
  clGray = claGray;
  clOlive = claOlive;
  clNavy = claNavy;
  clPurple = claPurple;
  clTeal = claTeal;
  clMaroon = claMaroon;
  clNone = claNull;
{$ENDIF}

type
  tagTMSRGBQUAD = record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;

  TTMSRGBQuad = tagTMSRGBQUAD;
  {$EXTERNALSYM TMSRGBQUAD}
  {$IFNDEF LCLLIB}
  TMSRGBQUAD = tagTMSRGBQUAD;
  {$ENDIF}

  tagTMSBITMAPINFOHEADER = record
    biSize: DWORD;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: Longint;
    biYPelsPerMeter: Longint;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;

  TTMSBitmapInfoHeader = tagTMSBITMAPINFOHEADER;
  {$EXTERNALSYM TMSBITMAPINFOHEADER}
  {$IFNDEF LCLLIB}
  TMSBITMAPINFOHEADER = tagTMSBITMAPINFOHEADER;
  {$ENDIF}

  tagTMSBITMAPINFO = record
    bmiHeader: TTMSBitmapInfoHeader;
    bmiColors: array[0..0] of TTMSRGBQuad;
  end;

  PTMSBitmapInfo = ^TTMSBitmapInfo;
  TTMSBitmapInfo = tagTMSBITMAPINFO;
  {$EXTERNALSYM TMSBITMAPINFO}
  {$IFNDEF LCLLIB}
  TMSBITMAPINFO = tagTMSBITMAPINFO;
  {$ENDIF}

function GetFontName(AFont: TFont): string;
begin
  {$IFDEF FMXLIB}
  Result := AFont.Family;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Result := AFont.Name;
  {$ENDIF}
end;

procedure SetFontName(AFont: TFont; AName: string);
begin
  {$IFDEF FMXLIB}
  AFont.Family := AName;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  AFont.Name := AName;
  {$ENDIF}
end;

function GetFontSize(AFont: TFont): integer;
begin
  {$IFDEF FMXLIB}
  Result := Round(AFont.Size);
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Result := AFont.Size;
  {$ENDIF}
end;

function CharInStr(s: string; Index: Integer): Char;
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

function CharsetToCodePage(CharSet: Integer): Integer;
begin
  Result := 0;

  case CharSet of
  128: Result := 932;
  129: Result := 949;
  134: Result := 936;
  136: Result := 950;
  161: Result := 1253;
  162: Result := 1254;
  177: Result := 1255;
  178: Result := 1256;
  186: Result := 1257;
  204: Result := 1251;
  222: Result := 874;
  238: Result := 1250;
  end;
end;

//------------------------------------------------------------------------------

function MapAnsiToUTF8(ch: char): string;
begin
  case ord(ch) of
  $80: Result := Unicodestring(#$20AC);
  $82: Result := Unicodestring(#$201A);
  $83: Result := Unicodestring(#$192);
  $84: Result := Unicodestring(#$201E);
  $85: Result := Unicodestring(#$2026);
  $86: Result := Unicodestring(#$2020);
  $87: Result := Unicodestring(#$2021);
  $88: Result := Unicodestring(#$2C6);
  $89: Result := Unicodestring(#$2030);
  $8A: Result := Unicodestring(#$160);
  $8B: Result := Unicodestring(#$2039);
  $8C: Result := Unicodestring(#$152);
  $8D: Result := Unicodestring(#$20);
  $8E: Result := Unicodestring(#$17D);
  $91: Result := Unicodestring(#$2018);
  $92: Result := Unicodestring(#$2019);
  $93: Result := Unicodestring(#$201C);
  $94: Result := Unicodestring(#$201D);
  $95: Result := Unicodestring(#$2022);
  $96: Result := Unicodestring(#$2013);
  $97: Result := Unicodestring(#$2014);
  $98: Result := Unicodestring(#$2DC);
  $99: Result := Unicodestring(#$2122);
  $9A: Result := Unicodestring(#$161);
  $9B: Result := Unicodestring(#$203A);
  $9C: Result := Unicodestring(#$153);
  $9D: Result := Unicodestring(#$20);
  $9E: Result := Unicodestring(#$17E);
  $9F: Result := Unicodestring(#$178)
  else
    Result := chr(0);
  end;
end;

//------------------------------------------------------------------------------

{$IFNDEF FMXLIB}
function ToUnicodeString(AText: TAnsiString; ACodePage: integer): string;
  {$IFDEF MSWINDOWS}
var
  L: Integer;
  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  Len: Integer;
  S: widestring;
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF LCLLIB}
  Len: Integer;
  S: string;
  {$ENDIF}
begin
  {$IFDEF LCLLIB}

  {$IFDEF MSWINDOWS}
  Len := Length(AText);
  L := MultiByteToWideChar(ACodePage, 0, PTAnsiChar(AText), Len, nil, 0);
  SetLength(S, L);
  MultiByteToWideChar(ACodePage, 0, PTAnsiChar(AText), Len, PWideChar(S), Length(S));
  Result := UTF8Encode(S);
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  if ACodePage = 874 then
    Result := CP874ToUTF8(AText);
  if ACodePage = 932 then
    Result := CP932ToUTF8(AText);
  if ACodePage = 936 then
    Result := CP936ToUTF8(AText);
  if ACodePage = 949 then
    Result := CP949ToUTF8(AText);
  if ACodePage = 950 then
    Result := CP950ToUTF8(AText);
  if ACodePage = 1250 then
    Result := CP1250ToUTF8(AText);
  if ACodePage = 1251 then
    Result := CP1251ToUTF8(AText);
  if ACodePage = 1252 then
    Result := CP1252ToUTF8(AText);
  if ACodePage = 1253 then
    Result := CP1253ToUTF8(AText);
  if ACodePage = 1254 then
    Result := CP1254ToUTF8(AText);
  if ACodePage = 1255 then
    Result := CP1255ToUTF8(AText);
  if ACodePage = 1256 then
    Result := CP1256ToUTF8(AText);
  if ACodePage = 1257 then
    Result := CP1257ToUTF8(AText);
  {$ENDIF}

  {$ENDIF}

  {$IFNDEF LCLLIB}
  Len := Length(AText);
  L := UnicodeFromLocaleChars(ACodePage, 0, PTAnsiChar(AText), Len, nil, 0);
  SetLength(S, L);
  UnicodeFromLocaleChars(ACodePage, 0, PTAnsiChar(AText), Len, PChar(S), Length(S));
  Result := S;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF FMXLIB}
function ToUnicodeString(ABytes: TBytes; ACodePage: integer): string;
var
  L: Integer;
  S: string;
  Len: Integer;
begin
  Len := Length(ABytes);
  L := UnicodeFromLocaleChars(ACodePage, 0, Pointer(ABytes), Len, nil, 0);
  SetLength(S, L);
  UnicodeFromLocaleChars(ACodePage, 0, Pointer(ABytes), Len, PChar(S), Length(S));
  Result := S;
end;
{$ENDIF}

//------------------------------------------------------------------------------

function IsCharInStr(ch: char; s: string): boolean;
var
  su: char;
begin
  su := ch;
  Result := Pos(su, s) > 0;
end;

//------------------------------------------------------------------------------

function IsNumChar(ch: char): boolean;
begin
  {$IFDEF LCLLIB}
  Result := CharInSet(ch, ['0'..'9']);
  {$ENDIF}

  {$IFNDEF LCLLIB}

  {$IFDEF FNCLIB}
  Result := ch.IsNumber;
  {$ENDIF}

  {$IFNDEF FNCLIB}
  {$IFNDEF DELPHIXE4_LVL}
  Result := Character.IsNumber(ch);
  {$ENDIF}

  {$IFDEF DELPHIXE4_LVL}
  Result := ch.IsNumber;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function IsAlphaChar(ch: char): boolean;
begin
  {$IFDEF LCLLIB}
  Result := CharInSet(ch, ['a'..'z','A'..'Z']);
  {$ENDIF}

  {$IFNDEF LCLLIB}

  {$IFDEF FNCLIB}
  Result := ch.IsLetter;
  {$ENDIF}

  {$IFNDEF FNCLIB}
  {$IFNDEF DELPHIXE4_LVL}
  Result := Character.IsLetter(ch);
  {$ENDIF}

  {$IFDEF DELPHIXE4_LVL}
  Result := ch.IsLetter;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function RemoveStartingSpace(S: String): String;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(s) do
  begin
    if (CharInStr(s,i) = ' ') then
      Result := copy(S, 2, Length(S)-1)
    else
      Break;
  end;
end;

//------------------------------------------------------------------------------

{
function IsHyperLink(Text: String): Boolean;
begin
  Result := (pos('http://', Text) > 0) or (pos('file://', Text) > 0) or (pos('ftp://', Text) > 0)
             or (pos('nntp://', Text) > 0) or (pos('https://', Text) > 0) or (pos('mailto:', Text) > 0);
end;
}

//------------------------------------------------------------------------------

function DBTagStrip(s:string):string;
var
  i,j: Integer;
begin
  repeat
    i := Pos('<#',s);
    if i > 0 then
      begin
        Result := Copy(s,1,i - 1);
        Delete(s,1,i);
        j := Pos('>',s);
        if j > 0 then
          Delete(s,j,1);
        Result := Result + s;
        s := Result;
      end
    else
      Result := s;
  until (i <= 0);
end;

//------------------------------------------------------------------------------

function CRLFStrip(s:string; dobreak:boolean):string;
var
  i: Integer;
  ch: char;
begin
  Result := '';

  for i := 1 to Length(s) do
  begin
    ch := CharInStr(s,i);
    if not ((ch = #13) or (ch = #10)) then
      Result := Result +ch
    else
      if (ch = #13) and dobreak then
        Result := Result + '<BR>';
  end;
end;

//------------------------------------------------------------------------------

function IPos(su,s:string):Integer;
begin
  Result := Pos(UpperCase(su),UpperCase(s));
end;

//------------------------------------------------------------------------------

function TagReplaceString(const Srch,Repl:string;var Dest:string):Boolean;
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

//------------------------------------------------------------------------------

function VarPos(su,s:string;var Res:Integer):Integer;
begin
  Res := Pos(su,s);
  Result := Res;
end;

//------------------------------------------------------------------------------

function HexVal(s:string): Integer;
var
  i,j: Integer;
  ch: char;
begin
  if Length(s) < 2 then
  begin
    Result := 0;
    Exit;
  end;

  s := UpperCase(s);

  ch := CharInStr(s,1);
  if ch >= 'A' then
    i := ord(ch) - ord('A') + 10
  else
    i := ord(ch) - ord('0');

  ch := CharInStr(s,2);
  if ch >= 'A' then
    j := ord(ch) - ord('A') + 10
  else
    j := ord(ch) - ord('0');

  Result := i shl 4 + j;
end;

//------------------------------------------------------------------------------

function Hex2Color(s:string): TColor;
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(s,2,2));
  g := Hexval(Copy(s,4,2)) shl 8;
  b := Hexval(Copy(s,6,2)) shl 16;
  Result := TColor(b + g + r);
end;

//------------------------------------------------------------------------------

function Text2Color(s:string): TColor;
begin
  Result := clBlack;

  if (s = 'clred') then Result := clred else
  if (s = 'clblack') then Result := clblack else
  if (s = 'clblue') then Result := clblue else
  if (s = 'clgreen') then Result := clgreen else
  if (s = 'claqua') then Result := claqua else
  if (s = 'clyellow') then Result := clyellow else
  if (s = 'clfuchsia') then Result := clfuchsia else
  if (s = 'clwhite') then Result := clwhite else
  if (s = 'cllime') then Result := cllime else
  if (s = 'clsilver') then Result := clsilver else
  if (s = 'clgray') then Result := clgray else
  if (s = 'clolive') then Result := clolive else
  if (s = 'clnavy') then Result := clnavy else
  if (s = 'clpurple') then Result := clpurple else
  if (s = 'clteal') then Result := clteal else
  if (s = 'clmaroon') then Result := clmaroon;

  {$IFNDEF FMXLIB}
  if Result <> clBlack then Exit;

  if (s = 'clbackground') then Result := clbackground else
  if (s = 'clactivecaption') then Result := clactivecaption else
  if (s = 'clinactivecaption') then Result := clinactivecaption else
  if (s = 'clmenu') then Result := clmenu else
  if (s = 'clwindow') then Result := clwindow else
  if (s = 'clwindowframe') then Result := clwindowframe else
  if (s = 'clmenutext') then Result := clmenutext else
  if (s = 'clwindowtext') then Result := clwindowtext else
  if (s = 'clcaptiontext') then Result := clcaptiontext else
  if (s = 'clactiveborder') then Result := clactiveborder else
  if (s = 'clinactiveborder') then Result := clinactiveborder else
  if (s = 'clappworkspace') then Result := clappworkspace else
  if (s = 'clhighlight') then Result := clhighlight else
  if (s = 'clhighlighttext') then Result := clhighlighttext else
  if (s = 'clbtnface') then Result := clbtnface else
  if (s = 'clbtnshadow') then Result := clbtnshadow else
  if (s = 'clgraytext') then Result := clgraytext else
  if (s = 'clbtntext') then Result := clbtntext else
  if (s = 'clinactivecaptiontext') then Result := clinactivecaptiontext else
  if (s = 'clbtnhighlight') then Result := clbtnhighlight else
  if (s = 'cl3ddkshadow') then Result := clgraytext else
  if (s = 'cl3dlight') then Result := cl3dlight else
  if (s = 'clinfotext') then Result := clinfotext else
  if (s = 'clinfobk') then Result := clinfobk;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function IStrToInt(s:string): Integer;
var
  Err,Res: Integer;
begin
  Result := 0;
  Val(s,Res,Err);
  if Err = 0 then
    Result := Res;
end;

//------------------------------------------------------------------------------
{$WARNINGS OFF}
procedure HTMLToRTFEx(s:string; {%H-}FImages: TCustomImageList; ShadowOffset: Integer;
                    {%H-}CheckHeight,Selected,{%H-}Blink,{%H-}HoverStyle,WordWrap,{%H-}Down: Boolean;
                    {%H-}ResFactor:Double;
                    URLColor,{%H-}HoverColor,{%H-}HoverFontColor,{%H-}ShadowColor:TColor; DefFont: TFont; RTFEngine: TRTFEngine);
var
  su: string;
  Anchor: Boolean;
  LastAnchor, AnchorText: string;
  isSup,isSub,isShad: Boolean;
  subh,suph,imgali,fs: Integer;
  ListIndex: Integer;
  Invisible: Boolean;
  FoundTag: Boolean;
  AltImg,ImgIdx: Integer;
  FrstBullet: Boolean;
  URLFont: TFont;
  d: integer;
{$IFNDEF FNCLIB}
  Pic: TPicture;
{$ENDIF}

  function ConvertHTMLLine(var s:string;Calc:Boolean):string;
  var
    su,Res,TagProp,Prop,{AltProp,}Tagp,LineText:string;
    cr: TRect;
    linebreak,imgbreak{,linkbreak}: Boolean;
    TagPos,SpacePos{,o,l}: Integer;
    WordLen: Integer;
    TagChar: Char;
    LengthFits{, SpaceBreak}: Boolean;
    //ControlType,ControlWidth,ControlID,ControlValue,ControlProp: string;

  begin
    Result := '';
    LineText := '';

    //sw := 0;

    LineBreak := False;
    ImgBreak := False;
    //LinkBreak := False;
    res := '';

    while (Length(s) > 0) and not LineBreak and not ImgBreak do
    begin
      // get next word or till next HTML tag
      TagPos := Pos('<',s);

      if WordWrap then
        SpacePos := Pos(' ',s)
      else
        SpacePos := 0;

      if (Tagpos > 0) and ((SpacePos > TagPos) or (SpacePos = 0)) then
      begin
        su := Copy(s,1,TagPos - 1);
      end
      else
      begin
        if SpacePos > 0 then
          su := Copy(s,1,SpacePos)
        else
          su := s;
      end;

      WordLen := Length(su);

      while Pos('&nbsp;',su) > 0 do
      begin
        TagReplacestring('&nbsp;',' ',su);
      end;

      while Pos('&lt;',su) > 0 do
      begin
        TagReplacestring('&lt;','<',su);
      end;

      while Pos('&gt;',su) > 0 do
      begin
        TagReplacestring('&gt;','>',su);
      end;

      //WordLenEx := Length(su);

      if WordLen > 0 then
      begin
        //StripVal := StripVal + su;

        if Invisible then
          Delete(s,1,WordLen);


        if not Invisible then
        begin
          // draw mode
          if not Calc then
          begin
            if isSup then
              cr.Bottom := cr.Bottom - suph;
            if isSub then
              cr.Bottom := cr.Bottom + subh;

            cr.Bottom := cr.Bottom - imgali;

            if isShad then
            begin
              OffsetRect(cr,ShadowOffset,ShadowOffset);
            end;

          end
        else
          begin

            if Anchor then
              AnchorText := su
            else
              RTFEngine.AddText(su);
          end;

          LengthFits := True;

          LineText := LineText + su;

          if LengthFits then
          begin
            Res := Res + Copy(s,1,WordLen);
            if not LengthFits and Calc and (LineText <> su) then
              s := '';
            Delete(s,1,WordLen);
            if Length(su) >= WordLen then
            begin
             { if su[WordLen] = ' ' then
                sw := Canvas.TextWidth(' ')
              else
                sw := 0; }
            end
            {else
              sw := 0};
          end
          else
          begin
            LineBreak := True;
          end;
        end;
      end;

      TagPos := Pos('<',s);

      if (TagPos = 1) and (Length(s) <= 2) then
        s := '';

      if not LineBreak and (TagPos = 1) and (Length(s) > 2) then
      begin
        if (s[2 - d] = '/') and (Length(s) > 3) then
        begin
          case UpCase(s[3 - d]) of
          'A':begin
                if Anchor then
                begin
                  {$IFNDEF FMXLIB}
                  RTFEngine.AddHyperLink(LastAnchor, AnchorText, URLFont, URLFont.Color);
                  {$ENDIF}
                  {$IFDEF FMXLIB}
                  RTFEngine.AddHyperLink(LastAnchor, AnchorText, URLFont);
                  {$ENDIF}
                  RTFEngine.AddFont(DefFont);
                  {$IFNDEF FMXLIB}
                  RTFEngine.AddForeColor(DefFont.Color);
                  {$ENDIF}
                  fs := Round(DefFont.Size * 2);
                  if RTFEngine.FontSize <> fs then
                    RTFEngine.AddFontSize(fs);
                  Anchor := False;
                end;
              end;
          'B':begin
                {$IFNDEF FMXLIB}
                if (s[4 - d] <> '>') and (RTFEngine.ForeColor <> clBlack) then
                  RTFEngine.AddForeColor(DefFont.Color)
                else
                {$ENDIF}
                  RTFEngine.AddBold(False);
              end;
          'S':begin
                TagChar := UpCase(s[4 - d]);

                if (TagChar = 'U') then
                begin
                  isSup := False;
                  isSub := False;
                end
                else
                 if (TagChar = 'H') then
                  isShad := False;
                 RTFEngine.AddStrikeOut(False); 
              end;
          'F':begin
                RTFEngine.AddFont(DefFont);
                {$IFNDEF FMXLIB}
                RTFEngine.AddForeColor(DefFont.Color);
                {$ENDIF}
                fs := Round(DefFont.Size * 2);
                if RTFEngine.FontSize <> fs then
                  RTFEngine.AddFontSize(fs);
              end;
          'H':begin
                if not Calc then
                begin
                {  Canvas.Font.Color := hifCol;
                  Canvas.Brush.Color := hibCol;
                  if hibCol = clNone then
                    Canvas.Brush.Style := bsClear;  }
                end;
              end;
          'I':begin
                RTFEngine.AddItalic(False);
              end;
          'L':begin
                LineBreak := True;
                RTFEngine.AddNewLine;
              end;
          'P':begin
                LineBreak := True;
                if not Calc then
                begin
                  {Canvas.Brush.Color := ParaColor;
                  if ParaColor = clNone then
                    Canvas.Brush.Style := bsClear;    }
                end;
              end;
          'U':begin
                if (s[4 - d] <> '>') and (ListIndex > 0) then
                begin
                  RTFEngine.EndBullet;
                  FrstBullet := False;
                end
                  //Dec(Listindex)
                else
                  RTFEngine.AddUnderLine(False);
              end;
          'R':begin

              end;
          'Z':Invisible := False;
          end;
        end
        else
        begin
          case Upcase(s[2 - d]) of
          'A':begin

                TagProp := Uppercase(Copy(s,3,Pos('>',s) - 1));  // <A href="anchor">

                if (VarPos('HREF',TagProp,TagPos)>0) then
                begin
                  TagProp := Copy(s,3,Pos('>',s) - 1);    // restore case ie: without upppercase

                  Prop := Copy(TagProp,TagPos + 4,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                  LastAnchor := Prop;
                  Anchor := True;
                end;
              end;
          'B':begin
                TagChar := Upcase(s[3 - d]);
                if TagChar = '>' then  // <B> tag
                  RTFEngine.AddBold(True)
                else
                  if TagChar = 'R' then // <BR> tag
                  begin
                    LineBreak := true;
                    //StripVal := StripVal + #13;
                    RTFEngine.AddNewLine;
                  end
                  else
                  begin
                    if TagChar = 'L' then // <BLINK> tag
                    begin
                      //if not Blink then Canvas.Font.Color := BlnkColor;
                    end
                    else
                    if TagChar = 'O' then  // <BODY ... >
                    begin
                      Res := Res + Copy(s,1,pos('>',s));
                      TagProp := Uppercase(Copy(s,6,pos('>',s)-1));

                      if (Pos('BACKGROUND',TagProp) > 0) and not Calc then
                      begin
                        Prop := Copy(TagProp,Pos('BACKGROUND',TagProp)+10,Length(TagProp));
                        Prop := Copy(Prop,Pos('"',Prop)+1,Length(prop));
                        Prop := Copy(Prop,1,Pos('"',Prop)-1);

                        {bmp := nil;

                        if (Pos(':',Prop) = 0) and Assigned(pc) then
                        begin
                          bmp := pc.FindPicture(Prop);
                        end;
                        }
                        if (Pos('://',Prop) > 0){ and Assigned(ic)} then
                        begin
                         { if ic.FindPicture(Prop) = nil then
                          with ic.AddPicture do
                          begin
                            Asynch := False;
                            LoadFromURL(Prop);
                          end;

                          bmp := ic.FindPicture(Prop); }
                        end;

                       { if bmp <> Nil then
                        begin
                          if not bmp.Empty and (bmp.Width > 0) and (bmp.Height > 0) then
                          begin
                            // do the tiling here
                            bmpy := 0;
                            hrgn := CreateRectRgn(fr.left, fr.top, fr.right,fr.bottom);
                            SelectClipRgn(Canvas.Handle, hrgn);

                            while (bmpy < fr.bottom-fr.top) do
                            begin
                              bmpx := 0;
                              while (bmpx < fr.right - fr.left) do
                              begin
                                Canvas.Draw(fr.left+bmpx,fr.top+bmpy,bmp);
                                bmpx := bmpx + bmp.width;
                              end;
                              bmpy := bmpy + bmp.height;
                            end;

                            SelectClipRgn(Canvas.handle, 0);
                            DeleteObject(hrgn);
                          end;
                        end; //end of bmp <> nil   }
                    end; //end of background

                      if (Pos('BGCOLOR',TagProp)>0) then
                      begin
                        Prop := Copy(TagProp,Pos('BGCOLOR',TagProp) + 7,Length(TagProp));
                        Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                        Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                        if not Calc then
                        begin
                          if Pos('CL',Prop) > 0 then
                            ;//Canvas.Brush.color := Text2Color(AnsiLowerCase(Prop));
                          if Pos('#',Prop) > 0 then
                            ;//Canvas.Brush.color := Hex2Color(Prop);

                        end;
                      end;
                    end;
                  end;
                end;
          'H':begin
                case Upcase(s[3 - d]) of
                'R':
                begin
                  LineBreak := True;

                end;
                'I':
                begin
                  if not Calc then
                  begin
                   { hifCol := Canvas.Font.Color;
                    hibCol := Canvas.Brush.Color;
                    if Canvas.Brush.Style = bsClear then
                      hibCol := clNone;

                    Canvas.Brush.Color := clHighLight;
                    Canvas.Font.Color := clHighLightText;
                    }
                  end;
                end;
                end;
              end;
          'I':begin
                TagChar := Upcase(s[3 - d]);

                if TagChar = '>' then // <I> tag
                  //Canvas.Font.Style := Canvas.Font.Style + [fsItalic]
                  RTFEngine.AddItalic(True)
                else
                if TagChar = 'N' then  // <IND> tag
                begin
                  TagProp := Copy(s,3,pos('>',s)-1);

                  Prop := Copy(TagProp,ipos('x',TagProp)+2,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop)+1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',Prop)-1);

                  {val(Prop,indent,err);
                  if err = 0 then
                  begin
                    if Indent > w then
                     begin
                       w := Indent;
                       cr.left := fr.left + Indent;
                     end;
                  end; }
                end
                else
                  if TagChar = 'M' then
                  begin
                    inc(ImgIdx);

                    //oldfont.color:=Canvas.font.color;
                    TagProp := Uppercase(Copy(s,3,pos('>',s) - 1));
                    Prop := Copy(TagProp,Pos('SRC',TagProp) + 4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    if (Pos('ALT',TagProp) > 0) and (AltImg = ImgIdx) then
                    begin
                      Prop := Copy(TagProp,Pos('ALT',TagProp) + 4,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                    end;

                    if Pos('WIDTH',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,Pos('WIDTH',TagProp) + 6,Length(TagProp));
                      Tagp := Copy(Tagp,Pos('"',tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,Pos('"',tagp) - 1);
                    end;

                    if Pos('HEIGHT',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,ipos('HEIGHT',TagProp) + 7,Length(TagProp));
                      Tagp := Copy(Tagp,pos('"',Tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,pos('"',Tagp) - 1);
                    end;

                    {$IFNDEF FNCLIB}
                    if Pos('IDX:',Prop) > 0 then
                    begin
                      Delete(Prop,1,4);
                      if Assigned(FImages) and (IStrToInt(Prop) < FImages.Count) then
                      begin
                        Pic := TPicture.Create;
                        Pic.Bitmap.Width := FImages.Width;
                        Pic.Bitmap.Height := FImages.Height;
                        FImages.Draw(Pic.Bitmap.Canvas, 0, 0, IStrToInt(Prop));
                        RTFEngine.AddPicture(Pic);
                        Pic.Free;
                      end;
                    end;
                    {$ENDIF}

                    if Pos('SSYS:',Prop) > 0 then
                    begin
                      Delete(Prop,1,5);
                    end;

                    if Pos('LSYS:',Prop) > 0 then
                    begin
                      Delete(Prop,1,5);
                    end;

                  end;
                end;
          'L':begin
                if not FrstBullet then
                  RTFEngine.AddNewLine
                else
                  FrstBullet := False;
               { w := w + 12 * ListIndex;
                if Linkbreak then
                  Imgbreak := True
                else
                  Linkbreak := True;
                }
              end;
          'U':begin
                if s[3 - d] <> '>' then
                begin
                  Inc(ListIndex);
                  LineBreak := True;
                  RTFEngine.StartBullet(false);
                  FrstBullet := True;
                end
                else
                  RTFEngine.AddUnderLine(True);
              end;
          'P':begin
                if (VarPos('>',s,TagPos)>0) then
                begin
                  TagProp := Uppercase(Copy(s,3,TagPos-1));

                  if VarPos('INDENT',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);
                    //PIndent := IStrToInt(Prop);
                  end;

                  if VarPos('BGCOLOR',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                   { NewColor := clNone;

                    if Length(Prop) > 0 then
                    begin
                      if Prop[1] = '#' then
                        NewColor := Hex2Color(Prop)
                      else
                        NewColor := Text2Color(AnsiLowerCase(prop));
                    end; }

                    if not Calc then
                    begin
                      {paracolor := Canvas.Brush.Color;
                      if Canvas.Brush.Style = bsClear then ParaColor := clNone;
                      }
                    end;
                  end;
                end;
            end;
        'F':begin
              if (VarPos('>',s,TagPos)>0) then
              begin
                TagProp := UpperCase(Copy(s,6,TagPos-6));

                if (VarPos('FACE',TagProp,TagPos) > 0) then
                begin
                  Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                  Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos('"',prop)-1);
                  //Canvas.Font.Name := Prop;
                  RTFEngine.AddFontName(Prop);
                end;

                if (VarPos(' COLOR',TagProp,TagPos) > 0) and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',prop)+1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',prop)-1);

                  if Length(Prop) > 0 then
                  begin
                    if Prop[1 - d] = '#' then
                      //Canvas.font.color := Hex2Color(Prop)
                      RTFEngine.AddForeColor(Hex2Color(Prop))
                    else
                      RTFEngine.AddForeColor(Text2Color(AnsiLowerCase(prop)));//Canvas.Font.Color := Text2Color(AnsiLowerCase(prop));
                  end;

                end;

                if (VarPos('BGCOLOR',TagProp,TagPos)>0) and not Calc and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+7,Length(TagProp));
                  Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos('"',prop)-1);
                  //BGColor := Canvas.Brush.Color;

                 { if Canvas.Brush.Style = bsClear then
                    bgcolor := clNone;
                 }
                  if Length(Prop) > 0 then
                  begin
                    if Prop[1 - d] = '#' then
                      //Canvas.Brush.Color := Hex2Color(Prop)
                    else
                      ;//Canvas.Brush.Color := Text2Color(AnsiLowerCase(prop));
                  end;

                end;

                if (VarPos('SIZE',TagProp,TagPos)>0) then
                begin
                  Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                  Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                  Prop := Copy(Prop,Pos('"',Prop)+1,Length(Prop));

                  case IStrToInt(Prop) of
                  1:RTFEngine.AddFontSize(8*2);
                  2:RTFEngine.AddFontSize(10*2);
                  3:RTFEngine.AddFontSize(12*2);
                  4:RTFEngine.AddFontSize(14*2);
                  5:RTFEngine.AddFontSize(16*2);
                  else
                    begin
                      fs := Round(IStrToInt(Prop) * 2);
                      RTFEngine.AddFontSize(fs);
                    end;
                  end;
                end;
              end;
            end;
        'S':begin
              TagChar := Upcase(s[3 - d]);

              if TagChar = '>' then
                RTFEngine.AddStrikeOut(True)
              else
              begin
                if TagChar = 'H' then
                  isShad := True
                else
                begin
                  if ipos('<SUB>',s)=1 then
                    isSub := True
                  else
                    if ipos('<SUP>',s)=1 then
                      isSup := True;
                end;
              end;
            end;
        'R':begin
              TagProp := Copy(s,3,pos('>',s)-1);
              prop := Copy(TagProp,ipos('a',TagProp)+2,Length(TagProp));
              prop := Copy(prop,pos('"',prop)+1,Length(prop));
              prop := Copy(prop,1,pos('"',prop)-1);
              //Val(prop,Indent,err);
              //StartRotated(Canvas,indent);
            end;
        'Z':Invisible := True;
        end;
      end;

      if (VarPos('>',s,TagPos) > 0) and not ImgBreak then
      begin
        Res := Res + Copy(s,1,TagPos);
        Delete(s,1,TagPos);
      end
      else
      begin
        if not Imgbreak then
          Delete(s,1,Length(s));
      end;
    end;

  end;

    {w := w - sw;

    if w > xsize then
      xsize := w;
     }
    Result := Res;
  end;

begin
  d := 0;
  {$IFDEF DELPHI_LLVM}
  inc(d);
  {$ENDIF}

  Anchor := False;
  isShad := False;
  Invisible := False;

  URLFont := TFont.Create;
  URLFont.Assign(DefFont);
  {$IFNDEF FMXLIB}
  URLFont.Color := URLColor;
  {$ENDIF}

  //PIndent := 0;

  //HlCount := 0;
  ListIndex := 0;
  //LiCount := 0;
  //StripVal := '';

  ImgIdx := 0;
  AltImg := -1;

  if Pos('&',s) > 0 then
  begin
    repeat
      Foundtag := False;

      if TagReplacestring('&amp;','&&',s) then Foundtag := True;
      if TagReplacestring('&quot;','"',s) then Foundtag := True;

      if TagReplacestring('&sect;','§',s) then Foundtag := True;
      if TagReplacestring('&permil;','®‰',s) then Foundtag := True;
      if TagReplacestring('&reg;','®',s) then Foundtag := True;

      if TagReplacestring('&copy;','©',s) then Foundtag := True;
      if TagReplacestring('&para;','¶',s) then Foundtag := True;

      if TagReplacestring('&trade;','™',s) then Foundtag := True;
      if TagReplacestring('&euro;','€',s) then Foundtag := True;
      if TagReplacestring('&pound;','£',s) then Foundtag := True;
      if TagReplacestring('&dollar;','$',s) then Foundtag := True;

    until not Foundtag;
  end;

  s := DBTagStrip(s);
  s := CRLFStrip(s,True);

  //InsPoint := 0;

  su := '';
  while Length(s) > 0 do
  begin
    {calculate part of the HTML text fitting on the next line}
    suph := 0;
    subh := 0;
    imgali := 0;
    isSup := False;
    isSub := False;

    //OldImgIdx := ImgIdx;

    su := su + ConvertHTMLLine(s,True);
  end;

  //Result := RTFEngine.GetText;
  URLFont.Free;
end;
{$WARNINGS ON}

//------------------------------------------------------------------------------

procedure HTMLToRTF(s:string; DefFont: TFont; RTFEngine: TRTFEngine);
begin
  HTMLToRTFEx(S, RTFEngine.Images, 0, False, False,False,False,False, False, 0, clBlue, clRed, clYellow, clGray, DefFont, RTFEngine);
end;

//------------------------------------------------------------------------------

{ TFontTable }

constructor TFontTable.Create;
begin
  inherited;
  FFonts := TFontCollection.Create(nil);
end;

//------------------------------------------------------------------------------

destructor TFontTable.Destroy;
begin
  FFonts.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TFontTable.AddFont(AFaceName: TFontName): String;
var
  i: Integer;
begin
  i := FFonts.IndexOf(AFaceName);
  if i >= 0 then
  begin
    Result := FFonts.Items[i].Code;
  end
  else
  begin
   with  FFonts.Add do
   begin
     FaceName := AFaceName;
     Result := Code;
   end;
  end;
end;

//------------------------------------------------------------------------------

function TFontTable.AddFont(AFont: TFont): string;
begin
  Result := AddFont(GetFontName(AFont));
end;

//------------------------------------------------------------------------------

function TFontTable.GetText: String;
var
  i: Integer;
begin
  if FFonts.Count <= 0 then
  begin
    Result := '';
    Exit;
  end;

  Result := '{\fonttbl';
  for i:= 0 to FFonts.Count-1 do
  begin
    Result := Result + FFonts.Items[i].GetText;
  end;
  Result := Result + '}';
end;

//------------------------------------------------------------------------------

function TFontTable.IndexOf(FaceName: String): Integer;
begin
  Result := FFonts.IndexOf(FaceName);
end;

//------------------------------------------------------------------------------

procedure TFontTable.LoadFonts(RTFString: String);
var
  TagPos, I, lastsp: Integer;
  fceName: String;
  AFont: TFont;
begin
// {\fonttbl{\f0\fnil Arial;}}
  TagPos := Pos('{\fonttbl', RTFString);
  if TagPos <= 0 then
    Exit;

  RTFString := copy(RTFString, TagPos + 9, Length(RTFString)-(TagPos+9));
  RTFString := RemoveStartingSpace(RTFString);

  TagPos := Pos(';}}', RTFString);
  if (CharInStr(RTFString,1) = '{') and (TagPos > 0) then
  begin
    AFont := TFont.Create;
    RTFString := copy(RTFString, 1, TagPos+3);

    while (Length(RTFString) > 0) do
    begin
      TagPos := Pos(';}', RTFString);
      if (TagPos <= 0) then
        Break;
      I := TagPos;
      Lastsp := -1;
      while (I > 0) do
      begin
        if CharInStr(RTFString,I) = ' ' then
          Lastsp := I
        else if (CharInStr(RTFString,I) = '\') then
        begin
          if Lastsp > 0 then
          begin
            FceName := Copy(RTFString, Lastsp, TagPos - Lastsp);
            SetFontName(AFont, Fcename);
            AddFont(AFont);
          end;
          Break;
        end;
        Dec(I);
      end;

      RTFString := Copy(RTFString, TagPos + 2, Length(RTFString) - (TagPos+2));
    end;
    
    AFont.Free;
  end;
end;

//------------------------------------------------------------------------------

{ TColorTable }

function TColorTable.AddColor(Clr: TColor): integer;
var
  i: Integer;
begin
  {$IFNDEF FMXLIB}
  Clr := ColorToRGB(Clr);
  {$ENDIF}

  i := FColorList.IndexOf(ColorToString(Clr));
  if i >= 0 then
  begin
    Result := i;
  end
  else
  begin
    i := FColorList.Add(ColorToString(Clr));
    Result := i;
  end;
end;

//------------------------------------------------------------------------------

constructor TColorTable.Create;
begin
  inherited;
  FColorList := TStringList.Create;
end;

//------------------------------------------------------------------------------

destructor TColorTable.Destroy;
begin
  FColorList.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function GetRVal(clr: TColor): byte;
begin
  Result := (clr and $FF);
end;

//------------------------------------------------------------------------------

function GetGVal(clr: TColor): byte;
begin
  Result := ((clr shr 8) and $FF);
end;

//------------------------------------------------------------------------------

function GetBVal(clr: TColor): byte;
begin
  Result := ((clr shr 16) and $FF);
end;

//------------------------------------------------------------------------------

function RGB(r,g,b : longint) : DWORD;
begin
  RGB := DWORD(((DWORD(BYTE(r))) or ((DWORD(WORD(g))) shl 8)) or ((DWORD(BYTE(b))) shl 16));
end;

//------------------------------------------------------------------------------

function TColorTable.GetText: String;
var
  i: Integer;
begin
  if FColorList.Count <= 0 then
  begin
    Result := '';
    Exit;
  end;

  Result := '{\colortbl ';
  for i := 0 to FColorList.Count-1 do
  begin
    Result := Result + '\red'+inttostr(GetRVal(StringToColor(FColorList[i])))+'\green'+Inttostr(GetGVal(StringToColor(FColorList[i])))+'\blue'+inttostr(GetBVal(StringToColor(FColorList[i])))+';';
  end;
    Result := Result + '}'
end;

//------------------------------------------------------------------------------

procedure TColorTable.LoadColors(RTFString: String);
var
  TagPos, I: Integer;
  Sub: String;

  function RTFStringToColor(S: String): TColor;
  var
    TagPos, I : Integer;
    Sub: String;
    R, G, B: Byte;
  begin
    {Result := clNone;
    if (Pos('\red', S) <= 0) or(Pos('\green', S) <= 0) or (Pos('\blue', S) <= 0) then
      Exit; }

    R := 0;
    TagPos := Pos('\red', S);
    if TagPos > 0 then
    begin
      Sub := '';
      I := TagPos + 4;

      {$IFDEF DELPHI_LLVM}
      while S[I - 1].IsNumber do
      begin
        Sub := Sub + S[I - 1];
      {$ELSE}
      while (IsNumChar(S[I])) do
      begin
        Sub := Sub + S[I];
      {$ENDIF}
        Inc(I);
        if (I > Length(S)) then
          Break;
      end;

      R := IStrToInt(Sub);
    end;

    G := 0;
    TagPos := Pos('\green', S);
    if TagPos > 0 then
    begin
      Sub := '';
      I := TagPos + 6;

      {$IFDEF DELPHI_LLVM}
      while S[I - 1].IsNumber do
      begin
        Sub := Sub + S[I - 1];
      {$ELSE}
      while (IsNumChar(S[I])) do
      begin
        Sub := Sub + S[I];
      {$ENDIF}
        Inc(I);
        if (I > Length(S)) then
          Break;
      end;
      G := IStrToInt(Sub);
    end;

    B := 0;
    TagPos := Pos('\blue', S);
    if TagPos > 0 then
    begin
      Sub := '';
      I := TagPos + 5;
      {$IFDEF DELPHI_LLVM}
      while S[I - 1].IsNumber do
      begin
        Sub := Sub + S[I - 1];
      {$ELSE}
      while (IsNumChar(S[I])) do
      begin
        Sub := Sub + S[I];
      {$ENDIF}
        Inc(I);
        if (I > Length(S)) then
          Break;
      end;
      b := IStrToInt(Sub);
    end;

    {$IFDEF FMXLIB}
    Result := MakeColor(R, G, B);
    {$ENDIF}
    {$IFNDEF FMXLIB}
    Result := RGB(R, G, B);
    {$ENDIF}
  end;
begin
// {\colortbl ;\red84\green84\blue84;\red0\green0\blue0;}
  TagPos := Pos('{\colortbl', RTFString);
  if TagPos <= 0 then
    Exit;

  RTFString := Copy(RTFString, TagPos + 10, Length(RTFString)-(TagPos+10));

  TagPos := Pos('}', RTFString);
  if TagPos <= 0 then
    Exit;
  Delete(RTFString, TagPos, Length(RTFString) - TagPos);

  RTFString := RemoveStartingSpace(RTFString);
  if CharInStr(RTFString,1) = ';' then
  begin
    RTFString := Copy(RTFString, 2, Length(RTFString)-1);
    AddColor(clBlack); // default Color 
  end;

  RTFString := RemoveStartingSpace(RTFString);

  I := 1;
  while I <= Length(RTFString) do
  begin
    if CharInStr(RTFString,I) = '\' then
      Break
    else
      Delete(RTFString, 1, 1);
    Inc(I);
  end;

  while Length(RTFString) > 0 do
  begin
    TagPos := Pos(';', RTFString);
    if TagPos <= 0 then
      Break;

    Sub := copy(RTFString, 1, TagPos);
    AddColor(RTFStringToColor(Sub));
    Delete(RTFString, 1, TagPos);
    //RTFString := copy(RTFString, TagPos+1, Length(RTFString) - (TagPos+1));
  end;
end;

//------------------------------------------------------------------------------

{ TRTFHeader }

constructor TRTFHeader.Create;
begin
  inherited;
  FViewKind := 4;
  FFontTable := TFontTable.Create;
  FColorTable := TColorTable.Create;
  FListTable := TListTable.Create;
end;

//------------------------------------------------------------------------------

destructor TRTFHeader.Destroy;
begin
  FFontTable.Free;
  FColorTable.Free;
  FListTable.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TRTFHeader.GetText: String;
begin
  Result := '{'+ RTFHeader + FFontTable.GetText + FColorTable.GetText +  FListTable.GetText +
            '\viewkind'+ inttostr(FViewKind)+'\uc1';
end;

//------------------------------------------------------------------------------

{ TRTFEngine }

procedure TRTFEngine.Clear;
begin
  FText := '';
  FRTFHeader.FColorTable.FColorList.Clear;
  FRTFHeader.FFontTable.FFonts.Clear;
  FRTFHeader.FListTable.FLists.Clear;
end;

//------------------------------------------------------------------------------

constructor TRTFEngine.Create;
begin
  inherited;
  FRTFHeader := TRTFHeader.Create;
  FNewLine := True;
  FFont := nil;
  FText := '';
  FIsPreviouseKW := True;
  FFontSize := 12;
  FHAlignment := taLeftJustify;
  FForeColor := clBlack;
  FBackColor := clWhite;
  FImages := nil;
  FCurrentCol := -1;
  FTotalCol := 0;
  FColColorList := TStringList.Create;
  FHighLightColor := clNone;
  {$IFDEF FMXLIB}
  FFontDPI := 72;
  {$ELSE}
  FFontDPI := 96;
  {$ENDIF}
  FPageSize := A4;
end;

//------------------------------------------------------------------------------

destructor TRTFEngine.Destroy;
begin
  FRTFHeader.Free;
  FColColorList.Free;
  if Assigned(FFont) then
    FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TRTFEngine.RefreshPara({%H-}S: String; KWCode: Integer): Boolean;
begin
  Result := False;
  if FNewLine then
  begin
    Result := (KWCode = 2);
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddIndent(Indent: integer);
begin
  AddInternal('\li'+IntToStr(PixelsToTwips(Indent)));
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddInternal(S: String; KWCode: Integer);
var
  enc: string;
  ch: char;
  i: integer;
begin
  if KWCode = 100 then
  begin
    S := StringReplace(S, '\', '\\', [rfReplaceAll]);
    S := StringReplace(S, '{', '\{', [rfReplaceAll]);
    S := StringReplace(S, '}', '\}', [rfReplaceAll]);
    S := ReplaceCR(S, True);

    // encode unicode chars
    enc := '';
    for i := 1 to length(S) do
    begin
      ch := CharInStr(S,i);
      {$IFNDEF LCLLIB}
      if ord(ch) > 127 then
      {$ENDIF}
      {$IFDEF LCLLIB}
      if ch > #127 then
      {$ENDIF}
        enc := enc + '\u' + IntToStr(ord(ch))+ 'G'
      else
       enc := enc + ch;
    end;

    S := enc;
  end;

  if RefreshPara(S, KWCode) then
  begin
    S := '\pard'+ S;
  end;

  if FIsPreviouseKW and (KWCode = 100) then       // 100 for Text
    S := ' '+ S;

  FText := FText + S;

  if (KWCode = 100) or (KWCode = 12{NextBullet}) then
    FIsPreviouseKW := False
  else
    FIsPreviouseKW := True;

  if FNewLine and (KWCode <> 1) then
    FNewLine := False;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddHAlignment(Align: TAlignment);
begin
  case Align of
    taLeftJustify : AddInternal('\ql', 2);
    taCenter      : AddInternal('\qc', 2);
    taRightJustify: AddInternal('\qr', 2);
  end;
  FHAlignment := Align;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddBackGroundColor(Clr: TColor);
begin
//  AddInternal('\cb'+Inttostr(FRTFHeader.ColorTable.AddColor(Clr)));
  AddInternal('\chshdng0\chcbpat'+Inttostr(FRTFHeader.ColorTable.AddColor(Clr)));

  FBackColor := Clr;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddFont(AFont: TFont);
begin
  if not Assigned(FFont) then
    FFont := TFont.Create;

  AddInternal(FRTFHeader.FontTable.AddFont(AFont));
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddForeColor(Clr: TColor);
begin
  AddInternal('\cf'+Inttostr(FRTFHeader.ColorTable.AddColor(Clr)));
  FForeColor := Clr;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddNewLine;
begin
//  if FStartBullet then
///    NextBullet(false)
//  else
  begin
    //if FLI <> 0 then
    //  AddInternal('\par\li' + inttostr(PixelsToTwips(FLI)), 1)
    //else
      AddInternal('\par', 1);
    FNewLine := True;
  end;
end;

procedure TRTFEngine.AddLine;
begin
//  if FStartBullet then
///    NextBullet(false)
//  else
  begin
    AddInternal('\line', 1);
    FNewLine := True;
  end;
end;


//------------------------------------------------------------------------------

procedure TRTFEngine.AddParagraph(LeftInd, RightInd: Integer);
begin
  FLI := LeftInd;

//  if LeftInd = 0 then
    AddInternal('\par');
//  else
//    AddInternal('\par\li'+inttostr(LeftInd));

  if RightInd > 0 then
    AddInternal('\ri'+inttostr(RightInd));
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddTab;
begin
  AddInternal('\tab');
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddText(T: String);
begin
  AddInternal(T, 100);
end;

//------------------------------------------------------------------------------

function TRTFEngine.GetText: string;
begin
  Result := FRTFHeader.GetText + FText + '\par}';
end;

//------------------------------------------------------------------------------

function TRTFEngine.MaxPageHeight: integer;
begin
  case FPageSize of
    A4: Result := 1123;
    A5: Result := 794;
    Letter: Result := 1056;
  else
    Result := 1123;
  end
end;

//------------------------------------------------------------------------------

function TRTFEngine.MaxPageWidth: integer;
begin
  case FPageSize of
    A4: Result := 794;
    A5: Result := 559;
    Letter: Result := 816;
  else
    Result := 794;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.GetBuffer(var Buffer, FontTable,ColorTable: string);
begin
  Buffer := FText;
  FontTable := FRTFHeader.FontTable.GetText;
  ColorTable := FRTFHeader.ColorTable.GetText;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddBold(Value: Boolean);
begin
  if Value then
    AddInternal('\b')
  else
    AddInternal('\b0');
    
  FBold := Value;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddFontSize(Value: Single);
var
  i: integer;
begin
  i := Round(Value);
  AddFontSize(i);
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddFontSize(Value: Integer);
begin
  AddInternal('\fs' + IntToStr(Round(Value * FFontDPI/96)));
  FFontSize := Value;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddItalic(Value: Boolean);
begin
  if Value then
    AddInternal('\i')
  else
    AddInternal('\i0');

  FItalic := Value;  
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddUnderLine(Value: Boolean);
begin
  if Value then
    AddInternal('\ul')
  else
    AddInternal('\ulnone');

  FUnderLine := Value;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddHighLightColor(Clr: TColor);
begin
  if clr = clNone then
    AddInternal('\highlight0')
  else
    AddInternal('\highlight'+ inttostr(FRTFHeader.ColorTable.AddColor(Clr)));

  FHighLightColor := Clr;    
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.SaveToFile(FileName: String);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := GetText;
  try

  {$IFNDEF LCLLIB}
  {$IF COMPILERVERSION > 24}
    sl.SaveToFile(FileName, TEncoding.ANSI);
  {$ELSE}
    sl.SaveToFile(FileName);
  {$IFEND}
  {$ENDIF}

  {$IFDEF LCLLIB}
    sl.SaveToFile(FileName);
  {$ENDIF}
  finally
    sl.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddColumn(ColWidth: Integer);
//var
//  s: string;
begin
//  if FWithBorders {and (FTotalCol = 0)} then
//    s := '\clbrdrl\brdrw10\brdrs\brdrcf0\clbrdrt\brdrw10\brdrs\brdrcf0\clbrdrr\brdrw10\brdrs\brdrcf0\clbrdrb\brdrw10\brdrs\brdrcf0 '
//  else
//    s := '';

  Inc(FTotalCol);
  FTableWidth := FTableWidth + ColWidth;
  {$IFNDEF LCLLIB}
  FColColorList.AddObject(ColorToString(RGB($FF,$FF,$FF)), TObject(FTableWidth));
  {$ENDIF}
  AddInternal('\cellx'+inttostr(FTableWidth), 6);
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.EndRow;
var
  S, brs: String;
  I, ci: Integer;

begin
  if FStartRow then
  begin
    S := '';
    for I := FCurrentCol to FTotalCol-1 do
    begin
      S := S + ' \cell';
    end;

    if FWithBorders then
      brs := '\clbrdrl\brdrw10\brdrs\brdrcf0\clbrdrt\brdrw10\brdrs\brdrcf0\clbrdrr\brdrw10\brdrs\brdrcf0\clbrdrb\brdrw10\brdrs\brdrcf0'
    else
      brs := '';

    S := S + '\pard \intbl {\rtlch \af0 \ltrch \trowd \ltrrow \ts15\trgaph108\trleft-108';
    case FTableAlignment of
      taLeftJustify : S := S + '\trql';
      taCenter      : S := S + '\trqc';
      taRightJustify: S := S + '\trqr';
    end;

    if FWithBorders then
      S := S + brs;

    for I := 0 to FColColorList.Count-1 do
    begin
      if (StringToColor(FColColorList[I]) <> $FFFFFF) then
      begin
        ci := FRTFHeader.ColorTable.AddColor(StringToColor(FColColorList[I]));
        S := S + '\clcfpat'+InttoStr(ci)+'\clshdng10000\clcfpatraw'+InttoStr(ci)+'\clshdngraw10000 ';
      end;
      {$IFNDEF LCLLIB}
      S := S + ' \cellx'+ InttoStr(Integer(FColColorList.Objects[I]));
      {$ENDIF}
      if FWithBorders {and (i = 0)} then
        S := S + brs;
    end;

    FStartRow := False;
    FCurrentCol := -1;
    AddInternal(S + '\row }');
    //AddInternal('\row');
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.StartRow;
var
  i: Integer;
begin
  FStartRow := True;
  FCurrentCol := 0;
  for i := 0 to FColColorList.Count-1 do
    FColColorList[i] := ColorToString(RGB($FF,$FF,$FF));
  AddInternal('\pard\intbl ', 7);
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddRow;
begin
  EndRow;
  StartRow;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.StartTable(Align: TAlignment; WithBorders: Boolean = True);
var
  s: string;
begin
  FStartTable := True;
  FTableWidth := 0;
  FTotalCol := 0;
  FColColorList.Clear;
  FTableAlignment := Align;
  FWithBorders := WithBorders;
  if FWithBorders then
    s := '\clbrdrl\brdrw10\brdrs\brdrcf0\clbrdrt\brdrw10\brdrs\brdrcf0\clbrdrr\brdrw10\brdrs\brdrcf0\clbrdrb\brdrw10\brdrs\brdrcf0 '
  else
    s := '';

  case Align of
    taLeftJustify : AddInternal('\trowd\trgaph108\trleft-108\trql' + s, 5);
    taCenter      : AddInternal('\trowd\trgaph108\trleft-108\trqc' + s, 5);
    taRightJustify: AddInternal('\trowd\trgaph108\trleft-108\trqr' + s, 5);
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.NextCell;
begin
  Inc(FCurrentCol);
  AddInternal('\cell ',8);
end;

//------------------------------------------------------------------------------

class function TRTFEngine.PixelsToTwips(pixels: integer): integer;
begin
  {$IFNDEF FMXLIB}
  Result := Round(pixels / Screen.PixelsPerInch * TwipsPerInch);
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := Round(pixels / 96 * TwipsPerInch);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

class function TRTFEngine.TwipsToPixels(twips: integer): integer;
begin
  {$IFNDEF FMXLIB}
  Result := Round(twips * Screen.PixelsPerInch / TwipsPerInch);
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := Round(twips * 96 / TwipsPerInch);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.EndTable;
begin
  if FStartTable then
  begin
    AddInternal('\pard\par', 5);
    FStartTable := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddFontName(FontName: TFontName);
var
  AFont: TFont;
begin
  AFont := TFont.Create;
  SetFontName(AFont, FontName);
  AddInternal(FRTFHeader.FontTable.AddFont(AFont));
  AFont.Free;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddStrikeOut(Value: Boolean);
begin
  if Value then
    AddInternal('\strike')
  else
    AddInternal('\strike0');

  FStrikeOut := Value;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddHTML(S: String);
var
  AFont: TFont;
begin
  AFont := TFont.Create;
  HTMLToRTF(S, AFont, self);
  AFont.Free;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.EndBullet;
begin
  if FStartBullet then
  begin
    AddInternal('\par\pard\ltrpar', 11);
    FStartBullet := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.StartBullet(Number: boolean; FontName: TFontName; CharNo: Integer; Color: TColor);
var
  aFont: TFont;
  li,ci: Integer;

begin
  if not FStartBullet and (FontName <> '') then
  begin
    AFont := TFont.Create;
    SetFontName(AFont, FontName);
    FBulletFont := FRTFHeader.FontTable.AddFont(AFont);
    ci := FRTFHeader.ColorTable.AddColor(Color);

    if FontName = 'Symbol' then
    begin
      // Symbol font mapping
      case CharNo of
      0: FBulletChar := '''B7';
      1: FBulletChar := '''B0';
      2: FBulletChar := '''AE';
      3: FBulletChar := '''D6';
      4: FBulletChar := '''2A';
      end;
    end
    else
    begin
      // Arial font mapping
      case CharNo of
      0: FBulletChar := '''BA';
      1: FBulletChar := '''B7';
      2: FBulletChar := '''2D';
      3: FBulletChar := '''7E';
      4: FBulletChar := '''2A';
      end;
    end;

    if Number then
      FBulletChar := '''00';

    li := FRTFHeader.ListTable.AddList(FBulletChar);

    { Wingdings font mapping
    case CharNo of
    0: FBulletChar := '''9F';
    1: FBulletChar := '''A7';
    2: FBulletChar := '''E0';
    3: FBulletChar := '''FC';
    4: FBulletChar := '''AB';
    end;
    }

    AddInternal('\ls'+inttostr(li)+'\ilvl0\cf'+inttostr(ci));

    if Number then
      AddInternal('{\listtext'+ FBulletFont +' 1.\tab}')
    else
      AddInternal('{\listtext'+ FBulletFont +' \' + FBulletChar+'\tab}');

//    if Number then
//      AddInternal('\pard{\listtext\f0 1.\tab}{\*\pn\pnlvlbody\pnf0\pnindent0\pnstart1\pndec{\pntxta.}}', 11)
//    else
//      AddInternal('\pard{\pntext'+FBulletFont+'\''B'+inttostr(CharNo)+'\tab}{\*\pn\pnlvlblt\pnf'+inttostr(fi)+'\pnindent0{\pntxtb\''D'+inttostr(CharNo)+'}}\ltrpar\fi-720\li720', 11);
//
//      AddInternal('\pard{\listtext'+FBulletFont+'\'+ FBulletChar+'\tab}{\*\pn\pnlvlblt\pnf'+inttostr(fi)+'\pnindent0{\pntxtb'+FBulletFont+'\'+ FBulletChar+'}}\ltrpar\fi-720\li720', 11);
    FStartBullet := True;
    AFont.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.NextBullet(Number: boolean; Index: integer);
begin
  if FStartBullet then
  begin
    if Number then
      AddInternal('\'#13#10'{\listtext'+ FBulletFont +' ' + inttostr(index+1) +'.\tab}')
    else
      AddInternal('\'#13#10'{\listtext'+ FBulletFont +' \' + FBulletChar+'\tab}');
    (*
    if Number then
      AddInternal('\par {\listtext'+FBulletFont+'\ '+IntToStr(Index)+'.\tab}', 12)
    else
      AddInternal('\par {\listtext'+FBulletFont+'\'+FBulletChar+'\tab}', 12);
    *)
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.ReDefColWidth;
begin
  if FStartTable then
  begin
    //FTableWidth := 0;
    //AddInternal('\trowd\trgaph108\trleft-108');
    StartTable(FTableAlignment, FWithBorders);
  end;
end;

//------------------------------------------------------------------------------

function TRTFEngine.ReplaceCR(s: string; dobreak: boolean): string;
var
  i: Integer;
  ch: char;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    ch := CharInStr(s,i);
    if not ((ch = #13) or (ch = #10)) then
    begin
      Result := Result + ch;
      if FNewLine then
        FNewLine := False;
    end
    else
      if (ch = #13) and dobreak then
      begin
       { if FStartBullet then
        begin
          Result := Result + '\par\pard\ltrpar';
          FStartBullet := False;
        end;}
        Result := Result + '\par ';
        FNewLine := True;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.SaveToStream(st: TStream);
var
  {$IFDEF FNCLIB}
  ss: TStringStream;
  {$ENDIF}
  {$IFNDEF FNCLIB}
  sa: RawByteString;
  {$ENDIF}
begin
  {$IFDEF FNCLIB}
  {$IFNDEF LCLLIB}
  ss := TStringStream.Create('',TEncoding.ANSI);
  {$ENDIF}
  {$IFDEF LCLLIB}
  ss := TStringStream.Create('');
  {$ENDIF}
  try
    ss.WriteString(GetText);
    ss.Position := 0;
    st.CopyFrom(ss, ss.size);
  finally
    ss.Free;
  end;
  {$ENDIF}

  {$IFNDEF FNCLIB}
  if Assigned(st) then
  begin
    {$WARNINGS OFF}
    sa := Utf8ToAnsi(GetText);
    {$WARNINGS ON}
    st.Write(TBytes(sa),Length(sa));
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddPageBreak;
begin
  AddInternal('\par\page ');
end;

//------------------------------------------------------------------------------
procedure TRTFEngine.AddHyperLink(Link, Text: String; AFont: TFont);
begin
  AddHyperlink(Link, Text, AFont, clBlue);
end;
//------------------------------------------------------------------------------

procedure TRTFEngine.AddHyperLink(Link, Text: String; AFont: TFont; URLColor: TColor);
var
  fi, ci: Integer;
  S: String;
begin
  fi := FRTFHeader.FontTable.IndexOf(GetFontName(AFont));
  ci := FRTFHeader.ColorTable.AddColor(URLColor);
  S := '{\field{\*\fldinst {\rtlch \af'+inttostr(fi)+'\afs'+inttostr(GetFontSize(AFont)*2)+'\ltrch \f'+inttostr(fi)+'\fs'+inttostr(GetFontSize(AFont)*2)+'  HYPERLINK "'+
        Link+'" }}{\fldrslt {\rtlch \af'+inttostr(fi)+'\afs'+inttostr(GetFontSize(AFont)*2)+' \ltrch \f'+inttostr(fi)+'\fs'+inttostr(GetFontSize(AFont)*2)+'\ul\cf'+inttostr(ci)+' '+Text+'}}}';

  FText := FText + S;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddHyperLinkPic(Link: string; Pic: TPicture);
var
  S: String;
begin
  S := '{\field{\*\fldinst HYPERLINK "'+
        Link+'" {\fldrslt '+ PicAsString(Pic)+ '}}}';

  FText := FText + S;
end;

//------------------------------------------------------------------------------
{$IFDEF FMXLIB}
function TRTFEngine.AddBitmap(ABmp: TBitmap; Bkg: TAlphaColor = claWhite): string;
const
  IMAGE_DIVIDER = 25.3;
var
  Str: TMemoryStream;
  j, dx, dy, n, n1: Integer;
  s: string;
  CellsLine, PicString: string;
  bArr: array[0..1023] of Byte;
  bmp: TBitmap;
  R: TRect;
begin
  if not Assigned(ABmp) or ABmp.IsEmpty then
    Exit;

  bmp := TBitmap.Create(15,15);

  try
    bmp.SetSize(ABmp.Width, ABmp.Height);
    if (bmp.Height > 500) or (bmp.Width > 500) then
    begin
      if (bmp.Height > 500) then
        bmp.Height := 500;
      if (bmp.Width > 500) then
        bmp.Width := 500;

      R := Rect(0, 0, bmp.Width, bmp.Height);
      bmp.Canvas.BeginScene;
      bmp.Canvas.Clear(Bkg);
      bmp.Canvas.DrawBitmap(ABmp, RectF(0,0,ABmp.Width, ABmp.Height),RectF(0,0,Bmp.Width, Bmp.Height),1);
      bmp.Canvas.EndScene;
    end
    else
    begin
      bmp.Canvas.BeginScene;
      bmp.Canvas.Clear(Bkg);
      bmp.Canvas.DrawBitmap(ABmp, RectF(0,0,ABmp.Width, ABmp.Height),RectF(0,0,ABmp.Width, ABmp.Height),1);
      bmp.Canvas.EndScene;
    end;

    PicString := '';
    Result := '';

    if not ((bmp = nil) or bmp.IsEmpty) then
    begin
      Str := TMemoryStream.Create;
      try
        dx := Round(bmp.Width);
        dy := Round(bmp.Height);

        Str.Position := 0;
        bmp.SaveToStream(str);
        Str.Position := 0;
        CellsLine := '{\sb0\li0\sl0\slmult0 {\pict\pngblip\picw' + FloatToStr(Round(dx * IMAGE_DIVIDER)) +
             '\pich' + FloatToStr(Round(dy * IMAGE_DIVIDER)) + '' + #13#10;

        PicString := PicString + CellsLine;

        n1 := 0;
        s := '';
        repeat
          n := Str.Read(bArr[0], 1024);
          for j := 0 to n - 1 do
          begin
            s := s + IntToHex(bArr[j], 2);
            Inc(n1);
            if n1 > 63 then
            begin
              n1 := 0;
              CellsLine := s + #13#10;
              PicString := PicString + CellsLine;
              s := '';
            end;
          end;
        until n < 1024;
      finally
        Str.Free;
      end;
      if n1 <> 0 then
      begin
        CellsLine := s + #13#10;
        PicString := PicString + CellsLine;
      end;
      s := '030000000000}';
      CellsLine := s + '}' + #13#10;
      PicString := PicString + CellsLine;

      FText := FText + PicString;
      Result := PicString;
    end;
  finally
    bmp.Free;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

function TRTFEngine.PicAsString(Pic: TPicture): string;
{$IFNDEF FMXLIB}
const
  IMAGE_DIVIDER = 25.3;
var
  Graphic: TGraphic;
  Str: TMemoryStream;
  j, fx, fy, dx, dy, n, n1: Integer;
  s, s0, s1: String;
  CellsLine, PicString: String;
  bArr: array[0..1023] of Byte;
  bmp: TBitmap;
  R: TRect;
{$ENDIF}
begin
  Result := '';
{$IFNDEF FMXLIB}
  if not Assigned(Pic) then
    Exit;

  bmp := TBitMap.Create;
  try
    bmp.Width := Pic.Width;
    bmp.Height := Pic.Height;
    if (bmp.Height > 150) or (bmp.Width > 150) then
    begin
      if (bmp.Height > 150) then
        bmp.Height := 150;
      if (bmp.Width > 150) then
        bmp.Width := 150;

      R := Rect(0, 0, bmp.Width, bmp.Height);
      bmp.Canvas.StretchDraw(R, Pic.Graphic);
    end
    else
      bmp.Canvas.Draw(0, 0, Pic.Graphic);

    PicString := '';
    Result := '';
    n := 0;
    bArr[0] := 0;
    Graphic := bmp;
    if not ((Graphic = nil) or Graphic.Empty) then
    begin
      Str := TMemoryStream.Create;
      //CellsStream := TMemoryStream.Create;
      try
        dx := Round(bmp.Width);
        dy := Round(bmp.Height);
        fx := Graphic.Width;
        fy := Graphic.Height;
        Graphic.SaveToStream(Str);
        Str.Position := 0;
        CellsLine := '{\sb0\li0\sl0\slmult0 {\pict\wmetafile8\picw' + FloatToStr(Round(dx * IMAGE_DIVIDER)) +
             '\pich' + FloatToStr(Round(dy * IMAGE_DIVIDER)) + '\picbmp\picbpp4' + #13#10;
        //CellsStream.Write(CellsLine[1], Length(CellsLine));
        PicString := PicString + CellsLine;
        Str.Read(n, 2);
        Str.Read(n, 4);
        n := n div 2 + 7;
        s0 := IntToHex(n + $24, 8);
        s := '010009000003' + Copy(s0, 7, 2) + Copy(s0, 5, 2) +
             Copy(s0, 3, 2) + Copy(s0, 1, 2) + '0000';
        s0 := IntToHex(n, 8);
        s1 := Copy(s0, 7, 2) + Copy(s0, 5, 2) + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s := s + s1 + '0000050000000b0200000000050000000c02';
        s0 := IntToHex(fy, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s0 := IntToHex(fx, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) +
          '05000000090200000000050000000102ffffff000400000007010300' + s1 +
          '430f2000cc000000';
        s0 := IntToHex(fy, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s0 := IntToHex(fx, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) + '00000000';
        s0 := IntToHex(fy, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s0 := IntToHex(fx, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) + '00000000';
        CellsLine := s + #13#10;
        //CellsStream.Write(CellsLine[1], Length(CellsLine));
        PicString := PicString + CellsLine;
        Str.Read(bArr[0], 8);
        n1 := 0; s := '';
        repeat
          n := Str.Read(bArr[0], 1024);
          for j := 0 to n - 1 do
          begin
            s := s + IntToHex(bArr[j], 2);
            Inc(n1);
            if n1 > 63 then
            begin
              n1 := 0;
              CellsLine := s + #13#10;
              //CellsStream.Write(CellsLine[1], Length(CellsLine));
              PicString := PicString + CellsLine;
              s := '';
            end;
          end;
        until n < 1024;
      finally
        Str.Free;
      end;
      if n1 <> 0 then
      begin
        CellsLine := s + #13#10;
        //CellsStream.Write(CellsLine[1], Length(CellsLine));
        PicString := PicString + CellsLine;
      end;
      s := '030000000000}';
      CellsLine := s + '}' + #13#10;
      //CellsStream.Write(CellsLine[1], Length(CellsLine));
      PicString := PicString + CellsLine;

      //FText := FText + PicString;
      Result := PicString;
      //if CellsStream.Size > 0 then
      begin
        try
          //FS := TFileStream.Create('xyz.rtf', fmCreate);
          //FS.CopyFrom(CellsStream, 0);
          //FS.Free;
        except
        end;
      end;
      //CellsStream.Free;
    end;
  finally
    bmp.Free;
  end;
{$ENDIF}
end;

function TRTFEngine.PicAsString(Pic: TGDIPPicture; PictureWidth, PictureHeight: integer): string;
{$IFNDEF FMXLIB}
const
  IMAGE_DIVIDER = 25.3;
var
  Graphic: TGraphic;
  Str: TMemoryStream;
  j, fx, fy, dx, dy, n, n1: Integer;
  s, s0, s1: String;
  CellsLine, PicString: String;
  bArr: array[0..1023] of Byte;
  bmp: TBitmap;
  R: TRect;
{$ENDIF}
begin
  Result := '';
{$IFNDEF FMXLIB}
  if not Assigned(Pic) then
    Exit;

  bmp := TBitmap.Create;
  try
    if Pic.Width = 0 then
      Pic.GetImageSizes;
    bmp.Width := Pic.Width;
    bmp.Height := Pic.Height;

//    if (bmp.Height > MaxPageHeight) or (bmp.Width > MaxPageWidth) then
//    begin
//      if (bmp.Height > MaxPageHeight) then
//        bmp.Height := MaxPageHeight;
//      if (bmp.Width > MaxPageWidth) then
//        bmp.Width := MaxPageWidth;
//
//      R := Rect(0, 0, bmp.Width, bmp.Height);
//      bmp.Canvas.StretchDraw(R, Pic.Graphic);
//    end
//    else
    begin
      R := Rect(0, 0, bmp.Width, bmp.Height);
      bmp.Canvas.StretchDraw(R, Pic.Graphic);
      bmp.Canvas.Draw(0,0,Pic.Graphic);
    end;

    PicString := '';
    Result := '';
    Graphic := bmp;
    n := 0;
    bArr[0] := 0;

    if not ((Graphic = nil) or Graphic.Empty) then
    begin
      Str := TMemoryStream.Create;
      try
        dx := Round(PictureWidth);
        dy := Round(PictureHeight);
        fx := Graphic.Width;
        fy := Graphic.Height;
        Graphic.SaveToStream(Str);
        Str.Position := 0;
        CellsLine := '{\sb0\li0\sl0\slmult0 {\pict\wmetafile8\picw' + FloatToStr(Round(dx * IMAGE_DIVIDER)) +
             '\pich' + FloatToStr(Round(dy * IMAGE_DIVIDER)) + '\picbmp\picbpp4' + #13#10;
        PicString := PicString + CellsLine;
        Str.Read(n, 2);
        Str.Read(n, 4);
        n := n div 2 + 7;
        s0 := IntToHex(n + $24, 8);
        s := '010009000003' + Copy(s0, 7, 2) + Copy(s0, 5, 2) +
             Copy(s0, 3, 2) + Copy(s0, 1, 2) + '0000';
        s0 := IntToHex(n, 8);
        s1 := Copy(s0, 7, 2) + Copy(s0, 5, 2) + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s := s + s1 + '0000050000000b0200000000050000000c02';
        s0 := IntToHex(fy, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s0 := IntToHex(fx, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) +
          '05000000090200000000050000000102ffffff000400000007010300' + s1 +
          '430f2000cc000000';
        s0 := IntToHex(fy, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s0 := IntToHex(fx, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) + '00000000';
        s0 := IntToHex(fy, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
        s0 := IntToHex(fx, 4);
        s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) + '00000000';
        CellsLine := s + #13#10;
        PicString := PicString + CellsLine;
        Str.Read(bArr[0], 8);
        n1 := 0; s := '';
        repeat
          n := Str.Read(bArr[0], 1024);
          for j := 0 to n - 1 do
          begin
            s := s + IntToHex(bArr[j], 2);
            Inc(n1);
            if n1 > 63 then
            begin
              n1 := 0;
              CellsLine := s + #13#10;
              PicString := PicString + CellsLine;
              s := '';
            end;
          end;
        until n < 1024;
      finally
        Str.Free;
      end;

      if n1 <> 0 then
      begin
        CellsLine := s + #13#10;
        PicString := PicString + CellsLine;
      end;

      s := '030000000000}';
      CellsLine := s + '}' + #13#10;
      PicString := PicString + CellsLine;
      Result := PicString;
    end;
  finally
    bmp.Free;
  end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

function TRTFEngine.AddPicture(Pic: TPicture): string;
begin
  Result := PicAsString(Pic);
  FText := FText + Result;
end;

//------------------------------------------------------------------------------

function TRTFEngine.AddPicture(Pic: TGDIPPicture; PictureWidth, PictureHeight: integer): string;
begin
  Result := PicAsString(Pic, PictureWidth, PictureHeight);
  FText := FText + Result;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddCellColor(Clr: TColor);
begin
  if FStartRow and (FCurrentCol >= 0) then
  begin
    //while FCurrentCol >= FColColorList.Count do
      //FColColorList.Add(ColorToString(clWhite));

    if FCurrentCol < FColColorList.Count then
      FColColorList[FCurrentCol] := ColorToString(Clr);
      
  end;
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddNormalScript;
begin
  AddInternal('\up0\nosupersub');
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddSubScript;
begin
  AddInternal('\sub\dn4');
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddSuperScript;
begin
  AddInternal('\super\up4');
end;

//------------------------------------------------------------------------------

procedure TRTFEngine.AddRTF(S: String);
var
  AFontTable: TFontTable;
  AColorTable: TColorTable;
  I, Braces: Integer;
  Tag, NewRTF: String;
  Tags: TStringList;
  ch: char;

  function RemoveRTFDef(S: String): String;
  var
    TagPos, I: Integer;
    Sub: String;
  begin
    Result := S;
    if Pos('{\rtf1', S) <= 0 then
      Exit;

    TagPos := Pos('\viewkind', S);
    if (TagPos > 0) then
    begin
      Result := copy(S, TagPos + 10, Length(S)-TagPos-9);

      Sub := copy(Result, 1, 4);
      //TagPos := Pos('\uc1', Sub);
      if (Sub = '\uc1') then
        Delete(Result, 1, 4);
        //Result := copy(Result, 1, TagPos);

      I := Length(Result);
      while (I > 0) do
      begin
        if (CharInStr(Result,I) = '}') then
        begin
          Result := Copy(Result, 1, I - 1);
          Break;
        end;
        Dec(I);
      end;
    end;
  end;

  function AcceptableTag(Tag: String): String;
  var
    Sub: String;
    I, Val, j: Integer;
  begin
    if (Tags.IndexOf(Tag) >= 0) then
      Result := Tag
    else
    begin
      Result := '';

      Sub := '';
      I := 1;
      while (I <= Length(Tag)) do
      begin
        if (IsNumChar(CharInStr(Tag,I))) then
        begin
          Sub := Sub + CharInStr(Tag,I);
          Delete(Tag, I, 1);
          Dec(I);
        end
        else
        begin
          if (Sub <> '') then
            Exit;
        end;

        Inc(I);
      end;
      Val := IStrToInt(Sub);

      if (Tag = '\cf') or (Tag = 'highlight') then
      begin
        if (Val >= 0) and (Val < AColorTable.Colors.Count) then
        begin
          Result := Tag + IntToStr(FRTFheader.ColorTable.AddColor(StringToColor(AColorTable.Colors[Val])));
        end;
      end
      else if (Tag = '\f') or (Tag = '\pnf') then
      begin
        if (Val >= 0) and (Val < AFontTable.Fonts.Count) then
        begin
          if (Tag = '\f') then
            Result := FRTFheader.FontTable.AddFont(AFontTable.Fonts[Val].FaceName)
          else
          begin
            j := FRTFheader.FontTable.IndexOf(AFontTable.Fonts[Val].FaceName);
            if (j < 0) then
            begin
              FRTFheader.FontTable.AddFont(AFontTable.Fonts[Val].FaceName);
              j := FRTFheader.FontTable.IndexOf(AFontTable.Fonts[Val].FaceName);
            end;
            Result := Tag + IntToStr(j);
          end;
        end;
      end
      else if (Tag = '\fs') or (Tag = '\li') or (Tag = '\fi-') then
      begin
        Result := Tag + Sub;
      end;

    end;
  end;

  procedure AddTag;
  begin
    if (Tag <> '') then
    begin
      NewRTF := NewRTF + AcceptableTag(Tag);

      Tag := '';
    end;
  end;

begin
  AFontTable := TFontTable.Create;
  AFontTable.LoadFonts(S);
  AColorTable := TColorTable.Create;
  AColorTable.LoadColors(S);

  Tags := TStringlist.Create;

  if not FStartTable then
    Tags.Add('\pard');
  Tags.Add('\ql'); Tags.Add('\qc');
  Tags.Add('\qr'); Tags.Add('\par'); Tags.Add('\tab');
  Tags.Add('\b'); Tags.Add('\b0'); Tags.Add('\i');
  Tags.Add('\i0'); Tags.Add('\ul'); Tags.Add('\ulnone');
  Tags.Add('\highlight0'); Tags.Add('\strike'); Tags.Add('\strike0');
  Tags.Add('\ltrpar'); Tags.Add('\pntext'); Tags.Add('\pnlvlblt');
  Tags.Add('\pnindent0'); Tags.Add('\pntxtb');
  Tags.Add('\''B7'); Tags.Add('\*'); Tags.Add('\pn');
  Tags.Add('\up0'); Tags.Add('\dn4'); Tags.Add('\up4');
  Tags.Add('\page'); Tags.Add('\li720'); Tags.Add('\fi-720');

  {
  //--- Add font
  for I:= 0 to AFontTable.Fonts.Count-1 do
  begin
    FRTFHeader.FontTable.AddFont(AFontTable.Fonts[I].FaceName);
  end;
  //--- Add Color
  for I:= 0 to AColorTable.Colors.Count-1 do
  begin

  end;  }

  //--- Remove RTF definition
  S := RemoveRTFDef(S);
  //S := RemoveStartingSpace(S);

  I := 1;
  Tag := '';
  Braces := 0;
  NewRTF := '';
  while (I <= Length(s)) do
  begin
    ch := CharInStr(S,I);
    case ch of
      '\':
      begin
        AddTag;
        Tag := '\';
      end;
      ' ':
      begin
        AddTag;
        NewRTF := NewRTF + ch;
      end;
      '{':
      begin
        AddTag;
        Inc(Braces);
        NewRTF := NewRTF + ch;
      end;
      '}':
      begin
        AddTag;
        Dec(Braces);
        NewRTF := NewRTF + ch;
      end;
      {'0','1','2','3','4','5','6','7','8','9':
      begin

      end}
      else
      begin
        if (Tag <> '') then
          Tag := Tag + ch
        else
          NewRTF := NewRTF + ch;
      end;
    end;


    Inc(I);
  end;

  if (Braces <> 0) then
  begin

  end;

  FText := FText + NewRTF;
  AFontTable.Free;
  AColorTable.Free;
  Tags.Free;
end;

//------------------------------------------------------------------------------

{ TFontCollection }

function TFontCollection.Add: TFontItem;
begin
  Result := TFontItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TFontCollection.Create(AOwner: TComponent);
begin
  inherited Create(TFontItem);
end;

//------------------------------------------------------------------------------

function TFontCollection.GetItem(Index: Integer): TFontItem;
begin
  Result := TFontItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TFontCollection.IndexOf(FaceName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i:= 0 to Self.Count-1 do
  begin
    if UpperCase(Items[i].FaceName) = UpperCase(FaceName) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TFontCollection.Insert(Index: Integer): TFontItem;
begin
  Result := TFontItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TFontCollection.SetItem(Index: Integer; const Value: TFontItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TFontItem }

constructor TFontItem.Create(Collection: TCollection);
begin
  inherited;

end;

//------------------------------------------------------------------------------

destructor TFontItem.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

function TFontItem.GetCode: String;
begin
  Result := '\f'+ inttostr(Index);
end;

//------------------------------------------------------------------------------

function TFontItem.GetText: String;
begin
  Result := '{'+Code+'\fnil '+FaceName+';}';
end;

//------------------------------------------------------------------------------

procedure TFontItem.SetFaceName(const Value: String);
begin
  FFaceName := Value;
end;

//------------------------------------------------------------------------------

function RTFHeader: string;
begin
  Result := RTF_DEF;
end;

{ TListTable }

function TListTable.AddList(ABullet: string): integer;
begin
  FLists.Add(ABullet);
  Result := FLists.Count;
end;

constructor TListTable.Create;
begin
  FLists := TStringList.Create;
end;

destructor TListTable.Destroy;
begin
  FLists.Free;
  inherited;
end;

function TListTable.GetText: String;
var
  i: integer;
  id,bul: string;
begin
  Result := '';
  for i := 0 to FLists.Count - 1 do
  begin
    id := inttostr(i + 1);

    bul := FLists[i];

    if bul = '''00' then
      Result := Result + '{\*\listtable{\list\listtemplateid'+id+'\listhybrid{\listlevel\levelnfc0\levelnfcn0\leveljc0\leveljcn0\levelfollow0\levelstartat1\levelspace360\levelindent0{\*\levelmarker \{decimal\}.}{\leveltext\leveltemplateid1\''02\''00.;}{\levelnumbers\''01;}\fi-360\li720\lin720 }{\listname ;}\listid'+id+'}}'
        +'{\*\listoverridetable{\listoverride\listid'+id+'\listoverridecount0\ls'+id+'}}'
    else
      Result := Result + '{\*\listtable{\list\listtemplateid'+id+'\listhybrid{\listlevel\levelnfc23\levelnfcn23\leveljc0\leveljcn0\levelfollow0\levelstartat1\levelspace360\levelindent0{\*\levelmarker \{check\}}{\leveltext\leveltemplateid'+id+'\''01\'+ bul+';}{\levelnumbers;}\fi-360\li720\lin720 }{\listname ;}\listid'+id+'}}'
      + '{\*\listoverridetable{\listoverride\listid'+id+'\listoverridecount0\ls'+id+'}}'
  end;
end;


{ TRTFDocument }

procedure TRTFDocument.AddBullet;
var
  re: TRTFElement;
begin
  re := TRTFElement.Create;
  re.BulletType := rbtItem;
  ElementTable.Add(re);
  inc(FBullets);
end;

procedure TRTFDocument.AddBulletEnd;
var
  re: TRTFElement;
begin
  if FBullets = 0 then
    Exit;
  re := TRTFElement.Create;
  re.BulletType := rbtEnd;
  ElementTable.Add(re);
end;

procedure TRTFDocument.AddBulletStart;
var
  re: TRTFElement;
begin
  re := TRTFElement.Create;
  re.BulletType := rbtStart;
  ElementTable.Add(re);
  FBullets := 0;
end;

constructor TRTFDocument.Create;
begin
  inherited;
  FColorTable := TRTFColorTable.Create;
  FFontTable := TRTFFontTable.Create;
  FElementTable := TRTFElementTable.Create;
  FRTFObjects := TRTFObjects.Create;
  FParseDepth := 0;
  FDefCP := 0;
  FDefUC := 0;
  {$IFDEF FMXLIB}
  FFontDPI := 72;
  {$ELSE}
  FFontDPI := 96;
  {$ENDIF}
end;

destructor TRTFDocument.Destroy;
var
  i: integer;
begin
  for i := 0 to FElementTable.Count - 1 do
  begin
    if Assigned(FElementTable.Items[i].Bitmap) then
    {$IFDEF FMXLIB}
      FElementTable.Items[i].Bitmap.DisposeOf;
    FElementTable.Items[i].DisposeOf;
    {$ENDIF}

    {$IFNDEF FMXLIB}
      FElementTable.Items[i].Bitmap.Free;
    FElementTable.Items[i].Free;
    {$ENDIF}
  end;

  for i := 0 to FColorTable.Count - 1 do
  {$IFDEF FMXLIB}
    FColorTable.Items[i].DisposeOf;
  {$ENDIF}
  {$IFNDEF FMXLIB}
    FColorTable.Items[i].Free;
  {$ENDIF}

  for i := 0 to FFontTable.Count - 1 do
  {$IFDEF FMXLIB}
    FFontTable.Items[i].DisposeOf;
  {$ENDIF}
  {$IFNDEF FMXLIB}
    FFontTable.Items[i].Free;
  {$ENDIF}

  for i := 0 to FRTFObjects.Count - 1 do
  {$IFDEF FMXLIB}
    FRTFObjects.Items[i].DisposeOf;
  {$ENDIF}
  {$IFNDEF FMXLIB}
    FRTFObjects.Items[i].Free;
  {$ENDIF}

  FColorTable.Free;
  FFontTable.Free;
  FElementTable.Free;
  FRTFObjects.Free;

  inherited;
end;

function TRTFDocument.FindFont(ID: string): integer;
var
  i: integer;

begin
  Result := -1;

  for i := 0 to FontTable.Count - 1 do
  begin
    if FontTable[i].ID = ID then
    begin
      Result := i;
      break;
    end;
  end;
end;

function SubStr(s: string; cnt: integer): string;
begin
  Result := Copy(s, 1, cnt);
end;

function TRTFDocument.IsUnicodeChar(s: string;var len: integer): boolean;
var
  i,e: integer;
  ch: char;
  uc2,gotnum: boolean;
begin
  Result := true;
  uc2 := false;
  gotnum := false;
  len := Length(s);

  for i := 1 to Length(s) do
  begin
    ch := CharInStr(s,i);

    if (i = 1) and ((ch <> 'u') and (ch <> 'U')) then
    begin
      Result := false;
      Break;
    end;

    if (i = 2) and ((ch = 'c') OR (ch = 'C')) then
    begin
      // ucX idenifier ?
      uc2 := true;
      Continue;
    end;

    if (i < Length(s)) and IsNumChar(ch) then
      gotnum := true;

    if (i > 2) and gotnum and ((ch = 'g') or (ch = 'G') or (ch = '?')) then
    begin
      len := i;
      Break;
    end;


    if (i > 1) and (i < Length(s)) then
    begin
      if not IsNumChar(ch) then
      begin
        Result := false;
        break;
      end;
    end;

    if (i = Length(s)) and IsNumChar(ch) then
    begin
      if uc2 then
      begin
        val(ch, FDefUC, e);
        if e <> 0 then
          FDefUC := 0;

        Result := false;
      end;

      Break;
    end;

  end;
end;

function TRTFDocument.ParseUnicodeChar(s: string; var nextrep: char): string;
var
  v: word;
  e: integer;
  ch: char;
begin
  Result := '';
  nextrep := '\';

  Delete(s,1,1);

  ch := CharInStr(s, Length(s));

  if not IsNumChar(ch) then
  begin
    if (ch <> 'g') and (ch <> 'G') then
      nextrep := ch
    else
      nextrep := #0;
    s := Copy(s, 1, Length(s) - 1);
  end;

  Val(s, v, e);

  if e = 0 then
  begin
    {$IFDEF LCLLIB}
    Result := UnicodeToUTF8(v);
    {$ENDIF}
    {$IFNDEF LCLLIB}
    Result := Char(v);
    {$ENDIF}
  end;
end;

function TRTFDocument.ParseColor(s: string): TColor;
var
  r,g,b: byte;
  su: string;
  e,i: integer;
  rr,rg,rb: integer;
begin
  r := 0;
  g := 0;
  b := 0;

  rr := pos('red',s);
  rg := pos('green',s);
  rb := pos('blue',s);

  if rr = 1 then
    s := '\' + s;

  // in case MSWord color prefixes like \caccenttwo\ctint102\cshade255\red247\green202\blue172;
  if (rr > 1) and (rg > rr) and (rb > rg) then
  begin
    delete(s, 1, rr - 2);
  end;

  if pos('\red',s) = 1 then
  begin
    delete(s,1,4);
    i := pos('\',s);

    if i > 0 then
    begin
      su := copy(s, 1, i - 1);
      val(su,r,e);

      delete(s, 1, i - 1);

      if pos('\green',s) = 1 then
      begin
        delete(s,1,6);
        i := pos('\',s);

        if i > 0 then
        begin
          su := copy(s, 1, i - 1);
          val(su,g,e);

          delete(s, 1, i - 1);

          if pos('\blue',s) = 1 then
          begin
            delete(s,1,5);
            val(s,b,e);
            if e <> 0 then
              b := 0;

          end;
        end;
      end;
    end;
  end;

  {$IFDEF FMXLIB}
  Result := MakeColor(r,g,b);
  {$ENDIF}

  {$IFNDEF FMXLIB}
  Result := RGB(r,g,b);
  {$ENDIF}
end;

procedure TRTFDocument.ParseColors(s: string; ColorTable: TRTFColorTable);
var
  i,j: integer;
  su: string;
  clr: TColor;
  rclr: TRTFColor;
begin
  j := 0;

  while length(s) > 0 do
  begin
    i := pos(';',s);

    if i > 0 then
    begin
      su := copy(s, 1, i - 1);

      if (su <> '') then
      begin
        clr := ParseColor(su);
        rclr := TRTFColor.Create;
        rclr.ID := inttostr(j);
        rclr.Color := clr;
        ColorTable.Add(rclr);
        inc(j);
      end
      else
      begin
        if ColorTable.Count = 0 then
        begin
          rclr := TRTFColor.Create;
          rclr.ID := inttostr(j);
          rclr.Color := clBlack;
          ColorTable.Add(rclr);
          inc(j);
        end;
      end;

      delete(s, 1, i);
    end
    else
      s :='';
  end;
end;

procedure TRTFDocument.ParseElements(s: string; isBul: boolean);
var
  i,j,k,l,e,m: integer;
  re: TRTFElement;
  su,st,se: string;
  sl: TStringList;
  cp: integer;
  sp,hl: boolean;
  nextrep,lc: char;
  numuni,unilen: integer;
  {$IFNDEF FMXLIB}
  sco: ansistring;
  {$ENDIF}
  {$IFDEF FMXLIB}
  b: byte;
  sco: string;
  {$ENDIF}

  procedure InsertTab;
  begin
    re := TRTFElement.Create;
    re.Text := '';
    re.FontName := FAttr.ref;
    re.FontSize := FAttr.refs;
    re.FontStyle := FAttr.res;
    re.Color := FAttr.rec;
    re.BkColor := FAttr.rebc;
    re.Indent := FAttr.reind;
    re.Alignment := FAttr.real;
    re.BaseLine := FAttr.rebl;
    re.Tab := true;
    re.URL := FURL;

    ElementTable.Add(re);
  end;

  procedure InsertText(t: string; CodePage: integer);
  var
    tu: string;
    tlb: integer;
    {$IFDEF FMXLIB}
    cha: TBytes;
    bc: integer;
    {$ENDIF}
  begin
    if t = '' then
      Exit;

    {$IFNDEF FMXLIB}
    if CodePage <> 0 then
    begin
      t := ToUnicodeString(ansistring(t), CodePage);
    end;
    {$ENDIF}

    {$IFDEF FMXLIB}
    if CodePage <> 0 then
    begin
      SetLength(cha,Length(t));

      for bc := 1 to Length(t) do
      begin
        cha[bc - 1] := ord(CharInStr(t,bc));
      end;

      t := ToUnicodeString(cha, CodePage);
    end;
    {$ENDIF}

    tlb := 0;

    while (Length(t) > 1) and (varpos(#13,t,tlb) > 1) do
    begin
      tu := copy(t,1,tlb - 1);
      re := TRTFElement.Create;
      re.Text := tu;
      re.FontName := FAttr.ref;
      re.FontSize := FAttr.refs;
      re.FontStyle := FAttr.res;
      re.Color := FAttr.rec;
      re.BkColor := FAttr.rebc;
      re.Indent := FAttr.reind;
      re.Alignment := FAttr.real;
      re.BaseLine := FAttr.rebl;
      re.URL := FURL;

      if isBul then
        re.BulletType := rbtItem;

      ElementTable.Add(re);

      if pos(#10,t) = tlb + 1 then
        inc(tlb);

      delete(t,1,tlb + 1);
    end;

    re := TRTFElement.Create;
    re.Text := t;
    re.FontName := FAttr.ref;
    re.FontSize := FAttr.refs;
    re.FontStyle := FAttr.res;
    re.Color := FAttr.rec;
    re.BkColor := FAttr.rebc;
    re.Indent := FAttr.reind;
    re.Alignment := FAttr.real;
    re.BaseLine := FAttr.rebl;
    re.URL := FURL;

    if isBul then
    begin
      re.BulletType := rbtItem;
      inc(FBullets);
    end;

    ElementTable.Add(re);
  end;

begin
  if s = #13 then
    Exit;

  sl := TStringList.Create;
  cp := DefCodePage;

  sco := '';
  nextrep := #0;
  numuni := 0;
  unilen := 0;

  s := StringReplace(s,'\~',' ',[rfReplaceAll]);
  s := StringReplace(s,'\{','{',[rfReplaceAll]);
  s := StringReplace(s,'\}','}',[rfReplaceAll]);
  s := StringReplace(s,'\\','\\  ',[rfReplaceAll]);
  s := StringReplace(s,'\'#13,'\par ',[rfReplaceAll]);

  while length(s) > 0 do
  begin
    i := pos('\',s);

    if (i > 0) then
    begin
      st := Copy(s,1, i - 1);
      if (st <> '') and (st <> #13) then
      begin
        if (nextrep <> '\') then
          InsertText(st,cp);
        delete(s, 1, i - 1);
      end;

      // get attributes
      i := pos(' ', s);
      j := pos(#13, s);

      if i = 0 then
        i := j;

      if j = 0 then
        j := i;

      sp := (i < j);

      i := Min(i,j);

      if sp then
      begin
        m := i;
        while m > 0 do
        begin
          if CharInstr(s,m) = '\' then
          begin
            if CharInStr(s,m + 1) <> '''' then
              sp := false;
            break;
          end
          else
            dec(m);
        end;
      end;

      if (i = 0) and (j = 0) then
        i := Length(s) + 1;

      if i > 0 then
      begin
        su := Copy(s, 1, i - 1);
        lc := CharInStr(s, i);

        if pos('\',su) = 1 then
        begin
          // list of attributes
          delete(su,1,1);
          sl.Clear;

          while pos('\',su) > 0 do
          begin
            if (length(su) > 0) and (su[1] = '\') then
            begin
              delete(su,1,1);
              inc(i);
              if pos('\',su) > 0 then
              begin
                se := Copy(su,1,pos('\',su) - 1);
                sl.Add('\' + se);
                delete(su,1,pos('\',su));
              end
              else
              begin
                sl.Add('\'+su);
              end;
            end
            else
            begin
              se := Copy(su,1,pos('\',su) - 1);
              sl.Add(se);
              delete(su,1,pos('\',su));
            end;
          end;

          sl.Add(su);

          hl := false;

          for l := 0 to sl.Count - 1 do
          begin
            su := sl.Strings[l];

            if (SubStr(su,1) = '''') then
            begin
              delete(su,1,1);
              //
              st := copy(su, 1, 2);
              delete(su, 1, 2);

              {$IFDEF FMXLIB}
              b := HexVal(st);

              if nextrep <> '\' then
              begin
                sco := sco + chr(b) + su;
              end
              else
                sco := sco + su + ' ';
              {$ENDIF}

              {$IFNDEF FMXLIB}
              if (nextrep <> '\') then
              begin
                sco := sco + ansichar(chr(HexVal(st)))+ ansistring(su);
                if l = sl.Count - 1 then
                  sp := true;
              end
              else
              begin
                sco := sco + ansistring(su) + ' ';
              end;
              {$ENDIF}

              if numuni = 2 then
                dec(numuni)
              else
              begin
                nextrep := #0;
                numuni := 0;
              end;

              Continue;
            end
            else
            begin
              if sco <> '' then
                InsertText(string(sco),cp);
              sco := '';
            end;

            if (su = 'tab') and not FAttr.rebul then
            begin
              InsertTab;
            end;

            if su = 'plain' then
            begin
              FAttr.res := [];
              FAttr.rebl := bNormal;
            end;

            if su = 'rquote' then
            begin
              InsertText('’',0);
            end;

            if su = 'lquote' then
            begin
              InsertText('‘',0);
            end;

            if su = 'ldblquote' then
            begin
              InsertText('”',0);
            end;

            if su = 'rdblquote' then
            begin
              InsertText('“',0);
            end;

            if su = 'plain' then
            begin
              // default paragraph attributes
              FAttr.real := taLeftJustify;
              FAttr.res := [];
              FAttr.reind := 0;
              FAttr.rec := clBlack;
              FAttr.rebc := clNone;
            end;

            if su = 'pard' then
            begin
              // default paragraph attributes
              FAttr.real := taLeftJustify;
              FAttr.res := [];
              FAttr.reind := 0;

              if FAttr.rebul and FPara and (FAttr.rebuldepth = FParseDepth) then
              begin
                AddBulletEnd;
                FAttr.rebul := false;
                FAttr.reind := 0;
                FPara := false;
              end;
            end;

            if su = 'i' then
              FAttr.res := FAttr.res + [TFontStyle.fsItalic];

            if su = 'i0' then
              FAttr.res := FAttr.res - [TFontStyle.fsItalic];

            if su = 'b' then
              FAttr.res := FAttr.res + [TFontStyle.fsBold];

            if su = 'b0' then
              FAttr.res := FAttr.res - [TFontStyle.fsBold];

            if su = 'ul' then
              FAttr.res := FAttr.res + [TFontStyle.fsUnderline];

            if su = 'ulnone' then
              FAttr.res := FAttr.res - [TFontStyle.fsUnderline];

            if su = 'strike' then
              FAttr.res := FAttr.res + [TFontStyle.fsStrikeOut];

            if su = 'strike0' then
              FAttr.res := FAttr.res - [TFontStyle.fsStrikeOut];

            if su = 'ql' then
              FAttr.real := taLeftJustify;

            if su = 'qj' then // justified not supported
              FAttr.real := taLeftJustify;

            if su = 'qc' then
              FAttr.real := taCenter;

            if su = 'qr' then
              FAttr.real := taRightJustify;

            if su = 'sub' then
              FAttr.rebl := bSubScript;

            if su = 'super' then
              FAttr.rebl := bSuperScript;

            if su = 'nosupersub' then
              FAttr.rebl := bNormal;

            // font color
//            if su = 'cf0' then
//              FAttr.rec := clBlack
//            else
              if SubStr(su,2) = 'cf' then
              begin
                delete(su,1,2);
                val(su, k, e);
                if k < ColorTable.Count then
                  FAttr.rec := ColorTable[k].Color
                else
                  FAttr.rec := clBlack;
              end;

            if  (SubStr(su,1) = 'u') and (su <> 'ul') and IsUnicodeChar(su,unilen) then
            begin
              numuni := FDefUC;
              InsertText(ParseUnicodeChar(Copy(su, 1, unilen), nextrep), 0);
              if Length(su) > unilen then
                InsertText(Copy(su, unilen + 1, Length(su)),0);

              if (lc = ' ') and (Length(su) = unilen) and (l = sl.Count - 1) then
              begin
                dec(i);
                sp := false;
              end
              else
                sp := true;
            end;

            // indent
            if su = 'line' then
            begin
              InsertText(#13,0);
            end
            else
            if (SubStr(su,2) = 'li') then
            begin
              delete(su,1,2);
              val(su, k, e);
              if (e = 0) and not FAttr.rebul then
                FAttr.reind := k;
            end;

            // font size
            if SubStr(su,2) = 'fs' then
            begin
              delete(su,1,2);
              val(su, k, e);
              FAttr.refs := Round((k div 2) * 96/FFontDPI);
              su := '';
            end;

            if SubStr(su,2) = 'f"' then
            begin
              delete(su,1,2);
              FAttr.ref := copy(su, 1, Length(su) - 1);
              su := '';
            end;

            if SubStr(su,1) = 's' then
            begin
              delete(su,1,1);
              val(su, k, e);
              if e = 0 then
              begin
                FAttr.refs := k;
                su := '';
              end;
            end;

            if SubStr(su,9) = 'highlight' then
            begin
              delete(su,1,9);
              val(su, k, e);
              if k = 0 then
                FAttr.rebc := clNone
              else
                if k < ColorTable.Count then
                  FAttr.rebc := ColorTable[k].Color;

              hl := true;
              su := '';
            end;

            if su = 'f0' then
            begin
              if FontTable.Count > 0 then
              begin
                FAttr.ref := FontTable[0].Name;
                cp := FontTable[0].CodePage;
              end
              else
                FAttr.ref := 'Arial';
            end
            else
              if SubStr(su,1) = 'f' then
              begin
                k := FindFont(su);

                if k >= 0 then
                begin
                  FAttr.ref := FontTable[k].Name;
                  cp := FontTable[k].CodePage;
                end
                else
                begin
                  FAttr.ref := 'Arial';
                  cp := DefCodePage;
                end;
              end;

            if (SubStr(su,7) = 'chcbpat') and not hl then
            begin
              delete(su,1,7);
              val(su, k, e);
              if k = 0 then
                FAttr.rebc := clNone
              else
                if k < ColorTable.Count then
                  FAttr.rebc := ColorTable[k].Color;
              su := '';
              sp := false;
            end;

            if su = 'par' then
            begin
              InsertText(#13,0);
              FPara := true;
            end;

            if SubStr(su,1) = '\' then
              InsertText(su,cp);
          end;

          if sco <> '' then
          begin
            InsertText(string(sco),cp);
            sco := '';
          end;
        end;

        if sp then
          Delete(s, 1, i - 1)
        else
          Delete(s, 1, i);
      end
      else
        s := '';
    end
    else
    begin
      InsertText(s,cp);
      s := '';
    end;
  end;

  sl.Free;
end;


function TRTFDocument.ParseFont(attr,s: string; var cp: integer): string;
var
  i: integer;
  cpp,e: integer;
begin
  cpp := pos('FCHARSET',Uppercase(attr));

  if cpp > 0 then
  begin
    delete(attr, 1, cpp + 7);

    attr := Trim(attr);
    if pos('\', attr) > 0 then
      delete(attr, pos('\', attr), Length(attr));
    val(attr, cp, e);
    if e <> 0 then
      cp := 0;
  end;

  cpp := pos('FCHARSET',Uppercase(s));

  if cpp > 0 then
  begin
    delete(s, 1, cpp + 7);

    attr := Trim(s);
    if pos('\', s) > 0 then
      delete(s, pos('\', attr), Length(s));
    val(s, cp, e);
    if e <> 0 then
      cp := 0;

    s := attr;
    if pos(' ',s) > 0 then
      delete(s, 1, pos(' ',s));
  end;

  Result := s;

  i := Pos(';',s);
  if i > 0 then
  begin
    Result := Copy(s, 1, i - 1);
  end;
end;

procedure TRTFDocument.ParseObjects;
var
  i,j,cp,fp,fi,docs: integer;
  docstate: TRTFDocState;
  docfont: TRTFFont;
  attr, fn, tbl, fe, ft, fc, s: string;
  ss,fld,nonshp: boolean;
  //isfoot: boolean;
  FOldAttr, FSubAttr, FPushAttr: TRTFElementAttribute;
  v,e: integer;

  procedure AddFontLib({%H-}AFontName, {%H-}AFontCodePage, {%H-}AFontID: string);
  begin

  end;

begin
  docstate := dsNone;
  cp := 0;
  fp := 0;
  DefCodePage := 0;
  docs := 0;

  for i := 0 to FRTFObjects.Count - 1 do
  begin

    if (docstate = dsFont) and (FRTFObjects[i].RTFType[1] = 'f') then
    begin
      // MS RTF format font table handling
      if (pos('MINOR', Uppercase(FRTFObjects[i].RTFType)) > 0) or (pos('MAJOR', Uppercase(FRTFObjects[i].RTFType)) > 0) then
      begin
        if (pos('\',FRTFObjects[i].RTFAttr) > 0) then
        FRTFObjects[i].RTFType := Copy(FRTFObjects[i].RTFAttr, 1, pos('\',FRTFObjects[i].RTFAttr) - 1);
      end;

      fn := ParseFont(FRTFObjects[i].RTFAttr, FRTFObjects[i].RTFData, cp);

      val(fn,v,e);
      if (e = 0) then // is a default font with just codepage
      begin
        fn := 'Calibri';
        cp := v;
      end;

      if (fn <> '') then
      begin
        j := FindFont(FRTFObjects[i].RTFType);

        if j >= 0 then
        begin
          FontTable.Items[j].Name := fn;
          FontTable.Items[j].CodePage := CharsetToCodePage(cp);
        end
        else
        begin
          docfont := TRTFFont.Create;
          docfont.Name := fn;
          docfont.CodePage := CharSetToCodePage(cp);
          docfont.ID := FRTFObjects[i].RTFType;
          if docfont.ID = 'f0' then
            DefCodePage := CharSetToCodePage(cp);
          FontTable.Add(docfont);
        end;
      end;

      docstate := dsFont;
      docs := i + 1;
    end;

    if Pos('fonttbl',FRTFObjects[i].RTFType) = 1 then
    begin
      if FRTFObjects[i].RTFAttr = '' then
        docstate := dsFont
      else
      begin
        if pos(';',FRTFObjects[i].RTFData) > 0  then
        begin
          tbl := FRTFObjects[i].RTFAttr + ' ' + FRTFObjects[i].RTFData;
          fi := 0;

          while VarPos(';', tbl, fp) > 0 do
          begin
            fe := Copy(tbl, 1, fp - 1);
            Delete(tbl,1, fp + 1);

            if VarPos(' ', fe, fp) > 0 then
            begin
              fn := ParseFont(Copy(fe, 1, fp - 1), Copy(fe, fp + 1, $FFFF),cp);
              docfont := TRTFFont.Create;
              docfont.Name := fn;
              docfont.CodePage := CharSetToCodePage(cp);
              docfont.ID := 'f' + inttostr(fi);
              DefCodePage := CharSetToCodePage(cp);
              FontTable.Add(docfont);
              inc(fi);
            end;
          end;
        end
        else
        begin
          fn := ParseFont(FRTFObjects[i].RTFAttr, FRTFObjects[i].RTFData,cp);
          docfont := TRTFFont.Create;
          docfont.Name := fn;
          docfont.CodePage := CharSetToCodePage(cp);
          docfont.ID := 'f0';
          DefCodePage := CharSetToCodePage(cp);
          FontTable.Add(docfont);
        end;
      end;
    end;

    if (FRTFObjects[i].RTFType = 'colortbl') or (FRTFObjects[i].RTFType = 'colortbl;') then
    begin
      docstate := dsColor;
      if FRTFObjects[i].RTFType = 'colortbl;' then
        FRTFObjects[i].RTFAttr := ';' + FRTFObjects[i].RTFAttr;

      ParseColors(FRTFObjects[i].RTFAttr + FRTFObjects[i].RTFData, ColorTable);

      docs := i;
    end;
  end;

  FAttr.rec := clBlack;
  FAttr.rebc := clNone;
  FAttr.ref := '';
  FAttr.res := [];
  FAttr.refs := -1;
  FAttr.real := taLeftJustify;
  FAttr.rebl := bNormal;
  FAttr.rebul := false;
  ss := false;
  fld := false;
  nonshp := false;
  FURL := '';
  //isfoot := false;

  FSubAttr := FAttr;

  for i := docs to FRTFObjects.Count - 1 do
  begin
    //outputdebugstring(pchar(FRTFObjects[i].RTFType +' - '+ FRTFObjects[i].RTFAttr + ' - ' + FRTFObjects[i].RTFData));

    if (FRTFObjects[i].RTFType = '*') and (FRTFObjects[i].RTFAttr = 'fldinst') then
    begin
      // specific MS WORD RTF extension for symbols
      v := pos('SYMBOL',FRTFObjects[i].RTFData);
      if v > 0  then
      begin
        s := FRTFObjects[i].RTFData;

        delete(s, 1, v + 6);
        v := pos(' ',s);

        if v > 0 then
          s := copy(s, 1, v);

        val(s, v, e);

        s := FRTFObjects[i].RTFData;

        s := ReplaceStr(s, 'SYMBOL '+inttostr(v), '');
        s := ReplaceStr(s, #13, '');
        s := ReplaceStr(s, '\\f ', '\f');
        s := ReplaceStr(s, '\\s ', '\s');
        s := s + ' ' + chr(v);

        FAttr := FSubAttr;
        ParseElements(s);
        FAttr := FOldAttr;
      end
      else
      begin
        // skip
        fld := not fld;
        Continue;
      end;
    end;

    if (FRTFObjects[i].RTFType = 'rtlch') and (varpos('HYPERLINK',  FRTFObjects[i].RTFData, v) > 0) then
    begin
      s := FRTFObjects[i].RTFData;
      delete(s, 1, v + 9);

      if length(s) > 1 then
      begin
        if CharInStr(s,1) = '"' then
        begin
          delete(s, 1, 1);
          if (VarPos('"', s, v) > 0) then
          begin
            s := Copy(s, 1, v - 1);
          end;

          FURL := s;
        end;
      end;
    end;

    if (FRTFObjects[i].RTFType = 'field') and fld then
      fld := false;

    if (FRTFObjects[i].RTFType = 'fldrslt') and fld then
      fld := false;

    if fld then
      Continue;

    if FRTFObjects[i].RTFType = 'rtf1' then
    begin
      ss := false;
      ParseElements(FRTFObjects[i].RTFData);
    end;

    if FRTFObjects[i].RTFType = 'rtf' then
    begin
      ss := false;
      ParseElements(FRTFObjects[i].RTFData);
    end;

    if (pos('cf',FRTFObjects[i].RTFType) = 1) and (pos('rtlch',FRTFObjects[i].RTFAttr) > 0) then
    begin
      ss := false;
      FOldAttr := FAttr;
      ParseElements('\rtlch'+'\'+FRTFObjects[i].RTFType + FRTFObjects[i].RTFAttr + FRTFObjects[i].RTFData);
      FAttr := FOldAttr;
    end;

    if FRTFObjects[i].RTFType = 'rtlch' then
    begin
      ss := false;
      FOldAttr := FAttr;
      ParseElements(FRTFObjects[i].RTFData);
      FSubAttr := FAttr;
      FAttr := FOldAttr;
    end;

    if FRTFObjects[i].RTFType = 'sv' then
    begin
      // skip
      ss := false;
    end;

    if FRTFObjects[i].RTFType = 'ltrch' then
    begin
      ss := false;
      FOldAttr := FAttr;
      ParseElements(FRTFObjects[i].RTFData);
      FSubAttr := FAttr;
      FAttr := FOldAttr;
    end;

    if FRTFObjects[i].RTFType = 'pard' then
    begin
      ss := false;
      FOldAttr := FAttr;
      ParseElements('\pard' + FRTFObjects[i].RTFData);
      FAttr := FOldAttr;
    end;

    if (FRTFObjects[i].RTFType = 'footerl') or (FRTFObjects[i].RTFType = 'footerr') or (FRTFObjects[i].RTFType = 'footerf') then
    begin
      //isfoot := true;

      // skip
      //ss := false;
      ParseElements(FRTFObjects[i].RTFData);

      FRTFObjects[i].RTFData := '';
      FRTFObjects[i].RTFAttr := '';

      //isfoot := false;
      Continue;
    end;

    if not ss then
    begin
      ft := copy(FRTFObjects[i].RTFType, 1, 1);
      fc := copy(FRTFObjects[i].RTFType, 2, 1);

      if FRTFObjects[i].RTFType = 'b' then
      begin
        FOldAttr := FAttr;
        ParseElements('\b '+FRTFObjects[i].RTFData);
        FAttr := FOldAttr;
      end;

      if FRTFObjects[i].RTFType = 'i' then
      begin
        FOldAttr := FAttr;
        ParseElements('\i '+FRTFObjects[i].RTFData);
        FAttr := FOldAttr;
      end;

      if FRTFObjects[i].RTFType = 'u' then
      begin
        FOldAttr := FAttr;
        ParseElements('\u '+FRTFObjects[i].RTFData);
        FAttr := FOldAttr;
      end;

      if (ft = 'f') and (fc >= '0') and (fc <= '9') and (FRTFObjects[i].RTFData <> '') then
      begin
        FOldAttr := FAttr;
        ParseElements('\' +FRTFObjects[i].RTFType+ ' ' + FRTFObjects[i].RTFData);
        FAttr := FOldAttr;
      end;

      if FRTFObjects[i].RTFType = 'ql' then
      begin
        FOldAttr := FAttr;
        ParseElements('\ql '+FRTFObjects[i].RTFData);
        FAttr := FOldAttr;
      end;

      if FRTFObjects[i].RTFType = 'qr' then
      begin
        FOldAttr := FAttr;
        ParseElements('\qr '+FRTFObjects[i].RTFData);
        FAttr := FOldAttr;
      end;

      if FRTFObjects[i].RTFType = 'qc' then
      begin
        FOldAttr := FAttr;
        ParseElements('\qc '+FRTFObjects[i].RTFData);
        FAttr := FOldAttr;
      end;

      if FRTFObjects[i].RTFType = 'qj' then
      begin
        FOldAttr := FAttr;
        ParseElements('\qj\'+FRTFObjects[i].RTFAttr + ' ' + FRTFObjects[i].RTFData);
        FAttr := FOldAttr;
      end;

    end;

    if FRTFObjects[i].RTFType = 'stylesheet' then
    begin
      ss := true;
    end;

    if FRTFObjects[i].RTFType = 'fldrslt' then
    begin
      ss := false;

      if FRTFObjects[i].RTFData <> '' then
      begin
        FOldAttr := FAttr;
        ParseElements('\ '+FRTFObjects[i].RTFData);
        FAttr := FOldAttr;
      end;

      if i + 1 < FRTFObjects.Count then
      begin
        if (FRTFObjects[i + 1].RTFType <> 'pict') and (FRTFObjects[i + 1].RTFType <> 'rtf1') then
        begin
          FPushAttr := FAttr;
          ParseElements('\' + FRTFObjects[i + 1].RTFType+'\'+FRTFObjects[i + 1].RTFAttr + ' ' + FRTFObjects[i + 1].RTFData);
          FRTFObjects[i + 1].RTFType := '';
          FAttr := FPushAttr;
        end;
      end;
    end;

    if FRTFObjects[i].RTFType = 'pntext' then
    begin
      ss := false;
      FOldAttr := FAttr;
      FPara := false;

      if not FAttr.rebul then
      begin
        FAttr.rebul := true;
        FAttr.rebuldepth := FParseDepth;
        AddBulletStart;
      end;

      attr := FRTFObjects[i].RTFAttr;
      if attr <> '' then
        attr := '\' + attr;

      ParseElements(attr + ' ' + FRTFObjects[i].RTFData, FOldAttr.rebul);

      FAttr := FOldAttr;
    end;

    if FRTFObjects[i].RTFType = 'listtext' then
    begin
      ss := false;
      FOldAttr := FAttr;
      FPara := false;

      if not FAttr.rebul then
      begin
        FAttr.rebul := true;
        FAttr.rebuldepth := FParseDepth;
        AddBulletStart;
      end;

      attr := FRTFObjects[i].RTFAttr;
      if attr <> '' then
        attr := '\' + attr;

      ParseElements(attr + ' ' +FRTFObjects[i].RTFData, FOldAttr.rebul);

      FAttr := FOldAttr;
    end;

    if FRTFObjects[i].RTFType = 'nonshppict' then
    begin
      nonshp := true;
    end;

    if (FRTFObjects[i].RTFType = 'pict') then
    begin
      ss := false;
      if (FRTFObjects[i].RTFData <> '') and not nonshp then
        ParsePicture(FRTFObjects[i].RTFAttr, FRTFObjects[i].RTFData)
      else
        if (FRTFObjects[i].RTFData <> '') and nonshp then
          nonshp := false;
    end;

    FURL := '';
  end;
end;

{$IFNDEF FMXLIB}
procedure TRTFDocument.ParsePicture(Attr, Data: string);
var
  r,su: string;
  i: integer;
  b: byte;
  ms: TMemoryStream;
  st: TStringStream;
  strlist: TStringList;
  mfh: TMetaFileHeader;
  mfr: TMetaFileRecord;
  mfs: TMetaFileStretchBlt;
  fnd: boolean;
  w: word;
  bmi: TTMSBitmapInfoHeader;
  {$IFDEF MSWINDOWS}
  bits: pointer;
  {$ENDIF}
  bitsinfo: pTMSBITMAPINFO;
  rgbquads4bit: array[0..255] of TTMSRGBQuad;
  elem: TRTFElement;
  bitssize,numcolors,colorsize: integer;
  ismeta: boolean;
  isjpeg: boolean;
  ispng: boolean;
  jpg: TJPEGImage;
  png: TPNGImage;
  scalex,scaley: integer;
  goalx,goaly: integer;
  rl: int64;
  {$IFNDEF MSWINDOWS}
  bar: array of byte;
  bari: byte;
  barb: integer;
  barp: integer;
  bmpx,bmpy: integer;
  bmp: TBitmap;
  c: TColor;
  {$ENDIF}

  function LookupColor(index: byte): TColor;
  var
    quad: TTMSRGBQuad;
  begin
    quad := rgbquads4bit[index];
    Result := RGB(quad.rgbRed, quad.rgbGreen, quad.rgbBlue);
  end;

begin
  if (Data = '') or (Attr = '') then
    Exit;

  ismeta := false;
  isjpeg := false;
  ispng := false;
  mfs.ROP := 0;
  bmi.biSize := sizeof(bmi);
  rgbquads4bit[0].rgbBlue := 0;

  // JPEGBLIP only without metafile
  if (pos('JPEGBLIP',Uppercase(Attr)) > 0) and (pos('WMETAFILE',Uppercase(Attr)) = 0) then
    isjpeg := true;

  if (pos('PNGBLIP',Uppercase(Attr)) > 0) and (pos('WMETAFILE',Uppercase(Attr)) = 0) then
    ispng := true;

  if pos('WMETAFILE',Uppercase(Attr)) > 0 then
    ismeta := true;

  if not ismeta and not isjpeg and not ispng then
    Exit;

  scalex := 100;
  scaley := 100;

  goalx := -1;
  goaly := -1;
  w := 0;

  su := Attr;
  i := pos('PICSCALEX',Uppercase(su));
  if i > 0 then
  begin
    delete(su,1,i + 8);
    i := pos('\',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    i := pos(' ',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    val(su,scalex,i);
  end;

  su := Attr;
  i := pos('PICSCALEY',Uppercase(su));
  if i > 0 then
  begin
    delete(su,1,i + 8);
    i := pos('\',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    i := pos(' ',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    val(su,scaley,i);
  end;

  i := pos('PICWGOAL',Uppercase(su));
  if i > 0 then
  begin
    delete(su,1,i + 7);
    i := pos('\',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    i := pos(' ',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    val(su,goalx,i);
  end;

  su := Attr;
  i := pos('PICHGOAL',Uppercase(su));
  if i > 0 then
  begin
    delete(su,1,i + 7);
    i := pos('\',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    i := pos(' ',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    val(su,goaly,i);
  end;


  strlist := TStringList.Create;
  //s := ReplaceStr(Data, #13, '');

  st := TStringStream.Create('');
  //st.WriteString(s);

  try
    strlist.Text := Data;
    for i := 0 to strlist.Count - 1 do
      st.WriteString(strlist[i]);
  finally
    strlist.Free;
  end;

  st.Position := 0;

  ms := TMemoryStream.Create;

  // convert hex coded string to byte stream

  for i := 1 to length(st.DataString) div 2 do
  begin
    r := st.ReadString(2);
    b := HexVal(r);
    ms.Write(b,1);
  end;

  ms.Position := 0;

  if isjpeg then
  begin
    jpg := TJPEGImage.Create;

    try
      // parse as JPEG file
      jpg.LoadFromStream(ms);

      if (jpg.Width > 0) and (jpg.Height > 0) then
      begin
        elem := TRTFElement.Create;
        elem.Bitmap := TBitmap.Create;

        if goalx <> -1 then
        begin
          elem.Bitmap.Width := TRTFEngine.TwipsToPixels(goalx);
          scalex := 1;
        end
        else
          elem.Bitmap.Width := round(jpg.Width * scalex/100);

        if goalx <> -1 then
        begin
          elem.Bitmap.Height := TRTFEngine.TwipsToPixels(goaly);
          scaley := 1;
        end
        else
          elem.Bitmap.Height := round(jpg.Height * scaley/100);

        if (scalex = 100) and (scaley = 100) then
          elem.Bitmap.Canvas.Draw(0,0,jpg)
        else
          elem.Bitmap.Canvas.StretchDraw(Rect(0,0,elem.Bitmap.Width, elem.Bitmap.Height), jpg);
        elem.Bitmap.Transparent := false;
        ElementTable.Add(elem);
      end;

    finally
      jpg.Free;
    end;
  end
  else
  if ispng then
  begin
    png := TPNGImage.Create;

    try
      // parse as PNG file
      png.LoadFromStream(ms);

      if (png.Width > 0) and (png.Height > 0) then
      begin
        elem := TRTFElement.Create;
        elem.Bitmap := TBitmap.Create;

        if goalx <> -1 then
        begin
          elem.Bitmap.Width := TRTFEngine.TwipsToPixels(goalx);
          scalex := 1;
        end
        else
          elem.Bitmap.Width := round(png.Width * scalex/100);

        if goalx <> -1 then
        begin
          elem.Bitmap.Height := TRTFEngine.TwipsToPixels(goaly);
          scaley := 1;
        end
        else
          elem.Bitmap.Height := round(png.Height * scaley/100);

        if (scalex = 100) and (scaley = 100) then
          elem.Bitmap.Canvas.Draw(0,0,png)
        else
          elem.Bitmap.Canvas.StretchDraw(Rect(0,0,elem.Bitmap.Width, elem.Bitmap.Height), png);
        elem.Bitmap.Transparent := false;

        ElementTable.Add(elem);
      end;

    finally
      png.Free;
    end;
  end
  else
  begin
    // metafile header parsing
    // http://wvware.sourceforge.net/caolan/ora-wmf.html

    ms.Read(w,2);

    mfh.metatype := w;

    ms.Read(w,2);

    mfh.metatype := w;

    ms.Read(w,2);

    mfh.versionnr := w;

    ms.Read(w,2);

    mfh.filesize := w;

    ms.Read(w,2);

    mfh.filesize := (w shl 16) + mfh.filesize;

    ms.Read(w,2);

    mfh.objects := w;

    ms.Read(w,2);

    mfh.maxsize := w;

    ms.Read(w,2);

    mfh.maxsize := (w shl 16) + mfh.maxsize;

    ms.Read(w,2);

    mfh.params := w;

    //

    fnd := false;

    repeat

      ms.Read(w,2);

      mfr.recordlen := w;

      ms.Read(w,2);

      mfr.recordlen := (w shl 16) + mfr.recordlen;

      ms.Read(w,2);

      mfr.func := w;

      if (mfr.func = $B41) or (mfr.func = $F43) then
        fnd := true
      else
      begin
        rl := mfr.recordlen;
        ms.Position := ms.Position + (rl * 2 - 6);
      end;

    until (fnd) or (ms.Position >= ms.Size);

    if fnd then
    begin
      // StretchDIBits record is different from DibStretchBlt header
      if mfr.func = $F43 then
        ms.Read(i,2);

      ms.Read(mfs,sizeof(mfs));

      ms.Read(bmi, sizeof(TBitmapInfoHeader));

      numcolors := 0;

      case bmi.biBitCount of
      1:
        begin
          numcolors := 2;
        end;
      4:
        begin
          if (bmi.biClrUsed = 0) then
            numcolors := 16
          else
            numcolors := bmi.biClrUsed;
        end;
      8:
        begin
          if (bmi.biClrUsed = 0) then
            numcolors := 256
          else
            numcolors := bmi.biClrUsed;
        end;
      16:
        begin

        end;
      24, 32:
        begin
          numcolors := bmi.biClrUsed;
        end;
      end;

      colorsize := numcolors * sizeof(TRGBQuad);

      GetMem(bitsinfo, sizeof(TBitmapInfoHeader) + colorsize);

      ms.Read(rgbquads4bit, colorsize);

      Move(rgbquads4bit, bitsinfo^.bmiColors, colorsize);

      elem := TRTFElement.Create;
      elem.Bitmap := TBitmap.Create;
      elem.Bitmap.Width := bmi.biWidth;
      elem.Bitmap.Height := bmi.biHeight;
      elem.Bitmap.Canvas.Brush.Color := clFuchsia;
      elem.Bitmap.Canvas.FillRect(Rect(0,0, bmi.biWidth - 1, bmi.biHeight - 1));
      elem.Bitmap.Transparent := false;

      {$IFDEF MSWINDOWS}

      rl := mfr.recordlen;

      bitssize := 2 * rl - sizeof(mfs) - sizeof(bmi);

      GetMem(bits, bitssize);

      ms.Read(bits^, bitssize);

      bitsinfo^.bmiHeader := bmi;

      SetBkColor(elem.Bitmap.Canvas.Handle, RGB(255,0,0));

      StretchDIBits(elem.Bitmap.Canvas.Handle, 0, 0, elem.Bitmap.Width, elem.Bitmap.Height, 0, 0, elem.Bitmap.Width, elem.Bitmap.Height, bits, Windows.TBitmapInfo(bitsinfo^), DIB_RGB_COLORS, SRCCOPY);

      FreeMem(bits);
      {$ENDIF}

      {$IFNDEF MSWINDOWS}
      bmp := elem.Bitmap;

      colorsize := numcolors * sizeof(TTMSRGBQuad);

      ms.Read(rgbquads4bit, colorsize);

      bitssize := 2 * mfr.recordlen - sizeof(mfs) - sizeof(bmi);

      SetLength(bar, bitssize);

      ms.Read(bar[0], bitssize);

      barb := 0;


      barp := 0;

      for bmpy := bmi.biHeight - 1 downto 0 do
      begin
        case bmi.biBitCount of
        1: barb := 7;
        4,8: barb := 0;
        end;

        for bmpx := 0 to bmi.biWidth - 1 do
        begin
          c := clNone;
          case bmi.biBitCount of
          1:
            begin
              bari := (bar[barp] and (1 shl barb)) shr barb;
              if barb > 0 then
                dec(barb)
              else
              begin
                barb := 7;
                inc(barp);
              end;
              c := LookupColor(bari);
            end;
          4:
            begin
              if barb = 0 then
              begin
                bari := (bar[barp] and $F0) shr 4;
                barb := 1;
              end
              else
              begin
                bari := (bar[barp] and $0F);
                barb := 0;
                inc(barp);
              end;
              c := LookupColor(bari);
            end;
          8:
            begin
              bari := bar[barp];
              inc(barp);
              c := LookupColor(bari);
            end;
          24:
            begin
              c := RGB(bar[barp + 2], bar[barp + 1], bar[barp]);
              barp := barp + 3;
            end;
          32:
            begin
              c := RGB(bar[barp + 2], bar[barp + 1], bar[barp + 0]);
              barp := barp + 4;
            end;
          end;

          bmp.Canvas.Pixels[bmpx,bmpy] := c;
        end;

        if (barp mod 4 <> 0) then
          barp := ((barp div 4) + 1)  * 4;
      end;
      {$ENDIF}

      ElementTable.Add(elem);
      FreeMem(bitsinfo);
    end;
  end;

  ms.Free;
  st.Free;
end;
{$ENDIF}

{$IFDEF FMXLIB}
procedure TRTFDocument.ParsePicture(Attr, Data: string);
var
  r, su: string;
  i: integer;
  b: byte;
  ms: TMemoryStream;
  st: TStringStream;
  strlist: TStringList;
  mfh: TMetaFileHeader;
  mfr: TMetaFileRecord;
  mfs: TMetaFileStretchBlt;
  fnd: boolean;
  w: word;
  bmi: TTMSBitmapInfoHeader;
  bar: TBytes;
  rgbquads4bit: array[0..255] of TTMSRGBQuad;
  elem: TRTFElement;
  bitssize, numcolors, colorsize: integer;
  bmp,jpg,png: TBitmap;
  bmpdt: TBitmapData;
  bari: byte;
  barb: integer;
  barp: integer;
  bmpx,bmpy: integer;
  c: TAlphaColor;
  ismeta: boolean;
  isjpeg: boolean;
  ispng: boolean;
  scalex,scaley: integer;
  goalx,goaly: integer;

  function LookupColor(index: byte): TAlphaColor;
  var
    quad: TTMSRGBQuad;
  begin
    quad := rgbquads4bit[index];
    Result := MakeColor(quad.rgbRed, quad.rgbGreen, quad.rgbBlue, 255);
  end;

begin
  if (Data = '') or (Attr = '') then
    Exit;

  isjpeg := false;
  ispng := false;
  ismeta := false;

  // JPEGBLIP only without metafile
  if (pos('JPEGBLIP',Uppercase(Attr)) > 0) and (pos('WMETAFILE',Uppercase(Attr)) = 0) then
    isjpeg := true;

  if (pos('PNGBLIP',Uppercase(Attr)) > 0) and (pos('WMETAFILE',Uppercase(Attr)) = 0) then
    ispng := true;

  if pos('WMETAFILE',Uppercase(Attr)) > 0 then
    ismeta := true;

  if not ismeta and not isjpeg and not ispng then
    Exit;

  scalex := 100;
  scaley := 100;
  goalx := -1;
  goaly := -1;

  su := Attr;
  i := pos('PICSCALEX',Uppercase(su));
  if i > 0 then
  begin
    delete(su,1,i + 8);
    i := pos('\',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    i := pos(' ',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    val(su,scalex,i);
  end;


  su := Attr;
  i := pos('PICSCALEY',Uppercase(su));
  if i > 0 then
  begin
    delete(su,1,i + 8);
    i := pos('\',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    i := pos(' ',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    val(su,scaley,i);
  end;

  i := pos('PICWGOAL',Uppercase(su));
  if i > 0 then
  begin
    delete(su,1,i + 7);
    i := pos('\',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    i := pos(' ',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    val(su,goalx,i);
  end;

  su := Attr;
  i := pos('PICHGOAL',Uppercase(su));
  if i > 0 then
  begin
    delete(su,1,i + 7);
    i := pos('\',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    i := pos(' ',su);
    if i > 0 then
      su := copy(su,1,i - 1);

    val(su,goaly,i);
  end;


  strlist := TStringList.Create;
  st := TStringStream.Create;

  try
    strlist.Text := Data;
    for i := 0 to strlist.Count - 1 do
      st.WriteString(strlist[i]);
  finally
    strlist.Free;
  end;

  st.Position := 0;

  ms := TMemoryStream.Create;

  for i := 1 to length(st.DataString) div 2 do
  begin
    r := st.ReadString(2);
    b := HexVal(r);
    ms.Write(b,1);
  end;

  ms.Position := 0;

  if isjpeg then
  begin
    jpg := TBitmap.Create;

    try
      // parse as JPEG file
      jpg.LoadFromStream(ms);

      if (jpg.Width > 0) and (jpg.Height > 0) then
      begin
        elem := TRTFElement.Create;
        elem.Bitmap := TBitmap.Create;

        if goalx <> -1 then
        begin
          elem.Bitmap.Width := TRTFEngine.TwipsToPixels(goalx);
        end
        else
          elem.Bitmap.Width := round(jpg.Width * scalex/100);

        if goalx <> -1 then
        begin
          elem.Bitmap.Height := TRTFEngine.TwipsToPixels(goaly);
        end
        else
          elem.Bitmap.Height := round(jpg.Height * scaley/100);

        elem.Bitmap.Canvas.BeginScene();
        elem.Bitmap.Canvas.DrawBitmap(jpg,RectF(0,0,jpg.Width, jpg.Height),RectF(0,0,elem.Bitmap.Width,elem.Bitmap.Height),255);
        elem.Bitmap.Canvas.EndScene;
        ElementTable.Add(elem);
      end;
    finally
      jpg.Free;
    end;
  end
  else
  if ispng then
  begin
    png := TBitmap.Create;

    try
      // parse as JPEG file
      png.LoadFromStream(ms);

      if (png.Width > 0) and (png.Height > 0) then
      begin
        elem := TRTFElement.Create;
        elem.Bitmap := TBitmap.Create;

        if goalx <> -1 then
        begin
          elem.Bitmap.Width := TRTFEngine.TwipsToPixels(goalx);
        end
        else
          elem.Bitmap.Width := round(png.Width * scalex/100);

        if goalx <> -1 then
        begin
          elem.Bitmap.Height := TRTFEngine.TwipsToPixels(goaly);
        end
        else
          elem.Bitmap.Height := round(png.Height * scaley/100);

        elem.Bitmap.Canvas.BeginScene();
        elem.Bitmap.Canvas.DrawBitmap(png,RectF(0,0,png.Width, png.Height),RectF(0,0,elem.Bitmap.Width,elem.Bitmap.Height),255);
        elem.Bitmap.Canvas.EndScene;
        ElementTable.Add(elem);
      end;
    finally
      png.Free;
    end;
  end
  else
  begin
    ms.Read(w,2);

    mfh.metatype := w;

    ms.Read(w,2);

    mfh.metatype := w;

    ms.Read(w,2);

    mfh.versionnr := w;

    ms.Read(w,2);

    mfh.filesize := w;

    ms.Read(w,2);

    mfh.filesize := (w shl 16) + mfh.filesize;

    ms.Read(w,2);

    mfh.objects := w;

    ms.Read(w,2);

    mfh.maxsize := w;

    ms.Read(w,2);

    mfh.maxsize := (w shl 16) + mfh.maxsize;

    ms.Read(w,2);

    mfh.params := w;

    fnd := false;

    repeat

      ms.Read(w,2);

      mfr.recordlen := w;

      ms.Read(w,2);

      mfr.recordlen := (w shl 16) + mfr.recordlen;

      ms.Read(w,2);

      mfr.func := w;

      if (mfr.func = $B41) or (mfr.func = $F43) then
        fnd := true
      else
        ms.Position := ms.Position + (mfr.recordlen * 2 - 6);

    until (fnd) or (ms.Position >= ms.Size);

    if fnd then
    begin
      if mfr.func = $F43 then
        ms.Read(i,2);

      ms.Read(mfs, sizeof(mfs));
      ms.Read(bmi, sizeof(TTMSBitmapInfoHeader));

      numcolors := 0;

      case bmi.biBitCount of
      1: numcolors := 2;
      4:
      begin
        if (bmi.biClrUsed = 0) then
          numcolors := 16
        else
          numcolors := bmi.biClrUsed;
      end;
      8:
      begin
        if (bmi.biClrUsed = 0) then
          numcolors := 256
        else
          numcolors := bmi.biClrUsed;
      end;
      16:;
      24, 32: numcolors := bmi.biClrUsed;
      end;

      colorsize := numcolors * sizeof(TTMSRGBQuad);

      ms.Read(rgbquads4bit, colorsize);

      bitssize := 2 * mfr.recordlen - sizeof(mfs) - sizeof(bmi);

      SetLength(bar, bitssize);

      ms.Read(bar, bitssize);

      bmp := TBitmap.Create;
      bmp.Width := bmi.biWidth;
      bmp.Height := bmi.biHeight;

      barb := 0;
      if bmp.Map(TMapAccess.Write, bmpdt) then
      begin
        barp := 0;

        for bmpy := bmi.biHeight - 1 downto 0 do
        begin
          case bmi.biBitCount of
          1: barb := 7;
          4,8: barb := 0;
          end;

          for bmpx := 0 to bmi.biWidth - 1 do
          begin
            c := claNull;
            case bmi.biBitCount of
            1:
              begin
                bari := (bar[barp] and (1 shl barb)) shr barb;
                if barb > 0 then
                  dec(barb)
                else
                begin
                  barb := 7;
                  inc(barp);
                end;
                c := LookupColor(bari);
              end;
            4:
              begin
                if barb = 0 then
                begin
                  bari := (bar[barp] and $F0) shr 4;
                  barb := 1;
                end
                else
                begin
                  bari := (bar[barp] and $0F);
                  barb := 0;
                  inc(barp);
                end;
                c := LookupColor(bari);
              end;
            8:
              begin
                bari := bar[barp];
                inc(barp);
                c := LookupColor(bari);
              end;
            24:
              begin
                c := MakeColor(bar[barp + 2], bar[barp + 1], bar[barp], 255);
                barp := barp + 3;
              end;
            32:
              begin
                c := MakeColor(bar[barp + 2], bar[barp + 1], bar[barp + 0], bar[barp + 3]);
                barp := barp + 4;
              end;
            end;

            bmpdt.SetPixel(bmpx,bmpy,c);
          end;

          if (barp mod 4 <> 0) then
            barp := ((barp div 4) + 1)  * 4;
        end;

        bmp.Unmap(bmpdt);
      end;

      elem := TRTFElement.Create;
      elem.Bitmap := TBitmap.Create;
      elem.Bitmap.Assign(bmp);
      ElementTable.Add(elem);
      bmp.Free;
    end;
  end;

  ms.Free;
  st.Free;
end;
{$ENDIF}


function TRTFDocument.ParseRTF(ts: TStringStream): TRTFParseState;
var
  ch,chn,chp: char;
  ro,rp: TRTFObject;
  state,oldstate: TRTFParseState;
  chw,sz: string;
  ret: integer;
begin
  state := psNone;
  oldstate := psNone;

  Result := state;
  ro := nil;

  if ts.Position >= ts.Size then
  begin
    sz := inttostr(ts.Position)+';'+inttostr(ts.Size);
    if sz = '0' then
      Result := psNone;
    Exit;
  end;

  ch := #0;

  while ts.Position < ts.Size do
  begin
    chp := ch;
    ts.Read(ch,1);

    if ch = #10 then
      Continue;

    if (state <> psData) and (ch = #13) then
    begin
      if (state = psAttr) then
      begin
        ts.Read(ch,1);
        if ch = #10 then
          ts.Read(ch,1);

        if ch <> '\' then
        begin
          ts.Position := ts.Position - 1;
          ch := ' '
        end
        else
          Continue;
      end
      else
        Continue;
    end;

    if ((state = psAttr) or (state = psType) or (state = psData) or (state = psObject)) and (ch = '}') and (chp <> '\') then
    begin
      dec(FParseDepth);
      Result := psNone;
      Exit;
    end
    else
    if (((state = psAttr) or (state = psType) or (state = psData)) and (ch = '{') and (chp <> '\')) or ((state = psObject) and (ch = '{') and (chp = '{')) then
    begin
      ret := 0;
      chn := #0;
      ts.Read(chn,1);

      while (chn = #13) or (chn = #10) do
      begin
        ts.Read(chn,1);
        inc(ret);
      end;

      ts.Position := ts.Position - 1;

      if (chn <> '\') and (chn <> '{') then
        state := psField
      else
      begin
        if (chn = '\') then
          ts.Position := ts.Position - 1 - ret;

        if (chn = '{') then
        begin
          ts.Position := ts.Position - 1;
        end;

        rp := ro;

        inc(FParseDepth);

        ParseRTF(ts);

        if (state in [psAttr, psData]) then
        begin
          // pick up old object type and continue?
          ro := TRTFObject.Create;
          ro.RTFType := rp.RTFType;
          ro.RTFAttr := rp.RTFAttr;
          FRTFObjects.Add(ro);

          if state = psType then
            state := psAttr
          else
          if state = psAttr then
            state := psData
          else
            state := psData;
        end;
  //      else
  //        state := psData;
      end;
    end
    else
    if (state = psNone) and (ch = '{') and not (chp = '\') then
    begin
      ro := TRTFObject.Create;
      FRTFObjects.Add(ro);
      state := psObject;
    end
    else
    if (state = psField) and (ch = '}') and not (chp = '\') then
    begin
      state := psData;
    end
    else
    if (state = psComment) and (ch = '}')  and not (chp = '\') then
    begin
      state := oldstate;
    end
    else
    if (state = psObject) then
    begin
      if (ch = '\') then
        state := psType
      else
      begin
        state := psAttr;
        ro.RTFAttr := ro.RTFAttr + ch;
      end;
    end
    else
    if (state = psType) and (ch = '\') then
    begin
      state := psAttr;
    end
    else
    if (state = psType) and (ch = ' ') then
    begin
      state := psData;
    end
    else
    if (state = psAttr) and (ch = ' ') then
    begin
      state := psData;
    end
    else
    begin
      chw := ch;

      // special range of Win1525 ansi chars
      if (ch >= chr($80)) and (ch <= chr($9F)) then
        chw := MapAnsiToUTF8(ch);

      if state = psType then
        ro.RTFType := ro.RTFType + chw;

      if state = psAttr then
        ro.RTFAttr := ro.RTFAttr + chw;

      if state = psData then
        ro.FDataStr.WriteString(chw);

      if state = psField then
        ro.FDataStr.WriteString(chw);
    end;
  end;
end;

{ TRTFElement }

constructor TRTFElement.Create;
begin
  inherited;
  FBitmap := nil;
  FBulletType := rbtNone;
  FText := '';
  FBkColor := clNone;
  FColor := clNone;
  FAlignment := taLeftJustify;
  FURL := '';
end;

destructor TRTFElement.Destroy;
begin
  inherited;
end;

{ TRTFObject }

constructor TRTFObject.Create;
begin
  inherited;
  FDataStr := TStringStream.Create('');
end;

destructor TRTFObject.Destroy;
begin
  FDataStr.Free;
  inherited;
end;

function TRTFObject.GetRTFData: string;
begin
  Result := FDataStr.DataString;
end;

end.
