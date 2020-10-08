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

unit AdvTypes;

{$I TMSDEFS.INC}

{$HINTS OFF}
{$IFDEF LCLLIB}
{$DEFINE USETRECTF}
{$ENDIF}
{$IFDEF VCLLIB}
{$IF COMPILERVERSION < 23}
{$DEFINE USETRECTF}
{$IFEND}
{$ENDIF}
{$HINTS ON}

{$IFDEF WEBLIB}
{$DEFINE USETRECTF}
{$ENDIF}

{$IFDEF FMXLIB}
{$DEFINE FMXWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE FMXWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF VCLLIB}
  Windows,
  {$ENDIF}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  UITypes,
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  LCLType, fpjson,
  {$ENDIF}
  Types, Classes, Graphics
  ,SysUtils, Controls, Math
  ;

type
  IAdvProductInfo = interface
  ['{C53329EA-7B3A-4507-AD9E-88ACD6A85054}']
    function GetVersion: string;
    function GetDocURL: string;
    function GetTipsURL: string;
  end;

const
  TAdvBaseDocURL = 'http://www.tmssoftware.biz/Download/Manuals/TMS%20Component%20Pack%20Quick%20Start.pdf';
  TAdvBaseTipsURL = 'http://www.tmssoftware.com/site/tmspack.asp?s=faq';
  {$HINTS OFF}
  {$IFNDEF LCLLIB}
  {$IF COMPILERVERSION > 28}
  pidWeb = $10000;
  {$ELSE}
  pidWeb = $1000;
  {$IFEND}
  {$ENDIF}
  {$HINTS ON}
  {$IFDEF FMXLIB}
  TMSPlatformsDesktop = pidWin32 or pidWin64 or pidOSX32;
  {$HINTS OFF}
  TMSPlatforms = pidWin32 or pidWin64 or pidOSX32 or {$IF COMPILERVERSION > 32}pidiOSSimulator32 or pidiOSSimulator64{$ELSE}pidiOSSimulator{$IFEND} or {$IF COMPILERVERSION > 28}pidiOSDevice32 or pidiOSDevice64{$ELSE}pidiOSDevice{$IFEND} or {$IF COMPILERVERSION > 32}pidAndroid32Arm or pidAndroid64Arm{$ELSE}pidAndroid{$IFEND};
  {$HINTS ON}
  TMSPlatformsWeb = TMSPlatforms or pidWeb;
  TMSPlatformsWebDesktop = TMSPlatformsDesktop or pidWeb;
  KEY_CANCEL = VKCANCEL;
  KEY_CONTROL = VKCONTROL;
  KEY_SHIFT = VKSHIFT;
  KEY_ESCAPE = VKESCAPE;
  KEY_INSERT = VKINSERT;
  KEY_DELETE = VKDELETE;
  KEY_TAB = VKTAB;
  KEY_PRIOR = VKPRIOR;
  KEY_NEXT = VKNEXT;
  KEY_UP = VKUP;
  KEY_DOWN = VKDOWN;
  KEY_RIGHT = VKRIGHT;
  KEY_LEFT = VKLEFT;
  KEY_HOME = VKHOME;
  KEY_END = VKEND;
  KEY_RETURN = VKRETURN;
  KEY_SPACE = VKSPACE;
  KEY_MENU = VKMENU;
  KEY_BACK = VKBACK;
  KEY_F1 = VKF1;
  KEY_F2 = VKF2;
  KEY_F3 = VKF3;
  KEY_F4 = VKF4;
  KEY_F5 = VKF5;
  KEY_F6 = VKF6;
  KEY_F7 = VKF7;
  KEY_F8 = VKF8;
  KEY_F9 = VKF9;
  KEY_F10 = VKF10;
  KEY_F11 = VKF11;
  KEY_F12 = VKF12;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  TMSPlatformsDesktop = pidWin32 or pidWin64;
  TMSPlatforms = pidWin32 or pidWin64;
  TMSPlatformsWeb = TMSPlatforms or pidWeb;
  TMSPlatformsWebDesktop = TMSPlatformsDesktop or pidWeb;
  {$ELSE}
  ssCommand = ssCtrl;
  TMSPlatformsDesktop = 0;
  TMSPlatforms = 0;
  TMSPlatformsWeb = 0;
  TMSPlatformsWebDesktop = 0;
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ssCommand = ssCtrl;
  {$ENDIF}
  KEY_CANCEL = VK_CANCEL;
  KEY_CONTROL = VK_CONTROL;
  KEY_SHIFT = VK_SHIFT;
  KEY_ESCAPE = VK_ESCAPE;
  KEY_INSERT = VK_INSERT;
  KEY_DELETE = VK_DELETE;
  KEY_TAB = VK_TAB;
  KEY_PRIOR = VK_PRIOR;
  KEY_NEXT = VK_NEXT;
  KEY_UP = VK_UP;
  KEY_DOWN = VK_DOWN;
  KEY_RIGHT = VK_RIGHT;
  KEY_LEFT = VK_LEFT;
  KEY_HOME = VK_HOME;
  KEY_END = VK_END;
  KEY_RETURN = VK_RETURN;
  KEY_SPACE = VK_SPACE;
  KEY_MENU = VK_MENU;
  KEY_BACK = VK_BACK;
  KEY_F1 = VK_F1;
  KEY_F2 = VK_F2;
  KEY_F3 = VK_F3;
  KEY_F4 = VK_F4;
  KEY_F5 = VK_F5;
  KEY_F6 = VK_F6;
  KEY_F7 = VK_F7;
  KEY_F8 = VK_F8;
  KEY_F9 = VK_F9;
  KEY_F10 = VK_F10;
  KEY_F11 = VK_F11;
  KEY_F12 = VK_F12;
  {$ENDIF}

{$IFDEF WEBLIB}
const
  SReadError = 'Stream read error';
  SWriteError = 'Stream write error';
{$ENDIF}

type
  {$IFDEF WEBLIB}

  EStreamError = class(Exception);
  EFilerError = class(EStreamError);
  EReadError = class(EFilerError);
  EWriteError = class(EFilerError);

  TResourceStream = TObject;
  TStream = class(TObject)
  private
    FPosition, FSize: Int64;
    FCapacity: Integer;
    FBytes: TBytes;
  public
    procedure SetSize(const NewSize: Int64);
    procedure SetCapacity(NewCapacity: NativeInt);
    procedure Clear;
    procedure SaveToFile(FileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    function Read(var Buffer: TBytes; Count: Longint): Longint; overload;
    function Read(var Buffer: TBytes; Offset, Count: Longint): Longint; overload;
    procedure ReadBuffer(var Buffer: TBytes; Count: NativeInt); overload;
    procedure ReadBuffer(var Buffer: TBytes; Offset, Count: NativeInt); overload;
    procedure WriteBuffer(const Buffer: TBytes; Count: NativeInt); overload;
    procedure WriteBuffer(const Buffer: TBytes; Offset, Count: NativeInt); overload;
    function CopyFrom(const Source: TStream; Count: Int64): Int64;
    function Write(const Buffer: TBytes; Count: Longint): Longint; overload;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; overload;
    property Position: Int64 read FPosition write FPosition;
    property Size: Int64 read FSize write SetSize;
    property Bytes: TBytes read FBytes;
  end;

  TMemoryStream = class(TStream);

  TStringStream = class(TStream)
  private
    function GetDataString: string;
    function StringToByteArray(AString: string): TBytes;
    function ByteArrayToString(ABytes: TBytes): string;
  public
    constructor Create(const AString: string); overload;
    function ReadString(Count: Integer): string;
    procedure WriteString(const AString: string);
    property DataString: string read GetDataString;
  end;

  TBytesStream = class(TStream);

  TPopupMenu = class(TComponent);
  PChar = string;
  {$ENDIF}

  {$IFDEF LCLLIB}
  TJSONValue = TJSONData;
  {$ENDIF}

  TAdvMouseButton = TMouseButton;

  {$IFDEF USETRECTF}
  {$IFNDEF WEBLIB}
  PSizeF = ^TSizeF;
  {$ENDIF}
  TSizeF = record
    cx: Single;
    cy: Single;
  {$IFNDEF WEBLIB}
  public
    property Width: Single read cx write cx;
    property Height: Single read cy write cy;
  {$ENDIF}
  end;

  {$IFNDEF WEBLIB}
  TPointFType = array [0..1] of Single;
  PPointF = ^TPointF;
  {$ENDIF}
  TPointF = record
    {$IFDEF WEBLIB}
    X, Y: Single;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    function Length: Single;
    procedure Offset(const ADeltaX, ADeltaY: Single);
    case Integer of
      0: (V: TPointFType;);
      1: (X: Single;
          Y: Single;);
    {$ENDIF}
  end;

  {$IFNDEF WEBLIB}
  PRectF = ^TRectF;
  {$ENDIF}
  TRectF = record
  {$IFNDEF WEBLIB}
  private
    function GetWidth: Single;
    procedure SetWidth(const Value: Single);
    function GetHeight: Single;
    procedure SetHeight(const Value: Single);
    function GetSize: TSizeF;
    procedure SetSize(const Value: TSizeF);
  public
    procedure Offset(const DX, DY: Single);
    procedure Inflate(const DX, DY: Single);
    function CenterAt(const ADesignatedArea: TRectF): TRectF;
    function SnapToPixel(const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
    function FitInto(const ADesignatedArea: TRectF; out Ratio: Single): TRectF; overload;
    function FitInto(const ADesignatedArea: TRectF): TRectF; overload;
    function IsEmpty: Boolean;
    function IntersectsWith(const R: TRectF): Boolean;
    function CenterPoint: TPointF;
    function Empty: TRectF;
    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;
    property Size: TSizeF read GetSize write SetSize;
    case Integer of
      0: (Left, Top, Right, Bottom: Single);
      1: (TopLeft, BottomRight: TPointF);
  {$ENDIF}
  {$IFDEF WEBLIB}
    Left, Top, Right, Bottom: Single;
  {$ENDIF}
  end;
  {$ENDIF}

  {$IFDEF FMXLIB}
  TAdvBitmap = TBitmap;
  TAdvDrawBitmap = TBitmap;
  {$ENDIF}
  {$IFDEF CMNLIB}
  TAdvBitmap = TPicture;
  TAdvDrawBitmap = TGraphic;
  {$ENDIF}
  {$IFDEF WEBLIB}
  TAdvBitmap = class(TBitmap);
  TAdvDrawBitmap = TAdvBitmap;
  {$ENDIF}

  {$IFNDEF LCLLIB}
  {$IFNDEF USETRECTF}
  TRectFHelper = record helper for TRectF
    function CenterAt(const ADesignatedArea: TRectF): TRectF;
    function SnapToPixel(const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
    function FitInto(const ADesignatedArea: TRectF; out Ratio: Single): TRectF; overload;
    function FitInto(const ADesignatedArea: TRectF): TRectF; overload;
  end;
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF WEBLIB}
  TAdvBitmapHelper = class helper for TAdvBitmap
    procedure LoadFromURL(AURL: String); overload;
    procedure LoadFromURL(AURL: String; {%H-}AInstance: NativeUInt); overload;
    procedure LoadFromResource(AResourceName: String); overload;
    procedure LoadFromResource(AResourceName: String; {%H-}AInstance: NativeUInt); overload;
    {$IFDEF VCLLIB}
    procedure LoadFromStream(AStream: TCustomMemoryStream);
    procedure SaveToStream(AStream: TCustomMemoryStream);
    {$ENDIF}
    {$IFDEF CMNLIB}
    class function CreateFromStream(AStream: TCustomMemoryStream): TAdvBitmap; overload;
    {$ENDIF}
    class function CreateFromResource(AResourceName: String): TAdvBitmap; overload;
    class function CreateFromResource(AResourceName: String; {%H-}AInstance: NativeUInt): TAdvBitmap; overload;
    class function CreateFromURL(AURL: String): TAdvBitmap; overload;
    class function CreateFromURL(AURL: String; {%H-}AInstance: NativeUInt): TAdvBitmap; overload;
  end;
  {$ENDIF}

  TAdvMargins = class(TPersistent)
  private
    FRight: Single;
    FBottom: Single;
    FTop: Single;
    FLeft: Single;
    FOnChange: TNotifyEvent;
    procedure SetBottom(const Value: Single);
    procedure SetLeft(const Value: Single);
    procedure SetRight(const Value: Single);
    procedure SetTop(const Value: Single);
    function IsLeftStored: Boolean;
    function IsTopStored: Boolean;
    function IsRightStored: Boolean;
    function IsBottomStored: Boolean;
  protected
    procedure Changed;
  public
    constructor Create; overload;
    constructor Create(const ARect: TRectF); overload;
    procedure Assign(Source: TPersistent); override;
    function Empty: Boolean;
    function Rect: TRectF;
    function PaddingRect(const R: TRectF): TRectF;
  published
    property Left: Single read FLeft write SetLeft stored IsLeftStored nodefault;
    property Top: Single read FTop write SetTop stored IsTopStored nodefault;
    property Right: Single read FRight write SetRight stored IsRightStored nodefault;
    property Bottom: Single read FBottom write SetBottom stored IsBottomStored nodefault;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvPoint = class(TPersistent)
  private
    FY: Single;
    FX: Single;
    FOnChange: TNotifyEvent;
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    function IsXStored: Boolean;
    function IsYStored: Boolean;
  protected
    procedure Changed;
  public
    constructor Create; overload;
    constructor Create(const APoint: TPointF); overload;
    procedure Assign(Source: TPersistent); override;
  published
    property X: Single read FX write SetX stored IsXStored nodefault;
    property Y: Single read FY write SetY stored IsYStored nodefault;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFNDEF LIMITEDGRAPHICSMODE}
  TAdvScaledBitmap = class(TCollectionItem)
  private
    FBitmap: TAdvBitmap;
    FScale: Single;
    FBitmapName: string;
    function IsScaleStored: Boolean;
    procedure SetBitmap(const Value: TAdvBitmap);
    procedure SetScale(const Value: Single);
    procedure SetBitmapName(const Value: string);
  protected
    procedure BitmapChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Bitmap: TAdvBitmap read FBitmap write SetBitmap;
    property BitmapName: string read FBitmapName write SetBitmapName;
    property Scale: Single read FScale write SetScale stored IsScaleStored nodefault;
  end;

  TAdvScaledBitmaps = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItems(Index: Integer): TAdvScaledBitmap;
    procedure SetItems(Index: Integer; const Value: TAdvScaledBitmap);
    function GetBitmap(Scale: Single): TAdvBitmap;
    procedure SetBitmap(Scale: Single; const Value: TAdvBitmap);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function Add(Scale: Single = 1.0): TAdvScaledBitmap; overload;
    {$IFNDEF WEBLIB}
    function AddBitmapFromFile(FileName: string; Scale: Single = 1.0): TAdvScaledBitmap;
    {$ENDIF}
    function AddBitmapFromResource(ResourceName: String; Scale: Single = 1.0): TAdvScaledBitmap; overload;
    function AddBitmapFromResource(ResourceName: String; AInstance: NativeUInt; Scale: Single = 1.0): TAdvScaledBitmap; overload;
    function AddBitmap(Bitmap: TAdvBitmap; Scale: Single = 1.0): TAdvScaledBitmap;
    function AddBitmapName(BitmapName: string; Scale: Single = 1.0): TAdvScaledBitmap;
    function AddDrawBitmap(Bitmap: TAdvDrawBitmap; Scale: Single = 1.0): TAdvScaledBitmap;
    function Insert(Index: Integer): TAdvScaledBitmap; overload;
    function Insert(Index: Integer; Scale: Single): TAdvScaledBitmap; overload;
    function GetBitmapByScale(Scale: Single): TAdvBitmap;
    function GetItemByScale(Scale: Single): TAdvScaledBitmap;
    property Items[Index: Integer]: TAdvScaledBitmap read GetItems write SetItems; default;
    property Bitmaps[Scale: Single]: TAdvBitmap read GetBitmap write SetBitmap;
  end;
  {$ENDIF}

  TAdvPictureFormat = (pfBMP, pfGIF, pfJPG, pfPNG, pfICO, pfTiff, pfMetaFile, pfNone);

  IAdvCustomEditor = interface
    ['{CC0C60B7-75F3-47CE-8A7F-8005A19F12E8}']
    procedure SetText(AValue: String);
    procedure SetSelStart(AValue: Integer);
    procedure SetSelLength(AValue: Integer);
    function GetTextLength: Integer;
  end;

  {$IFNDEF WEBLIB}
  {$IFDEF LCLLIB}generic {$ENDIF}TAdvOwnedCollection<T: class> = class(TOwnedCollection)
  public type
    TAdvCollection = {$IFDEF LCLLIB}specialize {$ENDIF}TAdvOwnedCollection<T>;

    TEnumerator = class
    private
      FIndex: Integer;
      FCol: TAdvCollection;
    public
      constructor Create(ACol: TAdvCollection);
      function GetCurrent: T;
      function MoveNext: Boolean;
      property Current: T read GetCurrent;
    end;
  public
    function GetEnumerator: TEnumerator;
    function GetItem(AIndex: Integer): T;
  end;
  {$ENDIF}
  {$IFDEF WEBLIB}
  TAdvOwnedCollection = class(TOwnedCollection);
  {$ENDIF}

  {$IFNDEF WEBLIB}
  TAdvPersistent = class(TInterfacedPersistent);
  {$ENDIF}
  {$IFDEF WEBLIB}
  TAdvPersistent = class(TPersistent);
  {$ENDIF}

  TAdvValueRelationShip = TValueRelationShip;

procedure InflateRectEx(var R: TRectF; DX, DY: Single);
function ConvertToRectF(const Rect: TRect): TRectF; overload;
function ConvertToRectF(const Rect: TRectF): TRectF; overload;
function ConvertToRect(const Rect: TRectF): TRect; overload;
function ConvertToRect(const Rect: TRect): TRect; overload;
function ConvertToSizeF(const Size: TSize): TSizeF; overload;
function ConvertToSizeF(const Size: TSizeF): TSizeF; overload;
function ConvertToSize(const Size: TSizeF): TSize; overload;
function ConvertToSize(const Size: TSize): TSize; overload;
function ConvertToPointF(const Point: TPoint): TPointF; overload;
function ConvertToPointF(const Point: TPointF): TPointF; overload;
function ConvertToPoint(const Point: TPointF): TPoint; overload;
function ConvertToPoint(const Point: TPoint): TPoint; overload;
function OffsetRectEx(var R: TRect; DX, DY: Integer): Boolean; overload;
function OffsetRectEx(var R: TRectF; DX, DY: Single): Boolean; overload;
function PtInRectEx(const Rect: TRectF; const P: TPointF): Boolean;
function IntersectRectEx(const Rect1: TRectF; const Rect2: TRectF): Boolean; overload;
function IntersectRectEx(out Rect: TRectF; const R1, R2: TRectF): Boolean; overload;
function EqualRectEx(const R1, R2: TRectF): Boolean; overload;
function EqualRectEx(const R1, R2: TRect): Boolean; overload;
function RectWidthEx(const Rect: TRect): Integer; overload;
function RectWidthEx(const Rect: TRectF): Single; overload;
function RectHeightEx(const Rect: TRect): Integer; overload;
function RectHeightEx(const Rect: TRectF): Single; overload;
function RectCenterEx(var R: TRect; const B: TRect): TRect; overload;
function RectCenterEx(var R: TRectF; const B: TRectF): TRectF; overload;
function RectCenterAtEx(const Rect: TRectF; const ADesignatedArea: TRectF): TRectF;
function RectSnapToPixelEx(const Rect: TRectF; const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
function RectFitIntoEx(const Rect: TRectF; const ADesignatedArea: TRectF; out Ratio: Single): TRectF; overload;
function RectFitIntoEx(const Rect: TRectF; const ADesignatedArea: TRectF): TRectF; overload;
function GetPointLength(const Point: TPointF): Single;
function MakeRectF(Left, Top, Width, Height: Single): TRectF;
function CenterPointEx(const R: TRectF): TPointF;
function CompareValueEx(const A, B: Extended; Epsilon: Extended = 0): TAdvValueRelationship;
function RectIsEmpty(const R: TRectF): Boolean;
function EmptyRect: TRectF;
function RectIntersectsWithEx(const ARect: TRectF; const R: TRectF): Boolean;
{$IFDEF USETRECTF}
function RectF(Left, Top, Right, Bottom: Single): TRectF; {$IFNDEF WEBLIB}inline; {$ENDIF}overload;
function PointF(X, Y: Single): TPointF; {$IFNDEF WEBLIB}inline; {$ENDIF}overload;
{$ENDIF}
function BitmapToDrawBitmap(ABitmap: TAdvBitmap): TAdvDrawBitmap; overload;
function IsBitmapEmpty(ABitmap: TAdvBitmap): Boolean;
function CharInArray(AChar: Char; ACharArray: array of char): Boolean;
function CharIsNumber(AChar: Char): Boolean;
function CharIsLetter(AChar: Char): Boolean;
function CharIsHex(AChar: Char): Boolean;
function CharIsLetterOrNumber(AChar: Char): Boolean;
{$IFDEF WEBLIB}
function VarToStr(const S: string): string;
function VarToDateTime(const S: string): TDateTime;
function AnsiPos(const Substr, S: string): Integer;
function AnsiUpperCase(const S: string): string;
function HInstance: Integer;
{$ENDIF}

implementation

uses
  {$IFDEF WEBLIB}
  JS
  {$ENDIF}
  {$IFNDEF WEBLIB}
  AdvUtils
  {$IFDEF VCLLIB}
  ,AnsiStrings
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,PNGImage, JPEG, GifImg
  {$ENDIF}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 24}
  ,Character
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$ENDIF}
  ;

{$IFDEF WEBLIB}
function VarToStr(const S: string): string;
begin
  Result := S;
end;

function VarToDateTime(const S: string): TDateTime;
begin
  Result := StrToDateTime(S);
end;

function HInstance: Integer;
begin
  Result := 0;
end;

function AnsiUpperCase(const S: string): string;
begin
  Result := UpperCase(S);
end;

function AnsiPos(const Substr, S: string): Integer;
begin
  Result := Pos(SubStr, S);
end;
{$ENDIF}

function CharIsHex(AChar: Char): Boolean;
begin
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$WARNINGS OFF}
  {$IF COMPILERVERSION > 24}
  Result := AChar.IsNumber or AChar.IsInArray(['a', 'b', 'c', 'd', 'e', 'f']) or AChar.IsInArray(['A', 'B', 'C', 'D', 'E', 'F']);
  {$ELSE}
  Result := (AChar in ['0'..'9']) or (AChar in ['a'..'f']) or (AChar in ['A'..'F']);
  {$IFEND}
  {$WARNINGS ON}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  Result := (AChar in ['0'..'9']) or (AChar in ['a'..'f']) or (AChar in ['A'..'F']);
  {$ENDIF}
end;

function CharIsLetter(AChar: Char): Boolean;
begin
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$WARNINGS OFF}
  {$IF COMPILERVERSION > 24}
  Result := AChar.IsLetter;
  {$ELSE}
  Result := (AChar in ['a'..'z']) or (AChar in ['A'..'Z']);
  {$IFEND}
  {$WARNINGS ON}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  Result := (AChar in ['a'..'z']) or (AChar in ['A'..'Z']);
  {$ENDIF}
end;

function CharIsNumber(AChar: Char): Boolean;
begin
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$WARNINGS OFF}
  {$IF COMPILERVERSION > 24}
  Result := AChar.IsNumber;
  {$ELSE}
  Result := (AChar in ['0'..'9']);
  {$IFEND}
  {$WARNINGS ON}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  Result := (AChar in ['0'..'9']);
  {$ENDIF}
end;

function CharIsLetterOrNumber(AChar: Char): Boolean;
begin
  Result := CharIsLetter(AChar) or CharIsNumber(AChar);
end;

function CharInArray(AChar: Char; ACharArray: array of char): Boolean;
var
  ch: Char;
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(ACharArray) - 1 do
  begin
    ch := ACharArray[I];
    if ch = AChar then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function IsBitmapEmpty(ABitmap: TAdvBitmap): Boolean;
var
  b: TAdvDrawBitmap;
begin
  Result := True;
  if Assigned(ABitmap) then
  begin
    b := BitmapToDrawBitmap(ABitmap);
    if Assigned(b) then
    begin
      {$IFDEF CMNLIB}
      Result := b.Empty;
      {$ENDIF}
      {$IFDEF WEBLIB}
      Result := b.Empty;
      {$ENDIF}
      {$IFDEF FMXLIB}
      Result := b.IsEmpty
      {$ENDIF}
    end;
  end;
end;

function BitmapToDrawBitmap(ABitmap: TAdvBitmap): TAdvDrawBitmap;
begin
  Result := nil;
  if Assigned(ABitmap) then
  begin
    {$IFDEF CMNLIB}
    Result := ABitmap.Graphic;
    {$ENDIF}
    {$IFDEF WEBLIB}
    Result := ABitmap;
    {$ENDIF}
    {$IFDEF FMXLIB}
    Result := ABitmap;
    {$ENDIF}
  end;
end;

{$IFNDEF WEBLIB}

{ TAdvBitmapHelper }

class function TAdvBitmapHelper.CreateFromResource(AResourceName: String): TAdvBitmap;
begin
  Result := CreateFromResource(AResourceName, TAdvUtils.GetHInstance);
end;

class function TAdvBitmapHelper.CreateFromResource(AResourceName: String; AInstance: NativeUInt): TAdvBitmap;
begin
  Result := TAdvBitmap.Create;
  Result.LoadFromResource(AResourceName, AInstance);
end;

class function TAdvBitmapHelper.CreateFromURL(AURL: String): TAdvBitmap;
begin
  Result := CreateFromURL(AURL, TAdvUtils.GetHInstance);
end;

class function TAdvBitmapHelper.CreateFromURL(AURL: String; AInstance: NativeUInt): TAdvBitmap;
begin
  Result := TAdvBitmap.Create;
  Result.LoadFromURL(AURL, AInstance);
end;

{$IFDEF VCLLIB}
procedure TAdvBitmapHelper.SaveToStream(AStream: TCustomMemoryStream);
begin
  if Assigned(AStream) then
  begin
    if Assigned(Graphic) then
      Graphic.SaveToStream(AStream);
  end;
end;

procedure TAdvBitmapHelper.LoadFromStream(AStream: TCustomMemoryStream);
var
  pic: TGraphic;
  gcc: TGraphicClass;
begin
  if Assigned(AStream) then
  begin
    pic := nil;
    if TAdvUtils.FindGraphicClass(AStream.Memory^, AStream.Size, gcc) then
      pic := gcc.Create;

    if Assigned(pic) then
    begin
      try
        AStream.Position := 0;
        pic.LoadFromStream(AStream);
        Self.Assign(pic);
      finally
        pic.Free;
      end;
    end;
  end;
end;
{$ENDIF}

{$IFDEF CMNLIB}
class function TAdvBitmapHelper.CreateFromStream(AStream: TCustomMemoryStream): TAdvBitmap;
begin
  Result := TAdvBitmap.Create;
  Result.LoadFromStream(AStream);
end;
{$ENDIF}

procedure TAdvBitmapHelper.LoadFromURL(AURL: String);
begin
  LoadFromURL(AURL, TAdvUtils.GetHInstance);
end;

procedure TAdvBitmapHelper.LoadFromURL(AURL: String; AInstance: NativeUInt);
var
  ms: TMemoryStream;
begin
  if Pos('HTTP', Uppercase(AURL)) <> 1 then
  begin
    LoadFromFile(AURL);
    Exit;
  end;

  ms := TAdvUtils.DownloadImage(AURL);
  try
    if Assigned(ms) then
      Self.LoadFromStream(ms);
  finally
    if Assigned(ms) then
      ms.Free;
  end;
end;

procedure TAdvBitmapHelper.LoadFromResource(AResourceName: String);
begin
  LoadFromResource(AResourceName, TAdvUtils.GetHInstance);
end;

procedure TAdvBitmapHelper.LoadFromResource(AResourceName: String; AInstance: NativeUInt);
var
  r: TResourceStream;
begin
  r := nil;
  try
    r := TAdvUtils.GetResourceStream(AResourceName, AInstance);
    if Assigned(r) then
      Self.LoadFromStream(r);
  finally
    if Assigned(r) then
      r.Free;
  end;
end;
{$ENDIF}

{$IFDEF USETRECTF}

{ TRectF }

{$IFNDEF WEBLIB}
function TRectF.GetSize: TSizeF;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

procedure TRectF.SetSize(const Value: TSizeF);
begin
  Width := Value.cx;
  Height := Value.cy;
end;

function TRectF.GetHeight: Single;
begin
  Result := Self.Bottom - Self.Top;
end;

procedure TRectF.SetHeight(const Value: Single);
begin
  Self.Bottom := Self.Top + Value;
end;

function TRectF.GetWidth: Single;
begin
  Result := Self.Right - Self.Left;
end;

procedure TRectF.SetWidth(const Value: Single);
begin
  Self.Right := Self.Left + Value;
end;

function TRectF.CenterPoint: TPointF;
begin
  Result.X := (Right - Left) / 2.0 + Left;
  Result.Y := (Bottom - Top) / 2.0 + Top;
end;

function TRectF.IntersectsWith(const R: TRectF): Boolean;
begin
  Result := not ( (Self.BottomRight.X < R.TopLeft.X) or
                  (Self.BottomRight.Y < R.TopLeft.Y) or
                  (R.BottomRight.X < Self.TopLeft.X) or
                  (R.BottomRight.Y < Self.TopLeft.Y) );
end;

procedure TRectF.Offset(const DX, DY: Single);
begin
  TopLeft.Offset(DX, DY);
  BottomRight.Offset(DX, DY);
end;

procedure TRectF.Inflate(const DX, DY: Single);
begin
  TopLeft.Offset(-DX, -DY);
  BottomRight.Offset(DX, DY);
end;

function TRectF.Empty: TRectF;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

function TRectF.IsEmpty: Boolean;
begin
  Result := (Right < Left) or SameValue(Right, Left)
         or (Bottom < Top) or SameValue(Bottom, Top);
end;
{$ENDIF}

{$IFNDEF WEBLIB}
procedure TPointF.Offset(const ADeltaX, ADeltaY: Single);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
end;

function TPointF.Length: Single;
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
end;
{$ENDIF}

function PtInRectEx(const Rect: TRectF; const P: TPointF): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

function PointF(X, Y: Single): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function OffsetRectEx(var R: TRect; DX, DY: Integer): Boolean;
begin
  {$IFNDEF WEBLIB}
  if @R <> nil then
  {$ENDIF}
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
    Result := True;
  end
  {$IFNDEF WEBLIB}
  else
    Result := False;
  {$ENDIF}
end;

function OffsetRectEx(var R: TRectF; DX, DY: Single): Boolean;
begin
  {$IFNDEF WEBLIB}
  if @R <> nil then
  {$ENDIF}
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
    Result := True;
  end
  {$IFNDEF WEBLIB}
  else
    Result := False;
  {$ENDIF}
end;

function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

procedure InflateRectEx(var R: TRectF; DX, DY: Single);
begin
  R.Left := R.Left - DX;
  R.Right := R.Right + DX;
  R.Top := R.Top - DY;
  R.Bottom := R.Bottom + DY;
end;

function IntersectRectEx(const Rect1: TRectF; const Rect2: TRectF): Boolean;
begin
  Result := (Rect1.Left < Rect2.Right)
        and (Rect1.Right > Rect2.Left)
        and (Rect1.Top < Rect2.Bottom)
        and (Rect1.Bottom > Rect2.Top);
end;

function EqualRectEx(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Right = R2.Right) and
    (R1.Top = R2.Top) and (R1.Bottom = R2.Bottom);
end;

function EqualRectEx(const R1, R2: TRectF): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Right = R2.Right) and
    (R1.Top = R2.Top) and (R1.Bottom = R2.Bottom);
end;

{$ELSE}

function EqualRectEx(const R1, R2: TRect): Boolean;
begin
  Result := EqualRect(R1, R2);
end;

function EqualRectEx(const R1, R2: TRectF): Boolean;
begin
  Result := EqualRect(R1, R2);
end;

function IntersectRectEx(const Rect1: TRectF; const Rect2: TRectF): Boolean;
begin
  Result := IntersectRect(Rect1, Rect2);
end;

function OffsetRectEx(var R: TRect; DX, DY: Integer): Boolean;
begin
  Result := OffsetRect(R, DX, DY);
end;

function OffsetRectEx(var R: TRectF; DX, DY: Single): Boolean;
begin
  Result := OffsetRect(R, DX, DY);
end;

procedure InflateRectEx(var R: TRectF; DX, DY: Single);
begin
  InflateRect(R, DX, DY);
end;

function PtInRectEx(const Rect: TRectF; const P: TPointF): Boolean;
begin
  Result := PtInRect(Rect, P);
end;

{$ENDIF}

{$IFNDEF WEBLIB}
function {$IFDEF USETRECTF}TRectF{$ELSE}{$IFNDEF LCLLIB}TRectFHelper{$ELSE}TRectF{$ENDIF}{$ENDIF}.FitInto(const ADesignatedArea: TRectF; out Ratio: Single): TRectF;
begin
  if (ADesignatedArea.Width <= 0) or (ADesignatedArea.Height <= 0) then
  begin
    Ratio := 1;
    Exit(Self);
  end;

  if (Self.Width / ADesignatedArea.Width) > (Self.Height / ADesignatedArea.Height) then
    Ratio := Self.Width / ADesignatedArea.Width
  else
    Ratio := Self.Height / ADesignatedArea.Height;

  Result := RectF(0, 0, Self.Width / Ratio, Self.Height / Ratio);
  RectCenterEx(Result, ADesignatedArea);
end;

function {$IFDEF USETRECTF}TRectF{$ELSE}{$IFNDEF LCLLIB}TRectFHelper{$ELSE}TRectF{$ENDIF}{$ENDIF}.FitInto(const ADesignatedArea: TRectF): TRectF;
var
  Ratio: Single;
begin
  Result := FitInto(ADesignatedArea, Ratio);
end;

function {$IFDEF USETRECTF}TRectF{$ELSE}{$IFNDEF LCLLIB}TRectFHelper{$ELSE}TRectF{$ENDIF}{$ENDIF}.CenterAt(const ADesignatedArea: TRectF): TRectF;
begin
  Result := Self;
  RectCenterEx(Result, ADesignatedArea);
end;

function {$IFDEF USETRECTF}TRectF{$ELSE}{$IFNDEF LCLLIB}TRectFHelper{$ELSE}TRectF{$ENDIF}{$ENDIF}.SnapToPixel(const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
var
  LScale, HalfPixel: Single;
begin
  if AScale <= 0 then
    LScale := 1
  else
    LScale := AScale;
  Result.Left := System.Trunc(Self.Left * LScale) / LScale;
  Result.Top := System.Trunc(Self.Top * LScale) / LScale;
  Result.Width := System.Round(Self.Width * LScale) / LScale;
  Result.Height := System.Round(Self.Height * LScale) / LScale;
  if APlaceBetweenPixels then
  begin
    HalfPixel := 1 / (2 * LScale);
    Result.Offset(HalfPixel, HalfPixel);
  end;
end;
{$ENDIF}

function RectWidthEx(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeightEx(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function RectWidthEx(const Rect: TRectF): Single;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeightEx(const Rect: TRectF): Single;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function RectCenterAtEx(const Rect: TRectF; const ADesignatedArea: TRectF): TRectF;
begin
  Result := Rect;
  RectCenterEx(Result, ADesignatedArea);
end;

function RectSnapToPixelEx(const Rect: TRectF; const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
var
  LScale, HalfPixel: Single;
begin
  if AScale <= 0 then
    LScale := 1
  else
    LScale := AScale;
  Result.Left := Trunc(Rect.Left * LScale) / LScale;
  Result.Top := Trunc(Rect.Top * LScale) / LScale;
  Result.Right := Result.Left + Round((Rect.Right - Rect.Left) * LScale) / LScale;
  Result.Bottom := Result.Top + Round((Rect.Bottom - Rect.Top) * LScale) / LScale;
  if APlaceBetweenPixels then
  begin
    HalfPixel := 1 / (2 * LScale);
    OffsetRectEx(Result, HalfPixel, HalfPixel);
  end;
end;

function RectFitIntoEx(const Rect: TRectF; const ADesignatedArea: TRectF; out Ratio: Single): TRectF;
begin
  if (ADesignatedArea.Right - ADesignatedArea.Left <= 0) or (ADesignatedArea.Bottom - ADesignatedArea.Top <= 0) then
  begin
    Ratio := 1;
    Exit(Rect);
  end;

  if ((Rect.Right - Rect.Left) / (ADesignatedArea.Right - ADesignatedArea.Left)) > ((Rect.Bottom - Rect.Top) / (ADesignatedArea.Bottom - ADesignatedArea.Top)) then
    Ratio := (Rect.Right - Rect.Left) / (ADesignatedArea.Right - ADesignatedArea.Left)
  else
    Ratio := (Rect.Bottom - Rect.Top) / (ADesignatedArea.Bottom - ADesignatedArea.Top);

  Result := RectF(0, 0, (Rect.Right - Rect.Left) / Ratio, (Rect.Bottom - Rect.Top) / Ratio);
  RectCenterEx(Result, ADesignatedArea);
end;

function RectFitIntoEx(const Rect: TRectF; const ADesignatedArea: TRectF): TRectF;
var
  Ratio: Single;
begin
  Result := RectFitIntoEx(Rect, ADesignatedArea, Ratio);
end;

function RectCenterEx(var R: TRect; const B: TRect): TRect;
begin
  OffsetRectEx(R, -R.Left, -R.Top);
  OffsetRectEx(R, (RectWidthEx(B) - RectWidthEx(R)) div 2, (RectHeightEx(B) - RectHeightEx(R)) div 2);
  OffsetRectEx(R, B.Left, B.Top);
  Result := R;
end;

function RectCenterEx(var R: TRectF; const B: TRectF): TRectF;
begin
  OffsetRectEx(R, -R.Left, -R.Top);
  OffsetRectEx(R, Round((RectWidthEx(B) - RectWidthEx(R)) / 2), Round((RectHeightEx(B) - RectHeightEx(R)) / 2));
  OffsetRectEx(R, B.Left, B.Top);
  Result := R;
end;

function IntersectRectEx(out Rect: TRectF; const R1, R2: TRectF): Boolean;
var
  tmpRect: TRectF;
begin
  tmpRect := R1;
  if R2.Left > R1.Left then tmpRect.Left := R2.Left;
  if R2.Top > R1.Top then tmpRect.Top := R2.Top;
  if R2.Right < R1.Right then tmpRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then tmpRect.Bottom := R2.Bottom;
  {$IFDEF WEBLIB}
  Result := not RectIsEmpty(tmpRect);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := not tmpRect.IsEmpty;
  {$ENDIF}
  if not Result then
  begin
    tmpRect.Top := 0.0;
    tmpRect.Bottom := 0.0;
    tmpRect.Left := 0.0;
    tmpRect.Right := 0.0;
  end;
  Rect := tmpRect;
end;

function EmptyRect: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
end;

function RectIsEmpty(const R: TRectF): Boolean;
begin
  Result := (R.Right < R.Left) or SameValue(R.Right, R.Left)
         or (R.Bottom < R.Top) or SameValue(R.Bottom, R.Top);
end;

function CompareValueEx(const A, B: Extended; Epsilon: Extended = 0): TAdvValueRelationShip;
begin
  if SameValue(A, B, Epsilon) then
    Result := EqualsValue
  else if A < B then
    Result := LessThanValue
  else
    Result := GreaterThanValue;
end;

function RectIntersectsWithEx(const ARect: TRectF; const R: TRectF): Boolean;
begin
  Result := (ARect.Left < R.Right)
        and (ARect.Right > R.Left)
        and (ARect.Top < R.Bottom)
        and (ARect.Bottom > R.Top);
end;

function CenterPointEx(const R: TRectF): TPointF;
begin
  Result.X := (R.Right - R.Left) / 2.0 + R.Left;
  Result.Y := (R.Bottom - R.Top) / 2.0 + R.Top;
end;

function MakeRectF(Left, Top, Width, Height: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  {$IFDEF WEBLIB}
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result.Width := Width;
  Result.Height := Height;
  {$ENDIF}
end;

function GetPointLength(const Point: TPointF): Single;
begin
  Result := Sqrt(Sqr(Point.X) + Sqr(Point.Y));
end;

function ConvertToRect(const Rect: TRect): TRect;
begin
  Result := Rect;
end;

function ConvertToRect(const Rect: TRectF): TRect;
begin
  Result.Left := Round(Rect.Left);
  Result.Top := Round(Rect.Top);
  Result.Right := Round(Rect.Right);
  Result.Bottom := Round(Rect.Bottom);
end;

function ConvertToRectF(const Rect: TRectF): TRectF;
begin
  Result := Rect;
end;

function ConvertToRectF(const Rect: TRect): TRectF;
begin
  Result.Left := Rect.Left;
  Result.Top := Rect.Top;
  Result.Right := Rect.Right;
  Result.Bottom := Rect.Bottom;
end;

function ConvertToPoint(const Point: TPoint): TPoint;
begin
  Result := Point;
end;

function ConvertToPoint(const Point: TPointF): TPoint;
begin
  Result.X := Round(Point.X);
  Result.Y := Round(Point.Y);
end;

function ConvertToPointF(const Point: TPointF): TPointF;
begin
  Result := Point;
end;

function ConvertToPointF(const Point: TPoint): TPointF;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
end;

function ConvertToSize(const Size: TSize): TSize;
begin
  Result := Size;
end;

function ConvertToSize(const Size: TSizeF): TSize;
begin
  Result.cx := Round(Size.cx);
  Result.cy := Round(Size.cy);
end;

function ConvertToSizeF(const Size: TSizeF): TSizeF;
begin
  Result := Size;
end;

function ConvertToSizeF(const Size: TSize): TSizeF;
begin
  Result.cx := Size.cx;
  Result.cy := Size.cy;
end;

{ TAdvMargins }

procedure TAdvMargins.Assign(Source: TPersistent);
begin
  if Source is TAdvMargins then
  begin
    FLeft := (Source as TAdvMargins).Left;
    FTop := (Source as TAdvMargins).Top;
    FRight := (Source as TAdvMargins).Right;
    FBottom := (Source as TAdvMargins).Bottom;
  end;
end;

procedure TAdvMargins.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvMargins.Create(const ARect: TRectF);
begin
  FLeft := ARect.Left;
  FBottom := ARect.Bottom;
  FRight := ARect.Right;
  FTop := ARect.Top;
end;

constructor TAdvMargins.Create;
begin
  FLeft := 0;
  FBottom := 0;
  FRight := 0;
  FTop := 0;
end;

function TAdvMargins.Empty: Boolean;
var
  r: TRectF;
begin
  r := RectF(Left, Top, Right, Bottom);
  {$IFDEF WEBLIB}
  Result := RectIsEmpty(R);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := R.IsEmpty;
  {$ENDIF}
end;

procedure TAdvMargins.SetBottom(const Value: Single);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TAdvMargins.SetLeft(const Value: Single);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TAdvMargins.SetRight(const Value: Single);
begin
  if FRight <> Value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TAdvMargins.SetTop(const Value: Single);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Changed;
  end;
end;

function TAdvMargins.IsLeftStored: Boolean;
begin
  Result := Left <> 0;
end;

function TAdvMargins.IsTopStored: Boolean;
begin
  Result := Top <> 0;
end;

function TAdvMargins.PaddingRect(const R: TRectF): TRectF;
begin
  Result := RectF(R.Left + FLeft, R.Top + FTop, R.Right - FRight, R.Bottom - FBottom);
end;

function TAdvMargins.Rect: TRectF;
begin
  Result := RectF(Left, Top, Right, Bottom);
end;

function TAdvMargins.IsBottomStored: Boolean;
begin
  Result := Bottom <> 0;
end;

function TAdvMargins.IsRightStored: Boolean;
begin
  Result := Right <> 0;
end;

{ TAdvPoint }

procedure TAdvPoint.Assign(Source: TPersistent);
begin
  if Source is TAdvPoint then
  begin
    FX := (Source as TAdvPoint).X;
    FY := (Source as TAdvPoint).Y;
  end;
end;

procedure TAdvPoint.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvPoint.Create(const APoint: TPointF);
begin
  FX := APoint.X;
  FY := APoint.Y;
end;

constructor TAdvPoint.Create;
begin
  FX := 0;
  FY := 0;
end;

procedure TAdvPoint.SetX(const Value: Single);
begin
  if FX <> Value then
  begin
    FX := Value;
    Changed;
  end;
end;

procedure TAdvPoint.SetY(const Value: Single);
begin
  if FY <> Value then
  begin
    FY := Value;
    Changed;
  end;
end;

function TAdvPoint.IsXStored: Boolean;
begin
  Result := X <> 0;
end;

function TAdvPoint.IsYStored: Boolean;
begin
  Result := Y <> 0;
end;

{$IFNDEF LIMITEDGRAPHICSMODE}

{ TAdvScaledBitmap }

procedure TAdvScaledBitmap.Assign(Source: TPersistent);
begin
  if Source is TAdvScaledBitmap then
  begin
    FBitmap.Assign((Source as TAdvScaledBitmap).Bitmap);
    FBitmapName := (Source as TAdvScaledBitmap).BitmapName;
    FScale := (Source as TAdvScaledBitmap).Scale;
  end;
end;

procedure TAdvScaledBitmap.BitmapChanged(Sender: TObject);
begin
  Changed(False);
end;

constructor TAdvScaledBitmap.Create(Collection: TCollection);
begin
  inherited;
  FBitmap := TAdvBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  FScale := 1.0;
end;

destructor TAdvScaledBitmap.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TAdvScaledBitmap.IsScaleStored: Boolean;
begin
  Result := Scale <> 1.0;
end;

procedure TAdvScaledBitmap.SetBitmap(const Value: TAdvBitmap);
begin
  if FBitmap <> Value then
  begin
    FBitmap.Assign(Value);
    Changed(False);
  end;
end;

procedure TAdvScaledBitmap.SetBitmapName(const Value: string);
begin
  if FBitmapName <> Value then
  begin
    FBitmapName := Value;
    Changed(False);
  end;
end;

procedure TAdvScaledBitmap.SetScale(const Value: Single);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    Changed(False);
  end;
end;

{ TAdvScaledBitmaps }

function TAdvScaledBitmaps.Add(Scale: Single = 1.0): TAdvScaledBitmap;
begin
  Result := TAdvScaledBitmap(inherited Add);
  Result.Scale := Scale;
end;

function TAdvScaledBitmaps.AddBitmap(Bitmap: TAdvBitmap; Scale: Single = 1.0): TAdvScaledBitmap;
begin
  Result := Add(Scale);
  Result.Bitmap.Assign(Bitmap);
end;

{$IFNDEF WEBLIB}
function TAdvScaledBitmaps.AddBitmapFromFile(FileName: string;
  Scale: Single): TAdvScaledBitmap;
begin
  Result := Add(Scale);
  Result.Bitmap.LoadFromFile(FileName);
end;
{$ENDIF}

function TAdvScaledBitmaps.AddBitmapFromResource(ResourceName: String; Scale: Single = 1.0): TAdvScaledBitmap;
begin
  {$IFDEF WEBLIB}
  Result := AddBitmapFromResource(ResourceName, 0, Scale);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := AddBitmapFromResource(ResourceName, HInstance, Scale);
  {$ENDIF}
end;

function TAdvScaledBitmaps.AddBitmapFromResource(ResourceName: String; AInstance: NativeUInt; Scale: Single = 1.0): TAdvScaledBitmap;
begin
  Result := Add(Scale);
  Result.Bitmap.LoadFromResource(ResourceName, AInstance);
end;

function TAdvScaledBitmaps.AddBitmapName(BitmapName: string; Scale: Single = 1.0): TAdvScaledBitmap;
begin
  Result := Add(Scale);
  Result.BitmapName := BitmapName;
end;

function TAdvScaledBitmaps.AddDrawBitmap(Bitmap: TAdvDrawBitmap;
  Scale: Single): TAdvScaledBitmap;
begin
  Result := Add(Scale);
  Result.Bitmap.Assign(Bitmap);
end;

constructor TAdvScaledBitmaps.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TAdvScaledBitmap);
end;

function TAdvScaledBitmaps.GetBitmap(Scale: Single): TAdvBitmap;
begin
  Result := GetBitmapByScale(Scale);
end;

function TAdvScaledBitmaps.GetBitmapByScale(
  Scale: Single): TAdvBitmap;
var
  it: TAdvScaledBitmap;
begin
  Result := nil;
  it := GetItemByScale(Scale);
  if Assigned(it) then
    Result := it.Bitmap;
end;

function TAdvScaledBitmaps.GetItemByScale(Scale: Single): TAdvScaledBitmap;
var
  I: Integer;
  cl: TAdvScaledBitmap;
  it: TAdvScaledBitmap;
  mx: Single;
begin
  Result := nil;
  cl := nil;
  mx := 0;
  for I := 0 to Count - 1 do
  begin
    it := Items[I];
    if it.Scale = Scale then
    begin
      Result := it;
      Break;
    end
    else if (it.Scale <> Scale) and (it.Scale > mx) then
    begin
      cl := it;
      mx := it.Scale;
    end;
  end;

  if Result = nil then
    Result := cl;
end;

function TAdvScaledBitmaps.GetItems(Index: Integer): TAdvScaledBitmap;
begin
  Result := TAdvScaledBitmap(inherited Items[Index]);
end;

function TAdvScaledBitmaps.Insert(Index: Integer;
  Scale: Single): TAdvScaledBitmap;
begin
  Result := TAdvScaledBitmap(inherited Insert(Index));
  Result.Scale := Scale;
end;

function TAdvScaledBitmaps.Insert(Index: integer): TAdvScaledBitmap;
begin
  Result := TAdvScaledBitmap(inherited Insert(Index));
end;

procedure TAdvScaledBitmaps.SetBitmap(Scale: Single;
  const Value: TAdvBitmap);
var
  b: TAdvScaledBitmap;
begin
  b := GetItemByScale(Scale);
  if not Assigned(b) then
    b := Add(Scale);

  b.Bitmap.Assign(Value);
end;

procedure TAdvScaledBitmaps.SetItems(Index: Integer; const Value: TAdvScaledBitmap);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvScaledBitmaps.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{$ENDIF}

{$IFDEF WEBLIB}
constructor TStringStream.Create(const AString: string);
begin
  inherited Create;
  FBytes := StringToByteArray(AString);
  FSize := Length(FBytes);
  FCapacity := FSize;
end;

function TStringStream.GetDataString: string;
var
  h: string;
begin
  h := ByteArrayToString(FBytes);
  if not Assigned(h) then
    Result := ''
  else
    Result := h;
end;

function TStringStream.ByteArrayToString(ABytes: TBytes): string;
var
  b: TBytes;
begin
  b := ABytes;

  asm
    function stringFromByteArray(data)
    {
      const extraByteMap = [ 1, 1, 1, 1, 2, 2, 3, 0 ];
      var count = data.length;
      var str = "";

      for (var index = 0;index < count;)
      {
        var ch = data[index++];
        if (ch & 0x80)
        {
          var extra = extraByteMap[(ch >> 3) & 0x07];
          if (!(ch & 0x40) || !extra || ((index + extra) > count))
            return null;

          ch = ch & (0x3F >> extra);
          for (;extra > 0;extra -= 1)
          {
            var chx = data[index++];
            if ((chx & 0xC0) != 0x80)
              return null;

            ch = (ch << 6) | (chx & 0x3F);
          }
        }

        str += String.fromCharCode(ch);
      }

      return str + "";
    }

    return stringFromByteArray(b);
  end;
end;

function TStringStream.StringToByteArray(AString: string): TBytes;
var
  s: string;
begin
  s := AString;
  asm
    function stringToByteArray(str) {
      var result = [];
      for (var i = 0; i < str.length; i++) {
        result.push(str.charCodeAt(i));
      }
      return result;
    }

    return stringToByteArray(s);
  end;
end;

function TStringStream.ReadString(Count: Integer): string;
var
  b: TBytes;
  I: Integer;
begin
  if Count > Size - Position then
    Count := Size - Position;

  SetLength(b, Count);
  for I := Position to Count + Position - 1 do
    b[I - Position] := FBytes[I];

  Result := ByteArrayToString(b);
  Position := Position + Count;
end;

procedure TStringStream.WriteString(const AString: string);
var
  LBytes: TBytes;
begin
  LBytes := StringToByteArray(AString);
  Write(LBytes, Length(LBytes));
end;

procedure TStream.SetSize(const NewSize: Int64);
var
  OldPosition: Longint;
begin
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then
    Position := NewSize;
end;

procedure TStream.SetCapacity(NewCapacity: NativeInt);
begin
  SetLength(FBytes, NewCapacity);
  FCapacity := NewCapacity;
end;

function TStream.CopyFrom(const Source: TStream; Count: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: TBytes;
begin
  if Count <= 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  SetLength(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer, N);
      WriteBuffer(Buffer, N);
      Dec(Count, N);
    end;
  finally
    SetLength(Buffer, 0);
  end;
end;

procedure TStream.ReadBuffer(var Buffer: TBytes; Count: NativeInt);
begin
  ReadBuffer(Buffer, 0, Count);
end;

procedure TStream.ReadBuffer(var Buffer: TBytes; Offset, Count: NativeInt);
var
  LTotalCount,
  LReadCount: NativeInt;
begin
  { Perform a read directly. Most of the time this will succeed
    without the need to go into the WHILE loop. }
  LTotalCount := Read(Buffer, Offset, Count);
  { Check if there was an error }
  if LTotalCount < 0 then
    raise EReadError.Create(SReadError);

  while (LTotalCount < Count) do
  begin
    { Try to read a contiguous block of <Count> size }
    LReadCount := Read(Buffer, Offset + LTotalCount, (Count - LTotalCount));

    { Check if we read something and decrease the number of bytes left to read }
    if LReadCount <= 0 then
      raise EReadError.Create(SReadError)
    else
      Inc(LTotalCount, LReadCount);
  end
end;

procedure TStream.WriteBuffer(const Buffer: TBytes; Count: NativeInt);
begin
  WriteBuffer(Buffer, 0, Count);
end;

procedure TStream.WriteBuffer(const Buffer: TBytes; Offset, Count: NativeInt);
var
  LTotalCount,
  LWrittenCount: NativeInt;
begin
  { Perform a write directly. Most of the time this will succeed
    without the need to go into the WHILE loop. }
  LTotalCount := Write(Buffer, Offset, Count);
  { Check if there was an error }
  if LTotalCount < 0 then
    raise EWriteError.Create(SWriteError);

  while (LTotalCount < Count) do
  begin
    { Try to write a contiguous block of <Count> size }
    LWrittenCount := Write(Buffer, Offset + LTotalCount, (Count - LTotalCount));

    { Check if we written something and decrease the number of bytes left to write}
    if LWrittenCount <= 0 then
      raise EWriteError.Create(SWriteError)
    else
      Inc(LTotalCount, LWrittenCount);
  end;
end;

function TStream.Read(var Buffer: TBytes; Count: Longint): Longint;
begin
  Result := Read(Buffer, 0, Count);
end;

function TStream.Read(var Buffer: TBytes; Offset, Count: Longint): Longint;
var
  I: Integer;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    if FSize > FPosition then
    begin
      if FSize > Count + FPosition then Result := Count
      else Result := FSize - FPosition;

      SetLength(Buffer, Min(Size, Count));

      for I := Offset + FPosition to Min(Size - 1, Offset + FPosition + Count - 1) do
        Buffer[I - Offset - FPosition] := FBytes[I];

      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

procedure TStream.SaveToStream(AStream: TStream);
begin
  if FSize <> 0 then
    AStream.WriteBuffer(FBytes, FSize);
end;

procedure TStream.LoadFromStream(AStream: TStream);
var
  Count: Int64;
begin
  AStream.Position := 0;
  Count := AStream.Size;
  SetSize(Count);
  if Count <> 0 then
    AStream.ReadBuffer(FBytes, Count);
end;

procedure TStream.SaveToFile(FileName: string);
var
  s: string;
  b: TBytes;
begin
  s := FileName;
  b := FBytes;
  asm
    var saveByteArray = (function () {
      var a = document.createElement("a");
      document.body.appendChild(a);
      a.style = "display: none";
      return function (data, name) {
          var blob = new Blob([new Uint8Array(data)], {type: "octet/stream"}),
            url = window.URL.createObjectURL(blob);
            a.href = url;
            a.download = name;
            a.click();
            window.URL.revokeObjectURL(url);
      };
    }());

    saveByteArray(b, s);
  end;
end;

procedure TStream.Clear;
begin
  SetLength(FBytes, 0);
  FCapacity := 0;
  FPosition := 0;
end;

function TStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
var
  b: TBytes;
  I: Integer;
begin
  Result := 0;
  if Length(Buffer) - Offset > 0 then
  begin
    SetLength(b, Length(Buffer) - Offset);
    for I := Offset to Length(Buffer) - 1 do
      b[i - Offset] := Buffer[I];

    Result := Write(b, Count);
  end;
end;

function TStream.Write(const Buffer: TBytes; Count: Longint): Longint;
var
  Pos: Int64;
  I: Integer;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      if Pos > FSize then
      begin
        if Pos > FCapacity then
          SetCapacity(Pos);
        FSize := Pos;
      end;

      for I := 0 to Length(Buffer) - 1 do
        FBytes[FPosition + I] := Buffer[I];

      FPosition := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;
{$ENDIF}

{$IFNDEF WEBLIB}

{ TAdvOwnedCollection<T>.TEnumerator }

constructor TAdvOwnedCollection{$IFNDEF LCLLIB}<T>{$ENDIF}.TEnumerator.Create(ACol: TAdvCollection);
begin
  inherited Create;
  FIndex := -1;
  FCol := ACol;
end;

function TAdvOwnedCollection{$IFNDEF LCLLIB}<T>{$ENDIF}.TEnumerator.GetCurrent: T;
begin
  Result := FCol.GetItem(FIndex);
end;

function TAdvOwnedCollection{$IFNDEF LCLLIB}<T>{$ENDIF}.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCol.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TAdvOwnedCollection<T> }

function TAdvOwnedCollection{$IFNDEF LCLLIB}<T>{$ENDIF}.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TAdvOwnedCollection{$IFNDEF LCLLIB}<T>{$ENDIF}.GetItem(AIndex: Integer): T;
begin
  Result := Items[AIndex] as T;
end;
{$ENDIF}

end.

