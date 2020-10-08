{*************************************************************************}
{ TMS TAdvScrollControl                                                   }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2018                                       }
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

unit AdvScrollControl;

{$I TMSDEFS.INC}

{$WARN UNSUPPORTED_CONSTRUCT OFF}

interface

uses
  Classes, Messages, Forms, Graphics, AdvGraphics, AdvGraphicsTypes,
  Controls, SysUtils, Types
  {$IFDEF VCLLIB}
  , uxTheme, Windows
  {$ENDIF}
  {$IFDEF DELPHIXE5_LVL}
  , VCL.Themes
  {$ENDIF}
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  , Win32Proc, Win32Extra, JwaWinUser
  {$ENDIF}
  , LCLType, LMessages
  {$ENDIF}
  {$IFDEF FNCLIB}
  {$IFDEF VCLLIB}
  , VCL.TMSFNCTypes
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.TMSFNCTypes
  {$ENDIF}
  {$ENDIF}
  ;

const
   TTMSFNCScrollControlDocURL = 'http://www.tmssoftware.biz/Download/Manuals/TMSFNCUIPackDevGuide.pdf';
   TTMSFNCScrollControlTipsURL = 'http://www.tmssoftware.com/site/tmsfncuipack.asp?s=faq';


type
  TRectF = record
    Left,Top, Bottom, Right, Width,Height: single;
  end;

  {$IFDEF DELPHIXE8_LVL}
  TCustomScrollingStyleHook = class(TScrollingStyleHook)
  strict protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;
  {$ENDIF}

  TCustomScrollingControl = class(TScrollingWinControl {$IFDEF FNCLIB} ,ITMSFNCProductInfo {$ENDIF})
  private
    {$IFDEF FNCLIB}
    FAdaptToStyle: boolean;
    {$ENDIF}
    FNHCanvas: TCanvas;
    FBorderStyle: TBorderStyle;
    FUseVCLStyles: boolean;
    FBorderColor: TColor;
    FRange: TSize;
    FAllowFocus: Boolean;
    FReadOnly: boolean;
    {$IFDEF LCLLIB}
    FCtl3D: boolean;
    {$ENDIF}
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMVScroll); message WM_HSCROLL;
    function GetScrollPosition: TPoint;
    procedure SetScrollPosition(const Value: TPoint);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure DrawBorders;
    function GetHasHorizontalScrollBar: boolean;
    function GetHasVerticalScrollBar: boolean;
  protected
    {$IFDEF FNCLIB}
    function GetAdaptToStyle: boolean; virtual;
    procedure SetAdaptToStyle(const Value: boolean); virtual;
    {$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDownN(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure MouseMoveN(Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure MouseUpN(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    {$IFDEF VCLLIB}
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    {$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDownN(var Key: Word; Shift: TShiftState); virtual; abstract;
    procedure KeyUpN(var Key: Word; Shift: TShiftState); virtual; abstract;
    procedure KeyPressN(var Key: Char); virtual; abstract;
    procedure Paint; virtual; abstract;
    procedure ScrollHorz(const X: integer);
    procedure ScrollVert(const Y: integer);
    function ScrollUp(const Delta: integer): boolean;
    function ScrollDown(const Delta: integer): boolean;
    procedure TopLeftChanged; virtual;
    procedure DoDblClick(X, Y: integer); virtual;
    function BorderSize: integer; virtual;
    function VScrollWidth: integer; virtual;
    function HScrollHeight: integer; virtual;
    function PageUp: boolean;
    function PageDown: boolean;
    function ScrollHome: boolean;
    function ScrollEnd: boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    function ScrollSizeVert: integer;
    function ScrollSizeHorz: integer;
    function SetRange(ARange: TSize): boolean; virtual;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property UseVCLStyles: boolean read FUseVCLStyles write FUseVCLStyles;
    function GetClientWidth: integer; virtual; abstract;
    function GetClientHeight: integer; virtual; abstract;
    procedure Backspace; virtual; abstract;
    procedure UpdateSize; virtual; abstract;
    function HasSelection: boolean; virtual; abstract;
    function GetElementCount: integer; virtual; abstract;
    procedure SelectAll; virtual; abstract;
    function SelectWordAtCaret: string; virtual; abstract;
    procedure GetWordAndIndexAtCaret(var AValue: string; var AIndex: integer; SpaceOnly: boolean = false); virtual; abstract;
    procedure UpdateWordAndIndexAtCaret(AValue: string; AIndex: integer; SpaceOnly: boolean = false); virtual; abstract;
    function SelectedText: string; virtual; abstract;
    procedure UpdateSelectionPoint(LeftSel: boolean; var X, Y: integer); virtual; abstract;
    function IsForwardSelection: boolean; virtual; abstract;
    procedure InsertChar(ch: char); overload; virtual; abstract;
    procedure InsertChar(value: string); overload; virtual; abstract;
    procedure DoSelectionChanged; virtual; abstract;
    procedure UpdateSelection; virtual; abstract;
    procedure DeleteSelection; virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    function GetSelectionFromXY: TPoint; virtual; abstract;
    function GetSelectionToXY: TPoint; virtual; abstract;
    function GetCaretXY: TPoint; virtual; abstract;
    function GetCaretLH: integer; virtual; abstract;
    function HasTextContent: boolean; virtual;
    procedure UpdateTextService;
    procedure CreateTextService;
    function GetVersion: string; virtual;
    function GetDocURL: string; virtual;
    function GetTipsURL: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: boolean; override;
    procedure PaintWindow(DC: HDC); override;
    property Canvas: TCanvas read FNHCanvas;
    property TopLeft: TPoint read GetScrollPosition write SetScrollPosition;
    {$IFDEF LCLLIB}
    property Ctl3D: boolean read FCtl3D write FCtl3D;
    {$ENDIF}
    property HasVerticalScrollBar: boolean read GetHasVerticalScrollBar;
    property HasHorizontalScrollBar: boolean read GetHasHorizontalScrollBar;
    procedure CopyToClipboard; virtual; abstract;
    procedure CutToClipboard; virtual; abstract;
    function PasteFromClipboard: string; virtual; abstract;
  published
    {$IFDEF FNCLIB}
    property AdaptToStyle: boolean read GetAdaptToStyle write SetAdaptToStyle default False;
    {$ENDIF}
    property Align;
    property Anchors;
    property AllowFocus: Boolean read FAllowFocus write FAllowFocus default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Font;
    property ShowHint;
    property ReadOnly: boolean read FReadOnly write FReadOnly default false;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

implementation

uses
  Math
  {$IFDEF FNCLIB}
  {$IFDEF FREEWARE}
  ,VCL.ExtCtrls, VCL.StdCtrls, System.Win.Registry, WinApi.ShellApi
  {$ENDIF}
  {$ENDIF}
  ;

const
  SCROLL_DISTANCE = 20;

{ TCustomScrollingControl }

function TCustomScrollingControl.BorderSize: integer;
begin
  Result := 0;
end;

function TCustomScrollingControl.CanFocus: boolean;
begin
  Result := inherited CanFocus;
  Result := Result and AllowFocus;
end;

constructor TCustomScrollingControl.Create(AOwner: TComponent);
begin
  inherited;

  //DoubleBuffered := true;

  FNHCanvas := TControlCanvas.Create;
  TControlCanvas(FNHCanvas).Control := Self;

  VertScrollBar.Tracking := true;
  HorzScrollBar.Tracking := true;
  FBorderStyle := bsSingle;
  FBorderColor := clNone;
  FUseVCLStyles := false;
  FAllowFocus := True;
  FReadOnly := False;

  Ctl3D := true;

  ControlStyle := ControlStyle + [csOpaque];

  {$IFDEF FNCLIB}
  FAdaptToStyle := false;
  {$ENDIF}
end;

procedure TCustomScrollingControl.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      if BorderStyle = bsNone then
        Style := Style and not WS_BORDER
      else
        Style := Style or WS_BORDER;
    end;

    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);

  end;
end;

procedure TCustomScrollingControl.CreateTextService;
begin
  //
end;

destructor TCustomScrollingControl.Destroy;
begin
  FNHCanvas.Free;
  inherited;
end;

procedure TCustomScrollingControl.DoDblClick(X, Y: integer);
begin
  //
end;

{$IFDEF FNCLIB}
function TCustomScrollingControl.GetAdaptToStyle: boolean;
begin
  Result := FAdaptToStyle;
end;
{$ENDIF}

function TCustomScrollingControl.GetDocURL: string;
begin
  Result := TTMSFNCScrollControlDocURL;
end;

function TCustomScrollingControl.GetHasHorizontalScrollBar: boolean;
begin
  Result := HorzScrollBar.IsScrollBarVisible;
end;

function TCustomScrollingControl.GetHasVerticalScrollBar: boolean;
begin
  Result := VertScrollBar.IsScrollBarVisible;
end;

function TCustomScrollingControl.GetScrollPosition: TPoint;
begin
  Result := Point(HorzScrollBar.ScrollPos, VertScrollBar.ScrollPos);
end;

function TCustomScrollingControl.GetTipsURL: string;
begin
  Result := TTMSFNCScrollControlTipsURL;
end;

function TCustomScrollingControl.GetVersion: string;
begin
  //
end;

procedure TCustomScrollingControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  KeyDownN(Key, Shift);
end;

procedure TCustomScrollingControl.KeyPress(var Key: Char);
begin
  inherited;
  KeyPressN(Key);
end;

procedure TCustomScrollingControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  KeyUpN(Key, Shift);
end;

procedure TCustomScrollingControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MouseDownN(Button, Shift, X, Y);
end;

procedure TCustomScrollingControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MouseMoveN(Shift, X, Y);
end;

procedure TCustomScrollingControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MouseUpN(Button, Shift, X, Y);
end;

{$IFDEF FMXLIB}
procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
begin
  if WheelDelta > 0 then
    ScrollDown(200)
  else
    ScrollUp(200);

  Handled := true;
end;
{$ENDIF}

{$IFDEF VCLLIB}
function TCustomScrollingControl.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  ScrollDown(200);
  Result := false;
end;

function TCustomScrollingControl.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  ScrollUp(200);
  Result := false;
end;
{$ENDIF}

function TCustomScrollingControl.PageDown: boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  if VertScrollBar.Range > Height then
    TopLeft := Point(TopLeft.X, TopLeft.Y + Height);
  Result := TopLeft.Y <> OldTopLeft.Y;
end;

function TCustomScrollingControl.PageUp: boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  TopLeft := Point(TopLeft.X, TopLeft.Y - Height);
  Result := TopLeft.Y <> OldTopLeft.Y;
end;

procedure TCustomScrollingControl.PaintWindow(DC: HDC);
begin
  FNHCanvas.Handle := DC;
  try
    Paint;
  finally
    FNHCanvas.Handle := 0;
  end;
end;

function TCustomScrollingControl.ScrollDown(const Delta: integer): boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  TopLeft := Point(TopLeft.X, TopLeft.Y + Delta);
  Result:= TopLeft.Y <> OldTopLeft.Y;
end;

function TCustomScrollingControl.ScrollEnd: boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  TopLeft := Point(0, VertScrollBar.Range - Height);
  Result:= TopLeft.Y <> OldTopLeft.Y;
end;

function TCustomScrollingControl.ScrollHome: boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  TopLeft := Point(0,0);
  Result:= TopLeft.Y <> OldTopLeft.Y;
end;

procedure TCustomScrollingControl.ScrollHorz(const X: integer);
begin
  TopLeft := Point(X, TopLeft.Y);
end;

function TCustomScrollingControl.ScrollSizeHorz: integer;
{$IFDEF VCLLIB}
var
  sbinfo: TSCROLLBARINFO;
{$ENDIF}
begin
  Result := 0;
{$IFDEF VCLLIB}
  if HandleAllocated then
  begin
    sbinfo.cbSize := Sizeof(TSCROLLBARINFO);
    GetScrollBarInfo(Handle, integer(OBJID_HSCROLL), sbinfo);
    if (sbinfo.rgstate[0] and STATE_SYSTEM_INVISIBLE = 0) then
      Result := GetSystemMetrics(SM_CYHSCROLL);
  end;
{$ENDIF}
end;

function TCustomScrollingControl.ScrollSizeVert: integer;
{$IFDEF VCLLIB}
var
  sbinfo: TSCROLLBARINFO;
{$ENDIF}
begin
  Result := 0;
{$IFDEF VCLLIB}
  if HandleAllocated then
  begin
    sbinfo.cbSize := Sizeof(TSCROLLBARINFO);
    GetScrollBarInfo(Handle, integer(OBJID_VSCROLL), sbinfo);
    if (sbinfo.rgstate[0] and STATE_SYSTEM_INVISIBLE = 0) then
      Result := GetSystemMetrics(SM_CXVSCROLL);
  end;
{$ENDIF}
end;

function TCustomScrollingControl.ScrollUp(const Delta: integer): boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  TopLeft := Point(TopLeft.X, TopLeft.Y - Delta);
  Result := TopLeft.Y <> OldTopLeft.Y;
end;

procedure TCustomScrollingControl.ScrollVert(const Y: integer);
begin
  TopLeft := Point(TopLeft.X, Y);
end;

{$IFDEF FNCLIB}
procedure TCustomScrollingControl.SetAdaptToStyle(const Value: boolean);
begin
  FAdaptToStyle := Value;
end;
{$ENDIF}

procedure TCustomScrollingControl.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

function TCustomScrollingControl.SetRange(ARange: TSize): boolean;
var
  vs: integer;
begin
  {$IFNDEF FMXLIB}
  Result := false;
  if not HandleAllocated then
    Exit;
  {$ENDIF}

  vs := ScrollSizeVert;
  if (FRange.cx <> ARange.cx) or (FRange.cy <> ARange.cy) then
  begin
    HorzScrollBar.Range := ARange.cx;
    VertScrollBar.Range := ARange.cy;
  end;
  FRange := ARange;
  Result := (vs <> ScrollSizeVert);
end;

procedure TCustomScrollingControl.SetScrollPosition(const Value: TPoint);
begin
  {$IFNDEF FMXLIB}
  if not HandleAllocated then
    Exit;
  {$ENDIF}
  HorzScrollBar.Position := Max(0, Min(Value.X, HorzScrollBar.Range));
  VertScrollBar.Position := Max(0, Min(Value.Y, VertScrollBar.Range));
end;

procedure TCustomScrollingControl.TopLeftChanged;
begin
 //
end;

procedure TCustomScrollingControl.UpdateTextService;
begin
  //
end;

function TCustomScrollingControl.HasTextContent: boolean;
begin
  Result := true;
end;

function TCustomScrollingControl.HScrollHeight: integer;
begin
  Result := 0;
end;

function TCustomScrollingControl.VScrollWidth: integer;
begin
  Result := 0;
end;

procedure TCustomScrollingControl.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TCustomScrollingControl.WMHScroll(var Message: TWMVScroll);
begin
  inherited;
  Invalidate;
  TopLeftChanged;
end;

procedure TCustomScrollingControl.DrawBorders;
{$IFDEF VCLLIB}
var
  DC: HDC;
  OldPen: HPen;
  ARect: TRect;
  hTheme: THandle;
  clr: TColor;
  opts: TDTBGOPTS;
{$ENDIF}
begin
  {$IFDEF VCLLIB}
  DC := GetWindowDC(Handle);

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);

    if IsThemeActive and not UseVCLStyles and (BorderColor = clNone) then
    begin
      htheme := OpenThemeData(Handle,'COMBOBOX');
      opts.dwSize := Sizeof(opts);
      opts.dwFlags := DTBG_OMITCONTENT;
      DrawThemeBackgroundEx(hTheme, DC, 0, 0, ARect, @opts);
      CloseThemeData(htheme);
    end
    else
    begin
      if BorderColor = clNone then
        clr := clGray
      else
        clr := BorderColor;

      OldPen := SelectObject(DC,CreatePen(PS_SOLID,1,ColorToRGB(clr)));

      MovetoEx(DC,ARect.Left ,ARect.Top ,nil);
      LineTo(DC,ARect.Right -1 ,ARect.Top );
      LineTo(DC,ARect.Right -1 ,ARect.Bottom - 1);
      LineTo(DC,ARect.Left,ARect.Bottom -1 );
      LineTo(DC,ARect.Left,ARect.Top );

      DeleteObject(SelectObject(DC,OldPen));
    end;
  finally
    ReleaseDC(Handle,DC);
  end;
  {$ENDIF}
end;

procedure TCustomScrollingControl.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if BorderStyle = bsSingle then
    DrawBorders;
end;

procedure TCustomScrollingControl.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TCustomScrollingControl.WMVScroll(var Message: TWMVScroll);
begin
  {$IFDEF VCLLIB}
  SendMessage(Handle, WM_SETREDRAW, 0, 0);
  {$ENDIF}
  inherited;
  TopLeftChanged;
  {$IFDEF VCLLIB}
  SendMessage(Handle, WM_SETREDRAW, 1, 0);
  {$ENDIF}
  Invalidate;
end;

{$IFDEF DELPHIXE8_LVL}

{ TCustomScrollingStyleHook }

type
  TWinControlClassHook = class(TWinControl);

constructor TCustomScrollingStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverrideEraseBkgnd := True;
  if seClient in Control.StyleElements then
    Brush.Color := StyleServices.GetStyleColor(scPanel)
  else
    Brush.Color :=  TWinControlClassHook(Control).Color;
end;

procedure TCustomScrollingStyleHook.WndProc(var Message: TMessage);
begin
  // Reserved for potential updates
  inherited;
end;
{$ENDIF}

{$IFNDEF FNCLIB}
{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}
{$ENDIF}

{$IFDEF FNCLIB}
{$IFDEF VCLLIB}
{$IFDEF FREEWARE}
{$I TMSProductTrial.inc}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFNDEF FREEWARE}
{$IFDEF VCLLIB}
initialization
{$ENDIF}
{$ENDIF}

{$IFDEF VCLLIB}
{$IFDEF DELPHIXE8_LVL}

  TCustomStyleEngine.RegisterStyleHook(TCustomScrollingControl, TCustomScrollingStyleHook);

finalization
  TCustomStyleEngine.UnRegisterStyleHook(TCustomScrollingControl, TCustomScrollingStyleHook);

{$ENDIF}
{$ENDIF}


end.
