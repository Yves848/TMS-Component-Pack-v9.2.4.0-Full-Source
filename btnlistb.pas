{************************************************************************}
{ TBUTTONLISTBOX component                                               }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ Copyright © 2001-2016                                                  }
{   TMS Software                                                         }
{   Email : info@tmssoftware.com                                         }
{   Web : http://www.tmssoftware.com                                     }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit BtnListB;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, StdCtrls, Messages, Controls, SysUtils, Graphics, ExtCtrls,
  AsgDD, ActiveX, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type

  TOleDragStartEvent = procedure (Sender:TObject; DropIndex: integer) of object;
  TOleDragStopEvent =  procedure (Sender:TObject; OLEEffect: integer) of object;
  TOleDragOverEvent = procedure (Sender:TObject; var Allow:boolean) of object;
  TOleDropEvent = procedure (Sender:TObject; DropIndex: integer) of object;

  TPopupButton = class(TCustomControl)
  private
    FCaption: string;
    FFlat: boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure Paint; override;
  published
    property Caption: string read FCaption write fCaption;
    property Flat: Boolean read FFlat write fFlat;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TButtonListbox = class(TCustomListBox)
  private
    { Private declarations }
    FItemIndex: integer;
    FMoveButton: TPopupButton;
    FClickPos: TPoint;
    FMouseDown: boolean;
    FOleDropTargetAssigned: boolean;
    FOnOleDrop: TOleDropEvent;
    FOnOleDragStart: TOleDragStartEvent;
    FOnOleDragStop: TOleDragStopEvent;
    FOnOleDragOver: TOleDragOverEvent;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure SetItemIndexEx(const Value: integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    { Protected declarations }
    procedure DrawItem(Index: Integer; Rect: TRect;State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc(var Message:tMessage); override;
    procedure AddItemInt(s:string;idx:integer);
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex: integer read fItemIndex write SetItemIndexEx;
    property Items;
    property ParentCtl3D;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnOleDrop: TOleDropEvent read FOnOleDrop write FOnOleDrop;
    property OnOleDragStart: TOleDragStartEvent read FOnOleDragStart write FOnOleDragStart;
    property OnOleDragStop: TOleDragStopEvent read FOnOleDragStop write FOnOleDragStop;
    property OnOleDragOver: TOleDragOverEvent read FOnOleDragOver write FOnOleDragOver;
    property Version: string read GetVersion write SetVersion;
  end;

  TListDropTarget = class(TASGDropTarget)
  private
    FList:TButtonListBox;
  public
    constructor Create(AList: TButtonListBox);
    procedure DropText(pt:TPoint; s:string); override;
    procedure DropCol(pt:TPoint; col:integer); override;
    procedure DropStream(pt: TPoint; ms: TMemoryStream); override;
    procedure DropRTF(pt:TPoint;s:string); override;
    procedure DropFiles(pt:TPoint; files: TStrings); override;
    procedure DropURL(pt:TPoint; s:string); override;
    procedure DragMouseMove(pt:TPoint; var Allow:boolean; DropFormats:TDropFormats); override;
    procedure DragMouseEnter; override;
    procedure DragMouseLeave; override;
  end;

  TListDropSource = class(TASGDropSource)
  private
    FList: TButtonListBox;
    FLastEffect: integer;
  public
    constructor Create(AList: TButtonListBox);
    procedure CurrentEffect(dwEffect: Longint); override;
    procedure QueryDrag; override;
    property LastEffect: integer read FLastEffect;
  end;

implementation

{$IFDEF DELPHIXE2_LVL}
uses
  VCL.Themes;
{$ENDIF}

const
  Effect3DSize = 3;

procedure TButtonListbox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);

    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State);
    Canvas.Handle := 0;
  end;
end;


procedure TButtonListbox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  usevclstyles: boolean;
  LColor: TColor;
  {$IFDEF DELPHIXE2_LVL}
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  SaveIndex: integer;
  clr: TColor;
  {$ENDIF}

begin
  usevclstyles := false;
  LColor := clWindowText;

  {$IFDEF DELPHIXE2_LVL}
  LStyle := StyleServices;
  if LStyle.Enabled and (LStyle.Name <> 'Windows') then
  begin
    usevclstyles := true;

    SaveIndex := SaveDC(Canvas.Handle);
    try
      LDetails := LStyle.GetElementDetails(tgFixedCellNormal);
      if LStyle.GetElementColor(LDetails, ecTextColor, clr) and (clr <> clNone) then
        LColor := clr;

      LStyle.DrawElement(Canvas.Handle, LDetails, rect);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
  end;
  {$ENDIF}

  if not usevclstyles then
  begin
    if Index = fItemIndex then
      Canvas.Brush.Color:=clGray
    else
      Canvas.Brush.Color:=clBtnFace;

    Canvas.FillRect(Rect);
    Frame3D(canvas,rect,clWhite,clGray,1);
  end;

  rect.left := rect.left + 2;
  rect.top := rect.top + 2;

  if (Index >= 0) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := LColor;
    DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]),rect,DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TButtonListbox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := ItemHeight;
end;

constructor TButtonListBox.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  Style := lbOwnerDrawVariable;
  FItemIndex := -1;
  FMouseDown := false;

  if not (csDesigning in ComponentState) then
  begin
    FMoveButton := TPopupButton.Create(Self);
    FMoveButton.Parent := Self;
    FMoveButton.Enabled := false;
    FMoveButton.Visible := false;
  end;
end;

procedure TButtonListbox.CreateWnd;
begin
  inherited;

  if not (csDesigning in ComponentState) then
   begin
     FOleDropTargetAssigned := RegisterDragDrop(Handle, TListDropTarget.Create(Self) ) = s_OK;
   end;
end;

procedure TButtonListbox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ItemIndex := loword(Sendmessage(Handle,LB_ITEMFROMPOINT,0,makelparam(X,Y)));
  FClickPos := Point(X,Y);
  FMouseDown := true;
end;

procedure TButtonListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FMouseDown := false;
  FMoveButton.Visible := false;
end;


procedure TButtonListbox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Idx, Col: Integer;
  DropSource: TListDropSource;
  dwEffects: Integer;
  pt : TPoint;

begin
  inherited;

  Idx := loword(SendMessage(Handle,LB_ITEMFROMPOINT,0,makelparam(X,Y)));

  if ((abs(FClickPos.X-X) > 3) or (abs(FClickPos.Y-Y) > 3))
      and FMouseDown and (Idx>=0) and (Items.Count>0) then
  begin
    FMoveButton.Caption := Items[Idx];

    Col := integer(Items.Objects[idx]);

    pt := ClientToScreen(point(x,y));

    FMoveButton.Top := pt.y;
    FMoveButton.Left := pt.x;

    FMoveButton.Width := Width;
    FMoveButton.Height := ItemHeight;
    FMoveButton.Visible := true;

    if Assigned(FOnOleDragStart) then
      FOnOleDragStart( Self, Col);

    DropSource := TListDropSource.Create(Self);

    StartColDoDragDrop(DropSource, Col, DROPEFFECT_COPY or DROPEFFECT_MOVE, dwEffects);

    if Assigned(FOnOleDragStop) then
      FOnOleDragStop( Self, dwEffects);

    FMoveButton.Visible := false;
    FMouseDown := false;
  end;
end;


procedure TButtonListbox.SetItemIndexEx(const Value: integer);
var
  R:TRect;
begin
  if fItemIndex<>Value then
  begin
    if fItemIndex>=0 then
    begin
      SendMessage(Handle,lb_getitemrect,fItemIndex,LParam(@r));
      InvalidateRect(Handle,@r,TRUE);
    end;

    fItemIndex := Value;

    if fItemIndex >= 0 then
    begin
      SendMessage(Handle,lb_getitemrect,fItemIndex,LParam(@r));
      InvalidateRect(Handle,@r,TRUE);
    end;
  end;
end;

destructor TButtonListbox.Destroy;
begin
  if not (csDesigning in ComponentState) then
    FMoveButton.Free;

  inherited;
end;

function TButtonListbox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TButtonListbox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TButtonListbox.SetVersion(const Value: string);
begin

end;

{ TPopupButton }

procedure TPopupButton.CreateParams(var Params: TCreateParams);
begin
  inherited;

  with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;

  Color := clBtnFace;
end;


procedure TPopupButton.Paint;
var
  r: TRect;
  usevclstyles: boolean;
  {$IFDEF DELPHIXE2_LVL}
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  SaveIndex: integer;
  {$ENDIF}
begin
  r := GetClientRect;
  usevclstyles := false;

  {$IFDEF DELPHIXE2_LVL}
  LStyle := StyleServices;
  if LStyle.Enabled and (LStyle.Name <> 'Windows') then
  begin
    usevclstyles := true;

    SaveIndex := SaveDC(Canvas.Handle);
    try
      LDetails := LStyle.GetElementDetails(tgFixedCellNormal);

      LStyle.DrawElement(Canvas.Handle, LDetails, r);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
  end;
  {$ENDIF}

  if not usevclstyles and not FFlat then
    Frame3D(canvas,r,clWhite,clGray,1);

  SetBkMode(Canvas.Handle, TRANSPARENT);
  DrawTextEx(Canvas.Handle, PChar(FCaption), Length(FCaption),r,DT_CENTER or DT_END_ELLIPSIS,nil);
end;


{ TListDropTarget }

constructor TListDropTarget.Create(aList: TButtonListBox);
begin
  inherited Create;
  FList := AList;
end;

procedure TListDropTarget.DragMouseEnter;
begin
  inherited;
end;

procedure TListDropTarget.DragMouseLeave;
begin
  inherited;
end;

procedure TListDropTarget.DragMouseMove(pt: TPoint; var Allow: boolean; DropFormats:TDropFormats);
begin
  inherited;

  Allow := dfCol in DropFormats;

  if Assigned(FList.FOnOleDragOver) then
    FList.FOnOleDragOver(FList, Allow);
end;

procedure TListDropTarget.DropCol(pt: TPoint; col: integer);
begin
  inherited;
  if Assigned(FList.OnOleDrop) then
    FList.OnOleDrop(FList,col);
end;

procedure TListDropTarget.DropFiles(pt: TPoint; files: TStrings);
begin
  inherited;
end;

procedure TListDropTarget.DropRTF(pt: TPoint; s: string);
begin
  inherited;
end;

procedure TListDropTarget.DropStream(pt: TPoint; ms: TMemoryStream);
begin
  inherited;
end;

procedure TListDropTarget.DropText(pt: TPoint; s: string);
begin
  inherited;
end;

procedure TListDropTarget.DropURL(pt: TPoint; s: string);
begin
  inherited;
end;

{ TListDropSource }

constructor TListDropSource.Create(AList: TButtonListBox);
begin
  inherited Create;
  FList := AList;
end;

procedure TListDropSource.CurrentEffect(dwEffect: Integer);
begin
  inherited;
end;

procedure TListDropSource.QueryDrag;
var
 pt: TPoint;
begin
  inherited;
  GetCursorPos(pt);

  FList.FMoveButton.Left := pt.x;
  FList.FMoveButton.Top := pt.y - FList.FMoveButton.Height;
end;

procedure TButtonListbox.Loaded;
begin
  inherited;
end;

procedure TButtonListbox.WndProc(var Message: tMessage);
begin
  inherited;
  if (message.msg = WM_DESTROY) then
  begin
    if FOleDropTargetAssigned then
      RevokeDragDrop(Handle);
  end;
end;

procedure TButtonListbox.AddItemInt(s: string; idx: integer);
begin
  Items.AddObject(s,TObject(idx));
end;


end.
