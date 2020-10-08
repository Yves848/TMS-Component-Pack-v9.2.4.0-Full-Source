{********************************************************************}
{ TPICKDIALOG component                                              }
{ for Delphi & C++ Builder                                           }
{                                                                    }
{ written by                                                         }
{          TMS Software                                              }
{          copyright © 1998 - 2019                                   }
{          Email : info@tmssoftware.com                              }
{          Website : https://www.tmssoftware.com                     }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit PickDlg;

{$I TMSDEFS.INC}
interface

// TODO : EVENTS

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CheckLst;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 7; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.5.0.1 : Fixed issue to copy also objects to selectlist
  // v1.6.0.0 : New : Font, ListFont property added
  // v1.6.1.0 : New : Support for multiline title
  // v1.7.0.0 : New : ListType property added to select between listbox & checklistbox
  // v1.7.1.0 : Improved: Support for High DPI: form size and text height

type
  TButtonPosition = (bpBottom,bpRight,bpNone);

  TListType = (ltList, ltCheckList);

  TDialogPosition = (fposCenter,fposAbsolute,fposDefault);

  TClickItemEvent = procedure(Sender:TObject;index:integer;itemstr:string) of object;
  TDblClickItemEvent = procedure(Sender:TObject;index:integer;itemstr:string) of object;

  TPickDialog = class;

  TSelectForm = class(TForm)
    SelectList: TListBox;
    okbtn: TButton;
    cancelbtn: TButton;
    title: TLabel;
    CheckSelectList: TCheckListBox;
    procedure OkbtnClick(Sender: TObject);
    procedure CancelbtnClick(Sender: TObject);
    procedure SelectListDblClick(Sender: TObject);
    procedure SelectListClick(Sender: TObject);
  private
    FParentcontrol: TPickDialog;
    { Private declarations }
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message wm_EraseBkGnd;
    procedure WMSize(var Message: TWMSize); message wm_size;
    procedure WMNCHitTest(var Message: TMessage); message wm_nchittest;
    procedure WMMinMaxInfo(var Message: TMessage); message wm_getminmaxinfo;
  public
    cresizex,cresizey: Integer;
    fbuttonpos: TButtonPosition;
    acceptdblclick: boolean;
    { Public declarations }
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPickDialog = class(TComponent)
  private
    FCaption:string;
    FTitle:string;
    FPickList:TStringlist;
    FSelectIndex:integer;
    FSelectString:string;
    FSelectList:tstringlist;
    FButtonpos:tbuttonposition;
    FMultisel,fsort,fshowtitle:boolean;
    FDialogPosition:TDialogPosition;
    FDblClick:boolean;
    FHeight: integer;
    FWidth: integer;
    FTopPosition: integer;
    FLeftPosition: integer;
    FCancelCaption:string;
    FOkCaption:string;
    FSelectData:tObject;
    FSizeable:boolean;
    FToolWindow:boolean;
    FSelectForm:tSelectForm;
    FCount:integer;
    FOnClickItem: TClickItemEvent;
    FOnDblClickItem: TDblClickItemEvent;
    FListFont: TFont;
    FFont: TFont;
    FListType: TListType;
    FDPIScale: single;
    procedure SetPickList(value: tstringlist);
    procedure SetHeight(value:integer);
    procedure SetWidth(value:integer);
    procedure CreateSelect;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetListFont(const Value: TFont);
  protected
    function GetVersionNr: Integer; virtual;
  public
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    function Execute:integer;
    procedure Show;
    procedure Hide;
    property SelectIndex:integer read FSelectIndex write fSelectIndex;
    property SelectString:string read FSelectString;
    property SelectData: TObject read FSelectData;
    property SelectList: TStringList read FSelectList;
  published
    property Caption:string read fCaption write fCaption;
    property AcceptDblClick: Boolean read fDblClick write fDblClick;
    property ButtonPosition: TButtonPosition read fButtonpos write fButtonpos;
    property CancelCaption:string read fCancelCaption write fCancelCaption;
    property DialogPosition:TDialogPosition read fDialogPosition write fDialogPosition;
    property Font: TFont read FFont write SetFont;
    property Height:integer read fHeight write SetHeight;
    property LeftPosition:integer read fLeftPosition write fLeftPosition;
    property ListFont: TFont read FListFont write SetListFont;
    property ListType: TListType read FListType write FListType default ltList;
    property MultiSel:boolean read fMultisel write fMultisel;
    property OkCaption:string read fOkCaption write fOkCaption;
    property PickItems:tStringlist read fPickList write SetPickList;
    property Sizeable:boolean read fSizeAble write fSizeAble;
    property ShowTitle: Boolean read fShowTitle write fShowTitle;
    property Sort: Boolean read fSort write fSort;
    property Title:string read fTitle write fTitle;
    property ToolWindow:boolean read fToolWindow write fToolWindow;
    property TopPosition:integer read fTopPosition write fTopPosition;
    property Version: string read GetVersion write SetVersion;
    property Width:integer read fWidth write SetWidth;
    property OnClickItem: TClickItemEvent read FOnClickItem write FOnClickItem;
    property OnDblClickItem: TDblClickItemEvent read FOnDblClickItem write FOnDblClickItem;
  end;


implementation

{$R *.DFM}

uses
  AdvStyleIF;

{ TPickDialog }

constructor TPickDialog.Create(aOwner: tComponent);
begin
  inherited Create(aOwner);
  FPickList := TStringlist.Create;
  FSelectList := TStringlist.Create;
  FFont := TFont.Create;
  FListFont := TFont.Create;
  FSelectIndex := -1;
  FSelectString := '';
  FWidth := 280;
  FHeight := 270;
  FCancelCaption := 'Cancel';
  FOkCaption := 'OK';
  FCount := 0;
  FListType := ltList;
end;

procedure TPickDialog.SetPickList(value: TStringList);
begin
  if Assigned(Value) then
    FPicklist.Assign(value);
end;

procedure TPickDialog.CreateSelect;
var
  r:TRect;
begin
  if FCount > 0 then
    Exit;

  if (csDesigning in ComponentState) then
    FSelectForm := TSelectForm.Create(Application)
  else
    FSelectForm := TSelectForm.Create(Owner);

  FDPIScale := GetDPIScale(FSelectForm, FSelectForm.Canvas);

  FSelectForm.Width:= Round(FDPIScale * fwidth);
  FSelectForm.Height:= Round(FDPIScale * fHeight);
  FSelectForm.CreSizeX:=Round(FDPIScale * fWidth);
  FSelectForm.CreSizeY:=Round(FDPIScale * fHeight);
  FSelectForm.acceptdblclick := fDblClick;
  FSelectForm.Fbuttonpos := fbuttonpos;
  FSelectForm.FParentControl:= self;

  case fDialogPosition of
  fposCenter:fSelectForm.Position:=poScreenCenter;
  fposDefault:fSelectForm.Position:=poDefaultPosOnly;
  fposAbsolute:
  begin
    fSelectForm.Top:=fTopPosition;
    fSelectForm.Left:=fLeftPosition;
    fSelectForm.Position:=poDesigned;
  end;
  end;

  fSelectForm.BorderIcons := [biSystemMenu];

  if FToolwindow then
  begin
    if FSizeable then
      FSelectForm.BorderStyle := bsSizeToolWin
    else
      FSelectForm.BorderStyle := bsToolWindow;
  end
 else
  begin
    if FSizeable then
      FSelectForm.BorderStyle := bsSizeable
    else
      FSelectForm.BorderStyle := bsSingle;
  end;

  r := FSelectform.GetClientRect;
  FSelectForm.Font.Assign(FFont);
  FSelectForm.Font.Height := Round(FDPIScale * FFont.Height);

  with FSelectForm do
  begin
    SelectList.Items.AddStrings(FPickList);
    CheckSelectList.Items.AddStrings(FPickList);
    Caption := FCaption;
    Okbtn.Caption := fOkCaption;
    Cancelbtn.Caption := fCancelCaption;
    Title.Caption := FTitle;
    Title.AutoSize := true;
    SelectList.MultiSelect := FMultisel;
    SelectList.Sorted := FSort;
    SelectList.Itemindex := -1;
    SelectList.Font.Assign(FListFont);
    SelectList.Font.Height := Round(FDPIScale * FListFont.Height);

    CheckSelectList.Font.Assign(FListFont);
    CheckSelectList.Font.Height := Round(FDPIScale * FListFont.Height);
    CheckSelectList.Sorted := FSort;
    CheckSelectList.ItemIndex := 0;

    if (SelectList.Items.Count > 0) then
    begin
      if (fSelectIndex = -1) then
        SelectList.ItemIndex := 0
      else
        SelectList.ItemIndex := FSelectIndex;

      CheckSelectList.ItemIndex := SelectList.ItemIndex;
    end;


   if not FShowTitle then
   begin
     Title.Visible := False;
     SelectList.top := SelectList.Top - Title.Height - 4;
     SelectList.Height := SelectList.Height + Title.Height + 4;
     SelectList.left := 8;
   end
   else
     SelectList.Top := Title.Top + Title.Height + 2;

   case fbuttonpos of
    bpNone:begin
            SelectList.Width := ClientWidth - 2*8;
            SelectList.Height := Clientheight- SelectList.top-2*8;
            cancelbtn.Visible := False;
            okbtn.visible := False;
           end;
    bpBottom:
       begin
         SelectList.Width := ClientWidth - 2*8;
         SelectList.Height := Clientheight- SelectList.Top-2*8-cancelbtn.Height;

         cancelbtn.Top := ClientHeight-Cancelbtn.Height-8;
         cancelbtn.Left := ClientWidth-Cancelbtn.Width-8;
         okbtn.Top := Clientheight-8-Okbtn.Height;
         okbtn.Left := ClientWidth-2*Cancelbtn.Width-2*8;
       end;
    bpRight:
       begin
         SelectList.Width := ClientWidth - cancelbtn.width- 3*8;
         SelectList.Height := ClientHeight- SelectList.top-8;
         Cancelbtn.Top := ClientHeight-2*cancelbtn.height-2*8;
         Cancelbtn.Left := ClientWidth-cancelbtn.width-8;
         Okbtn.top := ClientHeight-8-okbtn.height;
         Okbtn.left := Cancelbtn.left;
       end;
    end;

    CheckSelectList.Top := SelectList.Top;
    CheckSelectList.Left := SelectList.Left;
    CheckSelectList.Height := SelectList.Height;
    CheckSelectList.Width := SelectList.Width;

    SelectList.Visible := ListType = ltList;
    CheckSelectList.Visible := ListType = ltCheckList;
  end;

  inc(fcount);
end;

procedure TPickDialog.Show;
begin
  CreateSelect;
  FSelectForm.FormStyle := fsStayontop;
  FSelectForm.Show;
end;

procedure TPickDialog.Hide;
begin
  if (fcount > 0) then
    FSelectForm.Free;
  FCount := 0;
end;

function TPickDialog.Execute:integer;
var
  i: Integer;
begin
  try
    CreateSelect;

    with FSelectForm do
    begin
      Result := ShowModal;

      if ListType = ltList then
      begin
        FSelectIndex := SelectList.ItemIndex;
        if (FSelectIndex >= 0) and (Result = mrOK) then
        begin
          FSelectString := SelectList.Items[FSelectIndex];
          FSelectData := TObject(sendmessage(selectlist.handle,lb_getitemdata,FSelectIndex,0));
        end;

        if FMultiSel and (SelectList.items.count > 0) and (Result = mrOK) then
        begin
          FSelectList.Clear;
          for i := 0 to SelectList.Items.Count - 1 do
          begin
            if SelectList.Selected[i] then
            begin
              FSelectList.AddObject(SelectList.Items[i], SelectList.Items.Objects[i]);
            end;
          end;
        end;
      end
      else
      begin
        FSelectIndex := CheckSelectList.ItemIndex;
        if (FSelectIndex >= 0) and (Result = mrOK) then
        begin
          FSelectString := CheckSelectList.Items[FSelectIndex];
          FSelectData := TObject(SendMessage(CheckSelectlist.Handle,LB_getitemdata,FSelectIndex,0));
        end;

        if (CheckSelectList.Items.Count > 0) and (Result = mrOK) then
        begin
          FSelectList.Clear;
          for i := 0 to CheckSelectList.Items.Count - 1 do
          begin
            if CheckSelectList.Checked[i] then
            begin
              FSelectList.AddObject(CheckSelectList.Items[i], CheckSelectList.Items.Objects[i]);
            end;
          end;
        end;

      end;

    end;
  finally
    FSelectForm.Free;
    FCount := 0;
  end;
end;

destructor TPickDialog.Destroy;
begin
  FSelectList.Free;
  FPickList.Free;
  FFont.Free;
  FListFont.Free;
  inherited Destroy;
end;

procedure TSelectForm.okbtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TSelectForm.cancelbtnClick(Sender: TObject);
begin
  Modalresult := mrCancel;
end;

procedure TPickDialog.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TPickDialog.SetHeight(value: integer);
begin
  if value > 180 then
    FHeight := value;
end;

procedure TPickDialog.SetListFont(const Value: TFont);
begin
  FListFont.Assign(Value);
end;

procedure TPickDialog.SetWidth(value: integer);
begin
  if value > 180 then
    FWidth := value;
end;

function TPickDialog.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TPickDialog.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TPickDialog.SetVersion(const Value: string);
begin

end;

procedure TSelectForm.SelectListDblClick(Sender: TObject);
begin
  if AcceptDblClick then
    Modalresult := mrOk;

  with (fParentControl as TPickDialog) do
    if Assigned(OnDblClickItem) then
    begin
      if ListType = ltList then
        FOnDblClickItem(Owner,self.SelectList.ItemIndex,
          self.selectlist.items[self.SelectList.ItemIndex])
      else
        FOnDblClickItem(Owner,self.CheckSelectList.ItemIndex,
          self.CheckSelectList.items[self.CheckSelectlist.itemindex])

    end;

end;

procedure TSelectForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  r: TRect;
begin
  inherited;
  if (BorderStyle=bsSingle) then
    Exit;

  r := ClientRect;
  r.Left := r.Right - GetSystemMetrics(SM_CXVSCROLL);
  r.Top := r.Bottom - GetSystemMetrics(SM_CXHSCROLL);
  DrawFrameControl(Canvas.Handle,r,DFC_SCROLL,DFCS_SCROLLSIZEGRIP);
end;

procedure TSelectForm.WMNCHitTest(var Message: TMessage);
var
 r:trect;
 p:tpoint;
begin
 if (BorderStyle=bsSingle) then
   inherited
 else
  begin
   r := ClientRect;
   r.Left := r.right-GetSystemMetrics(SM_CXVSCROLL);
   r.Top := r.bottom-GetSystemMetrics(SM_CXHSCROLL);

   p.x := loword(message.lparam);
   p.y := hiword(message.lparam);

   p := ScreenToClient(p);

   if ptInRect(r,p) then
     message.result := HTBOTTOMRIGHT
   else
     inherited;
  end;
end;

procedure TSelectForm.WMMinMaxInfo(var Message: TMessage);
begin
  with PMinMaxInfo(message.lParam)^ do
  begin
    ptMinTrackSize.X := CreSizeX;
    ptMinTrackSize.Y := CreSizeY;
  end;
end;

procedure TSelectForm.WMSize(var Message: TWMSize);
begin
  if (BorderStyle in [bsSizeable,bsSizeToolWin]) then
    InvalidateRect(self.handle,nil,true);

  inherited;

  {resize the list here}
  if (BorderStyle in [bsSizeable,bsSizeToolWin]) then
  begin
    case fbuttonpos of
    bpNone:begin
            SelectList.Width := ClientWidth - 2*8;
            SelectList.Height := Clientheight- SelectList.top-2*8;
            cancelbtn.visible := False;
            okbtn.visible := False;
           end;
    bpBottom:
       begin
         SelectList.Width := ClientWidth - 2*8;
         SelectList.Height:= Clientheight- SelectList.top-2*8-cancelbtn.height;
         cancelbtn.Top := ClientHeight-cancelbtn.height-8;
         cancelbtn.Left := ClientWidth-cancelbtn.width-8;
         okbtn.Top := Clientheight-8-okbtn.height;
         okbtn.Left := ClientWidth-2*cancelbtn.width-2*8;
       end;
    bpRight:
       begin
         SelectList.Width := ClientWidth - cancelbtn.width- 3*8;
         SelectList.Height := ClientHeight- SelectList.top-8;
         cancelbtn.Top := ClientHeight-2*cancelbtn.height-2*8;
         cancelbtn.Left := ClientWidth-cancelbtn.width-8;
         okbtn.Top := ClientHeight-8-okbtn.height;
         okbtn.Left := cancelbtn.left;
       end;
    end;
  end;
end;

procedure TSelectForm.SelectListClick(Sender: TObject);
begin
  with (FParentControl as TPickDialog) do
  begin
    if Assigned(OnClickItem) then
    begin
      if ListType = ltList then
        FOnClickItem(owner,self.Selectlist.Itemindex,
          self.Selectlist.Items[self.Selectlist.Itemindex])
      else
        FOnClickItem(owner,self.CheckSelectlist.Itemindex,
          self.CheckSelectlist.Items[self.CheckSelectlist.Itemindex])
    end;
  end;
end;

end.
