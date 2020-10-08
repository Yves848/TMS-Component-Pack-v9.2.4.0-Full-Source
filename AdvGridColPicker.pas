{*******************************************************************}
{ TAdvGridColumnPicker component                                    }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 2015-2016                                          }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

unit AdvGridColPicker;

interface

uses
  Classes, AdvGrid, BtnListB, Forms, Controls;

  // version history
  // v1.0.0.0 : First release
  // v1.1.0.0 : New : Support for VCL styles
  // v1.1.1.0 : New : OnClose event added
  // v1.1.2.0 : New : Event OnAllowColumnDrag event added
  // v1.1.2.1 : Improved : Auto restore column move setting after picker closes
  // v1.2.0.0 : New  OnAllowColumnDrop event added

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 1; // Build nr.


resourcestring
  ENoGridAssigned = 'No grid assigned';

type
  TGridFieldChooser = class(TForm)
  private
    { Private declarations }
    colsource: TObject;
    FGrid: TAdvStringGrid;
    ButtonListbox1: TButtonListbox;
    procedure ButtonListbox1OleDragStart(Sender: TObject;
      DropIndex: Integer);
    procedure ButtonListbox1OleDragStop(Sender: TObject;
      OLEEffect: Integer);
    procedure ButtonListbox1OleDragOver(Sender: TObject;
      var Allow: Boolean);
  public
    { Public declarations }
    constructor CreateNew(AOwner: TComponent; Dummy: integer = 0); override;
    procedure CreateWnd; override;
    property Grid: TAdvStringGrid read FGrid write FGrid;
    procedure MoveToFieldChooser(AColumn: integer);
    procedure RemoveFromFieldChooser(AColumn, ToColumn: integer);
  end;

  TAllowColumnDragEvent = procedure(Sender: TObject; ACol: integer; var Allow: boolean) of object;

  TAdvGridColumnPicker = class(TComponent)
  private
    FGrid: TAdvStringGrid;
    FGridFieldChooser: TGridFieldChooser;
    FCaption: TCaption;
    FDragColumn: integer;
    FDragSource: TObject;
    FPickerHeight: integer;
    FPickerWidth: integer;
    FBorderStyle: TFormBorderStyle;
    FBorderIcons: TBorderIcons;
    FOnClose: TNotifyEvent;
    FOnAllowColumnDrag: TAllowColumnDragEvent;
    FOnAllowColumnDrop: TAllowColumnDragEvent;
    procedure SetVersionString(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    function GetVersionString: string; virtual;

    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    procedure GridOleDrag(Sender: TObject; Arow,
      Acol: Integer; data: string; var allow: Boolean);
    procedure GridOleDragOver(Sender: TObject; Arow,
      Acol: Integer; var allow: Boolean);
    procedure GridOleDragStart(Sender: TObject; Arow,
      Acol: Integer);
    procedure GridOleDragStop(Sender: TObject;
      OLEEffect: Integer);
    procedure GridOleDropCol(Sender: TObject; Arow, Acol,
      DropCol: Integer);
    procedure EnsureFieldChooser;
    procedure FieldChooserClose(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddToFieldChooser(ColumnIndex: Integer);
    procedure RemoveFromFieldChooser(ColumnIndex: Integer);
    procedure Show;
    procedure Hide;
    procedure Init;
  published
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle default bsToolWindow;
    property BorderIcons: TBorderIcons read FBorderIcons write FBorderIcons;
    property Caption: TCaption read FCaption write FCaption;
    property Grid: TAdvStringGrid read FGrid write FGrid;
    property PickerWidth: integer read FPickerWidth write FPickerWidth default 200;
    property PickerHeight: integer read FPickerHeight write FPickerHeight default 300;
    property Version: string read GetVersionString write SetVersionString;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnAllowColumnDrag: TAllowColumnDragEvent read FOnAllowColumnDrag write FOnAllowColumnDrag;
    property OnAllowColumnDrop: TAllowColumnDragEvent read FOnAllowColumnDrop write FOnAllowColumnDrop;
  end;


implementation

uses
  Windows, SysUtils ;


{ TGridFieldChooser }

procedure TGridFieldChooser.ButtonListbox1OleDragOver(Sender: TObject;
  var Allow: Boolean);
begin
  Allow := Sender <> ColSource;
end;

procedure TGridFieldChooser.ButtonListbox1OleDragStart(Sender: TObject;
  DropIndex: Integer);
begin
  ColSource := Sender;
end;

procedure TGridFieldChooser.ButtonListbox1OleDragStop(Sender: TObject;
  OLEEffect: Integer);
begin
  ColSource := nil;
end;

constructor TGridFieldChooser.CreateNew(AOwner: TComponent; Dummy: integer);
begin
  inherited;
  FormStyle := fsStayOnTop;
  BorderIcons := [biMinimize, biMaximize];
  ButtonListbox1 := TButtonListBox.Create(Self);
  ButtonListbox1.Align := alClient;
  ButtonListbox1.Parent := Self;
  ButtonListbox1.ItemHeight := 23;

  ButtonListbox1.OnOleDragOver := ButtonListbox1OleDragOver;
  ButtonListbox1.OnOleDragStart := ButtonListbox1OleDragStart;
  ButtonListbox1.OnOleDragStop := ButtonListbox1OleDragStop;
end;

procedure TGridFieldChooser.CreateWnd;
begin
  inherited;
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
end;


procedure TGridFieldChooser.MoveToFieldChooser(AColumn: integer);
var
  s: string;
begin
  s := Grid.Cells[AColumn, 0];
  if s = '' then
    s := 'Column ' + inttostr(AColumn);

  ButtonListbox1.AddItem(s, TObject(AColumn));
  Grid.SuppressColumn(AColumn);
end;

procedure TGridFieldChooser.RemoveFromFieldChooser(AColumn, ToColumn: integer);
var
  i,j: integer;
  delta: integer;
  x: Integer;
  FilterColFound: Boolean;
  FilterColFoundCondition: string;

begin
  FilterColFound := false;
  // Bugfix. Has the column coming back from the picker a filter put on it? Take a note and fix it later in this method.Ole.
  for x := 0 to Grid.Filter.count - 1 do
  begin
    if not FilterColFound then
    begin
      if Grid.Filter.Items[x].Column = AColumn then
      begin
        FilterColFound := true;
        FilterColFoundCondition := Grid.Filter.Items[x].Condition;
        Grid.Filter.Items[x].free;
      end;
    end;
  end;

  if not FilterColFound then
  begin
    // Depending of the placement of the new column. Fix placement also on the Filter. Add or remove a column in the filter.Ole.
    for x := 0 to Grid.Filter.count - 1 do
    begin
      if (Grid.Filter.Items[x].Column < ToColumn) and
        (Grid.Filter.Items[x].Column > AColumn) then
        Grid.Filter.Items[x].Column := Grid.Filter.Items[x].Column - 1
      else if (Grid.Filter.Items[x].Column >= ToColumn) and
        (Grid.Filter.Items[x].Column < AColumn) then
        Grid.Filter.Items[x].Column := Grid.Filter.Items[x].Column + 1;
    end;
  end;

  Grid.UnSuppressColumn(AColumn);

  Grid.EnhRowColMove := true;

  if AColumn < ToColumn then
    dec(ToColumn);

  // Now the ToColumn has the right value. Put back the filter found earlier. Ole.
  if FilterColFound then
  begin
    with Grid.Filter.Add do
    begin
      Condition := FilterColFoundCondition;
      Column := ToColumn;
    end
  end;
  Grid.MoveColumn(AColumn, ToColumn);

  if ToColumn > AColumn then
    delta := -1
  else
    delta := +1;

  for i := 0 to ButtonListbox1.Items.Count - 1 do
  begin
    j := integer(ButtonListbox1.Items.Objects[i]);

    if (delta = -1) and (j > AColumn) and (j <= ToColumn) then
    begin
      j := j + delta;
      ButtonListbox1.Items.Objects[i] := TObject(j);
    end;

    if (delta = +1) and (j >= ToColumn) and (j < AColumn) then
    begin
      j := j + delta;
      ButtonListbox1.Items.Objects[i] := TObject(j);
    end;
  end;

  Grid.EnhRowColMove := false;

  ButtonListBox1.Items.Delete(ButtonListBox1.ItemIndex);
end;

{ TAdvGridColumnPicker }

procedure TAdvGridColumnPicker.AddToFieldChooser(ColumnIndex: Integer);
begin
  if not Assigned(FGrid) then
    raise Exception.Create(ENoGridAssigned);

  EnsureFieldChooser;

  FGridFieldChooser.MoveToFieldChooser(ColumnIndex);
end;

constructor TAdvGridColumnPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGrid := nil;
  FGridFieldChooser := nil;
  FCaption := 'Columns';
  FPickerWidth := 200;
  FPickerHeight := 300;
  FBorderStyle := bsToolWindow;
  FBorderIcons := [biMinimize, biMaximize];
end;

destructor TAdvGridColumnPicker.Destroy;
begin
  if Assigned(FGridFieldChooser) then
    FGridFieldChooser.Close;
  inherited;
end;

procedure TAdvGridColumnPicker.EnsureFieldChooser;
begin
  if not Assigned(FGridFieldChooser) then
    FGridFieldChooser := TGridFieldChooser.CreateNew(Application);

  FGridFieldChooser.BorderStyle := BorderStyle;
  FGridFieldChooser.BorderIcons := BorderIcons;
  FGridFieldChooser.Caption := Caption;
  FGridFieldChooser.Grid := FGrid;
  FGridFieldChooser.Width := FPickerWidth;
  FGridFieldChooser.Height := FPickerHeight;
  FGridFieldChooser.OnClose := FieldChooserClose;
end;

procedure TAdvGridColumnPicker.FieldChooserClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FGrid) and not (csDestroying in ComponentState) then
    FGrid.EnhRowColMove := true;

  if Assigned(OnClose) then
    OnClose(Self);
end;

function TAdvGridColumnPicker.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvGridColumnPicker.GetVersionString: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

procedure TAdvGridColumnPicker.GridOleDrag(Sender: TObject; ARow, ACol: Integer;
  data: string; var Allow: Boolean);
begin
  Allow := (ARow = 0) and (ACol >= Grid.FixedCols);

  if Allow and Assigned(OnAllowColumnDrag) then
    OnAllowColumnDrag(Self, ACol, Allow);

  FDragColumn := Acol;
end;

procedure TAdvGridColumnPicker.GridOleDragOver(Sender: TObject; Arow,
  Acol: Integer; var allow: Boolean);
begin
  Allow := (FDragSource = nil) and (ACol > 0) and (ARow = 0);

  if Allow and  Assigned(OnAllowColumnDrop) then
  begin
    OnAllowColumnDrop(Self, ACol, Allow);
  end;
end;

procedure TAdvGridColumnPicker.GridOleDragStart(Sender: TObject; Arow,
  Acol: Integer);
begin
  FDragSource := Sender;
end;

procedure TAdvGridColumnPicker.GridOleDragStop(Sender: TObject;
  OLEEffect: Integer);
begin
  FGridFieldChooser.MoveToFieldChooser(FDragColumn);
  FDragSource := nil;
end;

procedure TAdvGridColumnPicker.GridOleDropCol(Sender: TObject; Arow, Acol,
  DropCol: Integer);
begin
  FGridFieldChooser.RemoveFromFieldChooser(DropCol,ACol);
end;

procedure TAdvGridColumnPicker.Hide;
begin
  if Assigned(FGridFieldChooser) then
    FGridFieldChooser.Hide;
  FGrid.EnhRowColMove := true;
end;

procedure TAdvGridColumnPicker.Init;
var
  i: integer;
begin
  if not Assigned(Grid) then
    raise Exception.Create('No grid assigned');

  for i := 0 to grid.ColCount - 1 do
  begin
    if grid.IsSuppressedColumn(i) then
      AddToFieldChooser(i);
  end;
end;

procedure TAdvGridColumnPicker.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FGrid) then
    FGrid := nil;
end;

procedure TAdvGridColumnPicker.RemoveFromFieldChooser(ColumnIndex: Integer);
var
  i: integer;
begin
  if not Assigned(FGridFieldChooser) then
    Exit;

  FGridFieldChooser.RemoveFromFieldChooser(ColumnIndex, ColumnIndex);

  for i := 0 to FGridFieldChooser.ButtonListbox1.Items.Count - 1 do
  begin
    if Integer(FGridFieldChooser.ButtonListBox1.Items.Objects[i]) = ColumnIndex then
    begin
      FGridFieldChooser.ButtonListBox1.Items.Delete(i);
      break;
    end;
  end;
end;

procedure TAdvGridColumnPicker.SetVersionString(const Value: string);
begin
  //
end;

procedure TAdvGridColumnPicker.Show;
begin
  if not Assigned(FGrid) then
    raise Exception.Create('No grid assigned');

  EnsureFieldChooser;

  FGrid.EnhRowColMove := false;
  FGrid.DragDropSettings.OleDropSource := True;
  FGrid.DragDropSettings.OleDropTarget := True;
  FGrid.DragDropSettings.OleCopyAlways := True;
  FGrid.DragDropSettings.OleColumnsOnly := True;
  FGrid.DragDropSettings.OleColumnPicker := True;

  FGrid.OnOleDrag := GridOleDrag;
  FGrid.OnOleDragOver := GridOleDragOver;
  FGrid.OnOleDragStart := GridOleDragStart;
  FGrid.OnOleDragStop := GridOleDragStop;
  FGrid.OnOleDropCol := GridOleDropCol;

  FGridFieldChooser.Show;
end;

end.
