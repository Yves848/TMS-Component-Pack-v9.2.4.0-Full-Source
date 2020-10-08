unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, Grids, AdvObj, BaseGrid, AdvGrid, DBAdvGrid, StdCtrls,
  CheckLst;

type
  TForm1 = class(TForm)
    DBAdvGrid1: TDBAdvGrid;
    ADOTable1: TADOTable;
    DataSource1: TDataSource;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CheckListBox1ClickCheck(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  IniFiles;

procedure TForm1.CheckListBox1ClickCheck(Sender: TObject);
var
  col: TDBGridColumnItem;
begin
  col := DBAdvGrid1.ColumnByFieldName[CheckListbox1.Items[CheckListbox1.ItemIndex]];
  caption := inttostr(col.Index);
  if CheckListBox1.Checked[CheckListBox1.ItemIndex] then
    DBAdvGrid1.UnHideColumn(col.Index)
  else
    DBAdvGrid1.HideColumn(col.index);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  inif: TINIFile;
begin
  inif := TINIFile.Create('.\settings.ini');
  inif.WriteString('STATES','DBADVGRID1',DBAdvGrid1.ColumnStatesToString);
  inif.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  inif: TINIFile;
  colstates: string;
  i: integer;
  col: TDBGridColumnItem;
begin
  adotable1.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=.\CARS.mdb;Persist Security Info=False';
  adotable1.Active := true;

  for i := 0 to adotable1.FieldCount - 1 do
  begin
    CheckListBox1.Items.Add(ADOTable1.Fields[i].FieldName);
    CheckListBox1.Checked[i] := true;
  end;

  DBAdvGrid1.SetColumnOrder;
  DBAdvGrid1.Options := DBAdvGrid1.Options + [goColSizing, goColMoving];

  inif := TINIFile.Create('.\settings.ini');
  colstates := inif.ReadString('STATES','DBADVGRID1','');
  inif.Free;

  if colstates <> '' then
    DBAdvGrid1.StringToColumnStates(colstates);

  for i := 0 to adotable1.FieldCount - 1 do
  begin
    col := DBAdvGrid1.ColumnByFieldName[CheckListbox1.Items[i]];
    if DBAdvGrid1.IsHiddenColumn(col.Index) then
      CheckListBox1.Checked[i] := false;
  end;
end;

end.
