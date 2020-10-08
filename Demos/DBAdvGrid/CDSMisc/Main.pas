unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, DBAdvGrid, ExtCtrls, DBCtrls, DB,
  StdCtrls, AdvUtil, AdvObj, DBClient;

type
  TFormMain = class(TForm)
    GridBiolife: TDBAdvGrid;
    SrcBiolife: TDataSource;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    GridVendors: TDBAdvGrid;
    SrcVendors: TDataSource;
    CheckBox4: TCheckBox;
    CdsBiolife: TClientDataSet;
    CdsVendors: TClientDataSet;
    DBNavigatorBioLife: TDBNavigator;
    DBNavigator1: TDBNavigator;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  CdsBiolife.Open;
  CdsVendors.Open;
  GridBiolife.Colwidths[GridBiolife.ColCount - 2] := 256;
  GridBiolife.AutoSizeRows(false,4);
end;

procedure TFormMain.CheckBox1Click(Sender: TObject);
begin
  GridBiolife.ShowPictureFields:= CheckBox1.Checked;
end;

procedure TFormMain.CheckBox2Click(Sender: TObject);
begin
  GridBiolife.ShowMemoFields:= CheckBox2.Checked;
end;

procedure TFormMain.CheckBox3Click(Sender: TObject);
begin
  GridVendors.ShowBooleanFields:= CheckBox3.Checked;
end;

procedure TFormMain.CheckBox4Click(Sender: TObject);
begin
  GridVendors.Columns[8].CheckTrue:= 'U.S.A.';
  GridVendors.Columns[8].CheckFalse:= 'Canada';
  GridVendors.Columns[8].CheckBoxField:= CheckBox4.checked;
end;

end.
