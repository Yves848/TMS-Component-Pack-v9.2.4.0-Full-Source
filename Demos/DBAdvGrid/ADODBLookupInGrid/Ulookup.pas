unit Ulookup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, Grids, BaseGrid, AdvGrid, DBAdvGrid, DBGrids,
  StdCtrls, AdvObj, AdvUtil, AdvDBLookupComboBox, frmctrllink;

type
  TForm1 = class(TForm)
    ADOTable1: TADOTable;
    DataSource1: TDataSource;
    DBAdvGrid1: TDBAdvGrid;
    ADOTable2: TADOTable;
    DataSource2: TDataSource;
    ADOConnection1: TADOConnection;
    ADOTable1Key: TAutoIncField;
    ADOTable1Name: TWideStringField;
    ADOTable1Street: TWideStringField;
    ADOTable1Place: TWideStringField;
    ADOTable1Country: TIntegerField;
    DBAdvGrid2: TDBAdvGrid;
    ADOTable1CountryName: TStringField;
    Label1: TLabel;
    Label2: TLabel;
    AdvDBLookupComboBox1: TAdvDBLookupComboBox;
    FormControlEditLink1: TFormControlEditLink;
    ADOTable1Capital: TWideStringField;
    procedure FormCreate(Sender: TObject);
    procedure FormControlEditLink1GetEditorValue(Sender: TObject;
      Grid: TAdvStringGrid; var AValue: string);
    procedure FormControlEditLink1SetEditorValue(Sender: TObject;
      Grid: TAdvStringGrid; AValue: string);
    procedure DBAdvGrid1GetEditorType(Sender: TObject; ACol, ARow: Integer;
      var AEditor: TEditorType);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DBAdvGrid1GetEditorType(Sender: TObject; ACol, ARow: Integer;
  var AEditor: TEditorType);
begin
  AEditor := edNormal;
  DBAdvGrid1.EditLink := nil;

  if (ACol = 4) then
  begin
    AEditor := edCustom;
    DBAdvGrid1.EditLink := FormControlEditLink1;
  end;
end;

procedure TForm1.FormControlEditLink1GetEditorValue(Sender: TObject;
  Grid: TAdvStringGrid; var AValue: string);
var
  s:string;
begin
  AValue := AdvDBLookupComboBox1.Text;
  s := adotable2.FieldByName('Capital').AsString;
  adotable1.FieldByName('Capital').AsString := s;
end;

procedure TForm1.FormControlEditLink1SetEditorValue(Sender: TObject;
  Grid: TAdvStringGrid; AValue: string);
begin
  AdvDBLookupComboBox1.Text := AValue;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  adotable1.Active := false;
  adotable2.Active := false;
  adoconnection1.Connected := false;
  adoconnection1.connectionstring := 'Provider=Microsoft.Jet.OLEDB.4.0;User ID=Admin;Data Source=.\lookup.mdb;Mode=Share Deny None;Extended Properties="";Jet OLEDB:System database="";Jet OLEDB:Registry Path="";Jet OLEDB:Database Password="";Jet OLEDB:Engine Type=5;Jet OLEDB:'+
    'Database Locking Mode=1;Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Global Bulk Transactions=1;Jet OLEDB:New Database Password="";Jet OLEDB:Create System Database=False;Jet OLEDB:Encrypt Database=False;Jet OLEDB:Don''t Copy Locale on Compact=False;'+
    'Jet OLEDB:Compact Without Replica Repair=False;Jet OLEDB:SFP=False';
  adoconnection1.Connected := true;
  adotable1.Active := true;
  adotable2.Active := true;
end;

end.
