unit UFDMemTable;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.StorageBin, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, AdvUtil, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, DBAdvGrid;

type
  TForm4 = class(TForm)
    FDMemTable1: TFDMemTable;
    DataSource1: TDataSource;
    DBAdvGrid1: TDBAdvGrid;
    procedure FormCreate(Sender: TObject);
    procedure DBAdvGrid1CanSort(Sender: TObject; ACol: Integer;
      var DoSort: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.DBAdvGrid1CanSort(Sender: TObject; ACol: Integer;
  var DoSort: Boolean);
begin
  FDMemTable1.Indexes.Clear;
  FDMemTable1.Indexes.Add;

  if ACol = 1 then
    FDMemTable1.Indexes.Items[0].Fields := 'Number';

  if ACol = 2 then
    FDMemTable1.Indexes.Items[0].Fields := 'String';

  FDMemTable1.Indexes.Items[0].Name := 'GRID';

  if DBAdvGrid1.SortSettings.Direction <> sdDescending then
    FDMemTable1.Indexes.Items[0].Options := [soDescending];

  FDMemTable1.Indexes.Items[0].Active := true;

  FDMemTable1.IndexName := 'GRID';
end;

function RandomString(cnt: integer): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to cnt - 1 do
    Result := Result + chr(ord('A') + random(26));
end;

procedure TForm4.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  with FDMemTable1.FieldDefs do begin
    with AddFieldDef do begin
      Name := 'Number';
      DataType := ftInteger;
    end;
    with AddFieldDef do begin
      Name := 'String';
      DataType := ftString;
      Size := 50;
    end;
  end;


  for i := 0 to 50 do
  begin

    with FDMemTable1 do begin
      Open;
      Append;
      Fields[0].AsInteger := random(100);
      Fields[1].AsString := randomstring(6);
      Post;
    end;
  end;

  FDMemTable1.Active := true;
  FDMemTable1.First;

  DBAdvGrid1.SortSettings.Show := true;
end;

end.
