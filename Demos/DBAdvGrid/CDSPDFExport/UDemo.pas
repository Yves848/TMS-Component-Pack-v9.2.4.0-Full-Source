unit UDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvUtil, AdvCustomComponent, AdvPDFIO,
  AdvGridPDFIO, Vcl.StdCtrls, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, DBAdvGrid,
  Data.DB, Datasnap.DBClient;

type
  TForm56 = class(TForm)
    ClientDataSet1: TClientDataSet;
    DBAdvGrid1: TDBAdvGrid;
    Button1: TButton;
    AdvGridPDFIO1: TAdvGridPDFIO;
    DataSource1: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form56: TForm56;

implementation

{$R *.dfm}

procedure TForm56.Button1Click(Sender: TObject);
begin
  AdvGridPDFIO1.Save('GridExport.pdf');
end;

procedure TForm56.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  AdvGridPDFIO1.Options.OpenInPDFReader := True;
  DBAdvGrid1.ShowPictureFields := True;
  DBAdvGrid1.ShowBooleanFields := True;
  DBAdvGrid1.Bands.Active := True;
  for I := 0 to DBAdvGrid1.ColCount - 1 do
    DBAdvGrid1.Columns[I].ShowBands := True;
end;

end.
