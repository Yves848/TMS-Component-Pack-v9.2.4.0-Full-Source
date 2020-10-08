unit Ucolpickerdemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvUtil, Vcl.Grids, AdvObj, BaseGrid,
  AdvGrid, AdvCGrid, Vcl.StdCtrls, AdvGridColPicker;

type
  TForm4 = class(TForm)
    AdvStringGrid1: TAdvStringGrid;
    AdvGridColumnPicker1: TAdvGridColumnPicker;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

uses
  inifiles;

procedure TForm4.Button3Click(Sender: TObject);
begin
  AdvGridColumnPicker1.Show;
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
var
  s: string;
  ini: TINIFile;
begin
  s := AdvStringGrid1.ColumnStatesToString;

  ini := TINIFile.Create('.\gridsettings.ini');
  ini.WriteString('GRID','SETTINGS', s);
  ini.Free;
end;

procedure TForm4.FormCreate(Sender: TObject);
var
  s: string;
  ini: TINIFile;
begin
  AdvStringGrid1.FixedCols := 0;
  AdvStringGrid1.ColCount := 10;
  AdvStringGrid1.LinearFill(true);
  AdvStringGrid1.SetColumnOrder;
  AdvStringGrid1.Options := AdvStringGrid1.Options + [goColMoving, goColSizing];
  AdvGridColumnPicker1.Grid := AdvStringGrid1;

  ini := TINIFile.Create('.\gridsettings.ini');
  s := ini.ReadString('GRID','SETTINGS', '');
  ini.Free;

  if s <> '' then
  begin
    AdvStringGrid1.StringToColumnStates(s);
    AdvGridColumnPicker1.Init;
  end;
end;

end.
