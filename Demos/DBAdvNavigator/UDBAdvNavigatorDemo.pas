unit UDBAdvNavigatorDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvUtil, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Vcl.ExtCtrls, DBAdvNavigator, Datasnap.DBClient, ShellAPI,
  Vcl.StdCtrls;

const
MAX_RECS = 100;

type
  TForm3 = class(TForm)
    DBAdvNavigator1: TDBAdvNavigator;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Label19: TLabel;
    Label18: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Populate;
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
    FCDS: TClientDataSet;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  FCDS := TClientDataSet.Create(Self);
  FCDS.FieldDefs.Add('ID', ftInteger, 0, True);
  FCDS.FieldDefs.Add('Name', ftString, 20, True);
  FCDS.FieldDefs.Add('Birthday', ftDateTime, 0, True);
  FCDS.FieldDefs.Add('Salary', ftCurrency, 0, True);
  FCDS.CreateDataSet;
  DataSource1.DataSet := FCDS;

  Populate;

end;

procedure TForm3.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm3.Populate;
const
FirstNames: array[0 .. 19] of string = ('John', 'Sarah', 'Fred', 'Beth',
'Eric', 'Tina', 'Thomas', 'Judy', 'Robert', 'Angela', 'Tim', 'Traci',
'David', 'Paula', 'Bruce', 'Jessica', 'Richard', 'Carla', 'James',
'Mary');
LastNames: array[0 .. 11] of string = ('Parker', 'Johnson', 'Jones',
'Thompson', 'Smith', 'Baker', 'Wallace', 'Harper', 'Parson', 'Edwards',
'Mandel', 'Stone');
var
Index: Integer;
t1, t2: DWord;
begin
RandSeed := 0;

 t1 := GetTickCount;
FCDS.DisableControls;
try
FCDS.EmptyDataSet;
for Index := 1 to MAX_RECS do begin
  FCDS.Append;
  FCDS.FieldByName('ID').AsInteger := Index;
  FCDS.FieldByName('Name').AsString := FirstNames[Random(20)] + ' ' +
  LastNames[Random(12)];
  FCDS.FieldByName('Birthday').AsDateTime := StrToDate('1/1/1950') +
  Random(10000);
  FCDS.FieldByName('Salary').AsFloat := 20000.0 + Random(600) * 100;
  FCDS.Post;
end;
FCDS.First;
finally
FCDS.EnableControls;
end;
//t2 := GetTickCount;
//lblFeedback.Caption := Format('%d ms to load %.0n records',
//[t2 - t1, MAX_RECS * 1.0]);
end;


end.
