{********************************************************************}
{ TAdvDBLookupComboBox demo                                          }
{ version 1.6                                                        }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2002 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvDBLookupComboboxDatasourceDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DBCtrls, Grids, DBGrids, DB, StdCtrls, ShellAPI,
  AdvDBLookupComboBox, ExtCtrls, Datasnap.DBClient;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    AdvDBLookupComboBox1: TAdvDBLookupComboBox;
    Label3: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    DBNavigator1: TDBNavigator;
    ClientDataSet1: TClientDataSet;
    ClientDataSet2: TClientDataSet;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure AdvDBLookupComboBox1Change(Sender: TObject);
    procedure Label4Click(Sender: TObject);
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
  Variants;

procedure TForm1.AdvDBLookupComboBox1Change(Sender: TObject);
begin
  if VarIsNull(AdvDBLookupComboBox1.KeyValue) then
    label7.Caption := ''
  else
    label7.Caption :=  AdvDBLookupComboBox1.KeyValue;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  clientdataset1.LoadFromFile('.\orders.cds');
  clientdataset2.LoadFromFile('.\customer.cds');

  clientdataset1.Active := true;
  clientdataset2.Active := true;
end;

procedure TForm1.Label4Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL)
end;

end.
