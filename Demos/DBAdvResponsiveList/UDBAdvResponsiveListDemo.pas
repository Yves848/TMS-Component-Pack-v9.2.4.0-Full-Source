{********************************************************************}
{ TMS TDBAdvResponsiveList Demo                                      }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UDBAdvResponsiveListDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvGraphics, AdvScrollControl,
  AdvResponsiveList, DBAdvResponsiveList, Data.DB, Datasnap.DBClient, Vcl.Grids,
  Vcl.DBGrids, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    DBAdvResponsiveList1: TDBAdvResponsiveList;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Edit1: TEdit;
    Label1: TLabel;
    Label19: TLabel;
    Label18: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Label19Click(Sender: TObject);
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
  ShellAPI;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  DBAdvResponsiveList1.FilterCondition.Text := edit1.Text;
  DBAdvResponsiveList1.FilterCondition.FilterData := [fdHeader];
  DBAdvResponsiveList1.UpdateFilter;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClientDataSet1.LoadFromFile('.\actors.cds');
  ClientDataSet1.Active := true;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
