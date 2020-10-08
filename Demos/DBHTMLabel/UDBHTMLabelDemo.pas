{********************************************************************}
{ TMS TDBHTMLabel Demo                                                }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1996 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UDBHTMLabelDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, HTMLabel, dbhtmlab,
  Data.DB, Datasnap.DBClient, AdvUtil, Vcl.Grids, AdvObj, BaseGrid, AdvGrid,
  DBAdvGrid;

type
  TForm3 = class(TForm)
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    DBHTMLabel1: TDBHTMLabel;
    DBAdvGrid1: TDBAdvGrid;
    Label19: TLabel;
    Label18: TLabel;
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation
uses
  ShellAPI;

{$R *.dfm}

procedure TForm3.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
