{********************************************************************}
{ TMS TDBAdvCardList Demo                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1996 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UDBAdvCardList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, AdvCardList, DBAdvCardList, StdCtrls, Grids, DBGrids,
  ExtCtrls, DBCtrls, ComCtrls, Buttons, AdvCardListStyler, JPEG, Mask,
  Datasnap.DBClient, ShellAPI, DBAdvNavigator;

type
  TForm1 = class(TForm)
    DataSource2: TDataSource;
    Panel1: TPanel;
    AdvCardListStyler2: TAdvCardListStyler;
    DBAdvCardList2: TDBAdvCardList;
    DBGrid2: TDBGrid;
    Splitter1: TSplitter;
    ClientDataSet1: TClientDataSet;
    DBAdvNavigator1: TDBAdvNavigator;
    Panel2: TPanel;
    Label19: TLabel;
    Label18: TLabel;
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
