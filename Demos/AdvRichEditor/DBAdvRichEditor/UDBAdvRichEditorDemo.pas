{********************************************************************}
{ TMS TDBAdvRichEditor Demo                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright � 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UDBAdvRichEditorDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvScrollControl, AdvRichEditorBase, AdvRichEditor, DBAdvRichEditor,
  StdCtrls, Grids, DBGrids, ExtCtrls, DBCtrls, DB, ADODB, AdvToolBar,
  AdvToolBarExt, AdvRichEditorToolBar, AdvToolBarStylers, ShellAPI;

type
  TForm1 = class(TForm)
    DBAdvRichEditor1: TDBAdvRichEditor;
    DataSource1: TDataSource;
    ADOTable1: TADOTable;
    Panel1: TPanel;
    AdvRichEditorFormatToolBar1: TAdvRichEditorFormatToolBar;
    Panel2: TPanel;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    AdvToolBarOfficeStyler1: TAdvToolBarOfficeStyler;
    Label1: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ADOTable1.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=.\RichEdit.mdb;Persist Security Info=False';
  ADOTable1.Active := true;
end;

procedure TForm1.Label3Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
