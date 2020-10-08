{***************************************************************************}
{ TDBInspectorBar demo                                                      }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2012                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{***************************************************************************}

unit UDBInspectorBarDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids, DBCtrls, StdCtrls, ExtCtrls,
  InspectorBar, DBInspectorBar, ShellAPI, Datasnap.DBClient
  {$IFDEF VER140}
  ,Variants
  {$ENDIF}
  ;

type
  TForm1 = class(TForm)
    DBInspectorBar1: TDBInspectorBar;
    CheckBox1: TCheckBox;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    DataSource2: TDataSource;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    ClientDataSet1: TClientDataSet;
    Label19: TLabel;
    Label18: TLabel;
    procedure CheckBox1Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
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

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  DBInspectorBar1.Panels[1].ShowNavigator := Checkbox1.Checked;
  DBNavigator1.Visible := not CheckBox1.Checked;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  shellexecute(0,'open',pchar(label1.Caption),nil,nil,SW_NORMAL);
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if checkbox2.Checked then
    DBInspectorBar1.Navigator.ButtonStyle := bsGlyphs
  else
    DBInspectorBar1.Navigator.ButtonStyle := bsDefault;

end;

end.
