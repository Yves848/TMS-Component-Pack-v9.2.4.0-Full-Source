{***************************************************************************}
{ TADVSPREADGRID Demo project                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1996 - 2019                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                              }
{***************************************************************************}

unit UAdvSpreadGridCustomLibsDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, AdvListV, StdCtrls, Grids, AdvGrid, advsprd, AdvPars, ShellAPI,
  MiscMathLib, Buttons, Tabs, ExtCtrls, BaseGrid, AdvUtil, AdvObj;

type
  TForm1 = class(TForm)
    MiscMathLib1: TMiscMathLib;
    LibBinder1: TLibBinder;
    FinanceMathLib1: TFinanceMathLib;
    GridBinder1: TGridBinder;
    TabSet1: TTabSet;
    Notebook1: TNotebook;
    SpreadSheet2: TAdvSpreadGrid;
    SpreadSheet1: TAdvSpreadGrid;
    Panel1: TPanel;
    Button1: TButton;
    Button3: TButton;
    Panel2: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure AdvSpreadGrid1ForumlaErrorInfo(Sender: TObject; ACol, ARow,
      ErrType, ErrPos, ErrParam: Integer; ErrStr: String);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure SpreadSheet1CellValidate(Sender: TObject; Col, Row: Integer;
      var Value: String; var Valid: Boolean);
    procedure Label7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  spreadsheet1.cells[1,1]:='5';
  spreadsheet1.cells[1,2]:='23';
  spreadsheet1.cells[1,3]:='11';
  spreadsheet1.cells[1,4]:='=HARMEAN(A1:A2)';
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  spreadsheet1.Cells[1,1]:='1';
  spreadsheet1.Cells[1,2]:='2';
  spreadsheet1.Cells[1,3]:='3';

  spreadsheet1.Cells[1,4]:='=SUM(Sheet1!A1:A3)';
  spreadsheet2.Cells[1,4]:='=SUM(Sheet1!A1:A3)';

  spreadsheet1.Recalc;
  spreadsheet2.Recalc;

end;

procedure TForm1.Label7Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.AdvSpreadGrid1ForumlaErrorInfo(Sender: TObject; ACol,
  ARow, ErrType, ErrPos, ErrParam: Integer; ErrStr: String);
begin
  Label6.Caption := SpreadSheet1.ErrorToString(ErrType) +' at cell ['+inttostr(ACol)+':'+inttostr(ARow)+'] position '+inttostr(ErrPos);
  Label7.Visible := False;
end;

procedure TForm1.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  Notebook1.PageIndex := NewTab;
end;

procedure TForm1.SpreadSheet1CellValidate(Sender: TObject; Col,
  Row: Integer; var Value: String; var Valid: Boolean);
begin
  Label6.Caption := 'For more information please visit:';
  Label7.Visible := True;
end;

end.
