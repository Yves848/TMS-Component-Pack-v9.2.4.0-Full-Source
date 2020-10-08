{********************************************************************}
{ TAdvDBFilter DEMO application                                      }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1998-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvDBFilterDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Grids, DBGrids, ShellAPI,
  AdvCustomFilterPanel, AdvDBFilterPanel, DB, ADODB, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    mygrid: TDBGrid;
    ADOTable1: TADOTable;
    ADOQuery1: TADOQuery;
    DataSource1: TDataSource;
    AdvDBFilterPanel1: TAdvDBFilterPanel;
    AdvDBFilterDialog1: TAdvDBFilterDialog;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Panel2: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure AdvDBFilterPanel1AfterFilter(Sender: TObject; var AFilter: string);
    procedure AdvDBFilterDialog1AfterFilter(Sender: TObject;
      var AFilter: string);
    procedure Label4Click(Sender: TObject);
    procedure AdvDBFilterDialog1Close(Sender: TObject);
  private
    procedure SetGridColumns;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AdvDBFilterDialog1AfterFilter(Sender: TObject;
  var AFilter: string);
begin
//if Sender = Self then
    SetGridColumns;
end;

procedure TForm1.AdvDBFilterDialog1Close(Sender: TObject);
begin
  AdvDBFilterPanel1.DataSource := DataSource1;
end;

procedure TForm1.AdvDBFilterPanel1AfterFilter(Sender: TObject; var AFilter: string);
begin
  SetGridColumns;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // only one filter component can use the datasource at a time, it is restored in OnClose
  AdvDBFilterPanel1.DataSource := nil;
  AdvDBFilterDialog1.Execute;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  SetGridColumns;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ADOQuery1.Active := true;
end;

procedure TForm1.Label4Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://www.tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  if TRadioButton(Sender).Tag = 1 then
  begin
    ADOQuery1.Active := True;
    ADOTable1.Active := False;
  end
  else
  begin
    ADOQuery1.Active := False;
    ADOTable1.Active := True;
  end;
end;

procedure TForm1.SetGridColumns;
begin
  mygrid.Columns[0].Width := 80;
  mygrid.Columns[1].Width := 70;
end;

end.


