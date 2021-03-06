{********************************************************************}
{ TAdvAlertWindow demo                                               }
{ for Delphi & C++Builder                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright � 2004 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvAlertWindowDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, AdvAlertWindow, StdCtrls, ImgList, Planner, ExtCtrls,
  ShellAPI, System.ImageList;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    AdvAlertWindow1: TAdvAlertWindow;
    ImageList2: TImageList;
    Button3: TButton;
    Timer1: TTimer;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Label1Click(Sender: TObject);
    procedure AdvAlertWindow1AnchorClick(Sender: TObject; Anchor: string;
      Item: TMsgCollectionItem);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddMessage;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AddMessage;
var
  NewMsg: string;
  orders_id, products_count: integer;
  date_purchased: tdatetime;
  orders_total: double;
begin
  orders_id := random(100);
  products_count := random(9);
  orders_total := random(10000);
  date_purchased := now + random(10);

  NewMsg := '<FONT face="Roboto" size="16">Order-Nr.: <b>'+inttostr(orders_id)+'</b></FONT><br>';
  NewMsg := NewMsg + '<FONT face="Roboto" size="10" color="#606368">Date: '+DateTimeToStr(date_purchased)+'<br><hr>';


  NewMsg := NewMsg + 'Product:&nbsp;' + inttostr(products_count)+'<br>';
  NewMsg := NewMsg + 'Account:&nbsp;�' + FormatFloat('00.00',orders_total)+'<\FONT>';

  AdvAlertWindow1.AlertMessages.Add.Text.Text := NewMsg;
end;

procedure TForm1.AdvAlertWindow1AnchorClick(Sender: TObject; Anchor: string;
  Item: TMsgCollectionItem);
begin
  ShellExecute(handle, 'open', pchar(Anchor), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  AdvAlertWindow1.Show;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  AdvAlertWindow1.Hide;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  AddMessage;
  AdvAlertWindow1.Show;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   AddMessage;
   AdvAlertWindow1.Show;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
    timer1.Enabled := checkbox1.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddMessage;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://www.tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Timer1.Enabled := false;
  Hide;
end;

end.
