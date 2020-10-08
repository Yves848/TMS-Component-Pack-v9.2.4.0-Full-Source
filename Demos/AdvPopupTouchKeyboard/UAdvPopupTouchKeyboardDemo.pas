{********************************************************************}
{ TMS TAdvTouchKeyboard  Demo                                        }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvPopupTouchKeyboardDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AdvTouchKeyboard, AdvEdit, ShellAPI;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    AdvPopupTouchKeyBoard1: TAdvPopupTouchKeyBoard;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Button3: TButton;
    Label2: TLabel;
    Label16: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure AdvPopupTouchKeyBoard1Close(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure AdvPopupTouchKeyBoard1Show(Sender: TObject);
    procedure Label16Click(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.AdvPopupTouchKeyBoard1Close(Sender: TObject);
begin
  Button3.Visible:=true;
end;

procedure TForm1.AdvPopupTouchKeyBoard1Show(Sender: TObject);
begin
  Button3.Visible:=false;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  AdvPopupTouchKeyBoard1.KeyboardType:=TKeyboardType.ktQWERTY;
  Button1.Visible:=false;
  Button2.Visible:=true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  AdvPopupTouchKeyBoard1.KeyboardType:=TKeyboardType.ktAZERTY;
  Button1.Visible:=true;
  Button2.Visible:=false;
end;


procedure TForm1.Button3Click(Sender: TObject);
begin
  AdvPopupTouchKeyBoard1.Show;
end;

procedure TForm1.Label16Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.Memo1Click(Sender: TObject);
begin
  AdvPopupTouchKeyBoard1.Show;
end;

end.
