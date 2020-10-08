{*************************************************************************}
{ THTMLCredit demo application                                            }
{ version 1.0                                                             }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2003 - 2019                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                              }
{*************************************************************************}


unit UHTMLCredit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, XPMan, HTMLCredit, System.ImageList, ShellAPI;

type
  TForm1 = class(TForm)
    HTMLCredit1: THTMLCredit;
    Button1: TButton;
    CheckBox1: TCheckBox;
    Label19: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  htmlcredit1.AutoScroll := true;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  htmlcredit1.Loop := checkbox1.Checked;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
