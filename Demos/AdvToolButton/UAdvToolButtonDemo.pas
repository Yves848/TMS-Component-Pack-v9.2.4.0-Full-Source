{********************************************************************}
{ TAdvToolButton demo                                                }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2002 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvToolButtonDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, AdvToolBtn, Vcl.StdCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    AdvToolButton1: TAdvToolButton;
    AdvToolButton2: TAdvToolButton;
    AdvToolButton3: TAdvToolButton;
    AdvToolButton4: TAdvToolButton;
    AdvToolButton5: TAdvToolButton;
    AdvToolButton6: TAdvToolButton;
    AdvToolButton7: TAdvToolButton;
    AdvToolButton8: TAdvToolButton;
    AdvToolButton9: TAdvToolButton;
    AdvToolButton10: TAdvToolButton;
    AdvToolButton11: TAdvToolButton;
    AdvToolButton12: TAdvToolButton;
    AdvToolButton13: TAdvToolButton;
    AdvToolButton14: TAdvToolButton;
    AdvToolButton15: TAdvToolButton;
    AdvToolButton16: TAdvToolButton;
    AdvToolButton17: TAdvToolButton;
    AdvToolButton18: TAdvToolButton;
    Label9: TLabel;
    Label1: TLabel;
    procedure Label9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Label9Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
