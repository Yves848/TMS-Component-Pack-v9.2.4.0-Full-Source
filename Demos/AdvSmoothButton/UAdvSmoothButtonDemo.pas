{********************************************************************}
{ TMS TAdvSmoothButton Demo                                          }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothButtonDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ExtCtrls, AdvSmoothButton, StdCtrls, ShellAPI;

type
  TForm166 = class(TForm)
    AdvSmoothButton1: TAdvSmoothButton;
    AdvSmoothButton2: TAdvSmoothButton;
    AdvSmoothButton3: TAdvSmoothButton;
    AdvSmoothButton4: TAdvSmoothButton;
    AdvSmoothButton5: TAdvSmoothButton;
    AdvSmoothButton6: TAdvSmoothButton;
    Label1: TLabel;
    Label5: TLabel;
    procedure AdvSmoothButton5Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form166: TForm166;

implementation

{$R *.dfm}

procedure TForm166.AdvSmoothButton5Click(Sender: TObject);
begin
  Label1.Caption := 'You have chosen ' + (Sender as TAdvSmoothButton).Status.Caption;
end;

procedure TForm166.Label5Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
