{**************************************************************************}
{ TWebUpdate and TWebUpdateWizard Demo                                     }
{                                                                          }
{                                                                          }
{ Copyright © 1998 - 2019                                                  }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : https://www.tmssoftware.com                                      }
{                                                                          }
{**************************************************************************}


unit UWebUpdateWizardDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, WUpdate, WUpdateWiz, Menus, ShellAPI
  {$IFDEF VER150}
  ,XPMan
  {$ENDIF}
  ;

type
  TForm1 = class(TForm)
    WebUpdateWizard1: TWebUpdateWizard;
    WebUpdate1: TWebUpdate;
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Label19: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  webupdatewizard1.Execute;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  webupdate1.DoUpdate;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
