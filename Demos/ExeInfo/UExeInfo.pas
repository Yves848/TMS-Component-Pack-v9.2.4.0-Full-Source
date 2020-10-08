{********************************************************************}
{ TMS TExeInfo Demo                                                  }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UExeInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExeInfo, StdCtrls, Menus, ShellAPI;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Help1: TMenuItem;
    About1: TMenuItem;
    ExeInfo1: TExeInfo;
    Label1: TLabel;
    Label19: TLabel;
    Label18: TLabel;
    procedure About1Click(Sender: TObject);
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


procedure TForm1.About1Click(Sender: TObject);
begin
  MessageDlg(ExeInfo1.ProductName + ' version ' + ExeInfo1.ProductVersion + ' by ' + ExeInfo1.CompanyName, mtInformation, [mbok], 0);
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
