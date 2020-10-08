{********************************************************************}
{ TMS FileControls DEMO application                                  }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2002-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UFileControlsDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileCtrl, FlCtrlEx, ShellAPI;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    FileListBoxEx1: TFileListBoxEx;
    DirectoryListBoxEx1: TDirectoryListBoxEx;
    Label2: TLabel;
    CheckFileListBoxEx1: TCheckFileListBoxEx;
    CheckDirectoryListBoxEx1: TCheckDirectoryListBoxEx;
    Label3: TLabel;
    Label4: TLabel;
    Label19: TLabel;
    Label18: TLabel;
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

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
 
