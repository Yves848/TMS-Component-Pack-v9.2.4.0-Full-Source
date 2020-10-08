{********************************************************************}
{ TFormShape DEMO application                                        }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1998-2012                                   }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UFormShapeDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmshape, ShellAPI;

type
  TForm1 = class(TForm)
    Formshape1: TFormshape;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label19: TLabel;
    Label18: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Label19Click(Sender: TObject);
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
self.Close;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
