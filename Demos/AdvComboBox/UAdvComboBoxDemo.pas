{********************************************************************}
{ TAdvComboBox DEMO application                                      }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1998-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvComboBoxDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvCombo, ShellAPI;

type
  TForm1 = class(TForm)
    AdvComboBox1: TAdvComboBox;
    AdvComboBox2: TAdvComboBox;
    Label2: TLabel;
    Label1: TLabel;
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Label1Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://www.tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
