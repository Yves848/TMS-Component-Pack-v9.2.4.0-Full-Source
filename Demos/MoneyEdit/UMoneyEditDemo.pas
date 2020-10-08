{********************************************************************}
{ TMS TMoneyEdit Demo                                                }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UMoneyEditDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MoneyEdit, ShellAPI;

type
  TForm1 = class(TForm)
    MoneyEdit1: TMoneyEdit;
    MoneyEdit2: TMoneyEdit;
    MoneyEdit3: TMoneyEdit;
    MoneyEdit4: TMoneyEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label19: TLabel;
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

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
