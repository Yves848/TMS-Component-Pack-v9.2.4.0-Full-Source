{********************************************************************}
{ TMS TGradientLabel Demo                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UGradientLabelDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GradientLabel, ShellAPI;

type
  TForm1 = class(TForm)
    GradientLabel1: TGradientLabel;
    GradientLabel4: TGradientLabel;
    GradientLabel8: TGradientLabel;
    GradientLabel9: TGradientLabel;
    GradientLabel10: TGradientLabel;
    GradientLabel11: TGradientLabel;
    Label1: TLabel;
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

{$R *.dfm}

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
