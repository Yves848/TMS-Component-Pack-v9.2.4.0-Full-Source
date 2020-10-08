{*************************************************************************}
{ TAdvMemo, TDBAdvMemo demo application                                   }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1998-2019                                        }
{            Email : info@tmssoftware.com                                 }
{            Web : https://www.tmssoftware.com                            }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit UAdvMemoEmoticonsDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvMemo, AdvmES, ExtCtrls, ShellAPI, Vcl.StdCtrls;

type
  TFAdvMemoDemo06 = class(TForm)
    AdvMemo1: TAdvMemo;
    AdvEmoticonMemoStyler1: TAdvEmoticonMemoStyler;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Label4: TLabel;
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FAdvMemoDemo06: TFAdvMemoDemo06;

implementation

{$R *.dfm}

procedure TFAdvMemoDemo06.Label4Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
