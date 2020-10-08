{********************************************************************}
{ TMS TAdvSmoothTouchKeyBoard Demo                                   }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothTouchKeyBoardDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvSmoothTouchKeyBoard, StdCtrls, ExtCtrls, AdvStyleIF, ShellAPI;

type
  TForm171 = class(TForm)
    Memo1: TMemo;
    AdvSmoothTouchKeyBoard1: TAdvSmoothTouchKeyBoard;
    Label7: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form171: TForm171;

implementation

{$R *.dfm}

procedure TForm171.FormCreate(Sender: TObject);
begin
  AdvSmoothTouchKeyBoard1.AutoCompletion.LookupList.LoadFromFile('fullwords.txt');
end;

procedure TForm171.Label1Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
