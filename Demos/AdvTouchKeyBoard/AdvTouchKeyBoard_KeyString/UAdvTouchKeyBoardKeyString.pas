{********************************************************************}
{ TAdvTouchKeyBoard KeyString Demo                                   }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2005 - 2020                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvTouchKeyBoardKeyString;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, AdvTouchKeyboard,
  Vcl.StdCtrls, AdvEdit, System.ImageList, Vcl.ImgList, ShellAPI;

type
  TForm1 = class(TForm)
    AdvEdit1: TAdvEdit;
    AdvTouchKeyboard1: TAdvTouchKeyboard;
    ImageList1: TImageList;
    Label5: TLabel;
    procedure Label5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Label5Click(Sender: TObject);
begin
  ShellExecute(handle,'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
