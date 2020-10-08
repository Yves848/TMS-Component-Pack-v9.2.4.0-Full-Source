{********************************************************************}
{ TMS TAdvFocusHelper Demo                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvFocusHelperDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvFocusHelper, StdCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    AdvFocusHelper1: TAdvFocusHelper;
    Edit1: TEdit;
    Edit2: TEdit;
    ListBox1: TListBox;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ComboBox1: TComboBox;
    Label8: TLabel;
    Label4: TLabel;
    procedure Label8Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Label8Click(Sender: TObject);
begin
  ShellExecute(handle,'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
