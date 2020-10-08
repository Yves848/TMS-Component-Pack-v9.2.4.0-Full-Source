{********************************************************************}
{ TMS Edit Controls Demo                                             }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UAdvEditControlsDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvEdit, AdvEdBtn, AdvFileNameEdit, AdvDirectoryEdit,
  Mask, advlued, AdvCombo, Lucombo, AdvQueryDialog, ShellAPI;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    AdvEditBtn1: TAdvEditBtn;
    UnitAdvEditBtn1: TUnitAdvEditBtn;
    Label2: TLabel;
    Label3: TLabel;
    AdvFileNameEdit1: TAdvFileNameEdit;
    Label4: TLabel;
    AdvDirectoryEdit1: TAdvDirectoryEdit;
    Label5: TLabel;
    AdvEdit1: TAdvEdit;
    Label6: TLabel;
    AdvEdit2: TAdvEdit;
    AdvEdit3: TAdvEdit;
    Label7: TLabel;
    Label8: TLabel;
    AdvEdit4: TAdvEdit;
    Label9: TLabel;
    AdvMaskEdit1: TAdvMaskEdit;
    Label10: TLabel;
    AdvLUEdit1: TAdvLUEdit;
    Label11: TLabel;
    Label12: TLabel;
    LUCombo1: TLUCombo;
    Label13: TLabel;
    LUEdit1: TLUEdit;
    Label14: TLabel;
    AdvQueryDialog1: TAdvQueryDialog;
    Button1: TButton;
    Edit1: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    AdvEdit5: TAdvEdit;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
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

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if AdvQueryDialog1.ShowModal=mrOK then
    Edit1.Text := AdvQueryDialog1.Text;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
