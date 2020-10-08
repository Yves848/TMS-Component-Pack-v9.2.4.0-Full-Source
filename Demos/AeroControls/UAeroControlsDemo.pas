{********************************************************************}
{ TMS Aero Controls Demo                                             }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAeroControlsDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AeroLabel, StdCtrls, AeroButtons, AeroWizardButton, Menus, ImgList,
  System.ImageList, ShellAPI;

type
  TForm4 = class(TForm)
    AeroLabel1: TAeroLabel;
    AeroWizardButton1: TAeroWizardButton;
    ImageList1: TImageList;
    AeroButton2: TAeroButton;
    AeroSpeedButton1: TAeroSpeedButton;
    PopupMenu1: TPopupMenu;
    vcl1: TMenuItem;
    all1: TMenuItem;
    web1: TMenuItem;
    fnc1: TMenuItem;
    AeroSpeedButton2: TAeroSpeedButton;
    procedure AeroLabel1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.AeroLabel1Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
