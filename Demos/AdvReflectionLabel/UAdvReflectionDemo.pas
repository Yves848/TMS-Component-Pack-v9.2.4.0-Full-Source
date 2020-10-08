{********************************************************************}
{ TMS TAdvReflectionLabel Demo                                       }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvReflectionDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvReflectionLabel, AdvOfficePager, ShellAPI,
  AdvOfficePagerStylers, ImgList, PictureContainer, AdvReflectionImage,
  AdvGlassButton, ExtCtrls;

type
  TForm1 = class(TForm)
    AdvReflectionLabel1: TAdvReflectionLabel;
    AdvReflectionImage1: TAdvReflectionImage;
    Label16: TLabel;
    AdvReflectionImage2: TAdvReflectionImage;
    procedure Label16Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Label16Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp',nil,nil,SW_SHOWNORMAL);
end;

end.
