{********************************************************************}
{ TMS TAdvGroupbox Demo                                              }
{ for Delphi & C++Builder                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                        }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvGroupBoxDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvGroupBox, AdvOfficePager, AdvOfficePagerStylers, jpeg,
  ExtCtrls, ImgList, AdvOfficeButtons, PictureContainer,
  AdvOfficeHint, AdvGlowButton, ShellAPI, System.ImageList;

type
  TForm2 = class(TForm)
    ImageList1: TImageList;
    AdvGroupBox1: TAdvGroupBox;
    AdvGroupBox2: TAdvGroupBox;
    AdvOfficeCheckBox1: TAdvOfficeCheckBox;
    AdvOfficeCheckBox2: TAdvOfficeCheckBox;
    AdvOfficeCheckBox5: TAdvOfficeCheckBox;
    AdvOfficeCheckBox3: TAdvOfficeCheckBox;
    AdvOfficeCheckBox4: TAdvOfficeCheckBox;
    AdvOfficeRadioButton3: TAdvOfficeRadioButton;
    AdvOfficeRadioButton6: TAdvOfficeRadioButton;
    AdvOfficeRadioButton5: TAdvOfficeRadioButton;
    AdvOfficeRadioButton4: TAdvOfficeRadioButton;
    Label4: TLabel;
    procedure AdvOfficeCheckBox5Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}



procedure TForm2.AdvOfficeCheckBox5Click(Sender: TObject);
begin
  if AdvOfficeCheckBox5.Checked then
  begin
    AdvOfficeCheckBox5.Caption := '<img src="idx:10">   Cookies';
    AdvGroupBox1.ImageIndex := 0;
  end
  else
  begin
    AdvOfficeCheckBox5.Caption := '<img src="idx:10">   <s>Cookies</s>';
  end;
end;

procedure TForm2.Label4Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
