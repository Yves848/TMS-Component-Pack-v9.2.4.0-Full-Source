{*************************************************************************}
{ TMS Demo application                                                    }
{ TAdvPicture version                                                     }
{ TTileBmp version                                                        }
{ TWallPaper version                                                      }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2002 - 2019                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{*************************************************************************}

unit UImageControlsDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvPicture, ComCtrls, AdvPageControl, Tilebmp,
  WallPaper, ExtCtrls;

type
  TForm1 = class(TForm)
    AdvPageControl2: TAdvPageControl;
    AdvTabSheet1: TAdvTabSheet;
    AdvTabSheet2: TAdvTabSheet;
    AdvTabSheet3: TAdvTabSheet;
    Button1: TButton;
    AdvPicture1: TAdvPicture;
    TileBmp1: TTileBmp;
    Button2: TButton;
    WallPaper1: TWallPaper;
    RadioGroup1: TRadioGroup;
    Label19: TLabel;
    Label18: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
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

uses
  ShellAPI;

procedure TForm1.Button1Click(Sender: TObject);
begin
  AdvPicture1.Picture.LoadFromFile('sunset.jpg')
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if (TileBmp1.Width = 150) then
  begin
    TileBmp1.Width := 585;
    TileBmp1.Height := 425;
  end
  else
  begin
    TileBmp1.Width := 150;
    TileBmp1.Height := 150;
  end;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0 :
      WallPaper1.ImagePosition := ipBottomLeft;
    1:
      WallPaper1.ImagePosition := ipBottomRight;
    2:
      WallPaper1.ImagePosition := ipCenter;
    3:
      WallPaper1.ImagePosition := ipStretched;
    4:
      WallPaper1.ImagePosition := ipTiled;
    5:
      WallPaper1.ImagePosition := ipTopLeft;
    6:
      WallPaper1.ImagePosition := ipTopRight;
  end;
end;

end.
