{********************************************************************}
{ TMS THotSpotImage Demo                                             }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UHotSpotImageDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, HotSpotImage, ExtCtrls, StdCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    HotSpotImage1: THotSpotImage;
    Label1: TLabel;
    Panel1: TPanel;
    Label19: TLabel;
    Label18: TLabel;
    procedure HotSpotImage1HotSpotEnter(Sender: TObject;
      HotSpot: THotSpot);
    procedure HotSpotImage1HotSpotExit(Sender: TObject; HotSpot: THotSpot);
    procedure HotSpotImage1HotSpotClick(Sender: TObject;
      HotSpot: THotSpot);
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

procedure TForm1.HotSpotImage1HotSpotEnter(Sender: TObject;
  HotSpot: THotSpot);
begin
  panel1.Caption := HotSpot.Hint;
end;

procedure TForm1.HotSpotImage1HotSpotExit(Sender: TObject;
  HotSpot: THotSpot);
begin
  Panel1.Caption := 'Motherboard';
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.HotSpotImage1HotSpotClick(Sender: TObject;
  HotSpot: THotSpot);
begin
 ShowMessage('You clicked on '+HotSpot.Hint);
end;

end.
