{********************************************************************}
{ TMS HotSpotShape   Demo                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1996 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UHotSpotShapeDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvShaper, HotSpotImage,
  Vcl.Imaging.pngimage, Vcl.StdCtrls, ShellAPI;

type
  TForm4 = class(TForm)
    AdvShaper1: TAdvShaper;
    HotSpotImage1: THotSpotImage;
    procedure HotSpotImage1HotSpotClick(Sender: TObject; HotSpot: THotSpot);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Lock: Boolean;
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.FormCreate(Sender: TObject);
begin
  Lock := False;
end;

procedure TForm4.HotSpotImage1HotSpotClick(Sender: TObject; HotSpot: THotSpot);
begin
  if HotSpot.ID = 1 then
    Application.Terminate;

  if not Lock then
  begin
    case HotSpot.ID of
      2: ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
      3: ShowMessage(DateToStr(Now)+ #13 + TimeToStr(Now));
      4: Beep;
      5: ShellExecute(handle, 'open', 'https://www.youtube.com/user/tmssoftwareTV', nil, nil, SW_SHOWNORMAL);
      6:
      begin
        Lock := true;
        ShowMessage('Locked');
      end;
      0: ShowMessage(HotSpot.Hint);
    end;
  end
  else if HotSpot.ID = 6 then
  begin
    Lock := false;
    ShowMessage('Unlocked');
  end;
end;

end.
