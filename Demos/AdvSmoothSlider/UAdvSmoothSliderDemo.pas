{********************************************************************}
{ TMS TAdvSmoothSlider Demo                                          }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothSliderDemo;

interface

uses
  Windows, Controls, AdvSmoothSlider, Classes, StdCtrls, Forms, SysUtils, ShellAPI;

type
  TForm4 = class(TForm)
    AdvSmoothSlider3: TAdvSmoothSlider;
    AdvSmoothSlider2: TAdvSmoothSlider;
    AdvSmoothSlider1: TAdvSmoothSlider;
    Label1: TLabel;
    Label2: TLabel;
    AdvSmoothSlider4: TAdvSmoothSlider;
    AdvSmoothSlider5: TAdvSmoothSlider;
    AdvSmoothSlider6: TAdvSmoothSlider;
    AdvSmoothSlider7: TAdvSmoothSlider;
    AdvSmoothSlider8: TAdvSmoothSlider;
    AdvSmoothSlider9: TAdvSmoothSlider;
    AdvSmoothSlider10: TAdvSmoothSlider;
    AdvSmoothSlider11: TAdvSmoothSlider;
    AdvSmoothSlider12: TAdvSmoothSlider;
    Label16: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure AdvSmoothSlider3StateChanged(Sender: TObject;
      State: TAdvSmoothSliderState; Value: Double);
    procedure AdvSmoothSlider2StateChanged(Sender: TObject;
      State: TAdvSmoothSliderState; Value: Double);
    procedure Label16Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.AdvSmoothSlider2StateChanged(Sender: TObject;
  State: TAdvSmoothSliderState; Value: Double);
begin
  label2.Caption := floattostr(Value);
  label2.Caption := floattostr(Value);
end;

procedure TForm4.AdvSmoothSlider3StateChanged(Sender: TObject;
  State: TAdvSmoothSliderState; Value: Double);
begin
  case State of
    ssOn: Label1.Caption := 'ON';
    ssOff: Label1.Caption := 'OFF';
  end;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
end;

procedure TForm4.Label16Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
