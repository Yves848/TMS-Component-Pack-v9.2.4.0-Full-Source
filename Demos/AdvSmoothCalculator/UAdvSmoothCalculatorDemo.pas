{********************************************************************}
{ TMS TAdvSmoothCalculator Demo                                      }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothCalculatorDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, AdvSmoothEdit, AdvSmoothEditButton, ShellAPI,
  AdvSmoothCalculatorDropDown, AdvSmoothCalculator, AdvStyleIF, Vcl.Mask;

type
  TForm343 = class(TForm)
    AdvSmoothCalculator1: TAdvSmoothCalculator;
    ComboBox1: TComboBox;
    AdvSmoothCalculatorDropDown1: TAdvSmoothCalculatorDropDown;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form343: TForm343;

implementation

{$R *.dfm}

procedure TForm343.ComboBox1Change(Sender: TObject);
begin
  AdvSmoothCalculator1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
  AdvSmoothCalculatorDropDown1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
end;

procedure TForm343.FormCreate(Sender: TObject);
begin
  AdvSmoothCalculator1.Precision := 2;
  AdvSmoothCalculatorDropDown1.Precision := 2;
  AdvSmoothCalculatorDropDown1.Calculator.Precision := 2;
  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex := 1;
  AdvSmoothCalculator1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[1]));
  AdvSmoothCalculatorDropDown1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[1]));
end;

procedure TForm343.Label3Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm343.TrackBar1Change(Sender: TObject);
begin
  AdvSmoothCalculator1.Precision := Trackbar1.Position;
    AdvSmoothCalculatorDropDown1.Precision := TrackBar1.Position;
  AdvSmoothCalculatorDropDown1.Calculator.Precision := Trackbar1.Position;
end;

end.
