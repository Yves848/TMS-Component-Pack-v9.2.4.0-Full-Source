{********************************************************************}
{ TMS TAdvSmoothGauge Demo                                           }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothGaugeDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvSmoothGauge, ComCtrls, StdCtrls, AdvStyleIF, ExtCtrls, ShellAPI;

type
  TForm185 = class(TForm)
    AdvSmoothGauge1: TAdvSmoothGauge;
    AdvSmoothGauge2: TAdvSmoothGauge;
    AdvSmoothGauge3: TAdvSmoothGauge;
    TrackBar1: TTrackBar;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    ComboBox1: TComboBox;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    Label1: TLabel;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form185: TForm185;
  g: TAdvSmoothGauge;

implementation

{$R *.dfm}

procedure TForm185.CheckBox1Click(Sender: TObject);
begin
  AdvSmoothGauge1.Animation := CheckBox1.Checked;
end;

procedure TForm185.ComboBox1Change(Sender: TObject);
var
  s: TTMSStyle;
begin
  AdvSmoothGauge1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
end;

procedure TForm185.FormCreate(Sender: TObject);
begin
  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex := 2;
  AdvSmoothGauge1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[2]));
  g := TAdvSmoothGauge.Create(Self);
  g.ValueFont.Color := clWhite;
  g.ValueFont.Size := 8;
  g.DivisionColor := clWhite;
  g.Digit.Color := clWhite;
  g.Width := 200;
  g.Height := 200;
  g.Top := 300;
  g.Left := TrackBar1.Left;
  g.Parent := Self;
end;

procedure TForm185.FormDestroy(Sender: TObject);
begin
  g.free;
end;

procedure TForm185.Label3Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm185.RadioButton1Click(Sender: TObject);
begin
  AdvSmoothGauge2.Value := -30;
end;

procedure TForm185.RadioButton2Click(Sender: TObject);
begin
  AdvSmoothGauge2.Value := 30;
end;

procedure TForm185.Timer1Timer(Sender: TObject);
begin
  g.Value := Random(100);
end;

procedure TForm185.TrackBar1Change(Sender: TObject);
begin
  AdvSmoothGauge3.Value := TrackBar1.Position;
  AdvSmoothGauge1.Value := TrackBar1.Position;
end;

end.
