{********************************************************************}
{ TMS TAdvRangeSlider Demo                                           }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UAdvRangeSliderDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, AdvTrackBar, Vcl.StdCtrls,
  AdvAppStyler, AdvStyleIF, ShellAPI;

type
  TForm1 = class(TForm)
    AdvRangeSlider1: TAdvRangeSlider;
    AdvRangeSlider2: TAdvRangeSlider;
    PaintBox1: TPaintBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Label16: TLabel;
    Label1: TLabel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure AdvRangeSlider1ChangeLeft(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.AdvRangeSlider1ChangeLeft(Sender: TObject);
begin
  Paintbox1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  styles:TStringList;
  i:Integer;
begin
  RadioGroup2.Items.Clear;
  styles:=TStringList.Create;
  FillStyleList(styles);
  for i := 0 to styles.Count-1 do
  begin
    RadioGroup2.Items.AddObject(styles.Strings[i],styles.Objects[i]);
  end;
  RadioGroup2.ItemIndex:=2;
  AdvRangeSlider1.SetComponentStyle(TTMSStyle(styles.Objects[2]));
  AdvRangeSlider2.SetComponentStyle(TTMSStyle(styles.Objects[2]));
end;

procedure TForm1.Label16Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  r: TRect;
  h: integer;
  FDPIScale: single;
begin
  FDPIScale:=Monitor.PixelsPerInch/96;
  //

  R := Paintbox1.ClientRect;
  Paintbox1.Canvas.Brush.Color := clWhite;
  Paintbox1.Canvas.Pen.Color := $F0F0F0;
  Paintbox1.Canvas.Rectangle(r);

  h := r.Height;

  r.Left := Round(AdvRangeSlider1.PositionLeft * 2 * 3 * FDPIScale);
  r.Right := Round(AdvRangeSlider1.PositionRight * 2 * 3 * FDPIScale);
  r.Top := h - Round(AdvRangeSlider2.PositionRight * 2 * 3 * FDPIScale);
  r.Bottom := h- Round(AdvRangeSlider2.PositionLeft * 2 * 3* FDPIScale);


  Paintbox1.Canvas.Brush.Color := clYellow;
  Paintbox1.Canvas.Pen.Color := clRed;

  Paintbox1.Canvas.Rectangle(R);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
var
  ts: TThumbShape;
begin
  ts := TThumbShape(RadioGroup1.ItemIndex);

  AdvRangeSlider1.ThumbLeft.Shape := ts;
  AdvRangeSlider1.ThumbRight.Shape := ts;

  AdvRangeSlider2.ThumbLeft.Shape := ts;
  AdvRangeSlider2.ThumbRight.Shape := ts;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);

begin
  AdvRangeSlider1.SetComponentStyle(TTMSStyle(RadioGroup2.Items.Objects[RadioGroup2.ItemIndex]));
  AdvRangeSlider2.SetComponentStyle(TTMSStyle(RadioGroup2.Items.Objects[RadioGroup2.ItemIndex]));
end;

end.
