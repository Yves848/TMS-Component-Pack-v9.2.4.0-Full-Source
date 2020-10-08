unit UAdvSignatureCaptureDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ColorGrd, Vcl.StdCtrls, ShellAPI,
  AdvSignatureCapture, Vcl.ExtCtrls;

type
  TForm4 = class(TForm)
    AdvSignatureCapture1: TAdvSignatureCapture;
    Button1: TButton;
    Label1: TLabel;
    ColorGrid1: TColorGrid;
    Label2: TLabel;
    PaintBox1: TPaintBox;
    Label3: TLabel;
    SaveDialog1: TSaveDialog;
    Label4: TLabel;
    Label5: TLabel;
    procedure ColorGrid1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
begin
  if savedialog1.Execute then
    AdvSignatureCapture1.SaveToImageFile(savedialog1.FileName);
end;

procedure TForm4.ColorGrid1Click(Sender: TObject);
begin
  AdvSignatureCapture1.Pen.Color := ColorGrid1.ForegroundColor;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  AdvSignatureCapture1.Pen.Width := 1;
end;

procedure TForm4.Label5Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm4.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  i := (Y + 5) div 10;
  AdvSignatureCapture1.Pen.Width := i;
  PaintBox1.Invalidate;
end;

procedure TForm4.PaintBox1Paint(Sender: TObject);
var
  i: integer;

  procedure DrawLine(Y, W: integer; Selected: boolean);
  begin
    if Selected then
      PaintBox1.Canvas.Pen.Color := clBlue
    else
      PaintBox1.Canvas.Pen.Color := clBlack;

    PaintBox1.Canvas.Pen.Width := W;

    PaintBox1.Canvas.MoveTo(5, Y);
    PaintBox1.Canvas.LineTo(PaintBox1.Width - 5, Y);
  end;

begin
  for i := 1 to 7 do
  begin
    DrawLine(i * 10,i, AdvSignatureCapture1.Pen.Width = i);
  end;
end;

end.
