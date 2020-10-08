unit UAdvShaperDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvShaper, Vcl.Imaging.pngimage, ShellAPI,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm13 = class(TForm)
    AdvShaper1: TAdvShaper;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Button1: TButton;
    procedure Image1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form13: TForm13;

implementation

{$R *.dfm}

procedure TForm13.Button1Click(Sender: TObject);
begin
 Close;
end;

procedure TForm13.Image1Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm13.Image2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm13.Image3Click(Sender: TObject);
begin
  ShowMessage('Temperature Clicked');
end;

procedure TForm13.Image4Click(Sender: TObject);
begin
  ShowMessage('Lights Clicked');
end;

end.
