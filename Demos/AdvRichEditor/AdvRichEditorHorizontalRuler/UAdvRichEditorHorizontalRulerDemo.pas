unit UAdvRichEditorHorizontalRulerDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvScrollControl, AdvRichEditorBase,
  AdvRichEditor, AdvCustomControl, AdvRichEditorRuler, Vcl.StdCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    AdvRichEditorHorizontalRuler1: TAdvRichEditorHorizontalRuler;
    AdvRichEditor1: TAdvRichEditor;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  AdvRichEditor1.SaveToFile('.\ruler.rte');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if FileExists('.\ruler.rte') then
  begin
    AdvRichEditor1.LoadFromFile('.\ruler.rte');
  end;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://www.tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
