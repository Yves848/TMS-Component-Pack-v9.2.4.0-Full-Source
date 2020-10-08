{********************************************************************}
{ TAdvPdfLib demo                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvPdfLibMemoDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, AdvMemo, advmemopdfio,
  AdvmPS, ExtCtrls, AdvPDFIO, AdvCustomComponent;

type
  TForm1 = class(TForm)
    AdvMemo1: TAdvMemo;
    AdvPascalMemoStyler1: TAdvPascalMemoStyler;
    AdvMemoPDFIO1: TAdvMemoPDFIO;
    Panel1: TPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    Edit3: TEdit;
    SaveDialog1: TSaveDialog;
    Panel2: TPanel;
    Label16: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Label16Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ShellAPI;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  fn: string;
begin
  if SaveDialog1.Execute then
  begin
    fn := SaveDialog1.FileName;

    if ExtractFileExt(fn) = '' then
      fn := fn + '.PDF';

    AdvMemoPDFIO1.Options.Header := Edit1.Text;
    AdvMemoPDFIO1.Options.Footer := Edit2.Text;
    AdvMemoPDFIO1.Information.Author := Edit3.Text;

    AdvMemoPDFIO1.Save(fn);

    ShellExecute(0,'open',PChar(fn),nil,nil,SW_NORMAL);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AdvMemo1.Lines.LoadFromFile('.\UAdvPdfLibMemoDemo.pas');
end;

procedure TForm1.Label16Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
