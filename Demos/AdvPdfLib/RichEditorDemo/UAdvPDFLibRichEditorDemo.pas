{***************************************************************************}
{ TAdvPDFLib sample application                                             }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2019                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                              }
{***************************************************************************}

unit UAdvPDFLibRichEditorDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, AdvRichEditorToolBar, AdvToolBar,
  AdvToolBarExt, AdvScrollControl, AdvRichEditorBase, AdvRichEditor, ShellAPI,
  StdCtrls, AdvRichEditorPDFIO, AdvPDFIO, AdvCustomComponent, Vcl.ExtCtrls,
  AdvAppStyler, AdvToolBarStylers;

type
  TForm1 = class(TForm)
    AdvRichEditor1: TAdvRichEditor;
    AdvRichEditorFormatToolBar1: TAdvRichEditorFormatToolBar;
    AdvRichEditorEditingToolBar1: TAdvRichEditorEditingToolBar;
    btnGeneratePdf: TButton;
    AdvRichEditorEditToolBar1: TAdvRichEditorEditToolBar;
    AdvRichEditorPDFIO1: TAdvRichEditorPDFIO;
    Panel1: TPanel;
    Label16: TLabel;
    Panel2: TPanel;
    Label1: TLabel;
    AdvToolBarOfficeStyler1: TAdvToolBarOfficeStyler;
    procedure btnGeneratePdfClick(Sender: TObject);
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

procedure TForm1.btnGeneratePdfClick(Sender: TObject);
begin
  AdvRichEditorPDFIO1.Options.OpenInPDFReader := true;
  AdvRichEditorPDFIO1.Save('.\sample.pdf');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AdvRichEditor1.LoadFromFile('.\sample.rte');

end;

procedure TForm1.Label16Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
