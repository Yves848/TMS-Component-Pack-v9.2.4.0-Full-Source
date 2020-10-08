{********************************************************************}
{ TMS TAdvRichEditor Demo                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UAdvRichEditorRibbonDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvToolBar, StdActns, ActnList, AdvToolBarExt, AdvGlowButton, PNGImage,
  StdCtrls, ExtActns, AdvRichEditorToolBar, AdvScrollControl, AdvRichEditorBase,
  AdvRichEditor, AdvToolBarStylers, AdvOfficeHint, AdvAppStyler, ShellAPI,
  PlatformDefaultStyleActnCtrls, ActnMan, System.Actions, Vcl.ExtCtrls;

type
  TForm1 = class(TAdvToolBarForm)
    AdvGlowButton1: TAdvGlowButton;
    AdvToolBarPager1: TAdvToolBarPager;
    AdvToolBarPager11: TAdvPage;
    AdvToolBarPager12: TAdvPage;
    AdvRichEditor1: TAdvRichEditor;
    AdvRichEditorClipboardRibbonToolBar1: TAdvRichEditorClipboardRibbonToolBar;
    AdvRichEditorFontRibbonToolBar1: TAdvRichEditorFontRibbonToolBar;
    AdvToolBarOfficeStyler1: TAdvToolBarOfficeStyler;
    AdvOfficeHint1: TAdvOfficeHint;
    AdvFormStyler1: TAdvFormStyler;
    AdvRichEditorParagraphRibbonToolBar1: TAdvRichEditorParagraphRibbonToolBar;
    AdvToolBar2: TAdvToolBar;
    AdvGlowButton2: TAdvGlowButton;
    AdvGlowButton3: TAdvGlowButton;
    ActionManager1: TActionManager;
    AdvRichEditorEditingRibbonToolBar1: TAdvRichEditorEditingRibbonToolBar;
    Panel1: TPanel;
    Label3: TLabel;
    Label1: TLabel;
    procedure AdvGlowButton1Click(Sender: TObject);
    procedure AdvGlowButton3Click(Sender: TObject);
    procedure AdvGlowButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  ExtDlgs;

procedure TForm1.AdvGlowButton1Click(Sender: TObject);
var
  rb: TAdvClipboardRibbonToolBar;
begin
  rb := TAdvClipboardRibbonToolBar.Create(Self);
  rb.Parent := AdvToolBarPager11;
end;

procedure TForm1.AdvGlowButton2Click(Sender: TObject);
var
  pd: TOpenPictureDialog;
begin
  if Assigned(AdvRichEditor1) then
  begin
    pd := TOpenPictureDialog.Create(Self);
    try
      if pd.Execute then
      begin
        AdvRichEditor1.InsertImage(pd.FileName);
        AdvRichEditor1.SetFocus;
      end;
    finally
      pd.Free;
    end;
  end;
end;

procedure TForm1.AdvGlowButton3Click(Sender: TObject);
var
  url: string;
begin
  url := '';
  InputQuery('Hyperlink','URL',url);
  AdvRichEditor1.SetSelectionHyperlink(url);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AdvRichEditor1.AlwaysFocus := true;
end;

procedure TForm1.Label3Click(Sender: TObject);
begin
  ShellExecute(handle,'open','http://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
