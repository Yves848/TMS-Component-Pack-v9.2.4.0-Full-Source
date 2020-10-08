{********************************************************************}
{ TMS TAdvRichEditor Demo                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{  DEMO REQUIRES TMS COMPONENT PACK V8.7.3.0 OR NEWER                }
{                                                                    }
{********************************************************************}

unit UAdvRichEditorMailClientDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdMessage, Vcl.StdCtrls,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdExplicitTLSClientServerBase, IdMessageClient, IdSMTPBase, IdSMTP, idText, idAttachmentFile,
  AdvRichEditorIO, AdvScrollControl, AdvRichEditorBase, AdvRichEditor, ShellAPI,
  AdvToolBar, AdvToolBarExt, AdvRichEditorToolBar, AdvRichEditorEmailIO,
  AdvToolBarStylers, System.ImageList, Vcl.ImgList, AdvEdit, Vcl.ExtCtrls;

type
  TForm3 = class(TForm)
    AdvRichEditor1: TAdvRichEditor;
    AdvRichEditorFormatToolBar1: TAdvRichEditorFormatToolBar;
    AdvToolBarOfficeStyler1: TAdvToolBarOfficeStyler;
    ImageList1: TImageList;
    edMailFrom: TAdvEdit;
    edMailTo: TAdvEdit;
    edSubject: TAdvEdit;
    Panel1: TPanel;
    btnEmail: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    edMailHost: TAdvEdit;
    Label1: TLabel;
    Label3: TLabel;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnEmailClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Integer);
    procedure Label3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.btnSaveClick(Sender: TObject);
begin
  AdvRichEditor1.SaveToFile('.\richeditor.rte');
end;

procedure TForm3.btnEmailClick(Sender: TObject);
var
  emailio: TAdvRichEditorEmailIO;
  idMessage: TidMessage;
  idSmtp: TidSMTP;
begin
  if edMailHost.Text = '' then
  begin
    //raise Exception.Create('No mail host specified');
    ShowMessage('No mail host specified');
    Exit;
  end;

  if edMailTo.Text = '' then
  begin
    //raise Exception.Create('No email recipient specified');
    ShowMessage('No email recipient specified');
    Exit;
  end;

  if edMailFrom.Text = '' then
  begin
    //raise Exception.Create('No email sender specified');
    ShowMessage('No email sender specified');
    Exit;
  end;

  emailio := TAdvRichEditorEmailIO.Create(Self);
  emailio.RichEditor := AdvRichEditor1;

  idMessage := TidMessage.Create(self);
  idSmtp := TidSmtp.Create(Self);
  try
    emailio.GenerateEmail(idmessage);
    idMessage.Subject := edSubject.Text;
    IdMessage.From.Address := edMailFrom.Text;
    IdMessage.Recipients.Add.Address := edMailTo.Text;
    IdMessage.Sender.Address := edMailFrom.Text;

    idsmtp.Host := edMailHost.Text;
    Idsmtp.Connect;
    idsmtp.Send(idMessage);
    idsmtp.Disconnect();
    ShowMessage('Your email was sent.');
  finally
    emailio.Free;
    idSmtp.Free;
    idMessage.Free;
  end;
end;

procedure TForm3.btnLoadClick(Sender: TObject);
begin
  AdvRichEditor1.LoadFromFile('.\richeditor.rte');
end;

procedure TForm3.FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
  MaxWidth, MaxHeight: Integer);
begin
  MinWidth := 700;
  MinHeight := 470;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  AdvRichEditor1.LoadFromFile('.\richeditor.rte');
end;

procedure TForm3.Label3Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
