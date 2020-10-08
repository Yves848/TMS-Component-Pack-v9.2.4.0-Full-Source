{********************************************************************}
{ TMS AdvEdit Demo                                                   }
{ for Delphi & C++Builder                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvEditBtnDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ShellAPI,
  Dialogs, StdCtrls, AdvEdit, AdvEdBtn, AdvDirectoryEdit, AdvFileNameEdit;

type
  TForm1 = class(TForm)
    Label9: TLabel;
    Label7: TLabel;
    AdvDirectoryEdit2: TAdvDirectoryEdit;
    AdvFileNameEdit2: TAdvFileNameEdit;
    AdvEditBtn4: TAdvEditBtn;
    AdvEditBtn3: TAdvEditBtn;
    AdvEditBtn2: TAdvEditBtn;
    AdvEditBtn1: TAdvEditBtn;
    Label6: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Label8: TLabel;
    procedure AdvEditBtn2ClickBtn(Sender: TObject);
    procedure AdvEditBtn4ClipboardCopy(Sender: TObject; value: String;
      var allow: Boolean);
    procedure AdvEditBtn4ClipboardCut(Sender: TObject; value: String;
      var allow: Boolean);
    procedure AdvEditBtn4ClipboardPaste(Sender: TObject; value: String;
      var allow: Boolean);
    procedure Label9Click(Sender: TObject);
    procedure AdvEditBtn3ClickBtn(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.AdvEditBtn2ClickBtn(Sender: TObject);
begin
  AdvEditBtn2.Text := '0';
end;

procedure TForm1.AdvEditBtn3ClickBtn(Sender: TObject);
begin
  ShellExecute(handle, 'open', PChar(AdvEditBtn3.Text), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.AdvEditBtn4ClipboardCopy(Sender: TObject; value: String;
  var allow: Boolean);
begin
  label8.Caption := 'copy : ' + value;
end;

procedure TForm1.AdvEditBtn4ClipboardCut(Sender: TObject; value: String;
  var allow: Boolean);
begin
  label8.Caption := 'cut : ' + value;
end;

procedure TForm1.AdvEditBtn4ClipboardPaste(Sender: TObject; value: String;
  var allow: Boolean);
begin
  label8.Caption := 'paste : ' + value;
end;

procedure TForm1.Label9Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://www.tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
