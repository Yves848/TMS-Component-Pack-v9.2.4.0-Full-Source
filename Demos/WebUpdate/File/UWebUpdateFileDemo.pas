unit UWebUpdateFileDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, wupdate, ShellAPI;

type
  TForm1 = class(TForm)
    WebUpdate1: TWebUpdate;
    Label1: TLabel;
    Button1: TButton;
    Label19: TLabel;
    Label18: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure WebUpdate1Status(Sender: TObject; statusstr: String;
      statuscode, errcode: Integer);
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
 webupdate1.DoUpdate;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.WebUpdate1Status(Sender: TObject; statusstr: String;
  statuscode, errcode: Integer);
begin
 showmessage(statusstr);
end;

end.
