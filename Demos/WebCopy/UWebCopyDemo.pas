{***************************************************************************}
{ TMS TWebCopy Demo                                                         }
{                                                                           }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2000 - 2019                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                              }
{                                                                           }
{***************************************************************************}

unit UWebCopyDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  WebCopy, Grids, StdCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    WebCopy1: TWebCopy;
    Label1: TLabel;
    Button1: TButton;
    tgt: TEdit;
    Label2: TLabel;
    Label19: TLabel;
    Label18: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure WebCopy1FileDone(Sender: TObject; idx: Integer);
    procedure Button1Click(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  tgt.Text := GetCurrentDir;

  with stringgrid1 do
  begin
    cells[1,0] := 'URL';
    cells[1,1] := 'http://www.tmssoftware.biz/download/updatebuilder.zip';
    cells[1,2]:='http://www.tmssoftware.net/public/FTPUploader.zip';
  end;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.WebCopy1FileDone(Sender: TObject; idx: Integer);
begin
  StringGrid1.Cells[0, idx + 1] := '*';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
 i: Integer;
begin
  WebCopy1.Items.Clear;
  for i := 1 to stringgrid1.rowcount-1 do
  begin
    StringGrid1.Cells[0, i] := '';
    if StringGrid1.Cells[1, i] <> '' then
      with WebCopy1.Items.Add do
      begin
        url := StringGrid1.Cells[1, i];
        TargetDir := tgt.Text;
      end;
  end;
  WebCopy1.ThreadExecute;
end;

end.
