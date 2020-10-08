{********************************************************************}
{ TMS TAdvResponsiveList Demo                                        }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UAdvResponsiveListDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvScrollControl, AdvResponsiveList,
  Vcl.StdCtrls, AdvGraphics, ShellAPI;

type
  TForm1 = class(TForm)
    AdvResponsiveList1: TAdvResponsiveList;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
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

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  AdvResponsiveList1.MultiSelect := CheckBox1.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  sl,col: TStringList;
  i: integer;
begin
  ReportMemoryLeaksOnShutdown := true;

  AdvResponsiveList1.Items.Clear;

  sl := TStringList.Create;
  col := TStringList.Create;
  col.StrictDelimiter := true;
  col.Delimiter := ',';

  AdvResponsiveList1.BeginUpdate;

  try
    sl.LoadFromFile('.\carlist.txt');

    for i := 0 to sl.Count - 1 do
    begin
      col.CommaText := sl.Strings[i];
      AdvResponsiveList1.Items.Add.Content := '<B>'+col[0]+'</B>'+'<BR><IMG SRC="file://.\'+col[2]+'"><BR><BR>'+col[1];
    end;

  finally
    AdvResponsiveList1.EndUpdate;
    col.Free;
    sl.Free;
  end;
end;

procedure TForm1.Label3Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
