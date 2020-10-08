{********************************************************************}
{ TMS TAdvSearchList Demo                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright � 2001 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UThumbnailListDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileCtrl, ThumbnailList, ShellAPI;

type
  TForm1 = class(TForm)
    DriveComboBox1: TDriveComboBox;
    dlb: TDirectoryListBox;
    tl: TThumbnailList;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    Label19: TLabel;
    Label18: TLabel;
    procedure dlbDblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure tlClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
    lastDir: string;
    RunningDir: string;
  public
    { Public declarations }
    procedure ShowDir;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
begin
  tl.BeginUpdate;
  tl.Thumbnails.Clear;

  with tl.Thumbnails.Add do
  begin
    caption := 'Baby.jpg';
    picture.LoadFromFile(RunningDir + '\Images\baby.jpg');
  end;
  tl.EndUpdate;  
end;

procedure TForm1.dlbDblClick(Sender: TObject);
begin
  ShowDir;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RunningDir := GetCurrentDir;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowDir;
end;

procedure TForm1.ShowDir;
begin
  if lastDir = dlb.Directory then exit;

  lastDir := dlb.Directory;

  tl.BeginUpdate;
  try
    tl.ShowFolder(dlb.Directory + '\*.*');
  finally
    tl.EndUpdate;
  end;

end;

procedure TForm1.tlClick(Sender: TObject);
begin
  if tl.ItemIndex >= 0 then
  label1.Caption := tl.Thumbnails.Items[tl.ItemIndex].FileName;
//  label1.Caption := tl.Items[tl.ItemIndex];
end;

end.
