{******************************************************************************}
{ TWEBUPDATE component sample project                                          }
{ for Delphi & C++Builder                                                      }
{                                                                              }
{ Uses update control file at : http://www.tmssoftware.com/update/sampapp.inf  }
{                                                                              }
{ written by                                                                   }
{    TMS Software                                                              }
{    copyright © 1998 - 2019                                                   }
{    Email : info@tmssoftware.com                                              }
{    Web   : https://www.tmssoftware.com                                       }
{                                                                              }
{ The source code is given as is. The author is not responsible                }
{ for any possible damage done due to the use of this code.                    }
{ The component can be freely used in any application. The source              }
{ code remains property of the writer and may not be distributed               }
{ freely as such.                                                              }
{******************************************************************************}

unit UWebUpdateSampleDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  wupdate, StdCtrls, uproxy, ExtCtrls, uselcomp, ComCtrls, Registry, ShellAPI;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    WebUpdate1: TWebUpdate;
    Threaded: TCheckBox;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
    Button2: TButton;
    Dlgs: TCheckBox;
    Image1: TImage;
    Label3: TLabel;
    Label19: TLabel;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure WebUpdate1Status(Sender: TObject; statusstr: String;
      statuscode, errcode: Integer);
    procedure WebUpdate1AppRestart(Sender: TObject; var allow: Boolean);
    procedure WebUpdate1FileProgress(Sender: TObject; filename: String;
      pos, size: Integer);
    procedure Button2Click(Sender: TObject);
    procedure WebUpdate1GetFileList(Sender: TObject; list: TStringList);
    procedure FormCreate(Sender: TObject);
    procedure WebUpdate1AppDoClose(Sender: TObject);
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
var
  VerInfo: TOSVersionInfo;
  OSVersion: string;
begin
  ListBox1.Clear;

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  OSVersion := IntToStr(verinfo.dwMajorVersion) + ':' + IntToStr(verinfo.dwMinorVersion);

  WebUpdate1.PostUpdateInfo.Data :=
    WebUpdate1.PostUpdateInfo.Data + '&TIME=' + FormatDateTime('dd/mm/yyyy@hh:nn', Now) + '&OS=' + OSVersion;

  if Threaded.Checked then
    WebUpdate1.DoThreadupdate
  else
    WebUpdate1.DoUpdate;
end;

procedure TForm1.WebUpdate1Status(Sender: TObject; StatusStr: String;
  Statuscode, Errcode: Integer);
begin
  ListBox1.Items.Add(StatusStr);

  if Dlgs.Checked then
    MessageDlg(StatusStr, mtInformation, [mbok], 0);

  if StatusCode = WebUpdateNoNewVersion then
    MessageDlg('No new version available', mtinformation, [mbok], 0);

  if StatusCode = WebUpdateNotFound then
    MessageDlg(StatusStr + #13'Update discontinued', mtinformation, [mbok], 0);
end;

procedure TForm1.WebUpdate1AppRestart(Sender: TObject; var allow: Boolean);
begin
  Allow := MessageDlg('Shutting down application to update executable files ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TForm1.WebUpdate1FileProgress(Sender: TObject; filename: String;
  pos, size: Integer);
begin
  progressbar1.max := size;
  progressbar1.position := pos;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  proxy: tproxy;
begin
  proxy := tproxy.Create(self);
  with proxy do
  begin
    Edit1.Text := WebUpdate1.Proxy;
    Edit2.Text := WebUpdate1.ProxyUserID;
    Edit3.Text := WebUpdate1.ProxyPassword;
  end;
  try
  if proxy.ShowModal = mrOk then
  begin
    WebUpdate1.Proxy := proxy.Edit1.Text;
    WebUpdate1.ProxyUserID := proxy.Edit2.Text;
    WebUpdate1.ProxyPassword := proxy.Edit1.Text;
  end;
  finally
   proxy.free;
 end;
end;

procedure TForm1.WebUpdate1GetFileList(Sender: TObject; list: TStringList);
var
  i: integer;
  selcomp: TSelcomp;
  mr: integer;
begin
  selcomp := TSelcomp.create(self);
  try
    SelComp.Checklist.items.Assign(list);

    for i := 1 to list.Count do
      SelComp.Checklist.Checked[i - 1] := True;

    mr := selcomp.ShowModal;

    for i := 1 to list.count do
    begin
      if (mr = mrCancel) or
        (not SelComp.checklist.Checked[i - 1]) then
          list.strings[i-1] := '';
    end;
  finally
    SelComp.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image1.Picture.loadfromfile('logo.bmp');
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.WebUpdate1AppDoClose(Sender: TObject);
begin
  Close;
end;

end.
