{*************************************************************************}
{ TMS TWebData Demo                                                       }
{                                                                         }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2001 - 2019                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{*************************************************************************}

unit UWebDataPageScanDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ShellAPI,
  Dialogs, WebImage, Buttons, StdCtrls, ComCtrls, ExtCtrls, WebData;

type
  TForm1 = class(TForm)
    WebData1: TWebData;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Panel2: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    WebImage1: TWebImage;
    Panel3: TPanel;
    Label19: TLabel;
    Label18: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  i: Integer;
begin
  WebData1.Data.Clear;
  with WebData1.Data.Add do
  begin
    if (pos('HTTP://',uppercase(edit1.Text)) = 0) and (pos('HTTPS://',uppercase(edit1.Text)) = 0) then
      url := 'http://'+edit1.Text
    else
      url := edit1.Text;
    OutputDebugString(pchar(url));
  end;
  WebData1.Execute;
  Listbox1.Items.Clear;

  for i := 0 to WebData1.ImageRefs.Count - 1 do
  begin
    if (pos('.JPG',uppercase(WebData1.ImageRefs[i])) > 0) or (pos('.GIF',uppercase(WebData1.ImageRefs[i])) > 0) then
      ListBox1.Items.Add(WebData1.ImageRefs[i]);
  end;

  Listbox2.Items.Assign(WebData1.Hyperlinks);
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if listbox1.itemindex >= 0 then
   webimage1.Url := listbox1.items[listbox1.ItemIndex];
  OutputDebugString(pchar(listbox1.items[listbox1.ItemIndex]));
end;

procedure TForm1.ListBox2Click(Sender: TObject);
begin
  if listbox2.itemindex >= 0 then
  begin
    if pos('.JPG',uppercase(listbox2.Items[Listbox2.ItemIndex])) > 0 then
       webimage1.Url := listbox2.Items[Listbox2.ItemIndex]
    else
    if pos('.GIF',uppercase(listbox2.Items[Listbox2.ItemIndex])) > 0 then
       webimage1.Url := listbox2.Items[Listbox2.ItemIndex]
    else
      edit1.Text := listbox2.Items[Listbox2.ItemIndex];
  end;

end;

end.
