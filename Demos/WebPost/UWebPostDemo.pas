{********************************************************************}
{ TMS TWebPost Demo                                                  }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2001 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UWebPostDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  WebPost, StdCtrls, ShellApi;

type
  TForm1 = class(TForm)
    WebPost1: TWebPost;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Label3: TLabel;
    ComboBox3: TComboBox;
    Label4: TLabel;
    Button1: TButton;
    Label5: TLabel;
    ComboBox4: TComboBox;
    Edit2: TEdit;
    Label19: TLabel;
    Label18: TLabel;
    procedure FormCreate(Sender: TObject);
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
 ComboBox1.ItemIndex := 0;
 ComboBox2.ItemIndex := 0;
 ComboBox3.ItemIndex := 0;
 ComboBox4.ItemIndex := 0;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
 wpi: TWebPostItem;
begin
 WebPost1.Items.Clear;

 with WebPost1.Items do
  begin
   wpi := Add;
   wpi.Name := 'DEVTOOL';
   wpi.Value := ComboBox1.Text;
   wpi := Add;
   wpi.Name := 'OS';
   wpi.Value := ComboBox2.Text;
   wpi := Add;
   wpi.Name := 'BROWSER';
   wpi.Value := ComboBox3.Text;
   wpi := Add;
   wpi.Name := 'CPU';
   wpi.Value := ComboBox4.Text;
   wpi := Add;
   wpi.Name := 'COMMENT';
   wpi.Value := Edit2.Text;
  end;

 if WebPost1.Execute then
  begin
   WebPost1.SaveToFile('results.htm');
   ShellExecute(Handle, 'open', 'results.htm', nil, nil, SW_NORMAL);
  end;
end;

end.
