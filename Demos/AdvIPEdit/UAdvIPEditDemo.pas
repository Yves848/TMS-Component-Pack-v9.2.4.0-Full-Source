unit UAdvIPEditDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Mask, AdvEdit,
  AdvIPEdit, ShellAPI;

type
  TForm92 = class(TForm)
    AdvIPEdit1: TAdvIPEdit;
    ComboBox1: TComboBox;
    Button1: TButton;
    Label1: TLabel;
    procedure ComboBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form92: TForm92;

implementation

{$R *.dfm}

procedure TForm92.Button1Click(Sender: TObject);
begin
  MessageDlg('IP address: ' + AdvIPEdit1.IPAddress, mtInformation, [mbOK], 0);
end;

procedure TForm92.ComboBox1Change(Sender: TObject);
begin
  case (Sender as TComboBox).ItemIndex of
    0:
    begin
      AdvIPEdit1.IPAddressType := ipv4;
      AdvIPEdit1.IPAddress := '192.168.1.120';
      AdvIPEdit1.Width := Round(120*Monitor.PixelsPerInch/96);
      Button1.Left := Round(190*Monitor.PixelsPerInch/96);
      Width := Round(305*Monitor.PixelsPerInch/96);

    end;
    1:
    begin
      AdvIPEdit1.IPAddressType := ipv6;
      AdvIPEdit1.IPAddress := '0:0:0:0:ffff:c0a8:178a';
      AdvIPEdit1.Width := Round(300*Monitor.PixelsPerInch/96);
      Button1.Left := Round(370*Monitor.PixelsPerInch/96);
      Width := Round(485*Monitor.PixelsPerInch/96);
    end;
  end;
end;

procedure TForm92.Label1Click(Sender: TObject);
begin
  ShellExecute(handle, 'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
