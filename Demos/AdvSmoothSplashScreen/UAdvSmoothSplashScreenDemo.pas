{********************************************************************}
{ TMS TAdvSmoothSplashScreen Demo                                    }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1996 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UAdvSmoothSplashScreenDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvSmoothSplashScreen, Vcl.StdCtrls, ShellAPI;

type
  TForm4 = class(TForm)
    AdvSmoothSplashScreen1: TAdvSmoothSplashScreen;
    Button1: TButton;
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
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
begin
  if AdvSmoothSplashScreen1.Showing then
  begin
    AdvSmoothSplashScreen1.Hide;
    Button1.Caption := 'Show';
  end
  else
  begin
   AdvSmoothSplashScreen1.Show;
   Button1.Caption := 'Hide';
  end;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin

  with AdvSmoothSplashScreen1.Items.Add do
  begin
    BeginUpdate;
    Picture.LoadFromFile('TMS_LOGO.png');
    PosX := (AdvSmoothSplashScreen1.Width - Picture.Width) div 2;
    PosY := 20;
    EndUpdate;
  end;

  AdvSmoothSplashScreen1.ListItemsSettings.Space := 20;
  AdvSmoothSplashScreen1.ListItemsSettings.HTMLFont.Height := -18;
  AdvSmoothSplashScreen1.ListItemsSettings.Rect.Left := 10;
  AdvSmoothSplashScreen1.ListItemsSettings.Rect.Top := 40;
  AdvSmoothSplashScreen1.ListItemsSettings.Rect.Width := AdvSmoothSplashScreen1.Width - 10;
  AdvSmoothSplashScreen1.ListItemsSettings.Rect.Height := AdvSmoothSplashScreen1.Height - 40;

  AdvSmoothSplashScreen1.ProgressBar.Left := 20;
  AdvSmoothSplashScreen1.ProgressBar.Top := AdvSmoothSplashScreen1.Height - 100;
  AdvSmoothSplashScreen1.ProgressBar.Height := 50;
  AdvSmoothSplashScreen1.ProgressBar.Width := AdvSmoothSplashScreen1.Width - 40;
  AdvSmoothSplashScreen1.ProgressBar.Minimum := 0;
  AdvSmoothSplashScreen1.ProgressBar.Maximum := 100;
  AdvSmoothSplashScreen1.ProgressBar.Position := 0;
  AdvSmoothSplashScreen1.ProgressBar.ValueVisible := True;
  AdvSmoothSplashScreen1.ProgressBar.Visible := True;

  AdvSmoothSplashScreen1.Show;

  Sleep(2000);

  AdvSmoothSplashScreen1.ProgressBar.Position := AdvSmoothSplashScreen1.ProgressBar.Position + Random(20);

  with AdvSmoothSplashScreen1.ListItems.Add do
  begin
    BeginUpdate;
    HTMLText := 'Creating form';
    EndUpdate;
  end;

  AdvSmoothSplashScreen1.ProgressBar.Position := AdvSmoothSplashScreen1.ProgressBar.Position + Random(20);

  Sleep(2000);

  with AdvSmoothSplashScreen1.ListItems.Add do
  begin
    BeginUpdate;
    HTMLText := 'Loading initial data';
    EndUpdate;
  end;

  AdvSmoothSplashScreen1.ProgressBar.Position := AdvSmoothSplashScreen1.ProgressBar.Position + Random(20);

  Sleep(2000);

  with AdvSmoothSplashScreen1.ListItems.Add do
  begin
    BeginUpdate;
    HTMLText := 'Retrieving necessary information';
    EndUpdate;
  end;

  AdvSmoothSplashScreen1.ProgressBar.Position := AdvSmoothSplashScreen1.ProgressBar.Position + Random(20);

  Sleep(2000);

  with AdvSmoothSplashScreen1.ListItems.Add do
  begin
    BeginUpdate;
    HTMLText := 'Issues <u><font color="clRed">parsing </font></u> to screen';
    EndUpdate;
  end;

  AdvSmoothSplashScreen1.ProgressBar.Position := AdvSmoothSplashScreen1.ProgressBar.Position + Random(30);

  Sleep(2000);

  with AdvSmoothSplashScreen1.ListItems.Add do
  begin
    BeginUpdate;
    HTMLText := '<font size="18" color="clGreen" ><B>Form is ready to show</B></font>';
    EndUpdate;
  end;

  AdvSmoothSplashScreen1.BeginUpdate;
  AdvSmoothSplashScreen1.ProgressBar.Position := 100;
  AdvSmoothSplashScreen1.EndUpdate;

  Sleep(1000);
end;

procedure TForm4.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
