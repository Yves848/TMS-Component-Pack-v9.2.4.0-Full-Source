{********************************************************************}
{ TMS TAdvSmoothCircularProgress Demo                                }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothCircularProgressDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, AdvSmoothCircularProgress, StdCtrls,
  ExtCtrls, Math, AdvStyleIF, ShellAPI;

type
  TForm832 = class(TForm)
    AdvSmoothCircularProgress1: TAdvSmoothCircularProgress;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    AdvSmoothCircularProgress2: TAdvSmoothCircularProgress;
    AdvSmoothCircularProgress3: TAdvSmoothCircularProgress;
    AdvSmoothCircularProgress4: TAdvSmoothCircularProgress;
    Timer1: TTimer;
    Label1: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form832: TForm832;

implementation

{$R *.dfm}

procedure TForm832.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
  0: AdvSmoothCircularProgress1.ApplyDefaultStyle;
  else
    AdvSmoothCircularProgress1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
  end;
end;

procedure TForm832.FormCreate(Sender: TObject);
begin
  Label1.Caption := 'Click on the scale of the bottom'+#13#10 +'circular progress for interaction.';
  Label1.Left := Panel1.Width - Label1.Width - 5;
  Label1.Top := (Panel1.Height - Label1.Height) div 2;

  FillStyleList(ComboBox1.Items);
  ComboBox1.Items.Insert(0,'Default');
  ComboBox1.ItemIndex := 0;
end;

procedure TForm832.Label3Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm832.Timer1Timer(Sender: TObject);
begin
  AdvSmoothCircularProgress1.Position := RandomRange(Round(AdvSmoothCircularProgress1.Minimum), Round(AdvSmoothCircularProgress1.Maximum));
  AdvSmoothCircularProgress3.Position := Random(100);
end;

end.

