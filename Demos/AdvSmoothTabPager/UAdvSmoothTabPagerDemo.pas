{********************************************************************}
{ TMS TAdvSmoothTabPager Demo                                        }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothTabPagerDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvSmoothTabPager, ComCtrls, AdvStyleIF, Vcl.ExtCtrls, ShellAPI;

type
  TForm179 = class(TForm)
    AdvSmoothTabPager1: TAdvSmoothTabPager;
    AdvSmoothTabPager11: TAdvSmoothTabPage;
    AdvSmoothTabPager12: TAdvSmoothTabPage;
    AdvSmoothTabPager13: TAdvSmoothTabPage;
    ComboBox2: TComboBox;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    MonthCalendar1: TMonthCalendar;
    Label4: TLabel;
    CheckBox2: TCheckBox;
    Label5: TLabel;
    Panel1: TPanel;
    Label7: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form179: TForm179;

implementation

{$R *.dfm}

procedure TForm179.CheckBox1Click(Sender: TObject);
begin
  AdvSmoothTabPager1.TabReorder := CheckBox1.Checked;
end;

procedure TForm179.CheckBox2Click(Sender: TObject);
begin
  AdvSmoothTabPager11.TabAppearance.Status.Visible := CheckBox2.Checked;
end;

procedure TForm179.ComboBox1Change(Sender: TObject);
begin
  AdvSmoothTabPager1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
  Panel1.Color := AdvSmoothTabPager1.Fill.Color;
end;

procedure TForm179.ComboBox2Change(Sender: TObject);
begin
  AdvSmoothTabPager1.TabPosition := TAdvSmoothTabPosition(ComboBox2.ItemIndex);
end;

procedure TForm179.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex := 2;
  AdvSmoothTabPager1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[2]));
  Panel1.Color := AdvSmoothTabPager1.Fill.Color;
end;

procedure TForm179.Label7Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
