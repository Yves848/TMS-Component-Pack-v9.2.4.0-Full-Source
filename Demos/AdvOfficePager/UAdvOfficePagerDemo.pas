{********************************************************************}
{ TMS TAdvOfficePager Demo                                           }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvOfficePagerDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvOfficePager, ImgList, AdvOfficePagerStylers, StdCtrls, ExtCtrls,
  ComCtrls, System.ImageList, AdvStyleIF, ShellAPI, Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    Label2: TLabel;
    ComboBox1: TComboBox;
    AdvOfficePager1: TAdvOfficePager;
    AdvOfficePagerOfficeStyler1: TAdvOfficePagerOfficeStyler;
    ImageList1: TImageList;
    AdvOfficePage1: TAdvOfficePage;
    AdvOfficePage2: TAdvOfficePage;
    AdvOfficePage3: TAdvOfficePage;
    AdvOfficePage4: TAdvOfficePage;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Memo1: TMemo;
    TreeView1: TTreeView;
    Label3: TLabel;
    Image1: TImage;
    RadioGroup2: TRadioGroup;
    CheckBox3: TCheckBox;
    Label1: TLabel;
    Label5: TLabel;
    procedure CheckBox3Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label5Click(Sender: TObject);
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
if CheckBox1.Checked = true then
begin
    AdvOfficePager1.ButtonSettings.CloseButton := true;
end
else
begin
    AdvOfficePager1.ButtonSettings.CloseButton := false;
end;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
if CheckBox2.Checked = true then
begin
    AdvOfficePager1.CloseOnTab := true;
end
else
begin
    AdvOfficePager1.CloseOnTab := false;
end;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
if CheckBox3.Checked = true then
begin
    AdvOfficePager1.ShowNonSelectedTabs := true;
end
else
begin
    AdvOfficePager1.ShowNonSelectedTabs := false;
end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);

begin
  AdvOfficePagerOfficeStyler1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Clear;
  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex:=0;
  AdvOfficePagerOfficeStyler1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));

end;

procedure TForm1.Label5Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
var
  FDPIScale : Single;
begin
  FDPIScale:=Monitor.PixelsPerInch/96;

  case RadioGroup2.ItemIndex of
  0: begin
       AdvOfficePager1.RotateTabLeftRight := true;
       AdvOfficePager1.TabSettings.Height := Round(26*FDPIScale);
       AdvOfficePager1.TabPosition := tpTop;
       Memo1.Width := Round(335*FDPIScale);
       Memo1.Height := Round(195*FDPIScale);
       Memo1.ScrollBars := ssNone;
       TreeView1.Width := Round(335*FDPIScale);
       TreeView1.Height := Round(195*FDPIScale);
       Image1.Left := Round(165*FDPIScale);
     end;
  1: begin
       AdvOfficePager1.RotateTabLeftRight := true;
       AdvOfficePager1.TabSettings.Height := Round(26*FDPIScale);
       AdvOfficePager1.TabPosition := tpLeft;
       Memo1.Width := Round(310*FDPIScale);
       Memo1.Height := Round(220*FDPIScale);
       Memo1.ScrollBars := ssVertical;
       TreeView1.Width := Round(310*FDPIScale);
       TreeView1.Height := Round(220*FDPIScale);
       Image1.Left := Round(145*FDPIScale);
       Image1.Top := Round(85*FDPIScale)
     end;
  2: begin
       AdvOfficePager1.RotateTabLeftRight := false;
       AdvOfficePager1.TabSettings.Height := Round(125/FDPIScale);
       AdvOfficePager1.TabPosition := tpLeft;
       Memo1.Left:= Round(10*FDPIScale);
       Memo1.Width := Round(235*FDPIScale);
       Memo1.Height := Round(225*FDPIScale);
       TreeView1.Width := Round(232*FDPIScale);
       TreeView1.Height := Round(225*FDPIScale);
       Memo1.ScrollBars := ssVertical;
       Image1.Left := Round(100*FDPIScale);
     end;
  end;
end;

end.
