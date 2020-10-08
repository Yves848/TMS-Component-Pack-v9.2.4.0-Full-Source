{********************************************************************}
{ TAdvPanel demo                                                     }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvPanelDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, advpanel, StdCtrls, htmlbtns, ImgList, htmltext, ComCtrls,
  ColorGrd, ShellAPI, AdvStyleIF, System.ImageList;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    AdvPanel1: TAdvPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    ImageList1: TImageList;
    AdvPanel2: TAdvPanel;
    RadioGroup1: TRadioGroup;
    AdvPanel3: TAdvPanel;
    AdvPanel4: TAdvPanel;
    AdvPanel5: TAdvPanel;
    GroupBox1: TGroupBox;
    ColorGrid1: TColorGrid;
    AdvPanelGroup1: TAdvPanelGroup;
    AdvPanel6: TAdvPanel;
    AdvPanel7: TAdvPanel;
    AdvPanel8: TAdvPanel;
    AdvPanel9: TAdvPanel;
    AdvPanel10: TAdvPanel;
    Button1: TButton;
    Button2: TButton;
    TabSheet4: TTabSheet;
    AdvPanelStyler1: TAdvPanelStyler;
    AdvPanel11: TAdvPanel;
    RadioGroup2: TRadioGroup;
    Button3: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label16: TLabel;
    procedure AdvPanel1AnchorClick(Sender: TObject; Anchor: String);
    procedure AdvPanel1AnchorHint(Sender: TObject; var Anchor: String);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure ColorGrid1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure AdvPanel1EndCollapsExpand(Sender: TObject);
    procedure Label16Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  styleList:TStringList;

implementation

{$R *.DFM}

procedure TForm1.AdvPanel1AnchorClick(Sender: TObject; Anchor: String);
begin
 if anchor='doc' then anchor:='You can open a new Word document from here';
 if anchor='xls' then anchor:='You can open a new Excel sheet from here';
 if anchor='mdb' then anchor:='You can open a new Access database from here';

 ShowMessage(Anchor);
end;

procedure TForm1.AdvPanel1AnchorHint(Sender: TObject; var Anchor: String);
begin
 if anchor='doc' then anchor:='Click here to create a new Word document';
 if anchor='xls' then anchor:='Click here to create a new Excel sheet';
 if anchor='mdb' then anchor:='Click here to create a new Access database';
end;

procedure TForm1.AdvPanel1EndCollapsExpand(Sender: TObject);
begin
  checkbox1.OnClick := nil;
  checkbox1.Checked := AdvPanel1.Collaps;
  checkbox1.OnClick := CheckBox1Click;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  advpanel1.collaps := not advpanel1.collaps;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  advpanel1.cansize:=not advpanel1.cansize;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  advpanel1.canmove:=not advpanel1.canmove;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  advpanel1.visible := not checkbox4.checked;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  advpanel1.Caption.MinMaxButton := not advpanel1.Caption.MinMaxButton;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  advpanel1.Caption.CloseButton := not advpanel1.Caption.CloseButton;
end;

procedure TForm1.CheckBox7Click(Sender: TObject);
begin
  if checkbox7.checked then advpanel1.borderstyle:=bsSingle else advpanel1.borderstyle:=bsNone;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
     advpanel2.Caption.ShadeType := TShadeType(radiogroup1.ItemIndex);
end;

procedure TForm1.ColorGrid1Change(Sender: TObject);
begin
   advpanel2.Caption.Color := colorgrid1.ForegroundColor;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
 styleList:=TStringList.Create;

 FillStyleList(styleList);
 RadioGroup2.Items.Clear;
 for i := 0 to styleList.Count-1 do
 begin
   RadioGroup2.Items.AddObject(styleList.Strings[i],styleList.Objects[i]);
 end;
 RadioGroup2.ItemIndex:=2;
 AdvPanelStyler1.SetComponentStyle(TTMSStyle(RadioGroup2.Items.Objects[2]));

end;

procedure TForm1.Label16Click(Sender: TObject);
begin
  ShellExecute( handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 advpanelgroup1.OpenAllPanels;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  advpanelgroup1.CloseAllPanels;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  AdvPanelStyler1.SetComponentStyle(TTMSStyle(RadioGroup2.Items.Objects[RadioGroup2.ItemIndex]));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ap: TAdvPanel;
begin
  ap := TAdvPanel.Create(AdvPanelGroup1);
  ap.Parent := AdvPanelGroup1;
  ap.Assign(advpanel9);
  ap.Text := 'Created at '+TimeToStr(Now);
  ap.Caption.Text := 'Lampreys';
  ap.Collaps := true;
  ap.FullHeight := 100;
  ap.Collaps := false;
  ap.Caption.Visible := true;
end;

end.
