{********************************************************************}
{ TAdvNavBar demo                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2005 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvNavBarDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, AdvStyleIF,
  Dialogs, ExtCtrls, AdvNavBar, ImgList, ComCtrls, StdCtrls, TeEngine,
  Series, TeeProcs, Chart, CheckLst, Menus, ShellAPI, System.ImageList;

type
  TForm1 = class(TForm)
    AdvNavBar1: TAdvNavBar;
    AdvNavBarPanel1: TAdvNavBarPanel;
    AdvNavBarPanel2: TAdvNavBarPanel;
    AdvNavBarPanel3: TAdvNavBarPanel;
    ImageList1: TImageList;
    AdvNavBarPanel4: TAdvNavBarPanel;
    AdvNavBarPanel5: TAdvNavBarPanel;
    MonthCalendar1: TMonthCalendar;
    TreeView1: TTreeView;
    ImageList2: TImageList;
    Edit1: TEdit;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckListBox1: TCheckListBox;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    PopupMenu1: TPopupMenu;
    Moreoptions1: TMenuItem;
    Button1: TButton;
    Infolabel: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox1: TComboBox;
    Label6: TLabel;
    procedure AdvNavBar1PopupMenuClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure AdvNavBar1SplitterMove(Sender: TObject; OldSplitterPosition,
      NewSplitterPosition: Integer);
    procedure AdvNavBar1PanelActivate(Sender: TObject; OldActivePanel,
      NewActivePanel: Integer; var Allow: Boolean);
    procedure Moreoptions1Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AdvNavBar1PopupMenuClick(Sender: TObject);
var
  pt: tpoint;
begin
  pt := point(advnavbar1.Left + advnavbar1.Width, advnavbar1.Top + advnavbar1.Height);
  pt.X := pt.X - 8;
  pt.Y := pt.Y - 8;
  pt := ClientToScreen(pt);

  popupmenu1.Popup(pt.X,pt.Y);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  advnavbar1.ActiveTabIndex := 2;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  AdvNavBar1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex:=0;
  AdvNavBar1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[0]));
end;

procedure TForm1.Label5Click(Sender: TObject);
begin
ShellExecute(handle,'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.AdvNavBar1SplitterMove(Sender: TObject;
  OldSplitterPosition, NewSplitterPosition: Integer);
begin
  InfoLabel.Caption := 'Active panel:'+IntToStr(AdvNavBar1.ActivePanel.PanelIndex)+' - splitter : '+IntToStr(NewSplitterPosition);
end;

procedure TForm1.AdvNavBar1PanelActivate(Sender: TObject; OldActivePanel,
  NewActivePanel: Integer; var Allow: Boolean);
begin
  InfoLabel.Caption := 'Active panel:'+IntToStr(NewActivePanel)+' - splitter : '+IntToStr(AdvNavBar1.SplitterPosition);
end;

procedure TForm1.Moreoptions1Click(Sender: TObject);
begin
  ShowMessage('More options can be set from here ...');
end;

end.
