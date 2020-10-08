unit UOffice2019DemoApplication;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvToolBar, AdvToolBarStylers,
  AdvOfficePager, Vcl.StdCtrls, AdvOfficeSelectors, AdvGlowButton, AdvStyleIF,
  AdvOfficePagerStylers, AdvToolBarExt, AdvRichEditorToolBar, AdvShapeButton,
  AdvCombo, System.ImageList, Vcl.ImgList, AdvOfficeComboBox, AdvSmoothListBox,
  AdvSmoothComboBox, AdvAppStyler, AdvOfficeStatusBar, AdvOfficeStatusBarStylers,
  AdvPanel, Planner, Vcl.ExtCtrls, AdvNavBar, Vcl.ComCtrls, AdvProgr, AdvMemo,
  AdvSmoothMenu, AdvUtil, Vcl.Grids, AdvObj, BaseGrid, AdvGrid,
  AdvSmoothImageListBox, AdvSmoothLabel, AdvSmoothGauge, AdvSmoothCalculator,
  AdvSmoothButton, AdvSmoothProgressBar, AdvSmoothCalendar, AdvSmoothSlider,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, AdvTypes, GDIPPictureContainer,
  TodoList, AdvSmoothSlideShow, Winapi.ShellAPI, PictureContainer,
  Office2019Frame, GDIPCustomItem;

type
  TForm6 = class(TForm)
    AdvToolBarPager1: TAdvToolBarPager;
    AdvToolBarPager11: TAdvPage;
    AdvToolBarPager12: TAdvPage;
    AdvToolBarPager13: TAdvPage;
    AdvFormStyler1: TAdvFormStyler;
    AdvShapeButton1: TAdvShapeButton;
    AdvNavBar1: TAdvNavBar;
    AdvNavBarPanel1: TAdvNavBarPanel;
    AdvNavBarPanel2: TAdvNavBarPanel;
    AdvNavBarPanel3: TAdvNavBarPanel;
    Planner1: TPlanner;
    AdvOfficeStatusBar1: TAdvOfficeStatusBar;
    AdvPanel1: TAdvPanel;
    AdvSmoothMenu1: TAdvSmoothMenu;
    AdvSmoothImageListBox1: TAdvSmoothImageListBox;
    AdvNavBarPanel4: TAdvNavBarPanel;
    AdvNavBarPanel5: TAdvNavBarPanel;
    AdvPanel2: TAdvPanel;
    AdvSmoothGauge1: TAdvSmoothGauge;
    AdvPanel3: TAdvPanel;
    BalancePanel: TAdvPanel;
    AdvSmoothCalculator1: TAdvSmoothCalculator;
    AdvSmoothCalendar1: TAdvSmoothCalendar;
    AdvSmoothProgressBar1: TAdvSmoothProgressBar;
    AdvSmoothSlider1: TAdvSmoothSlider;
    AdvSmoothSlider2: TAdvSmoothSlider;
    BtnOK: TAdvGlowButton;
    BtnCancel: TAdvGlowButton;
    AdvPanel4: TAdvPanel;
    NavBarIconsWhite: TAdvSVGImageCollection;
    VirtualImageListNavBar: TVirtualImageList;
    NavBarIconsBlack: TAdvSVGImageCollection;
    AdvToolBar2: TAdvToolBar;
    agb_Cut: TAdvGlowButton;
    agb_Copy: TAdvGlowButton;
    agb_Clear: TAdvGlowButton;
    VirtualImageListToolBarPagerLarge: TVirtualImageList;
    RibbonLargeIconsColor: TAdvSVGImageCollection;
    AdvToolBar3: TAdvToolBar;
    agb_Bold: TAdvGlowButton;
    agb_Underline: TAdvGlowButton;
    agb_StrickOut: TAdvGlowButton;
    agb_Italic: TAdvGlowButton;
    AdvToolBar5: TAdvToolBar;
    AdvToolBar1: TAdvToolBar;
    AdvGlowButton3: TAdvGlowButton;
    VirtualImageListToolBarPager: TVirtualImageList;
    RibbonIconsColor: TAdvSVGImageCollection;
    RibbonLargeIconsWhite: TAdvSVGImageCollection;
    RibbonIconsWhite: TAdvSVGImageCollection;
    agb_AlignLeft: TAdvGlowButton;
    agb_AlignCenter: TAdvGlowButton;
    agb_AlignRight: TAdvGlowButton;
    AdvOfficeFontSelector1: TAdvOfficeFontSelector;
    AdvOfficeFontSizeSelector1: TAdvOfficeFontSizeSelector;
    AdvToolBarSeparator1: TAdvToolBarSeparator;
    AdvOfficeTextColorSelector1: TAdvOfficeTextColorSelector;
    AdvOfficeColorSelector1: TAdvOfficeColorSelector;
    AdvGlowButton4: TAdvGlowButton;
    AdvGlowButton5: TAdvGlowButton;
    LblBloNum: TLabel;
    LblNotSt: TLabel;
    LblNotStNumb: TLabel;
    LblCA: TLabel;
    LblCANum: TLabel;
    lblAUC: TLabel;
    AdvQuickAccessToolBar1: TAdvQuickAccessToolBar;
    AdvGlowButton6: TAdvGlowButton;
    AdvGlowButton7: TAdvGlowButton;
    AdvGlowButton8: TAdvGlowButton;
    QuickAccessColor: TAdvSVGImageCollection;
    QuickAccessWhite: TAdvSVGImageCollection;
    VirtualImageListQuickAcces: TVirtualImageList;
    AdvComboBox1: TAdvComboBox;
    AdvToolBar6: TAdvToolBar;
    AdvGlowButton11: TAdvGlowButton;
    AdvGlowButton12: TAdvGlowButton;
    AdvToolBar4: TAdvToolBar;
    AdvGlowButton9: TAdvGlowButton;
    AdvGlowButton10: TAdvGlowButton;
    AdvStringGrid1: TAdvStringGrid;
    GridIcons: TAdvSVGImageCollection;
    AdvPage1: TAdvPage;
    AdvToolBar7: TAdvToolBar;
    AdvGlowButton13: TAdvGlowButton;
    AdvGlowButton14: TAdvGlowButton;
    AdvGlowButton15: TAdvGlowButton;
    Timer1: TTimer;
    PictureContainerPlanner: TPictureContainer;
    LblRTC: TLabel;
    LblBlocked: TLabel;
    LblPerc: TLabel;
    AdvToolBar8: TAdvToolBar;
    AdvToolBar9: TAdvToolBar;
    AdvToolBar10: TAdvToolBar;
    AdvGlowButton16: TAdvGlowButton;
    AdvGlowButton20: TAdvGlowButton;
    AdvGlowButton21: TAdvGlowButton;
    AdvGlowButton22: TAdvGlowButton;
    AdvGlowButton23: TAdvGlowButton;
    AdvGlowButton24: TAdvGlowButton;
    AdvGlowButton25: TAdvGlowButton;
    AdvGlowButton26: TAdvGlowButton;
    AdvGlowButton27: TAdvGlowButton;
    AdvGlowButton28: TAdvGlowButton;
    AdvGlowButton29: TAdvGlowButton;
    AdvGlowButton2: TAdvGlowButton;
    AdvGlowButton17: TAdvGlowButton;
    AdvGlowButton30: TAdvGlowButton;
    Office2019Frame11: TOffice2019Frame1;
    procedure FormCreate(Sender: TObject);
    procedure AdvComboBox1Change(Sender: TObject);
    procedure AdvSmoothMenu1ItemClick(Sender: TObject; ItemIndex: Integer);
    procedure AdvSmoothImageListBox1ItemClick(Sender: TObject;
      itemindex: Integer; Button: TMouseButton; Shift: TShiftState);
    procedure AddPlannerItems;
    procedure ProcessGridImages;
    procedure AdvGlowButton14Click(Sender: TObject);
    procedure AdvGlowButton15Click(Sender: TObject);
    procedure AdvGlowButton13Click(Sender: TObject);
    procedure AdvStringGrid1ClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure BtnOKClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure AdvOfficeFontSelector1Select(Sender: TObject);
    procedure AdvOfficeFontSizeSelector1Change(Sender: TObject);
    procedure SetLblTxtColors(ATxtColor: TColor);
    procedure Office2019Frame11ImageTextItem1ItemClick(Sender: TObject;
      Item: TCustomItem);
    procedure Office2019Frame11MenuItem01ItemClick(Sender: TObject;
      Item: TCustomItem);
  private
    { Private declarations }
    selRow: Integer;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.AddPlannerItems;
var
 PlannerItem: TPlannerItem;
begin
  Planner1.Items.Clear;

  PlannerItem := Planner1.Items.Add;

  PlannerItem.ItemPos := 0;
  PlannerItem.ItemBegin := 18;
  PlannerItem.ItemEnd := 23;
  PlannerItem.ReadOnly := True;
  PlannerItem.CaptionType := Planner.ctText;
  PlannerItem.CaptionText := 'Roadmap Summer Collection';
  PlannerItem.Text.Add('<b></b>Discuss the possbile trends with the designers.<br><br><br><br><br><img src="users"/>');

  PlannerItem := Planner1.Items.Add;

  PlannerItem.ItemPos := 0;
  PlannerItem.ItemBegin := 30;
  PlannerItem.ItemEnd := 34;
  PlannerItem.ReadOnly := True;
  PlannerItem.CaptionType := Planner.ctText;
  PlannerItem.CaptionText := 'Job Interview';
  PlannerItem.Text.Add('Introductory meeting for sales team.<br>Contact: <u>Victoria Sellers</u><br><br><br><img src="online"/><img src="video"/>');

  PlannerItem := Planner1.Items.Add;

  PlannerItem.ItemPos := 1;
  PlannerItem.ItemBegin := 28;
  PlannerItem.ItemEnd := 32;
  PlannerItem.ReadOnly := True;
  PlannerItem.CaptionType := Planner.ctText;
  PlannerItem.CaptionText := 'Dress & Play Feedback';
  PlannerItem.Text.Add('<B>Customer Happiness</B><br><br>Go over Quality check with Ms. De Wulf.<br><br><img src="support"/>');
end;

procedure TForm6.AdvComboBox1Change(Sender: TObject);
var
  StyleStr: string;
begin
  case AdvComboBox1.ItemIndex of
    0:
    begin
      AdvFormStyler1.Style := tsOffice2019White;
      AdvStringGrid1.Bands.PrimaryColor := $00FFFAF2;
    end;
    1:
    begin
      AdvFormStyler1.Style := tsOffice2019Gray;
      AdvStringGrid1.Bands.PrimaryColor := $00E0E0E0;
    end;
    2:
    begin
      AdvFormStyler1.Style := tsOffice2019Black;
      AdvStringGrid1.Bands.PrimaryColor := $00939393;
    end;
  end;

  StyleStr := AdvComboBox1.Items[AdvComboBox1.ItemIndex];

  //Change ImageCollections when Black style
  if StyleStr.Contains('Black') then
  begin
    VirtualImageListNavBar.ImageCollection := NavBarIconsWhite;
    VirtualImageListToolBarPagerLarge.ImageCollection := RibbonLargeIconsWhite;
    VirtualImageListToolBarPager.ImageCollection := RibbonIconsWhite;
    VirtualImageListQuickAcces.ImageCollection := QuickAccessWhite;
  end
  else
  begin
    VirtualImageListNavBar.ImageCollection := NavBarIconsBlack;
    VirtualImageListToolBarPagerLarge.ImageCollection := RibbonLargeIconsColor;
    VirtualImageListToolBarPager.ImageCollection := RibbonIconsColor;
    if StyleStr.Contains('Gray') then
    begin
      VirtualImageListQuickAcces.ImageCollection := QuickAccessWhite;
    end
    else
    begin
      VirtualImageListQuickAcces.ImageCollection := QuickAccessColor;
    end;
  end;

  //Recreate PlannerItems for style change;
  SetLblTxtColors(BtnCancel.Appearance.TextColor);
  AddPlannerItems;
end;

procedure TForm6.AdvGlowButton13Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://www.facebook.com/tmssoftware/', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm6.AdvGlowButton14Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm6.AdvGlowButton15Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/forum/', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm6.AdvOfficeFontSelector1Select(Sender: TObject);
begin
  AdvStringGrid1.Font.Name := AdvOfficeFontSelector1.SelectedFontName;
end;

procedure TForm6.AdvOfficeFontSizeSelector1Change(Sender: TObject);
begin
  AdvStringGrid1.Font.Size := AdvOfficeFontSizeSelector1.SelectedFontSize;
end;

procedure TForm6.BtnOKClick(Sender: TObject);
begin
  AdvSmoothProgressBar1.Position := 0;
  Timer1.Enabled := True;
  BtnOK.Enabled := False;
end;

procedure TForm6.BtnCancelClick(Sender: TObject);
begin
  if Timer1.Enabled then
    Timer1.Enabled := False;
  if selRow >=0 then
  begin
    AdvSmoothCalculator1.FloatValue := StrToFloat(AdvStringGrid1.Cells[9, selRow]);
    BtnOK.Enabled := True;
  end;
end;

procedure TForm6.AdvSmoothImageListBox1ItemClick(Sender: TObject;
  itemindex: Integer; Button: TMouseButton; Shift: TShiftState);
begin
  AdvSmoothImageListBox1.Footer.Caption := 'Selected Cloth: '+AdvSmoothImageListBox1.Items[itemindex].FileName;
end;

procedure TForm6.AdvSmoothMenu1ItemClick(Sender: TObject; ItemIndex: Integer);
begin
  case ItemIndex of
    0:
    begin
      AdvSmoothImageListBox1.Visible := True;
      BalancePanel.Visible := False;
    end;
    1:
    begin
      AdvSmoothImageListBox1.Visible := False;
      BalancePanel.Visible := True;
    end;
  end;
  AdvSmoothMenu1.SelectedItemIndex := ItemIndex;
end;

procedure TForm6.AdvStringGrid1ClickCell(Sender: TObject; ARow, ACol: Integer);
var
 idx: integer;
 s: string;
begin
  if ARow < 1 then
    Exit;

  selRow := ARow;

  if (AdvStringGrid1.Cells[8,ARow] <> '') then
  begin
      try
        s := AdvStringGrid1.Cells[8,selRow];
        AdvSmoothCalculator1.FloatValue := StrToFloat(s);
      except
        Exit;
      end;
  end
  else
    Exit;
  BtnOK.Enabled := True;

  idx := AdvSmoothImageListBox1.Items.Find(AdvStringGrid1.Cells[3,ARow]);
  if idx >= 0 then
  begin
    AdvSmoothImageListBox1.SelectedItemIndex := idx;
    AdvSmoothImageListBox1.Footer.Caption := 'Selected Cloth: '+AdvSmoothImageListBox1.Items[idx].FileName;
  end;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
  selRow := -1;

  //Fill AdvStringGrid
  AdvStringGrid1.SaveFixedCells := true;
  AdvStringGrid1.LoadFromCSV('sales.csv');
  AdvStringGrid1.HideColumn(6);
  AdvStringGrid1.HideColumn(8);
  ProcessGridImages;
  AdvStringGrid1.AutoSize := True;

  //Set Styles
  AdvFormStyler1.Style := TTMSStyle(tsOffice2019White);

  //Planner Settings
  Planner1.Header.Captions.Clear;
  Planner1.Header.Captions.Add('');
  Planner1.Header.Captions.Add(FormatDateTime('m/dd',Now));
  Planner1.Header.Captions.Add(FormatDateTime('m/dd',Now + 1));

  AddPlannerItems;

  //Fill Comboboxes
  AdvOfficeTextColorSelector1.SelectedColor := clRed;
  AdvOfficeFontSelector1.SelectedFontName := 'Segoe UI';
  AdvOfficeFontSizeSelector1.SelectedFontSize := 9;
  AdvOfficeColorSelector1.SelectedColor := clYellow;
  AdvStringGrid1.Font.Name := AdvOfficeFontSelector1.SelectedFontName;
  AdvStringGrid1.Font.Size := AdvOfficeFontSizeSelector1.SelectedFontSize;
  AdvStringGrid1.AutoFitColumns(true);

end;

procedure TForm6.Office2019Frame11ImageTextItem1ItemClick(Sender: TObject;
  Item: TCustomItem);
begin
  AdvShapeButton1.HideFrame;
end;

procedure TForm6.Office2019Frame11MenuItem01ItemClick(Sender: TObject;
  Item: TCustomItem);
begin
  AdvShapeButton1.HideFrame;
end;

procedure TForm6.ProcessGridImages;
var
  imgIndex: Integer;
  I: Integer;
begin
  for I := 1 to AdvStringGrid1.RowCount - 1 do
  begin
    if AdvStringGrid1.Cells[6,i] <> '' then
    begin
      imgIndex := StrToInt(AdvStringGrid1.Cells[6,i]);

      if imgIndex > 0 then
        AdvStringGrid1.AddPicture(7,i,GridIcons.GetItemByIndex(imgIndex).Data,true, TStretchMode.StretchWithAspectRatio, 2, haCenter, vaCenter);
    end;
  end;
end;

procedure TForm6.SetLblTxtColors(ATxtColor: TColor);
begin
  LblBlocked.Font.Color := ATxtColor;
  LblBloNum.Font.Color := ATxtColor;
  LblPerc.Font.Color := ATxtColor;
  LblNotSt.Font.Color := ATxtColor;
  LblNotStNumb.Font.Color := ATxtColor;
  LblCANum.Font.Color := ATxtColor;
  LblCA.Font.Color := ATxtColor;
  LblRTC.Font.Color := ATxtColor;
  lblAUC.Font.Color := ATxtColor;
end;

procedure TForm6.Timer1Timer(Sender: TObject);
begin
  if AdvSmoothProgressBar1.Position >= 100 then
  begin
    AdvSmoothProgressBar1.Position := 0;
    AdvStringGrid1.Cells[8,selRow] := AdvSmoothCalculator1.FloatValue.ToString;
    Timer1.Enabled := False;
    BtnOK.Enabled := True;
  end
  else
    AdvSmoothProgressBar1.Position := AdvSmoothProgressBar1.Position + Random(5);
end;

end.
