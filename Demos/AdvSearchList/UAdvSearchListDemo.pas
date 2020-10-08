{********************************************************************}
{ TMS TAdvSearchList Demo                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UAdvSearchListDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvScrollControl, AdvSearchList,
  Vcl.StdCtrls, AdvSearchEdit, Vcl.Samples.Spin, Vcl.CheckLst, Vcl.ExtCtrls, AdvGraphics,
  AdvAppStyler, ShellAPI;

type
  TForm5 = class(TForm)
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    AdvSearchList1: TAdvSearchList;
    AdvSearchEdit1: TAdvSearchEdit;
    SpinEdit1: TSpinEdit;
    ckSearchLimit: TCheckBox;
    GroupBox2: TGroupBox;
    AdvSearchList2: TAdvSearchList;
    CheckListBox1: TCheckListBox;
    AdvSearchEdit2: TAdvSearchEdit;
    Edit2: TEdit;
    ckDescr: TCheckBox;
    ColorDialog1: TColorDialog;
    Panel1: TPanel;
    Shape1: TShape;
    GroupBox3: TGroupBox;
    AdvSearchList3: TAdvSearchList;
    CheckListBox2: TCheckListBox;
    AdvSearchEdit3: TAdvSearchEdit;
    AdvFormStyler1: TAdvFormStyler;
    AdvSearchEdit4: TAdvSearchEdit;
    Label1: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure AdvSearchList1ItemChange(Sender: TObject; ItemIndex: Integer;
      Item: TSearchListItem);
    procedure ckSearchLimitClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure ckDescrClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure CheckListBox2ClickCheck(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitDictionaryList;
    procedure InitCarList;
    procedure InitTMSProductList;
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

const
  CLR_TMS  = $e39e00;
  CLR_VCL  = $3643f4;
  CLR_FMX  = $0098ff;
  CLR_FNC  = $ccc6bc;
  CLR_BIZ  = $ffb856;
  CLR_NET  = $b46800;
  CLR_IW   = $50af4c;
  CLR_LCL  = $ff009c;
  CLR_DEV  = $20a5da;
  CLR_FREE = $5a00ff;


procedure TForm5.AdvSearchList1ItemChange(Sender: TObject; ItemIndex: Integer;
  Item: TSearchListItem);
begin
  Edit1.OnChange := nil;
  Edit1.Text := item.Captions[0];
  Edit1.OnChange := Edit1Change;
end;

procedure TForm5.ckDescrClick(Sender: TObject);
begin
  if ckDescr.Checked then
    AdvSearchList2.FilterCondition.FilterData := fdAll
  else
    AdvSearchList2.FilterCondition.FilterData := fdText;
end;

procedure TForm5.CheckListBox1ClickCheck(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to CheckListBox1.Items.Count - 1 do
    begin
      AdvSearchList2.Categories[i].Filter := not CheckListBox1.Checked[i];
    end;

  AdvSearchList2.UpdateFilter;
end;

procedure TForm5.CheckListBox2ClickCheck(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to CheckListBox2.Items.Count - 1 do
    begin
      AdvSearchList3.Categories[i].Filter := not CheckListBox2.Checked[i];
    end;

  AdvSearchList3.UpdateFilter;
end;

procedure TForm5.ckSearchLimitClick(Sender: TObject);
begin
  if ckSearchLimit.Checked then
    AdvSearchList1.FilterCondition.MaxCategoryItems := SpinEdit1.Value
  else
    AdvSearchList1.FilterCondition.MaxCategoryItems := 0;
end;

procedure TForm5.Edit1Change(Sender: TObject);
begin
  AdvSearchList1.FilterCondition.Text := edit1.Text;
  AdvSearchList1.UpdateFilter;
end;

procedure TForm5.Edit2Change(Sender: TObject);
begin
  AdvSearchList2.FilterCondition.Text := edit2.Text;
  AdvSearchList2.UpdateFilter;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  InitDictionaryList;
  InitCarList;
  InitTMSProductList;
end;

procedure TForm5.InitCarList;
var
  i,cat: integer;
  sli: TSearchListItem;
  sl,cols: TStringList;
begin
  sl := TStringList.Create;
  cols := TStringList.Create;
  cols.Delimiter := ',';
  cols.StrictDelimiter := true;

  sl.LoadFromFile('.\..\..\carlist.txt');

  AdvSearchList2.Categories.Clear;
  AdvSearchList2.Categories.Add('Aston Martin',0);
  AdvSearchList2.Categories.Add('Audi',1);
  AdvSearchList2.Categories.Add('BMW',2);
  AdvSearchList2.Categories.Add('Ferarri',3);
  AdvSearchList2.Categories.Add('McLaren',4);
  AdvSearchList2.Categories.Add('Mercedes',5);
  AdvSearchList2.Categories.Add('Porsche',6);

  AdvSearchList2.Columns.Clear;
  AdvSearchList2.Columns.Add;
  AdvSearchList2.Columns.Add;
  AdvSearchList2.Columns[1].ControlFont := true;
  AdvSearchList2.Appearance.DescriptionControlFont := false;
  AdvSearchList2.Appearance.DescriptionFont.Color := clGray;
  AdvSearchList2.Appearance.DescriptionFont.Style := [fsItalic];
  AdvSearchList2.FilterCondition.Categories := true;
  AdvSearchList2.FilterCondition.CategoryItems := true;
  AdvSearchList2.FilterCondition.Column := 1;

  cat := -1;

  for i := 0 to sl.Count - 1 do
  begin
    cols.CommaText := sl.Strings[i];

    if StrToInt(cols[3]) <> cat then
    begin
      cat := StrToInt(cols[3]);
      sli := AdvSearchList2.Items.Add;
      sli.ItemType := itCategory;
      sli.CategoryID := cat;
      sli.Captions[0] := AdvSearchList2.Categories[cat].Caption;
    end;

    sli := AdvSearchList2.Items.Add;

    sli.Captions[1] := cols[0];
    sli.Descriptions[1] := cols[1];
    sli.CategoryID := StrToInt(cols[3]);

    sli.Columns[0].Picture.LoadFromFile('.\..\..\'+cols[2]);
  end;

  AdvSearchList2.Items.Sort(1, sdUp, false);

  AdvSearchList2.UpdateFilter;

  for i := 0 to AdvSearchList2.Categories.Count - 1 do
  begin
    CheckListBox1.Items.Add(AdvSearchList2.Categories[i].Caption);
    CheckListBox1.Checked[i] := true;
  end;

  AdvSearchList2.Appearance.HighlightFontStyle := [fsBold];

  AdvSearchEdit2.FilterCondition.Column := 1;
  AdvSearchEdit2.Columns.Assign(AdvSearchList2.Columns);
  AdvSearchEdit2.Categories.Assign(AdvSearchList2.Categories);
  AdvSearchEdit2.Items.Assign(AdvSearchList2.Items);
  AdvSearchEdit2.ItemHeight := 70;
end;

procedure TForm5.InitDictionaryList;
var
  i: integer;
  sli: TSearchListItem;
  sl: TStringList;
  prevc: char;
  s: string;
  catid: integer;
begin
  sl := TStringList.Create;
  sl.LoadFromFile('.\..\..\dictionary.txt');

  AdvSearchList1.Items.BeginUpdate;

  prevc := #0;
  catid := 0;

  for i := 0 to sl.Count - 1 do
  begin
    s := sl.Strings[i];

    if s[1] <> prevc then
    begin
      inc(catid);
      prevc := s[1];
      sli := AdvSearchList1.Items.Add(Uppercase(prevc));
      sli.ItemType := itCategory;
      sli.CategoryID := catid;
    end;

    sli := AdvSearchList1.Items.Add(sl.Strings[i]);
    sli.CategoryID := catid;
    sli.ImageIndexes[1] := Random(5);
  end;

  AdvSearchList1.Items.EndUpdate;

  sl.Free;

  AdvSearchList1.FilterCondition.FilterType := mStartWord;
  AdvSearchList1.FilterCondition.Column := 0;
  AdvSearchList1.FilterCondition.AutoSelect := true;
  AdvSearchList1.FilterCondition.CategoryItems := true;
  //AdvSearchList1.FilterCondition.MaxCategoryItems := 10;
  AdvSearchList1.FilterCondition.CaseSensitive := false;
  AdvSearchList1.FilterCondition.Categories := true;

  AdvSearchList1.Appearance.Banding := true;
  AdvSearchList1.Appearance.HighlightColor := clNone;
  AdvSearchList1.Appearance.HighlightTextColor := clGreen;
  AdvSearchList1.Appearance.HighlightFontStyle := [fsBold];
  AdvSearchList1.Appearance.FilterCount := fcShow;
  AdvSearchList1.Appearance.FilterCountFont.Size := 7;

  AdvSearchList1.UpdateFilter;

  AdvSearchEdit1.FilterCondition.Assign(AdvSearchList1.FilterCondition);
  AdvSearchEdit1.Appearance.Assign(AdvSearchList1.Appearance);

  AdvSearchEdit1.Items.Assign(AdvSearchList1.Items);
end;

procedure TForm5.InitTMSProductList;

  procedure AddItem(clr: TColor; acat, aname: string; ID: integer);
  var
    sli: TSearchListItem;
  begin
    sli := AdvSearchList3.Items.Add;
    sli.Columns[0].Shape := sRect;
    sli.Columns[0].Color := clr;
    sli.Captions[0] := acat;
    sli.Captions[1] := aname;
    sli.CategoryID := ID;
  end;

begin
  CheckListBox2.CheckAll(cbChecked);
  AdvSearchList3.Columns.Clear;
  AdvSearchList3.Columns.Add;
  AdvSearchList3.Columns.Add;
  AdvSearchList3.Columns.Add;

  AdvSearchList3.Categories.Clear;
  AdvSearchList3.Categories.Add('ALL',0);
  AdvSearchList3.Categories.Add('VCL',1);
  AdvSearchList3.Categories.Add('FMX',2);
  AdvSearchList3.Categories.Add('FNC',3);
  AdvSearchList3.Categories.Add('BIZ',4);
  AdvSearchList3.Categories.Add('DEV',5);
  AdvSearchList3.Categories.Add('IW',6);
  AdvSearchList3.Categories.Add('.NET',7);

  AdvSearchList3.Columns[0].ControlFont := false;
  AdvSearchList3.Columns[0].Font.Size := 12;
  AdvSearchList3.Columns[0].Font.Style := [fsBold];
  AdvSearchList3.Columns[0].Font.Color := clWhite;
  AdvSearchList3.Columns[0].Width := 44;
  AdvSearchList3.ItemHeight := 40;
  AdvSearchList3.FilterCondition.Column := 1;

  AdvSearchList3.Items.Clear;

  AdvSearchList3.Items.BeginUpdate;

  AddItem(CLR_TMS, 'ALL', 'TMS ALL-ACCESS',0);
  AddItem(CLR_VCL, 'VCL', 'TMS VCL Subscription',1);
  AddItem(CLR_VCL, 'VCL', 'TMS Component Pack',1);
  AddItem(CLR_VCL, 'VCL', 'TMS Component Studio',1);
  AddItem(CLR_VCL, 'VCL', 'TMS Cloud Pack',1);
  AddItem(CLR_VCL, 'VCL', 'TMS Cryptography Pack',1);
  AddItem(CLR_VCL, 'VCL', 'TMS WebGMaps',1);
  AddItem(CLR_VCL, 'VCL', 'TMS Diagram Studio',1);
  AddItem(CLR_VCL, 'VCL', 'TMS Advanced Charts',1);
  AddItem(CLR_VCL, 'VCL', 'TMS Flexcel',1);
  AddItem(CLR_FMX, 'FMX', 'TMS Component Studio for FireMonkey',2);
  AddItem(CLR_FMX, 'FMX', 'TMS Pack for FireMonkey',2);
  AddItem(CLR_FMX, 'FMX', 'TMS Charts for FireMonkey',2);
  AddItem(CLR_FMX, 'FMX', 'TMS WebGMaps for FireMonkey',2);
  AddItem(CLR_FNC, 'FNC', 'TMS FNC UI Pack',3);
  AddItem(CLR_FNC, 'FNC', 'TMS FNC Charts',3);
  AddItem(CLR_BIZ, 'BIZ', 'TMS XData',4);
  AddItem(CLR_BIZ, 'BIZ', 'TMS Aurelius',4);
  AddItem(CLR_BIZ, 'BIZ', 'TMS RemoteDB',4);
  AddItem(CLR_BIZ, 'BIZ', 'TMS Scripter',4);
  AddItem(CLR_BIZ, 'BIZ', 'TMS Workflow Studio',4);
  AddItem(CLR_DEV, 'DEV', 'TMS FixInsight',5);
  AddItem(CLR_DEV, 'DEV', 'TMS Logging',5);
  AddItem(CLR_IW, 'IW', 'TMS IntraWeb Component Pack Pro',6);
  AddItem(CLR_IW, 'IW', 'TMS IntraWeb iPhone Controls',6);
  AddItem(CLR_IW, 'IW', 'TMS IntraWeb Component Studio',6);
  AddItem(CLR_NET, '.NET', 'TMS Flexcel for .NET',7);
  AddItem(CLR_NET, '.NET', 'TMS Cloud Pack .NET',7);
  AddItem(CLR_NET, '.NET', 'TMS ASP.NET Component Pack',7);

  AdvSearchList3.Items.EndUpdate;

  AdvSearchList3.FilterCondition.Categories := true;

  AdvSearchEdit3.Categories.Assign(AdvSearchList3.Categories);
  AdvSearchEdit3.Columns.Assign(AdvSearchList3.Columns);
  AdvSearchEdit3.FilterCondition.Assign(AdvSearchList3.FilterCondition);
  AdvSearchEdit3.Items.Assign(AdvSearchList3.Items);
  AdvSearchEdit3.ItemHeight := 40;
end;

procedure TForm5.Label4Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm5.Panel1Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    Shape1.Brush.Color := ColorDialog1.Color;
    AdvSearchList2.Appearance.HighlightTextColor := ColorDialog1.Color;
    AdvSearchEdit2.Appearance.HighlightTextColor := ColorDialog1.Color;
  end;
end;

procedure TForm5.SpinEdit1Change(Sender: TObject);
begin
  if ckSearchLimit.Checked then
    AdvSearchList1.UpdateFilter;
end;

end.
