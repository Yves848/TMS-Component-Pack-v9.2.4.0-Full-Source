{********************************************************************}
{ TMS Office Graphics components demo                                }
{ for Delphi & C++Builder                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvOfficeGraphicsDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvOfficeComboBox, AdvGlowButton, AdvToolBar, AdvAppStyler,
  AdvToolBarStylers, AdvOfficeSelectors, ExtActns, ActnList, StdActns, ComCtrls,
  Menus, AdvMenus, AdvMenuStylers, AdvStyleIF, ExtCtrls, ImgList, ShellAPI,
  System.ImageList, System.Actions;

type
  TForm1 = class(TForm)
    AdvToolBarPager1: TAdvToolBarPager;
    AdvPage1: TAdvPage;
    AdvToolBar1: TAdvToolBar;
    AdvToolBar2: TAdvToolBar;
    AdvGlowButton1: TAdvGlowButton;
    AdvGlowButton2: TAdvGlowButton;
    AdvGlowButton3: TAdvGlowButton;
    AdvOfficeFontSelector1: TAdvOfficeFontSelector;
    AdvOfficeFontSizeSelector1: TAdvOfficeFontSizeSelector;
    AdvGlowButton4: TAdvGlowButton;
    AdvGlowButton5: TAdvGlowButton;
    AdvGlowButton6: TAdvGlowButton;
    AdvOfficeColorSelector1: TAdvOfficeColorSelector;
    AdvToolBarOfficeStyler1: TAdvToolBarOfficeStyler;
    AdvFormStyler1: TAdvFormStyler;
    ActionList1: TActionList;
    RichEditBold1: TRichEditBold;
    RichEditItalic1: TRichEditItalic;
    RichEditUnderline1: TRichEditUnderline;
    AdvToolBar3: TAdvToolBar;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    AdvOfficeColorSelector2: TAdvOfficeColorSelector;
    AdvPopupMenu1: TAdvPopupMenu;
    AdvGlowMenuButton1: TAdvGlowMenuButton;
    AdvMenuOfficeStyler1: TAdvMenuOfficeStyler;
    AdvOfficeCharacterSelector1: TAdvOfficeCharacterSelector;
    AdvPage2: TAdvPage;
    Notebook1: TNotebook;
    RichEdit1: TRichEdit;
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    AdvToolBar4: TAdvToolBar;
    AdvOfficePenStyleSelector1: TAdvOfficePenStyleSelector;
    AdvOfficeColorSelector3: TAdvOfficeColorSelector;
    AdvToolBar5: TAdvToolBar;
    AdvOfficeBrushStyleSelector1: TAdvOfficeBrushStyleSelector;
    AdvOfficePenWidthSelector1: TAdvOfficePenWidthSelector;
    AdvOfficeColorSelector4: TAdvOfficeColorSelector;
    AdvOfficeColorSelector5: TAdvOfficeColorSelector;
    AdvToolBar6: TAdvToolBar;
    AdvOfficeTableSelector1: TAdvOfficeTableSelector;
    AdvOfficeShadowSelector1: TAdvOfficeShadowSelector;
    AdvOfficeTableBorderSelector1: TAdvOfficeTableBorderSelector;
    AdvOfficeToolSelector1: TAdvOfficeToolSelector;
    ImageList1: TImageList;
    AdvToolBar7: TAdvToolBar;
    AdvOfficeScrollSelector1: TAdvOfficeScrollSelector;
    ImageList2: TImageList;
    Panel2: TPanel;
    Label1: TLabel;
    Label4: TLabel;
    Windows101: TMenuItem;
    procedure AdvOfficeColorSelector1SelectColor(Sender: TObject;
      AColor: TColor);
    procedure RichEdit1SelectionChange(Sender: TObject);
    procedure AdvOfficeFontSelector1Select(Sender: TObject);
    procedure AdvOfficeFontSizeSelector1Select(Sender: TObject);
    procedure AdvOfficeColorSelector2SelectColor(Sender: TObject;
      AColor: TColor);
    procedure file1Click(Sender: TObject);
    procedure AdvOfficeCharacterSelector1Select(Sender: TObject);
    procedure AdvOfficeCharacterSelector1Click(Sender: TObject);
    procedure AdvToolBarPager1Changing(Sender: TObject; FromPage,
      ToPage: Integer; var AllowChange: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure AdvOfficePenStyleSelector1Select(Sender: TObject; Index: Integer;
      Item: TAdvSelectorItem);
    procedure AdvOfficePenWidthSelector1Select(Sender: TObject; Index: Integer;
      Item: TAdvSelectorItem);
    procedure AdvOfficeBrushStyleSelector1SelectBrushStyle(Sender: TObject;
      AStyle: TBrushStyle);
    procedure AdvOfficeColorSelector3SelectColor(Sender: TObject;
      AColor: TColor);
    procedure AdvOfficeColorSelector4SelectColor(Sender: TObject;
      AColor: TColor);
    procedure AdvOfficeColorSelector5SelectColor(Sender: TObject;
      AColor: TColor);
    procedure AdvOfficeScrollSelector1Select(Sender: TObject; Index: Integer;
      Item: TAdvScrollSelectorItem);
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    APen: TPen;
    ABrush: TBrush;
  end;

var
  Form1: TForm1;
  styles: TStringList;

implementation

{$R *.dfm}

procedure TForm1.AdvOfficeBrushStyleSelector1SelectBrushStyle(Sender: TObject;
  AStyle: TBrushStyle);
begin
  PaintBox1.Repaint;
end;

procedure TForm1.AdvOfficeCharacterSelector1Click(Sender: TObject);
begin
  RichEdit1.SelText := AdvOfficeCharacterSelector1.SelectedChar;
end;

procedure TForm1.AdvOfficeCharacterSelector1Select(Sender: TObject);
begin
  RichEdit1.SelText := AdvOfficeCharacterSelector1.SelectedChar;
end;

procedure TForm1.AdvOfficeColorSelector1SelectColor(Sender: TObject;
  AColor: TColor);
begin
  RichEdit1.SelAttributes.Color := AColor;
end;

procedure TForm1.AdvOfficeColorSelector2SelectColor(Sender: TObject;
  AColor: TColor);
begin
  RichEdit1.Color := AColor;
end;

procedure TForm1.AdvOfficeColorSelector3SelectColor(Sender: TObject;
  AColor: TColor);
begin
  AdvOfficePenStyleSelector1.PenColor := AColor;
  PaintBox1.Repaint;
end;

procedure TForm1.AdvOfficeColorSelector4SelectColor(Sender: TObject;
  AColor: TColor);
begin
  AdvOfficeBrushStyleSelector1.BrushColor := AdvOfficeColorSelector4.SelectedColor;
  PaintBox1.Repaint;
end;

procedure TForm1.AdvOfficeColorSelector5SelectColor(Sender: TObject;
  AColor: TColor);
begin
  PaintBox1.Repaint;
end;

procedure TForm1.AdvOfficeFontSelector1Select(Sender: TObject);
begin
  RichEdit1.SelAttributes.Name := AdvOfficeFontSelector1.Text;
end;

procedure TForm1.AdvOfficeFontSizeSelector1Select(Sender: TObject);
begin
  RichEdit1.SelAttributes.Size := StrToInt(AdvOfficeFontSizeSelector1.Text);
end;

procedure TForm1.AdvOfficePenStyleSelector1Select(Sender: TObject;
  Index: Integer; Item: TAdvSelectorItem);
begin
  PaintBox1.Repaint;
end;

procedure TForm1.AdvOfficePenWidthSelector1Select(Sender: TObject;
  Index: Integer; Item: TAdvSelectorItem);
begin
  PaintBox1.Repaint;
end;

procedure TForm1.AdvOfficeScrollSelector1Select(Sender: TObject; Index: Integer;
  Item: TAdvScrollSelectorItem);
begin
  ShowMessage('Tool item '+ inttostr(Item.Index)+' selected');
end;

procedure TForm1.AdvToolBarPager1Changing(Sender: TObject; FromPage,
  ToPage: Integer; var AllowChange: Boolean);
begin
  notebook1.PageIndex := ToPage;
end;

procedure TForm1.file1Click(Sender: TObject);
begin
  AdvGlowMenuButton1.Caption:=styles.Strings[(Sender as TMenuItem).Tag];
  AdvFormStyler1.Style:= TTMSStyle(styles.Objects[(Sender as TMenuItem).Tag]);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  styleMenu:TMenuItem;
  i: Integer;
  winCount: Integer;
begin
  Notebook1.PageIndex:=0;
  RichEdit1.Visible:=true;
  winCount:=0;
  APen := TPen.Create;
  ABrush := TBrush.Create;
  styles:=TStringList.Create;
  AdvPopupMenu1.BeginUpdate;
  AdvPopupMenu1.Items.Clear;

  FillStyleList(styles);
  for i := 0 to styles.Count-1 do
  begin
    styleMenu := TMenuItem.Create(AdvPopupMenu1);
    styleMenu.Caption := styles.Strings[i];
    styleMenu.Tag:= i;
    styleMenu.OnClick:= file1Click;
    AdvPopupMenu1.Items.Add(styleMenu);
  end;
  AdvGlowMenuButton1.Caption:=styles.Strings[0];
  AdvFormStyler1.Style:= TTMSStyle(styles.Objects[0]);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  APen.Free;
  ABrush.Free;
end;

procedure TForm1.Label4Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  r: TRect;
begin

  PaintBox1.Canvas.Brush.Style := AdvOfficeBrushStyleSelector1.SelectedBrushStyle;
  PaintBox1.Canvas.Brush.Color := AdvOfficeColorSelector4.SelectedColor;

  SetBkMode(Canvas.Handle, TRANSPARENT);
  SetBkColor(PaintBox1.Canvas.Handle, ColorToRGB(AdvOfficeColorSelector5.SelectedColor));

  r := Rect(20,20,200,200);
  PaintBox1.Canvas.FillRect(r);

  Canvas.Brush.Style := bsClear;

  PaintBox1.Canvas.Pen.Color := AdvOfficeColorSelector3.SelectedColor;
  PaintBox1.Canvas.Pen.Width := AdvOfficePenWidthSelector1.SelectedPenWidth;
  PaintBox1.Canvas.Pen.Style := AdvOfficePenStyleSelector1.SelectedPenStyle;

  PaintBox1.Canvas.Rectangle(r);

end;

procedure TForm1.RichEdit1SelectionChange(Sender: TObject);
begin
  AdvOfficeColorSelector1.SelectedColor := RichEdit1.SelAttributes.Color;
  AdvOfficeFontSelector1.Text := RichEdit1.SelAttributes.Name;
  AdvOfficeFontSizeSelector1.Text := IntToStr(RichEdit1.SelAttributes.Size);
end;

end.
