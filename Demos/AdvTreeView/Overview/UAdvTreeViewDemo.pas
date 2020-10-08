{********************************************************************}
{ TMS TAdvTreeView Demo                                              }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UAdvTreeViewDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvTreeViewBase, AdvTreeViewData, Types,
  AdvCustomTreeView, AdvTreeView, Vcl.ExtCtrls, Vcl.StdCtrls, PictureContainer,
  AdvCustomControl, AdvGraphics, AdvGraphicsTypes;

type
  TForm98 = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    AdvTreeView1: TAdvTreeView;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Label16: TLabel;
    PictureContainer1: TPictureContainer;
    procedure FormCreate(Sender: TObject);
    procedure AdvTreeView1BeforeOpenInplaceEditor(Sender: TObject;
      ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanOpen: Boolean);
    procedure AdvTreeView1GetNodeText(Sender: TObject;
      ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
      AMode: TAdvTreeViewNodeTextMode; var AText: string);
    procedure AdvTreeView1NodeAnchorClick(Sender: TObject;
      ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: string);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Label16Click(Sender: TObject);
    procedure AdvTreeView1AfterDrawNodeText(Sender: TObject;
      AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
      ANode: TAdvTreeViewVirtualNode; AText: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form98: TForm98;

implementation

{$R *.dfm}

uses
  Math, ShellApi;

procedure TForm98.AdvTreeView1AfterDrawNodeText(Sender: TObject;
  AGraphics: TAdvGraphics; ARect: TRectF; AColumn: Integer;
  ANode: TAdvTreeViewVirtualNode; AText: string);
var
  r, rs: TRectF;
  fs, fst: Integer;
  t: String;
begin
  if (AColumn = 3) and Assigned(ANode.Node) then
  begin
    t := ANode.Node.Text[AColumn + 1];
    if TryStrToInt(t, fs) then
    begin
      fst := 1000;
      fs := Max(0, Min(fst, fst - fs));
      r := ARect;
      r.Top :=  r.Top + (r.Height - 20) / 2;
      r.Height := 20;
      InflateRect(r, 0, -2);
      r.Width := r.Width - 2;
      r := RectF(Int(r.Left) + 0.5, Int(r.Top) + 0.5, Int(r.Right) - 0.5, Int(r.Bottom) - 0.5);
      rs := r;
      rs.Width := rs.Width * fs / fst;
      AGraphics.Fill.Kind := gfkSolid;
      AGraphics.Fill.Color := clWhite;
      AGraphics.Stroke.Kind := gskSolid;
      AGraphics.DrawRectangle(Round(r.Left), Round(r.Top), Round(r.Right), Round(r.Bottom));
      AGraphics.Fill.Color := clWebSteelblue;
      AGraphics.DrawRectangle(Round(rs.Left), Round(rs.Top), Round(rs.Right), Round(rs.Bottom));
      if AdvTreeView1.IsNodeSelected(ANode.Node) then
        AGraphics.Stroke.Color := clWhite
      else
        AGraphics.Stroke.Color := clWebDarkgray;

      AGraphics.Fill.Kind := gfkNone;
      AGraphics.DrawRectangle(Round(r.Left), Round(r.Top), Round(r.Right), Round(r.Bottom));
    end;
  end;
end;

procedure TForm98.AdvTreeView1BeforeOpenInplaceEditor(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanOpen: Boolean);
begin
  ACanOpen := Assigned(ANode.Node) and not ANode.Node.DataBoolean;
end;

procedure TForm98.AdvTreeView1GetNodeText(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  AMode: TAdvTreeViewNodeTextMode; var AText: string);
begin
  if Assigned(ANode.Node) and (AMode = tntmDrawing) and (not ANode.Node.DataBoolean) and (AColumn = 4) and (ANode.Node.Text[AColumn] = '') then
    AText := '[Enter Amount]';
end;

procedure TForm98.AdvTreeView1NodeAnchorClick(Sender: TObject;
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: string);
begin
  ShellExecute(0, 'OPEN', PChar(AAnchor), '', '', SW_NORMAL);
end;

procedure TForm98.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    AdvTreeView1.Interaction.ClipboardMode := tcmFull
  else
    AdvTreeView1.Interaction.ClipboardMode := tcmNone;
end;

procedure TForm98.CheckBox2Click(Sender: TObject);
begin
  AdvTreeView1.Columns[1].Visible := CheckBox2.Checked;
  AdvTreeView1.ColumnsAppearance.StretchAll := not AdvTreeView1.Columns[1].Visible;
end;

procedure TForm98.CheckBox3Click(Sender: TObject);
begin
  AdvTreeView1.Columns[2].UseDefaultAppearance := not CheckBox3.Checked;
end;

procedure TForm98.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
    AdvTreeView1.ExpandAll
  else
    AdvTreeView1.CollapseAll;
end;

procedure TForm98.CheckBox5Click(Sender: TObject);
var
  I: Integer;
begin
  if CheckBox5.Checked then
  begin
    for I := 0 to AdvTreeView1.Columns.Count - 1 do
      AdvTreeView1.Columns[I].Sorting := tcsRecursive;
  end
  else
  begin
    for I := 0 to AdvTreeView1.Columns.Count - 1 do
      AdvTreeView1.Columns[I].Sorting := tcsNone;
  end;
end;

procedure TForm98.CheckBox6Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to AdvTreeView1.Columns.Count - 1 do
    AdvTreeView1.Columns[I].Filtering.Enabled := CheckBox6.Checked;

  if CheckBox5.Checked then
    AdvTreeView1.Columns[2].HorizontalTextAlign := gtaLeading
  else
    AdvTreeView1.Columns[2].HorizontalTextAlign := gtaTrailing;
end;

procedure TForm98.FormCreate(Sender: TObject);
var
  pn, n, subn: TAdvTreeViewNode;
  c: TAdvTreeViewColumn;
  I: Integer;
begin
  AdvTreeView1.BeginUpdate;
  AdvTreeView1.PictureContainer := PictureContainer1;
  AdvTreeView1.ClearColumns;
  AdvTreeView1.ClearNodes;

  AdvTreeView1.NodesAppearance.HeightMode := tnhmVariable;
  AdvTreeView1.NodesAppearance.SelectionArea := tsaFromText;
  AdvTreeView1.ColumnsAppearance.Stretch := True;
  AdvTreeView1.ColumnsAppearance.StretchAll := False;
  AdvTreeView1.ColumnsAppearance.StretchColumn := 1;

  c := AdvTreeView1.Columns.Add;
  c.Text := 'Item';
  c.Width := 120;

  c := AdvTreeView1.Columns.Add;
  c.Text := 'Description';
  c.WordWrapping := True;
  c.Width := 275;

  c := AdvTreeView1.Columns.Add;
  c.Text := 'Price';
  c.HorizontalTextAlign := gtaTrailing;
  c.Width := 60;
  c.Fill.Color := clWebSlategray;
  c.Font.Color := clWhite;
  c.TopFill.Color := clWebSlateGray;
  c.TopFill.Kind := gfkSolid;
  c.TopFont.Color := clWhite;
  c.TopFont.Size := 14;
  c.Font.Size := 14;

  c := AdvTreeView1.Columns.Add;
  c.Text := 'Stock';
  c.Width := 150;

  c := AdvTreeView1.Columns.Add;
  c.Text := 'Amount';
  c.EditorType := tcetEdit;
  c.Width := 100;

  c := AdvTreeView1.Columns.Add;
  c.Text := 'Delivery method';
  c.EditorType := tcetComboBox;
  c.EditorItems.Add('Standard (5-10 business days)');
  c.EditorItems.Add('Pro (2-3 business days, + $5)');
  c.EditorItems.Add('Exclusive (1 business day, + $10)');
  c.Width := 200;

  pn := AdvTreeView1.AddNode;
  pn.Text[0] := 'Cakes';
  pn.Extended := True;


  n := AdvTreeView1.AddNode(pn);
  n.Text[0] := 'Decoration';
  n.DataBoolean := True;

  subn := AdvTreeView1.AddNode(n);
  subn.CollapsedIconNames[0, False] := 'deco1.png';
  subn.ExpandedIconNames[0, False] := 'deco1.png';
  subn.Text[2] := Format('%m', [RandomRange(100, 1000) / RandomRange(1, 250)]);
  subn.Text[1] := 'A candle is wax with an <font color="#FF6700"><u>ignitable wick</ul></font> embedded that provides <ul><li>light<li>fragrance</ul> <br/>It can also be used to provide heat, or as a method of keeping time.';
  subn.Text[4] := IntToStr(RandomRange(5, 1000));
  subn.Text[5] := AdvTreeView1.Columns[5].EditorItems[Random(AdvTreeView1.Columns[5].EditorItems.Count)];

  subn := AdvTreeView1.AddNode(n);
  subn.CollapsedIconNames[0, False] := 'balloon.png';
  subn.ExpandedIconNames[0, False] := 'balloon.png';
  subn.Text[2] := Format('%m', [RandomRange(100, 1000) / RandomRange(1, 250)]);
  subn.Text[1] := 'A balloon is a <font bgcolor"#67FF00">flexible</font> <font bgcolor"#67FF00">bag</font> that can be inflated with a gas, such as helium, hydrogen, nitrous oxide, oxygen, or air.';


  n := AdvTreeView1.AddNode(pn);
  n.Text[0] := 'Types';
  n.DataBoolean := True;

  for I := 1 to 8 do
  begin
    Randomize;
    subn := AdvTreeView1.AddNode(n);
    subn.CheckTypes[0] := tvntCheckBox;
    subn.Checked[0] := Boolean(Random(2));
    subn.CollapsedIconNames[0, False] := 'cake'+inttostr(I)+'.png';
    subn.ExpandedIconNames[0, False] := 'cake'+inttostr(I)+'.png';
    subn.Text[2] := Format('%m', [RandomRange(100, 1000) / RandomRange(1, 250)]);
    if Odd(I) then
    begin
      subn.Text[5] := AdvTreeView1.Columns[5].EditorItems[Random(AdvTreeView1.Columns[5].EditorItems.Count)];
      subn.Text[4] := IntToStr(RandomRange(5, 1000));
    end;

    case I of
      1: subn.Text[1] := 'Cake is a form of <font color="#FF0000"><i>sweet</i></font> dessert that is <font size="14" bgcolor="#FF6633" color="#FFFFFF" face="Verdana">typically</font> baked.';
      2: subn.Text[1] := 'In its oldest forms, cakes were modifications of breads but now cover a wide range of preparations.';
      3: subn.Text[1] := 'Typical cake <a href="https://en.wikipedia.org/wiki/Ingredient">ingredients</a> are flour, sugar, eggs, and butter or oil.';
      5: subn.Text[1] := 'Cake is often served as a <font bgcolor="#666666" color="#FFFFFF">celebratory</font> dish on ceremonial occasions, for example <i>weddings</i>, <u>anniversaries</u>, and birthdays.';
      7: subn.Text[1] := 'There are countless cake recipes; some are bread-like, some rich and elaborate, and many are centuries old.';
    end;
  end;

  pn := AdvTreeView1.AddNode;
  pn.Text[0] := 'Biscuits';
  pn.Extended := True;

  for I := 1 to 1 do
  begin
    subn := AdvTreeView1.AddNode(pn);
    subn.CollapsedIconNames[0, False] := 'biscuit'+inttostr(I)+'.png';
    subn.ExpandedIconNames[0, False] := 'biscuit'+inttostr(I)+'.png';
    subn.Text[2] := Format('%m', [RandomRange(100, 1000) / RandomRange(1, 250)]);
  end;

  pn := AdvTreeView1.AddNode;
  pn.Text[0] := 'Pastries';
  pn.Extended := True;

  for I := 1 to 6 do
  begin
    subn := AdvTreeView1.AddNode(pn);
    subn.CheckTypes[0] := tvntRadioButton;
    subn.CollapsedIconNames[0, False] := 'pastry'+inttostr(I)+'.png';
    subn.ExpandedIconNames[0, False] := 'pastry'+inttostr(I)+'.png';
    subn.Text[2] := Format('%m', [RandomRange(100, 1000) / RandomRange(1, 250)]);

    case I of
      1: subn.Text[1] := 'Pastry is a <font color="#EE3C54">major</font> type of bakers'' <b>confectionery</b>. It includes many of the various kinds of baked products made from ingredients such as flour, sugar, milk, butter, shortening, baking powder, and eggs.';
      2: subn.Text[1] := 'Small tarts and other sweet baked products are called pastries. Common pastry dishes include pies, tarts, quiches and pasties.';
      3: subn.Text[1] := 'Pastry can also refer to the pastry dough,[3] from which such baked products are made.';
      5: subn.Text[1] := 'Pastry is differentiated from bread by having a higher fat content, which contributes to a flaky or crumbly texture.';
    end;
  end;

  AdvTreeView1.ExpandAll;
  AdvTreeView1.EndUpdate;
end;

procedure TForm98.Label16Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmspack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
