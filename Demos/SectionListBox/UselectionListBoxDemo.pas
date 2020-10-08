{**************************************************************************}
{ TMS TSectionListBox Demo                                                 )
{                                                                          }
{                                                                          }
{ Copyright © 1998 - 2019                                                  }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : https://www.tmssoftware.com                                      }
{**************************************************************************}

unit UselectionListBoxDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, slstbox, ExtCtrls, ComCtrls {$IFDEF VER120}, ImgList {$ENDIF}, ShellApi,
  System.ImageList, Vcl.ImgList;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    SectionListBox2: TSectionListBox;
    Button1: TButton;
    Button4: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    ListBox1: TListBox;
    GroupBox1: TGroupBox;
    SubItem: TLabel;
    SectionListBox1: TSectionListBox;
    SectionListBox3: TSectionListBox;
    CheckBox3: TCheckBox;
    RadioGroup1: TRadioGroup;
    CheckBox2: TCheckBox;
    CheckBox4: TCheckBox;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    SectionListBox5: TSectionListBox;
    Button6: TButton;
    Button7: TButton;
    TabSheet6: TTabSheet;
    SectionListBox6: TSectionListBox;
    TabSheet7: TTabSheet;
    SectionListBox7: TSectionListBox;
    TabSheet8: TTabSheet;
    SectionListBox8: TSectionListBox;
    TabSheet9: TTabSheet;
    SectionListBox9: TSectionListBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Label19: TLabel;
    Label18: TLabel;
    SectionListBox10: TSectionListBox;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SectionListBox2ExpandSection(Sender: TObject;
      SectionIdx: Integer);
    procedure SectionListBox2ContractSection(Sender: TObject;
      SectionIdx: Integer);
    procedure SectionListBox2SubItemClick(Sender: TObject; SectionIdx,
      SubItemIdx: Integer);
    procedure SectionListBox2DeleteSubItem(Sender: TObject; SectionIdx,
      SubItemIdx: Integer; var allow: Boolean);
    procedure SectionListBox2InsertSubItem(Sender: TObject; SectionIdx,
      SubItemIdx: Integer; var SubItem: String);
    procedure SectionListBox2DeleteSection(Sender: TObject;
      SectionIdx: Integer; var allow: Boolean);
    procedure SectionListBox2InsertSection(Sender: TObject;
      SectionIdx: Integer; section: TListSection);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure SectionListBox7DrawItem(Sender: TObject;
      section: TListSection; SectionIdx, SubItemIdx: Integer;
      Canvas: TCanvas; arect: TRect; astate: TOwnerDrawState);
    procedure SectionListBox9AnchorClick(Sender: TObject; index: Integer;
      anchor: String);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure SectionListBox10DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SectionListBox10DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SectionListBox10MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    DragSection, DragItem: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  SectionListBox2.ExpandAll;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SectionListBox2.ContractAll;
end;

procedure TForm1.SectionListBox2ExpandSection(Sender: TObject;
  SectionIdx: Integer);
begin
  ListBox1.Items.Add('Expand section ' + IntToStr(SectionIdx));
end;

procedure TForm1.SectionListBox10DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  SectionIdx, SubItemIdx: Integer;
  SIText: string;
begin
  SectionListBox10.GetItemAtXY(X, Y, SectionIdx,SubItemIdx);

  if (SectionIdx >= 0) and (SubItemIdx >= 0) then
  begin
    SIText := SectionListBox10.Sections[SectionIdx].SubItems[DragItem];
    SectionListBox10.Sections[SectionIdx].SubItems.Delete(DragItem);
    SectionListBox10.Sections[SectionIdx].SubItems.Insert(SubItemIdx, SIText);
  end;

  Repaint;
end;

procedure TForm1.SectionListBox10DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  SectionIdx, SubItemIdx: Integer;
begin
  SectionListBox10.GetItemAtXY(X, Y, SectionIdx, SubItemIdx);

  Accept := SectionIdx = DragSection;
end;

procedure TForm1.SectionListBox10MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SectionIdx, SubItemIdx: Integer;
begin
  SectionListBox10.GetItemAtXY(X, Y, SectionIdx, SubItemIdx);

  if SubItemIdx >= 0 then
  begin
    DragSection := SectionIdx;
    DragItem := SubItemIdx;
    SectionListBox10.BeginDrag(False, 4);
  end;
end;

procedure TForm1.SectionListBox2ContractSection(Sender: TObject;
  SectionIdx: Integer);
begin
  ListBox1.items.Add('Contract section ' + IntToStr(SectionIdx));
end;

procedure TForm1.SectionListBox2SubItemClick(Sender: TObject; SectionIdx,
  SubItemIdx: Integer);
begin
  SubItem.Caption := SectionListBox2.GetSectionsubItem(SectionIdx, SubItemIdx);
  ListBox1.items.add('Section: ' + IntToStr(SectionIdx) + ' SubItem: ' + IntToStr(SubItemIdx));

  if SectionIdx <= 5 then
  begin
    if SectionListBox2.Sections.Items[SectionIdx].SubItemImageIdx[SubItemIdx] = 2 then
      SectionListBox2.Sections.Items[SectionIdx].SubItemImageIdx[SubItemIdx] := 2
    else
      SectionListBox2.Sections.Items[SectionIdx].SubItemImageIdx[SubItemIdx] := 1;
  end;  
end;

procedure TForm1.SectionListBox2DeleteSubItem(Sender: TObject; SectionIdx,
  SubItemIdx: Integer; var Allow: Boolean);
begin
  with SectionListBox2 do
  Allow := MessageDlg('Delete item ' + GetSectionsubItem(SectionIdx,SubItemIdx) + '? ',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TForm1.SectionListBox2InsertSubItem(Sender: TObject; SectionIdx,
  SubItemIdx: Integer; var SubItem: String);
begin
  SubItem := 'This is a new SubItem';
end;

procedure TForm1.SectionListBox2DeleteSection(Sender: TObject;
  SectionIdx: Integer; var Allow: Boolean);
begin
  with (SectionListBox2.Sections.Items[SectionIdx] as TListSection) do
  Allow := MessageDlg('Delete section ' + caption + '? ',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TForm1.SectionListBox2InsertSection(Sender: TObject;
  SectionIdx: Integer; Section: TListSection);
begin
  Section.Caption := 'Second hand cars';
  Section.SubItems.Add('BMW' + #9 + '750iL' + #9 + '1995');
  Section.SubItems.Add('Mercedes' + #9 + 'SLK230' + #9 + '1997');
  Section.SubItems.Add('Audi' + #9 + 'A4' + #9 + '1996');

end;

procedure TForm1.Button4Click(Sender: TObject);
begin
 with SectionListBox2.Sections.Add  do
  begin
   Caption := 'Italian sport cars';
   SubItems.Add('Ferrari' + #9 + 'F355' + #9 + '1995');
   SubItems.Add('Lamborghini' + #9 + 'Diablo' + #9 + '1997');
   SubItems.Add('DeTomaso' + #9 + 'Pantera' + #9 + '1985');
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with SectionListBox2.Sections do
  begin
   if (Count > 0) then
     (Items[SectionListBox2.ItemIndex]).Free;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  (SectionListBox2.Sections.Items[0]).Fixed := CheckBox1.Checked;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
 SectionListBox2.OptimizeTabs(10);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
 i, j: Integer;
 lis: TListSection;
 s: string;
begin
  for i := 1 to SectionListBox2.Sections.Count do
    with SectionListBox2.Sections.Items[i - 1] do
    begin
      for j := 1 to SubItems.Count do
      begin
        if Odd(Random(100)) then
          SubItemImageIdx[j - 1] := 6
        else
          SubItemImageIdx[j - 1] := 7;
      end;
    end;

  with (SectionListBox2.Sections.Add) do
  begin
    Caption := 'Multiline varia';
    SubItems.Add('To do for sales'#13'- Send email to salesperson'#13'- Make presentation');
    SubItems.Add('To do for R&D'#13'- Start up research project');
  end;

  with SectionListBox2 do
  begin
    lis := SectionListBox2.Sections.Add;
    lis.Caption := 'Rich text';
    with richedit do
    begin
      Text := 'This is some rich text here';
      SelStart := 0;
      SelLength := 4;
      SelAttributes.Style := [fsBold];
      SelStart := 13;
      SelLength := 9;
      SelAttributes.Style := [fsBold];
      SelAttributes.Color := clRed;
    end;
    s := RichToString(RichEdit);
    lis.SubItems.Add(s);


    with RichEdit do
    begin
      Clear;
      SelAttributes.Name := '';
      SelAttributes.Style := [];
      SelAttributes.Color := clBlack;
      Text := 'Exposing the power  of RTF in the list ';
      SelStart := 0;
      SelLength := 8;
      SelAttributes.Name := 'Courier';
      SelStart := 13;
      SelLength := 5;
      SelAttributes.Style := [fsBold, fsItalic];
      SelAttributes.Color := clGreen;
    end;
    s := RichToString(RichEdit);
    lis.SubItems.Add(s);
  end;


  with (SectionListBox3.Sections.Items[1] as TListSection).SubItems do
  begin
    Add('function GetItemSection(idx:integer):TListSection;'#13 +
        ' Gets section associated with listbox item idx');
    Add('function GetItemSectionIndex(idx:integer):integer;'#13 +
        ' Gets the section index of the listbox item idx');
    Add('function GetSectionListIndex(idx:integer):integer;'#13 +
        ' Gets the index in the listbox of the section');
    Add('function GetSectionsubItem(SectionIdx,SubItemIdx:integer):string;'#13 +
        ' Gets SubItem text in section');
    Add('function GetListItemIndex(listindex:integer;var SectionIdx,SubItemIdx:integer):boolean;'#13 +
        ' Gets section and SubItemindex of listbox item listindex ');
    Add('function GetSelection(var SectionIdx,SubItemIdx:integer;var selstring:string):boolean;'#13 +
        ' Gets section and SubItem index of the current selected listbox item');
    Add('function SetSelection(SectionIdx,SubItemIdx:integer):boolean;'#13 +
        ' Sets the listbox selection based on SectionIdx and SubItemIdx');
    Add('function IsSection(idx:integer):boolean;'#13 +
        ' Returns true if the listbox item idx is a section header');
    Add('procedure ExpandAll;'#13 +
        ' Sets all Sections into the expanded state');
    Add('procedure ContractAll;'#13 +
        ' Sets all Sections into the contracted state');
    Add('procedure SaveToFile(filename:string);'#13 +
        ' Saves Sections and its SubItems to a file');
    Add('procedure LoadFromFile(filename:string);'#13 +
        ' Retrieves Sections and its SubItems from a file');
    Add('procedure OptimizeTabs(padding:integer);'#13 +
        ' Sets the TabPosition elements to the min. required settings');
    Add('procedure Clear;'#13 +
        ' Removes all Sections and SubItems from the listbox');
    Add('procedure BeginUpdate;'#13 +
        ' Disables redrawing during lengthy item changes');
    Add('procedure EndUpdate;'#13 +
        ' Enables redrawing');
  end;

  SectionListBox10.ExpandAll;


end;

procedure TForm1.Label2Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'https://www.tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_NORMAL);
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
 SectionListBox3.OneExpanded := CheckBox3.Checked;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: SectionListBox2.NodeType := lnFlat;
    1: SectionListBox2.NodeType := ln3D;
    2: SectionListBox2.NodeType := lnGlyph;
  end;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    SectionListBox2.Activesection := asFull
  else
    SectionListBox2.Activesection := asNodeOnly;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
 if CheckBox4.Checked then
   SectionListBox2.SectionFocus := sf3D
 else
   SectionListBox2.SectionFocus := sfDash;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  i: Integer;
begin
  with SectionListBox5.Sections.Items[0] do
    for i := 1 to SubItems.Count do
    begin
      SubItemCheckState[i-1] := False;
    end;

  with SectionListBox5.Sections.Items[1] do
    RadioIndex := 0;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  i: Integer;
begin
  with SectionListBox5.Sections.Items[0] do
    for i := 1 to SubItems.Count do
    begin
      SubItemCheckState[i - 1] := (i = 1);
    end;

  with SectionListBox5.Sections.Items[1] do
    RadioIndex := 3;
end;

procedure TForm1.SectionListBox7DrawItem(Sender: TObject;
  Section: TListSection; SectionIdx, SubItemIdx: Integer; Canvas: TCanvas;
  ARect: TRect; astate: TOwnerDrawState);
var
  oldcol: tColor;
  r: trect;
begin
  r := ARect;
  case SectionIdx of
    0:
    begin
      oldcol := Canvas.Brush.Color;
      case SubItemIdx of
        0:Canvas.Brush.Color := clYellow;
        1:Canvas.Brush.Color := clLime;
        2:Canvas.Brush.Color := clRed;
        3:Canvas.Brush.Color := clBlue;
        4:Canvas.Brush.Color := clFuchsia;
        5:Canvas.Brush.Color := clAqua;
        6:Canvas.Brush.Color := clWhite;
        7:Canvas.Brush.Color := clBlack;
      end;
      InflateRect(ARect, -10, -2);
      ARect.Right := ARect.Left + 25;
      Canvas.FillRect(ARect);
      Canvas.Brush.Color := clBlack;
      Canvas.FrameRect(ARect);
      Canvas.Brush.Color := oldcol;
      Canvas.TextOut(ARect.Left + 35, ARect.Top, Section.SubItems[SubItemIdx]);
    end;
    1:
    begin
      Canvas.Font.Name := Section.SubItems[SubItemIdx];
      Canvas.TextOut(ARect.Left + 10, ARect.Top, Section.SubItems[SubItemIdx]);
    end;
    2:
    begin
      oldcol := Canvas.Brush.Color;
      case SubItemIdx of
        0:Canvas.Brush.Style := bsSolid;
        1:Canvas.Brush.Style := bsCross;
        2:Canvas.Brush.Style := bsBDiagonal;
        3:Canvas.Brush.Style := bsHorizontal;
        4:Canvas.Brush.Style := bsVertical;
      end;
      Canvas.Brush.Color := clBlack;
      InflateRect(ARect, -10, -2);
      ARect.Right := ARect.Left + 25;
      Canvas.FillRect(ARect);
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := clBlack;
      Canvas.FrameRect(ARect);
      Canvas.Brush.Color := oldcol;
      Canvas.TextOut(ARect.Left + 35, ARect.Top, Section.SubItems[SubItemIdx]);
    end;
  end;

  if odFocused in astate then
    DrawFocusRect(Canvas.Handle, r);
end;

procedure TForm1.SectionListBox9AnchorClick(Sender: TObject;
  index: Integer; anchor: String);
begin
  MessageDlg('You clicked : ' + anchor, mtInformation, [mbok], 0);
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  SectionListBox1.TabPosMove := CheckBox5.Checked;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  SectionListBox5.FullFocus := CheckBox6.Checked;
end;

end.
