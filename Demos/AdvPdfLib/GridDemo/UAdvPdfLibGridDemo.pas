{********************************************************************}
{ TAdvPdfLib demo                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvPdfLibGridDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Grids, AdvObj,
  BaseGrid, AdvGrid, advgridpdfio, ImgList, AdvPDFIO, AdvPDFLib, ExtCtrls,
  AdvUtil, AdvCustomComponent, System.ImageList, PictureContainer;

type
  TForm1 = class(TForm)
    AdvStringGrid1: TAdvStringGrid;
    ImageList1: TImageList;
    Panel1: TPanel;
    AdvGridPDFIO1: TAdvGridPDFIO;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    RadioGroup1: TRadioGroup;
    PictureContainer1: TPictureContainer;
    Panel2: TPanel;
    Label1: TLabel;
    Label16: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure AdvStringGrid1GetCellColor(Sender: TObject; ARow, ACol: Integer;
      AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Label16Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitGrid;
  end;

var
  Form1: TForm1;

implementation

uses
  ShellAPI;

{$R *.dfm}

var
  bmp: TPicture;

procedure TForm1.AdvStringGrid1GetCellColor(Sender: TObject; ARow,
  ACol: Integer; AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
begin
  if (ARow = 6) and (ACol = 1) then
  begin
    ABrush.Color := clSilver;
    AFont.Color := clBlack;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  fn: string;
begin
  if SaveDialog1.Execute then
  begin
    fn := SaveDialog1.FileName;

    if ExtractFileExt(fn) = '' then
      fn := fn + '.PDF';

    case radiogroup1.ItemIndex of
    0: AdvGridPDFIO1.Options.PageOrientation := poPortrait;
    1: AdvGridPDFIO1.Options.PageOrientation := poLandscape;
    end;

    AdvGridPDFIO1.Options.Header := 'TAdvStringGrid PDF export demo';
    AdvGridPDFIO1.Save(fn);

    ShellExecute(0,'open',PChar(fn),nil,nil,SW_NORMAL);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  ico: TIcon;
begin
  ico := TIcon.Create;
  for I := 0 to ImageList1.Count - 1 do
  begin
    ImageList1.GetIcon(I, ico);
    with PictureContainer1.Items.Add do
    begin
      Picture.Assign(ico);
      Name := IntToStr(I);
    end;
  end;
  ico.free;

  InitGrid;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bmp.Free;
end;

procedure TForm1.InitGrid;
begin
  /// Property setters
  AdvStringGrid1.Font.Name := 'Arial';
  AdvStringGrid1.Options := AdvStringGrid1.Options + [goColSizing, goRowSizing];
  AdvStringGrid1.URLShow := True;

  /// 4 page rendering: 2 horiz & 2 vert
  AdvStringGrid1.ColCount := 6;
  AdvStringGrid1.RowCount := 20;

  AdvStringGrid1.AutoNumberCol(0);
  AdvStringGrid1.AutoNumberRow(0);

  /// single text cell
  AdvStringGrid1.Cells[1,1] := 'single cell text';

  /// left, center, right alignment
  AdvStringGrid1.Cells[1,2] := 'Left aligned text';
  AdvStringGrid1.Alignments[1,2] := taLeftJustify;
  AdvStringGrid1.Cells[2,2] := 'Center aligned text';
  AdvStringGrid1.Alignments[2,2] := taCenter;
  AdvStringGrid1.Cells[3,2] := 'Right aligned text';
  AdvStringGrid1.Alignments[3,2] := taRightJustify;

  /// text via properties (color, font)
  AdvStringGrid1.Colors[1,3] := clred;
  AdvStringGrid1.FontColors[1,3] := clWhite;
  AdvStringGrid1.Cells[1,3] := 'red bg, white font';

  /// wordwrapped cell text
  AdvStringGrid1.VAlignment := TVAlignment.vtaBottom;
  AdvStringGrid1.WordWraps[1,4] := True;
  AdvStringGrid1.Cells[1,4] := 'This is a wordwrapped cell, which means the text will wrap itself inside a single cell & is bottom aligned';
  AdvStringGrid1.VAlignment := TVAlignment.vtaTop;

  /// checkbox with(out) text
  AdvStringGrid1.AddCheckBox(1,5,True,False);
  AdvStringGrid1.Cells[1,5] := 'textqsdfqf';

  /// Cell properties via OnGetCellColor
  AdvStringGrid1.Cells[1,6] := 'OnGetCellColor';

  /// AddImageIdx with(out) text
  AdvStringGrid1.AddImageIdx(1,1,0,TCellHAlign.haCenter, TCellVAlign.vaCenter);
  AdvStringGrid1.AddImageIdx(1,2,1,TCellHAlign.haBeforeText, TCellVAlign.vaCenter);
  AdvStringGrid1.AddImageIdx(1,3,2,TCellHAlign.haRight, TCellVAlign.vaCenter);

  bmp := TPicture.Create;

  bmp.Graphic := PictureContainer1.Items.Items[3].Picture;

  AdvStringGrid1.AddPicture(1,6,bmp,true,ShrinkWithAspectRatio, 0, TCellHAlign.haBeforeText,TCellVAlign.vaCenter);

  /// cell merging
  ///  vertical col merge
  AdvStringGrid1.MergeCells(1,9,1,3);
  AdvStringGrid1.Cells[1,9] := 'Vertical Merge';
  ///  horizontal row merge
  AdvStringGrid1.MergeCells(1,7,3,1);
  AdvStringGrid1.Cells[1,7] := 'Horizontal Merge';
  ///  hor & vert merge
  AdvStringGrid1.MergeCells(3,9,2,2);
  AdvStringGrid1.Cells[3,9] := 'Horizontal & Vertical Merge';
  ///  with(out) wordwrapped text


  /// Hyperlinks
  ///  single cells
  AdvStringGrid1.Cells[2,12] := 'https://www.tmssoftware.com ';
  ///  merged cells
  ///  horizontal row merge
  AdvStringGrid1.MergeCells(3,12,2,1);
  AdvStringGrid1.Cells[3,12] := 'http://www.google.com';

  /// HTML
  ///  images
  AdvStringGrid1.Cells[1,13] := 'This is html <FONT color="clblue"> cell</FONT> <B>[1:13]</B>';
  AdvStringGrid1.Cells[2,13] := '<IMG src="4"/> html image';
  AdvStringGrid1.Cells[3,13] := 'this is <i>italic</i> text';
  AdvStringGrid1.Cells[3,14] := 'this is <U>underlined</U> text';
  ///  links
  AdvStringGrid1.Cells[1,15] := 'html link to the <a href="https://tmssoftware.com/site/tmsallaccess.asp"> All-Access Pack</a> site';


  /// Foreign Fonts
  AdvStringGrid1.Cells[1,16] := 'Russian';
  AdvStringGrid1.Cells[2,16] := 'Greek';
  AdvStringGrid1.Cells[3,16] := 'Arabic';

  AdvStringGrid1.Cells[1,17] := '??? ?? ???????? ??????';
  AdvStringGrid1.Cells[2,17] := '?? s?µ?????e dese???se µea';
  AdvStringGrid1.Cells[3,17] := '??? ?? ?????? ???????. ?? ??? ?????';

  /// Autosize
  AdvStringGrid1.AutoSizeColumns(true);
  AdvStringGrid1.ColWidths[1] := 250;
  AdvStringGrid1.RowHeights[4] := 40;

end;

procedure TForm1.Label16Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
