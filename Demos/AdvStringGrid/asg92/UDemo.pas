unit UDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvUtil, AdvCustomComponent, AdvPDFIO,
  AdvGridPDFIO, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList, PictureContainer;

type
  TForm56 = class(TForm)
    Panel1: TPanel;
    Button2: TButton;
    AdvStringGrid1: TAdvStringGrid;
    AdvGridPDFIO1: TAdvGridPDFIO;
    PictureContainer1: TPictureContainer;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure AdvGridPDFIO1GetFooter(Sender, AExportObject: TObject;
      APageIndex: Integer; var AFooter: string);
    procedure AdvStringGrid1GetCellColor(Sender: TObject; ARow, ACol: Integer;
      AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
    procedure AdvStringGrid1GetAlignment(Sender: TObject; ARow, ACol: Integer;
      var HAlign: TAlignment; var VAlign: TVAlignment);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form56: TForm56;

implementation

{$R *.dfm}

procedure TForm56.AdvGridPDFIO1GetFooter(Sender, AExportObject: TObject;
  APageIndex: Integer; var AFooter: string);
begin
  AFooter := 'Page ' + IntToStr(APageIndex + 1);
end;

procedure TForm56.AdvStringGrid1GetAlignment(Sender: TObject; ARow,
  ACol: Integer; var HAlign: TAlignment; var VAlign: TVAlignment);
begin
  if ACol = 5 then
    HAlign := taRightJustify;
end;

procedure TForm56.AdvStringGrid1GetCellColor(Sender: TObject; ARow,
  ACol: Integer; AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
begin
  if ACol = 5 then
    AFont.Color := clRed;
end;

procedure TForm56.Button2Click(Sender: TObject);
begin
  AdvGridPDFIO1.Save('GridExport.pdf');
end;

procedure TForm56.FormCreate(Sender: TObject);
var
  pic: TPicture;
begin
  AdvStringGrid1.ColCount := 6;
  AdvStringGrid1.RandomFill;
  AdvStringGrid1.AutoNumberCol(0);
  AdvStringGrid1.Colors[2,2] := clRed;
  AdvStringGrid1.Colors[3,3] := clLime;
  AdvStringGrid1.Colors[4,4] := clYellow;

  AdvStringGrid1.Alignments[2,3] := taCenter;
  AdvStringGrid1.Alignments[3,4] := taRightJustify;

  AdvStringGrid1.FontStyles[1,1] := [TFontStyle.fsBold];
  AdvStringGrid1.FontStyles[1,2] := [TFontStyle.fsItalic];

  AdvStringGrid1.MergeCells(1,4,2,2);

  pic := TPicture.Create;
  pic.Assign(PictureContainer1.Items[0].Picture);
  AdvStringGrid1.AddPicture(1, 7, pic, True, noStretch, 0, haBeforeText, vaCenter);

  pic := TPicture.Create;
  pic.Assign(PictureContainer1.Items[1].Picture);
  AdvStringGrid1.AddPicture(1, 8, pic, True, noStretch, 0, haBeforeText, vaCenter);

  pic := TPicture.Create;
  pic.Assign(PictureContainer1.Items[2].Picture);
  AdvStringGrid1.AddPicture(1, 9, pic, True, noStretch, 0, haBeforeText, vaCenter);

  AdvGridPDFIO1.Options.OpenInPDFReader := True;
  AdvGridPDFIO1.Options.Header := 'TMS TAdvStringGrid PDF Export';
end;

end.
