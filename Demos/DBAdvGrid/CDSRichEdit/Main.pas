unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids, BaseGrid, AdvGrid, DBAdvGrid,
  StdCtrls, Buttons, ExtCtrls, DBCtrls, AdvUtil, AdvObj, DBClient;

type
  TFormMain = class(TForm)
    GridEventsInfo: TDBAdvGrid;
    SrcEventsInfo: TDataSource;
    ItalBtn: TSpeedButton;
    BoldBtn: TSpeedButton;
    UnderBtn: TSpeedButton;
    ColorBtn: TSpeedButton;
    FontBtn: TSpeedButton;
    LeftBtn: TSpeedButton;
    CenterBtn: TSpeedButton;
    RightBtn: TSpeedButton;
    ComboFontName: TComboBox;
    ComboFontSize: TComboBox;
    ColorDialog: TColorDialog;
    FontDialog: TFontDialog;
    DBNavigator: TDBNavigator;
    CdsEventInfo: TClientDataSet;
    CdsEventInfoEVENT_NAME: TStringField;
    CdsEventInfoVANUENO: TAutoIncField;
    CdsEventInfoEVENT_DESCRIPTION: TMemoField;
    CdsEventInfoTICKET_PRICE: TCurrencyField;
    CdsEventInfoEVENT_DATE: TDateField;
    procedure FormCreate(Sender: TObject);
    procedure GridEventsInfoGetEditorType(Sender: TObject; ACol, ARow: Integer; var AEditor: TEditorType);
    procedure BoldBtnClick(Sender: TObject);
    procedure ItalBtnClick(Sender: TObject);
    procedure UnderBtnClick(Sender: TObject);
    procedure ColorBtnClick(Sender: TObject);
    procedure FontBtnClick(Sender: TObject);
    procedure ComboFontNameChange(Sender: TObject);
    procedure ComboFontSizeChange(Sender: TObject);
    procedure LeftBtnClick(Sender: TObject);
    procedure CenterBtnClick(Sender: TObject);
    procedure RightBtnClick(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TStrings(Data).Add(LogFont.lfFaceName);
  Result := 1;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  DC: HDC;
  i: Integer;
begin
  DC := GetDC(0);
  EnumFonts(DC, nil, @EnumFontsProc, Pointer(ComboFontName.Items));
  ReleaseDC(0, DC);
  ComboFontName.Sorted := True;
  ComboFontName.ItemIndex := ComboFontName.Items.IndexOf(Self.Font.Name);
  if ComboFontName.ItemIndex < 0 then ComboFontName.ItemIndex := 0;
  ComboFontSize.ItemIndex := 3;

  CdsEventInfo.Open;
  CdsEventInfo.LogChanges := False;

  GridEventsInfo.Columns[3].Width := 320;

end;

procedure TFormMain.GridEventsInfoGetEditorType(Sender: TObject; ACol,
  ARow: Integer; var AEditor: TEditorType);
begin
  if ACol = 3 then
    aEditor := edRichEdit;
end;

procedure TFormMain.BoldBtnClick(Sender: TObject);
begin
  if GridEventsInfo.InplaceRichEdit.Visible then
  if fsBold in GridEventsInfo.InplaceRichEdit.SelAttributes.Style then
   GridEventsInfo.InplaceRichEdit.SelAttributes.Style := GridEventsInfo.InplaceRichEdit.SelAttributes.Style-[fsBold]
  else
   GridEventsInfo.InplaceRichEdit.SelAttributes.Style := GridEventsInfo.InplaceRichEdit.SelAttributes.Style+[fsBold]
end;

procedure TFormMain.ItalBtnClick(Sender: TObject);
begin
  if GridEventsInfo.InplaceRichEdit.Visible then
  if fsItalic in GridEventsInfo.InplaceRichEdit.SelAttributes.Style then
   GridEventsInfo.InplaceRichEdit.SelAttributes.Style := GridEventsInfo.InplaceRichEdit.SelAttributes.Style-[fsItalic]
  else
   GridEventsInfo.InplaceRichEdit.SelAttributes.Style := GridEventsInfo.InplaceRichEdit.SelAttributes.Style+[fsItalic]
end;

procedure TFormMain.UnderBtnClick(Sender: TObject);
begin
  if GridEventsInfo.InplaceRichEdit.Visible then
  if fsUnderline in GridEventsInfo.InplaceRichEdit.SelAttributes.Style then
   GridEventsInfo.InplaceRichEdit.SelAttributes.Style := GridEventsInfo.InplaceRichEdit.SelAttributes.Style-[fsUnderline]
  else
   GridEventsInfo.InplaceRichEdit.SelAttributes.Style := GridEventsInfo.InplaceRichEdit.SelAttributes.Style+[fsUnderline]
end;

procedure TFormMain.ColorBtnClick(Sender: TObject);
begin
  if not GridEventsInfo.InplaceRichEdit.Visible then Exit;

  ColorDialog.Color := GridEventsInfo.InplaceRichEdit.SelAttributes.Color;
  if ColorDialog.Execute then
  begin
    GridEventsInfo.InplaceRichEdit.SelAttributes.Color := ColorDialog.Color;
  end;

end;

procedure TFormMain.FontBtnClick(Sender: TObject);
begin
  if not GridEventsInfo.InplaceRichEdit.Visible then Exit;

  FontDialog.Font.Name := GridEventsInfo.InplaceRichEdit.SelAttributes.Name;
  FontDialog.Font.Style := GridEventsInfo.InplaceRichEdit.SelAttributes.Style;
  FontDialog.Font.Size := GridEventsInfo.InplaceRichEdit.SelAttributes.Size;

  if FontDialog.Execute then
  begin
    GridEventsInfo.InplaceRichEdit.SelAttributes.Name := FontDialog.Font.Name;
    GridEventsInfo.InplaceRichEdit.SelAttributes.Style := FontDialog.Font.Style;
    GridEventsInfo.InplaceRichEdit.SelAttributes.Size := FontDialog.Font.Size;
  end;
end;

procedure TFormMain.ComboFontNameChange(Sender: TObject);
begin
  if GridEventsInfo.InplaceRichEdit.Visible then
  GridEventsInfo.InplaceRichEdit.SelAttributes.Name := ComboFontName.Items[ComboFontName.ItemIndex];

end;

procedure TFormMain.ComboFontSizeChange(Sender: TObject);
begin
  if GridEventsInfo.InplaceRichEdit.Visible then
    GridEventsInfo.InplaceRichEdit.SelAttributes.Size := StrToInt(ComboFontSize.Items[ComboFontSize.ItemIndex]);

end;

procedure TFormMain.LeftBtnClick(Sender: TObject);
begin
  if GridEventsInfo.InplaceRichEdit.Visible then
    GridEventsInfo.InplaceRichEdit.Paragraph.Alignment := taLeftJustify;

end;

procedure TFormMain.CenterBtnClick(Sender: TObject);
begin
  if GridEventsInfo.InplaceRichEdit.Visible then
    GridEventsInfo.InplaceRichEdit.Paragraph.Alignment := taCenter;

end;

procedure TFormMain.RightBtnClick(Sender: TObject);
begin
  if GridEventsInfo.InplaceRichEdit.Visible then
    GridEventsInfo.InplaceRichEdit.Paragraph.Alignment := taRightJustify;
end;

end.
