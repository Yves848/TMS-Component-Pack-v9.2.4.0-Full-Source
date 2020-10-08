unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, DBAdvGrid, ExtCtrls, DBCtrls, DB,
  StdCtrls, DBGrids, Mask, AsgLinks, asgprev,
  AsgFindDialog, AdvUtil, DBClient, AdvObj;

type
  TFormMain = class(TForm)
    GridCountry: TDBAdvGrid;
    SrcCountry: TDataSource;
    DBNavigator: TDBNavigator;
    CheckBoxAllowEditing: TCheckBox;
    CheckBoxAdvanceOnEnter: TCheckBox;
    CheckBoxAdvanceInsert: TCheckBox;
    CheckBoxAllowInsertRow: TCheckBox;
    CheckBoxAllowDeleteRow: TCheckBox;
    BtnCopy: TButton;
    BtnPaste: TButton;
    BtnSearch: TButton;
    AdvPreviewDialog: TAdvPreviewDialog;
    BtnPreview: TButton;
    AdvGridFindDialog: TAdvGridFindDialog;
    CdsCountry: TClientDataSet;
    CdsCountryName: TStringField;
    CdsCountryCapital: TStringField;
    CdsCountryContinent: TStringField;
    CdsCountryArea: TFloatField;
    CdsCountryPopulation: TFloatField;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxAllowEditingClick(Sender: TObject);
    procedure CheckBoxAdvanceOnEnterClick(Sender: TObject);
    procedure CheckBoxAdvanceInsertClick(Sender: TObject);
    procedure CheckBoxAllowInsertRowClick(Sender: TObject);
    procedure CheckBoxAllowDeleteRowClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
    procedure GridCountryGetEditorProp(Sender: TObject; ACol, ARow: Integer; AEditLink: TEditLink);
    procedure GridCountryGetEditorType(Sender: TObject; ACol, ARow: Integer; var AEditor: TEditorType);
    procedure BtnSearchClick(Sender: TObject);
    procedure GridCountryCanEditCell(Sender: TObject; ARow, ACol: Integer; var CanEdit: Boolean);
    procedure BtnPreviewClick(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  GridCountry.FloatingFooter.Visible := true;

  CdsCountry.Open;
  CdsCountry.LogChanges := False;

  GridCountry.FloatingFooter.ColumnCalc[4] := acSum;

  with GridCountry do
  begin
    fixedcolor := clwhite;
    columns[0].Color := clWhite;
    columns[1].Color := clWhite;
    look := glXP;
  end;

end;

procedure TFormMain.CheckBoxAllowEditingClick(Sender: TObject);
begin
  if CheckBoxAllowEditing.Checked then
    GridCountry.Options:= GridCountry.Options + [goEditing]
  else
    GridCountry.Options:= GridCountry.Options - [goEditing];
end;

procedure TFormMain.CheckBoxAdvanceOnEnterClick(Sender: TObject);
begin
  GridCountry.Navigation.AdvanceOnEnter:= CheckBoxAdvanceOnEnter.Checked;
end;

procedure TFormMain.CheckBoxAdvanceInsertClick(Sender: TObject);
begin
  GridCountry.Navigation.AdvanceInsert:= CheckBoxAdvanceInsert.Checked;
end;

procedure TFormMain.CheckBoxAllowInsertRowClick(Sender: TObject);
begin
  GridCountry.Navigation.AllowInsertRow:= CheckBoxAllowInsertRow.Checked;
end;

procedure TFormMain.CheckBoxAllowDeleteRowClick(Sender: TObject);
begin
  GridCountry.Navigation.AllowDeleteRow:= CheckBoxAllowDeleteRow.Checked;
end;

procedure TFormMain.BtnCopyClick(Sender: TObject);
begin
  GridCountry.CopySelectionToClipboard;
end;

procedure TFormMain.BtnPasteClick(Sender: TObject);
begin
  GridCountry.PasteSelectionFromClipboard;
end;

procedure TFormMain.GridCountryGetEditorProp(Sender: TObject; ACol, ARow: Integer; AEditLink: TEditLink);
begin
  if aCol = 3 then
  begin
    GridCountry.Combobox.Items.Add('South America');
    GridCountry.Combobox.Items.Add('North America');
  end;
end;

procedure TFormMain.GridCountryGetEditorType(Sender: TObject; ACol,
  ARow: Integer; var AEditor: TEditorType);
begin
  case ACol of
    3:
    begin
      AEditor:= edComboEdit;
    end;
    4, 5: AEditor:= edPositiveNumeric;
  end;
end;

procedure TFormMain.BtnSearchClick(Sender: TObject);
begin
  AdvGridFindDialog.Execute;
end;

procedure TFormMain.GridCountryCanEditCell(Sender: TObject; ARow,
  ACol: Integer; var CanEdit: Boolean);
begin
  canedit := (acol > 2);
end;

procedure TFormMain.BtnPreviewClick(Sender: TObject);
begin
  AdvPreviewDialog.Execute ;
end;

end.
