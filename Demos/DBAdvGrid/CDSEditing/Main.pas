unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, DBAdvGrid, ExtCtrls, DBCtrls, DB,
  StdCtrls, DBGrids, Mask, AsgLinks, asgprev, AdvUtil,
  AdvObj, DBClient, Provider;

type
  TFormMain = class(TForm)
    GridCountry: TDBAdvGrid;
    SrcCountry: TDataSource;
    DBNavigatorCountry: TDBNavigator;
    CheckBoxAdvanceOnEnter: TCheckBox;
    CheckBoxAdvanceInsert: TCheckBox;
    CheckBoxAllowEditing: TCheckBox;
    CheckBoxAllowInsertRow: TCheckBox;
    CheckBoxAllowDeleteRow: TCheckBox;
    BtnCopy: TButton;
    BtnPaste: TButton;
    Button3: TButton;
    EditSearch: TEdit;
    AdvPreviewDialog1: TAdvPreviewDialog;
    BtnPreview: TButton;
    Label1: TLabel;
    EditName: TDBEdit;
    Label2: TLabel;
    Label3: TLabel;
    EditCapital: TDBEdit;
    BtnOpenClose: TButton;
    CdsCountry: TClientDataSet;
    CdsCountryName: TStringField;
    CdsCountryCapital: TStringField;
    CdsCountryContinent: TStringField;
    CdsCountryArea: TFloatField;
    CdsCountryPopulation: TFloatField;
    test: TClientDataSet;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxAllowEditingClick(Sender: TObject);
    procedure CheckBoxAdvanceOnEnterClick(Sender: TObject);
    procedure CheckBoxAdvanceInsertClick(Sender: TObject);
    procedure CheckBoxAllowInsertRowClick(Sender: TObject);
    procedure CheckBoxAllowDeleteRowClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
    procedure GridCountryGetEditorType(Sender: TObject; ACol, ARow: Integer;
      var AEditor: TEditorType);
    procedure Button3Click(Sender: TObject);
    procedure BtnPreviewClick(Sender: TObject);
    procedure BtnOpenCloseClick(Sender: TObject);
    procedure GridCountryGetEditorProp(Sender: TObject; ACol, ARow: Integer;
      AEditLink: TEditLink);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
//  GridCountry.FloatingFooter.Visible := true;
  CdsCountry.Active := true;
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

procedure TFormMain.GridCountryGetEditorProp(Sender: TObject; ACol,
  ARow: Integer; AEditLink: TEditLink);
begin
  case ACol of
    3:
    begin
      GridCountry.Combobox.Items.Add('South America');
      GridCountry.Combobox.Items.Add('North America');
    end;
  end;

end;

procedure TFormMain.GridCountryGetEditorType(Sender: TObject; ACol,
  ARow: Integer; var AEditor: TEditorType);
begin
  case ACol of
    1,2: AEditor := edNormal;
    3: AEditor:= edComboEdit;
    4, 5: AEditor:= edPositiveNumeric;
  end;
end;

procedure TFormMain.Button3Click(Sender: TObject);
var
  pt:tpoint;
begin
  pt := point(-1,-1);
  pt := GridCountry.Find(pt,EditSearch.text,[fnAutoGoto]);
  if pt.Y = -1 then
    ShowMessage('Field not found');
end;

procedure TFormMain.BtnPreviewClick(Sender: TObject);
begin
  advpreviewdialog1.Execute ;
end;

procedure TFormMain.BtnOpenCloseClick(Sender: TObject);
begin
  if not CdsCountry.Active then
  begin
    CdsCountry.Active := true;
    BtnOpenClose.Caption := 'Close dataset';
  end
  else
  begin
    CdsCountry.Active := false;
    BtnOpenClose.Caption := 'Open dataset';
  end;
end;

end.
