unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, DB, DBAdvGrid, StdCtrls,
  AdvUtil, AdvObj, DBClient;

type
  TFormMain = class(TForm)
    SrcBiolife: TDataSource;
    GridBiolife: TDBAdvGrid;
    CheckBoxSortBiolife: TCheckBox;
    GridCountry: TDBAdvGrid;
    DataSource2: TDataSource;
    CheckBoxSortCountry: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    CdsCountry: TClientDataSet;
    CdsBiolife: TClientDataSet;
    CdsCountryName: TStringField;
    CdsCountryCapital: TStringField;
    CdsCountryContinent: TStringField;
    CdsCountryArea: TFloatField;
    CdsCountryPopulation: TFloatField;
    CdsBiolifeSpeciesNo: TFloatField;
    CdsBiolifeCategory: TStringField;
    CdsBiolifeCommon_Name: TStringField;
    CdsBiolifeSpeciesName: TStringField;
    CdsBiolifeLengthcm: TFloatField;
    CdsBiolifeLength_In: TFloatField;
    CdsBiolifeNotes: TMemoField;
    CdsBiolifeGraphic: TGraphicField;
    procedure GridBiolifeCanSort(Sender: TObject; ACol: Integer; var DoSort: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxSortBiolifeClick(Sender: TObject);
    procedure CheckBoxSortCountryClick(Sender: TObject);
  private
    procedure SwitchBiolifeIndex(aField: TField; desc: Boolean);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.SwitchBiolifeIndex(aField: TField; desc: Boolean);
// create or set clientdataset index on field
var
  ixName: string;
  ixDef: TIndexDef;

    function ExistsIndex(const aName: string): Boolean;
    var
      i: Integer;
    begin
      for i := 0 to CdsBiolife.IndexDefs.Count - 1 do begin
        if CdsBiolife.IndexDefs[i].Name = aName then begin
          Result := True;
          Exit;
        end;
      end;
      Result := False;
    end;

begin
  ixName := 'ix_' + aField.FieldName;
  if desc then
    ixName := ixName + '_d';

  // if index exists switch to that index
  if not ExistsIndex(ixName) then
  begin
    ixDef := CdsBiolife.IndexDefs.AddIndexDef;
    ixDef.Fields := aField.FieldName;
    ixDef.Name := ixName;
    if desc then
      ixDef.DescFields := aField.FieldName;
  end;

  CdsBiolife.IndexName := ixName;
  CdsBiolife.First;

end;

procedure TFormMain.GridBiolifeCanSort(Sender: TObject; ACol: Integer; var DoSort: Boolean);
var
  fld: TField;
  fldname:string;
  desc: Boolean;
begin
  fld := CdsBiolife.FieldList.Fields[ACol - 1];

  if fld is TBlobField then begin
    ShowMessage('Cannot sort on blob fields');
    Abort;
  end;

  DoSort := False; // disable internal sort

  // toggle sort order
  if GridBiolife.SortSettings.Direction = sdAscending then
    GridBiolife.SortSettings.Direction := sdDescending
  else
    GridBiolife.SortSettings.Direction := sdAscending;

  // get field name of the column clicked
  fldname :=  fld.FieldName;

  desc := GridBiolife.SortSettings.Direction = sdDescending;
  SwitchBiolifeIndex(fld, desc);

  GridBiolife.SortSettings.Column := ACol;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  CdsCountry.Open;
  CdsBiolife.Open;
  SwitchBiolifeIndex(CdsBiolife.Fields[0], false);
end;

procedure TFormMain.CheckBoxSortBiolifeClick(Sender: TObject);
begin
  GridBiolife.SortSettings.Show:= CheckBoxSortBiolife.Checked;
end;

procedure TFormMain.CheckBoxSortCountryClick(Sender: TObject);
begin
  GridCountry.SortSettings.Show:= CheckBoxSortCountry.Checked;
end;

end.
