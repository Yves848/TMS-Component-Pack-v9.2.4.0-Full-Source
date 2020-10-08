unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, DBAdvGrid, ExtCtrls, DBCtrls, DB,
  StdCtrls, AdvUtil, AdvObj, DBClient;

type
  TFormMain = class(TForm)
    GridBiolife: TDBAdvGrid;
    SrcBiolife: TDataSource;
    DBNavigator: TDBNavigator;
    BtnSetRange: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    CdsBiolife: TClientDataSet;
    CdsBiolifeSpeciesNo: TFloatField;
    CdsBiolifeCategory: TStringField;
    CdsBiolifeCommon_Name: TStringField;
    CdsBiolifeSpeciesName: TStringField;
    CdsBiolifeLengthcm: TFloatField;
    CdsBiolifeLength_In: TFloatField;
    CdsBiolifeNotes: TMemoField;
    CdsBiolifeGraphic: TGraphicField;
    procedure FormCreate(Sender: TObject);
    procedure BtnSetRangeClick(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  CdsBiolife.Open;
  CdsBiolife.LogChanges := False;
end;

procedure TFormMain.BtnSetRangeClick(Sender: TObject);
begin
  CdsBiolife.SetRange([Edit1.Text], [Edit2.Text]);
end;

end.
