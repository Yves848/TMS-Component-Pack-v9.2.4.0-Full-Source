unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, DBAdvGrid, ExtCtrls, DBCtrls, DB,
  StdCtrls, AdvUtil, AdvObj, DBClient;

type
  TFormMain = class(TForm)
    GridCountry: TDBAdvGrid;
    SrcCountry: TDataSource;
    DBNavigator: TDBNavigator;
    CdsCountry: TClientDataSet;
    CdsCountryName: TStringField;
    CdsCountryCapital: TStringField;
    CdsCountryContinent: TStringField;
    CdsCountryArea: TFloatField;
    CdsCountryPopulation: TFloatField;
    procedure FormCreate(Sender: TObject);
    procedure GridCountryGetRecordCount(Sender: TObject; var Count: Integer);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  CdsCountry.Open;
  CdsCountry.LogChanges := False;
end;

procedure TFormMain.GridCountryGetRecordCount(Sender: TObject; var Count: Integer);
begin
  Count := CdsCountry.RecordCount; // any value can be used here. for example Count := 3;
end;

end.
