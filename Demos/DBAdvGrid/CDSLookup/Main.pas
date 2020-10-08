unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids, BaseGrid, AdvGrid, DBAdvGrid,
  AdvUtil, AdvObj, DBClient, StdCtrls;

type
  TFormMain = class(TForm)
    DBGridEmployees: TDBGrid;
    DBAdvGridEmployees: TDBAdvGrid;
    DBGridCountries: TDBGrid;
    DBGridDepartments: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SrcCountry: TDataSource;
    SrcDepartment: TDataSource;
    SrcEmployee: TDataSource;
    CdsCountry: TClientDataSet;
    CdsCountryID: TAutoIncField;
    CdsCountryCOUNTRYNAME: TStringField;
    CdsDepartment: TClientDataSet;
    CdsDepartmentID: TAutoIncField;
    CdsDepartmentDEPARTMENT: TStringField;
    CdsEmployee: TClientDataSet;
    CdsEmployeeNAME: TStringField;
    CdsEmployeeAGE: TIntegerField;
    CdsEmployeeDEP: TIntegerField;
    CdsEmployeeCOUNTRY: TIntegerField;
    CdsEmployeeCOUNTRYNAME: TStringField;
    CdsEmployeeDEPARTMENT: TStringField;
    procedure FormCreate(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  CdsCountry.Active := True;
  CdsDepartment.Active := True;
  CdsEmployee.Active := True;

  CdsCountry.LogChanges := False;
  CdsDepartment.LogChanges := False;
  CdsEmployee.LogChanges := False;

end;

end.
