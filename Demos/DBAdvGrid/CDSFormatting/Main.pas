unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, DBAdvGrid, ExtCtrls, DBCtrls, DB,
  StdCtrls, AdvUtil, AdvObj, DBClient;

type
  TFormMain = class(TForm)
    GridEmployee: TDBAdvGrid;
    SrcEmployee: TDataSource;
    DBNavigator: TDBNavigator;
    SaveDialog: TSaveDialog;
    CdsEmployee: TClientDataSet;
    CdsEmployeeEmpNo: TIntegerField;
    CdsEmployeeLastName: TStringField;
    CdsEmployeeFirstName: TStringField;
    CdsEmployeePhoneExt: TStringField;
    CdsEmployeeHireDate: TDateTimeField;
    CdsEmployeeSalary: TFloatField;
    procedure FormCreate(Sender: TObject);
    procedure CdsEmployeeSalaryGetText(Sender: TField; var Text: String; DisplayText: Boolean);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  CdsEmployee.Active := true;
  CdsEmployee.LogChanges := False;
end;

procedure TFormMain.CdsEmployeeSalaryGetText(Sender: TField; var Text: String; DisplayText: Boolean);
begin
  Text := Format('%.2m', [Sender.AsFloat]);
end;

end.
