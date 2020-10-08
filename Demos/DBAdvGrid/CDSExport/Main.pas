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
    BtnSave: TButton;
    RadioGroupFileTypes: TRadioGroup;
    SaveDialog: TSaveDialog;
    BtnPrint: TButton;
    CdsEmployee: TClientDataSet;
    procedure FormCreate(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnPrintClick(Sender: TObject);
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

procedure TFormMain.BtnSaveClick(Sender: TObject);
var
  FileName: String;
  sl: TStringList;
  i: Integer;

  function AddExtension(s,ext:string): string;
  begin
    if pos(uppercase('.'+ext),uppercase(s)) > 0 then
      result := s
    else
      result := s + '.' + ext;

  end;

begin
  sl := TStringList.Create;
  for i := 1 to GridEmployee.ColCount - 1 do
    sl.Add(GridEmployee.Cells[i,0]);

  GridEmployee.SaveFixedCells := false;

  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
    case RadioGroupFileTypes.ItemIndex of
    0:GridEmployee.SaveToFile(AddExtension(FileName,'dat'));
    1:GridEmployee.SaveToHTML(AddExtension(FileName,'html'));
    2:GridEmployee.SaveToASCII(AddExtension(FileName,'txt'));
    3:GridEmployee.SaveToXLS(AddExtension(FileName,'XLS'));
    4:GridEmployee.SaveToBinFile(AddExtension(FileName,'.bin'));
    5:GridEmployee.SaveToXML(AddExtension(Filename,'xml'),'records','record',sl);
    6:GridEmployee.SaveToCSV(AddExtension(FileName,'csv'));
    7:GridEmployee.SaveToDOC(AddExtension(FileName,'doc'));
    end;
    sl.Free;
  end;
end;

procedure TFormMain.BtnPrintClick(Sender: TObject);
begin
  GridEmployee.Print;
end;

end.
