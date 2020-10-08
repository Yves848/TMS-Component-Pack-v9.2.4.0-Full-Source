unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, DBAdvGrid, ExtCtrls, DBCtrls, DB,
  StdCtrls, AdvObj, AdvUtil, DBClient;

type
  TFormMain = class(TForm)
    GridBiolife: TDBAdvGrid;
    SrcBiolife: TDataSource;
    NavigatorBiolife: TDBNavigator;
    CheckBoxPictureFields: TCheckBox;
    BtnPrint: TButton;
    PrintDialog: TPrintDialog;
    CdsBiolife: TClientDataSet;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxPictureFieldsClick(Sender: TObject);
    procedure BtnPrintClick(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  CdsBiolife.Active := true;
  GridBiolife.Colwidths[GridBiolife.ColCount - 2] := 256;
  GridBiolife.AutoSizeRows(False, 4);
  GridBiolife.PrintSettings.NoAutoSize := true;
end;

procedure TFormMain.BtnPrintClick(Sender: TObject);
begin
  if printdialog.execute then
    GridBiolife.Print;
end;

procedure TFormMain.CheckBoxPictureFieldsClick(Sender: TObject);
begin
  GridBiolife.ShowPictureFields:= CheckBoxPictureFields.Checked;
end;

end.
