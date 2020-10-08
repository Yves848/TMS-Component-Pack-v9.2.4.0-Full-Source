unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids, BaseGrid, AdvGrid, DBAdvGrid,
  AdvUtil, AdvObj, DBClient, StdCtrls;

type
  TFormMain = class(TForm)
    GridPhoto: TDBAdvGrid;
    SrcPhoto: TDataSource;
    CdsPhoto: TClientDataSet;
    CdsPhotoID: TAutoIncField;
    CdsPhotoTITLE: TStringField;
    CdsPhotoDESCRIPTION: TStringField;
    CdsPhotoJPEG: TBlobField;
    procedure FormCreate(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  CdsPhoto.Active := True;
  CdsPhoto.LogChanges := False;
  CdsPhoto.IndexFieldNames := 'ID';
  GridPhoto.columns[4].PictureField := true;
  GridPhoto.Columns[4].Width := 350;
end;

end.
