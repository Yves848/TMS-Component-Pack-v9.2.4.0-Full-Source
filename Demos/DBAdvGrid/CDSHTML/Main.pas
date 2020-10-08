unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, DBAdvGrid, ExtCtrls, DBCtrls, DB,
  StdCtrls, AdvUtil, AdvObj, DBClient;

type
  TFormMain = class(TForm)
    GridAnimals: TDBAdvGrid;
    SrcAnimals: TDataSource;
    DBNavigator: TDBNavigator;
    CdsAnimals: TClientDataSet;
    CdsAnimalsNAME: TStringField;
    CdsAnimalsSIZE: TSmallintField;
    CdsAnimalsWEIGHT: TSmallintField;
    CdsAnimalsAREA: TStringField;
    CdsAnimalsBMP: TBlobField;
    procedure FormCreate(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  GridAnimals.Columns[2].Header:= 'SIZE AND WEIGHT';
  GridAnimals.Columns[2].HTMLTemplate:= '<FONT color="clBlue">Size is </FONT> <B><#SIZE> ft</B> <FONT color="clRed"> and  weight is </FONT> <B><#Weight> kg</B>';

  CdsAnimals.Active := true;
end;

end.
