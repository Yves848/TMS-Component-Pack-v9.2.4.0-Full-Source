unit Uasg83;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, AdvDropDown,
  AdvCustomGridDropDown, AdvGridDropDown, System.ImageList, Vcl.ImgList;

type
  TForm1 = class(TForm)
    AdvGridDropDown1: TAdvGridDropDown;
    ImageList1: TImageList;
    procedure AdvGridDropDown1BeforeDropDown(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  AdvStyleIF,  AdvGrid;

{$R *.dfm}

procedure TForm1.AdvGridDropDown1BeforeDropDown(Sender: TObject);
const
  cars: array[0..6] of string = ('MERCEDES','BMW','AUDI','PORSCHE','FERRARI','VOLVO','RENAULT');
  types: array[0..4] of string = ('SEDAN','COUPE','SPORT','CABRIO','BREAK');
var
  i: integer;
begin


  AdvGridDropDown1.Grid.FixedCols := 0;
  AdvGridDropDown1.Grid.RowCount := 100;

  AdvGridDropDown1.Grid.ClearCols(0,1);
  AdvGridDropDown1.Grid.ClearCols(AdvGridDropDown1.Grid.ColCount - 1,1);

  AdvGridDropDown1.Grid.Cells[1,0] := 'Status';
  AdvGridDropDown1.Grid.Cells[2,0] := 'Brand';
  AdvGridDropDown1.Grid.Cells[3,0] := 'Type';
  AdvGridDropDown1.Grid.Cells[4,0] := 'Price';
  AdvGridDropDown1.Grid.Cells[5,0] := 'Edit';

  AdvGridDropDown1.Grid.MouseActions.CheckAllCheck := true;
  AdvGridDropDown1.Grid.AddCheckBoxColumn(0);
  AdvGridDropDown1.Grid.SearchFooter.Visible := true;
  AdvGridDropDown1.Grid.ControlLook.NoDisabledCheckRadioLook := true;
  AdvGridDropDown1.Grid.ControlLook.NoDisabledButtonLook := true;



  for i := 1 to AdvGridDropDown1.Grid.RowCount - 1 do
  begin
    AdvGridDropDown1.Grid.AddImageIdx(1,i,random(6), habeforeText, vaTop);
    AdvGridDropDown1.Grid.Cells[2,i] := cars[random(6)];
    AdvGridDropDown1.Grid.Cells[3,i] := types[random(4)];
    AdvGridDropDown1.Grid.Ints[4,i] := Random(50000);
    AdvGridDropDown1.Grid.AddButton(AdvGridDropDown1.Grid.ColCount - 1, i , 64, 20, 'Edit', haBeforeText, vaTop);
  end;

  AdvGridDropDown1.Grid.SetComponentStyle(tsOffice2010Blue);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AdvGridDropDown1.UseItems := false;
  AdvGridDropDown1.Columns.Clear;
  AdvGridDropDown1.Columns.Add.Width := 24;
  AdvGridDropDown1.Columns.Add.Width := 24;
  AdvGridDropDown1.Columns.Add;
  AdvGridDropDown1.Columns.Add;
  AdvGridDropDown1.Columns.Add;
  AdvGridDropDown1.Columns.Add.Width := 72;
  AdvGridDropDown1.DropDownWidth := 480;
  AdvGridDropDown1.DropDownHeight := 240;
end;

end.
