{********************************************************************}
{ TMS TAdvGridFilter Demo                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright � 2012-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvGridFilterDemo;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, Grids, AdvObj, BaseGrid, AdvGrid,
  AdvUtil, ShellAPI,
  StdCtrls, asgprint, Buttons, AeroButtons, Spin,
  ComCtrls, ExtCtrls, advgridfilterpanel,
  AdvCustomFilterPanel;

type
  TForm1 = class(TForm)
    btnShowDialog: TButton;
    AdvStringGrid1: TAdvStringGrid;
    AdvGridFilterDialog1: TAdvGridFilterDialog;
    AdvGridFilterPanel1: TAdvGridFilterPanel;
    Panel1: TPanel;
    Label1: TLabel;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnShowDialogClick(Sender: TObject);
    procedure btnHFColorClick(Sender: TObject);
    procedure btnItemFillClick(Sender: TObject);
    procedure btnLabelColorClick(Sender: TObject);
    procedure btnContainerFillClick(Sender: TObject);
    procedure Label7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses math;

{$R *.dfm}

procedure TForm1.btnContainerFillClick(Sender: TObject);
begin
  AdvGridFilterPanel1.ColorStart := clCream;
  AdvGridFilterPanel1.ColorEnd := clGreen;
  AdvGridFilterPanel1.ColorDirection := gdVertical;
end;

procedure TForm1.btnHFColorClick(Sender: TObject);
begin
  AdvGridFilterPanel1.HeaderColorStart := clGreen;
  AdvGridFilterPanel1.HeaderColorEnd := clGreen;
  AdvGridFilterPanel1.FooterColorStart := clCream;
  AdvGridFilterPanel1.FooterColorEnd := clCream;
end;

procedure TForm1.btnItemFillClick(Sender: TObject);
begin
  AdvGridFilterPanel1.ItemColorStart := clGreen;
  AdvGridFilterPanel1.ItemColorEnd := clCream;
  AdvGridFilterPanel1.ItemColorDirection := gdHorizontal;
end;

procedure TForm1.btnLabelColorClick(Sender: TObject);
var
  F: TFont;
begin
  F := TFont.Create;
  F.Color := clBlue;
  AdvGridFilterPanel1.LabelFont := F;
  F.Free;
end;

procedure TForm1.btnShowDialogClick(Sender: TObject);
begin
  AdvGridFilterDialog1.Execute;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AdvStringGrid1.SaveFixedCells := False;
  AdvStringGrid1.LoadFromCSV('.\cars.csv');
  AdvStringGrid1.CheckFalse := '0';
  AdvStringGrid1.CheckTrue := '1';

  AdvStringGrid1.AddCheckBoxColumn(9,false,true);

  AdvStringGrid1.Cells[1,0] := 'Brand';
  AdvStringGrid1.Cells[2,0] := 'Type';
  AdvStringGrid1.Cells[3,0] := 'Hp';
  AdvStringGrid1.Cells[4,0] := 'Max Speed';
  AdvStringGrid1.Cells[5,0] := 'City';
  AdvStringGrid1.Cells[6,0] := 'Country';
  AdvStringGrid1.Cells[7,0] := 'Price';
  AdvStringGrid1.Cells[8,0] := 'Year Built';
  AdvStringGrid1.Cells[9,0] := 'Available';

  AdvGridFilterPanel1.Init;
end;

procedure TForm1.Label7Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
