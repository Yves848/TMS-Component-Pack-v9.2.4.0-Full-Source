{*************************************************************************}
{ TMS TWebData Demo                                                       }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2001 - 2019                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{*************************************************************************}

unit UWebDataDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, WebData, StdCtrls, Grids, ShellAPI;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    Button1: TButton;
    WebData1: TWebData;
    Button2: TButton;
    Label19: TLabel;
    Label18: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  prod: string;
  i: Integer;
begin
  WebData1.Data.Clear;

  for i := 1 to stringgrid1.RowCount - 1 do
  begin
    prod := stringgrid1.Cells[1,i];
    with webdata1.Data.Add do
    begin
      ScanFirst := prod;
      ScanFrom := 'shopping-cart"></i>';
      ScanTo := '</td>';
      URL := 'https://tmssoftware.com/site/pricelist.asp';
    end;
  end;

  webdata1.Execute;

  for i := 1 to stringgrid1.RowCount - 1 do
  begin
    StringGrid1.Cells[2, i] := WebData1.Data.Items[i - 1].Data;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  stringgrid1.Cells[1, 0] := 'Product';
  stringgrid1.Cells[2, 0] := 'Price';
  stringgrid1.Cells[1, 1] := 'TMS ALL-ACCESS';
  stringgrid1.Cells[1, 2] := 'TMS VCL Subscription';
  stringgrid1.Cells[1, 3] := 'TMS Component Studio';
  stringgrid1.Cells[1, 4] := 'TMS FNC Component Studio';
  stringgrid1.Cells[1, 5] := 'TMS Business Subscription';
  stringgrid1.ColWidths[0] := 20;
  stringgrid1.ColWidths[1] := 220;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  stringgrid1.RowCount := stringgrid1.RowCount + 1;
end;

end.
