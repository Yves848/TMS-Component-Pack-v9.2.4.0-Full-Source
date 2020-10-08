unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, BaseGrid, AdvGrid, DBAdvGrid, ExtCtrls, DBCtrls, DB,
  StdCtrls, DBGrids, AdvUtil, AdvObj, DBClient;

type
  TFormMain = class(TForm)
    GridCustomer: TDBAdvGrid;
    SrcCustomer: TDataSource;
    ListBox: TListBox;
    BtnShowSelection: TButton;
    CdsCustomer: TClientDataSet;
    CdsCustomerCustNo: TFloatField;
    CdsCustomerCompany: TStringField;
    CdsCustomerAddr1: TStringField;
    CdsCustomerAddr2: TStringField;
    CdsCustomerCity: TStringField;
    CdsCustomerState: TStringField;
    CdsCustomerZip: TStringField;
    CdsCustomerCountry: TStringField;
    CdsCustomerPhone: TStringField;
    CdsCustomerFAX: TStringField;
    CdsCustomerTaxRate: TFloatField;
    CdsCustomerContact: TStringField;
    CdsCustomerLastInvoiceDate: TDateTimeField;
    procedure BtnShowSelectionClick(Sender: TObject);
    procedure GridCustomerGetCellColor(Sender: TObject; ARow, ACol: Integer; AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
    procedure GridCustomerCheckBoxClick(Sender: TObject; ACol, ARow: Integer; State: Boolean);
    procedure FormCreate(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.BtnShowSelectionClick(Sender: TObject);
var
  i,j: integer;
  state: boolean;
  bookmark:string;
begin
  ListBox.Items.Clear;
  GridCustomer.BeginUpdate;
  CdsCustomer.DisableControls;

  j := 0;
  CdsCustomer.First;
  for i := 1 to GridCustomer.RowCount - 1 do
  begin
    if GridCustomer.GetCheckBoxState(1,i,state) then
    begin
      if state then
      begin
        CdsCustomer.MoveBy(i - 1 - j);
        ListBox.items.Add(CdsCustomer.FieldByName('Company').AsString + ' : '+ CdsCustomer.FieldByName('Country').AsString);
        j := i - 1;
      end;
    end;
  end;
  CdsCustomer.EnableControls;
  GridCustomer.EndUpdate;
end;

procedure TFormMain.GridCustomerGetCellColor(Sender: TObject; ARow,
  ACol: Integer; AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
var
  state: boolean;
begin
  if (ACol >= GridCustomer.FixedCols) and GridCustomer.GetCheckBoxState(1, ARow,state) then
  begin
    if state then
    begin
      ABrush.Color := clRed;
      AFont.Color := clWhite;
      AFont.Style := [fsBold];
    end
    else
    begin
      ABrush.Color := clWindow;
      AFont.Color := clWindowText;
      AFont.Style := [];
    end;
  end;
end;


procedure TFormMain.GridCustomerCheckBoxClick(Sender: TObject; ACol,
  ARow: Integer; State: Boolean);
begin
  GridCustomer.RepaintRow(ARow);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  CdsCustomer.Open;
  CdsCustomer.LogChanges := False;
end;

end.


