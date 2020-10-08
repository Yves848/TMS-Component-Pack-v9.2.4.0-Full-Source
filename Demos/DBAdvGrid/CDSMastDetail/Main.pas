unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, BaseGrid, AdvGrid, DBAdvGrid, AdvUtil,
  AdvObj, DBClient, Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    DBAdvGrid1: TDBAdvGrid;
    DBAdvGrid2: TDBAdvGrid;
    SrcCustomer: TDataSource;
    SrcOrders: TDataSource;
    CdsCustomer: TClientDataSet;
    CdsOrders: TClientDataSet;
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
    CdsOrdersOrderNo: TFloatField;
    CdsOrdersCustNo: TFloatField;
    CdsOrdersSaleDate: TDateTimeField;
    CdsOrdersShipDate: TDateTimeField;
    CdsOrdersEmpNo: TIntegerField;
    CdsOrdersShipToContact: TStringField;
    CdsOrdersShipToAddr1: TStringField;
    CdsOrdersShipToAddr2: TStringField;
    CdsOrdersShipToCity: TStringField;
    CdsOrdersShipToState: TStringField;
    CdsOrdersShipToZip: TStringField;
    CdsOrdersShipToCountry: TStringField;
    CdsOrdersShipToPhone: TStringField;
    CdsOrdersShipVIA: TStringField;
    CdsOrdersPO: TStringField;
    CdsOrdersTerms: TStringField;
    CdsOrdersPaymentMethod: TStringField;
    CdsOrdersItemsTotal: TCurrencyField;
    CdsOrdersTaxRate: TFloatField;
    CdsOrdersFreight: TCurrencyField;
    CdsOrdersAmountPaid: TCurrencyField;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  CdsCustomer.Open;
  CdsOrders.Open;
end;

end.
