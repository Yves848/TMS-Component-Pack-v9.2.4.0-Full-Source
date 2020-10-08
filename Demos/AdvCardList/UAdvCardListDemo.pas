{***************************************************************************}
{ TAdvCardList sample application                                           }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2019                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{***************************************************************************}

unit UAdvCardListDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, AdvCardList, StdCtrls, AdvCardListStyler, JPEG, ExtCtrls , ShellAPI,
  AdvStyleIF;

type
  TForm1 = class(TForm)
    AdvCardList1: TAdvCardList;
    AdvCardListStyler1: TAdvCardListStyler;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    CheckBox2: TCheckBox;
    ComboBox2: TComboBox;
    Label2: TLabel;
    Panel2: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddCard(ACaption, AType, AWeb:string; APrice: Integer; AImage:string; ADate: TDateTime; Avail: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R carddemo.res}
{$R *.dfm}

procedure TForm1.AddCard(ACaption, AType, AWeb: string; APrice: Integer;
  AImage: string; ADate: TDateTime; Avail: Boolean);
var
  ac: TAdvCard;
  ms: TMemoryStream;
  reshandle: THandle;
  hglobal: THandle;
  ptr: Pointer;
  ressize: integer;
  JPEGImage: TJPEGImage;

begin
  ac := AdvCardList1.Cards.Add;
  ac.Caption := ACaption;
  ac.ItemList[0].AsString := AType;
  ac.ItemList[1].AsString := AWeb;
  ac.ItemList[2].AsInteger := APrice;
  if AImage <> '' then
  begin
    reshandle := FindResource(hinstance, PChar(AImage), PChar(RT_RCDATA));
    if reshandle <> 0 then
    begin
      hglobal := LoadResource(hinstance, reshandle);
      ms := TMemoryStream.Create;
      try
        ressize := SizeOfResource(hinstance, reshandle);
        ptr := LockResource(hglobal);
        ms.WriteBuffer(ptr^,ressize);
        ms.Position := 0;
        JPEGImage := TJPEGImage.Create;

        try
          JPEGImage.LoadFromStream(ms);
          ac.ItemList[3].Picture.Assign(JPEGImage);
        finally
          JPEGImage.Free;
        end;  
      finally
        FreeResource(reshandle);
        ms.Free;
      end;
    end;
  end;
  ac.ItemList[4].AsDate := ADate;
  ac.ItemList[5].AsBoolean := Avail;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // load cards
  AdvCardList1.BeginUpdate;
  AddCard('Bentley','Continental','www.bentleymotors.com',225,'ben1',encodedate(2018,07,1),true);
  AddCard('BMW','i8','www.bmw.com',142,'bmw2',encodedate(2014,3,1),false);
  AddCard('Mercedes','AMG GT','www.mbusa.com',120,'merc6',encodedate(2015,6,1),true);
  AddCard('Mercedes','G63 AMG','www.mbusa.com',150,'merc10',encodedate(2012,11,1),false);
  AddCard('Audi','R8','www.audi.com',190,'aud4',encodedate(2015,4,1),false);
  AddCard('Porsche','Panamera Turbo','www.porsche.com',160,'por6',encodedate(2016,8,1),false);
  AddCard('Porsche','918 Spyder','www.porsche.com',850,'por4',encodedate(2013,3,1),false);
  AddCard('BMW','7 Serie','www.bmw.com',100,'bmw6',encodedate(2015,2,1),true);
  AddCard('McLaren','MP4-12c','www.mclaren.com',260,'mcl3',encodedate(2011,9,1),false);
  AddCard('Aston Martin','Vanquish','www.astonmartin.com',290,'ast3',encodedate(2015,2,1),true);
  AddCard('Ferrari','458 Italia','www.ferrari.it',200,'fer5',encodedate(2009,2,1),true);
  AdvCardList1.EndUpdate;
  // present horizontal scrollbar
  AdvCardList1.LeftCol := 0;

  FillStyleList(ComboBox2.Items);
  Combobox2.ItemIndex := 2;
   AdvCardListStyler1.SetComponentStyle(TTMSStyle(combobox2.Items.Objects[2]));
end;

procedure TForm1.Label4Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://www.tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  case combobox1.itemindex of
  0: Advcardlist1.SortSettings.SortType := stCaption;
  1,2,3:
    begin
      Advcardlist1.SortSettings.SortIndex := combobox1.itemindex - 1;
      Advcardlist1.SortSettings.SortType := stItem;
    end;
  4,5:
    begin
      AdvCardList1.SortSettings.SortIndex := combobox1.itemindex;
      AdvCardList1.SortSettings.SortType := stItem;
    end;
  end;
  AdvCardList1.LeftCol := 0;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  AdvCardList1.MultiSelect := checkbox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  AdvCardList1.CardHoverAppearance.Enabled := checkbox2.Checked;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  AdvCardListStyler1.SetComponentStyle(TTMSStyle(combobox2.Items.Objects[Combobox2.ItemIndex]));
end;

end.
