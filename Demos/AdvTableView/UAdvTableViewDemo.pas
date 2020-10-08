{********************************************************************}
{ TMS TAdvTableView Demo                                             }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1996 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UAdvTableViewDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, HTMLabel,
  AdvCustomControl, AdvTableView, Vcl.StdCtrls, PictureContainer;

type
  TForm4 = class(TForm)
    Label4: TLabel;
    AdvTableView1: TAdvTableView;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    HTMLabel1: THTMLabel;
    Image1: TImage;
    Label6: TLabel;
    Label7: TLabel;
    PictureContainer1: TPictureContainer;
    PictureContainer2: TPictureContainer;
    procedure Label6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AdvTableView1ItemSelectionChanged(Sender: TObject);
    procedure AdvTableView1BeforeItemShowDetailControl(Sender: TObject;
      AItem: TAdvTableViewItem; ADetailControl: TControl; var AAllow: Boolean);
    procedure AdvTableView1ItemMoreOptionClick(Sender: TObject;
      AItem: TAdvTableViewItem; AMoreOption: TAdvTableViewMoreOption);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation
uses
  ShellAPI, AdvGraphicsTypes;

{$R *.dfm}

procedure TForm4.Label6Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm4.AdvTableView1BeforeItemShowDetailControl(Sender: TObject;
  AItem: TAdvTableViewItem; ADetailControl: TControl; var AAllow: Boolean);
begin
  Label2.Caption := 'Name : ' + AItem.HTMLTemplateItems.Values['NAME'];
  HTMLabel1.HTMLText.Text := AItem.HTMLTemplateItems.Values['DESCRIPTION'];
  Image1.Picture := PictureContainer2.FindBitmap(AItem.HTMLTemplateItems.Values['NAME']);
end;

procedure TForm4.AdvTableView1ItemMoreOptionClick(Sender: TObject;
  AItem: TAdvTableViewItem; AMoreOption: TAdvTableViewMoreOption);
begin
  case AMoreOption.Index of
    0: ShowMessage(AMoreOption.Text + ' clicked !');
    1: AdvTableView1.RemoveItem(AItem);
  end;
end;

procedure TForm4.AdvTableView1ItemSelectionChanged(Sender: TObject);
begin
  AdvTableView1.Footer.Text := AdvTableView1.SelectedItemCount.ToString + ' items selected';
end;

procedure TForm4.FormCreate(Sender: TObject);
var
  I: Integer;
  it: TAdvTableViewItem;
  t: string;
  mo: TAdvTableViewMoreOption;
const
  Names: array[0..18] of string = (
  'Alfa Romeo',
  'Audi',
  'BMW',
  'Chevrolet',
  'Citroen',
  'Ferrari',
  'Fiat',
  'Ford',
  'Honda',
  'Hyundai',
  'Infiniti',
  'Jaguar',
  'Jeep',
  'Seat',
  'Skoda',
  'Subaru',
  'Toyota',
  'Volkswagen',
  'Volvo'
  );
  Descriptions: array[0..18] of string =
  (
  'Alfa Romeo Automobiles S.p.A. is an Italian car manufacturer'+', founded as A.L.F.A. ("Anonima Lombarda Fabbrica Automobili", "Anonymous Lombard Automobile Factory") on 24 June 1910, in Milan.[3] The brand is known for sporty vehicles and has been involved in car racing since 1911.',
  'Audi AG is a German automobile manufacturer that designs, engineers, produces, markets and distributes luxury vehicles. Audi '+'is a member of the Volkswagen Group and has its roots at Ingolstadt, Bavaria, Germany. Audi-branded vehicles are produced in nine production facilities worldwide.',
  'Bayerische Motoren Werke AG, usually known under its abbreviation BMW, is a German luxury vehicle, sports car, motorcycle, and '+'engine manufacturing company founded in 1916. It is one of the best-selling luxury automakers in the world.',
  'Chevrolet, colloquially referred to as Chevy and formally the Chevrolet Division of General Motors Company, is an American automobile division of the American manufacturer General Motors (GM). ',
  'Citroën is a major French automobile manufacturer, part of the PSA Peugeot Citroën'+' group since 1976, founded in 1919 by French industrialist André-Gustave Citroën (1878–1935). In 1934, the firm established its reputation for innovative technology with the "Traction Avant".',
  'Ferrari N.V. is an Italian sports car manufacturer based in Maranello. Founded by Enzo Ferrari in 1939 out of '+'Alfa Romeo''s race division as Auto Avio Costruzioni, the company built its first car in 1940. However the company''s inception as an auto manufacturer is usually recognized in 1947, when the first Ferrari-badged car was completed.',
  'Fiat Automobiles S.p.A. is the largest automobile manufacturer in Italy, a subsidiary of FCA Italy S.p.A., which is part of Fiat Chrysler Automobiles.',
  'The Ford Motor Company (commonly referred to simply as "Ford") is an American multinational automaker headquartered in Dearborn, Michigan, a suburb of Detroit. It was founded by Henry Ford and incorporated on June 16, 1903.',
  'Honda Motor Co., Ltd. is a Japanese public multinational conglomerate corporation primarily known as a manufacturer of automobiles, aircraft, motorcycles, and power equipment.',
  'The Hyundai Motor Company is a South Korean multinational automotive manufacturer headquartered in Seoul, South Korea.',
  'Infiniti is the luxury vehicle division of Japanese automaker Nissan. Infiniti officially started selling vehicles on November 8, 1989 in North America. The marketing network for Infiniti-branded vehicles now includes dealers in over 50 countries.',
  'Jaguar is the luxury vehicle brand of Jaguar Land Rover, a British multinational car manufacturer with its headquarters in Whitley, Coventry, England, owned by the Indian company Tata Motors since 2008.',
  'Jeep is a brand of American automobiles that is a division of FCA US LLC (formerly Chrysler Group, LLC), a wholly owned'+' subsidiary of Fiat Chrysler Automobiles. The former Chrysler Corporation acquired the Jeep brand, along with the remaining assets of its owner American Motors, in 1987.',
  'SEAT, S.A. is a Spanish automobile manufacturer with its head office in Martorell, Catalonia, Spain.[3] It was founded on May 9, 1950, by the Instituto Nacional de Industria (INI), a state-owned industrial holding company.',
  'Škoda Auto, more commonly known as Škoda, is a Czech automobile manufacturer founded in 1895 as Laurin & Klement. It is headquartered in Mladá Boleslav, Czech Republic.',
  'Subaru is the automobile manufacturing division of Japanese transportation conglomerate Subaru Corporation (formerly known as Fuji Heavy Industries (FHI)), the twenty-second largest automaker by production worldwide in 2012.',
  'Toyota Motor Corporation is a Japanese multinational automotive manufacturer headquartered in Toyota, Aichi,'+' Japan. In March 2014, Toyota''s corporate structure consisted of 338,875 employees worldwide and, as of October 2016, was the ninth-largest company in the world by revenue.',
  'Volkswagen, shortened to VW, is a German automaker founded on May 28, 1937 by the German Labour Front and headquartered in Wolfsburg. It is the flagship marque of the Volkswagen Group, the largest automaker by worldwide sales in 2016.',
  'The Volvo Group is a Swedish multinational manufacturing company headquartered in Gothenburg.'
  );

  function FindIndexForName(AName: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to Length(Names) - 1 do
    begin
      if Names[I].ToLower = AName.ToLower then
      begin
        Result := I;
        Break;
      end;
    end;
  end;

  function GetDescriptionForIndex(AIndex: Integer): String;
  begin
    Result := '';
    if (AIndex >= 0) and (AIndex <= Length(Descriptions) - 1) then
      Result := Descriptions[AIndex];
  end;

begin
  AdvTableView1.BeginUpdate;
  AdvTableView1.PictureContainer:= PictureContainer1;
  AdvTableView1.Header.Text := 'Car List';
  AdvTableView1.Header.Font.Height := -24;
  AdvTableView1.Items.Clear;
  AdvTableView1.CategoryType := tvctAlphaBetic;

  AdvTableView1.ItemAppearance.HTMLTemplate := '<b><#NAME></b><br/><br/><font size="12" color="gcGray"><#DESCRIPTION></font>';

  for I := 0 to PictureContainer1.Items.Count - 1 do
  begin
    it := AdvTableView1.Items.Add;
    it.BitmapName := PictureContainer1.Items[I].Name;
    t := it.BitmapName.Replace('.png', '');

    it.HTMLTemplateItems.Values['NAME'] := t;
    it.HTMLTemplateItems.Values['DESCRIPTION'] := GetDescriptionForIndex(FindIndexForName(t));
    it.VerticalTextAlign := gtaLeading;
    it.CheckType := tvictCheckBox;
    if I mod (Random(5) + 1) = 0 then
    begin
      it.Accessory := tviaBadge;
      it.AccessoryText := inttostr(Random(5) + 4);
    end;
    Randomize;
  end;

  AdvTableView1.Items.Sort;

  mo := AdvTableView1.MoreOptions.Add;
  mo.Text := 'Buy';
  mo.Color := clGreen;
  mo.BorderColor := clGreen;
  mo := AdvTableView1.MoreOptions.Add;
  mo.Text := 'Delete';
  mo.Color := clRed;
  mo.BorderColor := clRed;

  AdvTableView1.Interaction.ShowEditButton := True;
  AdvTableView1.Interaction.ShowFilterButton := True;

  AdvTableView1.DefaultItem.DetailControl := Panel1;
  AdvTableView1.EndUpdate;

  HTMLabel1.Color := clWhite;
end;

end.
