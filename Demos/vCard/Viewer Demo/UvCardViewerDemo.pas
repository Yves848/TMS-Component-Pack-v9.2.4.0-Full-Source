unit UvCardViewerDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, vCard, ExtCtrls, ShellAPI;

type
  TForm4 = class(TForm)
    vCard1: TvCard;
    Button1: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Image1: TImage;
    Label19: TLabel;
    Label18: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
var
  i: integer;
begin
  if opendialog1.Execute then
  begin
    vcard1.vContacts.Clear;
    vcard1.LoadFromFile(opendialog1.FileName);

    if vCard1.vContacts.Count > 0 then
    begin
      Memo1.Lines.Clear;
      Memo1.Lines.Add('Name: ' + vCard1.vContacts[0].FullName);
      Memo1.Lines.Add('Company: ' + vCard1.vContacts[0].Company);
      Memo1.Lines.Add('Web: ' + vCard1.vContacts[0].WebsiteURL);

      if vCard1.vContacts[0].Addresses.Count > 0 then
      begin
        Memo1.Lines.Add('Street: ' + vCard1.vContacts[0].Addresses[0].Street + ' ' + vCard1.vContacts[0].Addresses[0].Number);
        Memo1.Lines.Add('City: ' + vCard1.vContacts[0].Addresses[0].City);
        Memo1.Lines.Add('Country: ' + vCard1.vContacts[0].Addresses[0].Country);
      end;

      for i := 0 to vCard1.vContacts[0].PhoneNumbers.Count - 1 do
      begin
        case vCard1.vContacts[0].PhoneNumbers[i].FieldType of
        ftHome:
          Memo1.Lines.Add('Phone Home: ' + vCard1.vContacts[0].PhoneNumbers[i].PhoneNumber);
        ftWork:
          Memo1.Lines.Add('Phone Work: ' + vCard1.vContacts[0].PhoneNumbers[i].PhoneNumber);
        ftOther:
          Memo1.Lines.Add('Phone Other: ' + vCard1.vContacts[0].PhoneNumbers[i].PhoneNumber);
        end;
      end;

      for i := 0 to vCard1.vContacts[0].Emails.Count - 1 do
      begin
        Memo1.Lines.Add('Email ' + inttostr(i + 1) + ' : ' + vCard1.vContacts[0].Emails[i].EmailAddress);
      end;

      image1.Picture.Assign(vCard1.vContacts[0].Photo);
    end;
  end;

end;

procedure TForm4.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
