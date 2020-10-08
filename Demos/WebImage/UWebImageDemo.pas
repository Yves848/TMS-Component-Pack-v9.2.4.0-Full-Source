{*************************************************************************}
{ TMS TWebImage Demo                                                      }
{                                                                         }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2001 - 2019                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{*************************************************************************}

unit UWebImageDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, WebImage, ExtCtrls, ComCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Regions: TListBox;
    pb: TProgressBar;
    Panel1: TPanel;
    wi: TWebImage;
    Label1: TLabel;
    Label2: TLabel;
    Label19: TLabel;
    Label18: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure wiDownLoadProgress(Sender: TObject; dwSize,
      dwTotSize: Cardinal);
    procedure FormCreate(Sender: TObject);
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  url:string;
begin
  case Regions.ItemIndex of
    0: url := 'https://www.freeusandworldmaps.com/images/World_Regions_Maps/EuropeColorText.jpg';
    1: url := 'https://www.freeusandworldmaps.com/images/World_Regions_Maps/AfricaColorText.jpg';
    2: url := 'https://www.freeusandworldmaps.com/images/World_Regions_Maps/CIS_AsiaColorText.jpg';
    3: url := 'https://www.freeusandworldmaps.com/images/World_Regions_Maps/NorthAmericaColorText.jpg';
    4: url := 'https://www.freeusandworldmaps.com/images/World_Regions_Maps/AustraliaOceaniaCText.jpg';
    5: url := 'https://www.freeusandworldmaps.com/images/World_Regions_Maps/LatinAmericaColorText.jpg';
  end;
 if not wi.webpicture.Busy then wi.webpicture.loadfromURL(url);
end;

procedure TForm1.wiDownLoadProgress(Sender: TObject; dwSize,
  dwTotSize: Cardinal);
begin
  pb.Position := dwSize;
  pb.Max := dwTotSize;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    Regions.ItemIndex := 0;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
