{********************************************************************}
{ TMS TAdvSmoothPanel Demo                                           }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothPanelDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  GDIPPictureContainer, StdCtrls, AdvSmoothPanel, jpeg, ExtCtrls, ShellAPI, AdvStyleIF;

type
  TForm93 = class(TForm)
    AdvSmoothPanel1: TAdvSmoothPanel;
    AdvSmoothPanel2: TAdvSmoothPanel;
    AdvSmoothPanel3: TAdvSmoothPanel;
    AdvSmoothPanel4: TAdvSmoothPanel;
    Image1: TImage;
    GDIPPictureContainer1: TGDIPPictureContainer;
    ComboBox1: TComboBox;
    Label9: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure AdvSmoothPanel1AnchorClick(Sender: TObject; Anchor: string);
    procedure Label20Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var                         
  Form93: TForm93;

implementation

{$R *.dfm}

procedure TForm93.AdvSmoothPanel1AnchorClick(Sender: TObject; Anchor: string);
begin
  ShellExecute(WindowHandle, 'Open', PChar(Anchor), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm93.ComboBox1Change(Sender: TObject);
begin

  AdvSmoothPanel2.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));

  if ComboBox1.Items.Strings[ComboBox1.ItemIndex].Contains('Black') then
  begin
    AdvSmoothPanel2.Caption.HTMLFont.Color:=clWhite;
  end
  else
  begin
    AdvSmoothPanel2.Caption.HTMLFont.Color:=clBlack;
  end;

end;

procedure TForm93.FormCreate(Sender: TObject);
begin
  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex := 1;
  AdvSmoothPanel2.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[1]));
end;

procedure TForm93.Label20Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
