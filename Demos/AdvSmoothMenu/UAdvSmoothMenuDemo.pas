{********************************************************************}
{ TMS TAdvSmoothMenu Demo                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothMenuDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvSmoothMenu, ExtCtrls, ComCtrls, StdCtrls, AdvStyleIF, jpeg,
  Vcl.Imaging.pngimage, ShellAPI;

type
  TForm93 = class(TForm)
    AdvSmoothMenu1: TAdvSmoothMenu;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Memo2: TMemo;
    Label2: TLabel;
    Image1: TImage;
    Label3: TLabel;
    Label4: TLabel;
    Image2: TImage;
    Memo1: TMemo;
    Label7: TLabel;
    Label8: TLabel;
    Image3: TImage;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    Edit3: TEdit;
    Edit2: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Edit1: TEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Memo3: TMemo;
    Label13: TLabel;
    Label12: TLabel;
    Button1: TButton;
    Label20: TLabel;
    Label19: TLabel;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AdvSmoothMenu1ItemClick(Sender: TObject; ItemIndex: Integer);
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

procedure TForm93.AdvSmoothMenu1ItemClick(Sender: TObject; ItemIndex: Integer);
begin
  PageControl1.ActivePageIndex := ItemIndex;
end;

procedure TForm93.ComboBox1Change(Sender: TObject);
begin
  AdvSmoothMenu1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
end;

procedure TForm93.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;
  AdvSmoothMenu1.DoubleBuffered := true;
  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex := 1;
  AdvSmoothMenu1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[1]));
end;

procedure TForm93.Label20Click(Sender: TObject);
begin
   ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
