{********************************************************************}
{ TMS TAdvSplitter Demo                                              }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSplitterDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, AdvSplitter, StdCtrls, ActnList, AdvOfficeImage, ShellAPI,
  AdvOfficePager, AdvGroupBox, AdvOfficePagerStylers, AdvAppStyler, AdvStyleIF;

type
  TForm2 = class(TForm)
    AdvSplitter2: TAdvSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    AdvSplitter3: TAdvSplitter;
    Time: TAdvOfficePager;
    AdvOfficePage1: TAdvOfficePage;
    AdvOfficePage2: TAdvOfficePage;
    AdvOfficeImage2: TAdvOfficeImage;
    AdvOfficeImage1: TAdvOfficeImage;
    AdvOfficeImage3: TAdvOfficeImage;
    Panel5: TPanel;
    AdvOfficePager1: TAdvOfficePager;
    AdvOfficePage3: TAdvOfficePage;
    AdvOfficePager2: TAdvOfficePager;
    AdvOfficePage4: TAdvOfficePage;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Edit1: TEdit;
    Button1: TButton;
    ComboBox1: TComboBox;
    AdvFormStyler1: TAdvFormStyler;
    AdvOfficePagerOfficeStyler1: TAdvOfficePagerOfficeStyler;
    Label20: TLabel;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label20Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.ComboBox1Change(Sender: TObject);
begin
  AdvFormStyler1.Style := TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex := 2;
  AdvFormStyler1.Style := TTMSStyle(ComboBox1.Items.Objects[2]);
end;

procedure TForm2.Label20Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
