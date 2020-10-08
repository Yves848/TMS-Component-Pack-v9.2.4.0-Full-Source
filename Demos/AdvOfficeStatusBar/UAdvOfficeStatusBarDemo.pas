{********************************************************************}
{ TMS TAdvOfficeStatusBar Demo                                       }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvOfficeStatusBarDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvOfficeStatusBar, AdvOfficeStatusBarStylers, ImgList, AdvOfficeHint,
  AdvToolBar, AdvToolBarStylers, System.ImageList, ShellAPI, AdvStyleIF,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    AdvOfficeStatusBar1: TAdvOfficeStatusBar;
    ImageList1: TImageList;
    AdvOfficeHint1: TAdvOfficeHint;
    Label1: TLabel;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  AdvOfficeStatusBar1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
  AdvOfficeHint1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex := 1;
  AdvOfficeHint1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[1]));
end;

end.
