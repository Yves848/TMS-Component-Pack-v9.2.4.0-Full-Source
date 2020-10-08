{********************************************************************}
{ TMS TAdvGlowButton Demo                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvGlowButtonDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvGlowButton, Menus, AdvMenus, jpeg, ExtCtrls, AdvToolBar,
  AdvToolBarStylers, StdCtrls, AdvStyleIF, ShellAPI;

type
  TForm2 = class(TForm)
    AdvGlowButton3: TAdvGlowButton;
    AdvGlowButton4: TAdvGlowButton;
    AdvGlowButton5: TAdvGlowButton;
    AdvGlowButton9: TAdvGlowButton;
    AdvGlowButton8: TAdvGlowButton;
    ComboBox1: TComboBox;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    AdvGlowButton10: TAdvGlowButton;
    AdvGlowButton11: TAdvGlowButton;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Label5: TLabel;
    PopupMenu1: TPopupMenu;
    File1: TMenuItem;
    View1: TMenuItem;
    Exit1: TMenuItem;
    Label6: TLabel;
    ComboBox3: TComboBox;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    AdvGlowButton1: TAdvGlowButton;
    Label1: TLabel;
    Label7: TLabel;
    AdvGlowButton2: TAdvGlowButton;
    procedure AdvGlowButton10Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.AdvGlowButton10Click(Sender: TObject);
begin
  AdvGlowButton10.ShortCutHint := 'Hint';
  AdvGlowButton10.ShortCutHintPos := shpBottom;
  AdvGlowButton10.ShowShortCutHint;
end;


procedure TForm2.ComboBox1Change(Sender: TObject);
begin
  AdvGlowButton11.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
  AdvGlowButton10.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
end;

procedure TForm2.ComboBox2Change(Sender: TObject);
begin
case ComboBox2.ItemIndex of
0:
  begin
  AdvGlowButton1.DropDownDirection := ddRight;
  end;
1:
  begin
  AdvGlowButton1.DropDownDirection := ddDown;
  end;
end;
end;

procedure TForm2.ComboBox3Change(Sender: TObject);
begin
case ComboBox3.ItemIndex of
0:
  begin
  AdvGlowButton1.DropDownPosition := dpRight;
  end;
1:
  begin
  AdvGlowButton1.DropDownPosition := dpBottom ;
  end;
end;

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex:=2;
  AdvGlowButton11.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[2]));
  AdvGlowButton10.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[2]));
end;

procedure TForm2.Label7Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
