{********************************************************************}
{ TMS AdvDualListBox Demo                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvDualListBoxDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, AdvGroupBox,
  AdvDualListBox, Menus, ShellAPI;

type
  TForm92 = class(TForm)
    AdvDualListBox1: TAdvDualListBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ListBox1: TListBox;
    PopupMenu1: TPopupMenu;
    Clear1: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure AdvDualListBox1MoveLeftRight(Sender: TObject; Index: Integer;
      var Allow: Boolean);
    procedure Clear1Click(Sender: TObject);
    procedure AdvDualListBox1MoveLeftRightAll(Sender: TObject;
      var Allow: Boolean);
    procedure AdvDualListBox1MoveRightLeft(Sender: TObject; Index: Integer;
      var Allow: Boolean);
    procedure AdvDualListBox1MoveRightLeftAll(Sender: TObject;
      var Allow: Boolean);
    procedure Label2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form92: TForm92;

implementation

{$R *.dfm}

procedure TForm92.AdvDualListBox1MoveLeftRight(Sender: TObject; Index: Integer;
  var Allow: Boolean);
begin
  ListBox1.Items.Add('MoveLeftRight: ' + AdvDualListBox1.ListLeft.Items[Index]);
end;

procedure TForm92.AdvDualListBox1MoveLeftRightAll(Sender: TObject;
  var Allow: Boolean);
begin
  ListBox1.Items.Add('MoveLeftRightAll triggered');
end;

procedure TForm92.AdvDualListBox1MoveRightLeft(Sender: TObject; Index: Integer;
  var Allow: Boolean);
begin
  ListBox1.Items.Add('MoveRightLeft: ' + AdvDualListBox1.ListRight.Items[Index]);
end;

procedure TForm92.AdvDualListBox1MoveRightLeftAll(Sender: TObject;
  var Allow: Boolean);
begin
  ListBox1.Items.Add('MoverRightLeftAll triggered');
end;

procedure TForm92.CheckBox1Click(Sender: TObject);
begin
  AdvDualListBox1.ListLeft.MultiSelect := CheckBox1.Checked;
  AdvDualListBox1.ListRight.MultiSelect := CheckBox1.Checked;
end;

procedure TForm92.CheckBox2Click(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
    AdvDualListBox1.MoveOptions := AdvDualListBox1.MoveOptions + [moMoveWithKey]
  else
    AdvDualListBox1.MoveOptions := AdvDualListBox1.MoveOptions - [moMoveWithKey];
end;

procedure TForm92.CheckBox3Click(Sender: TObject);
begin
  AdvDualListBox1.ListLeft.Sorted := CheckBox3.Checked;
  AdvDualListBox1.ListRight.Sorted := CheckBox3.Checked;
end;

procedure TForm92.CheckBox4Click(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
    AdvDualListBox1.Buttons.VisibleButtons := AdvDualListBox1.Buttons.VisibleButtons + [vbRightAll, vbLeftAll]
  else
    AdvDualListBox1.Buttons.VisibleButtons := AdvDualListBox1.Buttons.VisibleButtons - [vbRightAll, vbLeftAll];
end;

procedure TForm92.Clear1Click(Sender: TObject);
begin
  ListBox1.Items.Clear;
end;

procedure TForm92.FormCreate(Sender: TObject);
begin
  AdvDualListBox1.ListLeft.Items.BeginUpdate;
  AdvDualListBox1.ListLeft.Items.Add('Rome');
  AdvDualListBox1.ListLeft.Items.Add('Paris');
  AdvDualListBox1.ListLeft.Items.Add('Berlin');
  AdvDualListBox1.ListLeft.Items.Add('Vienna');
  AdvDualListBox1.ListLeft.Items.Add('Brussels');
  AdvDualListBox1.ListLeft.Items.Add('Amsterdam');
  AdvDualListBox1.ListLeft.Items.Add('Köln');
  AdvDualListBox1.ListLeft.Items.Add('Frankfurt');
  AdvDualListBox1.ListLeft.Items.Add('Munchen');
  AdvDualListBox1.ListLeft.Items.Add('Lyon');
  AdvDualListBox1.ListLeft.Items.Add('Marseille');
  AdvDualListBox1.ListLeft.Items.Add('Barcelona');
  AdvDualListBox1.ListLeft.Items.Add('Madrid');
  AdvDualListBox1.ListLeft.Items.Add('Rome');
  AdvDualListBox1.ListLeft.Items.Add('Oslo');
  AdvDualListBox1.ListLeft.Items.Add('London');
  AdvDualListBox1.ListLeft.Items.Add('Birmingham');
  AdvDualListBox1.ListLeft.Items.Add('Bern');
  AdvDualListBox1.ListLeft.Items.Add('Zürich');
  AdvDualListBox1.ListLeft.Items.Add('Firenze');
  AdvDualListBox1.ListLeft.Items.Add('Napels');
  AdvDualListBox1.ListLeft.Items.Add('Strasbourg');
  AdvDualListBox1.ListLeft.Items.Add('Bremen');
  AdvDualListBox1.ListLeft.Items.Add('Hamburg');
  AdvDualListBox1.ListLeft.Items.Add('Hanover');
  AdvDualListBox1.ListLeft.Items.Add('Leipzig');
  AdvDualListBox1.ListLeft.Items.Add('Prague');
  AdvDualListBox1.ListLeft.Items.Add('Dresden');
  AdvDualListBox1.ListLeft.Items.Add('Antwerpen');
  AdvDualListBox1.ListLeft.Items.Add('Utrecht');
  AdvDualListBox1.ListLeft.Items.Add('Eindhoven');
  AdvDualListBox1.ListLeft.Items.EndUpdate;
  AdvDualListBox1.ListLeft.Caption := 'Bucketlist';
  AdvDualListBox1.ListRight.Caption := 'Visited';
end;

procedure TForm92.Label2Click(Sender: TObject);
begin
  ShellExecute(handle,'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
