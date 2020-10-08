{********************************************************************}
{ TMS TListLink Demo                                                 }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1998 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UListLinkDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, listlink, ComCtrls, ShellAPI;

type
  TForm2 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    movelr: TButton;
    moverl: TButton;
    movesellr: TButton;
    moveselrl: TButton;
    copylr: TButton;
    copyrl: TButton;
    copysellr: TButton;
    copyselrl: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    tlistlink1: TListLink;
    ListView1: TListView;
    ListView2: TListView;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Label3: TLabel;
    Label4: TLabel;
    ListLink1: TListLink;
    Label19: TLabel;
    Label18: TLabel;
    procedure movelrClick(Sender: TObject);
    procedure moverlClick(Sender: TObject);
    procedure movesellrClick(Sender: TObject);
    procedure moveselrlClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Label19Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.movelrClick(Sender: TObject);
begin
 MessageDlg('Hello move LR',mtinformation,[mbok],0);
end;

procedure TForm2.moverlClick(Sender: TObject);
begin
 MessageDlg('Hello move RL',mtinformation,[mbok],0);
end;

procedure TForm2.movesellrClick(Sender: TObject);
begin
 MessageDlg('Hello move sel LR',mtinformation,[mbok],0);
end;

procedure TForm2.moveselrlClick(Sender: TObject);
begin
 MessageDlg('Hello move sel RL',mtinformation,[mbok],0);
end;

procedure TForm2.CheckBox1Click(Sender: TObject);
begin
  ListBox1.MultiSelect := CheckBox1.Checked;
end;

procedure TForm2.CheckBox2Click(Sender: TObject);
begin
  ListBox2.MultiSelect := CheckBox2.Checked;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  with listview1.Items.Add do
  begin
    Caption := 'BMW';
    SubItems.Add('Z4');
  end;
  with listview1.Items.Add do
  begin
    Caption := 'Mercedes';
    SubItems.Add('A class');
  end;
  with listview1.Items.Add do
  begin
    Caption := 'Porsche';
    SubItems.Add('911');
  end;
  with listview1.Items.Add do
  begin
    Caption := 'Ferrari';
    SubItems.Add('458');
  end;
  with listview1.Items.Add do
  begin
    Caption := 'Audi';
    SubItems.Add('R8');
  end;
end;

procedure TForm2.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
