unit UTreeListDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, treelist, StdCtrls, Spin, ImgList, System.ImageList, ShellAPI;

type
  TForm1 = class(TForm)
    TreeList1: TTreeList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    Button4: TButton;
    SpinEdit1: TSpinEdit;
    ImageList1: TImageList;
    Label19: TLabel;
    Label18: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
 tn: TTreeNode;
begin
 // ; separates the columns as this is the Separator property.
 tn := TreeList1.Items.add(nil, 'c:\;test.dat;123');
 tn.ImageIndex := 0; // sets the image index to 0 for this node in column 2
 tn := TreeList1.Items.add(nil, 'c:\;test.dat;456');
 tn.ImageIndex := 1; // sets the image index to 1 for this node in column 2
 TreeList1.Items.AddChild(tn, 'c:\windows\;win.ini;5655');
 TreeList1.Items.Add(nil, 'c:\;config.sys;2223');

 TreeList1.Items.Add(nil, 'myPath');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 // set one treelist column item only
  if Assigned(TreeList1.Selected) then
  begin
    TreeList1.SetNodeColumn(TreeList1.Selected, 1, 'other.bat');
    TreeList1.SetNodeColumn(TreeList1.Selected, 2, '123456');
  end
  else
  begin
    TreeList1.SetNodeColumn(treelist1.Items.Item[0], 1, 'other.bat');
    TreeList1.SetNodeColumn(treelist1.Items.Item[0], 2, '123456');
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
 s:string;
begin
  if Assigned(TreeList1.Selected) then
    s:= TreeList1.GetNodeColumn(TreeList1.Selected, 0) + TreeList1.GetNodeColumn(TreeList1.Selected, 1)
  else
    s := TreeList1.GetNodeColumn(TreeList1.Items.Item[0], 0) + TreeList1.GetNodeColumn(TreeList1.Items.Item[0], 1);

  ListBox1.Items.Add('Selected Path:');
  ListBox1.Items.Add(s);


end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  TreeList1.Columns[0].Width := SpinEdit1.Value;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
