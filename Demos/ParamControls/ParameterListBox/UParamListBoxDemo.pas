unit UParamListBoxDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, clisted, ParamListbox, Types;

type
  TForm1 = class(TForm)
    ParamListBox1: TParamListBox;
    CheckListEdit1: TCheckListEdit;
    Label1: TLabel;
    Label19: TLabel;
    Label18: TLabel;
    procedure ParamListBox1ParamCustomEdit(Sender: TObject; idx: Integer; href,
      value, props: string; EditRect: TRect);
    procedure CheckListEdit1Close(Sender: TObject);
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    edithref: string;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
uses
  Math, ShellAPI;


procedure TForm1.CheckListEdit1Close(Sender: TObject);
begin
  ParamListBox1.Parameter[edithref] := CheckListEdit1.Text;
  CheckListEdit1.Visible := false;
  ParamListBox1.SetFocus;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.ParamListBox1ParamCustomEdit(Sender: TObject; idx: Integer;
  href, value, props: string; EditRect: TRect);
var
  pt: TPoint;
begin
  pt := Point(EditRect.Left, EditRect.Top);
  pt := ScreenToClient(pt);
  CheckListEdit1.SetBounds(pt.X, pt.Y, Max(100, EditRect.Width), EditRect.Height);
  CheckListEdit1.Visible := true;
  CheckListEdit1.BringToFront;
  CheckListEdit1.Text := Value;
  CheckListEdit1.ShowCheckList;
  edithref := href;
end;

end.
