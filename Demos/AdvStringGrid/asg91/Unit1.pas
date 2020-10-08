unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvUtil, AdvToolBar, AdvToolBarStylers,
  AdvGrid, AdvRichEditorEditLink, Vcl.Grids, AdvObj, BaseGrid, AdvToolBarExt,
  AdvRichEditorToolBar, Vcl.StdCtrls, AdvScrollControl, AdvRichEditorBase,
  AdvRichEditor;

type
  TForm1 = class(TForm)
    AdvStringGrid1: TAdvStringGrid;
    AdvRichEditorEditLink1: TAdvRichEditorEditLink;
    AdvToolBarOfficeStyler1: TAdvToolBarOfficeStyler;
    AdvRichEditorFormatToolBar1: TAdvRichEditorFormatToolBar;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure AdvStringGrid1GetEditorProp(Sender: TObject; ACol, ARow: Integer;
      AEditLink: TEditLink);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AdvStringGrid1GetEditorProp(Sender: TObject; ACol,
  ARow: Integer; AEditLink: TEditLink);
begin
  AdvRichEditorFormatToolBar1.RichEditor :=  AdvRichEditorEditLink1.RichEditor;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if checkbox1.checked then
  begin
    AdvRichEditorEditLink1.PopupToolbar := true;
    AdvRichEditorFormatToolBar1.Visible := false;
    CheckBox1.Caption := 'Always visible toolbar';
  end
  else
  begin
    AdvRichEditorEditLink1.PopupToolbar := false;
    AdvRichEditorFormatToolBar1.Visible := true;
    CheckBox1.Caption := 'Popup toolbar (select text and hover over selected text)';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AdvStringGrid1.Options := AdvStringGrid1.Options + [goEditing];
  AdvStringGrid1.DefaultEditor := edCustom;
  AdvStringGrid1.EditLink := AdvRichEditorEditLink1;
end;

end.
