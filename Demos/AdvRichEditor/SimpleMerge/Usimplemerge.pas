unit Usimplemerge;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AdvScrollControl,
  AdvRichEditorBase, AdvRichEditor, Vcl.Grids;

type
  TForm1 = class(TForm)
    AdvRichEditor1: TAdvRichEditor;
    StringGrid1: TStringGrid;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  AdvRichEditor1.AddMultiLineText('Country: namefield'#13#10'City: cityfield');
  AdvRichEditor1.SelStart := 9;
  AdvRichEditor1.SelLength := 9;
  AdvRichEditor1.SetSelectionMergeField('namefield');

  AdvRichEditor1.SelStart := 28;
  AdvRichEditor1.SelLength := 9;
  AdvRichEditor1.SetSelectionMergeField('cityfield');

  AdvRIchEditor1.UnSelect;

  StringGrid1.RowCount := 6;
  StringGrid1.Cells[0,1] := 'France';
  StringGrid1.Cells[1,1] := 'Paris';
  StringGrid1.Cells[0,2] := 'Germany';
  StringGrid1.Cells[1,2] := 'Berlin';
  StringGrid1.Cells[0,3] := 'United Kingdom';
  StringGrid1.Cells[1,3] := 'London';
  StringGrid1.Cells[0,4] := 'Spain';
  StringGrid1.Cells[1,4] := 'Madrid';
  StringGrid1.Cells[0,5] := 'Italy';
  StringGrid1.Cells[1,5] := 'Rome';
end;

procedure TForm1.StringGrid1Click(Sender: TObject);
var
  sl: TStringList;
begin
  AdvRichEditor1.UnMerge;
  sl := TStringList.Create;

  sl.Add('namefield='+StringGrid1.Cells[0, StringGrid1.Row]);
  sl.Add('cityfield='+StringGrid1.Cells[1, StringGrid1.Row]);

  AdvRichEditor1.Merge(Sl);

  sl.Free;
end;

end.
