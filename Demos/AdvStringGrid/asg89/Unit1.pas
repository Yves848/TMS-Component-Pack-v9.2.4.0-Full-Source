unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvUtil, AdvGrid, AdvSearchEditEditLink,
  Vcl.Grids, AdvObj, BaseGrid, AdvSearchList, AdvSearchEdit, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    AdvStringGrid1: TAdvStringGrid;
    AdvSearchEditEditLink1: TAdvSearchEditEditLink;
    AdvSearchEditEditLink2: TAdvSearchEditEditLink;
    Label1: TLabel;
    procedure AdvStringGrid1GetEditorType(Sender: TObject; ACol, ARow: Integer;
      var AEditor: TEditorType);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AdvStringGrid1GetEditorType(Sender: TObject; ACol,
  ARow: Integer; var AEditor: TEditorType);
begin
  AEditor := edCustom;
  case ACol of
  1: AdvStringGrid1.EditLink := AdvSearchEditEditLink1;
  2: AdvStringGrid1.EditLink := AdvSearchEditEditLink2;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  sl: TStringList;
  i: integer;
begin
  AdvStringGrid1.Cells[1,0] := 'Country';
  AdvStringGrid1.Cells[2,0] := 'Continent';

  AdvSearchEditEditLink1.Columns.Clear;
  AdvSearchEditEditLink1.Columns.Add;

  AdvSearchEditEditLink1.CategoryButton.Visible := false;
  AdvSearchEditEditLink1.SearchButton.Visible := false;
  AdvSearchEditEditLink1.DropDownHeader.Caption := 'Countries';
  AdvSearchEditEditLink1.DropDownSizable := true;
  AdvSearchEditEditLink1.Appearance.HighlightTextColor := clBlue;
  AdvSearchEditEditLink1.Appearance.Banding := true;

  sl := TStringList.Create;

  try
    sl.LoadFromFile('.\..\..\countries.txt');

    // strip country
    for i := 0 to sl.Count - 1 do
      sl.Strings[i] := Copy(sl.Strings[i],4,Length(sl.Strings[i]));

    AdvSearchEditEditLink1.Items.LoadStrings(sl);
  finally
    sl.Free;
  end;

  AdvSearchEditEditLink2.Columns.Clear;
  AdvSearchEditEditLink2.Columns.Add;

  AdvSearchEditEditLink2.CategoryButton.Visible := false;
  AdvSearchEditEditLink2.SearchButton.Visible := false;
  AdvSearchEditEditLink2.DropDownHeader.Caption := 'Countries';
  AdvSearchEditEditLink2.DropDownSizable := true;
  AdvSearchEditEditLink2.Appearance.HighlightTextColor := clGreen;

  AdvSearchEditEditLink2.Items.Add.Captions[0] := 'Europe';
  AdvSearchEditEditLink2.Items.Add.Captions[0] := 'North-America';
  AdvSearchEditEditLink2.Items.Add.Captions[0] := 'South-America';
  AdvSearchEditEditLink2.Items.Add.Captions[0] := 'Africa';
  AdvSearchEditEditLink2.Items.Add.Captions[0] := 'Australia';
  AdvSearchEditEditLink2.Items.Add.Captions[0] := 'Asia';
  AdvSearchEditEditLink2.Items.Add.Captions[0] := 'Antartica';

  AdvStringGrid1.Cells[1,1] := 'Germany';
  AdvStringGrid1.Cells[2,1] := 'Europe';

  AdvStringGrid1.Cells[1,2] := 'Japan';
  AdvStringGrid1.Cells[2,2] := 'Asia';

  AdvStringGrid1.Cells[1,3] := 'Brazil';
  AdvStringGrid1.Cells[2,3] := 'South-America';

  AdvStringGrid1.Cells[1,4] := 'Kenia';
  AdvStringGrid1.Cells[2,4] := 'Africa';

  AdvStringGrid1.Cells[1,5] := 'Alaska';
  AdvStringGrid1.Cells[2,5] := 'Antartica';
end;

end.
