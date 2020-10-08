unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvUtil, Vcl.Grids, AdvObj, BaseGrid,
  AdvGrid, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    AdvStringGrid1: TAdvStringGrid;
    AdvStringGrid2: TAdvStringGrid;
    Label1: TLabel;
    procedure AdvStringGrid1OleDropFile(Sender: TObject; ARow, ACol: Integer;
      FileName: string; var Allow: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure AdvStringGrid1OleDrop(Sender: TObject; ARow, ACol: Integer;
      data: string; var Allow: Boolean);
    procedure AdvStringGrid1OleDrag(Sender: TObject; ARow, ACol: Integer;
      data: string; var Allow: Boolean);
    procedure AdvStringGrid1OleDragOver(Sender: TObject; ARow, ACol: Integer;
      var Allow: Boolean);
    procedure AdvStringGrid1OleDragStart(Sender: TObject; ARow, ACol: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    fn: string;
    dropsource: TAdvStringGrid;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AdvStringGrid1OleDrag(Sender: TObject; ARow, ACol: Integer;
  data: string; var Allow: Boolean);
begin
  fn := '';

  Allow :=  (Sender as TAdvStringgrid).HasFilePicture(ACol,ARow);
  if Allow then
    fn := (Sender as TAdvStringGrid).GetFilePicture(ACol,ARow).Filename;
end;

procedure TForm1.AdvStringGrid1OleDragOver(Sender: TObject; ARow, ACol: Integer;
  var Allow: Boolean);
begin
  if (Sender = dropsource) and (ACol = (Sender as TAdvStringGrid).Col) and (ARow = (Sender as TAdvStringGrid).Row) then
    Allow := false;
end;

procedure TForm1.AdvStringGrid1OleDragStart(Sender: TObject; ARow,
  ACol: Integer);
begin
  dropsource := Sender as TAdvStringGrid;
end;

procedure TForm1.AdvStringGrid1OleDrop(Sender: TObject; ARow, ACol: Integer;
  data: string; var Allow: Boolean);
var
  fp: TFilePicture;
begin
  if fn <> '' then
  begin
   fp := (Sender as TAdvstringgrid).GetFilePicture(ACol,ARow);
   if Assigned(fp) then
     fp.Filename := fn
   else
     (Sender as TAdvstringgrid).CreateFilePicture(ACol,ARow,false,StretchWithAspectRatio,0,haLeft,vaTop).Filename := fn;
   fn := '';
  end;
end;

procedure TForm1.AdvStringGrid1OleDropFile(Sender: TObject; ARow, ACol: Integer;
  FileName: string; var Allow: Boolean);
var
  ext: string;
begin
  ext := Uppercase(ExtractFileExt(FileName));

  if (ext = '.JPEG') or (ext = '.JPG') or (ext = '.PNG') then
    (Sender as TAdvstringgrid).CreateFilePicture(ACol,ARow,false,StretchWithAspectRatio,0,haLeft,vaTop).Filename := FileName;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  advstringgrid1.DragDropSettings.OleDropTarget := true;
  advstringgrid1.DragDropSettings.OleDropSource := true;

  advstringgrid2.DragDropSettings.OleDropTarget := true;
  advstringgrid2.DragDropSettings.OleDropSource := true;

  advstringgrid1.CreateFilePicture(1,1,true,noStretch, 0,haLeft,vaTop).Filename := '.\banana.png';
  advstringgrid1.CreateFilePicture(2,1,true,noStretch, 0,haLeft,vaTop).Filename := '.\kiwi.png';
  advstringgrid1.CreateFilePicture(3,1,true,noStretch, 0,haLeft,vaTop).Filename := '.\lemon.png';
  advstringgrid1.CreateFilePicture(4,1,true,noStretch, 0,haLeft,vaTop).Filename := '.\pear.png';
  advstringgrid1.CreateFilePicture(5,1,true,noStretch, 0,haLeft,vaTop).Filename := '.\strawberry.png';
end;

end.
