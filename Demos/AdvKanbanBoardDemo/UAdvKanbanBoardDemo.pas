{********************************************************************}
{ TMS TAdvKanbanBoard Demo                                           }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1996 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UAdvKanbanBoardDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AdvCustomControl,
  AdvCustomScrollControl, AdvKanbanBoard, Data.DB, Datasnap.DBClient,
  AdvCustomComponent, AdvKanbanBoardDatabaseAdapter, PictureContainer,
  Vcl.ColorGrd, HTMLabel;

type
  TForm4 = class(TForm)
    AdvKanbanBoard1: TAdvKanbanBoard;
    Label6: TLabel;
    Label7: TLabel;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    PictureContainer1: TPictureContainer;
    AdvKanbanBoardDatabaseAdapter1: TAdvKanbanBoardDatabaseAdapter;
    HTMLabel1: THTMLabel;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    ColorGrid1: TColorGrid;
    procedure PopulateDataset;
    procedure FormCreate(Sender: TObject);
    procedure AdvKanbanBoard1SelectItem(Sender: TObject;
      AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem);
    procedure AdvKanbanBoardDatabaseAdapter1FieldsToItem(Sender: TObject;
      AFields: TFields; AItem: TAdvKanbanBoardItem);
    procedure Button1Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    idx: Integer;

  end;

var
  Form4: TForm4;

implementation
uses
  ShellAPI;

{$R *.dfm}

procedure TForm4.PopulateDataset;
  procedure AddRecord(var AIndex: Integer; ATitle, ADescription: String; APriority, AStatus, AColor: TColor);
  begin
    ClientDataSet1.Append;
    ClientDataSet1.FieldByName('ID').AsInteger := idx;
    ClientDataSet1.FieldByName('Title').AsString := ATitle;
    ClientDataSet1.FieldByName('Description').AsString := ADescription;
    ClientDataSet1.FieldByName('Priority').AsInteger := APriority;
    ClientDataSet1.FieldByName('Status').AsInteger := AStatus;
    ClientDataSet1.FieldByName('Color').AsInteger := AColor;
    ClientDataSet1.Post;
    Inc(AIndex);
  end;

begin
  ClientDataSet1.CreateDataSet;
  ClientDataSet1.Open;
  ClientDataSet1.DisableControls;
  try
    idx := 0;

    AddRecord(idx, 'TMS AdvKanbanBoard Release', '<font color="clGray">Finish documentation</font><br/><br/><img src="button_duplicate"/>', 0, 0, clWhite);
    AddRecord(idx, 'TMS AdvKanbanBoard Release', '<img src="button_duplicate"/>', 0, 2, clWhite);
    AddRecord(idx, 'ADD NEW', '<font color="clWhite">New VCL UI Pack Component</font>', 0, 1, $0000CC99);
    AddRecord(idx, 'FIX', 'Fix issue in FNC Core', 1, 0, clWhite);
    AddRecord(idx, 'FNC', 'Interface Support<br/><img src="button_high-priority"/>', 3, 1, $0000CCFF);
    AddRecord(idx, 'FNC', 'Convert Calendar and Date-Picker to FMX, VCL and WEB<br/><img src="button_high-priority"/>', 0, 1, clWhite);
    AddRecord(idx, 'JQWidgets', 'Create a JQWidgets grid demo with CSS styling', 0, 0, $000077FF);
    AddRecord(idx, 'Documentation', 'Finish documentation for TMS FAQ on website<br/><img src="button_low-priority"/>', 4, 1, $00FFCC00);
    AddRecord(idx, 'Landing page', 'Add new blog post', 4, 0, clWhite);
    AddRecord(idx, 'Landing page', 'Add new image to the landing page, image is attached below.<br/><br/><img src="tms" width="200"/>', 0, 2, clWhite);

    ClientDataSet1.First;
  finally
    ClientDataSet1.EnableControls;
  end;
end;


procedure TForm4.AdvKanbanBoard1SelectItem(Sender: TObject;
  AColumn: TAdvKanbanBoardColumn; AItem: TAdvKanbanBoardItem);
begin
  OutputDebugString('test');
end;

procedure TForm4.AdvKanbanBoardDatabaseAdapter1FieldsToItem(Sender: TObject;
  AFields: TFields; AItem: TAdvKanbanBoardItem);
begin
  AItem.Expandable := True;
  AItem.UseDefaultAppearance := False;
  AItem.Color := AFields.FieldByName('Color').AsInteger;
  AItem.MarkColor := $0000A5FF;

  if AItem.Color = clRed then
  begin
    AItem.TitleColor := clWhite;
    AItem.TextColor := clWhite;
  end;

  case AFields.FieldByName('Priority').AsInteger of
    1: AItem.MarkType := [kbmtLeft];
    2: AItem.MarkType := [kbmtTop];
    3: AItem.MarkType := [kbmtRight];
    4: AItem.MarkType := [kbmtBottom];
    5: AItem.MarkType := [kbmtLeft, kbmtTop, kbmtRight, kbmtBottom];
  end;

  if Odd(Random(10)) then
    AItem.MarkColor := clBlue
  else
    AItem.MarkColor := clRed;
end;


procedure TForm4.Button1Click(Sender: TObject);
var
  Item: TAdvKanbanBoardItem;
begin
  AdvKanbanBoard1.BeginUpdate;

  Item := TAdvKanbanBoardItem.Create(AdvKanbanBoard1.Columns[0].Items);
  Item.Expandable := True;
  Item.UseDefaultAppearance := False;
  Item.Color := ColorGrid1.BackgroundColor;
  Item.Title := Edit1.Text;
  Item.Text := Memo1.Text;

  AdvKanbanBoard1.EndUpdate;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin

  PopulateDataset;

  AdvKanbanBoard1.BeginUpdate;
  AdvKanbanBoardDatabaseAdapter1.KanbanBoard := AdvKanbanBoard1;

  AdvKanbanBoardDatabaseAdapter1.Active := True;
  ClientDataSet1.Active := True;
  AdvKanbanBoard1.EndUpdate;

end;

procedure TForm4.Label6Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
