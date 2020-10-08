{********************************************************************}
{ TMS VCL Styles Demo                                                }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1996 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UVCLStyleDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, VCL.Themes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvUtil, AdvGraphics, Vcl.ExtCtrls, AdvScrollControl, AdvResponsiveList, Planner,
  Vcl.Grids, AdvObj, BaseGrid, AdvGrid, DBAdvGrid,  Vcl.Menus, Vcl.Imaging.pngimage, InspLinks, Data.DB,
  Datasnap.DBClient, Vcl.StdCtrls, AdvCustomControl, AdvCustomScrollControl, ShellAPI,
  AdvKanbanBoard, Vcl.ComCtrls, AdvProgr, AdvCustomComponent, AdvKanbanBoardDatabaseAdapter, Vcl.ImgList, PictureContainer,
  AdvGraphicsTypes, AdvNavBar, System.ImageList, AdvEdit,
  AdvEdBtn, AdvSmoothGauge, Vcl.Buttons, AdvMenus;

type
  TForm4 = class(TForm)
    DBAdvGrid1: TDBAdvGrid;
    Planner1: TPlanner;
    AdvResponsiveList1: TAdvResponsiveList;
    Panel1: TPanel;
    Label1: TLabel;
    AdvKanbanBoard1: TAdvKanbanBoard;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    AdvKanbanBoardDatabaseAdapter1: TAdvKanbanBoardDatabaseAdapter;
    PictureContainer1: TPictureContainer;
    AdvNavBar1: TAdvNavBar;
    AdvNavBarPanel1: TAdvNavBarPanel;
    AdvNavBarPanel2: TAdvNavBarPanel;
    AdvNavBarPanel3: TAdvNavBarPanel;
    AdvNavBarPanel4: TAdvNavBarPanel;
    ImageList1: TImageList;
    AdvNavBarPanel5: TAdvNavBarPanel;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Panel2: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    AdvSmoothGauge1: TAdvSmoothGauge;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    AdvEdit1: TAdvEdit;
    Image6: TImage;
    AdvPopupMenu1: TAdvPopupMenu;
    Windows1: TMenuItem;
    Luna1: TMenuItem;
    Windows10Dark1: TMenuItem;
    EmeraldLightSlate1: TMenuItem;
    SmokeyQuartzKamri1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure PopulateDataset;
    procedure AdvKanbanBoardDatabaseAdapter1FieldsToItem(Sender: TObject;
      AFields: TFields; AItem: TAdvKanbanBoardItem);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Windows1Click(Sender: TObject);
    procedure Luna1Click(Sender: TObject);
    procedure Windows10Dark1Click(Sender: TObject);
    procedure EmeraldLightSlate1Click(Sender: TObject);
    procedure SmokeyQuartzKamri1Click(Sender: TObject);
    procedure Image6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    testColor:TColor;
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.AdvKanbanBoardDatabaseAdapter1FieldsToItem(Sender: TObject;
  AFields: TFields; AItem: TAdvKanbanBoardItem);
begin
  if AFields.FieldByName('Priority').AsInteger = 0 then
  begin
      AItem.Expandable := False;
  end
  else
  begin
    AItem.Expandable := True;
  end;

  if AItem.Title.StartsWith('Meeting') or AItem.Title.StartsWith('Icons') then
    AItem.Expanded := false;

  AItem.UseDefaultAppearance := False;

  case AFields.FieldByName('Status').AsInteger of
    0: AItem.MarkColor := gcPlum;
    1: AItem.MarkColor := $00D07903;
    2: AItem.MarkColor := gcMediumaquamarine;
  end;
  AItem.MarkType := [kbmtTop];
end;

procedure TForm4.FormCreate(Sender: TObject);
var
  sl, col: TStringList;
  i: integer;
begin
  //AdvResponsiveList
  AdvResponsiveList1.Items.Clear;

  sl := TStringList.Create;
  col := TStringList.Create;

  col.StrictDelimiter := true;
  col.Delimiter := ',';

  AdvResponsiveList1.BeginUpdate;

  try
    sl.LoadFromFile('.\highlights.txt');
    AdvResponsiveList1.Appearance.ItemBorderStyle := bsNone;

    for i := 0 to sl.Count-1 do
    begin
      col.CommaText := sl.Strings[i];
      AdvResponsiveList1.Items.Add.Content := '<IMG SRC="file://.\'+col[0]+'"/> '+col[1];
    end;

  finally
    AdvResponsiveList1.EndUpdate;
    sl.Free;
    col.Free;
  end;

  //AdvKanbanBoard
  PopulateDataset;

  AdvKanbanBoard1.BeginUpdate;
  AdvKanbanBoardDatabaseAdapter1.KanbanBoard := AdvKanbanBoard1;

  AdvKanbanBoardDatabaseAdapter1.Active := True;
  ClientDataSet1.Active := True;
  AdvKanbanBoard1.EndUpdate;

  //AdvGrid
  DBAdvGrid1.LoadFromCSV('.\sales.csv');

end;

procedure TForm4.FormShow(Sender: TObject);
begin
  //AdvKanbanBoard
  AdvKanbanBoard1.Margins.Left := 3;
  AdvKanbanBoard1.Margins.Top := 1;
  AdvKanbanBoard1.Margins.Bottom := 1;
  AdvKanbanBoard1.Margins.Right := 0;
  AdvKanbanBoard1.BeginUpdate;
  AdvKanbanBoard1.ColumnsAppearance.Margins.Left := 3;
  AdvKanbanBoard1.ColumnsAppearance.Margins.Right := 1;
  AdvKanbanBoard1.ColumnsAppearance.Margins.Top := 1;
  AdvKanbanBoard1.ColumnsAppearance.Margins.Bottom := 1;
  AdvKanbanBoard1.Columns[0].HeaderFill.Kind := gfkSolid;
  AdvKanbanBoard1.ColumnsAppearance.Margins.Left := 0;
  AdvKanbanBoard1.Columns[0].HeaderFill.Color := gcPlum;
  AdvKanbanBoard1.Columns[1].HeaderFill.Color := $00D07903;
  AdvKanbanBoard1.Columns[1].HeaderFill.Kind := gfkSolid;
  AdvKanbanBoard1.Columns[2].HeaderFill.Color := gcMediumaquamarine;
  AdvKanbanBoard1.Columns[2].HeaderFill.Kind := gfkSolid;
  AdvKanbanBoard1.EndUpdate;

  //Planner
  Planner1.Mode.Date := Date;
  Planner1.Items.Items[0].Text.Add('<br><img src="USER1">');
end;

procedure TForm4.Image6Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm4.Panel1Click(Sender: TObject);
begin
  Image6.Visible := not Image6.Visible;
end;

procedure TForm4.PopulateDataset;
var
  idx: integer;

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

    AddRecord(idx, 'Documentation', 'Write developers guide and <font color="clGray">FAQ</font><br><br><img src="USER1">', 1, 0, gcPlum);
    AddRecord(idx, 'Meeting: Next phase', 'Plan a meeting with the customer to roll out the next phase of the program', 1, 0, clWhite);
    AddRecord(idx, 'QA', 'Test performance of new feature.<br><br><img src="user2"><img src="user6">', 0, 0, $0000CC99);

    AddRecord(idx, 'Fix urgent issues', 'Fix <u>high priority</u> issues documented by customer.<br><br><img src="user4"><img src="user3">', 1, 1, clWhite);
    AddRecord(idx, 'Customer update', 'Get in contact with the customer to let him know how far we are on developing the newly requested items.</b><br><br><img src="user5">', 1, 1, clWhite);
    AddRecord(idx, 'Icons', 'Create new flat icons', 1, 1, $000077FF);
    AddRecord(idx, 'Install new server', 'Name: <b>Serv2</b><br>On IP: <b>192.168.124.12</b><br><br><img src="user2">', 0, 2, clWhite);
    AddRecord(idx, 'Reload Licenses', '<br><br><img src="user7">', 0, 2, $0000CCFF);

    ClientDataSet1.First;
  finally
    ClientDataSet1.EnableControls;
  end;
end;

procedure TForm4.EmeraldLightSlate1Click(Sender: TObject);
begin
  TSTyleManager.SetStyle('Emerald Light Slate');
  EmeraldLightSlate1.Checked := True;
end;

procedure TForm4.Luna1Click(Sender: TObject);
begin
  TSTyleManager.SetStyle('Luna');
  Luna1.Checked := True;
end;

procedure TForm4.SmokeyQuartzKamri1Click(Sender: TObject);
begin
  TSTyleManager.SetStyle('Smokey Quartz Kamri');
  SmokeyQuartzKamri1.Checked := True;
end;

procedure TForm4.Windows10Dark1Click(Sender: TObject);
begin
  TSTyleManager.SetStyle('Windows10 Dark');
  Windows10Dark1.Checked := True;
end;

procedure TForm4.Windows1Click(Sender: TObject);
begin
  TSTyleManager.SetStyle('Windows');
  Windows1.Checked := True;
end;

end.
