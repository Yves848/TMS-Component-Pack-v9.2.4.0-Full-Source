unit umyCloudDataForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CloudMyCloudDataDataSet,
  Data.DB, Datasnap.DBClient, CloudCustomDataSet, Vcl.ExtCtrls, Vcl.DBCtrls,
  AdvmyCloudDataForm, AdvDBFormLayouter, CloudBase, Vcl.Grids, Vcl.DBGrids;

type
  TForm3 = class(TForm)
    DataSource1: TDataSource;
    AdvMyCloudDataDataSet1: TAdvMyCloudDataDataSet;
    Panel1: TPanel;
    btnConnect: TButton;
    btnOpenClose: TButton;
    DBNavigator2: TDBNavigator;
    AdvmyCloudDataFormPanel1: TAdvmyCloudDataFormPanel;
    AdvMyCloudDataConnection1: TAdvMyCloudDataConnection;
    DBGrid1: TDBGrid;
    ckDescr: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure AdvMyCloudDataConnection1Connected(Sender: TObject);
    procedure btnOpenCloseClick(Sender: TObject);
    procedure ckDescrClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoDataSetActive(Active: boolean);
  end;

var
  Form3: TForm3;

implementation

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  MYCLOUDDATAKEY = 'xxxxxxxxx';
//  MYCLOUDDATASECRET = 'xxxxxxxxx';

{$I TMSAPPIDS.INC}

{$R *.dfm}

procedure TForm3.AdvMyCloudDataConnection1Connected(Sender: TObject);
begin
  Panel1.Enabled := true;
  DoDataSetActive(true);
end;

procedure TForm3.DoDataSetActive(Active: boolean);
var
  i: integer;
begin
  if Active then
  begin
    AdvMyCloudDataDataSet1.Active := true;
    btnOpenClose.Caption := '&Close';

    for i := 0 to DBGrid1.Columns.Count - 1 do
      DBGrid1.Columns[i].Width := 64;
  end
  else
  begin
    AdvMyCloudDataDataSet1.Active := false;
    btnOpenClose.Caption := '&Open';
  end;

end;

procedure TForm3.btnConnectClick(Sender: TObject);
begin
  if AdvMyCloudDataConnection1.Connected then
  begin
    AdvMyCloudDataConnection1.Connected := false;
    DoDataSetActive(false);
    btnConnect.Caption := 'Connect';
    panel1.Enabled := false;
  end
  else
  begin
    AdvMyCloudDataConnection1.Connected := true;
    btnConnect.Caption := 'Disconnect';
  end;
end;

procedure TForm3.btnOpenCloseClick(Sender: TObject);
begin
  DoDataSetActive(not AdvMyCloudDataDataSet1.Active);
end;

procedure TForm3.ckDescrClick(Sender: TObject);
begin
  AdvmyCloudDataDataSet1.DisableControls;

  AdvmyCloudDataFormPanel1.Layout.BeginUpdate;

  if ckDescr.Checked then
  begin
    AdvmyCloudDataFormPanel1.Layout.Descriptions.Kind := dkHint;
    ckDescr.Caption := 'Description as label';
  end
  else
  begin
    AdvmyCloudDataFormPanel1.Layout.Descriptions.Kind := dkLabel;
    ckDescr.Caption := 'Description as hint';
  end;

  AdvmyCloudDataFormPanel1.Layout.EndUpdate;

  AdvmyCloudDataDataSet1.EnableControls;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;

  AdvMyCloudDataConnection1.PersistTokens.Location := plIniFile;
  AdvMyCloudDataConnection1.PersistTokens.Key := '.\myclouddata.ini';
  AdvMyCloudDataConnection1.PersistTokens.Section := 'tokens';

  AdvMyCloudDataConnection1.App.Key := MYCLOUDDATAKEY;
  AdvMyCloudDataConnection1.App.Secret := MYCLOUDDATASECRET;

  AdvMyCloudDataConnection1.App.CallBackPort := 8888;
  AdvMyCloudDataConnection1.App.CallBackURL := 'http://127.0.0.1:8888';

  Panel1.Enabled := false;
end;

end.
