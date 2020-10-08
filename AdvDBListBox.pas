{**************************************************************************}
{ TAdvDBListBox component                                                  }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2017                                                         }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvDBListBox;

interface

uses
  Windows, Classes, SysUtils, AdvListBox, DB;

type
  TAdvDBListBox = class;

  TAdvListBoxDataLink = class(TDataLink)
  private
    FListBox: TAdvDBListBox;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(distance:integer); override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AListBox: TAdvDBListBox);
    destructor Destroy; override;
  end;

  TAdvDBListBox = class(TAdvCustomListBox)
  private
    FDataLink: TAdvListBoxDataLink;
    FDataField: string;
    FDBUpdating: boolean;
    FLoadOnCreate: boolean;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function CheckDataSet: boolean;
  protected
    procedure DoInsertItem(Value: string); override;
    procedure ListClick(Sender: TObject); override;
    procedure GotoActiveRecord; virtual;
    procedure LoadFromDataSource; virtual;
    procedure Loaded; override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read FDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

implementation

{ TAdvDBListBox }

function TAdvDBListBox.CheckDataSet: boolean;
begin
  Result := False;
  if not Assigned(FDataLink) then Exit;
  if not Assigned(FDataLink.DataSource) then Exit;
  if FDataField = '' then Exit;

  if Assigned(FDataLink.Datasource.DataSet) then
    Result := FDataLink.Datasource.DataSet.Active;
end;

constructor TAdvDBListBox.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TAdvListBoxDataLink.Create(Self);
end;

procedure TAdvDBListBox.CreateWnd;
begin
  inherited;
  if FLoadOnCreate then
  begin
    FLoadOnCreate := false;
    LoadFromDataSource;
  end;

end;

destructor TAdvDBListBox.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

procedure TAdvDBListBox.DoInsertItem(Value: string);
var
  fld: TField;
begin
  if CheckDataSet then
  begin
    fld := FDataLink.DataSet.FindField(DataField);
    if Assigned(fld) then
    begin
      FDBUpdating := true;
      FDataLink.DataSet.Insert;
      fld.AsString := Value;
      FDataLink.DataSet.Post;
      FDBUpdating := false;
      LoadFromDataSource;
      if FilterActive then
        DoFilter;
      GotoActiveRecord;
    end;
  end;
end;

function TAdvDBListBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TAdvDBListBox.GotoActiveRecord;
begin
  if not FDBUpdating and CheckDataSet then
  begin
    FilteredItemIndex := FDataLink.DataSource.DataSet.RecNo - 1;
  end;
end;

procedure TAdvDBListBox.ListClick(Sender: TObject);
var
  li: TListBoxItem;
  idx: integer;
begin
  inherited;

  if CheckDataSet and not FDBUpdating then
  begin
    idx := ListBox.ItemIndex;
    li := GetItem(idx);
    if Assigned(li) then
    begin
      FDataLink.DataSource.DataSet.RecNo := li.Index + 1;

      ListBox.ItemIndex := idx;
    end;
  end;
end;

procedure TAdvDBListBox.Loaded;
begin
  inherited;
  LoadFromDataSource;
end;

procedure TAdvDBListBox.LoadFromDataSource;
var
  cb : TBookMark;
  d: TDataSet;
  fld: TField;
begin
  if FDBUpdating then
    Exit;

  if not HandleAllocated then
  begin
    FLoadOnCreate := true;
    Exit;
  end;

  if csLoading in ComponentState then
    Exit;

  Items.Clear;

  if not CheckDataSet then
    Exit;


  d := FDataLink.Datasource.DataSet;

  fld := d.FindField(FDataField);

  if not Assigned(fld) then
    raise Exception.Create('Datafield not found');

  FDBUpdating := true;

  d.DisableControls;

  cb := d.GetBookmark;

  d.First;

  while not d.Eof do
  begin
    Items.Add.Text := fld.DisplayText;
    d.Next;
  end;

  d.GotoBookmark(cb);
  d.EnableControls;

  FDBUpdating := false;
end;

procedure TAdvDBListBox.SetDataField(const Value: string);
begin
  FDataField := Value;
end;

procedure TAdvDBListBox.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

{ TAdvListBoxDataLink }

procedure TAdvListBoxDataLink.ActiveChanged;
begin
  inherited;
  FListBox.LoadFromDataSource;
end;

constructor TAdvListBoxDataLink.Create(AListBox: TAdvDBListBox);
begin
  inherited Create;
  FListBox := AListBox;
end;

procedure TAdvListBoxDataLink.DataSetChanged;
begin
  inherited;
  FListBox.LoadFromDataSource;
end;

procedure TAdvListBoxDataLink.DataSetScrolled(distance: integer);
begin
  inherited;
end;

destructor TAdvListBoxDataLink.Destroy;
begin

  inherited;
end;

procedure TAdvListBoxDataLink.RecordChanged(Field: TField);
begin
  inherited;
  FListBox.GotoActiveRecord;
end;

procedure TAdvListBoxDataLink.UpdateData;
begin
  inherited;

end;

end.
