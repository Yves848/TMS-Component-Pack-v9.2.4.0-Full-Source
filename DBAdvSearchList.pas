{************************************************************************}
{ TDBADVSEARCHLIST component                                             }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 2016 - 2018                                     }
{            Email : info@tmssoftware.com                                }
{            Web : http://www.tmssoftware.com                            }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit DBAdvSearchList;

interface

uses
  Classes, Types, Controls, DB, DBCtrls, AdvSearchList, Dialogs;

type
  TDBColumnItem = class(TColumnItem)
  private
    FDataField: string;
    FField: TField;
    procedure SetDataField(const Value: string);
  protected
    property Field: TField read FField write FField;
  published
    property DataField: string read FDataField write SetDataField;
  end;

  TDBColumnItems = class(TColumnItems)
  private
    FOnDataChanged: TNotifyEvent;
    function GetItems(Index: integer): TDBColumnItem;
    procedure SetItems(Index: integer; const Value: TDBColumnItem);
  protected
    procedure DataChanged; virtual;
    function GetItemClass: TCollectionItemClass; override;
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
  public
    function Add: TDBColumnItem;
    function Insert(Index: integer): TDBColumnItem;
    property Items[Index: integer]: TDBColumnItem read GetItems write SetItems; default;
  end;

  TDBAdvSearchListDataLink = class(TDataLink)
  private
    FOnActiveChange: TNotifyEvent;
  protected
    procedure ActiveChanged; override;
  published
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;

  TDBAdvSearchList = class(TAdvSearchList)
  private
    FDataLink: TDBAdvSearchListDataLink;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetColumnItems: TDBColumnItems;
    procedure SetColumnItems(const Value: TDBColumnItems);
  protected
    procedure ActiveChange(Sender: TObject); virtual;
    procedure DataChanged(Sender: TObject); virtual;
    function CreateColumns: TColumnItems; override;
    procedure LoadItems; virtual;
    function InitFields: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reload; virtual;
  published
    property Columns: TDBColumnItems read GetColumnItems write SetColumnItems;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

implementation

uses
  GDIPicture;

{ TDBAdvSearchList }

procedure TDBAdvSearchList.ActiveChange(Sender: TObject);
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSource) and Assigned(FDataLink.DataSet) then
    begin
      if not FDataLink.DataSet.Active or not DataSource.Enabled then
        Items.Clear
      else
      begin
        LoadItems;
      end;
    end
    else
      Items.Clear;
  end;
end;

constructor TDBAdvSearchList.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TDBAdvSearchListDataLink.Create;
  FDataLink.OnActiveChange := ActiveChange;

  (Columns as TDBColumnItems).OnDataChanged := DataChanged;
end;

function TDBAdvSearchList.CreateColumns: TColumnItems;
begin
  Result := TDBColumnItems.Create(Self);
end;

procedure TDBAdvSearchList.DataChanged(Sender: TObject);
begin
  LoadItems;
end;

destructor TDBAdvSearchList.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

function TDBAdvSearchList.GetColumnItems: TDBColumnItems;
begin
  Result := TDBColumnItems(inherited Columns);
end;

function TDBAdvSearchList.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBAdvSearchList.InitFields: boolean;
var
  i: integer;
  dbcol: TDBColumnItem;
begin
  Result := false;
  for i := 0 to Columns.Count - 1 do
  begin
    dbcol := (Columns[i] as TDBColumnItem);
    dbcol.Field := FDataLink.DataSet.FindField(dbcol.DataField);
    if Assigned(dbcol.Field) then
      Result := true;
  end;
end;

procedure TDBAdvSearchList.LoadItems;
var
  cb: TBookmark;
  it: TSearchListItem;
  col: TSearchColumnItem;
  i: integer;
  Stream: TMemoryStream;
   pic: TGDIPPicture;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) and
     Assigned(FDataLink.DataSet) and FDataLink.DataSet.Active then
  begin
    Items.Clear;

    if InitFields then
    begin
      FDataLink.DataSet.DisableControls;

      cb := FDataLink.DataSet.GetBookmark;
      FDataLink.DataSet.First;
      Stream := TMemoryStream.Create;

      try

        while not FDataLink.DataSet.Eof do
        begin
          it := Items.Add;

          for i := 0 to Columns.Count - 1  do
          begin
            if ((Columns[i] as TDBColumnItem).Field is TBlobField) then
            begin
              Stream.Clear;
              ((Columns[i] as TDBColumnItem).Field as TBlobField).SaveToStream(Stream);
              Stream.Position := 0;
              col := it.Columns[i];
              if not Assigned(col) then
                col := it.Columns.Add;
              pic := col.Picture;
              pic.LoadFromStream(Stream);
            end
            else
              it.Captions[i] := (Columns[i] as TDBColumnItem).Field.AsString;
          end;

          FDataLink.DataSet.Next;
        end;
      finally
        Stream.Free;
        FDataLink.DataSet.GotoBookMark(cb);
        FDataLink.DataSet.FreeBookmark(cb);

        FDataLink.DataSet.EnableControls;
      end;
    end;
  end;
end;

procedure TDBAdvSearchList.Reload;
begin
  LoadItems;
end;

procedure TDBAdvSearchList.SetColumnItems(const Value: TDBColumnItems);
begin
  Columns.Assign(Value);
end;

procedure TDBAdvSearchList.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

{ TDBColumnItems }

function TDBColumnItems.Add: TDBColumnItem;
begin
  Result := TDBColumnItem(inherited Add);
end;

procedure TDBColumnItems.DataChanged;
begin
  if Assigned(OnDataChanged) then
    OnDataChanged(Self);
end;

function TDBColumnItems.GetItemClass: TCollectionItemClass;
begin
  Result := TDBColumnItem;
end;


function TDBColumnItems.GetItems(Index: integer): TDBColumnItem;
begin
  Result := TDBColumnItem(inherited Items[Index]);
end;

function TDBColumnItems.Insert(Index: integer): TDBColumnItem;
begin
  Result := TDBColumnItem(inherited Insert(Index));
end;

procedure TDBColumnItems.SetItems(Index: integer; const Value: TDBColumnItem);
begin
  inherited Items[Index] := Value;
end;

{ TDBAdvSearchListDataLink }

procedure TDBAdvSearchListDataLink.ActiveChanged;
begin
  inherited;
  if Assigned(OnActiveChange) then
    OnActiveChange(Self);
end;

{ TDBColumnItem }

procedure TDBColumnItem.SetDataField(const Value: string);
begin
  if (FDataField <> Value) then
  begin
    FDataField := Value;
    (Collection as TDBColumnItems).DataChanged;
  end;
end;

end.
