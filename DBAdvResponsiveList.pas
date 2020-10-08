{*************************************************************************}
{ TMS TDBAdvResponsiveList                                                }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2016 - 2017                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit DBAdvResponsiveList;

interface

uses
  Classes, DB, DBCtrls, AdvResponsiveList, PictureContainer;

type

  TDataSetScrollEvent = procedure(Sender: TObject; Distance: integer) of object;

  TFieldsToResponsiveItemEvent = procedure(Sender: TObject; AItem: TResponsiveListItem; Fields: TFields) of object;

  TDBAdvResponsiveListDataLink = class(TDataLink)
  private
    FOnActiveChange: TNotifyEvent;
    FOnDataSetScrolled: TDataSetScrollEvent;
  protected
    procedure ActiveChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
  published
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
    property OnDataSetScrolled: TDataSetScrollEvent read FOnDataSetScrolled write FOnDataSetScrolled;
  end;

  TDBResponsiveListItem = class(TResponsiveListItem)
  private
  published
  end;

  TDBAdvResponsiveList = class(TAdvResponsiveList)
  private
    FDataLink: TDBAdvResponsiveListDataLink;
    FPrevSel: integer;
    FDisableScroll: boolean;
    FPictureContainer: TPictureContainer;
    FOnFieldsToItem: TFieldsToResponsiveItemEvent;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
  protected
    function CheckDataSet: boolean;
    procedure ActiveChange(Sender: TObject);
    procedure DataSetScrolled(Sender: TObject; Distance: Integer);
    function GetItemClass: TCollectionItemClass; override;
    procedure DoSelectItem(AIndex: Integer); override;
    procedure DoFieldsToItem(AItem: TResponsiveListItem; Fields: TFields); virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property OnFieldsToItem: TFieldsToResponsiveItemEvent read FOnFieldsToItem write FOnFieldsToItem;
  end;

implementation

uses
  StrUtils, SysUtils;

procedure ParseTemplateValues(ATemplate: string; Values: TStrings);
var
  aftertag,fld: string;
  i,j:integer;
begin
  while Pos('(#',ATemplate) > 0 do
  begin
    i := pos('(#',ATemplate);
    aftertag := copy(ATemplate,i,length(ATemplate)); //part after the tag
    j := pos(')',aftertag);
    fld := copy(aftertag,1,j-1);
    Delete(fld,1,2);
    Delete(ATemplate,1,i+j-1);
    Values.Add(fld);
  end;
end;

{ TDBAdvResponsiveList }

procedure TDBAdvResponsiveList.ActiveChange(Sender: TObject);
var
  cb: TBookmark;
  it: TDBResponsiveListItem;
  cond: TResponsiveCondition;
  sl: TStringList;
  fld: array of TField;
  i,j: integer;
  stream: TMemoryStream;
  pic: TPictureItem;
begin
  Items.Clear;

  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSource) and Assigned(FDataLink.DataSet) then
    begin
      if FDataLink.DataSet.Active and DataSource.Enabled then
      begin
        if not Assigned(PictureContainer) then
          PictureContainer := FPictureContainer;

        FDataLink.DataSet.DisableControls;

        cond := GetCondition;

        sl := TStringList.Create;

        ParseTemplateValues(cond.Template, sl);
        ParseTemplateValues(cond.HeaderTemplate, sl);
        ParseTemplateValues(cond.FooterTemplate, sl);

        SetLength(fld, sl.Count);

        cb := FDataLink.DataSet.GetBookmark;

        for i := 0 to sl.Count - 1 do
        begin
          fld[i] := FDataLink.DataSet.FindField(sl.Strings[i]);
        end;

        j := 0;

        FDataLink.DataSet.First;

        BeginUpdate;
        Stream := TMemoryStream.Create;

        try
          while not FDataLink.DataSet.Eof do
          begin
            it := TDBResponsiveListItem(Items.Add);

            for i := 0 to sl.Count - 1 do
            begin
              if Assigned(fld[i]) then
              begin
                if fld[i].DataType = ftMemo then
                begin
                  it.Values[Uppercase(fld[i].FieldName)] := fld[i].AsString;
                end
                else
                if fld[i].DataType = ftBlob then
                begin
                  Stream.Clear;

                  (fld[i] as TBlobField).SaveToStream(Stream);

                  Stream.Position := 0;

                  pic := PictureContainer.Items.Add;
                  pic.Name := 'DBIMG'+inttostr(j);
                  pic.Picture.LoadFromStream(Stream);
                  it.Values[Uppercase(fld[i].FieldName)] := pic.Name;
                  inc(j);
                end
                else
                  it.Values[Uppercase(fld[i].FieldName)] := fld[i].AsString;
              end;
            end;

            DoFieldsToItem(it, FDataLink.DataSet.Fields);

            FDataLink.DataSet.Next;
          end;

        finally
          EndUpdate;

          Stream.Free;
          sl.Free;
          FDataLink.DataSet.GotoBookmark(cb);
          FDataLink.DataSet.FreeBookmark(cb);
          FDataLink.DataSet.EnableControls;
        end;
      end;
    end;
  end;
end;

function TDBAdvResponsiveList.CheckDataSet: boolean;
begin
  Result := Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active;
end;

constructor TDBAdvResponsiveList.Create(AOwner: TComponent);
begin
  inherited;
  FPictureContainer := TPictureContainer.Create(Self);

  FDataLink := TDBAdvResponsiveListDataLink.Create;
  FDataLink.OnActiveChange := ActiveChange;
  FDataLink.OnDataSetScrolled := DataSetScrolled;
  FPrevSel := -1;
  FDisableScroll := false;
end;

procedure TDBAdvResponsiveList.DataSetScrolled(Sender: TObject;
  Distance: Integer);
begin
  if FDisableScroll then
    Exit;

  if ItemIndex >= 0 then
    ItemIndex := ItemIndex + Distance
  else
    ItemIndex := Distance;

  if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
    ScrollInView(Items[ItemIndex]);
end;

destructor TDBAdvResponsiveList.Destroy;
begin
  FPictureContainer.Free;
  FDataLink.Free;
  inherited;
end;

procedure TDBAdvResponsiveList.DoFieldsToItem(AItem: TResponsiveListItem;
  Fields: TFields);
begin
  if Assigned(OnFieldsToItem) then
    OnFieldsToItem(Self, AItem, Fields);
end;

procedure TDBAdvResponsiveList.DoSelectItem(AIndex: Integer);
begin
  inherited;
  if CheckDataSet then
  begin
    FDisableScroll := true;
    if FPrevSel = -1 then
    begin
      FDatalink.DataSet.First;
      FDatalink.DataSet.MoveBy(AIndex);
    end
    else
    begin
      FDatalink.DataSet.MoveBy(AIndex - FPrevSel);
      FPrevSel := AIndex;
    end;
    FDisableScroll := false;
  end;
end;

function TDBAdvResponsiveList.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBAdvResponsiveList.GetItemClass: TCollectionItemClass;
begin
  Result := TDBResponsiveListItem;
end;

procedure TDBAdvResponsiveList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and Assigned(FDataLink) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TDBAdvResponsiveList.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;



{ TDBAdvResponsiveListDataLink }

procedure TDBAdvResponsiveListDataLink.ActiveChanged;
begin
  inherited;
  if Assigned(OnActiveChange) then
    OnActiveChange(Self);
end;

procedure TDBAdvResponsiveListDataLink.DataSetScrolled(Distance: Integer);
begin
  inherited;
  if Assigned(OnDataSetScrolled) then
    OnDataSetScrolled(Self, Distance);
end;

end.
