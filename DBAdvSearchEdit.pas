{************************************************************************}
{ TDBADVSEARCHEDIT component                                             }
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

unit DBAdvSearchEdit;

interface

uses
  Classes, Types, Windows, AdvSearchEdit, Controls, DB, DBCtrls,
  AdvSearchList, DBAdvSearchList;

type

  TDBAdvSearchDropDown = class(TAdvSearchDropDown)

  protected
    function CreateSearchList: TAdvSearchList; override;

  end;

  TDBAdvSearchEdit = class(TAdvSearchEdit)
  private
    FDataLink: TFieldDataLink;
    FIsEditing: boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetListSource: TDataSource;
    procedure SetListSource(const Value: TDataSource);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DataUpdate(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure DoSelectValue(Sender: TObject; var Value: string); override;
    procedure DoEditChanged(Sender: TObject); override;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure DoKeyPress(Sender: TObject; var key:char); override;
    function CheckDataSet: boolean; virtual;
    function EditCanModify: Boolean; virtual;
    function CreateSearchDropDown: TAdvSearchDropDown; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ListSource: TDataSource read GetListSource write SetListSource;
  end;

implementation

{ TDBAdvSearchEdit }

procedure TDBAdvSearchEdit.ActiveChange(Sender: TObject);
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if not FDataLink.DataSet.Active or not DataSource.Enabled then
        Text := '';
    end
    else
      Text := '';
  end;
end;

function TDBAdvSearchEdit.CheckDataSet: boolean;
begin
  Result := Assigned(FDataLink.DataSource) and
            Assigned(FDataLink.DataSource.DataSet) and
            FDataLink.DataSource.DataSet.Active and
            (DataField <> '');
end;

constructor TDBAdvSearchEdit.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;

  FIsEditing := false;

  ControlStyle := ControlStyle + [csReplicatable];
end;

function TDBAdvSearchEdit.CreateSearchDropDown: TAdvSearchDropDown;
begin
  Result := TDBAdvSearchDropDown.Create(Self);
end;

procedure TDBAdvSearchEdit.DataChange(Sender: TObject);
begin
  if not Assigned(FDataLink.DataSet) then
    Exit;

  if FIsEditing then
    Exit;

  if Assigned(FDataLink.Field) then
    Text := FDataLink.Field.Text;
end;

procedure TDBAdvSearchEdit.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    FDataLink.Field.Text := Text;
end;

destructor TDBAdvSearchEdit.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

procedure TDBAdvSearchEdit.DoEditChanged(Sender: TObject);
begin
  FDataLink.Modified;
  inherited;
end;

procedure TDBAdvSearchEdit.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not Assigned(DataSource) then
  begin
    inherited;
    Exit;
  end;

  if (Key = VK_DELETE) or (Key = VK_BACK) or ((Key = VK_INSERT) and (ssShift in Shift)) then
  begin
    if not EditCanModify then
    begin
      Key := 0;
      Exit;
    end
    else
      FDataLink.Modified;
  end;

  if FDataLink.ReadOnly and (key = VK_DELETE) then
    Key := 0;

  inherited;

  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TDBAdvSearchEdit.DoKeyPress(Sender: TObject; var key: char);
begin
  if not Assigned(DataSource) then
  begin
    inherited;
    Exit;
  end;

  if (Key = #8) and not EditCanMOdify then
    Exit;

  inherited;

  if (Key >= #32) and (FDataLink.Field <> nil) and (Key <> '.') and
    not FDataLink.Field.IsValidChar(Key) or (FDataLink.ReadOnly) then
  begin
    MessageBeep(0);
    Key := #0;
  end;

  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDBAdvSearchEdit.DoSelectValue(Sender: TObject; var Value: string);
begin
  inherited;

  if CheckDataSet then
  begin
    FIsEditing := true;
    FDataLink.Edit;
    FDataLink.Modified;
    FIsEditing := false;
  end;
end;

function TDBAdvSearchEdit.EditCanModify: Boolean;
begin
  if Assigned(DataSource) then
    Result := FDataLink.Edit
  else
    Result := true;
end;

function TDBAdvSearchEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvSearchEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBAdvSearchEdit.GetListSource: TDataSource;
begin
  Result := (SearchList as TDBAdvSearchList).DataSource;
end;

procedure TDBAdvSearchEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;

  if (Operation = opRemove) and (SearchList <> nil) and (AComponent = ListSource) then
    ListSource := nil;
end;

procedure TDBAdvSearchEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBAdvSearchEdit.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBAdvSearchEdit.SetListSource(const Value: TDataSource);
begin
  (SearchList as TDBAdvSearchList).DataSource := Value;
end;

{ TDBAdvSearchDropDown }

function TDBAdvSearchDropDown.CreateSearchList: TAdvSearchList;
begin
  Result := TDBAdvSearchList.Create(Self);
end;

end.
