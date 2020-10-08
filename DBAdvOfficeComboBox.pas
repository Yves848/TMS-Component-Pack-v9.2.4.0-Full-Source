{************************************************************************}
{ TDBADVOFFICECOMBOBOX component                                         }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 2016 - 2019                                     }
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

unit DBAdvOfficeComboBox;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Types, Controls, StdCtrls, DB, DBCtrls,
  AdvOfficeComboBox, AdvFontCombo;

type

  TDBAdvOfficeComboBox = class(TAdvOfficeComboBox)
  private
    FDataLink: TFieldDataLink;
    FListLink: TFieldDataLink;
    FIsEditing: boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetListField: string;
    function GetListSource: TDataSource;
    procedure SetListField(const Value: string);
    procedure SetListSource(const Value: TDataSource);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DataUpdate(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure ListActiveChange(Sender: TObject);
    function CheckDataSet: Boolean; virtual;
    procedure ChangeItemIndex; override;
    function EditCanModify: Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure ValueChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Change; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ListField: string read GetListField write SetListField;
    property ListSource: TDataSource read GetListSource write SetListSource;
  end;

implementation

{ TDBAdvOfficeComboBox }

procedure TDBAdvOfficeComboBox.ActiveChange(Sender: TObject);
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

procedure TDBAdvOfficeComboBox.Change;
begin
  FDataLink.Modified;
  inherited;
end;

procedure TDBAdvOfficeComboBox.ChangeItemIndex;
begin
  inherited;
  if EditCanModify then
    FDataLink.Modified;
end;

function TDBAdvOfficeComboBox.CheckDataSet: Boolean;
begin
  Result := Assigned(FDataLink.DataSource) and
            Assigned(FDataLink.DataSource.DataSet) and
            FDataLink.DataSource.DataSet.Active and
            (DataField <> '');
end;

constructor TDBAdvOfficeComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;

  FListLink := TFieldDataLink.Create;
  FListLink.Control := Self;
  FListLink.OnActiveChange := ListActiveChange;

  FIsEditing := false;

  ControlStyle := ControlStyle + [csReplicatable];
end;

procedure TDBAdvOfficeComboBox.DataChange(Sender: TObject);
begin
  if not Assigned(FDataLink.DataSet) then
    Exit;

  if FIsEditing then
    Exit;

  if Assigned(FDataLink.Field) then
    Text := FDataLink.Field.Text;
end;

procedure TDBAdvOfficeComboBox.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    FDataLink.Field.Text := Text;
end;

destructor TDBAdvOfficeComboBox.Destroy;
begin
  FDataLink.Free;
  FListLink.Free;
  inherited;
end;

function TDBAdvOfficeComboBox.EditCanModify: Boolean;
begin
  if Assigned(DataSource) then
    Result := FDataLink.Edit
  else
    Result := true;
end;

function TDBAdvOfficeComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvOfficeComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBAdvOfficeComboBox.GetListField: string;
begin
  Result := FListLink.FieldName;
end;

function TDBAdvOfficeComboBox.GetListSource: TDataSource;
begin
  Result := FListLink.DataSource;
end;

procedure TDBAdvOfficeComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not CheckDataSet then
  begin
    inherited;
    Exit;
  end;

  if (Key = VK_DELETE) or (Key = VK_BACK) or ((Key = VK_INSERT) and (ssShift in Shift)) then
  begin
    if not EditCanModify then
    begin
      key := 0;
      Exit;
    end
    else
      FDataLink.Modified;
  end;

  if FDataLink.ReadOnly and (key = VK_DELETE) then
    Key := 0;

  inherited KeyDown(Key, Shift);

  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TDBAdvOfficeComboBox.KeyPress(var Key: Char);
begin
  if not CheckDataSet then
  begin
    inherited;
    Exit;
  end;

  if (Key = #8) and not EditCanMOdify then
    Exit;

  inherited KeyPress(Key);

  {$IFNDEF DELPHI_UNICODE}
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and (Key <> '.') and
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  if (Key >= #32) and (FDataLink.Field <> nil) and (Key <> '.') and
  {$ENDIF}
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

procedure TDBAdvOfficeComboBox.ListActiveChange(Sender: TObject);
var
  fld: TField;
  cb: TBookmark;
  s: string;
begin
  if Assigned(FListLink) then
  begin
    if Assigned(FListLink.DataSource) and Assigned(FListLink.DataSet) then
    begin
      if not FListLink.DataSet.Active or not DataSource.Enabled then
        Items.Clear
      else
      begin
        fld := FListLink.DataSet.FindField(ListField);

        if Assigned(fld) then
        begin
          Items.Clear;

          FListLink.DataSet.DisableControls;

          cb := FListLink.DataSet.GetBookmark;

          FListLink.DataSet.First;

          while not FListLink.DataSet.Eof do
          begin
            s := fld.AsString;
            if Items.IndexOf(s) = -1 then
              Items.Add(s);

            FListLink.DataSet.Next;
          end;

          FListLink.DataSet.GotoBookMark(cb);
          FListLink.DataSet.FreeBookmark(cb);

          FListLink.DataSet.EnableControls;
        end;
      end;
    end
    else
      Items.Clear;
  end;

end;

procedure TDBAdvOfficeComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;

  if (Operation = opRemove) and (FListLink <> nil) and (AComponent = ListSource) then
    ListSource := nil;
end;

procedure TDBAdvOfficeComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBAdvOfficeComboBox.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBAdvOfficeComboBox.SetListField(const Value: string);
begin
  FListLink.FieldName := Value;
end;

procedure TDBAdvOfficeComboBox.SetListSource(const Value: TDataSource);
begin
  FListLink.DataSource := Value;
end;

procedure TDBAdvOfficeComboBox.ValueChanged;
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

end.
