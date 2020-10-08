{***************************************************************************}
{ TAdvDBFormComboBox component                                              }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014 - 2016                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvDBFormComboBox;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Menus, Dialogs, StdCtrls, DB, DBCtrls;

type
  TAdvDBFormComboBox = class(TComboBox)
  private
    FDataLink: TFieldDataLink;
    FValue: string;
    FValues: TStrings;
    FOnChange: TNotifyEvent;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function GetValue: string;
    function GetButtonValue(Index: Integer): string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetValue(const Value: string);
    procedure SetValues(Value: TStrings);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure Changed; dynamic;
    procedure Click; override;
    procedure DropDown; override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property DataLink: TFieldDataLink read FDataLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
    property Value: string read GetValue write SetValue;
    property ItemIndex;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Values: TStrings read FValues write SetValues;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


procedure Register;

implementation

constructor TAdvDBFormComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FValue  := '';
  FValues := TStringList.Create;
  Style   := csDropDownList;
end;

destructor TAdvDBFormComboBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FValues.Free;
  inherited Destroy;
end;

procedure TAdvDBFormComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TAdvDBFormComboBox.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FValues.Count > 0 then
      ItemIndex := FValues.IndexOf(FDataLink.Field.Text)
    else
      ItemIndex := Items.IndexOf(FDataLink.Field.Text)
  end
  else
    ItemIndex := -1;
end;

procedure TAdvDBFormComboBox.UpdateData(Sender: TObject);
var
  idx: integer;
begin
  idx := Items.IndexOf(Text);
  if idx <> -1 then
  begin
    if (FValues.Count > 0) and (FValues.Count = Items.Count) then
      FDataLink.Field.Text := FValues[idx]
    else
      FDataLink.Field.Text := Text;
  end
  else
    FDataLink.Field.Text := '';
end;

function TAdvDBFormComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TAdvDBFormComboBox.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TAdvDBFormComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TAdvDBFormComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TAdvDBFormComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TAdvDBFormComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TAdvDBFormComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TAdvDBFormComboBox.GetValue : string;
begin
  Result := FValue;
end;

function TAdvDBFormComboBox.GetButtonValue(Index: Integer): string;
begin
  if (Index < FValues.Count) and (FValues[Index] <> '') then
    Result := FValues[Index]
  else if (Index < Items.Count) then
    Result := Items[Index]
  else
    Result := '';
end;

procedure TAdvDBFormComboBox.SetValue (const Value: string);
var
  I : Integer;
begin
  FValue := Value;
  if (ItemIndex < 0) or (GetButtonValue(ItemIndex) <> Value) then
  begin
    if (ItemIndex >= 0) then ItemIndex := -1;
    for I := 0 to Items.Count - 1 do
    begin
      if GetButtonValue(I) = Value then
      begin
        ItemIndex := I;
        break;
      end;
    end;
    Changed;
  end;
end;

procedure TAdvDBFormComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TAdvDBFormComboBox.Click;
begin
  if FDataLink.Edit then
  begin
   inherited Click;
   FDataLink.Modified;
  end;
end;

procedure TAdvDBFormComboBox.DropDown;
begin
  FDataLink.Edit;
  inherited DropDown;
end;

procedure TAdvDBFormComboBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
  DataChange(Self);
end;

procedure TAdvDBFormComboBox.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TAdvDBFormComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #32..#255:
      if not FDataLink.Edit then Key := #0;
    #27:
      FDataLink.Reset;
  end;
end;


procedure Register;
begin
  RegisterComponents('TMS Edit',[TAdvDBFormComboBox]);
end;

end.

