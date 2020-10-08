{***************************************************************************}
{ TAdvDBDataLabel component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2018                                               }
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

unit AdvDBDataLabel;

interface

uses
  Classes, Controls, AdvDataLabel, DBCtrls, DB;


type

  TAdvDBDataLabel = class(TAdvDataLabel)
  private
    FDataLink: TFieldDataLink;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetDataField: string;
    procedure SetDataField(Value: string);
  protected
    function GetDataString: string; override;
    procedure ShowFieldValue;
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;


implementation

{ TAdvDBDataLabel }

procedure TAdvDBDataLabel.ActiveChange(Sender: TObject);
begin
  if not Assigned(FDataLink.DataSet) then
  begin
    Data := '';
    Exit;
  end;

  if FDataLink.DataSet.Active then
    ShowFieldValue
  else
    Data := '';
end;

constructor TAdvDBDataLabel.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnActiveChange := ActiveChange;
  ControlStyle := ControlStyle + [csReplicatable];
end;

procedure TAdvDBDataLabel.DataChange(Sender: TObject);
begin
  if not Assigned(FDataLink.DataSet) then
  begin
    Data := '';
    Exit;
  end;

  ShowFieldValue;
end;

destructor TAdvDBDataLabel.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

function TAdvDBDataLabel.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TAdvDBDataLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TAdvDBDataLabel.GetDataString: string;
begin
  if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    Result := FDataLink.Field.DisplayText;
  end
  else
    Result := inherited GetDataString;
end;

procedure TAdvDBDataLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TAdvDBDataLabel.SetDataField(Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TAdvDBDataLabel.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TAdvDBDataLabel.ShowFieldValue;
begin
  if Assigned(FDataLink.Field) then
    Data := FDataLink.Field.Text
  else
    Data := '';
end;

end.
