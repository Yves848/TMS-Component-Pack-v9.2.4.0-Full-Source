{************************************************************************}
{ TDBAdvRichEditorRTFIO                                                  }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 2017                                            }
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

unit DBAdvRichEditorRTFIO;

interface

uses
  Classes, AdvRichEditor, AdvRichEditorBase, AdvRichEditorIO, DB, DBCtrls;

type
  TDBAdvRichEditorRTFIO = class(TAdvRichEditorRTFIO)
  private
    FDatalink: TFieldDataLink;
    FDBUpdate: boolean;
    FModifying: boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SetRichEditor(AValue: TAdvRichEditorBase); override;
    procedure ActiveChange(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    procedure DataUpdate(Sender: TObject); virtual;
    procedure EditModified(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TAdvRichEditorBaseEx = class(TAdvRichEditorBase)
  end;

implementation

{ TDBAdvRichEditorRTFIO }

procedure TDBAdvRichEditorRTFIO.ActiveChange(Sender: TObject);
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if not FDataLink.DataSet.Active then
        if Assigned(RichEditor) then
          RichEditor.Clear;
    end
    else
      if Assigned(RichEditor) then
        RichEditor.Clear;
  end;
end;

constructor TDBAdvRichEditorRTFIO.Create(AOwner: TComponent);
begin
  inherited;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;
  FDBUpdate := false;
  FModifying := false;
end;

procedure TDBAdvRichEditorRTFIO.DataChange(Sender: TObject);
var
  Stream: TStream;
begin
  if not Assigned(FDataLink.DataSet) then
    Exit;

  if not Assigned(RichEditor) then
    Exit;

  if Assigned(FDataLink.Field) and not FDBUpdate then
  begin
    FModifying := true;

    // load from blob
    Stream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmRead);
    try
      RichEditor.Clear;
      Load(Stream);
      RichEditor.Refresh;
    finally
      Stream.Free;
    end;
    RichEditor.Refresh;

    FModifying := false;
  end;
end;

procedure TDBAdvRichEditorRTFIO.DataUpdate(Sender: TObject);
var
  Stream: TStream;
begin
  if not Assigned(RichEditor) then
    Exit;

  if Assigned(FDataLink.Field) then
  begin
    FDBUpdate := true;
    Stream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmWrite);
    try
      Save(Stream);
    finally
      Stream.Free;
      FDBUpdate := false;
    end;
  end;

end;

destructor TDBAdvRichEditorRTFIO.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

procedure TDBAdvRichEditorRTFIO.EditModified(Sender: TObject);
begin
  if FModifying then
    Exit;

  if not (FDataLink.DataSet.State in [dsEdit,dsInsert]) then
  begin
    FDataLink.Edit;
    FDataLink.Modified;
  end
  else
  begin
    FDataLink.Modified;
  end;
end;

function TDBAdvRichEditorRTFIO.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvRichEditorRTFIO.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBAdvRichEditorRTFIO.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = RichEditor) then
    RichEditor := nil;

end;

procedure TDBAdvRichEditorRTFIO.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBAdvRichEditorRTFIO.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBAdvRichEditorRTFIO.SetRichEditor(AValue: TAdvRichEditorBase);
begin
  if (AValue = nil) and Assigned(RichEditor) then
    TAdvRichEditorBaseEx(RichEditor).OnModified := nil;

  inherited;

  if Assigned(RichEditor) then
    TAdvRichEditorBaseEx(RichEditor).OnModified := EditModified;
end;

end.
