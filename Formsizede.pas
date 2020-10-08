{***************************************************************************}
{ TFormSize component                                                       }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2016                                               }
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

unit formsizede;

{$I TMSDEFS.INC}

interface

uses
  Classes, DesignIntf, DesignEditors, FormSize, DB;

type

  { TFormSizeDataFieldProperty }
  TFormSizeDataFieldProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure GetValueList(List: TStrings);
    procedure SetValue(const Value: string); override;
  end;


implementation

{$IFDEF DELPHI2006_LVL}
uses
  WideStrings;
{$ENDIF}

{ TFormSizeDataFieldProperty }

function TFormSizeDataFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TFormSizeDataFieldProperty.GetValueList(List: TStrings);
var
  Component : TPersistent;
  {$IFNDEF DELPHIXE3_LVL}
  {$IFNDEF DELPHI2006_LVL}
  sl: TStringList;
  {$ENDIF}
  {$IFDEF DELPHI2006_LVL}
  sl: TWideStringList;
  i: integer;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  sl: TStringList;
  {$ENDIF}
  db: TDataBinding;

begin
  Component := GetComponent(0);
  if not Assigned(Component) then
    Exit;

  if Component is TDataBinding then
  begin
    db := Component as TDataBinding;

    if Assigned(db.FormSize) and Assigned(db.FormSize.DataSource) and Assigned(db.FormSize.DataSource.DataSet) then
    begin
      {$IFNDEF DELPHIXE3_LVL}
      {$IFDEF DELPHI2006_LVL}
      sl := TWideStringList.Create;
      {$ENDIF}
      {$IFNDEF DELPHI2006_LVL}
      sl := TStringList.Create;
      {$ENDIF}
      {$ENDIF}
      {$IFDEF DELPHIXE3_LVL}
      sl := TStringList.Create;
      {$ENDIF}

      db.FormSize.DataSource.DataSet.GetFieldNames(sl);

      {$IFNDEF DELPHIXE3_LVL}
      {$IFNDEF DELPHI2006_LVL}
      List.Assign(sl);
      {$ENDIF}

      {$IFDEF DELPHI2006_LVL}
      for i := 1 to sl.Count do
      begin
        List.Add(WideCharToString(PWideChar(sl.Strings[i - 1])));
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFDEF DELPHIXE3_LVL}
      List.Assign(sl);
      {$ENDIF}

      sl.Free;
    end;
  end;
end;

procedure TFormSizeDataFieldProperty.GetValues(Proc: TGetStrProc);
var
  j: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for j := 0 to Values.Count - 1 do
      Proc(Values[j]);
  finally
    Values.Free;
  end;
end;

procedure TFormSizeDataFieldProperty.SetValue(const Value: string);
var
  AFieldName: String;
  APos: Integer;
begin
  AFieldName := Value;

  APos := Pos('.', AFieldName);
  if (APos > 0) then
    AFieldName := Copy(AFieldName, APos + 1, Length(AFieldName));

  SetStrValue(AFieldName);
end;

end.
