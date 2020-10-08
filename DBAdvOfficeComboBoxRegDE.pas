{************************************************************************}
{ TDBADVOFFICECOMBOBOX component                                         }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 2016 - 2017                                     }
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

unit DBAdvOfficeComboBoxRegDE;

interface

uses
  Classes, DBAdvOfficeComboBox, DB, DesignIntf, DesignEditors, ContNrs;

type
  TDBAdvOfficeComboBoxFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


procedure Register;

implementation

{ TDBAdvOfficeComboBoxFieldNameProperty }

function TDBAdvOfficeComboBoxFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TDBAdvOfficeComboBoxFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  FCombo: TDBAdvOfficeComboBox;
  FDataSource: TDataSource;
  FDataSet: TDataSet;
  st: TStringList;
  i: Integer;
begin
  FCombo := GetComponent(0) as TDBAdvOfficeComboBox;
  FDataSource := FCombo.ListSource;
  if not Assigned(FDataSource) then
    Exit;

  FDataSet := FDataSource.DataSet;

  if not Assigned(FDataSet) then
    Exit;

  st := TStringList.Create;
  FDataSet.GetFieldNames(st);
  for i := 1 to st.Count do
    proc(st.Strings[i-1]);
  st.Free;
end;


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string),TDBAdvOfficeComboBox,'ListField',TDBAdvOfficeComboBoxFieldNameProperty);
end;

end.
