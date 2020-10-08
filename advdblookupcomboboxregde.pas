{***********************************************************************}
{ TAdvDBLookupComboReg component                                        }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   Copyright © 2002 - 2016                                             }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}

unit AdvDBLookupComboBoxRegDe;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvDBLookupComboBox, AdvDBLookupComboBoxDE, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string),TDBColumnItem,'ListField',TAdvDBComboFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TAdvDBLookupComboBox,'KeyField',TAdvDBComboFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TAdvDBLookupComboBox,'FilterField',TAdvDBComboFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TAdvDBLookupComboBox,'LabelField',TAdvDBComboFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TAdvDBLookupComboBox,'SortColumn',TAdvDBComboColumnNameProperty);
  RegisterComponentEditor(TAdvDBLookupComboBox,TAdvDBLookupComboBoxEditor);
end;

end.

