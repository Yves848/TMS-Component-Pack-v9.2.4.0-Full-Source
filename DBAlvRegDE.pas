{***********************************************************************}
{ TADVLISTVIEW, TDBADVLISTVIEW component                                }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   copyright © 1998-2016                                               }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}

unit DBAlvRegDe;

interface

{$I TMSDEFS.INC}

uses
  Advlistv, AlvDE,DBAdvLst,DB,DBAlvDE
  ,Classes,DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string),TListViewField,'FieldName',TLvFieldNameProperty);
end;



end.

