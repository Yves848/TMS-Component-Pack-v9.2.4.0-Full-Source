{********************************************************************}
{ TPARAMTREEVIEW component                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2000-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit paramtreeviewregde;

{$I TMSDEFS.INC}

interface

uses
  ParamTreeview, Classes, paramsde, ComCtrls, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TTreeNodes), TParamTreeView, 'Items', TParamNodesProperty);
  RegisterComponentEditor(TParamTreeView, TParamListDefaultEditor);
end;



end.

