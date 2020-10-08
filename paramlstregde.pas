{********************************************************************}
{ TPARAMLISTBOX component                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit paramlstregde;

interface

{$I TMSDEFS.INC}

uses
  Paramlistbox,Classes, paramsde, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TStrings), TParamListBox, 'Items', TParamStringListProperty);
  RegisterComponentEditor(TParamListBox, TParamListDefaultEditor);
end;



end.

