{***********************************************************************}
{ TADVLISTVIEW design time support                                      }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   copyright © 1998-2016                                               }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}

unit AlvRegDe;

interface

{$I TMSDEFS.INC}

uses
  Advlistv, AlvDE,Classes,DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TAdvListView,TAdvListViewEditor);
end;



end.

