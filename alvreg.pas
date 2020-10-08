{***********************************************************************}
{ TADVLISTVIEW, TDBADVLISTVIEW component                                }
{ for Delphi & C++Builder                                               }
{ version 1.6                                                           }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   copyright © 1998-2016                                               }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}
unit alvreg;

interface

{$I TMSDEFS.INC}

uses
  Classes, Advlistv;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TAdvListView]);
end;



end.

