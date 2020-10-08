{********************************************************************}
{ TADVMASKEDITBTN and TUnitAdvMaskEditBtn component                  }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2000 - 2016                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit AdvMEdBr;

interface

uses
  AdvMEdBtn,Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TAdvMaskEditBtn,TUnitAdvMaskEditBtn]);
end;



end.

