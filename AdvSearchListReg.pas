{*************************************************************************}
{ TMS TAdvSearchList                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2016                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvSearchListReg;

{$I TMSDEFS.INC}

{$R AdvSearchListReg.dcr}

interface

uses
  Classes, AdvSearchList, Variants
  {$IFNDEF FNCLIB}
  , AdvSearchEdit, DBAdvSearchList, DBAdvSearchEdit
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  {$IFNDEF FNCLIB}
  RegisterComponents('TMS Edits',[TAdvSearchList]);
  RegisterComponents('TMS Edits',[TAdvSearchEdit]);
  RegisterComponents('TMS Edits',[TDBAdvSearchEdit]);
  RegisterComponents('TMS Edits',[TDBAdvSearchList]);
  {$ENDIF}

  {$IFDEF FNCLIB}
  RegisterComponents('TMS FNC UI',[TAdvSearchList]);
  {$ENDIF}
end;


end.
