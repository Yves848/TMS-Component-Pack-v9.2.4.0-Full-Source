{*************************************************************************}
{ TADVTOOLBUTON component                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2002 - 2012                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{*************************************************************************}
unit atbregde;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvToolBtn, AdvToolButtonDE, DesignIntf;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TAdvToolButton, TAdvToolButtonEditor);  
end;

end.
