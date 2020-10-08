{***************************************************************************}
{ TMS Component Pack Pro                                                    }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1998 - 2017                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit TMSSplash;

interface

{$HINTS OFF}
{$IFDEF ConditionalExpressions}
{$if CompilerVersion >= 18}
procedure Register;
{$ifend}
{$ENDIF}

implementation

{$IFDEF ConditionalExpressions}
{$if CompilerVersion >= 18}
uses
  ToolsApi, Classes, Graphics, SysUtils, DesignIntf, Dialogs;

{$I TMSProductSplash.inc}

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  AddSplash;
end;
{$ifend}
{$ENDIF}
{$HINTS ON}

end.

