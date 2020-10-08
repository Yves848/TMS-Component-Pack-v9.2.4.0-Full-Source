{*************************************************************************}
{ TMS TAdvResponsiveListTypes                                             }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2016 - 2017                                       }
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
unit AdvResponsiveListTypes;

interface

{$I TMSDEFS.INC}

uses
  Classes, Variants;

type
  TResponsiveNameValuePair = record
    Name: string;
    Value: variant;
    {$IFDEF LCLLIB}
    class operator = (rnvp1, rnvp2: TResponsiveNameValuePair): Boolean;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    class operator Equal (rnvp1, rnvp2: TResponsiveNameValuePair): Boolean;
    {$ENDIF}
  end;

implementation

{ TResponsiveNameValuePair }

{$IFDEF LCLLIB}
class operator TResponsiveNameValuePair.=(rnvp1,
  rnvp2: TResponsiveNameValuePair): Boolean;
{$ENDIF}
{$IFNDEF LCLLIB}
class operator TResponsiveNameValuePair.Equal(rnvp1,
  rnvp2: TResponsiveNameValuePair): Boolean;
{$ENDIF}
begin
  Result := (rnvp1.Name = rnvp2.Name) and (rnvp1.Value = rnvp2.Value);
end;

end.
