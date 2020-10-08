{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2017                                              }
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

unit AdvGeneralDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvUtils, AdvTypes
  {$IFDEF WIN32}
  ,Windows, ShellApi, SysUtils, DateUtils, DesignIntf, DesignEditors
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,VCL.Dialogs
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,Dialogs
  {$ENDIF}
  ;

{$IFDEF WIN32}
type
  TAdvDefaultEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;
{$ENDIF}

implementation

{$IFDEF WIN32}

{ TAdvDefaultEditor }

procedure TAdvDefaultEditor.ExecuteVerb(Index: Integer);
var
  pi: IAdvProductInfo;
begin
  inherited;
  if Supports(Component, IAdvProductInfo, pi) then
  begin
    case Index of
      0: TAdvUtils.&Message(Component.ClassName + ' ' + pi.GetVersion + #13#10'© ' + IntToStr(YearOf(Now)) + ' TMS Software', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
      1:
      begin
        if pi.GetDocURL <> '' then
          TAdvUtils.OpenURL(pi.GetDocURL);
      end;
      2:
      begin
        if pi.GetTipsURL <> '' then
          TAdvUtils.OpenURL(pi.GetTipsURL);
      end;
    end;
  end;
end;

function TAdvDefaultEditor.GetVerb(Index: Integer): string;
begin
  inherited;
  case Index of
    0: Result := '&About';
    1: Result := '&Documentation';
    2: Result := '&Tips && FAQ';
  end;
end;

function TAdvDefaultEditor.GetVerbCount: Integer;
var
  pi: IAdvProductInfo;
begin
  Result := 0;
  if Supports(Component, IAdvProductInfo, pi) then
  begin
    Inc(Result);
    if pi.GetDocURL <> '' then
      Inc(Result);

    if pi.GetTipsURL <> '' then
      Inc(Result);
  end;
end;
{$ENDIF}

end.
