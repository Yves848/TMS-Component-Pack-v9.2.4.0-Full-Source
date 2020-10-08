{*************************************************************************}
{ TMS AdvOutlookList component                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2005 - 2016                                       }
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

{$I TMSDEFS.INC}

unit AdvOutLookListDE;

interface

uses
  Classes, Controls, AdvOutLookList, DesignIntf, DesignEditors;

type
  TAdvOutLookListEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TAdvOutlookListSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;


procedure Register;

implementation

uses
  SysUtils;

procedure Register;
begin
  RegisterComponentEditor(TAdvOutLookList, TAdvOutLookListEditor);
  RegisterSelectionEditor(TAdvOutLookList, TAdvOutlookListSelectionEditor);
end;

procedure TAdvOutLookListEditor.EditProperty(const PropertyEditor:IProperty; var Continue:Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'COLUMNS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TAdvOutLookListEditor.ExecuteVerb(Index: integer);
begin
  case Index of
  0: Edit;
  end;
end;

function TAdvOutLookListEditor.GetVerb(index: integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'Columns Editor';
  end;
end;

function TAdvOutLookListEditor.GetVerbCount: integer;
begin
 Result := 1;
end;

{ TAdvOutlookListSelectionEditor }

procedure TAdvOutlookListSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('OutlookGroupedList');
end;

end.