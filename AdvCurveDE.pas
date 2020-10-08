{**************************************************************************}
{ AdvCurveDE Design Time Editor                                            }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2013                                              }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvCurveDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvCurve, AdvCurveEditor, DesignIntf, DesignEditors;

type
  TCurveEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
  end;

  TCurvePointsProperty = class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;


implementation

uses
  SysUtils;

procedure TCurvePointsProperty.Edit;
var
  CurveEditor: TAdvCurveEditorDialog;
  CurveComp: TAdvCurve;
begin
  CurveEditor := TAdvCurveEditorDialog.Create(nil);
  CurveComp := TAdvCurve(GetComponent(0));
  CurveEditor.CurveComponent := CurveComp;
  try
    CurveEditor.Execute;
  finally
    Modified;
    CurveEditor.Free;
  end;
end;

function TCurvePointsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TCurveEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'POINTS') = 0) then
    begin
      PropertyEditor.Edit;
      Continue := False;
    end;
end;



end.


