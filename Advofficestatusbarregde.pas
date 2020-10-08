{*************************************************************************}
{ TOfficeStatusBar component                                              }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2006 - 2016                                       }
{           Email : info@tmssoftware.com                                  }
{           Website : http://www.tmssoftware.com/                         }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvOfficeStatusBarRegDE;

interface

uses
  Classes, DesignIntf, DesignEditors, AdvOfficeStatusBar;

type

  TAdvOfficeStatusBarEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  end;


procedure Register;

implementation

uses
  SysUtils;


{ TAdvOfficeStatusBarEditor }

procedure TAdvOfficeStatusBarEditor.EditProperty(
  const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'PANELS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure Register;
begin
  RegisterComponentEditor(TAdvOfficeStatusBar, TAdvOfficeStatusBarEditor);
end;


end.
