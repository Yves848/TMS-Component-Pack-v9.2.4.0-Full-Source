{**************************************************************************}
{ TAdvStickyPopupMenuDE DESIGN TIME EDITOR                                 }
{ for Delphi & C++Builder                                                  }
{ version 1.0                                                              }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2007                                              }
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

unit AdvStickyPopupMenuDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvStickyPopupMenu, DesignIntf, DesignEditors;

type
  TAdvStickyPopupMenuEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
  end;


implementation

uses
  SysUtils;
  
procedure TAdvStickyPopupMenuEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'MENUITEMS') = 0) then
    begin
      PropertyEditor.Edit;
      Continue := False;
    end;
end;

end.


