{*************************************************************************}
{ TAdvPanel, TAdvPanelGroup, TAdvPanelStyler component                    }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written                                                                 }
{   TMS Software                                                          }
{   copyright © 2000-2016                                                 }
{ Email : info@tmssoftware.com                                            }
{ Web : http://www.tmssoftware.com                                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The source         }
{ code remains property of the writer and may not be distributed          }
{ freely as such.                                                         }
{*************************************************************************}

{$I TMSDEFS.INC}

unit advpnlde;

interface

uses
  Classes, DesignIntf, DesignEditors;

{$I TMSDEFS.INC}

type
  TAdvPanelEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
  end;

implementation

uses
  SysUtils;

procedure TAdvPanelEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'TEXT') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


end.
