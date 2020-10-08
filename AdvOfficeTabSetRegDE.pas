{***************************************************************************}
{ TAdvOfficeTabSet component                                                }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2007 - 2016                                        }
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

unit AdvOfficeTabSetRegDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvOfficeTabSet, GDIPicture, GDIPicDE,
  DesignIntf, DesignEditors;

type

  TAdvOfficeTabSetEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  end;


procedure Register;

implementation

uses
  SysUtils;

{ TAdvOfficeTabSetEditor }

procedure TAdvOfficeTabSetEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ADVOFFICETABS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


procedure Register;
begin
  // Setting property Editor to all properties of type TGDIPPicture
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTabSetButtonSettings, '', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TOfficeTabCollectionItem, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TOfficeTabCollectionItem, 'DisabledPicture', TGDIPPictureProperty);
  RegisterComponentEditor(TAdvOfficeTabSet, TAdvOfficeTabSetEditor);
end;


end.

