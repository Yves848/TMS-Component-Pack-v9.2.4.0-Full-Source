{********************************************************************}
{ TAdvEditBtn component                                              }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2007 - 2017                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit advedbtnregde;

interface

{$I TMSDEFS.INC}

uses
  AdvEdBtn, GDIPicDE, Classes, GDIPicture, DesignIntf, DesignEditors;


procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvEditBtn, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvEditBtn, 'PictureHot', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvEditBtn, 'PictureDown', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvEditBtn, 'PictureDisabled', TGDIPPictureProperty);
end;

end.
