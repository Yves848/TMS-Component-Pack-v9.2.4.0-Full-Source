{********************************************************************}
{ TAdvReflectionImage component                                      }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2007                                                 }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvReflectionImageRegDE;

interface

{$I TMSDEFS.INC}
uses
  AdvReflectionImage, GDIPicDE, Classes, GDIPicture,
  DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvReflectionImage, 'Picture', TGDIPPictureProperty);
end;

end.

