{********************************************************************}
{ TDBAdvTrackBar design time support                                 }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2007 - 2016                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit DBAdvTrackBarRegDE;

interface

{$I TMSDEFS.INC}

uses
  AdvTrackBar, GDIPicDE, GDIPicture, Classes, DBAdvTrackBar, AdvTrackBarRegDE,
  DesignIntf, DesignEditors;


procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TDBAdvTrackBar, 'BackGround', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TDBAdvTrackBar, 'BackGroundDisabled', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TDBAdvTrackBar, 'RateActive', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TDBAdvTrackBar, 'RateInActive', TGDIPPictureProperty);
end;

end.

