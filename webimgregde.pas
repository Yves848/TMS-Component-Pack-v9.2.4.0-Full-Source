{****************************************************************}
{ TWebImage component                                            }
{ for Delphi & C++Builder                                        }
{                                                                }
{ written by                                                     }
{   TMS Software                                                 }
{   copyright © 2000-2016                                        }
{   Email : info@tmssoftware.com                                 }
{   Web : http://www.tmssoftware.com                             }
{****************************************************************}

unit WebImgRegDE;

interface

{$I TMSDEFS.INC}

uses
  WebImage, WebImgDE, Classes, DesignIntf, DesignEditors;


procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TWebPicture), TWebImage, 'WebPicture', TWebPictureProperty);
end;

end.

