{********************************************************************}
{ TPictureContainer component                                        }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2001 - 2016                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit PictureContainerRegDe;

interface

{$I TMSDEFS.INC}

uses
  PictureContainer, Classes, PictureContainerDE, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TPictureCollection), TPictureContainer, 'Items', TPictureContainerProperty);
  RegisterComponentEditor(TPictureContainer, TPictureContainerDefaultEditor);
end;

end.

