{********************************************************************}
{ TAdvOfficeSelector components                                      }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2006 - 2014                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvOfficeSelectorsRegDE;

interface

{$I TMSDEFS.INC}

uses
  AdvOfficeSelectors, Classes, DesignIntf, DesignEditors;

type

  TAdvOfficeScrollSelectorEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


procedure Register;

implementation


uses
  SysUtils, GDIPicture, GDIPicDE, AdvStyleIF, AdvToolButtonStyles, Forms, Controls;


{ TAdvOfficeScrollSelectorEditor }

procedure TAdvOfficeScrollSelectorEditor.ExecuteVerb(Index: Integer);
var
  style: TTMSStyle;
  psf: TAdvToolButtonStyleForm;
begin
  inherited;
  style := (Component as TAdvCustomOfficeScrollSelector).GetComponentStyle;

  psf := TAdvToolButtonStyleForm.Create(Application);
  case style of
    tsOffice2003Blue: psf.RadioGroup1.ItemIndex := 0;
    tsOffice2003Olive: psf.RadioGroup1.ItemIndex := 1;
    tsOffice2003Silver: psf.RadioGroup1.ItemIndex := 2;
    tsOffice2003Classic: psf.RadioGroup1.ItemIndex := 3;
    tsOffice2007Luna: psf.RadioGroup1.ItemIndex := 4;
    tsOffice2007Obsidian: psf.RadioGroup1.ItemIndex := 5;
    tsOffice2007Silver: psf.RadioGroup1.ItemIndex := 6;
    tsOffice2010Blue: psf.RadioGroup1.ItemIndex := 7;
    tsOffice2010Silver: psf.RadioGroup1.ItemIndex := 8;
    tsOffice2010Black: psf.RadioGroup1.ItemIndex := 9;
    tsWindowsXP: psf.RadioGroup1.ItemIndex := 10;
    tsWindowsVista: psf.RadioGroup1.ItemIndex := 11;
    tsWindows7: psf.RadioGroup1.ItemIndex := 12;
    tsTerminal: psf.RadioGroup1.ItemIndex := 13;
    tsWindows8: psf.RadioGroup1.ItemIndex := 14;
    tsOffice2013White: psf.RadioGroup1.ItemIndex := 15;
    tsOffice2013LightGray: psf.RadioGroup1.ItemIndex := 16;
    tsOffice2013Gray: psf.RadioGroup1.ItemIndex := 17;
    tsWindows10: psf.RadioGroup1.ItemIndex := 18;
    tsOffice2016White: psf.RadioGroup1.ItemIndex := 19;
    tsOffice2016Gray: psf.RadioGroup1.ItemIndex := 20;
    tsOffice2016Black: psf.RadioGroup1.ItemIndex := 21;
  end;

  if psf.ShowModal = mrOK then
  begin
    case psf.RadioGroup1.ItemIndex of
      0: style := tsOffice2003Blue;
      1: style := tsOffice2003Olive;
      2: style := tsOffice2003Silver;
      3: style := tsOffice2003Classic;
      4: style := tsOffice2007Luna;
      5: style := tsOffice2007Obsidian;
      6: style := tsOffice2007Silver;
      7: style := tsOffice2010Blue;
      8: style := tsOffice2010Silver;
      9: style := tsOffice2010Black;
      10: style := tsWindowsXP;
      11: style := tsWindowsVista;
      12: style := tsWindows7;
      13: style := tsTerminal;
      14: style := tsWindows8;
      15: style := tsOffice2013White;
      16: style := tsOffice2013LightGray;
      17: style := tsOffice2013Gray;
      18: style := tsWindows10;
      19: style := tsOffice2016White;
      20: style := tsOffice2016Gray;
      21: style := tsOffice2016Black;
    end;
    if (Component is TAdvCustomOfficeScrollSelector) then
       (Component as TAdvCustomOfficeScrollSelector).SetComponentStyle(style);
       Designer.Modified;
  end;
  psf.Free;
end;

function TAdvOfficeScrollSelectorEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Styles';
  end;
end;

function TAdvOfficeScrollSelectorEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficePenStyleSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficePenStyleSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeBrushStyleSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeBrushStyleSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeShadowSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeShadowSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTableBorderSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTableBorderSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeGradientDirectionSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeGradientDirectionSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficePenWidthSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficePenWidthSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeColorSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeColorSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTextColorSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTextColorSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeToolSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeToolSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTableSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTableSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeCharacterSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeCharacterSelector, 'DisabledPicture', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvScrollSelectorItem, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvSelectorItem, 'Picture', TGDIPPictureProperty);

  RegisterComponentEditor(TAdvCustomOfficeScrollSelector,TAdvOfficeScrollSelectorEditor);
end;

end.

