{********************************************************************}
{ TAdvTrackBar design time support                                   }
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

unit AdvTrackBarRegDE;

interface

{$I TMSDEFS.INC}

uses
  AdvTrackBar, GDIPicDE, Classes, GDIPicture,
  DesignIntf, DesignEditors, AdvToolButtonStyles;

type

  TAdvTrackBarEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TAdvRangeSliderEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

procedure Register;

implementation

uses
  Windows, Dialogs, Forms, SysUtils, AdvTrackBarGallery, AdvRangeSliderGallery,
  AdvTrackBarPersist, AdvStyleIF, Controls;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvTrackBar, 'BackGround', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvTrackBar, 'BackGroundDisabled', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvTrackBar, 'RateActive', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvTrackBar, 'RateInActive', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarThumb, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarThumb, 'PictureHot', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarThumb, 'PictureDown', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarThumb, 'PictureDisabled', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarSlider, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarSlider, 'PictureDisabled', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarSlider, 'PictureCompleted', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarSlider, 'PictureCompletedDisabled', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarTick, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarTick, 'PictureDisabled', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarButtons, 'MinPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarButtons, 'MinPictureHot', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarButtons, 'MinPictureDown', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarButtons, 'MaxPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarButtons, 'MaxPictureHot', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTrackBarButtons, 'MaxPictureDown', TGDIPPictureProperty);



  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvRangeSlider, 'BackGround', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvRangeSlider, 'BackGroundDisabled', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TRangeSliderSlider, 'PictureRemaining', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TRangeSliderSlider, 'PictureRemainingDisabled', TGDIPPictureProperty);

  RegisterComponentEditor(TAdvTrackBar, TAdvTrackBarEditor);
  RegisterComponentEditor(TAdvRangeSlider, TAdvRangeSliderEditor);
end;


function AddBackslash(const s: string): string;
begin
  if (Length(s) >= 1) and (s[Length(s)]<>'\') then
    Result := s + '\'
  else
    Result := s;
end;

function WinTempDir: string;
var
  buf:string;
  i: integer;
begin
  SetLength(buf, MAX_PATH);
  i := GetTempPath(Length(buf), PChar(buf));
  SetLength(buf, i);
  Result := AddBackslash(buf);
end;


{ TAdvTrackBarEditor }

procedure TAdvTrackBarEditor.ExecuteVerb(Index: integer);
var
  tbf: TAdvTrackBarGalleryForm;
  pp: TPropertyPersister;
  r: TRect;
  p: TWinControl;
  o: TComponent;
  atb: TAdvTrackbar;
  psf: TAdvToolButtonStyleForm;
  style: TTMSStyle;
  n: string;
begin
  inherited;
  if Index = 0 then
  begin
    pp := TPropertyPersister.Create(nil);
    pp.StorePropertiesToFile(Component,WinTempDir + 'temp.prop');
    pp.Free;

    tbf := TAdvTrackBarGalleryForm.Create(Application);
    tbf.Selection := '';
    if tbf.ShowModal = mrOK then
    begin
      r := (Component as TAdvTrackBar).BoundsRect;
      p := (Component as TAdvTrackBar).Parent;
      o := (Component as TAdvTrackBar).Owner;
      n := (Component as TAdvTrackBar).Name;

      pp := TPropertyPersister.Create(nil);
      if tbf.Selection <> '' then
      begin
        Component.Free;

        atb := TAdvTrackBar(Designer.CreateComponent(TAdvTrackBar,o,r.left, r.Top,r.Right - r.Left,r.Bottom - r.Top));
        atb.Parent := p;
        atb.Name := n;

        pp.ReStorePropertiesToFile(atb,tbf.Selection);
      end;
      pp.Free;
    end;
    tbf.Free;
  end
  else if Index = 1 then
  begin
    style := (Component as TAdvTrackBar).GetComponentStyle;

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
      if (Component is TAdvTrackBar) then
         (Component as TAdvTrackBar).SetComponentStyle(style);
         Designer.Modified;
    end;
    psf.Free;
  end;
end;

function TAdvTrackBarEditor.GetVerb(index: integer): string;
begin
  case Index of
  0: Result := 'Gallery';
  1: Result := 'Styles';
  end;

end;

function TAdvTrackBarEditor.GetVerbCount: integer;
begin
  Result := 2;
end;


{ TAdvRangeSliderEditor }

procedure TAdvRangeSliderEditor.ExecuteVerb(Index: integer);
var
  tbf: TAdvRangeSliderGalleryForm;
  pp: TPropertyPersister;
  r: TRect;
  p: TWinControl;
  o: TComponent;
  atb: TAdvRangeSlider;
  psf: TAdvToolButtonStyleForm;
  style: TTMSStyle;
  n: string;
begin
  inherited;
  case Index of
    0:
    begin
      pp := TPropertyPersister.Create(nil);

      pp.IgnoreSubProperties.Add('ColorRemaining');
      pp.IgnoreSubProperties.Add('ColorRemainingTo');
      pp.IgnoreSubProperties.Add('ColorRemainingDisabled');
      pp.IgnoreSubProperties.Add('ColorRemainingDisabledTo');
      pp.IgnoreSubProperties.Add('PictureRemaining');
      pp.IgnoreSubProperties.Add('PictureRemainingDisabled');

      pp.StorePropertiesToFile(Component,WinTempDir + 'temp.prop');
      pp.Free;

      tbf := TAdvRangeSliderGalleryForm.Create(Application);
      tbf.Selection := '';
      if tbf.ShowModal = mrOK then
      begin
        r := (Component as TAdvRangeSlider).BoundsRect;
        p := (Component as TAdvRangeSlider).Parent;
        o := (Component as TAdvRangeSlider).Owner;
        n := (Component as TAdvRangeSlider).Name;

        pp := TPropertyPersister.Create(nil);

        pp.IgnoreSubProperties.Add('ColorRemaining');
        pp.IgnoreSubProperties.Add('ColorRemainingTo');
        pp.IgnoreSubProperties.Add('ColorRemainingDisabled');
        pp.IgnoreSubProperties.Add('ColorRemainingDisabledTo');
        pp.IgnoreSubProperties.Add('PictureRemaining');
        pp.IgnoreSubProperties.Add('PictureRemainingDisabled');

      
        if tbf.Selection <> '' then
        begin
          Component.Free;

          atb := TAdvRangeSlider(Designer.CreateComponent(TAdvRangeSlider,o,r.left, r.Top,r.Right - r.Left,r.Bottom - r.Top));
          atb.Parent := p;
          atb.Name := n;

          pp.ReStorePropertiesToFile(atb,tbf.Selection);
        end;
        pp.Free;
      end;
      tbf.Free;
    end;
    1:
    begin
      style := (Component as TAdvRangeSlider).GetComponentStyle;

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
        if (Component is TAdvRangeSlider) then
           (Component as TAdvRangeSlider).SetComponentStyle(style);
           Designer.Modified;
      end;
      psf.Free;
    end;
  end;
end;

function TAdvRangeSliderEditor.GetVerb(index: integer): string;
begin
  case Index of
  0: Result := 'Gallery';
  1: Result := 'Styles';
  end;

end;

function TAdvRangeSliderEditor.GetVerbCount: integer;
begin
  Result := 2;
end;

end.

