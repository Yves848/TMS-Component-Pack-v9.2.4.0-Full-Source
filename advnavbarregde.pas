{********************************************************************}
{ TAdvNavBarRegDE                                                    }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 1999-2015                                            }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvNavBarRegDE;

{$I TMSDEFS.INC}

interface

uses
  Classes, Controls, AdvNavBar, DesignIntf, DesignEditors, AdvStyles,
  Forms, AdvStyleIF;

type
  TAdvNavBarEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvNavBarPanelEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponentEditor(TAdvNavBar,TAdvNavBarEditor);
  RegisterComponentEditor(TAdvNavBarPanel,TAdvNavBarPanelEditor);
end;


{ TAdvNavBarEditor }

procedure TAdvNavBarEditor.ExecuteVerb(Index: Integer);
var
  aAdvNavBarPanel: TAdvNavBarPanel;
  style: TTMSStyle;
  psf: TAdvStyleForm;
begin
  inherited;
  case Index of
    0:
    begin
      TAdvNavBar(Component).ControlStyle := TAdvNavBar(Component).ControlStyle + [csAcceptsControls];
      aAdvNavBarPanel := TAdvNavBarPanel(Designer.CreateComponent(TAdvNavBarPanel,Component,0,0,100,100));
      aAdvNavBarPanel.Parent := TAdvNavBar(Component);

      aAdvNavBarPanel.AdvNavBar := TAdvNavBar(Component);
      (Component as TAdvNavBar).Invalidate;
      TCustomControl(Component).ControlStyle := TAdvNavBar(Component).ControlStyle - [csAcceptsControls];
      aAdvNavBarPanel.Caption := aAdvNavBarPanel.Name;
    end;
    1:
    begin
      TAdvNavBar(Component).SelectPreviousPanel;
    end;
    2:
    begin
      TAdvNavBar(Component).SelectNextPanel;
    end;
    3:
    begin
      style := (Component as TAdvNavBar).GetComponentStyle;

      psf := TAdvStyleForm.Create(Application);
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
        if (Component is TAdvNavBar) then
           (Component as TAdvNavBar).SetComponentStyle(style);
           Designer.Modified;
      end;
      psf.Free;
    end;
  end;
end;

function TAdvNavBarEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Add panel';
    1: Result := 'Previous Panel';
    2: Result := 'Next Panel';
    3: Result := 'Styles';
  end;
end;

function TAdvNavBarEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

{ TAdvNavBarPanelEditor }

procedure TAdvNavBarPanelEditor.ExecuteVerb(Index: Integer);
var
  aAdvNavBarPanel: TAdvNavBarPanel;
begin
  inherited;
  case Index of
    0:
    begin
      TAdvNavBarPanel(Component).Parent.ControlStyle := TAdvNavBarPanel(Component).Parent.ControlStyle + [csAcceptsControls];
      aAdvNavBarPanel := TAdvNavBarPanel(Designer.CreateComponent(TAdvNavBarPanel,TAdvNavBarPanel(Component).Parent,0,0,100,100));

      aAdvNavBarPanel.AdvNavBar := TAdvNavBar(TAdvNavBarPanel(Component).Parent);
      TAdvNavBar(TAdvNavBarPanel(Component).Parent).Invalidate;
      TAdvNavBarPanel(Component).Parent.ControlStyle := TAdvNavBarPanel(Component).Parent.ControlStyle - [csAcceptsControls];
      aAdvNavBarPanel.Caption := aAdvNavBarPanel.Name;
    end;
    1:
    begin
      TAdvNavBar(TAdvNavBarPanel(Component).Parent).SelectPreviousPanel;
    end;
    2:
    begin
      TAdvNavBar(TAdvNavBarPanel(Component).Parent).SelectNextPanel;
    end;
    3:
    begin
      TAdvNavBarPanel(Component).AdvNavBar:=  nil;
      Component.Free;
    end;
  end;
end;

function TAdvNavBarPanelEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Add panel';
    1: Result := 'Previous Panel';
    2: Result := 'Next Panel';
    3: Result := 'Delete Panel';
  end;
end;

function TAdvNavBarPanelEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

end.
