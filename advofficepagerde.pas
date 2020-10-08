unit AdvOfficePagerDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, Graphics, Comctrls, Windows, Forms, TypInfo, Dialogs, ExtCtrls,
  Controls, ExtDlgs, AdvOfficePager, DesignIntf, DesignEditors, ContNrs;

type

  TAdvOfficePagerEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvOfficePageEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  SysUtils,
  VDBConsts;

function HTMLToRgb(color: tcolor): tcolor;
var
  r,g,b: integer;
begin
  r := (Color and $0000FF);
  g := (Color and $00FF00);
  b := (Color and $FF0000) shr 16;
  Result := b or g or (r shl 16);
end;

function BrightnessColor(Col: TColor; Brightness: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := Longint(ColorToRGB(Col));
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  r1 := Round( (100 + Brightness)/100 * r1 );
  g1 := Round( (100 + Brightness)/100 * g1 );
  b1 := Round( (100 + Brightness)/100 * b1 );

  Result := RGB(r1,g1,b1);
end;


function BrightnessColor(Col: TColor; BR,BG,BB: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := Longint(ColorToRGB(Col));
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  r1 := Round( (100 + BR)/100 * r1 );
  g1 := Round( (100 + BG)/100 * g1 );
  b1 := Round( (100 + BB)/100 * b1 );

  Result := RGB(r1,g1,b1);
end;


{ TAdvToolBarPagerEditor }

procedure TAdvOfficePagerEditor.ExecuteVerb(Index: Integer);
var
  AdvPage : TAdvOfficePage;
begin
  inherited;

  case Index of
  0:
    begin
      //TAdvToolBarPager(Component).ControlStyle := TAdvToolBarPager(Component).ControlStyle + [csAcceptsControls];
      AdvPage := TAdvOfficePage(Designer.CreateComponent(TAdvOfficePage,Component,23,0,100,100));
      AdvPage.Parent := TAdvOfficePager(Component);
      AdvPage.AdvOfficePager := TAdvOfficePager(Component);
      AdvPage.Caption := AdvPage.name;
      TAdvOfficepager(component).ActivePage:= AdvPage;
      //TAdvToolBarPager(Component).Update;
      //TAdvToolBarPager(Component).Invalidate;
      //TAdvToolBarPager(Component).ControlStyle := TAdvToolBarPager(Component).ControlStyle - [csAcceptsControls];
    end;
  1: TAdvOfficePager(Component).SelectNextPage(false);
  2: TAdvOfficePager(Component).SelectNextPage(True);
  end;
end;

function TAdvOfficePagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New Page';
  1: Result := 'Previous Page';
  2: Result := 'Next Page';
  end;
end;

function TAdvOfficePagerEditor.GetVerbCount: Integer;
begin
  if (Component.ClassName <> 'TAdvMultiFileMemo') then
    Result := 3
  else
    Result := 0;
end;

{ TAdvPageEditor }

procedure TAdvOfficePageEditor.ExecuteVerb(Index: Integer);
var
  AdvPage : TAdvOfficePage;
begin
  inherited;
  case Index of
  0:
    begin
      AdvPage := TAdvOfficePage(Designer.CreateComponent(TAdvOfficePage,TWinControl(Component).Parent,23,0,100,100));
      AdvPage.Parent := TWinControl(Component).Parent;
      AdvPage.AdvOfficePager := TAdvOfficePager(TWinControl(Component).Parent);
      AdvPage.Caption := AdvPage.Name;
      TAdvOfficePager(TWinControl(Component).Parent).ActivePage:= AdvPage;
    end;
  1: TAdvOfficePager(TAdvOfficePage(Component).Parent).SelectNextPage(false);
  2: TAdvOfficePager(TAdvOfficePage(Component).Parent).SelectNextPage(true);
  3:
    begin
      TAdvOfficePage(Component).AdvOfficePager := nil;
      Component.Free;
    end;
  4:
    begin
      AdvPage := TAdvOfficePage(Designer.CreateComponent(TAdvOfficePage,TWinControl(Component).Parent,23,0,100,100));
      AdvPage.Parent := TWinControl(Component).Parent;
      AdvPage.AdvOfficePager := TAdvOfficePager(TWinControl(Component).Parent);
      AdvPage.Assign(TAdvOfficePage(Component));
    end;
  end;
end;

function TAdvOfficePageEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New Page';
  1: Result := 'Previous Page';
  2: Result := 'Next Page';
  3: Result := 'Delete Page';
  4: Result := 'Clone page';
  end;
end;

function TAdvOfficePageEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;



end.
