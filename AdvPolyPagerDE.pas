unit AdvPolyPagerDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, Graphics, Comctrls, Windows, Forms, TypInfo, Dialogs, ExtCtrls,
  Controls, ExtDlgs, AdvPolyPager, DesignIntf, DesignEditors, ContNrs;

type

  TAdvPolyPagerEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure EditProperty(const PropertyEditor: IProperty; var Continue:
      Boolean); override;
  end;

  TAdvPolyPageEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  SysUtils, AdvStyles, AdvStyleIF,GDIPCustomItem,GDIPImageTextItem,GDIPWedgeItem,GDIPBase,GDIPTextItem;

const
  ImageTextItem_16x16 = 4;
  ImageTextItem_24x24 = 5;
  WedgeItem_16x16 = 6;
  WedgeItem_24x24 = 7;
  TextItem = 8;

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

procedure TAdvPolyPagerEditor.EditProperty(const PropertyEditor:
  IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'LIST') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TAdvPolyPagerEditor.ExecuteVerb(Index: Integer);
var
  AdvPage : TAdvPolyPage;
  AdvPolyPager:TAdvPolyPager;
  psf: TAdvStyleForm;
  style: TTMSStyle;
  s:string;
  Item: TCustomItem;

begin
  inherited;
  if (Index = 0) then
  begin
    style := (Component as TAdvPolyPager).GetComponentStyle;

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
        if (Component is TAdvPolyPager) then
           (Component as TAdvPolyPager).SetComponentStyle(style);
           Designer.Modified;
    end;
    psf.Free;
  end;
  case Index of
  1:
    begin
      AdvPage := TAdvPolyPage(Designer.CreateComponent(TAdvPolyPage,Component,23,0,100,100));
      AdvPage.Parent := TAdvPolyPager(Component);
      AdvPage.AdvPolyPager := TAdvPolyPager(Component);
      AdvPage.Caption := AdvPage.name;
      TAdvPolypager(component).ActivePage:= AdvPage;
    end;
  2: TAdvPolyPager(Component).SelectNextPage(false);
  3: TAdvPolyPager(Component).SelectNextPage(True);
  ImageTextItem_16x16..TextItem:
    begin
       AdvPolyPager := TAdvPolyPager(Component);
       s := InputBox('Caption:','Value:','New Page');
       AdvPage := TAdvPolyPage(Designer.CreateComponent(TAdvPolyPage,Component,23,0,100,100));
       advpage.Name := Designer.UniqueName(StringReplace('AdvPage_'+s,' ', '', [rfReplaceAll]));
       AdvPage.Parent := AdvPolyPager;
       AdvPage.AdvPolyPager := AdvPolyPager;
       AdvPage.Caption := s;
       AdvPolyPager.ActivePage:= AdvPage;
       Item := nil;

       if index in [ImageTextItem_16x16, ImageTextItem_24x24] then
       begin
         item := TCustomItem(Designer.CreateComponent(TImageTextItem,Component,23,0,10,10));
         Item.ItemOwner := AdvPolyPager.List.GetOwnerComponent;
         Item.Name := Designer.UniqueName(StringReplace('ImageTextItem_'+s, ' ', '', [rfReplaceAll]));
         TImageTextItem(Item).Caption := s;
         if index = ImageTextItem_16x16  then
         begin
           TImageTextItem(Item).ImageHeight := 16;
           TImageTextItem(Item).ImageWidth  := 16;
         end;

         if index = ImageTextItem_24x24  then
         begin
           TImageTextItem(Item).ImageHeight := 24;
           TImageTextItem(Item).ImageWidth  := 24;
         end;
       end;

       if index in [WedgeItem_16x16, WedgeItem_24x24] then
       begin
         item := TCustomItem(Designer.CreateComponent(TWedgeItem,Component,23,0,10,10));
         item.ItemOwner := AdvPolyPager.List.GetOwnerComponent;
         Item.Name := Designer.UniqueName(StringReplace('WedgeItem_'+s, ' ', '', [rfReplaceAll]));
         TWedgeItem(Item).Caption := s;
         if index = WedgeItem_16x16  then
         begin
           TWedgeItem(Item).ImageHeight := 16;
           TWedgeItem(Item).ImageWidth  := 16;
         end;
         if index = WedgeItem_24x24  then
         begin
           TWedgeItem(Item).ImageHeight := 24;
           TWedgeItem(Item).ImageWidth  := 24;
         end;
       end;

       if index in [TextItem] then
       begin
         item := TCustomItem(Designer.CreateComponent(TTextItem,Component,23,0,10,10));
         item.ItemOwner := AdvPolyPager.List.GetOwnerComponent;
         Item.Name := Designer.UniqueName(StringReplace('TextItem_'+s, ' ', '', [rfReplaceAll]));
         TTextItem(Item).Caption := s;
       end;

       if Assigned(Item) and Assigned(Item.Control) then
       begin
         if item.ItemOwner is TWinControl then
           item.Control.Parent := TWinControl(item.ItemOwner);
       end;

       AdvPolyPager.List.AssignEvents(item);
       AdvPolyPager.List.AddItem(item);

       AdvPage.ItemLink := Item;
       Designer.SelectComponent(item);
       Designer.Modified;
    end;
  end;
end;

function TAdvPolyPagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Styles';
    1: Result := 'New Page';
    2: Result := 'Previous Page';
    3: Result := 'Next Page';
    ImageTextItem_16x16: result := 'New Page && ImageTextItem (16x16)';
    ImageTextItem_24x24: result := 'New Page && ImageTextItem (24x24)';
    WedgeItem_16x16    : result := 'New Page && WedgeItem (16x16)';
    WedgeItem_24x24    : result := 'New Page && WedgeItem (24x24)';
    TextItem           : result := 'New Page && TextItem';
  end;
end;

function TAdvPolyPagerEditor.GetVerbCount: Integer;
begin
  Result := TextItem+1;
end;

{ TAdvPageEditor }

procedure TAdvPolyPageEditor.ExecuteVerb(Index: Integer);
var
  AdvPage : TAdvPolyPage;
  psf: TAdvStyleForm;
  style: TTMSStyle;
begin
  inherited;
  if (Index = 0) then
  begin
    style := (Component as TAdvPolyPage).GetComponentStyle;

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
      if (Component is TAdvPolyPage) then
         (Component as TAdvPolyPage).SetComponentStyle(style);
         Designer.Modified;
    end;
    psf.Free;
  end;

  case Index of
  1:
    begin
      AdvPage := TAdvPolyPage(Designer.CreateComponent(TAdvPolyPage,TWinControl(Component).Parent,23,0,100,100));
      AdvPage.Parent := TWinControl(Component).Parent;
      AdvPage.AdvPolyPager := TAdvPolyPager(TWinControl(Component).Parent);
      AdvPage.Caption := AdvPage.Name;
      TAdvPolyPager(TWinControl(Component).Parent).ActivePage:= AdvPage;
    end;
  2: TAdvPolyPager(TAdvPolyPage(Component).Parent).SelectNextPage(false);
  3: TAdvPolyPager(TAdvPolyPage(Component).Parent).SelectNextPage(true);
  4:
    begin
    TAdvPolyPage(Component).AdvPolyPager := nil;
    Component.Free;
    end;
  end;
end;

function TAdvPolyPageEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'Styles';
  1: Result := 'New Page';
  2: Result := 'Previous Page';
  3: Result := 'Next Page';
  4: Result := 'Delete Page';
  end;
end;

function TAdvPolyPageEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;



end.
