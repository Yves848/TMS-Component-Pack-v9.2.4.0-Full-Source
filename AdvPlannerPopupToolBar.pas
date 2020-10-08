{*************************************************************************}
{ TMS TAdvGridToolBar Popup                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2015                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvPlannerPopupToolBar;

{$I TMSDEFS.INC}

interface

uses
  Classes, Planner, AdvToolBarPopup, AdvToolBar, Graphics, Controls, Types,
  Grids, RTTI, Windows, AdvStyleIF
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type

  TAdvPlannerPopupToolBarWindow = class(TAdvPopupToolBarWindow)
  private
    FPlanner: TCustomPlanner;
    FPlannerItem: TPlannerItem;
  protected
    function DoFontStyle(AType: TFontStyleType): boolean; override;
    procedure DoFontSize(ASize: integer); override;
    procedure DoFont(AName: string); override;
    procedure DoFontColor(AColor: TColor); override;
    procedure DoColor(AColor: TColor); override;
    function DoAlign(AAlign: TAlignment): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Planner: TCustomPlanner read FPlanner write FPlanner;
    property PlannerItem: TPlannerItem read FPlannerItem write FPlannerItem;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvPlannerPopupToolBar = class(TPlannerPopup)
  private
    FPopupWindow: TAdvPlannerPopupToolBarWindow;
    FToolBarStyler: TCustomAdvToolBarStyler;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show(PT: TPoint); override;
    procedure Hide; override;
    function MouseInPopup(PT: TPoint): boolean; override;
  published
    property ToolBarStyler: TCustomAdvToolBarStyler read FToolBarStyler write FToolBarStyler;
  end;

procedure Register;

implementation

type
  TWinControlEx = class(TWinControl);
  TCustomAdvToolBarStylerEx = class(TCustomAdvToolBarStyler);


procedure Register;
begin
  RegisterComponents('TMS Planner',[TAdvPlannerPopupToolBar]);
end;

{ TAdvGridPopupToolBarWindow }

constructor TAdvPlannerPopupToolBarWindow.Create(AOwner: TComponent);
begin
  Options := [oBold,oItalic,oUnderline,oStrikeThrough,oColor,oFontColor,oAlignLeft,oAlignCenter,oAlignRight];
  inherited;
end;

function TAdvPlannerPopupToolBarWindow.DoAlign(AAlign: TAlignment): boolean;
begin
  PlannerItem.Alignment := AAlign;
  Result := false;

  if PlannerItem.IsEditing then
  begin
    if PlannerItem.InplaceEdit = peMemo then
      Planner.MemoEdit.Alignment := AAlign;
    if PlannerItem.InplaceEdit = peMaskEdit then
      Planner.MaskEdit.Alignment := AAlign;
  end;

end;

procedure TAdvPlannerPopupToolBarWindow.DoColor(AColor: TColor);
begin
  PlannerItem.Color := AColor;
  PlannerItem.ColorTo := AColor;
  PlannerItem.SelectColor := AColor;
  PlannerItem.SelectColorTo := AColor;

  if PlannerItem.IsEditing then
  begin
    if PlannerItem.InplaceEdit = peMemo then
      Planner.MemoEdit.Color := AColor;
    if PlannerItem.InplaceEdit = peMaskEdit then
      Planner.MaskEdit.Color := AColor;
  end;
end;

procedure TAdvPlannerPopupToolBarWindow.DoFont(AName: string);
begin
  PlannerItem.Font.Name := AName;
  PlannerItem.CaptionFont.Name := AName;

  if PlannerItem.IsEditing then
  begin
    if PlannerItem.InplaceEdit = peMemo then
      Planner.MemoEdit.Font.Name := AName;
    if PlannerItem.InplaceEdit = peMaskEdit then
      Planner.MaskEdit.Font.Name := AName;
  end;

end;

procedure TAdvPlannerPopupToolBarWindow.DoFontColor(AColor: TColor);
begin
  PlannerItem.Font.Color := AColor;
  PlannerItem.CaptionFont.Color := AColor;
  PlannerItem.SelectFontColor := AColor;

  if PlannerItem.IsEditing then
  begin
    if PlannerItem.InplaceEdit = peMemo then
      Planner.MemoEdit.Font.Color := AColor;
    if PlannerItem.InplaceEdit = peMaskEdit then
      Planner.MaskEdit.Font.Color := AColor;
  end;
end;

procedure TAdvPlannerPopupToolBarWindow.DoFontSize(ASize: integer);
begin
  PlannerItem.Font.Size := ASize;
  PlannerItem.CaptionFont.Size := ASize;

  if PlannerItem.IsEditing then
  begin
    if PlannerItem.InplaceEdit = peMemo then
      Planner.MemoEdit.Font.Size := ASize;
    if PlannerItem.InplaceEdit = peMaskEdit then
      Planner.MaskEdit.Font.Size := ASize;
  end;
end;

function TAdvPlannerPopupToolBarWindow.DoFontStyle(AType: TFontStyleType): boolean;

  function ToggleStyle(fntstyle: TFontStyle): boolean;
  var
    fs: TFontStyles;

  begin
    fs := PlannerItem.Font.Style;

    if fntstyle in fs then
      fs := fs - [fntstyle]
    else
      fs := fs + [fntstyle];

    PlannerItem.Font.Style := fs;
    Result := fntstyle in fs;

    fs := PlannerItem.CaptionFont.Style;

    if fntstyle in fs then
      fs := fs - [fntstyle]
    else
      fs := fs + [fntstyle];

    PlannerItem.CaptionFont.Style := fs;

    if PlannerItem.IsEditing then
    begin
      if PlannerItem.InplaceEdit = peMemo then
        Planner.MemoEdit.Font.Style := PlannerItem.Font.Style;
      if PlannerItem.InplaceEdit = peMaskEdit then
        Planner.MaskEdit.Font.Style := PlannerItem.Font.Style;
    end;
  end;

begin
  case AType of
  fstBold: Result := ToggleStyle(fsBold);
  fstItalic: Result := ToggleStyle(fsItalic);
  fstUnderline: Result := ToggleStyle(fsUnderline);
  fstStrikeThrough: Result := ToggleStyle(fsStrikeOut)
  else
    Result := false;
  end;
end;

{ TAdvPlannerPopupToolBar }

constructor TAdvPlannerPopupToolBar.Create(AOwner: TComponent);
begin
  inherited;

  FPopupWindow := TAdvPlannerPopupToolBarWindow.Create(Self);
end;

destructor TAdvPlannerPopupToolBar.Destroy;
begin

  inherited;
end;

procedure TAdvPlannerPopupToolBar.Hide;
begin
  inherited;
  FPopupWindow.Visible := false;
  FPopupWindow.Parent := nil;
end;

function TAdvPlannerPopupToolBar.MouseInPopup(PT: TPoint): boolean;
begin
  Result := PtInRect(Rect(FPopupWindow.Left, FPopupWindow.Top, FPopupWindow.Left + FPopupWindow.Width, FPopupWindow.Top + FPopupWindow.Height), PT);
end;

procedure TAdvPlannerPopupToolBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FToolBarStyler) then
    FToolBarStyler := nil;
end;

procedure TAdvPlannerPopupToolBar.Show(PT: TPoint);
var
  fs: TFontStyles;
  clr: TColor;
  i: integer;
  tmsif: ITMSStyle;
begin
  inherited;
  FPopupWindow.Left := PT.X;
  FPopupWindow.Top := PT.Y;
  FPopupWindow.Parent := Planner;
  FPopupWindow.Planner := Planner;
  FPopupWindow.PlannerItem := PlannerItem;
  FPopupWindow.ToolBarStyler := ToolBarStyler;

  if Assigned(ToolBarStyler) then
  begin
    for i := 0 to FPopupWindow.ToolBar.ControlCount - 1 do
    begin
      if (FPopupWindow.ToolBar.Controls[i].GetInterface(ITMSStyle, tmsif)) then
          tmsif.SetComponentStyle(TCustomAdvToolBarStylerEx(ToolBarStyler).TMSStyle);
    end;
  end;

  // initialize to current grid cell state
  fs := PlannerItem.Font.Style;
  FPopupWindow.SetFontStyleState(fstBold, fsBold in fs);
  FPopupWindow.SetFontStyleState(fstItalic, fsItalic in fs);
  FPopupWindow.SetFontStyleState(fstUnderline, fsUnderline in fs);
  FPopupWindow.SetFontStyleState(fstStrikeThrough, fsStrikeOut in fs);

  FPopupWindow.BkColorSelector.Tools[0].BackGroundColor := PlannerItem.Color;
  FPopupWindow.TxtColorSelector.Tools[0].BackGroundColor := PlannerItem.Font.Color;

  clr := PlannerItem.Font.Color;

  if clr = clNone then
    clr := clBlack;

  FPopupWindow.SetFontColor(clr);
  FPopupWindow.SetColor(PlannerItem.Color);
  FPopupWindow.SetFontSize(PlannerItem.Font.Size);
  FPopupWindow.SetFont(PlannerItem.Font.Name);

  FPopupWindow.Visible := true;
  FPopupWindow.Width :=  FPopupWindow.Width + 1;
  FPopupWindow.Width :=  FPopupWindow.Width - 1;

end;

end.
