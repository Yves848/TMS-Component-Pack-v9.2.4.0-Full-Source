{*************************************************************************}
{ TMS TAdvSearchEdit                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2016 - 2019                                       }
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

unit AdvSearchEdit;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Types, Forms, Controls, StdCtrls, Messages, Graphics,
  AdvGlowButton, AdvSearchList, AdvDropDown, SysUtils, Dialogs, Menus, GDIPicture,
  AdvStyleIF, ImgList;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 9; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : Improved : Control border drawing
  // v1.0.2.0 : New : Exposed TabOrder property
  //          : New : TAdvSearchEdit.SearchList exposed
  //          : New : FocusColor, FocusFontColor, FocusBorder, FocusBorderColor properties added
  //          : New : DisabledColor property added
  // v1.0.3.0 : New : Property ItemIndex added at TAdvSearchEdit level
  // v1.0.3.1 : Fixed : Issue with Appearance.SelectionColor / Appearance.SelectionTextColor when no VCL styles are used
  //          : Fixed : Issue with handling Shift,Ctrl key when dropdown is displayed
  // v1.0.4.0 : New : Exposed Images property to set an imagelist
  // v1.0.4.1 : Fixed : Initialization issue with EmptyText
  // v1.0.4.2 : Improved : Lookup handling & ItemIndex handling
  // v1.0.4.3 : Fixed : Issue with footer & header button images
  // v1.0.4.4 : Fixed : Issue with focus when setting Form.ActiveControl
  // v1.0.5.0 : New : Exposed Font property
  // v1.0.6.0 : New : Exposed public property Edit to access embedded search edit control directly
  // v1.0.6.1 : Improved : Changed sequence to update ItemIndex to get the index correct from the OnChange event
  // v1.0.7.0 : New : Event OnSelect added
  // v1.0.8.0 : New : DropDownHeight property exposed for TAdvSearchEdit
  // v1.0.9.0 : New : OnFiltered event added
  // v1.0.9.1 : Fixed : Issue with setting ItemIndex programmatically
  // v1.0.9.2 : Fixed : Issue in combination with FastMM4

type

  TDropDownSelectEvent = procedure(Sender: TObject; var NewValue: string) of object;

  TAdvSearchDropDown = class(TAdvCustomDropDown)
  private
    FOldText: string;
    FItemIndex: integer;
    FSearchList: TAdvSearchList;
    FOnSelect: TDropDownSelectEvent;
    FKeyDropDown: boolean;
    FOnFiltered: TNotifyEvent;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
  protected
    procedure CreateDropDownForm; override;
    procedure BeforeDropDown; override;
    procedure UpdateIndex;
  protected
    procedure OnDropDownControlKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyPress(var Key: Char); override;
    procedure OnDropDownControlClick(Sender: TObject);
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function CreateSearchList: TAdvSearchList; virtual;
    procedure DoShowDropDown; override;
    property ItemIndex: integer read FItemIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SearchList: TAdvSearchList read FSearchList;
    property OnSelect: TDropDownSelectEvent read FOnSelect write FOnSelect;
    property OnFiltered: TNotifyEvent read FOnFiltered write FOnFiltered;
    property DropDownHeight;
  end;

  TGetButtonAppearance = procedure(Sender: TObject; var Appearance: TGlowButtonAppearance) of object;
  TSetButtonAppearance = procedure(Sender: TObject; Appearance: TGlowButtonAppearance) of object;

  TSearchEditButton = class(TPersistent)
  private
    FWidth: integer;
    FVisible: boolean;
    FCaption: string;
    FOnChange: TNotifyEvent;
    FPicture: TGDIPPicture;
    FOnGetAppearance: TGetButtonAppearance;
    FOnSetAppearance: TSetButtonAppearance;
    FBorderStyle: TBorderStyle;
    FAppearance: TGlowButtonAppearance;
    procedure SetCaption(const Value: string);
    procedure SetVisible(const Value: boolean);
    procedure SetWidth(const Value: integer);
    procedure SetPicture(const Value: TGDIPPicture);
    function GetAppearance: TGlowButtonAppearance;
    procedure SetAppearance(const Value: TGlowButtonAppearance);
    procedure SetBorderStyle(const Value: TBorderStyle);
  protected
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetAppearance: TGetButtonAppearance read FOnGetAppearance write FOnGetAppearance;
    property OnSetAppearance: TSetButtonAppearance read FOnSetAppearance write FOnSetAppearance;
  published
    property Appearance: TGlowButtonAppearance read GetAppearance write SetAppearance;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property Caption: string read FCaption write SetCaption;
    property Picture: TGDIPPicture read FPicture write SetPicture;
    property Width: integer read FWidth write SetWidth default 24;
    property Visible: boolean read FVisible write SetVisible default true;
  end;

  TPopupMenuType = (pmCheck, pmRadio);

  TSearchEditPopupButton = class(TSearchEditButton)
  private
    FPopupType: TPopupMenuType;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property PopupType: TPopupMenuType read FPopupType write FPopupType default pmCheck;
  end;

  TCategoryItemClick = procedure(Sender: TObject; CategoryIndex: integer; isChecked: boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSearchEdit = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FEdit: TAdvSearchDropDown;
    FCatBtn: TAdvGlowButton;
    FSearchBtn: TAdvGlowButton;
    FCatPopup: TPopupMenu;
    FCategoryButton: TSearchEditPopupButton;
    FSearchButton: TSearchEditButton;
    FOnDropDownHeaderButtonClick: TDropDownButtonItemClick;
    FOnBeforeDropDown: TNotifyEvent;
    FOnGetHeaderText: TGetTextEvent;
    FOnDrawFooter: TDrawBackGroundEvent;
    FOnBeforeDropUp: TNotifyEvent;
    FOnDropDownFooterButtonClick: TDropDownButtonItemClick;
    FOnGetFooterText: TGetTextEvent;
    FOnDropDown: TDropDown;
    FOnDrawHeader: TDrawBackGroundEvent;
    FOnDropUP: TDropUP;
    FOnCategoryPopupClick: TCategoryItemClick;
    FOnSearchButtonClick: TNotifyEvent;
    FBorderStyle: TBorderStyle;
    FBorderColor: TColor;
    FUseVCLStyles: boolean;
    FOnChange: TNotifyEvent;
    FFocusBorder: boolean;
    FFocusBorderColor: TColor;
    FDisabledColor: TColor;
    FNormalColor: TColor;
    FNormalFontColor: TColor;
    FFocusFontColor: TColor;
    FFocusColor: TColor;
    FItemIndex: integer;
    FOnSelect: TNotifyEvent;
    FOnFiltered: TNotifyEvent;
    FDropDownHeight: integer;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    function GetCategories: TCategoryList;
    procedure SetCategories(const Value: TCategoryList);
    function GetAppearance: TAdvSearchListAppearance;
    procedure SetAppearance(const Value: TAdvSearchListAppearance);
    function GetColumns: TColumnItems;
    procedure SetColumns(const Value: TColumnItems);
    function GetItems: TSearchList;
    procedure SetItems(const Value: TSearchList);
    function GetItemHeight: Integer;
    procedure SetItemHeight(const Value: Integer);
    function GetFilterCondition: TFilterCondition;
    procedure SetFilterCondition(const Value: TFilterCondition);
    function GetDropDownFooter: TFooterAppearance;
    function GetDropDownHeader: THeaderAppearance;
    procedure SetDropDownFooter(const Value: TFooterAppearance);
    procedure SetDropDownHeader(const Value: THeaderAppearance);
    function GetDropDownShadow: boolean;
    function GetDropDownSizeable: boolean;
    procedure SetDropDownShadow(const Value: boolean);
    procedure SetDropDownSizeable(const Value: boolean);
    function GetDropDownWidth: integer;
    procedure SetDropDownWidth(const Value: integer);
    procedure SetCategoryButton(const Value: TSearchEditPopupButton);
    procedure SetSearchButton(const Value: TSearchEditButton);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetBorderColor(const Value: TColor);
    procedure SetOnDrawFooter(const Value: TDrawBackGroundEvent);
    procedure SetOnDrawHeader(const Value: TDrawBackGroundEvent);
    function GetEmptyText: string;
    function GetEmptyTextFocused: boolean;
    function GetEmptyTextStyle: TFontStyles;
    procedure SetEmptyText(const {%H-}Value{%H+}: string);
    procedure SetEmptyTextFocused(const Value: boolean);
    procedure SetEmptyTextStyle(const Value: TFontStyles);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    function GetAutoSelect: boolean;
    procedure SetAutoSelect(const Value: boolean);
    function GetSearchList: TAdvSearchList;
    procedure SetDisabledColor(const Value: TColor);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    function GetDropDownHeight: integer;
    procedure SetDropDownHeight(const Value: integer);
  protected
    function GetDocURL: string;
    function CreateSearchDropDown: TAdvSearchDropDown; virtual;
    procedure DrawBorders; virtual;
    procedure DrawCategoryButton(Sender: TObject; Canvas: TCanvas; Rect: TRect; State: TGlowButtonState);
    procedure CatBtnClick(Sender: TObject);
    procedure CatBtnGetAppearance(Sender: TObject; var Appearance: TGlowButtonAppearance);
    procedure CatBtnSetAppearance(Sender: TObject; Appearance: TGlowButtonAppearance);
    procedure SearchBtnGetAppearance(Sender: TObject; var Appearance: TGlowButtonAppearance);
    procedure SearchBtnSetAppearance(Sender: TObject; Appearance: TGlowButtonAppearance);
    procedure DoCatMenuItemClick(Sender: TObject);
    procedure DoCategoryButtonChanged(Sender: TObject);
    procedure DoSearchButtonChanged(Sender: TObject);
    procedure DoCategoryPopupClick(CategoryIndex: integer; isChecked: boolean); virtual;
    procedure DoSearchButtonClick(Sender: TObject); virtual;
    procedure DoDropDownHeaderButtonClick(Sender: TObject; ButtonIndex: Integer);
    procedure DoDropDownFooterButtonClick( Sender: TObject; ButtonIndex: Integer);
    procedure DoDrawHeader(Sender: TObject; ACanvas: TCanvas; ARect: TRect);
    procedure DoDrawFooter(Sender: TObject; ACanvas: TCanvas; ARect: TRect);
    procedure DoGetHeaderText(Sender: TObject; var Text: string);
    procedure DoGetFooterText(Sender: TObject; var Text: string);
    procedure DoBeforeDropDown(Sender: TObject);
    procedure DoEditChanged(Sender: TObject); virtual;
    procedure DoSelectValue(Sender: TObject; var Value: string); virtual;
    procedure DoFiltered(Sender: TObject); virtual;
    procedure DoDropDown(Sender: TObject; var AcceptDrop: Boolean);
    procedure DoDropUp(Sender: TObject; Cancelled: Boolean);
    procedure DoBeforeDropUp(Sender: TObject);

    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoKeyPress(Sender: TObject; var key:char); virtual;
    procedure HandleEnter(Sender: TObject); virtual;
    procedure HandleExit(Sender: TObject); virtual;

    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    property UseVCLStyles: boolean read FUseVCLStyles write FUseVCLStyles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle); virtual;
    procedure SetColorTones(ATones: TColorTones); virtual;
    procedure SetFocus; override;
    procedure Init;
    function GetComponentStyle: TTMSStyle;
    property SearchList: TAdvSearchList read GetSearchList;
    procedure UpdateFilter; virtual;
    procedure LoadStrings(Value: TStrings);
    procedure SelectAll;
    property Edit: TAdvSearchDropDown read FEdit;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
  published
    property Align;
    property Anchors;
    property Appearance: TAdvSearchListAppearance read GetAppearance write SetAppearance;
    property AutoSelect: boolean read GetAutoSelect write SetAutoSelect default true;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Categories: TCategoryList read GetCategories write SetCategories;
    property CategoryButton: TSearchEditPopupButton read FCategoryButton write SetCategoryButton;
    property Columns: TColumnItems read GetColumns write SetColumns;
    property Constraints;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clSilver;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownHeader: THeaderAppearance read GetDropDownHeader write SetDropDownHeader;
    property DropDownHeight: integer read GetDropDownHeight write SetDropDownHeight;
    property DropDownFooter: TFooterAppearance read GetDropDownFooter write SetDropDownFooter;
    property DropDownShadow: boolean read GetDropDownShadow write SetDropDownShadow;
    property DropDownSizable: boolean read GetDropDownSizeable write SetDropDownSizeable;
    property DropDownWidth: integer read GetDropDownWidth write SetDropDownWidth;

    property EmptyText: string read GetEmptyText write SetEmptyText;
    property EmptyTextFocused: boolean read GetEmptyTextFocused write SetEmptyTextFocused;
    property EmptyTextStyle: TFontStyles read GetEmptyTextStyle write SetEmptyTextStyle;

    property FilterCondition: TFilterCondition read GetFilterCondition write SetFilterCondition;
    property FocusBorder: boolean read FFocusBorder write FFocusBorder default False;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property FocusFontColor: TColor read FFocusFontColor write FFocusFontColor default clNone;
    property Font;
    property Images: TCustomImageList read GetImages write SetImages;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property Items: TSearchList read GetItems write SetItems;
    property PopupMenu;
    property SearchButton: TSearchEditButton read FSearchButton write SetSearchButton;
    property ShowHint;
    property TabOrder;
    property Text: string read GetText write SetText;
    property Version: string read GetVersion write SetVersion;
    property Visible;

    property OnCategoryPopupClick: TCategoryItemClick read FOnCategoryPopupClick write FOnCategoryPopupClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDropDownHeaderButtonClick: TDropDownButtonItemClick read FOnDropDownHeaderButtonClick write FOnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick: TDropDownButtonItemClick read FOnDropDownFooterButtonClick write FOnDropDownFooterButtonClick;
    property OnDrawHeader: TDrawBackGroundEvent read FOnDrawHeader write SetOnDrawHeader;
    property OnDrawFooter: TDrawBackGroundEvent read FOnDrawFooter write SetOnDrawFooter;
    property OnGetHeaderText: TGetTextEvent read FOnGetHeaderText write FOnGetHeaderText;
    property OnGetFooterText: TGetTextEvent read FOnGetFooterText write FOnGetFooterText;

    property OnBeforeDropDown: TNotifyEvent read FOnBeforeDropDown write FOnBeforeDropDown;
    property OnDropDown: TDropDown read FOnDropDown write FOnDropDown;
    property OnBeforeDropUp: TNotifyEvent read FOnBeforeDropUp write FOnBeforeDropUp;
    property OnDropUp: TDropUP read FOnDropUP write FOnDropUp;
    property OnFiltered: TNotifyEvent read FOnFiltered write FOnFiltered;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnSearchButtonClick: TNotifyEvent read FOnSearchButtonClick write FOnSearchButtonClick;

    property OnMouseUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnExit;
    property OnEnter;

  end;

implementation

uses
  uxTheme;

const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);


{ TAdvSearchEdit }

procedure TAdvSearchEdit.CatBtnClick(Sender: TObject);
var
  mnu: TMenuItem;
  pt: TPoint;
  i: integer;
begin
  FCatPopup.Items.Clear;

  pt := Point(FCatBtn.Left, FCatBtn.Top + FCatBtn.Height);

  pt := FCatBtn.ClientToScreen(pt);

  for i := 0 to Categories.Count - 1 do
  begin
    mnu := TMenuItem.Create(FCatPopup);
    mnu.Caption := Categories[i].Caption;

    if FCategoryButton.PopupType = pmCheck then
    begin
      mnu.Checked := not Categories[i].Filter;
      mnu.RadioItem := false;
    end
    else
    begin
      mnu.Checked := Categories[i].Filter;
      mnu.RadioItem := true;
    end;

    mnu.AutoCheck := true;
    mnu.GroupIndex := 1;
    mnu.Tag := i;
    mnu.OnClick := DoCatMenuItemClick;

    FCatPopup.Items.Add(mnu);
  end;

  FCatPopup.Popup(pt.X, pt.Y);
end;

procedure TAdvSearchEdit.CatBtnGetAppearance(Sender: TObject;
  var Appearance: TGlowButtonAppearance);
begin
  Appearance := FCatBtn.Appearance;
end;

procedure TAdvSearchEdit.CatBtnSetAppearance(Sender: TObject;
  Appearance: TGlowButtonAppearance);
begin
  FCatBtn.Appearance.Assign(Appearance);
end;

procedure TAdvSearchEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;

  if not Enabled then
  begin
    Color := FDisabledColor;
    FEdit.Color := FDisabledColor;
  end
  else
  begin
    Color := FNormalColor;
    FEdit.Color := FNormalColor;
  end;
end;

procedure TAdvSearchEdit.CMFontChanged(var Message: TMessage);
begin
  FEdit.Font.Assign(Font);
end;

constructor TAdvSearchEdit.Create(AOwner: TComponent);
var
  FDesignTime: boolean;

begin
  inherited;

  FCategoryButton := TSearchEditPopupButton.Create;
  FCategoryButton.OnGetAppearance := CatBtnGetAppearance;
  FCategoryButton.OnSetAppearance := CatBtnSetAppearance;

  FSearchButton := TSearchEditButton.Create;
  FSearchButton.OnGetAppearance := SearchBtnGetAppearance;
  FSearchButton.OnSetAppearance := SearchBtnSetAppearance;

  FEdit := CreateSearchDropDown;
  FEdit.Parent := Self;
  FEdit.BorderStyle := bsNone;
  FEdit.OnDropDown := DoDropDown;
  FEdit.OnDropUp := DoDropUp;
  FEdit.OnDropDownHeaderButtonClick := DoDropDownHeaderButtonClick;
  FEdit.OnDropDownFooterButtonClick := DoDropDownFooterButtonClick;
  FEdit.OnBeforeDropDown := DoBeforeDropDown;
  FEdit.OnBeforeDropUp := DoBeforeDropUp;
  FEdit.OnGetHeaderText := DoGetHeaderText;
  FEdit.OnGetFooterText := DoGetFooterText;
  FEdit.OnSelect := DoSelectValue;
  FEdit.OnFiltered := DoFiltered;
  FEdit.OnChange := DoEditChanged;
  FEdit.OnKeyDown := DoKeyDown;
  FEdit.OnKeyUp := DoKeyUp;
  FEdit.OnKeyPress := DoKeyPress;
  FEdit.OnEnter := HandleEnter;
  FEdit.OnExit := HandleExit;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    FEdit.EmptyText := 'Search ...';

  FCatBtn := TAdvGlowButton.Create(Self);
  FCatBtn.Parent := Self;
  FCatBtn.Width := 24;
  FCatBtn.BorderStyle := bsNone;
  FCatBtn.OnClick := CatBtnClick;
  FCatBtn.OnDrawButton := DrawCategoryButton;

  FSearchBtn := TAdvGlowButton.Create(Self);
  FSearchBtn.Parent := Self;
  FSearchBtn.Width := 24;
  FSearchBtn.BorderStyle := bsNone;
  FSearchBtn.OnClick := DoSearchButtonClick;

  FCatPopup := TPopupMenu.Create(Self);

  FUseVCLStyles := false;
  FBorderColor := clNone;
  FBorderStyle := bsSingle;
  Width := 300;
  Height := 21;

  FItemIndex := -1;

  FCatBtn.Align := alLeft;
  FCatBtn.SetComponentStyle(tsOffice2016White);
  FSearchBtn.Align := alRight;
  FSearchBtn.SetComponentStyle(tsOffice2016White);
  FSearchBtn.Caption := '...';

  FEdit.Margins.Top := 0;
  FEdit.Margins.Bottom := 0;
  FEdit.Margins.Left := 1;
  FEdit.Margins.Right := 1;
  FEdit.AlignWithMargins := true;
  FEdit.Align := alClient;
  FEdit.SetComponentStyle(tsOffice2016White);

  FFocusBorder := false;
  FFocusBorderColor := clNone;
  FDisabledColor := clSilver;
  FFocusColor := clNone;
  FFocusFontColor := clBlack;

  FNormalColor := clWindow;
  FNormalFontColor := clWindowText;

  FCategoryButton.OnChange := DoCategoryButtonChanged;
  FSearchButton.OnChange := DoSearchButtonChanged;
end;

procedure TAdvSearchEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BorderStyles[FBorderStyle];
end;

function TAdvSearchEdit.CreateSearchDropDown: TAdvSearchDropDown;
begin
  Result := TAdvSearchDropDown.Create(Self);
end;

destructor TAdvSearchEdit.Destroy;
begin
  FCategoryButton.Free;
  FSearchButton.Free;
  FCatPopup.Free;
  FEdit.Free;
  FCatBtn.Free;
  FSearchBtn.Free;
  inherited;
end;

procedure TAdvSearchEdit.DoBeforeDropDown(Sender: TObject);
begin
  if Assigned(OnBeforeDropDown) then
    OnBeforeDropDown(Self);
end;

procedure TAdvSearchEdit.DoBeforeDropUp(Sender: TObject);
begin
  if Assigned(OnBeforeDropUp) then
    OnBeforeDropUp(Self);
end;

procedure TAdvSearchEdit.DoCategoryButtonChanged(Sender: TObject);
begin
  FCatBtn.Visible := FCategoryButton.Visible;

  if not FCategoryButton.Visible then
    FCatBtn.Width := 0
  else
    FCatBtn.Width := FCategoryButton.Width;

  FCatBtn.Caption := FCategoryButton.Caption;
  FCatBtn.Picture.Assign(FCategoryButton.Picture);
  FCatBtn.BorderStyle := FCategoryButton.BorderStyle;
end;

procedure TAdvSearchEdit.DoCategoryPopupClick(CategoryIndex: integer; isChecked: boolean);
begin
  if Assigned(OnCategoryPopupClick) then
    OnCategoryPopupClick(Self, CategoryIndex, isChecked);
end;

procedure TAdvSearchEdit.DoCatMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  with (Sender as TMenuItem) do
  begin
    DoCategoryPopupClick((Sender as TMenuItem).Tag, not Checked);

    if FCategoryButton.PopupType = pmCheck then
      Categories[Tag].Filter := not Checked
    else
    begin
      for i := 0 to Categories.Count - 1 do
        Categories[i].Filter := False;

      Categories[Tag].Filter := True;
    end;
  end;
end;

procedure TAdvSearchEdit.DoDrawFooter(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect);
begin
  if Assigned(OnDrawFooter) then
    OnDrawFooter(Self, ACanvas, ARect);
end;

procedure TAdvSearchEdit.DoDrawHeader(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect);
begin
  if Assigned(OnDrawHeader) then
    OnDrawHeader(Self, ACanvas, ARect);
end;

procedure TAdvSearchEdit.DoDropDown(Sender: TObject; var AcceptDrop: Boolean);
begin
  if Assigned(OnDropDown) then
    OnDropDown(Self, AcceptDrop);
end;

procedure TAdvSearchEdit.DoDropDownFooterButtonClick(Sender: TObject;
  ButtonIndex: Integer);
begin
  if Assigned(OnDropDownFooterButtonClick) then
    OnDropDownFooterButtonClick(Self, ButtonIndex);
end;

procedure TAdvSearchEdit.DoDropDownHeaderButtonClick(Sender: TObject;
  ButtonIndex: Integer);
begin
  if Assigned(OnDropDownHeaderButtonClick) then
    OnDropDownHeaderButtonClick(Self, ButtonIndex);
end;

procedure TAdvSearchEdit.DoDropUp(Sender: TObject; Cancelled: Boolean);
begin
  if Assigned(OnDropUp) then
    OnDropUp(Self, Cancelled);
end;

procedure TAdvSearchEdit.DoEditChanged(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvSearchEdit.DoFiltered(Sender: TObject);
begin
  if Assigned(OnFiltered) then
    OnFiltered(Self);
end;

procedure TAdvSearchEdit.HandleEnter(Sender: TObject);
begin
  DrawBorders;
  if FFocusColor <> clNone then
  begin
    // prepare for restore
    FNormalColor := FEdit.Color;

    FEdit.Color := FFocusColor;
    Color := FFocusColor;
  end;

  if FFocusFontColor <> clNone then
  begin
    FNormalFontColor := FEdit.Font.Color;

    FEdit.Font.Color := FFocusFontColor;
    Font.Color := FFocusFontColor;
  end;
end;

procedure TAdvSearchEdit.HandleExit(Sender: TObject);
begin
  DrawBorders;

  if FFocusColor <> clNone then
  begin
    FEdit.Color := FNormalColor;
    Color := FNormalColor;
  end;

  if FFocusFontColor <> clNone then
  begin
    FEdit.Font.Color := FNormalFontColor;
    Font.Color := FNormalFontColor;
  end;
end;

procedure TAdvSearchEdit.Init;
var
  OldColor: TColor;
begin
  FNormalColor := Color;
  FNormalFontColor := Font.Color;

  if not Enabled then
  begin
    OldColor := Color;
    Color := FDisabledColor;
    FNormalColor := OldColor;
  end;
end;

procedure TAdvSearchEdit.DoGetFooterText(Sender: TObject; var Text: string);
begin
  Text := DropDownFooter.Caption;
  if Assigned(OnGetFooterText) then
    OnGetFooterText(Self, Text);
end;

procedure TAdvSearchEdit.DoGetHeaderText(Sender: TObject; var Text: string);
begin
  Text := DropDownHeader.Caption;
  if Assigned(OnGetHeaderText) then
    OnGetHeaderText(Self, Text);
end;

procedure TAdvSearchEdit.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure TAdvSearchEdit.DoKeyPress(Sender: TObject; var key: char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
end;

procedure TAdvSearchEdit.DoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;

procedure TAdvSearchEdit.DoSearchButtonChanged(Sender: TObject);
begin
  FSearchBtn.Visible := FSearchButton.Visible;

  if not FSearchButton.Visible then
    FSearchBtn.Width := 0
  else
    FSearchBtn.Width := FSearchButton.Width;

  FSearchBtn.Caption := FSearchButton.Caption;
  FSearchBtn.Picture.Assign(FSearchButton.Picture);
  FSearchBtn.BorderStyle := FSearchButton.BorderStyle;
end;

procedure TAdvSearchEdit.DoSearchButtonClick(Sender: TObject);
begin
  if Assigned(OnSearchButtonClick) then
    OnSearchButtonClick(Self);
end;

procedure TAdvSearchEdit.DoSelectValue(Sender: TObject; var Value: string);
begin
  FItemIndex := FEdit.SearchList.SelectedItem.Index;

  if Assigned(OnSelect) then
    OnSelect(Self);
end;

procedure TAdvSearchEdit.DrawBorders;
{$IFDEF VCLLIB}
var
  DC: HDC;
  OldPen: HPen;
  ARect: TRect;
  //hTheme: THandle;
  clr: TColor;
{$ENDIF}
begin
  {$IFDEF VCLLIB}
  DC := GetWindowDC(Handle);

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);

    if IsThemeActive and not UseVCLStyles and (BorderColor = clNone) then
    begin
      //hTheme := OpenThemeData(Handle,'COMBOBOX');

      if GetFocus = FEdit.Handle then
        clr := $D77800
      else
        clr := $7A7A7A;

      if FocusBorder and (GetFocus = FEdit.Handle) and (FocusBorderColor <> clNone) then
        clr := FocusBorderColor;

      OldPen := SelectObject(DC,CreatePen(PS_SOLID, 1, ColorToRGB(clr)));

      MovetoEx(DC,ARect.Left ,ARect.Top ,nil);
      LineTo(DC,ARect.Right -1 ,ARect.Top );
      LineTo(DC,ARect.Right -1 ,ARect.Bottom - 1);
      LineTo(DC,ARect.Left,ARect.Bottom -1 );
      LineTo(DC,ARect.Left,ARect.Top );

      DeleteObject(SelectObject(DC,OldPen));

//      if GetFocus = FEdit.Handle then
//        DrawThemeBackground(hTheme, DC, CP_BACKGROUND, CBS_HOT, ARect, 0)
//      else
//        DrawThemeBackground(hTheme, DC, CP_BACKGROUND, CBS_NORMAL, ARect, 0);
//
//      CloseThemeData(hTheme);
    end
    else
    begin
      if BorderColor = clNone then
        clr := clBlack
      else
        clr := BorderColor;

      if FocusBorder and (GetFocus = FEdit.Handle) and (FocusBorderColor <> clNone) then
        clr := FocusBorderColor;

      OldPen := SelectObject(DC,CreatePen(PS_SOLID, 1, ColorToRGB(clr)));

      MovetoEx(DC,ARect.Left ,ARect.Top ,nil);
      LineTo(DC,ARect.Right -1 ,ARect.Top );
      LineTo(DC,ARect.Right -1 ,ARect.Bottom - 1);
      LineTo(DC,ARect.Left,ARect.Bottom -1 );
      LineTo(DC,ARect.Left,ARect.Top );

      DeleteObject(SelectObject(DC,OldPen));
    end;
  finally
    ReleaseDC(Handle,DC);
  end;
  {$ENDIF}
end;

procedure TAdvSearchEdit.DrawCategoryButton(Sender: TObject; Canvas: TCanvas;
  Rect: TRect; State: TGlowButtonState);
var
  d,l: integer;
begin
  if (FCatBtn.Caption = '') and (FCatBtn.Picture.Empty) then
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;

    d := (FCatBtn.Height - 10) div 2;
    l := (FCatBtn.Width - 14) div 2;

    Canvas.MoveTo(Rect.Left + l, Rect.Top + d);
    Canvas.LineTo(Rect.Right - l, Rect.Top + d);

    Canvas.MoveTo(Rect.Left + l, Rect.Top + d + 5);
    Canvas.LineTo(Rect.Right - l, Rect.Top + d + 5);

    Canvas.MoveTo(Rect.Left + l, Rect.Top + d + 10);
    Canvas.LineTo(Rect.Right - l, Rect.Top + d + 10);
  end;

end;

function TAdvSearchEdit.GetAppearance: TAdvSearchListAppearance;
begin
  Result := FEdit.SearchList.Appearance;
end;

function TAdvSearchEdit.GetAutoSelect: boolean;
begin
  Result := FEdit.AutoSelect;
end;

function TAdvSearchEdit.GetCategories: TCategoryList;
begin
  Result := FEdit.SearchList.Categories;
end;

function TAdvSearchEdit.GetColumns: TColumnItems;
begin
  Result := FEdit.SearchList.Columns;
end;

function TAdvSearchEdit.GetComponentStyle: TTMSStyle;
begin
  Result := FEdit.GetComponentStyle;
end;

function TAdvSearchEdit.GetDocURL: string;
begin
  Result := TTMSFNCSearchListDocURL;
end;

function TAdvSearchEdit.GetDropDownFooter: TFooterAppearance;
begin
  Result := FEdit.DropDownFooter;
end;

function TAdvSearchEdit.GetDropDownHeader: THeaderAppearance;
begin
  Result := FEdit.DropDownHeader;
end;

function TAdvSearchEdit.GetDropDownHeight: integer;
begin
  Result := FDropDownHeight;
end;

function TAdvSearchEdit.GetDropDownShadow: boolean;
begin
  Result := FEdit.DropDownShadow;
end;

function TAdvSearchEdit.GetDropDownSizeable: boolean;
begin
  Result := FEdit.DropDownSizeable;
end;

function TAdvSearchEdit.GetDropDownWidth: integer;
begin
  Result := FEdit.DropDownWidth;
end;

function TAdvSearchEdit.GetEmptyText: string;
begin
  Result := FEdit.EmptyText;
end;

function TAdvSearchEdit.GetEmptyTextFocused: boolean;
begin
  Result := FEdit.EmptyTextFocused;
end;

function TAdvSearchEdit.GetEmptyTextStyle: TFontStyles;
begin
  Result := FEdit.EmptyTextStyle;
end;

function TAdvSearchEdit.GetFilterCondition: TFilterCondition;
begin
  Result := FEdit.SearchList.FilterCondition;
end;

function TAdvSearchEdit.GetImages: TCustomImageList;
begin
  Result := nil;
  if not (csDestroying in ComponentState) then
    Result := FEdit.SearchList.Images;
end;

function TAdvSearchEdit.GetItemHeight: Integer;
begin
  Result := 20;
  if not (csDestroying in ComponentState) then
    Result := FEdit.SearchList.ItemHeight;
end;

function TAdvSearchEdit.GetItemIndex: Integer;
begin
  Result := -1;
  if not (csDestroying in ComponentState) then
    Result := FEdit.ItemIndex;
end;

function TAdvSearchEdit.GetItems: TSearchList;
begin
  Result := nil;
  if not (csDestroying in ComponentState) then
    Result := FEdit.SearchList.Items;
end;

function TAdvSearchEdit.GetSearchList: TAdvSearchList;
begin
  Result := nil;
  if not (csDestroying in ComponentState) then
    Result := FEdit.SearchList;
end;

function TAdvSearchEdit.GetSelLength: Integer;
begin
  Result := FEdit.SelLength;
end;

function TAdvSearchEdit.GetSelStart: Integer;
begin
  Result := FEdit.SelStart;
end;

function TAdvSearchEdit.GetText: string;
begin
  Result := FEdit.Text;
end;

function TAdvSearchEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvSearchEdit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvSearchEdit.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
    Init;

  FCatBtn.Picture.Assign(FCategoryButton.Picture);
  FSearchBtn.Picture.Assign(FSearchButton.Picture);
end;

procedure TAdvSearchEdit.LoadStrings(Value: TStrings);
begin
  if Columns.Count = 0 then
  begin
    Columns.Add;
    FilterCondition.Column := 0;
  end;
  Items.LoadStrings(Value);
end;

procedure TAdvSearchEdit.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TAdvSearchEdit.SearchBtnGetAppearance(Sender: TObject;
  var Appearance: TGlowButtonAppearance);
begin
  Appearance := FSearchBtn.Appearance;
end;

procedure TAdvSearchEdit.SearchBtnSetAppearance(Sender: TObject;
  Appearance: TGlowButtonAppearance);
begin
  FSearchBtn.Appearance.Assign(Appearance);
end;

procedure TAdvSearchEdit.SelectAll;
begin
  FEdit.SelectAll;
end;

procedure TAdvSearchEdit.SetAppearance(const Value: TAdvSearchListAppearance);
begin
  FEdit.SearchList.Appearance.Assign(Value);
end;

procedure TAdvSearchEdit.SetAutoSelect(const Value: boolean);
begin
  FEdit.AutoSelect := Value;
end;

procedure TAdvSearchEdit.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvSearchEdit.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TAdvSearchEdit.SetCategories(const Value: TCategoryList);
begin
  FEdit.SearchList.Categories.Assign(Value);
end;

procedure TAdvSearchEdit.SetCategoryButton(const Value: TSearchEditPopupButton);
begin
  FCategoryButton.Assign(Value);
end;

procedure TAdvSearchEdit.SetColorTones(ATones: TColorTones);
begin
  FEdit.SetColorTones(ATones);
  FCatBtn.SetColorTones(ATones);
  FSearchBtn.SetColorTones(ATones);
end;

procedure TAdvSearchEdit.SetColumns(const Value: TColumnItems);
begin
  FEdit.SearchList.Columns.Assign(Value);
end;

procedure TAdvSearchEdit.SetComponentStyle(AStyle: TTMSStyle);
begin
  FEdit.SetComponentStyle(AStyle);
  FCatBtn.SetComponentStyle(AStyle);
  FSearchBtn.SetComponentStyle(AStyle);
end;

procedure TAdvSearchEdit.SetDisabledColor(const Value: TColor);
begin
  if (FDisabledColor <> Value) then
  begin
    FDisabledColor := Value;
    Invalidate;
  end;
end;

procedure TAdvSearchEdit.SetDropDownFooter(const Value: TFooterAppearance);
begin
  FEdit.DropDownFooter.Assign(Value);
end;

procedure TAdvSearchEdit.SetDropDownHeader(const Value: THeaderAppearance);
begin
  FEdit.DropDownHeader.Assign(Value);
end;

procedure TAdvSearchEdit.SetDropDownHeight(const Value: integer);
begin
  FDropDownHeight := Value;
  FEdit.DropDownHeight := Value;
end;

procedure TAdvSearchEdit.SetDropDownShadow(const Value: boolean);
begin
  FEdit.DropDownShadow := Value;
end;

procedure TAdvSearchEdit.SetDropDownSizeable(const Value: boolean);
begin
  FEdit.DropDownSizeable := Value;
end;

procedure TAdvSearchEdit.SetDropDownWidth(const Value: integer);
begin
  FEdit.DropDownWidth := Value;
end;

procedure TAdvSearchEdit.SetEmptyText(const Value: string);
begin
  FEdit.EmptyText := Value;
end;

procedure TAdvSearchEdit.SetEmptyTextFocused(const Value: boolean);
begin
  FEdit.EmptyTextFocused := Value;
end;

procedure TAdvSearchEdit.SetEmptyTextStyle(const Value: TFontStyles);
begin
  FEdit.EmptyTextStyle := Value;
end;

procedure TAdvSearchEdit.SetFilterCondition(const Value: TFilterCondition);
begin
  FEdit.SearchList.FilterCondition.Assign(Value);
end;

procedure TAdvSearchEdit.SetFocus;
begin
  FEdit.SetFocus;
end;

procedure TAdvSearchEdit.SetImages(const Value: TCustomImageList);
begin
  FEdit.SearchList.Images := Value;
  FEdit.Images := Value;
end;

procedure TAdvSearchEdit.SetItemHeight(const Value: Integer);
begin
  FEdit.SearchList.ItemHeight := Value;
end;

procedure TAdvSearchEdit.SetItemIndex(const Value: Integer);
var
  sli: TSearchListItem;
begin
  FEdit.SearchList.ItemIndex := Value;
  FEdit.FItemIndex := Value;

  if (Value >= 0) and (Value < FEdit.FSearchList.Items.Count) then
  begin
    sli := FEdit.FSearchList.Items[Value];

    if Assigned(sli) then
    begin
      if FEdit.FSearchList.FilterCondition.Column < sli.Columns.Count  then
        Text := sli.Columns[FEdit.FSearchList.FilterCondition.Column].Caption;
      FEdit.SelectAll;
    end;
  end
  else
    Text := '';
end;

procedure TAdvSearchEdit.SetItems(const Value: TSearchList);
begin
  FEdit.SearchList.Items.Assign(Value);
end;

procedure TAdvSearchEdit.SetOnDrawFooter(const Value: TDrawBackGroundEvent);
begin
  FOnDrawFooter := Value;

  if Assigned(Value) then
    FEdit.OnDrawFooter := DoDrawFooter
  else
    FEdit.OnDrawFooter := nil;
end;

procedure TAdvSearchEdit.SetOnDrawHeader(const Value: TDrawBackGroundEvent);
begin
  FOnDrawHeader := Value;

  if Assigned(Value) then
    FEdit.OnDrawHeader := DoDrawHeader
  else
    FEdit.OnDrawHeader := nil;
end;

procedure TAdvSearchEdit.SetSearchButton(const Value: TSearchEditButton);
begin
  FSearchButton.Assign(Value);
end;

procedure TAdvSearchEdit.SetSelLength(const Value: Integer);
begin
  FEdit.SelLength := Value;
end;

procedure TAdvSearchEdit.SetSelStart(const Value: Integer);
begin
  FEdit.SelStart := Value;
end;

procedure TAdvSearchEdit.SetText(const Value: string);
begin
  FEdit.Text := Value;
end;

procedure TAdvSearchEdit.SetVersion(const Value: string);
begin
//
end;

procedure TAdvSearchEdit.UpdateFilter;
begin
  FEdit.SearchList.UpdateFilter;
end;

procedure TAdvSearchEdit.WMChar(var Msg: TWMChar);
begin
  if Assigned(FEdit) then
    FEdit.DefaultHandler(Msg);
end;

procedure TAdvSearchEdit.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if BorderStyle = bsSingle then
    DrawBorders;
end;

procedure TAdvSearchEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if Visible then
    FEdit.SetFocus;
end;

{ TAdvSearchDropDown }

procedure TAdvSearchDropDown.BeforeDropDown;
begin
  inherited;
  FSearchList.Width := Width;
  FSearchList.UpdateFilter;
end;

constructor TAdvSearchDropDown.Create(AOwner: TComponent);
begin
  FSearchList := CreateSearchList;
  FSearchList.FilterCondition.AutoSelect := true;
  inherited;
  FSearchList.OnClick := OnDropDownControlClick;
  FItemIndex := -1;
end;

procedure TAdvSearchDropDown.CreateDropDownForm;
begin
  inherited;

  if Assigned(FSearchList) then
    FSearchList.Parent := FDropDownForm;

  Control := FSearchList;

  FDropDownForm.CancelOnDeActivate := False;
end;

function TAdvSearchDropDown.CreateSearchList: TAdvSearchList;
begin
  Result := TAdvSearchList.Create(Self);
end;

destructor TAdvSearchDropDown.Destroy;
begin
  FSearchList.Free;
  inherited;
end;

procedure TAdvSearchDropDown.DoShowDropDown;
begin
  inherited;
  if not FKeyDropDown then
  begin
    FSearchList.FilterCondition.Text := '';
    FSearchList.UpdateFilter;
  end;
end;

procedure TAdvSearchDropDown.KeyUp(var Key: Word; Shift: TShiftState);
var
  doshow: boolean;
begin
  inherited;

  if (Key < ord('0')) and (Key <> VK_BACK) then
    Exit;

  doshow := (FOldText <> Text) and not ((Text = '') and (FOldText <> ''));

  FOldText := Text;

  FItemIndex := -1;
  FSearchList.FilterCondition.Text := Text;
  FSearchList.UpdateFilter;

  if Assigned(OnFiltered) then
    OnFiltered(Self);

  if doshow and (FSearchList.ItemCount > 0) and not DroppedDown and not (Key in [VK_RETURN, VK_ESCAPE]) then
  begin
    FKeyDropDown := true;
    ShowDropDown;
    FKeyDropDown := false;
  end;

end;

procedure TAdvSearchDropDown.OnDropDownControlClick(Sender: TObject);
var
  sli: TSearchListItem;
  NewValue: string;
begin
  sli := FSearchList.SelectedItem;
  if Assigned(sli) then
  begin
    if sli.Columns.Count > FSearchList.FilterCondition.Column then
    begin
      NewValue := sli.Columns[FSearchList.FilterCondition.Column].Caption;

      if Assigned(OnSelect) then
        OnSelect(Self, NewValue);

      //if NewValue <> Text then
      begin
        UpdateIndex;
        Text := NewValue;
        SelStart := 0;
        SelLength := Length(Text);
        HideDropDown(false);
      end;
    end;
  end;
end;

procedure TAdvSearchDropDown.OnDropDownControlKeyDown(var Key: Word;
  Shift: TShiftState);
var
  sli: TSearchListItem;
  s: string;
begin
  if not (Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_RETURN]) or (ssAlt in Shift) then
    inherited;

  if (Key = VK_TAB) then
  begin
    Key := 0;
    HideDropDown(false);
    SelectAll;
    Exit;
  end;

  if (Key = VK_RETURN) then
  begin
    sli := FSearchList.SelectedItem;

    if Assigned(sli) then
    begin
      if FSearchList.FilterCondition.Column < sli.Columns.Count  then
      begin
        s := sli.Columns[FSearchList.FilterCondition.Column].Caption;

        if Assigned(OnSelect) then
          OnSelect(Self, S);

        UpdateIndex;

        Text := s;
      end;
      SelectAll;
      inherited;
    end;
  end;
end;

procedure TAdvSearchDropDown.OnDropDownControlKeyPress(var Key: Char);
begin
  inherited;
end;

procedure TAdvSearchDropDown.OnDropDownControlKeyUp(var Key: Word;
  Shift: TShiftState);
begin
  if not (Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT]) or (ssAlt in Shift) then
    inherited;
end;

procedure TAdvSearchDropDown.UpdateIndex;
var
  sli: TSearchListItem;
begin
  sli := FSearchList.SelectedItem;
  if Assigned(sli) then
    FItemIndex := sli.Index
  else
    FItemIndex := -1;
end;

procedure TAdvSearchDropDown.WMKeyDown(var Msg: TWMKeydown);
var
  IsAlt: Boolean;
begin
  inherited;

  IsAlt := (GetKeyState(VK_MENU) and $8000 = $8000);

  if Enabled and not IsAlt then
  begin
    case Msg.CharCode of
    VK_DOWN:
      begin
        FSearchList.SelectNextItem(FSearchList.ItemIndex);
        UpdateIndex;
      end;
    VK_UP:
      begin
        FSearchList.SelectPreviousItem(FSearchList.ItemIndex);
        UpdateIndex;
      end;
    end;
  end;
end;

{ TCategoryButton }

procedure TSearchEditButton.Assign(Source: TPersistent);
begin
  if (Source is TSearchEditButton) then
  begin
    FWidth := (Source as TSearchEditButton).Width;
    FVisible := (Source as TSearchEditButton).Visible;
    FCaption := (Source as TSearchEditButton).Caption;
    FPicture.Assign((Source as TSearchEditButton).Picture);
  end;
end;

procedure TSearchEditButton.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TSearchEditButton.Create;
begin
  inherited;
  FWidth := 24;
  FVisible := true;
  FPicture := TGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FAppearance := TGlowButtonAppearance.Create;
end;

destructor TSearchEditButton.Destroy;
begin
  FAppearance.Free;
  FPicture.Free;
  inherited;
end;

function TSearchEditButton.GetAppearance: TGlowButtonAppearance;
begin
  if Assigned(OnGetAppearance) then
    OnGetAppearance(Self, Result)
  else
   Result := FAppearance;
end;

procedure TSearchEditButton.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TSearchEditButton.SetAppearance(const Value: TGlowButtonAppearance);
begin
  if Assigned(OnSetAppearance) then
    OnSetAppearance(Self, Value)
  else
    FAppearance.Assign(Value);
end;

procedure TSearchEditButton.SetBorderStyle(const Value: TBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Changed;
  end;
end;

procedure TSearchEditButton.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TSearchEditButton.SetPicture(const Value: TGDIPPicture);
begin
  FPicture.Assign(Value);
  Changed;
end;

procedure TSearchEditButton.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TSearchEditButton.SetWidth(const Value: integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TSearchEditPopupButton }

procedure TSearchEditPopupButton.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TSearchEditPopupButton) then
  begin
    FPopupType := (Source as TSearchEditPopupButton).PopupType;
  end;

end;

constructor TSearchEditPopupButton.Create;
begin
  inherited;
  FPopupType := pmCheck;
end;

end.
