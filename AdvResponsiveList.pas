{*************************************************************************}
{ TMS TAdvResponsiveList                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2016 - 2019                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvResponsiveList;

{$I TMSDEFS.INC}

{$IFDEF VCLLIB}
{$IFDEF DELPHIXE2_LVL}
{$DEFINE USEVCLSTYLES}
{$ENDIF}
{$IFDEF FNCLIB}
{$DEFINE USEVCLSTYLES}
{$ENDIF}
{$ENDIF}


interface

uses
  {$IFDEF VCLLIB}
  Windows,
  {$ENDIF}
  Classes, Types, Variants, Generics.Collections,
  AdvGraphics, AdvGraphicsTypes, AdvTypes, AdvScrollControl,
  AdvResponsiveListTypes, AdvUtils, PictureContainer
  {$IFNDEF FMXLIB}
  , Controls, Forms, Graphics
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.Controls, FMX.Forms, FMX.Types, FMX.Graphics, System.UIConsts, System.UITypes
  {$ENDIF}
  {$IFDEF VCLLIB}
  , Messages, ImgList
  {$IFNDEF FNCLIB}
  , GDIPicture
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF LCLLIB}
  , LCLType, LMessages, LCLIntf, ImgList
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.1.0.0 : New : Keyboard support added
  //          : New : OnBeforeItemSelect, OnAfterItemSelect events added
  //          : New : Multiselect capability added
  //          : New : ConditionCategory;
  //          : New : Touch scroll support
  //          : New : OnBackgroundDraw() event
  //          : New : Support for displaying controls in items
  //          : New : OnItemControlClick() event
  // v1.1.1.0 : New : Support for VCL styles
  // v1.2.0.0 : New : HeaderTemplate capability added
  //          : New : FooterTemplate capability added
  //          : New : Appearance.ItemContentMargin added
  //          : New : Filter capability
  //          : Improved : Keyboard handling
  // v1.2.0.1 : Fixed : Issue with scrollhandling and using child controls
  // v1.2.1.0 : New : OnFieldsToItem event added in TDBAdvResponsiveList
  // v1.3.0.0 : New : Hover background, text & border color capability
  // v1.3.0.1 : Fixed : Filtering with HTML formatted items
  // v1.3.0.2 : Fixed : Issue with item selection after removing items
  // v1.3.1.0 : New : Support for mouse wheel handling added
  // v1.3.1.1 : Improved : Small code change to allow to delete items from item click
  // v1.3.2.0 : New : Per monitor support for high DPI
  // v1.3.2.1 : Improved : VCL styles handling

const
  TTMSFNCResponsiveListDocURL = 'https://www.tmssoftware.biz/download/manuals/TMSFNCResponsiveListDevGuide.pdf';
  TTMSFNCResponsiveListTipsURL = 'https://www.tmssoftware.com/site/tmsfncuipack.asp?s=faq';
{$IFDEF FNCLIB}
  clBlue = gcBlue;
  clWhite = gcWhite;
  clBlack = gcBlack;
  clNone = gcNull;
  clGray = gcGray;
  clYellow = gcYellow;
  clSilver = gcSilver;
  clRed = gcRed;
  clWindow = gcWhite;
  clWindowText = gcBlack;
  clHighlight = gcLightSkyBlue;
  clHighlightText = gcWhite;
{$ENDIF}

type
  TAdvResponsiveList = class;

  TResponsiveCondition = class(TCollectionItem)
  private
    FColumns: integer;
    FItemHeight: integer;
    FItemWidth: integer;
    FHeightFrom: integer;
    FWidthTo: integer;
    FRows: Integer;
    FWidthFrom: integer;
    FHeightTo: integer;
    FMargins: TMargins;
    FTemplate: string;
    FHeaderTemplate: string;
    FFooterTemplate: string;
    FTag: NativeInt;
    FCategory: integer;
    procedure SetMargins(const Value: TMargins);
    procedure SetColumns(const Value: integer);
    procedure SetRows(const Value: Integer);
    procedure SetTemplate(const Value: string);
    procedure SetHeightFrom(const Value: integer);
    procedure SetHeightTo(const Value: integer);
    procedure SetItemHeight(const Value: integer);
    procedure SetItemWidth(const Value: integer);
    procedure SetWidthFrom(const Value: integer);
    procedure SetWidthTo(const Value: integer);
    procedure SetFooterTemplate(const Value: string);
    procedure SetHeaderTemplate(const Value: string);
  protected
    procedure MarginsChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Category: integer read FCategory write FCategory default -1;
    property Columns: integer read FColumns write SetColumns;

    property FooterTemplate: string read FFooterTemplate write SetFooterTemplate;

    property HeaderTemplate: string read FHeaderTemplate write SetHeaderTemplate;
    property HeightFrom: integer read FHeightFrom write SetHeightFrom default 0;
    property HeightTo: integer read FHeightTo write SetHeightTo default -1;

    property ItemWidth: integer read FItemWidth write SetItemWidth default -1;
    property ItemHeight: integer read FItemHeight write SetItemHeight default -1;

    property Margins: TMargins read FMargins write SetMargins;
    property Rows: Integer read FRows write SetRows default 0;

    property Template: string read FTemplate write SetTemplate;

    property WidthFrom: integer read FWidthFrom write SetWidthFrom default 0;
    property WidthTo: integer read FWidthTo write SetWidthTo default -1;
    property Tag: NativeInt read FTag write FTag;
  end;

  TResponsiveConditions = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TResponsiveCondition;
    procedure SetItem(Index: Integer; const Value: TResponsiveCondition);
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    function Add: TResponsiveCondition;
    function Insert(Index: integer): TResponsiveCondition;
    property Items[Index: Integer]: TResponsiveCondition read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TResponsiveListItemFilterData = (fdContent, fdHeader, fdFooter);
  TResponsiveListItemFilterDataSet = set of TResponsiveListItemFilterData;
  TResponsiveListItemFilterType = (mText, mEntireWord, mStartWord, mEndWord);

  TResponsiveListItemFilter = class(TPersistent)
  private
    FCaseSensitive: boolean;
    FText: string;
    FFilterData: TResponsiveListItemFilterDataSet;
    FFilterType: TResponsiveListItemFilterType;
    FCondition: string;
  protected
    property Condition: string read FCondition write FCondition;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property FilterData: TResponsiveListItemFilterDataSet read FFilterData write FFilterData default [fdContent];
    property FilterType: TResponsiveListItemFilterType read FFilterType write FFilterType default mText;
    property Text: string read FText write FText;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive default false;
  end;

  TItemSizeType = (isAuto, isFixed, isPerc, isFill);

  TResponsiveNameValuePairList = TList<TResponsiveNameValuePair>;

  TResponsiveListItem = class(TCollectionItem)
  private
    FOwnerList: TAdvResponsiveList;
    FContent: string;
    FVisible: boolean;
    FWidth: Integer;
    FHeightType: TItemSizeType;
    FOnChange: TNotifyEvent;
    FHeight: Integer;
    FWidthType: TItemSizeType;
    FColor: TColor;
    FRect: TRect;
    FControl: TControl;
    FBorderColor: TColor;
    FBorderStyle: TBorderStyle;
    FNameValueList: TResponsiveNameValuePairList;
    FFooterColor: TColor;
    FHeaderText: string;
    FHeaderHeight: Integer;
    FFooterText: string;
    FFooterHeight: Integer;
    FHeaderColor: TColor;
    FHeaderTextColor: TColor;
    FFooterTextColor: TColor;
    FTextColor: TColor;
    FShadow: boolean;
    FSelected: boolean;
    FHovered: boolean;
    FTag: NativeInt;
    FSelectedColor: TColor;
    FSelectedTextColor: TColor;
    FSelectedBorderColor: TColor;
    FControlName: string;
    FContentRect: TRect;
    FCol: Integer;
    FRow: Integer;
    FPictureContainer: TPictureContainer;
    FContentMargin: integer;
    FFiltered: boolean;
    {$IFNDEF FMXLIB}
    FImages: TCustomImageList;
    {$ENDIF}
    FHotBorderColor: TColor;
    FHotTextColor: TColor;
    FHotColor: TColor;
    FDPIScale: single;
    procedure SetContent(const Value: string);
    procedure SetVisible(const Value: boolean);
    procedure SetHeight(const Value: Integer);
    procedure SetHeightType(const Value: TItemSizeType);
    procedure SetWidth(const Value: Integer);
    procedure SetWidthType(const Value: TItemSizeType);
    procedure SetColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderStyle(const Value: TBorderStyle);
    function GetValue(AName: string): variant;
    procedure SetValue(AName: string; const Value: variant);
    procedure SetFooterColor(const Value: TColor);
    procedure SetFooterHeight(const Value: Integer);
    procedure SetFooterText(const Value: string);
    procedure SetHeaderColor(const Value: TColor);
    procedure SetHeaderHeight(const Value: Integer);
    procedure SetHeaderText(const Value: string);
    procedure SetFooterTextColor(const Value: TColor);
    procedure SetHeaderTextColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
    procedure SetShadow(const Value: boolean);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedTextColor(const Value: TColor);
    procedure SetSelectedBorderColor(const Value: TColor);
    procedure SetControl(const Value: TControl);
    procedure SetPictureContainer(const Value: TPictureContainer);
  protected
    procedure DrawItem(AGraphics: TAdvGraphics; ATemplate, AHeaderTemplate, AFooterTemplate: string;
  ARect: TRect; Focus: boolean); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteControlName(Writer: TWriter);
    procedure ReadControlName(Reader: TReader);
    property ContentRect: TRect read FContentRect write FContentRect;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
    property ContentMargin: integer read FContentMargin write FContentMargin;
    property Filtered: boolean read FFiltered write FFiltered;
    property Hovered: boolean read FHovered write FHovered;
    function IsVisible: boolean; virtual;
  public
    constructor Create({%H-}ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Rect: TRect read FRect;
    property Control: TControl read FControl write SetControl;
    property ControlName: string read FControlName write FControlName;
    property NameValueList: TResponsiveNameValuePairList read FNameValueList;
    property Values[AName: string]: variant read GetValue write SetValue;
    property Selected: boolean read FSelected write FSelected;
    property PictureContainer: TPictureContainer read FPictureContainer write SetPictureContainer;
    {$IFNDEF FMXLIB}
    property Images: TCustomImageList read FImages write FImages;
    {$ENDIF}
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Content: string read FContent write SetContent;
    property Color: TColor read FColor write SetColor default clWindow;
    property FooterColor: TColor read FFooterColor write SetFooterColor default clNone;
    property FooterHeight: Integer read FFooterHeight write SetFooterHeight default 16;
    property FooterText: string read FFooterText write SetFooterText;
    property FooterTextColor: TColor read FFooterTextColor write SetFooterTextColor default clWhite;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor default clNone;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 16;
    property HeaderText: string read FHeaderText write SetHeaderText;
    property HeaderTextColor: TColor read FHeaderTextColor write SetHeaderTextColor default clWhite;
    property HotBorderColor: TColor read FHotBorderColor write FHotBorderColor default clNone;
    property HotColor: TColor read FHotColor write FHotColor default clNone;
    property HotTextColor: TColor read FHotTextColor write FHotTextColor default clNone;
    property SelectedBorderColor: TColor read FSelectedBorderColor write SetSelectedBorderColor default clSilver;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clHighlight;
    property SelectedTextColor: TColor read FSelectedTextColor write SetSelectedTextColor default clHighlightText;

    property Height: Integer read FHeight write SetHeight default 0;
    property HeightType: TItemSizeType read FHeightType write SetHeightType default isAuto;
    property Shadow: boolean read FShadow write SetShadow default false;
    property Tag: NativeInt read FTag write FTag default 0;
    property TextColor: TColor read FTextColor write SetTextColor default clWindowText;
    property Visible: boolean read FVisible write SetVisible;
    property Width: Integer read FWidth write SetWidth default 0;
    property WidthType: TItemSizeType read FWidthType write SetWidthType default isAuto;
  end;

  TResponsiveListItems = class(TOwnedCollection)
  private
    FList: TAdvResponsiveList;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TResponsiveListItem;
    procedure SetItem(Index: Integer; const Value: TResponsiveListItem);
    function GetList: TAdvResponsiveList;
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure Changed; virtual;
    procedure Init(Item: TResponsiveListItem);
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    function Add: TResponsiveListItem;
    function Insert(Index: Integer): TResponsiveListItem;
    property Items[Index: Integer]: TResponsiveListItem read GetItem write SetItem; default;
    property List: TAdvResponsiveList read GetList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TResponsiveListItemsApplying = (iaNew, iaAll);

  TResponsiveListAppearance = class(TPersistent)
  private
    FItemBorderStyle: TBorderStyle;
    FItemBorderColor: TColor;
    FItemColor: TColor;
    FItemFooterHeight: Integer;
    FItemHeaderColor: TColor;
    FItemFooterColor: TColor;
    FItemHeaderHeight: Integer;
    FItemHeaderTextColor: TColor;
    FItemFooterTextColor: TColor;
    FItemTextColor: TColor;
    FItemsApplying: TResponsiveListItemsApplying;
    FOnChange: TNotifyEvent;
    FItemShadow: boolean;
    FSelectedItemTextColor: TColor;
    FSelectedItemColor: TColor;
    FSelectedItemBorderColor: TColor;
    FItemContentMargin: integer;
    FHotItemColor: TColor;
    FHotItemBorderColor: TColor;
    FHotItemTextColor: TColor;
    procedure SetItemBorderColor(const Value: TColor);
    procedure SetItemBorderStyle(const Value: TBorderStyle);
    procedure SetItemColor(const Value: TColor);
    procedure SetItemFooterColor(const Value: TColor);
    procedure SetItemFooterHeight(const Value: Integer);
    procedure SetItemFooterTextColor(const Value: TColor);
    procedure SetItemHeaderColor(const Value: TColor);
    procedure SetItemHeaderHeight(const Value: Integer);
    procedure SetItemHeaderTextColor(const Value: TColor);
    procedure SetItemsApplying(const Value: TResponsiveListItemsApplying);
    procedure SetItemTextColor(const Value: TColor);
    procedure SetItemShadow(const Value: boolean);
    procedure SetSelectedItemColor(const Value: TColor);
    procedure SetSelectedItemTextColor(const Value: TColor);
    procedure SetSelectedItemBorderColor(const Value: TColor);
    procedure SetItemContentMargin(const Value: integer);
  protected
    procedure Changed; virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property ItemsApplying: TResponsiveListItemsApplying read FItemsApplying write SetItemsApplying default iaAll;
    property ItemBorderColor: TColor read FItemBorderColor write SetItemBorderColor default clGray;
    property ItemBorderStyle: TBorderStyle read FItemBorderStyle write SetItemBorderStyle default bsSingle;
    property ItemColor: TColor read FItemColor write SetItemColor default clWindow;
    property ItemContentMargin: integer read FItemContentMargin write SetItemContentMargin default 0;
    property ItemTextColor: TColor read FItemTextColor write SetItemTextColor default clWindowText;
    property ItemFooterColor: TColor read FItemFooterColor write SetItemFooterColor default clNone;
    property ItemFooterTextColor: TColor read FItemFooterTextColor write SetItemFooterTextColor default clWhite;
    property ItemFooterHeight: Integer read FItemFooterHeight write SetItemFooterHeight default 16;
    property ItemHeaderColor: TColor read FItemHeaderColor write SetItemHeaderColor default clHighlight;
    property ItemHeaderTextColor: TColor read FItemHeaderTextColor write SetItemHeaderTextColor default clWhite;
    property ItemHeaderHeight: Integer read FItemHeaderHeight write SetItemHeaderHeight default 16;
    property ItemShadow: boolean read FItemShadow write SetItemShadow default false;
    property SelectedItemBorderColor: TColor read FSelectedItemBorderColor write SetSelectedItemBorderColor default clSilver;
    property SelectedItemColor: TColor read FSelectedItemColor write SetSelectedItemColor default clHighlight;
    property SelectedItemTextColor: TColor read FSelectedItemTextColor write SetSelectedItemTextColor default clHighlightText;
    property HotItemBorderColor: TColor read FHotItemBorderColor write FHotItemBorderColor default clNone;
    property HotItemTextColor: TColor read FHotItemTextColor write FHotItemTextColor default clNone;
    property HotItemColor: TColor read FHotItemColor write FHotItemColor default clNone;
  end;


  TBeforeDrawResponsiveListItemEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; AItem: TResponsiveListItem; ARect: TRect; var DoDefaultDraw: boolean) of object;

  TAfterDrawResponsiveListItemEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; AItem: TResponsiveListItem; ARect: TRect) of object;

  TResponsiveListBackgroundDrawEvent = procedure(Sender: TObject; AGraphics: TAdvGraphics; ARect: TRect) of object;

  TResponsiveListItemEvent = procedure(Sender: TObject; AItem: TResponsiveListItem) of object;

  TResponsiveListItemAllowEvent = procedure(Sender: TObject; AItem: TResponsiveListItem; var Allow: boolean) of object;

  TResponsiveListItemAnchorEvent = procedure(Sender: TObject; AItem: TResponsiveListItem; Anchor: string; var DefaultHandling: boolean) of object;

  TResponsiveListItemControlEvent = procedure(Sender: TObject; AItem: TResponsiveListItem; ControlID, ControlValue, ControlType: string) of object;

  TResponsiveListItemFilterEvent = procedure(Sender: TObject; AItem: TResponsiveListItem; var Retain: boolean) of object;

  TResponsiveConditionEvent = procedure(Sender: TObject; ACondition: TResponsiveCondition) of object;

  {$IFDEF DELPHIXE2_LVL}
  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  {$ENDIF}
  TAdvResponsiveList = class(TCustomScrollingControl)
  private
    FListItems: TResponsiveListItems;
    FConditions: TResponsiveConditions;
    FAppearance: TResponsiveListAppearance;
    FMaxSize: TSize;
    FDesignTime: boolean;
    FItemDown: TResponsiveListItem;
    FXYDown: TPoint;
    FXYMoving: boolean;
    FCondition: TResponsiveCondition;
    FOnBeforeDrawItem: TBeforeDrawResponsiveListItemEvent;
    FOnAfterDrawItem: TAfterDrawResponsiveListItemEvent;
    FOnBackgroundDraw: TResponsiveListBackgroundDrawEvent;
    FOnItemClick: TResponsiveListItemEvent;
    FOnConditionChange: TResponsiveConditionEvent;
    FItemIndex: Integer;
    FLastItemClick: integer;
    FHoverIndex: Integer;
    FActiveTemplate: string;
    FActiveHeaderTemplate: string;
    FActiveFooterTemplate: string;
    FRowCount: integer;
    FColCount: integer;
    FConditionCategory: integer;
    {$IFNDEF FMXLIB}
    FImages: TCustomImageList;
    {$ENDIF}
    FOldCursor: TCursor;
    FPictureContainer: TPictureContainer;
    {$IFDEF VCLLIB}
    {$IFNDEF FNCLIB}
    FImageCache: THTMLPictureCache;
    {$ENDIF}
    {$ENDIF}
    FOnItemLeave: TResponsiveListItemEvent;
    FOnItemEnter: TResponsiveListItemEvent;
    FOnItemFilter: TResponsiveListItemFilterEvent;
    FOnItemAnchorClick: TResponsiveListItemAnchorEvent;
    FOnItemControlClick:  TResponsiveListItemControlEvent;
    FOnBeforeSelectItem: TResponsiveListItemAllowEvent;
    FOnAfterSelectItem: TResponsiveListItemEvent;
    FMultiSelect: boolean;
    FFilterCondition: TResponsiveListItemFilter;
    FDPIScale: single;
    procedure SetItems(const Value: TResponsiveListItems);
    procedure SetAppearance(const Value: TResponsiveListAppearance);
    procedure SetConditions(const Value: TResponsiveConditions);
    procedure SetConditionCategory(const Value: integer);
    {$IFDEF VCLLIB}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    {$ENDIF}
    {$IFDEF LCLLIB}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message LM_GETDLGCODE;
    {$ENDIF}

    {$IFNDEF FMXLIB}
    procedure CMMouseLeave(var {%H-}Message: TMessage); message CM_MOUSELEAVE;
    {$ENDIF}
    {$IFDEF USEVCLSTYLES}
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$ENDIF}
    procedure SetVersion(const {%H-}Value: string);
    procedure SetItemIndex(const Value: Integer);
    {$IFNDEF FMXLIB}
    procedure SetImages(const Value: TCustomImageList);
    {$ENDIF}
    function GetCursorEx: TCursor;
    procedure SetCursorEx(const Value: TCursor);
    function GetTabStopEx: boolean;
    procedure SetTabStopEx(const Value: boolean);
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetFilterCondition(const Value: TResponsiveListItemFilter);
  protected
    function GetVersion: string; override;
    function GetDocURL: string; override;
    function GetTipsURL: string; override;
    {$IFDEF USEVCLSTYLES}
    procedure InitVCLStyle(init: boolean);
    {$ENDIF}
    {$IFDEF FNCLIB}
    {$IFNDEF FMXLIB}
    procedure SetAdaptToStyle(const Value: boolean); override;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure InitStyle; override;
    procedure ResetToDefaultStyle; override;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    procedure AlignControls({%H-}AControl: TControl; var {%H-}Rect: TRect); override;
    {$ENDIF}
    procedure MouseDownN({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMoveN({%H-}Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUpN({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
    procedure DoDblClick({%H-}X, {%H-}Y: integer); override;
    procedure KeyDownN(var {%H-}Key: Word; {%H-}Shift: TShiftState); override;
    procedure KeyUpN(var {%H-}Key: Word; {%H-}Shift: TShiftState); override;
    procedure KeyPressN(var {%H-}Key: Char); override;
    procedure TopLeftChanged; override;
    procedure SelectAll; override;
    function GetElementCount: integer; override;
    procedure GetWordAndIndexAtCaret(var {%H-}AValue: string; var {%H-}AIndex: integer; SpaceOnly: boolean = false); override;
    procedure UpdateWordAndIndexAtCaret({%H-}AValue: string; {%H-}AIndex: integer; SpaceOnly: boolean = false); override;

    function SelectWordAtCaret: string; override;
    procedure Backspace; override;
    procedure UpdateSize; override;
    function HasSelection: boolean; override;
    function SelectedText: string; override;

    procedure UpdateSelectionPoint({%H-}LeftSel: boolean; var {%H-}X, {%H-}Y: integer); override;
    function IsForwardSelection: boolean; override;
    procedure InsertChar({%H-}ch: char); overload; override;
    procedure InsertChar({%H-}Value: string); overload; override;
    procedure DoSelectionChanged; override;
    procedure UpdateSelection; override;
    procedure DeleteSelection; override;
    function GetSelectionFromXY: TPoint; override;
    function GetSelectionToXY: TPoint; override;
    function GetCaretXY: TPoint; override;
    function GetCaretLH: integer; override;

    procedure DoItemClick(AItem: TResponsiveListItem); virtual;
    procedure DoItemAnchorClick(AItem: TResponsiveListItem; Anchor: string); virtual;
    procedure DoItemControlClick(AItem: TResponsiveListItem; ControlID, ControlValue, ControlType: string); virtual;
    procedure DoItemEnter(AItem: TResponsiveListItem); virtual;
    procedure DoItemLeave(AItem: TResponsiveListItem); virtual;
    function DoItemFilter(AItem: TResponsiveListItem): boolean; virtual;
    procedure DoBeforeSelectItem(AItem: TResponsiveListItem; var Allow: boolean); virtual;
    procedure DoAfterSelectItem(AItem: TResponsiveListItem); virtual;
    procedure DoSelectItem(AIndex: Integer); virtual;
    procedure DoConditionChange(ACondition: TResponsiveCondition); virtual;
    procedure ConditionsChanged(Sender: TObject); virtual;
    function RenderItems: TResponsiveCondition; virtual;
    procedure ResolveControls; virtual;
    procedure UpdateControls; virtual;
    procedure Paint; override;
    procedure PaintBackground(AGraphics: TAdvGraphics); virtual;

    function GetVersionNr: Integer; virtual;

    function GetClientWidth: integer; override;
    function GetClientHeight: integer; override;

    function GetConditionClass: TCollectionItemClass; virtual;
    function CreateConditions: TResponsiveConditions; virtual;
    function GetItemClass: TCollectionItemClass; virtual;
    function CreateListItems: TResponsiveListItems; virtual;
    function MatchFromTo(AValue, AFrom, ATo: Integer): boolean;
    function FixedWidth(FromItem, Count: Integer): Integer;
    function FixedHeight(FromItem, Count: Integer): Integer;
    function FixedColumns(FromItem, Count: Integer): Integer;
    function FixedRows(FromItem, Count: Integer): Integer;
    function HasTextContent: boolean; override;
    procedure DrawItem(AGraphics: TAdvGraphics; AItem: TResponsiveListItem; ATemplate, AHeaderTemplate, AFooterTemplate: string; ARect: TRect; Focus: boolean); virtual;
    procedure ItemsChanged(Sender: TObject); virtual;
    procedure AppearanceChanged(Sender: TObject); virtual;
    {$IFNDEF FMXLIB}
    procedure CreateWnd; override;
    {$ENDIF}
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    {$IFDEF VCLLIB}
    {$IFNDEF FNCLIB}
    property PictureCache: THTMLPictureCache read FImageCache write FImageCache;
    {$ENDIF}
    {$ENDIF}
    property ColCount: integer read FColCount write FColCount;
    property RowCount: integer read FRowCount write FRowCount;
    procedure Refresh;
    function FilterItem(Condition: TResponsiveCondition; Item: TResponsiveListItem): boolean; virtual;
    {$IFNDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
    {$IFDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded; override;
    {$IFNDEF FMXLIB}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    {$ENDIF}
    procedure UpdateFilter; virtual;
    procedure ClearFilter; virtual;

    procedure DoEnter; override;
    procedure DoExit; override;

    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;
    function PasteFromClipboard: string; override;

    property ConditionCategory: integer read FConditionCategory write SetConditionCategory;
    function GetCondition: TResponsiveCondition;
    function FindItemAtPoint(pt: TPoint): TResponsiveListItem;
    function AnchorAtPoint(AItem: TResponsiveListItem; pt: TPoint; var Anchor: string): boolean;
    function ControlAtPoint(AItem: TResponsiveListItem; pt: TPoint; var ControlID, ControlValue, ControlType: string): boolean;
    function FindControlItem(AControl: TControl): TResponsiveListItem;
    procedure ScrollInView(AItem: TResponsiveListItem);
    procedure ClearSelection; override;
  published
    property Anchors;
    property Appearance: TResponsiveListAppearance read FAppearance write SetAppearance;
    {$IFNDEF FMXLIB}
    property BorderColor;
    {$ENDIF}
    property Color;
    property Conditions: TResponsiveConditions read FConditions write SetConditions;
    property Cursor: TCursor read GetCursorEx write SetCursorEx;
    {$IFNDEF FMXLIB}
    property DoubleBuffered;
    {$ENDIF}
    property FilterCondition: TResponsiveListItemFilter read FFilterCondition write SetFilterCondition;
    {$IFNDEF FMXLIB}
    property Images: TCustomImageList read FImages write SetImages;
    {$ENDIF}
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Items: TResponsiveListItems read FListItems write SetItems;
    property MultiSelect: boolean read FMultiSelect write FMultiSelect default false;
    property PictureContainer: TPictureContainer read FPictureContainer write SetPictureContainer;
    property PopupMenu;
    {$IFDEF USEVCLSTYLES}
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}
    {$ENDIF}
    property TabStop: boolean read GetTabStopEx write SetTabStopEx default true;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnAfterDrawItem: TAfterDrawResponsiveListItemEvent read FOnAfterDrawItem write FOnAfterDrawItem;
    property OnAfterSelectItem: TResponsiveListItemEvent read FOnAfterSelectItem write FOnAfterSelectItem;
    property OnBackgroundDraw: TResponsiveListBackgroundDrawEvent read FOnBackgroundDraw write FOnBackgroundDraw;
    property OnBeforeDrawItem: TBeforeDrawResponsiveListItemEvent read FOnBeforeDrawItem write FOnBeforeDrawItem;
    property OnBeforeSelectItem: TResponsiveListItemAllowEvent read FOnBeforeSelectItem write FOnBeforeSelectItem;
    property OnItemAnchorClick: TResponsiveListItemAnchorEvent read FOnItemAnchorClick write FOnItemAnchorClick;
    property OnItemClick: TResponsiveListItemEvent read FOnItemClick write FOnItemClick;
    property OnItemControlClick: TResponsiveListItemControlEvent read FOnItemControlClick write FOnItemControlClick;
    property OnItemEnter: TResponsiveListItemEvent read FOnItemEnter write FOnItemEnter;
    property OnItemFilter: TResponsiveListItemFilterEvent read FOnItemFilter write FOnItemFilter;
    property OnItemLeave: TResponsiveListItemEvent read FOnItemLeave write FOnItemLeave;
    property OnConditionChange: TResponsiveConditionEvent read FOnConditionChange write FOnConditionChange;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    {$IFNDEF FMXLIB}
    property OnKeyPress;
    {$ENDIF}
    property OnResize;
  end;

implementation

uses
  AdvGraphicsHTMLEngine, SysUtils, AdvStyleIF
  {$IFDEF VCLLIB}
  , ShellApi
  {$ENDIF}
  {$IFDEF USEVCLSTYLES}
  , VCL.Themes
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.TMSFNCStyles
  {$ENDIF}
  ;

const
  MOVE_OFFSET = 4;

function RectIntersects(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left < R2.Right)
        and (R1.Right > R2.Left)
        and (R1.Top < R2.Bottom)
        and (R1.Bottom > R2.Top);
end;

function RenderTemplate(Template: string; AItem: TResponsiveListItem): string;
var
  beforetag,aftertag,fld,dbfld: string;
  i,j:integer;
  v: variant;
begin
  beforetag:='';
  while Pos('(#',Template) > 0 do
  begin
    i := pos('(#',Template);
    beforetag := beforetag+copy(Template,1,i-1); //part prior to the tag
    aftertag := copy(Template,i,length(Template)); //part after the tag
    j := pos(')',aftertag);
    fld := copy(aftertag,1,j-1);
    Delete(fld,1,2);
    Delete(Template,1,i+j-1);

    dbfld := '';

    v := AItem.Values[fld];
    if v <> Null then
      dbfld := v
    else
      dbfld := '('+fld+')';

    beforetag := beforetag + dbfld;
  end;

  Result := beforetag + Template;
end;


{ TAdvResponsiveList }

{$IFNDEF FMXLIB}
procedure TAdvResponsiveList.AlignControls({%H-}AControl: TControl; var {%H-}Rect: TRect);
var
  i: integer;
  it,itc: TResponsiveListItem;
  DoUpdate: boolean;
begin
  RenderItems;

  DoUpdate := false;

  // update control - item link
  if (csDesigning in ComponentState) then
  begin
    for i := 0 to ControlCount - 1 do
    begin
      if Controls[i].Align = alNone then
      begin
        it := FindItemAtPoint(Point(Controls[i].Left, Controls[i].Top));

        if Assigned(it) and not Assigned(it.Control) then
        begin
          itc := FindControlItem(Controls[i]);
          if Assigned(itc) then
            itc.Control := nil;
          it.Control := Controls[i];
          DoUpdate := true;
        end;
      end;
    end;
  end;

  // clear items' control in case child controls were destroyed
  for i := 0 to Items.Count - 1 do
    Items[i].FControl := nil;

  ResolveControls;

  UpdateControls;

  if DoUpdate then
    Refresh;
end;
{$ENDIF}

procedure TAdvResponsiveList.UpdateControls;
var
  i,hp, vp: integer;
begin
//  vp := GetScrollPos(Self.Handle, SB_VERT);
//  hp := GetScrollPos(Self.Handle, SB_HORZ);

  vp := TopLeft.y;
  hp := TopLeft.x;

  // do align of controls in items
  for i := 0 to Items.Count - 1 do
  begin
    if Assigned(Items[i].Control) then
    begin
      {$IFDEF FMXLIB}
      if Items[i].Control.Align = TAlignLayout.Client then
      {$ENDIF}
      {$IFDEF VCLLIB}
      if Items[i].Control.Align = alClient then
      {$ENDIF}
        Items[i].Control.SetBounds(Items[i].Rect.Left - hp,Items[i].Rect.Top - vp,
          Items[i].Rect.Right - Items[i].Rect.Left, Items[i].Rect.Bottom - Items[i].Rect.Top);
    end;
  end;

end;


function TAdvResponsiveList.AnchorAtPoint(AItem: TResponsiveListItem;
  pt: TPoint; var Anchor: string): boolean;
var
  sa, s, template: string;
  cr: TRect;
  gr: TAdvGraphics;

begin
  cr := AItem.ContentRect;

  if FActiveTemplate <> '' then
    template := FActiveTemplate
  else
    template := AItem.Content;

  s := RenderTemplate(template, AItem);

  gr := TAdvGraphics.CreateBitmapCanvas;
  gr.Font.Assign(Font);

  sa := gr.DrawText(ConvertToRectF(cr), s, true, gtaLeading, gtaLeading, gttNone, 0, -1, -1, true, true, pt.X, pt.Y);

  gr.Free;

  Result := (sa <> '');

  Anchor := sa;
end;

function TAdvResponsiveList.ControlAtPoint(AItem: TResponsiveListItem;
  pt: TPoint; var ControlID, ControlValue, ControlType: string): boolean;
var
  sa, s, template: string;
  cr: TRect;
  gr: TAdvGraphics;
  CID, CV, CT: string;

begin
  cr := AItem.ContentRect;

  if FActiveTemplate <> '' then
    template := FActiveTemplate
  else
    template := AItem.Content;

  s := RenderTemplate(template, AItem);

  gr := TAdvGraphics.CreateBitmapCanvas;
  gr.Font.Assign(Font);
  CID := '';
  CV := '';
  CT := '';

  sa := gr.DrawText(ConvertToRectF(cr), s, CID, CV, CT, true, gtaLeading, gtaLeading, gttNone, 0, -1, -1, true, true, pt.X, pt.Y);

  gr.Free;

  Result := (sa <> '') and (CID <> '');

  if Result then
  begin
    ControlID := CID;
    ControlValue := CV;
    ControlType := CT;
  end;
end;


procedure TAdvResponsiveList.AppearanceChanged(Sender: TObject);
var
  i: integer;
begin
  if Appearance.ItemsApplying = iaAll then
  begin
    for i := 0 to Items.Count - 1 do
      Items.Init(Items[i]);
  end;
  Refresh;
end;

procedure TAdvResponsiveList.Assign(Source: TPersistent);
begin
  if (Source is TAdvResponsiveList) then
  begin
    Appearance.Assign((Source as TAdvResponsiveList).Appearance);
    Conditions.Assign((Source as TAdvResponsiveList).Conditions);
    Items.Assign((Source as TAdvResponsiveList).Items);
    ResolveControls;
    Realign;
  end;
end;

procedure TAdvResponsiveList.Backspace;
begin
end;

procedure TAdvResponsiveList.DoEnter;
begin
  inherited;
  Refresh;
end;


procedure TAdvResponsiveList.DoExit;
begin
  inherited;
  Refresh;
end;


procedure TAdvResponsiveList.BeginUpdate;
begin
  {$IFDEF FMXLIB}
  inherited;
  {$ENDIF}
  Items.BeginUpdate;
end;

procedure TAdvResponsiveList.UpdateFilter;
var
  i: integer;
  cond: TResponsiveCondition;
begin
  BeginUpdate;

  try
    if FilterCondition.Text = '' then
      ClearFilter
    else
    begin
      cond := GetCondition;

      if not FilterCondition.CaseSensitive then
        FilterCondition.Condition := UpperCase(FilterCondition.Text);

      for i := 0 to Items.Count - 1 do
      begin
        Items[i].Filtered := FilterItem(cond, Items[i]);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TAdvResponsiveList.ClearFilter;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].Filtered := false;
end;

function FilterText(Condition, Text: string; CaseSensitive: boolean; FilterType: TResponsiveListItemFilterType): boolean;
var
  p, diff: integer;
begin
  Result := true;

  if not CaseSensitive then
    Text := UpperCase(Text);

  case FilterType of
  mText: Result := Pos(Condition, Text) > 0;
  mStartWord: Result := Pos(Condition, Text) = 1;
  mEndWord:
    begin
      diff := Length(Text) - Length(Condition);
      if diff = 0 then
        Result := (Condition = Text)
      else
      begin
        p := Pos(Condition, Text);
        Result := (p > 0) and (p = diff + 1);
      end;
    end;
  end;
end;


function TAdvResponsiveList.FilterItem(Condition: TResponsiveCondition; Item: TResponsiveListItem): boolean;
var
  s: string;
begin
  if fdContent in FilterCondition.FilterData then
  begin
    if Condition.Template <> '' then
      s := RenderTemplate(Condition.Template, Item)
    else
      s := Item.Content;

    s := TAdvUtils.HTMLStrip(s);

    Result := not FilterText(FilterCondition.Condition, s, FilterCondition.CaseSensitive, FilterCondition.FilterType);
    if not Result then
      Exit;
  end;

  if fdHeader in FilterCondition.FilterData then
  begin
    if Condition.HeaderTemplate <> '' then
      s := RenderTemplate(Condition.HeaderTemplate, Item)
    else
      s := Item.Content;

    s := TAdvUtils.HTMLStrip(s);

    Result := not FilterText(FilterCondition.Condition, s, FilterCondition.CaseSensitive, FilterCondition.FilterType);
    if not Result then
      Exit;
  end;

  if fdFooter in FilterCondition.FilterData then
  begin
    if Condition.FooterTemplate <> '' then
      s := RenderTemplate(Condition.FooterTemplate, Item)
    else
      s := Item.Content;

    s := TAdvUtils.HTMLStrip(s);

    Result := not FilterText(FilterCondition.Condition, s, FilterCondition.CaseSensitive, FilterCondition.FilterType);
    if not Result then
      Exit;
  end;

  Result := not DoItemFilter(Item);
end;

{$IFNDEF DELPHIXE10_LVL}
procedure TAdvResponsiveList.ChangeScale(M, D: Integer);
{$ENDIF}
{$IFDEF DELPHIXE10_LVL}
procedure TAdvResponsiveList.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ENDIF}
begin
  inherited;

  FDPIScale := GetDPIScale(Self, Canvas);

  RenderItems;
  ResolveControls;
  ReAlign;
end;

{$IFNDEF FMXLIB}

procedure TAdvResponsiveList.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

{$IFDEF USEVCLSTYLES}
procedure TAdvResponsiveList.CMStyleChanged(var Message: TMessage);
begin
  InitVCLStyle(true);
end;
{$ENDIF}

procedure TAdvResponsiveList.CMMouseLeave(var {%H-}Message: TMessage);
begin
  if FHoverIndex <> -1 then
  begin
    DoItemLeave(Items[FHoverIndex]);
    FHoverIndex := -1;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TAdvResponsiveList.ClearSelection;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].Selected := false;
end;


procedure TAdvResponsiveList.ConditionsChanged(Sender: TObject);
begin
  RenderItems;
  Refresh;
end;


procedure TAdvResponsiveList.CopyToClipboard;
begin
end;

constructor TAdvResponsiveList.Create(AOwner: TComponent);
var
  cond: TResponsiveCondition;
  li: TResponsiveListItem;
  i: integer;
begin
  inherited;
  FDPIScale := 1;

  {$IFDEF FMXLIB}
  ClipChildren := true;
  {$ENDIF}

  FListItems := CreateListItems;
  FListItems.OnChange := ItemsChanged;

  FConditions := CreateConditions;
  FConditions.OnChange := ConditionsChanged;

  FFilterCondition := TResponsiveListItemFilter.Create;

  FAppearance := TResponsiveListAppearance.Create;
  FAppearance.OnChange := AppearanceChanged;

  FXYDown := Point(-1,-1);

  {$IFDEF VCLLIB}
  {$IFNDEF FNCLIB}
  FImageCache := THTMLPictureCache.Create;
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF FMXLIB}
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque] - [csParentBackground];
  {$ENDIF}

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    // Always 3 columns
    cond := FConditions.Add;
    cond.Columns := 3;
    cond.WidthFrom := 0;
    cond.WidthTo := -1;

    for i := 1 to 3 do
    begin
      li := FListItems.Add;
      li.HeaderText := 'Item ' + inttostr(i);
      li.Content := 'Content of the item goes here ...';
    end;
  end;

  FConditionCategory := -1;
  FMultiSelect := false;
  FHoverIndex := -1;
  FItemIndex := -1;
  TabStop := true;
  Width := 300;
  Height := 200;

  {$IFDEF FNCLIB}
  ReadOnly := true;
  CreateTextService;
  {$ENDIF}
end;

function TAdvResponsiveList.CreateConditions: TResponsiveConditions;
begin
  Result := TResponsiveConditions.Create(Self, GetConditionClass);
end;

function TAdvResponsiveList.CreateListItems: TResponsiveListItems;
begin
  Result := TResponsiveListItems.Create(Self, GetItemClass);
end;

{$IFNDEF FMXLIB}
procedure TAdvResponsiveList.CreateWnd;
begin
  inherited;
  FDPIScale := GetDPIScale(Self, Canvas);
  RenderItems;
  ResolveControls;
  ReAlign;
  SetBounds(Left,Top,Width,Height);
  {$IFDEF USEVCLSTYLES}
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    InitVCLStyle(false);
  {$ENDIF}

end;
{$ENDIF}

procedure TAdvResponsiveList.CutToClipboard;
begin
end;


procedure TAdvResponsiveList.DeleteSelection;
begin
end;

destructor TAdvResponsiveList.Destroy;
begin
  FFilterCondition.Free;
  {$IFDEF VCLLIB}
  {$IFNDEF FNCLIB}
  FImageCache.Free;
  {$ENDIF}
  {$ENDIF}
  FAppearance.Free;
  FConditions.Free;
  FListItems.Free;
  inherited;
end;

procedure TAdvResponsiveList.DoAfterSelectItem(AItem: TResponsiveListItem);
begin
  if Assigned(OnAfterSelectItem) then
    OnAfterSelectItem(Self, AItem);
end;

procedure TAdvResponsiveList.DoBeforeSelectItem(AItem: TResponsiveListItem;
  var Allow: boolean);
begin
  if Assigned(OnBeforeSelectItem) then
    OnBeforeSelectItem(Self, AItem, Allow);
end;

procedure TAdvResponsiveList.DoConditionChange(
  ACondition: TResponsiveCondition);
begin
  if Assigned(OnConditionChange) then
    OnConditionChange(Self, ACondition);
end;

procedure TAdvResponsiveList.DoItemAnchorClick(AItem: TResponsiveListItem;
  Anchor: string);
var
  defaulthandling: boolean;
begin
  //
  defaulthandling := true;

  if Assigned(OnItemAnchorClick) then
    OnItemAnchorClick(Self, AItem, Anchor, defaulthandling);

  {$IFDEF VCLLIB}
  if defaulthandling then
    ShellExecute(0,'open',PChar(Anchor),nil,nil,SW_SHOW);
  {$ENDIF}
end;

procedure TAdvResponsiveList.DoItemControlClick(AItem: TResponsiveListItem;
  ControlID, ControlValue, ControlType: string);
begin
  if Assigned(OnItemControlClick) then
    OnItemControlClick(Self, AItem, ControlID, ControlValue, ControlType);
end;

procedure TAdvResponsiveList.DoItemClick(AItem: TResponsiveListItem);
begin
  if Assigned(OnItemClick) then
    OnItemClick(Self, AItem);
end;

procedure TAdvResponsiveList.DoItemEnter(AItem: TResponsiveListItem);
begin
  AItem.Hovered := true;
  if Assigned(OnItemEnter) then
    OnItemEnter(Self, AItem);
end;

procedure TAdvResponsiveList.DoItemLeave(AItem: TResponsiveListItem);
begin
  AItem.Hovered := false;
  if Assigned(OnItemLeave) then
    OnItemLeave(Self, AItem);
end;

function TAdvResponsiveList.DoItemFilter(AItem: TResponsiveListItem): boolean;
begin
  Result := false;
  if Assigned(OnItemFilter) then
    OnItemFilter(Self, AItem, Result);
end;

procedure TAdvResponsiveList.DoSelectionChanged;
begin
end;

procedure TAdvResponsiveList.DoSelectItem(AIndex: Integer);
var
  Allow: boolean;
begin
  if (AIndex >= 0) and (AIndex < Items.Count) and (AIndex <> ItemIndex) then
  begin
    Allow := true;
    DoBeforeSelectItem(Items[AIndex],Allow);

    if Allow then
    begin
      ItemIndex := AIndex;
      DoAfterSelectItem(Items[AIndex]);
    end;
  end;

end;

procedure TAdvResponsiveList.DrawItem(AGraphics: TAdvGraphics;
  AItem: TResponsiveListItem; ATemplate, AHeaderTemplate, AFooterTemplate: string; ARect: TRect; Focus: boolean);
var
  DoDefaultDraw: boolean;
begin
  DoDefaultDraw := true;

  if Assigned(OnBeforeDrawItem) then
    OnBeforeDrawItem(Self, AGraphics, AItem, ARect, DoDefaultDraw);

  if DoDefaultDraw then
  begin
    AItem.PictureContainer := FPictureContainer;
    {$IFNDEF FMXLIB}
    AItem.Images := FImages;
    {$ENDIF}
    AItem.DrawItem(AGraphics, ATemplate, AHeaderTemplate, AFooterTemplate, ARect, Focus);

    if Assigned(AItem.Control) and (csDesigning in ComponentState) then
    begin
      AGraphics.Stroke.Color := clRed;
      AGraphics.Fill.Kind := gfkNone;
      AGraphics.DrawRectangle(ConvertToRectf(ARect));
    end;

    if Assigned(OnAfterDrawItem) then
      OnAfterDrawItem(Self, AGraphics, AItem, ARect);
  end;
end;

procedure TAdvResponsiveList.EndUpdate;
begin
  {$IFDEF FMXLIB}
  inherited;
  {$ENDIF}
  Items.EndUpdate;
  RenderItems;
  SetRange(FMaxSize);
  Refresh;
end;

function TAdvResponsiveList.FindControlItem(
  AControl: TControl): TResponsiveListItem;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].Control = AControl then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TAdvResponsiveList.FindItemAtPoint(pt: TPoint): TResponsiveListItem;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].IsVisible then
    begin
      if PtInRect(Items[i].Rect, pt) then
      begin
        Result := Items[i];
        Break;
      end;
    end;
  end;
end;

function TAdvResponsiveList.FixedColumns(FromItem, Count: Integer): Integer;
var
  i: integer;
begin
  Result := 0;

  for i := FromItem to FromItem + Count - 1 do
  begin
    if i < Items.Count then
    begin
      if Items[i].WidthType = isFixed then
        inc(Result);
    end;
  end;
end;

function TAdvResponsiveList.FixedHeight(FromItem, Count: Integer): Integer;
var
  i: integer;
begin
  Result := 0;

  for i := FromItem to FromItem + Count - 1 do
  begin
    if i < Items.Count then
    begin
      if Items[i].HeightType = isFixed then
        Result := Result + Items[i].Height;
    end;
  end;
end;

function TAdvResponsiveList.FixedRows(FromItem, Count: Integer): Integer;
var
  i: integer;
begin
  Result := 0;

  for i := FromItem to FromItem + Count - 1 do
  begin
    if i < Items.Count then
    begin
      if Items[i].HeightType = isFixed then
        inc(Result);
    end;
  end;
end;

function TAdvResponsiveList.FixedWidth(FromItem, Count: Integer): Integer;
var
  i: integer;
begin
  Result := 0;

  for i := FromItem to FromItem + Count - 1 do
  begin
    if i < Items.Count then
    begin
      if Items[i].WidthType = isFixed then
        Result := Result + Items[i].Width;
    end;
  end;
end;

function TAdvResponsiveList.GetCaretLH: integer;
begin
  Result := 0;
end;

function TAdvResponsiveList.GetCaretXY: TPoint;
begin
  Result := Point(-1,-1);
end;

function TAdvResponsiveList.GetClientHeight: integer;
begin
  {$IFNDEF FMXLIB}
  if HandleAllocated then
  {$IFDEF LCLLIB}
    Result := ClientHeight  - BorderSize - HScrollHeight
  {$ENDIF}
  {$IFNDEF LCLLIB}
    Result := ClientHeight
  {$ENDIF}
  else
    Result := Height;
  {$ENDIF}

  {$IFDEF FMXLIB}
  Result := Round(Height) - ScrollSizeHorz;
  {$ENDIF}
end;

function TAdvResponsiveList.GetClientWidth: integer;
begin
  {$IFNDEF FMXLIB}
  if HandleAllocated then
  {$IFDEF LCLLIB}
    Result := ClientWidth - BorderSize - VScrollWidth
  {$ENDIF}
  {$IFNDEF LCLLIB}
    Result := ClientWidth
  {$ENDIF}
  else
    Result := Width;
  {$ENDIF}

  {$IFDEF FMXLIB}
  Result := Round(Width) - ScrollSizeVert;
  {$ENDIF}
end;

function TAdvResponsiveList.GetCondition: TResponsiveCondition;
var
  i: integer;
begin
  Result := nil;

  // default condition
  if Conditions.Count > 0 then
    Result := Conditions[0];

  for i := 0 to Conditions.Count - 1 do
  begin
    if MatchFromTo(Round(Width), Round(FDPIScale * Conditions[i].WidthFrom), Round(FDPIScale * Conditions[i].WidthTo)) and
       MatchFromTo(Round(Height), Round(FDPIScale * Conditions[i].HeightFrom), Round(FDPIScale * Conditions[i].HeightTo)) then
    begin
      if (ConditionCategory = -1) or (Conditions[i].Category = ConditionCategory) then
      begin
        // stop at first match
        Result := Conditions[i];
        break;
      end;
    end;
  end;
end;

function TAdvResponsiveList.GetConditionClass: TCollectionItemClass;
begin
  Result := TResponsiveCondition;
end;

function TAdvResponsiveList.GetCursorEx: TCursor;
begin
  Result := inherited Cursor;
end;

function TAdvResponsiveList.GetDocURL: string;
begin
  Result := TTMSFNCResponsiveListDocURL;
end;

function TAdvResponsiveList.GetTipsURL: string;
begin
  Result := TTMSFNCResponsiveListTipsURL;
end;

function TAdvResponsiveList.GetElementCount: integer;
begin
  Result := 0;
end;

function TAdvResponsiveList.GetItemClass: TCollectionItemClass;
begin
  Result := TResponsiveListItem;
end;

function TAdvResponsiveList.GetSelectionFromXY: TPoint;
begin
   Result := Point(-1,-1);
end;

function TAdvResponsiveList.GetSelectionToXY: TPoint;
begin
   Result := Point(-1,-1);
end;

function TAdvResponsiveList.GetTabStopEx: boolean;
begin
  Result := inherited TabStop;
end;

function TAdvResponsiveList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvResponsiveList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvResponsiveList.GetWordAndIndexAtCaret(var AValue: string;
  var AIndex: integer; SpaceOnly: boolean = false);
begin
end;

function TAdvResponsiveList.HasSelection: boolean;
begin
  Result := false;
end;

function TAdvResponsiveList.HasTextContent: boolean;
begin
  Result := false;
end;

{$IFDEF FNCLIB}
{$IFNDEF FMXLIB}
procedure TAdvResponsiveList.SetAdaptToStyle(const Value: Boolean);
begin
  inherited;
{$IFDEF USEVCLSTYLES}
  InitVCLStyle(not Value);
{$ENDIF}
end;
{$ENDIF}
{$ENDIF}


{$IFDEF FMXLIB}
procedure TAdvResponsiveList.InitStyle;
var
  c: TAlphaColor;

begin
  inherited;
  c := claNull;

  if TTMSFNCStyles.GetStyleBackgroundFillColor(c) then
  begin
    Color := c;
    Appearance.ItemColor := c;
  end;

  c := claNull;

  if TTMSFNCStyles.GetStyleLineFillColor(c) then
  begin
    Stroke.Color := c;
    Appearance.ItemBorderColor := c;
  end;

  c := claNull;

  if TTMSFNCStyles.GetStyleTextFontColor(c) then
  begin
    Font.Color := c;
    Appearance.ItemTextColor := c
  end;

  c := claNull;

  if TTMSFNCStyles.GetStyleSelectionFillColor(c) then
    Appearance.SelectedItemColor := c;

  c := claNull;

  if TTMSFNCStyles.GetStyleAlternativeTextFontColor(c) then
    Appearance.SelectedItemTextColor := c;

  Appearance.ItemHeaderColor := Appearance.SelectedItemColor;
  Appearance.ItemHeaderTextColor := Appearance.SelectedItemTextColor;

end;

procedure TAdvResponsiveList.ResetToDefaultStyle;
begin
  inherited;
  Color := claWhite;
  Stroke.Color := claGray;
  Font.Color := claBlack;
  Appearance.SelectedItemColor := claBlue;
  Appearance.SelectedItemTextColor := claWhite;
  Appearance.ItemColor := claWhite;
  Appearance.ItemTextColor := claBlack;
  Appearance.ItemHeaderColor := Appearance.SelectedItemColor;
  Appearance.ItemHeaderTextColor := Appearance.SelectedItemTextColor;
end;
{$ENDIF}


{$IFDEF USEVCLSTYLES}
procedure TAdvResponsiveList.InitVCLStyle(init: boolean);
var
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  clr: TColor;
begin
  {$IFDEF FNCLIB}
  if not AdaptToStyle then
    Exit;
  {$ENDIF}

  UseVCLStyles := False;

  LStyle := StyleServices;

  if LStyle.Enabled and (LStyle.Name <> 'Windows') then
  begin
    UseVCLStyles := True;

    LDetails := LStyle.GetElementDetails(tgCellNormal);
    LStyle.GetElementColor(LDetails, ecFillColor, clr);

    {$IFDEF DELPHIXE6_LVL}
    if seClient in StyleElements then
    {$ENDIF}
    begin
      Color := clr;
      Appearance.ItemColor := clr;
    end;

    LStyle.GetElementColor(LDetails, ecBorderColor, clr);
    {$IFDEF DELPHIXE6_LVL}
    if seBorder in StyleElements then
    {$ENDIF}
    begin
      BorderColor := clr;
      Appearance.ItemBorderColor := clr;
    end;

    if LStyle.GetElementColor(LStyle.GetElementDetails(tgFixedCellNormal), ecFillColor, clr) and (clr <> clNone) then
      Appearance.ItemHeaderColor := clr
    else
      Appearance.ItemHeaderColor := Appearance.SelectedItemColor;

    if LStyle.GetElementColor(LStyle.GetElementDetails(tgFixedCellNormal), ecTextColor, clr) and (clr <> clNone) then
      Appearance.ItemHeaderTextColor := clr
    else
      Appearance.ItemHeaderTextColor := Appearance.SelectedItemTextColor;


    if LStyle.GetElementColor(LStyle.GetElementDetails(teEditTextNormal), ecTextColor, clr) and (clr <> clNone) then
    begin
      {$IFDEF DELPHIXE6_LVL}
      if seFont in StyleElements then
      {$ENDIF}
      begin
        Font.Color := clr;
        Appearance.ItemTextColor := clr;
      end;
    end;

    if LStyle.GetElementColor(LStyle.GetElementDetails(tgCellSelected), ecFilLColor, clr) and (clr <> clNone) then
    begin
      Appearance.SelectedItemColor := clred; //clr;
    end
    else
      Appearance.SelectedItemColor := clHighlight;

    if LStyle.GetElementColor(LStyle.GetElementDetails(tgCellSelected), ecTextColor, clr) and (clr <> clNone) then
    begin
      Appearance.SelectedItemTextColor := clr;
    end
    else
      Appearance.SelectedItemTextColor := clHighlightText;
  end
  else
  begin
    if init then
    begin
      Color := clWindow;
      BorderColor := clGray;
      Font.Color := clWindowText;
      Appearance.SelectedItemColor := clHighlight;
      Appearance.SelectedItemTextColor := clHighlightText;
      Appearance.ItemColor := clWindow;
      Appearance.ItemTextColor := clWindowText;
      Appearance.ItemHeaderColor := Appearance.SelectedItemColor;
      Appearance.ItemHeaderTextColor := Appearance.SelectedItemTextColor;
    end;
  end;

end;
{$ENDIF}

procedure TAdvResponsiveList.InsertChar(ch: char);
begin
end;

procedure TAdvResponsiveList.InsertChar(value: string);
begin
end;

function TAdvResponsiveList.IsForwardSelection: boolean;
begin
  Result := false;
end;

procedure TAdvResponsiveList.ItemsChanged(Sender: TObject);
begin
  RenderItems;
  SetRange(FMaxsize);
  Refresh;
end;

procedure TAdvResponsiveList.KeyDownN(var {%H-}Key: Word; {%H-}Shift: TShiftState);
var
  idx: integer;
begin
  // nothing to do
  if Items.Count = 0 then
    Exit;

  idx := ItemIndex;

  case Key of
  KEY_UP:
    begin
      if ItemIndex >= 0 then
      begin
        if Items[ItemIndex].Row > 0 then
          DoSelectItem(ItemIndex - ColCount);
      end;
    end;
  KEY_DOWN:
    begin
      if ItemIndex >= 0 then
      begin
        if (Items[ItemIndex].Row <= RowCount) then
        begin
          DoSelectItem(ItemIndex + ColCount);
        end;
      end;
    end;
  KEY_RIGHT:
    begin
      if ItemIndex >= 0 then
      begin
        if ItemIndex < Items.Count - 1 then
          DoSelectItem(ItemIndex + 1);
      end;
    end;
  KEY_LEFT:
    begin
      if ItemIndex > 0 then
      begin
        DoSelectItem(ItemIndex - 1);
      end;
    end;
  KEY_HOME:
    begin
      if ItemIndex >= 0 then
        DoSelectItem(0);
    end;
  KEY_END:
    begin
      if ItemIndex >= 0 then
        DoSelectItem(Items.Count - 1);
    end;
  KEY_PRIOR:
    begin
      if ItemIndex >= 0 then
      begin
        if Items[ItemIndex].Row > 2 then
          DoSelectItem(ItemIndex - 2 * ColCount)
        else
          DoSelectItem(ItemIndex mod ColCount);
      end;

    end;
  KEY_NEXT:
    begin
      if ItemIndex >= 0 then
      begin
        if (Items[ItemIndex].Row <= RowCount - 2 * ColCount) then
          DoSelectItem(ItemIndex + 2 * ColCount)
        else
          DoSelectItem((RowCount - 1) * ColCount + (ItemIndex mod ColCount));
      end;
    end;
  KEY_SPACE:
    begin
      if ItemIndex = -1 then
        DoSelectItem(0);
    end;
  end;

  if (idx <> ItemIndex) and (ItemIndex >= 0) then
    ScrollInView(Items[ItemIndex]);
end;

procedure TAdvResponsiveList.KeyPressN(var Key: Char);
begin
  // do nothing
end;

procedure TAdvResponsiveList.KeyUpN(var Key: Word; Shift: TShiftState);
begin
  // do nothing
end;

procedure TAdvResponsiveList.Loaded;
begin
  inherited;
end;

function TAdvResponsiveList.MatchFromTo(AValue, AFrom, ATo: Integer): boolean;
begin
  Result := ((AValue >= AFrom) or (AFrom <= -1)) and ((AValue < ATo) or (ATo <= -1))
end;

procedure TAdvResponsiveList.MouseDownN({%H-}Button: TMouseButton;
  {%H-}Shift: TShiftState; X, Y: Integer);
var
  Anchor: string;
  CID, CV, CT, C: string;
begin
  if AllowFocus then
    SetFocus;

  FXYDown := Point(X,Y);
  FItemDown := FindItemAtPoint(Point(X + TopLeft.X,Y + TopLeft.Y));

  if Assigned(FItemDown) then
  begin
    Anchor := '';
    CID := '';
    CV := '';
    CT := '';

    if ControlAtPoint(FItemDown, Point(X, Y), CID, CV, CT) then
    begin
      C := FItemDown.Content;

      if CT = 'CHECK' then
      begin

        if CV = 'TRUE' then
          SetControlValue(C, CID, 'FALSE')
        else
          SetControlValue(C, CID, 'TRUE');

        FItemDown.Content := C;
      end;

      if CT = 'RADIO' then
      begin
        C := ClearRadioControls(C);

        SetControlValue(C, CID, 'TRUE');

        FItemDown.Content := C;
      end;

      if CT = 'BUTTON' then
      begin

      end;

      FXYDown := Point(-1,-1);
      FItemDown := nil;
      DoItemControlClick(FItemDown, CID, CV, CT);
    end
    else
    if AnchorAtPoint(FItemDown, Point(X, Y), Anchor) then
    begin
      DoItemAnchorClick(FItemDown, Anchor);
    end;
  end;
end;

procedure TAdvResponsiveList.MouseMoveN({%H-}Shift: TShiftState; X, Y: Integer);
var
  it: TResponsiveListItem;
  Anchor: string;
  delta: integer;
  tl: TPoint;
begin
  if (FXYDown.X <> -1) and (FXYDown.Y <> -1) then
  begin
    if (Abs(FXYDown.X - X) > MOVE_OFFSET) or (Abs(FXYDown.Y - Y) > MOVE_OFFSET) then
    begin
      delta := FXYDown.Y - Y;

      tl := TopLeft;

      if HasVerticalScrollBar then
        tl.Y := TopLeft.Y + delta;

      delta := FXYDown.X - X;

      if HasHorizontalScrollBar then
        tl.X := TopLeft.X + delta;

      FXYMoving := true;
      FXYDown := Point(X,Y);

      TopLeft := tl;
      Exit;
    end;
  end;

  it := FindItemAtPoint(Point(X + TopLeft.X,Y + TopLeft.Y));

  if not Assigned(it) then
  begin
    if Cursor = crHandPoint then
      inherited Cursor := FOldCursor;

    if (FHoverIndex > -1) and (FHoverIndex < Items.Count) then
      DoItemLeave(Items[FHoverIndex]);

    FHoverIndex := -1;

    Refresh;
  end
  else
  begin
    if (FHoverIndex <> it.Index) and (FHoverIndex >= 0) then
      DoItemLeave(Items[FHoverIndex]);

    if (FHoverIndex <> it.Index) then
    begin
      FHoverIndex := it.Index;
      DoItemEnter(it);

      Refresh;
    end;

    Anchor := '';

    if AnchorAtPoint(it, Point(X, Y), Anchor) then
    begin
      inherited Cursor := crHandPoint;
    end
    else
      if Cursor = crHandPoint then
        inherited Cursor := FOldCursor;
  end;
end;

procedure TAdvResponsiveList.MouseUpN({%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
  X, Y: Integer);
var
  FItemUp: TResponsiveListItem;
  i,idx: integer;
begin
  FXYDown := Point(-1,-1);
  FItemUp := FindItemAtPoint(Point(X + TopLeft.X,Y + TopLeft.Y));

  if (FItemUp = FItemDown) and Assigned(FItemUp) and not FXYMoving then
  begin
    if not MultiSelect or (Shift = []) then
    begin
      if MultiSelect then
        ClearSelection;

      idx := FItemUp.Index;
      DoSelectItem(idx);
      Refresh;
      DoItemClick(FItemUp);

      if Items.Count < idx then
        idx := -1;
      FLastItemClick := idx;
    end
    else
    begin
      if (ssCtrl in Shift) then
      begin
        FItemUp.FSelected := not FItemUp.FSelected;
        if FItemUp.FSelected then
        begin
          FLastItemClick := FItemUp.Index;
        end
        else
          FLastItemClick := -1;
      end;

      if (ssShift in Shift) and (FLastItemClick <> -1) then
      begin
        if FItemUp.Index > FLastItemClick then
        begin
          for i := FLastItemClick to FItemUp.Index do
            Items[i].FSelected := true;
        end
        else
        begin
          for i := FLastItemClick downto FItemUp.Index do
            Items[i].FSelected := true;
        end;

      end;

      Refresh;
      DoItemClick(FItemUp);
    end;
  end;
  FXYMoving := false;
end;

procedure TAdvResponsiveList.DoDblClick({%H-}X, {%H-}Y: integer);
begin
  //
end;

procedure TAdvResponsiveList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  {$IFNDEF FMXLIB}
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  {$ENDIF}

  if (AOperation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;

  inherited;
end;

procedure TAdvResponsiveList.PaintBackground(AGraphics: TAdvGraphics);
begin
  if Assigned(OnBackgroundDraw) then
    OnBackgroundDraw(Self, AGraphics, ClientRect)
  else
  begin
    AGraphics.Fill.Color := Color;
    AGraphics.Fill.Kind := gfkSolid;
    AGraphics.Stroke.Color := Color;

    {$IFDEF FMXLIB}
    if BorderStyle = bsSingle then
      AGraphics.Stroke.Color := clGray;
    {$ENDIF}

    AGraphics.DrawRectangle(RectF(0,0,Width,Height));
  end;
end;

procedure TAdvResponsiveList.Paint;
var
  i: Integer;
  dr: TRect;
  cond: TResponsiveCondition;
  gr, grb: TAdvGraphics;
  sel: integer;

begin
  inherited;

  if Conditions.Count = 0 then
  begin
    gr := TAdvGraphics.Create(Canvas);
    try
      PaintBackground(gr);
    finally
      gr.Free;
    end;
    Exit;
  end;

  FActiveHeaderTemplate := '';
  FActiveFooterTemplate := '';

  cond := RenderItems;

  {$IFNDEF FMXLIB}
  grb := TAdvGraphics.CreateBitmapCanvas(Round(Width), Round(Height));
  gr := TAdvGraphics.Create(Canvas);
  {$ENDIF}

  {$IFDEF FMXLIB}
  grb := TAdvGraphics.Create(Canvas);
  {$ENDIF}

  try
    {$IFNDEF FMXLIB}
    grb.BeginScene;
    grb.ImageList := FImages;
    {$ENDIF}

    PaintBackground(grb);

    grb.Font.Assign(Font);

    sel := ItemIndex;

    for i := 0 to Items.Count - 1 do
    begin
      if Items[i].IsVisible then
      begin
        if (sel = -1) then
          sel := i;

        grb.Fill.Color := Items[i].Color;

        if Assigned(Items[i].Control) and (csDesigning in ComponentState) then
          grb.Fill.Color := clRed;

        dr := Items[i].Rect;

        OffsetRect(dr, -TopLeft.X, -TopLeft.Y);

        if Assigned(cond) then
        begin
          FActiveTemplate := cond.Template;
          FActiveFooterTemplate := cond.FooterTemplate;
          FActiveHeaderTemplate := cond.HeaderTemplate;
        end
        else
        begin
          FActiveTemplate := Items[i].Content;
        end;

        if RectIntersects(dr, ClientRect) then
          DrawItem(grb, Items[i], FActiveTemplate, FActiveHeaderTemplate, FActiveFooterTemplate, dr, Focused and (sel = i));
      end;
    end;

    {$IFDEF FMXLIB}
    grb.Fill.Kind := gfkNone;
    if BorderStyle = bsSingle then
      grb.Stroke.Color := clGray;
    grb.DrawRectangle(RectF(0,0,Width,Height));
    {$ENDIF}

    {$IFNDEF FMXLIB}
    grb.EndScene;
    gr.DrawBitmap(RectF(0,0,Width,Height), grb.Bitmap);
    {$ENDIF}

  finally
    {$IFNDEF FMXLIB}
    gr.Free;
    {$ENDIF}
    grb.Free;
  end;
end;

function TAdvResponsiveList.PasteFromClipboard: string;
begin
  Result := '';
end;

procedure TAdvResponsiveList.Refresh;
begin
  {$IFDEF FMXLIB}
  Repaint;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Invalidate;
  {$ENDIF}
end;

function TAdvResponsiveList.RenderItems: TResponsiveCondition;
var
  cond: TResponsiveCondition;
  i,cols,rows,ith,itw: integer;
  x,y,w,h,cc,rc,iw,ih,fw,fc,fh,fr: integer;
  r: TRect;
  ml,mr,mt,mb: integer;

begin
  Result := nil;

  if not HandleAllocated then
    Exit;

  cols := 0;
  rows := 0;
  ith := GetClientHeight;
  itw := GetClientWidth;
  w := itw;
  h := ith;
  ml := 0;
  mr := 0;
  mt := 0;
  mb := 0;
  FMaxSize.cx := 0;
  FMaxSize.cy := 0;
  FRowCount := 0;
  FColCount := 0;

  // get the right condition
  cond := GetCondition;

  Result := cond;

  if Assigned(cond) and (cond <> FCondition) then
  begin
    FCondition := cond;
    DoConditionChange(FCondition);
  end;

  if Assigned(cond) then
  begin
    cols := cond.Columns;
    rows := cond.Rows;
    ith := Round(FDPIScale * cond.ItemHeight);
    itw := Round(FDPIScale * cond.ItemWidth);
    ml := Round(FDPIScale * cond.Margins.Left);
    mr := Round(FDPIScale * cond.Margins.Right);
    mt := Round(FDPIScale * cond.Margins.Top);
    mb := Round(FDPIScale * cond.Margins.Bottom);
  end;

  if (cols = 0) and (rows = 0) then
    cols := 1;

  if ith <= -1 then
    ith := GetClientHeight;

  if itw <= -1 then
    itw := GetClientWidth;

  if cols > 0 then
  begin
    w := GetClientWidth div cols;
    h := ith;
  end;

  if rows > 0 then
  begin
    w := itw;
    h := GetClientHeight div rows;
  end;

  x := 0;
  y := 0;
  cc := 0;
  rc := 0;
  fw := 0;
  fc := 0;
  fh := 0;
  fr := 0;

  if cols > 0 then
  begin
    fw := FixedWidth(0, cols);
    fc := FixedColumns(0, cols);
  end;

  if rows > 0 then
  begin
    fh := FixedHeight(0, rows);
    fr := FixedRows(0, rows);
  end;


  for i := 0 to Items.Count - 1 do
  begin
    iw := w;
    ih := h;

    if Items[i].IsVisible then
    begin


      case Items[i].WidthType of
      isAuto:
        begin
          if cols - fc > 0 then
            iw := (GetClientWidth - fw) div (cols - fc)
          else
            iw := w;
        end;
      isFixed:
        begin
          if cols > 0 then
            iw := Items[i].Width;
        end;
      isPerc: iw := Round(GetClientWidth * Items[i].Width /100);
      isFill: iw := GetClientWidth - x;
      end;

      case Items[i].HeightType of
      isAuto:
        begin
          if rows - fr > 0 then
            ih := (GetClientHeight - fh) div (rows - fr)
          else
            ih := h;
        end;
      isFixed:
        begin
          if rows > 0 then
            ih := Items[i].Height;
        end;
      isPerc: ih := Round(GetClientHeight * Items[i].Height /100);
      isFill: ih := GetClientHeight - y;
      end;

      r := Rect(x,y,x + iw,y + ih);

      r.Left := r.Left + ml;
      r.Top := r.Top + mt;
      r.Right := r.Right - mr;
      r.Bottom := r.Bottom - mb;

      Items[i].FRect := r;
      Items[i].Col := cc;
      Items[i].Row := rc;

      if r.Right > FMaxSize.cx then
        FMaxSize.cx := r.Right;

      if r.Bottom >= FMaxSize.cy then
        FMaxSize.cy := r.Bottom;

      if cols > 0 then
      begin
        if cc < cols - 1 then
        begin
          inc(cc);
          x := x + iw;
        end
        else
        begin
          cc := 0;
          inc(rc);
          x := 0;
          y := y + ih;
          fw := FixedWidth(i + 1, cols);
          fc := FixedColumns(i + 1, cols);
        end;
      end;

      if (rows > 0) then
      begin
        if rc < rows - 1 then
        begin
          inc(rc);
          y := y + ih;
        end
        else
        begin
          rc := 0;
          inc(cc);
          y := 0;
          x := x + iw;
          fh := FixedHeight(i + 1, rows);
          fr := FixedColumns(i + 1, rows);
        end;
      end;
    end;

    if cc + 1 > FColCount then
      FColCount := cc + 1;
    if rc + 1 > FRowCount then
      FRowCount := rc;
  end;
end;

procedure TAdvResponsiveList.ResolveControls;
var
  i,j,cnt: integer;
begin
  // resolve name to control references
  for i := 0 to Items.Count - 1 do
  begin
    if (Items[i].ControlName <> '')  then
    begin
      {$IFNDEF FMXLIB}
      cnt := ControlCount;
      {$ENDIF}
      {$IFDEF FMXLIB}
      cnt := Controls.Count;
      {$ENDIF}

      for j := 0 to cnt - 1 do
      begin
        if Controls[j].Name = Items[i].ControlName then
        begin
          Items[i].Control := Controls[j];
          Controls[j].Visible := Items[i].IsVisible;
          break;
        end;
      end;
    end;
  end;
end;

procedure TAdvResponsiveList.ScrollInView(AItem: TResponsiveListItem);
var
  r: TRect;
  dx,dy: integer;
begin
  r := AItem.FRect;

  dx := 0;
  dy := 0;

  if r.Left < TopLeft.X then
    dx := r.Left - Topleft.X;

  if r.Right > TopLeft.X +  Width then
    dx := r.Right - (TopLeft.X + Round(Width));

  if r.Top < TopLeft.Y then
    dy := r.Top - TopLeft.Y;

  if r.Bottom > TopLeft.Y + Height then
    dy := r.Bottom - (TopLeft.Y + Round(Height));

  TopLeft := Point(TopLeft.X + dx, TopLeft.Y + dy);
end;

procedure TAdvResponsiveList.SelectAll;
begin
end;

function TAdvResponsiveList.SelectedText: string;
begin
  Result := '';
end;

function TAdvResponsiveList.SelectWordAtCaret: string;
begin
  Result := '';
end;

procedure TAdvResponsiveList.SetAppearance(
  const Value: TResponsiveListAppearance);
begin
  FAppearance.Assign(Value);
end;

{$IFNDEF FMXLIB}
procedure TAdvResponsiveList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  dx,dy: integer;
  sz:TSize;
begin
  dx := AWidth - Width;
  dy := AHeight - Height;

  // adapt scrollbar to avoid showing it
  sz := FMaxSize;
  if (dx < 0) then
    sz.cx := sz.cx + dx;
  if (dy < 0) then
    sz.cy := sz.cy + dy;
  SetRange(sz);

  inherited;

  RenderItems;
  SetRange(FMaxsize);

  Refresh;
end;
{$ENDIF}

procedure TAdvResponsiveList.SetConditionCategory(const Value: integer);
begin
  FConditionCategory := Value;
  BeginUpdate;
  EndUpdate;
end;

procedure TAdvResponsiveList.SetConditions(const Value: TResponsiveConditions);
begin
  FConditions.Assign(Value);
end;

procedure TAdvResponsiveList.SetCursorEx(const Value: TCursor);
begin
  inherited Cursor := Value;
  FOldCursor := Value;
end;

procedure TAdvResponsiveList.SetFilterCondition(
  const Value: TResponsiveListItemFilter);
begin
  FFilterCondition.Assign(Value);
end;

{$IFNDEF FMXLIB}
procedure TAdvResponsiveList.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;
{$ENDIF}

procedure TAdvResponsiveList.SetItemIndex(const Value: Integer);
begin
  if (FItemIndex <> Value) then
  begin
    if (FItemIndex <> -1) and (FItemIndex < Items.Count) then
      Items[FItemIndex].FSelected := false;

    FItemIndex := Value;

    if (FItemIndex <> -1) and (FItemIndex < Items.Count) then
      Items[FItemIndex].FSelected := true;
    Refresh;
  end;
end;

procedure TAdvResponsiveList.SetItems(const Value: TResponsiveListItems);
begin
  FListItems.Assign(Value);
end;

procedure TAdvResponsiveList.SetPictureContainer(
  const Value: TPictureContainer);
begin
  FPictureContainer := Value;

  if Assigned(FPictureContainer) then
    FPictureContainer.RegisterControl(Self);
end;

procedure TAdvResponsiveList.SetTabStopEx(const Value: boolean);
begin
  inherited TabStop := Value;
end;

procedure TAdvResponsiveList.SetVersion(const {%H-}Value: string);
begin
  //
end;

procedure TAdvResponsiveList.TopLeftChanged;
begin
  inherited;
  RenderItems;
  UpdateControls;
end;

procedure TAdvResponsiveList.UpdateSelection;
begin
end;

procedure TAdvResponsiveList.UpdateSelectionPoint(LeftSel: boolean; var X,
  Y: integer);
begin
end;

procedure TAdvResponsiveList.UpdateSize;
begin
{$IFDEF FMXLIB}
  RenderItems;
  SetRange(FMaxsize);
{$ENDIF}
end;

procedure TAdvResponsiveList.UpdateWordAndIndexAtCaret(AValue: string;
  AIndex: integer; SpaceOnly: boolean = false);
begin
end;

{ TResponsiveListItems }

function TResponsiveListItems.Add: TResponsiveListItem;
begin
  Result := TResponsiveListItem(inherited Add);
end;

procedure TResponsiveListItems.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TResponsiveListItems.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);

  FList := AOwner as TAdvResponsiveList;
end;

function TResponsiveListItems.GetItem(Index: Integer): TResponsiveListItem;
begin
  Result := TResponsiveListItem(inherited Items[Index]);
end;

function TResponsiveListItems.GetList: TAdvResponsiveList;
begin
  Result := FList;
end;

procedure TResponsiveListItems.Init(Item: TResponsiveListItem);
begin
  Item.Color := FList.Appearance.ItemColor;
  Item.TextColor := FList.Appearance.ItemTextColor;
  Item.BorderColor := FList.Appearance.ItemBorderColor;
  Item.BorderStyle := FList.Appearance.ItemBorderStyle;
  Item.FooterColor := FList.Appearance.ItemFooterColor;
  Item.FooterTextColor := FList.Appearance.ItemFooterTextColor;
  Item.FooterHeight := FList.Appearance.ItemFooterHeight;
  Item.HeaderColor := FList.Appearance.ItemHeaderColor;
  Item.HeaderTextColor := FList.Appearance.ItemHeaderTextColor;
  Item.HeaderHeight := FList.Appearance.ItemHeaderHeight;
  Item.HotColor := FList.Appearance.HotItemColor;
  Item.HotBorderColor := FList.Appearance.HotItemBorderColor;
  Item.HotTextColor := FList.Appearance.HotItemTextColor;
  Item.Shadow := FList.Appearance.ItemShadow;
  Item.SelectedBorderColor := FList.Appearance.SelectedItemBorderColor;
  Item.SelectedColor := FList.Appearance.SelectedItemColor;
  Item.SelectedTextColor := FList.Appearance.SelectedItemTextColor;
  Item.PictureContainer := FList.PictureContainer;
  Item.ContentMargin := FList.Appearance.ItemContentMargin;
  {$IFNDEF FMXLIB}
  Item.Images := FList.Images;
  {$ENDIF}
end;

function TResponsiveListItems.Insert(Index: Integer): TResponsiveListItem;
begin
  Result := TResponsiveListItem(inherited Insert(Index));
end;

procedure TResponsiveListItems.SetItem(Index: Integer;
  const Value: TResponsiveListItem);
begin
  inherited Items[Index] := Value;
end;

procedure TResponsiveListItems.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

{ TResponsiveListItem }

procedure TResponsiveListItem.Assign(Source: TPersistent);
begin
  if (Source is TResponsiveListItem) then
  begin
    FContent := (Source as TResponsiveListItem).Content;
    FColor := (Source as TResponsiveListItem).Color;
    FWidth := (Source as TResponsiveListItem).Width;
    FWidthType := (Source as TResponsiveListItem).WidthType;
    FHeight := (Source as TResponsiveListItem).Height;
    FHeightType := (Source as TResponsiveListItem).HeightType;
    FVisible := (Source as TResponsiveListItem).Visible;
    FFooterText := (Source as TResponsiveListItem).FooterText;
    FFooterColor := (Source as TResponsiveListItem).FooterColor;
    FFooterHeight := (Source as TResponsiveListItem).FooterHeight;
    FHeaderText := (Source as TResponsiveListItem).HeaderText;
    FHeaderColor := (Source as TResponsiveListItem).HeaderColor;
    FHeaderHeight := (Source as TResponsiveListItem).HeaderHeight;
    FHotColor := (Source as TResponsiveListItem).HotColor;
    FHotBorderColor := (Source as TResponsiveListItem).HotBorderColor;
    FHotTextColor := (Source as TResponsiveListItem).HotTextColor;
    FSelectedBorderColor := (Source as TResponsiveListItem).SelectedBorderColor;
    FSelectedColor := (Source as TResponsiveListItem).SelectedColor;
    FSelectedTextColor :=(Source as TResponsiveListItem).SelectedTextColor;
    FShadow := (Source as TResponsiveListItem).Shadow;
    FTag := (Source as TResponsiveListItem).Tag;
    FRect := (Source as TResponsiveListItem).Rect;
    FControlName := (Source as TResponsiveListItem).ControlName;
  end;
end;

constructor TResponsiveListItem.Create({%H-}ACollection: TCollection);
begin
  inherited Create(ACollection);
  FOwnerList := (ACollection as TResponsiveListItems).FList;
  FWidth := 0;
  FHeight := 0;
  FVisible := True;
  FWidthType := isAuto;
  FHeightType := isAuto;
  FColor := clWindow;
  FControl := nil;
  FControlName := '';
  FNameValueList := TResponsiveNameValuePairList.Create;
  FFooterColor := clNone;
  FFooterHeight := 16;
  FHeaderColor := clNone;
  FHeaderHeight := 16;
  FHeaderTextColor := clWhite;
  FFooterTextColor := clWhite;
  FTextColor := clWindowText;
  FSelectedColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FSelectedBorderColor := clSilver;
  FHotColor := clNone;
  FHotTextColor := clNone;
  FHotBorderColor := clNone;
  FShadow := false;
  FFiltered := false;
  (ACollection as TResponsiveListItems).Init(Self);
end;

procedure TResponsiveListItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  {$IFNDEF LCLLIB}
  Filer.DefineProperty('ControlName', ReadControlName, WriteControlName, True);
  {$ENDIF}
  {$IFDEF LCLLIB}
  Filer.DefineProperty('ControlName', @ReadControlName, @WriteControlName, True);
  {$ENDIF}
end;

destructor TResponsiveListItem.Destroy;
begin
  FNameValueList.Free;
  inherited;
end;

procedure TResponsiveListItem.DrawItem(AGraphics: TAdvGraphics; ATemplate, AHeaderTemplate, AFooterTemplate: string;
  ARect: TRect; Focus: boolean);
var
  s, template: string;
  hr: TRect;
  cr: TRect;
  {$IFDEF USEVCLSTYLES}
  LDetails: TThemedElementDetails;
  {$ENDIF}

begin
  FDPIScale := FOwnerList.FDPIScale;
  AGraphics.PictureContainer := FPictureContainer;
  {$IFNDEF FMXLIB}
  AGraphics.ImageList := FImages;
  {$ENDIF}

  if Selected then
    AGraphics.Fill.Color := SelectedColor
  else
  begin
    if Hovered and (HotColor <> clNone) then
       AGraphics.Fill.Color := HotColor
    else
      AGraphics.Fill.Color := Color;
  end;


  AGraphics.Fill.Kind := gfkSolid;

  if BorderStyle = bsSingle then
  begin
    if Selected then
      AGraphics.Stroke.Color := SelectedBorderColor
    else
    begin
      if Hovered and (HotBorderColor <> clNone) then
          AGraphics.Stroke.Color := HotBorderColor
      else
        AGraphics.Stroke.Color := BorderColor;
    end;
  end
  else
    AGraphics.Stroke.Color := AGraphics.Fill.Color;

  AGraphics.Stroke.Kind := gskSolid;

  if Shadow then
  begin
    ARect.Right := ARect.Right - 1;
    ARect.Bottom := ARect.Bottom - 1;
  end;

  {$IFDEF USEVCLSTYLES}
  if FOwnerList.UseVCLStyles and Selected then
  begin
    LDetails := StyleServices.GetElementDetails(tgCellSelected);
    StyleServices.DrawElement(AGraphics.Canvas.Handle, LDetails, ARect, ARect);
  end
  else
  {$ENDIF}
    AGraphics.DrawRectangle(ConvertToRectF(ARect));

  cr := ARect;

  if FooterColor <> clNone then
  begin
    hr := ARect;

    if BorderStyle = bsSingle then
      AGraphics.DrawLine(PointF(hr.Left,hr.Bottom - Round(FDPIScale * FooterHeight)), PointF(hr.Right - 1,hr.Bottom - Round(FDPIScale * FooterHeight)));

    AGraphics.Fill.Color := FooterColor;
    AGraphics.Stroke.Color := FooterColor;

    hr.Top := hr.Bottom - Round(FDPIscale * FooterHeight);

    if BorderStyle = bsSingle then
    begin
      hr.Left := hr.Left + 1;
      hr.Right := hr.Right - 1;
      hr.Bottom := hr.Bottom - 1;
      hr.Top := hr.Top + 1;
    end;

    AGraphics.DrawRectangle(ConvertToRectF(hr));

    AGraphics.Font.Color := FooterTextColor;

    if AFooterTemplate <> '' then
      s := RenderTemplate(AFooterTemplate, Self)
    else
      s := FooterText;

    AGraphics.DrawText(RectF(ARect.Left + 2, ARect.Bottom - Round(FDPIScale * FooterHeight), ARect.Right - 2, ARect.Bottom), s);
    cr.Bottom := cr.Bottom - Round(FDPIScale * FooterHeight);
  end;

  if BorderStyle = bsSingle then
    AGraphics.Stroke.Color := BorderColor
  else
    AGraphics.Stroke.Color := Color;

  if HeaderColor <> clNone then
  begin
    hr := ARect;

    if BorderStyle = bsSingle then
    begin
      AGraphics.DrawLine(PointF(hr.Left,hr.Top + Round(FDPIScale * HeaderHeight)),PointF(hr.Right - 1,hr.Top + Round(FDPIScale * HeaderHeight)));
    end;

    AGraphics.Fill.Color := HeaderColor;
    AGraphics.Stroke.Color := HeaderColor;

    hr.Bottom := hr.Top + Round(FDPIScale * HeaderHeight);

    if BorderStyle = bsSingle then
    begin
      hr.Left := hr.Left + 1;
      hr.Right := hr.Right - 1;
      hr.Top := hr.Top + 1;
    end;

    if AHeaderTemplate <> '' then
      s := RenderTemplate(AHeaderTemplate, Self)
    else
      s := HeaderText;

    {$IFDEF USEVCLSTYLES}
    if FOwnerList.UseVCLStyles then
    begin
      LDetails := StyleServices.GetElementDetails(tgFixedCellNormal);
      StyleServices.DrawElement(AGraphics.Canvas.Handle, LDetails, hr, hr);
    end
    else
    {$ENDIF}
      AGraphics.DrawRectangle(ConvertToRectF(hr));

    AGraphics.Font.Color := HeaderTextColor;
    AGraphics.DrawText(RectF(hr.Left + 2, hr.Top, hr.Right - 2, hr.Top + Round(FDPIScale * HeaderHeight)), s);
    cr.Top := cr.Top + Round(FDPIScale * HeaderHeight);
  end;

  if Shadow then
  begin
    AGraphics.Stroke.Color := clGray;
    AGraphics.Stroke.Kind := gskSolid;
    AGraphics.DrawLine(PointF(ARect.Right, ARect.Top + 1), PointF(ARect.Right, ARect.Bottom));
    AGraphics.DrawLine(PointF(ARect.Right, ARect.Bottom), PointF(ARect.Left + 1, ARect.Bottom));
  end;

  InflateRect(cr,-2 - ContentMargin,-2 - ContentMargin);

  FContentRect := cr;

  if ATemplate <> '' then
    template := ATemplate
  else
    template := Content;

  s := RenderTemplate(template, Self);

  if Selected then
    AGraphics.Font.Color := SelectedTextColor
  else
  begin
    if Hovered and (HotTextColor <> clNone) then
      AGraphics.Font.Color := HotTextColor
    else
      AGraphics.Font.Color := TextColor;
  end;

  AGraphics.DrawText(ConvertToRectF(cr), s, true, gtaLeading, gtaLeading, gttNone, 0, -1, -1, true, false);

  if Focus then
  begin
    OffsetRect(ARect, -1, -1);
    AGraphics.DrawFocusRectangle(ARect, gcBlack);
  end;
end;

function TResponsiveListItem.GetValue(AName: string): variant;
var
  i: integer;
begin
  Result := Null;

  for i := 0 to NameValueList.Count - 1 do
  begin
    if NameValueList.Items[i].Name = AName then
    begin
      Result := NameValueList.Items[i].Value;
      break;
    end;
  end;
end;

function TResponsiveListItem.IsVisible: boolean;
begin
  Result := Visible and not Filtered;
end;

procedure TResponsiveListItem.ReadControlName(Reader: TReader);
begin
  FControlName := Reader.ReadString;
end;

procedure TResponsiveListItem.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetBorderStyle(const Value: TBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetContent(const Value: string);
begin
  if (FContent <> Value) then
  begin
    FContent := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetControl(const Value: TControl);
begin
  FControl := Value;
  if Assigned(FControl) then
    FControlName := FControl.Name
  else
    FControlName := '';
end;

procedure TResponsiveListItem.SetFooterColor(const Value: TColor);
begin
  if (FFooterColor <> Value) then
  begin
    FFooterColor := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetFooterHeight(const Value: Integer);
begin
  if (FFooterHeight <> Value) then
  begin
    FFooterHeight := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetFooterText(const Value: string);
begin
  if (FFooterText <> Value) then
  begin
    FFooterText := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetFooterTextColor(const Value: TColor);
begin
  if (FFooterTextColor <> Value) then
  begin
    FFooterTextColor := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetHeaderColor(const Value: TColor);
begin
  if (FHeaderColor <> Value) then
  begin
    FHeaderColor := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetHeaderHeight(const Value: Integer);
begin
  if (FHeaderHeight <> Value) then
  begin
    FHeaderHeight := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetHeaderText(const Value: string);
begin
  if (FHeaderText <> Value) then
  begin
    FHeaderText := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetHeaderTextColor(const Value: TColor);
begin
  if (FHeaderTextColor <> Value) then
  begin
    FHeaderTextColor := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetHeight(const Value: Integer);
begin
  if (FHeight <> Value) then
  begin
    FHeight := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetHeightType(const Value: TItemSizeType);
begin
  if (FHeightType <> Value) then
  begin
    FHeightType := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetPictureContainer(
  const Value: TPictureContainer);
begin
  FPictureContainer := Value;
end;

procedure TResponsiveListItem.SetSelectedBorderColor(const Value: TColor);
begin
  if (FSelectedBorderColor <> Value) then
  begin
    FSelectedBorderColor := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetSelectedColor(const Value: TColor);
begin
  if (FSelectedColor <> Value) then
  begin
    FSelectedColor := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetSelectedTextColor(const Value: TColor);
begin
  if (FSelectedTextColor <> Value) then
  begin
    FSelectedTextColor := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetShadow(const Value: boolean);
begin
  if (FShadow <> Value) then
  begin
    FShadow := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetValue(AName: string; const Value: variant);
var
  i: integer;
  fnd: boolean;
  nv: TResponsiveNameValuePair;
begin
  fnd := false;
  nv.Name := AName;
  nv.Value := Value;

  for i := 0 to NameValueList.Count - 1 do
  begin
    if NameValueList.Items[i].Name = AName then
    begin
      NameValueList.Items[i] := nv;
      fnd := true;
      break;
    end;
  end;

  if not fnd then
    NameValueList.Add(nv);
end;

procedure TResponsiveListItem.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetWidth(const Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.SetWidthType(const Value: TItemSizeType);
begin
  if (FWidthType <> Value) then
  begin
    FWidthType := Value;
    Changed(False);
  end;
end;

procedure TResponsiveListItem.WriteControlName(Writer: TWriter);
begin
  Writer.WriteString(FControlName);
end;

{ TResponsiveConditions }

function TResponsiveConditions.Add: TResponsiveCondition;
begin
  Result := TResponsiveCondition(inherited Add);
end;

procedure TResponsiveConditions.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TResponsiveConditions.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
end;

function TResponsiveConditions.GetItem(Index: Integer): TResponsiveCondition;
begin
  Result := TResponsiveCondition(inherited Items[Index]);
end;

function TResponsiveConditions.Insert(Index: integer): TResponsiveCondition;
begin
  Result := TResponsiveCondition(inherited Insert(Index));
end;

procedure TResponsiveConditions.SetItem(Index: Integer;
  const Value: TResponsiveCondition);
begin
  inherited Items[Index] := Value;
end;

procedure TResponsiveConditions.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

{ TResponsiveCondition }

procedure TResponsiveCondition.Assign(Source: TPersistent);
begin
  if (Source is TResponsiveCondition) then
  begin
    FCategory := (Source as TResponsiveCondition).Category;
    FWidthFrom := (Source as TResponsiveCondition).WidthFrom;
    FWidthTo := (Source as TResponsiveCondition).WidthTo;
    FHeightFrom := (Source as TResponsiveCondition).HeightFrom;
    FHeightTo := (Source as TResponsiveCondition).HeightTo;
    FRows := (Source as TResponsiveCondition).Rows;
    FColumns := (Source as TResponsiveCondition).Columns;
    FTemplate := (Source as TResponsiveCondition).Template;
    FHeaderTemplate := (Source as TResponsiveCondition).HeaderTemplate;
    FFooterTemplate := (Source as TResponsiveCondition).FooterTemplate;
    FItemHeight := (Source as TResponsiveCondition).ItemHeight;
    FItemWidth := (Source as TResponsiveCondition).ItemWidth;
    FMargins.Assign((Source as TResponsiveCondition).Margins);
    FTag := (Source as TResponsiveCondition).Tag;
  end;
end;

constructor TResponsiveCondition.Create(Collection: TCollection);
begin
  FMargins := TMargins.Create(nil);
  FMargins.OnChange := MarginsChanged;

  inherited;

  // default 3 columns
  FColumns := 3;

  // undefined nr of rows
  FRows := 0;

  // applies from width 0 to infinite
  FWidthFrom := 0;
  FWidthTo := -1;

  // applies to all heights
  FHeightFrom := -1;
  FHeightTo := -1;

  FItemWidth := -1;
  FItemHeight := -1;
  FCategory := -1;
end;

destructor TResponsiveCondition.Destroy;
begin
  FMargins.Free;
  inherited;
end;

procedure TResponsiveCondition.MarginsChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TResponsiveCondition.SetColumns(const Value: integer);
begin
  if (Value >= 0) and (FColumns <> Value) then
  begin
    FColumns := Value;
    Changed(False);
  end;
end;

procedure TResponsiveCondition.SetFooterTemplate(const Value: string);
begin
  if (FFooterTemplate <> Value) then
  begin
    FFooterTemplate := Value;
    Changed(False);
  end;
end;

procedure TResponsiveCondition.SetHeaderTemplate(const Value: string);
begin
  if (FHeaderTemplate <> Value) then
  begin
    FHeaderTemplate := Value;
    Changed(False);
  end;
end;

procedure TResponsiveCondition.SetHeightFrom(const Value: integer);
begin
  if (FHeightFrom <> Value) then
  begin
    FHeightFrom := Value;
    Changed(False);
  end;
end;

procedure TResponsiveCondition.SetHeightTo(const Value: integer);
begin
  if (FHeightTo <> Value) then
  begin
    FHeightTo := Value;
    Changed(False);
  end;
end;

procedure TResponsiveCondition.SetItemHeight(const Value: integer);
begin
  if (FItemHeight <> Value) then
  begin
    FItemHeight := Value;
    Changed(False);
  end;
end;

procedure TResponsiveCondition.SetItemWidth(const Value: integer);
begin
  if (FItemWidth <> Value) then
  begin
    FItemWidth := Value;
    Changed(False);
  end;
end;

procedure TResponsiveCondition.SetMargins(const Value: TMargins);
begin
  FMargins.Assign(Value);
  Changed(False);
end;

procedure TResponsiveCondition.SetRows(const Value: Integer);
begin
  if (Value >= 0) and (FRows <> Value) then
  begin
    FRows := Value;
    Changed(False);
  end;
end;

procedure TResponsiveCondition.SetTemplate(const Value: string);
begin
  if (FTemplate <> Value) then
  begin
    FTemplate := Value;
    Changed(false);
  end;
end;

procedure TResponsiveCondition.SetWidthFrom(const Value: integer);
begin
  if (FWidthFrom <> Value) then
  begin
    FWidthFrom := Value;
    Changed(False);
  end;
end;

procedure TResponsiveCondition.SetWidthTo(const Value: integer);
begin
  if (FWidthTo <> Value) then
  begin
    FWidthTo := Value;
    Changed(False);
  end;
end;

{ TResponsiveListAppearance }

procedure TResponsiveListAppearance.Assign(Source: TPersistent);
begin
  if (Source is TResponsiveListAppearance) then
  begin
    FItemColor := (Source as TResponsiveListAppearance).ItemColor;
    FItemTextColor := (Source as TResponsiveListAppearance).ItemTextColor;
    FItemBorderColor := (Source as TResponsiveListAppearance).ItemBorderColor;
    FItemBorderStyle := (Source as TResponsiveListAppearance).ItemBorderStyle;
    FItemFooterColor := (Source as TResponsiveListAppearance).ItemFooterColor;
    FItemFooterTextColor := (Source as TResponsiveListAppearance).ItemFooterTextColor;
    FItemFooterHeight := (Source as TResponsiveListAppearance).ItemFooterHeight;
    FItemHeaderColor := (Source as TResponsiveListAppearance).ItemHeaderColor;
    FItemHeaderTextColor := (Source as TResponsiveListAppearance).ItemHeaderTextColor;
    FItemHeaderHeight := (Source as TResponsiveListAppearance).ItemHeaderHeight;
    FItemsApplying := (Source as TResponsiveListAppearance).ItemsApplying;
    FSelectedItemBorderColor := (Source as TResponsiveListAppearance).SelectedItemBorderColor;
    FSelectedItemColor := (Source as TResponsiveListAppearance).SelectedItemColor;
    FSelectedItemTextColor := (Source as TResponsiveListAppearance).SelectedItemTextColor;
    FHotItemColor := (Source as TResponsiveListAppearance).HotItemColor;
    FHotItemTextColor := (Source as TResponsiveListAppearance).HotItemTextColor;
    FHotItemBorderColor := (Source as TResponsiveListAppearance).HotItemBorderColor;
  end;
end;

procedure TResponsiveListAppearance.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TResponsiveListAppearance.Create;
begin
  inherited;
  FItemColor := clWindow;
  FItemTextColor := clWindowText;
  FItemBorderColor := clGray;
  FItemBorderStyle := bsSingle;
  FItemFooterColor := clNone;
  FItemFooterTextColor := clWhite;
  FItemFooterHeight := 16;
  FItemHeaderColor := clHighLight;
  FItemHeaderHeight := 16;
  FItemHeaderTextColor := clWhite;
  FItemsApplying := iaAll;
  FSelectedItemBorderColor := clSilver;
  FSelectedItemColor := clHighlight;
  FSelectedItemTextColor := clHighlightText;
  FHotItemColor := clNone;
  FHotItemTextColor := clNone;
  FHotItemBorderColor := clNone;
end;

procedure TResponsiveListAppearance.SetItemBorderColor(const Value: TColor);
begin
  if (FItemBorderColor <> Value) then
  begin
    FItemBorderColor := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemBorderStyle(
  const Value: TBorderStyle);
begin
  if (FItemBorderStyle <> Value) then
  begin
    FItemBorderStyle := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemColor(const Value: TColor);
begin
  if (FItemColor <> Value) then
  begin
    FItemColor := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemContentMargin(const Value: integer);
begin
  if (FItemContentMargin <> Value) then
  begin
    FItemContentMargin := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemFooterColor(const Value: TColor);
begin
  if (FItemFooterColor <> Value) then
  begin
    FItemFooterColor := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemFooterHeight(const Value: Integer);
begin
  if (FItemFooterHeight <> Value) then
  begin
    FItemFooterHeight := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemFooterTextColor(const Value: TColor);
begin
  if (FItemFooterTextColor <> Value) then
  begin
    FItemFooterTextColor := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemHeaderColor(const Value: TColor);
begin
  if (FItemHeaderColor <> Value) then
  begin
    FItemHeaderColor := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemHeaderHeight(const Value: Integer);
begin
  if (FItemHeaderHeight <> Value) then
  begin
    FItemHeaderHeight := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemHeaderTextColor(const Value: TColor);
begin
  if (FItemHeaderTextColor <> Value) then
  begin
    FItemHeaderTextColor := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemsApplying(
  const Value: TResponsiveListItemsApplying);
begin
  if (FItemsApplying <> Value) then
  begin
    FItemsApplying := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemShadow(const Value: boolean);
begin
  if (FItemShadow <> Value) then
  begin
    FItemShadow := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetItemTextColor(const Value: TColor);
begin
  if (FItemTextColor <> Value) then
  begin
    FItemTextColor := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetSelectedItemBorderColor(
  const Value: TColor);
begin
  if (FSelectedItemBorderColor <> Value) then
  begin
    FSelectedItemBorderColor := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetSelectedItemColor(const Value: TColor);
begin
  if (FSelectedItemColor <> Value) then
  begin
    FSelectedItemColor := Value;
    Changed;
  end;
end;

procedure TResponsiveListAppearance.SetSelectedItemTextColor(
  const Value: TColor);
begin
  if (FSelectedItemTextColor <> Value) then
  begin
    FSelectedItemTextColor := Value;
    Changed;
  end;
end;

{ TResponsiveListItemFilter }

procedure TResponsiveListItemFilter.Assign(Source: TPersistent);
begin
  if (Source is TResponsiveListItemFilter) then
  begin
    FCaseSensitive := (Source as TResponsiveListItemFilter).CaseSensitive;
    FText := (Source as TResponsiveListItemFilter).Text;
    FFilterType := (Source as TResponsiveListItemFilter).FilterType;
    FFilterData := (Source as TResponsiveListItemFilter).FilterData;
  end;
end;

constructor TResponsiveListItemFilter.Create;
begin
  FCaseSensitive := false;
  FFilterData := [fdContent];
  FFilterType := mText;
end;

end.
