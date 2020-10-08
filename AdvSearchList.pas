{*************************************************************************}
{ TMS TAdvSearchList                                                      }
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

unit AdvSearchList;

{$I TMSDEFS.INC}
{$IFDEF VCLLIB}
{$IFDEF DELPHIXE6_LVL}
{$DEFINE USEVCLSTYLES}
{$ENDIF}
{$IFDEF FNCLIB}
{$DEFINE USEVCLSTYLES}
{$ENDIF}
{$ENDIF}


interface

uses
  Classes, Generics.Collections,
  AdvScrollControl, AdvGraphics, AdvTypes, AdvUtils, AdvGraphicsTypes, Types
  {$IFDEF VCLLIB}
  , Windows, Messages, ImgList
  {$ENDIF}
  {$IFNDEF FMXLIB}
  , Controls, Graphics
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.Controls, FMX.Graphics, System.UIConsts
  {$ENDIF}
  {$IFNDEF FNCLIB}
  , GDIPicture
  {$ENDIF}
  {$IFDEF FNCLIB}
  {$IFNDEF LCLLIB}
  , System.UITypes
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF LCLLIB}
  , LMessages, LCLType, LCLIntf, ImgList
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 10; // Release nr.
  BLD_VER = 0; // Build nr.

  TTMSFNCSearchListDocURL = 'https://www.tmssoftware.biz/download/manuals/TMSFNCSearchListDevGuide.pdf';
  TTMSFNCSearchListTipsURL = 'https://www.tmssoftware.com/site/tmsfncuipack.asp?s=faq';

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : New : Support for VCL styles
  // v1.0.1.1 : Fixed : Issue with mouse wheel scrolling
  //          : Fixed : Issue with setting Columns[index].Visible = false
  // v1.0.2.0 : New : Property WheelIncrement added
  // v1.0.2.1 : Fixed : Issue with Appearance.SelectionColor,Appearance.SelectionTextColor
  // v1.0.2.2 : Fixed : Issue with BorderColor & Font.Color
  // v1.0.3.0 : New : Method UnSelect added
  //          : New : WordWrapping & Trimming properties added at column level
  // v1.0.3.1 : Improved : Focus handling
  // v1.0.4.0 : New : Alignment & VertAlignment properties added at column level
  // v1.0.5.0 : Improved : When HighlightColor or HighlightTextColor is clNone, no more highlighting will be done
  // v1.0.5.1 : Improved : High DPI handling
  // v1.0.5.2 : Fixed : Issue with focus drawing
  // v1.0.6.0 : New : Horizontal scrolling support added
  // v1.0.6.1 : Fixed : Design-time issue with scrollbars
  // v1.0.6.2 : Fixed : Issue with ShowItemCategory & text size calculation
  // v1.0.6.3 : Fixed : Selected item drawing when ShowItemCategory is true
  // v1.0.7.0 : New : Added function TDBAdvSearchList.Reload;
  // v1.0.7.1 : Fixed : Issue with hiding control when double-clicked
  // v1.0.8.0 : New : SearchList.Items.IndexOf(AValue) added
  // v1.0.8.1 : Fixed : Issue with alignment in last column
  // v1.0.8.2 : Improved : Vertical scrolling painting behavior
  // v1.0.9.0 : New : Per monitor support for high DPI
  // v1.0.10.0: New : TSearchListItem.OwnsObject property added


  BAR_WIDTH = 4;

{$IFDEF FNCLIB}
const
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
  clInfoBk = gcLightyellow;
{$ENDIF}

type
  {$IFDEF FNCLIB}

  {$IFDEF FMXLIB}
  TGDIPPicture = class(TBitmap);
  {$ENDIF}

  {$IFNDEF FMXLIB}
  TGDIPPicture = class(TPicture);
  {$ENDIF}

  {$ENDIF}

  TCategoryItem = class(TCollectionItem)
  private
    FID: integer;
    FCaption: string;
    FFilter: boolean;
    FTag: NativeInt;
    procedure SetCaption(const Value: string);
    procedure SetFilter(const Value: boolean);
  protected
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Filter: boolean read FFilter write SetFilter default false;
    property ID: integer read FID write FID;
    property Tag: NativeInt read FTag write FTag;
  end;

  TCategoryList = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItems(AIndex: Integer): TCategoryItem;
    procedure SetItems(AIndex: Integer; const Value: TCategoryItem);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
    procedure Update(Item: TCollectionItem); override;
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TComponent);
    function Add: TCategoryItem; overload;
    function Add(Caption: string; ID: integer): TCategoryItem; overload;
    function Insert(AIndex: integer): TCategoryItem;
    property Items[AIndex: Integer]: TCategoryItem read GetItems write SetItems; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TColumnItem = class(TCollectionItem)
  private
    FWidth: integer;
    FColor: TColor;
    FFont: TFont;
    FVisible: boolean;
    FControlFont: boolean;
    FTag: NativeInt;
    FTrimming: boolean;
    FWordWrap: boolean;
    FVertAlignment: TAdvGraphicsTextAlign;
    FAlignment: TAdvGraphicsTextAlign;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetVisible(const Value: boolean);
    procedure SetWidth(const Value: integer);
    procedure SetControlFont(const Value: boolean);
    procedure SetWordWrap(const Value: boolean);
    procedure SetTrimming(const Value: boolean);
    procedure SetAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetVertAlignment(const Value: TAdvGraphicsTextAlign);
  protected
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAdvGraphicsTextAlign read FAlignment write SetAlignment default gtaLeading;
    property Color: TColor read FColor write SetColor default clNone;
    property ControlFont: boolean read FControlFont write SetControlFont default true;
    property Font: TFont read FFont write SetFont;
    property Tag: NativeInt read FTag write FTag default 0;
    property Trimming: boolean read FTrimming write SetTrimming default false;
    property Visible: boolean read FVisible write SetVisible default true;
    property VertAlignment: TAdvGraphicsTextAlign read FVertAlignment write SetVertAlignment default gtaLeading;
    property Width: integer read FWidth write SetWidth default 128;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;

  end;

  TColumnItems = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItems(AIndex: Integer): TColumnItem;
    procedure SetItems(AIndex: Integer; const Value: TColumnItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetItemClass: TCollectionItemClass; virtual;
  public
    constructor Create(AOwner: TComponent);
    function Add: TColumnItem;
    function Insert(AIndex: integer): TColumnItem;
    property Items[AIndex: Integer]: TColumnItem read GetItems write SetItems; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TColumnItemShape = (sNone, sRect, sBar, sBackground);

  TSearchColumnItem = class(TCollectionItem)
  private
    FPicture: TGDIPPicture;
    FCaption: string;
    FDescription: string;
    FImageIndex: TImageIndex;
    FTextColor: TColor;
    FShape: TColumnItemShape;
    FColor: TColor;
    procedure SetDescription(const Value: string);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetPicture(const Value: TGDIPPicture);
    procedure SetCaption(const Value: string);
    function GetPicture: TGDIPPicture;
    procedure SetColor(const Value: TColor);
    procedure SetShape(const Value: TColumnItemShape);
    procedure SetTextColor(const Value: TColor);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor default clNone;
    property Description: string read FDescription write SetDescription;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Picture: TGDIPPicture read GetPicture write SetPicture;
    property Shape: TColumnItemShape read FShape write SetShape default sNone;
    property TextColor: TColor read FTextColor write SetTextColor default clNone;
  end;

  TSearchColumnItems = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItems(AIndex: Integer): TSearchColumnItem;
    procedure SetItems(AIndex: Integer; const Value: TSearchColumnItem);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TSearchColumnItem;
    function Insert(AIndex: Integer): TSearchColumnItem;
    property Items[AIndex: Integer]: TSearchColumnItem read GetItems write SetItems; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSearchItemType = (itItem, itCategory);

  TSortDirection = (sdDown, sdUp);

  TSearchListItem = class(TCollectionItem)
  private
    FCategoryID: Integer;
    FItemType: TSearchItemType;
    FVisible: boolean;
    FColumns: TSearchColumnItems;
    FCategoryCount: Integer;
    FTag: NativeInt;
    FObject: TObject;
    FOwnsObject: boolean;
    procedure SetItemType(const Value: TSearchItemType);
    procedure SetVisible(const Value: boolean);
    procedure SetColumns(const Value: TSearchColumnItems);
    function GetCaptions(AIndex: Integer): string;
    procedure SetCaptions(AIndex: Integer; const Value: string);
    function GetDescriptions(AIndex: Integer): string;
    function GetImageIndexes(AIndex: Integer): TImageIndex;
    procedure SetDescriptions(AIndex: Integer; const Value: string);
    procedure SetImageIndexes(AIndex: Integer; const Value: TImageIndex);
    procedure ColumnsChanged(Sender: TObject);
  protected
    function CreateColumns: TSearchColumnItems; virtual;
    property CategoryCount: integer read FCategoryCount write FCategoryCount;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Visible: boolean read FVisible write SetVisible;
    property Captions[AIndex: Integer]: string read GetCaptions write SetCaptions;
    property Descriptions[AIndex: Integer]: string read GetDescriptions write SetDescriptions;
    property ImageIndexes[AIndex: Integer]: TImageIndex read GetImageIndexes write SetImageIndexes;
    property &Object: TObject read FObject write FObject;
    property OwnsObject: boolean read FOwnsObject write FOwnsObject;
  published
    property CategoryID: integer read FCategoryID write FCategoryID default -1;
    property Columns: TSearchColumnItems read FColumns write SetColumns;
    property ItemType: TSearchItemType read FItemType write SetItemType default itItem;
    property Tag: NativeInt read FTag write FTag default 0;
  end;

  TSearchList = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    FSortColumn: integer;
    FSortDirection: TSortDirection;
    FSortCaseSensitive: boolean;
    function GetItem(AIndex: integer): TSearchListItem;
    procedure SetItem(AIndex: integer; const Value: TSearchListItem);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
    procedure Update(Item: TCollectionItem); override;
    procedure Changed; virtual;
    {$IFNDEF LCLLIB}
    procedure QuickSort(L, R: Integer);
    {$ENDIF}
    function Compare(Item1, Item2 : TCollectionItem): Integer; virtual;
  public
    constructor Create(AOwner: TComponent);
    function Add: TSearchListItem; overload;
    function Add(ACaption: string; ADescription: string = ''): TSearchListItem; overload;
    function Add(ACaption: string; ADescription: string; AObject: TObject = nil): TSearchListItem; overload;
    function Insert(AIndex: integer): TSearchListItem;
    function VisibleItems: integer;
    function IndexOf(ACaption: string): integer;
    procedure LoadStrings(Value: TStrings);
    procedure Sort(AColumn: Integer; ADirection: TSortDirection = sdUp; CaseSensitive: boolean = true);
    property Items[AIndex: integer]: TSearchListItem read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TItemClickEvent = procedure(Sender: TObject; ItemIndex: Integer; Item: TSearchListItem) of object;

  TFilterCount = (fcHide, fcShow);

  TAdvSearchListAppearance = class(TPersistent)
  private
    FCategoryColor: TColor;
    FHighlightTextColor: TColor;
    FHighlightFontStyle: TFontStyles;
    FHighlightColor: TColor;
    FOnChange: TNotifyEvent;
    FCategoryFont: TFont;
    FSelectionTextColor: TColor;
    FSelectionColor: TColor;
    FDescriptionFont: TFont;
    FDescriptionControlFont: boolean;
    FFilterCountFont: TFont;
    FFilterCountFormat: string;
    FFilterCount: TFilterCount;
    FBandColorEven: TColor;
    FBandColorOdd: TColor;
    FBanding: boolean;
    FCategoryControlFont: boolean;
    FShowItemCategory: boolean;
    FItemCategoryFont: TFont;
    FItemCategoryFormat: string;
    procedure SetCategoryColor(const Value: TColor);
    procedure SetCategoryFont(const Value: TFont);
    procedure SetHighlightColor(const Value: TColor);
    procedure SetHighlightFontStyle(const Value: TFontStyles);
    procedure SetHighlightTextColor(const Value: TColor);
    procedure SetSelectionColor(const Value: TColor);
    procedure SetSelectionTextColor(const Value: TColor);
    procedure SetDescriptionFont(const Value: TFont);
    procedure SetDescriptionControlFont(const Value: boolean);
    procedure SetFilterCount(const Value: TFilterCount);
    procedure SetFilterCountFont(const Value: TFont);
    procedure SetFilterCountFormat(const Value: string);
    procedure SetBandColorEven(const Value: TColor);
    procedure SetBandColorOdd(const Value: TColor);
    procedure SetBanding(const Value: boolean);
    procedure SetCategoryControlFont(const Value: boolean);
    procedure SetItemCategoryFont(const Value: TFont);
    procedure SetItemCategoryFormat(const Value: string);
    procedure SetShowItemCategory(const Value: boolean);
    procedure DescriptionFontChanged(Sender: TObject);
    procedure CategoryFontChanged(Sender: TObject);
    procedure ItemCategoryFontChanged(Sender: TObject);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Banding: boolean read FBanding write SetBanding default False;
    property BandColorOdd: TColor read FBandColorOdd write SetBandColorOdd default TColor(clInfoBk);
    property BandColorEven: TColor read FBandColorEven write SetBandColorEven default TColor(clWindow);
    property CategoryColor: TColor read FCategoryColor write SetCategoryColor default TColor(clSilver);
    property CategoryControlFont: boolean read FCategoryControlFont write SetCategoryControlFont default True;
    property CategoryFont: TFont read FCategoryFont write SetCategoryFont;
    property DescriptionFont: TFont read FDescriptionFont write SetDescriptionFont;
    property DescriptionControlFont: boolean read FDescriptionControlFont write SetDescriptionControlFont default True;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor default clNone;
    property HighlightTextColor: TColor read FHighlightTextColor write SetHighlightTextColor default TColor(clRed);
    property HighlightFontStyle: TFontStyles read FHighlightFontStyle write SetHighlightFontStyle default [TFontStyle.fsBold];
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default TColor(clHighlight);
    property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor default TColor(clHighlightText);
    property FilterCount: TFilterCount read FFilterCount write SetFilterCount default fcHide;
    property FilterCountFont: TFont read FFilterCountFont write SetFilterCountFont;
    property FilterCountFormat: string read FFilterCountFormat write SetFilterCountFormat;
    property ItemCategoryFont: TFont read FItemCategoryFont write SetItemCategoryFont;
    property ItemCategoryFormat: string read FItemCategoryFormat write SetItemCategoryFormat;
    property ShowItemCategory: boolean read FShowItemCategory write SetShowItemCategory default false;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFilterItemEvent = procedure(Sender: TObject; AItem: TSearchListItem; var Retain: boolean) of object;

  TFilterType = (mText, mEntireWord, mStartWord, mEndWord);

  TFilterData = (fdText, fdDescription, fdAll);

  TFilterCondition = class(TPersistent)
  private
    FFilterType: TFilterType;
    FText: string;
    FTextUppercase: string;
    FColumn: Integer;
    FCategories: boolean;
    FCaseSensitive: boolean;
    FCategoryItems: boolean;
    FAutoSelect: boolean;
    FAllColumns: boolean;
    FFilterData: TFilterData;
    FMaxCategoryItems: Integer;
    procedure SetFilterType(const Value: TFilterType);
    procedure SetText(const Value: string);
    procedure SetColumn(const Value: Integer);
    procedure SetFilterData(const Value: TFilterData);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property AllColumns: boolean read FAllColumns write FAllColumns default False;
    property AutoSelect: boolean read FAutoSelect write FAutoSelect;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive default False;
    property Categories: boolean read FCategories write FCategories default False;
    property CategoryItems: boolean read FCategoryItems write FCategoryItems default False;
    property Column: Integer read FColumn write SetColumn default 0;
    property FilterData: TFilterData read FFilterData write SetFilterData default fdText;
    property FilterType: TFilterType read FFilterType write SetFilterType default mText;
    property MaxCategoryItems: Integer read FMaxCategoryItems write FMaxCategoryItems default -1;
    property Text: string read FText write SetText;
  end;


  TItemStatus = (isNormal, isSelected, isDisabled);
  TItemState = set of TItemStatus;

  TSearchItemColumnGetDataEvent = procedure(Sender: TObject; AIndex, AColumn: integer; var ACaption: string; var ADescription: string; var AImageIndex: integer) of object;
  TSearchItemColumnDrawEvent = procedure(Sender: TObject; AIndex, AColumn: integer; ACanvas: TCanvas; ARect: TRect; ItemState: TItemState; var DefaultDraw: boolean) of object;

  TDisplayList = TList<integer>;

  TAdvCustomSearchList = class(TCustomScrollingControl)
  private
    FTopRow: integer;
    FItemCount: integer;
    FItemIndex: integer;
    FItemHeight: integer;
    FItems: TSearchList;
    FUpdateCount: integer;
    FHighlightText: string;
    FAppearance: TAdvSearchListAppearance;
    FFilterCondition: TFilterCondition;
    FDisplayList: TDisplayList;
    FOnFilterItem: TFilterItemEvent;
    FColumns: TColumnItems;
    FDesignTime: boolean;
    {$IFNDEF FMXLIB}
    FImages: TCustomImageList;
    {$ENDIF}
    FCategories: TCategoryList;
    FOnDrawSearchItemColumn: TSearchItemColumnDrawEvent;
    FOnGetSearchItemColumn: TSearchItemColumnGetDataEvent;
    FCategoryItemHeight: integer;
    FOnItemClick: TItemClickEvent;
    FOnItemChange: TItemClickEvent;
    FWheelIncrement: integer;
    FDPIScale: single;
    {$IFNDEF FMXLIB}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    {$ENDIF}
    {$IFDEF USEVCLSTYLES}
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$ENDIF}
    procedure SetTopRow(const Value: integer);
    procedure SetItemIndex(const Value: integer);
    procedure SetItemHeight(const Value: integer);
    procedure SetItems(const Value: TSearchList);
    function GetRows: integer;
    procedure ListChanged(Sender: TObject);
    procedure ColumnsChanged(Sender: TObject);
    procedure SetAppearance(const Value: TAdvSearchListAppearance);
    procedure AppearanceChanged(Sender: TObject);
    procedure SetFilterCondition(const Value: TFilterCondition);
    procedure SetColumns(const Value: TColumnItems);
    {$IFNDEF FMXLIB}
    procedure SetImages(const Value: TCustomImageList);
    {$ENDIF}
    procedure SetCategories(const Value: TCategoryList);
    procedure SetVersion(const {%H-}Value: string);
    function GetVersionNr: Integer;
    procedure SetCategoryItemHeight(const Value: integer);
    function GetFilteredItemCount: integer;
    function GetItemCount: integer;
    function GetTotalItemCount: integer;
  protected
    function GetVersion: string; override;
    function GetDocURL: string; override;
    function GetTipsURL: string; override;
    {$IFNDEF FMXLIB}
    procedure CreateWnd; override;
    {$ENDIF}
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
    procedure MouseDownN({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer); override;
    procedure MouseMoveN({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer); override;
    procedure MouseUpN({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer); override;
    procedure DoDblClick({%H-}X, {%H-}Y: integer); override;
    procedure KeyDownN(var {%H-}Key: Word; {%H-}Shift: TShiftState); override;
    procedure KeyUpN(var {%H-}Key: Word; {%H-}Shift: TShiftState); override;
    procedure KeyPressN(var {%H-}Key: Char); override;
    procedure Paint; override;
    function GetClientWidth: integer; override;
    function GetClientHeight: integer; override;
    {$IFNDEF FMXLIB}
    function DoMouseWheelDown({%H-}Shift: TShiftState; {%H-}MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp({%H-}Shift: TShiftState; {%H-}MousePos: TPoint): Boolean; override;
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    {$ENDIF}
    procedure DoSelectionChanged; override;
    function HasSelection: boolean; override;
    function IsForwardSelection: boolean; override;
    procedure DeleteSelection; override;
    procedure UpdateSelection; override;
    procedure ClearSelection; override;
    function GetSelectionFromXY: TPoint; override;
    function GetSelectionToXY: TPoint; override;
    function GetCaretXY: TPoint; override;
    function GetCaretLH: integer; override;
    function GetElementCount: integer; override;
    function SelectedText: string; override;
    function SelectWordAtCaret: string; override;
    procedure GetWordAndIndexAtCaret(var {%H-}AValue: string; var {%H-}AIndex: integer; SpaceOnly: boolean = false); override;
    procedure UpdateWordAndIndexAtCaret({%H-}AValue: string; {%H-}AIndex: integer; SpaceOnly: boolean = false); override;
    procedure UpdateSelectionPoint({%H-}LeftSel: boolean; var {%H-}X, {%H-}Y: integer); override;
    procedure SelectAll; override;
    procedure Backspace; override;
    procedure InsertChar({%H-}ch: char); overload; override;
    procedure InsertChar({%H-}Value: string); overload; override;
    procedure DoGetSearchItemColumn(AIndex, AColumn: integer; var ACaption, ADescription: string; var AImageIndex: integer);
    procedure DoDrawSearchItemColumn(AIndex, AColumn: integer; ACanvas: TCanvas; ARect: TRect; AState: TItemState; var DefaultDraw: boolean);
    procedure DoItemClick(ItemIndex: integer); virtual;
    procedure DoItemChange(ItemIndex: integer); virtual;
    function CreateSearchList: TSearchList; virtual;
    function CreateColumns: TColumnItems; virtual;
    function CreateCategories: TCategoryList; virtual;
    function FilterItem(AItem: TSearchListItem): boolean; virtual;
    function GetRowIndex(Y: Integer): Integer;
    function GetRowYPos(AIndex: Integer; EndY: boolean = false): Integer;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function GetSize({%H-}VertSizeOnly: boolean): TSize;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Refresh;
    procedure Resize; override;
    procedure UpdateSize; override;
    property Rows: integer read GetRows;
    property Categories: TCategoryList read FCategories write SetCategories;
    property Columns: TColumnItems read FColumns write SetColumns;
    property Items: TSearchList read FItems write SetItems;
    {$IFNDEF FMXLIB}
    property Images: TCustomImageList read FImages write SetImages;
    {$ENDIF}
    property Version: string read GetVersion write SetVersion;
    property OnGetSearchItemColumn: TSearchItemColumnGetDataEvent read FOnGetSearchItemColumn write FOnGetSearchItemColumn;
    property OnDrawSearchItemColumn: TSearchItemColumnDrawEvent read FOnDrawSearchItemColumn write FOnDrawSearchItemColumn;
    property OnFilterItem: TFilterItemEvent read FOnFilterItem write FOnFilterItem;
    property OnItemClick: TItemClickEvent read FOnItemClick write FOnItemClick;
    property OnItemChange: TItemClickEvent read FOnItemChange write FOnItemChange;
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
    property CategoryItemHeight: integer read FCategoryItemHeight write SetCategoryItemHeight default 20;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 20;
    property TopRow: integer read FTopRow write SetTopRow;
    property ItemCount: integer read GetItemCount;
    property FilteredItemCount: integer read GetFilteredItemCount;
    property TotalItemCount: integer read GetTotalItemCount;
    procedure UpdateFilter; virtual;
    procedure ClearFilter; virtual;
    {$IFDEF FMXLIB}
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    procedure BeginUpdate;
    procedure EndUpdate;
    {$ENDIF}
    function SelectedItem: TSearchListItem;
    function SelectNextItem(AIndex: Integer): boolean;
    function SelectPreviousItem(AIndex: Integer): boolean;
    function SelectFirstItem: boolean;
    function SelectLastItem: boolean;
    function SelectPrevPageItem: boolean;
    function SelectNextPageItem: boolean;
    function DisplayToItemIndex(DisplayIndex: Integer): Integer;
    function CategoryName(ID: Integer): string;
    procedure UnSelect;
    procedure ScrollToItem(const AIndex: Integer);
    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;
    function PasteFromClipboard: string; override;
  published
    property Appearance: TAdvSearchListAppearance read FAppearance write SetAppearance;
    property FilterCondition: TFilterCondition read FFilterCondition write SetFilterCondition;
    property WheelIncrement: integer read FWheelIncrement write FWheelIncrement default 8;
  end;

  {$IFDEF DELPHIXE2_LVL}
  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  {$ENDIF}
  TAdvSearchList = class(TAdvCustomSearchList)
  private
    function GetTabSop: boolean;
    procedure SetTabStop(const Value: boolean);
  published
    {$IFNDEF FMXLIB}
    property BorderColor;
    {$ENDIF}
    property Categories;
    property CategoryItemHeight;
    property Color;
    property Columns;
    {$IFNDEF FMXLIB}
    property Images;
    {$ENDIF}
    property ItemHeight;
    property Items;
    property PopupMenu;
    {$IFDEF USEVCLSTYLES}
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}
    {$ENDIF}
    property TabStop: boolean read GetTabSop write SetTabStop default true;
    property Version;

    property OnDrawSearchItemColumn;
    property OnGetSearchItemColumn;
    property OnFilterItem;
    property OnItemClick;
    property OnItemChange;

    property OnClick;
    {$IFNDEF FMXLIB}
    property OnContextPopup;
    property OnKeyPress;
    {$ENDIF}
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
  end;


implementation

uses
  SysUtils, Math, AdvStyleIf
  {$IFDEF USEVCLSTYLES}
  , VCL.Themes
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.TMSFNCStyles
  {$ENDIF}
  ;

const
  clOrange = $00A5FF;

{$IFDEF LCLLIB}
var
  cslcol: integer;
  csldir: TSortDirection;
  cslcase: boolean;
  cslcomp: TCollectionSortCompare;
{$ENDIF}

{$IFNDEF LCLLIB}
type
// Helper class to allow sorting of a TCollection
{$HINTS OFF}
  TShadowedCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    {$IFDEF DELPHIXE3_LVL}
    FItems: TList<TCollectionItem>;
    {$ENDIF}
    {$IFNDEF DELPHIXE3_LVL}
    FItems: TList;
    {$ENDIF}
  end;
{$HINTS ON}
{$ENDIF}


function IPos(su,s:string): Integer;
begin
  Result := Pos(UpperCase(su),UpperCase(s));
end;

function VarPos(su,s:string;var Res:Integer):Integer;
begin
  Res := Pos(su,s);
  Result := Res;
end;

function HTMLStripAll(s:string):string;
var
  TagPos, PTP: integer;
begin
  Result := '';
  TagPos := 0;

  // remove all tags
  while (VarPos('<',s,TagPos)>0) do
  begin
    PTP := TagPos;
    if (VarPos('>',s,TagPos) > PTP) then
    begin
      Result := Result + Copy(s,1,PTP - 1);
      Delete(s,1,TagPos)
    end
    else
      Break;
  end;
  Result := Result + s;
end;

function HiLight(s,h,tag:string;DoCase,FullText:boolean):string;
var
  hs: string;
  l,k,m: Integer;
  IsHTML: Boolean;
begin
  // This code removes other highlighting, i.e. all "<"+Tag+">" and "</"+Tag+">"
  // - this will avoid a "duplicate" highlighting as this could lead to highlight
  //   unappropriated parts of a text or confuse a user
  // To avoid case sensitivity I use the fast ASCII version of Uppercase function
  // as this is good enough for all tags (they can’t contain international characters)
  tag := Uppercase(tag); // an upper-case version of a tag used in the rest of the function
  hs := Uppercase(s); // an upper-case version of the searched text
  // I repeatedly look for "<"+tag+">" and remove it from both upper-case (hs) and original (s)
  // version of the searched text. This part can be better optimized, but in most cases
  // the highlight tag won’t be presented in the searched text or it will be there only once.

  l := Pos('<' + tag + '>',hs);
  while (l > 0) do
  begin
    Delete(s, l, Length(tag) + 2);
    Delete(hs, l, Length(tag) + 2);
    l := Pos('<' + tag + '>',hs);
  end;

  // Same for "</"+tag+">"
  l := Pos('</' + tag + '>',hs);
  while (l > 0) do
  begin
    Delete(s, l, Length(tag) + 3);
    Delete(hs, l, Length(tag) + 3);
    l := Pos('</' + tag + '>',hs);
  end;

  // This code is little confusing as it is not always clear when the text is HTML and when
  // is just a plain text. Some parts of TAdvStringGrid code recognizes HTML text by a presence
  // of "</" string in the text and others handle all texts like HTML. I have selected the first
  // version (the presence of "</") as I hope it is the wanted behaviour.

  // Convert h to upper-case version for non-case-sensitive search.
  // The ANSI version has been used to convert all letters including international ones.
  if not DoCase then
    h := AnsiUppercase(h);
  // Now I try to detect presence of HTML in the searched text
  IsHTML := (Pos('</', s) > 0);
  // For HTML version I need to create a plain text version without all tags and special characters
  // converted to the normal ones.
  if IsHTML then
    hs := TAdvUtils.UnFixMarkup(HTMLStripAll(s), true)

  else hs := s;
  // For non-case-sensitive search I need the upper-case version.
  if not DoCase then
    hs := AnsiUppercase(hs);

  // I will clear the result as I need to add handled parts to it, to avoid an extra variable.
  Result := '';

  // When the full text search is provided (i.e. both texts must match) I need just to compare
  // h and hs. If they match I can highlight the whole text.
  if FullText then
  begin
    // This works for both html and plain texts, but for HTML version the version with tags
    // should be included in the result opposite to plain text version where we need the version
    // with fixed special characters
    if (h = hs) then
      if (IsHTML) then s := '<' + tag + '>' + s + '</' + tag + '>'
      else s := '<' + tag + '>' + TAdvUtils.FixMarkup(s) + '</' + tag + '>';
  end
  else
  if not IsHTML then
  begin
    // The plain-text partial search is little more complicated. Mainly I need to convert all special
    // characters to the HTML version when the highlighting is applied. Otherwise I need to keep the
    // original plain-text version as there is not tag in the text and it will be handled as plain text.
    l := Pos(h, hs);
    if (l > 0) then
    begin
      repeat
        Result := Result + TAdvUtils.FixMarkup(Copy(s, 1, l - 1), false) + '<' + tag + '>' + TAdvUtils.FixMarkup(Copy(s, l, Length(h)), false) + '</' + tag + '>';
        Delete(s, 1, l + length(h) - 1);
        Delete(hs, 1, l + length(h) - 1);
        l := Pos(h, hs);
      until (l = 0);
      s := TAdvUtils.FixMarkup(s, false);
    end;
  end
  else
  begin
    // The last and more complex processing for HTML texts. The main idea is to find the text fh (i.e.
    // h with fixed special characters) in the searched text hs (i.e. s without tags). When I found
    // a matching, I will copy everything from the original text up to the matching location including
    // tags.
    // Try to find a matching and store it to L.
    l := Pos(h, hs);
    while (l > 0) do
    begin
      // Some matching was found, I can safely delete everything up to the end of matching
      // from hs (s without tags)
      Delete(hs, 1, l + length(h) - 1);
      // Now I need to move a part of s before the matching to the Result.
      // But it still can contain both tags and special characters!
      repeat
        // Try to find a start of the first tag or special character in remaining part of s, if it exists.
        k := pos('<', s);
        m := pos('&', s);
        // Move all text, all tags up to the matching and all special characters before the matching
        // to the result. Keep on mind the tags are not calculated, but special characters are handled as
        // a single character.
        while ((k > 0) and (k <= l)) or ((m > 0) and (m < l)) do
        begin
          // We need to move the part of S before the matching and the tag/special character into the result,
          // whatever of them is the first.
          if (k > 0) and ((m <= 0) or (k < m)) then
          begin
            Result := Result + copy(s, 1, k - 1);
            Delete(s, 1, k - 1);
            l := l - (k - 1);
            m := m - (k - 1);
            k := pos('>', s);
            if (k = 0) then k := Length(s);
            Result := Result + copy(s, 1, k);
            Delete(s, 1, k);
            m := m - k;
            k := pos('<', s);
          end
          else
          begin
            Result := Result + copy(s, 1, m - 1);
            Delete(s, 1, m - 1);
            l := l - (m - 1) - 1; // -1 = special character
            k := k - (m - 1);
            m := pos(';', s);
            if (m = 0) then m := Length(s);
            Result := Result + copy(s, 1, m);
            Delete(s, 1, m);
            k := k - m;
            m := pos('&', s);
          end;
        end;
        // No more tags or special characters were found or they lie after a start of the matching or the
        // special character is the first character of the matching. I can move a part of S before
        // the matching to Result and start highlighting adding "<"+tag+">"
        Result := Result + copy(s, 1, l - 1) + '<' + tag + '>';
        Delete(s, 1, l - 1);
        // Now I will exclude the deleted part of s from the position of the first tag/spec.char
        // to re-use the values.
        k := k - (l - 1);
        m := m - (l - 1);
        // Store the length of matching text. Again, it can contain other tags.
        l := Length(h);
        // While there exists some tag in the matching text, move part of S before the tag into the result,
        // move the tag and try to find another one, until no one is found, it lies behind the matching text
        // or the end of S is reached (the last condition should never occur)
        while (((k > 0) and (k <= l)) or ((m > 0) and (m <= l))) and (l > 0) and (s <> '') do
        begin
          if (k > 0) and ((m <= 0) or (k < m)) then
          begin
            Result := Result + copy(s, 1, k - 1);
            Delete(s, 1, k - 1);
            l := l - (k - 1);
            m := m - (k - 1);
            k := pos('>', s);
            if (k = 0) then k := Length(s);
            Result := Result + copy(s, 1, k);
            // A special handling to keep highlighting after </font> tag
            if (Uppercase(copy(s, 1, 6)) = '</FONT') then
              Result := Result + '</' + tag + '><' + tag + '>';
            Delete(s, 1, k);
            m := m - k;
            k := pos('<', s);
          end
          else
          begin
            Result := Result + copy(s, 1, m - 1);
            Delete(s, 1, m - 1);
            l := l - (m - 1) - 1; // -1 = special character
            k := k - (m - 1);
            m := pos(';', s);
            if (m = 0) then m := Length(s);
            Result := Result + copy(s, 1, m);
            Delete(s, 1, m);
            k := k - m;
            m := pos('&', s);
          end;
        end;
        // L contains resting characters to move from S to the result. We don't need any other testing now.
        if (l > 0) then
        begin
          Result := Result + copy(s, 1, l);
          Delete(s, 1, l);
        end;
        // The whole matching was moved to result, so we must "close" highlighting now.
        Result := Result + '</' + tag + '>';
        l := 0;
        // Continue until whole string before the matching and the matching are moved
        // from S into the result.
      until (l = 0);
      // Now try to find another matching.
      l := Pos(h, hs);
    end;
  end;
  // S contains an unhandled part of the original text without any other matching.
  // - for full-text and plain-text matching the variable S can contain pre-processed
  //   values as the handling was different
  Result := Result + s;
end;


{ TAdvCustomSearchList }

procedure TAdvCustomSearchList.AppearanceChanged(Sender: TObject);
begin
  Refresh;
end;

procedure TAdvCustomSearchList.Assign(Source: TPersistent);
begin
  if (Source is TAdvCustomSearchList) then
  begin
    Appearance.Assign((Source as TAdvCustomSearchList).Appearance);
    Items.Assign((Source as TAdvCustomSearchList).Items);
    Categories.Assign((Source as TAdvCustomSearchList).Categories);
    FCategoryItemHeight := (Source as TAdvCustomSearchList).CategoryItemHeight;
    FItemHeight := (Source as TAdvCustomSearchList).ItemHeight;
    FilterCondition.Assign((Source as TAdvCustomSearchList).FilterCondition);
    Columns.Assign((Source as TAdvCustomSearchList).Columns);
    ReadOnly := (Source as TAdvCustomSearchList).ReadOnly;
    ShowHint := (Source as TAdvCustomSearchList).ShowHint;
    Align := (Source as TAdvCustomSearchList).Align;
    Anchors := (Source as TAdvCustomSearchList).Anchors;
    {$IFNDEF FMXLIB}
    Images := (Source as TAdvCustomSearchList).Images;
    {$ENDIF}
  end;
end;

procedure TAdvCustomSearchList.Backspace;
begin
end;

procedure TAdvCustomSearchList.BeginUpdate;
begin
  {$IFDEF FMXLIB}
  inherited;
  {$ENDIF}
  inc(FUpdateCount);
  Items.BeginUpdate;
end;

function TAdvCustomSearchList.CategoryName(ID: Integer): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Categories.Count - 1 do
    if Categories[i].ID = ID then
    begin
      Result := Categories[i].Caption;
      break;
    end;
end;

{$IFNDEF DELPHIXE10_LVL}
procedure TAdvCustomSearchList.ChangeScale(M, D: Integer);
{$ENDIF}
{$IFDEF DELPHIXE10_LVL}
procedure TAdvCustomSearchList.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ENDIF}
begin
  inherited;
  FDPIScale := GetDPIScale(Self, Canvas);
end;

procedure TAdvCustomSearchList.ClearFilter;
begin
  FilterCondition.Text := '';
  UpdateFilter;
end;

procedure TAdvCustomSearchList.ClearSelection;
begin
end;

{$IFNDEF FMXLIB}
procedure TAdvCustomSearchList.CreateWnd;
begin
  inherited;
  FDPIScale := GetDPIScale(Self, Canvas);
{$IFDEF USEVCLSTYLES}
  InitVCLStyle(true);
{$ENDIF}
end;

procedure TAdvCustomSearchList.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if (Msg.CharCode = VK_RETURN) then
    Msg.Result := 1;
end;
{$ENDIF}

{$IFDEF USEVCLSTYLES}
procedure TAdvCustomSearchList.CMStyleChanged(var Message: TMessage);
begin
  InitVCLStyle(true);
end;
{$ENDIF}

procedure TAdvCustomSearchList.ColumnsChanged(Sender: TObject);
begin
  Refresh;
end;

procedure TAdvCustomSearchList.CopyToClipboard;
begin
end;

constructor TAdvCustomSearchList.Create(AOwner: TComponent);
begin
  inherited;

  FDPIScale := 1;

  FItemHeight := 20;
  FCategoryItemHeight := 20;

  FItems := CreateSearchList;
  FItems.OnChange := ListChanged;

  FColumns := CreateColumns;
  FColumns.Add;
  FColumns.OnChange := ColumnsChanged;

  FCategories := CreateCategories;

  FDisplayList := TDisplayList.Create;
  FAppearance := TAdvSearchListAppearance.Create;
  FAppearance.OnChange := AppearanceChanged;
  FFilterCondition := TFilterCondition.Create;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  {$IFNDEF FMXLIB}
  DoubleBuffered := true;
  {$ENDIF}
  Color := clWhite;

  Width := 250;
  Height := 175;

  if FDesignTime then
  begin
    Items.Add.Captions[0] := 'Item A';
    Items.Add.Captions[0] := 'Item B';
    Items.Add.Captions[0] := 'Item C';
  end;

  {$IFDEF FNCLIB}
  ReadOnly := true;
  CreateTextService;
  {$ENDIF}
  TabStop := true;

  FWheelIncrement := 8;
end;

function TAdvCustomSearchList.CreateCategories: TCategoryList;
begin
  Result := TCategoryList.Create(Self);
end;

function TAdvCustomSearchList.CreateColumns: TColumnItems;
begin
  Result := TColumnItems.Create(Self);
end;

function TAdvCustomSearchList.CreateSearchList: TSearchList;
begin
  Result := TSearchList.Create(Self);
end;

procedure TAdvCustomSearchList.CutToClipboard;
begin
end;

procedure TAdvCustomSearchList.DeleteSelection;
begin
end;

destructor TAdvCustomSearchList.Destroy;
begin
  FItems.Free;
  FColumns.Free;
  FCategories.Free;
  FDisplayList.Free;
  FFilterCondition.Free;
  FAppearance.Free;
  inherited;
end;

function TAdvCustomSearchList.DisplayToItemIndex(
  DisplayIndex: Integer): Integer;
begin
  Result := -1;
  if (DisplayIndex >= 0) and (DisplayIndex < FDisplayList.Count) then
    Result := FDisplayList.Items[DisplayIndex];
end;

procedure TAdvCustomSearchList.DoDrawSearchItemColumn(AIndex, AColumn: integer;
  ACanvas: TCanvas; ARect: TRect; AState: TItemState; var DefaultDraw: boolean);
begin
  if Assigned(OnDrawSearchItemColumn) then
    OnDrawSearchItemColumn(Self, AIndex, AColumn, ACanvas, ARect, AState, DefaultDraw);
end;

procedure TAdvCustomSearchList.DoGetSearchItemColumn(AIndex, AColumn: integer;
  var ACaption, ADescription: string; var AImageIndex: integer);
begin
  if Assigned(OnGetSearchItemColumn) then
    OnGetSearchItemColumn(Self, AIndex, AColumn, ACaption, ADescription, AImageIndex);
end;

procedure TAdvCustomSearchList.DoItemChange(ItemIndex: integer);
begin
  if Assigned(OnItemChange) then
  begin
    OnItemChange(Self, ItemIndex, Items[ItemIndex]);
  end;
end;

procedure TAdvCustomSearchList.DoItemClick(ItemIndex: integer);
begin
  if Assigned(OnItemClick) then
    OnItemClick(Self, ItemIndex, Items[ItemIndex]);
end;

{$IFDEF FMXLIB}
procedure TAdvCustomSearchList.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if WheelDelta > 0 then
  begin
    SelectNextPageItem;
    ScrollToItem(ItemIndex);
  end
  else
  begin
    SelectPrevPageItem;
    ScrollToItem(ItemIndex);

    if (ItemIndex > 0) then
    begin
      if Items[ItemIndex -1].ItemType = itCategory  then
      begin
        ScrollToItem(ItemIndex - 1);
      end;
    end;
  end;
  Handled := true;
end;
{$ENDIF}

{$IFNDEF FMXLIB}
function TAdvCustomSearchList.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  SelectNextPageItem;
  ScrollToItem(ItemIndex);

  Result := true;
end;

function TAdvCustomSearchList.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  SelectPrevPageItem;
  ScrollToItem(ItemIndex);

  if (ItemIndex > 0) then
  begin
    if Items[ItemIndex -1].ItemType = itCategory  then
    begin
      ScrollToItem(ItemIndex - 1);
    end;
  end;


  Result := true;
end;
{$ENDIF}


procedure TAdvCustomSearchList.DoSelectionChanged;
begin
end;

procedure TAdvCustomSearchList.EndUpdate;
begin
  {$IFDEF FMXLIB}
  inherited;
  {$ENDIF}
  if FUpdateCount > 0 then
    dec(FUpdateCount);
  Items.EndUpdate;
end;

function TAdvCustomSearchList.FilterItem(AItem: TSearchListItem): boolean;
var
  col: Integer;
  fs, fis: string;
  fromcol,tocol,diff,p: Integer;
begin
  Result := true;

  if FilterCondition.Categories then
  begin
    if (AItem.CategoryID >= 0) and (AItem.CategoryID < Categories.Count) and Categories[AItem.CategoryID].Filter then
    begin
      Result := false;
      Exit;
    end;
  end;

  if FilterCondition.AllColumns then
  begin
    fromcol := 0;
    tocol := Columns.Count - 1;
  end
  else
  begin
    fromcol := FilterCondition.Column;
    tocol := FilterCondition.Column;
  end;

  for col := fromcol to tocol do
  begin
    if (FilterCondition.FilterData in [fdText, fdAll]) then
    begin
      if FilterCondition.CaseSensitive then
      begin
        fs := FilterCondition.Text;
        fis := AItem.Captions[col];
      end
      else
      begin
        fs := FilterCondition.FTextUppercase;
        fis := Uppercase(AItem.Captions[col]);
      end;

      if FilterCondition.Text = '' then
        Result := true
      else
      begin
        case FilterCondition.FilterType of
        mText: Result := Pos(fs, fis) > 0;
        mStartWord: Result := Pos(fs, fis) = 1;
        mEndWord:
          begin
            diff := Length(fis) - Length(fs);
            if diff = 0 then
              Result := (fs = fis)
            else
            begin
              p := Pos(fs, fis);
              Result := (p > 0) and (p = diff + 1);
            end;
          end;
        end;
      end;
    end;

    if not Result and (FilterCondition.FilterData in [fdDescription, fdAll]) then
    begin
      if FilterCondition.CaseSensitive then
      begin
        fs := FilterCondition.Text;
        fis := AItem.Descriptions[col];
      end
      else
      begin
        fs := FilterCondition.FTextUppercase;
        fis := Uppercase(AItem.Descriptions[col]);
      end;

      if FilterCondition.Text = '' then
        Result := true
      else
      begin
        case FilterCondition.FilterType of
        mText: Result := Pos(fs, fis) > 0;
        mStartWord: Result := Pos(fs, fis) = 1;
        mEndWord:
          begin
            diff := Length(fis) - Length(fs);
            if diff = 0 then
              Result := (fs = fis)
            else
            begin
              p := Pos(fs, fis);
              Result := (p > 0) and (p = diff + 1);
            end;
          end;
        end;
      end;
    end;

    if Result then
      Break;
  end;

  if Assigned(OnFilterItem) then
    OnFilterItem(Self, AItem, Result);
end;

function TAdvCustomSearchList.GetCaretLH: integer;
begin
  Result := 0;
end;

function TAdvCustomSearchList.GetCaretXY: TPoint;
begin
  Result := Point(0,0);
end;

function TAdvCustomSearchList.GetClientHeight: integer;
begin
  {$IFNDEF FMXLIB}
  if HandleAllocated then
    Result := ClientHeight {- 3 * BorderSize - HScrollHeight}
  else
    Result := Height;
  {$ENDIF}

  {$IFDEF FMXLIB}
  Result := Round(Height) - ScrollSizeHorz;
  {$ENDIF}
end;

function TAdvCustomSearchList.GetClientWidth: integer;
begin

  {$IFNDEF FMXLIB}


  if HandleAllocated then
    {$IFDEF LCLLIB}
    Result := ClientWidth - 3 * BorderSize - VScrollWidth
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

function TAdvCustomSearchList.GetDocURL: string;
begin
  Result := TTMSFNCSearchListDocURL;
end;

function TAdvCustomSearchList.GetTipsURL: string;
begin
  Result := TTMSFNCSearchListTipsURL;
end;

function TAdvCustomSearchList.GetElementCount: integer;
begin
  Result := 0;
end;

function TAdvCustomSearchList.GetFilteredItemCount: integer;
begin
  Result := Items.Count - FDisplayList.Count;
end;

function TAdvCustomSearchList.GetItemCount: integer;
begin
  Result := FItemCount;
end;

function TAdvCustomSearchList.GetRows: integer;
begin
  Result := FDisplayList.Count;
end;

function TAdvCustomSearchList.GetSelectionFromXY: TPoint;
begin
  Result := Point(-1,-1);
end;

function TAdvCustomSearchList.GetSelectionToXY: TPoint;
begin
  Result := Point(-1,-1);
end;

function TAdvCustomSearchList.GetSize(VertSizeOnly: boolean): TSize;
var
  sz: TSize;
  i: integer;
begin
  FDisplayList.Clear;

  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].Visible then
      FDisplayList.Add(i);
  end;

  sz.cx := 0;
  for i := 0 to Columns.Count - 1 do
    sz.cx := sz.cx + Columns[i].Width;

  {$IFNDEF FMXLIB}
  sz.cx := Max(sz.cx, GetClientWidth - GetSystemMetrics(SM_CXVSCROLL));
  {$ENDIF}
  {$IFDEF FMXLIB}
  sz.cx := Max(sz.cx, GetClientWidth - VScrollWidth);
  {$ENDIF}

  sz.cy := GetRowYPos(FDisplayList.Count - 1, true);

  Result := sz;
end;

function TAdvCustomSearchList.GetTotalItemCount: integer;
begin
  Result := Items.Count;
end;

function TAdvCustomSearchList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomSearchList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvCustomSearchList.GetWordAndIndexAtCaret(var AValue: string;
  var AIndex: integer; SpaceOnly: boolean);
begin
end;

function TAdvCustomSearchList.HasSelection: boolean;
begin
  Result := false;
end;

{$IFDEF FNCLIB}
{$IFNDEF FMXLIB}
procedure TAdvCustomSearchList.SetAdaptToStyle(const Value: Boolean);
begin
  inherited;
{$IFDEF USEVCLSTYLES}
  InitVCLStyle(not Value);
{$ENDIF}
end;
{$ENDIF}
{$ENDIF}


{$IFDEF FMXLIB}
procedure TAdvCustomSearchList.InitStyle;
var
  c: TAlphaColor;

begin
  inherited;
  c := claNull;

  if TTMSFNCStyles.GetStyleBackgroundFillColor(c) then
    Color := c;

  c := claNull;

  if TTMSFNCStyles.GetStyleTextFontColor(c) then
    Font.Color := c;

  c := claNull;

  if TTMSFNCStyles.GetStyleSelectionFillColor(c) then
    Appearance.SelectionColor := c;

  c := claNull;

  if TTMSFNCStyles.GetStyleAlternativeTextFontColor(c) then
    Appearance.SelectionTextColor := c;

end;

procedure TAdvCustomSearchList.ResetToDefaultStyle;
begin
  inherited;
  Color := claWhite;
  Stroke.Color := claGray;
  Font.Color := claBlack;
  Appearance.SelectionColor := claBlue;
  Appearance.SelectionTextColor := claWhite;
end;
{$ENDIF}

{$IFDEF USEVCLSTYLES}
procedure TAdvCustomSearchList.InitVCLStyle(init: boolean);
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
    end;

    LStyle.GetElementColor(LDetails, ecBorderColor, clr);
    {$IFDEF DELPHIXE6_LVL}
    if seBorder in StyleElements then
    {$ENDIF}
    begin
      BorderColor := clr;
    end;

    LStyle.GetElementColor(LStyle.GetElementDetails(tgFixedCellNormal), ecBorderColor, clr);

    if LStyle.GetElementColor(LStyle.GetElementDetails(teEditTextNormal), ecTextColor, clr) and (clr <> clNone) then
    begin
      {$IFDEF DELPHIXE6_LVL}
      if seFont in StyleElements then
      {$ENDIF}
      begin
        Font.Color := clr;
      end;
    end;

    if LStyle.GetElementColor(LStyle.GetElementDetails(teEditTextSelected), ecFilLColor, clr) and (clr <> clNone) then
    begin
      Appearance.SelectionColor := clr;
    end
    else
      Appearance.SelectionColor := clHighlight;

    if LStyle.GetElementColor(LStyle.GetElementDetails(teEditTextSelected), ecTextColor, clr) and (clr <> clNone) then
    begin
      Appearance.SelectionTextColor := clr;
    end
    else
      Appearance.SelectionTextColor := clHighlightText;

  end
  else
  begin
    if init then
    begin
      Color := clWindow;
      BorderColor := BorderColor;
      Font.Color := Font.Color;
      Appearance.SelectionColor := Appearance.SelectionColor;
      Appearance.SelectionTextColor := Appearance.SelectionTextColor;
    end;
  end;
end;
{$ENDIF}

procedure TAdvCustomSearchList.InsertChar(ch: char);
begin

end;

procedure TAdvCustomSearchList.InsertChar(value: string);
begin

end;

function TAdvCustomSearchList.IsForwardSelection: boolean;
begin
  Result := false;
end;

procedure TAdvCustomSearchList.KeyDownN(var Key: Word; Shift: TShiftState);
var
  d: integer;
begin
  TopRow := TopLeft.Y div ItemHeight;

  case Key of
  KEY_DOWN:
    begin
      if ItemIndex < Rows - 1 then
      begin
        SelectNextItem(ItemIndex);

        d := GetRowYPos(ItemIndex, true);

        if d >= TopLeft.Y + GetClientHeight then
        begin
          ScrollDown(ItemHeight);
          // in case scrolling was not far enough
          d := GetRowYPos(ItemIndex, true);
          if d >= TopLeft.Y + GetClientHeight then
          begin
            ScrollDown(Round(FDPIScale * ItemHeight));
          end;
        end;
      end;
    end;
  KEY_UP:
    begin
      if ItemIndex > 0 then
      begin
        SelectPreviousItem(ItemIndex);
        d := GetRowYPos(ItemIndex);
        if d < TopLeft.Y then
          ScrollToItem(ItemIndex);
      end;
    end;
  KEY_HOME:
    begin
      SelectFirstItem;
      ScrollToItem(ItemIndex);
    end;
  KEY_END:
    begin
      SelectLastItem;
      ScrollToItem(ItemIndex);
    end;
  KEY_PRIOR:
    begin
      if SelectPrevPageItem then
        ScrollToItem(ItemIndex);
    end;
  KEY_NEXT:
    begin
      if SelectNextPageItem then
        ScrollToItem(ItemIndex);
    end;
  end;
end;

procedure TAdvCustomSearchList.KeyPressN(var Key: Char);
begin

end;

procedure TAdvCustomSearchList.KeyUpN(var Key: Word; Shift: TShiftState);
begin

end;

procedure TAdvCustomSearchList.ListChanged(Sender: TObject);
begin
  UpdateSize;
  Refresh;
end;

procedure TAdvCustomSearchList.MouseDownN(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i, h, idx: integer;
begin
  if AllowFocus and CanFocus then
    SetFocus;

  i := GetRowIndex(TopLeft.Y);
  h := GetRowYPos(i);

  idx := GetRowIndex(Y + h);

  if idx >= 0 then
    DoItemChange(DisplayToItemIndex(idx));

  ItemIndex := idx;

  Refresh;
end;

procedure TAdvCustomSearchList.MouseMoveN(Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TAdvCustomSearchList.MouseUpN(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i, h, idx: integer;
begin
  i := GetRowIndex(TopLeft.Y);
  h := GetRowYPos(i);

  idx := GetRowIndex(Y + h);

  if idx >= 0 then
    DoItemClick(DisplayToItemIndex(idx));
end;

procedure TAdvCustomSearchList.DoDblClick(X, Y: integer);
begin
  //
end;

procedure TAdvCustomSearchList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  {$IFNDEF FMXLIB}
  if (AOperation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    Refresh;
  end;
  {$ENDIF}

  inherited;
end;

procedure TAdvCustomSearchList.Paint;
var
  i,j,h,ih,li,x,img: integer;
  s,d: string;
  r,dr: TRect;
  sa: string;
  xsize,ysize: integer;
  defdraw,focusdraw,shapedraw: boolean;
  gr, grb: TAdvGraphics;
  tt: TAdvGraphicsTextTrimming;
  sc: single;
  str: TAdvGraphicsStroke;
begin
  inherited;

  sc := 1;

  {$IFDEF FMXLIB}
  if not (csDesigning in ComponentState) then
    sc := TAdvUtils.GetDPIScale;
  {$ENDIF}

  grb := TAdvGraphics.CreateBitmapCanvas(Round(Width * sc), Round(Height * sc));

  gr := TAdvGraphics.Create(Canvas);

  try
    grb.BeginScene;

    grb.Fill.Color := Color;
    {$IFDEF FMXLIB}
    grb.Stroke.Assign(Stroke);
    {$ENDIF}
    {$IFNDEF FMXLIB}
    grb.Stroke.Color := Color;
    {$ENDIF}

    grb.DrawRectangle(ConvertToRectF(Rect(0,0,Round(Width),Round(Height))));

    {$IFNDEF FMXLIB}
    grb.ImageList := Images;
    {$ENDIF}

    grb.Font.Assign(Font);

    TopRow := GetRowIndex(TopLeft.Y);

    if TopRow = -1 then
      TopRow := 0;

    h := 0;
    ih := Round(FDPIScale * ItemHeight);

    if FDesignTime then
    begin
      x := 0;
      grb.Stroke.Kind := gskDot;
      grb.Stroke.Color := gcSilver;

      for i := 0 to Columns.Count - 1 do
      begin
        x := x + Round(FDPIscale * Columns[i].Width);
        grb.DrawLine(PointF(x, 0), PointF(x, Height));
      end;
    end;


    for i := TopRow to Rows - 1 do
    begin
      li := FDisplayList.Items[i];

      {$IFDEF FMXLIB}
      if not (csDesigning in ComponentState) then
        x := -Round(HorizontalScrollBar.Value)
      else
        x := 0;
      {$ENDIF}

      {$IFNDEF FMXLIB}
      x := -HorzScrollBar.Position;
      {$ENDIF}

      focusdraw := false;

      for j := 0 to Columns.Count - 1 do
      begin
        if not Columns[j].Visible or (Columns[j].Width = 0) then
          Continue;

        s := Items[li].Captions[j];
        d := Items[li].Descriptions[j];
        img := Items[li].ImageIndexes[j];

        focusdraw := (i = ItemIndex) and (Items[li].ItemType = itItem);

        shapedraw := false;

        tt := gttNone;

        if Columns[j].Trimming then
          tt := gttWord;

        DoGetSearchItemColumn(li,j,s,d,img);

        if Items[li].ItemType = itCategory then
          ih := Round(FDPIScale * CategoryItemHeight)
        else
          ih := Round(FDPIScale * ItemHeight);


        if j < Columns.Count - 1 then
          r := Rect(x, h, Min(GetClientWidth, x + Round(FDPIScale * Columns[j].Width)), h + ih)
        else
          r := Rect(x, h, GetClientWidth, h + ih);

        grb.Font.Assign(Font);

        if (Items[li].ItemType = itCategory) then
        begin
          grb.Fill.Color := Appearance.CategoryColor;
          grb.Stroke.Color := Appearance.CategoryColor;

          if Appearance.CategoryControlFont then
          begin
            grb.Font.Assign(Appearance.CategoryFont);
            grb.Font.Height := Round(FDPIScale * Appearance.CategoryFont.Height);
          end;

          grb.DrawRectangle(ConvertToRectF(r));

          r.Left := r.Left + 2;
          r.Top := r.Top + 2;

          grb.DrawText(ConvertToRectF(r), s, false, gtaLeading, Columns[j].VertAlignment, tt);

          // last column
          if (j = Columns.Count - 1) and (Appearance.FilterCount = fcShow) then
          begin
            grb.Font.Assign(Appearance.FilterCountFont);
            grb.Font.Height := Round(FDPIScale * Appearance.FilterCountFont.Height);

            if (Appearance.FilterCountFormat <> '') then
               s := Format(Appearance.FilterCountFormat,[Items[li].CategoryCount])
            else
               s := '(' + inttostr(Items[li].CategoryCount)+ ')';

            grb.DrawText(ConvertToRectF(r),s, false, gtaTrailing, Columns[j].VertAlignment, tt);
          end;
          grb.Font.Assign(Font);
        end
        else
        begin
          if not Columns[j].ControlFont then
          begin
            grb.Font.Assign(Columns[j].Font);
            grb.Font.Height := Round(FDPIScale * Columns[j].Font.Height);
          end;

          if Appearance.Banding then
          begin
            if Odd(i) then
              grb.Fill.Color := Appearance.BandColorOdd
            else
              grb.Fill.Color := Appearance.BandColorEven;

            grb.Fill.Kind := gfkSolid;
            grb.Stroke.Color := grb.Fill.Color;
            grb.DrawRectangle(ConvertToRectF(r));
          end;

          // item is selected
          if (i = ItemIndex) then
          begin
            grb.Fill.Color := Appearance.SelectionColor;
            grb.Stroke.Color := grb.Fill.Color;
            grb.DrawRectangle(ConvertToRectf(r));
            grb.Font.Color := Appearance.SelectionTextColor;
          end
          else
          begin
            if (Columns[j].Color <> TColor(clNone)) then
            begin
              grb.Fill.Color := Columns[j].Color;
              grb.Stroke.Color := grb.Fill.Color;
              grb.DrawRectangle(ConvertToRectF(r));
            end;
          end;

          if Assigned(Items[li].Columns[j]) and (Items[li].Columns[j].Color <> TColor(clNone)) then
          begin
            grb.Fill.Kind := gfkSolid;
            grb.Fill.Color := Items[li].Columns[j].Color;
            grb.Stroke.Color := Items[li].Columns[j].Color;

            dr := r;
            case Items[li].Columns[j].Shape of
            sRect:
              begin
                InflateRect(dr, -3,-3);
                shapedraw := true;
              end;
            sBar:
              begin
                InflateRect(dr, -2,-2);
                dr.Right := dr.Left + BAR_WIDTH;
                r.Left := r.Left + 2 + BAR_WIDTH;
              end;
            end;

            grb.DrawRectangle(ConvertToRectF(dr));
          end;

          defdraw := true;

          DoDrawSearchItemColumn(li,j,grb.Canvas,r,[],defdraw);

          if defdraw then
          begin
            r.Left := r.Left + 2;
            r.Top := r.Top + 2;
            ysize := 0;

            if Assigned(Items[li].Columns[j]) and Assigned(Items[li].Columns[j].Picture) and

            {$IFNDEF FMXLIB}

            {$IFNDEF FNCLIB}
              not Items[li].Columns[j].Picture.Empty then
                grb.DrawBitmap(ConvertToRectf(r), Items[li].Columns[j].Picture);
            {$ENDIF}

            {$IFDEF FNCLIB}
              Assigned(Items[li].Columns[j].Picture.Graphic) and
              not Items[li].Columns[j].Picture.Graphic.Empty then
                grb.DrawBitmap(ConvertToRectf(r), Items[li].Columns[j].Picture.Graphic);
            {$ENDIF}

            {$ENDIF}

            {$IFDEF FMXLIB}
              not Items[li].Columns[j].Picture.IsEmpty then
                grb.DrawBitmap(ConvertToRectf(r), Items[li].Columns[j].Picture);
            {$ENDIF}

            if Assigned(Items[li].Columns[j]) and (Items[li].Columns[j].TextColor <> TColor(clNone)) then
              grb.Font.Color := Items[li].Columns[j].TextColor;

            {$IFNDEF FMXLIB}
            if Assigned(Images) and (img >= 0) then
            begin
              Images.Draw(grb.Canvas, r.Left, r.Top, img);
              r.Left := r.Left + Images.Width + 2;
            end;
            {$ENDIF}

            if ((FFilterCondition.Column = j) or FFilterCondition.AllColumns) and (FFilterCondition.Text <> '') and (FFilterCondition.FilterData in [fdText, fdAll]) and ((Appearance.HighlightTextColor <> clNone) or (Appearance.HighlightColor <> clNone) or (Appearance.HighlightFontStyle <> Font.Style)) then
              s := hilight(s,FFilterCondition.Text, 'hi', FilterCondition.CaseSensitive, false);

            grb.HighlightColor := Appearance.HighlightColor;
            grb.HighlightTextColor := Appearance.HighlightTextColor;
            grb.HighlightFontStyle := Appearance.HighlightFontStyle;

            if shapedraw and (pos('</', s) = 0) then
            begin
              grb.DrawText(ConvertToRectF(dr),s,false,gtaCenter, gtaCenter);
            end
            else
            begin
              if FDesignTime and (s = '') then
                s := 'item '+inttostr(j)+':'+inttostr(i);

              YSize := Round(grb.CalculateTextSize(s,ConvertToRectF(r)).cy);

              if (j < Columns.Count - 1) or (Columns[j].Alignment in [gtaTrailing, gtaCenter]) then
                r.Right := r.Left + Round(FDPIScale * Columns[j].Width)
              else
                r.Right := 10000;

              grb.DrawText(ConvertToRectF(r),s,Columns[j].WordWrap, Columns[j].Alignment, Columns[j].VertAlignment, tt);

              if Appearance.ShowItemCategory and (FilterCondition.Column = j) then
              begin
                sa := CategoryName(Items[li].CategoryID);
                if sa <> '' then
                begin
                  XSize := Round(grb.CalculateTextSize(s,ConvertToRectF(r)).cx);
                  s := Format(Appearance.ItemCategoryFormat, [sa]);

                  grb.Font.Assign(Appearance.ItemCategoryFont);
                  grb.Font.Height := Round(FDPIScale * Appearance.ItemCategoryFont.Height);

                  r.Left := r.Left + xsize + 4;
                  grb.Fill.Kind := gfkNone;
                  grb.DrawText(ConvertToRectF(r), s, Columns[j].WordWrap, Columns[j].Alignment, Columns[j].VertAlignment, tt);
                  grb.Fill.Kind := gfkSolid;
                end;
              end;
            end;

            if d <> '' then
            begin
              r.Top := r.Top + YSize;

              if not Appearance.DescriptionControlFont then
              begin
                grb.Font.Assign(Appearance.DescriptionFont);
                grb.Font.Height := Round(FDPIScale * Appearance.DescriptionFont.Height);

                if (i = ItemIndex) then
                begin
                  grb.Font.Color := Appearance.SelectionTextColor;
                end;
              end;

              if ((FFilterCondition.Column = j) or FFilterCondition.AllColumns) and (FFilterCondition.Text <> '') and (FFilterCondition.FilterData in [fdDescription, fdAll]) and ((Appearance.HighlightTextColor <> clNone) or (Appearance.HighlightColor <> clNone) or (Appearance.HighlightFontStyle <> Font.Style)) then
                d := hilight(d, FFilterCondition.Text, 'hi', FilterCondition.CaseSensitive, false);

              grb.URLColor := clBlue;
              grb.DrawText(ConvertToRectF(r), d, Columns[j].WordWrap, Columns[j].Alignment, Columns[j].VertAlignment);
            end;
          end;

          if i = ItemIndex then
          begin
            grb.Font.Color := clWindowText;
            grb.Fill.Color := clWindow;
          end;
        end;

        x := x + Round(FDPIScale * Columns[j].Width);
      end;

      if focusdraw and Focused then
      begin
        r := Rect(0,h, GetClientWidth - 1, h + ih);
        grb.DrawFocusRectangle(r);

        str := TAdvGraphicsStroke.Create(gskSolid, gcWhite);
        try
          str.Assign(grb.Stroke);
          grb.DrawFocusRectangle(r);
          grb.Stroke.Assign(str);
        finally
          str.Free;
        end;
      end;


      h := h + ih;
      if h > Height then
        break;
    end;

    grb.EndScene;
    gr.DrawBitmap(RectF(0,0,Width,Height), grb.Bitmap);

  finally
    gr.Free;
    grb.Free;
  end;
end;

function TAdvCustomSearchList.PasteFromClipboard: string;
begin
  Result := '';
end;

procedure TAdvCustomSearchList.Refresh;
begin
  {$IFDEF FMXLIB}
  Repaint;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Invalidate;
  {$ENDIF}
end;

procedure TAdvCustomSearchList.DoEnter;
begin
  inherited;
  Refresh;
end;

procedure TAdvCustomSearchList.DoExit;
begin
  inherited;
  Refresh;
end;


procedure TAdvCustomSearchList.Resize;
begin
  inherited;
  UpdateSize;
  Refresh;
end;

procedure TAdvCustomSearchList.ScrollToItem(const AIndex: Integer);
var
  h: integer;
begin
  h := GetRowYPos(AIndex);
  ScrollVert(h);
end;

procedure TAdvCustomSearchList.SelectAll;
begin
end;

function TAdvCustomSearchList.SelectedItem: TSearchListItem;
var
  li: Integer;
begin
  Result := nil;

  if (ItemIndex >=0) and (ItemIndex < FDisplayList.Count) then
  begin
    li := FDisplayList.Items[ItemIndex];
    Result := Items[li];
  end;
end;

function TAdvCustomSearchList.SelectedText: string;
begin
  Result := '';
end;

function TAdvCustomSearchList.SelectFirstItem: boolean;
begin
  Result := SelectNextItem(-1);
end;

function TAdvCustomSearchList.SelectLastItem: boolean;
begin
  Result := SelectPreviousItem(Rows);
end;

function TAdvCustomSearchList.SelectNextItem(AIndex: Integer): boolean;
var
  li,idx: integer;
begin
  Result := false;

  while AIndex + 1 < Rows do
  begin
    li := FDisplayList.Items[AIndex + 1];

    if Items[li].ItemType <> itCategory then
    begin
      idx := AIndex + 1;
      DoItemChange(DisplayToItemIndex(idx));
      ItemIndex := idx;
      Result := true;
      Break;
    end;
    inc(AIndex);
  end;
end;

function TAdvCustomSearchList.SelectPreviousItem(AIndex: Integer): boolean;
var
  li,idx: integer;
begin
  Result := false;

  while AIndex - 1 >= 0 do
  begin
    li := FDisplayList.Items[AIndex - 1];

    if Items[li].ItemType <> itCategory then
    begin
      idx := AIndex - 1;
      DoItemChange(DisplayToItemIndex(idx));
      ItemIndex := idx;
      Result := true;
      Break;
    end;
    dec(AIndex);
  end;
end;

function TAdvCustomSearchList.SelectPrevPageItem: boolean;
var
  d,i: integer;
begin
  Result := false;
  if ItemIndex > 0 then
  begin
    d := ItemIndex;
    i := Max(1, ItemIndex - FWheelIncrement);
    while not SelectPreviousItem(i) and (i < d) do
      inc(i);
    Result := true;
  end;
end;

function TAdvCustomSearchList.SelectNextPageItem: boolean;
var
  d,i: integer;
begin
  Result := false;

  if ItemIndex < Rows - 1 then
  begin
    d := ItemIndex;
    i := Min(ItemIndex + FWheelIncrement, Rows - 2);
    while not SelectNextItem(i) and (i > d) do
      dec(i);
    Result := true;
  end;
end;


function TAdvCustomSearchList.SelectWordAtCaret: string;
begin
  Result := '';
end;

procedure TAdvCustomSearchList.SetAppearance(const Value: TAdvSearchListAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvCustomSearchList.SetCategories(const Value: TCategoryList);
begin
  FCategories.Assign(Value);
end;

procedure TAdvCustomSearchList.SetCategoryItemHeight(const Value: integer);
begin
  if (FCategoryItemHeight <> Value) then
  begin
    FCategoryItemHeight := Value;
    Refresh;
  end;
end;

procedure TAdvCustomSearchList.SetColumns(const Value: TColumnItems);
begin
  FColumns.Assign(Value);
end;

procedure TAdvCustomSearchList.SetFilterCondition(const Value: TFilterCondition);
begin
  FFilterCondition.Assign(Value);
end;

{$IFNDEF FMXLIB}
procedure TAdvCustomSearchList.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Refresh;
end;
{$ENDIF}

procedure TAdvCustomSearchList.SetItemHeight(const Value: integer);
begin
  if (FItemHeight <> Value) then
  begin
    FItemHeight := Value;
    UpdateSize;
    Refresh;
  end;
end;

procedure TAdvCustomSearchList.SetItemIndex(const Value: integer);
begin
  FItemIndex := Value;
  Refresh;
end;

procedure TAdvCustomSearchList.SetItems(const Value: TSearchList);
begin
  FItems := Value;
end;

procedure TAdvCustomSearchList.SetTopRow(const Value: integer);
begin
  FTopRow := Value;
end;

procedure TAdvCustomSearchList.SetVersion(const Value: string);
begin
//
end;

function TAdvCustomSearchList.GetRowIndex(Y: Integer): Integer;
var
  i,d: Integer;
begin
  Result := -1;

  d := 0;

  if Y = 0 then
  begin
    Result := 0;
    Exit;
  end;

  for i := 0 to FDisplayList.Count - 1 do
  begin
    if Items[FDisplayList.Items[i]].ItemType = itCategory then
      d := d + Round(FDPIScale * CategoryItemHeight)
    else
      d := d + Round(FDPIScale * ItemHeight);

    if d > Y then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAdvCustomSearchList.GetRowYPos(AIndex: Integer; EndY: boolean = false): Integer;
var
  i,d,e: Integer;
begin
  d := 0;
  e := 0;

  if EndY then
    e := 1;

  if AIndex < FDisplayList.Count then
  begin
    for i := 0 to AIndex - 1 + e do
    begin
      if Items[FDisplayList.Items[i]].ItemType = itCategory then
        d := d + Round(FDPIScale * CategoryItemHeight)
      else
        d := d + Round(FDPIScale * ItemHeight);
    end;
  end;

  if EndY then
    d := d + Round(FDPIScale * ItemHeight);


  Result := d;
end;


procedure TAdvCustomSearchList.UnSelect;
begin
  ClearFilter;
  FItemIndex := -1;
end;

procedure TAdvCustomSearchList.UpdateFilter;
var
  i,idx,lastcat: integer;
  sel,res,gotmax: boolean;
begin
  sel := false;
  idx := -1;
  lastcat := -1;
  gotmax := false;

  FItemCount := 0;

  FHighlightText := FilterCondition.Text;

  Items.BeginUpdate;

  for i := 0 to Items.Count - 1 do
  begin
    if (Items[i].ItemType = itCategory) then
    begin
      lastcat := i;
      Items[i].CategoryCount := 0;
      gotmax := false;
    end;

    if (Items[i].ItemType <> itCategory) or (FilterCondition.CategoryItems) then
    begin
      res := FilterItem(Items[i]);
      Items[i].Visible := res and not gotmax;

      if res then
        inc(FItemCount);

      if res and (lastcat <> -1) and FilterCondition.CategoryItems then
      begin
        Items[lastcat].Visible := true;

        if (Items[i].ItemType <> itCategory) then
        begin
          Items[lastcat].CategoryCount := Items[lastcat].CategoryCount + 1;
          if FilterCondition.MaxCategoryItems > 0 then
          begin
            gotmax := Items[lastcat].CategoryCount >= FilterCondition.MaxCategoryItems;
          end;
        end;
      end;

      if not sel and res and FilterCondition.AutoSelect and (FHighLightText <> '') and not (Items[i].ItemType = itCategory) then
      begin
        idx := i;
        sel := true;
      end;
    end;
  end;

  Items.EndUpdate;

  if idx <> -1 then
  begin
    if FDisplayList.Count = 0 then
      GetSize(true);

    ItemIndex := FDisplayList.IndexOf(idx);
  end
  else
    ItemIndex := -1;
end;

procedure TAdvCustomSearchList.UpdateSelection;
begin
end;

procedure TAdvCustomSearchList.UpdateSelectionPoint(LeftSel: boolean; var X, Y: integer);
begin
end;

procedure TAdvCustomSearchList.UpdateSize;
var
  sz: TSize;
  cond: boolean;
begin
  if FUpdateCount > 0 then
    Exit;

  if (csDestroying in ComponentState) then
    Exit;

  if not HandleAllocated then
    Exit;

  FUpdateCount := 1;

  sz := GetSize(False);

  cond := sz.cx > GetClientWidth;
  sz.cx := 0;

  // recalculate size in case SetRange affects scrollbar
  if SetRange(sz) or cond then
  begin
    {$IFNDEF LCLLIB}
    SetRange(GetSize(False));
    {$ENDIF}
    {$IFDEF VCLLIB}
    VertScrollBar.Increment := ItemHeight;
    {$ENDIF}
    {$IFDEF LCLLIB}
    VertScrollBar.SmallChange := ItemHeight;
    {$ENDIF}
    {$IFDEF FMXLIB}
    VerticalScrollBar.SmallChange := ItemHeight;
    {$ENDIF}
  end;

  FUpdateCount := 0;
end;

procedure TAdvCustomSearchList.UpdateWordAndIndexAtCaret(AValue: string;
  AIndex: integer; SpaceOnly: boolean = false);
begin
end;

{$IFNDEF FMXLIB}
procedure TAdvCustomSearchList.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;
{$ENDIF}

{ TSearchList }

function TSearchList.Add: TSearchListItem;
begin
  Result := TSearchListItem(inherited Add);
end;

function TSearchList.Add(ACaption, ADescription: string): TSearchListItem;
begin
  Result := TSearchListItem(inherited Add);
  Result.Captions[0] := ACaption;
  Result.Descriptions[0] := ADescription;
end;

function TSearchList.Add(ACaption, ADescription: string;
  AObject: TObject): TSearchListItem;
begin
  Result := TSearchListItem(inherited Add);
  Result.Captions[0] := ACaption;
  Result.Descriptions[0] := ADescription;
  Result.&Object := AObject;
end;

procedure TSearchList.Changed;
begin
  if (UpdateCount = 0) and Assigned(OnChange) then
    OnChange(Self);
end;

function TSearchList.Compare(Item1, Item2: TCollectionItem): integer;
var
  iright, ileft: TSearchListItem;
  s1,s2: string;
begin
  ileft := TSearchListItem(Item1);
  iright := TSearchListItem(Item2);

  if ileft.CategoryID = iright.CategoryID then
  begin
    if ileft.ItemType = iright.ItemType then
    begin
      s1 := ileft.Captions[FSortColumn];
      s2 := iright.Captions[FSortColumn];

      if not FSortCaseSensitive then
      begin
        s1 := Uppercase(s1);
        s2 := Uppercase(s2);
      end;

      Result := CompareStr(s1, s2);
    end
    else
    begin
      if ileft.ItemType = itCategory then
        Result := -1
      else
        Result := 1;
    end;
  end
  else
  begin
    if ileft.CategoryID > iright.CategoryID then
      Result := 1
    else
      Result := -1;
  end;
end;

constructor TSearchList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, GetItemClass);
end;

function TSearchList.GetItem(AIndex: integer): TSearchListItem;
begin
  Result := TSearchListItem(inherited Items[AIndex]);
end;

function TSearchList.GetItemClass: TCollectionItemClass;
begin
  Result := TSearchListItem;
end;

function TSearchList.IndexOf(ACaption: string): integer;
var
  i: integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    if Items[i].Captions[0] = ACaption then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TSearchList.Insert(AIndex: integer): TSearchListItem;
begin
  Result := TSearchListItem(inherited Insert(AIndex));
end;

procedure TSearchList.LoadStrings(Value: TStrings);
var
  i: integer;
begin
  BeginUpdate;

  for i := 0 to Value.Count - 1 do
  begin
    Add.Captions[0] := Value.Strings[i];
  end;

  EndUpdate;
end;

procedure TSearchList.SetItem(AIndex: integer; const Value: TSearchListItem);
begin
  inherited Items[AIndex] := Value;
end;


{$IFNDEF LCLLIB}
procedure TSearchList.QuickSort(L, R: Integer);
var
  I, J, p: Integer;
  Save: TCollectionItem;
  {$IFDEF DELPHIXE3_LVL}
  SortList: TList<TCollectionItem>;
  {$ENDIF}
  {$IFNDEF DELPHIXE3_LVL}
  SortList: TList;
  {$ENDIF}
begin
  //This cast allows us to get at the private elements in the base class
  SortList := TShadowedCollection(Self).FItems;

  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Compare(Items[I], Items[P]) < 0 do
        Inc(I);
      while Compare(Items[J], Items[P]) > 0 do
        Dec(J);

      if I <= J then
      begin
        Save              := TCollectionItem(SortList.Items[I]);
        SortList.Items[I] := SortList.Items[J];
        SortList.Items[J] := Save;

        if P = I then
          P := J
        else
          if P = J then
            P := I;

        Inc(I);
        Dec(J);
      end;
    until I > J;

    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;

end;
{$ENDIF}

{$IFDEF LCLLIB}
function CompareFN(a, b: TCollectionItem): integer;
var
  iright, ileft: TSearchListItem;
  s1,s2: string;
begin
  ileft := TSearchListItem(a);
  iright := TSearchListItem(b);

  if ileft.CategoryID = iright.CategoryID then
  begin
    if ileft.ItemType = iright.ItemType then
    begin
      s1 := ileft.Captions[cslcol];
      s2 := iright.Captions[cslcol];

      if not cslcase then
      begin
        s1 := Uppercase(s1);
        s2 := Uppercase(s2);
      end;

      Result := CompareStr(s1, s2);
    end
    else
    begin
      if ileft.ItemType = itCategory then
        Result := -1
      else
        Result := 1;
    end;
  end
  else
  begin
    if ileft.CategoryID > iright.CategoryID then
      Result := 1
    else
      Result := -1;
  end;

  if csldir = sdUp then
    Result := -1 * Result;
end;
{$ENDIF}


procedure TSearchList.Sort(AColumn: Integer; ADirection: TSortDirection;
  CaseSensitive: boolean);
begin
  FSortColumn := AColumn;
  FSortDirection := ADirection;
  FSortCaseSensitive := CaseSensitive;

  {$IFDEF LCLLIB}
  cslcomp := @CompareFN;
  cslcol := AColumn;
  cslcase := CaseSensitive;
  csldir := ADirection;
  inherited Sort(cslcomp);
  {$ENDIF}

  {$IFNDEF LCLLIB}
  if Count > 1 then
    QuickSort(0, pred(Count));
  {$ENDIF}
end;


procedure TSearchList.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

function TSearchList.VisibleItems: integer;
var
  i,r: integer;
begin
  r := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Visible then
      inc(r);
  end;

  Result := r;
end;

{ TSearchListItem }

procedure TSearchListItem.Assign(Source: TPersistent);
begin
  if (Source is TSearchListItem) then
  begin
    FCategoryID := (Source as TSearchListItem).CategoryID;
    Columns.Assign((Source as TSearchListItem).Columns);
    FItemType := (Source as TSearchListItem).ItemType;
    FTag := (Source as TSearchListItem).Tag;
    FOwnsObject := (Source as TSearchListItem).OwnsObject;
    FObject := (Source as TSearchListItem).FObject;
  end;
end;

procedure TSearchListItem.ColumnsChanged(Sender: TObject);
begin
  (Collection as TSearchList).Changed;
end;

constructor TSearchListItem.Create(Collection: TCollection);
begin
  inherited;
  FColumns := CreateColumns;
  FColumns.OnChange := ColumnsChanged;
  FColumns.Add;
  FItemType := itItem;
  FVisible := True;
  FTag := 0;
end;

function TSearchListItem.CreateColumns: TSearchColumnItems;
begin
  Result := TSearchColumnItems.Create(Self);
end;

destructor TSearchListItem.Destroy;
begin
  if OwnsObject and Assigned(FObject) then
    FObject.Free;

  FColumns.Free;
  inherited;
end;

function TSearchListItem.GetCaptions(AIndex: Integer): string;
begin
  Result := '';
  if AIndex < Columns.Count then
    Result := Columns[AIndex].Caption;
end;

function TSearchListItem.GetDescriptions(AIndex: Integer): string;
begin
  Result := '';
  if AIndex < Columns.Count then
    Result := Columns[AIndex].Description;
end;

function TSearchListItem.GetImageIndexes(AIndex: Integer): TImageIndex;
begin
  Result := -1;
  if AIndex < Columns.Count then
    Result := Columns[AIndex].ImageIndex;
end;

procedure TSearchListItem.SetCaptions(AIndex: Integer; const Value: string);
begin
  while Columns.Count <= AIndex do
    Columns.Add;

  Columns[AIndex].Caption := Value
end;

procedure TSearchListItem.SetColumns(const Value: TSearchColumnItems);
begin
  FColumns.Assign(Value);
end;

procedure TSearchListItem.SetDescriptions(AIndex: Integer; const Value: string);
begin
  while Columns.Count <= AIndex do
    Columns.Add;

  Columns[AIndex].Description := Value;
  Changed(False);
end;

procedure TSearchListItem.SetImageIndexes(AIndex: Integer;
  const Value: TImageIndex);
begin
  while Columns.Count <= AIndex do
    Columns.Add;

  Columns[AIndex].ImageIndex := Value;
  Changed(False);
end;

procedure TSearchListItem.SetItemType(const Value: TSearchItemType);
begin
  if (FItemType <> Value) then
  begin
    FItemType := Value;
    Changed(False);
  end;
end;

procedure TSearchListItem.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

{ TAdvSearchListAppearance }

procedure TAdvSearchListAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvSearchListAppearance) then
  begin
    FBanding := (Source as TAdvSearchListAppearance).Banding;
    FBandColorEven := (Source as TAdvSearchListAppearance).BandColorEven;
    FBandColorOdd := (Source as TAdvSearchListAppearance).BandColorOdd;
    FCategoryColor := (Source as TAdvSearchListAppearance).CategoryColor;
    FCategoryControlFont := (Source as TAdvSearchListAppearance).CategoryControlFont;
    FCategoryFont.Assign((Source as TAdvSearchListAppearance).CategoryFont);
    FHighlightTextColor := (Source as TAdvSearchListAppearance).HighlightTextColor;
    FHighlightColor := (Source as TAdvSearchListAppearance).HighlightColor;
    FHighlightFontStyle := (Source as TAdvSearchListAppearance).HighlightFontStyle;
    FSelectionTextColor := (Source as TAdvSearchListAppearance).SelectionTextColor;
    FSelectionColor := (Source as TAdvSearchListAppearance).SelectionColor;
    FDescriptionFont.Assign((Source as TAdvSearchListAppearance).DescriptionFont);
    FDescriptionControlFont := (Source as TAdvSearchListAppearance).DescriptionControlFont;
    FFilterCountFont.Assign((Source as TAdvSearchListAppearance).FilterCountFont);
    FFilterCount := (Source as TAdvSearchListAppearance).FilterCount;
    FFilterCountFormat := (Source as TAdvSearchListAppearance).FilterCountFormat;
    FItemCategoryFont.Assign((Source as TAdvSearchListAppearance).ItemCategoryFont);
    FItemCategoryFormat := (Source as TAdvSearchListAppearance).ItemCategoryFormat;
    FShowItemCategory := (Source as TAdvSearchListAppearance).ShowItemCategory;
  end;
end;

procedure TAdvSearchListAppearance.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvSearchListAppearance.Create;
begin
  inherited;
  FCategoryColor := TColor(clSilver);
  FCategoryControlFont := True;
  FCategoryFont := TFont.Create;
  {$IFNDEF FMXLIB}
  FCategoryFont.OnChange := CategoryFontChanged;
  {$ENDIF}
  {$IFDEF FMXLIB}
  FCategoryFont.OnChanged := CategoryFontChanged;
  {$ENDIF}
  FHighlightTextColor := TColor(clRed);
  FHighlightColor := clNone;
  FHighlightFontStyle := [TFontStyle.fsBold];
  FSelectionColor := TColor(clHighlight);
  FSelectionTextColor := TColor(clHighlightText);
  FDescriptionFont := TFont.Create;
  {$IFNDEF FMXLIB}
  FDescriptionFont.OnChange := DescriptionFontChanged;
  {$ENDIF}
  {$IFDEF FMXLIB}
  FDescriptionFont.OnChanged := DescriptionFontChanged;
  {$ENDIF}
  FDescriptionControlFont := true;
  FFilterCount := fcHide;
  FFilterCountFont := TFont.Create;
  FFilterCountFormat := '(%d)';
  FBanding := False;
  FBandColorOdd := TColor(clInfoBk);
  FBandColorEven := TColor(clWindow);
  FItemCategoryFont := TFont.Create;
  {$IFNDEF FMXLIB}
  FItemCategoryFont.OnChange := ItemCategoryFontChanged;
  {$ENDIF}
  {$IFDEF FMXLIB}
  FItemCategoryFont.OnChanged := ItemCategoryFontChanged;
  {$ENDIF}
  FItemCategoryFont.Color := TColor(clOrange);
  FItemCategoryFormat := 'in %s';
end;

destructor TAdvSearchListAppearance.Destroy;
begin
  FCategoryFont.Free;
  FDescriptionFont.Free;
  FFilterCountFont.Free;
  FItemCategoryFont.Free;
  inherited;
end;

procedure TAdvSearchListAppearance.CategoryFontChanged(Sender: TObject);
begin
  FCategoryControlFont := false;
  Changed;
end;

procedure TAdvSearchListAppearance.DescriptionFontChanged(Sender: TObject);
begin
  FDescriptionControlFont := false;
  Changed;
end;

procedure TAdvSearchListAppearance.ItemCategoryFontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSearchListAppearance.SetBandColorEven(const Value: TColor);
begin
  if (FBandColorEven <> Value) then
  begin
    FBandColorEven := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetBandColorOdd(const Value: TColor);
begin
  if (FBandColorOdd <> Value) then
  begin
    FBandColorOdd := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetBanding(const Value: boolean);
begin
  if (FBanding <> Value) then
  begin
    FBanding := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetCategoryColor(const Value: TColor);
begin
  if (FCategoryColor <> Value) then
  begin
    FCategoryColor := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetCategoryControlFont(const Value: boolean);
begin
  if (FCategoryControlFont <> Value) then
  begin
    FCategoryControlFont := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetCategoryFont(const Value: TFont);
begin
  FCategoryFont.Assign(Value);
  Changed;
end;

procedure TAdvSearchListAppearance.SetDescriptionControlFont(
  const Value: boolean);
begin
  FDescriptionControlFont := Value;
end;

procedure TAdvSearchListAppearance.SetDescriptionFont(const Value: TFont);
begin
  FDescriptionFont.Assign(Value);
  Changed;
end;

procedure TAdvSearchListAppearance.SetFilterCount(const Value: TFilterCount);
begin
  if (FFilterCount <> Value) then
  begin
    FFilterCount := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetFilterCountFont(const Value: TFont);
begin
  FFilterCountFont.Assign(Value);
  Changed;
end;

procedure TAdvSearchListAppearance.SetFilterCountFormat(const Value: string);
begin
  if (FFilterCountFormat <> Value) then
  begin
    FFilterCountFormat := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetHighlightColor(const Value: TColor);
begin
  if (FHighlightColor <> Value) then
  begin
    FHighlightColor := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetHighlightFontStyle(
  const Value: TFontStyles);
begin
  if (FHighlightFontStyle <> Value) then
  begin
    FHighlightFontStyle := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetHighlightTextColor(const Value: TColor);
begin
  if (FHighlightTextColor <> Value) then
  begin
    FHighlightTextColor := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetItemCategoryFont(const Value: TFont);
begin
  FItemCategoryFont.Assign(Value);
  Changed;
end;

procedure TAdvSearchListAppearance.SetItemCategoryFormat(const Value: string);
begin
  if (FItemCategoryFormat <> Value) then
  begin
    FItemCategoryFormat := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetSelectionColor(const Value: TColor);
begin
  if (FSelectionColor <> Value) then
  begin
    FSelectionColor := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetSelectionTextColor(const Value: TColor);
begin
  if (FSelectionTextColor <> Value) then
  begin
    FSelectionTextColor := Value;
    Changed;
  end;
end;

procedure TAdvSearchListAppearance.SetShowItemCategory(const Value: boolean);
begin
  if (FShowItemCategory <> Value) then
  begin
    FShowItemCategory := Value;
    Changed;
  end;
end;

{ TFilterCondition }

procedure TFilterCondition.Assign(Source: TPersistent);
begin
  if (Source is TFilterCondition) then
  begin
    FAllColumns := (Source as TFilterCondition).AllColumns;
    FAutoSelect := (Source as TFilterCondition).AutoSelect;
    FFilterType := (Source as TFilterCondition).FilterType;
    FFilterData := (Source as TFilterCondition).FilterData;
    FText := (Source as TFilterCondition).Text;
    FCategories := (Source as TFilterCondition).Categories;
    FColumn := (Source as TFilterCondition).Column;
    FCaseSensitive := (Source as TFilterCondition).CaseSensitive;
    FCategoryItems := (Source as TFilterCondition).CategoryItems;
    FMaxCategoryItems := (Source as TFilterCondition).MaxCategoryItems;
  end;
end;

constructor TFilterCondition.Create;
begin
  inherited Create;

  FAllColumns := False;
  FCaseSensitive := False;
  FColumn := 0;
  FFilterType := TFilterType.mText;
  FCategoryItems := False;
  FFilterData := TFilterData.fdText;
  FMaxCategoryItems := -1;
end;

procedure TFilterCondition.SetColumn(const Value: Integer);
begin
  FColumn := Value;
end;

procedure TFilterCondition.SetFilterData(const Value: TFilterData);
begin
  FFilterData := Value;
end;

procedure TFilterCondition.SetFilterType(const Value: TFilterType);
begin
  FFilterType := Value;
end;

procedure TFilterCondition.SetText(const Value: string);
begin
  FText := Value;
  FTextUppercase := UpperCase(FText);
end;


{ TCategoryItem }

procedure TCategoryItem.Assign(Source: TPersistent);
begin
  if (Source is TCategoryItem) then
  begin
    FID := (Source as TCategoryItem).ID;
    FCaption := (Source as TCategoryItem).Caption;
    FTag := (Source as TCategoryItem).Tag;
    FFilter := (Source as TCategoryItem).Filter;
  end;
end;

procedure TCategoryItem.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TCategoryItem.SetFilter(const Value: boolean);
begin
  FFilter := Value;
end;

{ TCategoryList }

function TCategoryList.Add: TCategoryItem;
begin
  Result := TCategoryItem(inherited Add);
end;

function TCategoryList.Add(Caption: string; ID: integer): TCategoryItem;
begin
  Result := TCategoryItem(inherited Add);
  Result.Caption := Caption;
  Result.ID := ID;
end;

procedure TCategoryList.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TCategoryList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, GetItemClass);
end;

function TCategoryList.GetItemClass: TCollectionItemClass;
begin
  Result := TCategoryItem;
end;

function TCategoryList.GetItems(AIndex: Integer): TCategoryItem;
begin
  Result := TCategoryItem(inherited Items[AIndex]);
end;

function TCategoryList.Insert(AIndex: integer): TCategoryItem;
begin
  Result := TCategoryItem(inherited Insert(AIndex));
end;

procedure TCategoryList.SetItems(AIndex: Integer; const Value: TCategoryItem);
begin
  inherited Items[AIndex] := Value;
end;

procedure TCategoryList.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

{ TSearchColumnItem }

procedure TSearchColumnItem.SetDescription(const Value: string);
begin
  if (FDescription <> Value) then
  begin
    FDescription := Value;
    Changed(False);
  end;
end;

procedure TSearchColumnItem.SetImageIndex(const Value: TImageIndex);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TSearchColumnItem.SetPicture(const Value: TGDIPPicture);
begin
  if not Assigned(FPicture) then
    FPicture := TGDIPPicture.Create;

  FPicture.Assign(Value);
  Changed(False);
end;

procedure TSearchColumnItem.SetShape(const Value: TColumnItemShape);
begin
  if (FShape <> Value) then
  begin
    FShape := Value;
    Changed(False);
  end;
end;

procedure TSearchColumnItem.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed(False);
  end;
end;

procedure TSearchColumnItem.Assign(Source: TPersistent);
begin
  if (Source is TSearchColumnItem) then
  begin
    FCaption := (Source as TSearchColumnItem).Caption;
    FDescription := (Source as TSearchColumnItem).Description;
    FImageIndex := (Source as TSearchColumnItem).ImageIndex;
    FShape := (Source as TSearchColumnItem).Shape;
    FColor := (Source as TSearchColumnItem).Color;
    FTextColor := (Source as TSearchColumnItem).TextColor;

    if Assigned(Picture) then
    begin
      if not Assigned(FPicture) then
        FPicture := TGDIPPicture.Create;

      FPicture.Assign((Source as TSearchColumnItem).Picture);
    end;
  end;
end;

constructor TSearchColumnItem.Create(Collection: TCollection);
begin
  inherited;
  FImageIndex := -1;
  FPicture := nil;
  FColor := clNone;
  FTextColor := clNone;
  FShape := sNone;
end;

destructor TSearchColumnItem.Destroy;
begin
  if Assigned(FPicture) then
    FPicture.Free;
  inherited;
end;

function TSearchColumnItem.GetPicture: TGDIPPicture;
begin
  if not Assigned(FPicture) then
    FPicture := TGDIPPicture.Create;

  Result := FPicture;
end;

procedure TSearchColumnItem.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TSearchColumnItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

{ TSearchColumnItems }

function TSearchColumnItems.Add: TSearchColumnItem;
begin
  Result := TSearchColumnItem(inherited Add);
end;

constructor TSearchColumnItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetItemClass);
end;

function TSearchColumnItems.GetItemClass: TCollectionItemClass;
begin
  Result := TSearchColumnItem;
end;

function TSearchColumnItems.GetItems(AIndex: Integer): TSearchColumnItem;
begin
  Result := nil;
  if AIndex < Count then
    Result := TSearchColumnItem(inherited Items[AIndex]);
end;

function TSearchColumnItems.Insert(AIndex: integer): TSearchColumnItem;
begin
  Result := TSearchColumnItem(inherited Insert(AIndex));
end;

procedure TSearchColumnItems.SetItems(AIndex: Integer;
  const Value: TSearchColumnItem);
begin
  inherited Items[AIndex] := Value;
end;

procedure TSearchColumnItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ TColumnItem }

procedure TColumnItem.Assign(Source: TPersistent);
begin
  if (Source is TColumnItem) then
  begin
    FWidth := (Source as TColumnItem).Width;
    FColor := (Source as TColumnItem).Color;
    FVisible := (Source as TColumnItem).Visible;
    FFont.Assign((Source as TColumnItem).Font);
    FControlFont := (Source as TColumnItem).ControlFont;
    FTag := (Source as TColumnItem).Tag;
    FWordWrap := (Source as TColumnItem).WordWrap;
    FTrimming := (Source as TColumnItem).Trimming;
  end;
end;

constructor TColumnItem.Create(Collection: TCollection);
begin
  inherited;
  FFont := TFont.Create;
  {$IFNDEF FMXLIB}
  FFont.OnChange := FontChanged;
  {$ENDIF}
  {$IFDEF FMXLIB}
  FFont.OnChanged := FontChanged;
  {$ENDIF}
  FVisible := true;
  FWidth := 128;
  FColor := clNone;
  FControlFont := True;
  FTag := 0;
  FAlignment := gtaLeading;
  FVertAlignment := gtaLeading;
  FWordWrap := true;
end;

destructor TColumnItem.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TColumnItem.FontChanged(Sender: TObject);
begin
  Changed(False);
  FControlFont := false;
end;

procedure TColumnItem.SetAlignment(const Value: TAdvGraphicsTextAlign);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TColumnItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

procedure TColumnItem.SetControlFont(const Value: boolean);
begin
  FControlFont := Value;
end;

procedure TColumnItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TColumnItem.SetTrimming(const Value: boolean);
begin
  if (FTrimming <> Value) then
  begin
    FTrimming := Value;
    Changed(False);
  end;
end;

procedure TColumnItem.SetVertAlignment(const Value: TAdvGraphicsTextAlign);
begin
  if (FVertAlignment <> Value) then
  begin
    FVertAlignment := Value;
    Changed(False);
  end;
end;

procedure TColumnItem.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

procedure TColumnItem.SetWidth(const Value: integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

procedure TColumnItem.SetWordWrap(const Value: boolean);
begin
  if (FWordWrap <> Value) then
  begin
    FWordWrap := Value;
    Changed(False);
  end;
end;

{ TColumnItems }

function TColumnItems.Add: TColumnItem;
begin
  Result := TColumnItem(inherited Add);
end;

constructor TColumnItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, GetItemClass);
end;

function TColumnItems.GetItemClass: TCollectionItemClass;
begin
  Result := TColumnItem;
end;

function TColumnItems.GetItems(AIndex: Integer): TColumnItem;
begin
  Result := TColumnItem(inherited Items[AIndex]);
end;

function TColumnItems.Insert(AIndex: integer): TColumnItem;
begin
  Result := TColumnItem(inherited Insert(AIndex));
end;

procedure TColumnItems.SetItems(AIndex: Integer; const Value: TColumnItem);
begin
  inherited Items[AIndex] := Value;
end;

procedure TColumnItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ TAdvSearchList }

function TAdvSearchList.GetTabSop: boolean;
begin
  Result := inherited TabStop;
end;

procedure TAdvSearchList.SetTabStop(const Value: boolean);
begin
  inherited TabStop := Value;
end;

end.
