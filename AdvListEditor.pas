{***************************************************************************}
{ TAdvListEditor component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2019                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit AdvListEditor;

{$R AdvListEditor.res}

interface

uses
  Classes, Graphics, Controls, StdCtrls, Forms, AdvGDIP, Types, Windows,
  SysUtils, AdvEdit, AdvEddd, Messages, ImgList;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 7; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.0.0.0 : First release
  // 1.0.1.0 : New: Added property MaxItems
  // 1.0.1.1 : Fixed : Issue with readonly handling
  // 1.1.0.0 : New : Font in lookup dropdown customizable
  //         : New : Spacing added for items in lookup dropdown
  //         : New : Added capability to define multiple separator characters
  //         : New : LookupPopup.ValueSeparator property added
  // 1.1.0.1 : Fixed & improvements wrt item click & hint sensitivity
  // 1.1.0.2 : Fixed : Issue with OnKey* event handling
  // 1.1.0.3 : Fixed : Various small issues
  // 1.1.0.4 : Fixed : Issue with backspace under specific circumstances
  // 1.1.1.0 : New : Lookup methods lmFromStartDisplayAndValue and lmFullDisplayAndValue added
  //         : New : Event OnLookupNeedData added
  // 1.2.1.0 : New : Cancel adding an item via setting EditText = '' in the OnValueEditDone event.
  // 1.2.1.1 : Improved : Performance with adding Lookup items when using BeginUpdate/EndUpdate
  //         : Fixed : Issue with directly assigning TAdvListValues to AdvListEditor.Lookup
  // 1.2.2.0 : New : Property MinLines added
  // 1.2.2.1 : Improved : lookup editing inserts lookup value at edit position
  // 1.2.2.2 : Fixed : Issue with display when editor position = 0 and first item is selected
  // 1.2.2.3 : Fixed : Rare issue with inplace editor width update incorrect
  //         : Fixed : Rare issue with lookup incorrectly stopping
  //         : Fixed : Issue with AutoSize and control resize
  // 1.2.2.4 : Fixed : Issue with AutoSize and runtime imagelist assignment
  // 1.2.2.5 : Fixed : Issue with use of MaxItems
  // 1.2.2.6 : Fixed : Issue with setting ValueSeparator
  // 1.2.2.7 : Fixed : Issues with high DPI setting
  //         : Fixed : Issue with autosize & alignment
  // 1.2.2.8 : Fixed : Issue with persisting tag during editing
  // 1.2.2.9 : Fixed : Rare issue with using spaces in value entry
  // 1.2.2.10: Fixed : Issue with ampersand character in values
  // 1.2.2.11: Fixed : Issue with use of larger than standard font
  // 1.2.2.12: Improved : Small change with handling Tag during start of editing item
  // 1.2.2.13: Fixed : Issue with editor height when autosize = true
  // 1.2.2.14: Fixed : Rare issue with autosize & editing
  // 1.2.3.0 : New : OnValueAppearance event to customize colors for each item
  // 1.2.3.1 : Fixed : Issue with left key handling in list with one item
  // 1.3.0.0 : New : Support for VCL styles added
  // 1.3.0.1 : Fixed : Issue with full virtual lookup list
  // 1.3.0.2 : Fixed : Issue with custom popup menu handling
  // 1.4.0.0 : New : Moving of values with Ctrl-Left/Ctrl-Right/Ctrl-Home/Ctrl-End
  //         : New : OnValueMove / OnValueMoved event
  //         : New : Property AllowMoving added to control moving of items
  //         : New : OnDblClick event
  //         : New : Public function EditorRect: TRect to get rectangle of inplace editor
  // 1.4.0.1 : Improved : Behavior when ReadOnly = true
  // 1.5.0.0 : New : ShowDeleteButton property added to show delete button on items
  // 1.5.0.1 : Fixed : Issue with deleting item from the OnValueEditDone event
  // 1.5.0.2 : Fixed : Issue with events & OnExit handling in particular circumstances
  // 1.5.1.0 : New : Public property Edit: TAdvEdit added to access inplace editor
  // 1.5.2.0 : New : Public property TAdvListValue.Item: pointer added
  // 1.6.0.0 : New : Multiselect capability added
  //         : New : Focused and FocusedAndSelected appearance added
  //         : New : OnSelectionChanged event added
  //         : New : OnValueDeleted event added
  //         : New : StartEditOnDblClick property added
  // 1.6.0.1 : Fixed : Issue with Windows high-contrast themes
  // 1.6.1.0 : New : AdvListEditor.Lookup.HasValue() function added
  // 1.6.1.1 : Fixed : Issue with ShowDeleteButton on older Delphi versions
  // 1.7.0.0 : New : VCL Styles support added
  // 1.7.0.1 : Fixed : Issue with border when no VCL Style was used


type
  TAdvListValue = class(TCollectionItem)
  private
    FTag: integer;
    FValue: string;
    FDisplayText: string;
    FImageIndex: integer;
    FItem: pointer;
    FSelected: Boolean;
    procedure SetDisplayText(const AValue: string);
    procedure SetValue(const AValue: string);
    procedure SetImageIndex(const Value: integer);
  protected
     procedure Changed; virtual;
     function GetWidth(Canvas: TCanvas): integer;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Item: pointer read FItem write FItem;
  published
    property DisplayText: string read FDisplayText write SetDisplayText;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Value: string read FValue write SetValue;
    property Selected: Boolean read FSelected write FSelected;
    property Tag: integer read FTag write FTag default 0;
  end;

  TAdvListValues = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: integer): TAdvListValue;
    procedure SetItem(Index: integer; const Value: TAdvListValue);
  protected
    procedure Changed; virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property Items[Index: integer]: TAdvListValue read GetItem write SetItem; default;
    function Add: TAdvListValue;
    function Insert(Index: integer): TAdvListValue;
    function AddPair(DisplayText, Value: string): TAdvListValue; overload;
    function AddPair(DisplayText, Value: string; ImageIndex: integer): TAdvListValue; overload;
    function HasValue(DisplayText: string): boolean;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TValueClickEvent = procedure(Sender: TObject; ValueIndex: integer) of object;

  TItemAppearance = class(TPersistent)
  private
    FBorderColor: TColor;
    FTextColor: TColor;
    FColorTo: TColor;
    FColorFrom: TColor;
    FRounding: integer;
    FOnChange: TNotifyEvent;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetRounding(const Value: integer);
    procedure SetTextColor(const Value: TColor);
  protected
    procedure DoChange; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property ColorFrom: TColor read FColorFrom write SetColorFrom;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Rounding: integer read FRounding write SetRounding default 6;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAppearance = class(TPersistent)
  private
    FNormal: TItemAppearance;
    FSelected: TItemAppearance;
    FFocused: TItemAppearance;
    FFocusedAndSelected: TItemAppearance;
    FOnChange: TNotifyEvent;
    procedure SetNormal(const Value: TItemAppearance);
    procedure SetSelected(const Value: TItemAppearance);
    procedure SetFocused(const Value: TItemAppearance);
    procedure SetFocusedAndSelected(const Value: TItemAppearance);
    procedure ItemAppearanceChange(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Normal: TItemAppearance read FNormal write SetNormal;
    property Focused: TItemAppearance read FFocused write SetFocused;
    property Selected: TItemAppearance read FSelected write SetSelected;
    property FocusedAndSelected: TItemAppearance read FFocusedAndSelected write SetFocusedAndSelected;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TValueEditEvent = procedure(Sender: TObject; Value: TAdvListValue; var EditText: string) of object;
  TValueHintEvent = procedure(Sender: TObject; Value: TAdvListValue; var HintStr: string) of object;
  TValueMoveEvent = procedure(Sender: TObject; Value: TAdvListValue; ToIndex: Integer; var Allow: Boolean) of object;
  TValueMovedEvent = procedure(Sender: TObject; Value: TAdvListValue; FromIndex: Integer) of object;
  TValueDeleteEvent = procedure(Sender: TObject; Value: TAdvListValue; var Allow: boolean) of object;
  TValueAppearanceEvent = procedure(Sender: TObject; Value: TAdvListValue; AAppearance: TItemAppearance; ASelected: Boolean) of object;

  TLookupMethod = (lmFromStart, lmFull, lmFromStartDisplayAndValue, lmFullDisplayAndValue);

  TLookupPopup = class(TPersistent)
  private
    FCount: integer;
    FColor: TColor;
    FCaseSensitive: boolean;
    FEnabled: boolean;
    FOnChange: TNotifyEvent;
    FFromChar: integer;
    FFont: TFont;
    FSpacing: integer;
    FValueSeparator: TValueSeparator;
    procedure SetColor(const Value: TColor);
    procedure SetCount(const Value: integer);
    procedure SetCaseSensitive(const Value: boolean);
    procedure SetEnabled(const Value: boolean);
    procedure SetFromChar(const Value: integer);
    procedure SetFont(const Value: TFont);
    procedure FontChanged(Sender: TObject);
    procedure SetSpacing(const Value: integer);
    procedure SetValueSeparator(const Value: TValueSeparator);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive default false;
    property Color: TColor read FColor write SetColor default clWindow;
    property Count: integer read FCount write SetCount default 6;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Font: TFont read FFont write SetFont;
    property FromChar: integer read FFromChar write SetFromChar default 1;
    property Spacing: integer read FSpacing write SetSpacing default 0;
    property ValueSeparator: TValueSeparator read FValueSeparator write SetValueSeparator default vsBracket;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvListEditor = class(TCustomControl)
  private
    {$IFDEF DELPHIXE2_LVL}
    FStyledBorderColor: TColor;
    FStyledClientColor: TColor;
    FStyledFontColor: TColor;
    {$ENDIF}
    FAppearance: TAppearance;
    FEdit: TAdvEdit;
    FList: TAdvListValues;
    FLookup: TAdvListValues;
    FSeparator: string;
    FCaption: string;
    FItemIndex: integer;
    FEditPos: integer;
    FDownIndex: integer;
    FImages: TCustomImageList;
    FOnValueClick: TValueClickEvent;
    FOnValueEditStart: TValueEditEvent;
    FOnValueEditDone: TValueEditEvent;
    FOnValueMove: TValueMoveEvent;
    FOnValueMoved: TValueMovedEvent;
    FOnValueHint: TValueHintEvent;
    FAutoSize: boolean;
    FHintItem: integer;
    FOnValueDelete: TValueDeleteEvent;
    FBorderStyle: TBorderStyle;
    FBorderColor: TColor;
    FLookupMethod: TLookupMethod;
    FDesignTime: boolean;
    FReadOnly: boolean;
    FLookupPopup: TLookupPopup;
    FMaxLines: integer;
    FMaxItems: integer;
    FOnLookupNeedData: TLookupNeedDataEvent;
    FEditOffset: integer;
    FMinLines: integer;
    FEditTag: integer;
    FEditImageIndex: integer;
    FOnValueAppearance: TValueAppearanceEvent;
    FUseVCLStyles: boolean;
    FAllowMoving: boolean;
    FShowDeleteButton: boolean;
    FDelBtnWidth: integer;
    FOnAutoResize: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnValueDeleted: TNotifyEvent;
    FMultiSelect: Boolean;
    FStartEditOnDblClick: Boolean;
    FJustHandledDDblClick: Boolean;
    FLastFiredSelection: TIntegerDynArray;
    FSelectionRangeStartIdx: Integer;
    {$IFDEF DELPHIXE2_LVL}
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$ENDIF}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure InitVCLStyle(Init: Boolean = False);
    procedure SetList(const Value: TAdvListValues);
    procedure SetCaption(const Value: string);
    procedure SetItemIndexEx(const Value: integer);
    procedure SetItemIndex(const Value: integer; Select, StartSelectionRange, CallSelectionChangeEvent: Boolean);
    procedure SetLookup(const Value: TAdvListValues);
    procedure SetAppearance(const Value: TAppearance);
    procedure EditLookupIndexSelect(Sender: TObject; Index: integer; var AValue: string);
    procedure EditLookupNeedData(Sender: TObject; Value: string; List: TStrings; var ItemIndex: integer);
    procedure SetImages(const Value: TCustomImageList);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetAutoSizeEx(const Value: boolean);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetBorderColor(const Value: TColor);
    procedure SetLookupMethod(const Value: TLookupMethod);
    procedure SetReadOnly(const Value: boolean);
    procedure AppearanceChange(Sender: TObject);
    procedure LookupPopupChange(Sender: TObject);
    procedure SetLookupPopup(const Value: TLookupPopup);
    procedure SetMaxLines(const Value: integer);
    procedure SetMaxItems(const Value: integer);
    procedure SetMinLines(const Value: integer);
    procedure SetShowDeleteButton(const Value: boolean);
    procedure SetMultiSelect(const Value: Boolean);
    function GetSelection: TIntegerDynArray;
    procedure SetSelection(const Value: TIntegerDynArray);
    procedure SelectRange(NewItemIndex: Integer);
    function GetItemIndexAboveOrBelow(direction: Integer): Integer;
    function GetSelectedCount: Integer;
  protected
    procedure EditKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState); virtual;
    procedure EditDblClick(Sender: TObject); virtual;
    procedure EditClipboardPaste(Sender: TObject; value: string; var allow: Boolean);
    procedure DrawItem(ACanvas: TCanvas; R: TRect; AItem: TAdvListValue; Focused: boolean); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    function GetEditPos(Index: integer; AWidth: integer): TPoint;
    procedure UpdateEditPos(Index: integer);
    function ItemHeight: integer;
    function EditWidth: integer;
    procedure HideEdit;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure Loaded; override;
    procedure ListChanged(Sender: TObject); virtual;
    procedure LookupChanged(Sender: TObject); virtual;
    function MoveValue(AValue: TAdvListValue; ToIndex: Integer): Boolean;
    procedure DoAutoSize; virtual;
    function GetAutoHeight(AWidth: integer): integer;
    procedure DoValueAppearance(AValue: TAdvListValue; AAppearance: TItemAppearance; ASelected: Boolean); virtual;
    procedure DoValueClick(Index: integer); virtual;
    procedure DoValueHint(Index: integer; var HintStr: string); virtual;
    procedure DoValueEditStart(AValue: TAdvListValue; var EditText: string); virtual;
    procedure DoValueEditDone(AValue: TAdvListValue; var EditText: string); virtual;
    procedure DoValueDelete(AValue: TAdvListValue; var Allow: boolean); virtual;
    procedure DoValueDeleted; virtual;
    procedure DoValueMove(AValue: TAdvListValue; ToIndex: integer; var Allow: boolean); virtual;
    procedure DoValueMoved(AValue: TAdvListValue; FromIndex: integer); virtual;
    procedure DoLookupNeedData(Value: string; List: TStrings; var ItemIndex: integer); virtual;
    procedure DoSelectionChanged; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function HasMaxItems: boolean;
    function GetVersionNr: Integer; virtual;
    procedure UpdateDelBtn;
    property UseVCLStyles: boolean read FUseVCLStyles write FUseVCLStyles;
    procedure ClearSelection(NotifyForSelectionChange: Boolean=True);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function ValueAtXY(X,Y: integer): integer;
    function ValueRect(Index: integer): TRect;
    function EditorRect(out ARect: TRect): Boolean;
    procedure StartEdit(Index: integer);
    procedure StopEdit;
    procedure SelectAll;
    procedure DeleteSelection;
    property ItemIndex: integer read FItemIndex write SetItemIndexEx;
    property Edit: TAdvEdit read FEdit;
    property SelectedCount: Integer read GetSelectedCount;
    property Selection: TIntegerDynArray read GetSelection write SetSelection;
    property OnAutoResize: TNotifyEvent read FOnAutoResize write FOnAutoResize;
  published
    property Align;
    property AllowMoving: boolean read FAllowMoving write FAllowMoving default false;
    property Anchors;
    property Appearance: TAppearance read FAppearance write SetAppearance;
    property AutoSize: boolean read FAutoSize write SetAutoSizeEx default true;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $00B99D7F;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Caption: string read FCaption write SetCaption;
    property Color;
    property EditOffset: integer read FEditOffset write FEditOffset;
    property Font;
    property HelpContext;
    property HelpKeyword;
    property Images: TCustomImageList read FImages write SetImages;
    property Lookup: TAdvListValues read FLookup write SetLookup;
    property LookupMethod: TLookupMethod read FLookupMethod write SetLookupMethod default lmFromStart;
    property LookupPopup: TLookupPopup read FLookupPopup write SetLookupPopup;
    property MaxItems: integer read FMaxItems write SetMaxItems default 0;
    property MaxLines: integer read FMaxLines write SetMaxLines default 0;
    property MinLines: integer read FMinLines write SetMinLines default 0;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default false;
    property ParentColor default false;
    property ParentFont;
    property PopupMenu;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default false;
    property ShowDeleteButton: boolean read FShowDeleteButton write SetShowDeleteButton default false;
    property Separator: string read FSeparator write FSeparator;
    property ShowHint;
    property StartEditOnDblClick: Boolean read FStartEditOnDblClick write FStartEditOnDblClick default false;
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    {$ENDIF}
    property Values: TAdvListValues read FList write SetList;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHIXE_LVL}
    property OnGesture;
    {$ENDIF}
    {$IFDEF DELPHI2007_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnValueDelete: TValueDeleteEvent read FOnValueDelete write FOnValueDelete;
    property OnValueDeleted: TNotifyEvent read FOnValueDeleted write FOnValueDeleted;
    property OnValueEditStart: TValueEditEvent read FOnValueEditStart write FOnValueEditStart;
    property OnValueEditDone: TValueEditEvent read FOnValueEditDone write FOnValueEditDone;
    property OnValueMove: TValueMoveEvent read FOnValueMove write FOnValueMove;
    property OnValueMoved: TValueMovedEvent read FOnValueMoved write FOnValueMoved;
    property OnValueClick: TValueClickEvent read FOnValueClick write FOnValueClick;
    property OnValueHint: TValueHintEvent read FOnValueHint write FOnValueHint;
    property OnValueAppearance: TValueAppearanceEvent read FOnValueAppearance write FOnValueAppearance;
    property OnLookupNeedData: TLookupNeedDataEvent read FOnLookupNeedData write FOnLookupNeedData;

  end;


implementation

uses
  Math
{$IFDEF DELPHIXE2_LVL}
  , VCL.Themes
{$ENDIF}
  ;

const
  ITEMMARGIN = 10;
  EDITMARGIN = 7;
  LEFTOFFSET = 4;
  TOPOFFSET = 2;
  ITEMHEIGHT = 18;
  LINEMARGIN = 6;
  IMAGEMARGIN = 4;

  {$IFDEF DELPHI_UNICODE}
type
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}



{ TAdvListEditor }

procedure TAdvListEditor.AppearanceChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvListEditor.ClearSelection(NotifyForSelectionChange: Boolean=True);
var
  i: Integer;
begin
  for i := 0 to Values.Count-1 do
    Values[i].Selected := False;
  if NotifyForSelectionChange then
    DoSelectionChanged;
end;

{$IFDEF DELPHIXE2_LVL}
procedure TAdvListEditor.CMStyleChanged(var Message: TMessage);
begin
  InitVCLStyle(True);
end;
{$ENDIF}

procedure TAdvListEditor.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FEdit) then
    FEdit.Color := Color;
end;

procedure TAdvListEditor.CMFontChanged(var Message: TMessage);
begin
  inherited;

  UpdateDelBtn;

  if FAutoSize then
    DoAutoSize;

  UpdateEditPos(Values.Count);
  Repaint;
end;

procedure TAdvListEditor.CMHintShow(var Msg: TMessage);
var
  hi: PHintInfo;
  i: integer;
begin
  inherited;

  hi := PHintInfo(Msg.LParam);

  i := ValueAtXY(hi.CursorPos.X, hi.CursorPos.Y);

  if (i <> -1) then
  begin
    hi.HintStr := Values[i].Value;
    DoValueHint(i, hi.HintStr);
  end;
end;

constructor TAdvListEditor.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF DELPHIXE6_LVL}
  StyleElements := [seFont, seClient, seBorder];
  {$ENDIF}

  DoubleBuffered := true;
  FEdit := TAdvEdit.Create(Self);
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyPress := EditKeyPress;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnKeyUp := EditKeyUp;
  FEdit.OnDblClick := EditDblClick;
  FEdit.OnClipboardPaste := EditClipboardPaste;
  FEdit.OnLookupIndexSelect := EditLookupIndexSelect;
  FEdit.OnLookupNeedData := EditLookupNeedData;
  FEdit.Width := 2;
  FEdit.Height := 14;
  FEdit.Left := 1;
  FEdit.FullTextSearch := false;

  FEditOffset := -2;

  FList := TAdvListValues.Create(Self);
  FList.OnChange := ListChanged;

  FLookup := TAdvListValues.Create(Self);
  FLookup.OnChange := LookupChanged;
  FLookupMethod := lmFromStart;

  FLookupPopup := TLookupPopup.Create;
  FLookupPopup.OnChange := LookupPopupChange;

  Width := 250;
  Height := 24;
  ParentColor := false;
  Color := clWindow;
  FItemIndex := -1;
  FMaxLines := 0;
  Separator := ';';
  FBorderColor := $00B99D7F;
  FBorderStyle := bsSingle;
  FAutoSize := True;
  FHintItem := -1;
  FDelBtnWidth := 12;

  FUseVCLStyles := False;

  FAppearance := TAppearance.Create;
  FAppearance.OnChange := AppearanceChange;

  FAppearance.Normal.ColorFrom := RGB(220,230,248);
  FAppearance.Normal.ColorTo := RGB(189,207,241);
  FAppearance.Normal.BorderColor := RGB(120,133,215);
  FAppearance.Normal.TextColor := clBlack;

  FAppearance.Focused.ColorFrom := RGB(220,230,248);
  FAppearance.Focused.ColorTo := RGB(189,207,241);
  FAppearance.Focused.BorderColor := clWindow;
  FAppearance.Focused.TextColor := clBlack;

  FAppearance.Selected.ColorFrom := RGB(103, 155, 228);
  FAppearance.Selected.ColorTo := RGB(35,110,216);
  FAppearance.Selected.BorderColor := RGB(35,110,216);
  FAppearance.Selected.TextColor := clBlack;

  FAppearance.FocusedAndSelected.ColorFrom := RGB(161, 193, 238);
  FAppearance.FocusedAndSelected.ColorTo := RGB(108, 158, 228);
  FAppearance.FocusedAndSelected.BorderColor := RGB(35,110,216);
  FAppearance.FocusedAndSelected.TextColor := clWindow;
  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  InitVCLStyle;
end;

procedure TAdvListEditor.CreateWnd;
begin
  inherited;

  UpdateDelBtn;

  FEdit.Parent := Self;
  UpdateEditPos(Values.Count);

  FEdit.Lookup.Enabled := true;
  FEdit.Lookup.CaseSensitive := false;
  FEdit.Lookup.NumChars := 1;
  FEdit.Lookup.ShowValue := true;
  FEdit.Font.Assign(Font);
  FEdit.FullTextSearch := (LookupMethod = lmFull) or (LookupMethod = lmFullDisplayAndValue);
  FEdit.Lookup.SearchValue := (LookupMethod = lmFromStartDisplayAndValue) or (LookupMethod = lmFullDisplayAndValue);

  LookupPopupChange(Self);

  if FDesignTime then
    Values.AddPair('Value 1','');

  if AutoSize then
    DoAutoSize;
end;

destructor TAdvListEditor.Destroy;
begin
  FEdit.Free;
  FList.Free;
  FLookup.Free;
  FLookupPopup.Free;
  FAppearance.Free;
  inherited;
end;

function TAdvListEditor.GetAutoHeight(AWidth: integer): integer;
var
  pt: TPoint;
  NewHeight: integer;
  MinHeight: integer;
begin
  Result := Height;

  pt := GetEditPos(Values.Count, AWidth);

  NewHeight := pt.Y + ItemHeight + LINEMARGIN;

  if MinLines > 0 then
  begin
    MinHeight :=  TOPOFFSET + (ItemHeight - Canvas.TextHeight('gh')) div 2
      + MinLines * (ItemHeight + LINEMARGIN);
    if BorderStyle = bsSingle then
      inc(MinHeight);
    if NewHeight < MinHeight then
      NewHeight := MinHeight;
  end;

  if (MaxLines > 0) and (NewHeight > (MaxLines + 1) * (ItemHeight + LINEMARGIN)) then
    Exit;

  Result := NewHeight;
end;

procedure TAdvListEditor.DoAutoSize;
var
  oldHeight: Integer;
begin
  oldHeight := Height;
  Height := GetAutoHeight(Width);
  FEdit.Height := Canvas.TextHeight('gh')+ 3;
  if (Height <> oldHeight) and Assigned(FOnAutoResize) then
    FOnAutoResize(Self);
end;

procedure TAdvListEditor.DoEnter;
begin
  inherited;

  if ReadOnly then
  begin
    FEdit.Width := 2;
    FEdit.ReadOnly := true;
    Exit;
  end
  else
   FEdit.ReadOnly := false;

  if not FMultiSelect then
    UpdateEditPos(Values.Count);


  FEdit.PopupMenu := PopupMenu;
  FEdit.SetFocus;
end;

procedure TAdvListEditor.DoExit;
begin
  inherited;
  if not ReadOnly then
  begin
    StopEdit;    
    if not FMultiSelect then
      UpdateEditPos(Values.Count);
  end;
end;

procedure TAdvListEditor.DoLookupNeedData(Value: string; List: TStrings; var ItemIndex: integer);
begin
  if Assigned(OnLookupNeedData) then
    OnLookupNeedData(Self, Value, List, ItemIndex);
end;

procedure TAdvListEditor.DoValueAppearance(AValue: TAdvListValue; AAppearance: TItemAppearance; ASelected: Boolean);
begin
  if Assigned(OnValueAppearance) then
    OnValueAppearance(Self, AValue, AAppearance, ASelected);
end;

procedure TAdvListEditor.DoValueClick(Index: integer);
begin
  if Assigned(OnValueClick) then
    OnValueClick(Self, Index);
end;

procedure TAdvListEditor.DoValueDelete(AValue: TAdvListValue;
  var Allow: boolean);
begin
  if Assigned(OnValueDelete) then
    OnValueDelete(Self, AValue, Allow);
end;

procedure TAdvListEditor.DoValueDeleted;
begin
  if Assigned(OnValueDeleted) then
    OnValueDeleted(Self);
end;

procedure TAdvListEditor.DoValueEditDone(AValue: TAdvListValue; var EditText: string);
begin
  if Assigned(OnValueEditDone) then
    OnValueEditDone(Self, AValue, EditText);
end;

procedure TAdvListEditor.DoValueEditStart(AValue: TAdvListValue; var EditText: string);
begin
  if Assigned(OnValueEditStart) then
    OnValueEditStart(Self, AValue, EditText);
end;

procedure TAdvListEditor.DoValueHint(Index: integer; var HintStr: string);
begin
  if Assigned(OnValueHint) then
    OnValueHint(Self, Values[Index], HintStr);
end;

procedure TAdvListEditor.DoValueMove(AValue: TAdvListValue; ToIndex: integer; var Allow: boolean);
begin
  if Assigned(OnValueMove) then
    OnValueMove(Self, AValue, ToIndex, Allow);
end;

procedure TAdvListEditor.DoValueMoved(AValue: TAdvListValue; FromIndex: integer);
begin
  if Assigned(OnValueMoved) then
    OnValueMoved(Self, AValue, FromIndex);
end;

procedure TAdvListEditor.DoSelectionChanged();
var
  currentSelection: TIntegerDynArray;
  function SelectionsEqual: Boolean;
  var
    i: Integer;
  begin
    Result := Length(currentSelection) = Length(FLastFiredSelection);
    if Result then
      for i := 0 to Length(currentSelection)-1 do
        if currentSelection[i] <> FLastFiredSelection[i] then
        begin
          Result := False;
          Break;
        end;
  end;
begin
  if Assigned(OnSelectionChanged) then
  begin
    currentSelection := GetSelection;
    if not SelectionsEqual then
    begin
      OnSelectionChanged(Self);
      FLastFiredSelection := currentSelection;
    end;
  end;
end;

procedure TAdvListEditor.DrawItem(ACanvas: TCanvas; R: TRect;
  AItem: TAdvListValue; Focused: boolean);
var
  gp: TGPGraphics;
  gpBrush: TGPLinearGradientBrush;
  gpSBrush: TGPSolidBrush;
  gpPen: TGPPen;
  gprect: TGPRectF;
  gpPath: TGPGraphicsPath;
  clrfrom,clrto,clrbrdr: TColor;
  d,w: integer;
  ap: TItemAppearance;
  {$IFDEF DELPHIXE2_LVL}
  LStyle: TCustomStyleServices;
  {$ENDIF}
begin
  ap := TItemAppearance.Create;
  try
    gp := TGPGraphics.Create(ACanvas.Handle);
    try
      gpRect := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);

      gp.SetSmoothingMode(SmoothingModeAntiAlias);

      if FMultiSelect and Focused and AItem.Selected then
        ap.Assign(Appearance.FocusedAndSelected)
      else if Focused then
      begin
        if FMultiSelect then
          ap.Assign(Appearance.Focused)
        else
          ap.Assign(Appearance.Selected);
      end
      else if FMultiSelect and AItem.Selected then
        ap.Assign(Appearance.Selected)
      else
        ap.Assign(Appearance.Normal);

      {$IFDEF DELPHIXE2_LVL}
      if UseVCLStyles then
      begin
        LStyle := StyleServices;
        {$IFDEF DELPHIXE6_LVL}
        if (seBorder in StyleElements) then
        {$ENDIF}
          ap.FBorderColor := LStyle.GetStyleColor(scBorder);
        if FMultiSelect and Focused and AItem.Selected then
        begin
          {$IFDEF DELPHIXE6_LVL}
          if (seFont in StyleElements) then
          {$ENDIF}
            ap.FTextColor := LStyle.GetStyleFontColor(sfButtonTextHot);
          {$IFDEF DELPHIXE6_LVL}
          if (seClient in StyleElements) then
          {$ENDIF}
            ap.FColorFrom := LStyle.GetStyleColor(scButtonHot);
        end
        else
        if Focused then
        begin
          if FMultiSelect then
          begin
            {$IFDEF DELPHIXE6_LVL}
            if (seFont in StyleElements) then
            {$ENDIF}
              ap.FTextColor := LStyle.GetStyleFontColor(sfButtonTextFocused);
            {$IFDEF DELPHIXE6_LVL}
            if (seClient in StyleElements) then
            {$ENDIF}
              ap.FColorFrom := LStyle.GetStyleColor(scButtonFocused);
          end
          else
          begin
            {$IFDEF DELPHIXE6_LVL}
            if (seFont in StyleElements) then
            {$ENDIF}
              ap.FTextColor := LStyle.GetStyleFontColor(sfButtonTextPressed);
            {$IFDEF DELPHIXE6_LVL}
            if (seClient in StyleElements) then
            {$ENDIF}
              ap.FColorFrom := LStyle.GetStyleColor(scButtonPressed);
          end;
        end
        else
        if FMultiSelect and AItem.Selected then
        begin
          {$IFDEF DELPHIXE6_LVL}
          if (seFont in StyleElements) then
          {$ENDIF}
            ap.FTextColor := LStyle.GetStyleFontColor(sfButtonTextDisabled);
          {$IFDEF DELPHIXE6_LVL}
          if (seClient in StyleElements) then
          {$ENDIF}
            ap.FColorFrom := LStyle.GetStyleColor(scButtonDisabled);
        end
        else
        begin
          {$IFDEF DELPHIXE6_LVL}
          if (seFont in StyleElements) then
          {$ENDIF}
            ap.FTextColor := LStyle.GetStyleFontColor(sfButtonTextNormal);
          {$IFDEF DELPHIXE6_LVL}
          if (seClient in StyleElements) then
          {$ENDIF}
            ap.FColorFrom := LStyle.GetStyleColor(scButtonNormal);
        end;
        {$IFDEF DELPHIXE6_LVL}
        if (seClient in StyleElements) then
        {$ENDIF}
          ap.FColorTo := ap.FColorFrom;
      end;
      {$ENDIF}

      DoValueAppearance(AItem, ap, Focused);

      gpPath := CreateRoundRectangle(R, ap.Rounding);

      clrfrom := ap.ColorFrom;
      clrto := ap.ColorTo;
      clrbrdr := ap.BorderColor;

      if clrto = clNone then
        clrto := clrfrom;

      gpBrush := TGPLinearGradientBrush.Create(gpRect,ColorToARGB(clrfrom),
        ColorToARGB(clrto),LinearGradientModeVertical);

      gpPen := TGPPen.Create(ColorToARGB(clrbrdr),1);

      gp.FillPath(gpBrush, gpPath);

      gp.DrawPath(gpPen, gpPath);


      gpPen.Free;
      gpBrush.Free;
      gpPath.Free;

      if ShowDeleteButton then
      begin
        gpsBrush := TGPSolidBrush.Create(ColorToARGB(clRed));
        w := r.Bottom - r.Top;
        gp.FillEllipse(gpsbrush,  r.Right - (w - 2), r.Top + 2, w - 4, w - 4);
        gpsBrush.Free;

        gpPen := TGPPen.Create(ColorToARGB(clWindow),2);

        d := Round(w / 2) + 1;

        gp.DrawLine(gpPen, r.Right - (w - 2) + d, r.Top + 2 + d, r.Right - 2 -d, r.Bottom - 2 - d);

        gp.DrawLine(gpPen, r.Right - 2 - d, r.Top + 2 + d, r.Right - (w - 2) + d, r.Bottom - 2 - d);

        gpPen.Free;

      end;

    finally
      gp.Free;
    end;

    Canvas.Font.Color := ap.TextColor;

    if (AItem.ImageIndex >= 0) and Assigned(Images) then
    begin
      d := R.Bottom - R.Top - Images.Height;
      if d > 0 then
        d := d div 2
      else
        d := 0;

      Images.Draw(Canvas, R.Left + 2 + (ap.Rounding div 2), R.Top + d, AItem.ImageIndex);
      R.Left := R.Left + Images.Width + IMAGEMARGIN;
    end;

    Canvas.Brush.Style := bsClear;

    R.Left := R.Left + 2 + (ap.Rounding div 2);

    DrawText(Canvas.Handle, PChar(AItem.DisplayText), Length(AItem.DisplayText), R, DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
  finally
    ap.free;
  end;
end;

procedure TAdvListEditor.EditClipboardPaste(Sender: TObject; value: string;
  var allow: Boolean);
begin
  FEdit.Width := Canvas.TextWidth(Value) + EDITMARGIN;
end;

procedure TAdvListEditor.EditDblClick(Sender: TObject);
begin
  DblClick;
end;

// This function is used when UP or DOWN keys are pressed to go one line up or down
function TAdvListEditor.GetItemIndexAboveOrBelow(direction: Integer): Integer;
var
  i,j,dx,dy,dw,th, currentPos: integer;
  line, currentItemLine, currentItemLeft, currentItemRight, overlap, maxOverlap: Integer;
  matchedIndexes, matchedOverlappedWidths: TIntegerDynArray;
begin
  Result := -1;

  matchedIndexes := nil;
  matchedOverlappedWidths := nil;

  Canvas.Font.Assign(Font);
  th := ItemHeight;

  currentItemLeft := -1;
  currentItemRight := -1;
  currentItemLine := -1;
  if ItemIndex <> -1 then
    currentPos := ItemIndex
  else
    currentPos := FEditPos;
  // Do 2 loops. In first (j = 0) the dimensions of current item (ItemIndex)
  // will be calculated. In the second loop (j = 1) the possible items in
  // previous or next line will be detected.
  for j := 0 to 1 do
  begin
    dx := LEFTOFFSET;
    dy := TOPOFFSET;
    line := 0;
    for i := 0 to Values.Count - 1 do
    begin
      dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

      if (i = FEditPos) then
      begin
        dx := dx + EditWidth;
      end;

      if dx + dw - ITEMMARGIN + 4 >= Width then
      begin
        dx := LEFTOFFSET;
        dy := dy + th + LINEMARGIN;
        Inc(line);
        if (j = 1) then
        begin
          if (direction > 0) and (line > currentItemLine + 1) then
            Break
          else if (direction < 0) and (line = currentItemLine) then
            Break
        end;
      end;

      if (j = 0) and (i = Min(currentPos, Values.Count - 1)) then
      begin
        currentItemLine := line;
        currentItemLeft := dx;
        currentItemRight := dx + dw - 4;
        Break;
      end
      else if (j = 1) then
      begin
        if (line = currentItemLine + Sign(direction)) then
        begin
          if (currentItemLeft <= dx) and (currentItemRight > dx) then
            overlap := currentItemRight - dx
          else if (currentItemLeft > dx) and (currentItemLeft < dx + dw - 4) then
            overlap := dx + dw - 4 - currentItemLeft
          else
            overlap := 0;
          if (overlap <> 0) or (i = Values.Count-1) then
          begin
            SetLength(matchedIndexes, Length(matchedIndexes) + 1);
            matchedIndexes[Length(matchedIndexes)-1] := i;
            SetLength(matchedOverlappedWidths, Length(matchedOverlappedWidths) + 1);
            matchedOverlappedWidths[Length(matchedOverlappedWidths)-1] := overlap;
          end;
        end;
      end;
      dx := dx + dw;
    end;
  end;

  // Now find out the most overlapping item index of above or below line
  if Length(matchedIndexes) <> 0 then
  begin
    maxOverlap := -1;
    for i := 0 to Length(matchedIndexes)-1 do
      if matchedOverlappedWidths[i] > maxOverlap then
      begin
        Result := matchedIndexes[i];
        maxOverlap := matchedOverlappedWidths[i];
      end;
  end;
end;

procedure TAdvListEditor.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  EditText: string;
  lv: TAdvListValue;
  Allow, Lst, isHidden: boolean;
  edPos, lvIdx, nextLineItemIdx: integer;


  procedure ProcessLeftOrRight(isLeft: Boolean);
  begin
  if FEdit.Text <> '' then
  begin
    EditText := FEdit.Text;
    lv := Values.Insert(FEditPos);
    DoValueEditDone(lv, EditText);
    if EditText <> '' then
    begin
      lv.DisplayText := EditText;
      FEdit.SetEditText('');
      if isLeft then
        UpdateEditPos(lv.Index)
      else
        UpdateEditPos(lv.Index + 1);
    end
    else
    begin
      Allow := true;
      DoValueDelete(lv, Allow);
      if Allow then
      begin
        if (lv.Index + 1 < Values.Count) then ItemIndex := lv.Index + 1
        else ItemIndex := lv.Index - 1;
        Values.Delete(lv.Index);
        DoValueDeleted;
      end
      else
        ItemIndex := lv.Index;
      FEdit.SetEditText('');
      FEdit.Width := 2;
      if isLeft then
        FEdit.Left := FEdit.Left - 2
      else
        FEdit.Left := Max(1,FEdit.Left - 2);
    end;
  end
  else
  begin
    if isLeft then
      ItemIndex := FEditPos - 1
    else
      ItemIndex := FEditPos;
    FEdit.SetEditText('');
    FEdit.Width := 2;
    if isLeft then
      FEdit.Left := FEdit.Left - 2
    else
      FEdit.Left := Max(1,FEdit.Left - 2);
  end;
  end;

begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);

  if (Key in [VK_RETURN]) and (FEdit.Text <> '') and not FEdit.LookupVisible then
  begin
    StopEdit;
    UpdateEditPos(FEditPos + 1);
    ItemIndex := -1;
    Key := 0;
  end;

  inherited;

  isHidden := (FEdit.Width = 2) and (FEditPos = Values.Count);

  if (Key in [VK_ESCAPE]) and (FEdit.OrigText <> '') and (FEdit.Text <> '') then
  begin
    StopEdit;
    ItemIndex := FEditPos;
    FEdit.Width := 2;
    //UpdateEditPos(Values.Count);
    Key := 0;
  end;

  if (Key = VK_F2) and (ItemIndex >= 0) and not ReadOnly then
  begin
    StartEdit(ItemIndex);
  end;

  if (Key = VK_BACK) and (FEdit.SelStart = 0) and (FEditPos > 0) and not isHidden then
  begin
    if FEditPos > Values.Count then
      FEditPos := Values.Count;

    if (ItemIndex >=0) then
    begin
      if Values.Count > 0 then
      begin
        Allow := true;
        edPos := FEditPos;
        DoValueDelete(Values[FEditPos - 1], Allow);
        if Allow then
        begin
          Lst := FEditPos = Values.Count;
          Values.Delete(FEditPos - 1);
          Invalidate;

          if Lst and (FEditPos < Values.Count) then
            UpdateEditPos(edPos)
          else
            UpdateEditPos(edPos - 1);
          DoValueDeleted; 
        end;
      end;
    end
    else
    begin
      if FEditPos > 0 then
        ItemIndex := FEditPos - 1;
      Invalidate;
      Key := 0;
      Exit;
    end;
  end;

  if ((Key = VK_DELETE) and (ItemIndex >=0)) or ((Key = VK_BACK) and isHidden) then
  begin
    // Friedemann Schmidt
    if FMultiSelect then
      DeleteSelection
    else
    begin
      lvIdx := ItemIndex;
      Allow := true;
      DoValueDelete(Values[lvIdx],Allow);
      if Allow then
      begin
        Values.Delete(lvIdx);
        if (lvIdx < Values.Count) then ItemIndex := lvIdx
        else ItemIndex := Values.Count - 1;
        UpdateEditPos(lvIdx);
        DoValueDeleted;
      end;
    end;
    Key := 0;
  end;

  if (Key = VK_DELETE) and (FEdit.Text = '') then
  begin
    if (FEditPos < Values.Count) and (ItemIndex >= 0) then
    begin
      lvIdx := FEditPos;
      Allow := true;
      DoValueDelete(Values[lvIdx],Allow);
      if Allow then
      begin
        Values.Delete(lvIdx);
        if (lvIdx < Values.Count) then ItemIndex := lvIdx
        else ItemIndex := Values.Count - 1;
        UpdateEditPos(lvIdx);
        DoValueDeleted;
      end;
    end
    else if (FEditPos >= 0) and (FEditPos < Values.Count) then
    begin
      ItemIndex := FEditPos;
      HideEdit;
      FEdit.Width := 2;
      UpdateEditPos(Values.Count);
    end;
    Invalidate;
  end;

  if (Key = VK_HOME) and (FEdit.SelStart = 0) and (FEdit.SelLength = 0) then
  begin
    if FMultiSelect and (Shift = [ssShift]) then
      SelectRange(0)
    else if ReadOnly then
      ItemIndex := 0
    else if (Shift = [ssCtrl]) and AllowMoving then
    begin
      if (ItemIndex > 0) then
      begin
        lv := Values[ItemIndex];
        if MoveValue(lv, 0) then
        begin
          ItemIndex := lv.Index;
          FEdit.SetEditText('');
          FEdit.Width := 2;
          FEdit.Left := Max(1,FEdit.Left - 2);
        end;
      end;
    end
    else if (ItemIndex > 0) then
    begin
      FEditPos := 0;
      UpdateEditPos(0);
    end;
  end;

  if (Key = VK_END) and (FEdit.SelStart = 0) and (FEdit.SelLength = 0) then
  begin
    if FMultiSelect and (Shift = [ssShift]) then
      SelectRange(Values.Count-1)
    else if ReadOnly then
      ItemIndex := Values.Count - 1
    else if (Shift = [ssCtrl]) and AllowMoving then
    begin
      if (ItemIndex < Values.Count - 1) then
      begin
        lv := Values[ItemIndex];
        if MoveValue(lv, Values.Count - 1) then
        begin
          ItemIndex := lv.Index;
          FEdit.SetEditText('');
          FEdit.Width := 2;
          FEdit.Left := Max(1,FEdit.Left - 2);
        end;
      end;
    end
    else if (ItemIndex > 0) then
      UpdateEditPos(Values.Count);
  end;

  if (Key = VK_LEFT) and (FEdit.SelStart = 0) and (FEdit.SelLength = 0) then
  begin
    if (ItemIndex >= 0) then
    begin
      if ReadOnly or HasMaxItems then
      begin
        if ItemIndex > 0 then
          ItemIndex := ItemIndex - 1;
      end
      else if (Shift = [ssCtrl]) and AllowMoving then
      begin
        if (ItemIndex > 0) then
        begin
          lv := Values[ItemIndex];
          if MoveValue(lv, ItemIndex - 1) then
          begin
            ItemIndex := lv.Index;
            FEdit.SetEditText('');
            FEdit.Width := 2;
            FEdit.Left := Max(1,FEdit.Left - 2);
          end;
        end;
      end
      else if (Shift = [ssShift]) and FMultiSelect then
        SelectRange(ItemIndex-1)
      else
      begin
        FEditPos := ItemIndex;
        ItemIndex := -1;
        UpdateEditPos(FEditPos);
        FEdit.SetEditText('');
      end;
    end
    else
      if FEditPos > 0 then
        ProcessLeftOrRight(True);
  end;

  if (Key = VK_RIGHT) and (FEdit.SelStart = Length(FEdit.Text)) then
  begin
    if (ItemIndex < Values.Count) and (ItemIndex >= 0) then
    begin
      if ReadOnly or HasMaxItems then
      begin
        if ItemIndex < Values.Count - 1 then
          ItemIndex := ItemIndex + 1;
      end
      else if (Shift = [ssCtrl]) and AllowMoving then
      begin
        if (ItemIndex < Values.Count - 1) then
        begin
          lv := Values[ItemIndex];
          if MoveValue(lv, ItemIndex + 1) then
          begin
            ItemIndex := lv.Index;
            FEdit.SetEditText('');
            FEdit.Width := 2;
            FEdit.Left := Max(1,FEdit.Left - 2);
          end;
        end;
      end
      else if (Shift = [ssShift]) and FMultiSelect then
        SelectRange(ItemIndex+1)
      else
      begin
        FEditPos := ItemIndex + 1;
        ItemIndex := -1;
        UpdateEditPos(FEditPos);
        FEdit.SetEditText('');
      end;
    end
    else
      if FEditPos < Values.Count then
        ProcessLeftOrRight(False);
  end;
  if (Key = VK_DOWN) and (FEdit.SelStart = 0) and (FEdit.SelLength = 0) then
  begin
    nextLineItemIdx := GetItemIndexAboveOrBelow(1);
    if nextLineItemIdx <> -1 then
    begin
      if (ItemIndex = -1) and (FEditPos < Values.Count) then
        ProcessLeftOrRight(False);
      if FMultiSelect and (Shift = [ssShift]) then
        SelectRange(nextLineItemIdx)
      else
      begin
        ClearSelection(False);
        ItemIndex := nextLineItemIdx;
        Invalidate;
      end;
    end;
  end;
  if (Key = VK_UP) and (FEdit.SelStart = 0) and (FEdit.SelLength = 0) then
  begin
    nextLineItemIdx := GetItemIndexAboveOrBelow(-1);
    if nextLineItemIdx <> -1 then
    begin
      if (ItemIndex = -1) and (FEditPos > 0) then
        ProcessLeftOrRight(True);
      if FMultiSelect and (Shift = [ssShift]) then
        SelectRange(nextLineItemIdx)
      else
      begin
        ClearSelection(False);
        ItemIndex := nextLineItemIdx;
        Invalidate;
      end;
    end;
  end;
 
  if FMultiSelect and (Key = Word('A')) and (Shift = [ssCtrl]) then
  begin
    SelectAll;
    Key := 0;
  end;
end;

function IsSeparator(Key: char; Separator: string): boolean;
var
  i: integer;
begin
  Result := false;

  for i := 1 to Length(Separator) do
    if Key = Separator[i] then
    begin
      Result := true;
      break;
    end;
end;


procedure TAdvListEditor.EditKeyPress(Sender: TObject; var Key: Char);
var
  EditText: string;
  lv: TAdvListValue;
  Allow: boolean;
begin
  if Assigned(OnKeypress) then
    OnKeyPress(Self,Key);

  // invisible editor
  if (FEdit.Width <= 2) or ReadOnly then
  begin
    Key := #0;
    Exit;
  end;

  if (Key = #13) and not FEdit.LookupVisible then
  begin
    Key := #0;
  end;

  {
  if (Key = #27) and not FEdit.LookupVisible then
  begin
    Key := #0;
    Exit;
  end;
  }

  if (Key = #8) and (FEdit.Text = '') then
  begin
    Key := #0;
    Exit;
  end;

  if IsSeparator(Key, Separator) then
  begin
    EditText := FEdit.Text;
    FEdit.Width := 2;
    lv := Values.Add;
    DoValueEditDone(lv,EditText);
    if EditText <> '' then
      lv.DisplayText := EditText
    else
    begin
      Allow := true;
      DoValueDelete(lv,Allow);
      if Allow then
      begin 
        Values.Delete(lv.Index);
        DoValueDeleted;
      end;
    end;

    UpdateEditPos(Values.Count);
    inherited;
    Key := #0;
    FEdit.SetEditText('');
    Exit;
  end
  else
  begin
    inherited;
    EditText := FEdit.Text;
    if Key <> #8 then
      EditText := EditText + Key;

    FEdit.Width := Canvas.TextWidth(EditText) + EDITMARGIN;
    UpdateEditPos(FEditPos);
    Invalidate;
  end;
end;


procedure TAdvListEditor.EditKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;

procedure TAdvListEditor.EditLookupIndexSelect(Sender: TObject; Index: integer;
  var AValue: string);
var
  disp: string;
  lv: TAdvListValue;
  Allow: boolean;
  EP,cnt: integer;

begin
  if not (FEdit.Lookup.DisplayList.Count > Index) then
    Exit;


  disp := FEdit.Lookup.DisplayList.Strings[Index];

  if FEditPos >= Values.Count then
    lv := Values.Add
  else
    lv := Values.Insert(FEditPos);

  EP := lv.Index;

  lv.DisplayText := disp;
  lv.Value := avalue;
  lv.Tag := Lookup.Items[Index].Tag;
  lv.ImageIndex := integer(FEdit.Lookup.ValueList.Objects[Index]);

  cnt := Values.Count;

  DoValueEditDone(lv, disp);

  // item was removed during validation
  if cnt <> Values.Count then
    Exit;

  if disp <> '' then
    Values[EP].DisplayText := disp
  else
  begin
    Allow := true;
    DoValueDelete(Values[EP],Allow);
    if Allow then
    begin
      Values.Delete(EP);
      DoValueDeleted;
    end;
  end;

  FEdit.SetEditText('');
  AValue := '';

  UpdateEditPos(EP + 1);
end;

procedure TAdvListEditor.EditLookupNeedData(Sender: TObject; Value: string;
  List: TStrings; var ItemIndex: integer);
begin
  DoLookupNeedData(Value, List, ItemIndex);
end;

function TAdvListEditor.EditorRect(out ARect: TRect): Boolean;
begin
  Result := (FEdit.Width > 2);
  ARect := FEdit.BoundsRect;
end;

function TAdvListEditor.EditWidth: integer;
begin
  if FEdit.Width <= 2 then
    Result := 0
  else
    Result := FEdit.Width + EDITMARGIN;
end;

procedure TAdvListEditor.StopEdit;
var
  EditText: string;
  lv: TAdvListValue;
  Allow: boolean;
begin
  EditText := FEdit.Text;

  if (FEditPos >= Values.Count) and (EditText = '') then
    Exit;

  if FEditPos >= Values.Count then
    lv := Values.Add
  else
    lv := Values.Insert(FEditPos);

  lv.Tag := FEditTag;
  lv.ImageIndex := FEditImageIndex;

  DoValueEditDone(lv, EditText);

  if EditText <> '' then
  begin
    lv.DisplayText := EditText;
    ItemIndex := lv.Index;
    FEditPos := ItemIndex;
  end
  else
  begin
    Allow := true;
    DoValueDelete(lv, Allow);
    if Allow then
    begin
      if (lv.Index + 1 < Values.Count) then ItemIndex := lv.Index + 1
      else ItemIndex := lv.Index - 1;
      Values.Delete(lv.Index);
      DoValueDeleted;
    end
    else
      ItemIndex := lv.Index;
  end;

  FEdit.SetEditText('');
end;

procedure TAdvListEditor.StartEdit(Index: integer);
var
  s: string;
  lv: TAdvListValue;
begin
  s := Values[Index].DisplayText;
  lv := Values[Index];
  DoValueEditStart(lv, s);
  FEditTag := lv.Tag;
  FEditImageIndex := lv.ImageIndex;
  Values.Delete(Index);
  FEdit.Width := Max(10, Canvas.TextWidth(s) + EDITMARGIN);
  FEdit.SetEditText(s);
  UpdateEditPos(Index);
  FEdit.PopupMenu := PopupMenu;
  FEdit.SelectAll;
  FEdit.SetFocus;
end;

function TAdvListEditor.GetEditPos(Index: integer; AWidth: integer): TPoint;
var
  j, dx, dy, tw, th, dt: integer;
begin
  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dt := th - Canvas.TextHeight('gh');

  dx := LEFTOFFSET;
  dy := TOPOFFSET + (dt div 2);

  if BorderStyle = bsSingle then
    inc(dy);

  for j := 0 to Index - 1 do
  begin
    if (j < Values.Count) then
    begin
      tw := Values[j].GetWidth(Canvas) + ITEMMARGIN;

      if dx + tw - ITEMMARGIN + 4 >= AWidth then
      //if dx + tw >= Width then
      begin
        dx := LEFTOFFSET + tw;
        dy := dy + th + LINEMARGIN;
      end
      else
        dx := dx + tw;
    end;
  end;

  if dx + FEdit.Width >= Width then
  begin
    dx := LEFTOFFSET;
    dy := dy + th + LINEMARGIN;
  end;

  Result := Point(dx,dy);
end;

function TAdvListEditor.GetSelectedCount: Integer;
var
  i: Integer;
begin
  if FMultiSelect then
  begin
  Result := 0;
  for i := 0 to Values.Count-1 do
    if Values[i].Selected then
      Inc(Result);
  end
  else if (ItemIndex >= 0) and (ItemIndex < Values.Count) then
    Result := 1
  else
    Result := 0;
end;

// Friedemann Schmidt
function TAdvListEditor.GetSelection: TIntegerDynArray;
var
  i: Integer;
begin
  Result := nil;
  if not FMultiSelect and (ItemIndex <> -1) then
  begin
    SetLength(Result, 1);
    Result[0] := ItemIndex;
  end
  else if FMultiSelect then
  begin
    for i := 0 to Values.Count-1 do
      if Values[i].Selected then
      begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := i;
      end;
  end;
end;

function TAdvListEditor.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvListEditor.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TAdvListEditor.HasMaxItems: boolean;
begin
  Result := (MaxItems > 0) and (Values.Count = MaxItems);
end;

procedure TAdvListEditor.HideEdit;
begin
  FEdit.Width := 2;
  UpdateEditPos(Values.Count);
end;

procedure TAdvListEditor.InitVCLStyle(Init: Boolean);
{$IFDEF DELPHIXE2_LVL}
var
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  clr: TColor;
{$ENDIF}
begin
{$IFDEF DELPHIXE2_LVL}
  LStyle := StyleServices;

  FUseVCLStyles := (StyleServices.Name <> 'Windows') and LStyle.Enabled;

  if FUseVCLStyles then
  begin
    LDetails := LStyle.GetElementDetails(teEditTextNormal);
    if LStyle.GetElementColor(LDetails, ecTextColor, clr) and (clr <> clNone) {$IFDEF DELPHIXE6_LVL} and (seFont in StyleElements) {$ENDIF} then
    begin
      FStyledFontColor := clr;
    end;
    clr := LStyle.GetStyleColor(scEdit);
    if (clr <> clNone) {$IFDEF DELPHIXE6_LVL} and (seClient in StyleElements) {$ENDIF} then
    begin
      FStyledClientColor := clr;
    end;
    clr := LStyle.GetStyleColor(scBorder);
    if (clr <> clNone) {$IFDEF DELPHIXE6_LVL} and (seBorder in StyleElements) {$ENDIF} then
    begin
      FStyledBorderColor := clr;
    end;
  end;
{$ENDIF}
end;

function TAdvListEditor.ItemHeight: integer;
var
  th: integer;
begin
  Canvas.Font.Assign(Font);

  th := Canvas.TextHeight('gh');

  if Assigned(Images) then
    th := Max(Images.Height, th);

  Result := th;
end;

procedure TAdvListEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

procedure TAdvListEditor.ListChanged(Sender: TObject);
begin
  Invalidate;

  if FAutoSize then
    DoAutoSize;

//  if (csDesigning in ComponentState) then
  UpdateEditPos(Values.Count);
end;


procedure TAdvListEditor.Loaded;
begin
  inherited;

  UpdateDelBtn;

  UpdateEditPos(Values.Count);

  if ReadOnly then
  begin
    FEdit.Width := 1;
    FEdit.Left := 1;
  end;

  if FAutoSize then
    DoAutoSize;

  LookupPopup.Font.Size := ScaleFromSmallFontsDimension(LookupPopup.Font.Size);
end;

procedure TAdvListEditor.LookupChanged(Sender: TObject);
var
  i: integer;
begin
  FEdit.Lookup.DisplayList.Clear;
  FEdit.Lookup.ValueList.Clear;

  for i := 0 to FLookup.Count - 1 do
  begin
    FEdit.Lookup.DisplayList.Add(FLookup.Items[i].DisplayText);
    FEdit.Lookup.ValueList.AddObject(FLookup.Items[i].Value, TObject(FLookup.Items[i].ImageIndex));
  end;
end;

procedure TAdvListEditor.LookupPopupChange(Sender: TObject);
begin
  if Assigned(FEdit) and HandleAllocated then
  begin
    FEdit.Lookup.Enabled := LookupPopup.Enabled;
    FEdit.Lookup.Color := FLookupPopup.Color;
    FEdit.Lookup.CaseSensitive := FLookupPopup.CaseSensitive;
    FEdit.Lookup.DisplayCount := FLookupPopup.Count;
    FEdit.Lookup.NumChars := FLookupPopup.FromChar;
    FEdit.Lookup.Font.Assign(FLookupPopup.Font);
    FEdit.Lookup.Spacing := FLookupPopup.Spacing;
    FEdit.Lookup.ValueSeparator := FLookupPopup.ValueSeparator;
  end;
end;
{
  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if i = FEditPos then
      dx := dx + EditWidth;

    if dx + dw - ITEMMARGIN + 4 >= Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + th + LINEMARGIN;
    end;

    if (x >= dx) and (x <= dx + dw - 4) and (y > dy) and (y < dy + ITEMHEIGHT) then
    begin
      // hit an item
      Result := i;
      Exit;
    end;

    dx := dx + dw;
  end;
end;
}

procedure TAdvListEditor.DblClick;
var
  i,dx,dy,dw,th: integer;
  mousePos: TPoint;
begin
  inherited;

  if not FStartEditOnDblClick then
    Exit;

  mousePos := ScreenToClient(Mouse.CursorPos);

  Canvas.Font.Assign(Font);
  th := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if (i = FEditPos) then
    begin
      dx := dx + EditWidth;
    end;

    if dx + dw - ITEMMARGIN + 4 >= Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + th + LINEMARGIN;
    end;

    if (mousePos.x >= dx) and (mousePos.x <= dx + dw - 4) and (mousePos.y > dy) and (mousePos.y < dy + ItemHeight + LINEMARGIN)  then
    begin
      FJustHandledDDblClick := True;
      StartEdit(i);
      break;
    end;
    dx := dx + dw;
  end;
end;
procedure TAdvListEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i,dx,dy,dw,th: integer;
  allow, justStoppedEdit: boolean;

begin
  inherited;


  if FJustHandledDDblClick then
  begin
    FJustHandledDDblClick := False;
    exit;
  end;
  FDownIndex := -1;

  Canvas.Font.Assign(Font);
  th := ItemHeight;

  if FEdit.Text <> '' then
  begin
    StopEdit;
    justStoppedEdit := True;
  end
  else   
    justStoppedEdit := False;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if (i = FEditPos) and not justStoppedEdit then
    begin
      dx := dx + EditWidth;
    end;

    if dx + dw - ITEMMARGIN + 4 >= Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + th + LINEMARGIN;
    end;

    if (x >= dx) and (x <= dx + dw - 4) and (y > dy) and (y < dy + ItemHeight + LINEMARGIN)  then
    begin
      FDownIndex := i;

      if ShowDeleteButton then
      begin
        if x > dx + dw - 4 - FDelBtnWidth then
        begin
          Allow := true;
          DoValueDelete(Values[i], Allow);
          if Allow then
          begin
            Values.Delete(i);
            DoValueDeleted;
          end;
        end;
      end
      else
      begin
        if not Focused then
          SetFocus;
        // already selected & edit
        if (Button <> mbRight) and not StartEditOnDblClick and (i = ItemIndex) and not ReadOnly and not (ssCtrl in Shift) then

        begin
          ItemIndex := -1;
          StartEdit(i);
        end
        else
        // first selection
        begin
          if FMultiSelect then
          begin
            if ssShift in Shift then
            begin
              SelectRange(i);
            end
            else if (ssCtrl in Shift) then
            begin
              Values[i].Selected := not Values[i].Selected;
              if ItemIndex <> i then
                SetItemIndex(i, False, True, True)
              else
              begin
                Invalidate;
                DoSelectionChanged;
              end;
              FSelectionRangeStartIdx := ItemIndex;
            end
            else if not (ssCtrl in Shift) then
            begin
              if (Button <> mbRight) or not Values[i].Selected then
                ClearSelection(False);
              if i <> ItemIndex then
                SetItemIndex(i, True, True, True)
              else
              begin
                Values[i].Selected := True;
              end;
            end
          end
          else
            SetItemIndex(i, False, True, True);

          FEdit.Width := 2;
        end;

        // make sure keys continue to be received
        FEdit.PopupMenu := PopupMenu;
        FEdit.SetFocus;
      end;

      Exit;
    end;

    // after last item on row
    if (x >= dx) and (dx + dw > Width) and (y > dy) and (y < dy + ItemHeight + LINEMARGIN) and not ReadOnly then
    begin
      UpdateEditPos(i);
      Exit;
    end;

    // before first item
    if (x < LEFTOFFSET) and (y > dy) and (y < dy + ItemHeight) and not ReadOnly then
    begin
      UpdateEditPos(i);
      Exit;
    end;

    if (x > dx + dw - 4) and (x <= dx + dw) and (y > dy) and (y < dy + ItemHeight + LINEMARGIN) and not ReadOnly then
    begin
      UpdateEditPos(i + 1);
      Exit;
    end;

    dx := dx + dw;
  end;

  if (x > dx) and (y > dy) and not ReadOnly then
  begin
    UpdateEditPos(Values.Count);
  end;

  FEdit.PopupMenu := PopupMenu;

  if not ReadOnly then
    FEdit.SetFocus;
end;

procedure TAdvListEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  inherited;
  i := ValueAtXY(X, Y);
  if i <> FHintItem then
  begin
    FHintItem := i;
    Application.CancelHint;
  end;
end;

procedure TAdvListEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if (ValueAtXY(X,Y) = FDownIndex) and (FDownIndex >= 0) then
    DoValueClick(FDOwnIndex);

  if (Button = mbRight) and (FEdit.Visible) and (FEdit.Focused) and not Assigned(PopupMenu) then
  begin
    SendMessage(FEdit.Handle, WM_RBUTTONDOWN, 0, MakeLParam(2,2));
    SendMessage(FEdit.Handle, WM_RBUTTONUP, 0, MakeLParam(2,2));
  end;
end;

function TAdvListEditor.MoveValue(AValue: TAdvListValue; ToIndex: Integer): Boolean;
var
  FromIndex: Integer;
begin
  if (AValue.Index = ToIndex) then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  DoValueMove(AValue, ToIndex, Result);
  if not Result then Exit;

  FromIndex := AValue.Index;
  AValue.Index := ToIndex;
  DoValueMoved(AValue, FromIndex);
end;

procedure TAdvListEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;

  inherited;
end;

procedure TAdvListEditor.Paint;
var
  i: integer;
  dx,dy,dw: integer;
  fh: integer;
  R: TRect;
  CurBorderColor, CurClientColor, CurFontColor: TColor;
  {$IFDEF DELPHIXE2_LVL}
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  SaveIndex: Integer;
  {$ENDIF}
begin

  {$IFDEF DELPHIXE2_LVL}
  if FUseVCLStyles {$IFDEF DELPHIXE6_LVL} and (seBorder in StyleElements) {$ENDIF} then
    CurBorderColor := FStyledBorderColor
  else
    CurBorderColor := BorderColor;
  if FUseVCLStyles {$IFDEF DELPHIXE6_LVL} and (seClient in StyleElements) {$ENDIF} then
    CurClientColor := FStyledClientColor
  else
    CurClientColor := Color;
  if FUseVCLStyles {$IFDEF DELPHIXE6_LVL} and (seFont in StyleElements) {$ENDIF} then
    CurFontColor   := FStyledFontColor
  else
    CurFontColor   := Font.Color;

  Canvas.Brush.Color := CurClientColor;

  if (not (csDesigning in ComponentState)) and FUseVCLStyles {$IFDEF DELPHIXE6_LVL} and (seClient in StyleElements) {$ENDIF} then
  begin
    SaveIndex := SaveDC(Canvas.Handle);
    try
      LStyle := StyleServices;
      LDetails := LStyle.GetElementDetails(teEditTextNormal);
      LStyle.DrawElement(Canvas.Handle, LDetails, Rect(0, 0, Width, Height));
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
  end;
  {$ENDIF}

  inherited;

  Canvas.Font.Assign(Font);
  Canvas.Font.Color := CurFontColor;

  fh := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  if BorderStyle = bsSingle then
    inc(dy);

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if (i = FEditPos) and not ReadOnly then
    begin
      if (dx + FEdit.Width >= Width) then
      begin
        dx := LEFTOFFSET;
        dy := dy + fh + LINEMARGIN;
      end;

      dx := dx + EditWidth;
    end;

    if dx + dw - ITEMMARGIN + 4 >= Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + fh + LINEMARGIN;
    end;

    DrawItem(Canvas, Rect(dx, dy, dx + dw - ITEMMARGIN + 4, dy + fh + 2), Values[i], ItemIndex = i);

    dx := dx + dw;
  end;

  {$IFDEF DELPHIXE2_LVL}
  if (BorderStyle = bsNone) and UseVCLStyles then
  begin
    R := ClientRect;
    InflateRect(R, -1,-1);
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := CurBorderColor;
    Canvas.Rectangle(R);
  end
  else
  {$ENDIF}
  if (BorderStyle = bsSingle) and not UseVCLStyles then
  begin
    R := ClientRect;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := FBorderColor;
    Canvas.Rectangle(R);
  end;
end;

procedure TAdvListEditor.Resize;
begin
  inherited;
end;

procedure TAdvListEditor.SetAppearance(const Value: TAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvListEditor.SetAutoSizeEx(const Value: boolean);
begin
  if (FAutoSize <> Value) then
  begin
    FAutoSize := Value;
    if Value then
      DoAutoSize;
  end;
end;

procedure TAdvListEditor.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvListEditor.SetBorderStyle(const Value: TBorderStyle);
begin
  FBorderStyle := Value;
end;

procedure TAdvListEditor.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  oldHeight: Integer;
begin
  oldHeight := Height;

  if FAutoSize and HandleAllocated and not (csLoading in ComponentState) then
    AHeight := GetAutoHeight(AWidth);

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if (oldHeight <> Height) and Assigned(FOnAutoResize) then
    FOnAutoResize(Self);
end;

procedure TAdvListEditor.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TAdvListEditor.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
  if FAutoSize then
    DoAutoSize;
end;

procedure TAdvListEditor.SetItemIndexEx(const Value: integer);
begin
  SetItemIndex(Value, True, True, True);
end;

procedure TAdvListEditor.SetItemIndex(const Value: integer; Select, StartSelectionRange, CallSelectionChangeEvent: Boolean);
begin
  if (FItemIndex <> Value) then
  begin
    FItemIndex := Value;
    if FMultiSelect then
    begin
      if (FItemIndex < 0) or (FItemIndex > Values.Count-1) then
      begin
        ClearSelection(CallSelectionChangeEvent);
        CallSelectionChangeEvent := False;
      end
      else if (FItemIndex >= 0) and (FItemIndex < Values.Count) then
      begin
        if Select then
          Values[FItemIndex].Selected := True;
        if StartSelectionRange then
          FSelectionRangeStartIdx := FItemIndex;
      end;
    end;
    Invalidate;

    if FMultiSelect and CallSelectionChangeEvent then
      DoSelectionChanged;
  end;
end;

procedure TAdvListEditor.SetList(const Value: TAdvListValues);
begin
  FList.Assign(Value);
end;

procedure TAdvListEditor.SetLookup(const Value: TAdvListValues);
begin
  FLookup.Assign(Value);
end;

procedure TAdvListEditor.SetLookupMethod(const Value: TLookupMethod);
begin
  FLookupMethod := Value;
  if Assigned(FEdit) then
  begin
    FEdit.FullTextSearch := (FLookupMethod = lmFull) or (FLookupMethod = lmFullDisplayAndValue);
    FEdit.Lookup.SearchValue := (FLookupMethod = lmFromStartDisplayAndValue) or (FLookupMethod = lmFullDisplayAndValue);
  end;
end;

procedure TAdvListEditor.SetLookupPopup(const Value: TLookupPopup);
begin
  FLookupPopup.Assign(Value);
end;

procedure TAdvListEditor.SetMaxItems(const Value: integer);
begin
  if (FMaxItems <> Value) then
  begin
    FMaxItems := Value;

    if FMaxItems > 0 then
    begin
      while Values.Count > FMaxItems do
        Values.Delete(Values.Count - 1);
    end;
  end;
end;

procedure TAdvListEditor.SetMaxLines(const Value: integer);
begin
  if (Value >= 0) then
  begin
    FMaxLines := Value;
    if (Value > 0) and (AutoSize) then
      DoAutoSize;
  end;
end;

procedure TAdvListEditor.SetMinLines(const Value: integer);
begin
  if (Value >= 0) then
  begin
    FMinLines := Value;
    if (Value > 0) and (AutoSize) then
      DoAutoSize;
  end;
end;

procedure TAdvListEditor.SetMultiSelect(const Value: Boolean);
begin
  FMultiSelect := Value;
  if not FMultiSelect then
    ClearSelection;
end;

procedure TAdvListEditor.SetReadOnly(const Value: boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
    Invalidate;
  end;
end;

procedure TAdvListEditor.SetSelection(const Value: TIntegerDynArray);
var
  i, oldItemIndex, newItemIndex: Integer;
  hasToSetNewItemIndex: Boolean;
begin
  if not FMultiSelect then
    Exit;

  oldItemIndex := ItemIndex;
  newItemIndex := -1;
  hasToSetNewItemIndex := oldItemIndex <> -1;
  ClearSelection(False);
  for i := 0 to Length(Value)-1 do
    if (Value[i] >= 0) and (Value[i] < Values.Count) then
    begin
      Values[Value[i]].Selected := True;
      if Value[i] = oldItemIndex then
        hasToSetNewItemIndex := False
      else if newItemIndex = -1 then
        newItemIndex := Value[i];
    end;

  if hasToSetNewItemIndex and (newItemIndex <> -1) then
    SetItemIndex(newItemIndex, False, True, True)
  else
  begin
    Invalidate;
    DoSelectionChanged;
  end;
end;

// Friedemann Schmidt
procedure TAdvListEditor.SelectAll;
var
  i: Integer;
begin
  if not FMultiSelect then
    Exit;
  for i := 0 to Values.Count-1 do
    Values[i].Selected := True;
  Invalidate;
  DoSelectionChanged;
end;

// Friedemann Schmidt
procedure TAdvListEditor.DeleteSelection;
var
  i, firstDeletedIdx: Integer;
  allow, deletedSomething: Boolean;
begin
  if not FMultiSelect then
    Exit;
  Values.BeginUpdate;
  try
    deletedSomething := False;
    firstDeletedIdx := -1;
    i := 0;
    while i < Values.Count do
      if Values[i].Selected then
      begin
        allow := True;
        DoValueDelete(Values[i], allow);
        if Allow then
        begin
          Values.Delete(i);
          deletedSomething := True;
        end;
        firstDeletedIdx := i;
      end
      else
        Inc(i);
    if (firstDeletedIdx < Values.Count) then
    begin
      SetItemIndex(firstDeletedIdx, True, True, True);
      FEditPos := ItemIndex;
    end
    else
    begin
      ItemIndex := Values.Count - 1;
      UpdateEditPos(ItemIndex);
    end;
    if deletedSomething then
      DoValueDeleted();
  finally
    Values.EndUpdate;
  end;
end;

// Friedemann Schmidt
procedure TAdvListEditor.SelectRange(NewItemIndex: Integer);
var
  i: Integer;
  rangeStartWasSelected: Boolean;
begin
  if not FMultiSelect
    or (FSelectionRangeStartIdx < 0)
    or (FSelectionRangeStartIdx > Values.Count-1)
    or (NewItemIndex < 0)
    or (NewItemIndex > Values.Count-1) then
    Exit;

  rangeStartWasSelected := Values[FSelectionRangeStartIdx].Selected;
  ClearSelection(False);
  for i := Min(FSelectionRangeStartIdx, NewItemIndex) to Max(FSelectionRangeStartIdx, NewItemIndex) do
    if (i <> FSelectionRangeStartIdx) or rangeStartWasSelected then
      Values[i].Selected := True;
  SetItemIndex(NewItemIndex, False, False, True);
end;

procedure TAdvListEditor.SetShowDeleteButton(const Value: boolean);
begin
  if (FShowDeleteButton <> Value) then
  begin
    FShowDeleteButton := Value;
    UpdateEditPos(Values.Count);
    Invalidate;
  end;
end;

procedure TAdvListEditor.SetVersion(const Value: string);
begin

end;

procedure TAdvListEditor.UpdateDelBtn;
begin
  if HandleAllocated then
  begin
    Canvas.Font.Assign(Font);
    FDelBtnWidth := Canvas.TextHeight('gh') + 4;
  end;
end;

procedure TAdvListEditor.UpdateEditPos(Index: integer);
var
  pt: TPoint;
begin
  ItemIndex := -1;

  if Index > Values.Count then
   Index := Values.Count;

  FEditPos := Index;

  pt := GetEditPos(Index,Width);

  FEdit.Left := pt.X + FEditOffset;
  FEdit.Top := pt.Y + 1;

  if (FEdit.Top + FEdit.Height > Height) then
  begin
    if AutoSize then
    begin
      DoAutoSize;
    end
    else
    begin
      FEditPos := 0;
      pt := GetEditPos(Index, Width);

      FEdit.Left := pt.X;
      FEdit.Top := pt.Y + 1;
    end;
  end;

  if not (csDesigning in ComponentState) then
  begin
    if FEdit.Width = 2 then
      FEdit.Width := 10;
  end;

  if BorderStyle = bsSingle then
  begin
    if FEdit.Left + FEdit.Width >= Width then
      FEdit.Width := Width - FEdit.Left - 1;
  end;

  if MaxItems > 0 then
  begin
    if (Values.Count >= MaxItems) then
      FEdit.Width := 0
    else
    begin
      if FEdit.Width = 0 then
        FEdit.Width := 10;
    end;
  end;

  Invalidate;
end;

function TAdvListEditor.ValueAtXY(X, Y: integer): integer;
var
  i,dx,dy,dw,th: integer;
begin
  Result := -1;

  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if i = FEditPos then
      dx := dx + EditWidth;

    if dx + dw - ITEMMARGIN + 4 >= Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + th + LINEMARGIN;
    end;

    if (x >= dx) and (x <= dx + dw - 4) and (y > dy) and (y < dy + ITEMHEIGHT) then
    begin
      // hit an item
      Result := i;
      Exit;
    end;

    dx := dx + dw;
  end;
end;

function TAdvListEditor.ValueRect(Index: integer): TRect;
var
  i,dx,dy,dw,th: integer;
begin
  Result := Rect(0,0,0,0);

  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if i = FEditPos then
      dx := dx + EditWidth;

    if dx + dw > Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + th + LINEMARGIN;
    end;

    if i = Index then
    begin
      Result := Rect(dx, dy, dx + dw, dy + th + LINEMARGIN);
      break;
    end;

    dx := dx + dw;
  end;
end;

{ TAdvListValues }

function TAdvListValues.Add: TAdvListValue;
begin
  Result := TAdvListValue(inherited Add);
end;

function TAdvListValues.AddPair(DisplayText, Value: string): TAdvListValue;
begin
  Result := AddPair(DisplayText,Value,-1);
end;

function TAdvListValues.AddPair(DisplayText, Value: string;
  ImageIndex: integer): TAdvListValue;
var
  lv: TAdvListValue;
begin
  lv := Add;
  lv.DisplayText := DisplayText;
  lv.Value := Value;
  lv.ImageIndex := ImageIndex;
  Result := lv;
end;

procedure TAdvListValues.Changed;
begin
  if Assigned(OnChange) and (UpdateCount = 0) then
    OnChange(Self);
end;

constructor TAdvListValues.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,TAdvListValue);
end;

destructor TAdvListValues.Destroy;
begin

  inherited;
end;

function TAdvListValues.GetItem(Index: integer): TAdvListValue;
begin
  Result := TAdvListValue(inherited Items[Index]);
end;

function TAdvListValues.HasValue(DisplayText: string): boolean;
var
  i: integer;
begin
  Result := true;
  for i := 0 to Count - 1 do
  begin
     if Uppercase(Items[i].DisplayText) = Uppercase(DisplayText) then
       Exit;
  end;

  Result := false;
end;

function TAdvListValues.Insert(Index: integer): TAdvListValue;
begin
  Result := TAdvListValue(inherited Insert(Index));
end;

procedure TAdvListValues.SetItem(Index: integer; const Value: TAdvListValue);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvListValues.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

{ TAdvListValue }

procedure TAdvListValue.Assign(Source: TPersistent);
begin
  if (Source is TAdvListValue) then
  begin
    FDisplayText := (Source as TAdvListValue).DisplayText;
    FImageIndex := (Source as TAdvListValue).ImageIndex;
    FValue := (Source as TAdvListValue).Value;
    FTag := (Source as TAdvListValue).Tag;
    FItem := (Source as TAdvListValue).Item;
  end;
end;

procedure TAdvListValue.Changed;
begin
  if Assigned(Collection) then
    (Collection as TAdvListValues).Changed;
end;

constructor TAdvListValue.Create(Collection: TCollection);
begin
  inherited;
  FTag := 0;
  FImageIndex := -1;
end;

function TAdvListValue.GetWidth(Canvas: TCanvas): integer;
var
  s: string;
  imglist: TCustomImageList;
  le: TAdvListEditor;

begin
  le := TAdvListEditor((Collection as TAdvListValues).Owner);

  s := DisplayText;
  if s = '' then
    s := 'ww';

  Result := Canvas.TextWidth(s) + (le.Appearance.Normal.Rounding div 2);

  imglist := le.Images;

  if (FImageIndex >= 0) and Assigned(imglist) then
  begin
    Result := Result + imglist.Width + IMAGEMARGIN;
  end;

  if le.ShowDeleteButton then
    Result := Result + le.FDelBtnWidth;
end;

procedure TAdvListValue.SetDisplayText(const AValue: string);
begin
  if (FDisplayText <> AValue) then
  begin
    FDisplayText := AValue;
    Changed;
  end;
end;

procedure TAdvListValue.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvListValue.SetValue(const AValue: string);
begin
  if (FValue <> AValue) then
  begin
    FValue := AValue;
    Changed;
  end;
end;

{ TItemAppearance }

procedure TItemAppearance.Assign(Source: TPersistent);
begin
  if (Source is TItemAppearance) then
  begin
    FBorderColor := (Source as TItemAppearance).BorderColor;
    FColorFrom := (Source as TItemAppearance).ColorFrom;
    FColorTo := (Source as TItemAppearance).ColorTo;
    FRounding := (Source as TItemAppearance).Rounding;
    FTextColor := (Source as TItemAppearance).TextColor;
  end;
end;

constructor TItemAppearance.Create;
begin
  inherited;
  FRounding := 6;
  FTextColor := clBlack;
end;

procedure TItemAppearance.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TItemAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    DoChange;
  end;
end;

procedure TItemAppearance.SetColorFrom(const Value: TColor);
begin
  if (FColorFrom <> Value) then
  begin
    FColorFrom := Value;
    DoChange;
  end;
end;

procedure TItemAppearance.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    DoChange;
  end;
end;

procedure TItemAppearance.SetRounding(const Value: integer);
begin
  if (FRounding <> Value) then
  begin
    FRounding := Value;
    DoChange;
  end;
end;

procedure TItemAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    DoChange;
  end;
end;

{ TAppearance }

procedure TAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAppearance) then
  begin
    FNormal.Assign((Source as TAppearance).Normal);
    FSelected.Assign((Source as TAppearance).Selected);
    FFocused.Assign((Source as TAppearance).Focused);
    FFocusedAndSelected.Assign((Source as TAppearance).FocusedAndSelected);
  end;
end;

constructor TAppearance.Create;
begin
  inherited;
  FNormal := TItemAppearance.Create;
  FNormal.OnChange := ItemAppearanceChange;

  FSelected := TItemAppearance.Create;
  FSelected.OnChange := ItemAppearanceChange;
  FFocused := TItemAppearance.Create;
  FFocused.OnChange := ItemAppearanceChange;
  FFocusedAndSelected := TItemAppearance.Create;
  FFocusedAndSelected.OnChange := ItemAppearanceChange;
end;

destructor TAppearance.Destroy;
begin
  FNormal.Free;
  FSelected.Free;
  FFocused.Free;
  FFocusedAndSelected.Free;
  inherited;
end;

procedure TAppearance.ItemAppearanceChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAppearance.SetNormal(const Value: TItemAppearance);
begin
  FNormal.Assign(Value);
end;

procedure TAppearance.SetSelected(const Value: TItemAppearance);
begin
  FSelected.Assign(Value);
end;

procedure TAppearance.SetFocused(const Value: TItemAppearance);
begin
  FFocused.Assign(Value);
end;

procedure TAppearance.SetFocusedAndSelected(const Value: TItemAppearance);
begin
  FFocusedAndSelected.Assign(Value);
end;

{ TLookupPopup }

procedure TLookupPopup.Assign(Source: TPersistent);
begin
  if (Source is TLookupPopup) then
  begin
    FColor := (Source as TLookupPopup).Color;
    FCount := (Source as TLookupPopup).Count;
    FCaseSensitive := (Source as TLookupPopup).CaseSensitive;
    FFont.Assign((Source as TLookupPopup).Font);
    FValueSeparator := (Source as TLookupPopup).ValueSeparator;
  end;
end;

procedure TLookupPopup.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TLookupPopup.Create;
begin
  inherited;
  FColor := clWindow;
  FCount := 6;
  FFromChar := 1;
  FCaseSensitive := false;
  FEnabled := true;
  FFont := TFont.Create;
  FFont.Name := 'Arial';
  FFont.Style := [];
  FFont.Size := 8;
  FFont.OnChange := FontChanged;
  FValueSeparator := vsBracket;
end;

destructor TLookupPopup.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TLookupPopup.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TLookupPopup.SetCaseSensitive(const Value: boolean);
begin
  if (FCaseSensitive <> Value) then
  begin
    FCaseSensitive := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetCount(const Value: integer);
begin
  if (FCount <> Value) then
  begin
    FCount := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TLookupPopup.SetFromChar(const Value: integer);
begin
  if (Value >= 1) and (FFromChar <> Value) then
  begin
    FFromChar := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetSpacing(const Value: integer);
begin
  if (FSpacing <> Value) then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetValueSeparator(const Value: TValueSeparator);
begin
  if (FValueSeparator <> Value) then
  begin
    FValueSeparator := Value;
    Changed;
  end;
end;

end.
