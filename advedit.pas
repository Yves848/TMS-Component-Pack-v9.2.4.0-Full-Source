{*************************************************************************}
{ TAdvEdit & TAdvMaskEdit component                                       }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 1996 - 2019                                       }
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

unit AdvEdit;

{$I TMSDEFS.INC}

interface

{$R AdvEditGlyphs.res}

uses
  Windows, Dialogs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Mask, AdvEdDD, Types
  {$IFDEF DELPHI_UNICODE}
  , Character
  {$ENDIF}
  {$IFDEF DELPHIXE2_LVL}
  , Themes, System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 3; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v2.6.1.0 : added event OnLookupIndexSelect
  // v2.6.2.0 : fix for WinXP themed border drawing
  // v2.6.2.1 : fix for decimalseparator entry when multiple characters are selected
  // v2.6.2.2 : fix for OnClipboardPaste event
  // v2.7.0.0 : New FocusBorderColor property
  //          : optimized property storage in DFM file
  // v2.7.0.1 : fix for issue with DefaultHandling and return key in forms with KeyPreview
  // v2.7.0.2 : fix for transparent = true in VCL.NET
  // v2.7.0.3 : fix for transparent with parent controls without WM_ERASEBKGND
  // v2.7.0.4 : fix for TabOnFullLength for pasting
  // v2.7.0.5 : fix for handling eaRight edit & focus alignment
  // v2.7.0.6 : improved handling of ExcelStyleDecimalSeparator
  // v2.7.1.0 : improved disabled border drawing with WinXP theme
  // v2.7.2.0 : New : DisabledBorder property added
  //          : Improved : focus border drawing for TAdvMaskEdit
  // v2.7.3.0 : New : AllowNumericNullValue added
  // v2.7.3.1 : Fixed : issue with using Color & FlatParentColor = false
  // v2.7.3.2 : Fixed : issue with ESC/Enter key handling on form with KeyPreview=true
  // v2.7.3.3 : Fixed : issue with design time destroy of TCustomAdvEdit, TAdvMaskEdit with Label
  // v2.7.3.4 : Fixed : issue with lookup case insensitive compare
  // v2.7.3.5 : Improved : painting on leaving focus
  // v2.7.4.0 : Fixed : issue with OnChange event
  //          : New : label alignemnts lpRightTop, lpRightCenter, lpRighBottom added
  //          : Improved : clipboard event handling
  // v2.7.4.1 : Fixed : issue with keypreview on frames
  // v2.7.5.0 : Improved : sign not longer taken in account for LengthLimit in etNumeric, etFloat, etMoney types
  // v2.7.5.1 : Fixed : edit rect issue with Ctl3D = false
  // v2.7.5.2 : Fixed : issue with OnChange for etMoney type
  // v2.7.5.3 : Fixed : issue with label margin
  // v2.7.5.4 : Fixed : issue with OnChange & backspace
  // v2.8.0.0 : New : PrecisionDisplay property to show floating point values in shortest possible way
  // v2.8.0.1 : Fixed : issue with PrecisionDisplay when used with edit types other than etFloat, etMoney
  // v2.8.0.2 : Improved : exposed GetTextSize function in public section
  // v2.8.1.1 : Fixed : issue with SoftBorder drawing
  // v2.8.1.2 : Fixed : issue with OnChange
  // v2.8.1.3 : Fixed : issue with ShowError / OnValueValidate & resetting the error
  // v2.8.1.4 : Fixed : issue with TAdvMaskEdit.ParentFont = true and label
  // v2.8.1.5 : Fixed : issue with FocusBorderColor <> clNone and leaving focus
  // v2.8.1.6 : Improved : behaviour with empty text and setting AllowNumericNullValue = true
  // v2.8.1.7 : Fixed : issue with disabled border drawing
  // v2.8.1.8 : Fixed : issue with AutoTab = true on TAdvMaskEdit
  // v2.8.1.9 : Fixed : issue with label font and ShowURL = true
  // v2.8.1.10: Fixed : issue with edit rect when Ctl3D = false
  // v2.8.1.11: Improved : ParentFont set to false when LabelFont is used.
  // v2.8.1.12: Fixed : issue with setting text = '' when AllowNumericNullValue = true
  // v2.8.1.13: Fixed : Modified not set for backspace key
  // v2.8.1.14: Fixed : issue with lookup for non-existing entry
  // v2.8.1.15: Improved : mixed case edit handling
  // v2.8.1.16: Improved : caret positioning when both prefix & suffix are used
  // v2.8.1.17: Fixed : border issue in TAdvMaskEdit when Flat & SoftBorder enabled
  // v2.8.1.18: Fixed : issue with display of lookup list
  // v2.8.2.0 : Improved : Setting FlatLineColor = clNone removes flat line painting
  // v2.8.2.1 : Fixed : issue with handling click on URL
  // v2.8.2.2 : Fixed : border issue with Ctl3D = false and large fonts
  // v2.8.2.3 : Fixed : issue with Paste and EditType = etAlphaNumeric
  // v2.8.3.0 : Improved : lookup settings to avoid duplicates
  // v2.8.4.0 : New : property FocusLabel added to TAdvMaskEdit
  // v2.8.4.1 : Fixed : issue with toggling Enabled property
  // v2.8.4.2 : Fixed : issue with combining ShowError and FocusColor
  //          : Fixed : issue with ParentFont and using LabelCaption
  // v2.8.5.0 : Improved : perform auto focus only when parent form is active
  // v2.8.5.1 : Fixed : issue with LabelFont and Form ScaleBy
  //            Fixed : issue with EmptyText on Windows Vista / 7
  // v2.8.5.2 : Fixed : issue with auto separators in etMoney edit type and backspace key
  // v2.8.5.3 : Fixed : issue with running in different locale from design time locale
  // v2.8.5.4 : Fixed : issue with EditAlign / FocusAlign set to different values
  // v2.8.5.5 : Improved : Empty text drawing position
  // v2.8.5.6 : Fixed : issue with Modified property when backspace on all selected text
  // v2.8.6.0 : Fixed : issue with Precision persistence
  //          : New : Label positions lpTopRight, lpBottomRight added
  // v2.8.6.1 : Fixed : Small issue with OnExit event in TDBAdvEdit
  // v2.8.6.2 : Fixed : Issue with OnChange event with Backspace when all text is selected
  // v2.8.6.3 : Fixed : Issue with OnClick event for first click inside selected text
  // v2.8.6.4 : Fixed : Issue with backspace & lookup
  //          : Fixed : Issue with Alt-F4 & lookup
  // v2.8.6.5 : Fixed : Issue with displaying lookup list in BidiMode right to left
  // v2.8.6.6 : Fixed : Issue with runtime creation & etMoney type setting before handle is allocated
  // v2.8.6.7 : Fixed : Issue with designtime activate of dataset with TDBAdvMaskEdit
  // v2.8.6.8 : Fixed : Issue with FocusBorder = true
  // v2.8.6.9 : Fixed : Issue with Paste in etHex edit type
  // v2.8.6.10: Fixed : Issue with assigning text EditType = etMoney and Precision = 0
  // v2.8.6.11: Fixed : Issue with TabOnFullLength when Shift is pressed
  // v2.8.7.0 : New : Property CharCase exposed
  // v2.8.7.1 : Fixed : Issue with pasting multiline text
  // v2.8.7.2 : Fixed : Some missing LabelPosition in TAdvMaskEdit added
  // v2.9.0.0 : New : Property EmptyTextFocused added
  // v2.9.0.1 : Fixed : Issue with OnKeyDown and VK_BACK when Prefix is used
  // v2.9.0.2 : Fixed : Issue with font size for EmptyText
  // v2.9.0.3 : Fixed : Issue with label positioning with accelerators
  // v2.9.0.4 : Fixed : Issue with Modified in DB controls with VK_BACK key
  // v2.9.0.5 : Fixed : Issue with etPassword and OnChange event
  // v2.9.1.0 : New : BorderColor property added in TCustomAdvEdit, TAdvMaskEdit
  // v2.9.1.1 : Fixed : Issue with Modified & BackSpace under specific circumstances
  // v2.9.1.2 : Fixed : Issue with KeyPreview and Up/Down key
  // v2.9.1.3 : Fixed : Issue to work in XE2 styled app
  // v2.9.1.4 : Fixed : Issue with changing Enabled in combination with ErrorColor
  // v2.9.2.0 : New : FullTextSearch property added
  // v2.9.2.1 : Fixed : Issue with up/down and lookuplist
  // v2.9.2.2 : Improved : Behavior with Alt key handling during lookup
  // v2.9.3.0 : New : Lookup.AcceptOnTab property added
  // v2.9.3.1 : Fixed : Issue with OnChange & Ctrl-X
  // v2.9.3.2 : Fixed : Issue with Ctrl-A
  // v2.9.3.3 : Fixed : Issue with ShowError & toggling Enabled
  // v2.9.4.0 : New : Added property EmptyTextStyle
  // v2.9.4.1 : Fixed : Issue with OnChange event and entering neg. sign
  // v2.9.4.2 : Fixed : Issue with Ctl3D = false and borders
  // v2.9.4.3 : Fixed : Issue with ShowError and LabelFont
  // v3.0.0.0 : Improved : Handling of FocusFontColor
  //          : Improved : Lookup handling
  //          : New : Lookup.ShowValue added
  // v3.0.0.1 : Fixed : Small issue with LabelAlwaysEnabled and use via RTTI of TCustomAdvEdit
  // v3.0.0.2 : Fixed : Issue with lookup list and use on fsStayOnTop forms
  // v3.0.1.0 : Improved : Label font handling with ChangeScale()
  // v3.0.1.1 : Fixed : Issue with TAdvMaskEdit focus label updating
  // v3.0.1.2 : Fixed : Lookup activation issue
  // v3.0.1.3 : Fixed : Issue with designtime border painting
  // v3.0.1.4 : Fixed : Issue with runtime creation of parent control that has TCustomAdvEdit child(s)
  // v3.0.1.5 : Fixed : Issue with label position & reparenting
  // v3.0.1.6 : Improved : Lookup value display
  // v3.0.1.7 : Fixed : Memory leak with TCustomAdvEdit OLE drag & drop
  // v3.1.0.0 : New : Font in lookup dropdown customizable
  //          : New : Spacing for items in lookup dropdown added
  //          : New : Lookup.ValueSeparator property added
  // v3.1.0.1 : Fixed : Lookup dropdown size calculation
  // v3.1.0.2 : Fixed : Issue with Empty text display and VCL styles
  // v3.1.1.0 : Improved : Aligning with label
  // v3.1.1.1 : Fixed : Issue with CharCase <> ecNormal and use of Prefix
  // v3.2.0.0 : New : ShowFieldName added in TDBAdvEdit
  // v3.2.0.1 : Fixed : Issue with OnChange & backspace in specific circumstances
  // v3.2.0.2 : Improved : Behavior for eaRight, eaCenter EditAlign in small edit control
  // v3.2.0.3 : Fixed : Issue with French thousand separator type
  // v3.2.0.4 : Fixed : Rare issue with visible changes & labels
  // v3.2.0.5 : Fixed : Issue with persisting Precision under specific conditions
  // v3.2.0.6 : Fixed : Issue with setting Precision = -1 for etMoney type
  // v3.3.0.0 : New : Lookup.SearchValue property added
  //          : New : OnLookupNeedData event added
  // v3.3.1.0 : New : MinValue/MaxValue properties added for automatic range validation in etNumeric mode
  //          : New : MinFloatValue/MaxFloatValue properties added for automatic range validation in etFloat mode
  // v3.3.1.1 : Fixed : Issue with OnLookupSelect event
  // v3.3.2.0 : New : EditorEnabled property added
  //          : Improved : Centering of labels with edit control
  // v3.3.2.1 : Fixed : Issue with handling ESC key while lookup is displayed
  //          : Improved : Auto cut-off of text exceeding MaxLength instead of blocking
  // v3.3.2.2 : Fixed : Edit alignment issue on Windows XP
  // v3.3.2.3 : Improved : Validation with float nrs & different locales
  // v3.3.2.4 : Fixed : Issue with combination of focus color & error color
  // v3.3.2.5 : Fixed : Issue with use of OnValueValidate & ShowError = false
  // v3.3.2.6 : Fixed : Handling for ShowError for very specific OnValueValidate handling
  // v3.3.2.7 : Fixed : Rare issue with EmptyText and Ctl3D = false
  // v3.3.2.8 : Fixed : Issues with high DPI setting
  // v3.3.3.0 : New : Auto limit entered values for etNumeric edit type to min/max values
  // v3.3.3.1 : Fixed : Issue with OnLookupIndexSelect
  // v3.3.3.2 : Fixed : Issue with error color & enabled state
  // v3.3.4.0 : New : OnValidateEdit, OnValidateError events added in TAdvMaskEdit
  // v3.3.4.1 : Fixed : Regression with handling duplicates
  // v3.3.4.2 : Fixed : Issue with using TCustomAdvEdit on a TCategoryPanel
  // v3.3.4.3 : Fixed : Issue with lookup handling
  // v3.3.4.4 : Fixed : Issue with using FocusLabel with Anchors
  // v3.3.5.0 : New : etInvalidChars edit type added
  //          : New : Event OnClipboardCanPaste added
  // v3.3.5.1 : Fixed : Issue in TDBAdvEdit with AllowNumericNullValue = true and etFloat, etMoney edit types
  // v3.3.5.2 : Fixed : Issue with ReturnIsTab in combination with SetFocus call from events related to Return key
  // v3.3.5.3 : Fixed : Particular case with pdShortest notation
  // v3.3.5.4 : Fixed : Issue with ReturnIsTab and Shift-Enter handling
  // v3.3.6.0 : New : OnUpdateRecord event added in TDBAdvEdit, TDBAdvMaskEdit
  // v3.3.6.1 : Fixed : Rare issue with anchoring & focuslabel
  // v3.3.7.0 : Improved : HighDPI support
  // v3.3.7.1 : Fixed : Issue with TDBAdvEdit.Modified handling
  // v3.3.7.2 : Fixed : Issue with auto key repeat when ReturnIsTab = true
  // v3.3.7.3 : Fixed : OnDialogClose event handling in TAdvDirectoryEdit
  // v3.3.7.4 : Fixed : Issue with combining signed numeric entry and suffixes
  // v3.3.8.0 : New : Moved DefaultHandling property to published
  // v3.3.9.0 : New : AllowShares property added in TAdvDirectoryEdit
  //          : Fixed : Issue with ReturnIsTab = true and using the lookup list
  // v3.4.0.0 : New : Public Value: variant property added to get & set value as variant
  // v3.4.0.1 : Fixed : Ctrl-Enter handling when ReturnIsTab = true
  // v3.4.1.0 : New : DisabledTextColor property added in TCustomAdvEdit
  //          : Fixed : Issue with TAdvMaskEdit and label visibility
  // v3.4.1.1 : Fixed : Issue with high DPI mode when Flat = true
  // v3.4.2.0 : New : Property DefaultHandling added to TAdvMaskEdit
  //          : New : StyleElements exposed
  //          : New : OnAfterClipboardCut, OnAfterClipboardCopy, OnAfterClipboardPaste events added
  // v3.4.2.1 : Improved : BiDiMode support for EmptyText display
  // v3.4.2.2 : Fixed: Issue with setting float or numeric values containing spaces
  // v3.4.2.3 : Fixed: Issue with disabled drawing when EditAlign <> eaLeft
  // v3.4.3.0 : Improved : Persistence will only persist lookup info and not text value for TDBAdvEdit
  //          : Improved : Behavior when Form.Scaled = false
  // v3.4.3.1 : Fixed : Issue with TDBAdvMaskEdit in combination with readonly dataset
  // v3.4.3.2 : Fixed : Issue with use of TCustomAdvEdit on form with Ctl3D = false
  // v3.4.4.0 : New : LookupVisible is writable property to allow to programmatically show the lookuplist
  // v3.4.5.0 : New : Support for multimonitor High DPI handling
  // v3.4.5.1 : Fixed : Issue with lookup font scaling
  // v3.4.5.2 : Fixed : Issue with using TAdvEdit on parent forms not descending from TForm
  // v3.4.5.3 : Fixed : Issue with ShowModified = true in TAdvMaskEdit
  // v3.4.5.4 : Fixed : Issue with OnChange triggering during control loading
  // v3.4.5.5 : Fixed : Painting issue with Ctl3D = false and large fonts
  //          : Improved : OnChange event handling during form OnCreate
  // v3.4.5.6 : Fixed : Rare issue with empty text repainting
  // v3.4.6.0 : New : Property AutoValidate added
  // v3.4.6.1 : Fixed : Issue with label creation on TCategoryPanelGroup
  // v3.4.6.2 : Fixed : Regression with backspace handling & OnChange
  //          : Fixed : Issue with runtime label caption updating
  // v3.4.6.3 : Fixed : Issue with TDBAdvEdit check for editing in dataset
  // v3.4.6.4 : Fixed : LengthLimit handling in paste
  // v3.4.6.5 : Fixed : Issue with ReadOnly in TDBAdvEdit
  // v3.4.6.6 : Fixed : Issue with disabled control on parent control with DoubleBuffered = true
  // v3.4.7.0 : Improved : Label positioning in high DPI
  // v3.4.7.1 : Fixed : Issue with lookup list font in high DPI
  // v3.4.8.0 : New : Enabled new Windows folder dialog UI style by default
  // v3.4.8.1 : Fixed : Issue with flat TAdvMaskedit handling
  // v3.4.9.0 : New : Public method Validate added
  // v3.4.9.1 : Fixed : High DPI changing scale issue with label font size
  // v3.5.0.0 : Improved : VCL Styles support
  // v3.5.0.1 : Improved : Behavior with readonly fields for TDBAdvEdit

const
  LOOKUPITEMHEIGHT = 12;

type
  {$IFDEF DELPHI_UNICODE}
  {$EXTERNALSYM THintInfo}
  THintInfo = Controls.THintInfo;
  {$EXTERNALSYM PHintInfo}
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TLabelPosition = (lpLeftTop, lpLeftCenter, lpLeftBottom, lpTopLeft, lpBottomLeft,
    lpLeftTopLeft, lpLeftCenterLeft, lpLeftBottomLeft, lpTopCenter, lpBottomCenter,
    lpRightTop, lpRightCenter, lpRighBottom, lpTopRight, lpBottomRight);

  TAdvEditType = (etString, etNumeric, etFloat, etUppercase, etMixedCase, etLowerCase,
    etPassword, etMoney, etRange, etHex, etAlphaNumeric, etValidChars, etInvalidChars);

  TAutoType = (atNumeric, atFloat, atString, atDate, atTime, atHex);

  TValueValidateEvent = procedure(Sender: TObject; Value: string; var IsValid: Boolean) of object;

  TClipboardEvent = procedure(Sender: TObject; Value: string; var allow: Boolean) of object;

  TClipboardAllowEvent = procedure(Sender: TObject; var Value: string; var allow: Boolean) of object;

  TMaskCompleteEvent = procedure(Sender: TObject; Value: string; var accept: Boolean) of object;

  TLookupSelectEvent = procedure(Sender: TObject; var Value: string) of object;

  TLookupIndexSelectEvent = procedure(Sender: TObject; Index: Integer; var Value: string) of object;

  TLookupNeedDataEvent = procedure(Sender: TObject; Value: string; List: TStrings; var ItemIndex: integer) of object;

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TRangeList = class(TList)
  private
    procedure SetInteger(Index: Integer; Value: Integer);
    function GetInteger(Index: Integer): Integer;
  public
    constructor Create;
    property Items[index: Integer]: Integer read GetInteger write SetInteger; default;
    procedure Add(Value: integer);
    procedure AddMultiple(Value, Count: Integer);
    procedure Delete(Index: Integer);
    procedure Show;
    function InList(Value: integer): Boolean;
    function StrToList(s: string): Boolean;
  end;

  TPersistenceLocation = (plInifile, plRegistry);

  TPersistence = class(TPersistent)
  private
    FEnable: boolean;
    FKey: string;
    FSection: string;
    FLocation: TPersistenceLocation;
  published
    property Enable: Boolean read FEnable write FEnable default False;
    property Key: string read FKey write FKey;
    property Section: string read FSection write FSection;
    property Location: TPersistenceLocation read FLocation write FLocation default plIniFile;
  end;

  TItemClickEvent = procedure(Sender: TObject; Index: Integer) of object;

  TListBoxTab = class(TListBox)
  private
    FHandleTab: boolean;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  published
    property HandleTab: boolean read FHandleTab write FHandleTab;
  end;

  { TListHintWindow }
  TListHintWindow = class(THintWindow)
  private
    FListControl: TListBox;
    procedure WMNCButtonDown(var Message: TMessage); message WM_NCLBUTTONDOWN;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BorderWidth;
    property Text;
  published
    property ListControl: TListBox read FListControl write FListControl;
  end;

  TValueSeparator = (vsBracket, vsSquareBracket, vsLargerSmaller, vsNone);

  TLookupSettings = class(TPersistent)
  private
    FDisplayList: TStringList;
    FValueList: TStringList;
    FDisplayCount: Integer;
    FColor: TColor;
    FEnabled: Boolean;
    FNumChars: Integer;
    FCaseSensitive: Boolean;
    FHistory: Boolean;
    FMulti: Boolean;
    FSeparator: char;
    FAcceptOnTab: boolean;
    FShowValue: boolean;
    FSearchValue: boolean;
    FFont: TFont;
    FSpacing: Integer;
    FValueSeparator: TValueSeparator;
    procedure SetDisplayList(const Value: TStringList);
    procedure SetValueList(const Value: TStringList);
    procedure SetNumChars(const Value: Integer);
    procedure SetFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddPair(DisplayText,Value: string);
  published
    property AcceptOnTab: Boolean read FAcceptOnTab write FAcceptOnTab default False;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property Color: TColor read FColor write FColor default clWindow;
    property DisplayCount: Integer read FDisplayCount write FDisplayCount default 4;
    property DisplayList: TStringList read FDisplayList write SetDisplayList;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Font: TFont read FFont write SetFont;
    property History: Boolean read FHistory write FHistory default False;
    property NumChars: Integer read FNumChars write SetNumChars default 2;
    property ValueList: TStringList read FValueList write SetValueList;
    property Multi: Boolean read FMulti write FMulti default False;
    property Separator: char read FSeparator write FSeparator;
    property SearchValue: boolean read FSearchValue write FSearchValue default false;
    property ShowValue: boolean read FShowValue write FShowValue default false;
    property Spacing: Integer read FSpacing write FSpacing default 0;
    property ValueSeparator: TValueSeparator read FValueSeparator write FValueSeparator default vsBracket;
  end;

  TURLClickEvent = procedure(Sender: TObject; URL: string; var Show: Boolean) of object;

  TEditAlign = (eaLeft, eaRight, eaDefault, eaCenter);

  TPrecisionDisplay = (pdNormal, pdShortest);

  TCustomAdvEditLabel = class(TLabel)
  private
    FAlwaysEnable: boolean;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
  published
    property AlwaysEnable: boolean read FAlwaysEnable write FAlwaysEnable;
  end;

  TCustomAdvEdit = class(TCustomEdit)
  private
    { Private declarations }
    FAlignChanging: Boolean;
    FLabel: TCustomAdvEditLabel;
    FLabelFont: TFont;
    FLabelPosition: TLabelPosition;
    FLabelMargin: Integer;
    FLabelTransparent: Boolean;
    FAutoFocus: Boolean;
    FAutoValidate: Boolean;
    FCanUndo: Boolean;
    FEditType: TAdvEditType;
    FEditAlign: TEditAlign;
    FOldEditAlign: TEditAlign;
    FOldBorder: TBorderStyle;
    FExcelStyleDecimalSeparator: Boolean;
    FTabOnFullLength: Boolean;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FDisabledBorder: boolean;
    FNormalColor: TColor;
    FFocusColor: TColor;
    FFocusFontColor: TColor;
    FErrorColor: TColor;
    FErrorFontColor: TColor;
    FError: Boolean;
    FFocusLabel: Boolean;
    FFontColor: TColor;
    FModifiedColor: TColor;
    FReturnIsTab: Boolean;
    FShowModified: Boolean;
    FIsModified: Boolean;
    FShowURL: Boolean;
    FURLColor: TColor;
    FFocusWidthInc: Integer;
    FFocusAlign: TEditAlign;
    FLengthLimit: SmallInt;
    FPrecision: SmallInt;
    FPrecisionDisplay: TPrecisionDisplay;
    FPrefix: string;
    FSuffix: string;
    FOldString: string;
    FSigned: Boolean;
    FIsUrl: Boolean;
    FFlat: Boolean;
    FMouseInControl: Boolean;
    FFlatLineColor: TColor;
    FPersistence: TPersistence;
    FOnValueValidate: TValueValidateEvent;
    FOnClipboardCut: TClipboardEvent;
    FOnClipboardPaste: TClipboardEvent;
    FOnClipboardCanPaste: TClipboardAllowEvent;
    FOnClipboardCopy: TClipboardEvent;
    FBlockCopy: boolean;
    FFlatParentColor: Boolean;
    FTransparent: Boolean;
    FCaretPos: TPoint;
    FOleDropSource: Boolean;
    FOleDropTarget: Boolean;
    FOleDropTargetAssigned: Boolean;
    FIsDragSource: Boolean;
    FButtonDown: Boolean;
    FFocusBorder: Boolean;
    FFocusBorderColor: TColor;
    FBorderColor: TColor;
    FHintShowLargeText: Boolean;
    FShowError: Boolean;
    FAutoThousandSeparator: Boolean;
    FEmptyText: string;
    FEmptyTextFocused: boolean;
    FEmptyTextStyle: TFontStyles;
    FSoftBorder: Boolean;
    FDefaultHandling: Boolean;
    FBlockDefaultHandling: Boolean;
    FLabelAlwaysEnabled: Boolean;
    FBorder3D: Boolean;
    FErrorMarkerLen: Integer;
    FErrorMarkerPos: Integer;
    FIndentR: Integer;
    FIndentL: Integer;
    FLoadedHeight: Integer;
    FLookupList: TListHintWindow;
    FLookupListBox: TListboxTab;
    FLookup: TLookupSettings;
    FOnLookupSelect: TLookupSelectEvent;
    FOnLookupIndexSelect: TLookupIndexSelectEvent;
    FIsValidating: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnURLClick: TURLClickEvent;
    FOnLabelClick: TNotifyEvent;
    FOnLabelDblClick: TNotifyEvent;
    FValidChars: string;
    FInvalidChars: string;
    FIsWinXP: Boolean;
    FIsThemed: Boolean;
    FBlockChange: Boolean;
    FAllowNumericNullValue: Boolean;
    FParentFnt: boolean;
    FFullTextSearch: boolean;
    FDesignTime: boolean;
    FReturnAsTabKey: word;
    FMinValue: Longint;
    FMaxValue: Longint;
    FMinFloatValue: Double;
    FMaxFloatValue: Double;
    FEditorEnabled: boolean;
    FGotReturn: boolean;
    FOnLookupNeedData: TLookupNeedDataEvent;
    FLblFntHeight: integer;
    FLblUpdate: boolean;
    FOnAfterClipboardCut: TNotifyEvent;
    FOnAfterClipboardPaste: TNotifyEvent;
    FOnAfterClipboardCopy: TNotifyEvent;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMCancelMode(var Message: TMessage); message CM_CANCELMODE;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNCtlColorEdit(var Message: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMCopy(var Message: TWMCopy); message WM_COPY;
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TWMKeyUp); message WM_KEYUP;
    procedure WMSysKeyDown(var Msg: TMessage); message WM_SYSKEYDOWN;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMLButtonDown(var Msg: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMDestroy(var Msg: TMessage); message WM_DESTROY;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    procedure SetEditType(value: TAdvEditType);
    function GetText: string;
    procedure SetText(value: string);
    function GetFloat: double;
    function GetInt: integer;
    function FixedLength(s: string): Integer;
    function AllowMin(ch: char): boolean;
    function DecimalPos: Integer;
    procedure SetFloat(const Value: double);
    procedure SetInt(const Value: integer);
    procedure SetPrefix(const Value: string);
    procedure SetSuffix(const Value: string);
    procedure SetLabelCaption(const value: string);
    function GetLabelCaption: string;
    procedure SetLabelPosition(const value: TLabelPosition);
    procedure SetLabelMargin(const value: integer);
    procedure SetLabelTransparent(const value: boolean);
    procedure SetFlat(const value: boolean);
    procedure SetFlatRect(const Value: Boolean);
    procedure SetPrecision(const Value: SmallInt);
    procedure SetPrecisionDisplay(const Value: TPrecisionDisplay);
    function EStrToFloat(s: string): extended;
    procedure UpdateLabel;
    procedure UpdateLabelPos;
    procedure AutoSeparators;
    function GetModified: boolean;
    procedure SetModified(const Value: boolean);
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
    procedure PaintEdit;
    procedure DrawControlBorder(DC: HDC);
    procedure DrawBorder;
    function Is3DBorderButton: Boolean;
    procedure SetDisabledColor(const Value: TColor);
    procedure SetDisabledBorder(const Value: boolean);
    procedure SetEditAlign(const Value: TEditAlign);
    procedure SetCanUndo(const Value: boolean);
    function GetColorEx: TColor;
    procedure SetColorEx(const Value: TColor);
    procedure SetTransparent(const Value: boolean);
    procedure SetFlatLineColor(const Value: TColor);
    procedure SetFlatParentColor(const Value: boolean);
    procedure LabelFontChange(Sender: TObject);
    procedure SetLabelFont(const Value: TFont);
    procedure ApplyErrorColor;
    function GetError: Boolean;
    procedure SetError(const Value: Boolean);
    procedure ApplyURL(const Value: Boolean);
    procedure DrawErrorLines(Canvas: TCanvas; ErrPos, ErrLen: Integer);
    procedure SetOleDropSource(const Value: boolean);
    procedure SetOleDropTarget(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure CloseLookup;
    procedure DoneLookup;
    function GetHeightEx: Integer;
    procedure ListKeyPress(Sender: TObject; var Key: Char);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetAutoThousandSeparator(const Value: Boolean);
    procedure SetBorder3D(const Value: Boolean);
    procedure SetEmptyText(const Value: string);
    procedure SetErrorMarkerLen(const Value: Integer);
    procedure SetErrorMarkerPos(const Value: Integer);
    procedure SetFocusBorder(const Value: Boolean);
    procedure SetHeightEx(const Value: Integer);
    procedure SetLabelAlwaysEnabled(const Value: Boolean);
    procedure SetSoftBorder(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetEmptyTextStyle(const Value: TFontStyles);
    function GetLookupVisible: boolean;
    function GetValue: variant;
    procedure SetValue(const Value: variant);
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetLookupVisible(const Value: boolean);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CreateLabel: TCustomAdvEditLabel;
    procedure Loaded; override;
    procedure UpdateLookup;
    procedure InvalidateCaret(pt: tpoint);
    procedure EraseCaret;
    procedure DrawCaretByCursor;
    procedure SetCaretByCursor;
    property IndentR: Integer read FIndentR write FIndentR;
    property IndentL: Integer read FIndentL write FIndentL;
    function DoValidate(value: string): Boolean; virtual;
    procedure ValidateEvent(Value: string; var IsValid: Boolean); virtual;
    procedure LabelClick(Sender: TObject); virtual;
    procedure LabelDblClick(Sender: TObject); virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetTextDirect(s:string);
    function TestURL: Boolean; virtual;
    function IsDB: boolean; virtual;
    property BlockDefaultHandling: boolean read FBlockDefaultHandling write FBlockDefaultHandling;
    {$IFNDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
    {$IFDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    procedure DoLookupNeedData(Value: string; List: TStrings; var ItemIndex: integer); virtual;

    property AllowNumericNullValue: Boolean read FAllowNumericNullValue write FAllowNumericNullValue default False;
    property AutoFocus: boolean read FAutoFocus write FAutoFocus default False;
    property AutoThousandSeparator: Boolean read FAutoThousandSeparator write SetAutoThousandSeparator default True;
    property AutoValidate: Boolean read FAutoValidate write FAutoValidate default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property DefaultHandling: Boolean read FDefaultHandling write FDefaultHandling default True;
    property EditAlign: TEditAlign read FEditAlign write SetEditAlign default eaLeft;
    property EditorEnabled: boolean read FEditorEnabled write FEditorEnabled default True;
    property EditType: TAdvEditType read FEditType write SetEditType default etString;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property EmptyTextFocused: boolean read FEmptyTextFocused write FEmptyTextFocused default false;
    property EmptyTextStyle: TFontStyles read FEmptyTextStyle write SetEmptyTextStyle;
    property ErrorMarkerPos: Integer read FErrorMarkerPos write SetErrorMarkerPos default 0;
    property ErrorMarkerLen: Integer read FErrorMarkerLen write SetErrorMarkerLen default 0;
    property ErrorColor: TColor read FErrorColor write FErrorColor default clRed;
    property ErrorFontColor: TColor read FErrorFontColor write FErrorFontColor default clWhite;
    property ExcelStyleDecimalSeparator: boolean read fExcelStyleDecimalSeparator write fExcelStyleDecimalSeparator default False;
    property Flat: boolean read FFlat write SetFlat default False;
    property FlatLineColor: TColor read fFlatLineColor write SetFlatLineColor default clBlack;
    property FlatParentColor: boolean read fFlatParentColor write SetFlatParentColor default true;
    property FocusAlign: TEditAlign read FFocusAlign write FFocusAlign default eaDefault;
    property FocusBorder: Boolean read FFocusBorder write SetFocusBorder default False;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property FocusFontColor: TColor read FFocusFontColor write FFocusFontColor default clWindowText;
    property FocusLabel: Boolean read FFocusLabel write FFocusLabel default false;
    property FocusWidthInc: Integer read FFocusWidthInc write FFocusWidthInc default 0;
    property FullTextSearch: boolean read FFullTextSearch write FFullTextSearch default false;
    property Height: Integer read GetHeightEx write SetHeightEx;
    property MinValue: Longint read FMinValue write FMinValue default 0;
    property MaxValue: Longint read FMaxValue write FMaxValue default 0;
    property MinFloatValue: double read FMinFloatValue write FMinFloatValue;
    property MaxFloatValue: double read FMaxFloatValue write FMaxFloatValue;
    property ModifiedColor: TColor read FModifiedColor write FModifiedColor default clHighlight;
    property DisabledBorder: Boolean read FDisabledBorder write SetDisabledBorder default true;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clSilver;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clNone;
    property ShowError: Boolean read FShowError write FShowError default False;
    property ShowModified: Boolean read FShowModified write FShowModified default False;
    property ShowURL: Boolean read FShowURL write FShowURL default False;
    property SoftBorder: Boolean read FSoftBorder write SetSoftBorder default False;
    property URLColor: TColor read FURLColor write FURLColor default clBlue;
    property ReturnIsTab: Boolean read fReturnIsTab write FReturnIsTab default False;
    property LengthLimit: smallint read fLengthLimit write FLengthLimit default 0;
    property TabOnFullLength: boolean read fTabOnFullLength write FTabOnFullLength default False;
    property Precision: smallint read FPrecision write SetPrecision default 0;
    property PrecisionDisplay: TPrecisionDisplay read FPrecisionDisplay write SetPrecisionDisplay default pdNormal;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
    property LabelCaption: string read GetLabelCaption write SetLabelCaption;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpLeftTop;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin default 4;
    property LabelTransparent: Boolean read FLabelTransparent write SetLabelTransparent default False;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write SetLabelAlwaysEnabled default False;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property Lookup: TLookupSettings read FLookup write FLookup;
    property Persistence: TPersistence read FPersistence write FPersistence;
    property CanUndo: boolean read FCanUndo write SetCanUndo default True;
    property Color: TColor read GetColorEx write SetColorEx;
    property HintShowLargeText: boolean read FHintShowLargeText write FHintShowLargeText default False;
    property InvalidChars: string read FInvalidChars write FInvalidChars;
    property OleDropTarget: Boolean read fOleDropTarget write SetOleDropTarget default False;
    property OleDropSource: Boolean read fOleDropSource write SetOleDropSource default False;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Signed: Boolean read FSigned write FSigned default False;
    property Text: string read GetText write SetText;
    property Transparent: boolean read FTransparent write SetTransparent default False;
    property ValidChars: string read FValidChars write FValidChars;
    property Version: string read GetVersion write SetVersion;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnLookupSelect: TLookupSelectEvent read FOnLookupSelect write FOnLookupSelect;
    property OnLookupIndexSelect: TLookupIndexSelectEvent read FOnLookupIndexSelect write FOnLookupIndexSelect;
    property OnLabelClick: TNotifyEvent read FOnLabelClick write FOnLabelClick;
    property OnLabelDblClick: TNotifyEvent read FOnLabelDblClick write FOnLabelDblClick;
    property OnLookupNeedData: TLookupNeedDataEvent read FOnLookupNeedData write FOnLookupNeedData;
    property OnURLClick: TURLClickEvent read FOnURLClick write FOnURLClick;
    property OnValueValidate: TValueValidateEvent read FOnValueValidate write FOnValueValidate;
    property OnAfterClipboardCopy: TNotifyEvent read FOnAfterClipboardCopy write FOnAfterClipboardCopy;
    property OnAfterClipboardCut: TNotifyEvent read FOnAfterClipboardCut write FOnAfterClipboardCut;
    property OnAfterClipboardPaste: TNotifyEvent read FOnAfterClipboardPaste write FOnAfterClipboardPaste;
    property OnClipboardCopy: TClipboardEvent read FOnClipboardCopy write FOnClipboardCopy;
    property OnClipboardCut: TClipboardEvent read FOnClipboardCut write FOnClipboardCut;
    property OnClipboardPaste: TClipboardEvent read FOnClipboardPaste write FOnClipboardPaste;
    property OnClipboardCanPaste: TClipboardAllowEvent read FOnClipboardCanPaste write FOnClipboardCanPaste;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SelectAll;
    procedure SelectBeforeDecimal;
    procedure SelectAfterDecimal;
    procedure Init;
    procedure Clear; override;
    function GetTextSize: integer;
    function CharFromPos(pt: TPoint): Integer;
    function PosFromChar(uChar: word): TPoint;
    property FloatValue: double read GetFloat write SetFloat;
    property IntValue: Integer read GetInt write SetInt;
    property Modified: Boolean read GetModified write SetModified;
    property IsError: Boolean read GetError write SetError;
    function RangeStrToList(rangelist: TRangeList): Boolean;
    procedure ListToRangeStr(rangelist: TRangeList);
    procedure SetEditText(AValue: string);
    procedure LoadPersist; virtual;
    procedure SavePersist; virtual;
    procedure Validate; virtual;
    property Value: variant read GetValue write SetValue;
    property ReturnAsTabKey: word read FReturnAsTabKey write FReturnAsTabKey;
    property OrigText: string read FOldString write FOldString;
    property LookupVisible: boolean read GetLookupVisible write SetLookupVisible;
    property EditLabel: TCustomAdvEditLabel read FLabel;
    property Border3D: Boolean read FBorder3D write SetBorder3D;
    class function HexToInt(s: string): Integer;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvEdit = class(TCustomAdvEdit)
  published
    { Published declarations }
    property OnValueValidate;
    property OnAfterClipboardCopy;
    property OnAfterClipboardCut;
    property OnAfterClipboardPaste;
    property OnClipboardCopy;
    property OnClipboardCut;
    property OnClipboardPaste;
    property OnClipboardCanPaste;
    property AllowNumericNullValue;
    property AutoFocus;
    property AutoThousandSeparator;
    property BorderColor;
    property DefaultHandling;
    property EditAlign;
    property EditorEnabled;
    property EditType;
    property EmptyText;
    property EmptyTextFocused;
    property EmptyTextStyle;
    property ErrorMarkerPos;
    property ErrorMarkerLen;
    property ErrorColor;
    property ErrorFontColor;
    property ExcelStyleDecimalSeparator;
    property Flat;
    property FlatLineColor;
    property FlatParentColor;
    property FocusAlign;
    property FocusBorder;
    property FocusBorderColor;
    property FocusColor;
    property FocusFontColor;
    property FocusLabel;
    property FocusWidthInc;
    property FullTextSearch;
    property Height;
    property MinValue;
    property MaxValue;
    property MinFloatValue;
    property MaxFloatValue;
    property ModifiedColor;
    property DisabledBorder;
    property DisabledColor;
    property DisabledTextColor;
    property ShowError;
    property ShowModified;
    property ShowURL;
    property SoftBorder;
    property URLColor;
    property ReturnIsTab;
    property LengthLimit;
    property TabOnFullLength;
    property Precision;
    property PrecisionDisplay;
    property Prefix;
    property Suffix;
    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;
    property Lookup;
    property Persistence;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property Align;
    property Anchors;
    property BiDiMode;
    property ParentBiDiMode;
    property Constraints;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CanUndo;
    property CharCase;
    property Color;
    property Ctl3D;
{$IFDEF DELPHI_UNICODE}
    property DoubleBuffered;
{$ENDIF}
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Hint;
    property HintShowLargeText;
    property ImeMode;
    property ImeName;
    property InvalidChars;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property OleDropTarget;
    property OleDropSource;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Signed;
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}
    property TabOrder;
    property TabStop;
    property Text;
    property Transparent;
    property ValidChars;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseUp;
    property OnContextPopup;
    property OnLookupSelect;
    property OnLookupIndexSelect;
    property OnLabelClick;
    property OnLabelDblClick;
    property OnLookupNeedData;
    property OnURLClick;
    property Version;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvCustomMaskEdit = class(TCustomMaskEdit)
  private
    { Private declarations }
    FLabel: TLabel;
    FAutoFocus: boolean;
    FAutoTab: Boolean;
    FReturnIsTab: Boolean;
    FAlignment: TAlignment;
    FFocusColor: TColor;
    FFocusFontColor: TColor;
    FNormalColor: TColor;
    FLoadedColor: TColor;
    FFontColor: TColor;
    FModifiedColor: tcolor;
    FShowModified: Boolean;
    FLabelMargin: integer;
    FLabelPosition: TLabelPosition;
    FLabelTransparent: boolean;
    FSelectFirstChar: boolean;
    FFlat: boolean;
    FOnMaskComplete: TMaskCompleteEvent;
    FDisabledColor: TColor;
    FDisabledBorder: Boolean;
    FOriginalValue: string;
    FCanUndo: Boolean;
    FLabelFont: TFont;
    FLabelAlwaysEnabled: Boolean;
    FFlatLineColor: TColor;
    FSoftBorder: Boolean;
    FFocusBorder: Boolean;
    FFocusBorderColor: TColor;
    FBorderColor: TColor;
    FIndentR: Integer;
    FIndentL: Integer;
    FMouseInControl: Boolean;
    FBorder3D: Boolean;
    FFlatParentColor: Boolean;
    FOldBorder: TBorderStyle;
    FIsThemed: Boolean;
    FFocusLabel: Boolean;
    FParentFnt: Boolean;
    FGotReturn: Boolean;
    FOnValidateError: TNotifyEvent;
    FOnValidateEdit: TNotifyEvent;
    FLblFntHeight: integer;
    FLblUpdate: boolean;
    FDefaultHandling: Boolean;
    FBlockDefaultHandling: boolean;
    FOnAfterClipboardCut: TNotifyEvent;
    FOnAfterClipboardPaste: TNotifyEvent;
    FOnClipboardCut: TClipboardEvent;
    FOnClipboardPaste: TClipboardEvent;
    FOnAfterClipboardCopy: TNotifyEvent;
    FOnClipboardCopy: TClipboardEvent;
    procedure SetAlignment(value: TAlignment);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMCopy(var Message: TWMCopy); message WM_COPY;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TWMKeyUp); message WM_KEYUP;
    function GetLabelCaption: string;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelMargin(const Value: integer);
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure UpdateLabel;
    procedure UpdateLabelPos;
    procedure SetFlat(const Value: boolean);
    function GetModified: boolean;
    procedure SetModified(const Value: boolean);
    procedure SetLabelTransparent(const Value: boolean);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetDisabledBorder(const Value: boolean);
    function GetEnabledEx: Boolean;
    procedure SetEnabledEx(const Value: Boolean);
    function GetColorEx: TColor;
    procedure SetColorEx(const Value: TColor);
    procedure SetLabelFont(const Value: TFont);
    procedure LabelFontChanged(Sender: TObject);
    procedure SetVisible(const Value: Boolean);
    procedure SetFlatLineColor(const Value: TColor);
    procedure PaintEdit;
    procedure SetSoftBorder(const Value: Boolean);
    procedure DrawBorder;
    procedure DrawControlBorder(DC: HDC);
    function Is3DBorderButton: Boolean;
    procedure SetBorder3D(const Value: Boolean);
    procedure SetFlatRect(const Value: Boolean);
    procedure SetFlatParentColor(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetAutoFocus(const Value: boolean);
    procedure SetBorderColor(const Value: TColor);
    function GetVisible: boolean;
  protected
    { Protected declarations }
    property IndentR: Integer read FIndentR write FIndentR;
    property IndentL: Integer read FIndentL write FIndentL;
    function GetVersionNr: Integer; virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure ValidateError; override;
    function CreateLabel: TLabel;
    property NormalColor: TColor read FNormalColor;
    property BlockDefaultHandling: boolean read FBlockDefaultHandling write FBlockDefaultHandling;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    destructor Destroy; override;
    property Modified: Boolean read GetModified write SetModified;
    procedure Loaded; override;
    procedure ValidateEdit; override;

    property EditLabel: TLabel read FLabel;
    property Border3D: Boolean read FBorder3D write SetBorder3D;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoFocus: Boolean read fAutoFocus write SetAutoFocus;
    property AutoTab: Boolean read FAutoTab write FAutoTab default true;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property CanUndo: Boolean read FCanUndo write FCanUndo default false;
    property Color: TColor read GetColorEx write SetColorEx;
    property DefaultHandling: Boolean read FDefaultHandling write FDefaultHandling;
    property DisabledBorder: Boolean read FDisabledBorder write SetDisabledBorder default true;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clSilver;
    property Enabled: Boolean read GetEnabledEx write SetEnabledEx;
    property Flat: Boolean read FFlat write SetFlat;
    property FlatLineColor: TColor read FFlatLineColor write SetFlatLineColor;
    property FlatParentColor: Boolean read FFlatParentColor write SetFlatParentColor;
    property FocusBorderColor: TColor read  FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property FocusBorder: Boolean read FFocusBorder write FFocusBorder;
    property FocusFontColor: TColor read FFocusFontColor write FFocusFontColor;
    property FocusLabel: Boolean read FFocusLabel write FFocusLabel default false;
    property LabelCaption: string read GetLabelCaption write SetLabelCaption;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write FLabelAlwaysEnabled;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin;
    property LabelTransparent: boolean read FLabelTransparent write SetLabelTransparent;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property ModifiedColor: TColor read FModifiedColor write FModifiedColor;
    property ReturnIsTab: Boolean read FReturnIsTab write FReturnIsTab default True;
    property ShowModified: boolean read FShowModified write FShowModified;
    property SoftBorder: Boolean read FSoftBorder write SetSoftBorder default False;
    property SelectFirstChar: boolean read FSelectFirstChar write FSelectFirstChar;
    property Version: string read GetVersion write SetVersion;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnAfterClipboardCopy: TNotifyEvent read FOnAfterClipboardCopy write FOnAfterClipboardCopy;
    property OnAfterClipboardCut: TNotifyEvent read FOnAfterClipboardCut write FOnAfterClipboardCut;
    property OnAfterClipboardPaste: TNotifyEvent read FOnAfterClipboardPaste write FOnAfterClipboardPaste;
    property OnClipboardCopy: TClipboardEvent read FOnClipboardCopy write FOnClipboardCopy;
    property OnClipboardCut: TClipboardEvent read FOnClipboardCut write FOnClipboardCut;
    property OnClipboardPaste: TClipboardEvent read FOnClipboardPaste write FOnClipboardPaste;
    property OnMaskComplete: TMaskCompleteEvent read fOnMaskComplete write FOnMaskComplete;
    property OnValidateError: TNotifyEvent read FOnValidateError write FOnValidateError;
    property OnValidateEdit: TNotifyEvent read FOnValidateEdit write FOnValidateEdit;
  end;

  TAdvMaskEdit = class(TAdvCustomMaskEdit)
  published
    // TCustomMaskEdit properties
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DefaultHandling;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    {$IFDEF DELPHIXE_LVL}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    {$ENDIF}
    property Visible;

    // TAdvCustomMaskEdit properties
    property AutoFocus;
    property AutoTab;
    property CanUndo;
    property BorderColor;
    property DisabledBorder;
    property DisabledColor;
    property Flat;
    property FlatLineColor;
    property FlatParentColor;
    property ShowModified;
    property FocusBorderColor;
    property FocusColor;
    property FocusBorder;
    property FocusFontColor;
    property FocusLabel;
    property LabelCaption;
    property LabelAlwaysEnabled;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelFont;
    property ModifiedColor;
    property ReturnIsTab;
    property SelectFirstChar;
    property SoftBorder;
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}
    property Version;
    // events
    property OnAfterClipboardCopy;
    property OnAfterClipboardCut;
    property OnAfterClipboardPaste;
    property OnClipboardCopy;
    property OnClipboardCut;
    property OnClipboardPaste;
    property OnMaskComplete;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHIXE_LVL}
    property OnGesture;
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  PQueryParams = ^TQueryParams;
  TQueryParams = record
    Precision: Integer;
    Flat: Boolean;
    Lengthlimit: Integer;
    Prefix: string;
    Suffix: string;
  end;


  TEditDropTarget = class(TAEDropTarget)
  private
    FAdvEdit: TCustomAdvEdit;
  public
    constructor Create(aEdit: TCustomAdvEdit);
    procedure DropText(pt: tpoint; s: string); override;
    procedure DragMouseMove(pt: tpoint; var allow: Boolean); override;
  end;


const
  BorderRec: array[TBorderStyle] of Integer = (1, -1);

var
  ADVEDIT_DEFAULTHANDLING : boolean = True;

function AdvInputQuery(const QueryType: TAdvEditType; QueryParams: PQueryParams; const ACaption, APrompt: string;
  var Value: string): Boolean;

function IsStringType(s: string): TAutoType;

implementation

uses

  Shellapi, ActiveX, Consts, Inifiles, Registry, Clipbrd, Math, AdvStyleIF;

const
  Ctrl_Codes = [vk_back, vk_tab, vk_return];
  Numeric_Codes = [ord('0')..ord('9'), ord('-')];
  Money_Codes = Numeric_Codes;
  Float_Codes = Numeric_Codes + [ord(','), ord('.')];
  Range_Codes = Numeric_Codes + [ord(','), ord(';')];
  Hex_Codes = Numeric_Codes + [ord('A')..ord('F'), ord('a')..ord('f')];
  Bin_Codes = ['0', '1'];
  AlphaNum_Codes = [ord('0')..ord('9')] + [ord('a')..ord('z'), ord('A')..ord('Z')];


{$I DELPHIXE.INC}

function CalculateDPIScale(AControl: TControl): single;
var
  FHDC: HDC;
  FDPI: integer;
  {$IFDEF DELPHI10_LVL}
  frm: TCustomForm;
  {$ENDIF}

  function FontHeightAtDpi(iDPI, iFontSize: Integer): Integer;
  var
    FTmpCanvas: TCanvas;
    FHDC: HDC;
  begin
    FTmpCanvas := TCanvas.Create;
    try
      FHDC := GetDC(0);
      FTmpCanvas.Handle := FHDC;
      FTmpCanvas.Font.PixelsPerInch := iDPI; //must be set BEFORE size
      FTmpCanvas.Font.Size := iFontSize;
      Result := FTmpCanvas.TextHeight('0');
      ReleaseDC(0,FHDC)
    finally
      FTmpCanvas.Free;
    end;
  end;

begin
  Result := 1.0;

  {$IFDEF DELPHI10_LVL}
  frm := GetParentForm(AControl);
  if Assigned(frm) and (frm is TForm) and (frm as TForm).Scaled then
  begin
    FDPI := TForm(frm).Monitor.PixelsPerInch;
  end
  else
  {$ENDIF}
  begin
    FHDC := GetDC(0);
    try
      FDPI := GetDeviceCaps(FHDC, LOGPIXELSX);
    finally
      ReleaseDC(0, FHDC);
    end;
  end;

  try
    if FDPI <> 96 then
      Result := FontHeightAtDpi(FDPI, 9) / FontHeightAtDpi(96, 9);
  finally
  end;
end;


function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

function RangeListCompare(Item1, Item2: Pointer): Integer;
begin
  {$IFDEF DELPHIXE_LVL}
  if nativeuint(Item1) > nativeuint(Item2) then Result := 1 else
    if nativeuint(Item1) = nativeuint(Item2) then Result := 0 else Result := -1;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  if integer(Item1) > integer(Item2) then Result := 1 else
    if integer(Item1) = integer(Item2) then Result := 0 else Result := -1;
  {$ENDIF}
end;

function CheckTerminator(ch: char): boolean;
const
  Terminators = [' ', ',', '.', '/', '\', ':', '*', '$', '-', '(', ')', '[', ']', '"','''','&'];
begin
  {$IFNDEF DELPHI_UNICODE}
  Result := ch in Terminators;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Result := (ch = ' ') or (ch = ',') or (ch = '.') or (ch = '-') or (ch = '''') or
    (ch = '\') or (ch = '/') or (ch = ':') or (ch = '*') or (ch = '$') or (ch = ')') or (ch = ')') or (ch = '"');
  {$ENDIF}
end;

function ShiftCase(Name: string): string;

  {$IFNDEF DELPHI_UNICODE}
  function LowCase(C: char): char;
  begin
    if C in ['A'..'Z'] then
      LowCase := Chr(Ord(C) - Ord('A') + Ord('a'))
    else
      Lowcase := C;
  end;
  {$ENDIF}


var
  I, L: Integer;
  NewName: string;
  First: Boolean;
begin
  First := true;
  NewName := Name;
  L := Length(Name);
  for I := 1 to L do
  begin
    if CheckTerminator(NewName[I]) then
      First := true
    else
      if First then
      begin
        {$IFNDEF DELPHI_UNICODE}
        NewName[I] := Upcase(Name[I]);
        {$ENDIF}
        {$IFDEF DELPHI_UNICODE}
        {$IFDEF DELPHIXE4_LVL}
        NewName[I] := Name[I].ToUpper;
        {$ENDIF}
        {$IFNDEF DELPHIXE4_LVL}
        NewName[I] := character.ToUpper(Name[I]);
        {$ENDIF}
        {$ENDIF}
        First := false;
      end
      else
      begin
        {$IFNDEF DELPHI_UNICODE}
        NewName[I] := Lowcase(Name[I]);
        {$ENDIF}
        {$IFDEF DELPHI_UNICODE}
        {$IFDEF DELPHIXE4_LVL}
        NewName[I] := Name[I].ToLower;
        {$ENDIF}
        {$IFNDEF DELPHIXE4_LVL}
        NewName[I] := character.ToLower(Name[I]);
        {$ENDIF}
        {$ENDIF}
      end;

    if (Copy(NewName, 1, I) = 'Mc') or (Copy(NewName, 1, I) = 'Mac') or
      ((Pos(' Mc', NewName) = I - 2) and (I > 2)) or
      ((I > L - 3) and ((Copy(NewName, I - 1, 2) = ' I') or
      (Copy(NewName, I - 2, 3) = ' II'))) then
      First := true;
  end;
  ShiftCase := NewName;
end;


function IsType(s: string): TAutoType;
var
  i: Integer;
  isI, isF, isH: Boolean;
  th, de, mi: Integer;

begin
  Result := atString;

  isI := true;
  isF := true;
  isH := true;

  if s = '' then
  begin
    isI := false;
    isF := false;
    isH := false;
  end;

  th := -1; de := 0; mi := 0;

  for i := 1 to Length(s) do
  begin
    if not (ord(s[i]) in Numeric_Codes) then isI := false;
    if not ((ord(s[i]) in Numeric_Codes) or (s[i] = ThousandSeparator) or (s[i] = DecimalSeparator)) then isF := false;
    if not (ord(s[i]) in Hex_Codes) then isH := false;

    if (s[i] = ThousandSeparator) and (i - th < 3) then isF := false;

    if s[i] = ThousandSeparator then th := i;
    if s[i] = DecimalSeparator then inc(de);
    if s[i] = '-' then inc(mi);
  end;

  if isH and not isI then
    Result := atHex;

  if isI then
    Result := atNumeric
  else
  begin
    if isF then
      Result := atFloat;
  end;

  if (mi > 1) or (de > 1) then
    Result := atString;
end;

function IsStringType(s: string): TAutoType;
begin
  Result := IsType(s);
end;

function StripThousandSep(s: string): string;
begin
  while (Pos(ThousandSeparator, s) > 0) do
    Delete(s, Pos(ThousandSeparator, s), 1);
  Result := s;
end;

function TCustomAdvEdit.EStrToFloat(s: string): extended;
begin
  Result := 0;

  if Pos(ThousandSeparator, s) > 0 then
    s := StripThousandSep(s);

  if (FPrecision > 0) and (Length(s) > FPrecision) then
    if s[Length(s) - FPrecision] = Thousandseparator then
      s[Length(s) - FPrecision] := Decimalseparator;
  try
    TryStrToFloat(s, Result);
  except
    Result := 0;
  end;
end;

class function TCustomAdvEdit.HexToInt(s: string): Integer;
var
  i: Integer;
  r, m: Integer;

  function CharVal(c: char): Integer;
  begin
    Result := 0;
    if ((c >= '0') and (c <= '9')) then Result := ord(c) - ord('0');
    if ((c >= 'A') and (c <= 'F')) then Result := ord(c) - ord('A') + 10;
    if ((c >= 'a') and (c <= 'f')) then Result := ord(c) - ord('a') + 10;
  end;

begin
  r := 0;
  m := 1;
  for i := Length(s) downto 1 do
  begin
    r := r + m * CharVal(s[i]);
    m := m shl 4;
  end;
  Result := r;
end;

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

function TCustomAdvEdit.CharFromPos(pt: TPoint): Integer;
begin
  Result := Loword(SendMessage(self.Handle, EM_CHARFROMPOS, 0, makelparam(pt.x, pt.y)));
end;

procedure TCustomAdvEdit.Clear;
begin
  Text := '';
end;

function TCustomAdvEdit.PosFromChar(uChar: word): TPoint;
var
  pt: tpoint;
  l: Integer;
  DC: HDC;
  s: string;
  sz: TSize;
  FLastChar: boolean;
begin
  s := inherited Text;

  if (uChar > Length(s)) then
    uChar := Length(s);

  FLastChar := (uChar >= Length(s));

  if FLastChar and (uChar = 0) then
  begin
    // empty text
    Result := Point(0, 0);
    Exit;
  end;

  if FLastChar then
    Dec(uChar);

  l := SendMessage(Handle, EM_POSFROMCHAR, uChar, 0);

  pt := Point(SmallInt(loword(l)), SmallInt(hiword(l)));
  if FLastChar then
  begin
    s := 'w' + Copy(s, Length(s), 1);
    DC := GetDC(Handle);
    try
      GetTextExtentPoint32(DC, PChar(s), 2, sz);
      pt.x := pt.x + sz.cx;
      GetTextExtentPoint32(DC, PChar(s), 1, sz);
      pt.x := pt.x - sz.cx;
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
  Result := pt;
end;

function TCustomAdvEdit.GetTextSize: Integer;
var
  DC: HDC;
  sz: TSize;
  holdFont: THandle;
begin
  DC := GetDC(Handle);
  holdFont := SelectObject(DC, Font.Handle);
  GetTextExtentPoint32(DC, pchar(Text), Length(Text), sz);
  result := sz.CX;
  SelectObject(DC, holdFont);
  ReleaseDC(Handle, DC);
end;


procedure TCustomAdvEdit.InvalidateCaret(pt: TPoint);
var
  r: TRect;
begin
  r := rect(pt.x, pt.y, pt.x + 1, pt.y - Font.Height);
  InvalidateRect(self.Handle, @r, true);
end;

procedure TCustomAdvEdit.EraseCaret;
var
  pt: TPoint;
begin
  pt := FCaretPos;
  FCaretPos := Point(-1, -1);
  InvalidateCaret(pt);
end;

procedure TCustomAdvEdit.DrawCaretByCursor;
var
  nChar: Integer;
  pt, ptCursor: TPoint;
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);

  nChar := CharFromPos(ptCursor);

  pt := PosFromChar(nChar);

  if (fCaretPos.x <> pt.x) or (fCaretPos.y <> pt.y) then
  begin
    InvalidateCaret(fCaretPos);
    InvalidateCaret(pt);
    FCaretPos := pt;
  end;
end;

procedure TCustomAdvEdit.SetCaretByCursor;
var
  ptCursor: TPoint;
  nChar: integer;
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);
  nChar := loword(SendMessage(self.Handle, EM_CHARFROMPOS, 0, makelong(ptCursor.x, ptCursor.y)));
  SelStart := nChar;
  SelLength := 0;
end;

function CheckSeparator(ch: char): boolean;
begin
  {$IFNDEF DELPHI_UNICODE}
  Result := ch in ['-', ',', ';'];
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  Result := (ch = '-') or (ch = ',') or (ch = ';');
  {$ENDIF}
end;

procedure TCustomAdvEdit.WMChar(var Msg: TWMKey);
var
  oldSelStart, oldprec: Integer;
  s: string;
  key: char;
  isCtrl,ismod: Boolean;
  cf: TCustomForm;
  pfl: integer;

  function ScanPrecision(s: string; inspos: Integer): Boolean;
  var
    mdist: Integer;
  begin
    Result := false;
    inspos := inspos - Length(FPrefix);
    if FPrecision <= 0 then Exit;

    if (Length(s) - inspos > FPrecision) then
    begin
      Result := false;
      exit;
    end;

    if (Pos(decimalseparator, s) > 0) then
    begin
      mdist := Length(s) - Pos(decimalseparator, s);
      if (inspos >= Pos(decimalseparator, s)) and (mdist >= FPrecision) then
        Result := true;
    end;
  end;

  function ScanDistance(s: string; inspos: Integer): Boolean;
  var
    mdist: Integer;
  begin
    Result := false;
    inspos := inspos - Length(FPrefix);
    mdist := Length(s);

    if (Pos(thousandseparator, s) = 0) then
    begin
      Result := false;
      Exit;
    end;

    while (Pos(thousandseparator, s) > 0) do
    begin
      if abs(Pos(thousandseparator, s) - inspos) < mdist then mdist := abs(Pos(thousandseparator, s) - inspos);
      if (abs(Pos(thousandseparator, s) - inspos) < 3) then
      begin
        Result := true;
        break;
      end;

      if inspos > Pos(thousandseparator, s) then inspos := inspos - Pos(thousandseparator, s);
      delete(s, 1, Pos(thousandseparator, s));
    end;

    if (mdist > 3) then
    begin
      Result := true;
    end;
  end;

begin
  if not EditorEnabled then
  begin
    Msg.Result := 0;
    Exit;
  end;

  IsCtrl := GetKeyState(VK_CONTROL) and $8000 = $8000;

  if (SelLength = Length(Text)) and (Text <> '') and (EditType <> etPassword) and (Msg.CharCode <> VK_BACK) then
  begin
    FBlockChange := true;
  end;

  if (Msg.CharCode = VK_RETURN) and IsCtrl then
  begin
    Msg.CharCode := 0;
    Msg.Result := 1;
    Exit;
  end;

  pfl := Length(FPrefix);

  if (Msg.CharCode = VK_RETURN) then
  begin
    key := #13;
    if Assigned(OnKeyPress) then
      OnKeyPress(Self, key);

    Msg.CharCode := 0;

    if not DefaultHandling and not ReturnIsTab then
    begin
      if (Parent is TWinControl) then
      begin
        PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_RETURN, 0);

        cf := GetParentForm(self);

        if Assigned(cf) then
          if cf.KeyPreview then
          begin
            inherited;
            Exit;
          end;

        PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_RETURN, 0);
      end;
    end;
    Exit;
  end;

  if (Msg.CharCode = VK_ESCAPE) then
  begin
    if not DefaultHandling then
    begin
      if (Parent is TWinControl) then
      begin

        cf := GetParentForm(self);

        if Assigned(cf) then
          if cf.KeyPreview then
          begin
            inherited;
            Exit;
          end;

        PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_ESCAPE, 0);
        PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_ESCAPE, 0);
      end;
    end;
  end;

  if (Msg.Charcode = VK_ESCAPE) then
  begin
    inherited;
    Exit;
  end;

  // handle Ctrl-A
  if IsCtrl and (Msg.CharCode = 1) then
  begin
    SelectAll;
    Msg.CharCode := 0;
    Exit;
  end;

  // handle Ctrl-C, Ctrl-X, Ctrl-V
  if IsCtrl and (Msg.CharCode in [1, 3, 22, 24]) then
  begin
    inherited;
    Exit;
  end;

  if (msg.charcode = ord('.')) and (FExcelStyleDecimalSeparator) and (msg.keydata and $400000 = $400000) then
  begin
    msg.charcode := ord(decimalseparator);
  end;

  if (msg.charcode = ord(',')) and (FExcelStyleDecimalSeparator) and (msg.keydata and $400000 = $400000) then
  begin
    msg.charcode := ord(decimalseparator);
  end;

  if (msg.CharCode = vk_back) and (FPrefix <> '') then
    if (SelStart <= pfl) and (SelLength = 0) then Exit;

  if (FLengthLimit > 0) and (FixedLength(self.Text) > FLengthLimit) and
    (SelLength = 0) and (SelStart < DecimalPos)
  and (msg.charcode <> vk_back) and (msg.charcode <> ord(decimalseparator)) and not AllowMin(chr(msg.CharCode)) then Exit;

  if (msg.charcode = VK_BACK) then
  begin
    s := self.Text;

    if SelLength = 0 then
      delete(s, SelStart - pfl, 1)
    else
      delete(s, Max(1, SelStart - pfl), SelLength);

    ismod := (s <> Text) or Modified;
    Modified := ismod;

    inherited;

    if (EditType = etMoney) then
      AutoSeparators;

    if (Text = '') and ((s <> '') or ismod) then
      Change;

    Modified := ismod;

    //if (lengthlimit > 0) and (fixedLength(s) - 1 > lengthlimit) then
    UpdateLookup;
    Exit;
  end;

  if IsCtrl and (Msg.CharCode = 10) then
    Exit;

  if (EditType in [etMoney, etNumeric, etFloat]) and not FSigned and (msg.charcode = ord('-')) then
  begin
    Exit;
  end;

  case EditType of
    etString, etPassword:
      begin
        SetModified(true);
        inherited;
      end;
    etAlphaNumeric:
      begin
        if msg.charcode in AlphaNum_Codes + Ctrl_Codes then
        begin
          SetModified(true);
          inherited;
        end;
      end;
    etValidChars:
      begin
        if (pos(chr(msg.CharCode), ValidChars) > 0) or (msg.CharCode = 8) then
        begin
          SetModified(true);
          inherited;
        end;
      end;
    etInvalidChars:
      begin
        if (pos(chr(msg.CharCode), InvalidChars) = 0) or (msg.CharCode = 8) then
        begin
          SetModified(true);
          inherited;
        end;
      end;
    etNumeric:
      begin
        if (msg.CharCode = ord('-')) then
        begin
          if (SelLength = Length(self.Text)) then
          begin
            inherited Text := Prefix + '-' + Suffix;
            SelStart := 1;
            FBlockChange := false;
            Exit;
          end;

          s := self.Text;
          oldSelStart := SelStart;
      // oldSelLength := SelLength;
          if (Pos('-', s) > 0) then
          begin
            delete(s, 1, 1);
            inherited Text := Prefix + s + Suffix;
            if (oldSelStart > 0) and (oldSelStart > pfl) then
              SelStart := oldSelStart - 1
            else
              SelStart := pfl;
            SelLength := 0;
          end
          else
          begin
            inherited Text := Prefix + '-' + self.Text + Suffix;
            SelLength := 0;
            SelStart := oldSelStart + 1;
            SetModified(true);
          end;
      // SelLength := oldSelLength;
        end
        else
        begin
          if (msg.charcode in Numeric_Codes + Ctrl_Codes) then
            inherited;

          if ((GetKeyState(vk_rcontrol) and $8000 = $8000) or
            (GetKeyState(vk_lcontrol) and $8000 = $8000)) then
            inherited;
        end;
      end;
    etHex: if msg.charcode in Hex_Codes + Ctrl_Codes then
      begin
        SetModified(true);      
        inherited;
      end;
    etRange:
      begin
        if msg.charcode in Range_Codes + Ctrl_Codes then
        begin
          SetModified(true);        
          s := (inherited Text) + ' ';
          if (msg.charcode in [ord('-'), ord(','), ord(';')]) then
          begin
            if (SelStart <= pfl) then Exit;
            if (SelStart > pfl) and (CheckSeparator(s[SelStart])) then
              Exit;
            if (SelStart > pfl) and (CheckSeparator(s[SelStart + 1])) then
              Exit;
            inherited;
          end
          else
            inherited;
        end;
      end;
    etMoney:
      begin
        if (chr(msg.charcode) = decimalseparator) and ((Pos(decimalseparator, self.Text) > 0) or (FPrecision = 0)) then
        begin
          if (FPrecision > 0) then
          begin
            if SelLength = Length(Text) then
              Text := '0,0';
            SelectAfterDecimal;
          end;
          FBlockChange := false;
          Exit;
        end;

        if (msg.charcode in Money_Codes + Ctrl_Codes) or (chr(msg.charcode) = decimalseparator) then
        begin
          if (chr(msg.charcode) = thousandseparator) or (chr(msg.charcode) = decimalseparator) then
          begin
            if scandistance(self.Text, SelStart) then
              Exit;
          end;

          if scanprecision(self.Text, SelStart) and (msg.charcode in [$30..$39, ord(decimalseparator)]) and (SelLength = 0) then
            begin
              if (FPrecision > 0) and (SelStart - pfl >= Pos(decimalseparator, self.text))
                and (msg.charcode in [$30..$39]) and (SelStart - pfl < Length(self.text)) then
              begin
                SelLength := 1;
              end
              else
                Exit;
            end;

          if (SelStart = 0) and (self.Text = '') and (msg.charcode = ord(decimalseparator)) then
            begin
              inherited Text := '0' + decimalseparator;
              SelStart := 2;
              SetModified(true);
              Exit;
            end;

          if (msg.charcode = ord('-')) and (SelLength = 0) then
          begin
            s := self.Text;
            oldprec := FPrecision;
            FPrecision := 0;
            oldSelStart := SelStart;

            if (Pos('-', s) <> 0) then
            begin
              delete(s, 1, 1);
              inherited Text := s + Suffix;
              SetModified(true);
              if (oldSelStart > 0) and (oldSelStart > pfl) then
                SelStart := oldSelStart - 1
              else
                SelStart := Length(Prefix);
            end
            else
            begin
              if (floatvalue <> 0) or (1 > 0) then
              begin
                inherited Text := '-' + self.Text + Suffix;
                SelLength := 0;
                SelStart := oldSelStart + 1;
                SetModified(true);
              end;
            end;
            FPrecision := oldprec;
            Exit;
          end;

          inherited;

          if (self.Text <> '') and (self.Text <> '-') and
            (chr(msg.charcode) <> decimalseparator) then
          begin
            if inherited Modified then
              SetModified(true);
            AutoSeparators;
          end;
        end;
      end;
    etFloat:
      begin
        if (msg.charcode = ord(',')) and (DecimalSeparator <> ',') {and (ThousandSeparator <> ',')} then
          Exit;
        if (msg.charcode = ord('.')) and (DecimalSeparator <> '.') {and (ThousandSeparator <> '.')} then
          Exit;

        if (msg.charcode in Float_Codes + Ctrl_Codes) or (Msg.CharCode = Ord(ThousandSeparator)) or (Msg.CharCode = Ord(DecimalSeparator)) then
        begin
          if (chr(msg.charcode) = decimalseparator) and
            (Pos(decimalseparator, self.getseltext) = 0) and
            ((Pos(decimalseparator, self.Text) > 0) or (FPrecision = 0)) then
          begin
            if (FPrecision > 0) then SelectAfterDecimal;
            Exit;
          end;

          if ((msg.charcode = ord(',')) and (Pos(',', self.Text) > 0) and (Pos(',', self.getSelText) = 0)) and
            (chr(msg.charcode) <> thousandseparator) then exit;

          if (chr(msg.charcode) = thousandseparator) or (chr(msg.charcode) = decimalseparator) then
          begin
            if scandistance(self.Text, SelStart) then exit;
          end;

          if ScanPrecision(self.text, SelStart) and (msg.charcode in [$30..$39, ord(decimalseparator)]) and (SelLength = 0) then
            begin
              if (FPrecision > 0) and (SelStart - pfl >= Pos(decimalseparator, self.Text))
                and (msg.CharCode in [$30..$39]) and (SelStart - pfl < Length(self.Text)) then
              begin
                SelLength := 1;
              end
              else
                Exit;
            end;

          if (SelStart = 0) and (self.Text = '') and (msg.charcode = ord(decimalseparator)) then

            begin
              inherited Text := '0' + decimalseparator;
              SelStart := 2;
              SetModified(true);
              Exit;
            end;

          if (msg.charcode = ord('-')) and (SelLength = 0) then
          begin
            s := self.Text;
            oldprec := FPrecision;
            FPrecision := 0;
            oldSelStart := SelStart;

            if (Pos('-', s) <> 0) then
            begin
              delete(s, 1, 1);
              inherited Text := s + Suffix;
              if (oldSelStart > 0) and (oldSelStart > pfl) then
                SelStart := oldSelStart - 1
              else
                SelStart := pfl;
              SetModified(true);
            end
            else
            begin
              if (floatvalue <> 0) or (1 > 0) then
              begin
                inherited Text := '-' + self.text + Suffix;
                SelLength := 0;
                SelStart := oldSelStart + 1;
                SetModified(true);
              end;
            end;
            FPrecision := oldprec;
            Exit;
          end;
          inherited;
        end;
      end;
    etUppercase:
      begin
        s := AnsiUpperCase(chr(msg.charcode));
        msg.charcode := ord(s[1]);
        inherited;
      end;
    etLowercase:
      begin
        s := AnsiLowerCase(chr(msg.charcode));
        msg.charcode := ord(s[1]);
        inherited;
      end;
    etMixedCase:
      begin
        oldSelStart := SelStart;

        inherited;

        inherited Text := ShiftCase(self.Text);
        SelStart := oldSelStart + 1;
      end;
  end;

  if (FTabOnFullLength) then
  begin
    if (FLengthlimit > 0) and (FixedLength(Text) > FLengthLimit) and
      (SelLength = 0) and (SelStart = FLengthlimit) then
      begin
        cf := GetParentForm(self);
        if Assigned(cf) then
          cf.Perform(WM_NEXTDLGCTL, 0, 0);
      end;
  end;

  if inherited Modified then
    SetModified(true);

  UpdateLookup;
end;

procedure TCustomAdvEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(params);

  if not (FEditType = etPassword) then
  begin
    Params.Style := Params.Style or ES_MULTILINE;
  end;

  if not IsVista then
  begin
    case FEditAlign of
      eaRight:
        begin
          Params.Style := Params.Style and not (ES_LEFT) and not (ES_CENTER);
          Params.Style := Params.Style or (ES_RIGHT);
        end;
      eaCenter:
        begin
          Params.Style := Params.Style and not (ES_LEFT) and not (ES_RIGHT);
          Params.Style := Params.Style or (ES_CENTER);
        end;
    end;
  end;
end;

procedure TCustomAdvEdit.CMFontChanged(var Message: TMessage);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    if (FLabel <> nil) and ParentFont and not ShowError then
    begin
      FLabel.Font.Assign(Font);
    end;

  inherited;
  SetFlatRect(FFlat);
end;

procedure TCustomAdvEdit.CMCancelMode(var Message: TMessage);
begin
  inherited;
  if Assigned(FLookupList) and FLookupList.Visible then
    FLookupList.Hide;
end;

procedure TCustomAdvEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if IsError and ShowError and Enabled then
  begin
    ApplyErrorColor;
  end;
end;

procedure TCustomAdvEdit.SetFlatRect(const Value: Boolean);
var
  loc: TRect;
  lft: integer;
  cf: TCustomForm;
  isctl3D: boolean;
begin
  if HandleAllocated then
  begin
    lft := 0;

    cf := GetParentForm(Self);

    isctl3D := Ctl3D;

    if Assigned(cf) and (cf is TForm) then
    begin
      isctl3D := isctl3D and (cf as TForm).Ctl3D;
    end;

    if not isCtl3D and (BorderStyle = bsNone) then
      lft := Font.Size div 3;

    if Value then
    begin
      loc.Left := lft + 4 + IndentL;
      loc.Top := 3;
      loc.Right := Clientrect.Right - 4 - IndentR - lft;
      loc.Bottom := Clientrect.Bottom - 4;
    end
    else
    begin
      loc.Left := lft + IndentL;
      loc.Top := 0;
      loc.Right := ClientRect.Right - IndentR - lft;
      loc.Bottom := ClientRect.Bottom;
    end;

    if not isCtl3D then
    begin
      loc.Left := Loc.Left + 3;
      loc.Right := loc.Right - 3;
    end;

    SendMessage(Handle, EM_SETRECTNP, 0, LParam(@loc));
  end;
end;

procedure TCustomAdvEdit.SetFlat(const value: boolean);
var
  OldColor: TColor;

begin
  if (csLoading in ComponentState) then
  begin
    FFlat := Value;
    Exit;
  end;

  if FFlat <> Value then
  begin
    FFlat := Value;
    if FFlat then
    begin
      OldColor := Color;
      if FFlatParentColor then
      begin
        Color := (Parent as TWinControl).Brush.Color;
        FocusColor := Color;
      end
      else
      begin
        Color := FNormalColor;
        FocusColor := FFocusColor;
      end;

      FNormalColor := OldColor;
      BorderStyle := bsNone;
      SetFlatRect(True);
    end
    else
    begin
      Color := FNormalColor;
      FocusColor := FFocusColor;
      BorderStyle := FOldBorder;
      SetFlatRect(False);
    end;
    Invalidate;
  end;
end;

procedure TCustomAdvEdit.CNCtlColorEdit(var Message: TWMCtlColorEdit);
begin
  inherited;
  if FTransparent then
    SetBkMode(Message.ChildDC, Windows.TRANSPARENT);
end;

procedure TCustomAdvEdit.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
  inherited;
  if FTransparent then
    SetBkMode(Message.ChildDC, Windows.TRANSPARENT);
end;


procedure TCustomAdvEdit.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvEdit.SetFlatLineColor(const Value: TColor);
begin
  FFlatLineColor := Value;
  Invalidate;
end;

procedure TCustomAdvEdit.SetFlatParentColor(const Value: boolean);
begin
  FFlatParentColor := Value;
  Invalidate;
end;

procedure TCustomAdvEdit.SetEditText(AValue: string);
begin
  OrigText := AValue;
  Text := AValue;
end;

procedure TCustomAdvEdit.SetEditType(value: TAdvEditType);
var
  at: TAutoType;
begin
  if FEditType <> Value then
  begin
    FEditType := Value;
    if FEditType = etPassword then
    begin
      PassWordChar := '*';
      FCanUndo := False;
    // FEditAlign := eaLeft;
      ReCreateWnd;
    end
    else
      Passwordchar := #0;

    if (Text <> '') or (not AllowNumericNullValue) then
    begin
      at := IsType(self.Text);
      case FEditType of
        etHex: if not (at in [atNumeric, atHex]) then self.IntValue := 0;
        etNumeric: if (at <> atNumeric) then self.IntValue := 0;
        etFloat, etMoney: if not (at in [atFloat, atNumeric]) then self.FloatValue := 0.0;
      end;
    end;

    if FDesignTime and (FEditType = etFloat) and (Precision = 0) then
      Precision := 2;
  end;
end;

procedure TCustomAdvEdit.SetEditAlign(const Value: TEditAlign);
begin
  if FEditAlign <> Value then
  begin
    FEditAlign := Value;
    ReCreateWnd;
    {force a proper re-alignment}
    Width := Width + 1;
    Width := Width - 1;
  end;
end;

procedure TCustomAdvEdit.SetCanUndo(const Value: boolean);
begin
  if FCanUndo <> Value then
  begin
    FCanUndo := Value;
  //CanUndo is not compatible with etPassword style
    if FCanUndo and (FEditType = etPassWord) then
      FCanUndo := False;
    ReCreateWnd;
  {force a proper re-alignment}
    self.Width := self.Width + 1;
    self.Width := self.Width - 1;
  end;
end;

procedure TCustomAdvEdit.WMActivate(var Message: TMessage);
begin
  inherited;
end;

procedure TCustomAdvEdit.WMKillFocus(var Msg: TWMKillFocus);
var
  IsValid: Boolean;
  OldModified: Boolean;
  TT: string;
  i: integer;
begin
  if (csLoading in ComponentState) then
    Exit;

  if Assigned(FLookupList) and FLookupList.Visible and not (msg.FocusedWnd = FLookupList.Handle) then
    FLookupList.Hide;

  if not IsError then
  begin
    if (Color <> FNormalColor) then
    begin
      Color := FNormalColor;
      if FIsWinXP then
      begin
        Width := Width - 1;
        Width := Width + 1;
      end;
    end;
  end;

  if not ((IsError and ShowError) or (ShowModified)) then
  begin
    if not (TestURL and ShowURL) then
      Font.Color := FFontColor;
  end;

  if FFocusLabel and (FLabel <> nil) then
  begin
    FLabel.Font.Style := FLabel.Font.Style - [fsBold];
    UpdateLabelPos;
  end;

  if (FPrecision > 0) and (EditType in [etFloat, etMoney]) then
  begin
    if (self.Text <> '') or not FAllowNumericNullValue then
    begin
      // update for precision
      OldModified := Modified;
      Floatvalue := self.Floatvalue;
      Modified := OldModified;
    end;
  end;

  if (EditType in [etNumeric]) and (Self.Text = '') and not FAllowNumericNullValue then
    Text := '0';

  if (EditType in [etFloat, etMoney]) and (Self.Text = '') and not FAllowNumericNullValue  then
    Floatvalue := 0.0;

  if (EditType = etNumeric) and ((FMinValue <> 0) or (FMaxValue <> 0)) then
  begin
    if AllowNumericNullValue and (Text = '') then
      i := 0
    else
    begin
      try
        i := StrToInt(Text);
      except
        i := 0;
      end;
    end;

    if i < MinValue then
      Text := IntToStr(MinValue)
    else
    if i > MaxValue then
      Text := IntToStr(MaxValue);
  end;

  IsValid := DoValidate(Text);

  if not IsValid then
  begin
    Msg.Result := 0;
  end;

  inherited;

  if FFocusBorder then
  begin
    Width := Width - 1;
    Width := Width + 1;
  end;

  if FAlignChanging then
    Exit;

  if FFocusWidthInc > 0 then
    Width := Width - FFocusWidthInc;


  if (FEditAlign <> FOldEditAlign) and (FFocusAlign <> eaDefault) then
  begin
    EditAlign := FOldEditAlign;
  end;

  if (EmptyText <> '') and (Text = '') then
    Invalidate;

  TT := Trim(Text);

  if FLookup.Enabled and FLookup.History and (TT <> '') then
  begin
    if FLookup.DisplayList.IndexOf(TT) = -1 then
      FLookup.DisplayList.Add(TT);
  end;

  if (FFocusBorderColor <> clNone) or (FBorderColor <> clNone) then
    SendMessage(self.Handle, WM_NCPAINT, 0,0);

  if (ErrorMarkerLen > 0) then
    Invalidate;
end;

procedure TCustomAdvEdit.SelectAll;
var
  pfl: integer;
begin
  SelStart := 0;
  SelLength := Length(self.text);

  pfl := Length(FPrefix);

  if pfl > 0 then
  begin
    if (SelStart < pfl) then
    begin
      SelStart := pfl;
      SelLength := Length(self.Text);
    end;
  end;

  if (fSuffix <> '') then
  begin
    SelStart := pfl;
    SelLength := Length(self.Text);
  end;

  if (Pos('://', self.Text) > 0) and FShowURL then
  begin
    SelStart := Pos('://', self.text) + 2;
    SelLength := Length(self.text);
  end;

  if (Pos('mailto:', self.text) > 0) and FShowURL then
  begin
    SelStart := Pos('mailto:', self.text) + 6;
    SelLength := Length(self.text);
  end;
end;

procedure TCustomAdvEdit.SelectBeforeDecimal;
var
  i: Integer;
begin
  i := Pos(decimalseparator, self.text);
  if (i > 0) then
    SelStart := i + Length(FPrefix) - 1
  else
    SelStart := Length(FPrefix);
end;

procedure TCustomAdvEdit.SelectAfterDecimal;
var
  i: Integer;
begin
  i := Pos(decimalseparator, self.Text);

  if (i > 0) then
    SelStart := i + Length(FPrefix)
  else
    SelStart := Length(FPrefix);
end;


procedure TCustomAdvEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  if csLoading in ComponentState then
    Exit;

  inherited;

  FOldString := Text;

  if (FFocusColor <> clNone) and DoValidate(Text) then
  begin
    inherited Color := FFocusColor;
    if FIsWinXP then
    begin
      Width := Width - 1;
      Width := Width + 1;
    end;
  end;

  if not ((IsError and ShowError) or (ShowModified)) then
  begin
    if (Font.Color <> FFontColor) then
      FFontColor := Font.Color;

    if not (TestURL and ShowURL) and (FFocusFontColor <> clNone) then
      Font.Color := FFocusFontColor;
  end;

  if AutoSelect then
    SelectAll;

  if FFocusLabel and (FLabel <> nil) then
  begin
    FLabel.Font.Style := FLabel.Font.Style + [fsBold];
    UpdateLabelPos;
  end;

  if (FFocusWidthInc > 0) and not FAlignChanging then
    Width := Width + FFocusWidthInc;

  if not FAlignChanging then
    FOldEditAlign := FEditAlign;

  if (FEditAlign <> FFocusAlign) and (FFocusAlign <> eaDefault) then
  begin
    FAlignChanging := True;
    EditAlign := FFocusAlign;
    FAlignChanging := False;
  end;

  if ((EmptyText <> '') and (Text = '')) or (FocusBorder) or (FFocusBorderColor <> clNone) then
  begin
    if BorderStyle = bsNone then
      SetFlatRect(true);
    Invalidate;
  end;

end;

procedure TCustomAdvEdit.WMSysKeyDown(var Msg: TMessage);
begin
  if Msg.WParam = VK_MENU then
  begin
    CloseLookup;
  end;

  inherited;
end;

constructor TCustomAdvEdit.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;
  i: integer;

begin
  inherited Create(aOwner);
  FFocusColor := clNone;
  FReturnAsTabKey := VK_TAB;
  FFocusFontColor := clWindowText;
  FNormalColor := clWindow;
  FFocusAlign := eaDefault;
  FFontColor := self.Font.Color;
  FBorderColor := clNone;
  FModifiedColor := clHighLight;
  FErrorColor := clRed;
  FErrorFontColor := clWhite;
  FError := False;
  FLabel := nil;
  FLabelMargin := 4;
  FURLColor := clBlue;
  FDisabledColor := clSilver;
  FDisabledTextColor := clNone;
  FDisabledBorder := true;
  FPersistence := TPersistence.Create;
  FFlatParentColor := True;
  FFlatLineColor := clBlack;
  FCaretPos := point(-1, -1);
  FButtonDown := false;
  FMouseInControl := false;
  FCanUndo := True;
  FLabelFont := TFont.Create;
  FLabelFont.OnChange := LabelFontChange;
  FAutoThousandSeparator := True;
  FDefaultHandling := ADVEDIT_DEFAULTHANDLING;
  FOldBorder := bsSingle;
  FFocusBorderColor := clNone;
  FEditorEnabled := true;
  FIndentL := 0;
  FIndentR := 0;
  FEmptyTextFocused := False;
  FLblUpdate := False;
  FAutoValidate := True;

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) or
    ((verinfo.dwMajorVersion = 5) and (verinfo.dwMinorVersion >= 1));

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);
  FLookupList := nil;

  if not (csDesigning in ComponentState) then
  begin
    FLookupList := TListHintWindow.Create(Self);
    FLookupList.Visible := False;
    FLookupList.Width := 0;
    FLookupList.Height := 0;
    //FLookupList.Parent := Self;
    FLookupList.BorderWidth := 1;
    FLookupList.ControlStyle := FLookupList.ControlStyle + [csReplicatable];
    FLookupListbox := TListBoxTab.Create(FLookupList);

    with FLookupListBox do
    begin
      Parent := FLookupList;
      Align := alClient;
      Style := lbOwnerDrawFixed;
      ItemHeight := LOOKUPITEMHEIGHT;
      Ctl3D := false;
      TabStop := true;
      BorderStyle := bsNone;
      TabOrder := 0;
      OnKeyPress := ListKeyPress;
      OnMouseUp := ListMouseUp;
      ControlStyle := Controlstyle + [csReplicatable];
    end;

    FLookupList.ListControl := FLookupListBox;
  end;

  FLookup := TLookupSettings.Create;

  FParentFnt := false;
end;

destructor TCustomAdvEdit.Destroy;
begin
  if (FLabel <> nil) then
  begin
    FLabel.Parent := nil;
    FLabel.Free;
    FLabel := nil;
  end;
  
  FLabelFont.Free;
  FPersistence.Free;
  FPersistence := nil;
  FLookup.Free;

  inherited Destroy;
end;

procedure TCustomAdvEdit.SetParent(AParent: TWinControl);
begin
  inherited;

  if not (csDestroying in ComponentState) then
  begin
    if FLabel <> nil then
      FLabel.Parent := AParent;
  end;
end;

procedure TCustomAdvEdit.ApplyURL(const Value: Boolean);
begin
  if Value then
  begin
    FParentFnt := false;  
    Font.Style := Font.Style + [fsUnderline];
    Font.Color := FURLColor;
    FIsUrl := True;
    Cursor := crHandPoint;
    Invalidate;
  end
  else
  begin
    Font.Style := Font.Style - [fsUnderline];
    Font.Color := FFontColor;
    FIsUrl := False;
    Cursor := crDefault;
  end;
end;

procedure TCustomAdvEdit.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = EN_CHANGE) then
  begin
    if FBlockChange then
    begin
      FBlockChange := false;
      Exit;
    end;
  end;

  if (Message.NotifyCode = EN_CHANGE) then
    if FTransparent then
    begin
      Invalidate;
    end;

  if (Message.NotifyCode = EN_CHANGE) and (FShowURL) then
  begin
    if TestURL and not FIsUrl then
    begin
      ApplyURL(True);
    end
    else
      if FIsUrl and not TestURL then
      begin
        ApplyURL(False);
      end;
  end;

  inherited;
end;


procedure TCustomAdvEdit.WMKeyDown(var Msg: TWMKeydown);
var
  selp: Integer;
  s: string;
  isCtrl,isShift: Boolean;
  ss: TShiftState;
begin
  FGotReturn := false;

  if not EditorEnabled then
  begin
    Msg.Result := 1;
    Msg.CharCode := 0;
    Exit;
  end;

  ss := [];

  IsCtrl := GetKeyState(VK_CONTROL) and $8000 = $8000;

  IsShift := GetKeyState(VK_SHIFT) and $8000 = $8000;

  if IsCtrl then
    ss := ss+ [ssCtrl];

  if IsShift then
    ss := ss + [ssShift];

  if Assigned(FLookupList) and not FLookupList.Visible then
  begin
    if (msg.CharCode = VK_UP) or (msg.CharCode = VK_DOWN)  then
    begin
      DoKeyDown(Msg);
      Msg.Result := 1;
      Exit;
    end;
  end;

  if (msg.CharCode = VK_RETURN) then
    FGotReturn := true;

  if (msg.charcode = VK_RETURN) and FReturnIsTab and not IsCtrl then
  begin
    msg.CharCode := VK_TAB;
    PostMessage(Handle, WM_KEYDOWN, VK_TAB, 0);
  end;

  if FGotReturn and Assigned(FLookupList) and FLookupList.Visible then
  begin
    DoneLookup;
    Exit;
  end;

  if (msg.CharCode in [VK_TAB, VK_RETURN]) and IsCtrl then
  begin
    inherited;
    Exit;
  end;

  if (msg.CharCode = VK_HOME) and Assigned(FLookupList) and FLookupList.Visible then
  begin
    FLookupListBox.ItemIndex := 0;
    Msg.Result := 1;
    Exit;
  end;

  if (msg.CharCode = VK_END) and Assigned(FLookupList) and FLookupList.Visible then
  begin
    FLookupListBox.ItemIndex := FLookupListBox.Items.Count - 1;
    Msg.Result := 1;
    Exit;
  end;

  if (msg.CharCode = VK_NEXT) and Assigned(FLookupList) and FLookupList.Visible then
  begin
    if FLookupListBox.ItemIndex + FLookup.DisplayCount < FLookupListBox.Items.Count then
      FLookupListBox.ItemIndex := FLookupListBox.ItemIndex + FLookup.DisplayCount
    else
      FLookupListBox.ItemIndex := FLookupListBox.Items.Count - 1;
    Msg.Result := 1;
    Exit;
  end;

  if (msg.CharCode = VK_PRIOR) and Assigned(FLookupList) and FLookupList.Visible then
  begin
    if FLookupListBox.ItemIndex > FLookup.DisplayCount then
      FLookupListBox.ItemIndex := FLookupListBox.ItemIndex - FLookup.DisplayCount
    else
      FLookupListBox.ItemIndex := 0;
    Msg.Result := 1;
    Exit;
  end;

  if (msg.charcode = VK_RIGHT) and (FSuffix <> '') then
  begin
    selp := hiword(SendMessage(Handle, EM_GETSEL, 0, 0));
    if selp >= Length(Text) then
    begin
      msg.CharCode := 0;
      msg.Result := 0;
      Exit;
    end;
  end;

  if (msg.charcode = VK_DELETE) and (FSuffix <> '') then
  begin
    selp := hiword(SendMessage(Handle, EM_GETSEL, 0, 0));
    if (selp >= Length(Text)) and (SelLength = 0) then
    begin
      if Assigned(OnKeyDown) then
        OnKeyDown(Self, msg.CharCode, ss);

      msg.CharCode := 0;
      msg.Result := 0;

      Exit;
    end;
    SetModified(true);
  end;

  if (msg.charcode = VK_LEFT) and (FPrefix <> '') then
  begin
    selp := hiword(SendMessage(Handle, EM_GETSEL, 0, 0));

    if selp <= Length(FPrefix) then
    begin
      msg.CharCode := 0;
      msg.Result := 0;
      Exit;
    end;
  end;

  if (msg.charcode = VK_END) and (FSuffix <> '') then
  begin
    if (GetKeyState(VK_SHIFT) and $8000 = 0) then
    begin
      SelStart := Length(Text);
      SelLength := 0;
    end
    else
      SelLength := Length(Text) - SelStart;
    msg.CharCode := 0;
    msg.Result := 0;
    Exit;
  end;

  if (msg.charcode = VK_HOME) and (FPrefix <> '') then
  begin
    if (getkeystate(VK_SHIFT) and $8000 = 0) then
    begin
      SelStart := Length(fPrefix);
      SelLength := 0;
    end
    else
    begin
      SendMessage(Handle, EM_SETSEL, Length(FPrefix) + Length(Text), Length(FPrefix));
    end;
    msg.Charcode := 0;
    msg.Result := 0;
    Exit;
  end;

  if (msg.charcode = VK_BACK) and (FPrefix <> '') then
  begin
    if (SelStart <= Length(FPrefix) + 1) then
    begin
      if Assigned(OnKeyDown) then
        OnKeyDown(Self, msg.CharCode,[]);
      msg.CharCode := 0;
      msg.Result := 0;
      Exit;
    end;
    SetModified(true);
  end;

  if (msg.CharCode = VK_DELETE) and (SelStart >= Length(FPrefix)) then
  begin
    s := Text;
    if SelLength = 0 then
      Delete(s, SelStart - Length(FPrefix) + 1, 1)
    else
      Delete(s, SelStart - Length(FPrefix) + 1, SelLength);

    if (lengthlimit > 0) and (FixedLength(s) - 1 > LengthLimit) then
    begin
      if Assigned(OnKeyDown) then
        OnKeyDown(Self, msg.CharCode, ss);

      msg.CharCode := 0;
      msg.Result := 0;
      Exit;
    end;
    SetModified(true);
  end;

  if (msg.CharCode = VK_UP) and Assigned(FLookupList) and FLookupList.Visible then
  begin
    if FLookupListBox.ItemIndex > 0 then
      FLookupListBox.ItemIndex := FLookupListBox.ItemIndex - 1;
    msg.CharCode := 0;
  end;

  if (msg.CharCode = VK_DOWN) and Assigned(FLookupList) and FLookupList.Visible then
  begin
    if FLookupListBox.ItemIndex + 1 < FLookupListBox.Items.Count then
      FLookupListBox.ItemIndex := FLookupListBox.ItemIndex + 1;
    msg.CharCode := 0;
  end;

  inherited;

  if (msg.CharCode = VK_DELETE) and (EditType = etMoney) then
    AutoSeparators;

  if (FPrefix <> '') and (SelStart < Length(FPrefix)) then
    SelStart := Length(FPrefix);
end;

procedure TCustomAdvEdit.WMKeyUp(var Msg: TWMKeyUp);
//var
//  cf: TCustomForm;
begin
  inherited;

//  if (msg.charcode = VK_RETURN) and FReturnIsTab and FGotReturn then
//  begin
//    msg.CharCode := VK_TAB;
//    PostMessage(Handle, WM_KEYDOWN, VK_TAB, 0);

//    if IsWindowVisible(Handle) then
//    begin
//      cf := GetParentForm(self);
//
//      if Assigned(cf) then
//        cf.Perform(WM_NEXTDLGCTL, 0, 0);
//    end;
//  end;

  FGotReturn := false;
end;

procedure TCustomAdvEdit.WMCopy(var Message: TWMCopy);
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnClipboardCopy) and not FBlockCopy then
    FOnClipboardCopy(self, copy(self.Text, SelStart + 1 - Length(fPrefix), SelLength), allow);
  FBlockCopy := False;
  if Allow then
  begin
    inherited;
    if Assigned(OnAfterClipboardCopy) then
      OnAfterClipboardCopy(Self);
  end;
end;

procedure TCustomAdvEdit.WMCut(var Message: TWMCut);
var
  Allow: Boolean;
begin
  Allow := True;
  FBlockCopy := True;
  if Assigned(FOnClipboardCut) then
  begin
    FOnClipboardCut(self, copy(self.text, SelStart + 1 - Length(fPrefix), SelLength), allow);
  end;

  if Allow then
  begin
    inherited;
    if Assigned(OnAfterClipboardCut) then
      OnAfterClipboardCut(Self);
  end;

  FBlockCopy := False;

  Change;
end;


procedure TCustomAdvEdit.WMPaste(var Msg: TMessage);
var
{$IFNDEF DELPHI_UNICODE}
  Data: THandle;
  content: PChar;
{$ENDIF}
  newstr, cliptxt, valstr: string;
  newss, newsl, i: Integer;
  allow: Boolean;

  function InsertString(s: string): string;
  var
    ss: Integer;
  begin
    Result := self.text;
    ss := SelStart - Length(fPrefix);
    if (SelLength = 0) then
    begin
      insert(s, result, ss + 1);
      newsl := 0;
      newss := ss + Length(s) + Length(fPrefix);
    end
    else
    begin
      delete(result, ss + 1, SelLength);
      insert(s, result, ss + 1);
      newsl := Length(s);
      newss := ss + Length(fPrefix);
    end;
  end;

begin
  if ReadOnly then
    Exit;

  {$IFDEF DELPHI_UNICODE}
  if ClipBoard.HasFormat(CF_UNICODETEXT) then
  begin
    //
  end;

  if ClipBoard.HasFormat(CF_TEXT) then
  begin
    Allow := True;
    cliptxt := Clipboard.AsText;
    if pos(#13,cliptxt) > 0 then
      cliptxt := copy(cliptxt,1,pos(#13,cliptxt) - 1);
    newstr := InsertString(cliptxt);
  {$ENDIF}

  if (LengthLimit > 0) and (Length(newstr) > LengthLimit) then
    newstr := copy(newstr, 1, LengthLimit);



{$IFNDEF DELPHI_UNICODE}

  if ClipBoard.HasFormat(CF_TEXT) then
  begin
    ClipBoard.Open;
    Data := GetClipBoardData(CF_TEXT);
    try
      if Data <> 0 then
        Content := PChar(GlobalLock(Data))
      else
        Content := nil
    finally
      if Data <> 0 then
        GlobalUnlock(Data);
      ClipBoard.Close;
    end;

    if Content = nil then
      Exit;

    Allow := True;

    cliptxt := StrPas(Content);
    if pos(#13,cliptxt) > 0 then
      cliptxt := copy(cliptxt,1,pos(#13,cliptxt) - 1);
    newstr := InsertString(cliptxt);

{$ENDIF}

    if Assigned(FOnClipboardPaste) then
      FOnClipboardPaste(self, newstr, Allow);

    if Assigned(FOnClipboardCanPaste) then
      FOnClipboardCanPaste(self, newstr, Allow);

    if not Allow then Exit;

    if MaxLength > 0 then
    begin
      newstr := Copy(newstr,1,MaxLength);
//{$IFNDEF DELPHI_UNICODE}
//      if Length(StrPas(Content)) + Length(Self.Text) - SelLength > MaxLength then
//{$ENDIF}

//{$IFDEF DELPHI_UNICODE}
//    if Length(Clipboard.AsText) + Length(Self.Text) - SelLength > MaxLength then
//{$ENDIF}
//        Exit;
    end;

    if FEditType in [etString, etPassWord, etLowerCase, etUpperCase, etMixedCase] then
      SetModified(true);

    case FEditType of
      etAlphaNumeric:
        begin
          Allow := True;
          for i := 1 to length(newstr) do
            if not (ord(newstr[i]) in AlphaNum_Codes) then
              Allow := False;

          if Allow then
          begin
            SetModified(True);
            Self.Text := newstr;
          end;
        end;
      etNumeric:
        begin
          if IsType(newstr) = atNumeric then
          begin
            if not (not Signed and (pos('-', newstr) > 0)) then
            begin
              SetModified(True);
              self.Text := newstr;
            end;
          end;
        end;
      etFloat, etMoney:
        begin
          newstr := Trim(newstr);
          if IsType(newstr) in [atFloat, atNumeric] then
          begin
{$IFNDEF DELPHI_UNICODE}
            if not ((FPrecision = 0) and (Pos(DecimalSeparator, StrPas(Content)) > 0)) then
{$ENDIF}
{$IFDEF DELPHI_UNICODE}
            if not ((FPrecision = 0) and (Pos(DecimalSeparator, Clipboard.AsText) > 0)) then
{$ENDIF}
              begin
                if not (not Signed and (pos('-', newstr) > 0)) then
                begin
                  SetModified(True);
                  self.Text := newstr;
                  Floatvalue := Floatvalue;
                end;
              end;
            end;
        end;
      etHex:
        begin
          if (IsType(NewStr) in [atHex,atNumeric]) then
            self.Text := NewStr;
        end;
      etString, etPassWord: self.Text := NewStr;
      etLowerCase: self.Text := AnsiLowerCase(NewStr);
      etUpperCase: self.Text := AnsiUpperCase(NewStr);
      etMixedCase: self.Text := ShiftCase(NewStr);
      etValidChars:
        begin
          Allow := true;
          valstr := '';
          for i := 1 to length(newstr) do
          begin
            if pos(newstr[i], ValidChars) <> 0 then
              valstr := valstr + newstr[i];
          end;
          if valstr <> '' then
          begin
            SetModified(True);
            self.Text := valstr;
          end;
        end;
      etInvalidChars:
        begin
          valstr := '';
          for i := 1 to length(newstr) do
          begin
            if pos(newstr[i], InvalidChars) = 0 then
              valstr := valstr + newstr[i];
          end;
          if valstr <> '' then
          begin
            SetModified(True);
            self.Text := valstr;
          end;
        end;

    end;

    if (FEditType = etMoney) and (Length(self.Text) > 3) then
      SelectAll
    else
    begin
      SelStart := newss;
      SelLength := newsl;
    end;
  end;

  UpdateLookup;

  if Assigned(OnAfterClipboardPaste) then
    OnAfterClipboardPaste(Self);

  if TabOnFullLength then
  begin
    if (length(self.Text) = LengthLimit) and (LengthLimit > 0) then
    begin
      Windows.SetFocus(GetNextDlgTabItem(Parent.Handle,self.Handle,true));
    end;
  end;
end;

procedure TCustomAdvEdit.WMPaint(var Msg: TWMPaint);
var
  Canvas: TCanvas;
  ps: TPaintStruct;
  CallEndPaint: Boolean;
  r: TRect;
  dstyle: DWORD;
begin
  if Enabled or (DisabledTextColor = clNone) then
  begin
    inherited;
    PaintEdit;
  end
  else
  begin
    CallEndPaint := False;
    Canvas := TCanvas.Create;
    try
      if msg.DC <> 0 then
      begin
        Canvas.Handle := msg.DC;
        ps.fErase := true;
      end
      else
      begin
        BeginPaint(Handle, ps);
        CallEndPaint:= True;
        Canvas.Handle := ps.hdc;
      end;
      if ps.fErase then
        Perform(WM_ERASEBKGND, Canvas.Handle, 0);

      SaveDC(Canvas.Handle);
      try
        Canvas.Brush.Style := bsClear;
        Canvas.Font := Font;
        Canvas.Font.Color := DisabledTextColor;

        if DoubleBuffered and Assigned(Parent) and (Parent.DoubleBuffered) then
        begin
          Canvas.Brush.Color := Color;
          r := GetClientRect;
          Canvas.Rectangle(r);
        end;

        SendMessage(Handle, EM_GETRECT, 0, LParam(@r));

        dstyle := DT_SINGLELINE or DT_EDITCONTROL or DT_VCENTER;

        case EditAlign of
        eaRight:
          begin
            dstyle := dstyle or DT_RIGHT;
            DrawText(Canvas.Handle,PChar(Text),Length(Text),r,dstyle);
          end;
        eaCenter:
          begin
            dstyle := dstyle or DT_CENTER;
            DrawText(Canvas.Handle,PChar(Text),Length(Text),r,dstyle);
          end;
        else
          Canvas.TextOut(1, 1, Text);
        end;

      finally
        RestoreDC(Canvas.Handle, - 1);
      end;
    finally
      if CallEndPaint then
        EndPaint(handle, ps);
      Canvas.Free
    end;
  end;

  if Border3D or (FFocusBorderColor <> clNone) or (FBorderColor <> clNone) or (not Enabled and DisabledBorder) then
    DrawBorder;
end;

procedure TCustomAdvEdit.PaintEdit;
var
  DC: HDC;
  Oldpen: HPen;
  Loc,R: TRect;
  Canvas: TCanvas;
  {$IFDEF DELPHIXE2_LVL}
  LStyle: TCustomStyleServices;
  {$ENDIF}
  x: integer;
begin
  if FFlat then
  begin
    DC := GetDC(Handle);
    if FFocusBorder and (GetFocus = Handle) then
    begin
      DrawControlBorder(DC);
    end
    else
    begin
      if FFlatLineColor <> clNone then
      begin
        if Enabled then
          OldPen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(FFlatLineColor)))
        else
          OldPen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(clGray)));
        SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
        if FSoftBorder then
        begin
          MovetoEx(DC, Loc.Left - 4 + IndentL, Height - 1, nil);
          LineTo(DC, Width - 1, Height - 1);
          LineTo(DC, Width - 1, Loc.Top - 3);
          LineTo(DC, Loc.Left - 4 + IndentL, Loc.Top - 3);
          LineTo(DC, Loc.Left - 4 + IndentL, Height - 1);
        end
        else
        begin
          MovetoEx(DC, Loc.Left - 2 + IndentL, Height - 1, nil);
          LineTo(DC, Width - IndentR, Height - 1);
        end;

        DeleteObject(SelectObject(DC, OldPen));
      end;
    end;

    ReleaseDC(Handle, DC);
  end;

  if (FCaretPos.x <> -1) and (FCaretPos.y <> -1) then
  begin
    DC := GetDC(Handle);
    Rectangle(DC, FCaretPos.x, FCaretPos.y, FCaretPos.x + 1, FCaretPos.y - Font.Height);
    ReleaseDC(Handle, DC);
  end;

  if (Text = '') and ( (FEmptyTextFocused) or (GetFocus <> Handle) ) and (FEmptyText <> '') then
  begin
    DC := GetDC(Handle);
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      if IsVista then
      begin
        Canvas.Brush.Color := Color;
      end
      else
        SetBkMode(Canvas.Handle, Windows.TRANSPARENT);

     {$IFDEF DELPHIXE2_LVL}
     LStyle := StyleServices;
     if LStyle.Enabled and (LStyle.Name <> 'Windows') then
       Canvas.Brush.Style := bsClear;
     {$ENDIF}

      Canvas.Font.Assign(Font);
      Canvas.Font.Style := EmptyTextStyle;
      Canvas.Font.Color := clGray;
      if not Ctl3D then
        x := 6
      else
        x := 3;

      R := Rect(x,0, ClientWidth - x, ClientHeight);

      if BiDiMode = bdRightToLeft then
        DrawText(Canvas.Handle, PChar(FEmptyText), Length(FEmptyText), R, DT_VCENTER or DT_SINGLELINE or DT_RIGHT)
      else
        Canvas.TextOut(x + 1, 2, FEmptyText);
    finally
      Canvas.Free;
      ReleaseDC(Handle, DC);
    end;
  end;

  if (GetFocus <> Handle) and (ErrorMarkerLen > 0) then
  begin
    DC := GetDC(Handle);
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      DrawErrorLines(Canvas, ErrorMarkerPos, ErrorMarkerLen);
    finally
      Canvas.Free;
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TCustomAdvEdit.DrawErrorLines(Canvas: TCanvas; ErrPos, ErrLen: Integer);
var
  pt1: TPoint;
  pt2: TPoint;
  l: Integer;
  o: Integer;
  ep: Integer;
  Rect: TRect;
  h: Integer;
begin
  Rect := GetClientRect;
  if ErrPos >= Length(Text) then
  begin
    ep := Length(Text);
    l := SendMessage(Handle, EM_POSFROMCHAR, ep, 0);
    pt1 := Point(LoWord(l), HiWord(l));
    pt1.X := pt1.X + 4;
  end
  else
  begin
    l := SendMessage(Handle, EM_POSFROMCHAR, ErrPos, 0);
    pt1 := Point(LoWord(l), HiWord(l));
  end;

  if ErrPos + ErrLen >= Length(Text) then
  begin
    ep := Length(Text) - 1;
    l := SendMessage(Handle, EM_POSFROMCHAR, ep, 0);
    pt2 := Point(LoWord(l), HiWord(l));
    pt2.X := pt2.X + 4;
    pt2.Y := pt1.Y;
  end
  else
  begin
    l := SendMessage(Handle, EM_POSFROMCHAR, ErrPos + ErrLen - 1, 0);
    pt2 := Point(LoWord(l), HiWord(l));
  end;

  Canvas.Pen.Color := clRed;
  Canvas.Pen.Width := 2;

  Canvas.Font.Assign(Font);
  h := Canvas.TextHeight('gh') - 1;

  l := pt1.X;
  o := 3;

  Canvas.MoveTo(Rect.Left + l, Rect.Top + pt1.Y + h + o);

  while l <= pt2.X do
  begin
    if o = 3 then o := 0 else o := 3;
    Canvas.LineTo(Rect.Left + l + 3, pt2.Y + h + o);
    Inc(l, 3);
  end;

  if o = 3 then o := 0 else o := 3;
  Canvas.LineTo(Rect.Left + l + 3, Rect.Top + pt2.Y + h + o);
end;


procedure TCustomAdvEdit.WMEraseBkGnd(var Message: TWMEraseBkGnd);
var
  DC: HDC;
  i: Integer;
  p: TPoint;
begin
  if FTransparent then
  begin
    if Assigned(Parent) then
    begin
      DC := Message.DC;
      if DC <> 0 then
      begin
        i := SaveDC(DC);
        p := ClientOrigin;
        Windows.ScreenToClient(Parent.Handle, p);
        p.x := -p.x;
        p.y := -p.y;
        MoveWindowOrg(DC, p.x, p.y);
        SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
        SendMessage(Parent.Handle, WM_PAINT, DC, 0);
        if (Parent is TWinCtrl) then
          (Parent as TWinCtrl).PaintCtrls(DC, nil);
        RestoreDC(DC, i);
      end;
    end;
  end
  else
    inherited;
end;

procedure TCustomAdvEdit.WMMouseMove(var Msg: TWMMouse);
var
  m: pchar;
  s: string;
  dwEffects: Integer;
  isCopy: Boolean;
  hres: HResult;

begin
  inherited;
  if (SelLength > 0) and (FButtonDown) and (FOleDropSource) then
  begin
    GetMem(m, SelLength + 1);
    GetSelTextBuf(m, SelLength + 1);
    s := StrPas(m);
    FreeMem(m);

    FIsDragSource := true;
    hres := StartTextDoDragDrop(s, '', DROPEFFECT_COPY or DROPEFFECT_MOVE, dwEffects);
    FIsDragSource := false;

    isCopy := (GetKeyState(VK_CONTROL) and $8000 = $8000);

    if not isCopy and (hres = DRAGDROP_S_DROP) then
    begin
    {cut the text here}
      ClearSelection;
      EraseCaret;
      Invalidate;
    end;

    FButtonDown := False;
  end;
end;

procedure TCustomAdvEdit.WMLButtonDown(var Msg: TWMMouse);
var
  uchar: Integer;
begin
  // click outside selection
  uchar := CharFromPos(point(msg.xpos, msg.ypos));

  if (SelLength <= 0) or (uchar < SelStart) or (uChar > SelStart + SelLength) or
    (GetFocus <> self.Handle) then
    inherited
  else
    if (uChar >= SelStart) and (uChar <= SelStart + SelLength) and (SelLength > 0) then
      FButtonDown := True;
end;

procedure TCustomAdvEdit.WMLButtonUp(var Msg: TWMMouse);
var
  uchar: Integer;
  show: Boolean;
begin
  if FButtonDown then
  begin
    uchar := CharFromPos(point(msg.xpos, msg.ypos));
    SelStart := uChar;
    SelLength := 0;
    if Assigned(OnClick) then
      OnClick(Self);
  end;

  FButtonDown := false;

  inherited;

  if FIsUrl and (self.Handle = GetFocus) and FShowURL then
  begin
    show := True;
    if Assigned(FOnURLClick) then
      FOnURLClick(self, self.Text, show);

    if show then
      ShellExecute(0, 'open', pchar(self.Text), nil, nil, SW_NORMAL);
    inherited;
    Exit;
  end;


  if (fPrefix <> '') then
  begin
    if (SelStart < Length(fPrefix)) then
    begin
      SelStart := Length(fPrefix);
      SelLength := Length(self.Text);
    end;
  end;
  if (fSuffix <> '') then
  begin
    if (SelStart > Length(self.text)) then
    begin
      if (fPrefix <> '') then
        SelStart := Length(fPrefix) + Length(self.Text)
      else
        SelStart := Length(self.Text);
      SelLength := 0;
    end;
    if (SelStart + SelLength > Length(self.text)) then
    begin
      if (fPrefix <> '') then
        SelLength := Length(self.Text)// - SelStart;
      else
        SelLength := Length(self.Text) - SelStart;
    end;
  end;
end;

procedure TCustomAdvEdit.SetPrefix(const Value: string);
var
  s: string;
begin
  s := self.Text;
  fPrefix := Value;
  inherited Text := s;
//changed for v1.8
  Text := s;
end;

procedure TCustomAdvEdit.SetSuffix(const Value: string);
var
  s: string;
begin
  s := self.text;
  fSuffix := Value;
  inherited Text := s;
//changed for v1.8
  Text := s;
end;

function TCustomAdvEdit.DecimalPos: Integer;
var
  i: Integer;
begin
  i := Pos(decimalseparator, self.text);
  if (i = 0) then Result := Length(fprefix) + Length(self.text) + Length(fSuffix) + 1
  else Result := Length(fPrefix) + i;
end;

function TCustomAdvEdit.AllowMin(ch: char): boolean;
begin
  Result := Signed and (EditType in [etFloat,etNumeric, etMoney]) and (ch = '-');
end;

function TCustomAdvEdit.FixedLength(s: string): Integer;
var
  i: Integer;
begin
  s := StripThousandSep(s);
  i := Pos(decimalseparator, s);
  if (i > 0) then Result := i else Result := Length(s) + 1;

  if Signed and (EditType in [etFloat,etNumeric, etMoney]) and (pos('-',s) > 0) then
    Result := Result - 1;
end;

function TCustomAdvEdit.GetText: string;
var
  s: string;
begin
  s := inherited Text;
  if (fPrefix <> '') and (Pos(Uppercase(fPrefix), Uppercase(s)) = 1) then delete(s, 1, Length(fPrefix));
  if (fSuffix <> '') then delete(s, Length(s) - Length(fSuffix) + 1, Length(fSuffix));
  Result := s;
end;

procedure TCustomAdvEdit.SetText(Value: string);
var
  fmt, neg: string;
  f: extended;
  ChgEvt: TNotifyEvent;
begin
  if not ((Value = '') and AllowNumericNullValue) then
  begin
     if (EditType in [etFloat, etMoney]) then
       Value := Trim(Value);

    case FEditType of
      etFloat: if not (IsType(Value) in [atFloat, atNumeric]) then Value := '0';
      etMoney: if not (IsType(Value) in [atFloat, atNumeric]) then Value := '0';
      etHex: if not (IsType(Value) in [atHex, atNumeric]) then Value := '0';
      etNumeric: if not (IsType(Value) in [atNumeric]) then Value := '0';
    end;

    if (PrecisionDisplay = pdNormal) then
    begin
      if ((FPrecision > 0) or (FEditType = etMoney)) and (value <> '') then
      begin
        if (FEditType in [etMoney]) and (FPrecision >= 0) then
        begin
          if (Pos('-', value) > 0) then neg := '-' else neg := '';
          fmt := '%.' + IntToStr(FPrecision) + 'n';
          Value := Format(fmt, [EStrToFloat(Value)]);
        end;

        if (FEditType in [etFloat]) then
        begin
          fmt := '%.' + inttostr(FPrecision) + 'f';
          f := EStrToFloat(value);
          Value := Format(fmt, [f]);
        end;
      end;
    end
    else
    begin
      if (FEditType in [etFloat, etMoney]) then
      begin
        f := EStrToFloat(Value);
        Value := Format('%g', [f]);
      end;
    end;
  end;

  if (FEditType in [etHex]) then
    Value := AnsiUpperCase(Value);

  ChgEvt := OnChange;

//  if not HandleAllocated then
//    OnChange := nil;

  if (csLoading in ComponentState) then
    OnChange := nil;

  inherited Text := FPrefix + Value + FSuffix;

  OnChange := ChgEvt;

  SetModified(False);

  if FShowURL then ApplyURL(TestURL);
end;

procedure TCustomAdvEdit.SetTextDirect(s: string);
begin
  inherited Text := s;
end;

function TCustomAdvEdit.GetVisible: boolean;
begin
  Result := inherited Visible;
end;

procedure TCustomAdvEdit.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if (FLabel <> nil) then
    FLabel.Visible := Visible;
end;

procedure TCustomAdvEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TCustomAdvEdit.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FLabel) then
    FLabel.Visible := Visible;
end;

procedure TCustomAdvEdit.SetVisible(const Value: boolean);
begin
  inherited Visible := Value;
  if Assigned(FLabel) then
    FLabel.Visible := Value;
end;


function TCustomAdvEdit.CreateLabel: TCustomAdvEditLabel;
begin
  Result := TCustomAdvEditLabel.Create(self);
  Result.Parent := Parent;
  Result.FocusControl := self;
  Result.Font.Assign(LabelFont);
  Result.OnClick := LabelClick;
  Result.OnDblClick := LabelDblClick;
  Result.ParentFont := ParentFont;
end;

function TCustomAdvEdit.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.Caption
  else
    Result := '';
end;

function TCustomAdvEdit.GetLookupVisible: boolean;
begin
  Result := false;
  if Assigned(FLookupList) then
    Result := FLookupList.Visible;
end;

procedure TCustomAdvEdit.AutoSeparators;
var
  s, si, neg: string;
  d: Double;
  Diffl, OldSelStart, OldPrec: Integer;

begin
  s := self.Text;
  Diffl := Length(s);
  OldSelStart := 0;

  if HandleAllocated then
    OldSelStart := SelStart;

  if (s = '') then
    Exit;

  if (Pos('-', s) = 1) then
  begin
    Delete(s, 1, 1);
    neg := '-';
  end
  else
    neg := '';

  if (Pos(DecimalSeparator, s) > 0) then
    s := Copy(s, Pos(DecimalSeparator, s), 255)
  else
    s := '';

  d := Trunc(Abs(self.FloatValue));

  if FAutoThousandSeparator then
    si := Format('%n', [d])
  else
    si := Format('%f', [d]);

  si := Copy(si, 1, Pos(decimalseparator, si) - 1);

  OldPrec := FPrecision;
  FPrecision := 0;

  FBlockChange := (Text <> FPrefix + neg + si + s + FSuffix);

  inherited Text := FPrefix + neg + si + s + fSuffix;

  FBlockChange := false;

  FPrecision := OldPrec;

  Diffl := Length(self.Text) - Diffl;

  if HandleAllocated then
  begin
    SelStart := OldSelStart + Diffl;
    SelLength := 0;
  end;
end;
                  
procedure TCustomAdvEdit.UpdateLabel;
begin
  if Assigned(FLabel.Parent) then
  begin
    FLabel.Transparent := FLabeltransparent;

    if not FParentFnt or ShowError then
    begin
      FLabel.Font.Assign(FLabelFont);
    end
    else
      FLabel.Font.Assign(Font);

    if FocusLabel then
    begin
      if Focused then
        FLabel.Font.Style := FLabel.Font.Style + [fsBold]
      else
        FLabel.Font.Style := FLabel.Font.Style - [fsBold];
    end;

    if FLabel.Parent.HandleAllocated then
      UpdateLabelPos;
  end;
end;

procedure TCustomAdvEdit.UpdateLabelPos;
var
  tw,brdr,lblmargin: Integer;
  r: TRect;
begin
  r := Rect(0,0,1000,255);
  DrawText(FLabel.Canvas.Handle, PChar(FLabel.Caption), Length(FLabel.Caption), r, DT_HIDEPREFIX or DT_CALCRECT);
  tw := r.Right;

  brdr := 0;
  if BorderStyle = bsSingle then
    brdr := 2;

  lblmargin := Trunc(FLabelMargin *  CalculateDPIScale(Self));

  case FLabelPosition of
    lpLeftTop:
      begin
        FLabel.Top := self.Top;
        FLabel.Left := self.Left - tw - lblmargin;
      end;
    lpLeftCenter:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := Top + ((Height - brdr - FLabel.Height) div 2)
        else
          FLabel.Top := Top - ((FLabel.Height - Height + brdr) div 2);

        FLabel.Left := Left - tw - lblmargin;
      end;
    lpLeftBottom:
      begin
        FLabel.Top := self.Top + self.Height - FLabel.Height;
        FLabel.Left := self.Left - tw - lblmargin;
      end;
    lpTopLeft:
      begin
        FLabel.Top := self.Top - FLabel.Height - lblmargin;
        FLabel.Left := self.Left;
      end;
    lpTopRight:
      begin
        FLabel.Top := self.Top - FLabel.Height - lblmargin;
        FLabel.Left := self.Left + self.Width - FLabel.Width;
      end;
    lpTopCenter:
      begin
        FLabel.Top := self.Top - FLabel.height - lblmargin;
        if self.Width - FLabel.Width > 0 then
          FLabeL.Left := self.Left + ((self.Width - FLabel.Width) div 2)
        else
          FLabeL.Left := self.Left - ((FLabel.Width - self.Width) div 2)
      end;
    lpBottomLeft:
      begin
        FLabel.top := self.top + self.height + lblmargin;
        FLabel.left := self.left;
      end;
    lpBottomCenter:
      begin
        FLabel.top := self.top + self.height + lblmargin;
        if self.Width - FLabel.Width > 0 then
          FLabeL.Left := self.Left + ((self.Width - FLabel.width) div 2)
        else
          FLabeL.Left := self.Left - ((FLabel.Width - self.width) div 2)
      end;
    lpBottomRight:
      begin
        FLabel.top := self.top + self.height + lblmargin;
        FLabel.Left := self.Left + self.Width - FLabel.Width;
      end;
    lpLeftTopLeft:
      begin
        FLabel.top := self.top;
        FLabel.left := self.left - lblmargin;
      end;
    lpLeftCenterLeft:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := self.top + ((Height - brdr - FLabel.height) div 2)
        else
          FLabel.Top := self.Top - ((FLabel.Height - Height + brdr) div 2);
        FLabel.left := self.left - lblmargin;
      end;
    lpLeftBottomLeft:
      begin
        FLabel.top := self.top + self.height - FLabel.height;
        FLabel.left := self.left - lblmargin;
      end;
    lpRightTop:
      begin
        FLabel.Top := self.Top;
        FLabel.Left := self.Left + Self.Width + lblmargin;
      end;
    lpRightCenter:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := Top + ((Height - brdr - FLabel.Height) div 2)
        else
          FLabel.Top := Top - ((FLabel.Height - Height + brdr) div 2);

        FLabel.Left := self.Left + Self.Width + lblmargin;
      end;
    lpRighBottom:
      begin
        FLabel.Top := self.Top + self.Height - FLabel.Height;
        FLabel.Left := self.Left + Self.Width + lblmargin;
      end;
  end;

  FLabel.Visible := Visible;
end;

procedure TCustomAdvEdit.SetLabelPosition(const value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TCustomAdvEdit.SetLabelMargin(const value: integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TCustomAdvEdit.SetLabelFont(const Value: TFont);
begin
  if not ParentFont then
    FLabelFont.Assign(Value);

  if FLabel <> nil then
    UpdateLabel;
end;

procedure TCustomAdvEdit.LabelFontChange(Sender: TObject);
begin
  if Assigned(FLabel) then
    UpdateLabel;

  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    ParentFont := false;

  if not FLblUpdate then
    FLblFntHeight := LabelFont.Height;
end;

procedure TCustomAdvEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  lblmargin: integer;
begin
  if Assigned(FLabel) then
  begin
    lblmargin := Trunc(FLabelMargin *  CalculateDPIScale(Self));

    case LabelPosition of
      lpLeftTop, lpLeftCenter, lpLeftBottom:
        begin
          if (Align in [alTop, alClient, alBottom]) then
          begin
            AWidth := AWidth - (FLabel.Width + lblmargin);
            ALeft := ALeft + (FLabel.Width + lblmargin);
          end;
        end;
      lpRightTop, lpRightCenter, lpRighBottom:
        begin
          if (Align in [alTop, alClient, alBottom]) then
            AWidth := AWidth - (FLabel.Width + lblmargin);
        end;
      lpTopLeft, lpTopCenter, lpTopRight:
        begin
          if (Align in [alTop, alClient, alRight, alLeft]) then
            ATop := ATop + FLabel.Height;
        end;
    end;
  end;

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if (FLabel <> nil) then
  begin
    if (FLabel.Parent <> nil) then
      UpdateLabel;
  end;

  SetFlatRect(FFlat);

  if FFlat then
    Flat := FFlat;
end;

procedure TCustomAdvEdit.SetLabelCaption(const value: string);
begin
  if (FLabel = nil) and (Value <> '') then
  begin
    FLabel := CreateLabel;
    FLabel.Caption := Value;
    UpdateLabel;
  end;

  if Assigned(FLabel) and (Value <> FLabel.Caption) then
  begin
    FLabel.Caption := Value;
    UpdateLabel;
  end;
end;

procedure TCustomAdvEdit.SetLabelTransparent(const value: boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TCustomAdvEdit.SetLookupVisible(const Value: boolean);
begin
  if not Lookup.Enabled then
  begin
    Lookup.Enabled := true;
    Lookup.NumChars := 0;
  end;
  SetFocus;
  UpdateLookup;
end;

procedure TCustomAdvEdit.CMMouseEnter(var Msg: TMessage);
var
  pf: TCustomForm;
begin
  inherited;

  pf := GetParentForm(self);

  if FAutoFocus and not (csDesigning in ComponentState) then
  begin
    if Assigned(pf) then
    begin
      if (GetActiveWindow = pf.Handle) then
        SetFocus;
    end
    else
      SetFocus;
  end;

    
  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    if FFocusBorder then
      DrawBorder;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TCustomAdvEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    if FFocusBorder then Invalidate;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

function TCustomAdvEdit.GetFloat: double;
var
  s: string;
//  d: double;
//  e: integer;
begin
  Result := 0;
  case FEditType of
    etHex: if self.Text <> '' then Result := HexToInt(self.Text);
    etString:
      begin
        //val(self.Text, d, e);
        Result := EStrToFloat(self.Text);
      end;
    etNumeric, etFloat:
      if (self.Text <> '') then
      begin
        s := self.Text;
        if (s = '-') then
          Result := 0
        else
          Result := EStrToFloat(s);
      end;  
    etMoney:
      if self.Text <> '' then
      begin
        s := StripThousandSep(self.Text);
        if (Pos(Decimalseparator, s) = Length(s)) then Delete(s, Pos(decimalseparator, s), 1);
        if (s = '') or (s = '-') then Result := 0 else
          Result := EStrToFloat(s);
      end;
  end;
end;

function ValStr(s: string): Integer;
var
  err: Integer;
begin
  val(s, result, err);
end;

function TCustomAdvEdit.GetInt: integer;
begin
  Result := 0;
  case FEditType of
    etHex: if (self.Text <> '') then Result := HexToInt(self.Text);
    etNumeric, etFloat: Result := ValStr(self.Text);
    etMoney: Result := ValStr(StripThousandSep(self.Text));
  end;
end;

procedure TCustomAdvEdit.SetFloat(const Value: double);
var
  s:string;
begin
  case FEditType of
    etHex: self.Text := IntToHex(trunc(value), 0);
    etNumeric:
      if (FPrecision >= 0) then
        self.Text := Format('%.' + inttostr(FPrecision) + 'n', [value])
      else
        self.Text := Format('%g', [Value]);
    etFloat, etString:
      if (FPrecision >= 0) then
      begin
        s := Format('%.' + inttostr(FPrecision) + 'f', [value]);
        self.Text := s;
      end  
      else
        self.Text := Format('%g', [Value]);
    etMoney:
      begin
        if (FPrecision >= 0) then
          self.Text := Format('%.' + inttostr(FPrecision) + 'f', [value]) else self.Text := Format('%g', [Value]);
        AutoSeparators;
      end;
  end;
  SetModified(True);
end;

procedure TCustomAdvEdit.SetInt(const Value: integer);
begin
  case FEditType of
    etHex: self.Text := IntToHex(value, 0);
    etNumeric: self.Text := Inttostr(value);
    etFloat: self.Text := Inttostr(value);
    etMoney:
      begin
        self.Text := IntToStr(value);
        AutoSeparators;
      end;
  end;
  SetModified(True);
end;

procedure TCustomAdvEdit.SetPrecision(const Value: smallint);
var
  at: TAutoType;
begin
  if (FPrecision <> Value) and (editType in [etFloat, etMoney, etString]) then
  begin
    FPrecision := Value;
    if (Text <> '') or (not AllowNumericNullValue) then
    begin
      at := IsType(self.text);
      if (at in [atFloat, atNumeric]) then
        FloatValue := FloatValue
      else
        FloatValue := 0.0;
    end;
  end;
end;

procedure TCustomAdvEdit.SetPrecisionDisplay(const Value: TPrecisionDisplay);
begin
  if (FPrecisionDisplay <> Value) then
  begin
    FPrecisionDisplay := Value;
    FloatValue := FloatValue;
  end;
end;

function TCustomAdvEdit.GetModified: boolean;
begin
  Result := fIsModified;
end;

procedure TCustomAdvEdit.SetModified(const Value: boolean);
begin
  if csLoading in ComponentState then
    Exit;

  if ReadOnly then
    Exit;

  if FShowModified then
  begin
    if Value then
      Font.Color := FModifiedColor
    else
      Font.Color := FFontColor;
  end;

  inherited Modified := Value;

  FIsModified := Value;
end;


procedure TCustomAdvEdit.ListToRangeStr(rangelist: TRangeList);
var
  c: Integer;
  fstart, fcurr: Integer;
  s: string;
begin
  RangeList.sort(RangeListCompare);

  c := 1;
  fstart := RangeList.Items[0];
  fcurr := fstart;
  s := '';
  while (c < RangeList.Count) do
  begin
    if RangeList.Items[c] <> fCurr + 1 then
    begin
      if (fStart = -2) then {new possible start?}
      begin
        fStart := Rangelist.Items[c];
      end
      else
      begin
        if Length(s) > 0 then s := s + ',';
        if (fStart <> fCurr) then s := s + inttostr(fStart) + '-' + inttostr(fCurr) else s := s + inttostr(fCurr);
        fStart := Rangelist.Items[c];
      end;
    end;
    fCurr := RangeList.Items[c];
    inc(c);
  end;

  if Length(s) > 0 then
    s := s + ',';
  if (FStart <> FCurr) then
    s := s + inttostr(fStart) + '-' + inttostr(fCurr)
  else
    s := s + inttostr(fCurr);

  inherited Text := s;
end;

function TCustomAdvEdit.RangeStrToList(rangelist: TRangeList): Boolean;
begin
  Result := RangeList.StrToList(self.Text);
end;

procedure TCustomAdvEdit.LoadPersist;
var
  Inifile: TInifile;
  RegInifile: TRegInifile;
  s: string;
  i, j: Integer;
begin
  if FPersistence.Enable and (FPersistence.Section <> '') then
  begin
    if fPersistence.Location = plInifile then
    begin
      if FPersistence.Key = '' then
        FPersistence.Key := ChangeFileExt(ParamStr(0), '.INI');
        
      if Persistence.Section = '' then
        Persistence.Section := Name;

      Inifile := TInifile.Create(FPersistence.Key);

      if Lookup.Enabled and Lookup.History then
      begin
        i := IniFile.ReadInteger(FPersistence.Section, 'LOOKUPCOUNT', 0);
        for j := 1 to i do
        begin
          s := Inifile.ReadString(FPersistence.Section, 'LOOKUPVAL' + inttostr(j), '');
          if s <> '' then
            Lookup.ValueList.Add(s);

          s := Inifile.ReadString(FPersistence.Section, 'LOOKUPDISPL' + inttostr(j), '');
          if s <> '' then
            Lookup.DisplayList.Add(s);
        end;
      end;

      s := Inifile.ReadString(FPersistence.Section, self.Name, '@');

      Inifile.Free;
    end
    else
    begin
      RegInifile := TRegInifile.Create(fPersistence.Key);

      if Lookup.Enabled and Lookup.History then
      begin
        i := RegIniFile.ReadInteger(FPersistence.Section, 'LOOKUPCOUNT', 0);
        for j := 1 to i do
        begin
          s := RegInifile.ReadString(FPersistence.Section, 'LOOKUPVAL' + inttostr(j), '');
          if s <> '' then
            Lookup.ValueList.Add(s);

          s := RegInifile.ReadString(FPersistence.Section, 'LOOKUPDISPL' + inttostr(j), '');
          if s <> '' then
            Lookup.DisplayList.Add(s);
        end;
      end;

      s := RegInifile.ReadString(fPersistence.Section, self.Name, '@');

      RegInifile.Free;
    end;

    if (s <> '@') and not IsDB then
      inherited Text := s;
  end;
end;

procedure TCustomAdvEdit.SavePersist;
var
  Inifile: TInifile;
  RegInifile: TRegInifile;
  i: Integer;
begin
  if not Assigned(FPersistence) then Exit;

  if FPersistence.Enable then
  begin
    if fPersistence.Location = plInifile then
    begin
      if FPersistence.Key = '' then
        FPersistence.Key := ChangeFileExt(ParamStr(0), '.INI');

      if FPersistence.Section = '' then
        FPersistence.Section := Name;

      Inifile := TInifile.Create(fPersistence.Key);
      Inifile.WriteString(fPersistence.Section, self.Name, fPrefix + self.Text + fSuffix);

      if Lookup.Enabled and Lookup.History then
      begin
        IniFile.WriteInteger(FPersistence.Section, 'LOOKUPCOUNT', Lookup.DisplayList.Count);
        for i := 1 to Lookup.DisplayList.Count do
        begin
          Inifile.WriteString(FPersistence.Section, 'LOOKUPDISPL' + inttostr(i), Lookup.DisplayList[i - 1]);
          if i < Lookup.ValueList.Count then
            Inifile.WriteString(FPersistence.Section, 'LOOKUPVAL' + inttostr(i), Lookup.ValueList[i - 1]);
        end;
      end;

      Inifile.Free;
    end
    else
    begin
      RegInifile := TRegInifile.Create(fPersistence.Key);
      RegInifile.WriteString(fPersistence.Section, self.Name, fPrefix + self.Text + fSuffix);

      if Lookup.Enabled and Lookup.History then
      begin
        RegIniFile.WriteInteger(FPersistence.Section, 'LOOKUPCOUNT', Lookup.DisplayList.Count);
        for i := 1 to Lookup.DisplayList.Count do
        begin
          RegInifile.WriteString(FPersistence.Section, 'LOOKUPDISPL' + inttostr(i), Lookup.DisplayList[i - 1]);
          if i < Lookup.ValueList.Count then
            RegInifile.WriteString(FPersistence.Section, 'LOOKUPVAL' + inttostr(i), Lookup.ValueList[i - 1]);
        end;
      end;

      RegInifile.Free;
    end;
  end;
end;

procedure TCustomAdvEdit.WMDestroy(var Msg: TMessage);
begin
  if (csLoading in ComponentState) then
    Exit;

  if not (csDesigning in ComponentState) then
    SavePersist;

  if FOleDropTargetAssigned then
    RevokeDragDrop(self.Handle);

  DefaultHandler(msg);
end;

function TCustomAdvEdit.TestURL: Boolean;
begin
  Result := (Pos('://', self.text) > 0) or (Pos('@', self.text) > 1) or (Pos('www.', lowercase(self.Text)) = 1);
end;

function TCustomAdvEdit.GetError: Boolean;
begin
  Result := FError;
end;

procedure TCustomAdvEdit.ApplyErrorColor;
begin

  if FError then
  begin
    inherited Color := FErrorColor;
    Font.Color := FErrorFontColor;
  end
  else
  begin
    if GetFocus = Handle then
    begin
      if FFocusColor <> clNone then
        inherited Color := FFocusColor
      else
        inherited Color := FNormalColor;
      if FFocusFontColor <> clNone then
        Font.Color := FFocusFontColor;
    end
    else
    begin
      Color := FNormalColor;
      Font.Color := FFontColor;
    end;
  end;
end;

procedure TCustomAdvEdit.SetError(const Value: Boolean);
begin
  if (csDesigning in ComponentState) or
    (csLoading in ComponentState) then
    Exit;

  if (Value <> FError) then
  begin
    FError := Value;

    if not ShowError then
      Exit;

    ApplyErrorColor;
  end;
end;

procedure TCustomAdvEdit.Change;
var
  IsValid: Boolean;
begin
  inherited Change;

  if not (csLoading in ComponentState) then
  begin
    if FShowError then
    begin
      IsValid := DoValidate(Self.Text);
      IsError := not IsValid;
    end;
  end;

  if (EmptyText <> '') then
    Invalidate;
end;

{$IFNDEF DELPHIXE10_LVL}
procedure TCustomAdvEdit.ChangeScale(M, D: Integer);
{$ENDIF}
{$IFDEF DELPHIXE10_LVL}
procedure TCustomAdvEdit.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ENDIF}
begin
  inherited;

  if not (csLoading in ComponentState) then
  begin
    FLabelFont.Height := MulDiv(FLabelFont.Height,M,D);
  end;
end;

procedure TCustomAdvEdit.Init;
var
  OldColor: TColor;
begin
  FNormalColor := Color;

  FFontColor := Font.Color;
  FOldBorder := BorderStyle;
  FFlat := not FFlat;
  SetFlat(not FFlat);

  if FLabel <> nil then UpdateLabel;

  if not Enabled then
  begin
    OldColor := Color;
    if FDisabledColor <> clNone then
      Color := FDisabledColor
    else
      Color := clWindow;
    FNormalColor := OldColor;
  end;
end;

procedure TCustomAdvEdit.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) then
  begin
    Init;
    // at design time , designer is 96dpi, so scaling is not needed
    Height := Round(FLoadedHeight * CalculateDPIScale(Self));
    SetBounds(Left, Top, Width, Height);
  end;

  if not (csDesigning in ComponentState) then
    LoadPersist;

  if not LabelAlwaysEnabled and Assigned(FLabel) then
  begin
    FLabel.Enabled := Enabled;
    FLabel.AlwaysEnable := LabelAlwaysEnabled;
  end;

  if (FLabel <> nil) then
    UpdateLabel;

  FLblFntHeight := LabelFont.Height;

  if ParentFont and not ShowError and Assigned(FLabel) then
  begin
    FLabel.Font.Assign(Font);
  end;

  FParentFnt := ParentFont;

  if (FLabel <> nil) then
    UpdateLabel;

{$IFDEF DELPHIXE2_LVL}
  if not (csDesigning in ComponentState) and StyleServices.Enabled and (StyleServices.Name <> 'Windows') then
  begin
    LabelTransparent := true;
  end;
{$ENDIF}
end;

procedure TCustomAdvEdit.DrawBorder;
var
  DC: HDC;
begin
  if Enabled and not (FFlat or (FBorderColor <> clNone) or (FFocusBorder and FMouseInControl) or Border3D or (FFocusBorderColor <> clNone)) then
    Exit;

  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TCustomAdvEdit.DrawControlBorder(DC: HDC);
var
  ARect: TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
  clr: TColor;
  HasFocus: boolean;
begin
  if (BorderColor = clNone) and (csDesigning in ComponentState) then
    Exit;

  clr := clNone;
  HasFocus := GetFocus = Handle;

  if not Enabled and FIsThemed and DisabledBorder then
    clr := clSilver
  else
  begin
    if (FBorderColor <> clNone) and ((not HasFocus) or (FFocusBorderColor = clNone)) then
      clr := FBorderColor
    else
      if (FFocusBorderColor <> clNone) and HasFocus then
        clr := FFocusBorderColor;
  end;

  if clr <> clNone then
  begin
    BtnFaceBrush := CreateSolidBrush(ColorToRGB(clr));
    try
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
    finally
      DeleteObject(BtnFaceBrush);
    end;
    Exit;
  end;

  if not Enabled or (FocusBorder and FIsThemed) then
    Exit;

  if Is3DBorderButton then
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
    BtnFaceBrush := CreateSolidBrush(ColorToRGB((Parent as TWinControl).Brush.Color));

  WindowBrush := CreateSolidBrush(GetSysColor(COLOR_WINDOW));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderButton then
    begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
    end
    else
    begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, BtnFaceBrush);
    end;
  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

function TCustomAdvEdit.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);

  Result := (Result and FFocusBorder) or (Border3D);
end;


function TCustomAdvEdit.IsDB: boolean;
begin
  Result := false;
end;

procedure TCustomAdvEdit.SetOleDropSource(const Value: boolean);
begin
  FOleDropSource := Value;
end;

procedure TCustomAdvEdit.SetOleDropTarget(const Value: boolean);
begin
  FOleDropTarget := Value;
  if not (csDesigning in ComponentState) then
  begin
    if FOleDropTarget then
    begin
      FOleDropTargetAssigned := RegisterDragDrop(self.Handle, TEditDropTarget.Create(self)) = s_OK;
    end
    else
      if FOleDropTargetAssigned then RevokeDragDrop(Self.Handle);
  end;
end;

procedure TCustomAdvEdit.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if (FFocusBorderColor <> clNone) or (FBorderColor <> clNone) or (not Enabled and DisabledBorder) then
    DrawBorder;
end;

procedure TCustomAdvEdit.SetEnabled(Value: boolean);
var
  OldValue: Boolean;
  OldColor: TColor;
begin
  if (Value = Enabled) then
    Exit;

  OldValue := Enabled;

  inherited;

  if (csLoading in ComponentState) or
    (csDesigning in ComponentState) then
    Exit;

  if (OldValue <> Value) then
  begin
    if Value then
    begin
      Color := FNormalColor;
    end
    else
    begin
      if not IsError then
        OldColor := Color
      else
        OldColor := FNormalColor;

      if FDisabledColor <> clNone then
        Color := FDisabledColor
      else
        Color := clWindow;

      FNormalColor := OldColor;
    end;

    if Value and FError and ShowError then
      ApplyErrorColor;

    if Assigned(FLabel) then
      if not FLabelAlwaysEnabled then
        FLabel.Enabled := Value;
  end;

  Width := Width + 1;
  Width := Width - 1;
end;

procedure TCustomAdvEdit.SetDisabledColor(const Value: TColor);
begin
  if (FDisabledColor <> Value) then
  begin
    FDisabledColor := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvEdit.SetDisabledTextColor(const Value: TColor);
begin
  if (FDisabledTextColor <> Value) then
  begin
    FDisabledTextColor := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvEdit.SetDisabledBorder(const Value: boolean);
begin
  if (FDisabledBorder <> Value) then
  begin
    FDisabledBorder := Value;
    Invalidate;
  end;
end;

function TCustomAdvEdit.GetColorEx: TColor;
begin
  Result := inherited Color;
end;

procedure TCustomAdvEdit.SetColorEx(const Value: TColor);
begin
  inherited Color := Value;
  FNormalColor := Value;
end;

procedure TCustomAdvEdit.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) and not ShowError and ParentFont then
  begin
    FLabel.Font.Assign(Font);
    UpdateLabel;
  end;
end;

procedure TCustomAdvEdit.CMHintShow(var Msg: TMessage);
var
  hi: PHintInfo;

begin
  if (GetTextSize > Width) and (FHintShowLargeText) then
  begin
    hi := PHintInfo(Msg.LParam);
    hi.HintStr := Text;
    hi.HintPos := ClientToScreen(Point(0, 0));
  end;

  inherited;
end;

procedure TCustomAdvEdit.SetAutoThousandSeparator(const Value: Boolean);
begin
  FAutoThousandSeparator := Value;
  if FEditType in [etMoney, etFloat] then AutoSeparators;
end;

procedure TCustomAdvEdit.SetEmptyText(const Value: string);
begin
  if (FEmptyText <> Value) then
  begin
    FEmptyText := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvEdit.SetEmptyTextStyle(const Value: TFontStyles);
begin
  if (FEmptyTextStyle <> Value) then
  begin
    FEmptyTextStyle := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvEdit.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
var
  IsPrev: Boolean;
  p: TWinControl;

begin
  p := Parent;
  while Assigned(p) and not (p is TCustomForm) do
    p := p.Parent;

  if not Assigned(p) then
    Exit;

  if FBlockDefaultHandling then
  begin
    FBlockDefaultHandling := false;
    Exit;
  end;

  IsPrev := (p as TCustomForm).KeyPreview;

  if (Msg.CharCode = VK_ESCAPE) and FCanUndo and not IsPrev and Assigned(FLookupList) then
  begin
    if not FLookupList.Visible then
    begin
      Text := FOldString;
      SelectAll;
    end
    else
    begin
      FBlockDefaultHandling := true;
      FLookupList.Hide;
    end;

    Font.Color := FFocusFontColor;
    SetModified(False);
    Msg.CharCode := 0;
    Msg.Result := 0;
    // Take care of default key handling
    if (Parent is TWinControl) and FDefaultHandling then
    begin
      PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_ESCAPE, 0);
      PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_ESCAPE, 0);
    end;
  end;

  if (Msg.CharCode = VK_RETURN) and FDefaultHandling and not IsPrev then
  begin
    // Take care of default key handling
    if (Parent is TWinControl) then
    begin
      if (GetFocus = Parent.handle) then
      begin
        PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_RETURN, 0);
        PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_RETURN, 0);
      end;
    end;
  end;

  inherited;
end;

procedure TCustomAdvEdit.SetSoftBorder(const Value: Boolean);
begin
  if FSoftBorder <> Value then
  begin
    FSoftBorder := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvEdit.SetLabelAlwaysEnabled(const Value: Boolean);
begin
  FLabelAlwaysEnabled := Value;
  if Assigned(FLabel) then
  begin
    if Value then
      FLabel.Enabled := True;
    FLabel.AlwaysEnable := Value;
  end;
  Invalidate;
end;


procedure TCustomAdvEdit.SetBorder3D(const Value: Boolean);
begin
  if (FBorder3D <> Value) then
  begin
    FBorder3D := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvEdit.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    if HandleAllocated then
      SendMessage(Handle, WM_NCPAINT, 0,0);
  end;
end;

procedure TCustomAdvEdit.CreateWnd;
var
  isScaled: boolean;
  frm: TCustomForm;
begin
  inherited;

  SetFlatRect(FFlat);

  isScaled := true;

  frm := GetParentForm(Self);

  if Assigned(frm) and (frm is TForm) then
    isScaled := (frm as TForm).Scaled;

  if not ParentFont and (LabelCaption <> '') and not (csDesigning in ComponentState) and isScaled then
  begin
    FLblUpdate := true;
    LabelFont.Height := Round(FLblFntHeight * CalculateDPIScale(Self));
    FLblUpdate := false;
  end;

  if Assigned(FLabel) then
    UpdateLabelPos;

  if IsVista then
  begin

    case EditAlign of
    eaRight:
      SetWindowLong(Handle, GWL_STYLE,
                  GetWindowLong(Handle, GWL_STYLE)
                  or ES_RIGHT);

    eaCenter:
      SetWindowLong(Handle, GWL_STYLE,
                  GetWindowLong(Handle, GWL_STYLE)
                  or ES_CENTER);
    end;
  end;

  IsError := not DoValidate(Text);

  if IsError and Enabled and ShowError then
  begin
    ApplyErrorColor;
  end;
end;

procedure TCustomAdvEdit.SetErrorMarkerLen(const Value: Integer);
begin
  FErrorMarkerLen := Value;
  Invalidate;
end;

procedure TCustomAdvEdit.SetErrorMarkerPos(const Value: Integer);
begin
  FErrorMarkerPos := Value;
  Invalidate;
end;

procedure TCustomAdvEdit.SetFocusBorder(const Value: Boolean);
begin
  if (FFocusBorder <> Value) then
  begin
    FFocusBorder := Value;
    Invalidate;
  end;
end;

function TCustomAdvEdit.GetHeightEx: Integer;
begin
  Result := inherited Height;
end;

procedure TCustomAdvEdit.SetHeightEx(const Value: Integer);
begin
  if (csLoading in ComponentState) then
    FLoadedHeight := Value;
  inherited Height := Value;
end;

function VarPos(su, s: string; var Res: Integer): Integer;
begin
  Res := Pos(su, s);
  Result := Res;
end;

procedure TCustomAdvEdit.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoneLookup;
end;

procedure TCustomAdvEdit.CloseLookup;
begin
  SetForegroundWindow(Handle);
  SetActiveWindow(Handle);
  Self.SetFocus;
  if Assigned(FLookupList) then
    FLookupList.Hide;
end;

procedure TCustomAdvEdit.DoLookupNeedData(Value: string; List: TStrings; var ItemIndex: integer);
begin
  if Assigned(OnLookupNeedData) then
    OnLookupNeedData(Self, Value, List, ItemIndex);
end;

procedure TCustomAdvEdit.DoneLookup;
var
  idx, vp: Integer;
  NewValue: string;
  LookupText, NewText: string;
begin
  if (FlookupListBox.ItemIndex = -1) then
    Exit;

  idx := Integer(FLookupListBox.Items.Objects[FlookupListBox.ItemIndex]);
  NewValue := FLookupListBox.Items[FLookupListBox.ItemIndex];

  CloseLookup;

  if (FLookup.ValueList.Count > 0) then
  begin
    if (idx >= 0) and (idx < FLookup.ValueList.Count) then
      NewValue := FLookup.ValueList.Strings[idx]
    else
      NewValue := FLookupListbox.Items[FLookupListBox.ItemIndex];

    if Assigned(FOnLookupSelect) then
      FOnLookupSelect(Self, NewValue);

    if Assigned(FOnlookupIndexSelect) then
      FOnLookupIndexSelect(Self, idx, NewValue);
  end
  else
  begin
    if Assigned(FOnLookupSelect) then
      FOnLookupSelect(Self, NewValue);

    if Assigned(FOnlookupIndexSelect) then
      FOnLookupIndexSelect(Self, idx, NewValue);
  end;

  if FLookup.Multi then
  begin
    NewValue := NewValue + FLookup.Separator;
    LookupText := Text; // get current text value & strip till last lookup part
    NewText := '';
    while VarPos(FLookup.Separator, LookupText, vp) > 0 do
    begin
      NewText := NewText + Copy(LookupText, 1, vp);
      Delete(LookupText, 1, vp);
    end;
    Text := NewText + NewValue;
  end
  else
    Text := NewValue;

  if FLookup.Multi then
    SelStart := length(Text)
  else
    SelectAll;
end;

procedure TCustomAdvEdit.ListKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    DoneLookup;

  if (Key = #9) and (Lookup.AcceptOnTab) then
    DoneLookup;
end;

procedure TCustomAdvEdit.UpdateLookup;
var
  pt, cp: TPoint;
  i, tp: Integer;
  mw, mh, tw, vp, vpv, cnt, idx: Integer;
  LookupText, ValStr: string;
  lx,ly: Integer;
  ismatch: boolean;
  seps,sepe: string;

begin
  if not FLookup.Enabled then
    Exit;

  tp := 0;

  if FLookup.Multi then
  begin
    LookupText := Text;
    while VarPos(FLookup.Separator, LookupText, vp) > 0 do
    begin
      tp := tp + vp;
      Delete(LookupText, 1, vp);
    end;
  end
  else
    LookupText := Text;


  if (Length(LookupText) >= FLookup.NumChars) and Assigned(FLookupList) then
  begin
    FLookupList.Visible := false;
    FLookupList.Parent := self;

    pt := ClientToScreen(Point(0, 0));
    FLookupList.Text := '  ';
    FLookupList.Color := FLookup.Color;
    FLookupListbox.Color := FLookup.Color;
    FLookupListBox.Font.Assign(FLookup.Font);
    FLookupListBox.Font.Size := ScaleFromSmallFontsDimension(Lookup.Font.Size);
    FLookupListbox.Ctl3D := False;
    FLookupListBox.HandleTab := Lookup.AcceptOnTab;

    ly := pt.Y + Height - 4;

    i := SendMessage(Handle, EM_POSFROMCHAR, tp, 0);

    if i >= 0 then
    begin
      cp := Point(loword(i), hiword(i));

      if cp.X > Width then
        cp.X := Width;
    end
    else
      cp := Point(0,0);

    lx := pt.X + 8 + cp.X;

    FLookupList.Color := clWindow;

    FLookupListBox.Items.Clear;

    for i := 1 to FLookup.DisplayList.Count do
    begin
      if FLookup.CaseSensitive then
        vp := Pos(LookupText, FLookup.FDisplayList.Strings[i - 1])
      else
        vp := Pos(AnsiUppercase(LookupText), AnsiUppercase(FLookup.FDisplayList.Strings[i - 1]));

      vpv := -1;
      if FLookup.SearchValue and (i <= FLookup.ValueList.Count) then
      begin
        if FLookup.CaseSensitive then
          vpv := Pos(LookupText, FLookup.FValueList.Strings[i - 1])
        else
          vpv := Pos(AnsiUppercase(LookupText), AnsiUppercase(FLookup.FValueList.Strings[i - 1]));
      end;

      if FullTextSearch then
        ismatch := (vp > 0) or (vpv > 0)
      else
        ismatch := (vp = 1) or (vpv = 1);

      if LookupText = '' then
        ismatch := true;


      seps := '';
      sepe := '';

      case FLookup.ValueSeparator of
        vsBracket:
          begin
            seps := '(';
            sepe := ')';
          end;
        vsSquareBracket:
          begin
            seps := '[';
            sepe := ']';
          end;
        vsLargerSmaller:
          begin
            seps := '<';
            sepe := '>';
          end;
      end;

      if ismatch then
      begin
        ValStr := '';
        if FLookup.ShowValue and (i <= FLookup.FValueList.Count) and (Flookup.FValueList.Strings[i - 1] <> '') then
          ValStr := ' '+ seps + Flookup.FValueList.Strings[i - 1] + sepe;

        FLookupListbox.Items.AddObject(FLookup.FDisplayList.Strings[i - 1] + ValStr, TObject(i - 1));
      end;
    end;

    idx := 0;
    DoLookupNeedData(LookupText, FLookupListBox.Items, idx);
    cnt := FLookupListBox.Items.Count;

    FLookupListBox.Sorted := True;

    mw := 50;
    FLookupList.Width := 0;

    FLookupList.Canvas.Font.Assign(FLookupListBox.Font);
    FLookupListBox.Font.Assign(FLookupListBox.Font);
    FLookupListBox.ItemHeight := FLookupList.Canvas.TextHeight('gh') + FLookup.Spacing;

    FLookupListBox.ItemIndex := idx;

    if cnt < FLookup.DisplayCount then
      mh := (cnt * FLookupListBox.ItemHeight) + 4
    else
      mh := (FLookup.DisplayCount * FLookupListBox.ItemHeight) + 4;

    if cnt > 0 then
    begin
      for i := 1 to cnt do
      begin
        tw := FLookupList.Canvas.TextWidth(FLookupListBox.Items[i - 1]);

        if tw > mw then
          mw := tw;
      end;

      mw := mw + 8;

      if cnt > FLookup.DisplayCount then
        mw := mw + GetSystemMetrics(SM_CXHSCROLL);
    end;

    if cnt > 0 then
    begin
      FLookupList.Parent := self;
      FLookupListBox.Font.Assign(Lookup.Font);
      FLookupListBox.Font.Size := ScaleFromSmallFontsDimension(Lookup.Font.Size);

      FLookupList.Top := ly;
      FLookupList.Left := lx;
      MoveWindow(FLookupList.Handle, lx, ly, mw, mh, true);
      FLookupList.Visible := true;
      FLookupListBox.Font.Size := ScaleFromSmallFontsDimension(Lookup.Font.Size);
      FLookupList.Top := ly;
      FLookupList.Left := lx;

      SetWindowPos(FLookupList.Handle, HWND_TOPMOST,0,0,0,0,SWP_NOSIZE or  SWP_NOMOVE or SWP_NOACTIVATE);
    end
    else
    begin
      FLookupList.Hide;
    end;
  end
  else
  begin
    FLookupList.Visible := False;
    FLookupList.Parent := nil;
  end;
end;

procedure TCustomAdvEdit.LabelClick(Sender: TObject);
begin
  if Assigned(FOnLabelClick) then
    FOnLabelClick(Self);
end;

procedure TCustomAdvEdit.LabelDblClick(Sender: TObject);
begin
  if Assigned(FOnLabelDblClick) then
    FOnLabelDblClick(Self);
end;

function TCustomAdvEdit.DoValidate(Value: string): Boolean;
var
  IsValid: Boolean;
  i: integer;
  d: double;
begin
  IsValid := True;

  Result := IsValid;

  if FIsValidating then
    Exit;

  FIsValidating := True;

  if (EditType = etNumeric) and AllowNumericNullValue and (Value = '') then
    isValid := true
  else
    if (EditType = etNumeric) and ((FMinValue <> 0) or (FMaxValue <> 0)) then
    begin
      try
        i := StrToInt(value);
      except
        i := 0;
      end;
      isValid := (i >= FMinValue) and (i <= FMaxValue);

      if AutoValidate and not isValid then
      begin
        if i < FMinValue then
          IntValue := FMinValue
        else
          if i > FMaxValue then
            IntValue := FMaxValue;
      end;
    end;

  if (EditType in [etFloat, etMoney]) and ((FMinFloatValue <> 0) or (FMaxFloatValue <> 0)) then
  begin
    d := EStrToFloat(value);
    isValid := (d >= FMinFloatValue) and (d <= FMaxFloatValue);

    if AutoValidate and not isValid then
    begin
      if d < FMinFloatValue then
        FloatValue := FMinFloatValue
      else
        if d > FMaxFloatValue then
          FloatValue := FMaxFloatValue;
    end;
  end;

  ValidateEvent(Value, isValid);

  FIsValidating := False;

  Result := IsValid;
end;

procedure TCustomAdvEdit.Validate;
begin
  IsError := not DoValidate(Self.Text);
  if IsError and Enabled and ShowError then
  begin
    ApplyErrorColor;
  end;
end;

procedure TCustomAdvEdit.ValidateEvent(Value: string; var IsValid: Boolean);
begin
  if HandleAllocated then
    if Assigned(FOnValueValidate) then
      FOnValueValidate(Self, value, IsValid);
end;

function TCustomAdvEdit.GetValue: variant;
begin
  if (EditType = etNumeric) then
    Result := IntValue
  else
  if (EditType = etFloat) then
    Result := FloatValue
  else
    Result := Text;
end;

function TCustomAdvEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TCustomAdvEdit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TCustomAdvEdit.SetValue(const Value: variant);
begin
  Text := Value;
end;

procedure TCustomAdvEdit.SetVersion(const Value: string);
begin

end;

{TAdvCustomMaskEdit}

constructor TAdvCustomMaskEdit.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(aOwner);
  FAutoTab := True;
  FLabelMargin := 4;
  FReturnIsTab := True;
  FBorderColor := clNone;
  FFocusColor := clNone;
  FNormalColor := clWindow;
  FModifiedColor := clRed;
  FFocusBorderColor := clNone;
  FDisabledColor := clSilver;
  FDisabledBorder := true;
  FLabelFont := TFont.Create;
  FLabelFont.OnChange := LabelFontChanged;
  FFlatParentColor := True;
  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);
  FIndentL := 0;
  FIndentR := 0;
  FParentFnt := false;
  FLblUpdate := false;
  FDefaultHandling := ADVEDIT_DEFAULTHANDLING;
end;


procedure TAdvCustomMaskEdit.SetAlignment(value: tAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TAdvCustomMaskEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(params);

  if (PasswordChar = #0) then
  begin
    Params.Style := Params.Style or ES_MULTILINE;
  end;

  if (FAlignment = taRightJustify) then
  begin
    params.style := params.style and not (ES_LEFT) and not (ES_CENTER);
    params.style := params.style or (ES_RIGHT);
    params.style := params.style or (ES_MULTILINE);
  end;

  if (FAlignment = taCenter) then
  begin
    params.style := params.style and not (ES_LEFT) and not (ES_RIGHT);
    params.style := params.style or (ES_CENTER);
    params.style := params.style or (ES_MULTILINE);
  end;

end;

procedure TAdvCustomMaskEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  accept: Boolean;
  cf: TCustomForm;
begin
  inherited KeyUp(Key, Shift);

  if (Pos(' ', Text) = 0) and (self.SelStart = Length(EditText)) and (Editmask <> '') and (Text <> '') then
  begin
    Accept := true;
    if Assigned(FOnMaskComplete) then
      FOnMaskComplete(self, self.Text, accept);

    if FAutoTab and Accept then
    begin
      cf := GetParentForm(self);
      if Assigned(cf) then
        cf.Perform(WM_NEXTDLGCTL, 0, 0);
    end;
  end;
end;

procedure TAdvCustomMaskEdit.DoEnter;
begin
  if (EditMask <> '') and FSelectFirstChar then
  begin
    SelStart := 0;
    SelLength := 1;
  end;
  inherited;
end;

procedure TAdvCustomMaskEdit.CMMouseEnter(var Msg: TMessage);
begin
  if FAutoFocus then
    SetFocus;

  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    if FFocusBorder then DrawBorder;
  end;
end;

procedure TAdvCustomMaskEdit.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    if FFocusBorder then Invalidate;
  end;
end;


procedure TAdvCustomMaskEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;

  if csLoading in ComponentState then
    Exit;

  inherited Color := FNormalColor;

  if not (ShowModified) then
    Font.Color := FFontColor;

  if FFocusLabel and (FLabel <> nil) then
    FLabel.Font.Style := FLabel.Font.Style - [fsBold];

  if (FFocusBorderColor <> clNone) or (FBorderColor <> clNone) then
  begin
    SendMessage(Handle, WM_NCPAINT, 0,0);
    Width := Width - 1;
    Width := Width + 1;
  end;
end;

procedure TAdvCustomMaskEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;

  if csLoading in ComponentState then
    Exit;

  if FFocusColor <> clNone then
    inherited Color := FFocusColor;

  if not ShowModified then
  begin
    if (Font.Color <> FFontColor) then
      FFontColor:= Font.Color;
    if (FFocusFontColor <> clNone) then
      Font.Color := FFocusFontColor;
  end;

  FOriginalValue := self.Text;

  if FFocusLabel and (FLabel <> nil) then
  begin
    FLabel.Font.Style := FLabel.Font.Style + [fsBold];
    UpdateLabelPos;
  end;

  if AutoSelect then
    SelectAll;

  if (FocusBorder) or (FFocusBorderColor <> clNone) then
  begin
    if BorderStyle = bsNone then
      SetFlatRect(true);
    Invalidate;
  end;
end;

procedure TAdvCustomMaskEdit.WMChar(var Msg: TWMKey);
var
  cf: TCustomForm;
  key: char;
begin
  if (Msg.CharCode = VK_RETURN) then
  begin
    key := #13;
    if Assigned(OnKeyPress) then
      OnKeyPress(Self, key);

    Msg.CharCode := 0;

    if not DefaultHandling and not ReturnIsTab then
    begin
      if (Parent is TWinControl) then
      begin
        PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_RETURN, 0);

        cf := GetParentForm(self);

        if Assigned(cf) then
          if cf.KeyPreview then
          begin
            inherited;
            Exit;
          end;

        PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_RETURN, 0);
      end;
    end;
    Exit;
  end;

  if (Msg.CharCode = VK_ESCAPE) then
  begin
    if not DefaultHandling then
    begin
      if (Parent is TWinControl) then
      begin

        cf := GetParentForm(self);

        if Assigned(cf) then
          if cf.KeyPreview then
          begin
            inherited;
            Exit;
          end;

        PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_ESCAPE, 0);
        PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_ESCAPE, 0);
      end;
    end;
  end;

  if (Msg.Charcode = VK_ESCAPE) then
  begin
    inherited;
    Exit;
  end;

  inherited;

  if FShowModified then
    Font.Color := FModifiedColor;
end;

procedure TAdvCustomMaskEdit.WMCopy(var Message: TWMCopy);
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnClipboardCopy) then
    FOnClipboardCopy(self, copy(Text, SelStart + 1, SelLength), allow);

  if Allow then
  begin
    inherited;
    if Assigned(FOnAfterClipboardCopy) then
      FOnAfterClipboardCopy(Self);
  end;
end;

procedure TAdvCustomMaskEdit.WMCut(var Message: TWMCut);
var
  Allow: boolean;
begin
  Allow := True;
  if Assigned(FOnClipboardCut) then
  begin
    FOnClipboardCut(self, copy(self.text, SelStart + 1, SelLength), allow);
  end;

  if Allow then
  begin
    inherited;
    if Assigned(FOnAfterClipboardCut) then
      FOnAfterClipboardCut(Self);
  end;
end;

function TAdvCustomMaskEdit.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.caption
  else
    Result := '';
end;

procedure TAdvCustomMaskEdit.SetLabelCaption(const value: string);
begin
  if FLabel = nil then
    FLabel := CreateLabel;
  FLabel.Caption := value;
  UpdateLabel;
end;


function TAdvCustomMaskEdit.CreateLabel: TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent := Self.Parent;
  Result.FocusControl := Self;
  Result.Font.Assign(LabelFont);
  Result.ParentFont := self.ParentFont;
end;


procedure TAdvCustomMaskEdit.SetLabelMargin(const Value: integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomMaskEdit.SetLabelTransparent(const Value: boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;


procedure TAdvCustomMaskEdit.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomMaskEdit.UpdateLabel;
begin
  if Assigned(FLabel.Parent) then
  begin
    FLabel.Transparent := FLabeltransparent;

    if not FParentFnt then
    begin
      FLabel.Font.Assign(FLabelFont);
    end
    else
      FLabel.Font.Assign(Font);

    if FLabel.Parent.HandleAllocated then
      UpdateLabelPos;
  end;
end;

procedure TAdvCustomMaskEdit.UpdateLabeLPos;
var
  tw,lblmargin: integer;
  r: TRect;
begin
  r := Rect(0,0,1000,255);
  DrawText(FLabel.Canvas.Handle, PChar(FLabel.Caption), Length(FLabel.Caption), r, DT_HIDEPREFIX or DT_CALCRECT);
  tw := r.Right;

  lblmargin := Round(FLabelMargin *  CalculateDPIScale(Self));


  case FLabelPosition of
    lpLeftTop: begin
        FLabel.Top := Top;
        FLabel.Left := Left - tw - lblmargin;
      end;
    lpLeftCenter: begin
        FLabel.Top := Top + ((Height - FLabel.Height) div 2);
        FLabel.Left := Left - tw - lblmargin;
      end;
    lpLeftBottom: begin
        FLabel.Top := Top + Height - FLabel.Height;
        FLabel.Left := Left - tw - lblmargin;
      end;
    lpTopLeft: begin
        FLabel.Top := Top - FLabel.Height - lblmargin;
        FLabel.Left := Left;
      end;
    lpTopRight:
      begin
        FLabel.Top := Top - FLabel.Height - lblmargin;
        FLabel.Left := Left + Width - FLabel.Width;
      end;
    lpTopCenter:
      begin
        FLabel.Top := Top - FLabel.Height - lblmargin;
        if Width - FLabel.Width > 0 then
          FLabeL.Left := Left + ((Width - FLabel.Width) div 2)
        else
          FLabeL.Left := Left - ((FLabel.Width - Width) div 2)
      end;
    lpBottomLeft: begin
        FLabel.Top := Top + Height + lblmargin;
        FLabel.Left := Left;
      end;
    lpBottomCenter:
      begin
        FLabel.Top := Top + Height + lblmargin;
        if Width - FLabel.Width > 0 then
          FLabeL.Left := Left + ((Width - FLabel.width) div 2)
        else
          FLabeL.Left := Left - ((FLabel.Width - width) div 2)
      end;
    lpBottomRight:
      begin
        FLabel.Top := Top + Height + lblmargin;
        FLabel.Left := Left + Width - FLabel.Width;
      end;
    lpLeftTopLeft:
      begin
        FLabel.Top := Top;
        FLabel.Left := Left - lblmargin;
      end;
    lpLeftCenterLeft:
      begin
        FLabel.Top := Top + ((Height - FLabel.Height) div 2);
        FLabel.Left := Left - lblmargin;
      end;
    lpLeftBottomLeft:
      begin
        FLabel.Top := Top + Height - FLabel.Height;
        FLabel.Left := Left - lblmargin;
      end;
    lpRightTop:
      begin
        FLabel.Top := Top;
        FLabel.Left := Left + Width + lblmargin;
      end;
    lpRightCenter:
      begin
        FLabel.Top := Top + ((Height - FLabel.Height) div 2);
        FLabel.Left := Left + Width + lblmargin;
      end;
    lpRighBottom:
      begin
        FLabel.Top := Top + Height - FLabel.Height;
        FLabel.Left := Left + Width + lblmargin;
      end;
  end;

  FLabel.Visible := Visible;
end;


procedure TAdvCustomMaskEdit.ValidateEdit;
begin
  if Assigned(OnValidateEdit) then
    FOnValidateEdit(Self);
  inherited;
end;

procedure TAdvCustomMaskEdit.ValidateError;
begin
  if Assigned(OnValidateError) then
    FOnValidateError(Self)
end;

destructor TAdvCustomMaskEdit.Destroy;
begin
  if FLabel <> nil then
  begin
    FLabel.Parent := nil;
    FLabel.Free;
    FLabel := nil;
  end;
  FLabelFont.Free;
  inherited Destroy;
end;

procedure TAdvCustomMaskEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  lblmargin: integer;
begin
  if Assigned(FLabel) then
  begin
    lblmargin := Round(FLabelMargin *  CalculateDPIScale(Self));

    case LabelPosition of
      lpLeftTop, lpLeftCenter, lpLeftBottom:
        begin
          if (Align in [alTop, alClient, alBottom]) then
          begin
            AWidth := AWidth - (FLabel.Width + lblmargin);
            ALeft := ALeft + (FLabel.Width + lblmargin);
          end;
        end;
      lpRightTop, lpRightCenter, lpRighBottom:
        begin
          if (Align in [alTop, alClient, alBottom]) then
            AWidth := AWidth - (FLabel.Width + lblmargin);
        end;
      lpTopLeft, lpTopCenter, lpTopRight:
        begin
          if (Align in [alTop, alClient, alRight, alLeft]) then
            ATop := ATop + FLabel.Height;
        end;
    end;
  end;

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if (FLabel <> nil) then
  begin
    if (FLabel.Parent <> nil) then
      UpdateLabel;
  end;

  if FFlat then
    SetFlatRect(FFlat);
end;

procedure TAdvCustomMaskEdit.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if FFlat then
    begin
      if not (csLoading in ComponentState) then
        if FFlatParentColor then
          Color := (Parent as TWinControl).Brush.Color;
      Borderstyle := bsNone;
      SetFlatRect(True);
    end
    else
    begin
      Color := clWindow;
      BorderStyle := FOldBorder;
      SetFlatRect(False);
    end;
    Invalidate;
  end;
end;

procedure TAdvCustomMaskEdit.CMFontChanged(var Message: TMessage);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    if (FLabel <> nil) and ParentFont then
    begin
      FLabel.Font.Assign(Font);
    end;
  inherited;
  SetFlatRect(FFlat);
end;


procedure TAdvCustomMaskEdit.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if (FFocusBorderColor <> clNone) or (FBorderColor <> clNone) or not Enabled then
  begin
    DrawBorder;
  end;
end;

procedure TAdvCustomMaskEdit.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) and ParentFont  then
  begin
    FLabel.Font.Assign(Font);
    UpdateLabel;
  end;
end;


procedure TAdvCustomMaskEdit.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then
    FLabel.Visible := Visible;
end;

procedure TAdvCustomMaskEdit.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FLabel) then
    FLabel.Visible := Visible;
end;

procedure TAdvCustomMaskEdit.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
var
  IsPrev: Boolean;
  p: TWinControl;

begin
  p := Parent;
  while Assigned(p) and not (p is TCustomForm) do
    p := p.Parent;

  if not Assigned(p) then
    Exit;

  if FBlockDefaultHandling then
  begin
    FBlockDefaultHandling := false;
    Exit;
  end;

  IsPrev := (p as TCustomForm).KeyPreview;

  if (Msg.CharCode = VK_RETURN) and FDefaultHandling and not IsPrev then
  begin
    // Take care of default key handling
    if (Parent is TWinControl) then
    begin
      if (GetFocus = Parent.handle) then
      begin
        PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_RETURN, 0);
        PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_RETURN, 0);
      end;
    end;
  end;

  inherited;

end;

procedure TAdvCustomMaskEdit.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  PaintEdit;
  if Border3D or (FFocusBorderColor <> clNone) or (FBorderColor <> clNone)  then
    DrawBorder;
end;

procedure TAdvCustomMaskEdit.WMPaste(var Msg: TMessage);
var
{$IFNDEF DELPHI_UNICODE}
  Data: THandle;
  content: PChar;
{$ENDIF}
  newstr, cliptxt: string;
  newss, newsl: Integer;
  allow: Boolean;

  function InsertString(s: string): string;
  var
    ss: Integer;
  begin
    Result := self.text;
    ss := SelStart;
    if (SelLength = 0) then
    begin
      insert(s, result, ss + 1);
      newsl := 0;
      newss := ss + Length(s);
    end
    else
    begin
      delete(result, ss + 1, SelLength);
      insert(s, result, ss + 1);
      newsl := Length(s);
      newss := ss;
    end;
  end;

begin
  if ReadOnly then
    Exit;

  {$IFDEF DELPHI_UNICODE}
  if ClipBoard.HasFormat(CF_UNICODETEXT) then
  begin
    //
  end;

  if ClipBoard.HasFormat(CF_TEXT) then
  begin
    Allow := True;
    cliptxt := Clipboard.AsText;
    if pos(#13,cliptxt) > 0 then
      cliptxt := Copy(cliptxt,1,pos(#13,cliptxt) - 1);
    newstr := InsertString(cliptxt);
  {$ENDIF}


{$IFNDEF DELPHI_UNICODE}

  if ClipBoard.HasFormat(CF_TEXT) then
  begin
    ClipBoard.Open;
    Data := GetClipBoardData(CF_TEXT);
    try
      if Data <> 0 then
        Content := PChar(GlobalLock(Data))
      else
        Content := nil
    finally
      if Data <> 0 then
        GlobalUnlock(Data);
      ClipBoard.Close;
    end;

    if Content = nil then
      Exit;

    Allow := True;

    cliptxt := StrPas(Content);
    if pos(#13,cliptxt) > 0 then
      cliptxt := copy(cliptxt,1,pos(#13,cliptxt) - 1);
    newstr := InsertString(cliptxt);

{$ENDIF}

    if Assigned(FOnClipboardPaste) then
      FOnClipboardPaste(self, newstr, Allow);

    if Allow then
    begin
      inherited;

      SelStart := newss;
      SelLength := newsl;

      if Assigned(FOnAfterClipboardPaste) then
        FOnAfterClipboardPaste(Self);
    end;
  end;
end;

procedure TAdvCustomMaskEdit.WMKeyDown(var Msg: TWMKeydown);
begin
  FGotReturn := (msg.CharCode = VK_RETURN);

  if (msg.CharCode = VK_ESCAPE) and (Alignment <> taLeftJustify) then
  begin
    if CanUndo then
      Text := FOriginalValue;
    PostMessage(Parent.Handle, WM_KEYDOWN, VK_ESCAPE, 0);
  end;

  if (msg.charcode = VK_RETURN) and FReturnIsTab and FGotReturn then
  begin
    msg.CharCode := VK_TAB;
    PostMessage(Handle, WM_KEYDOWN, VK_TAB, 0);
  end;

  inherited;
end;

procedure TAdvCustomMaskEdit.WMKeyUp(var Msg: TWMKeyUp);
//var
//  cf: TCustomForm;
begin
  inherited;

//  if (msg.charcode = VK_RETURN) and FReturnIsTab and FGotReturn then
//  begin
//    if IsWindowVisible(Handle) then
//    begin
//      cf := GetParentForm(self);
//
//      if Assigned(cf) then
//        cf.Perform(WM_NEXTDLGCTL, 0, 0);
//    end;
//  end;

  FGotReturn := false;
end;

function TAdvCustomMaskEdit.GetModified: boolean;
begin
  Result := inherited Modified;
end;

procedure TAdvCustomMaskEdit.SetModified(const Value: boolean);
begin
  if FShowModified then
  begin
    if Value then
      Font.Color := FModifiedColor
    else
      Font.Color := FFontColor;
  end;

  inherited Modified := Value;
end;

procedure TAdvCustomMaskEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  if FLabel <> nil then
    FLabel.Parent := AParent;
end;

procedure TAdvCustomMaskEdit.SetDisabledColor(const Value: TColor);
begin
  FDisabledColor := Value;
  Invalidate;
end;

procedure TAdvCustomMaskEdit.SetDisabledBorder(const Value: boolean);
begin
  FDisabledBorder := Value;
  Invalidate;
end;

function TAdvCustomMaskEdit.GetEnabledEx: Boolean;
begin
  Result := inherited Enabled;
end;

procedure TAdvCustomMaskEdit.SetEnabledEx(const Value: Boolean);
var
  OldValue: Boolean;
  OldColor: TColor;
begin
  OldValue := inherited Enabled;

  inherited Enabled := Value;

  if (csLoading in ComponentState) or
    (csDesigning in ComponentState) then Exit;

  if (OldValue <> Value) then
  begin
    if value then
    begin
      Color := FNormalColor;
    end
    else
    begin
      OldColor := Color;
      Color := FDisabledColor;
      FNormalColor := OldColor;
    end;
  end;

  if Assigned(FLabel) then
    if not FLabelAlwaysEnabled then
      FLabel.Enabled := Value;
end;

function TAdvCustomMaskEdit.GetColorEx: TColor;
begin
  Result := inherited Color;
end;

procedure TAdvCustomMaskEdit.SetColorEx(const Value: TColor);
begin
  if csLoading in ComponentState then
    FLoadedColor := Value;

  inherited Color := Value;
  if not (csLoading in ComponentState) then
    FNormalColor := Value;
end;

procedure TAdvCustomMaskEdit.Loaded;
var
  FOldColor: TColor;
begin
  inherited Loaded;
  FFontColor := Font.Color;
  FOldBorder := BorderStyle;

  FFlat := not FFlat;
  SetFlat(not FFlat);

  if Assigned(FLabel) and not Enabled then
    if not FLabelAlwaysEnabled then
      FLabel.Enabled := False;

  inherited Color := FLoadedColor;
  FNormalColor := FLoadedColor;

  FParentFnt := ParentFont;

  if FlatParentColor and Flat then
    Color := (Parent as TWinControl).Brush.Color;

  if not Enabled then
  begin
    FOldColor := Color;
    Color := FDisabledColor;
    FNormalColor := FOldColor;
  end;

  if (FLabel <> nil) then
    UpdateLabel;

  if ParentFont and Assigned(FLabel) then
    FLabel.Font.Assign(Font);

  if (FLabel <> nil) then
  begin
    UpdateLabel;
    UpdateLabelPos;
  end;

  FLblFntHeight := LabelFont.Height;

{$IFDEF DELPHIXE2_LVL}
  if not (csDesigning in ComponentState) and StyleServices.Enabled and (StyleServices.Name <> 'Windows') then
  begin
    LabelTransparent := true;
  end;
{$ENDIF}

end;

procedure TAdvCustomMaskEdit.SetLabelFont(const Value: TFont);
begin
  FLabelFont.Assign(Value);
  if Assigned(FLabel) then
    FLabel.Font.Assign(FLabelFont);
end;

procedure TAdvCustomMaskEdit.LabelFontChanged(Sender: TObject);
begin
  if Assigned(FLabel) then
  begin
    FLabel.Font.Assign(FLabelFont);
  end;
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    ParentFont := false;

  if not FLblUpdate then
    FLblFntHeight := LabelFont.Height;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));

  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;

end;

function AdvInputQuery(const QueryType: TAdvEditType; QueryParams: PQueryParams; const ACaption, APrompt: string;
  var Value: string): Boolean;

var
  Form: TForm;
  Prompt: TLabel;
  Edit: TCustomAdvEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
  try
    Canvas.Font := Font;
    DialogUnits := GetAveCharSize(Canvas);
    BorderStyle := bsDialog;
    Caption := ACaption;
    ClientWidth := MulDiv(180, DialogUnits.X, 4);
    ClientHeight := MulDiv(63, DialogUnits.Y, 8);
    Position := poScreenCenter;
    Prompt := TLabel.Create(Form);
    with Prompt do
    begin
      Parent := Form;
      AutoSize := True;
      Left := MulDiv(8, DialogUnits.X, 4);
      Top := MulDiv(8, DialogUnits.Y, 8);
      Caption := APrompt;
    end;
    Edit := TCustomAdvEdit.Create(Form);
    with Edit do
    begin
      Parent := Form;
      Left := Prompt.Left;
      Top := MulDiv(19, DialogUnits.Y, 8);
      Width := MulDiv(164, DialogUnits.X, 4);
      MaxLength := 255;
      FocusColor := clNone;
      Init;
      DefaultHandling := false;

      Text := Value;
      SelectAll;
      EditType := QueryType;
      if QueryParams <> nil then
      begin
        Prefix := QueryParams^.Prefix;
        Suffix := QueryParams^.Suffix;
        Precision := QueryParams^.Precision;
        LengthLimit := QueryParams^.LengthLimit;
        Flat := QueryParams^.Flat;
        if Flat then Height := Height - 4;
      end;

    end;
    ButtonTop := MulDiv(41, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(50, DialogUnits.X, 4);
    ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := SMsgDlgOK;
      ModalResult := mrOk;
      Default := True;
      TabStop := true;
      SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
        ButtonHeight);
    end;
    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := SMsgDlgCancel;
      ModalResult := mrCancel;
      Cancel := True;
      TabStop := true;
      SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
        ButtonHeight);
    end;

    if ShowModal = mrOk then
    begin
      Value := Edit.Text;
      Result := True;
    end;
  finally
    Form.Free;
  end;
end;


constructor TRangeList.Create;
begin
  inherited Create;
end;

procedure TRangeList.SetInteger(Index: Integer; Value: Integer);
begin
  inherited Items[Index] := Pointer(Value);
end;

function TRangeList.GetInteger(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

procedure TRangeList.Add(Value: Integer);
begin
  if IndexOf(pointer(value)) = -1 then inherited Add(Pointer(Value));
end;

procedure TRangeList.AddMultiple(Value, Count: Integer);
var
  i: Integer;
begin
  for i := 1 to Count do Add(value + i - 1);
end;


procedure TRangeList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TRangeList.InList(value: integer): Boolean;
begin
  Result := not (IndexOf(pointer(value)) = -1);
end;

procedure TRangeList.Show;
var
  c: Integer;
begin
  for c := 1 to Count do
    OutputDebugString(pchar(inttostr(Items[c - 1])));
end;

function TRangeList.StrToList(s: string): Boolean;
var
  c, code: Integer;
  res: Boolean;

  function DoRange(s: string): Boolean;
  var
    i, i1, i2: Integer;
  begin
    Result := true;
    val(copy(s, 1, Pos('-', s) - 1), i1, code);
    if (code <> 0) then Result := false;
    val(copy(s, Pos('-', s) + 1, Length(s)), i2, code);
    if (code <> 0) then Result := false;
    if result then for i := i1 to i2 do Add(i);
  end;

  function SepPos(s: string): Integer;
  var
    p1, p2: Integer;
  begin
    p1 := Pos(',', s);
    p2 := Pos(';', s);

    if ((p1 < p2) and (p1 > 0)) or (p2 = 0) then Result := p1 else Result := p2;

  end;

begin
  self.Clear;
  res := true;

  while (Length(s) > 0) do
  begin
    if SepPos(s) > 0 then
    begin
      if (Pos('-', s) < SepPos(s)) and (Pos('-', s) > 0) then
      begin
        if not DoRange(copy(s, 1, SepPos(s) - 1)) then res := false;
      end
      else
      begin
        val(copy(s, 1, SepPos(s) - 1), c, code);
        if (code <> 0) then res := false
        else Add(c);
      end;
      system.delete(s, 1, SepPos(s));
    end
    else
    begin
      if Pos('-', s) > 0 then
      begin
        if not DoRange(s) then res := false;
      end
      else
      begin
        val(s, c, code);
        if (code <> 0) then res := false
        else Add(c);
      end;
      s := '';
    end;
  end;
  Result := res;
end;

function TAdvCustomMaskEdit.GetVisible: boolean;
begin
  Result := inherited Visible;
end;

procedure TAdvCustomMaskEdit.SetVisible(const Value: Boolean);
begin
  inherited Visible := Value;
  if (FLabel <> nil) then
    FLabel.Visible := Value;
end;

procedure TAdvCustomMaskEdit.DrawBorder;
var
  DC: HDC;
begin
  if Enabled and not (FFlat or (FBorderColor <> clNone) or (FFocusBorder and FMouseInControl) or Border3D or (FFocusBorderColor <> clNone)) then
    Exit;

  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
  finally
    ReleaseDC(Handle, DC);
  end;
end;


procedure TAdvCustomMaskEdit.DrawControlBorder(DC: HDC);
var
  ARect: TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
begin
  if (csDesigning in ComponentState) then
    Exit;

  if not Enabled and FIsThemed and DisabledBorder then
  begin
    BtnFaceBrush := CreateSolidBrush(ColorToRGB(clSilver));
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    FrameRect(DC, ARect, BtnFaceBrush);
    DeleteObject(BtnFaceBrush);
    Exit;
  end;

  if (FBorderColor <> clNone) then
  begin
    if (GetFocus <> Handle) or (FFocusBorderColor = clNone)  then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
      Exit;
    end;
  end;

  if (FFocusBorderColor <> clNone) then
  begin
    if (GetFocus = Handle) then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FFocusBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
    end;
    Exit;
  end;

  if not Enabled then
    Exit;


  if Is3DBorderButton then
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
    BtnFaceBrush := CreateSolidBrush(ColorToRGB((parent as TWinControl).Brush.color));

  WindowBrush := CreateSolidBrush(GetSysColor(COLOR_WINDOW));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderButton then
    begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
    end
    else
    begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, BtnFaceBrush);
    end;
  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

function TAdvCustomMaskEdit.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);

  Result := (Result and FFocusBorder) or (Border3D);
end;

procedure TAdvCustomMaskEdit.SetFlatLineColor(const Value: TColor);
begin
  FFlatLineColor := Value;
  Invalidate;
end;

procedure TAdvCustomMaskEdit.PaintEdit;
var
  DC: HDC;
  Oldpen: HPen;
  Loc: TRect;

begin

  if FFlat then
  begin
    DC := GetDC(Handle);

    if FFocusBorder and (GetFocus = Handle) then
      DrawControlBorder(DC)
    else
    begin
      OldPen := SelectObject(dc, CreatePen(PS_SOLID, 1, ColorToRGB(FFlatLineColor)));
      SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));

      if FSoftBorder then
      begin
        MovetoEx(DC, Loc.Left - 4 + IndentL, Height - 1, nil);
        LineTo(DC, Width - 1, Height - 1);
        LineTo(DC, Width - 1, Loc.Top - 3);
        LineTo(DC, Loc.Left - 4 + IndentL, Loc.Top - 3);
        LineTo(DC, Loc.Left - 4 + IndentL, Height - 1);
      end
      else
      begin
        MovetoEx(DC, Loc.Left - 2 + IndentL, Height - 1, nil);
        LineTo(DC, Width - IndentR, Height - 1);
      end;

      DeleteObject(SelectObject(DC, OldPen));
    end;

    ReleaseDC(Handle, DC);
  end;
end;

procedure TAdvCustomMaskEdit.SetFlatRect(const Value: Boolean);
var
  loc: TRect;
  lft: integer;
begin
  if HandleAllocated then
  begin

    lft := 0;
    if not Ctl3D then
    begin
      lft := Font.Size div 3;
    end;

    if Value then
    begin
      loc.Left := lft + 4 + IndentL;
      loc.Top := 3;
      loc.Right := Clientrect.Right - 4 - IndentR - lft;
      loc.Bottom := Clientrect.Bottom - 4;
    end
    else
    begin
      loc.Left := lft + IndentL;
      loc.Top := 0;
      loc.Right := ClientRect.Right - IndentR - lft;
      loc.Bottom := ClientRect.Bottom;
    end;

    if not Ctl3D then
    begin
      loc.Left := Loc.Left + 1;
      loc.Right := loc.Right - 1;
    end;

    SendMessage(Handle, EM_SETRECTNP, 0, LParam(@loc));
  end;
end;

procedure TAdvCustomMaskEdit.SetSoftBorder(const Value: Boolean);
begin
  FSoftBorder := Value;
  Invalidate;
end;

procedure TAdvCustomMaskEdit.SetBorder3D(const Value: Boolean);
begin
  FBorder3D := Value;
end;

procedure TAdvCustomMaskEdit.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    if HandleAllocated then
      SendMessage(Handle, WM_NCPAINT, 0,0);
  end;
end;

procedure TAdvCustomMaskEdit.SetFlatParentColor(const Value: Boolean);
begin
  FFlatParentColor := Value;
  Invalidate;
end;

procedure TAdvCustomMaskEdit.CreateWnd;
var
  isScaled: boolean;
  frm: TCustomForm;
begin
  inherited;
  SetFlatRect(FFlat);

  isScaled := true;

  frm := GetParentForm(Self);

  if Assigned(frm) and (frm is TForm) then
    isScaled := (frm as TForm).Scaled;

  if not ParentFont and (LabelCaption <> '') and not (csDesigning in ComponentState) and isScaled then
  begin
    FLblUpdate := true;
    LabelFont.Height := Round(FLblFntHeight * CalculateDPIScale(Self));
    FLblUpdate := false;
  end;

  if Assigned(FLabel) then
    UpdateLabelPos;
end;

function TAdvCustomMaskEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomMaskEdit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvCustomMaskEdit.SetVersion(const Value: string);
begin

end;

procedure TAdvCustomMaskEdit.SetAutoFocus(const Value: boolean);
begin
  FAutoFocus := Value;
end;

{ TEditDropTarget }

constructor TEditDropTarget.Create(AEdit: TCustomAdvEdit);
begin
  inherited Create;
  FAdvEdit := AEdit;
end;

procedure TEditDropTarget.DragMouseMove(pt: TPoint; var Allow: boolean);
begin
  inherited;
  pt := FAdvEdit.ScreenToClient(pt);
  FAdvEdit.DrawCaretByCursor;
end;

procedure TEditDropTarget.DropText(pt: tpoint; s: string);
var
  isCopy: Boolean;
  uchar: Integer;

begin
  inherited;

// do not copy multiline text
  if Pos(#13, s) > 0 then s := copy(s, 1, Pos(#13, s) - 1);
  if Pos(#10, s) > 0 then s := copy(s, 1, Pos(#10, s) - 1);

  if (FAdvEdit.FIsDragSource) then
  begin
    uchar := FAdvEdit.CharFromPos(pt);
    if (uchar >= FAdvEdit.SelStart) and
      (uchar <= FAdvEdit.SelStart + fAdvEdit.SelLength) then
      Exit;
  end;

  isCopy := (getkeystate(vk_control) and $8000 = $8000);

  if (fAdvEdit.fIsDragSource) and not isCopy then
  begin
    fAdvEdit.ClearSelection;
  end;

  FAdvEdit.EraseCaret;
  FAdvEdit.SetCaretByCursor;
  FAdvEdit.SetSelTextBuf(pchar(s));
  FAdvEdit.Invalidate;
end;

procedure Initialize;
//var
//  Result: HRESULT;
begin
  //Result :=
  OleInitialize(nil);
  //Assert(Result in [S_OK, S_FALSE], Format('OleInitialize failed ($%x)', [Result]));
end;


{ TListHintWindow }
constructor TListHintWindow.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TListHintWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;

begin
  inherited CreateParams(Params);
  Params.Style := Params.Style + WS_BORDER;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
    ((Win32MajorVersion > 5) or
    ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;

  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;


destructor TListHintWindow.Destroy;
begin
  inherited;
end;

procedure TListHintWindow.WMActivate(var Message: TMessage);
begin
  inherited;

  if integer(Message.WParam) = integer(False) then
    Hide
  else
    if FListControl.CanFocus and not (csDestroying in FListControl.ComponentState) then
      FListControl.SetFocus;
end;

procedure TListHintWindow.WMNCButtonDown(var Message: TMessage);
begin
  inherited;
end;

procedure TListHintWindow.WMNCHitTest(var Message: TWMNCHitTest);
var
  pt: TPoint;
begin
// Make the hint sizable
  pt := ScreenToClient(Point(Message.XPos, Message.YPos));

  if (pt.X > Width - 10) and (pt.Y > Height - 10) then
    message.Result := HTBOTTOMRIGHT
end;

{ TLookupSettings }

procedure TLookupSettings.AddPair(DisplayText, Value: string);
begin
  FDisplayList.Add(DisplayText);
  FValueList.Add(Value);
end;

procedure TLookupSettings.Assign(Source: TPersistent);
begin
  if (Source is TLookupSettings) then
  begin
    FAcceptOnTab := (Source as TLookupSettings).AcceptOnTab;
    FCaseSensitive := (Source as TLookupSettings).CaseSensitive;
    FColor := (Source as TLookupSettings).Color;
    FDisplayCount := (Source as TLookupSettings).DisplayCount;
    FDisplayList.Assign((Source as TLookupSettings).DisplayList);
    FEnabled := (Source as TLookupSettings).Enabled;
    FHistory := (Source as TLookupSettings).History;
    FNumChars := (Source as TLookupSettings).NumChars;
    FValueList.Assign((Source as TLookupSettings).ValueList);
    FMulti := (Source as TLookupSettings).Multi;
    FSeparator := (Source as TLookupSettings).Separator;
    FShowValue := (Source as TLookupSettings).ShowValue;
    FSearchValue := (Source as TLookupSettings).SearchValue;
    FFont.Assign((Source as TLookupSettings).Font);
    FSpacing := (Source as TLookupSettings).Spacing;
    FValueSeparator := (Source as TLookupSettings).ValueSeparator;
  end;
end;

constructor TLookupSettings.Create;
begin
  inherited Create;
  FDisplayList := TStringList.Create;
  FDisplayList.Duplicates :=  dupIgnore;
  FValueList := TStringList.Create;
  FColor := clWindow;
  FDisplayCount := 4;
  FNumChars := 2;
  FEnabled := False;
  FSeparator := ';';
  FShowValue := False;
  FFont := TFont.Create;
  FFont.Name := 'Arial';
  FFont.Size := 8;
  FFont.Style := [];
  FValueSeparator := vsBracket;
end;

destructor TLookupSettings.Destroy;
begin
  FFont.Free;
  FValueList.Free;
  FDisplayList.Free;
  inherited;
end;

procedure TLookupSettings.SetDisplayList(const Value: TStringList);
begin
  FDisplayList.Assign(Value);
end;

procedure TLookupSettings.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TLookupSettings.SetNumChars(const Value: Integer);
begin
  if Value >= 0 then
    FNumChars := Value
end;

procedure TLookupSettings.SetValueList(const Value: TStringList);
begin
  FValueList.Assign(Value);
end;

{ TListBoxTab }

procedure TListBoxTab.WMGetDlgCode(var Message: TMessage);
begin
  if FHandleTab then
    Message.Result := DLGC_WANTTAB;
end;

{ TCustomAdvEditLabel }

procedure TCustomAdvEditLabel.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if AlwaysEnable and not Enabled then
    Enabled := true;
end;

initialization
  RegisterClass(TCustomAdvEditLabel);
{$IFNDEF TMSDISABLEOLE}
  Initialize;
{$ENDIF}

finalization
{$IFNDEF TMSDISABLEOLE}
  OleUninitialize
{$ENDIF}

end.
