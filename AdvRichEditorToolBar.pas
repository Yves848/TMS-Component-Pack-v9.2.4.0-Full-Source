{*************************************************************************}
{ TMS TAdvRichEditor toolbar                                              }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2019                                       }
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
unit AdvRichEditorToolBar;

interface

uses
  Classes, AdvToolBar, Forms, Graphics, AdvToolBarExt, AdvToolBarRes, AdvRichEditor,
  AdvRichEditorBase, ActnList, AdvOfficeSelectors, AdvOfficeComboBox,
  AdvGlowButton, AdvRichEditorIO;

type
  TAdvRichEditorToolBar = class(TCustomAdvToolBar)
  private
    FRichEditor: TAdvRichEditor;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SelectionBullets(Sender: TObject);
    procedure SelectBullet(Sender: TObject; Index: Integer; Item: TAdvSelectorItem);
    procedure SelectionNumbering(Sender: TObject);
    procedure SelectFontName(Sender: TObject; AName: string);
    procedure SelectFontSize(Sender: TObject; ASize: integer);
    procedure SelectColor(Sender: TObject; AColor: TColor);
    procedure SelectTextColor(Sender: TObject; AColor: TColor);
    procedure ExitFontSize(Sender: TObject);
    procedure ExitFontName(Sender: TObject);
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetButton(Id: integer): TAdvCustomGlowButton;
  published
    property Align;
    property AlignWithMargins;
    property Anchors;
    property RichEditor: TAdvRichEditor read FRichEditor write FRichEditor;
  end;

  // Classic docking toolbars

  TAdvRichEditorToolBarFormatButton = (btBold, btItalic, btUnderline, btStrikeThrough, btSubscript, btSuperScript,
    btInsertPicture, btInsertspecialChar, btBullet, btNumberedBullet, btTextColor, btBackgroundColor, btAlignLeft, btAlignCenter,
    btAlignRight, btInsertHyperlink, btIndent, btUnindent, btFontSizeDown, btFontSizeUp);

  TAdvRichEditorToolBarFormatButtons = set of TAdvRichEditorToolBarFormatButton;

  TAdvRichEditorFormatHints = class(TPersistent)
  private
    FNumberedBulletContent: string;
    FUnderlineContent: string;
    FSubScriptContent: string;
    FItalicTitle: string;
    FInsertHyperlinkTitle: string;
    FBackgroundColorTitle: string;
    FBulletTitle: string;
    FAlignCenterTitle: string;
    FAlignLeftTitle: string;
    FBoldTitle: string;
    FIndentTitle: string;
    FStrikeThroughTitle: string;
    FUnIndentTitle: string;
    FInsertSpecialCharTitle: string;
    FInsertPictureTitle: string;
    FAlignRightTitle: string;
    FSuperScriptTitle: string;
    FItalicContent: string;
    FInsertHyperlinkContent: string;
    FBackgroundColorContent: string;
    FTextColorTitle: string;
    FBulletContent: string;
    FAlignCenterContent: string;
    FAlignLeftContent: string;
    FNumberedBulletTitle: string;
    FBoldContent: string;
    FIndentContent: string;
    FStrikeThroughContent: string;
    FUnderlineTitle: string;
    FUnIndentContent: string;
    FInsertSpecialCharContent: string;
    FSubScriptTitle: string;
    FInsertPictureContent: string;
    FAlignRightContent: string;
    FSuperScriptContent: string;
    FTextColorContent: string;
    FFontSizeDownTitle: string;
    FFontSizeUpTitle: string;
    FFontSizeDownContent: string;
    FFontSizeUpContent: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BoldTitle: string read FBoldTitle write FBoldTitle;
    property BoldContent: string read FBoldContent write FBoldContent;

    property ItalicTitle: string read FItalicTitle write FItalicTitle;
    property ItalicContent: string read FItalicContent write FItalicContent;

    property UnderlineTitle: string read FUnderlineTitle write FUnderlineTitle;
    property UnderlineContent: string read FUnderlineContent write FUnderlineContent;

    property StrikeThroughTitle: string read FStrikeThroughTitle write FStrikeThroughTitle;
    property StrikeThroughContent: string read FStrikeThroughContent write FStrikeThroughContent;

    property SubScriptTitle: string read FSubScriptTitle write FSubScriptTitle;
    property SubScriptContent: string read FSubScriptContent write FSubScriptContent;

    property SuperScriptTitle: string read FSuperScriptTitle write FSuperScriptTitle;
    property SuperScriptContent: string read FSuperScriptContent write FSuperScriptContent;

    property InsertPictureTitle: string read FInsertPictureTitle write FInsertPictureTitle;
    property InsertPictureContent: string read FInsertPictureContent write FInsertPictureContent;

    property InsertSpecialCharTitle: string read FInsertSpecialCharTitle write FInsertSpecialCharTitle;
    property InsertSpecialCharContent: string read FInsertSpecialCharContent write FInsertSpecialCharContent;

    property BulletTitle: string read FBulletTitle write FBulletTitle;
    property BulletContent: string read FBulletContent write FBulletContent;

    property NumberedBulletTitle: string read FNumberedBulletTitle write FNumberedBulletTitle;
    property NumberedBulletContent: string read FNumberedBulletContent write FNumberedBulletContent;

    property TextColorTitle: string read FTextColorTitle write FTextColorTitle;
    property TextColorContent: string read FTextColorContent write FTextColorContent;

    property BackgroundColorTitle: string read FBackgroundColorTitle write FBackgroundColorTitle;
    property BackgroundColorContent: string read FBackgroundColorContent write FBackgroundColorContent;

    property AlignLeftTitle: string read FAlignLeftTitle write FAlignLeftTitle;
    property AlignLeftContent: string read FAlignLeftContent write FAlignLeftContent;

    property AlignCenterTitle: string read FAlignCenterTitle write FAlignCenterTitle;
    property AlignCenterContent: string read FAlignCenterContent write FAlignCenterContent;

    property AlignRightTitle: string read FAlignRightTitle write FAlignRightTitle;
    property AlignRightContent: string read FAlignRightContent write FAlignRightContent;

    property InsertHyperlinkTitle: string read FInsertHyperlinkTitle write FInsertHyperlinkTitle;
    property InsertHyperlinkContent: string read FInsertHyperlinkContent write FInsertHyperlinkContent;

    property IndentTitle: string read FIndentTitle write FIndentTitle;
    property IndentContent: string read FIndentContent write FIndentContent;

    property UnIndentTitle: string read FUnIndentTitle write FUnIndentTitle;
    property UnIndentContent: string read FUnIndentContent write FUnIndentContent;

    property FontSizeUpTitle: string read FFontSizeUpTitle write FFontSizeUpTitle;
    property FontSizeUpContent: string read FFontSizeUpContent write FFontSizeUpContent;

    property FontSizeDownTitle: string read FFontSizeDownTitle write FFontSizeDownTitle;
    property FontSizeDownContent: string read FFontSizeDownContent write FFontSizeDownContent;
  end;

  TAdvRichEditorFormatToolBar = class(TAdvRichEditorToolBar)
  private
    FOptions: TAdvRichEditorToolBarFormatButtons;
    FHints: TAdvRichEditorFormatHints;
    FBackgroundColorPicker: TAdvOfficeColorSelector;
    FTextColorPicker: TAdvOfficeColorSelector;
    FSymbolPicker: TAdvOfficeToolSelector;
    procedure SetHints(const Value: TAdvRichEditorFormatHints);
  protected
    procedure SelectPicture(Sender: TObject);
    procedure SelectIndent(Sender: TObject);
    procedure SelectUnIndent(Sender: TObject);
    procedure ClickSpecialChar(Sender: TObject);
    procedure SelectSpecialChar(Sender: TObject; Index: Integer; Item: TAdvSelectorItem);
    procedure InsertHyperlink(Sender: TObject);
    procedure SetOptions(Value: TAdvRichEditorToolBarFormatButtons);
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
    procedure Loaded; override;
    property TextColorPicker: TAdvOfficeColorSelector read FTextColorPicker;
    property BackgroundColorPicker: TAdvOfficeColorSelector read FBackgroundColorPicker;
    property SymbolPicker: TAdvOfficeToolSelector read FSymbolPicker;
  published
    property Hints: TAdvRichEditorFormatHints read FHints write SetHints;
    property Options: TAdvRichEditorToolBarFormatButtons read FOptions write SetOptions;
  end;

  TAdvRichEditorToolBarEditButton = (btFileOpen, btFileSave, btCopy, btPaste, btCut, btUndo, btRedo);

  TAdvRichEditorToolBarEditButtons = set of TAdvRichEditorToolBarEditButton;

  TAdvRichEditorEditHints = class(TPersistent)
  private
    FPasteContent: string;
    FUndoTitle: string;
    FRedoTitle: string;
    FFileSaveContent: string;
    FCopyTitle: string;
    FFileOpenContent: string;
    FUndoContent: string;
    FRedoContent: string;
    FCopyContent: string;
    FCutTitle: string;
    FPasteTitle: string;
    FFileSaveTitle: string;
    FCutContent: string;
    FFileOpenTitle: string;

  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property FileOpenTitle: string read FFileOpenTitle write FFileOpenTitle;
    property FileOpenContent: string read FFileOpenContent write FFileOpenContent;

    property FileSaveTitle: string read FFileSaveTitle write FFileSaveTitle;
    property FileSaveContent: string read FFileSaveContent write FFileSaveContent;

    property CutTitle: string read FCutTitle write FCutTitle;
    property CutContent: string read FCutContent write FCutContent;

    property CopyTitle: string read FCopyTitle write FCopyTitle;
    property CopyContent: string read FCopyContent write FCopyContent;

    property PasteTitle: string read FPasteTitle write FPasteTitle;
    property PasteContent: string read FPasteContent write FPasteContent;

    property UndoTitle: string read FUndoTitle write FUndoTitle;
    property UndoContent: string read FUndoContent write FUndoContent;

    property RedoTitle: string read FRedoTitle write FRedoTitle;
    property RedoContent: string read FRedoContent write FRedoContent;
  end;


  TAdvRichEditorEditToolBar = class(TAdvRichEditorToolBar)
  private
    FHints: TAdvRichEditorEditHints;
    FOptions: TAdvRichEditorToolBarEditButtons;
    FRecentFileName: string;
    procedure SetHints(const Value: TAdvRichEditorEditHints);
  protected
    procedure OpenFile(Sender: TObject);
    procedure SaveFile(Sender: TObject);
    procedure SaveToHTML(AFileName: string);
    procedure SaveToRTF(AFileName: string);

    procedure SetOptions(Value: TAdvRichEditorToolBarEditButtons);
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
    property RecentFileName: string read FRecentFileName;
    procedure Loaded; override;
  published
    property Hints: TAdvRichEditorEditHints read FHints write SetHints;
    property Options: TAdvRichEditorToolBarEditButtons read FOptions write SetOptions;
  end;


  TAdvRichEditorToolBarEditingButton = (btFind, btReplace, btHighlight, btSelectAll);

  TAdvRichEditorToolBarEditingButtons = set of TAdvRichEditorToolBarEditingButton;

  TAdvRichEditorEditingHints = class(TPersistent)
  private
    FFindTitle: string;
    FReplaceTitle: string;
    FFindContent: string;
    FReplaceContent: string;
    FSelectAllTitle: string;
    FSelectAllContent: string;
    FHighlightTitle: string;
    FHighlightContent: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property FindTitle: string read FFindTitle write FFindTitle;
    property FindContent: string read FFindContent write FFindContent;
    property ReplaceTitle: string read FReplaceTitle write FReplaceTitle;
    property ReplaceContent: string read FReplaceContent write FReplaceContent;
    property SelectAllTitle: string read FSelectAllTitle write FSelectAllTitle;
    property SelectAllContent: string read FSelectAllContent write FSelectAllContent;
    property HighlightTitle: string read FHighlightTitle write FHighlightTitle;
    property HighlightContent: string read FHighlightContent write FHighlightContent;
  end;

  TAdvRichEditorEditingToolBar = class(TAdvRichEditorToolBar)
  private
    FHints: TAdvRichEditorEditingHints;
    FOptions: TAdvRichEditorToolBarEditingButtons;
    procedure SetHints(const Value: TAdvRichEditorEditingHints);
    procedure SetOptions(const Value: TAdvRichEditorToolBarEditingButtons);
  protected
    procedure FindHandler(Sender: TObject);
    procedure ReplaceHandler(Sender: TObject);
    procedure Find(Sender: TObject);
    procedure Replace(Sender: TObject);
    procedure Highlight(Sender: TObject);
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
    procedure Loaded; override;
  published
    property Hints: TAdvRichEditorEditingHints read FHints write SetHints;
    property Options: TAdvRichEditorToolBarEditingButtons read FOptions write SetOptions;
  end;

  // Ribbon toolbars

  TAdvRichEditorClipboardHints = class(TPersistent)
  private
    FPasteContent: string;
    FCopyTitle: string;
    FCopyContent: string;
    FCutTitle: string;
    FPasteTitle: string;
    FCutContent: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property CutTitle: string read FCutTitle write FCutTitle;
    property CutContent: string read FCutContent write FCutContent;
    property CopyTitle: string read FCopyTitle write FCopyTitle;
    property CopyContent: string read FCopyContent write FCopyContent;
    property PasteTitle: string read FPasteTitle write FPasteTitle;
    property PasteContent: string read FPasteContent write FPasteContent;
  end;

  TAdvRichEditorClipboardCaptions = class(TPersistent)
  private
    FCut: string;
    FPaste: string;
    FCopy: string;
    FOnChange: TNotifyEvent;
    procedure SetCopy(const Value: string);
    procedure SetCut(const Value: string);
    procedure SetPaste(const Value: string);
  protected
    procedure DoChanged; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Copy: string read FCopy write SetCopy;
    property Cut: string read FCut write SetCut;
    property Paste: string read FPaste write SetPaste;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvRichEditorClipboardRibbonToolBar = class(TAdvRichEditorToolBar)
  private
    FCut,FCopy,FPaste: TAdvGlowButton;
    FHints: TAdvRichEditorClipboardHints;
    FCaptions: TAdvRichEditorClipboardCaptions;
    procedure SetHints(const Value: TAdvRichEditorClipboardHints);
    procedure SetCaptions(const Value: TAdvRichEditorClipboardCaptions);
  protected
    procedure CreateWnd; override;
    procedure CaptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property AutoPositionControls default false;
    property Captions: TAdvRichEditorClipboardCaptions read FCaptions write SetCaptions;
    property Hints: TAdvRichEditorClipboardHints read FHints write SetHints;
  end;

  TAdvRichEditorFontHints = class(TPersistent)
  private
    FUnderlineContent: string;
    FSubScriptContent: string;
    FItalicTitle: string;
    FBackgroundColorTitle: string;
    FBoldTitle: string;
    FStrikeThroughTitle: string;
    FSuperScriptTitle: string;
    FItalicContent: string;
    FBackgroundColorContent: string;
    FTextColorTitle: string;
    FBoldContent: string;
    FStrikeThroughContent: string;
    FUnderlineTitle: string;
    FSubScriptTitle: string;
    FSuperScriptContent: string;
    FTextColorContent: string;
    FFontSizeDownTitle: string;
    FFontSizeUpTitle: string;
    FFontSizeDownContent: string;
    FFontSizeUpContent: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BoldTitle: string read FBoldTitle write FBoldTitle;
    property BoldContent: string read FBoldContent write FBoldContent;

    property ItalicTitle: string read FItalicTitle write FItalicTitle;
    property ItalicContent: string read FItalicContent write FItalicContent;

    property UnderlineTitle: string read FUnderlineTitle write FUnderlineTitle;
    property UnderlineContent: string read FUnderlineContent write FUnderlineContent;

    property StrikeThroughTitle: string read FStrikeThroughTitle write FStrikeThroughTitle;
    property StrikeThroughContent: string read FStrikeThroughContent write FStrikeThroughContent;

    property SubScriptTitle: string read FSubScriptTitle write FSubScriptTitle;
    property SubScriptContent: string read FSubScriptContent write FSubScriptContent;

    property SuperScriptTitle: string read FSuperScriptTitle write FSuperScriptTitle;
    property SuperScriptContent: string read FSuperScriptContent write FSuperScriptContent;

    property TextColorTitle: string read FTextColorTitle write FTextColorTitle;
    property TextColorContent: string read FTextColorContent write FTextColorContent;

    property BackgroundColorTitle: string read FBackgroundColorTitle write FBackgroundColorTitle;
    property BackgroundColorContent: string read FBackgroundColorContent write FBackgroundColorContent;

    property FontSizeUpTitle: string read FFontSizeUpTitle write FFontSizeUpTitle;
    property FontSizeUpContent: string read FFontSizeUpContent write FFontSizeUpContent;

    property FontSizeDownTitle: string read FFontSizeDownTitle write FFontSizeDownTitle;
    property FontSizeDownContent: string read FFontSizeDownContent write FFontSizeDownContent;
  end;


  TAdvRichEditorFontRibbonToolBar = class(TAdvRichEditorToolBar)
  private
    FHints: TAdvRichEditorFontHints;
    procedure SetHints(const Value: TAdvRichEditorFontHints);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property Hints: TAdvRichEditorFontHints read FHints write SetHints;
  end;

  TAdvRichEditorParagraphHints = class(TPersistent)
  private
    FNumberedBulletContent: string;
    FBulletTitle: string;
    FAlignCenterTitle: string;
    FAlignLeftTitle: string;
    FIndentTitle: string;
    FUnIndentTitle: string;
    FAlignRightTitle: string;
    FBulletContent: string;
    FAlignCenterContent: string;
    FAlignLeftContent: string;
    FNumberedBulletTitle: string;
    FIndentContent: string;
    FUnIndentContent: string;
    FAlignRightContent: string;

  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BulletTitle: string read FBulletTitle write FBulletTitle;
    property BulletContent: string read FBulletContent write FBulletContent;

    property NumberedBulletTitle: string read FNumberedBulletTitle write FNumberedBulletTitle;
    property NumberedBulletContent: string read FNumberedBulletContent write FNumberedBulletContent;

    property AlignLeftTitle: string read FAlignLeftTitle write FAlignLeftTitle;
    property AlignLeftContent: string read FAlignLeftContent write FAlignLeftContent;

    property AlignCenterTitle: string read FAlignCenterTitle write FAlignCenterTitle;
    property AlignCenterContent: string read FAlignCenterContent write FAlignCenterContent;

    property AlignRightTitle: string read FAlignRightTitle write FAlignRightTitle;
    property AlignRightContent: string read FAlignRightContent write FAlignRightContent;

    property IndentTitle: string read FIndentTitle write FIndentTitle;
    property IndentContent: string read FIndentContent write FIndentContent;

    property UnIndentTitle: string read FUnIndentTitle write FUnIndentTitle;
    property UnIndentContent: string read FUnIndentContent write FUnIndentContent;
  end;


  TAdvRichEditorParagraphRibbonToolBar = class(TAdvRichEditorToolBar)
  private
    FHints: TAdvRichEditorParagraphHints;
    procedure SetHints(const Value: TAdvRichEditorParagraphHints);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property Hints: TAdvRichEditorParagraphHints read FHints write SetHints;
  end;

  TAdvRichEditorInsertHints = class(TPersistent)
  private
    FInsertHyperlinkTitle: string;
    FInsertSpecialCharTitle: string;
    FInsertPictureTitle: string;
    FInsertHyperlinkContent: string;
    FInsertSpecialCharContent: string;
    FInsertPictureContent: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property InsertHyperlinkTitle: string read FInsertHyperlinkTitle write FInsertHyperlinkTitle;
    property InsertHyperlinkContent: string read FInsertHyperlinkContent write FInsertHyperlinkContent;

    property InsertPictureTitle: string read FInsertPictureTitle write FInsertPictureTitle;
    property InsertPictureContent: string read FInsertPictureContent write FInsertPictureContent;

    property InsertSpecialCharTitle: string read FInsertSpecialCharTitle write FInsertSpecialCharTitle;
    property InsertSpecialCharContent: string read FInsertSpecialCharContent write FInsertSpecialCharContent;
  end;


  TAdvRichEditorInsertRibbonToolBar = class(TAdvRichEditorToolBar)
  private
    FHints: TAdvRichEditorInsertHints;
    procedure SetHints(const Value: TAdvRichEditorInsertHints);
  protected
    procedure SelectPicture(Sender: TObject);
    procedure ClickSpecialChar(Sender: TObject);
    procedure SelectSpecialChar(Sender: TObject; Index: Integer; Item: TAdvSelectorItem);
    procedure InsertHyperlink(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property Hints: TAdvRichEditorInsertHints read FHints write SetHints;
  end;


  TAdvRichEditorEditingCaptions = class(TPersistent)
  private
    FHighlight: string;
    FReplace: string;
    FSelectAll: string;
    FFind: string;
    FOnChange: TNotifyEvent;
    procedure SetFind(const Value: string);
    procedure SetHighlight(const Value: string);
    procedure SetReplace(const Value: string);
    procedure SetSelectAll(const Value: string);
  protected
    procedure DoChanged;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Find: string read FFind write SetFind;
    property Replace: string read FReplace write SetReplace;
    property SelectAll: string read FSelectAll write SetSelectAll;
    property Highlight: string read FHighlight write SetHighlight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvRichEditorEditingRibbonToolBar = class(TAdvRichEditorToolBar)
  private
    FFind, FReplace, FSelectAll, FHighlight: TAdvGlowButton;
    FHints: TAdvRichEditorEditingHints;
    FCaptions: TAdvRichEditorEditingCaptions;
    FOptions: TAdvRichEditorToolBarEditingButtons;
    procedure SetHints(const Value: TAdvRichEditorEditingHints);
    procedure SetCaptions(const Value: TAdvRichEditorEditingCaptions);
    procedure SetOptions(const Value: TAdvRichEditorToolBarEditingButtons);
  protected
    procedure FindHandler(Sender: TObject);
    procedure ReplaceHandler(Sender: TObject);
    procedure Find(Sender: TObject);
    procedure Replace(Sender: TObject);
    procedure Highlight(Sender: TObject);
    procedure CaptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property Captions: TAdvRichEditorEditingCaptions read FCaptions write SetCaptions;
    property Hints: TAdvRichEditorEditingHints read FHints write SetHints;
    property Options: TAdvRichEditorToolBarEditingButtons read FOptions write SetOptions;
  end;

resourcestring
   MsgText = 'Text';

implementation

uses
  SysUtils, ExtDlgs, Dialogs, PNGImage, JPEG, Math, Windows;

const
  BTNSIZE = 24;

{ TAdvRichEditorToolBar }

procedure TAdvRichEditorFormatToolBar.ClickSpecialChar(Sender: TObject);
var
  ch: char;
  s: string;
begin
  if Assigned(FRichEditor) then
  begin
    s := (Sender as TAdvOfficeToolSelector).Caption;
    if s <> '' then
    begin
      ch := (s[1]);
      FRichEditor.InsertChar(ch);
    end;
  end;
end;

constructor TAdvRichEditorFormatToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  sep: TAdvToolBarSeparator;
  actn: TAction;
  fs: TAdvOfficeFontSelector;
  fss: TAdvOfficeFontSizeSelector;
  cs: TAdvOfficeColorSelector;
  ts: TAdvOfficeToolSelector;

begin
  inherited;

  FHints := TAdvRichEditorFormatHints.Create;

  ShowRightHandle := false;

  fs := TAdvOfficeFontSelector.Create(Self);
  fs.OnSelectFontName := SelectFontName;
  fs.OnExit := ExitFontName;
  actn := TAdvRichEditorFontName.Create(Self);
  fs.Action := actn;
  AddToolBarControl(fs);

  fss := TAdvOfficeFontSizeSelector.Create(Self);
  fss.OnSelectFontSize := SelectFontSize;
  fss.OnExit := ExitFontSize;
  actn := TAdvRichEditorFontSize.Create(Self);
  fss.Action := actn;
  fss.Width := 46;

  AddToolBarControl(fss);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TAdvRichEditorBold, bsCheck, 'TMSRETBBOLD','Bold');
  atb.OfficeHint.Title := Hints.BoldTitle;
  atb.OfficeHint.Notes.Text := Hints.BoldContent;
  atb.Tag := integer(btBold);

  atb := AddButton(HInstance,TAdvRichEditorItalic, bsCheck, 'TMSRETBITALIC','Italic');
  atb.OfficeHint.Title := Hints.ItalicTitle;
  atb.OfficeHint.Notes.Text := Hints.ItalicContent;
  atb.Tag := integer(btItalic);

  atb := AddButton(HInstance,TAdvRichEditorUnderline, bsCheck, 'TMSRETBUNDERLINE','Underline');
  atb.OfficeHint.Title := Hints.UnderlineTitle;
  atb.OfficeHint.Notes.Text := Hints.UnderlineContent;
  atb.Tag := integer(btUnderline);

  atb := AddButton(HInstance,TAdvRichEditorStrikeOut, bsCheck, 'TMSRETBSTRIKE','Strikethrough');
  atb.OfficeHint.Title := Hints.StrikeThroughTitle;
  atb.OfficeHint.Notes.Text := Hints.StrikeThroughContent;
  atb.Tag := integer(btStrikeThrough);

  atb := AddButton(HInstance,TAdvRichEditorSubscript, bsCheck, 'TMSRETBSUBSCR','Subscript');
  atb.OfficeHint.Title := Hints.SubScriptTitle;
  atb.OfficeHint.Notes.Text := Hints.SubScriptContent;
  atb.Tag := integer(btSubscript);

  atb := AddButton(HInstance,TAdvRichEditorSuperscript, bsCheck, 'TMSRETBSUPERSCR','Superscript');
  atb.OfficeHint.Title := Hints.SuperScriptTitle;
  atb.OfficeHint.Notes.Text := Hints.SuperScriptContent;
  atb.Tag := integer(btSuperScript);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TAdvRichEditorAlignLeft, bsCheck, 'TMSRETBALIGNLEFT','Align left');

  atb.OfficeHint.Title := Hints.AlignLeftTitle;
  atb.OfficeHint.Notes.Text := Hints.AlignLeftContent;
  atb.Tag := integer(btAlignLeft);

  atb := AddButton(HInstance,TAdvRichEditorAlignCenter, bscheck, 'TMSRETBALIGNCENTER','Align center');
  atb.OfficeHint.Title := Hints.AlignCenterTitle;
  atb.OfficeHint.Notes.Text := Hints.AlignCenterContent;
  atb.Tag := integer(btAlignCenter);

  atb := AddButton(HInstance,TAdvRichEditorAlignRight, bsCheck, 'TMSRETBALIGNRIGHT','Align right');
  atb.OfficeHint.Title := Hints.AlignRightTitle;
  atb.OfficeHint.Notes.Text := Hints.AlignRightContent;
  atb.Tag := integer(btAlignRight);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  cs := TAdvOfficeColorSelector.Create(Self);
  cs.SelectedColor := clBlack;
  cs.ShowDisabled := false;
  cs.ShowSelectedColor := true;
  cs.OnSelectColor := SelectTextColor;
  cs.Tools.Items[0].BackGroundColor := Font.Color;
  cs.Tag := integer(btTextColor);
  cs.OfficeHint.Title := Hints.TextColorTitle;
  cs.OfficeHint.Notes.Text := Hints.TextColorContent;
  cs.Width := atb.Width;
  cs.Height := atb.Height;

  FTextColorPicker := cs;

  actn := TAdvRichEditorTextColor.Create(Self);
  actn.ActionComponent := cs;
  cs.Action := actn;

  AddToolBarControl(cs);

  cs := TAdvOfficeColorSelector.Create(Self);
  cs.ShowSelectedColor := true;
  cs.OnSelectColor := SelectColor;
  cs.SelectedColor := clWhite;
  cs.ShowDisabled := false;
  cs.Tools.Items[0].BackGroundColor := Color;
  cs.Tag := integer(btBackgroundColor);
  cs.OfficeHint.Title := Hints.BackgroundColorTitle;
  cs.OfficeHint.Notes.Text := Hints.BackgroundColorContent;
  cs.Width := atb.Width;
  cs.Height := atb.Height;

  FBackgroundColorPicker := cs;

  actn := TAdvRichEditorColor.Create(Self);
  actn.ActionComponent := cs;
  cs.Action := actn;

  AddToolBarControl(cs);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBPICTURE','Insert picture');
  atb.OnClick := SelectPicture;
  atb.OfficeHint.Title := Hints.InsertPictureTitle;
  atb.OfficeHint.Notes.Text := Hints.InsertPictureContent;
  atb.Tag := integer(btInsertPicture);

  ts := TAdvOfficeToolSelector.Create(Self);
  ts.DropDownButton := true;
  ts.Width := 32;
  ts.Height := BTNSIZE;
  ts.OwnerDrawToolHeight := 20;
  ts.ShowCaption := true;
  ts.ShowDisabled := false;
  ts.Tools.Add.Caption := '©';
  ts.Tools.Add.Caption := '®';
  ts.Tools.Add.Caption := '™';
  ts.Tools.Add.Caption := '¼';
  ts.Tools.Add.Caption := '½';
  ts.Tools.Add.Caption := '¾';
  ts.Tools.Add.Caption := '±';
  ts.Tools.Add.Caption := '«';
  ts.Tools.Add.Caption := '»';
  ts.SelectedIndex := 0;
  ts.Caption := '©';
  ts.OnClick := ClickSpecialChar;
  ts.OnSelect := SelectSpecialChar;
  ts.Tag := integer(btInsertspecialChar);
  ts.OfficeHint.Title := Hints.InsertSpecialCharTitle;
  ts.OfficeHint.Notes.Text := Hints.InsertSpecialCharContent;
  AddToolBarControl(ts);

  FSymbolPicker := ts;

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBURL','Insert hyperlink');
  atb.OnClick := InsertHyperlink;
  atb.OfficeHint.Title := Hints.InsertHyperlinkTitle;
  atb.OfficeHint.Notes.Text := Hints.InsertHyperlinkContent;
  atb.Tag := integer(btInsertHyperlink);

  ts := TAdvOfficeToolSelector.Create(Self);
  ts.DropDownButton := true;
  ts.ShowDisabled := false;
  ts.Width := 32;
  ts.Height := BTNSIZE;
  ts.Action := TAdvRichEditorBulletType.Create(Self);
  ts.OwnerDrawToolHeight := 20;
  ts.Tag := integer(btBullet);

  ts.Tools.Add.Picture.LoadFromResourceName(HInstance,'TMSRETBCIRCLE');
  ts.Tools.Add.Picture.LoadFromResourceName(HInstance,'TMSRETBSQUARE');
  ts.Tools.Add.Picture.LoadFromResourceName(HInstance,'TMSRETBARROW');
  ts.Tools.Add.Picture.LoadFromResourceName(HInstance,'TMSRETBTICK');
  ts.Tools.Add.Picture.LoadFromResourceName(HInstance,'TMSRETBSTAR');
  ts.SelectedIndex := 0;
  ts.DropDownCheck := true;
  ts.OnClick := SelectionBullets;
  ts.OnSelect := SelectBullet;
  ts.OfficeHint.Title := Hints.BulletTitle;
  ts.OfficeHint.Notes.Text := Hints.BulletContent;
  AddToolBarControl(ts);

  atb := AddButton(HInstance,TAdvRichEditorNumberedBulletType, bsCheck, 'TMSRETBOL','Numbered');
  atb.OfficeHint.Title := Hints.NumberedBulletTitle;
  atb.OfficeHint.Notes.Text := Hints.NumberedBulletContent;
  atb.Tag := integer(btNumberedBullet);

  atb := AddButton(HInstance,TAdvRichEditorUnIndent, bsButton, 'TMSRETBINDENTLESS','Indent less');
  atb.OfficeHint.Title := Hints.UnIndentTitle;
  atb.OfficeHint.Notes.Text := Hints.UnIndentContent;
  atb.Tag := integer(btUnIndent);

  atb := AddButton(HInstance,TAdvRichEditorIndent, bsButton, 'TMSRETBINDENTMORE','Indent more');
  atb.OfficeHint.Title := Hints.IndentTitle;
  atb.OfficeHint.Notes.Text := Hints.IndentContent;
  atb.Tag := integer(btIndent);

  atb := AddButton(HInstance, TAdvRichEditorFontSizeUp, bsButton, 'TMSRETBFNTSZUP', 'Up');
//  atb.OfficeHint.Title := Hints.FontSizeUpTitle;
//  atb.OfficeHint.Notes.Text := Hints.FontSizeUpContent;
  atb.Tag := integer(btFontSizeUp);

  atb := AddButton(HInstance, TAdvRichEditorFontSizeDown, bsButton, 'TMSRETBFNTSZDOWN', 'Down');
//  atb.OfficeHint.Title := Hints.FontSizeDownTitle;
//  atb.OfficeHint.Notes.Text := Hints.FontSizeDownContent;
  atb.Tag := integer(btFontSizeDown);

  Options := [btBold, btItalic, btUnderline, btStrikeThrough, btSubscript, btSuperScript,
    btInsertPicture, btInsertspecialChar, btBullet, btNumberedBullet, btTextColor, btBackgroundColor, btAlignLeft, btAlignCenter,
    btAlignRight, btInsertHyperlink, btIndent, btUnindent];
end;


destructor TAdvRichEditorFormatToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

{ TAdvRichEditorToolBar }

procedure TAdvRichEditorToolBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = RichEditor) then
    FRichEditor := nil;
end;

procedure TAdvRichEditorToolBar.SelectBullet(Sender: TObject;
  Index: Integer; Item: TAdvSelectorItem);
begin
  if Assigned(RichEditor) then
  begin
    RichEditor.SetSelectionBullets(TBulletType(Index));
    RichEditor.SetFocus;
  end;
end;

procedure TAdvRichEditorToolBar.SelectionBullets(Sender: TObject);
begin
  if Assigned(RichEditor) then
  begin
    RichEditor.SetSelectionBullets(TBulletType((Sender as TAdvOfficeToolSelector).SelectedIndex));
    if RichEditor.Visible then
      RichEditor.SetFocus;
  end;
end;

procedure TAdvRichEditorToolBar.SelectionNumbering(Sender: TObject);
begin
  if Assigned(RichEditor) then
  begin
    RichEditor.SetSelectionBullets(btNumber);
    if RichEditor.Visible then
      RichEditor.SetFocus;
  end;
end;

procedure TAdvRichEditorToolBar.SelectColor(Sender: TObject;
  AColor: TColor);
begin
  if Assigned(RichEditor) then
  begin
    RichEditor.SetSelectionBkColor(AColor);
  end;
end;

procedure TAdvRichEditorToolBar.SelectFontName(Sender: TObject;
  AName: string);
begin
  if Assigned(RichEditor) then
  begin
    RichEditor.SetSelectionFontName(AName);
    if RichEditor.Visible then
      RichEditor.SetFocus;
  end;
end;

procedure TAdvRichEditorToolBar.SelectFontSize(Sender: TObject;
  ASize: integer);
begin
  if Assigned(RichEditor) then
  begin
    RichEditor.SetSelectionFontSize(ASize);
    if RichEditor.Visible then
      RichEditor.SetFocus;
  end;
end;

procedure TAdvRichEditorToolBar.SelectTextColor(Sender: TObject;
  AColor: TColor);
begin
  if Assigned(RichEditor) then
  begin
    RichEditor.SetSelectionColor(AColor);
  end;
end;


constructor TAdvRichEditorToolBar.Create(AOwner: TComponent);
var
  i: integer;

begin
  inherited;

  if Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for i := 0 to AOwner.ComponentCount - 1 do
      if (AOwner.Components[i] is TAdvRichEditor) then
      begin
        FRichEditor := AOwner.Components[i] as TAdvRichEditor;
        break;
      end;
  end;
end;

procedure TAdvRichEditorToolBar.CreateWnd;
begin
  inherited;
  AssignGlowButtonsEvent;
end;

procedure TAdvRichEditorToolBar.ExitFontName(Sender: TObject);
var
  fn: string;
  i: integer;
begin
  if not Assigned(RichEditor) then
    Exit;

  if (Sender as TAdvOfficeFontSelector).DroppedDown then
    Exit;

  fn := (Sender as TAdvOfficeFontSelector).Text;

  i := (Sender as TAdvOfficeFontSelector).Items.IndexOf(fn);

  if (i >= 0) then
  begin
    (Sender as TAdvOfficeFontSelector).SelectedFontName := fn;
    RichEditor.SetSelectionFontName(fn);
    RichEditor.SetFocus;
  end;
end;

procedure TAdvRichEditorToolBar.ExitFontSize(Sender: TObject);
var
  fs,e: integer;

begin
  if not Assigned(RichEditor) then
    Exit;

  if (Sender as TAdvOfficeFontSizeSelector).DroppedDown then
    Exit;

  val((Sender as TAdvOfficeFontSizeSelector).Text,fs,e);

  if (e = 0) and (fs > 0) then
  begin
    (Sender as TAdvOfficeFontSizeSelector).SelectedFontSize := fs;
    RichEditor.SetSelectionFontSize(fs);
    RichEditor.SetFocus;
  end;
end;


function TAdvRichEditorToolBar.GetButton(Id: integer): TAdvCustomGlowButton;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to ControlCount - 1 do
  begin
    if (Controls[i].Tag = ID) and (Controls[i] is TAdvCustomGlowButton) then
    begin
      Result := Controls[i] as TAdvCustomGlowButton;
    end;
  end;
end;

{ TAdvRichEditorEditToolBar }

constructor TAdvRichEditorEditToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  sep: TAdvToolBarSeparator;
begin
  inherited;

  FHints := TAdvRichEditorEditHints.Create;

  ShowRightHandle := false;

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBOPEN','Open file');
  atb.OfficeHint.Title := FHints.FileOpenTitle;
  atb.OfficeHint.Notes.Text := FHints.FileOpenContent;
  atb.OnClick := OpenFile;
  atb.Tag := integer(btFileOpen);

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBSAVE','Save');
  atb.OfficeHint.Title := FHints.FileSaveTitle;
  atb.OfficeHint.Notes.Text := FHints.FileSaveContent;
  atb.OnClick := SaveFile;
  atb.Tag := integer(btFileSave);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TAdvRichEditorCut, bsButton, 'TMSRETBCUT', 'Cut to clipboard');
  atb.OfficeHint.Title := FHints.CutTitle;
  atb.OfficeHint.Notes.Text := FHints.CutContent;
  atb.Tag := integer(btCut);

  atb := AddButton(HInstance,TAdvRichEditorCopy, bsButton, 'TMSRETBCOPY', 'Copy to clipboard');
  atb.OfficeHint.Title := FHints.CopyTitle;
  atb.OfficeHint.Notes.Text := FHints.CopyContent;
  atb.Tag := integer(btCopy);

  atb := AddButton(HInstance,TAdvRichEditorPaste, bsButton, 'TMSRETBPASTE', 'Paste from clipboard');
  atb.OfficeHint.Title := FHints.PasteTitle;
  atb.OfficeHint.Notes.Text := FHints.PasteContent;
  atb.Tag := integer(btPaste);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TAdvRichEditorUndo, bsButton, 'TMSRETBUNDO', 'Undo');
  atb.OfficeHint.Title := FHints.UndoTitle;
  atb.OfficeHint.Notes.Text := FHints.UndoContent;
  atb.Tag := integer(btUndo);

  atb := AddButton(HInstance,TAdvRichEditorRedo, bsButton, 'TMSRETBREDO', 'Redo');
  atb.OfficeHint.Title := FHints.RedoTitle;
  atb.OfficeHint.Notes.Text := FHints.RedoContent;
  atb.Tag := integer(btRedo);

  Options := [btFileOpen, btFileSave, btCopy, btPaste, btCut, btUndo, btRedo];
end;

destructor TAdvRichEditorEditToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TAdvRichEditorEditToolBar.Loaded;
begin
  inherited;
  UpdateHints;
end;

procedure TAdvRichEditorEditToolBar.OpenFile(Sender: TObject);
var
  od: TOpenDialog;
  fe: string;
  RTFIO: TAdvRichEditorRTFIO;
begin
  od := TOpenDialog.Create(Self);
  od.Filter := 'Text files|*.txt|RTF files|*.rtf|RTE files|*.rte|All files|*.*';
  try
    if od.Execute then
      if Assigned(FRichEditor) then
      begin
        FRecentFileName := od.FileName;
        fe := Uppercase(ExtractFileExt(od.FileName));

        if fe = '.TXT' then
          FRichEditor.LoadFromTextFile(od.FileName)
        else
        if fe = '.RTF' then
        begin
          RTFIO := TAdvRichEditorRTFIO.Create(Self);
          try
            RTFIO.RichEditor := FRichEditor;
            FRichEditor.Clear;
            RTFIO.Load(od.FileName);
          finally
            RTFIO.Free;
          end;
        end
        else
          FRichEditor.LoadFromFile(od.FileName);
      end;
  finally
    od.Free;
  end;
end;

procedure TAdvRichEditorEditToolBar.SaveFile(Sender: TObject);
var
  sd: TSaveDialog;
  fe,fn: string;
begin
  sd := TSaveDIalog.Create(Self);
  sd.Filter := 'Text files|*.txt|RTE files|*.rte|HTML files|*.htm|Rich text|*.rtf';
  try
    if sd.Execute then
    begin
      FRecentFileName := sd.FileName;
      fn := sd.FileName;
      fe := Uppercase(ExtractFileExt(sd.FileName));

      if (fe = '') then
      begin
        if sd.FilterIndex = 1 then
          fn := fn + '.txt';
        if sd.FilterIndex = 2 then
          fn := fn + '.rte';
        if sd.FilterIndex = 3 then
          fn := fn + '.htm';
        if sd.FilterIndex = 4 then
          fn := fn + '.rtf';
      end;

      case sd.FilterIndex of
      1: FRichEditor.SaveToText(fn);
      2: FRichEditor.SaveToFile(fn);
      3: SaveToHTML(fn);
      4: SaveToRTF(fn);
      end;
    end;
  finally
    sd.Free;
  end;
end;

procedure TAdvRichEditorEditToolBar.SaveToHTML(AFileName: string);
var
  html: TAdvRichEditorHTMLIO;
begin
  html := TAdvRichEditorHTMLIO.Create(Self);
  try
    html.RichEditor := FRichEditor;
    html.Save(AFileName,ExtractFilePath(AFileName));
  finally
    html.Free;
  end;
end;

procedure TAdvRichEditorEditToolBar.SaveToRTF(AFileName: string);
var
  rtf: TAdvRichEditorRTFIO;
begin
  rtf := TAdvRichEditorRTFIO.Create(Self);
  try
    rtf.RichEditor := FRichEditor;
    rtf.Save(AFileName);
  finally
    rtf.Free;
  end;
end;

procedure TAdvRichEditorFormatToolBar.InsertHyperlink(Sender: TObject);
var
  url: string;
begin
  if Assigned(RichEditor) then
  begin
    url := '';
    if Assigned(RichEditor.Caret) and Assigned(RichEditor.Caret.Element) then
      url := RichEditor.Caret.Element.URL;
    InputQuery('Hyperlink','URL',url);
    RichEditor.SetSelectionHyperlink(url);
  end;
end;

procedure TAdvRichEditorFormatToolBar.Loaded;
begin
  inherited;
  UpdateHints;
end;

procedure TAdvRichEditorFormatToolBar.SelectIndent(Sender: TObject);
begin
  if Assigned(RichEditor) then
  begin
    RichEditor.SetSelectionIndent(100);
    RichEditor.SetFocus;
  end;
end;

procedure TAdvRichEditorFormatToolBar.SelectPicture(Sender: TObject);
var
  pd: TOpenPictureDialog;
begin
  if Assigned(RichEditor) then
  begin
    pd := TOpenPictureDialog.Create(Self);
    try
      if pd.Execute then
      begin
        RichEditor.InsertImage(pd.FileName);
        RichEditor.SetFocus;
      end;
    finally
      pd.Free;
    end;
  end;
end;

procedure TAdvRichEditorFormatToolBar.SelectSpecialChar(Sender: TObject;
  Index: Integer; Item: TAdvSelectorItem);
var
  ch: char;
  s: string;
begin
  if Assigned(FRichEditor) then
  begin
    s := (Sender as TAdvOfficeToolSelector).Tools[Index].Caption;
    if s <> '' then
    begin
      ch := (s[1]);
      FRichEditor.InsertChar(ch);
    end;
  end;
end;


procedure TAdvRichEditorFormatToolBar.SelectUnIndent(Sender: TObject);
begin
  if Assigned(RichEditor) then
  begin
    RichEditor.SetSelectionIndent(-100);
    RichEditor.SetFocus;
  end;
end;


procedure TAdvRichEditorFormatToolBar.SetHints(
  const Value: TAdvRichEditorFormatHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvRichEditorFormatToolBar.SetOptions(
  Value: TAdvRichEditorToolBarFormatButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TAdvRichEditorFormatToolBar.UpdateButtons;
var
  i: integer;
  j: TAdvRichEditorToolBarFormatButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TAdvRichEditorToolBarFormatButton) to High(TAdvRichEditorToolBarFormatButton) do
    begin
      if Controls[i].Tag = integer(j) then
      begin
        Controls[i].Visible := j in Options;
      end;
    end;
  end;
end;

procedure TAdvRichEditorFormatToolBar.UpdateHints;
var
  agb: TAdvCustomGlowButton;
begin
  agb := GetButton(integer(btBold));
  agb.Hint := Hints.BoldTitle;
  agb.OfficeHint.Title := Hints.BoldTitle;
  agb.OfficeHint.Notes.Text := Hints.BoldContent;

  agb := GetButton(integer(btItalic));
  agb.Hint := Hints.ItalicTitle;
  agb.OfficeHint.Title := Hints.ItalicTitle;
  agb.OfficeHint.Notes.Text := Hints.ItalicContent;

  agb := GetButton(integer(btUnderline));
  agb.Hint := Hints.UnderlineTitle;
  agb.OfficeHint.Title := Hints.UnderlineTitle;
  agb.OfficeHint.Notes.Text := Hints.UnderlineContent;

  agb := GetButton(integer(btStrikeThrough));
  agb.Hint := Hints.StrikeThroughTitle;
  agb.OfficeHint.Title := Hints.StrikeThroughTitle;
  agb.OfficeHint.Notes.Text := Hints.StrikeThroughContent;

  agb := GetButton(integer(btSubscript));
  agb.Hint := Hints.SubscriptTitle;
  agb.OfficeHint.Title := Hints.SubscriptTitle;
  agb.OfficeHint.Notes.Text := Hints.SubscriptContent;

  agb := GetButton(integer(btSuperScript));
  agb.Hint := Hints.SuperScriptTitle;
  agb.OfficeHint.Title := Hints.SuperScriptTitle;
  agb.OfficeHint.Notes.Text := Hints.SuperScriptContent;

  agb := GetButton(integer(btTextColor));
  agb.Hint := Hints.TextColorTitle;
  agb.OfficeHint.Title := Hints.TextColorTitle;
  agb.OfficeHint.Notes.Text := Hints.TextColorContent;

  agb := GetButton(integer(btBackgroundColor));
  agb.Hint := Hints.BackgroundColorTitle;
  agb.OfficeHint.Title := Hints.BackgroundColorTitle;
  agb.OfficeHint.Notes.Text := Hints.BackgroundColorContent;

  agb := GetButton(integer(btInsertPicture));
  agb.Hint := Hints.InsertPictureTitle;
  agb.OfficeHint.Title := Hints.InsertPictureTitle;
  agb.OfficeHint.Notes.Text := Hints.InsertPictureContent;

  agb := GetButton(integer(btInsertspecialChar));
  agb.Hint := Hints.InsertSpecialCharTitle;
  agb.OfficeHint.Title := Hints.InsertSpecialCharTitle;
  agb.OfficeHint.Notes.Text := Hints.InsertSpecialCharContent;

  agb := GetButton(integer(btBullet));
  agb.Hint := Hints.BulletTitle;
  agb.OfficeHint.Title := Hints.BulletTitle;
  agb.OfficeHint.Notes.Text := Hints.BulletContent;

  agb := GetButton(integer(btNumberedBullet));
  agb.Hint := Hints.NumberedBulletTitle;
  agb.OfficeHint.Title := Hints.NumberedBulletTitle;
  agb.OfficeHint.Notes.Text := Hints.NumberedBulletContent;

  agb := GetButton(integer(btAlignLeft));
  agb.Hint := Hints.AlignLeftTitle;
  agb.OfficeHint.Title := Hints.AlignLeftTitle;
  agb.OfficeHint.Notes.Text := Hints.AlignLeftContent;

  agb := GetButton(integer(btAlignCenter));
  agb.Hint := Hints.AlignCenterTitle;
  agb.OfficeHint.Title := Hints.AlignCenterTitle;
  agb.OfficeHint.Notes.Text := Hints.AlignCenterContent;

  agb := GetButton(integer(btAlignRight));
  agb.Hint := Hints.AlignRightTitle;
  agb.OfficeHint.Title := Hints.AlignRightTitle;
  agb.OfficeHint.Notes.Text := Hints.AlignRightContent;

  agb := GetButton(integer(btInsertHyperlink));
  agb.Hint := Hints.InsertHyperlinkTitle;
  agb.OfficeHint.Title := Hints.InsertHyperlinkTitle;
  agb.OfficeHint.Notes.Text := Hints.InsertHyperlinkContent;

  agb := GetButton(integer(btIndent));
  agb.Hint := Hints.IndentTitle;
  agb.OfficeHint.Title := Hints.IndentTitle;
  agb.OfficeHint.Notes.Text := Hints.IndentContent;

  agb := GetButton(integer(btUnIndent));
  agb.Hint := Hints.UnIndentTitle;
  agb.OfficeHint.Title := Hints.UnIndentTitle;
  agb.OfficeHint.Notes.Text := Hints.UnIndentContent;
end;

procedure TAdvRichEditorEditToolBar.SetHints(
  const Value: TAdvRichEditorEditHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvRichEditorEditToolBar.SetOptions(
  Value: TAdvRichEditorToolBarEditButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TAdvRichEditorEditToolBar.UpdateButtons;
var
  i: integer;
  j: TAdvRichEditorToolBarEditButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TAdvRichEditorToolBarEditButton) to High(TAdvRichEditorToolBarEditButton) do
    begin
      if Controls[i].Tag = integer(j) then
        Controls[i].Visible := j in Options;
    end;
  end;
end;

procedure TAdvRichEditorEditToolBar.UpdateHints;
var
  agb: TAdvCustomGlowButton;
begin
  agb := GetButton(integer(btFileOpen));
  agb.Hint := Hints.FileOpenTitle;
  agb.OfficeHint.Title := Hints.FileOpenTitle;
  agb.OfficeHint.Notes.Text := Hints.FileOpenContent;

  agb := GetButton(integer(btFileSave));
  agb.Hint := Hints.FileSaveTitle;
  agb.OfficeHint.Title := Hints.FileSaveTitle;
  agb.OfficeHint.Notes.Text := Hints.FileSaveContent;

  agb := GetButton(integer(btCopy));
  agb.Hint := Hints.CopyTitle;
  agb.OfficeHint.Title := Hints.CopyTitle;
  agb.OfficeHint.Notes.Text := Hints.CopyContent;

  agb := GetButton(integer(btCut));
  agb.Hint := Hints.CutTitle;
  agb.OfficeHint.Title := Hints.CutTitle;
  agb.OfficeHint.Notes.Text := Hints.CutContent;

  agb := GetButton(integer(btPaste));
  agb.Hint := Hints.PasteTitle;
  agb.OfficeHint.Title := Hints.PasteTitle;
  agb.OfficeHint.Notes.Text := Hints.PasteContent;

  agb := GetButton(integer(btUndo));
  agb.Hint := Hints.UndoTitle;
  agb.OfficeHint.Title := Hints.UndoTitle;
  agb.OfficeHint.Notes.Text := Hints.UndoContent;

  agb := GetButton(integer(btRedo));
  agb.Hint := Hints.RedoTitle;
  agb.OfficeHint.Title := Hints.RedoTitle;
  agb.OfficeHint.Notes.Text := Hints.RedoContent;
end;

{ TAdvRichEditorClipboardRibbonToolBar }

procedure TAdvRichEditorClipboardRibbonToolBar.CaptionsChanged(Sender: TObject);
begin
  if Assigned(FCut) then
    FCut.Caption := Captions.Cut;
  if Assigned(FCopy) then
    FCopy.Caption := Captions.Copy;
  if Assigned(FPaste) then
    FPaste.Caption := Captions.Paste;
end;

constructor TAdvRichEditorClipboardRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TAdvRichEditorClipboardHints.Create;
  FCaptions := TAdvRichEditorClipboardCaptions.Create;
  FCaptions.OnChange := CaptionsChanged;

  Caption := 'Clipboard';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance, TAdvRichEditorPaste, bsButton, 'TMSRETBPASTELARGE', 'Paste from clipboard');
  atb.Tag := integer(btPaste);
  atb.Hint := FHints.PasteTitle;
  atb.OfficeHint.Title := FHints.PasteTitle;
  atb.OfficeHint.Notes.Text := FHints.PasteContent;
  atb.Caption := FCaptions.Paste;
  atb.ShowCaption := true;
  atb.DropDownButton := true;
  atb.DropDownPosition := dpBottom;
  atb.MinButtonSizeState := bsLarge;
  atb.MaxButtonSizeState := bsLarge;
  atb.Layout := blGlyphTop;
  atb.ShortCutHint := 'V';
  atb.Width := 40;
  atb.Height := 64;

  FPaste := atb;

  atb := AddButton(HInstance, TAdvRichEditorCut, bsButton, 'TMSRETBCUT', 'Cut to clipboard');
  atb.Caption := 'Cut';
  atb.Hint := FHints.CutTitle;
  atb.OfficeHint.Title := FHints.CutTitle;
  atb.OfficeHint.Notes.Text := FHints.CutContent;
  atb.ShowCaption := true;
  atb.Tag := integer(btCut);
  atb.ShortCutHint := 'X';
  atb.Width := 60;
  atb.Height := 24;
  atb.Left := 41;
  atb.Top := 2;

  FCut := atb;

  atb := AddButton(HInstance, TAdvRichEditorCopy, bsButton, 'TMSRETBCOPY', 'Copy to clipboard');
  atb.Hint := FHints.CopyTitle;
  atb.OfficeHint.Title := FHints.CopyTitle;
  atb.OfficeHint.Notes.Text := FHints.CopyContent;
  atb.Caption := 'Copy';
  atb.ShowCaption := true;
  atb.Tag := integer(btCopy);
  atb.ShortCutHint := 'C';
  atb.Width := 60;
  atb.Height := 24;
  atb.Left := 41;
  atb.Top := 26;

  FCopy := atb;

  Width := 102;
  Height := 85;
end;

procedure TAdvRichEditorClipboardRibbonToolBar.CreateWnd;
begin
  inherited;
  AutoPositionControls := false;
  ToolBarState := tsFixed;
  FCopy.Top := 24;
  FCopy.Left := 41;
end;

destructor TAdvRichEditorClipboardRibbonToolBar.Destroy;
begin
  FHints.Free;
  FCaptions.Free;
  inherited;
end;

procedure TAdvRichEditorClipboardRibbonToolBar.SetCaptions(
  const Value: TAdvRichEditorClipboardCaptions);
begin
  FCaptions := Value;
end;

procedure TAdvRichEditorClipboardRibbonToolBar.SetHints(
  const Value: TAdvRichEditorClipboardHints);
begin
  FHints := Value;
end;

procedure TAdvRichEditorClipboardRibbonToolBar.UpdateHints;
var
  agb: TAdvCustomGlowButton;
begin
  agb := GetButton(integer(btCopy));
  agb.Hint := FHints.CopyTitle;
  agb.OfficeHint.Title := FHints.CopyTitle;
  agb.OfficeHint.Notes.Text := FHints.CopyContent;

  agb := GetButton(integer(btPaste));
  agb.Hint := FHints.PasteTitle;
  agb.OfficeHint.Title := FHints.PasteTitle;
  agb.OfficeHint.Notes.Text := FHints.PasteContent;

  agb := GetButton(integer(btCut));
  agb.Hint := FHints.CutTitle;
  agb.OfficeHint.Title := FHints.CutTitle;
  agb.OfficeHint.Notes.Text := FHints.CutContent;
end;

{ TAdvRichEditorFontRibbonToolBar }

constructor TAdvRichEditorFontRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  fs: TAdvOfficeFontSelector;
  fss: TAdvOfficeFontSizeSelector;
  cs: TAdvOfficeColorSelector;
  actn: TAction;
  l: integer;
begin
  inherited;

  FHints := TAdvRichEditorFontHints.Create;

  Caption:= 'Font';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  fs := TAdvOfficeFontSelector.Create(Self);
  fs.OnSelectFontName := SelectFontName;
  fs.OnExit := ExitFontName;
  actn := TAdvRichEditorFontName.Create(Self);
  fs.Action := actn;
  AddToolBarControl(fs);
  fs.Left := 2;
  fs.Top := 2;

  fss := TAdvOfficeFontSizeSelector.Create(Self);
  fss.OnSelectFontSize := SelectFontSize;
  fss.OnExit := ExitFontSize;
  actn := TAdvRichEditorFontSize.Create(Self);
  fss.Action := actn;
  AddToolBarControl(fss);
  fss.Left := fs.Left + fs.Width;
  fss.Top := 2;
  fss.Width := 46;
  l := fss.Left + fss.Width;

  atb := AddButton(HInstance, TAdvRichEditorFontSizeUp, bsButton, 'TMSRETBFNTSZUP', 'Up');
  atb.Left := l;
  atb.Top := 2;
  atb.Hint := FHints.FontSizeUpTitle;
  atb.OfficeHint.Title := FHints.FontSizeUpTitle;
  atb.OfficeHint.Notes.Text := FHints.FontSizeUpContent;
  atb.Tag := integer(btFontSizeUp);
  atb.ShortCutHint := 'SU';
  actn := TAdvRichEditorFontSizeUp.Create(Self);
  atb.Action := actn;

  atb := AddButton(HInstance, TAdvRichEditorFontSizeDown, bsButton, 'TMSRETBFNTSZDOWN', 'Down');
  atb.Left := l + 24;
  atb.Top := 2;
  atb.Hint := FHints.FontSizeDownTitle;
  atb.OfficeHint.Title := FHints.FontSizeDownTitle;
  atb.OfficeHint.Notes.Text := FHints.FontSizeDownContent;
  atb.Tag := integer(btFontSizeDown);
  atb.ShortCutHint := 'SD';
  actn := TAdvRichEditorFontSizeDown.Create(Self);
  atb.Action := actn;

  atb := AddButton(HInstance, TAdvRichEditorBold, bsCheck, 'TMSRETBBOLD', 'Bold');
  atb.Hint := FHints.BoldTitle;
  atb.OfficeHint.Title := FHints.BoldTitle;
  atb.OfficeHint.Notes.Text := FHints.BoldContent;
  atb.ShortCutHint := '1';
  atb.Left := 2;
  atb.Top := 26;
  atb.Position := bpLeft;

  atb := AddButton(HInstance, TAdvRichEditorItalic, bsCheck, 'TMSRETBITALIC', 'Italic');
  atb.Hint := FHints.ItalicTitle;
  atb.OfficeHint.Title := FHints.ItalicTitle;
  atb.OfficeHint.Notes.Text := FHints.ItalicContent;
  atb.ShortCutHint := '2';
  atb.Left := 26;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance, TAdvRichEditorUnderline, bsCheck, 'TMSRETBUNDERLINE', 'Underline');
  atb.Hint := FHints.UnderlineTitle;
  atb.OfficeHint.Title := FHints.UnderlineTitle;
  atb.OfficeHint.Notes.Text := FHints.UnderlineContent;
  atb.ShortCutHint := '3';
  atb.Left := 50;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance, TAdvRichEditorStrikeout, bsCheck, 'TMSRETBSTRIKE', 'Strikeout');
  atb.Hint := FHints.StrikeThroughTitle;
  atb.OfficeHint.Title := FHints.StrikeThroughTitle;
  atb.OfficeHint.Notes.Text := FHints.StrikeThroughContent;
  atb.ShortCutHint := '4';
  atb.Left := 74;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance, TAdvRichEditorSubScript, bsCheck, 'TMSRETBSUBSCR', 'SubScript');
  atb.Hint := FHints.SubScriptTitle;
  atb.OfficeHint.Title := FHints.SubScriptTitle;
  atb.OfficeHint.Notes.Text := FHints.SubScriptContent;
  atb.ShortCutHint := '5';
  atb.Left := 98;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance, TAdvRichEditorSuperScript, bsCheck, 'TMSRETBSUPERSCR', 'SuperScript');
  atb.Hint := FHints.SuperScriptTitle;
  atb.OfficeHint.Title := FHints.SuperScriptTitle;
  atb.OfficeHint.Notes.Text := FHints.SuperScriptContent;
  atb.ShortCutHint := '6';
  atb.Left := 122;
  atb.Top := 26;
  atb.Position := bpRight;

  cs := TAdvOfficeColorSelector.Create(Self);
  cs.OfficeHint.Title := FHints.TextColorTitle;
  cs.OfficeHint.Notes.Text := FHints.TextColorContent;
  cs.SelectedColor := clBlack;
  cs.ShowDisabled := false;
  cs.OnSelectColor := SelectTextColor;
  cs.Tools.Items[0].BackGroundColor := Font.Color;
  cs.Tag := integer(btTextColor);
  cs.OfficeHint.Title := 'Text color';
  cs.OfficeHint.Notes.Text := 'Set selection text color';
  cs.Top := 26;
  cs.Left := 150;
  cs.Width := 24;
  cs.Height := 24;
  cs.Position := bpLeft;

//  actn := TAdvRichEditorTextColor.Create(Self);
//  actn.ActionComponent := cs;
//  cs.Action := actn;

  AddToolBarControl(cs);

  cs := TAdvOfficeColorSelector.Create(Self);
  cs.Hint := FHints.BackgroundColorTitle;
  cs.OfficeHint.Title := FHints.BackgroundColorTitle;
  cs.OfficeHint.Notes.Text := FHints.BackgroundColorContent;
  cs.OnSelectColor := SelectColor;
  cs.SelectedColor := clWhite;
  cs.ShowDisabled := false;
  cs.Tools.Items[0].BackGroundColor := Color;
  cs.Tag := integer(btBackgroundColor);
  cs.Top := 26;
  cs.Left := 174;
  cs.Width := 24;
  cs.Height := 24;
  cs.Position := bpRight;
  AddToolBarControl(cs);

  Width := 225;
  Height := 85;
end;

destructor TAdvRichEditorFontRibbonToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TAdvRichEditorFontRibbonToolBar.SetHints(
  const Value: TAdvRichEditorFontHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvRichEditorFontRibbonToolBar.UpdateHints;
var
  agb: TAdvCustomGlowButton;
begin
  agb := GetButton(integer(btFontSizeDown));
  agb.Hint := FHints.FontSizeDownTitle;
  agb.OfficeHint.Title := FHints.FontSizeDownTitle;
  agb.OfficeHint.Notes.Text := FHints.FontSizeDownContent;

  agb := GetButton(integer(btFontSizeUp));
  agb.Hint := FHints.FontSizeUpTitle;
  agb.OfficeHint.Title := FHints.FontSizeUpTitle;
  agb.OfficeHint.Notes.Text := FHints.FontSizeUpContent;

  agb := GetButton(integer(btBold));
  agb.Hint := FHints.BoldTitle;
  agb.OfficeHint.Title := FHints.BoldTitle;
  agb.OfficeHint.Notes.Text := FHints.BoldContent;

  agb := GetButton(integer(btItalic));
  agb.Hint := FHints.ItalicTitle;
  agb.OfficeHint.Title := FHints.ItalicTitle;
  agb.OfficeHint.Notes.Text := FHints.ItalicContent;

  agb := GetButton(integer(btUnderline));
  agb.Hint := FHints.UnderlineTitle;
  agb.OfficeHint.Title := FHints.UnderlineTitle;
  agb.OfficeHint.Notes.Text := FHints.UnderlineContent;

  agb := GetButton(integer(btStrikeThrough));
  agb.Hint := FHints.StrikeThroughTitle;
  agb.OfficeHint.Title := FHints.StrikeThroughTitle;
  agb.OfficeHint.Notes.Text := FHints.StrikeThroughContent;

  agb := GetButton(integer(btSubscript));
  agb.Hint := FHints.SubScriptTitle;
  agb.OfficeHint.Title := FHints.SubScriptTitle;
  agb.OfficeHint.Notes.Text := FHints.SubScriptContent;

  agb := GetButton(integer(btSuperscript));
  agb.Hint := FHints.SuperScriptTitle;
  agb.OfficeHint.Title := FHints.SuperScriptTitle;
  agb.OfficeHint.Notes.Text := FHints.SuperScriptContent;

end;

{ TAdvRichEditorParagraphRibbonToolBar }

constructor TAdvRichEditorParagraphRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  ts: TAdvOfficeToolSelector;
begin
  inherited;

  FHints := TAdvRichEditorParagraphHints.Create;

  Caption := 'Paragraph';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance,TAdvRichEditorAlignLeft, bsCheck, 'TMSRETBALIGNLEFT','Align left');
  atb.Hint := FHints.AlignLeftTitle;
  atb.OfficeHint.Title := FHints.AlignLeftTitle;
  atb.OfficeHint.Notes.Text := FHints.AlignLeftContent;
  atb.Tag := integer(btAlignLeft);
  atb.ShortCutHint := 'AL';
  atb.Left := 2;
  atb.Top := 26;
  atb.Position := bpLeft;

  atb := AddButton(HInstance,TAdvRichEditorAlignCenter, bscheck, 'TMSRETBALIGNCENTER','Align center');
  atb.Hint := FHints.AlignCenterTitle;
  atb.OfficeHint.Title := FHints.AlignCenterTitle;
  atb.OfficeHint.Notes.Text := FHints.AlignCenterContent;
  atb.Tag := integer(btAlignCenter);
  atb.ShortCutHint := 'AC';
  atb.Left := 26;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance,TAdvRichEditorAlignRight, bsCheck, 'TMSRETBALIGNRIGHT','Align right');
  atb.Hint := FHints.AlignRightTitle;
  atb.OfficeHint.Title := FHints.AlignRightTitle;
  atb.OfficeHint.Notes.Text := FHints.AlignRightContent;
  atb.ShortCutHint := 'AR';
  atb.Left := 50;
  atb.Top := 26;
  atb.Tag := integer(btAlignRight);
  atb.Position := bpRight;

  ts := TAdvOfficeToolSelector.Create(Self);
  ts.DropDownButton := true;
  ts.ShowDisabled := false;
  ts.Width := 32;
  ts.Height := BTNSIZE;
  ts.Action := TAdvRichEditorBulletType.Create(Self);
  ts.OwnerDrawToolHeight := 20;
  ts.Tag := integer(btBullet);

  ts.Tools.Add.Picture.LoadFromResourceName(HInstance,'TMSRETBCIRCLE');
  ts.Tools.Add.Picture.LoadFromResourceName(HInstance,'TMSRETBSQUARE');
  ts.Tools.Add.Picture.LoadFromResourceName(HInstance,'TMSRETBARROW');
  ts.Tools.Add.Picture.LoadFromResourceName(HInstance,'TMSRETBTICK');
  ts.Tools.Add.Picture.LoadFromResourceName(HInstance,'TMSRETBSTAR');
  ts.SelectedIndex := 0;
  ts.DropDownCheck := true;
  ts.OnClick := SelectionBullets;
  ts.OnSelect := SelectBullet;
  ts.OfficeHint.Title := FHints.BulletTitle;
  ts.ShortCutHint := 'U';
  ts.OfficeHint.Notes.Text := FHints.BulletContent;
  AddToolBarControl(ts);

  atb := AddButton(HInstance, TAdvRichEditorNumberedBulletType, bsCheck, 'TMSRETBOL','Numbered');
  atb.Hint := FHints.NumberedBulletTitle;
  atb.OfficeHint.Title := FHints.NumberedBulletTitle;
  atb.OfficeHint.Notes.Text := FHints.NumberedBulletContent;
  atb.ShortCutHint := 'N';
  atb.Tag := integer(btNumberedBullet);
  atb.Left := 36;
  atb.Top := 2;

  atb := AddButton(HInstance, TAdvRichEditorUnIndent, bsButton, 'TMSRETBINDENTLESS','Indent less');
  atb.Hint := FHints.UnIndentTitle;
  atb.OfficeHint.Title := FHints.UnIndentTitle;
  atb.OfficeHint.Notes.Text := FHints.UnIndentContent;
  atb.ShortCutHint := 'AO';
  atb.Tag := integer(btUnIndent);
  atb.Left := 60;
  atb.Top := 2;

  atb := AddButton(HInstance, TAdvRichEditorIndent, bsButton, 'TMSRETBINDENTMORE','Indent more');
  atb.Hint := FHints.IndentTitle;
  atb.OfficeHint.Title := FHints.IndentTitle;
  atb.OfficeHint.Notes.Text := FHints.IndentContent;
  atb.ShortCutHint := 'AI';
  atb.Tag := integer(btIndent);
  atb.Left := 84;
  atb.Top := 2;

  Width := 110;
  Height := 85;
end;


destructor TAdvRichEditorParagraphRibbonToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TAdvRichEditorParagraphRibbonToolBar.SetHints(
  const Value: TAdvRichEditorParagraphHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvRichEditorParagraphRibbonToolBar.UpdateHints;
var
  agb: TAdvCustomGlowButton;
begin
  agb := GetButton(integer(btAlignLeft));
  agb.Hint := FHints.AlignLeftTitle;
  agb.OfficeHint.Title := FHints.AlignLeftTitle;
  agb.OfficeHint.Notes.Text := FHints.AlignLeftContent;

  agb := GetButton(integer(btAlignRight));
  agb.Hint := FHints.AlignRightTitle;
  agb.OfficeHint.Title := FHints.AlignRightTitle;
  agb.OfficeHint.Notes.Text := FHints.AlignRightContent;

  agb := GetButton(integer(btAlignCenter));
  agb.Hint := FHints.AlignCenterTitle;
  agb.OfficeHint.Title := FHints.AlignCenterTitle;
  agb.OfficeHint.Notes.Text := FHints.AlignCenterContent;

  agb := GetButton(integer(btNumberedBullet));
  agb.Hint := FHints.NumberedBulletTitle;
  agb.OfficeHint.Title := FHints.NumberedBulletTitle;
  agb.OfficeHint.Notes.Text := FHints.NumberedBulletContent;

  agb := GetButton(integer(btIndent));
  agb.Hint := FHints.IndentTitle;
  agb.OfficeHint.Title := FHints.IndentTitle;
  agb.OfficeHint.Notes.Text := FHints.IndentContent;

  agb := GetButton(integer(btUnIndent));
  agb.Hint := FHints.UnIndentTitle;
  agb.OfficeHint.Title := FHints.UnIndentTitle;
  agb.OfficeHint.Notes.Text := FHints.UnIndentContent;
end;

{ TAdvRichEditorFormatHints }

procedure TAdvRichEditorFormatHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorFormatHints) then
  begin
    FNumberedBulletContent := (Source as TAdvRichEditorFormatHints).NumberedBulletContent;
    FUnderlineContent := (Source as TAdvRichEditorFormatHints).UnderlineContent;
    FSubScriptContent := (Source as TAdvRichEditorFormatHints).SubScriptContent;
    FItalicTitle := (Source as TAdvRichEditorFormatHints).ItalicTitle;
    FInsertHyperlinkTitle := (Source as TAdvRichEditorFormatHints).InsertHyperlinkTitle;
    FBackgroundColorTitle := (Source as TAdvRichEditorFormatHints).BackgroundColorTitle;
    FBulletTitle := (Source as TAdvRichEditorFormatHints).BulletTitle;
    FAlignCenterTitle := (Source as TAdvRichEditorFormatHints).AlignCenterTitle;
    FAlignLeftTitle := (Source as TAdvRichEditorFormatHints).AlignLeftTitle;
    FBoldTitle := (Source as TAdvRichEditorFormatHints).BoldTitle;
    FIndentTitle := (Source as TAdvRichEditorFormatHints).IndentTitle;
    FStrikeThroughTitle := (Source as TAdvRichEditorFormatHints).StrikeThroughTitle;
    FUnIndentTitle := (Source as TAdvRichEditorFormatHints).UnIndentTitle;
    FInsertSpecialCharTitle := (Source as TAdvRichEditorFormatHints).InsertSpecialCharTitle;
    FInsertPictureTitle := (Source as TAdvRichEditorFormatHints).InsertSpecialCharTitle;
    FAlignRightTitle := (Source as TAdvRichEditorFormatHints).AlignRightTitle;
    FSuperScriptTitle := (Source as TAdvRichEditorFormatHints).SuperScriptTitle;
    FItalicContent := (Source as TAdvRichEditorFormatHints).ItalicContent;
    FInsertHyperlinkContent := (Source as TAdvRichEditorFormatHints).InsertHyperlinkContent;
    FBackgroundColorContent := (Source as TAdvRichEditorFormatHints).BackgroundColorContent;
    FTextColorTitle := (Source as TAdvRichEditorFormatHints).TextColorTitle;
    FBulletContent := (Source as TAdvRichEditorFormatHints).BulletContent;
    FAlignCenterContent := (Source as TAdvRichEditorFormatHints).AlignCenterContent;
    FAlignLeftContent := (Source as TAdvRichEditorFormatHints).AlignLeftContent;
    FNumberedBulletTitle := (Source as TAdvRichEditorFormatHints).NumberedBulletTitle;
    FBoldContent := (Source as TAdvRichEditorFormatHints).BoldContent;
    FIndentContent := (Source as TAdvRichEditorFormatHints).IndentContent;
    FStrikeThroughContent := (Source as TAdvRichEditorFormatHints).StrikeThroughContent;
    FUnderlineTitle := (Source as TAdvRichEditorFormatHints).UnderlineTitle;
    FUnIndentContent := (Source as TAdvRichEditorFormatHints).UnIndentContent;
    FInsertSpecialCharContent := (Source as TAdvRichEditorFormatHints).InsertSpecialCharContent;
    FSubScriptTitle := (Source as TAdvRichEditorFormatHints).SubScriptTitle;
    FInsertPictureContent := (Source as TAdvRichEditorFormatHints).InsertPictureContent;
    FAlignRightContent := (Source as TAdvRichEditorFormatHints).AlignRightContent;
    FSuperScriptContent := (Source as TAdvRichEditorFormatHints).SuperScriptContent;
    FTextColorContent := (Source as TAdvRichEditorFormatHints).TextColorContent;
    FFontSizeDownTitle := (Source as TAdvRichEditorFormatHints).FontSizeDownTitle;
    FFontSizeUpTitle := (Source as TAdvRichEditorFormatHints).FontSizeUpTitle;
    FFontSizeDownContent := (Source as TAdvRichEditorFormatHints).FontSizeDownContent;
    FFontSizeUpContent := (Source as TAdvRichEditorFormatHints).FontSizeUpContent;
  end;
end;

constructor TAdvRichEditorFormatHints.Create;
begin
  inherited;

  FItalicTitle := 'Italic (Ctrl+I)';
  FItalicContent := 'Select italic font style';

  FBoldTitle := 'Bold (Ctrl+B)';
  FBoldContent := 'Select bold font style';

  FStrikeThroughTitle := 'Strikethrough';
  FUnderlineTitle := 'Underline (Ctrl+U)';

  FUnderlineContent := 'Select underline font style';
  FStrikeThroughContent := 'Select strikethrough font style';

  FSuperScriptTitle := 'Superscript';
  FSuperScriptContent := 'Set superscript text';

  FSubScriptTitle := 'Subscript';
  FSubScriptContent := 'Set subscript text';

  FAlignLeftTitle := 'Align text left (Ctrl+L)';
  FAlignLeftContent := 'Align the text to left';

  FAlignCenterTitle := 'Align center (Ctrl+E)';
  FAlignCenterContent := 'Center text';

  FAlignRightTitle := 'Align text right (Ctrl+R)';
  FAlignRightContent := 'Align the text to right';

  FBulletTitle := 'Insert bullet';
  FBulletContent := 'Insert a bullet for list';

  FNumberedBulletTitle := 'Start list';
  FNumberedBulletContent := 'Start a numbered list';

  FIndentTitle := 'Increase indent';
  FIndentContent := 'Increase the indent level of the paragraph';

  FUnIndentTitle := 'Decrease indent';
  FUnIndentContent := 'Decrease the indent level of the paragraph';

  FInsertPictureTitle := 'Insert picture';
  FInsertPictureContent := 'Insert a picture from file';

  FBackgroundColorTitle := 'Background color';
  FBackgroundColorContent := 'Set selection background color';

  FTextColorTitle := 'Text color';
  FTextColorContent := 'Set selection text color';

  FInsertHyperlinkTitle := 'Set hyperlink';
  FInsertHyperlinkContent := 'Set hyperlink for text';

  FInsertSpecialCharTitle := 'Insert special character';
  FInsertSpecialCharContent := 'Insert a special character';

  FFontSizeUpTitle := 'Increase font size';
  FFontSizeUpContent := 'Make your text a bit bigger';

  FFontSizeDownTitle := 'Decrease font size';
  FFontSizeDownContent := 'Make your text a bit smaller';
end;

{ TAdvRichEditorEditHints }

procedure TAdvRichEditorEditHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorEditHints) then
  begin
    FPasteContent := (Source as TAdvRichEditorEditHints).PasteContent;
    FUndoTitle := (Source as TAdvRichEditorEditHints).UndoTitle;
    FRedoTitle := (Source as TAdvRichEditorEditHints).RedoTitle;
    FFileSaveContent := (Source as TAdvRichEditorEditHints).FileSaveContent;
    FCopyTitle := (Source as TAdvRichEditorEditHints).CopyTitle;
    FFileOpenContent := (Source as TAdvRichEditorEditHints).FileOpenContent;
    FUndoContent := (Source as TAdvRichEditorEditHints).UndoContent;
    FRedoContent := (Source as TAdvRichEditorEditHints).RedoContent;
    FCopyContent := (Source as TAdvRichEditorEditHints).CopyContent;
    FCutTitle := (Source as TAdvRichEditorEditHints).CutTitle;
    FPasteTitle := (Source as TAdvRichEditorEditHints).PasteTitle;
    FFileSaveTitle := (Source as TAdvRichEditorEditHints).FileSaveTitle;
    FCutContent := (Source as TAdvRichEditorEditHints).CutContent;
    FFileOpenTitle := (Source as TAdvRichEditorEditHints).FileOpenTitle;
  end;

end;

constructor TAdvRichEditorEditHints.Create;
begin
  inherited;

  FPasteTitle := 'Paste (Ctrl+V)';
  FPasteContent := 'Add content on the clipboard to your document';

  FCopyTitle := 'Copy (Ctrl+C)';
  FCopyContent := 'Put a copy of the selection on the clipboard';

  FCutTitle := 'Cut (Ctrl+X)';
  FCutContent := 'Remove the selection to the clipboard';

  FUndoTitle := 'Undo (Ctrl+Z)';
  FUndoContent := 'Undo typing';

  FRedoTitle := 'Redo (Ctrl+Y)';
  FRedoContent := 'Redo typing';

  FFileSaveTitle := 'Save (Ctrl+S)';
  FFileSaveContent := 'Save document to file';

  FFileOpenTitle := 'Open (Ctrl+O)';
  FFileOpenContent := 'Open new document from file';
end;

{ TAdvRichEditorParagraphHints }

procedure TAdvRichEditorParagraphHints.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TAdvRichEditorParagraphHints.Create;
begin
  inherited;

  FAlignLeftTitle := 'Align text left (Ctrl+L)';
  FAlignLeftContent := 'Align the text to left';

  FAlignCenterTitle := 'Align center (Ctrl+E)';
  FAlignCenterContent := 'Center text';

  FAlignRightTitle := 'Align text right (Ctrl+R)';
  FAlignRightContent := 'Align the text to right';

  FBulletTitle := 'Insert bullet';
  FBulletContent := 'Insert a bullet for list';

  FNumberedBulletTitle := 'Start list';
  FNumberedBulletContent := 'Start a numbered list';

  FIndentTitle := 'Increase indent';
  FIndentContent := 'Increase the indent level of the paragraph';

  FUnIndentTitle := 'Decrease indent';
  FUnIndentContent := 'Decrease the indent level of the paragraph';

end;

{ TAdvRichEditorFontHints }

procedure TAdvRichEditorFontHints.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TAdvRichEditorFontHints.Create;
begin
  inherited Create;

  FItalicTitle := 'Italic (Ctrl+I)';
  FItalicContent := 'Select italic font style';

  FBoldTitle := 'Bold (Ctrl+B)';
  FBoldContent := 'Select bold font style';

  FStrikeThroughTitle := 'Strikethrough';
  FUnderlineTitle := 'Underline (Ctrl+U)';

  FUnderlineContent := 'Select underline font style';
  FStrikeThroughContent := 'Select strikethrough font style';

  FSuperScriptTitle := 'Superscript';
  FSuperScriptContent := 'Set superscript text';

  FSubScriptTitle := 'Subscript';
  FSubScriptContent := 'Set subscript text';

  FBackgroundColorTitle := 'Background color';
  FBackgroundColorContent := 'Set selection background color';

  FTextColorTitle := 'Text color';
  FTextColorContent := 'Set selection text color';

  FFontSizeUpTitle := 'Increase font size';
  FFontSizeUpContent := 'Make your text a bit bigger';

  FFontSizeDownTitle := 'Decrease font size';
  FFontSizeDownContent := 'Make your text a bit smaller';
end;

{ TAdvRichEditorClipboardHints }

procedure TAdvRichEditorClipboardHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorClipboardHints) then
  begin
    FCutTitle := (Source as TAdvRichEditorClipboardHints).CutTitle;
    FCutContent := (Source as TAdvRichEditorClipboardHints).CutContent;
    FCopyTitle := (Source as TAdvRichEditorClipboardHints).CopyTitle;
    FCopyContent := (Source as TAdvRichEditorClipboardHints).CopyContent;
    FPasteTitle := (Source as TAdvRichEditorClipboardHints).PasteTitle;
    FPasteContent := (Source as TAdvRichEditorClipboardHints).PasteContent;
  end;
end;

constructor TAdvRichEditorClipboardHints.Create;
begin
  inherited;

  FPasteTitle := 'Paste (Ctrl+V)';
  FPasteContent := 'Add content on the clipboard to your document';

  FCopyTitle := 'Copy (Ctrl+C)';
  FCopyContent := 'Put a copy of the selection on the clipboard';

  FCutTitle := 'Cut (Ctrl+X)';
  FCutContent := 'Remove the selection to the clipboard';

end;

{ TAdvRichEditorInsertRibbonToolBar }

procedure TAdvRichEditorInsertRibbonToolBar.ClickSpecialChar(Sender: TObject);
var
  ch: char;
  s: string;
begin
  if Assigned(FRichEditor) then
  begin
    s := (Sender as TAdvOfficeToolSelector).Caption;
    if s <> '' then
    begin
      ch := (s[1]);
      FRichEditor.InsertChar(ch);
    end;
  end;
end;

constructor TAdvRichEditorInsertRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  ts: TAdvOfficeToolSelector;
begin
  inherited;
  FHints := TAdvRichEditorInsertHints.Create;

  Caption := 'Insert';

  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBPICTURE','Insert picture');
  atb.OnClick := SelectPicture;
  atb.Hint := Hints.InsertPictureTitle;
  atb.OfficeHint.Title := Hints.InsertPictureTitle;
  atb.OfficeHint.Notes.Text := Hints.InsertPictureContent;
  atb.Tag := integer(btInsertPicture);
  atb.Left := 2;
  atb.Top := 2;


  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBURL','Insert hyperlink');
  atb.OnClick := InsertHyperlink;
  atb.Hint := Hints.InsertHyperlinkTitle;
  atb.OfficeHint.Title := Hints.InsertHyperlinkTitle;
  atb.OfficeHint.Notes.Text := Hints.InsertHyperlinkContent;
  atb.Tag := integer(btInsertHyperlink);
  atb.Left := 28;
  atb.Top := 2;


  ts := TAdvOfficeToolSelector.Create(Self);
  ts.DropDownButton := true;
  ts.Width := 32;
  ts.Height := BTNSIZE;
  ts.OwnerDrawToolHeight := 20;
  ts.ShowCaption := true;
  ts.ShowDisabled := false;
  ts.Tools.Add.Caption := '©';
  ts.Tools.Add.Caption := '®';
  ts.Tools.Add.Caption := '™';
  ts.Tools.Add.Caption := '¼';
  ts.Tools.Add.Caption := '½';
  ts.Tools.Add.Caption := '¾';
  ts.Tools.Add.Caption := '±';
  ts.Tools.Add.Caption := '«';
  ts.Tools.Add.Caption := '»';
  ts.SelectedIndex := 0;
  ts.Caption := '©';
  ts.OnClick := ClickSpecialChar;
  ts.OnSelect := SelectSpecialChar;
  ts.Tag := integer(btInsertspecialChar);
  ts.Hint := Hints.InsertSpecialCharTitle;
  ts.OfficeHint.Title := Hints.InsertSpecialCharTitle;
  ts.OfficeHint.Notes.Text := Hints.InsertSpecialCharContent;

  ts.Left := 54;
  ts.Top := 2;

  AddToolBarControl(ts);

  Width := 88;
  Height := 85;
end;

destructor TAdvRichEditorInsertRibbonToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TAdvRichEditorInsertRibbonToolBar.InsertHyperlink(Sender: TObject);
var
  url: string;
begin
  if Assigned(RichEditor) then
  begin
    url := '';
    InputQuery('Hyperlink','URL',url);
    RichEditor.SetSelectionHyperlink(url);
  end;
end;

procedure TAdvRichEditorInsertRibbonToolBar.SelectPicture(Sender: TObject);
var
  pd: TOpenPictureDialog;
begin
  if Assigned(RichEditor) then
  begin
    pd := TOpenPictureDialog.Create(Self);
    try
      if pd.Execute then
      begin
        RichEditor.InsertImage(pd.FileName);
        RichEditor.SetFocus;
      end;
    finally
      pd.Free;
    end;
  end;
end;

procedure TAdvRichEditorInsertRibbonToolBar.SelectSpecialChar(Sender: TObject;
  Index: Integer; Item: TAdvSelectorItem);
var
  ch: char;
  s: string;
begin
  if Assigned(FRichEditor) then
  begin
    s := (Sender as TAdvOfficeToolSelector).Tools[Index].Caption;
    if s <> '' then
    begin
      ch := (s[1]);
      FRichEditor.InsertChar(ch);
    end;
  end;
end;

procedure TAdvRichEditorInsertRibbonToolBar.SetHints(
  const Value: TAdvRichEditorInsertHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvRichEditorInsertRibbonToolBar.UpdateHints;
var
  agb: TAdvCustomGlowButton;
begin
  agb := GetButton(integer(btInsertPicture));
  agb.Hint := Hints.InsertPictureTitle;
  agb.OfficeHint.Title := Hints.InsertPictureTitle;
  agb.OfficeHint.Notes.Text := Hints.InsertPictureContent;

  agb := GetButton(integer(btInsertHyperlink));
  agb.Hint := Hints.InsertHyperlinkTitle;
  agb.OfficeHint.Title := Hints.InsertHyperlinkTitle;
  agb.OfficeHint.Notes.Text := Hints.InsertHyperlinkContent;

  agb := GetButton(integer(btInsertspecialChar));
  agb.Hint := Hints.InsertSpecialCharTitle;
  agb.OfficeHint.Title := Hints.InsertSpecialCharTitle;
  agb.OfficeHint.Notes.Text := Hints.InsertSpecialCharContent;
end;

{ TAdvRichEditorInsertHints }

procedure TAdvRichEditorInsertHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorInsertHints) then
  begin
    FInsertHyperlinkTitle := (Source as TAdvRichEditorInsertHints).InsertHyperlinkTitle;
    FInsertHyperlinkContent := (Source as TAdvRichEditorInsertHints).InsertHyperlinkContent;

    FInsertPictureTitle := (Source as TAdvRichEditorInsertHints).InsertPictureTitle;
    FInsertPictureContent := (Source as TAdvRichEditorInsertHints).InsertPictureContent;

    FInsertSpecialCharTitle := (Source as TAdvRichEditorInsertHints).InsertSpecialCharTitle;
    FInsertSpecialCharContent := (Source as TAdvRichEditorInsertHints).InsertSpecialCharContent;
  end;
end;

constructor TAdvRichEditorInsertHints.Create;
begin
  inherited;

  FInsertHyperlinkTitle := 'Set hyperlink';
  FInsertHyperlinkContent := 'Set hyperlink for text';

  FInsertSpecialCharTitle := 'Insert special character';
  FInsertSpecialCharContent := 'Insert a special character';

  FInsertPictureTitle := 'Insert picture';
  FInsertPictureContent := 'Insert a picture from file';
end;

{ TAdvRichEditorEditingHints }

procedure TAdvRichEditorEditingHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorEditingHints) then
  begin
    FindTitle := (Source as TAdvRichEditorEditingHints).FindTitle;
    FindContent := (Source as TAdvRichEditorEditingHints).FindContent;
    ReplaceTitle := (Source as TAdvRichEditorEditingHints).ReplaceTitle;
    ReplaceContent := (Source as TAdvRichEditorEditingHints).ReplaceContent;
    SelectAllTitle := (Source as TAdvRichEditorEditingHints).SelectAllTitle;
    SelectAllContent := (Source as TAdvRichEditorEditingHints).SelectAllContent;
    HighlightTitle := (Source as TAdvRichEditorEditingHints).HighlightTitle;
    HighlightContent := (Source as TAdvRichEditorEditingHints).HighlightContent;
  end;
end;

constructor TAdvRichEditorEditingHints.Create;
begin
  inherited;
  FindTitle := '&Find';
  FindContent := 'Finds text in the document';
  ReplaceTitle := 'Re&place';
  ReplaceContent := 'Replaces occurrences of text';
  SelectAllTitle := 'Select All (Ctrl+A)';
  SelectAllContent := 'Selects all text in the document';
  HighlightTitle := 'Highlight';
  HighlightContent := 'Highlights text in the document';
end;

{ TAdvRichEditorEditingToolBar }

constructor TAdvRichEditorEditingToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TAdvRichEditorEditingHints.Create;

  ShowRightHandle := false;

  atb := AddButton(HInstance, nil, bsButton, 'TMSRETBFIND','Find');
  atb.Hint := Hints.FindTitle;
  atb.OfficeHint.Title := Hints.FindTitle;
  atb.OfficeHint.Notes.Text := Hints.FindContent;
  atb.Tag := integer(btFind);
  atb.OnClick := Find;

  atb := AddButton(HInstance, nil, bsButton, 'TMSRETBREPLACE','Replace');
  atb.Hint := Hints.ReplaceTitle;
  atb.OfficeHint.Title := Hints.ReplaceTitle;
  atb.OfficeHint.Notes.Text := Hints.ReplaceContent;
  atb.Tag := integer(btReplace);
  atb.OnClick := Replace;

  atb := AddButton(HInstance, TAdvRichEditorSelectAll, bsButton, 'TMSRETBSELALL','SelectAll');
  atb.Hint := Hints.SelectAllTitle;
  atb.OfficeHint.Title := Hints.SelectAllTitle;
  atb.OfficeHint.Notes.Text := Hints.SelectAllContent;
  atb.Tag := integer(btSelectAll);

  atb := AddButton(HInstance, nil, bsButton, 'TMSRETBHIGHLIGHT','Highlight');
  atb.Hint := Hints.HighlightTitle;
  atb.OfficeHint.Title := Hints.HighlightTitle;
  atb.OfficeHint.Notes.Text := Hints.HighlightContent;
  atb.Tag := integer(btHighlight);
  atb.OnClick := Highlight;

  Options := [btFind, btReplace, btSelectAll, btHighlight];
end;

destructor TAdvRichEditorEditingToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TAdvRichEditorEditingToolBar.Find(Sender: TObject);
var
  fd: TFindDialog;
begin
  fd := TFindDialog.Create(Self);

  fd.Options := fd.Options - [frWholeWord] + [frHideWholeWord, frHideUpDown];
  fd.OnFind := FindHandler;

  fd.Execute;
end;

procedure TAdvRichEditorEditingToolBar.FindHandler(Sender: TObject);
begin
  if Assigned(RichEditor) then
    RichEditor.Find((Sender as TFindDialog).FindText, frMatchCase in (Sender as TFindDialog).Options);
end;

procedure TAdvRichEditorEditingToolBar.Highlight(Sender: TObject);
var
  s: string;
begin
  if Assigned(RichEditor) then
  begin
    if InputQuery(Hints.HighlightTitle, MsgText,s) then
    begin
      if (s <> '') then
        RichEditor.Highlight(s)
      else
        RichEditor.UnHighlight;
    end;
  end;
end;

procedure TAdvRichEditorEditingToolBar.Loaded;
begin
  inherited;
  UpdateHints;
end;

procedure TAdvRichEditorEditingToolBar.Replace(Sender: TObject);
var
  fd: TReplaceDialog;
begin
  fd := TReplaceDialog.Create(Self);
  fd.Options := fd.Options - [frWholeWord] + [frHideWholeWord, frHideUpDown];
  fd.OnFind := FindHandler;
  fd.OnReplace := ReplaceHandler;
  fd.Execute;
end;

procedure TAdvRichEditorEditingToolBar.ReplaceHandler(Sender: TObject);
begin
  if Assigned(RichEditor) then
  begin
    if frReplaceAll in (Sender as TReplaceDialog).Options then
      RichEditor.ReplaceAll((Sender as TReplaceDialog).FindText,(Sender as TReplaceDialog).ReplaceText, frMatchCase in (Sender as TReplaceDialog).Options)
    else
      RichEditor.Replace((Sender as TReplaceDialog).FindText,(Sender as TReplaceDialog).ReplaceText, frMatchCase in (Sender as TReplaceDialog).Options);
  end;
end;

procedure TAdvRichEditorEditingToolBar.SetHints(
  const Value: TAdvRichEditorEditingHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvRichEditorEditingToolBar.SetOptions(
  const Value: TAdvRichEditorToolBarEditingButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TAdvRichEditorEditingToolBar.UpdateButtons;
var
  i: integer;
  j: TAdvRichEditorToolBarEditingButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TAdvRichEditorToolBarEditingButton) to High(TAdvRichEditorToolBarEditingButton) do
    begin
      if Controls[i].Tag = integer(j) then
      begin
        Controls[i].Visible := j in Options;
      end;
    end;
  end;

end;

procedure TAdvRichEditorEditingToolBar.UpdateHints;
var
  atb: TAdvCustomGlowButton;
begin
  atb := GetButton(integer(btFind));
  atb.Hint := Hints.FindTitle;
  atb.OfficeHint.Title := Hints.FindTitle;
  atb.OfficeHint.Notes.Text := Hints.FindContent;

  atb := GetButton(integer(btReplace));
  atb.Hint := Hints.ReplaceTitle;
  atb.OfficeHint.Title := Hints.ReplaceTitle;
  atb.OfficeHint.Notes.Text := Hints.ReplaceContent;

  atb := GetButton(integer(btSelectAll));
  atb.Hint := Hints.SelectAllTitle;
  atb.OfficeHint.Title := Hints.SelectAllTitle;
  atb.OfficeHint.Notes.Text := Hints.SelectAllContent;

  atb := GetButton(integer(btHighlight));
  atb.Hint := Hints.HighlightTitle;
  atb.OfficeHint.Title := Hints.HighlightTitle;
  atb.OfficeHint.Notes.Text := Hints.HighlightContent;
end;

{ TAdvRichEditorEditingRibbonToolBar }

procedure TAdvRichEditorEditingRibbonToolBar.CaptionsChanged(Sender: TObject);
begin
  FFind.Caption := Captions.Find;
  FReplace.Caption := Captions.Replace;
  FSelectAll.Caption := Captions.SelectAll;
  FHighlight.Caption := Captions.Highlight;
end;

constructor TAdvRichEditorEditingRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TAdvRichEditorEditingHints.Create;

  FCaptions := TAdvRichEditorEditingCaptions.Create;
  FCaptions.OnChange := CaptionsChanged;

  Caption := 'Editing';

  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBFINDL','Find');
  atb.OnClick := Find;
  atb.Caption := FCaptions.Find;
  atb.ShowCaption := true;
  atb.Layout := blGlyphTopAdjusted;
  atb.Hint := Hints.FindTitle;
  atb.OfficeHint.Title := Hints.FindTitle;
  atb.OfficeHint.Notes.Text := Hints.FindContent;
  atb.Tag := integer(btFind);
  atb.ShortCutHint := 'F';
  atb.Left := 2;
  atb.Top := 2;
  atb.Width := 40;
  atb.Height := 64;

  FFind := atb;

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBREPLACEL','Replace');
  atb.OnClick := Replace;
  atb.Caption := FCaptions.Replace;
  atb.ShowCaption := true;
  atb.Layout := blGlyphTopAdjusted;
  atb.Hint := Hints.ReplaceTitle;
  atb.OfficeHint.Title := Hints.ReplaceTitle;
  atb.OfficeHint.Notes.Text := Hints.ReplaceContent;
  atb.Tag := integer(btReplace);
  atb.ShortCutHint := 'R';
  atb.Left := 42;
  atb.Top := 2;
  atb.Width := 44;
  atb.Height := 64;

  FReplace := atb;

  atb := AddButton(HInstance, TAdvRichEditorSelectAll, bsButton, 'TMSRETBSELALLL','SelectAll');
  atb.Caption := FCaptions.SelectAll;
  atb.ShowCaption := true;
  atb.Layout := blGlyphTopAdjusted;
  atb.Hint := Hints.SelectAllTitle;
  atb.OfficeHint.Title := Hints.SelectAllTitle;
  atb.OfficeHint.Notes.Text := Hints.SelectAllContent;
  atb.Tag := integer(btSelectAll);
  atb.ShortCutHint := 'SA';
  atb.Left := 86;
  atb.Top := 2;
  atb.Width := 40;
  atb.Height := 64;

  FSelectAll := atb;

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBHIGHLIGHTL','High light');
  atb.Caption := FCaptions.Highlight;
  atb.ShowCaption := true;
  atb.Layout := blGlyphTopAdjusted;

  atb.OnClick := Highlight;
  atb.Hint := Hints.HighlightTitle;
  atb.OfficeHint.Title := Hints.HighlightTitle;
  atb.OfficeHint.Notes.Text := Hints.HighlightContent;
  atb.Tag := integer(btHighlight);
  atb.ShortCutHint := 'H';
  atb.Left := 126;
  atb.Top := 2;
  atb.Width := 40;
  atb.Height := 64;

  FHighlight:= atb;

  Width := 171;
  Height := 85;

end;

destructor TAdvRichEditorEditingRibbonToolBar.Destroy;
begin
  FCaptions.Free;
  FHints.Free;
  inherited;
end;

procedure TAdvRichEditorEditingRibbonToolBar.Find(Sender: TObject);
var
  fd: TFindDialog;
begin
  fd := TFindDialog.Create(Self);

  fd.Options := fd.Options - [frWholeWord] + [frHideWholeWord, frHideUpDown];
  fd.OnFind := FindHandler;

  fd.Execute;
end;

procedure TAdvRichEditorEditingRibbonToolBar.FindHandler(Sender: TObject);
begin
  if Assigned(RichEditor) then
    RichEditor.Find((Sender as TFindDialog).FindText, frMatchCase in (Sender as TFindDialog).Options);
end;

procedure TAdvRichEditorEditingRibbonToolBar.Highlight(Sender: TObject);
var
  s: string;
begin
  if Assigned(RichEditor) then
  begin
    if InputQuery(Hints.HighlightTitle, MsgText,s) then
    begin
      if (s <> '') then
        RichEditor.Highlight(s)
      else
        RichEditor.UnHighlight;
    end;
  end;
end;

procedure TAdvRichEditorEditingRibbonToolBar.Replace(Sender: TObject);
var
  fd: TReplaceDialog;
begin
  fd := TReplaceDialog.Create(Self);
  fd.Options := fd.Options - [frWholeWord] + [frHideWholeWord, frHideUpDown];
  fd.OnFind := FindHandler;
  fd.OnReplace := ReplaceHandler;
  fd.Execute;
end;

procedure TAdvRichEditorEditingRibbonToolBar.ReplaceHandler(Sender: TObject);
begin
  if Assigned(RichEditor) then
  begin
    if frReplaceAll in (Sender as TReplaceDialog).Options then
      RichEditor.ReplaceAll((Sender as TReplaceDialog).FindText,(Sender as TReplaceDialog).ReplaceText, frMatchCase in (Sender as TReplaceDialog).Options)
    else
      RichEditor.Replace((Sender as TReplaceDialog).FindText,(Sender as TReplaceDialog).ReplaceText, frMatchCase in (Sender as TReplaceDialog).Options);
  end;
end;

procedure TAdvRichEditorEditingRibbonToolBar.SetCaptions(
  const Value: TAdvRichEditorEditingCaptions);
begin
  FCaptions.Assign(Value);
end;

procedure TAdvRichEditorEditingRibbonToolBar.SetHints(
  const Value: TAdvRichEditorEditingHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvRichEditorEditingRibbonToolBar.SetOptions(
  const Value: TAdvRichEditorToolBarEditingButtons);
begin
  FOptions := Value;
end;

procedure TAdvRichEditorEditingRibbonToolBar.UpdateHints;
var
  agb: TAdvCustomGlowButton;
begin
  agb := GetButton(integer(btFind));
  agb.Hint := Hints.FindTitle;
  agb.OfficeHint.Title := Hints.FindTitle;
  agb.OfficeHint.Notes.Text := Hints.FindContent;

  agb := GetButton(integer(btReplace));
  agb.Hint := Hints.ReplaceTitle;
  agb.OfficeHint.Title := Hints.ReplaceTitle;
  agb.OfficeHint.Notes.Text := Hints.ReplaceContent;

  agb := GetButton(integer(btSelectAll));
  agb.Hint := Hints.SelectAllTitle;
  agb.OfficeHint.Title := Hints.SelectAllTitle;
  agb.OfficeHint.Notes.Text := Hints.SelectAllContent;

  agb := GetButton(integer(btHighlight));
  agb.Hint := Hints.HighlightTitle;
  agb.OfficeHint.Title := Hints.HighlightTitle;
  agb.OfficeHint.Notes.Text := Hints.HighlightContent;

  Options := [btFind, btReplace, btSelectAll, btHighlight];
end;

{ TAdvRichEditorClipboardCaptions }

procedure TAdvRichEditorClipboardCaptions.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorClipboardCaptions) then
  begin
    FCut := (Source as TAdvRichEditorClipboardCaptions).Cut;
    FCopy := (Source as TAdvRichEditorClipboardCaptions).Copy;
    FPaste := (Source as TAdvRichEditorClipboardCaptions).Paste;
  end;
end;

constructor TAdvRichEditorClipboardCaptions.Create;
begin
  inherited;
  FCut := 'Cut';
  FCopy := 'Copy';
  FPaste := 'Paste';
end;

procedure TAdvRichEditorClipboardCaptions.DoChanged;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvRichEditorClipboardCaptions.SetCopy(const Value: string);
begin
  if (FCopy <> Value) then
  begin
    FCopy := Value;
    DoChanged;
  end;
end;

procedure TAdvRichEditorClipboardCaptions.SetCut(const Value: string);
begin
  if (FCut <> Value) then
  begin
    FCut := Value;
    DoChanged;
  end;
end;

procedure TAdvRichEditorClipboardCaptions.SetPaste(const Value: string);
begin
  if (FPaste <> Value) then
  begin
    FPaste := Value;
    DoChanged;
  end;
end;

{ TAdvRichEditorEditingCaptions }

procedure TAdvRichEditorEditingCaptions.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorEditingCaptions) then
  begin
    FFind := (Source as TAdvRichEditorEditingCaptions).Find;
    FReplace := (Source as TAdvRichEditorEditingCaptions).Replace;
    FSelectAll := (Source as TAdvRichEditorEditingCaptions).SelectAll;
    FHighlight := (Source as TAdvRichEditorEditingCaptions).Highlight;
  end;
end;

constructor TAdvRichEditorEditingCaptions.Create;
begin
  inherited;
  FFind := 'Find';
  FReplace := 'Replace';
  FSelectAll := 'Select all';
  FHighlight := 'High light';

end;

procedure TAdvRichEditorEditingCaptions.DoChanged;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvRichEditorEditingCaptions.SetFind(const Value: string);
begin
  if (FFind <> Value) then
  begin
    FFind := Value;
    DoChanged;
  end;
end;

procedure TAdvRichEditorEditingCaptions.SetHighlight(const Value: string);
begin
  if (FHighlight <> Value) then
  begin
    FHighlight := Value;
    DoChanged;
  end;
end;

procedure TAdvRichEditorEditingCaptions.SetReplace(const Value: string);
begin
  if (FReplace <> Value) then
  begin
    FReplace := Value;
    DoChanged;
  end;
end;

procedure TAdvRichEditorEditingCaptions.SetSelectAll(const Value: string);
begin
  if (FSelectAll <> Value) then
  begin
    FSelectAll := Value;
    DoChanged;
  end;
end;

end.
