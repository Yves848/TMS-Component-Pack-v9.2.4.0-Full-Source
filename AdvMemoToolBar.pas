{*************************************************************************}
{ TMS TAdvMemo toolbar                                              }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2016                                       }
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
unit AdvMemoToolBar;

interface

uses
  Classes, AdvToolBar, Forms, Controls, Graphics, AdvToolBarExt,
  AdvMemo, ActnList, AdvOfficeSelectors, AdvOfficeComboBox,
  AdvGlowButton, AdvMemoStylerManager, AdvMemoActions;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.0.0 : Initial release

type
  TAdvMemoToolBar = class(TCustomAdvToolBar)
  private
    FMemo: TAdvMemo;
  protected
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetButton(Id: integer): TAdvCustomGlowButton;
  published
    property Memo: TAdvMemo read FMemo write FMemo;
  end;

  // Classic docking toolbars
  TAdvMemoToolBarEditButton = (btFileOpen, btFileSave, btCopy, btPaste, btCut, btUndo, btRedo, btFileSaveAs, btFileNew);

  TAdvMemoToolBarEditButtons = set of TAdvMemoToolBarEditButton;

  TAdvMemoEditHints = class(TPersistent)
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
    FFileSaveAsTitle: string;
    FFileNewTitle: string;
    FFileSaveAsContent: string;
    FFileNewContent: string;

  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property FileOpenTitle: string read FFileOpenTitle write FFileOpenTitle;
    property FileOpenContent: string read FFileOpenContent write FFileOpenContent;

    property FileSaveTitle: string read FFileSaveTitle write FFileSaveTitle;
    property FileSaveContent: string read FFileSaveContent write FFileSaveContent;

    property FileSaveAsTitle: string read FFileSaveAsTitle write FFileSaveAsTitle;
    property FileSaveAsContent: string read FFileSaveAsContent write FFileSaveAsContent;

    property FileNewTitle: string read FFileNewTitle write FFileNewTitle;
    property FileNewContent: string read FFileNewContent write FFileNewContent;

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

  TAdvMemoToolbarFileOpenEvent = procedure(Sender: TObject; FileName: string) of object;
  TAdvMemoToolbarFileSaveEvent = procedure(Sender: TObject; FileName: string; IsSaveAs: boolean) of object;
  TAdvMemoToolbarFileNewEvent = procedure(Sender: TObject; var ACaption: string) of object;

  TAdvMemoCommonOpsToolBar = class(TAdvMemoToolBar)
  private
    FFileOpenBtn, FFileNewBtn, FFileSaveBtn, FFileSaveAsBtn: TAdvGlowButton;
    FOnInitNewMemo, FOnInitOpenMemo: TNotifyEvent;
  protected
    procedure OnButtonInitMemo(Sender: TObject);
  public
    //Useful to start with an Untitled memo
    procedure DoFileNew;
    //Useful to call from FormCloseQuery
    function DoPromptAndSaveIfModified: boolean;
  published
    property OnInitNewMemo: TNotifyEvent read FOnInitNewMemo write FOnInitNewMemo;
    property OnInitOpenMemo: TNotifyEvent read FOnInitOpenMemo write FOnInitOpenMemo;
  end;

  TAdvMemoEditToolBar = class(TAdvMemoCommonOpsToolBar)
  private
    FHints: TAdvMemoEditHints;
    FOptions: TAdvMemoToolBarEditButtons;
    procedure SetHints(const Value: TAdvMemoEditHints);

  protected
    procedure SetOptions(Value: TAdvMemoToolBarEditButtons);
    procedure UpdateButtons;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
    procedure Loaded; override;

  published
    property Hints: TAdvMemoEditHints read FHints write SetHints;
    property Options: TAdvMemoToolBarEditButtons read FOptions write SetOptions;
  end;

  TAdvMemoToolBarEditingButton = (btFind, btReplace, btSelectAll);

  TAdvMemoToolBarEditingButtons = set of TAdvMemoToolBarEditingButton;

  TAdvMemoEditingHints = class(TPersistent)
  private
    FFindTitle: string;
    FReplaceTitle: string;
    FFindContent: string;
    FReplaceContent: string;
    FSelectAllTitle: string;
    FSelectAllContent: string;
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
  end;

  TAdvMemoEditingToolBar = class(TAdvMemoToolBar)
  private
    FHints: TAdvMemoEditingHints;
    FOptions: TAdvMemoToolBarEditingButtons;
    procedure SetHints(const Value: TAdvMemoEditingHints);
    procedure SetOptions(const Value: TAdvMemoToolBarEditingButtons);
  protected
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
    procedure Loaded; override;
  published
    property Hints: TAdvMemoEditingHints read FHints write SetHints;
    property Options: TAdvMemoToolBarEditingButtons read FOptions write SetOptions;
  end;

  // Ribbon toolbars

  TAdvMemoClipboardHints = class(TPersistent)
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

  TAdvMemoClipboardCaptions = class(TPersistent)
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

  TAdvMemoClipboardRibbonToolBar = class(TAdvMemoToolBar)
  private
    FCut,FCopy,FPaste: TAdvGlowButton;
    FHints: TAdvMemoClipboardHints;
    FCaptions: TAdvMemoClipboardCaptions;
    procedure SetHints(const Value: TAdvMemoClipboardHints);
    procedure SetCaptions(const Value: TAdvMemoClipboardCaptions);
  protected
    procedure CreateWnd; override;
    procedure CaptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property AutoPositionControls default false;
    property Captions: TAdvMemoClipboardCaptions read FCaptions write SetCaptions;
    property Hints: TAdvMemoClipboardHints read FHints write SetHints;
  end;

  TAdvMemoEditingCaptions = class(TPersistent)
  private
    FReplace: string;
    FSelectAll: string;
    FFind: string;
    FOnChange: TNotifyEvent;
    procedure SetFind(const Value: string);
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvMemoEditingRibbonToolBar = class(TAdvMemoEditingToolBar)
  private
    FFind, FReplace, FSelectAll: TAdvGlowButton;
    FHints: TAdvMemoEditingHints;
    FCaptions: TAdvMemoEditingCaptions;
    procedure SetHints(const Value: TAdvMemoEditingHints);
    procedure SetCaptions(const Value: TAdvMemoEditingCaptions);
  protected
    procedure CaptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property Captions: TAdvMemoEditingCaptions read FCaptions write SetCaptions;
    property Hints: TAdvMemoEditingHints read FHints write SetHints;
  end;

  TAdvMemoFileHints = class(TPersistent)
  private
    FFileOpenContent: string;
    FFileOpenTitle: string;
    FFileSaveContent: string;
    FFileSaveTitle: string;
    FFileSaveAsContent: string;
    FFileSaveAsTitle: string;
    FFileNewContent: string;
    FFileNewTitle: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property FileOpenContent: string read FFileOpenContent write FFileOpenContent;
    property FileOpenTitle: string read FFileOpenTitle write FFileOpenTitle;
    property FileSaveContent: string read FFileSaveContent write FFileSaveContent;
    property FileSaveTitle: string read FFileSaveTitle write FFileSaveTitle;
    property FileSaveAsContent: string read FFileSaveAsContent write FFileSaveAsContent;
    property FileSaveAsTitle: string read FFileSaveAsTitle write FFileSaveAsTitle;
    property FileNewContent: string read FFileNewContent write FFileNewContent;
    property FileNewTitle: string read FFileNewTitle write FFileNewTitle;
  end;

  TAdvMemoFileCaptions = class(TPersistent)
  private
    FFileOpen: string;
    FFileSave: string;
    FFileSaveAs: string;
    FFileNew: string;
    FOnChange: TNotifyEvent;
    procedure SetFileOpen(const Value: string);
    procedure SetFileSave(const Value: string);
    procedure SetFileSaveAs(const Value: string);
    procedure SetFileNew(const Value: string);
  protected
    procedure DoChanged; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property FileOpen: string read FFileOpen write SetFileOpen;
    property FileSave: string read FFileSave write SetFileSave;
    property FileSaveAs: string read FFileSaveAs write SetFileSaveAs;
    property FileNew: string read FFileNew write SetFileNew;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvMemoFileRibbonToolBar = class(TAdvMemoCommonOpsToolBar)
  private
    FFileOpen, FFileSave, FFileSaveAs, FFileNew: TAdvGlowButton;
    FHints: TAdvMemoFileHints;
    FCaptions: TAdvMemoFileCaptions;
    procedure SetHints(const Value: TAdvMemoFileHints);
    procedure SetCaptions(const Value: TAdvMemoFileCaptions);
  protected
    procedure CreateWnd; override;
    procedure CaptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property AutoPositionControls default false;
    property Captions: TAdvMemoFileCaptions read FCaptions write SetCaptions;
    property Hints: TAdvMemoFileHints read FHints write SetHints;
  end;

procedure Register;

{$R AdvMemoToolBar.res}

implementation

uses
  SysUtils, ExtDlgs, Dialogs, PNGImage, JPEG, Math, Windows;

const
  BTNSIZE = 24;
  DEFAULT_OPEN_DIALOG_FILTER = 'Text files|*.txt|All files}*.*';
  DEFAULT_SAVE_DIALOG_FILTER = 'Text files|*.txt';
  DEFAULT_SAVED_DIALOG_DEFAULT_EXT = 'txt';
  DEFAULT_UNTITLED_CAPTION = 'Untitled';

{ TAdvMemoToolBar }

procedure TAdvMemoToolBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = Memo) then
    FMemo := nil;
end;

constructor TAdvMemoToolBar.Create(AOwner: TComponent);
var
  i: integer;

begin
  inherited;

  if Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for i := 0 to AOwner.ComponentCount - 1 do
      if (AOwner.Components[i] is TAdvMemo) then
      begin
        FMemo := AOwner.Components[i] as TAdvMemo;
        break;
      end;
  end;
end;


procedure TAdvMemoToolBar.CreateWnd;
begin
  inherited;
  AssignGlowButtonsEvent;
end;

function TAdvMemoToolBar.GetButton(Id: integer): TAdvCustomGlowButton;
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

{ TAdvMemoCommonOpsToolBar }

procedure TAdvMemoCommonOpsToolBar.DoFileNew;
begin
  FFileNewBtn.Action.ExecuteTarget(FMemo);
end;

function TAdvMemoCommonOpsToolBar.DoPromptAndSaveIfModified: boolean;
begin
  result := TAdvMemoFileSaveAction(FFileSaveBtn.Action).DoPromptAndSaveIfModified(FMemo);
end;

procedure TAdvMemoCommonOpsToolBar.OnButtonInitMemo(Sender: TObject);
begin
  if (Sender.ClassType = TAdvMemoFileOpenAction) and assigned(FOnInitOpenMemo) then
    FOnInitOpenMemo(Sender)
  else
  if (Sender.ClassType = TAdvMemoFileNewAction) and assigned(FOnInitNewMemo) then
    FOnInitNewMemo(Sender);
end;

{ TAdvMemoEditToolBar }

constructor TAdvMemoEditToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  sep: TAdvToolBarSeparator;
begin
  inherited;

  FHints := TAdvMemoEditHints.Create;

  ShowRightHandle := false;

  atb := AddButton(HInstance, TAdvMemoFileNewAction, bsButton, 'TMSMEMOTBNew','New file');
  atb.OfficeHint.Title := FHints.FileNewTitle;
  atb.OfficeHint.Notes.Text := FHints.FileNewContent;
  atb.Tag := integer(btFileNew);
  FFileNewBtn := atb;
  TAdvMemoFileNewAction(FFileNewBtn.Action).OnInitMemo :=  OnButtonInitMemo;

  atb := AddButton(HInstance, TAdvMemoFileOpenAction, bsButton, 'TMSMEMOTBOPEN', 'Open File');
  atb.OfficeHint.Title := FHints.FileOpenTitle;
  atb.OfficeHint.Notes.Text := FHints.FileOpenContent;
  atb.Tag := integer(btFileOpen);
  FFileOpenBtn := atb;
  TAdvMemoFileOpenAction(FFileOpenBtn.Action).OnInitMemo :=  OnButtonInitMemo;

  atb := AddButton(HInstance, TAdvMemoFileSaveAction, bsButton, 'TMSMEMOTBSAVE', 'Save');
  atb.OfficeHint.Title := FHints.FileSaveTitle;
  atb.OfficeHint.Notes.Text := FHints.FileSaveContent;
  atb.Tag := integer(btFileSave);
  FFileSaveBtn := atb;

  atb := AddButton(HInstance, TAdvMemoFileSaveAsAction, bsButton, 'TMSMEMOTBSAVEAS','Save file by another name');
  atb.OfficeHint.Title := FHints.FileSaveAsTitle;
  atb.OfficeHint.Notes.Text := FHints.FileSaveAsContent;
  atb.Tag := integer(btFileSaveAs);
  FFileSaveAsBtn := atb;

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TAdvMemoCut, bsButton, 'TMSMEMOTBCUT', 'Cut to clipboard');
  atb.OfficeHint.Title := FHints.CutTitle;
  atb.OfficeHint.Notes.Text := FHints.CutContent;
  atb.Tag := integer(btCut);

  atb := AddButton(HInstance,TAdvMemoCopy, bsButton, 'TMSMEMOTBCOPY', 'Copy to clipboard');
  atb.OfficeHint.Title := FHints.CopyTitle;
  atb.OfficeHint.Notes.Text := FHints.CopyContent;
  atb.Tag := integer(btCopy);

  atb := AddButton(HInstance,TAdvMemoPaste, bsButton, 'TMSMEMOTBPASTE', 'Paste from clipboard');
  atb.OfficeHint.Title := FHints.PasteTitle;
  atb.OfficeHint.Notes.Text := FHints.PasteContent;
  atb.Tag := integer(btPaste);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TAdvMemoUndo, bsButton, 'TMSMEMOTBUNDO', 'Undo');
  atb.OfficeHint.Title := FHints.UndoTitle;
  atb.OfficeHint.Notes.Text := FHints.UndoContent;
  atb.Tag := integer(btUndo);

  atb := AddButton(HInstance,TAdvMemoRedo, bsButton, 'TMSMEMOTBREDO', 'Redo');
  atb.OfficeHint.Title := FHints.RedoTitle;
  atb.OfficeHint.Notes.Text := FHints.RedoContent;
  atb.Tag := integer(btRedo);

  Options := [btFileOpen, btFileSave, btCopy, btPaste, btCut, btUndo, btRedo, btFileSaveAs, btFileNew];
end;

destructor TAdvMemoEditToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TAdvMemoEditToolBar.Loaded;
begin
  inherited;
  UpdateHints;
end;


procedure TAdvMemoEditToolBar.SetHints(
  const Value: TAdvMemoEditHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvMemoEditToolBar.SetOptions(
  Value: TAdvMemoToolBarEditButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TAdvMemoEditToolBar.UpdateButtons;
var
  i: integer;
  j: TAdvMemoToolBarEditButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TAdvMemoToolBarEditButton) to High(TAdvMemoToolBarEditButton) do
    begin
      if Controls[i].Tag = integer(j) then
        Controls[i].Visible := j in Options;
    end;
  end;
end;

procedure TAdvMemoEditToolBar.UpdateHints;
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

  agb := GetButton(integer(btFileSaveAs));
  agb.Hint := Hints.FileSaveAsTitle;
  agb.OfficeHint.Title := Hints.FileSaveAsTitle;
  agb.OfficeHint.Notes.Text := Hints.FileSaveAsContent;

  agb := GetButton(integer(btFileNew));
  agb.Hint := Hints.FileNewTitle;
  agb.OfficeHint.Title := Hints.FileNewTitle;
  agb.OfficeHint.Notes.Text := Hints.FileNewContent;

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

{ TAdvMemoClipboardRibbonToolBar }

procedure TAdvMemoClipboardRibbonToolBar.CaptionsChanged(Sender: TObject);
begin
  if Assigned(FCut) then
    FCut.Caption := Captions.Cut;
  if Assigned(FCopy) then
    FCopy.Caption := Captions.Copy;
  if Assigned(FPaste) then
    FPaste.Caption := Captions.Paste;
end;

constructor TAdvMemoClipboardRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TAdvMemoClipboardHints.Create;
  FCaptions := TAdvMemoClipboardCaptions.Create;
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

  atb := AddButton(HInstance, TAdvMemoPaste, bsButton, 'TMSMEMOTBPASTELARGE', 'Paste from clipboard');
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
  atb.Height := 74;

  FPaste := atb;

  atb := AddButton(HInstance, TAdvMemoCut, bsButton, 'TMSMEMOTBCUT', 'Cut to clipboard');
  atb.Caption := 'Cut';
  atb.Hint := FHints.CutTitle;
  atb.OfficeHint.Title := FHints.CutTitle;
  atb.OfficeHint.Notes.Text := FHints.CutContent;
  atb.ShowCaption := true;
  atb.Tag := integer(btCut);
  atb.ShortCutHint := 'X';
  atb.Width := 60;
  atb.Height := 24;
  atb.Left := 42;
  atb.Top := 2;

  FCut := atb;

  atb := AddButton(HInstance, TAdvMemoCopy, bsButton, 'TMSMEMOTBCOPY', 'Copy to clipboard');
  atb.Hint := FHints.CopyTitle;
  atb.OfficeHint.Title := FHints.CopyTitle;
  atb.OfficeHint.Notes.Text := FHints.CopyContent;
  atb.Caption := 'Copy';
  atb.ShowCaption := true;
  atb.Tag := integer(btCopy);
  atb.ShortCutHint := 'C';
  atb.Width := 60;
  atb.Height := 24;
  atb.Left := 42;
  atb.Top := 26;

  FCopy := atb;

  Width := 105;
  Height := 85;
end;

procedure TAdvMemoClipboardRibbonToolBar.CreateWnd;
begin
  inherited;
  AutoPositionControls := false;
  ToolBarState := tsFixed;
  FCopy.Top := 24;
  FCopy.Left := 41;
end;

destructor TAdvMemoClipboardRibbonToolBar.Destroy;
begin
  FHints.Free;
  FCaptions.Free;
  inherited;
end;

procedure TAdvMemoClipboardRibbonToolBar.SetCaptions(
  const Value: TAdvMemoClipboardCaptions);
begin
  FCaptions := Value;
end;

procedure TAdvMemoClipboardRibbonToolBar.SetHints(
  const Value: TAdvMemoClipboardHints);
begin
  FHints := Value;
end;

procedure TAdvMemoClipboardRibbonToolBar.UpdateHints;
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

{ TAdvMemoEditHints }

procedure TAdvMemoEditHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvMemoEditHints) then
  begin
    FPasteContent := (Source as TAdvMemoEditHints).PasteContent;
    FUndoTitle := (Source as TAdvMemoEditHints).UndoTitle;
    FRedoTitle := (Source as TAdvMemoEditHints).RedoTitle;
    FFileSaveContent := (Source as TAdvMemoEditHints).FileSaveContent;
    FCopyTitle := (Source as TAdvMemoEditHints).CopyTitle;
    FFileOpenContent := (Source as TAdvMemoEditHints).FileOpenContent;
    FUndoContent := (Source as TAdvMemoEditHints).UndoContent;
    FRedoContent := (Source as TAdvMemoEditHints).RedoContent;
    FCopyContent := (Source as TAdvMemoEditHints).CopyContent;
    FCutTitle := (Source as TAdvMemoEditHints).CutTitle;
    FPasteTitle := (Source as TAdvMemoEditHints).PasteTitle;
    FFileSaveTitle := (Source as TAdvMemoEditHints).FileSaveTitle;
    FCutContent := (Source as TAdvMemoEditHints).CutContent;
    FFileOpenTitle := (Source as TAdvMemoEditHints).FileOpenTitle;
    FFileSaveAsContent := (Source as TAdvMemoEditHints).FileSaveAsContent;
    FFileSaveAsTitle := (Source as TAdvMemoEditHints).FileSaveAsTitle;
    FFileNewContent := (Source as TAdvMemoEditHints).FileNewContent;
    FFileNewTitle := (Source as TAdvMemoEditHints).FileNewTitle;
  end;

end;

constructor TAdvMemoEditHints.Create;
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

  FFileSaveAsTitle := 'Save As';
  FFileSaveAsContent := 'Save document by another file name';

  FFileNewTitle := 'New';
  FFileNewContent := 'Create new memo';
end;


{ TAdvMemoClipboardHints }

procedure TAdvMemoClipboardHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvMemoClipboardHints) then
  begin
    FCutTitle := (Source as TAdvMemoClipboardHints).CutTitle;
    FCutContent := (Source as TAdvMemoClipboardHints).CutContent;
    FCopyTitle := (Source as TAdvMemoClipboardHints).CopyTitle;
    FCopyContent := (Source as TAdvMemoClipboardHints).CopyContent;
    FPasteTitle := (Source as TAdvMemoClipboardHints).PasteTitle;
    FPasteContent := (Source as TAdvMemoClipboardHints).PasteContent;
  end;
end;

constructor TAdvMemoClipboardHints.Create;
begin
  inherited;

  FPasteTitle := 'Paste (Ctrl+V)';
  FPasteContent := 'Add content on the clipboard to your document';

  FCopyTitle := 'Copy (Ctrl+C)';
  FCopyContent := 'Put a copy of the selection on the clipboard';

  FCutTitle := 'Cut (Ctrl+X)';
  FCutContent := 'Remove the selection to the clipboard';

end;

{ TAdvMemorEditingHints }

procedure TAdvMemoEditingHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvMemoEditingHints) then
  begin
    FindTitle := (Source as TAdvMemoEditingHints).FindTitle;
    FindContent := (Source as TAdvMemoEditingHints).FindContent;
    ReplaceTitle := (Source as TAdvMemoEditingHints).ReplaceTitle;
    ReplaceContent := (Source as TAdvMemoEditingHints).ReplaceContent;
    SelectAllTitle := (Source as TAdvMemoEditingHints).SelectAllTitle;
    SelectAllContent := (Source as TAdvMemoEditingHints).SelectAllContent;
  end;
end;

constructor TAdvMemoEditingHints.Create;
begin
  inherited;
  FindTitle := 'Find';
  FindContent := 'Finds text in the document';
  ReplaceTitle := 'Replace';
  ReplaceContent := 'Replaces occurrences of text';
  SelectAllTitle := 'Select All (Ctrl+A)';
  SelectAllContent := 'Selects all text in the document';
end;

{ TAdvMemoFileHints }

procedure TAdvMemoFileHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvMemoFileHints) then
  begin
    FFileSaveContent := (Source as TAdvMemoEditHints).FileSaveContent;
    FFileOpenContent := (Source as TAdvMemoEditHints).FileOpenContent;
    FFileSaveTitle := (Source as TAdvMemoEditHints).FileSaveTitle;
    FFileOpenTitle := (Source as TAdvMemoEditHints).FileOpenTitle;

    FFileSaveAsContent := (Source as TAdvMemoEditHints).FileSaveAsContent;
    FFileSaveAsTitle := (Source as TAdvMemoEditHints).FileSaveAsTitle;
    FFileNewContent := (Source as TAdvMemoEditHints).FileNewContent;
    FFileNewTitle := (Source as TAdvMemoEditHints).FileNewTitle;
  end;
end;

constructor TAdvMemoFileHints.Create;
begin
  inherited;

  FFileSaveTitle := 'Save (Ctrl+S)';
  FFileSaveContent := 'Save document to file';

  FFileOpenTitle := 'Open (Ctrl+O)';
  FFileOpenContent := 'Open new document from file';

  FFileSaveAsTitle := 'Save As';
  FFileSaveAsContent := 'Save document by another file name';

  FFileNewTitle := 'New';
  FFileNewContent := 'Create new file';
end;

{ TAdvMemoEditingToolBar }

constructor TAdvMemoEditingToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TAdvMemoEditingHints.Create;

  ShowRightHandle := false;

  atb := AddButton(HInstance, TAdvMemoFindAction, bsButton, 'TMSMEMOTBFIND','Find');
  atb.Hint := Hints.FindTitle;
  atb.OfficeHint.Title := Hints.FindTitle;
  atb.OfficeHint.Notes.Text := Hints.FindContent;
  atb.Tag := integer(btFind);

  atb := AddButton(HInstance, TAdvMemoReplaceAction, bsButton, 'TMSMEMOTBREPLACE','Replace');
  atb.Hint := Hints.ReplaceTitle;
  atb.OfficeHint.Title := Hints.ReplaceTitle;
  atb.OfficeHint.Notes.Text := Hints.ReplaceContent;
  atb.Tag := integer(btReplace);

  atb := AddButton(HInstance, TAdvMemoSelectAll, bsButton, 'TMSMEMOTBSELALL','SelectAll');
  atb.Hint := Hints.SelectAllTitle;
  atb.OfficeHint.Title := Hints.SelectAllTitle;
  atb.OfficeHint.Notes.Text := Hints.SelectAllContent;
  atb.Tag := integer(btSelectAll);

  Options := [btFind, btReplace, btSelectAll];
end;

destructor TAdvMemoEditingToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TAdvMemoEditingToolBar.Loaded;
begin
  inherited;
  UpdateHints;
end;

procedure TAdvMemoEditingToolBar.SetHints(
  const Value: TAdvMemoEditingHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvMemoEditingToolBar.SetOptions(
  const Value: TAdvMemoToolBarEditingButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TAdvMemoEditingToolBar.UpdateButtons;
var
  i: integer;
  j: TAdvMemoToolBarEditingButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TAdvMemoToolBarEditingButton) to High(TAdvMemoToolBarEditingButton) do
    begin
      if Controls[i].Tag = integer(j) then
        Controls[i].Visible := j in Options;
    end;
  end;

end;

procedure TAdvMemoEditingToolBar.UpdateHints;
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
end;

{ TAdvMemoEditingRibbonToolBar }

procedure TAdvMemoEditingRibbonToolBar.CaptionsChanged(Sender: TObject);
begin
  FFind.Caption := Captions.Find;
  FReplace.Caption := Captions.Replace;
  FSelectAll.Caption := Captions.SelectAll;
end;

constructor TAdvMemoEditingRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TAdvMemoEditingHints.Create;

  FCaptions := TAdvMemoEditingCaptions.Create;
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

  atb := AddButton(HInstance, TAdvMemoFindAction, bsButton, 'TMSMEMOTBFINDL','Find');
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
  atb.Height := 74;

  FFind := atb;

  atb := AddButton(HInstance, TAdvMemoReplaceAction, bsButton, 'TMSMEMOTBREPLACEL','Replace');
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
  atb.Height := 74;

  FReplace := atb;

  atb := AddButton(HInstance, TAdvMemoSelectAll, bsButton, 'TMSMEMOTBSELALLL','SelectAll');
  atb.Caption := FCaptions.SelectAll;
  atb.ShowCaption := true;
  atb.Layout := blGlyphTopAdjusted;
  atb.Hint := Hints.SelectAllTitle;
  atb.OfficeHint.Title := Hints.SelectAllTitle;
  atb.OfficeHint.Notes.Text := Hints.SelectAllContent;
  atb.Tag := integer(btSelectAll);
  atb.ShortCutHint := 'A';
  atb.Left := 86;
  atb.Top := 2;
  atb.Width := 40;
  atb.Height := 74;

  FSelectAll := atb;
end;

destructor TAdvMemoEditingRibbonToolBar.Destroy;
begin
  FCaptions.Free;
  FHints.Free;
  inherited;
end;

procedure TAdvMemoEditingRibbonToolBar.SetCaptions(
  const Value: TAdvMemoEditingCaptions);
begin
  FCaptions.Assign(Value);
end;

procedure TAdvMemoEditingRibbonToolBar.SetHints(
  const Value: TAdvMemoEditingHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvMemoEditingRibbonToolBar.UpdateHints;
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
end;

{ TAdvMemoClipboardCaptions }

procedure TAdvMemoClipboardCaptions.Assign(Source: TPersistent);
begin
  if (Source is TAdvMemoClipboardCaptions) then
  begin
    FCut := (Source as TAdvMemoClipboardCaptions).Cut;
    FCopy := (Source as TAdvMemoClipboardCaptions).Copy;
    FPaste := (Source as TAdvMemoClipboardCaptions).Paste;
  end;
end;

constructor TAdvMemoClipboardCaptions.Create;
begin
  inherited;
  FCut := 'Cut';
  FCopy := 'Copy';
  FPaste := 'Paste';
end;

procedure TAdvMemoClipboardCaptions.DoChanged;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvMemoClipboardCaptions.SetCopy(const Value: string);
begin
  if (FCopy <> Value) then
  begin
    FCopy := Value;
    DoChanged;
  end;
end;

procedure TAdvMemoClipboardCaptions.SetCut(const Value: string);
begin
  if (FCut <> Value) then
  begin
    FCut := Value;
    DoChanged;
  end;
end;

procedure TAdvMemoClipboardCaptions.SetPaste(const Value: string);
begin
  if (FPaste <> Value) then
  begin
    FPaste := Value;
    DoChanged;
  end;
end;

{ TAdvMemoEditingCaptions }

procedure TAdvMemoEditingCaptions.Assign(Source: TPersistent);
begin
  if (Source is TAdvMemoEditingCaptions) then
  begin
    FFind := (Source as TAdvMemoEditingCaptions).Find;
    FReplace := (Source as TAdvMemoEditingCaptions).Replace;
    FSelectAll := (Source as TAdvMemoEditingCaptions).SelectAll;
  end;
end;

constructor TAdvMemoEditingCaptions.Create;
begin
  inherited;
  FFind := 'Find';
  FReplace := 'Replace';
  FSelectAll := 'Select all';
end;

procedure TAdvMemoEditingCaptions.DoChanged;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvMemoEditingCaptions.SetFind(const Value: string);
begin
  if (FFind <> Value) then
  begin
    FFind := Value;
    DoChanged;
  end;
end;

procedure TAdvMemoEditingCaptions.SetReplace(const Value: string);
begin
  if (FReplace <> Value) then
  begin
    FReplace := Value;
    DoChanged;
  end;
end;

procedure TAdvMemoEditingCaptions.SetSelectAll(const Value: string);
begin
  if (FSelectAll <> Value) then
  begin
    FSelectAll := Value;
    DoChanged;
  end;
end;

{ TAdvMemoFileCaptions }

procedure TAdvMemoFileCaptions.Assign(Source: TPersistent);
begin
  if (Source is TAdvMemoFileCaptions) then
  begin
    FFileOpen := (Source as TAdvMemoFileCaptions).FileOpen;
    FFileSave := (Source as TAdvMemoFileCaptions).FileSave;
    FFileSaveAs := (Source as TAdvMemoFileCaptions).FileSaveAs;
    FFileNew := (Source as TAdvMemoFileCaptions).FileNew;
  end;
end;

constructor TAdvMemoFileCaptions.Create;
begin
  inherited;
  FFileOpen := 'Open';
  FFileSave := 'Save';
  FFileSaveAs := 'Save As';
  FFileNew := 'New';
end;

procedure TAdvMemoFileCaptions.DoChanged;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvMemoFileCaptions.SetFileOpen(const Value: string);
begin
  if (FFileOpen <> Value) then
  begin
    FFileOpen := Value;
    DoChanged;
  end;
end;

procedure TAdvMemoFileCaptions.SetFileSave(const Value: string);
begin
  if (FFileSave <> Value) then
  begin
    FFileSave := Value;
    DoChanged;
  end;
end;

procedure TAdvMemoFileCaptions.SetFileSaveAs(const Value: string);
begin
  if (FFileSaveAs <> Value) then
  begin
    FFileSaveAs := Value;
    DoChanged;
  end;
end;

procedure TAdvMemoFileCaptions.SetFileNew(const Value: string);
begin
  if (FFileNew <> Value) then
  begin
    FFileNew := Value;
    DoChanged;
  end;
end;


{ TAdvMemoFileRibbonToolBar }

procedure TAdvMemoFileRibbonToolBar.CaptionsChanged(Sender: TObject);
begin
  FFileOpen.Caption := Captions.FileOpen;
  FFileSave.Caption := Captions.FileSave;
  FFileSaveAs.Caption := Captions.FileSaveAs;
  FFileNew.Caption := Captions.FileNew;
end;

constructor TAdvMemoFileRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TAdvMemoFileHints.Create;

  FCaptions := TAdvMemoFileCaptions.Create;
  FCaptions.OnChange := CaptionsChanged;

  Caption := 'File';

  CaptionAlignment := taCenter;
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance, TAdvMemoFileOpenAction, bsButton, 'TMSMEMOTBOPENLARGE','Open file');
  FFileOpenBtn := atb;
  TAdvMemoFileOpenAction(FFileOpenBtn.Action).OnInitMemo :=  OnButtonInitMemo;
  atb.Caption := FCaptions.FileOpen;
  atb.ShowCaption := true;
  atb.Hint := Hints.FileOpenTitle;
  atb.OfficeHint.Title := Hints.FileOpenTitle;
  atb.OfficeHint.Notes.Text := Hints.FileOpenContent;
  atb.Tag := integer(btFileOpen);
  atb.MinButtonSizeState := bsLarge;
  atb.MaxButtonSizeState := bsLarge;
  atb.Layout := blGlyphTop;
  atb.ShortCutHint := 'O';
  atb.Left := 2;
  atb.Top := 2;
  atb.Width := 40;
  atb.Height := 74;

  FFileOpen := atb;

  atb := AddButton(HInstance, TAdvMemoFileNewAction, bsButton, 'TMSMEMOTBNEW','New memo');
  FFileNewBtn := atb;
  TAdvMemoFileNewAction(FFileNewBtn.Action).OnInitMemo :=  OnButtonInitMemo;
  atb.Caption := FCaptions.FileNew;
  atb.ShowCaption := true;
  atb.Hint := Hints.FileNewTitle;
  atb.OfficeHint.Title := Hints.FileNewTitle;
  atb.OfficeHint.Notes.Text := Hints.FileNewContent;
  atb.Tag := integer(btFileNew);
  atb.Layout := blGlyphLeftAdjusted;
  atb.ShortCutHint := 'N';
  atb.Left := 42;
  atb.Top := 2;
  atb.Width := 65;
  atb.Height := 24;

  FFileNew := atb;

  atb := AddButton(HInstance, TAdvMemoFileSaveAction, bsButton, 'TMSMEMOTBSAVE','Save file');
  FFileSaveBtn := atb;
  atb.Caption := FCaptions.FileSave;
  atb.ShowCaption := true;
  atb.Hint := Hints.FileSaveTitle;
  atb.OfficeHint.Title := Hints.FileSaveTitle;
  atb.OfficeHint.Notes.Text := Hints.FileSaveContent;
  atb.Tag := integer(btFileSave);
  atb.Layout := blGlyphLeftAdjusted;
  atb.ShortCutHint := 'S';
  atb.Left := 42;
  atb.Top := 26;
  atb.Width := 65;
  atb.Height := 24;

  FFileSave := atb;

  atb := AddButton(HInstance, TAdvMemoFileSaveAsAction, bsButton, 'TMSMEMOTBSAVEAS','Save file by another name');
  FFileSaveAsBtn := atb;
  atb.Caption := FCaptions.FileSaveAs;
  atb.ShowCaption := true;
  atb.Hint := Hints.FileSaveAsTitle;
  atb.OfficeHint.Title := Hints.FileSaveAsTitle;
  atb.OfficeHint.Notes.Text := Hints.FileSaveAsContent;
  atb.Tag := integer(btFileSaveAs);
  atb.Layout := blGlyphLeftAdjusted;
  atb.ShortCutHint := 'W';
  atb.Left := 42;
  atb.Top := 50;
  atb.Width := 65;
  atb.Height := 24;

  FFileSaveAs := atb;

  Width := 107;
  Height := 85;
end;

procedure TAdvMemoFileRibbonToolBar.CreateWnd;
begin
  inherited;
  AutoPositionControls := false;
  ToolBarState := tsFixed;
end;

destructor TAdvMemoFileRibbonToolBar.Destroy;
begin
  FCaptions.Free;
  FHints.Free;
  inherited;
end;

procedure TAdvMemoFileRibbonToolBar.SetCaptions(
  const Value: TAdvMemoFileCaptions);
begin
  FCaptions.Assign(Value);
end;

procedure TAdvMemoFileRibbonToolBar.SetHints(
  const Value: TAdvMemoFileHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvMemoFileRibbonToolBar.UpdateHints;
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
end;

//todo: to merge with admemoreg.pas code
procedure Register;
begin
  RegisterComponents('TMS Memo',[TAdvMemoEditingToolBar,
                                 TAdvMemoEditToolBar,
                                 TAdvMemoClipboardRibbonToolBar,
                                 TAdvMemoEditingRibbonToolBar,
                                 TAdvMemoFileRibbonToolBar
                                 ]);
end;

end.
