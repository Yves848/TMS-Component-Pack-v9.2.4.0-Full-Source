{***************************************************************************}
{ TAdvMemo component                                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2017                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}
unit AdvMemoActions;

interface

{$I TMSDEFS.INC}

uses
  Classes, Forms, Controls, Graphics, ActnList, AdvMemo,
  AdvOfficeSelectors, AdvMemoStylerManager
  {$IFDEF DELPHIXE3_LVL}
  , System.Actions, System.UITypes
  {$ENDIF}
  ;

type
  TAdvMemoCut = class(TAdvMemoAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvMemoCopy = class(TAdvMemoAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvMemoPaste = class(TAdvMemoAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvMemoSelectAll = class(TAdvMemoAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvMemoUndo = class(TAdvMemoAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvMemoRedo = class(TAdvMemoAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvMemoDelete = class(TAdvMemoAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    { UpdateTarget is required because TEditAction.UpdateTarget specifically
      checks to see if the action is TEditCut or TEditCopy }
    procedure UpdateTarget(Target: TObject); override;
  end;


  // inform application of the file opened
  TAdvMemoActionFileOpenEvent = procedure(Sender: TObject; FileName: string) of object;

  TAdvMemoFileSaveAction = class(TAdvMemoAction)
  protected
    function PromptSaveFileAs(FMemo: TAdvMemo; var OutFileName: string): boolean;
    function DoSaveFileAs(FMemo: TAdvMemo): boolean;
    function DoSaveFile(FMemo: TAdvMemo): boolean;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    // Can also be used as a utility function on FormCLoseQuery by the application
    function DoPromptAndSaveIfModified(FMemo: TAdvMemo): boolean;
  published
  end;

  TAdvMemoFileOpenAction = class(TAdvMemoFileSaveAction)
  private
    FOnInitMemo: TNotifyEvent;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property OnInitMemo: TNotifyEvent read FOnInitMemo write FOnInitMemo;
  end;

  TAdvMemoFileSaveAsAction = class(TAdvMemoFileSaveAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvMemoFileNewAction = class(TAdvMemoFileSaveAction)
  private
    FOnInitMemo: TNotifyEvent;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property OnInitMemo: TNotifyEvent read FOnInitMemo write FOnInitMemo;
  end;

  // Find, Replace actions
  TAdvMemoFindAction = class(TAdvMemoAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvMemoReplaceAction = class(TAdvMemoAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

implementation

uses
  Windows, Dialogs, Sysutils, Clipbrd, AdvMemoActionsRes;

const
  DEFAULT_OPEN_DIALOG_FILTER = 'Text files|*.txt|All files}*.*';
  DEFAULT_SAVE_DIALOG_FILTER = 'Text files|*.txt';
  DEFAULT_SAVED_DIALOG_DEFAULT_EXT = 'txt';

{ TEditCopy }

procedure TAdvMemoCopy.ExecuteTarget(Target: TObject);
begin
  GetControl(Target).CopyToClipboard;
end;

{ TEditCut }

procedure TAdvMemoCut.ExecuteTarget(Target: TObject);
begin
  GetControl(Target).CutToClipboard;
end;

{ TEditPaste }

procedure TAdvMemoPaste.ExecuteTarget(Target: TObject);
begin
  GetControl(Target).PasteFromClipboard;
end;

procedure TAdvMemoPaste.UpdateTarget(Target: TObject);
begin
  Enabled := Clipboard.HasFormat(CF_TEXT);
end;

{ TEditSelectAll }

procedure TAdvMemoSelectAll.ExecuteTarget(Target: TObject);
begin
  GetControl(Target).SelectAll;
end;

procedure TAdvMemoSelectAll.UpdateTarget(Target: TObject);
begin
  Enabled := Length(GetControl(Target).Lines.Text) > 0;
end;

{ TEditUndo }

procedure TAdvMemoUndo.ExecuteTarget(Target: TObject);
begin
  GetControl(Target).Undo;
end;

procedure TAdvMemoUndo.UpdateTarget(Target: TObject);
begin
  Enabled := GetControl(Target).CanUndo;
end;

{ TAdvMemoRedo }

procedure TAdvMemoRedo.ExecuteTarget(Target: TObject);
begin
  GetControl(Target).Redo;
end;

procedure TAdvMemoRedo.UpdateTarget(Target: TObject);
begin
  Enabled := GetControl(Target).CanRedo;
end;

{ TEditDelete }

procedure TAdvMemoDelete.ExecuteTarget(Target: TObject);
begin
  GetControl(Target).DeleteSelection;
end;

procedure TAdvMemoDelete.UpdateTarget(Target: TObject);
begin
  Enabled := GetControl(Target).SelLength > 0;
end;


function GetFilterIndex(AStylerManager: TAdvMemoStylerManager; AFilter: string): integer;
var
  i: integer;
begin
  Result := 1;
  for i := 0 to Pred(AStylerManager.Items.Count) do
  begin
    if SameText(AFilter, AStylerManager.Items[i].Styler.Filter) then
    begin
      result := i+1;
      break;
    end;
  end;
end;

procedure SetStylerByExt(AMemo: TAdvMemo; AStylerManager: TAdvMemoStylerManager; AFileName: string);
var
  AMemoStyler: TAdvCustomMemoStyler;
begin
  if AStylerManager <> nil then
  with AStylerManager do
  begin
    AMemoStyler := GetStylerByFileName(AFileName);
    if AMemoStyler <> nil then
      AMemo.SyntaxStyles := AMemoStyler
    else
    if (DefaultStylerIndex >= 0) and (DefaultStylerIndex <= Items.Count - 1) then
      AMemo.SyntaxStyles := Items.Items[DefaultStylerIndex].Styler
    else
      AMemo.SyntaxStyles := nil;
  end;
end;

{TAdvMemoFileOpenAction}

procedure TAdvMemoFileOpenAction.ExecuteTarget(Target: TObject);
var
  od: TOpenDialog;
  lookFilter: string;
  FMemo: TAdvMemo;
begin
  FMemo := TAdvMemo(GetControl(Target));
  if not DoPromptAndSaveIfModified(FMemo) then
      exit;
  FMemo.FileName := '';
  FMemo.Lines.Clear;
  Fmemo.Modified := false;
  od := TOpenDialog.Create(Self);
  od.Filter := DEFAULT_OPEN_DIALOG_FILTER;
  if Assigned(FMemo.StylerManager) then
  begin
    lookFilter := od.Filter;
    if FMemo.SyntaxStyles <> nil then
      lookFilter := FMemo.SyntaxStyles.Filter;
    od.Filter := TAdvMemoStylerManager(FMemo.StylerManager).GetFilter(-1);
    od.FilterIndex := GetFilterIndex(TAdvMemoStylerManager(FMemo.StylerManager), lookFilter);
  end;
  try
    if od.Execute then
    begin
      FMemo.FileName := od.FileName;
      SetStylerByExt(FMemo, TAdvMemoStylerManager(FMemo.StylerManager), FMemo.FileName);
      FMemo.Lines.LoadFromFile(FMemo.FileName);
      FMemo.Modified := false;
      if Assigned(FOnInitMemo) then
        FOnInitMemo(Self);
    end;
  finally
    od.Free;
  end;
end;

procedure TAdvMemoFileOpenAction.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := true;
end;

{TAdvMemoFileSaveAction}
function TAdvMemoFileSaveAction.PromptSaveFileAs(FMemo: TAdvMemo; var OutFileName: string): boolean;
var
  sd: TSaveDialog;
  lookFilter: string;
begin
  result := false;
  OutFileName := '';
  sd := TSaveDialog.Create(Self);
  sd.Options := sd.Options + [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
  sd.Filter := DEFAULT_SAVE_DIALOG_FILTER;
  sd.DefaultExt := DEFAULT_SAVED_DIALOG_DEFAULT_EXT;

  lookFilter := sd.Filter;
  if FMemo.SyntaxStyles <> nil then
    lookFilter := FMemo.SyntaxStyles.Filter;

  // Give preference to Styler Manager
  if FMemo.StylerManager <> nil then
  begin
    sd.Filter := TAdvMemoStylerManager(FMemo.StylerManager).GetFilter(-1);
    sd.FilterIndex := GetFilterIndex(TAdvMemoStylerManager(FMemo.StylerManager), lookFilter);
  end
  else
  if FMemo.SyntaxStyles <> nil then
  begin
    sd.Filter := FMemo.SyntaxStyles.Filter;
    sd.DefaultExt := FMemo.SyntaxStyles.DefaultExtension;
  end;

  try
    if sd.Execute then
    begin
      OutFileName := sd.FileName;
      result := true;
    end;
  finally
    sd.Free;
  end;
end;

function TAdvMemoFileSaveAction.DoSaveFileAs(FMemo: TAdvMemo): boolean;
var
  AFileName: string;
begin
  result := false;
  if not assigned(FMemo) then
    exit;
  if not PromptSaveFileAs(FMemo, AFileName) then
    exit;
  FMemo.Lines.SaveToFile(AFileName);
  FMemo.Modified := false;
  FMemo.FileName := AFileName;

  SetStylerByExt(FMemo, TAdvMemoStylerManager(FMemo.StylerManager), FMemo.FileName);

  result := true;
end;

function TAdvMemoFileSaveAction.DoSaveFile(FMemo: TAdvMemo): boolean;
begin
  result := false;
  if not assigned(FMemo) then
    exit;
  if FMemo.FileName = '' then
  begin
    result := DoSaveFileAs(FMemo);
    exit;
  end;

  FMemo.Lines.SaveToFile(FMemo.FileName);
  FMemo.Modified := false;
  result := true;
end;

function TAdvMemoFileSaveAction.DoPromptAndSaveIfModified(FMemo: TAdvMemo): boolean;
var
  modalResult: integer;
  aFileName: string;
begin
  result := true;
  if FMemo.Modified then
  begin
    if FMemo.FileName = '' then
      aFileName := 'New'
    else
      aFileName := ExtractFileName(FMemo.FileName);
    modalResult := MessageDlg('Do you want to save changes to ' + aFileName + ' File ?', mtConfirmation, [mbYes, mbNO, mbCancel], 0);
    if ModalResult = mrYes then
    begin
      try
        result := DoSaveFile(FMemo);
      except
        result := false;
        ShowMessage('Error on saving file');
        exit;
      end;
    end
    else
    if ModalResult = mrNo then
    begin
      //discard entered text
      FMemo.FileName := '';
      FMemo.Lines.Clear;
      Fmemo.Modified := false;
      result := true; //modified ignored
    end
    else
    if ModalResult = mrCancel then
      result := false;
  end;
end;

procedure TAdvMemoFileSaveAction.ExecuteTarget(Target: TObject);
var
  FMemo: TAdvMemo;
begin
  FMemo := TAdvMemo(GetControl(Target));
  DoSaveFile(FMemo);
end;

procedure TAdvMemoFileSaveAction.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := TAdvMemo(GetControl(Target)).modified;
end;

{TAdvMemoFileSaveAsAction}

procedure TAdvMemoFileSaveAsAction.ExecuteTarget(Target: TObject);
var
  FMemo: TAdvMemo;
begin
  FMemo := TAdvMemo(GetControl(Target));
  DoSaveFileAs(FMemo);
end;

procedure TAdvMemoFileSaveAsAction.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := True;
end;

{TAdvMemoFileNewAction}

procedure TAdvMemoFileNewAction.ExecuteTarget(Target: TObject);
var
  FMemo: TAdvMemo;
begin
  FMemo := TAdvMemo(GetControl(Target));
  if not DoPromptAndSaveIfModified(FMemo) then
      exit;
  // Set the default styler
  SetStylerByExt(FMemo, TAdvMemoStylerManager(FMemo.StylerManager), '');
  // Better to keep this after so as to generate OnFileNameChange
  // after setting the styler
  FMemo.FileName := '';
  FMemo.Lines.Clear;
  Fmemo.Modified := false;
  if Assigned(FOnInitMemo) then
    FOnInitMemo(Self);
end;

procedure TAdvMemoFileNewAction.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := True;
end;

{ TAdvMemoFindAction }

procedure TAdvMemoFindAction.ExecuteTarget(Target: TObject);
var
  fd: TAdvMemoFindDialog;
begin
  fd := TAdvMemoFindDialog.Create(Self);
  fd.AdvMemo := GetControl(Target);
  fd.Options := fd.Options - [frWholeWord] + [frHideWholeWord, frHideUpDown];
  fd.Execute;
end;

procedure TAdvMemoFindAction.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := Length(GetControl(Target).Lines.Text) > 0;
end;

{ TAdvMemoReplaceAction }

procedure TAdvMemoReplaceAction.ExecuteTarget(Target: TObject);
var
  fd: TAdvMemoFindReplaceDialog;
begin
  fd := TAdvMemoFindReplaceDialog.Create(Self);
  fd.AdvMemo := GetControl(Target);
  fd.Options := fd.Options - [frWholeWord] + [frHideWholeWord, frHideUpDown];
  fd.Execute;
end;

procedure TAdvMemoReplaceAction.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := Length(GetControl(Target).Lines.Text) > 0;
end;

procedure TAdvMemoCut.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := GetControl(Target).SelLength > 0;
end;

procedure TAdvMemoCopy.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := GetControl(Target).SelLength > 0;
end;

end.
