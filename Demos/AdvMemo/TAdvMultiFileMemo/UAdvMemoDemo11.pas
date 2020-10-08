{*************************************************************************}
{ TAdvMemo, TDBAdvMemo demo application                                   }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1998-2019                                        }
{            Email : info@tmssoftware.com                                 }
{            Web : https://www.tmssoftware.com                            }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

{ Notable features:
  -- Aplication is relieved of the task to create and assign
     language Stylers. Many stylers are Built-in (those listed
     in TAdvMultiFileStyler. They can be changed selectively ty
     using the property StylersBuiltin in Object Inspector.

  -- Once you do that, the stylers are created automatically
     and are applied on OpenMemoFile based on the file extension.

  -- Advanced programmers can add stylers not included in the above
     list or can override an existing styler by supplying their
     own styler collection in a Styler Manager component made up
     on the form or created dynamically through the property
     StylersCustom. Overriding occurs because the Custom collection
     is searched first for a styler based on the File Extension.

  -- The convenience method GetOpenDialogFilter is useful to get
     all the filters for OpenDialog based on the selections of
     stylers made above, including Custom and Built-in.

  -- Method  OpenMemoFile:
    -- Opens the file in a new memo in a new tab and assigns proper
       styler based on the file extension.
    -- Sends the new memo to the application in the event OnInitMemo
       so that the application can set default properties for the
       new memo. For example, this demo sets up word wrap and removes
       right margin for normal TXT files, identified by the condition
       when no styler is assigned.

  -- The event OnCloseMemoPage gives the application a chance
     to examine isModified and take necessary actions-- close,
     no close and can save before close.

  -- Method OpenUntitledMemo helps to implement File--New where
     an Untitled memo is opened with the given Caption. Even a
     styler is assigned based on the extension used in the Caption.
     If no styler is found for the extension, a DefaultStyler can
     also be assigned via that property.

  -- The Event OnSaveUntitled allows the Application to use its
     own Save prompt to ask for FileName when saving an Untitled Memo

  -- Convenience method SaveMemo uses the above event for an Untitled memo.

  -- Convenience method SaveAll can be used to save all memos and
     uses the above event for Untitled Memos.

  -- Convenience method CanCloseQuery can be straightaway used
     for FormCloseQuery implementation that does a proper save of
     all memos that need saving before returning true.

  -- Convenience method CloseAllPages can be useful to do a similar
     save as above and also close all the pages.
}


unit UAdvMemoDemo11;

interface

uses
  Windows, Messages, SysUtils, UITypes, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, AdvOfficePager,
  AdvMultiFileMemo, AdvMemo, AdvMemoStylerManager, Menus,
  Rtti, Generics.Collections, AdvmWS, AdvmCSS, AdvmCSHS, Vcl.ImgList;

type
  TTFAdvMultiFileMemoDemo01 = class(TForm)
    AdvMultiFileMemo1: TAdvMultiFileMemo;
    OpenDialog1: TOpenDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenFile1: TMenuItem;
    SaveFile1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Close1: TMenuItem;
    AdvCSSMemoStyler1: TAdvCSSMemoStyler;
    AdvMemoStylerManager1: TAdvMemoStylerManager;
    AdvHTMLMemoStyler1: TAdvHTMLMemoStyler;
    New1: TMenuItem;
    SaveAll1: TMenuItem;
    N1: TMenuItem;
    N3: TMenuItem;
    Settings1: TMenuItem;
    ActiveLineShow1: TMenuItem;
    Gutter1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    CloseAll1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure AdvMultiFileMemo1InitMemo(Sender: TObject; NewMemo: TAdvMemo);
    procedure OpenFile1Click(Sender: TObject);
    procedure AdvMultiFileMemo1CloseMemoPage(Sender: TObject;
      AMemoPage: TAdvMultiFileMemoPage; var AllowClose: Boolean);
    procedure SaveFile1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure SaveAll1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActiveLineShow1Click(Sender: TObject);
    procedure Gutter1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure CloseAll1Click(Sender: TObject);
    procedure AdvMultiFileMemo1SaveUntitled(Sender: TObject;
      UntitledMemoPage: TAdvMultiFileMemoPage; var SaveFileName: string);
  private
  public
  end;

var
  TFAdvMultiFileMemoDemo01: TTFAdvMultiFileMemoDemo01;

implementation

uses
  TypInfo;

{$R *.dfm}

//---------------------------------------------
procedure TTFAdvMultiFileMemoDemo01.OpenFile1Click(Sender: TObject);
begin
  openDialog1.FileName := '';
  if not openDialog1.execute then
    exit;
  { Features related to open:
    -- You don't have to create stylers. You just need to specify what
       languages you need in the property StylersBuiltin in Object
       Inspector.
    -- Creates and assigns a styler based on File Extension internally
    -- Also checks for file already open and activates tab if present
  }
  if not AdvMultiFileMemo1.OpenMemoFile(OpenDialog1.FileName) then
    ShowMessage('Error on opening file');
end;

//---------------------------------------------
procedure TTFAdvMultiFileMemoDemo01.FormCreate(Sender: TObject);
begin
  // File opening wildcard filters are determined based on what
  // languages you have ticked ON in the property StylersBuiltin
  // in the Object Inspector
  OpenDialog1.Filter := AdvMultiFileMemo1.GetOpenDialogFilter(true, true);
end;

//---------------------------------------------
procedure TTFAdvMultiFileMemoDemo01.AdvMultiFileMemo1InitMemo(Sender: TObject; NewMemo: TAdvMemo);
begin
  // For TXT files, switch on word wrap and remove right margin
  if NewMemo.SyntaxStyles = nil then
  begin
    NewMemo.WordWrap := wwClientWidth;
    NewMemo.showRightMargin := false;
  end;
end;

//---------------------------------------------
procedure TTFAdvMultiFileMemoDemo01.SaveFile1Click(Sender: TObject);
begin
  if AdvMultiFileMemo1.ActiveMemoPage = nil then
    exit;
  try
    // A false return indicates an Untitled memo not saved
    if not AdvMultiFileMemo1.ActiveMemoPage.SaveMemo then
      ShowMessage('Untitled Memo was not saved');
  except
    ShowMessage('Error on saving file');
  end;
end;

//---------------------------------------------
procedure TTFAdvMultiFileMemoDemo01.SaveAs1Click(Sender: TObject);
var
  aMemo: TAdvMemo;
  APage: TAdvMultiFileMemoPage;
begin
  if AdvMultiFileMemo1.ActiveMemoPage = nil then
    exit;
  AMemo := AdvMultiFileMemo1.ActiveMemoPage.Memo;

  //Populate save dialog property based on Stylers
  SaveDialog1.Title := 'Save As';
  SaveDialog1.FileName := '';
  if AMemo.SyntaxStyles <> nil then
  begin
    SaveDialog1.Filter := AMemo.SyntaxStyles.Filter;
    SaveDialog1.DefaultExt := AMemo.SyntaxStyles.DefaultExtension;
  end
  else
  begin
    SaveDialog1.Filter := 'Text Files (*.txt)|*.txt';
    SaveDialog1.DefaultExt := 'txt';
  end;
  if not SaveDialog1.Execute then
    exit;
  //check for already open file
  APage := AdvMultiFileMemo1.GetPageWithFile(SaveDialog1.FileName);
  if APage <> nil then
  begin
    ShowMessage('That file is already open. Please select another file name.');
    exit;
  end;
  try
    AdvMultiFileMemo1.ActiveMemoPage.SaveMemoAs(SaveDialog1.FileName);
  except
    ShowMessage('Error on saving file');
  end;
end;

//---------------------------------------------
procedure TTFAdvMultiFileMemoDemo01.Close1Click(Sender: TObject);
begin
  if AdvMultiFileMemo1.ActiveMemoPage = nil then
    exit;
  AdvMultiFileMemo1.CloseActivePage;
end;

//---------------------------------------------
procedure TTFAdvMultiFileMemoDemo01.SaveAll1Click(Sender: TObject);
begin
  if AdvMultiFileMemo1.ActiveMemoPage = nil then
    exit;
  try
    // Saves all pages only if modified unless it's an untitled page
    if not AdvMultiFileMemo1.SaveAll then
      ShowMessage('Save All could not be completed');
  except
    ShowMessage('Error on saving file');
  end;
end;

//---------------------------------------------
// Sample operation on the active memo
procedure TTFAdvMultiFileMemoDemo01.ActiveLineShow1Click(Sender: TObject);
var
  AMemo: TAdvMemo;
begin
  if AdvMultiFileMemo1.ActiveMemoPage = nil then
    exit;
  AMemo := AdvMultiFileMemo1.ActiveMemoPage.Memo;
  AMemo.ActiveLineSettings.ShowActiveLine := (Sender as TMenuItem).Checked;
  AMemo.ActiveLineSettings.ActiveLineAtCursor := true; ;
end;

//---------------------------------------------
// Sample operation on the active memo
procedure TTFAdvMultiFileMemoDemo01.Gutter1Click(Sender: TObject);
var
  AMemo: TAdvMemo;
begin
  if AdvMultiFileMemo1.ActiveMemoPage = nil then
    exit;
  AMemo := AdvMultiFileMemo1.ActiveMemoPage.Memo;
  AMemo.Gutter.Visible := (Sender as TMenuItem).Checked;
end;

//---------------------------------------------
// Sample operation on the active memo
procedure TTFAdvMultiFileMemoDemo01.Copy1Click(Sender: TObject);
var
  AMemo: TAdvMemo;
begin
  if AdvMultiFileMemo1.ActiveMemoPage = nil then
    exit;
  AMemo := AdvMultiFileMemo1.ActiveMemoPage.Memo;
  AMemo.CopyToClipBoard;
end;

//---------------------------------------------
// Sample operation on the active memo
procedure TTFAdvMultiFileMemoDemo01.Paste1Click(Sender: TObject);
var
  AMemo: TAdvMemo;
begin
  if AdvMultiFileMemo1.ActiveMemoPage = nil then
    exit;
  AMemo := AdvMultiFileMemo1.ActiveMemoPage.Memo;
  AMemo.PasteFromClipBoard;
end;

//---------------------------------------------
procedure TTFAdvMultiFileMemoDemo01.New1Click(Sender: TObject);
begin
  // A file extension automatically uses that styler. If no Styler
  // is found for the extension then the Styler assigned to
  // DefaultStyler is used by the component.
  if not AdvMultiFileMemo1.OpenUntitledMemo('Untitled.css') then
    ShowMessage('Error on opening the new file');
end;

//---------------------------------------------
// In this Event, Application uses its own Save prompt to ask
// for FileName when saving an Untitled Memo
procedure TTFAdvMultiFileMemoDemo01.AdvMultiFileMemo1SaveUntitled(
  Sender: TObject; UntitledMemoPage: TAdvMultiFileMemoPage;
  var SaveFileName: string);
begin
  SaveDialog1.Title := 'Save Untitled Memo';

  // Default empty name sent so as to stop the Save
  SaveDialog1.FileName := '';

  with UntitledMemoPage do
  if Memo.SyntaxStyles <> nil then
  begin
    SaveDialog1.Filter := Memo.SyntaxStyles.Filter;
    SaveDialog1.DefaultExt := Memo.SyntaxStyles.DefaultExtension;
  end
  else
  begin
    SaveDialog1.Filter := AdvMultiFileMemo1.GetOpenDialogFilter(true, false);
    SaveDialog1.DefaultExt := 'txt';
  end;
  if SaveDialog1.Execute then
    SaveFileName := SaveDialog1.FileName;
end;

//---------------------------------------------
// In this event, Application gets a chance to prompt and Save
// and also allow or prevent a close when a page is being closed.
procedure TTFAdvMultiFileMemoDemo01.AdvMultiFileMemo1CloseMemoPage(
  Sender: TObject; AMemoPage: TAdvMultiFileMemoPage; var AllowClose: Boolean);
var
  modalResult: integer;
begin
  if AMemoPage.IsModified then
  begin
    modalResult := MessageDlg('Do you want to save changes to '+AMemoPage.Caption + '?', mtConfirmation, [mbYes, mbNO, mbCancel], 0);
    if ModalResult = mrYes then
    begin
      try
        AllowClose := AMemoPage.SaveMemo(true);
      except
        ShowMessage('Error on saving file');
        AllowClose := false;
        exit;
      end;
    end
    else
    if ModalResult = mrNo then
      AllowClose := true //no need to save before closing
    else
    if ModalResult = mrCancel then
      AllowClose := false;
  end;
end;

//---------------------------------------------
procedure TTFAdvMultiFileMemoDemo01.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  // Goes through all the pages, inquiring with the application by
  // using the event described above whether it can be closed.
  // Returns false if application refuses it for any page.
  CanClose := AdvMultiFileMemo1.CanCloseQuery;
end;

//---------------------------------------------
procedure TTFAdvMultiFileMemoDemo01.CloseAll1Click(Sender: TObject);
begin
  // Uses the same inquiry as above and also closes the pages
  AdvMultiFileMemo1.CLoseAllPages;
end;

end.
