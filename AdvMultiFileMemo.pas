{***************************************************************************}
{ TAdvMultiFileMemoPage component                                           }
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
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvMultiFileMemo;


interface

uses
  SysUtils, Classes, Windows, Messages, Controls, ImgList, AdvOfficePager,
  Generics.Collections, AdvMemo, AdvmBS, AdvmCSHS, AdvMemoStylerManager,
  AdvmPS, AdvmSQLS, AdvmWS;

{$R ADVMULTIFILEMEMO.RES}

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.0.0.0 : Initial release
  // 1.0.0.1 : Fixed : Issue with Modified handling wrt Undo


type
  // These stylers are builtin and shown in the StylersBuiltin
  // property in the Object Inspector where they can be selected.
  TAdvMultiFileStyler = (styBasic, styCSharp, styPascal, styHtml, styJs, stySql);
  TAdvMultiFileStylers = set of TAdvMultiFileStyler;

  TAdvCustomMemoStylerClass = class of TAdvCustomMemoStyler;

  // We need our own Page class to save some information
  // and save our logic.
  TAdvMultiFileMemoPage = class(TAdvOfficePage)
  private
    FFileName: string;
    FMemo: TAdvMemo;
    FIsModified: boolean;
    FIsUntitled: boolean;
  protected
    procedure SetMemo(AMemo: TAdvMemo);
    procedure DoOnChange(Sender: TObject);
    procedure SetModified(Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function OpenFile(AFileName: string): boolean; virtual;
    function OpenUntitled(ACaption: string): boolean; virtual;
    function HasFileOpened(AFileName: string): boolean;

    // A false return means, the untitled memo was not saved
    // as a OnSaveUntitled prompt was canceled by the application
    function SaveMemo(OnlyIfModified: boolean=false): boolean; virtual;

    //A new styler is assigned based on new File Name extension
    procedure SaveMemoAs(AFileName: string); virtual;

    property FileName: string read FFileName;
    property Memo: TAdvMemo read FMemo write SetMemo;
    property IsModified: boolean read FIsModified write SetModified;
    property IsUntitled: boolean read FIsUntitled write FIsUntitled;
  end;

  // Application gets chance to set default properties of a
  // TAdvMemo being created on a tab.
  TAdvMultiFileInitMemoEvent = procedure(Sender: TObject; NewMemo: TAdvMemo) of object;

  // Application can ask and save the memo if needed.
  // Application has to return AllowClose true to close.
  TAdvMultiFileCloseMemoPageEvent = procedure(Sender: TObject; AMemoPage: TAdvMultiFileMemoPage; var AllowClose: boolean) of object;

  // Application gets a chance to prompt for a file name to save to.
  // Passing an empty SaveFileName causes the Save to be skipped.
  TAdvMultiFileSaveUntitledEvent = procedure(Sender: TObject; UntitledMemoPage: TAdvMultiFileMemoPage; var SaveFileName: string) of object;

  TAdvMultiFileMemo = class(TAdvOfficePager)
  private
    //members to dynamically determine the stylers required
    FStylersBuiltin: TAdvMultiFileStylers;

    //creates and keeps list of styler instances required
    FStylerManager: TAdvMemoStylerManager;

    //External Styles passed by the application
    FStylersCustom: TAdvMemoStylerManager;

    // Used to assign a styler to an Untitled Memo with an extension
    // in Caption for which no Styler is found.
    FDefaultStyler: TAdvCustomMemoStyler;

    FOnInitMemo: TAdvMultiFileInitMemoEvent;
    FOnCloseMemoPage: TAdvMultiFileCloseMemoPageEvent;
    FOnSaveUntitled: TAdvMultiFileSaveUntitledEvent;

    FModifyStateImgList : TImageList;
    FShowModified: boolean;

  protected
    function CreateDesignPages: boolean; override;
    procedure DrawDesignHelp; override;
    function GetVersionNr: integer; override;
    // At run time creates Stylers as set up in StylersBuiltin property
    procedure PopulateStylerManager;
    procedure SetStylersBuiltin(const Value: TAdvMultiFileStylers);
    function GetActiveMemoPage: TAdvMultiFileMemoPage;
    function GetActiveMemo: TAdvMemo;
    function GetStylerByFileName(AFileName: string): TAdvCustomMemoStyler;
    function SaveUntitledMemo(UntitledMemoPage: TAdvMultiFileMemoPage): boolean;
    procedure DoInitMemo(AMemo: TAdvMemo); virtual;
    procedure DoCloseMemo(AMemo: TAdvMultiFileMemoPage; var CanClose: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;

    // Application can get the full set of filters needed for
    // all supported languages set up in StylersBuiltin property
    function GetOpenDialogFilter(IncludeTextFiles: boolean; IncludeAllFiles: boolean): string; virtual;

    // Opens the file in a new Memo on a new Tab
    // Creates and assigns a styler based on File Extension internally
    // Also checks for file already open and activates tab if present
	  function OpenMemoFile(AFileName: string): boolean; virtual;

    // Can be used for File--New type action. If a Caption is passed
    // a Styler is assigned based on the extension in the Caption. If
    // none found, the DefaultStyler property's Styler is used.
	  function OpenUntitledMemo(ACaption: string=''): boolean; virtual;

    // Saves all memos, including untitled memos for which OnSaveUntitled
    // event is used by application to prompt for file name.
    // If it returns false then it means an Untitled memo could
    // not be saved as the Save Prompt was canceled.
    function SaveAll: boolean; virtual;

    // Goes through all the pages, inquiring with the application
    // if it can be closed. Returns false if application refuses it
    // for any page. Can be used in FormCloseQuery by the application.
    function CanCloseQuery: boolean; virtual;


    // First performs same logic as described above for
    // CanCloseQuery. Then if all the pages are properly
    // saved and can be closed, also closes them.
    function CloseAllPages: boolean; virtual;

    function GetPageWithFile(AFileName: string): TAdvMultiFileMemoPage;

    // Gets the first modified page
    function GetModifiedPage: TAdvMultiFileMemoPage;

    // Any of the pages is modified
    function GetIsModified: boolean; virtual;

    property ActiveMemoPage: TAdvMultiFileMemoPage read GetActiveMemoPage;
    property ActiveMemo: TAdvMemo read GetActiveMemo;
    property IsModified: boolean read GetIsModified;
  published
    // Application gets chance to set default properties of a
    // TAdvMemo being created on a tab.
    property OnInitMemo: TAdvMultiFileInitMemoEvent read FOnInitMemo write FOnInitMemo;

    // Application gets to decide whether to ask for save before
    // closing or cancel.
    property OnCloseMemoPage: TAdvMultiFileCloseMemoPageEvent read FOnCloseMemoPage write FOnCloseMemoPage;

    // Application gets a chance to prompt for a file name to save to.
    // Passing an empty SaveFileName causes the Save to be skipped
    property OnSaveUntitled: TAdvMultiFileSaveUntitledEvent read FOnSaveUntitled write FOnSaveUntitled;

    // Aplication is relieved of the task to create and assign
    // language Stylers. Many stylers are builtin. All one
    // needs to do is customize the desired stylers in the
    // Object Inspector for the property StylersBuiltin.
    property StylersBuiltin: TAdvMultiFileStylers read FStylersBuiltin write SetStylersBuiltin default [];
    // Application can pass Custom stylers via a Styler Manager containing those
    // stylers.
    property StylersCustom: TAdvMemoStylerManager read FStylersCustom write FStylersCustom default nil;

    // Used to assign a styler to an Untitled Memo for which no
    // Styler is found based on the Caption's extension
    property DefaultStyler: TAdvCustomMemoStyler read FDefaultStyler write FDefaultStyler default nil;

    property ShowModified: boolean read FShowModified write FShowModified default true;
  end;


implementation

uses
  Graphics;

//------------------------------------------------------------------------------
constructor TAdvMultiFileMemoPage.Create(AOwner: TComponent);
begin
  inherited;

  FFileName := '';
  FMemo := nil;
  IsModified := false;
  IsUntitled := false;
end;

//------------------------------------------------------------------------------
function TAdvMultiFileMemoPage.OpenFile(AFileName: string): boolean;
begin
  Result := false;

  try
    FMemo.Lines.LoadFromFile(AFileName);
  except
    Exit;
  end;

  Result := true;
  IsModified := false;
  FFileName := AFileName;
  Caption := ExtractFileName(AFileName);
  //done last so as not to set IsModified when loading
  FMemo.OnChange := DoOnChange;
  FMemo.CurX := 0;
  FMemo.CurY := 0;
  FMemo.SetFocus;
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemoPage.OpenUntitled(ACaption: string): boolean;
begin
  //This makes sure that the memo can be closed if just created
  //new and nothing is entered
  IsModified := false;
  IsUntitled := true;
  Caption := ACaption;
  //done last so as not to set IsModified when loading
  FMemo.OnChange := DoOnChange;
  FMemo.CurX := 0;
  FMemo.CurY := 0;
  FMemo.Clear;
  FMemo.SetFocus;
  Result := true;
end;

//------------------------------------------------------------------------------

procedure TAdvMultiFileMemoPage.SetMemo(AMemo: TAdvMemo);
begin
  FMemo := AMemo;
end;

//------------------------------------------------------------------------------

procedure TAdvMultiFileMemoPage.DoOnChange(Sender: TObject);
begin
  IsModified := FMemo.Modified;
end;

//------------------------------------------------------------------------------

procedure TAdvMultiFileMemoPage.SetModified(Value: boolean);
var
  useIndex: integer;
begin
  if csLoading in ComponentState then
    Exit;

  FIsModified := Value;

  if Assigned(Parent) then
  begin
    if not (Parent as TAdvMultiFileMemo).ShowModified then
    begin
      ImageIndex :=-1;
      Exit;
    end;
  end;

  useIndex := 0;
  if Value then
    useIndex := 1;
  if ImageIndex <> useIndex then
    ImageIndex := useIndex;
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemoPage.HasFileOpened(AFileName: string): boolean;
begin
  Result := SameText(FFileName, AFileName);
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemoPage.SaveMemo(OnlyIfModified: boolean=false): boolean;
begin
  if IsUntitled then
  begin
    //Activate as prompt may be needed
    (Parent as TAdvMultiFileMemo).ActivePage := Self;
    Result :=(Parent as TAdvMultiFileMemo).SaveUntitledMemo(self);
    Exit;
  end;

  if OnlyIfModified and IsModified then
  begin
    Memo.lines.SaveToFile(FFileName);
    IsModified := false;
  end;
  Result := true;
end;

//------------------------------------------------------------------------------

procedure TAdvMultiFileMemoPage.SaveMemoAs(AFileName: string);
begin
  Memo.lines.SaveToFile(AFileName);
  FFileName := AFileName;
  Caption := ExtractFileName(AFileName);
  IsModified := false;
  IsUntitled := false;
end;

//------------------------------------------------------------------------------

constructor TAdvMultiFileMemo.Create(AOwner: TComponent);
var
  ABitmap: TBitmap;
begin
  inherited;
  //All builtins are ON by default
  FStylersBuiltin := [styBasic, styCSharp, styPascal, styHtml, styJs, stySql];
  FStylerManager := nil;
  FShowModified := true;
  CloseOnTab := True;
  FreeOnClose := true;

  FModifyStateImgList := TImageList.Create(Self);
  FModifyStateImgList.height := 16;
  FModifyStateImgList.width := 16;
  FModifyStateImgList.DrawingStyle := dsTransparent;
  FModifyStateImgList.Masked := true;
  ABitmap := TBitmap.Create;                                        //Index:
  try
    ABitmap.LoadFromResourceName(HInstance,'TMSAODOC');          //O:
    FModifyStateImgList.AddMasked(ABitmap, clFuchsia);
    ABitmap.LoadFromResourceName(HInstance,'TMSAODOCEDIT');          //O:
    FModifyStateImgList.AddMasked(ABitmap, clFuchsia);
    //If DFM assigns Images, it overrides this automatically
    Images := FModifyStateImgList;
  finally
    ABitmap.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMultiFileMemo.CreateWnd;
begin
  inherited;
  ButtonSettings.CloseButton := True;
  ButtonSettings.CloseButtonLook := cblChrome;
  ShowCloseOnNonSelectedTabs := True;

  // Do not create StylerManager in design or when loading DFM
  if not
     ((csReading in Owner.ComponentState)
       or (csLoading in Owner.ComponentState)
       or (csDesigning in Owner.ComponentState)
     )
  then
    PopulateStylerManager;
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemo.CreateDesignPages: boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------

procedure TAdvMultiFileMemo.DrawDesignHelp;
begin
  //no design time help
end;

//------------------------------------------------------------------------------

procedure TAdvMultiFileMemo.PopulateStylerManager;
var
  AStylerClass: TAdvCustomMemoStylerClass;
  AStyler: TAdvMultiFileStyler;
  AMemoStyler: TAdvCustomMemoStyler;
  AnItem: TAdvMemoStylersCollectionItem;
begin
  if FStylerManager = nil then
    FStylerManager := TAdvMemoStylerManager.Create(Owner);
  for AStyler := Low(TAdvMultiFileStyler) to High(TAdvMultiFileStyler) do
  begin
    if AStyler in FStylersBuiltin then
    begin
      AStylerClass := nil;
      case AStyler of
        styBasic:
            AStylerClass := TAdvBasicMemoStyler;
        styCSharp:
            AStylerClass := TAdvCSharpMemoStyler;
        styPascal:
            AStylerClass := TAdvPascalMemoStyler;
        styHtml:
            AStylerClass := TAdvHTMLMemoStyler;
        styJs:
            AStylerClass := TAdvJSMemoStyler;
        stySql:
            AStylerClass := TAdvSQLMemoStyler;
      end;
      // For refresh, the following logic is needed
      // Because populate.. can be called from 2 different
      // places and in future from SetStylersBuiltin method too.
      if (AStylerClass <> nil) then
      begin
        AMemoStyler := AStylerClass.Create(Owner);
        // The problem is, FStylerManager can only search by
        // StylerName that we get only after creating the object
        if FStylerManager.GetStylerByName(AMemoStyler.StylerName) <> nil then
          AMemoStyler.Free
        else
        begin
          AnItem := FStylerManager.Items.Add;
          AnItem.Styler := AMemoStyler;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMultiFileMemo.SetStylersBuiltin(const Value: TAdvMultiFileStylers);
begin
  if Value <> FStylersBuiltin then
  begin
    FStylersBuiltin := Value;
    // Run time refresh not yet supported because Populate
    // function does not remove styler from the collection
    // when the value is switched off. If needed, a parameter
    // needs to be passed to clear the list.
    // PopulateStylerManager;
  end;
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemo.GetActiveMemoPage: TAdvMultiFileMemoPage;
begin
  Result := nil;
  if ActivePageIndex = -1 then
    Exit;
  Result := TAdvMultiFileMemoPage(AdvPages[ActivePageIndex]);
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemo.GetActiveMemo: TAdvMemo;
var
  APage: TAdvMultiFileMemoPage;
begin
  Result := nil;
  APage := GetActiveMemoPage;
  if APage = nil then
    Exit;
  Result := APage.Memo;
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemo.GetOpenDialogFilter(IncludeTextFiles: boolean; IncludeAllFiles: boolean): string;
var
  I: integer;
  FilterList: TStringList;
begin
  Result := '';
  PopulateStylerManager;
  FilterList := TStringList.Create;
  FilterList.Sorted := true;
  FilterList.Duplicates := dupIgnore;
  try
    if Assigned(FStylersCustom) then
    begin
      for I := 0 to FStylersCustom.Items.Count-1 do
        FilterList.Add(FStylersCustom.Items[I].Styler.Filter + '|');
    end;
    for I := 0 to FStylerManager.Items.Count-1 do
      FilterList.Add(FStylerManager.Items[I].Styler.Filter + '|');

    if IncludeTextFiles then
      FilterList.Add('Text Files (*.txt)|*.txt|');

    Result := FilterList.Text;
  finally
    FilterList.Free;
  end;
  if IncludeAllFiles then
    Result := Result + 'All files (*.*)|*.*';
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemo.GetPageWithFile(AFileName: string): TAdvMultiFileMemoPage;
var
  I: integer;
begin
  for I := 0 to AdvPageCount-1 do
  begin
    Result := TAdvMultiFileMemoPage(AdvPages[I]);
    if SameText(result.FileName, AFileName) then
      Exit;
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemo.GetModifiedPage: TAdvMultiFileMemoPage;
var
  I: integer;
begin
  for I := 0 to AdvPageCount-1 do
  begin
    Result := TAdvMultiFileMemoPage(AdvPages[I]);
    if Result.IsModified then
      Exit;
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemo.GetIsModified: boolean;
begin
  Result := false;
  if GetModifiedPage = nil then
    Exit;
  Result := true;
end;

//------------------------------------------------------------------------------

function TAdvMultiFileMemo.GetStylerByFileName(AFileName: string): TAdvCustomMemoStyler;
begin
  Result := nil;
  if Assigned(FStylersCustom) then
    Result := FStylersCustom.GetStylerByFileName(AFileName);
  if Result = nil then
  begin
    if not Assigned(FStylerManager) then
      PopulateStylerManager;

    if Assigned(FStylerManager) then
      Result := FStylerManager.GetStylerByFileName(AFileName);
  end;
end;

function TAdvMultiFileMemo.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------
function TAdvMultiFileMemo.openMemoFile(AFileName: string): boolean;
var
  NewMemo: TAdvMemo;
  APage: TAdvMultiFileMemoPage;
  AStyler: TAdvCustomMemoStyler;
begin
  APage := GetPageWithFile(AFileName);
  if APage <> nil then
  begin
    ActivePage := APage;
    Result := true;
    Exit;
  end;
  APage := TAdvMultiFileMemoPage.Create(Owner);
  APage.AdvOfficePager := Self;
  ActivePage := APage;
  NewMemo := TAdvMemo.Create(Owner);
  NewMemo.Align := alClient;
  NewMemo.Parent := APage;
  APage.Memo := NewMemo;
  APage.TabHint := AFileName;
  AStyler := GetStylerByFileName(AFileName);
  if AStyler <> nil then
    NewMemo.SyntaxStyles := AStyler
  else
    NewMemo.SyntaxStyles := DefaultStyler;

  DoInitMemo(NewMemo);

  Result := APage.OpenFile(AFileName);
  if not Result then
    CloseActivePage;
end;

//------------------------------------------------------------------------------

procedure TAdvMultiFileMemo.DoCloseMemo(AMemo: TAdvMultiFileMemoPage; var CanClose: boolean);
begin
  if Assigned(OnCloseMemoPage) then
    OnCloseMemoPage(Self, AMemo, CanClose);
end;

//------------------------------------------------------------------------------

procedure TAdvMultiFileMemo.DoInitMemo(AMemo: TAdvMemo);
begin
  if Assigned(FOnInitMemo) then
    FOnInitMemo(Self, AMemo);
end;

//------------------------------------------------------------------------------
function TAdvMultiFileMemo.OpenUntitledMemo(ACaption: string = ''): boolean;
var
  NewMemo: TAdvMemo;
  APage: TAdvMultiFileMemoPage;
  AStyler: TAdvCustomMemoStyler;
begin
  APage := TAdvMultiFileMemoPage.Create(Owner);
  APage.AdvOfficePager := Self;
  APage.IsUntitled := true;
  ActivePage := APage;
  NewMemo := TAdvMemo.Create(Owner);
  NewMemo.Align := alClient;
  NewMemo.Parent := APage;
  APage.Memo := NewMemo;
  if ACaption = '' then
    ACaption := 'Untitled';

  // If caption was passed with extension, a styler is
  // searched for. If no extension, will return a nil anyway
  AStyler := GetStylerByFileName(ACaption);
  if AStyler <> nil then
    NewMemo.SyntaxStyles := AStyler
  else
    NewMemo.SyntaxStyles := DefaultStyler;

  DoInitMemo(NewMemo);

  Result := APage.OpenUntitled(ACaption);
  if not Result then
    CloseActivePage;
end;

//------------------------------------------------------------------------------
function TAdvMultiFileMemo.SaveUntitledMemo(UntitledMemoPage: TAdvMultiFileMemoPage): boolean;
var
  AFileName: string;
  AStyler: TAdvCustomMemoStyler;
begin
  Result := false;
  if Assigned(FOnSaveUntitled) then
  begin
    AFileName := '';
    FOnSaveUntitled(Self, UntitledMemoPage, AFileName);
    if AFileName <> '' then
    begin
      //On real error, let it create exception for application
      UntitledMemoPage.SaveMemoAs(AFileName);
      AStyler := GetStylerByFileName(AFileName);
      if AStyler <> nil then
        UntitledMemoPage.Memo.SyntaxStyles := AStyler
      else
        UntitledMemoPage.Memo.SyntaxStyles := DefaultStyler;
      Result := true;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAdvMultiFileMemo.SaveAll: boolean;
var
  I: integer;
begin
  Result := true;

  //The active page must be saved first before starting the loop
  if ActiveMemoPage <> nil then
  begin
    Result := ActiveMemoPage.SaveMemo(true);
    if not Result then
      Exit;
  end;

  //Save all other pages only if modified unless untitled
  for I := 0 to AdvPageCount-1 do
  begin
    Result := TAdvMultiFileMemoPage(AdvPages[I]).SaveMemo(true);
    if not Result then
      Exit;
  end;
end;

//------------------------------------------------------------------------------
function TAdvMultiFileMemo.CanCloseQuery: boolean;
var
  I: integer;
  APage: TAdvMultiFileMemoPage;
  AllowClose: boolean;
begin
  result := true;
  for I := 0 to AdvPageCount-1 do
  begin
    APage := TAdvMultiFileMemoPage(AdvPages[I]);
    ActivePage := APage;
    AllowClose := true;

    DoCloseMemo(APage, AllowClose);

    if not AllowClose then
    begin
      Result := false;
      Exit;
    end;
    // Application should have saved the memo
    // If not, we need to discard it for subsequent close
    APage.IsModified := false;
  end;
end;

//------------------------------------------------------------------------------
function TAdvMultiFileMemo.CloseAllPages: boolean;
begin
  Result := false;
  if not CanCloseQuery then
    Exit;

  while AdvPageCount > 0 do
  begin
    ActivePage := TAdvMultiFileMemoPage(AdvPages[0]);
    CloseActivePage;
  end;
  Result := true;
end;

end.