{*********************************************************************}
{ TAdvDirectoryEdit                                                   }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by                                                          }
{  TMS Software                                                       }
{  copyright � 2002 - 2019                                            }
{  Email : info@tmssoftware.com                                       }
{  Web : http://www.tmssoftware.com                                   }
{                                                                     }
{ The source code is given as is. The author is not responsible       }
{ for any possible damage done due to the use of this code.           }
{ The component can be freely used in any application. The source     }
{ code remains property of the author and may not be distributed      }
{ freely as such.                                                     }
{*********************************************************************}

unit AdvDirectoryEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AdvEdBtn, AdvEdit, Types;

{$I TMSDEFS.INC}

type
  TDirectoryShowEvent = procedure(Sender: TObject; var InitialPath: string) of object;

  TDialogCloseEvent = procedure(Sender: TObject; var NewDirectory: string; OK: boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDirectoryEdit = class(TAdvEditBtn)
  private
    { Private declarations }
    FDummy: Byte;
    FOnClickBtn:TNotifyEvent;
    FOnValueValidate: TValueValidateEvent;
    FBrowseDialogText: string;
    FAllowNewFolder: Boolean;
    FOnShowDirectory: TDirectoryShowEvent;
    FOnDialogClose: TDialogCloseEvent;
    FAllowShares: Boolean;
    FOldText: string;
    FAutoDirectoryLookup: boolean;
    procedure SetAutoDirectoryLookup(const Value: boolean);
  protected
    { Protected declarations }
    procedure BtnClick (Sender: TObject); override;
    procedure ValueValidate(Sender: TObject; Value: String; Var IsValid: Boolean); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function IsValidDirectory: Boolean;
    procedure ClickButton;
  published
    property AutoDirectoryLookup: boolean read FAutoDirectoryLookup write SetAutoDirectoryLookup default false;
    property AutoThousandSeparator: Byte read FDummy;
    property AllowNewFolder: Boolean read FAllowNewFolder write FAllowNewFolder default False;
    property AllowShares: Boolean read FAllowShares write FAllowShares default false;
    property BrowseDialogText:string read FBrowseDialogText write FBrowseDialogText;
    property EditAlign: Byte read FDummy;
    property EditType: Byte read FDummy;
    property ExcelStyleDecimalSeparator: Byte read FDummy;
    property PasswordChar: Byte read FDummy;
    property Precision: Byte read FDummy;
    property Signed: Byte read FDummy;
    property ShowURL: Byte read FDummy;
    property URLColor: Byte read FDummy;
    property OnClickBtn: TNotifyEvent read FOnClickBtn write FOnClickBtn;
    property OnDialogClose: TDialogCloseEvent read FOnDialogClose write FOnDialogClose;
    property OnValueValidate: TValueValidateEvent read FOnValueValidate write FOnValueValidate;
    property OnShowDirectoryDialog: TDirectoryShowEvent read FOnShowDirectory write FOnShowDirectory;
  end;

implementation

uses
 {$WARNINGS OFF}
 // avoid platform specific warning
  FileCtrl, ShlObj, ActiveX;
 {$WARNINGS ON}

const
  BIF_NONEWFOLDERBUTTON = $0200;
  BIF_NEWDIALOGSTYLE    = $0040;

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;

var
  _initdir: string;

{$R *.RES}

constructor TAdvDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Glyph.LoadFromResourceName (HInstance, 'AdvDirectoryEdit');
  Button.OnClick := BtnClick;
  inherited OnValueValidate := ValueValidate;
  ButtonWidth := 18;
  FBrowseDialogText := 'Select Directory';
  FAllowNewFolder := False;
end;


function AdvDirectoryEditCallBack (Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
var
  Temp: String;
  pt: TPoint;
  wa, rect : TRect;

begin
  if uMsg = BFFM_INITIALIZED then
  begin
    wa := Screen.WorkAreaRect;

    GetWindowRect(Wnd, Rect);

    with TAdvDirectoryEdit(lpData) do
    begin
      {$WARNINGS OFF}
      // avoid platform specific warning
      if (Text = '') then
        Temp := GetCurrentDir
      else
        Temp := ExcludeTrailingBackslash (Text);
      {$WARNINGS ON}

      if (_initdir <> '') then
        temp := _initdir;

      SendMessage(Wnd, BFFM_SETSELECTION, 1, LInteger(PChar(Temp)));

      with TAdvDirectoryEdit(lpData) do
      begin
        pt := Point(0,Height);
        pt := ClientToScreen(pt);

        if pt.X + (rect.Right - rect.Left) > wa.Right then
          pt.X := pt.X - (rect.Right - rect.Left);

        if pt.Y + (rect.Bottom - rect.Top) < wa.Bottom then
        begin
          SetWindowPos(Wnd,HWND_NOTOPMOST,pt.X,pt.Y,1024,1024,SWP_NOSIZE or SWP_NOZORDER)
        end
        else
        begin
          SetWindowPos(Wnd,HWND_NOTOPMOST,pt.X,wa.Bottom  - (rect.Bottom - rect.Top) - 64,0,0,SWP_NOSIZE or SWP_NOZORDER)
        end;
      end;
    end;
  end;
  Result := 0;
end;

procedure TAdvDirectoryEdit.BtnClick (Sender: TObject);
var
  bi: TBrowseInfo;
  iIdList: PItemIDList;
  ResStr: array[0..MAX_PATH] of char;
  MAlloc: IMalloc;
  InitDir: string;
  res: boolean;
  fldr: string;
  //pidlPrograms: PItemIDList;
  // BIF_NONEWFOLDERBUTTON
begin
  //if (not SUCCEEDED(SHGetSpecialFolderLocation(GetActiveWindow, CSIDL_DRIVES,
  //  pidlPrograms))) then
  //  Exit;

  FillChar(bi, sizeof(bi), #0);

  with bi do
  begin
    if Text <> '' then
      InitDir := Text
    else
      InitDir := GetCurrentDir;

    if Assigned(FOnShowDirectory) then
      FOnShowDirectory(Self, InitDir);

    StrPCopy(ResStr,InitDir);

    hwndOwner := Parent.Handle;
    pszDisplayName := ResStr;

    _initdir := resstr;

    lpszTitle := PChar(FBrowseDialogText);

    ulFlags := BIF_RETURNONLYFSDIRS;
    if FAllowNewFolder then
    begin
      ulFlags := ulFlags or BIF_NEWDIALOGSTYLE;

     if FAllowShares then
        ulFlags := ulFlags OR BIF_SHAREABLE;
    end;

    // when BIF_NEWDIALOGSTYLE flag is set, GetWindowRect in
    // BFFM_INITIALIZED returns incorrect height
    // This seems to be a bug in Microsoft's SHBrowseForFolder


    if not FAllowNewFolder then
      ulFlags := ulFlags or BIF_NONEWFOLDERBUTTON;

    ulFlags := ulFlags or BIF_USENEWUI;

    lpfn := AdvDirectoryEditCallBack;
    lParam := LInteger(Self);
  end;

  BlockDefaultHandling := true;

  iIdList := Nil;
  try
    iIdList := SHBrowseForFolder(bi);
  except
  end;

  if iIdList <> nil then
  begin
    try
      FillChar(ResStr,sizeof(ResStr),#0);
      res := SHGetPathFromIDList(iIdList, ResStr);

      fldr := StrPas(ResStr);

      if Assigned(OnDialogClose) then
        OnDialogClose(self, fldr, res);

      if res then
      begin
        if Text <> fldr then
        begin
          Text := fldr;
          Modified := True;
        end;
      end;
    finally
      SHGetMalloc(MAlloc);
      Malloc.Free(iIdList);
    end;
  end
  else
  begin
    fldr := Text;
    if Assigned(OnDialogClose) then
      OnDialogClose(self, fldr, false);
  end;
end;

procedure TAdvDirectoryEdit.ValueValidate(Sender: TObject; Value: String; Var IsValid: Boolean);
begin
  IsValid := true;
  if ShowError and (Value <> '') then
    IsValid := SysUtils.DirectoryExists(Value);
end;

function TAdvDirectoryEdit.IsValidDirectory: Boolean;
begin
  Result := SysUtils.DirectoryExists(Text);
  IsError := not Result;
end;

procedure TAdvDirectoryEdit.ClickButton;
begin
  BtnClick(self);
end;

procedure TAdvDirectoryEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_F4 then
    BtnClick(Self);
end;

procedure TAdvDirectoryEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  txt: string;
  sr: TSearchRec;
  isdir: boolean;
begin
  inherited;

  FAutoDirectoryLookup := true;

  if (Key = VK_F4) then
    BtnClick(Self);

  SetFocus;

  if FocusColor <> clNone then
    Color := FocusColor;

  txt := Text;

  Lookup.CaseSensitive := false;

  if Lookup.Enabled and FAutoDirectoryLookup and (txt <> FOldText) then
  begin
    FOldText := txt;

    isdir := (pos(':', txt) > 0) or (pos('/', txt) >  0) or (pos('\',txt) > 0);

    if FindFirst(txt +'*',faDirectory, sr) = 0 then
    begin
      lookup.DisplayList.Clear;

      repeat

        if (sr.attr and faDirectory = faDirectory) then
        begin
          if not isdir then
            lookup.DisplayList.Add(sr.Name)
          else
            lookup.DisplayList.Add(ExtractFilePath(txt) + sr.Name);
        end;

      until FindNext(sr) <> 0;

      FindClose(sr);

      if (lookup.DisplayList.Count = 1) and (pos(Uppercase(txt), Uppercase(lookup.DisplayList.Strings[0])) > 0) then
        Exit;

      if (lookup.DisplayList.Count > 0) then
        UpdateLookup;
    end;
  end;

end;

procedure TAdvDirectoryEdit.SetAutoDirectoryLookup(const Value: boolean);
begin
  FAutoDirectoryLookup := Value;
  if Value then
  begin
    Lookup.Enabled := true;
    Lookup.NumChars := 1;
    Lookup.CaseSensitive := false;
  end;
end;

end.
