{***************************************************************************}
{ TFormSize component                                                       }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2019                                        }
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

unit FormSize;

{$I TMSDEFS.INC}

interface

uses
  SysUtils, Windows, Messages, Classes, Forms, ShellAPI, Registry, Types, DB;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 4; // Build nr.

  // version history
  // 1.1.0.1 : Fixed issue with magnet function on left/top side
  // 1.1.0.2 : Added exception handling in LoadFromSettings, SaveFormSettings
  // 1.2.0.0 : Added support to be used on Unicode forms (TTntForm)
  // 1.3.0.0 : Added support to persist in registry
  // 1.3.1.0 : Improved : position persistence
  // 1.3.1.1 : Fixed : Issue with form position persistence & magnet function
  // 1.3.1.2 : Fixed : Issue with Win64 and Windows 8
  // 1.3.1.3 : Fixed : Issue with maximized state & multimonitor config
  // 1.3.1.4 : Fixed : Issue with restoring form position
  // 1.3.1.5 : Improved : support for multimonitor use
  // 1.4.0.0 : New : Persist settings to database
  // 1.4.1.0 : New : Use form default location setting when no form size settings are persisted
  // 1.4.1.1 : Fixed : Issue with restoring size when no values are in the registry
  // 1.4.1.2 : Fixed : Issue with position handling different from poDesigned
  // 1.4.1.3 : Fixed : Issue with database persistence
  // 1.4.1.4 : Fixed : Issue with loading form due to breaking change in Delphi 10.3 Rio

type
  EFormSizeError = class(Exception);

  TFormSize = class;

  TPersistLocation = (plIniFile, plRegistry, plDatabase);

  TDataBinding = class(TPersistent)
  private
    FFormSize: TFormSize;
    FFieldUser: string;
    FFieldSizeWidth: string;
    FFieldPosX: string;
    FFieldPosY: string;
    FFieldMachine: string;
    FFieldFormName: string;
    FFieldSizeHeight: string;
    FFieldWindowState: string;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
    property FormSize: TFormSize read FFormSize;
  published
    property FieldPosX: string read FFieldPosX write FFieldPosX;
    property FieldPosY: string read FFieldPosY write FFieldPosY;
    property FieldSizeWidth: string read FFieldSizeWidth write FFieldSizeWidth;
    property FieldSizeHeight: string read FFieldSizeHeight write FFieldSizeHeight;
    property FieldFormName: string read FFieldFormName write FFieldFormName;
    property FieldUser: string read FFieldUser write FFieldUser;
    property FieldMachine: string read FFieldMachine write FFieldMachine;
    property FieldWindowState: string read FFieldWindowState write FFieldWindowState;
  end;

  TFormSizeDataLink = class(TDataLink)
  private
    FOnActiveChanged: TNotifyEvent;
  public
    procedure ActiveChanged; override;
  published
    property OnActiveChanged: TNotifyEvent read FOnActiveChanged write FOnActiveChanged;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TFormSize = class(TComponent)
  private
    { Private declarations }
    OldWndProc: TFarProc;
    NewWndProc: Pointer;
    FSaveMachine: boolean;
    FSaveUser: boolean;
    FSavePosition: boolean;
    FSaveSize: boolean;
    FSaveName: string;
    FSaveKey: string;
    FDragAlways: boolean;
    FMagnet: boolean;
    FMagnetDistance: integer;
    FLocation: TPersistLocation;
    FDataLink: TFormSizeDataLink;
    FDataBinding: TDataBinding;
    FUseHook: boolean;
    FLoaded: boolean;
    function CreateKey:string;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetDataBinding(const Value: TDataBinding);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure HookWndProc(var Msg: TMessage);
    procedure DoLoadPlacement;
    procedure DoSavePlacement;
    procedure Loaded; override;
    procedure DataSetActiveChanged(Sender: TObject);
    function PersistName: string;
    function PersistUser: string;
    function PersistMachine: string;
    procedure InstallHook;
    procedure RemoveHook;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SaveFormSettings;
    procedure LoadFormSettings;
    property UseHook: boolean read FUseHook write FUseHook;
  published
    { Published declarations }
    property DataBinding: TDataBinding read FDataBinding write SetDataBinding;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragAlways: boolean read FDragAlways write FDragAlways default False;
    property Location: TPersistLocation read FLocation write FLocation default plIniFile;
    property Magnet: boolean read FMagnet write FMagnet default False;
    property MagnetDistance: integer read FMagnetDistance write FMagnetDistance;

    property SavePosition: boolean read FSavePosition write FSavePosition default True;
    property SaveSize: boolean read FSaveSize write FSaveSize default True;
    property SaveUser: boolean read FSaveUser write FSaveUser;
    property SaveMachine: boolean read FSaveMachine write FSaveMachine;
    property SaveName: string read FSaveName write FSaveName;
    property SaveKey: string read FSaveKey write FSaveKey;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  INIFiles, Dialogs, Variants;

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}

{$WARNINGS OFF}
constructor TFormSize.Create(AOwner:TComponent);
var
  I, Instances: Integer;
begin
  inherited Create(AOwner);
  if not (Owner is TForm) then
    raise EFormSizeError.Create('Control parent must be a form!');

  Instances := 0;
  for I := 0 to Owner.ComponentCount - 1 do
    if (Owner.Components[I] is TFormSize) then
      Inc(Instances);
  if (Instances > 1) then
    raise EFormSizeError.Create('The form already contains a TFormSize component');

  FLoaded := false;
  FSavePosition := True;
  FSaveSize := True;
  FSaveName := '.\FORM.INI';
  FSaveKey := Owner.Name;
  FMagnetDistance := 32;
  FLocation := plInifile;
  FUseHook := true;

  FDataLink := TFormSizeDataLink.Create;
  FDataLink.OnActiveChanged := DataSetActiveChanged;
  FDataBinding := TDataBinding.Create(Self);

  { Hook parent }
  InstallHook;
end;  { TFormSize.Create }


procedure TFormSize.Loaded;
begin
  inherited;
  {$IFNDEF DELPHIXE12_LVL}
  if not (csDesigning in ComponentState) and (FSavePosition or FSaveSize) then
    DoLoadPlacement;
  {$ENDIF}
end;  { TFormSize.Loaded }

procedure TFormSize.DataSetActiveChanged(Sender: TObject);
begin
  DoLoadPlacement;
end;

destructor TFormSize.Destroy;
begin
  { Unhook parent }
  RemoveHook;

  FDataBinding.Free;
  FDataLink.Free;

  { Clean up }
  inherited Destroy;
end;  { TFormSize.Destroy }

{$WARNINGS ON}

procedure TFormSize.DoLoadPlacement;
var
  ARect: TRect;
  Maximize: Boolean;
  Settings: TIniFile;
  RegInifile: TRegInifile;
  Key: string;
  FOldMagnet: boolean;
  mon: TMonitor;
  dim: Integer;
  v: variant;
  fld: TField;
  hasval, rehook: boolean;
  c: integer;

begin
  if (FSaveName = '') or (FSaveKey = '') then
    Exit;

  FOldMagnet := FMagnet;
  FMagnet := false;
  Maximize := false;
  hasval := false;

  ARect := (Owner as TForm).BoundsRect;

  case Location of
  plIniFile:
    begin
      Settings := TIniFile.Create(FSaveName);
      try
        Key := CreateKey;

        with Settings, ARect do
        begin

          if FSavePosition then
          begin
            hasval := ReadInteger(Key, 'Left', $fff) <> $fff;
            Left := ReadInteger(Key, 'Left', Left);
            Top := ReadInteger(Key, 'Top', Top);
          end;

          if FSaveSize then
          begin
            hasval := ReadInteger(Key, 'Right', $fff) <> $fff;
            Right := ReadInteger(Key, 'Right', Right);
            Bottom := ReadInteger(Key, 'Bottom', Bottom);
          end
          else
          begin
            Right := Left + (Owner as TForm).Width;
            Bottom := Top + (Owner as TForm).Height;
          end;

          Maximize := ReadBool(Key, 'Maximized',
            (Owner as TForm).WindowState = wsMaximized);
        end;
      finally
        Settings.Free;
      end;
    end;
  plRegistry:
    begin
      RegIniFile := TRegIniFile.Create(FSaveName);
      try
        ARect := (Owner as TForm).BoundsRect;
        Key := CreateKey;

        with RegInifile, ARect do
        begin
          if FSavePosition then
          begin
            hasval := ReadInteger(Key, 'Left', $fff) <> $fff;

            Left := ReadInteger(Key, 'Left', Left);
            Top := ReadInteger(Key, 'Top', Top);
          end;

          if FSaveSize then
          begin
            hasval := ReadInteger(Key, 'Right', $fff) <> $fff;

            Right := ReadInteger(Key, 'Right', Right);
            Bottom := ReadInteger(Key, 'Bottom', Bottom);
          end
          else
          begin
            Right := Left + (Owner as TForm).Width;
            Bottom := Top + (Owner as TForm).Height;
          end;

          Maximize := ReadBool(Key, 'Maximized',
            (Owner as TForm).WindowState = wsMaximized);
        end;
      finally
        RegInifile.Free;
      end;
    end;
  plDatabase:
    begin
      if Assigned(FDataLink.DataSource) and Assigned(FDataLink.DataSource.DataSet) then
      begin
        if FDataLink.DataSource.DataSet.Active and (DataBinding.FieldFormName <> '') then
        begin
          Key := DataBinding.FieldFormName;

          dim := 0;

          if (DataBinding.FieldUser <> '') then
          begin
            Key := Key + ';' + DataBinding.FieldUser;
            inc(dim);
          end;

          if (DataBinding.FieldMachine <> '') then
          begin
            Key := Key + ';' + DataBinding.FieldMachine;
            inc(dim);
          end;

          v := VarArrayCreate([0,dim], varVariant);

          dim := 0;

          v[dim] := PersistName;
          inc(dim);

          if (DataBinding.FieldUser <> '') then
          begin
            v[dim] := PersistUser;
            inc(dim);
          end;

          if (DataBinding.FieldMachine <> '') then
          begin
            v[dim] := PersistMachine;
          end;

          ARect := (Owner as TForm).BoundsRect;

          if FDataLink.DataSource.DataSet.Locate(Key, v, []) then
          begin
            if DataBinding.FieldPosX <> '' then
            begin
              fld := FDataLink.DataSource.DataSet.FindField(DataBinding.FieldPosX);
              if Assigned(fld) then
              begin
                ARect.Left := fld.AsInteger;
                hasval := true;
              end;
            end;
            if DataBinding.FieldPosY <> '' then
            begin
              fld := FDataLink.DataSource.DataSet.FindField(DataBinding.FieldPosY);
              if Assigned(fld) then
                ARect.Top := fld.AsInteger;
            end;
            if DataBinding.FieldSizeWidth <> '' then
            begin
              fld := FDataLink.DataSource.DataSet.FindField(DataBinding.FieldSizeWidth);
              if Assigned(fld) then
              begin
                hasval := true;
                ARect.Right := ARect.Left + fld.AsInteger;
              end;
            end;
            if DataBinding.FieldSizeHeight <> '' then
            begin
              fld := FDataLink.DataSource.DataSet.FindField(DataBinding.FieldSizeHeight);
              if Assigned(fld) then
                ARect.Bottom := ARect.Top + fld.AsInteger;
            end;
          end;
        end;
      end;
    end;
  end;

  { Make sure the window is entirely visible on the screen }
  with ARect do
  begin
    if ((Right > Screen.DesktopWidth) and not (Maximize)) then
    begin
      Dec(Left, (Right - Screen.DesktopWidth));
      Right := Screen.DesktopWidth;
    end;

    if (Left < Screen.DesktopLeft) then
    begin
      Inc(Right, (Screen.DesktopLeft - Left));
      Left := Screen.DesktopLeft;
    end;

    if ((Bottom > Screen.DesktopHeight) and not(Maximize)) then
    begin
      Dec(Top, (Bottom - Screen.DesktopHeight));
      Bottom := Screen.DesktopHeight;
    end;

    if (Top < Screen.DesktopTop) then
    begin
      Inc(Bottom, (Screen.DesktopTop - Top));
      Top := Screen.DesktopTop;
    end;
  end;

  if Maximize then
  begin
    mon := Screen.MonitorFromPoint(Point(ARect.Left + ((ARect.Right - ARect.Left) div 2),
      ARect.Top + ((ARect.Bottom - ARect.Top) div 2)));

    ARect := mon.BoundsRect;
  end;

//  Placement.rcNormalPosition := ARect;
//  Placement.length := Sizeof(Placement);
//  SetWindowPlacement((Owner as TForm).Handle, @Placement);
//  MoveWindow((Owner as TForm).Handle, ARect.Left, ARect.Top, ARect.Width, ARect.Height, true);

  if hasval then
  begin
    rehook := ((Owner as TForm).Position <> poDesigned);

    // do not change Position to poDesigned for ribbon forms to prevent handle recreation
    for c := 0 to ((Owner as TForm).ControlCount - 1) do
    begin
      if (Owner as TForm).Controls[c].ClassName = 'TAdvToolBarPager' then
        rehook := false;
    end;

    if rehook then
    begin
      RemoveHook;
      (Owner as TForm).Position := poDesigned;
    end;

    (Owner as TForm).BoundsRect := ARect;
    MoveWindow((Owner as TForm).Handle, ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, true);

    if rehook then
      InstallHook;
  end;

  if Maximize then
    (Owner as TForm).WindowState := wsMaximized;

  FMagnet := FOldMagnet;
end;  { TFormSize.LoadPlacement }

procedure TFormSize.DoSavePlacement;
var
  Placement: TWindowPlacement;
  Settings: TIniFile;
  RegInifile: TReginifile;
  Key: string;
  v: Variant;
  dim: integer;
  fld: TField;
begin
  if (FSaveName = '') or (FSaveKey = '') then
    Exit;

  Placement.length := SizeOf(Placement);
  GetWindowPlacement((Owner as TForm).Handle, @Placement);

  case Location of
  plIniFile:
    begin
      Settings := TIniFile.Create(FSaveName);
      try
        Key := CreateKey;

        with Settings, Placement, rcNormalPosition do
        begin
          if FSavePosition then
          begin
            WriteInteger(Key, 'Left', (Owner as TForm).Left);
            WriteInteger(Key, 'Top', (Owner as TForm).Top);
          end;
          if FSaveSize then
          begin
            WriteInteger(Key, 'Right', (Owner as TForm).Left + (Owner as TForm).Width);
            WriteInteger(Key, 'Bottom', (Owner as TForm).Top + (Owner as TForm).Height);
            WriteBool(Key, 'Maximized', showCmd = SW_SHOWMAXIMIZED);
          end;
        end;
      finally
        Settings.Free;
      end;

    end;
  plRegistry:
    begin
      RegInifile := TRegIniFile.Create(FSaveName);
      try
        Key := CreateKey;

        with RegInifile, Placement, rcNormalPosition do
        begin
          if FSavePosition then
          begin
            WriteInteger(Key, 'Left', (Owner as TForm).Left);
            WriteInteger(Key, 'Top', (Owner as TForm).Top);
          end;
          if FSaveSize then
          begin
            WriteInteger(Key, 'Right', (Owner as TForm).Left + (Owner as TForm).Width);
            WriteInteger(Key, 'Bottom', (Owner as TForm).Top + (Owner as TForm).Height);
            WriteBool(Key, 'Maximized', showCmd = SW_SHOWMAXIMIZED);
          end;
        end;
      finally
        RegInifile.Free;
      end;

    end;
  plDataBase:
    begin
      if Assigned(FDataLink.DataSource) and Assigned(FDataLink.DataSource.DataSet) then
      begin
        if FDataLink.DataSource.DataSet.Active and (DataBinding.FieldFormName <> '') then
        begin
          Key := DataBinding.FieldFormName;

          dim := 0;

          if (DataBinding.FieldUser <> '') then
          begin
            Key := Key + ';' + DataBinding.FieldUser;
            inc(dim);
          end;

          if (DataBinding.FieldMachine <> '') then
          begin
            Key := Key + ';' + DataBinding.FieldMachine;
            inc(dim);
          end;

          v := VarArrayCreate([0,dim], varVariant);

          dim := 0;

          v[dim] := PersistName;
          inc(dim);

          if (DataBinding.FieldUser <> '') then
          begin
            v[dim] := PersistUser;
            inc(dim);
          end;

          if (DataBinding.FieldMachine <> '') then
          begin
            v[dim] := PersistMachine;
          end;

          if FDataLink.DataSource.DataSet.Locate(Key, v, []) then
            FDataLink.DataSource.DataSet.Edit
          else
            FDataLink.DataSource.DataSet.Insert;

          with FDataLink.DataSource.DataSet do
          begin
            fld := FindField(DataBinding.FieldFormName);
            if Assigned(fld) then
              fld.AsString := Self.Owner.Name;

            if DataBinding.FieldPosX <> '' then
            begin
              fld := FindField(DataBinding.FieldPosX);
              if Assigned(fld) then
                fld.AsInteger := (Self.Owner as TForm).Left;
            end;
            if DataBinding.FieldPosY <> '' then
            begin
              fld := FindField(DataBinding.FieldPosY);
              if Assigned(fld) then
                fld.AsInteger := (Self.Owner as TForm).Top;
            end;
            if DataBinding.FieldSizeWidth <> '' then
            begin
              fld := FindField(DataBinding.FieldSizeWidth);
              if Assigned(fld) then
                fld.AsInteger := (Self.Owner as TForm).Width;
            end;
            if DataBinding.FieldSizeHeight <> '' then
            begin
              fld := FindField(DataBinding.FieldSizeHeight);
              if Assigned(fld) then
                fld.AsInteger := (Self.Owner as TForm).Height;
            end;
            if DataBinding.FieldUser <> '' then
            begin
              fld := FindField(DataBinding.FieldUser);
              if Assigned(fld) then
                fld.AsString := PersistUser;
            end;
            if DataBinding.FieldMachine <> '' then
            begin
              fld := FindField(DataBinding.FieldMachine);
              if Assigned(fld) then
                fld.AsString := PersistMachine;
            end;

            Post;
          end;
        end;
      end;
    end;
  end;
end;  { TFormSize.DoSavePlacement }

procedure TFormSize.HookWndProc(var Msg: TMessage);
var
  xpos,ypos:word;
  pt: TPoint;
  wp: PWindowPos;
  R: TRect;
  AD : TAppBarData;
  lim_left,lim_top,lim_right,lim_bottom : integer;

begin
  with Msg do
  begin
    case Msg of
    WM_WINDOWPOSCHANGING:
      begin
        if FMagnet then
        begin
          fillchar(AD,sizeof(AD),0);
          AD.cbSize := sizeof(AD);
          SHAppBarMessage(ABM_GETTASKBARPOS,AD);

          lim_left := 0;
          lim_right := GetSystemMetrics(SM_CXSCREEN);
          lim_top := 0;
          lim_bottom := GetSystemMetrics(SM_CYSCREEN);

          case AD.uEdge of
          ABE_BOTTOM: lim_bottom := lim_bottom - (AD.rc.Bottom -AD.rc.Top);
          ABE_TOP: lim_top := lim_top + (AD.rc.Bottom -AD.rc.Top);
          ABE_LEFT: lim_left := lim_left + (AD.rc.Right -AD.rc.Left);
          ABE_RIGHT: lim_right := lim_right - (AD.rc.Right -AD.rc.Left);
          end;

          wp := PWindowPos(lparam);

          R := (Owner as TForm).BoundsRect;
          if (wp^.x<lim_left + FMagnetDistance) or (wp^.x<lim_left) then
            wp^.x := lim_left;
          if (wp^.y<lim_top + FMagnetDistance) or (wp^.y<lim_top) then
            wp^.y := lim_top;

          if (wp^.y + (R.Bottom-R.Top) > lim_bottom-FMagnetDistance) then
             wp^.y := lim_bottom - (R.Bottom-R.Top);

          if (wp^.x + (R.Right-R.Left) > lim_right-FMagnetDistance) then
             wp^.x := lim_right - (R.Right-R.Left);
        end;
      end;
    end;

    Result := CallWindowProc(OldWndProc, (Owner as TForm).Handle, Msg,
                                wParam, lParam);

    case Msg of
    WM_DESTROY:
      if not (csDesigning in ComponentState) and FSavePosition then
        DoSavePlacement;
    WM_SHOWWINDOW:
      begin
        {$IFDEF DELPHIXE12_LVL}
        if not (csDesigning in ComponentState) and (FSavePosition or FSaveSize)  then
        begin
          if not FLoaded then
          begin
            FLoaded := true;
            DoLoadPlacement;
          end;
        end;
        {$ENDIF}
      end;
    WM_NCHITTEST:
      begin
        if FDragAlways and not (csDesigning in ComponentState) then
        begin
          xpos := loword(lParam);
          ypos := hiword(lParam);
          pt := (Owner as TForm).ScreenToClient(point(xpos,ypos));
          if PtInRect((Owner as TForm).ClientRect,pt) then
            Result := htCaption;
        end;
      end;
    end;
  end;
end;

procedure TFormSize.InstallHook;
begin
  if not FUseHook then
    Exit;

  {$IFDEF DELPHI_UNICODE}
  OldWndProc := TFarProc(GetWindowLongPtr((Owner as TForm).Handle, GWL_WNDPROC));
  NewWndProc := MakeObjectInstance(HookWndProc);
  SetWindowLongPtr((Owner as TForm).Handle, GWL_WNDPROC, LInteger(NewWndProc));
  {$ENDIF}

  {$IFNDEF DELPHI_UNICODE}
  OldWndProc := TFarProc(GetWindowLong((Owner as TForm).Handle, GWL_WNDPROC));
  NewWndProc := MakeObjectInstance(HookWndProc);
  SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LInteger(NewWndProc));
  {$ENDIF}
end;

{ TFormSize.HookWndProc }

function TFormSize.CreateKey: string;
begin
  Result := SaveKey;
  if FSaveUser then
    Result := Result + '-' + PersistUser;
  if FSaveMachine then
    Result := Result + '-' + PersistMachine;
end;

procedure TFormSize.LoadFormSettings;
var
  osavepos,osavesize: Boolean;
begin
  osavesize := FSaveSize;
  osavepos := FSavePosition;
  FSaveSize := True;
  FSavePosition := True;
  try
    DoLoadPlacement;
  finally
    FSaveSize := osavesize;
    FSavePosition := osavepos;
  end;
end;

function TFormSize.PersistMachine: string;
var
  buf: array[0..255] of char;
  bufsize: dword;
begin
  bufsize := sizeof(buf);
  GetComputerName(buf,bufsize);
  Result := StrPas(buf);
end;

function TFormSize.PersistName: string;
begin
  Result := Owner.Name;
end;

function TFormSize.PersistUser: string;
var
  buf: array[0..255] of char;
  bufsize: dword;
begin
  bufsize := sizeof(buf);
  GetUserName(buf,bufsize);
  Result := StrPas(buf);
end;

procedure TFormSize.RemoveHook;
begin
  if not FUseHook then
    Exit;

  {$IFDEF DELPHI_UNICODE}
  if (Owner <> nil) and Assigned(OldWndProc) then
    SetWindowLongPtr((Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  if (Owner <> nil) and Assigned(OldWndProc) then
    SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc));
  {$ENDIF}

  if Assigned(NewWndProc) then
    FreeObjectInstance(NewWndProc);
end;

procedure TFormSize.SaveFormSettings;
var
  osavepos,osavesize: Boolean;
begin
  osavesize := FSaveSize;
  osavepos := FSavePosition;
  FSaveSize := True;
  FSavePosition := True;
  try
    DoSavePlacement;
  finally
    FSaveSize := osavesize;
    FSavePosition := osavepos;
  end;
end;

function TFormSize.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TFormSize.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TFormSize.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TFormSize.SetDataBinding(const Value: TDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TFormSize.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TFormSize.SetVersion(const Value: string);
begin

end;

{ TDataBinding }

procedure TDataBinding.Assign(Source: TPersistent);
begin
  if (Source is TDataBinding) then
  begin
    FFieldUser := (Source as TDataBinding).FieldUser;
    FFieldFormName := (Source as TDataBinding).FieldFormName;
    FFieldPosX := (Source as TDataBinding).FieldPosX;
    FFieldPosY := (Source as TDataBinding).FieldPosY;
    FFieldSizeWidth := (Source as TDataBinding).FieldSizeWidth;
    FFieldSizeHeight := (Source as TDataBinding).FieldSizeHeight;
    FFieldWindowState := (Source as TDataBinding).FieldWindowState;
  end;
end;

constructor TDataBinding.Create(AOwner: TComponent);
begin
  inherited Create;

  FFormSize := nil;

  if Assigned(AOwner) and (AOwner is TFormSize) then
    FFormSize := AOwner as TFormSize;
end;

{ TFormSizeDataLink }

procedure TFormSizeDataLink.ActiveChanged;
begin
  inherited;
  if Assigned(OnActiveChanged) then
    OnActiveChanged(Self);
end;


end.
