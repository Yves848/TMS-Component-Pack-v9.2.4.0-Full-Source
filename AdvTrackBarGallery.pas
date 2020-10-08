{********************************************************************}
{ TAdvTrackBar component                                             }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright � 2007 - 2012                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit advtrackbargallery;

interface

{$R ADVTRACKBARGALLERY.RES}

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvTrackBar, AdvTrackBarPersist;

type
  TAdvTrackBarGalleryForm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ListBox1: TListBox;
    AdvTrackBar1: TAdvTrackBar;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
    FSelection: string;
    procedure ResToFile(ResName,FileName: string);
  public
    { Public declarations }
    procedure LoadList;
    property Selection: string read FSelection write FSelection;
  end;

var
  AdvTrackBarGalleryForm: TAdvTrackBarGalleryForm;

implementation

uses
  ShlObj, ActiveX, ShellAPI;

{$R *.dfm}

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function ForceDirectories(Dir: string): Boolean;
begin
  Result := True;
  if Length(Dir) = 0 then
    Exit;
//    raise Exception.Create('Cannot create directory');

  if Dir[length(Dir)] = '\' then
    Delete(Dir,length(Dir),1);

  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;

function AddBackslash(const s: string): string;
begin
  if (Length(s) >= 1) and (s[Length(s)]<>'\') then
    Result := s + '\'
  else
    Result := s;
end;

function WinTempDir: string;
var
  buf:string;
  i: integer;
begin
  SetLength(buf, MAX_PATH);
  i := GetTempPath(Length(buf), PChar(buf));
  SetLength(buf, i);
  Result := AddBackslash(buf);
end;

{ get Application data folder }

procedure FreePidl( pidl: PItemIDList );
var
  allocator: IMalloc;
begin
  if Succeeded(SHGetMalloc(allocator)) then
    allocator.Free(pidl);
end;

function GetAppData: string;
var
  pidl: PItemIDList;
  Path: array [0..MAX_PATH-1] of char;
begin
  Result := '';

  if Succeeded(
       SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, pidl)
     ) then
  begin
    if SHGetPathFromIDList(pidl, Path) then
      Result := AddBackSlash(StrPas(Path));
    FreePidl(pidl);
    ForceDirectories(Result + 'tmssoftware');
  end;
end;

procedure TAdvTrackBarGalleryForm.ListBox1Click(Sender: TObject);
var
  pp: TPropertyPersister;
begin
  if listbox1.ItemIndex < 0 then
    Exit;
  FSelection := GetAppData + 'tmssoftware\'+ listbox1.items[listbox1.itemIndex]+'.TBPROP';

  AdvTrackBar1.Free;

  AdvTrackBar1 := TAdvTrackBar.Create(self);
  AdvTrackBar1.Parent := GroupBox1;
  AdvTrackBar1.Left := 10;
  AdvTrackBar1.Top := 20;
  //AdvTrackBar1.Width :=
  //AdvTrackBar1.Height :=

  pp := TPropertyPersister.Create(self);
  pp.IgnoreSubProperties.Add('RotateText');
  pp.IgnoreSubProperties.Add('TrackLabel');
  pp.IgnoreSubProperties.Add('TTrackBarThumb.ThumbPointer');
  pp.IgnoreProperties.Add('ThumbPointer');
  pp.IgnoreSubProperties.Add('TFont.Quality');
  pp.IgnoreProperties.Add('Quality');
  pp.ReStorePropertiesToFile(AdvTrackBar1, FSelection);
  pp.Free;
end;

{$WARNINGS OFF}
procedure TAdvTrackBarGalleryForm.LoadList;
var
  SR: TSearchRec;
  FileAttrs: Integer;
  len: integer;
  fnddir: string;
begin
  Listbox1.Items.Clear;

  // get gallery files
  FileAttrs := faArchive;

  fnddir := GetAppData + 'tmssoftware\*.TBPROP';

  if FindFirst(fnddir,FileAttrs,SR) = 0 then
  begin
    repeat
      if (sr.Attr and FileAttrs) = sr.Attr then
      begin
        len := Length(ExtractFileExt(sr.Name));
        listbox1.Items.Add(copy(sr.Name,1,length(sr.Name)-len));
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;
{$WARNINGS ON}



procedure TAdvTrackBarGalleryForm.Button1Click(Sender: TObject);
var
  pp: TPropertyPersister;
  fname: string;
begin
  if Edit1.Text = '' then
  begin
    ShowMessage('Please specify a name for saving to gallery');
    Exit;
  end;

  if ListBox1.Items.IndexOf(edit1.Text) <> -1 then
  begin
    if MessageDlg('Name already exists. Are you sure to overwrite ?',mtWarning,[mbYes,mbNo],0) = mrNo then
      Exit;
  end;

  fname := GetAppData + 'tmssoftware\'+ Edit1.Text + '.TBPROP';

  pp := TPropertyPersister.Create(self);
  pp.IgnoreSubProperties.Add('RotateText');
  pp.IgnoreSubProperties.Add('TrackLabel');
  pp.IgnoreSubProperties.Add('TTrackBarThumb.ThumbPointer');
  pp.IgnoreProperties.Add('ThumbPointer');
  pp.IgnoreSubProperties.Add('TFont.Quality');
  pp.IgnoreProperties.Add('Quality');
  pp.IgnoreSubProperties.Add('TFont.Quality');
  pp.IgnoreProperties.Add('Quality');

  pp.StorePropertiesToFile(AdvTrackBar1,fname);
  pp.Free;

  LoadList;
end;

procedure TAdvTrackBarGalleryForm.ResToFile(ResName,FileName: string);
var
  reshandle: THandle;
  hglobal: THandle;
  ressize: dword;
  ptr: pointer;
  rtext: ansistring;
  tf: TextFile;
  i: Integer;
  resourcename: ansistring;

begin

  resourcename := ansistring(resname);

  reshandle := FindResourceA(hinstance, PAnsiChar(resourcename), PAnsiChar(RT_RCDATA));
  hglobal := LoadResource(hinstance, reshandle);
  Ressize := SizeOfResource(hinstance, reshandle);
  ptr := LockResource(hglobal);

  rtext := '';
  for i := 1 to ressize do
  begin
    rtext := rtext + ansichar(ptr^);
    ptr := pointer(integer(ptr) + 1);
  end;

  {$IFDEF DELPHI6_LVL}
  AssignFile(tf, GetAppData + 'tmssoftware\'+FileName+'.TBPROP');
  {$ELSE}
  AssignFile(tf,'.\'+FileName+'.TBPROP');
  {$ENDIF}
  {$i-}
  rewrite(tf);
  {$i+}
  if ioresult = 0 then
  begin
    write(tf,rtext);
    CloseFile(tf);
  end;
end;


procedure TAdvTrackBarGalleryForm.FormCreate(Sender: TObject);
var
  pp: TPropertyPersister;
begin
  pp := TPropertyPersister.Create(self);
//  pp.IgnoreSubProperties.Add('RotateText');
//  pp.IgnoreProperties.Add('TrackLabel');

  pp.ReStorePropertiesToFile(AdvTrackBar1,WinTempDir + 'temp.prop');
  pp.Free;
  LoadList;

  if ListBox1.Items.Count = 0 then //
  begin
    // extract resources
    ResToFile('TB1','Airco');
    ResToFile('TB2','Apple Volume');
    ResToFile('TB3','Apple');
    ResToFile('TB4','Aqua');
    ResToFile('TB5','Circular');
    ResToFile('TB6','Cylinder');
    ResToFile('TB7','iPhone');
    ResToFile('TB8','iTunes');
    ResToFile('TB9','Mixer');
    ResToFile('TB10','Office 2007 Luna');
    ResToFile('TB11','Rating');
    ResToFile('TB12','Volume 1');
    ResToFile('TB13','WinAmp');
    ResToFile('TB14','Windows Media Player 10 Seek');
    ResToFile('TB15','Windows Media Player 10');
    ResToFile('TB16','Windows Media Player 11');
    LoadList;
  end;
end;

end.
