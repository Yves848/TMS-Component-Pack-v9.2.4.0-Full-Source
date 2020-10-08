{*************************************************************************}
{ THotSpotImage component                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2002 - 2012                                       }
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

unit HotSpotEditorDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, HotSpotEditor, HotSpotImage, Controls,
  DesignIntf  , DesignEditors;


type
  THotSpotImageEditor = class(TDefaultEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THotSpotEditor = class(TPropertyEditor)
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;

    procedure TransferToHotSpotImage;
    procedure TransferFromHotSpotImage;
  end;


implementation

{$WARNINGS OFF}
uses
  SysUtils,Dialogs, ToolIntf, ShlObj, ActiveX, ShellAPI, Windows;
{$WARNINGS ON}

{ get My Documents folder }

procedure FreePidl( pidl: PItemIDList );
var
  allocator: IMalloc;
begin
  if Succeeded(SHGetMalloc(allocator)) then
    Allocator.Free(pidl);
end;

function GetMyDocuments: string;
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
      Result := StrPas(Path);
    FreePidl(pidl);
  end;
end;

{ THotSpotEditor }

//------------------------------------------------------------------------------
procedure THotSpotEditor.Edit;
var
  frmEditor: TfrmHSIEditor;
  s:string;
begin
  TransferFromHotSpotImage;
  frmEditor := TfrmHSIEditor.Create(nil);

  s := GetMyDocuments;

  //s := PrivateDirectory;
  {$WARNINGS OFF}
  s := IncludeTrailingBackslash(s);
  {$WARNINGS ON}

  frmEditor.LoadSettings(s + 'hotspotedit.ini');
  try
    if frmEditor.ShowModal = mrOK then
    begin
      TransferToHotSpotImage;
      Modified;
    end;
    frmEditor.SaveSettings(s + 'hotspotedit.ini');
  finally
    frmEditor.Free;
  end;
end;

//------------------------------------------------------------------------------
function THotSpotEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

//------------------------------------------------------------------------------
function THotSpotEditor.GetValue: String;
begin
  Result := '(THotSpots)';
end;

{ THotSpotImageEditor }

//------------------------------------------------------------------------------

procedure THotSpotImageEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
begin
  if PropertyEditor.GetName = 'HotSpots' then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end
  else inherited;
end;

//------------------------------------------------------------------------------
procedure THotSpotImageEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

//------------------------------------------------------------------------------
function THotSpotImageEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Edit hotspots...';
end;

//------------------------------------------------------------------------------
function THotSpotImageEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//------------------------------------------------------------------------------
procedure THotSpotEditor.TransferFromHotSpotImage;
var
  Comp: THotSpotImage;
begin
  if propCount = 0 then
    Exit;
  Comp := THotSpotImage(GetComponent(0));
  if Comp = nil then
    Exit;
  if FHotSpots_org = nil then
    Exit;
  if FPicture=nil then
    Exit;
  if FHotSpots=nil then
    Exit;

  ImageList := Comp.Images;  
  FHotSpots_org.Assign(Comp.HotSpots);
  FPicture.Assign(Comp.Picture);
  if Comp.Stretch then
    FHotSpots_org.ReScale(FPicture.Width,FPicture.Height);
  FHotSpots.Assign(FHotSpots_org);
  HotSpotsChanged:=false;
end;

//------------------------------------------------------------------------------
procedure THotSpotEditor.TransferToHotSpotImage;
var
  Comp: THotSpotImage;
begin
  Comp := THotSpotImage(GetComponent(0));
  Comp.HotSpots.Assign(FHotSpots);
  if Comp.Stretch then
    Comp.HotSpots.ReScale(Comp.Width,Comp.Height)
  else
    Comp.HotSpots.ReScale(Comp.Picture.Width,Comp.Picture.Height);
  Comp.Picture := FPicture;
  Comp.RePaint;
end;

end.
