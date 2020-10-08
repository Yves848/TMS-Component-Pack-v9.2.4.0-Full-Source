{************************************************************************}
{ TADVSHELL : Windows shell functions                                    }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by                                                             }
{   TMS Software                                                         }
{   copyright © 2017                                                     }
{   Email : info@tmssoftware.com                                         }
{   Web : http://www.tmssoftware.com                                     }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit advshell;

interface

uses
  Types;

procedure ShowSysContextMenu(AFileName: string; APos: TPoint);

implementation

uses
  ShlObj, ComObj, ActiveX, Controls, SysUtils, Windows, Messages, Classes;

type
  THelperWindow = class(TWinControl)
  public
    { Public declarations }
     procedure WndProc(var Msg: TMessage); override;
  end;

var
  ContextMenu2: IContextMenu2 = nil;
  HelperWndw: THelperWindow;

procedure ShowSysContextMenu(AFileName: string; APos: TPoint);
var
  DeskFolder, Folder: IShellFolder;
  Eaten, Attributes: ULONG;
  pIdl, FolderpIdl: PItemIDList;
  ContextMenu: IContextMenu;
  Menu: HMENU;
  HND: THandle;
  Pos: TPoint;
  Cmd: DWORD;
  fn,fp: string;
  CommandInfo: TCMInvokeCommandInfo;
begin
  fn := ExtractFileName(AFileName);
  fp := ExtractFilePath(AFileName);

  if fn = '' then
    fp := AFileName;

  HND := HelperWndw.Handle;

  // IShellFolder for Desktop folder (root)
  OleCheck(SHGetDesktopFolder(DeskFolder));

  // Item ID List for the folder that the file is in
  Attributes := 0;
  OleCheck(DeskFolder.ParseDisplayName(HND, nil,
                    PWideChar(WideString(fp)),
                    Eaten, FolderpIdl, Attributes));

  if fn <> '' then
  begin
    // IShellFolder for the folder the file is in
    OleCheck(DeskFolder.BindToObject(FolderpIdl, nil, IID_IShellFolder, Folder));
    CoTaskMemFree(FolderpIdl);

    // Item ID List for the file, relative to the folder it is in
    Attributes := 0;
    OleCheck(Folder.ParseDisplayName(HND, nil,
             PWideChar(WideString(fn)),
             Eaten, pIdl, Attributes));
  end;

  // IContextMenu for the relative Item ID List
  if fn <> '' then
  begin
    OleCheck(Folder.GetUIObjectOf(HND, 1, pIdl, IID_IContextMenu, nil, ContextMenu));
    CoTaskMemFree(pIdl);
  end
  else
  begin
    OleCheck(DeskFolder.GetUIObjectOf(HND, 1, FolderpIdl, IID_IContextMenu, nil, ContextMenu));
    CoTaskMemFree(FolderpIdl);
  end;


  Menu := CreatePopupMenu;
  try
    // Populate our menu with shortcut items
    OleCheck(ContextMenu.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_EXPLORE));

    // ContextMenu2 used in WndProc
    ContextMenu.QueryInterface(IID_IContextMenu2, ContextMenu2);
    try
      Pos := Point(APos.X,APos.Y);
      // launch the menu
      Bool(Cmd) := TrackPopupMenu(Menu,
                            TPM_LEFTBUTTON or TPM_RIGHTBUTTON or TPM_RETURNCMD,
                            Pos.X, Pos.Y, 0, HND, nil);
    finally
      // clear so that we don't intervene every owner drawn menu item message in WndProc
      ContextMenu2 := nil;
    end;

    // Invoke command
    if Bool(Cmd) then
    begin
      FillChar(CommandInfo, SizeOf(CommandInfo), 0);
      CommandInfo.cbSize := SizeOf(CommandInfo);
      CommandInfo.hwnd := HND;
      CommandInfo.lpVerb := LPCSTR(MakeIntResource(Cmd - 1));
      CommandInfo.nShow := SW_SHOWNORMAL;
      OleCheck(ContextMenu.InvokeCommand(CommandInfo));
    end;

  finally
    DestroyMenu(Menu);
  end;
end;


{ THelperWindow }

procedure THelperWindow.WndProc(var Msg: TMessage);
begin
  if ((Msg.Msg = WM_INITMENUPOPUP) or (Msg.Msg = WM_DRAWITEM)
              or (Msg.Msg = WM_MEASUREITEM)) and Assigned(ContextMenu2) then
    ContextMenu2.HandleMenuMsg(Msg.Msg, Msg.WParam, Msg.LParam)
  else
    inherited;
end;

initialization
  OleInitialize(nil);
  HelperWndw := THelperWindow.CreateParented(GetDesktopWindow);

finalization
  HelperWndw.Free;
  OleUninitialize;

end.
