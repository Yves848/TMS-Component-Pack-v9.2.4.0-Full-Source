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
unit AdvMemoActionsRes;

{$I TMSDEFS.INC}

interface

uses
  SysUtils, Classes, ActnList, AdvMemo,
  StdActns, ImgList, Controls, AdvMemoActions
  {$IFDEF DELPHIXE3_LVL}
  , System.Actions
  {$ENDIF}
  ;

type
  TAdvMemoActionsResource = class(TDataModule)
    ActionList1: TActionList;
    FileOpen1: TAdvMemoFileOpenAction;
    ImageList1: TImageList;
    EditCut1: TAdvMemoCut;
    EditCopy1: TAdvMemoCopy;
    EditPaste1: TAdvMemoPaste;
    EditDelete1: TAdvMemoDelete;
    EditUndo1: TAdvMemoUndo;
    EditRedo1: TAdvMemoRedo;
    EditSelectAll1: TAdvMemoSelectAll;
    FileSave1: TAdvMemoFileSaveAction;
    FileSaveAs1: TAdvMemoFileSaveAsAction;
    FileNew1: TAdvMemoFileNewAction;
    EditFind1: TAdvMemoFindAction;
    EditReplace1: TAdvMemoReplaceAction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AdvMemoActionsResource: TAdvMemoActionsResource;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
