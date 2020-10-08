{***************************************************************************}
{ TAdvMemo component                                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2017                                        }
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

unit AdvMemoActionsReg;

interface

{$I TMSDEFS.INC}

uses
  AdvMemo;

procedure Register;

implementation

uses
  AdvMemoActions, ActnList, AdvMemoActionsRes
  {$IFDEF DELPHIXE3_LVL}
  , System.Actions
  {$ENDIF}
  ;

procedure Register;
begin
  RegisterActions('AdvMemo', [TAdvMemoCut, TAdvMemoCopy, TAdvMemoPaste,
    TAdvMemoDelete, TAdvMemoUndo, TAdvMemoRedo, TAdvMemoSelectAll,
    TAdvMemoFileOpenAction, TAdvMemoFileSaveAction, TAdvMemoFileSaveAsAction,
    TAdvMemoFileNewAction, TAdvMemoFindAction, TAdvMemoReplaceAction],
    TAdvMemoActionsResource);
end;

end.
