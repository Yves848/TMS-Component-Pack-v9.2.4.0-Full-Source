{*************************************************************************}
{ TMS TAdvSearchList & TAdvSearchEdit                                     }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2016                                              }
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

unit AdvSearchListRegDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, GDIPicture, GDIPicDE, DesignIntf, DesignEditors,
  AdvSearchEdit, AdvSearchList
{$IFDEF DELPHIXE2_LVL}
  , System.UITypes
{$ENDIF}
  ;

type
  TAdvSearchListDefaultEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty; var Continue:Boolean); override;
  public
  end;



  TAdvSearchEditSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;



implementation

uses
  SysUtils;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TSearchEditButton, 'Picture', TGDIPPictureProperty);
  RegisterSelectionEditor(TAdvSearchEdit, TAdvSearchEditSelectionEditor);
  RegisterComponentEditor(TAdvSearchList,TAdvSearchListDefaultEditor);
  RegisterComponentEditor(TAdvSearchEdit,TAdvSearchListDefaultEditor);
end;

{ TAdvSearchEditSelectionEditor }

procedure TAdvSearchEditSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('AdvSearchList');
end;

{ TAdvSearchListDefaultEditor }

procedure TAdvSearchListDefaultEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'Columns') = 0) then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;



end.
