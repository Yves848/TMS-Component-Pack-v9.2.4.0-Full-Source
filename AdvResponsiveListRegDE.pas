{*************************************************************************}
{ TMS TAdvResponsiveList                                                  }
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

unit AdvResponsiveListRegDE;

interface

uses
  Classes, AdvResponsiveList, AdvResponsiveListDE, AdvResponsiveListDesigner,
  DesignIntf, DesignEditors, Forms, Controls;

type
  TResponsiveConditionsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TAdvResponsiveListEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;

  end;

  TAdvResponsiveListSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;


procedure Register;

implementation

uses
  SysUtils, ShellAPI, Windows, Dialogs;

{ TResponsiveConditionsProperty }

procedure TResponsiveConditionsProperty.Edit;
var
  List: TAdvResponsiveList;
  Dsgnr: TAdvResponsiveListDesignerForm;
begin
  List := TAdvResponsiveList(GetComponent(0));

  Dsgnr := TAdvResponsiveListDesignerForm.Create(Application);
  Dsgnr.List.Assign(List);

  if Dsgnr.ShowModal = mrOK then
  begin
    List.BeginUpdate;
    List.Assign(Dsgnr.List);
    List.EndUpdate;
    Modified;
  end;
end;

function TResponsiveConditionsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TAdvResponsiveListEditor }

procedure TAdvResponsiveListEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'CONDITIONS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


procedure TAdvResponsiveListEditor.ExecuteVerb(Index: integer);
var
  compiler:string;

begin
  {$I COMPILERTEXT.INC}

  case Index of
  0:
    begin
      MessageDlg(Component.ClassName+' version '+(Component as TAdvResponsiveList).Version +' for '+compiler+#13#10'© 2016-2017 by TMS software',
               mtinformation,[mbok],0);
    end;
  1:
    begin
      ShellExecute(0, 'open', 'http://www.tmssoftware.biz/Download/Manuals/TMS%20TAdvResponsiveList.pdf', nil, nil, SW_SHOW);
    end;
  2:
    begin
      ShellExecute(0, 'open', 'http://www.tmssoftware.com/site/advresponsivelist.asp?s=faq', nil, nil, SW_SHOW);
    end;
  end;
end;

function TAdvResponsiveListEditor.GetVerb(index: integer): string;
begin
  case Index of
  0: Result := '&Version';
  1: Result := '&Manual';
  2: Result := '&Tips && FAQ';
  end;
end;

function TAdvResponsiveListEditor.GetVerbCount: integer;
begin
  Result := 3;
end;

{ TAdvResponsiveListSelectionEditor }

procedure TAdvResponsiveListSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('AdvGraphics');
end;



procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TResponsiveConditions), TAdvResponsiveList,
    'Conditions', TResponsiveConditionsProperty);

  RegisterComponentEditor(TAdvResponsiveList, TAdvResponsiveListEditor);

  RegisterSelectionEditor(TAdvResponsiveList, TAdvResponsiveListSelectionEditor);
end;

end.
