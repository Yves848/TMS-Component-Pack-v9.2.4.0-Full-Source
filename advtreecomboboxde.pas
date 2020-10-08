unit AdvTreeComboBoxDE;

{$i tmsdefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,ComCtrls,buttons,imglist,menus,extctrls,
  DesignIntf, DesignEditors;

type
  TAdvTreeComboBoxEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

implementation

procedure TAdvTreeComboBoxEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
begin
 if (PropertyEditor.GetName = 'Items') then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


procedure TAdvTreeComboBoxEditor.ExecuteVerb(Index: integer);
begin
  case index of
  0:begin
     edit;
    end;
  end;
  Designer.Modified;
end;

function TAdvTreeComboBoxEditor.GetVerb(index: integer): string;
begin
  case index of
  0:Result := '&Items';
  end;
end;

function TAdvTreeComboBoxEditor.GetVerbCount: integer;
begin
  Result := 1;
end;


end.
