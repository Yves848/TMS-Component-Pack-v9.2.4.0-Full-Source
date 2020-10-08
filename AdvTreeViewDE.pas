{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2015                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvTreeViewDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvGeneralDE, AdvTreeView, AdvCustomTreeView, AdvTreeViewData
  {$IFDEF FMXLIB}
  ,VCL.Dialogs
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,Dialogs
  {$ENDIF}
  {$IFDEF WIN32}
  ,SysUtils, DesignIntf, DesignEditors
  {$ENDIF}
  ;

{$IFDEF WIN32}
type
  TAdvCustomTreeViewProtected = class(TAdvCustomTreeView);

  TAdvTreeViewDesigntimeEditor = class(TAdvDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
{$ENDIF}

implementation

{$IFDEF FMXLIB}
uses
  PictureContainer, Forms, UITypes
  {$IFDEF DELPHIXE10_LVL}
  ,FMX.DialogService.Sync
  {$ENDIF}
  {$IFDEF DELPHIXE7_LVL}
  ,AdvTreeViewEditor
  {$ENDIF}
  ;
{$ENDIF}

{$IFDEF WIN32}
procedure TAdvTreeViewDesigntimeEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'COLUMNS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;
{$ENDIF}

procedure TAdvTreeViewDesigntimeEditor.ExecuteVerb(Index: Integer);
var
  {$IFDEF FMXLIB}
  {$IFNDEF DELPHIXE10_LVL}
  v: string;
  {$ENDIF}
  {$ELSE}
  v: string;
  {$ENDIF}
  {$IFDEF DELPHIXE2_LVL}
  i: Integer;
  n: TAdvTreeViewNode;
  a: array of string;
  vs: array of string;
  {$ENDIF}
  s: Integer;
  {$IFDEF FMXLIB}
  {$IFDEF DELPHIXE7_LVL}
  e: TAdvTreeViewEditor;
  bc: IAdvPictureContainer;
  {$ENDIF}
  {$ENDIF}
begin
  inherited;
  s := inherited GetVerbCount;
  {$IFNDEF FMXLIB}
  s := s + 1;
  {$ENDIF}

  case Index - s of
    {$IFDEF FMXLIB}
    {$IFDEF DELPHIXE7_LVL}
    0:
    begin
      e := TAdvTreeViewEditor.Create(Application);
      if Supports(Component, IAdvPictureContainer, bc) then
        e.PictureContainer := bc.PictureContainer;

      e.TreeView := TAdvCustomTreeView(Component);
      if e.Execute = mrOK then
        Designer.Modified;

      e.Free;
    end;
    {$ENDIF}
    {$ENDIF}
    1: TAdvCustomTreeView(Component).ClearNodes;
    2: TAdvCustomTreeView(Component).ClearColumns;
    3:
    begin
      {$IFDEF FMXLIB}
      {$IFNDEF DELPHIXE10_LVL}
      if InputQuery('Enter column text', 'Text:', v) then
        TAdvCustomTreeViewProtected(Component).Columns.Add.Text := v;
      {$ELSE}
      SetLength(vs, 1);
      if TDialogServiceSync.InputQuery('Enter column text', ['Text:'], vs) then
        TAdvCustomTreeViewProtected(Component).Columns.Add.Text := vs[0];
      {$ENDIF}
      {$ELSE}
      if InputQuery('Enter column text', 'Text:', v) then
        TAdvCustomTreeViewProtected(Component).Columns.Add.Text := v;
      {$ENDIF}
    end;
    {$IFDEF DELPHIXE2_LVL}
    4:
    begin
      SetLength(a, TAdvCustomTreeViewProtected(Component).Columns.Count);
      if Length(a) = 0 then
        raise Exception.Create('Please add a new column first')
      else
      begin
        for I := 0 to TAdvCustomTreeViewProtected(Component).Columns.Count - 1 do
          a[I] := 'Value for ' + TAdvCustomTreeViewProtected(Component).GetColumnText(I) + ':';

        SetLength(vs, Length(a));
        {$IFDEF FMXLIB}
        {$IFNDEF DELPHIXE10_LVL}
        if InputQuery('Enter node text', a, vs) then
        {$ELSE}
        if TDialogServiceSync.InputQuery('Enter node text', a, vs) then
        {$ENDIF}
        {$ELSE}
        if InputQuery('Enter node text', a, vs) then
        {$ENDIF}
        begin
          n := TAdvCustomTreeViewProtected(Component).AddNode;
          for I := 0 to Length(vs) - 1 do
            n.Text[I] := vs[I];
        end;
      end;
    end;
    {$ENDIF}
  end;
end;

function TAdvTreeViewDesigntimeEditor.GetVerb(Index: Integer): string;
var
  s: Integer;
begin
  Result := inherited;
  s := inherited GetVerbCount;
  {$IFNDEF FMXLIB}
  s := s + 1;
  {$ENDIF}
  case Index - s of
    {$IFDEF FMXLIB}
    {$IFDEF DELPHIXE7_LVL}
    0: Result := 'Configure';
    {$ENDIF}
    {$ENDIF}
    1: Result := 'Clear &Nodes';
    2: Result := 'Clear &Columns';
    3: Result := '&Add Column';
    {$IFDEF DELPHIXE2_LVL}
    4: Result := 'Add N&ode';
    {$ENDIF}
  end;
end;

function TAdvTreeViewDesigntimeEditor.GetVerbCount: Integer;
begin
  Result := inherited;
  {$IFDEF DELPHIXE2_LVL}
  Result := Result + 4;
  {$ELSE}
  Result := Result + 3;
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF DELPHIXE7_LVL}
  Result := Result + 1;
  {$ENDIF}
  {$ENDIF}
end;

end.



