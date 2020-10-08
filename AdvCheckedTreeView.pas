{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2015 - 2016                                 }
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

unit AdvCheckedTreeView;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvTreeView
  {$IFNDEF LCLLIB}
  , Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  , fgl
  {$ENDIF}
  ,AdvTreeViewData
  {$IFDEF FMXLIB}
  , AdvBaseControl
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New : CheckedNodes function to return nodes that are checked
  // v1.0.1.1 : Fixed : Issue with designtime initialization of checked nodes

type
  TAdvCheckedTreeViewNodes = class;

  TAdvCheckedTreeViewNodeValue = class(TAdvTreeViewNodeValue)
  public
    constructor Create(Collection: TCollection); override;
  end;

  TAdvCheckedTreeViewNodeValues = class(TAdvTreeViewNodeValues)
  protected
    function GetItemClass: TCollectionItemClass; override;
  end;

  TAdvCheckedTreeViewNode = class(TAdvTreeViewNode)
  protected
    function CreateNodeValues: TAdvTreeViewNodeValues; override;
    function CreateNodes: TAdvTreeViewNodes; override;
  end;

  TAdvTreeViewCheckedNodes = array of TAdvTreeViewNode;

  TAdvCheckedTreeViewNodes = class(TAdvTreeViewNodes)
  protected
    function GetItemClass: TCollectionItemClass; override;
    function CheckedNodesInternal(AColumn: Integer = 0; ARecurse: Boolean = True): TAdvTreeViewCheckedNodes; virtual;
  public
    function CheckedNodes(AColumn: Integer = 0; ARecurse: Boolean = True): TAdvTreeViewCheckedNodes; virtual;
  end;

  {$IFDEF FMXLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  {$IFDEF VCLLIB}
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  {$ENDIF}
  TAdvCheckedTreeView = class(TAdvTreeView)
  private
    function GetChecked(ANode: TAdvTreeViewNode): Boolean;
    procedure SetChecked(ANode: TAdvTreeViewNode; const Value: Boolean);
  protected
    function CreateNodes: TAdvTreeViewNodes; override;
    function GetVersion: string; override;
  public
    procedure InitSample; override;
    property Checked[ANode: TAdvTreeViewNode]: Boolean read GetChecked write SetChecked;
    function CheckedNodes(AColumn: Integer = 0; ARecurse: Boolean = True): TAdvTreeViewCheckedNodes; virtual;
  end;

implementation

{ TAdvCheckedTreeView }

function TAdvCheckedTreeView.CheckedNodes(AColumn: Integer;
  ARecurse: Boolean): TAdvTreeViewCheckedNodes;
begin
  Result := TAdvCheckedTreeViewNodes(Nodes).CheckedNodes(AColumn, ARecurse);
end;

function TAdvCheckedTreeView.CreateNodes: TAdvTreeViewNodes;
begin
  Result := TAdvCheckedTreeViewNodes.Create(Self, nil);
end;

function TAdvCheckedTreeView.GetChecked(ANode: TAdvTreeViewNode): Boolean;
begin
  Result := False;
  if Assigned(ANode) then
    Result := ANode.Checked[0];
end;

function TAdvCheckedTreeView.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCheckedTreeView.InitSample;
var
  pAudi, pMercedes, pSub, n: TAdvTreeViewNode;
begin
  BeginUpdate;
  ClearNodeList;
  Columns.Clear;
  Nodes.Clear;

  Columns.Add.Text := 'Model';

  pAudi := AddNode;
  pAudi.Text[0] := 'Audi';
  pAudi.Extended := True;

  pSub := AddNode(pAudi);
  pSub.Text[0] := 'A3';

  pSub := AddNode(pAudi);
  pSub.Text[0] := 'A5 series';

  n := AddNode(pSub);
  n.Text[0] := 'S5';

  n := AddNode(pSub);
  n.Text[0] := 'RS5';

  pSub := AddNode(pAudi);
  pSub.Text[0] := 'A8';

  pMercedes := AddNode;
  pMercedes.Text[0] := 'Mercedes';
  pMercedes.Extended := True;

  pSub := AddNode(pMercedes);
  pSub.Text[0] := 'SLS';

  pSub := AddNode(pMercedes);
  pSub.Text[0] := 'SLK';

  pSub := AddNode(pMercedes);
  pSub.Text[0] := 'GLA';

  ExpandAll;
  EndUpdate;
end;

procedure TAdvCheckedTreeView.SetChecked(ANode: TAdvTreeViewNode;
  const Value: Boolean);
begin
  if Assigned(ANode) then
    ANode.Checked[0] := Value;
end;

{ TAdvCheckedTreeViewNode }

function TAdvCheckedTreeViewNode.CreateNodes: TAdvTreeViewNodes;
begin
  Result := TAdvCheckedTreeViewNodes.Create(TreeView, Self);
end;

function TAdvCheckedTreeViewNode.CreateNodeValues: TAdvTreeViewNodeValues;
begin
  Result := TAdvCheckedTreeViewNodeValues.Create(TreeView, Self);
end;

{ TAdvCheckedTreeViewNodes }

function TAdvCheckedTreeViewNodes.CheckedNodes(AColumn: Integer;
  ARecurse: Boolean): TAdvTreeViewCheckedNodes;
begin
  Result := CheckedNodesInternal(AColumn, ARecurse);
end;

function TAdvCheckedTreeViewNodes.CheckedNodesInternal(AColumn: Integer; ARecurse: Boolean): TAdvTreeViewCheckedNodes;
var
  I: Integer;
  t: TAdvTreeViewData;
  n: TAdvTreeViewNode;
  k: Integer;
  a: TAdvTreeViewCheckedNodes;
begin
  Result := nil;
  t := TreeView;
  if not Assigned(t) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    n := Items[I];
    if TAdvCheckedTreeView(t).Checked[n] then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := n;
    end;

    if ARecurse then
    begin
      a := TAdvCheckedTreeViewNodes(n.Nodes).CheckedNodesInternal(AColumn, ARecurse);
      for K := 0 to Length(a) - 1 do
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := a[K];
      end;
    end;
  end;
end;

function TAdvCheckedTreeViewNodes.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvCheckedTreeViewNode;
end;

{ TAdvCheckedTreeViewNodeValue }

constructor TAdvCheckedTreeViewNodeValue.Create(Collection: TCollection);
var
  t: TAdvTreeViewData;
begin
  inherited;
  t := TreeView;
  if Assigned(t) and t.IsDesigntime then
    CheckType := tvntCheckBox;
end;

{ TAdvCheckedTreeViewNodeValues }

function TAdvCheckedTreeViewNodeValues.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvCheckedTreeViewNodeValue;
end;

end.

