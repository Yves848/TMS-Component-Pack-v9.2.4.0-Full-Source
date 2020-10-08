{*************************************************************************}
{ TMS TAdvSearchEdit EditLink                                             }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2016 - 2018                                       }
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

unit AdvSearchEditEditLink;

interface

uses
  Classes, AdvGrid, AdvSearchEdit, AdvSearchList, AdvDropDown,
  Controls, Types, Graphics, Forms;

type
  TAdvSearchEditEditLink = class(TEditLink)
  private
    FSearchList: TSearchList;
    FColumnItems: TColumnItems;
    FCategoryList: TCategoryList;
    FEdit: TAdvSearchEdit;
    FAppearance: TAdvSearchListAppearance;
    FItemHeight: integer;
    FSearchButton: TSearchEditButton;
    FCategoryButton: TSearchEditButton;
    FDropDownShadow: boolean;
    FEmptyTextStyle: TFontStyles;
    FEmptyText: string;
    FDropDownWidth: integer;
    FEmptyTextFocused: boolean;
    FDropDownSizable: boolean;
    FFilterCondition: TFilterCondition;
    FDropDownFooter: TFooterAppearance;
    FDropDownHeader: THeaderAppearance;
    FOnSearchButtonClick: TNotifyEvent;
    procedure SetCategoryList(const Value: TCategoryList);
    procedure SetColumnItems(const Value: TColumnItems);
    procedure SetSearchList(const Value: TSearchList);
    procedure SetAppearance(const Value: TAdvSearchListAppearance);
    procedure SetCategoryButton(const Value: TSearchEditButton);
    procedure SetSearchButton(const Value: TSearchEditButton);
    procedure SetFilterCondition(const Value: TFilterCondition);
    procedure SetDropDownFooter(const Value: TFooterAppearance);
    procedure SetDropDownHeader(const Value: THeaderAppearance);
  protected
    procedure EditExit(Sender: TObject);
    function CreateSearchList: TSearchList; virtual;
    function CreateColumns: TColumnItems; virtual;
    function CreateCategories: TCategoryList; virtual;
    procedure DoSearchButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetFocus(Value: Boolean); override;
    procedure SetProperties; override;
    procedure SetRect(r: TRect); override;
    function SelectedItem: TSearchListItem;
  published
    property Appearance: TAdvSearchListAppearance read FAppearance write SetAppearance;
    property Categories: TCategoryList read FCategoryList write SetCategoryList;
    property CategoryButton: TSearchEditButton read FCategoryButton write SetCategoryButton;
    property Columns: TColumnItems read FColumnItems write SetColumnItems;
    property DropDownFooter: TFooterAppearance read FDropDownFooter write SetDropDownFooter;
    property DropDownHeader: THeaderAppearance read FDropDownHeader write SetDropDownHeader;
    property DropDownShadow: boolean read FDropDownShadow write FDropDownShadow default true;
    property DropDownSizable: boolean read FDropDownSizable write FDropDownSizable default true;
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth default 0;
    property EmptyText: string read FEmptyText write FEmptyText;
    property EmptyTextFocused: boolean read FEmptyTextFocused write FEmptyTextFocused default false;
    property EmptyTextStyle: TFontStyles read FEmptyTextStyle write FEmptyTextStyle default [];
    property FilterCondition: TFilterCondition read FFilterCondition write SetFilterCondition;
    property ItemHeight: integer read FItemHeight write FItemHeight default 20;
    property Items: TSearchList read FSearchList write SetSearchList;
    property SearchButton: TSearchEditButton read FSearchButton write SetSearchButton;
    property OnSearchButtonClick: TNotifyEvent read FOnSearchButtonClick write FOnSearchButtonClick;
  end;

procedure Register;


implementation

uses
  Windows;

{ TAdvSearchEditEditLink }

constructor TAdvSearchEditEditLink.Create(AOwner: TComponent);
begin
  inherited;
  WantKeyLeftRight := True;
  WantKeyHomeEnd := True;
  WantKeyReturn := False;
  WantKeyEscape := False;

  FSearchList := CreateSearchList;
  FCategoryList := CreateCategories;
  FColumnItems := CreateColumns;
  FAppearance := TAdvSearchListAppearance.Create;
  FCategoryButton := TSearchEditButton.Create;
  FSearchButton := TSearchEditButton.Create;
  FDropdownFooter := TFooterAppearance.Create(Self);
  FDropdownHeader := THeaderAppearance.Create(Self);
  FFilterCondition := TFilterCondition.Create;
  FItemHeight := 20;
  FEmptyText := 'Search ...';
  FEmptyTextStyle := [];
  FEmptyTextFocused := false;
end;

function TAdvSearchEditEditLink.CreateCategories: TCategoryList;
begin
  Result := TCategoryList.Create(Self);
end;

function TAdvSearchEditEditLink.CreateColumns: TColumnItems;
begin
  Result := TColumnItems.Create(Self);
end;

procedure TAdvSearchEditEditLink.CreateEditor(AParent: TWinControl);
begin
  FEdit := TAdvSearchEdit.Create(Grid);

  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  FEdit.BorderStyle := bsNone;
  FEdit.Parent := AParent;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnSearchButtonClick := DoSearchButtonClick;
end;

function TAdvSearchEditEditLink.CreateSearchList: TSearchList;
begin
  Result := TSearchList.Create(Self);
end;

destructor TAdvSearchEditEditLink.Destroy;
begin
  FDropDownFooter.Free;
  FDropDownHeader.Free;
  FFilterCondition.Free;
  FCategoryButton.Free;
  FSearchButton.Free;
  FAppearance.Free;
  FSearchList.Free;
  FCategoryList.Free;
  FColumnItems.Free;
  inherited;
end;

procedure TAdvSearchEditEditLink.DestroyEditor;
begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := nil;
end;

procedure TAdvSearchEditEditLink.DoSearchButtonClick(Sender: TObject);
begin
  if Assigned(OnSearchButtonClick) then
    OnSearchButtonClick(Sender);
end;

procedure TAdvSearchEditEditLink.EditExit(Sender: TObject);
var
  pt: TPoint;
  grid: TAdvStringGrid;
begin
  grid := (GetParent as TAdvStringGrid);

  grid.HideInplaceEdit;
  FEdit.Hide;

  GetCursorPos(pt);

  pt := grid.ScreenToClient(pt);

  if PtInRect(grid.ClientRect,pt) then
  begin
    if grid.CanFocus then
      grid.SetFocus;
  end;

end;

procedure TAdvSearchEditEditLink.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = VK_DOWN) and (ssAlt in Shift) then
    Exit;

  inherited;
end;

function TAdvSearchEditEditLink.GetEditControl: TWinControl;
begin
  Result := FEdit;
end;

function TAdvSearchEditEditLink.GetEditorValue: String;
begin
  Result := FEdit.Text;
end;

function TAdvSearchEditEditLink.SelectedItem: TSearchListItem;
begin
  Result := FEdit.SearchList.SelectedItem;
end;

procedure TAdvSearchEditEditLink.SetAppearance(
  const Value: TAdvSearchListAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvSearchEditEditLink.SetCategoryButton(
  const Value: TSearchEditButton);
begin
  FCategoryButton.Assign(Value);
end;

procedure TAdvSearchEditEditLink.SetCategoryList(const Value: TCategoryList);
begin
  FCategoryList.Assign(Value);
end;

procedure TAdvSearchEditEditLink.SetColumnItems(const Value: TColumnItems);
begin
  FColumnItems.Assign(Value);
end;

procedure TAdvSearchEditEditLink.SetDropDownFooter(
  const Value: TFooterAppearance);
begin
  FDropDownFooter.Assign(Value);
end;

procedure TAdvSearchEditEditLink.SetDropDownHeader(
  const Value: THeaderAppearance);
begin
  FDropDownHeader.Assign(Value);
end;

procedure TAdvSearchEditEditLink.SetEditorValue(s: String);
begin
  FEdit.Text := s;
end;

procedure TAdvSearchEditEditLink.SetFilterCondition(
  const Value: TFilterCondition);
begin
  FFilterCondition.Assign(Value);
end;

procedure TAdvSearchEditEditLink.SetFocus(Value: Boolean);
begin
  inherited;
  FEdit.SetFocus;
  if not FEdit.AutoSelect then
    FEdit.SelStart := Length(FEdit.Text);
end;

procedure TAdvSearchEditEditLink.SetProperties;
begin
  FEdit.Categories.Assign(FCategoryList);
  FEdit.Items.Assign(FSearchList);
  FEdit.Columns.Assign(FColumnItems);
  FEdit.ItemHeight := FItemHeight;
  FEdit.Appearance := FAppearance;
  FEdit.CategoryButton.Assign(FCategoryButton);
  FEdit.SearchButton.Assign(FSearchButton);
  FEdit.FilterCondition.Assign(FFilterCondition);
  FEdit.DropDownShadow := FDropDownShadow;
  FEdit.DropDownSizable := FDropDownSizable;
  FEdit.DropDownWidth := FDropDownWidth;
  FEdit.EmptyText := FEmptyText;
  FEdit.EmptyTextFocused := FEmptyTextFocused;
  FEdit.EmptyTextStyle := FEmptyTextStyle;
  FEdit.DropDownHeader.Assign(FDropDownHeader);
  FEdit.DropDownFooter.Assign(FDropDownFooter);

  inherited;
end;

procedure TAdvSearchEditEditLink.SetRect(r: TRect);
begin
  inherited;
end;

procedure TAdvSearchEditEditLink.SetSearchButton(
  const Value: TSearchEditButton);
begin
  FSearchButton.Assign(Value);
end;

procedure TAdvSearchEditEditLink.SetSearchList(const Value: TSearchList);
begin
  FSearchList.Assign(Value);
end;

procedure Register;
begin
  RegisterComponents('TMS Grids',[TAdvSearchEditEditLink]);
end;

end.
