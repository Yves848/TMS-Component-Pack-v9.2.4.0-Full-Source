{**************************************************************************}
{ TADVSTRINGGRID EDITLINKS                                                 }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2000-2018                                         }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AsgLinks;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls, Graphics, Forms, SysUtils,
  MoneyEdit, AdvEdit, CListEd, AdvGrid, ColorCombo, ImagePicker, ShellApi,
  AdvFileNameEdit, AdvDirectoryEdit, Dialogs, LUCombo;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvEditEditLink = class(TEditLink)
  private
    FEdit: TAdvEdit;
    FEditColor: TColor;
    FModifiedColor: TColor;
    FEditType: TAdvEditType;
    FSuffix: String;
    FPrefix: String;
    FEditAlign: TEditAlign;
    FShowModified: Boolean;
    FPrecision: Integer;
    FSigned: Boolean;
    FExcelStyleDecimalSeparator: Boolean;
    FMaxLength: Integer;
    FValidChars: string;
  protected
    procedure EditExit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetFocus(Value: Boolean); override;
    procedure SetProperties; override;
    procedure SetRect(r: TRect); override;
  published
    property EditAlign: TEditAlign read FEditAlign Write FEditAlign;
    property EditColor: TColor read FEditColor Write FEditColor;
    property MaxLength: Integer read FMaxLength write FMaxLength;
    property ModifiedColor: TColor read FModifiedColor Write FModifiedColor;
    property EditType: TAdvEditType read FEditType Write FEditType;
    property Prefix: String read FPrefix Write FPrefix;
    property ShowModified: boolean read FShowModified Write FShowModified;
    property Suffix: String read FSuffix Write FSuffix;
    property Precision: integer read FPrecision Write FPrecision;
    property Signed: boolean read FSigned write FSigned;
    property ExcelStyleDecimalSeparator: boolean read FExcelStyleDecimalSeparator write FExcelStyleDecimalSeparator;
    property ValidChars: string read FValidChars write FValidChars;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvFileNameEditLink = class(TEditLink)
  private
    FEdit: TAdvFileNameEdit;
    FModifiedColor: TColor;
    FEditColor: TColor;
    FShowModified: boolean;
    FFilterIndex: Integer;
    FFilter: string;
    FInitialDir: string;
    FDialogTitle: string;
    FDialogKind: TFileDialogKind;
    FDialogOptions: TOpenOptions;
    FDefaultExt : String;
  protected
    procedure EditExit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
  published
    property EditColor: TColor read FEditColor write FEditColor;
    property ModifiedColor: TColor read FModifiedColor write FModifiedColor;
    property ShowModified: boolean read FShowModified write FShowModified;
    property Filter: string read FFilter write FFilter;
    property FilterIndex: Integer read FFilterIndex write FFilterIndex default 1;
    property InitialDir: string read FInitialDir write FInitialDir;
    property DialogOptions: TOpenOptions read FDialogOptions write FDialogOptions default [ofHidereadOnly, ofEnableSizing];
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DialogKind: TFileDialogKind read FDialogKind write FDialogKind;
    Property DefaultExt: string read FDefaultExt write FDefaultExt;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDirectoryEditLink = class(TEditLink)
  private
    FEdit: TAdvDirectoryEdit;
    FShowModified: Boolean;
    FModifiedColor: TColor;
    FEditColor: TColor;
  protected
    procedure EditExit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
  published
    property EditColor: TColor read FEditColor write FEditColor;
    property ShowModified: Boolean read FShowModified write FShowModified;
    property ModifiedColor: TColor read FModifiedColor write FModifiedColor;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TColorComboEditLink = class(TEditLink)
  private
    FEdit: TAdvColorComboBox;
    FDropDownHeight: integer;
    FDropDownWidth: integer;
  protected
    procedure EditExit(Sender: TObject);
  public
    Constructor Create(aOwner: TComponent); override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    procedure SetRect(r: trect); override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
  published
    property DropDownHeight: integer read FDropDownHeight write FDropDownHeight;
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TMoneyEditLink = class(TEditLink)
  Private
    FEdit: TMoneyEdit;
    FCalculatorLook: TCalculatorLook;
    procedure SetCalculatorLook(Const Value: TCalculatorLook);
  protected
    procedure EditExit(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
    procedure SetCellProps(AColor: TColor; AFont: TFont); override;
  Published
    Property CalculatorLook: TCalculatorLook read FCalculatorLook write SetCalculatorLook;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCheckEditLink = class(TEditLink)
  Private
    FEdit: TCheckListEdit;
  protected
    procedure EditExit(sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetCellProps(AColor: TColor; AFont: TFont); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TImagePickerEditLink = class(TEditLink)
  private
    FEdit: TImagePicker;
    FDropDownHeight: integer;
    FDropDownWidth: integer;
    FImages: TImageList;
  protected
    procedure EditExit(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    procedure SetRect(r: trect); override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
  published
    property DropDownHeight: integer read FDropDownHeight write FDropDownHeight;
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth;
    property Images: TImageList read FImages write FImages;
  end;

// class definition
// Lookup column combo box editor class

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TLUComboEditLink = class(TEditLink)
  private
    FCellHeight: Integer;
    FItems: TStrings;
    FFlat: Boolean;
    FDropHeight: Integer;
    FDropWidth: Integer;
    FEtched: Boolean;
    procedure SetItems(const Value: TStrings);
  protected
    procedure EditExit(Sender: TObject);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    Combo: TLUCombo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    function GetEditorValue:string; override;
    procedure SetEditorValue(s:string); override;
    function GetEditControl:TWinControl; override;
    procedure SetProperties; override;
    {$IFDEF ASG194}
    procedure SetCellProps(AColor: TColor; AFont: TFont); override;
    {$ENDIF}
    procedure SetRect(R: TRect); override;
  published
    property DropHeight: Integer read FDropHeight write FDropHeight;
    property DropWidth: Integer read FDropWidth write FDropWidth;
    property Etched: Boolean read FEtched write FEtched;
    property Flat: Boolean read FFlat write FFlat;
    property Items: TStrings read FItems write SetItems;
  end;


procedure Register;

implementation

{$I DELPHIXE.INC}

type
  TMyWinControl = class(TWinControl)
  published
 //   property OnExit;
  end;

function StripThousandSep(s: string): string;
begin
  while (Pos(ThousandSeparator, s) > 0) do
    Delete(s, Pos(ThousandSeparator, s), 1);
  Result := s;
end;


procedure Register;
begin
  RegisterComponents('TMS Grids', [TAdvEditEditLink, TColorComboEditLink,
    TCheckEditLink, TMoneyEditLink, TImagePickerEditLink,
    TAdvFileNameEditLink,
    TAdvDirectoryEditLink,
    TLUComboEditLink]);
end;

{ TAdvEditEditLink }

procedure TAdvEditEditLink.CreateEditor(AParent: TWinControl);
begin
  FEdit := TAdvEdit.Create(Grid);
  FEdit.ShowModified := True;
  FEdit.DefaultHandling := False;
  FEdit.ModifiedColor := clRed;
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  WantKeyLeftRight := True;
  WantKeyHomeend := True;
  FEdit.Parent := AParent;
  FEdit.Color := EditColor;
  FEdit.DefaultHandling := True;
  FEdit.Flat := true;
  FEdit.FlatLineColor := clNone;
end;

procedure TAdvEditEditLink.DestroyEditor;
begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := nil;
end;

Function TAdvEditEditLink.GetEditorValue: String;
begin
  if Assigned(FEdit) then
  begin
//    if EditType = etMoney then
//      Result := StripThousandSep(FEdit.Text)
//    else
      Result := FEdit.Text;
  end;
end;

procedure TAdvEditEditLink.EditExit(sender: TObject);
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

Function TAdvEditEditLink.GetEditControl: TWinControl;
begin
  Result := FEdit;
end;

procedure TAdvEditEditLink.SetEditorValue(s: String);
begin
  FEdit.Text := s;
end;

procedure TAdvEditEditLink.SetFocus(Value: Boolean);
begin
  inherited;
  if not FEdit.AutoSelect then
    FEdit.SelStart := Length(FEdit.Text);
end;

Constructor TAdvEditEditLink.Create(aOwner: TComponent);
begin
  inherited;
  WantKeyLeftRight := True;
  WantKeyHomeend := True;
  EditColor := clWindow;
  ModifiedColor := clRed;
  EditType := etString;
end;

procedure TAdvEditEditLink.SetProperties;
begin
  inherited;
  FEdit.Color := FEditColor;
  FEdit.FocusColor := FEditColor;
  FEdit.EditType := FEditType;
  FEdit.EditAlign := FEditAlign;
  FEdit.ModifiedColor := FModifiedColor;
  FEdit.Prefix := FPrefix;
  FEdit.Suffix := FSuffix;
  FEdit.ShowModified := FShowModified;
  FEdit.MaxLength := FMaxLength;
  FEdit.Precision := FPrecision;
  FEdit.Signed := FSigned;
  FEdit.ValidChars := FValidChars;
  FEdit.ExcelStyleDecimalSeparator := FExcelStyleDecimalSeparator;
end;

procedure TAdvEditEditLink.SetRect(r: TRect);
begin
  inherited;
end;

Destructor TAdvEditEditLink.Destroy;
begin
  inherited;
end;

{ TColorComboEditLink }

procedure TColorComboEditLink.CreateEditor(AParent: TWinControl);
begin
  FEdit := TAdvColorComboBox.Create(Grid);
  FEdit.Style := csOwnerDrawFixed;
  FEdit.OnExit := EditExit;
  FEdit.OnKeydown := EditKeyDown;
  FEdit.Width := 0;
  FEdit.Height := 0;
  FEdit.IsWinXP := Grid.IsThemed;
  FEdit.Parent := AParent;
end;

procedure TColorComboEditLink.DestroyEditor;
begin
  FEdit.Free;
end;

function TColorComboEditLink.GetEditorValue: String;
begin
  Result := FEdit.Items[FEdit.ItemIndex];
end;

procedure TColorComboEditLink.EditExit(sender: TObject);
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

function TColorComboEditLink.GetEditControl: TWinControl;
begin
  Result := FEdit;
end;

procedure TColorComboEditLink.SetRect(r: TRect);
begin
  inherited;
  FEdit.Height := r.Bottom - r.Top + FDropDownHeight;
end;

procedure TColorComboEditLink.SetEditorValue(s: String);
Var
  i: Integer;
begin
  FEdit.Items.Clear;
  For i := 0 To 15 Do
    FEdit.Items.Add(IntToStr(i));
  If s = '' Then
    s := '0';
  FEdit.Text := s;
  FEdit.ItemIndex := StrToInt(s);
end;

Constructor TColorComboEditLink.Create(aOwner: TComponent);
begin
  inherited;
  WantKeyUpDown := True;
  DropDownWidth := 100;
  DropDownHeight := 100;
end;

procedure TColorComboEditLink.SetProperties;
begin
  inherited;
  FEdit.DropWidth := 150;
end;



{ TMoneyEditLink }

Constructor TMoneyEditLink.Create(aOwner: TComponent);
begin
  inherited;
  WantKeyLeftRight := True;
  WantKeyHomeend := True;
  FCalculatorLook := TCalculatorLook.Create;
end;

procedure TMoneyEditLink.CreateEditor(AParent: TWinControl);
begin
  inherited;
  FEdit := TMoneyEdit.Create(Grid);
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeydown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  FEdit.Parent := AParent;

  FEdit.CalculatorLook.Flat := True;
  FEdit.CalculatorLook.ButtonColor := clBlue;
  FEdit.CalculatorLook.Color := clYellow;
  FEdit.CalculatorLook.Font.Color := clWhite;
  FEdit.CalculatorLook.Font.Name := 'Tahoma';
  FEdit.CalculatorLook.Font.Style := [fsBold];
end;

Destructor TMoneyEditLink.Destroy;
begin
  FCalculatorLook.Free;
  inherited;
end;

procedure TMoneyEditLink.DestroyEditor;
begin
  FEdit.Free;
end;

procedure TMoneyEditLink.EditExit(sender: TObject);
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

Function TMoneyEditLink.GetEditControl: TWinControl;
begin
  Result := FEdit;
end;

Function TMoneyEditLink.GetEditorValue: String;
begin
  Result := FEdit.Text;
end;

procedure TMoneyEditLink.SetCalculatorLook(Const Value: TCalculatorLook);
begin
  FCalculatorLook.Assign(Value);
end;

procedure TMoneyEditLink.SetCellProps(AColor: TColor; AFont: TFont);
begin
  inherited;
  FEdit.Color := AColor;
end;

procedure TMoneyEditLink.SetEditorValue(s: String);
begin
  FEdit.Text := s;
end;

procedure TMoneyEditLink.SetProperties;
begin
  inherited;
  FEdit.CalculatorLook.Assign(FCalculatorLook);
end;

{ TCheckEditLink }

Constructor TCheckEditLink.Create(aOwner: TComponent);
begin
  inherited;
  WantKeyUpDown := True;
end;

procedure TCheckEditLink.CreateEditor(AParent: TWinControl);
begin
  inherited;
  FEdit := TCheckListEdit.Create(Grid);
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeydown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  FEdit.Parent := AParent;
end;

procedure TCheckEditLink.DestroyEditor;
begin
  FEdit.Free;
end;

procedure TCheckEditLink.EditExit(sender: TObject);
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

Function TCheckEditLink.GetEditControl: TWinControl;
begin
  Result := FEdit;
end;


function TCheckEditLink.GetEditorValue: String;
begin
  Result := FEdit.Text;
end;

procedure TCheckEditLink.SetCellProps(AColor: TColor; AFont: TFont);
begin
  inherited;
  FEdit.Color := AColor;
end;

procedure TCheckEditLink.SetEditorValue(s: String);
begin
  FEdit.Text := s;
end;

{ TImagePickerEditLink }

constructor TImagePickerEditLink.Create(aOwner: TComponent);
begin
  inherited;
  WantKeyUpDown := True;
  DropDownWidth := 100;
  DropDownHeight := 100;
end;

procedure TImagePickerEditLink.CreateEditor(AParent: TWinControl);
begin
  inherited;
  FEdit := TImagePicker.Create(Grid);
  FEdit.OnExit := EditExit;
  FEdit.OnKeydown := EditKeyDown;
  FEdit.Width := 0;
  FEdit.Height := 0;
  FEdit.Parent := AParent;
  FEdit.Flat := True;
  FEdit.FlatLineColor := clNone;
  FEdit.Etched := True;
  FEdit.ItemHeight := 32;
end;

procedure TImagePickerEditLink.DestroyEditor;
begin
  FEdit.Free;
end;

procedure TImagePickerEditLink.EditExit(Sender: TObject);
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

function TImagePickerEditLink.GetEditControl: TWinControl;
begin
  Result := FEdit;
end;

function TImagePickerEditLink.GetEditorValue: String;
begin
  if FEdit.HandleAllocated then
  begin
    If FEdit.ItemIndex >= 0 Then
    begin
      Result := IntToStr(FEdit.Items.Items[FEdit.ItemIndex].ImageIndex);
    end;
  end;
end;

procedure TImagePickerEditLink.SetEditorValue(s: String);
begin
  if s = '' then
    s := '0';
  FEdit.SelectByImageIdx(StrToInt(s));
end;

procedure TImagePickerEditLink.SetProperties;
begin
  inherited;
  FEdit.Images := FImages;

end;

procedure TImagePickerEditLink.SetRect(r: trect);
begin
  inherited;
  FEdit.Height := r.Bottom - r.Top + FDropDownHeight;
  FEdit.DropHeight := FDropDownHeight;
  FEdit.DropWidth := FDropDownWidth;
  FEdit.EditHeight := r.Bottom - r.Top - 7;
end;


{ TAdvFileNameEditLink }

constructor TAdvFileNameEditLink.Create(AOwner: TComponent);
begin
  inherited;
  WantKeyLeftRight := True;
  WantKeyHomeend := True;
  EditColor := clWindow;
  ModifiedColor := clRed;
end;

procedure TAdvFileNameEditLink.CreateEditor(AParent: TWinControl);
begin
  FEdit := TAdvFileNameEdit.Create(Grid);
  FEdit.ShowModified := True;
  FEdit.DefaultHandling := False;
  FEdit.ModifiedColor := clRed;
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  WantKeyLeftRight := True;
  WantKeyHomeend := True;
  FEdit.Parent := AParent;
  FEdit.Color := EditColor;
end;

destructor TAdvFileNameEditLink.Destroy;
begin
  inherited;
end;

procedure TAdvFileNameEditLink.DestroyEditor;
begin
  If Assigned(FEdit) Then
    FEdit.Free;
  FEdit := Nil;
end;

procedure TAdvFileNameEditLink.EditExit(Sender: TObject);
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

function TAdvFileNameEditLink.GetEditControl: TWinControl;
begin
  Result := FEdit;
end;

function TAdvFileNameEditLink.GetEditorValue: String;
begin
  Result := FEdit.Text;
end;

procedure TAdvFileNameEditLink.SetEditorValue(s: String);
begin
  FEdit.Text := s;
end;

procedure TAdvFileNameEditLink.SetProperties;
begin
  inherited;
  FEdit.Color := FEditColor;
  FEdit.ModifiedColor := FModifiedColor;
  FEdit.ShowModified := FShowModified;

  FEdit.Filter := FFilter;
  FEdit.FilterIndex := FFilterIndex;
  FEdit.InitialDir := FInitialDir;
  FEdit.DialogOptions := FDialogOptions;
  FEdit.DialogTitle := FDialogTitle;
  FEdit.DialogKind := FDialogKind;
  FEdit.DefaultExt := FDefaultExt;
end;



{ TAdvDirectoryEditLink }

constructor TAdvDirectoryEditLink.Create(AOwner: TComponent);
begin
  inherited;
  WantKeyLeftRight := True;
  WantKeyHomeend := True;
  EditColor := clWindow;
  ModifiedColor := clRed;
end;

procedure TAdvDirectoryEditLink.CreateEditor(AParent: TWinControl);
begin
  FEdit := TAdvDirectoryEdit.Create(Grid);
  FEdit.ShowModified := True;
  FEdit.DefaultHandling := False;
  FEdit.ModifiedColor := clRed;
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  WantKeyLeftRight := True;
  WantKeyHomeend := True;
  FEdit.Parent := AParent;
  FEdit.Color := EditColor;
end;

destructor TAdvDirectoryEditLink.Destroy;
begin
  inherited;
end;

procedure TAdvDirectoryEditLink.DestroyEditor;
begin
  If Assigned(FEdit) Then
    FEdit.Free;
  FEdit := Nil;
end;

procedure TAdvDirectoryEditLink.EditExit(Sender: TObject);
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

function TAdvDirectoryEditLink.GetEditControl: TWinControl;
begin
  Result := FEdit;
end;

function TAdvDirectoryEditLink.GetEditorValue: String;
begin
  Result := FEdit.Text;
end;

procedure TAdvDirectoryEditLink.SetEditorValue(s: String);
begin
  FEdit.Text := s;
end;

procedure TAdvDirectoryEditLink.SetProperties;
begin
  inherited;
  FEdit.Color := FEditColor;
  FEdit.ModifiedColor := FModifiedColor;
  FEdit.ShowModified := FShowModified;
end;


//------------------------------------------------
constructor TLUComboEditLink.Create(AOwner: TComponent);
//------------------------------------------------
begin
  inherited;
  Combo := nil;
  FDropHeight := 250;
  FDropWidth := 300;
  FItems := TStringList.Create;
  FFlat:=true;
  WantKeyUpDown := True;
end;

//------------------------------------------------
procedure TLUComboEditLink.CreateEditor(AParent: TWinControl);
//------------------------------------------------
begin
  inherited;
  if not Assigned(Combo) then
  begin
    Combo := TLUCombo.Create(AParent);
    Combo.Parent := AParent;
    Combo.OnExit := EditExit;
    Combo.OnKeydown := EditKeyDown;
    Combo.FocusBorder := False;
  end;
end;
 
//------------------------------------------------
destructor TLUComboEditLink.Destroy;
//------------------------------------------------
begin
  FItems.Free;
  inherited;
end;
 
//------------------------------------------------
procedure TLUComboEditLink.EditExit(Sender: TObject);
//------------------------------------------------
var
  pt: TPoint;
  grid: TAdvStringGrid;
begin
  grid := (GetParent as TAdvStringGrid);

  grid.HideInplaceEdit;
  Combo.Hide;

  GetCursorPos(pt);

  pt := grid.ScreenToClient(pt);

  if PtInRect(grid.ClientRect,pt) then
  begin
    if grid.CanFocus then
      grid.SetFocus;
  end;
end;
 
//------------------------------------------------
function TLUComboEditLink.GetEditControl: TWinControl;
//------------------------------------------------
begin
  Result := Combo;
end;
 
//------------------------------------------------
function TLUComboEditLink.GetEditorValue: string;
//------------------------------------------------
begin
  result := Combo.text;
end;
 
//------------------------------------------------
procedure TLUComboEditLink.Notification(AComponent: TComponent;  AOperation: TOperation);
//------------------------------------------------
begin
   inherited;
end;
 
{$IFDEF ASG194}
//------------------------------------------------
procedure TLUComboEditLink.SetCellProps(AColor: TColor; AFont: TFont);
//------------------------------------------------
begin
  FCombo.Color := AColor;
  FCombo.Font := AFont;
end;
{$ENDIF}
 
//------------------------------------------------
procedure TLUComboEditLink.SetEditorValue(s: string);
//------------------------------------------------
begin
  Combo.Text := s;
end;
 
//------------------------------------------------
procedure TLUComboEditLink.SetItems(const Value: TStrings);
//------------------------------------------------
begin
  FItems.Assign(Value);
end;
 
//------------------------------------------------
procedure TLUComboEditLink.SetProperties;
//------------------------------------------------
var
  i: Integer;
begin
  inherited;
  Combo.Flat := FFlat;
  Combo.Etched := FEtched;
  Combo.Height := FDropHeight;
  Combo.DropWidth := FDropWidth;
  Combo.Flat := true;
 
  Combo.Items.Clear;
  for i := 1 to FItems.Count do
  begin
    Combo.Items.Add(fitems.Strings[i - 1]);
  end;
end;

//------------------------------------------------
procedure TLUComboEditLink.SetRect(r: TRect);
//------------------------------------------------
begin
  inherited;
  FCellHeight := r.Bottom - r.Top;
end;




end.
