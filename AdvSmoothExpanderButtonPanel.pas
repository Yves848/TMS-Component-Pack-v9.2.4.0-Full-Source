{**************************************************************************}
{ TAdvSmoothExpanderButtonPanel component                                  }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2010 - 2019                                                }
{   Email : info@tmssoftware.com                                           }
{   Web : https://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvSmoothExpanderButtonPanel;

{$I TMSDEFS.inc}

interface

uses
  Windows, Messages, ExtCtrls, SysUtils, Classes, Graphics, Controls, StdCtrls, forms,
  Comobj, Activex, Math, AdvStyleIF, ImgList,
  GDIPPictureContainer, GDIPFill, AdvSmoothExpanderPanel, AdvGDIP, Types
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.0.2.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.0.2.1 : Fixed : Issue with Access violation in Autosize
  // v1.0.2.2 : Fixed : Issue with disabled buttons and repainting
  // v1.0.3.0 : New : Built-in support for Office 2010 colors
  // v1.0.4.0 : New : Tag property for each button
  // v1.0.4.1 : Improved : Only draw buttons in view
  // v1.0.4.2 : Fixed : Issue with OnButtonClick for disabled buttons buttons
  // v1.1.0.0 : New : PictureContainer and ImageList support
  // v1.2.0.0 : New : Metro style support
  // v1.3.0.0 : New : Alternative layout mode with Rows / Columns and separate column / row span for each button
  // v1.3.1.0 : Improved : Support for High DPI (button, text, expand/collapse and buttonstatus size)

type
  TAdvSmoothExpanderButtonPanel = class;

  TAdvSmoothExpanderButtonAutoSize = class(TPersistent)
  private
    FOwner: TAdvSmoothExpanderButtonPanel;
    FDecreaseHeight: Boolean;
    FIncreaseHeight: Boolean;
    FOnChange: TNotifyEvent;
    FDecreaseWidth: Boolean;
    FIncreaseWidth: Boolean;
    procedure SetDecreaseHeight(const Value: Boolean);
    procedure SetIncreaseHeight(const Value: Boolean);
    procedure SetDecreaseWidth(const Value: Boolean);
    procedure SetIncreaseWidth(const Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothExpanderButtonPanel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property IncreaseHeight: Boolean read FIncreaseHeight write SetIncreaseHeight default True;
    property DecreaseHeight: Boolean read FDecreaseHeight write SetDecreaseHeight default False;
    property IncreaseWidth: Boolean read FIncreaseWidth write SetIncreaseWidth default False;
    property DecreaseWidth: Boolean read FDecreaseWidth write SetDecreaseWidth default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothExpanderButtonStatus = class(TPersistent)
  private
    FOwner: TAdvSmoothExpanderButtonPanel;
    FOffsetTop: integer;
    FOffsetLeft: integer;
    FVisible: Boolean;
    FAppearance: TGDIPStatus;
    FOnChange: TNotifyEvent;
    procedure SetOffsetLeft(const Value: integer);
    procedure SetOffsetTop(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetAppearance(const Value: TGDIPStatus);
  protected
    procedure Changed;
    procedure AppearanceChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothExpanderButtonPanel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default false;
    property OffsetLeft: integer read FOffsetLeft write SetOffsetLeft default 0;
    property OffsetTop: integer read FOffsetTop write SetOffsetTop default 0;
    property Appearance: TGDIPStatus read FAppearance write SetAppearance;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothExpanderButtonPosItem = class(TPersistent)
  public
    ButtonIdx: Integer;
    ButtonSet: Boolean;
    ColSpan, RowSpan, Col, Row: Integer;
    R: TRect;
  end;

  TAdvSmoothExpanderButton = class(TCollectionItem)
  private
    FOwner: TAdvSmoothExpanderButtonPanel;
    FDown: Boolean;
    FEnabled: Boolean;
    FPicture: TAdvGDIPPicture;
    FBevel: boolean;
    FColor: TColor;
    FBevelColor: TColor;
    FCaption: string;
    FStatusCaption: String;
    FTextColor: TColor;
    FColorDisabled: TColor;
    FTag: Integer;
    FPictureName: String;
    FImageIndex: Integer;
    FDisabledImageIndex: Integer;
    FDisabledPictureName: String;
    FRowSpan: Integer;
    FColumnSpan: Integer;
    FVisible: Boolean;
    FButtonPosition: TAdvSmoothExpanderButtonPosItem;
    procedure SetBevel(const Value: boolean);
    procedure SetBevelColor(const Value: TColor);
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetStatusCaption(const Value: String);
    procedure SetTextColor(const Value: TColor);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetTag(const Value: Integer);
    procedure SetImageIndex(const Value: Integer);
    procedure SetPictureName(const Value: String);
    procedure SetDisabledImageIndex(const Value: Integer);
    procedure SetDisabledPictureName(const Value: String);
    procedure SetColumnSpan(const Value: Integer);
    procedure SetRowSpan(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
    property ButtonPosition: TAdvSmoothExpanderButtonPosItem read FButtonPosition write FButtonPosition;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ColumnSpan: Integer read FColumnSpan write SetColumnSpan default 1;
    property RowSpan: Integer read FRowSpan write SetRowSpan default 1;
    property Bevel: boolean read FBevel write SetBevel default true;
    property BevelColor: TColor read FBevelColor write SetBevelColor default clWhite;
    property Caption: string read FCaption write SetCaption;
    property StatusCaption: String read FStatusCaption write SetStatusCaption;
    property Color: TColor read FColor write SetColor default clSilver;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled default clGray;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property Tag: Integer read FTag write SetTag default 0;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property PictureName: String read FPictureName write SetPictureName;
    property DisabledImageIndex: Integer read FDisabledImageIndex write SetDisabledImageIndex default -1;
    property DisabledPictureName: String read FDisabledPictureName write SetDisabledPictureName;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TAdvSmoothExpanderButtons = class(TCollection)
  private
    FOwner: TAdvSmoothExpanderButtonPanel;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothExpanderButton;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothExpanderButton);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothExpanderButtonPanel);
    function Add: TAdvSmoothExpanderButton;
    function Insert(Index: Integer): TAdvSmoothExpanderButton;
    property Items[Index: Integer]: TAdvSmoothExpanderButton read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothExpanderButtonStatusDraw = procedure(Sender: TObject; StatusAppearance: TGDIPFill; StatusFont: TFont; ButtonIndex: integer) of object;

  TAdvSmoothExpanderButtonClickEvent = procedure(Sender: TObject; ButtonIndex: integer) of object;

  TAdvSmoothExpanderButtonPos = record
    ButtonSet: Boolean;
  end;

  TAdvSmoothExpanderButtonPosArray = array of array of TAdvSmoothExpanderButtonPos;

  TAdvSmoothExpanderButtonPosList = array of TAdvSmoothExpanderButtonPosItem;

  TAdvSmoothExpanderButtonDisplayList = array of TAdvSmoothExpanderButton;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothExpanderButtonPanel = class(TAdvSmoothExpanderPanel, ITMSStyle)
  private
    FUpdateCount: Integer;
    FMetroStyle: Boolean;
    FFocused, FCalculateHeight: Boolean;
    FFocusedButtonIndex: integer;
    FDesignTime: Boolean;
    FCache: TGPBitmap;
    FValidCache: Boolean;
    FButtons: TAdvSmoothExpanderButtons;
    FButtonStatus: TAdvSmoothExpanderButtonStatus;
    FOnDrawStatus: TAdvSmoothExpanderButtonStatusDraw;
    FVerticalSpacing: integer;
    FHorizontalSpacing: integer;
    FButtonAppearance: TGDIPButton;
    FButtonHeight: integer;
    FButtonWidth: integer;
    FOnButtonClick: TAdvSmoothExpanderButtonClickEvent;
    FOnButtonClicked: TAdvSmoothExpanderButtonClickEvent;
    FVerticalMargin: integer;
    FButtonList: TAdvSmoothExpanderButtonPosList;
    FHorizontalMargin: integer;
    FAutoSize: TAdvSmoothExpanderButtonAutoSize;
    FAlternativeLayout: Boolean;
    FRows: integer;
    FColumns: integer;
    FDisplayList: TAdvSmoothExpanderButtonDisplayList;
    procedure SetButtons(const Value: TAdvSmoothExpanderButtons);
    procedure SetAlternativeLayout(const Value: Boolean);
    procedure SetButtonStatus(const Value: TAdvSmoothExpanderButtonStatus);
    procedure SetHorizontalSpacing(const Value: integer);
    procedure SetVerticalSpacing(const Value: integer);
    procedure SetButtonAppearance(const Value: TGDIPButton);
    procedure SetButtonHeight(const Value: integer);
    procedure SetButtonWidth(const Value: integer);
    procedure SetHorizontalMargin(const Value: integer);
    procedure SetVerticalMargin(const Value: integer);
    procedure SetAS(const Value: TAdvSmoothExpanderButtonAutoSize);
    procedure SetColumns(const Value: integer);
    procedure SetRows(const Value: integer);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Changed;
    procedure PanelChanged(Sender: TObject);
    procedure ButtonStatusChanged(Sender: TObject);
    procedure ButtonsChanged(Sender: TObject);
    procedure AutoSizeChanged(Sender: TObject);
    procedure GDIPPaint(g: TGPGraphics); override;
    function GetVersionNr: integer; override;
    function ButtonAtXY(X, Y: integer): integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function GetMaxButtons: Integer;
    function GetButtonHeight: Integer;
    function GetButtonWidth: Integer;
    procedure CalculateHeight;
    procedure CalculateButtonsPos;
    procedure CalculateButtons;
    procedure DoButtonClick(idx: integer); virtual;
    procedure DoButtonClicked(idx: integer); virtual;
    procedure AfterExpand; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    procedure Resize; override;
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  published
    property AutoSize: TAdvSmoothExpanderButtonAutoSize read FAutoSize write SetAS;
    property Buttons: TAdvSmoothExpanderButtons read FButtons write SetButtons;
    property ButtonStatus: TAdvSmoothExpanderButtonStatus read FButtonStatus write SetButtonStatus;
    property ButtonAppearance: TGDIPButton read FButtonAppearance write SetButtonAppearance;
    property ButtonVerticalMargin: integer read FVerticalMargin write SetVerticalMargin default 35;
    property ButtonHorizontalMargin: integer read FHorizontalMargin write SetHorizontalMargin default 5;
    property ButtonHorizontalSpacing: integer read FHorizontalSpacing write SetHorizontalSpacing default 5;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 75;
    property ButtonHeight: integer read FButtonHeight write SetButtonHeight default 30;
    property Columns: integer read FColumns write SetColumns default -1;
    property Rows: integer read FRows write SetRows default -1;
    property ButtonVerticalSpacing: integer read FVerticalSpacing write SetVerticalSpacing default 5;
    property OnDrawStatus: TAdvSmoothExpanderButtonStatusDraw read FOnDrawStatus write FOnDrawStatus;
    property OnButtonClick: TAdvSmoothExpanderButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnButtonClicked: TAdvSmoothExpanderButtonClickEvent read FOnButtonClicked write FOnButtonClicked;
    property AlternativeLayout: Boolean read FAlternativeLayout write SetAlternativeLayout default False;
  end;

implementation

uses
  CommCtrl, ShellApi;

{$i GDIPHTMLEngine.pas}

{ TAdvSmoothExpanderButtonPanel }

procedure TAdvSmoothExpanderButtonPanel.AfterExpand;
begin
  inherited;
  Changed;
end;

procedure TAdvSmoothExpanderButtonPanel.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothExpanderButtonPanel) then
  begin
    FAlternativeLayout := (Source as TAdvSmoothExpanderButtonPanel).AlternativeLayout;
    FButtons.Assign((Source as TAdvSmoothExpanderButtonPanel).Buttons);
    FButtonStatus.Assign((Source as TAdvSmoothExpanderButtonPanel).FButtonStatus);
    FButtonAppearance.Assign((Source as TAdvSmoothExpanderButtonPanel).ButtonAppearance);
    FHorizontalSpacing := (Source as TAdvSmoothExpanderButtonPanel).ButtonHorizontalSpacing;
    FVerticalSpacing := (Source as TAdvSmoothExpanderButtonPanel).ButtonVerticalSpacing;
    FButtonWidth := (Source as TAdvSmoothExpanderButtonPanel).ButtonWidth;
    FButtonHeight := (Source as TAdvSmoothExpanderButtonPanel).ButtonHeight;
    FAutoSize := (Source as TAdvSmoothExpanderButtonPanel).AutoSize;
    FVerticalMargin := (Source as TAdvSmoothExpanderButtonPanel).ButtonVerticalMargin;
    FHorizontalMargin := (Source as TAdvSmoothExpanderButtonPanel).ButtonHorizontalMargin;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.AutoSizeChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothExpanderButtonPanel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TAdvSmoothExpanderButtonPanel.ButtonAtXY(X, Y: integer): integer;
var
  I: Integer;
  r: TRect;
begin
  Result := -1;
  for I := 0 to Length(FDisplayList) - 1 do
  begin
    if Assigned(FDisplayList[I].ButtonPosition) then
    begin
      r := FDisplayList[I].ButtonPosition.R;

      if PtInGPRect(MakeRect(r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top), Point(X, Y)) then
      begin
        Result := FDisplayList[I].Index;
        Break;
      end;
    end;
  end;                                    
end;

procedure TAdvSmoothExpanderButtonPanel.ButtonsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothExpanderButtonPanel.ButtonStatusChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothExpanderButtonPanel.CalculateButtons;
var
  ButtonX, ButtonY, ButtonW, ButtonH: Integer;
  hs, vs, w, h: Integer;
  AButtonIndex: Integer;
  AButton: TAdvSmoothExpanderButton;
  exw, exh: Integer;
  I: Integer;
  Buttonpos: TAdvSmoothExpanderButtonPosItem;
begin
  if (csDestroying in ComponentState) or (csLoading in ComponentState) or (FUpdateCount <> 0) then
    Exit;

  if (Rows > 0) and (Columns > 0) and AlternativeLayout then
  begin
    SetLength(FDisplayList, 0);

    if (Buttons.Count = 0) or (Length(FButtonList) = 0) then
      Exit;

    hs := ButtonHorizontalSpacing;
    vs := ButtonVerticalSpacing;
    if ButtonWidth > 0 then
      ButtonW := ButtonWidth
    else
    begin
      w := Width - vs - ButtonHorizontalMargin;
      ButtonW := w;
      if Columns > 0 then
       ButtonW := (w - (Columns * hs)) div Columns;
    end;

    if ButtonHeight > 0 then
      ButtonH := ButtonHeight
    else
    begin
      h := Height - vs - ButtonVerticalMargin;
      ButtonH := h;
      if Rows > 0 then
       ButtonH := (h - (Rows * vs)) div Rows;
    end;

    for I := 0 to Length(FButtonList) - 1 do
    begin
      Buttonpos := FButtonList[I];

      if Assigned(Buttonpos) then
      begin
        AButtonIndex := Buttonpos.ButtonIdx;
        if (AButtonIndex >= 0) and (AButtonIndex <= Buttons.Count - 1) then
        begin
          AButton := Buttons[AButtonIndex];
          if AButton.Visible then
          begin
            exw := ButtonW * Buttonpos.ColSpan + (vs * (Buttonpos.ColSpan - 1));
            exh := ButtonH * Buttonpos.RowSpan + (hs * (Buttonpos.RowSpan - 1));
            ButtonX := ButtonHorizontalMargin + hs + ButtonW * Buttonpos.col + (hs * Buttonpos.col);
            ButtonY := ButtonVerticalMargin + vs + ButtonH * Buttonpos.row + (hs * Buttonpos.row);
            AButton.ButtonPosition := Buttonpos;
            Buttonpos.R := Rect(ButtonX, ButtonY, ButtonX + exw, ButtonY + exh);
            SetLength(FDisplayList, Length(FDisplayList) + 1);
            FDisplayList[Length(FDisplayList) - 1] := AButton;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.CalculateButtonsPos;
  procedure FindNewButtonPos(AButton: TAdvSmoothExpanderButton; var ANewR: Integer; var ANewC: Integer; var AFound: Boolean; ARows , ACurRow, AColumns, ACurCol: Integer; PosArr: TAdvSmoothExpanderButtonPosArray);
  var
    i, k: integer;
    cspan, rspan: Integer;
    J, L: Integer;
  begin

    AFound := False;
    for I := ANewr to ARows - 1 do
    begin
      for K := ANewC to AColumns - 1 do
      begin
        cspan := AButton.ColumnSpan;
        cspan := Min(cspan, Columns - ANewC);
        rspan := AButton.RowSpan;
        rspan := Min(rspan, Rows - ANewR);

        AFound := true;
        for J := 0 to rspan - 1 do
        begin
          for L := 0 to cspan - 1 do
          begin
            if PosArr[I + J, K + L].ButtonSet then
              AFound := False;
          end;
        end;

        if AFound then
          Break;
        Inc(ANewC);
      end;
      if AFound then
        Break;

      ANewC := 0;
      Inc(ANewr);
    end;
end;

var
  ButtonPosArr: TAdvSmoothExpanderButtonPosArray;
  r, newr, c, newc: Integer;
  AButtonIndex: Integer;
  AButton: TAdvSmoothExpanderButton;
  cspan, rspan, newcspan, newrspan: Integer;
  I: Integer;
  K: Integer;
  J: Integer;
  S: Integer;
  f: Boolean;
  Buttonpos: TAdvSmoothExpanderButtonPosItem;
  w, h: Integer;
  x, y: Integer;
  DPIScale: single;
begin
  if (csDestroying in ComponentState) or (csLoading in ComponentState) or (FUpdateCount <> 0) then
    Exit;

  SetLength(FDisplayList, 0);
  for I := 0 to Length(FButtonList) - 1 do
    FButtonList[I].Free;

  SetLength(FButtonList, 0);

  if Buttons.Count = 0 then
    Exit;

  DPIScale := GetDPIScale(Self, Canvas);
  w := Round(GetButtonWidth * DPIScale);
  h := Round(GetButtonHeight * DPIScale);
  J := 0;
  S := 0;
  AButtonIndex := 0;
  while AButtonIndex <= Buttons.Count - 1 do
  begin
    if (Rows > 0) and (Columns > 0) and AlternativeLayout then
    begin
      c := 0;
      r := 0;
      SetLength(ButtonPosArr, 0, 0);
      SetLength(ButtonPosArr, Rows, Columns);

      while r < Rows do
      begin
        while c < Columns do
        begin
          if (AButtonIndex >= 0) and (AButtonIndex <= Buttons.Count - 1) then
          begin
            AButton := Buttons[AButtonIndex];
            if not AButton.Visible then
            begin
              Inc(AButtonIndex);
              Continue;
            end;

            cspan := AButton.ColumnSpan;
            cspan := Min(cspan, Columns - c);
            rspan := AButton.RowSpan;
            rspan := Min(rspan, Rows - r);

            newcspan := c;
            newrspan := r;
            newcspan := newcspan + cspan - 1;
            newrspan := newrspan + rspan - 1;

            Buttonpos := TAdvSmoothExpanderButtonPosItem.Create;
            Buttonpos.ButtonIdx := AButtonIndex;
            Buttonpos.ButtonSet := True;
            Buttonpos.ColSpan := cspan;
            Buttonpos.RowSpan := rspan;
            Buttonpos.Row := r;
            Buttonpos.Col := c;
            AButton.ButtonPosition := Buttonpos;

            SetLength(FButtonList, Length(FButtonList) + 1);
            FButtonList[Length(FButtonList) - 1] := Buttonpos;

            for I := r to newrspan do
              for K := c to newcspan do
                ButtonPosArr[I, K].ButtonSet := True;
          end;
          Inc(AButtonIndex);
          Inc(c);
          newc := c;
          newr := r;
          if (AButtonIndex >= 0) and (AButtonIndex <= Buttons.Count - 1) then
            findnewButtonpos(Buttons[AButtonIndex], newr, newc, f, Rows, newr, Columns, newc, ButtonPosArr);
          c := newc;
          r := newr;

          if (c >= Columns) or (r >= Rows) then
            Break;
        end;
        c := 0;
        Inc(r);
        newc := c;
        newr := r;
        if (AButtonIndex >= 0) and (AButtonIndex <= Buttons.Count - 1) then
          findnewButtonpos(Buttons[AButtonIndex], newr, newc, f, Rows, newr, Columns, newc, ButtonPosArr);
        c := newc;
        r := newr;
        if r >= Rows then
          Break;
      end;
    end
    else
    begin
      if (AButtonIndex >= 0) and (AButtonIndex <= Buttons.Count - 1) then
      begin
        AButton := Buttons[AButtonIndex];
        if not AButton.Visible then
        begin
          Inc(AButtonIndex);
          Continue;
        end;

        x := Round(ButtonHorizontalMargin * DPIScale) + S * w;
        y := Round(ButtonVerticalMargin * DPIScale) + J * h;
        if x + w > InsideRect.Right then
        begin
          S := 0;
          Inc(J);
          x := Round(ButtonHorizontalMargin * DPIScale) + S * w;
          y := Round(ButtonVerticalMargin * DPIScale) + J * h;
          Inc(S);
        end
        else
          Inc(S);

        Buttonpos := TAdvSmoothExpanderButtonPosItem.Create;
        Buttonpos.ButtonIdx := AButtonIndex;
        Buttonpos.R := Rect(x, y, x + w, y + h);
        AButton.ButtonPosition := Buttonpos;
        SetLength(FButtonList, Length(FButtonList) + 1);
        FButtonList[Length(FButtonList) - 1] := Buttonpos;
        SetLength(FDisplayList, Length(FDisplayList) + 1);
        FDisplayList[Length(FDisplayList) - 1] := AButton;
      end;
      Inc(AButtonIndex);      
    end;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.CalculateHeight;
var
  ah: Integer;
  aw: Integer;
begin
  if not AutoSize.DecreaseHeight and not AutoSize.IncreaseHeight or
    (csDestroying in ComponentState) or (csLoading in ComponentState) or (FUpdateCount <> 0) then
      Exit;

  FCalculateHeight := false;
  ah := 0;
  if Length(FDisplayList) > 0 then
  begin
    ah := FDisplayList[Length(FDisplayList) - 1].ButtonPosition.R.Bottom + 5;
    if (Fill.ShadowColor <> clNone) then
      ah := ah + Fill.ShadowOffset;
  end;

  aw := 0;
  if Length(FDisplayList) > 0 then
  begin
    aw := FDisplayList[Length(FDisplayList) - 1].ButtonPosition.R.Right + 5;
    if (Fill.ShadowColor <> clNone) then
      aw := aw + Fill.ShadowOffset;
  end;

  if AutoSize.IncreaseHeight and Expanded and (ah > Height) then
  begin
    Height := ah;
    ExpandedHeight := Height;
  end
  else if AutoSize.DecreaseHeight and (ah < Height) then
  begin
    Height := ah;
    ExpandedHeight := Height;
  end;

  if AutoSize.IncreaseWidth and (aw > Width) then
    Width := aw
  else if AutoSize.DecreaseWidth and (aw < Width) then
    Width := aw;

  FCalculateHeight := true;
end;

procedure TAdvSmoothExpanderButtonPanel.Changed;
begin
  inherited;
  FValidCache := false;
  CalculateButtonsPos;
  CalculateButtons;
  CalculateHeight;
  Invalidate;
end;

constructor TAdvSmoothExpanderButtonPanel.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := true;
  FAlternativeLayout := False;
  FAutoSize := TAdvSmoothExpanderButtonAutoSize.Create(Self);
  FAutoSize.OnChange := AutoSizeChanged;
  FButtons := TAdvSmoothExpanderButtons.Create(Self);
  Fbuttons.OnChange := ButtonsChanged;
  FButtonStatus := TAdvSmoothExpanderButtonStatus.Create(Self);
  FButtonStatus.OnChange := ButtonStatusChanged;
  FButtonAppearance := TGDIPButton.Create;
  FButtonAppearance.OnChange := ButtonsChanged;
  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  FButtonWidth := 75;
  FButtonHeight := 30;
  FVerticalMargin := 35;
  FHorizontalMargin := 5;
  FCache := TGPBitmap.Create(Width, Height - ButtonVerticalMargin);
  FFocusedButtonIndex := 0;
  FCalculateHeight := true;
  FVerticalSpacing := 5;
  FHorizontalSpacing := 5;
  FColumns := -1;
  FRows := -1;
  Width := 286;
  Height := 128;  
end;

destructor TAdvSmoothExpanderButtonPanel.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FButtonList) - 1 do
    FButtonList[I].Free;

  FAutoSize.Free;
  Fbuttons.Free;
  Fbuttonstatus.Free;
  FButtonAppearance.Free;
  if FCache <> nil then
    FCache.Free;
  inherited;
end;

procedure TAdvSmoothExpanderButtonPanel.DoButtonClick(idx: integer);
begin
  if Assigned(OnButtonClick) then
    OnButtonClick(Self, idx);
end;

procedure TAdvSmoothExpanderButtonPanel.DoButtonClicked(idx: integer);
begin
  if Assigned(OnButtonClicked) then
    OnButtonClicked(Self, idx);
end;

procedure TAdvSmoothExpanderButtonPanel.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothExpanderButtonPanel.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothExpanderButtonPanel.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then  
    Changed;
end;

procedure DrawFocus(g: TGPGraphics; r: TGPRectF; rn: Integer);
var
  pathfocus: TGPGraphicsPath;
  pfocus: TGPPen;
begin
  pathfocus := GDIPFill.CreateRoundRectangle(r, rn, rtBoth, false);
  g.SetSmoothingMode(SmoothingModeDefault);
  pfocus := TGPPen.Create(MakeColor(255, clBlack), 1);
  pfocus.SetDashStyle(DashStyleDot);
  g.DrawPath(pfocus, pathfocus);
  pfocus.Free;
  pathfocus.Free;
  g.SetSmoothingMode(SmoothingModeAntiAlias);
end;

procedure TAdvSmoothExpanderButtonPanel.GDIPPaint(g: TGPGraphics);
var
  gp: TGPGraphics;
  i: integer;
  f: TGDIPFill;
  ft: TFont;
  c: TColor;
  br: TRect;
  rOut: TRect;
  r: TRect;
  rtType: TFillRoundingType;
  hs, vs: Integer;
  sx, sy: Integer;
  DPIScale: single;
begin
  inherited;
  SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);
  if not FValidCache then
  begin

    DPIScale := GetDPIScale(Self, Canvas);
    gp := TGPGraphics.Create(FCache);
    gp.SetSmoothingMode(SmoothingModeAntiAlias);
    gp.SetTextRenderingHint(TextRenderingHintAntiAlias);
    gp.Clear(MakeColor(0, clWhite));
    f := TGDIPFill.Create;
    ft := TFont.Create;

    hs := ButtonHorizontalSpacing;
    vs := ButtonVerticalSpacing;

    for I := 0 to Length(FDisplayList) - 1 do
    begin
      f.Assign(ButtonStatus.Appearance.Fill);
      ft.Assign(Buttonstatus.Appearance.Font);

      with FDisplayList[I] do
      begin
        if Assigned(ButtonPosition) then
        begin
          br := ButtonPosition.R;

          if IntersectRect(rOut, ClientRect, br) and (br.Right < r.Right) and (br.Bottom < r.Bottom) then
          begin
            FButtonAppearance.BeginUpdate;
            c := FButtonAppearance.Font.Color;
            if TextColor <> clNone then
              FButtonAppearance.Font.Color := TextColor;

            if FMetroStyle then
              rtType := rtNone
            else
              rtType := rtBoth;

            FButtonAppearance.Rounding := Round(FButtonAppearance.Rounding * DPIScale);
            FButtonAppearance.Font.Height := Round(FButtonAppearance.Font.Height * DPIScale);
            if Enabled then
            begin

              FButtonAppearance.Draw(gp, Caption, br.Left, br.Top, br.Right - br.Left, br.Bottom - br.Top, vs, hs, Color, clNone, BevelColor, FButtonAppearance.Font.Color,
                false, FDown, Bevel, false, false, rtType, Picture, 0, 0, true, ImageIndex, PictureName);
            end
            else
            begin
              FButtonAppearance.Draw(gp, Caption, br.Left, br.Top, br.Right - br.Left, br.Bottom - br.Top, vs, hs, ColorDisabled, clNone, BevelColor, FButtonAppearance.Font.Color,
                false, FDown, Bevel, false, false, rtType, Picture, 0, 0, true, DisabledImageIndex, DisabledPictureName);
            end;
            FButtonAppearance.Rounding := Round(FButtonAppearance.Rounding / DPIScale);
            FButtonAppearance.Font.Height := Round(FButtonAppearance.Font.Height / DPIScale);

            if TextColor <> clNone then
              FbuttonAppearance.Font.Color := c;
            FButtonAppearance.EndUpdate;

            if TabStop and (FFocusedButtonIndex = I) and FFocused then
              DrawFocus(gp, MakeRect(br.Left + hs,br.Top +vs, (br.Right - br.Left) - (hs * 2), (br.Bottom - br.Top) - (vs * 2)), 8);

            if ButtonStatus.Visible and ((StatusCaption <> '') or not ButtonStatus.Appearance.Fill.Picture.Empty) then
            begin
              with ButtonStatus do
              begin
                Appearance.Font.Height := Round(DPIScale * Appearance.Font.Height);
                if Assigned(FOnDrawStatus) then
                  FOnDrawStatus(Self, ButtonStatus.Appearance.Fill, ButtonStatus.Appearance.Font, i);

                Appearance.CalculateSize(gp, StatusCaption);
                sx := br.Right + Round(DPIScale * FButtonStatus.OffsetLeft) - ButtonStatus.Appearance.GetWidth;
                sy := br.Top + Round(DPIScale * ButtonStatus.OffsetTop);

                Appearance.Draw(gp, ButtonStatus.OffsetLeft + sx, sy, 0, 0, true, StatusCaption);
                Appearance.Font.Height := Round(Appearance.Font.Height / DPIScale);
              end;
            end;
          end;
        end;
      end;
    end;
    f.Free;
    ft.Free;
    gp.Free;
    FValidCache := true;
  end;

  if FValidCache then
  begin
    g.DrawImage(FCache, 0, 0);
  end;
end;

function TAdvSmoothExpanderButtonPanel.GetButtonHeight: integer;
begin
  if Rows = -1 then
    Result := ButtonHeight
  else
    Result := (InsideRect.Bottom - ButtonVerticalMargin ) div Rows;
end;

function TAdvSmoothExpanderButtonPanel.GetButtonWidth: integer;
begin
  if Columns = -1 then
    Result := ButtonWidth
  else
    Result := (InsideRect.Right - ButtonHorizontalMargin * 2) div Columns;
end;

function TAdvSmoothExpanderButtonPanel.GetMaxButtons: integer;
begin
  Result := (InsideRect.Right - ButtonHorizontalMargin) div GetButtonWidth;
end;

function TAdvSmoothExpanderButtonPanel.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothExpanderButtonPanel.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  tempfi, fi: integer;
begin
  inherited;
  fi := FFocusedButtonIndex;
  tempfi := fi;
  case Key of
  VK_UP: tempfi := tempfi - GetMaxButtons;
  VK_Left: Dec(tempfi);
  VK_RIGHT: Inc(tempfi);
  VK_DOWN: tempfi := tempfi + GetMaxButtons;
  VK_PRIOR: tempfi := tempfi - 5;
  VK_NEXT: tempfi := tempfi + 5;
  VK_HOME: tempfi := 0;
  VK_END: tempfi := Buttons.Count - 1;
  VK_SPACE:
  begin
    if (FFocusedButtonIndex >= 0) and (FFocusedButtonIndex <= Buttons.Count - 1) then
    begin
      with Buttons[FFocusedButtonIndex] do
      begin
        FDown := true;
        Changed;
        if Enabled then
          DoButtonClick(FFocusedButtonIndex);
      end;
    end;
  end;
  end;
  if (tempfi >= 0) and (tempfi <= Buttons.Count - 1) then
  begin
    FFocusedButtonIndex := tempfi;
    if (fi <> FFocusedButtonIndex) and FFocused then
      Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.KeyUp(var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key = VK_SPACE then
  begin
    if (FFocusedButtonIndex >= 0) and (FFocusedButtonIndex <= Buttons.Count - 1) then
    begin
      with Buttons[FFocusedButtonIndex] do
      begin
        FDown := false;
        Changed;
        if Enabled then
          DoButtonClicked(FFocusedButtonIndex);
      end;
    end;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.Loaded;
begin
  inherited;
  if Assigned(FCache)  then
    FCache.Free;
  FCache := TGPBitmap.Create(Width, Height - ButtonVerticalMargin);
  FValidCache := false;
end;

procedure TAdvSmoothExpanderButtonPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  btnidx: integer;
begin
  inherited;
//  SetFocus;
  btnidx := ButtonAtXY(X, Y);
  if (btnidx <> -1) and (Buttons[btnidx].Enabled) then
  begin
    FFocusedButtonIndex := btnidx;
    Buttons[btnidx].Fdown := true;
    FValidCache := false;
    Changed;
    DoButtonClick(btnidx);
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

procedure TAdvSmoothExpanderButtonPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  btndown: Boolean;
  btnidx: integer;
begin
  inherited;

  btnidx := ButtonAtXY(X, Y);
  if (btnidx <> -1) and (Buttons[btnidx].Enabled) then
    DoButtonClicked(btnidx);

  btndown := false;
  for I := 0 to buttons.Count - 1 do
  begin
    if Buttons[I].Fdown then
    begin
      btndown := true;
      Break;
    end;
  end;

  if btndown then
  begin
    for I := 0 to buttons.Count - 1 do
      Buttons[I].Fdown := false;

    FValidCache := false;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if Assigned(FButtonAppearance) then
    FButtonAppearance.DoNotification(Self, AComponent, AOperation);
  inherited;
end;

procedure TAdvSmoothExpanderButtonPanel.PanelChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothExpanderButtonPanel.Resize;
var
  h: integer;
  sc: Single;
begin
  inherited;

  if IsResizeEnabled then
  begin
    CalculateButtonsPos;
    CalculateButtons;
    if FCalculateHeight then
      CalculateHeight;

    h := Height;
    if Assigned(FCache) then
    begin
      h := FCache.Height;
      FCache.Free;
    end;

    sc := GetDPIScale(Self, Canvas);

    if Expanded then
    begin
      FCache := TGPBitmap.Create(Round(Width * sc), Round((Height - 2) * sc))
    end
    else
      FCache := TGPBitmap.Create(Round(Width * sc), Round(h * sc));

    FValidCache := false;
    Invalidate;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetAS(
  const Value: TAdvSmoothExpanderButtonAutoSize);
begin
  if FAutoSize <> value then
  begin
    FAutoSize.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetButtonAppearance(
  const Value: TGDIPButton);
begin
  if FButtonAppearance <> Value then
  begin
    FButtonAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetButtonHeight(const Value: integer);
begin
  if FButtonHeight <> value then
  begin
    FButtonHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetButtons(
  const Value: TAdvSmoothExpanderButtons);
begin
  if FButtons <> value then
  begin
    FButtons.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetAlternativeLayout(const Value: Boolean);
begin
  if FAlternativeLayout <> Value then
  begin
    FAlternativeLayout := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetButtonStatus(
  const Value: TAdvSmoothExpanderButtonStatus);
begin
  if FButtonStatus <> value then
  begin
    FButtonStatus.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetButtonWidth(const Value: integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetColorTones(ATones: TColorTones);
begin
  inherited;
  FMetroStyle := True;
  ButtonAppearance.SimpleLayout := True;
end;

procedure TAdvSmoothExpanderButtonPanel.SetColumns(const Value: integer);
begin
  if (FColumns <> value) and (Value = -1) or (Value > 0) then
  begin
    FColumns := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetComponentStyle(AStyle: TTMSStyle);
var
  i: integer;
begin
  inherited;
  FMetroStyle := False;
  ButtonAppearance.SimpleLayout := False;
  for I := 0 to Buttons.Count - 1 do
  begin
    with Buttons[I] do
    begin
      // TODO : do color settings here
      case astyle of
        tsOffice2003Blue:
          Color := $00E3B28D;
        tsOffice2003Silver:
          Color := $00927476;
        tsOffice2003Olive:
          Color := $447A63; //08CC0B1; 006B7760;
        tsOffice2003Classic:
          Color := $00C9D1D5;
        tsOffice2007Luna:
          Color := $00FDEADA;
        tsOffice2007Obsidian:
          Color := $006E6E6D;
        tsWindowsXP:
          Color := $B9D8DC;
        tsWhidbey:
          Color := $00828F92;
        tsCustom: ;
        tsOffice2007Silver:
          Color := $00E7DCD5;
        tsWindowsVista:
          Color := $FDF8F1;
        tsWindows7:
          Color := $FCEBDC;
        tsTerminal:
          Color := clBtnFace;
        tsOffice2010Blue:
          Color := $F0DAC7;
        tsOffice2010Silver:
          Color := $EDE5E0;
        tsOffice2010Black:
          Color := $919191;
      end;
    end;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetHorizontalMargin(
  const Value: integer);
begin
  if FHorizontalMargin <> value then
  begin
    FHorizontalMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetHorizontalSpacing(
  const Value: integer);
begin
  if FHorizontalSpacing <> value then
  begin
    FHorizontalSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetRows(const Value: integer);
begin
  if (FRows <> value) and (Value = -1) or (Value > 0) then
  begin
    FRows := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetVerticalMargin(const Value: integer);
begin
  if FVerticalMargin <> value then
  begin
    FVerticalMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonPanel.SetVerticalSpacing(
  const Value: integer);
begin
  if FVerticalSpacing <> value then
  begin
    FVerticalSpacing := Value;
    Changed;
  end;
end;

{ TAdvSmoothExpanderButtons }

function TAdvSmoothExpanderButtons.Add: TAdvSmoothExpanderButton;
begin
  Result := TAdvSmoothExpanderButton(inherited Add);
end;

constructor TAdvSmoothExpanderButtons.Create(AOwner: TAdvSmoothExpanderButtonPanel);
begin
  inherited Create(TAdvSmoothExpanderButton);
  FOwner := AOwner;
end;

procedure TAdvSmoothExpanderButtons.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothExpanderButtons.GetItem(
  Index: Integer): TAdvSmoothExpanderButton;
begin
  Result := TAdvSmoothExpanderButton(inherited Items[Index]);
end;

function TAdvSmoothExpanderButtons.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothExpanderButtons.Insert(
  Index: Integer): TAdvSmoothExpanderButton;
begin
  Result := TAdvSmoothExpanderButton(inherited Insert(Index));
end;

procedure TAdvSmoothExpanderButtons.SetItem(Index: Integer;
  const Value: TAdvSmoothExpanderButton);
begin
  inherited Items[Index] := value;
end;

{ TAdvSmoothExpanderButton }

procedure TAdvSmoothExpanderButton.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothExpanderButton) then
  begin
    FRowSpan := (Source as TAdvSmoothExpanderButton).RowSpan;
    FColumnSpan := (Source as TAdvSmoothExpanderButton).ColumnSpan;
    FEnabled := (Source as TAdvSmoothExpanderButton).Enabled;
    FVisible := (Source as TAdvSmoothExpanderButton).Visible;
    FPicture.Assign((Source as TAdvSmoothExpanderButton).Picture);
    FBevel := (Source as TAdvSmoothExpanderButton).Bevel;
    FBevelColor := (Source as TAdvSmoothExpanderButton).BevelColor;
    FColor := (Source as TAdvSmoothExpanderButton).Color;
    FBevelColor := (Source as TAdvSmoothExpanderButton).BevelColor;
    FCaption := (Source as TAdvSmoothExpanderButton).Caption;
    FTextColor := (Source as TAdvSmoothExpanderButton).TextColor;
    FTag := (Source as TAdvSmoothExpanderButton).Tag;
    FColorDisabled := (Source as TAdvSmoothExpanderButton).ColorDisabled;
    FDisabledImageIndex := (Source as TAdvSmoothExpanderButton).DisabledImageIndex;
    FDisabledPictureName := (Source as TAdvSmoothExpanderButton).DisabledPictureName;
    FImageIndex := (Source as TAdvSmoothExpanderButton).ImageIndex;
    FPictureName := (Source as TAdvSmoothExpanderButton).PictureName;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothExpanderButton.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothExpanderButtons).FOwner;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FColor := clSilver;
  FBevel := true;
  FBevelColor := clWhite;
  FEnabled := True;
  FVisible := True;
  FTextColor := clBlack;
  FColorDisabled := clGray;
  FTag := 0;
  FColumnSpan := 1;
  FRowSpan := 1;
  FImageIndex := -1;
  FDisabledImageIndex := -1;
  Fowner.Changed;
end;

destructor TAdvSmoothExpanderButton.Destroy;
begin
  FButtonPosition := nil;
  FPicture.Free;
  Fowner.Changed;
  inherited;
end;

procedure TAdvSmoothExpanderButton.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothExpanderButton.SetBevel(const Value: boolean);
begin
  if FBevel <> value then
  begin
    FBevel := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetBevelColor(const Value: TColor);
begin
  if FBevelColor <> Value then
  begin
    FBevelColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetColorDisabled(const Value: TColor);
begin
  if FColorDisabled <> Value then
  begin
    FColorDisabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetColumnSpan(const Value: Integer);
begin
  if FColumnSpan <> Value then
  begin
    FColumnSpan := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetDisabledImageIndex(const Value: Integer);
begin
  if FDisabledImageIndex <> Value then
  begin
    FDisabledImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetDisabledPictureName(const Value: String);
begin
  if FDisabledPictureName <> Value then
  begin
    FDisabledPictureName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetPicture(const Value: TAdvGDIPPicture);
begin
  if FPicture <> value then
  begin
    FPicture.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetPictureName(const Value: String);
begin
  if FPictureName <> Value then
  begin
    FPictureName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetRowSpan(const Value: Integer);
begin
  if FRowSpan <> Value then
  begin
    FRowSpan := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetStatusCaption(const Value: String);
begin
  if FStatusCaption <> value then
  begin
    FStatusCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetTag(const Value: Integer);
begin
  if FTag <> Value then
  begin
    FTag := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetTextColor(const Value: TColor);
begin
  if FTextColor <> value then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButton.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TAdvSmoothExpanderButtonStatus }

procedure TAdvSmoothExpanderButtonStatus.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothExpanderButtonStatus.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothExpanderButtonStatus) then
  begin
    FAppearance.Assign((Source as TAdvSmoothExpanderButtonStatus).Appearance);
    FOffsetTop := (Source as TAdvSmoothExpanderButtonStatus).OffsetTop;
    FOffsetLeft := (Source as TAdvSmoothExpanderButtonStatus).OffsetLeft;
    FVisible := (Source as TAdvSmoothExpanderButtonStatus).Visible;
  end;
end;

procedure TAdvSmoothExpanderButtonStatus.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothExpanderButtonStatus.Create(AOwner: TAdvSmoothExpanderButtonPanel);
begin
  FOwner := AOwner;
  FOffsetTop := 0;
  FOffsetLeft := 0;
  FVisible := False;
  FAppearance := TGDIPStatus.Create;
  FAppearance.OnChange := AppearanceChanged;
  if FOwner.FDesigntime then
  begin
    FAppearance.Fill.Color := clRed;
    FAppearance.Fill.GradientType := gtSolid;
    FAppearance.Fill.BorderColor := clGray;
    FAppearance.Font.Color := clWhite;
  end;
end;

destructor TAdvSmoothExpanderButtonStatus.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

procedure TAdvSmoothExpanderButtonStatus.SetAppearance(const Value: TGDIPStatus);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    AppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothExpanderButtonStatus.SetOffsetLeft(const Value: integer);
begin
  if FOffsetLeft <> value then
  begin
    FOffsetLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonStatus.SetOffsetTop(const Value: integer);
begin
  if FOffsetTop <> value then
  begin
    FOffsetTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonStatus.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TAdvSmoothExpanderButtonAutoSize }

procedure TAdvSmoothExpanderButtonAutoSize.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothExpanderButtonAutoSize) then
  begin
    FIncreaseHeight := (Source as TAdvSmoothExpanderButtonAutoSize).IncreaseHeight;
    FDecreaseHeight := (Source as TAdvSmoothExpanderButtonAutoSize).DecreaseHeight;
    FIncreaseWidth := (Source as TAdvSmoothExpanderButtonAutoSize).IncreaseWidth;
    FDecreaseWidth := (Source as TAdvSmoothExpanderButtonAutoSize).DecreaseWidth;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonAutoSize.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothExpanderButtonAutoSize.Create(
  AOwner: TAdvSmoothExpanderButtonPanel);
begin
  FOwner := AOwner;
  FDecreaseHeight := False;
  FIncreaseHeight := True;
  FDecreaseWidth := False;
  FIncreaseWidth := False;
end;

destructor TAdvSmoothExpanderButtonAutoSize.Destroy;
begin
  inherited;
end;

procedure TAdvSmoothExpanderButtonAutoSize.SetDecreaseHeight(
  const Value: Boolean);
begin
  if FDecreaseHeight <> value then
  begin
    FDecreaseHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonAutoSize.SetDecreaseWidth(
  const Value: Boolean);
begin
  if FDecreaseWidth <> Value then
  begin
    FDecreaseWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonAutoSize.SetIncreaseHeight(
  const Value: Boolean);
begin
  if FIncreaseHeight <> value then
  begin
    FIncreaseHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderButtonAutoSize.SetIncreaseWidth(
  const Value: Boolean);
begin
  if FIncreaseWidth <> Value then
  begin
    FIncreaseWidth := Value;
    Changed;
  end;
end;

end.
