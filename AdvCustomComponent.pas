{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2016                                        }
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

unit AdvCustomComponent;

{$I TMSDEFS.INC}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}

interface

uses
  Classes, Controls, AdvTypes
  {$IFNDEF WEBLIB}
  ,AdvPersistence
  {$ENDIF}
  ,TypInfo;

type
  {$IFDEF CMNWEBLIB}
  TAdvCustomComponent = class(TCustomControl, IAdvProductInfo{$IFNDEF WEBLIB}, IAdvPersistence{$ENDIF})
  private
    FStored: Boolean;
  {$ELSE}
  TAdvCustomComponent = class(TControl, IAdvProductInfo, IAdvPersistence)
  {$ENDIF}
  private
    FAdaptToStyle: Boolean;
  protected
    {$IFDEF WEBLIB}
    class function GetVersionNumber(AMaj, AMin, ARel, ABld: ShortInt): string;
    {$ENDIF}
    function CanSaveProperty({%H-}AObject: TObject; {%H-}APropertyName: string; {%H-}APropertyType: TTypeKind): Boolean; virtual;
    function CanLoadProperty({%H-}AObject: TObject; {%H-}APropertyName: string; {%H-}APropertyType: TTypeKind): Boolean; virtual;
    function GetVersion: string; virtual;
    function GetDocURL: string; virtual;
    function GetTipsURL: string; virtual;
    function GetInstance: NativeUInt; virtual;
    procedure SetAdaptToStyle(const Value: Boolean); virtual;
    procedure Paint; override;
    procedure Loaded; override;
    procedure RegisterRuntimeClasses; virtual;
    property AdaptToStyle: Boolean read FAdaptToStyle write SetAdaptToStyle default False;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFDEF FMXLIB}
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure SetBounds(X, Y, {%H-}AWidth, {%H-}AHeight: Integer); override;
    property Stored: Boolean read FStored write FStored;
    {$ENDIF}
    function IsDesignTime: Boolean; virtual;
    function IsLoading: Boolean; virtual;
    function IsReading: Boolean; virtual;
    function IsDesigning: Boolean; virtual;
    function IsDestroying: Boolean; virtual;
    {$IFNDEF WEBLIB}
    procedure SaveSettingsToFile(AFileName: string); virtual;
    procedure LoadSettingsFromFile(AFileName: string); virtual;
    procedure SaveSettingsToStream(AStream: TStream); virtual;
    procedure LoadSettingsFromStream(AStream: TStream); virtual;
    {$ENDIF}
  published
    {$IFDEF FMXLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION > 27}
    property Size;
    {$IFEND}
    {$HINTS ON}
    property Position;
    {$ENDIF}
    property Visible {$IFDEF CMNLIB}default False{$ENDIF};
    property Width;
    property Height;
  end;

  TAdvCustomComponentClass = class of TAdvCustomComponent;

implementation

uses
  AdvUtils, SysUtils, AdvGraphics,
  Graphics, AdvGraphicsTypes
  {$IFNDEF LCLLIB}
  ,Types
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,PngImage
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf
  {$ENDIF}
  ;

{ TAdvCustomComponent }

{$IFDEF WEBLIB}
class function TAdvCustomComponent.GetVersionNumber(AMaj, AMin, ARel, ABld: ShortInt): string;
begin
  Result := '';
end;
{$ENDIF}

function TAdvCustomComponent.CanSaveProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
begin
  {$IFNDEF WEBLIB}
  if AObject = Self then
    Result := (TAdvUtils.IndexOfTextInArray(APropertyName, ExcludePropertyList) = -1)
  else
  {$ENDIF}
    Result := True;
end;

function TAdvCustomComponent.CanLoadProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
begin
  {$IFNDEF WEBLIB}
  if AObject = Self then
    Result := (TAdvUtils.IndexOfTextInArray(APropertyName, ExcludePropertyList) = -1)
  else
  {$ENDIF}
    Result := True;
end;

constructor TAdvCustomComponent.Create(AOwner: TComponent);
begin
  inherited;
  Width := 26;
  Height := 26;

  if not IsDesigning then
    RegisterRuntimeClasses;
end;

function TAdvCustomComponent.GetDocURL: string;
begin
  Result := TAdvBaseDocURL;
end;

function TAdvCustomComponent.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TAdvCustomComponent.GetTipsURL: string;
begin
  Result := TAdvBaseTipsURL;
end;

function TAdvCustomComponent.GetVersion: string;
begin
  Result := '';
end;

function TAdvCustomComponent.IsDesignTime: Boolean;
begin
  Result := (csDesigning in ComponentState) and not ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

function TAdvCustomComponent.IsReading: Boolean;
begin
  Result := (csReading in Owner.ComponentState);
end;

function TAdvCustomComponent.IsDesigning: Boolean;
begin
  Result := (csDesigning in ComponentState);
end;

function TAdvCustomComponent.IsDestroying: Boolean;
begin
  Result := (csDestroying in ComponentState);
end;

function TAdvCustomComponent.IsLoading: Boolean;
begin
  Result := (csLoading in Owner.ComponentState);
end;

procedure TAdvCustomComponent.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Visible := False;
end;

{$IFNDEF WEBLIB}
procedure TAdvCustomComponent.SaveSettingsToFile(AFileName: string);
begin
  TAdvPersistence.SaveSettingsToFile(Self, AFileName);
end;

procedure TAdvCustomComponent.LoadSettingsFromFile(AFileName: string);
begin
  TAdvPersistence.LoadSettingsFromFile(Self, AFileName);
end;

procedure TAdvCustomComponent.SaveSettingsToStream(AStream: TStream);
begin
  TAdvPersistence.SaveSettingsToStream(Self, AStream);
end;

procedure TAdvCustomComponent.LoadSettingsFromStream(AStream: TStream);
begin
  TAdvPersistence.LoadSettingsFromStream(Self, AStream);
end;
{$ENDIF}

procedure TAdvCustomComponent.Paint;
var
  {$IFDEF VCLLIB}
  png: TPngImage;
  {$ENDIF}
  pic: TAdvBitmap;
  g: TAdvGraphics;
  r: TResourceStream;
  {$IFDEF WEBLIB}
  rc: TRegisteredComponent;
  {$ENDIF}
begin
  inherited;
  r := nil;
  pic := TAdvBitmap.Create;
  {$IFDEF VCLLIB}
  png := TPNGImage.Create;
  {$ENDIF}
  g := TAdvGraphics.Create(Canvas);
  try
    g.Stroke.Kind := gskSolid;
    g.Stroke.Color := gcBlack;
    g.DrawRectangle(RectF(0, 0, Width, Height));

    {$IFNDEF WEBLIB}
    r := TAdvUtils.GetResourceStream(UpperCase(ClassName), GetInstance);
    if Assigned(r) then
    begin
      {$IFDEF VCLLIB}
      png.LoadFromStream(r);
      pic.Assign(png);
      {$ELSE}
      pic.LoadFromStream(r);
      {$ENDIF}
    end;
    {$ENDIF}

    {$IFDEF WEBLIB}
    rc := GetRegisteredComponent(ClassName);
    if Assigned(rc) then
      pic.LoadFromResource(rc.Icon);
    {$ENDIF}

    g.DrawBitmap(RectF(0, 0, Width, Height), BitmapToDrawBitmap(pic));
  finally
    if Assigned(r) then
      r.Free;

    pic.Free;
    {$IFDEF VCLLIB}
    png.Free;
    {$ENDIF}
    g.Free;
  end;
end;

procedure TAdvCustomComponent.RegisterRuntimeClasses;
begin
end;

procedure TAdvCustomComponent.SetAdaptToStyle(const Value: Boolean);
begin
  FAdaptToStyle := Value;
end;

{$IFDEF FMXLIB}
procedure TAdvCustomComponent.SetBounds(X, Y, AWidth, AHeight: Single);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TAdvCustomComponent.SetBounds(X, Y, AWidth, AHeight: Integer);
{$ENDIF}
begin
  inherited SetBounds(X, Y, 26, 26);
end;

end.
