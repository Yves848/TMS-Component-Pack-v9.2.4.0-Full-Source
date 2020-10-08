{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2017                                        }
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

unit AdvPersistence;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LCLLIB}
  fgl,
  {$IFNDEF MSWINDOWS}
  LCLIntF,
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  Generics.Collections,
  {$ENDIF}
  {$IFDEF WEBLIB}
  Contnrs,
  {$ENDIF}
  Classes, TypInfo, Variants, SysUtils,
  AdvTypes,
  AdvJSONReader,
  AdvJSONWriter;

type
  TStreamEx = TStream;

  IAdvPersistence = interface
  ['{363F04AF-B8A7-4C47-A2D6-8ED9C44CEFF6}']
    procedure SaveSettingsToFile(AFileName: string);
    procedure LoadSettingsFromFile(AFileName: string);
    procedure SaveSettingsToStream(AStream: TStreamEx);
    procedure LoadSettingsFromStream(AStream: TStreamEx);
    function CanSaveProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
    function CanLoadProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
  end;

  EAdvReaderException = class(Exception)
  end;

  {$IFDEF WEBLIB}
  TAdvObjectList = class(TObjectList)
  private
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; const Value: TObject);
  public
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  TAdvPropertyInfo = TTypeMemberProperty;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvObjectList = class(TObjectList<TObject>);
  TAdvPropertyInfo = PPropInfo;
  {$ENDIF}

  TAdvObjectArray = array of TObject;

  TAdvWriter = class
  private
    FWriter: TAdvJSONWriter;
    property Writer: TAdvJSONWriter read FWriter;
    procedure WritePropInfoValue(AInstance: TObject; const APropInfo: TAdvPropertyInfo);
    procedure WriteProperties(AObject: TObject);
    procedure WriteProperty(AObject: TObject; AProp: TAdvPropertyInfo);
    procedure WriteGenericList(AList: TAdvObjectList);
    procedure WriteCollection(ACollection: TCollection);
    {$IFDEF LCLLIB}
    procedure WriteList(AList: TList);
    {$ENDIF}
    procedure WriteBitmap(ABitmap: TAdvBitmap);
    procedure WriteSingleObject(AObject: TObject);
    procedure WriteObject(AObject: TObject);
  public
    constructor Create(AStream: TStreamEx);
    destructor Destroy; override;
    procedure Write(AObject: TObject);
  end;

  TAdvReader = class
  private
    type
      TAdvObjectReference = class
      public
        Instance: TObject;
        Prop: TAdvPropertyInfo;
        Id: string;
        constructor Create(AInstance: TObject; AProp: TAdvPropertyInfo; const AId: string);
      end;

      {$IFDEF WEBLIB}
      TAdvObjectReferences = class(TObjectList)
      private
        function GetItem(Index: Integer): TAdvObjectReference;
        procedure SetItem(Index: Integer; const Value: TAdvObjectReference);
      public
        property Items[Index: Integer]: TAdvObjectReference read GetItem write SetItem; default;
      end;
      {$ENDIF}
      {$IFNDEF WEBLIB}
      TAdvObjectReferences = TObjectList<TAdvObjectReference>;
      {$ENDIF}
  private
    FReferences: TAdvObjectReferences;
    FReader: TAdvJSONReader;
    function ReadSingleObject(ABaseClass: TClass): TObject; overload;
    property Reader: TAdvJSONReader read FReader;
    procedure ReadSingleObject(AObject: TObject); overload;
    procedure ReadProperties(AObject: TObject);
    procedure ReadProperty(AObject: TObject; AProp: TAdvPropertyInfo);
    procedure ReadPropInfoValue(AInstance: TObject; const APropInfo: TAdvPropertyInfo);
    procedure ReadExistingObject(AObject: TObject);
    procedure ReadGenericList(AList: TAdvObjectList);
    procedure ReadCollection(ACollection: TCollection);
    {$IFDEF LCLLIB}
    procedure ReadList(AList: TList);
    {$ENDIF}
    procedure ReadBitmap(ABitmap: TAdvBitmap);
  public
    constructor Create(AStream: TStreamEx);
    destructor Destroy; override;
    function Read(AClass: TClass): TObject; overload;
    procedure Read(AObject: TObject); overload;
  end;

  {$IFDEF WEBLIB}
  PTypeInfo = TypInfo.TTypeInfo;
  {$ELSE}
  PTypeInfo = TypInfo.PTypeInfo;
  {$ENDIF}

  TAdvObjectPersistence = class
  public
    class function SaveObjectToString(AObject: TObject): string;
    class procedure LoadObjectFromString(AObject: TObject; AString: string);
  end;

  TAdvPersistence = class
  public class var
    ClassTypeVariable: string;
  public
    class procedure SaveSettingsToFile(AObject: TObject; AFileName: string);
    class procedure LoadSettingsFromFile(AObject: TObject; AFileName: string);
    class procedure SaveSettingsToStream(AObject: TObject; AStream: TStreamEx);
    class procedure LoadSettingsFromStream(AObject: TObject; AStream: TStreamEx);
    class procedure GetEnumValues(AValues: TStrings; APropInfo: TAdvPropertyInfo);
    class function CreateObject(const AClassName: string; BaseClass: TClass): TObject;
    class function GetPropInfoDataTypeInfo(APropInfo: TAdvPropertyInfo): PTypeInfo;
    class function GetPropInfoDataTypeInfoClassType(APropInfo: TAdvPropertyInfo): TClass;
    class function GetPropInfoType(APropInfo: TAdvPropertyInfo): TTypeKind; virtual;
    class function GetPropInfoName(APropInfo: TAdvPropertyInfo): string; virtual;
    class function GetPropInfoTypeName(APropInfo: TAdvPropertyInfo): string;
    class function GetEnumName(ATypeInfo: PTypeInfo; AValue: Integer): string;
    class function IsWriteOnly(APropInfo: TAdvPropertyInfo): Boolean; virtual;
    class function IsReadOnly(APropInfo: TAdvPropertyInfo): Boolean; virtual;
    class function IsAssignableProperty(AObject: TObject; APropInfo: TAdvPropertyInfo): Boolean; virtual;
    class function IsColor(APropertyName: string): Boolean; virtual;
    class function IsStrokeKind(APropertyName: string): Boolean; virtual;
    class function IsFillKind(APropertyName: string): Boolean; virtual;
    class function IsDate(APropertyName: string): Boolean; virtual;
    class function IsDateTime(APropertyName: string): Boolean; virtual;
    class function IsTime(APropertyName: string): Boolean; virtual;
    class function IsGenericList(AClass: TClass): Boolean; virtual;
    class function IsCollection(AClass: TClass): Boolean; virtual;
    class function IsComponent(AClass: TClass): Boolean; virtual;
    class function IsControl(AClass: TClass): Boolean; virtual;
    class function IsList(AClass: TClass): Boolean; virtual;
    class function IsDescendingClass(AClass: TClass; AClassParentList: array of string): Boolean; virtual;
    class function IsBitmap(AClass: TClass): Boolean; virtual;
    class function IsStrings(AClass: TClass): Boolean; virtual;
  end;

var
  ExcludePropertyList: array[0..52] of string = (
     'Align',
     'AllowFocus',
     'Anchors',
     'BevelEdges',
     'BevelInner',
     'BevelKind',
     'BevelOuter',
     'BevelWidth',
     'BiDiMode',
     'PictureContainer',
     'BorderSpacing',
     'CanParentFocus',
     'ClipChildren',
     'ClipParent',
     'Constraints',
     'Ctl3D',
     'DisableFocusEffect',
     'DoubleBuffered',
     'DragCursor',
     'DragKind',
     'DragMode',
     'Enabled',
     'EnableDragHighLight',
     'Height',
     'Hint',
     'HitTest',
     'Locked',
     'Margins',
     'Name',
     'Opacity',
     'Padding',
     'ParentBiDiMode',
     'ParentColor',
     'ParentCtl3D',
     'ParentDoubleBuffered',
     'ParentFont',
     'ParentShowHint',
     'PopupMenu',
     'Position',
     'RotationAngle',
     'RotationCenter',
     'Scale',
     'ShowHint',
     'Size',
     'StyleElements',
     'StyleName',
     'TabOrder',
     'TabStop',
     'Tag',
     'Touch',
     'TouchTargetExpansion',
     'Visible',
     'Width');

implementation

uses
  {$IFDEF FMXLIB}
  UITypes,
  {$ENDIF}
  StrUtils,
  Controls,
  Graphics,
  AdvUtils;

const
  {$IFDEF FMXLIB}
  gcNull = $00000000;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  gcNull = -1;
  {$ENDIF}

type
  {$IFDEF FMXLIB}
  TAdvPersistenceColor = TAlphaColor;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TAdvPersistenceColor = TColor;
  {$ENDIF}

type
{$IFDEF FMXLIB}
  TControlClass = class of TControl;
{$ENDIF}
{$IFDEF CMNWEBLIB}
  TCustomControlClass = class of TCustomControl;
{$ENDIF}

{$IFNDEF WEBLIB}
{$IFNDEF DELPHI_LLVM}
{$IFNDEF LCLLIB}
type
  {$HINTS OFF}
  {$IF COMPILERVERSION < 26}
  TSymbolNameBase = string[255];
  TSymbolName = type TSymbolNameBase;
  {$IFEND}
  {$HINTS ON}
  PSymbolName = ^TSymbolName;
{$ENDIF}
{$IFDEF LCLLIB}
type
  PSymbolName = ^ShortString;
{$ENDIF}

function GetShortStringString(const ShortStringPointer: PSymbolName): string;
begin
  Result := string(ShortStringPointer^);
end;
{$ENDIF}
{$IFDEF DELPHI_LLVM}
function GetShortStringString(const ShortStringPointer: PByte): string;
var
  ShortStringLength: Byte;
  FirstShortStringCharacter: MarshaledAString;
  ConvertedLength: Cardinal;
  UnicodeCharacters: array[Byte] of Char;
begin
  if not Assigned(ShortStringPointer) then
    Result := ''
  else
  begin
    ShortStringLength := ShortStringPointer^;
    if ShortStringLength = 0 then
      Result := ''
    else
    begin
      FirstShortStringCharacter := MarshaledAString(ShortStringPointer+1);
      ConvertedLength := UTF8ToUnicode(
          UnicodeCharacters,
          Length(UnicodeCharacters),
          FirstShortStringCharacter,
          ShortStringLength
        );

      ConvertedLength := ConvertedLength-1;
      SetString(Result, UnicodeCharacters, ConvertedLength);
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

function GetTypeInfoEx(APropInfo: TAdvPropertyInfo): PTypeInfo;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.PropType{$IFNDEF LCLLIB}^{$ENDIF};
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.typeinfo;
  {$ENDIF}
end;

function GetEnumValueEx(ATypeInfo: PTypeInfo; AValue: string): Integer;
begin
  {$IFNDEF WEBLIB}
  Result := TypInfo.GetEnumValue(ATypeInfo, AValue);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := TTypeInfoEnum(ATypeInfo).EnumType.NameToInt[AValue];
  {$ENDIF}
end;

function GetEnumNameEx(ATypeInfo: PTypeInfo; AValue: Integer): string;
begin
  {$IFNDEF WEBLIB}
  Result := TypInfo.GetEnumName(ATypeInfo, AValue);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := TTypeInfoEnum(ATypeInfo).EnumType.IntToName[AValue];
  {$ENDIF}
end;

function GetColorRed(AColor: TAdvPersistenceColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).R;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetRValue(AColor);
  {$ENDIF}
end;

function GetColorGreen(AColor: TAdvPersistenceColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).G;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetGValue(AColor);
  {$ENDIF}
end;

function GetColorBlue(AColor: TAdvPersistenceColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).B;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetBValue(AColor);
  {$ENDIF}
end;

function HTMLToColorEx(AHTML: string): TAdvPersistenceColor;

function HexVal(s:string): Integer;
var
  i,j: Integer;
  i1, i2: Integer;
begin
  if Length(s) < 2 then
  begin
    Result := 0;
    Exit;
  end;

  {$IFDEF DELPHI_LLVM}
  i1 := 0;
  i2 := 1;
  {$ELSE}
  i1 := 1;
  i2 := 2;
  {$ENDIF}

  if s[i1] >= 'A' then
    i := ord(s[i1]) - ord('A') + 10
  else
    i := ord(s[i1]) - ord('0');

  if s[i2] >= 'A' then
    j := ord(s[i2]) - ord('A') + 10
  else
    j := ord(s[i2]) - ord('0');

  Result := i shl 4 + j;
end;

{$IFDEF CMNWEBLIB}
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(AHTML,2,2));
  g := Hexval(Copy(AHTML,4,2)) shl 8;
  b := Hexval(Copy(AHTML,6,2)) shl 16;
  Result :=  b + g + r;
{$ENDIF}

{$IFDEF FMXLIB}
const
  Alpha = TAdvPersistenceColor($FF000000);
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(AHTML,2,2)) shl 16;
  g := Hexval(Copy(AHTML,4,2)) shl 8;
  b := Hexval(Copy(AHTML,6,2));
  Result := Alpha or TAdvPersistenceColor(b + g + r);
{$ENDIF}
end;

function ColorToHTMLEx(AColor: TAdvPersistenceColor): string;
const
  HTMLHexColor = '#RRGGBB';
  HexDigit: array[0..$F] of Char = '0123456789ABCDEF';
var
  c: TAdvPersistenceColor;
  i: Integer;
begin
  {$IFDEF DELPHI_LLVM}
  i := 0;
  {$ELSE}
  i := 1;
  {$ENDIF}

  {$IFDEF FMXLIB}
  c := AColor;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  c := ColorToRGB(AColor);
  {$ENDIF}
  Result := HtmlHexColor;
  Result[1 + i] := HexDigit[GetColorRed(c) shr 4];
  Result[2 + i] := HexDigit[GetColorRed(c) and $F];
  Result[3 + i] := HexDigit[GetColorGreen(c) shr 4];
  Result[4 + i] := HexDigit[GetColorGreen(c) and $F];
  Result[5 + i] := HexDigit[GetColorBlue(c) shr 4];
  Result[6 + i] := HexDigit[GetColorBlue(c) and $F];
end;

{ TAdvWriter }

constructor TAdvWriter.Create(AStream: TStreamEx);
begin
  FWriter := TAdvJSONWriter.Create(AStream);
end;

destructor TAdvWriter.Destroy;
begin
  FWriter.Free;
  inherited;
end;

procedure TAdvWriter.WriteGenericList(AList: TAdvObjectList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    WriteSingleObject(AList[I]);
  Writer.WriteEndArray;
end;

procedure TAdvWriter.Write(AObject: TObject);
begin
  WriteObject(AObject);
end;

procedure TAdvWriter.WriteBitmap(ABitmap: TAdvBitmap);
var
  ms: TMemoryStream;
begin
  if IsBitmapEmpty(ABitmap) then
  begin
    FWriter.WriteString('');
    Exit;
  end;

  ms := TMemoryStream.Create;
  try
    {$IFNDEF WEBLIB}
    ABitmap.SaveToStream(ms);
    {$ENDIF}
    {$IFDEF WEBLIB}
    raise Exception.Create('Implement SaveToStream on TAdvBitmap');
    {$ENDIF}
    ms.Position := 0;
    FWriter.WriteString(TAdvUtils.SaveStreamToHexStr(ms));
  finally
    ms.Free;
  end;
end;

procedure TAdvWriter.WriteCollection(ACollection: TCollection);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to ACollection.Count - 1 do
    WriteSingleObject(ACollection.Items[I]);
  Writer.WriteEndArray;
end;

{$IFDEF LCLLIB}
procedure TAdvWriter.WriteList(AList: TList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    WriteSingleObject(TObject(AList[I]));
  Writer.WriteEndArray;
end;
{$ENDIF}

procedure TAdvWriter.WriteObject(AObject: TObject);
begin
  if AObject = nil then
    Writer.WriteNull
  else
  if TAdvPersistence.IsGenericList(AObject.ClassType) then
    WriteGenericList(TAdvObjectList(AObject))
  {$IFDEF LCLLIB}
  else if TAdvPersistence.IsList(AObject.ClassType) then
    WriteList(TList(AObject))
  {$ENDIF}
  else if TAdvPersistence.IsCollection(AObject.ClassType) then
    WriteCollection(TCollection(AObject))
  else if TAdvPersistence.IsBitmap(AObject.ClassType) then
    WriteBitmap(TAdvBitmap(AObject))
  else
    WriteSingleObject(AObject);
end;

procedure TAdvWriter.WriteSingleObject(AObject: TObject);
begin
  Writer.WriteBeginObject;
  Writer.WriteName(TAdvPersistence.ClassTypeVariable);
  Writer.WriteString(AObject.ClassName);
  WriteProperties(AObject);
  Writer.WriteEndObject;
end;

procedure TAdvWriter.WritePropInfoValue(AInstance: TObject; const APropInfo: TAdvPropertyInfo);
var
  cn: string;
  pName: string;
  en: string;
  k: TTypeKind;
  p: TAdvPropertyInfo;
  o: TObject;
begin
  if TAdvPersistence.IsWriteOnly(APropInfo) then
  begin
    Writer.WriteNull;
    Exit;
  end;

  o := AInstance;
  p := APropInfo;
  k := TAdvPersistence.GetPropInfoType(p);
  pName := TAdvPersistence.GetPropInfoName(p);

  case k of
    tkInteger:
    begin
      cn := TAdvPersistence.GetPropInfoTypeName(p);
      if (cn = 'TAlphaColor') or (cn = 'TColor') or (cn = 'TGraphicsColor') then
      begin
        if GetOrdProp(o, p) = gcNull then
          Writer.WriteString('gcNull')
        else
          Writer.WriteString(ColorToHTMLEx(GetOrdProp(o, p)))
      end
      else
        Writer.WriteInteger(GetOrdProp(o, p));
    end;
    {$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}: Writer.WriteString(GetStrProp(o, p));
    tkEnumeration:
      if TAdvPersistence.GetPropInfoDataTypeInfo(p) = TypeInfo(Boolean) then
        Writer.WriteBoolean(Boolean(GetOrdProp(o, p)))
      else
        Writer.WriteInteger(GetOrdProp(o, p));
    {$IFDEF LCLWEBLIB}
    tkBool: Writer.WriteBoolean(Boolean(GetOrdProp(o, p)));
    {$ENDIF}
    tkFloat: Writer.WriteDouble(GetFloatProp(o, p));
    {$IFNDEF WEBLIB}
    tkInt64: Writer.WriteInteger(GetInt64Prop(o, p));
    {$ENDIF}
    tkSet: Writer.WriteInteger(GetOrdProp(o, p));
    else
    begin
      en := GetEnumNameEx(TypeInfo(TTypeKind), Integer(k));
      //raise EAdvReaderException.CreateFmt('Cannot write property %s with type %s', [pName, en]);
    end;
  end;
end;

procedure TAdvWriter.WriteProperties(AObject: TObject);
var
  {$IFNDEF WEBLIB}
  ci: Pointer;
  c: Integer;
  pl: PPropList;
  {$ENDIF}
  {$IFDEF WEBLIB}
  ci: TTypeInfoClass;
  p: TAdvPropertyInfo;
  a: TTypeMemberPropertyDynArray;
  {$ENDIF}
  I: Integer;
begin
  if Assigned(AObject) then
  begin
    {$IFNDEF WEBLIB}
    ci := AObject.ClassInfo;
    c := GetPropList(ci, tkAny, nil);
    GetMem(pl, c * SizeOf(TAdvPropertyInfo));
    {$ENDIF}
    {$IFDEF WEBLIB}
    ci := TypeInfo(AObject);
    {$ENDIF}
    try
      {$IFNDEF WEBLIB}
      GetPropList(ci, tkAny, pl);
      for I := 0 to c - 1 do
        WriteProperty(AObject, pl^[i]);
      {$ENDIF}
      {$IFDEF WEBLIB}
      a := GetPropList(ci, tkAny);
      for I := 0 to Length(a) - 1 do
        WriteProperty(AObject, a[I]);
      {$ENDIF}
    finally
      {$IFNDEF WEBLIB}
      FreeMem(pl);
      {$ENDIF}
    end;
  end;
end;

procedure TAdvWriter.WriteProperty(AObject: TObject; AProp: TAdvPropertyInfo);
var
  pName: string;
  k: TTypeKind;
  b: Boolean;
  p: IAdvPersistence;
begin
  if not Assigned(AProp) then
    Exit;

  pName := TAdvPersistence.GetPropInfoName(AProp);
  k := TAdvPersistence.GetPropInfoType(AProp);

  b := True;
  if Supports(AObject, IAdvPersistence, p) then
    b := p.CanSaveProperty(AObject, pName, k);

  if b then
  begin
    Writer.WriteName(PName);
    if k in [tkClass] then
      WriteObject(GetObjectProp(AObject, pName))
    else
      WritePropInfoValue(AObject, AProp);
  end;
end;

{ TAdvReader }

constructor TAdvReader.Create(AStream: TStreamEx);
begin
  FReader := TAdvJSONReader.Create(AStream);
  FReferences := TAdvObjectReferences.Create(true);
end;

destructor TAdvReader.Destroy;
begin
  FReader.Free;
  FReferences.Free;
  inherited;
end;

function TAdvReader.ReadSingleObject(ABaseClass: TClass): TObject;
var
  cn: string;
begin
  Reader.ReadBeginObject;
  if not Reader.HasNext or (Reader.ReadName <> TAdvPersistence.ClassTypeVariable) then
    raise EAdvReaderException.Create('"'+TAdvPersistence.ClassTypeVariable+'" property not found in Object descriptor.');
  cn := Reader.ReadString;
  Result := TAdvPersistence.CreateObject(cn, ABaseClass);
  try
    ReadProperties(Result);
    Reader.ReadEndObject;
  except
    Result.Free;
    raise;
  end;
end;

procedure TAdvReader.ReadExistingObject(AObject: TObject);
begin
  if Assigned(AObject) then
  begin
    Reader.ReadBeginObject;
    if not Reader.HasNext or (Reader.ReadName <> TAdvPersistence.ClassTypeVariable) then
      raise EAdvReaderException.Create('"'+TAdvPersistence.ClassTypeVariable+'" property not found in Object descriptor.');

    Reader.ReadString;
    ReadProperties(AObject);
    Reader.ReadEndObject;
  end
  else
    Reader.ReadNull;
end;

procedure TAdvReader.ReadGenericList(AList: TAdvObjectList);
var
  obj: TObject;
begin
  AList.Clear;
  Reader.ReadBeginArray;
  while Reader.HasNext do
  begin
    obj := ReadSingleObject(TObject);
    AList.Add(obj);
  end;
  Reader.ReadEndArray;
end;

function TAdvReader.Read(AClass: TClass): TObject;
begin
  Result := ReadSingleObject(AClass);
end;

procedure TAdvReader.Read(AObject: TObject);
begin
  if AObject = nil then
    Reader.ReadNull
  else
  if TAdvPersistence.IsGenericList(AObject.ClassType) then
    ReadGenericList(TAdvObjectList(AObject))
  {$IFDEF LCLLIB}
  else if TAdvPersistence.IsList(AObject.ClassType) then
    ReadList(TList(AObject))
  {$ENDIF}
  else if TAdvPersistence.IsCollection(AObject.ClassType) then
    ReadCollection(TCollection(AObject))
  else if TAdvPersistence.IsBitmap(AObject.ClassType) then
    ReadBitmap(TAdvBitmap(AObject))
  else
    ReadSingleObject(AObject);
end;

procedure TAdvReader.ReadBitmap(ABitmap: TAdvBitmap);
var
  s: string;
  ms: TMemoryStream;
begin
  s := Reader.ReadString;
  if s <> '' then
  begin
    ms := TMemoryStream.Create;
    try
      TAdvUtils.LoadStreamFromHexStr(s, ms);
      ms.Position := 0;
      ABitmap.LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end;
end;

procedure TAdvReader.ReadCollection(ACollection: TCollection);
var
  obj: TObject;
begin
  ACollection.Clear;
  Reader.ReadBeginArray;
  while Reader.HasNext do
  begin
    obj := ReadSingleObject(TObject);
    if Assigned(obj) then
    begin
      try
        if obj is TPersistent then
          ACollection.Add.Assign(obj as TPersistent);
      finally
        obj.Free;
      end;
    end;
  end;
  Reader.ReadEndArray;
end;

{$IFDEF LCLLIB}
procedure TAdvReader.ReadList(AList: TList);
var
  obj: TObject;
begin
  AList.Clear;
  Reader.ReadBeginArray;
  while Reader.HasNext do
  begin
    obj := ReadSingleObject(TObject);
    AList.Add(obj);
  end;
  Reader.ReadEndArray;
end;
{$ENDIF}

procedure TAdvReader.ReadProperties(AObject: TObject);
var
  Prop: TAdvPropertyInfo;
begin
  while Reader.HasNext do
  begin
    Prop := GetPropInfo(AObject, Reader.ReadName);
    if Assigned(Prop) then
      ReadProperty(AObject, Prop);
  end;
end;

procedure TAdvReader.ReadProperty(AObject: TObject; AProp: TAdvPropertyInfo);
var
  pName: string;
  ct: TClass;
  b: Boolean;
  p: IAdvPersistence;
  k: TTypeKind;
begin
  if not Assigned(AProp) then
    Exit;

  k := TAdvPersistence.GetPropInfoType(AProp);
  pName := TAdvPersistence.GetPropInfoName(AProp);

  if k in [tkClass] then
  begin
    ct := TAdvPersistence.GetPropInfoDataTypeInfoClassType(AProp);
    if TAdvPersistence.IsGenericList(ct) then
      ReadGenericList(TAdvObjectList(GetObjectProp(AObject, pName)))
    {$IFDEF LCLLIB}
    else if TAdvPersistence.IsList(ct) then
      ReadList(TList(GetObjectProp(AObject, pName)))
    {$ENDIF}
    else if TAdvPersistence.IsCollection(ct) then
      ReadCollection(TCollection(GetObjectProp(AObject, pName)))
    else if TAdvPersistence.IsBitmap(ct) then
      ReadBitmap(TAdvBitmap(GetObjectProp(AObject, pName)))
    else
      ReadExistingObject(GetObjectProp(AObject, pName));
  end
  else
  begin
    b := True;
    if Supports(AObject, IAdvPersistence, p) then
      b := p.CanLoadProperty(AObject, pName, k);

    if b then
      ReadPropInfoValue(AObject, AProp);
  end;
end;

procedure TAdvReader.ReadPropInfoValue(AInstance: TObject; const APropInfo: TAdvPropertyInfo);
var
  pName, cn, cnv, en: string;
  k: TTypeKind;
  p: TAdvPropertyInfo;
  o: TObject;
  i: Integer;
  s: string;
  b: Boolean;
  d: Double;
  ii: Int64;
begin
  if TAdvPersistence.IsWriteOnly(APropInfo) then
  begin
    Reader.ReadNull;        
    Exit;
  end;
 
  o := AInstance;
  p := APropInfo;
  pName := TAdvPersistence.GetPropInfoName(p);
  k := TAdvPersistence.GetPropInfoType(p);

  case k of
    tkInteger:
    begin
      cn := TAdvPersistence.GetPropInfoTypeName(p);
      if (cn = 'TAlphaColor') or (cn = 'TColor') or (cn = 'TGraphicsColor') then
      begin
        cnv := Reader.ReadString;
        if not TAdvPersistence.IsReadOnly(p) then
        begin
          if cnv = 'gcNull' then
            SetOrdProp(o, pName, gcNull)
          else
            SetOrdProp(o, pName, HTMLToColorEx(cnv));
        end;
      end
      else
      begin
        i := Reader.ReadInteger;
        if not TAdvPersistence.IsReadOnly(p) then
          SetOrdProp(o, p, i);
      end;
    end;
    {$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}: 
    begin
      s := Reader.ReadString;
      if not TAdvPersistence.IsReadOnly(p) then       
        SetStrProp(o, p, s);
    end;
    tkEnumeration:
      if TAdvPersistence.GetPropInfoDataTypeInfo(p) = TypeInfo(Boolean) then
      begin
        b := Reader.ReadBoolean;
        if not TAdvPersistence.IsReadOnly(p) then        
          SetOrdProp(o, p, Integer(b))
      end
      else
      begin
        i := Reader.ReadInteger;
        if not TAdvPersistence.IsReadOnly(p) then        
          SetOrdProp(o, p, i);
      end;
    {$IFDEF LCLWEBLIB}
    tkBool:
    begin
      b := Reader.ReadBoolean;
      if not TAdvPersistence.IsReadOnly(p) then
        SetOrdProp(o, p, Integer(b))
    end;
    {$ENDIF}
    tkFloat:
    begin
      d := Reader.ReadDouble;
      if not TAdvPersistence.IsReadOnly(p) then
        SetFloatProp(o, p, d)
    end;
    {$IFNDEF WEBLIB}
    tkInt64:
    begin
      ii := Reader.ReadInt64;
      if not TAdvPersistence.IsReadOnly(p) then
        SetOrdProp(o, p, ii)
    end;
    {$ENDIF}
    tkSet:
    begin
      i := Reader.ReadInteger;
      if not TAdvPersistence.IsReadOnly(p) then
        SetOrdProp(o, p, i);
    end
    else
    begin
      en := GetEnumNameEx(TypeInfo(TTypeKind), Integer(k));
      //raise EAdvReaderException.CreateFmt('Cannot read property %s with type %s', [pName, en]);
    end;
  end;
end;

procedure TAdvReader.ReadSingleObject(AObject: TObject);
begin
  Reader.ReadBeginObject;
  if not Reader.HasNext or (Reader.ReadName <> TAdvPersistence.ClassTypeVariable) then
    raise EAdvReaderException.Create('"'+TAdvPersistence.ClassTypeVariable+'" property not found in Object descriptor.');
  Reader.ReadString;
  try
    ReadProperties(AObject);
    Reader.ReadEndObject;
  except
    raise;
  end;
end;

{ TAdvReader.TAdvObjectReference }

constructor TAdvReader.TAdvObjectReference.Create(AInstance: TObject;
  AProp: TAdvPropertyInfo; const AId: string);
begin
  Instance := AInstance;
  Prop := AProp;
  Id := AId;
end;

{ TAdvPersistence }

class procedure TAdvPersistence.LoadSettingsFromFile(AObject: TObject; AFileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    {$IFNDEF WEBLIB}
    ms.LoadFromFile(AFileName);
    {$ENDIF}
    LoadSettingsFromStream(AObject, ms);
  finally
    ms.Free;
  end;
end;

class procedure TAdvPersistence.LoadSettingsFromStream(AObject: TObject; AStream: TStreamEx);
var
  Reader: TAdvReader;
  {$IFDEF WEBLIB}
  d, t: string;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  d, t: Char;
  {$ENDIF}
begin
  AStream.Position := 0;
  Reader := TAdvReader.Create(AStream);
  try
    t := FormatSettings.ThousandSeparator;
    d := FormatSettings.DecimalSeparator;
    FormatSettings.DecimalSeparator := '.';
    FormatSettings.ThousandSeparator := ',';
    Reader.Read(AObject);
    FormatSettings.DecimalSeparator := d;
    FormatSettings.ThousandSeparator := t;
  finally
    Reader.Free;
  end;
end;

class procedure TAdvPersistence.SaveSettingsToFile(AObject: TObject;
  AFileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    SaveSettingsToStream(AObject, ms);
    ms.SaveToFile(AFileName);
  finally
    ms.Free;
  end;
end;

class procedure TAdvPersistence.SaveSettingsToStream(AObject: TObject;
  AStream: TStreamEx);
var
  Writer: TAdvWriter;
  {$IFDEF WEBLIB}
  d, t: string;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  d, t: Char;
  {$ENDIF}
begin
  Writer := TAdvWriter.Create(AStream);
  try
    t := FormatSettings.ThousandSeparator;
    d := FormatSettings.DecimalSeparator;
    FormatSettings.DecimalSeparator := '.';
    FormatSettings.ThousandSeparator := ',';
    Writer.Write(AObject);
    FormatSettings.DecimalSeparator := d;
    FormatSettings.ThousandSeparator := t;
  finally
    Writer.Free;
  end;
end;

class procedure TAdvPersistence.GetEnumValues(AValues: TStrings; APropInfo: TAdvPropertyInfo);
var
  p: PTypeInfo;
  {$IFNDEF WEBLIB}
  su: PTypeData;
  {$IFNDEF LCLLIB}
  ct: PPTypeInfo;
  {$ENDIF}
  {$IFDEF LCLLIB}
  ct: PTypeInfo;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  pi: TTypeInfoInteger;
  ps: PTypeInfo;
  {$ENDIF}
  I: Integer;
  k: TTypeKind;
begin
  p := GetTypeInfoEx(APropInfo);
  {$IFNDEF WEBLIB}
  su := GetTypeData(p);
  if Assigned(su) then
  begin
    ct := su^.CompType;
    if Assigned(ct) then
    begin
      k := ct^.Kind;
      case k of
        tkEnumeration:
        begin
          su := GetTypeData(ct{$IFNDEF LCLLIB}^{$ENDIF});
          for i := su^.MinValue to su^.MaxValue do
            AValues.Add(GetEnumNameEx(ct{$IFNDEF LCLLIB}^{$ENDIF},i));
        end;
      end;
    end
    else
    begin
      for i := su^.MinValue to su^.MaxValue do
        AValues.Add(GetEnumNameEx(p,i));
    end;
  end;
  {$ENDIF}
  {$IFDEF WEBLIB}
  if Assigned(p) and (p is TTypeInfoSet) then
    p := TTypeInfoSet(p).comptype;

  if Assigned(p) and (p is TTypeInfoInteger) then
  begin
   pi := TTypeInfoInteger(p);
   for i := pi.MinValue to pi.MaxValue do
     AValues.Add(GetEnumNameEx(p, i));
  end;
  {$ENDIF}
end;

class function TAdvPersistence.GetPropInfoDataTypeInfoClassType(APropInfo: TAdvPropertyInfo): TClass;
{$IFDEF WEBLIB}
var
  t: PTypeInfo;
{$ENDIF}
begin
  {$IFNDEF WEBLIB}
  Result := GetTypeData(APropInfo^.PropType{$IFNDEF LCLLIB}^{$ENDIF})^.ClassType
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := nil;
  if Assigned(APropInfo) and Assigned(APropInfo.typeinfo) then
  begin
    t := APropInfo.typeinfo;
    asm
      if (t.class){
        return t.class.ClassType();
      }
    end;
  end;
  {$ENDIF}
end;

class function TAdvPersistence.GetPropInfoDataTypeInfo(
  APropInfo: TAdvPropertyInfo): PTypeInfo;
begin
  {$IFNDEF WEBLIB}
  Result := GetTypeData(APropInfo^.PropType{$IFNDEF LCLLIB}^{$ENDIF})^.BaseType{$IFNDEF LCLLIB}^{$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := nil;
  if Assigned(APropInfo) then
    Result := APropInfo.typeinfo;
  {$ENDIF}
end;

class function TAdvPersistence.GetPropInfoName(APropInfo: TAdvPropertyInfo): string;
begin
  {$IFNDEF WEBLIB}
  Result := GetShortStringString(@APropInfo{$IFDEF LCLLIB}^{$ENDIF}.Name);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.name;
  {$ENDIF}
end;

class function TAdvPersistence.GetPropInfoType(APropInfo: TAdvPropertyInfo): TTypeKind;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.PropType^{$IFNDEF LCLLIB}^{$ENDIF}.Kind;
  {$ENDIF}
  {$IFDEF WEBLIB}
  if Assigned(APropInfo.typeinfo) then
    Result := APropInfo.typeinfo.kind
  else
    Result := tkUnknown;
  {$ENDIF}
end;

class function TAdvPersistence.GetEnumName(ATypeInfo: PTypeInfo; AValue: Integer): string;
begin
  {$IFNDEF WEBLIB}
  Result := TypInfo.GetEnumName(ATypeInfo, AValue);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := TTypeInfoEnum(ATypeInfo).EnumType.IntToName[AValue];
  {$ENDIF}
end;

class function TAdvPersistence.GetPropInfoTypeName(APropInfo: TAdvPropertyInfo): string;
begin
  {$IFNDEF WEBLIB}
  Result := GetShortStringString(@APropInfo{$IFDEF LCLLIB}^{$ENDIF}.PropType^.Name);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := '';
  if Assigned(APropInfo.typeinfo) then
    Result := APropInfo.typeinfo.name;
  {$ENDIF}
end;

class function TAdvPersistence.IsList(AClass: TClass): boolean;
begin
  Result := IsDescendingClass(AClass, ['TList']);
end;

class function TAdvPersistence.IsAssignableProperty(AObject: TObject;
  APropInfo: TAdvPropertyInfo): Boolean;
var
  oProp: TObject;
  k: TTypeKind;
  pName: string;
begin
  Result := False;
  k := GetPropInfoType(APropInfo);
  if k in [tkClass] then
  begin
    pName := GetPropInfoName(APropInfo);
    oProp := GetObjectProp(AObject, pName);
    Result := (Assigned(oProp) and IsComponent(oProp.ClassType)) or not Assigned(oProp);
  end;
end;

class function TAdvPersistence.IsBitmap(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TBitmap', 'TPicture', 'TAdvBitmap']);
end;

class function TAdvPersistence.IsReadOnly(
  APropInfo: TAdvPropertyInfo): Boolean;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.SetProc = nil;
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.setter = '';
  {$ENDIF}
end;

class function TAdvPersistence.IsStrings(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TStrings']);
end;

class function TAdvPersistence.IsStrokeKind(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TAdvGraphicsStrokeKind');
end;

class function TAdvPersistence.IsTime(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TTime');
end;

class function TAdvPersistence.IsWriteOnly(APropInfo: TAdvPropertyInfo): Boolean;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.GetProc = nil;
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.getter = '';
  {$ENDIF}
end;

class function TAdvPersistence.IsCollection(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TCollection']);
end;

class function TAdvPersistence.IsColor(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TAlphaColor') or (APropertyName = 'TColor') or (APropertyName = 'TGraphicsColor');
end;

class function TAdvPersistence.IsComponent(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TComponent', 'TAdvCustomComponent']);
end;

class function TAdvPersistence.IsControl(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TControl']);
end;

class function TAdvPersistence.IsDate(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TDate');
end;

class function TAdvPersistence.IsDateTime(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TDateTime');
end;

class function TAdvPersistence.IsDescendingClass(AClass: TClass;
  AClassParentList: array of string): Boolean;
var
  cn: string;
  I: Integer;
begin
  if not Assigned(AClass) then
    Exit(False);
  repeat
    cn := AClass.ClassName;
    for I := 0 to Length(AClassParentList) - 1 do
    begin
      if (cn = AClassParentList[I]) then
        Exit(True);
    end;
    AClass := AClass.ClassParent;
  until not Assigned(AClass);
  Result := False;
end;

class function TAdvPersistence.IsFillKind(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TAdvGraphicsFillKind');
end;

class function TAdvPersistence.IsGenericList(AClass: TClass): boolean;
var
  cn: string;
begin
  if not Assigned(AClass) then
    Exit(False);
  repeat
    cn := AClass.ClassName;
    if AnsiStartsStr('TList<', cn) or AnsiStartsStr('TObjectList<', cn) then
      Exit(True);
    AClass := AClass.ClassParent;
  until not Assigned(AClass);
  Result := False;
end;

class function TAdvPersistence.CreateObject(const AClassName: string;
  BaseClass: TClass): TObject;
var
  ObjType: TPersistentClass;
begin
  ObjType := GetClass(AClassName);
  if ObjType = nil then
    raise EAdvReaderException.CreateFmt('Type "%s" not found', [AClassName]);
  if not ObjType.InheritsFrom(TObject) then
    raise EAdvReaderException.Create('Type "%s" is not an class type');
  if BaseClass <> nil then
    if not ObjType.InheritsFrom(BaseClass) then
      raise EAdvReaderException.CreateFmt('Type "%s" does not inherit from %s',
        [AClassName, BaseClass.ClassName]);

  {$IFDEF CMNWEBLIB}
  if ObjType.InheritsFrom(TCustomControl) then
    Result := TCustomControlClass(ObjType).Create(nil)
  {$ENDIF}
  {$IFDEF FMXLIB}
  if ObjType.InheritsFrom(TControl) then
    Result := TControlClass(ObjType).Create(nil)
  {$ENDIF}
  else if ObjType.InheritsFrom(TCollectionItem) then
    Result := TCollectionItemClass(ObjType).Create(nil)
  else if ObjType.InheritsFrom(TPersistent) then
    Result := TPersistentClass(ObjType).Create
  else
    raise EAdvReaderException.CreateFmt('Type "%s" not supported', [AClassName]);
end;

{$IFDEF WEBLIB}
function TAdvReader.TAdvObjectReferences.GetItem(Index: Integer): TAdvObjectReference;
begin
  Result := TAdvObjectReference(inherited Items[Index]);
end;

procedure TAdvReader.TAdvObjectReferences.SetItem(Index: Integer; const Value: TAdvObjectReference);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

{$IFDEF WEBLIB}
function TAdvObjectList.GetItem(Index: Integer): TObject;
begin
  Result := TObject(inherited Items[Index]);
end;

procedure TAdvObjectList.SetItem(Index: Integer; const Value: TObject);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

{ TAdvObjectPersistence }

class procedure TAdvObjectPersistence.LoadObjectFromString(AObject: TObject; AString: string);
var
  ms: TStringStream;
begin
  ms := TStringStream.Create(AString);
  try
    TAdvPersistence.LoadSettingsFromStream(AObject, ms);
  finally
    ms.Free;
  end;
end;

class function TAdvObjectPersistence.SaveObjectToString(
  AObject: TObject): string;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('');
  try
    TAdvPersistence.SaveSettingsToStream(AObject, ss);
    ss.Position := 0;
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

initialization
  TAdvPersistence.ClassTypeVariable := '$type';

end.


