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

unit AdvJSONWriter;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$INLINE ON}{$R-}{$Q-}

interface

uses
  {$IFNDEF LCLWEBLIB}
  Generics.Collections,
  {$ENDIF}
  Classes, SysUtils, AdvTypes;

type
  TAdvJSONStreamWriter = class
  private
  var
    FStream: TStream;
    FWriteStream: TStringStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure Write(const Value: string);
  end;

  TAdvJSONWriter = class
  public
    type
      ECannotWriteName = class(Exception)
      public
        constructor Create;
      end;
      EMultipleRootNotAllowed = class(Exception)
      public
        constructor Create;
      end;
      EObjectOrArrayExpected = class(Exception)
      public
        constructor Create;
      end;
      EInvalidNesting = class(Exception)
      public
        constructor Create;
      end;
      EMissingValue = class(Exception)
      public
        constructor Create;
      end;
      ETooManyDepthLevels = class(Exception)
      public
        constructor Create;
      end;
      EEmptyJson = class(Exception)
      public
        constructor Create;
      end;
      EEmptyName = class(Exception)
      public
        constructor Create;
      end;
  private
    type
      TAdvJSONScope = (jscEmptyDocument, jscEmptyArray, jscEmptyObject, jscNonEmptyDocument,
        jscNonEmptyArray, jscNonEmptyObject, jscDanglingName);
    const
      MaxStackSize = 255;
  private
    FWriter: TAdvJSONStreamWriter;
    FStack: array[0..MaxStackSize] of TAdvJSONScope;
    FStackSize: integer;
    FIndent: string;
    FSeparator: string;
    FDeferredName: string;
    FClosed: boolean;
    procedure SetIndentLength(const Value: integer);
    function GetIndentLength: integer;
    function OpenItem(const Empty: TAdvJSONScope; const OpenBracket: string): TAdvJSONWriter;
    function CloseItem(const Empty, NonEmpty: TAdvJSONScope; const CloseBracket: string): TAdvJSONWriter;
    procedure PushScope(const Scope: TAdvJSONScope); inline;
    function PeekScope: TAdvJSONScope; inline;
    procedure ReplaceTop(const Scope: TAdvJSONScope); inline;
    procedure WriteDeferredName; inline;
    procedure InternalWriteString(const Value: string);
    procedure NewLine; inline;
    procedure BeforeName;
    procedure BeforeValue(const Root: boolean);
  public
    constructor Create(const AStream: TStream);
    destructor Destroy; override;
    function WriteBeginArray: TAdvJSONWriter;
    function WriteEndArray: TAdvJSONWriter;
    function WriteBeginObject: TAdvJSONWriter;
    function WriteEndObject: TAdvJSONWriter;
    function WriteName(const Name: string): TAdvJSONWriter;
    function WriteString(const Value: string): TAdvJSONWriter;
    function WriteRawString(const Value: string): TAdvJSONWriter;
    function WriteBoolean(const Value: boolean): TAdvJSONWriter;
    function WriteNull: TAdvJSONWriter;
    function WriteDouble(const Value: double): TAdvJSONWriter;
    function WriteInteger(const Value: Int64): TAdvJSONWriter;
    procedure Close;
    property IndentLength: integer read GetIndentLength write SetIndentLength;
  end;

implementation

uses
  AdvUtils;

resourcestring
  ErrInvalidString = 'The file contains a string that can''t be encoded in UTF-8';

procedure RaiseErrInvalidString;
begin
  raise Exception.Create(ErrInvalidString);
end;

{ TAdvJSONWriter }

procedure TAdvJSONWriter.BeforeName;
begin
  case PeekScope of
    TAdvJSONScope.jscNonEmptyObject: FWriter.Write(',');
    TAdvJSONScope.jscEmptyObject: ;
  else
    raise ECannotWriteName.Create;
  end;
  NewLine;
  ReplaceTop(TAdvJSONScope.jscDanglingName);
end;

procedure TAdvJSONWriter.BeforeValue(const Root: boolean);
begin
  case PeekScope of
    TAdvJSONScope.jscNonEmptyDocument:
      raise EMultipleRootNotAllowed.Create;
    TAdvJSONScope.jscEmptyDocument:
      begin
        if not Root then
           raise EObjectOrArrayExpected.Create;
        ReplaceTop(TAdvJSONScope.jscNonEmptyDocument);
      end;
    TAdvJSONScope.jscEmptyArray:
      begin
        ReplaceTop(TAdvJSONScope.jscNonEmptyArray);
        NewLine;
      end;
    TAdvJSONScope.jscNonEmptyArray:
      begin
        FWriter.Write(',');
        NewLine;
      end;
    TAdvJSONScope.jscDanglingName:
      begin
        FWriter.Write(FSeparator);
        ReplaceTop(TAdvJSONScope.jscNonEmptyObject);
      end;
  else
    raise EInvalidNesting.Create;
  end;
end;

function TAdvJSONWriter.CloseItem(const Empty, NonEmpty: TAdvJSONScope;
  const CloseBracket: string): TAdvJSONWriter;
var
  Context: TAdvJSONScope;
begin
  Context := PeekScope;
  if not (Context in [Empty, NonEmpty]) then
    raise EInvalidNesting.Create;
  if FDeferredName <> '' then
    raise EMissingValue.Create;
  Dec(FStackSize);
  if Context = NonEmpty then
    NewLine;
  FWriter.Write(CloseBracket);
  Result := Self;
end;

procedure TAdvJSONWriter.Close;
begin
  if (FStackSize > 1) or ((FStackSize = 1) and (PeekScope <> TAdvJSONScope.jscNonEmptyDocument)) then
    raise EInvalidNesting.Create;
  FClosed := true;
end;

constructor TAdvJSONWriter.Create(const aStream: TStream);
begin
  inherited Create;
  FWriter := TAdvJSONStreamWriter.Create(aStream);
  FSeparator := ':';
  PushScope(TAdvJSONScope.jscEmptyDocument);
end;

destructor TAdvJSONWriter.Destroy;
begin
  FWriter.Free;
  inherited;
end;

procedure TAdvJSONWriter.NewLine;
var
  I: integer;
begin
  if FIndent <> '' then
  begin
    FWriter.Write(#13#10);
    for I := 1 to FStackSize - 1 do
      FWriter.Write(FIndent);
  end;
end;

function TAdvJSONWriter.OpenItem(const Empty: TAdvJSONScope;
  const OpenBracket: string): TAdvJSONWriter;
begin
  BeforeValue(true);
  PushScope(Empty);
  FWriter.Write(OpenBracket);
  Result := Self;
end;

function TAdvJSONWriter.PeekScope: TAdvJSONScope;
begin
  if FStackSize = 0 then
    raise EEmptyJson.Create;
  Result := FStack[FStackSize - 1];
end;

procedure TAdvJSONWriter.PushScope(const Scope: TAdvJSONScope);
begin
  if FStackSize > MaxStackSize then
    raise ETooManyDepthLevels.Create;
  FStack[FStackSize] := Scope;
  Inc(FStackSize);
end;

procedure TAdvJSONWriter.ReplaceTop(const Scope: TAdvJSONScope);
begin
  if FStackSize = 0 then
    raise EEmptyJson.Create;
  FStack[FStackSize - 1] := Scope;
end;

procedure TAdvJSONWriter.SetIndentLength(const Value: integer);
begin
  if Value <= 0 then
  begin
    FIndent := '';
    FSeparator := ':';
  end else
  begin
    FIndent := StringOfChar(#32, Value);
    FSeparator := ': ';
  end;
end;

function TAdvJSONWriter.GetIndentLength: integer;
begin
  Result := Length(FIndent);
end;

procedure TAdvJSONWriter.InternalWriteString(const Value: string);
begin
  FWriter.Write('"');
  FWriter.Write(TAdvUtils.EscapeString(Value));
  FWriter.Write('"');
end;

function TAdvJSONWriter.WriteString(const Value: string): TAdvJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  InternalWriteString(Value);
  Result := Self;
end;

function TAdvJSONWriter.WriteEndArray: TAdvJSONWriter;
begin
  Result := CloseItem(TAdvJSONScope.jscEmptyArray, TAdvJSONScope.jscNonEmptyArray, ']');
end;

function TAdvJSONWriter.WriteEndObject: TAdvJSONWriter;
begin
  Result := CloseItem(TAdvJSONScope.jscEmptyObject, TAdvJSONScope.jscNonEmptyObject, '}');
end;

function TAdvJSONWriter.WriteBeginArray: TAdvJSONWriter;
begin
  WriteDeferredName;
  Result := OpenItem(TAdvJSONScope.jscEmptyArray, '[');
end;

function TAdvJSONWriter.WriteBeginObject: TAdvJSONWriter;
begin
  WriteDeferredName;
  Result := OpenItem(TAdvJSONScope.jscEmptyObject, '{');
end;

function TAdvJSONWriter.WriteBoolean(const Value: boolean): TAdvJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  if Value then
    FWriter.Write('true')
  else
    FWriter.Write('false');
    Result := Self;
end;

procedure TAdvJSONWriter.WriteDeferredName;
begin
  if FDeferredName <> '' then
  begin
    BeforeName;
    InternalWriteString(FDeferredName);
    FDeferredName := '';
  end;
end;

function TAdvJSONWriter.WriteDouble(const Value: double): TAdvJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  FWriter.Write(FloatToStr(Value));
  Result := Self;
end;

function TAdvJSONWriter.WriteName(const Name: string): TAdvJSONWriter;
begin
  if Name = '' then
    raise EEmptyName.Create;
  if FDeferredName <> '' then
    raise EMissingValue.Create;
  if FStackSize = 0 then
    raise EEmptyJson.Create;
  FDeferredName := Name;
  Result := Self;
end;

function TAdvJSONWriter.WriteNull: TAdvJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  FWriter.Write('null');
  Result := Self;
end;

function TAdvJSONWriter.WriteRawString(const Value: string): TAdvJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  FWriter.Write('"');
  FWriter.Write(Value);
  FWriter.Write('"');
  Result := Self;
end;

function TAdvJSONWriter.WriteInteger(const Value: Int64): TAdvJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  FWriter.Write(IntToStr(Value));
  Result := Self;
end;

{ TAdvJSONStreamWriter }

constructor TAdvJSONStreamWriter.Create(aStream: TStream);
begin
  FStream := aStream;
  FWriteStream := TStringStream.Create('');
end;

destructor TAdvJSONStreamWriter.Destroy;
begin
  try
    FWriteStream.Position := 0;
    FStream.CopyFrom(FWritestream, FWriteStream.Size);
  finally
    FWriteStream.Free;
  end;
  inherited;
end;

procedure TAdvJSONStreamWriter.Write(const Value: string);
begin
  FWriteStream.WriteString(Value);
end;

{ TAdvJSONWriter.ECannotWriteName }

constructor TAdvJSONWriter.ECannotWriteName.Create;
begin
  inherited Create('Cannot write name in current Json scope');
end;

{ TAdvJSONWriter.EMultipleRootNotAllowed }

constructor TAdvJSONWriter.EMultipleRootNotAllowed.Create;
begin
  inherited Create('Multiple root values not allowed');
end;

{ TAdvJSONWriter.EObjectOrArrayExpected }

constructor TAdvJSONWriter.EObjectOrArrayExpected.Create;
begin
  inherited Create('Object or array expected as top-level value');
end;

{ TAdvJSONWriter.EInvalidNesting }

constructor TAdvJSONWriter.EInvalidNesting.Create;
begin
  inherited Create('Invalid nesting. Not all arrays/objects were properly closed.');
end;

{ TAdvJSONWriter.EMissingValue }

constructor TAdvJSONWriter.EMissingValue.Create;
begin
  inherited Create('Json value missing');
end;

{ TAdvJSONWriter.ETooManyDepthLevels }

constructor TAdvJSONWriter.ETooManyDepthLevels.Create;
begin
  inherited Create('Maximum level of nested structured reached.');
end;

{ TAdvJSONWriter.EEmptyJson }

constructor TAdvJSONWriter.EEmptyJson.Create;
begin
  inherited Create('Json is still empty. Cannot perform operation.');
end;

{ TAdvJSONWriter.EEmptyName }

constructor TAdvJSONWriter.EEmptyName.Create;
begin
  inherited Create('Cannot write empty name');
end;

end.
