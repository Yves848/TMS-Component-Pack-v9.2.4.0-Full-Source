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

unit AdvJSONReader;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$SCOPEDENUMS ON}
{$INLINE ON}{$R-}{$Q-}

interface

uses
  {$IFNDEF LCLWEBLIB}
  Generics.Collections,
  {$ENDIF}
  Classes, SysUtils, AdvTypes;

type
  TAdvJSONStreamReader = class
  public
    type
      EInvalidJsonInput = class(Exception)
      public
        constructor Create;
      end;
      EInternalError = class(Exception)
      public
        constructor Create;
      end;
      EEndOfInputReached = class(Exception)
      public
        constructor Create;
      end;
  private
  var
    FStream: TStream;
    FReadStream: TStringStream;
  public
    constructor Create(const aStream: TStream);
    destructor Destroy; override;
    function NextChar: char; inline;
    function PeekChar: char; inline;
    function ReadChar: char; inline;
    procedure Backup(const {%H-}c: char);
    procedure MoveNext(const Count: integer = 1); inline;
    function Eof: boolean; inline;
  end;

  TAdvJSONToken = (jstoBeginObject, jstoEndObject, jstoBeginArray, jstoEndArray,
    jstoName, jstoBoolean, jstoNull, jstoText, jstoNumber, jstoEOF);

  TAdvJSONReader = class
  private
    type
      TAdvJSONState = (jstNone, jstBeginObject, jstEndObject, jstBeginArray, jstEndArray, jstTrue, jstFalse,
        jstNull, jstDoubleQuoted, jstBuffered, jstDoubleQuotedName, jstInt64, jstNumber, jstEOF);
      TAdvJSONNumberState = (jnstNone, jnstSign, jnstDigit, jnstDecimal, jnstFraction, jnstExpE, jnstExpSign, jnstExpDigit);
      TAdvJSONScope = (jscEmptyDocument, jscEmptyArray, jscEmptyObject, jscNonEmptyDocument,
        jscNonEmptyArray, jscNonEmptyObject, jscDanglingName);
  public
    type
      EInvalidStateException = class(Exception)
      public
        constructor Create(const AState: TAdvJSONState);
      end;
      EUnterminatedArray = class(Exception)
      public
        constructor Create;
      end;
      EUnterminatedObject = class(Exception)
      public
        constructor Create;
      end;
      ENameExpected = class(Exception)
      public
        constructor Create;
      end;
      EColonExpected = class(Exception)
      public
        constructor Create;
      end;
      EReaderClosed = class(Exception)
      public
        constructor Create;
      end;
      EMultipleRootNotAllowed = class(Exception)
      public
        constructor Create;
      end;
      EExpectedValue = class(Exception)
      public
        constructor Create;
      end;
      EObjectOrArrayExpected = class(Exception)
      public
        constructor Create;
      end;
      ETooManyDepthLevels = class(Exception)
      public
        constructor Create;
      end;
      EInvalidEscaped = class(Exception)
      public
        constructor Create;
      end;
    const
      MaxNumberBuffer = 255;
      MaxStackSize = 255;
  private
    FReader: TAdvJSONStreamReader;
    FStack: array[0..MaxStackSize] of TAdvJSONScope;
    FStackSize: integer;
    FPeeked: TAdvJSONState;
    FPeekedInt64: Int64;
    FPeekedNumber: array[0..MaxNumberBuffer] of Char;
    FPeekedString: string;
    function NextPeek: TAdvJSONState; inline;
    procedure CheckState(const State: TAdvJSONState); inline;
    procedure SkipChar; inline;
    function IsLiteral(C: Char): boolean;
    function IsDigit(C: Char): boolean; inline;
    function DoPeek: TAdvJSONState;
    procedure PushScope(const Scope: TAdvJSONScope);
    function NextNonWhitespace: Char;
    function ReadChar: Char;
    function PeekKeyword: TAdvJSONState;
    function PeekNumber: TAdvJSONState;
    function InternalReadQuoted(const BuildString: boolean): string;
    function ReadQuoted: string;
    procedure SkipQuoted;
  private
    function SkipWhitespaceUntilEnd: boolean;
  public
    constructor Create(const AStream: TStream);
    destructor Destroy; override;
    procedure ReadBeginArray;
    procedure ReadEndArray;
    procedure ReadBeginObject;
    procedure ReadEndObject;
    function ReadName: string;
    function ReadString: string;
    function ReadBoolean: boolean;
    function ReadDouble: double;
    function ReadInt64: Int64;
    function ReadInteger: integer;
    procedure SkipValue;
    procedure ReadNull;
    function HasNext: boolean;
    function Peek: TAdvJSONToken;
    function IsNull: boolean;
    function Eof: boolean;
  end;

implementation

uses
  AdvUtils;

const
  Wspace: Set of byte = [$20, $A, $D, $9, $C];

function ArrayOfCharToString(AArray: array of char): string;
{$IFDEF WEBLIB}
var
  I: Integer;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  Result := '';
  for I := 0 to Length(AArray) - 1 do
  begin
    if AArray[I] = #0 then
      Break;

    Result := Result + AArray[I];
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := string(AArray);
  {$ENDIF}
end;

{ TAdvJSONReader }

constructor TAdvJSONReader.Create(const aStream: TStream);
begin
  inherited Create;
  FReader := TAdvJSONStreamReader.Create(aStream);
  FPeeked := TAdvJSONState.jstNone;
  FStack[0] := TAdvJSONScope.jscEmptyDocument;
  FStackSize := 1;
end;

destructor TAdvJSONReader.Destroy;
begin
  FReader.Free;
  inherited;
end;

function TAdvJSONReader.Eof: boolean;
begin
  Result := (Peek = TAdvJSONToken.jstoEOF);
end;

function TAdvJSONReader.Peek: TAdvJSONToken;
begin
  case NextPeek of
    TAdvJSONState.jstBeginObject:
      Result := TAdvJSONToken.jstoBeginObject;

    TAdvJSONState.jstEndObject:
      Result := TAdvJSONToken.jstoEndObject;

    TAdvJSONState.jstBeginArray:
      Result := TAdvJSONToken.jstoBeginArray;

    TAdvJSONState.jstEndArray:
      Result := TAdvJSONToken.jstoEndArray;

    TAdvJSONState.jstDoubleQuotedName:
      Result := TAdvJSONToken.jstoName;

    TAdvJSONState.jstTrue,
    TAdvJSONState.jstFalse:
      Result := TAdvJSONToken.jstoBoolean;

    TAdvJSONState.jstNull:
      Result := TAdvJSONToken.jstoNull;

    TAdvJSONState.jstDoubleQuoted,
    TAdvJSONState.jstBuffered:
      Result := TAdvJSONToken.jstoText;

    TAdvJSONState.jstInt64,
    TAdvJSONState.jstNumber:
      Result := TAdvJSONToken.jstoNumber;

    TAdvJSONState.jstEOF:
      Result := TAdvJSONToken.jstoEOF;
  else
    Assert(false);
    Result := TAdvJSONToken.jstoEOF;
  end;
end;

function TAdvJSONReader.PeekKeyword: TAdvJSONState;
begin
  case FReader.PeekChar of
    't', 'T':
      begin
        FReader.MoveNext;
        case FReader.NextChar of
          'r', 'R': case FReader.NextChar of
            'u', 'U': case FReader.NextChar of
              'e', 'E':
                if FReader.EOF or not IsLiteral(FReader.PeekChar) then
                  Exit(TAdvJSONState.jstTrue);
            end;
          end;
        end;
      end;
    'f', 'F':
      begin
        FReader.MoveNext;
        case FReader.NextChar of
          'a', 'A': case FReader.NextChar of
            'l', 'L': case FReader.NextChar of
              's', 'S': case FReader.NextChar of
                'e', 'E':
                  if FReader.EOF or not IsLiteral(FReader.PeekChar) then
                    Exit(TAdvJSONState.jstFalse);
              end;
            end;
          end;
        end;
      end;
    'n', 'N':
      begin
        FReader.MoveNext;
        case FReader.NextChar of
          'u', 'U': case FReader.NextChar of
            'l', 'L': case FReader.NextChar of
              'l', 'L':
                if FReader.EOF or not IsLiteral(FReader.PeekChar) then
                  Exit(TAdvJSONState.jstNull);
            end;
          end;
        end;
      end;
  else
    Exit(TAdvJSONState.jstNone);
  end;
  raise EExpectedValue.Create;
end;

function TAdvJSONReader.PeekNumber: TAdvJSONState;
const
  MinIncompleteInteger = Low(Int64) div 10;
var
  Last: TAdvJSONNumberState;
  Negative: boolean;
  FitsInInt64: boolean;
  Value: Int64;
  NewValue: Int64;
  C: Char;
  BufIndex: integer;
begin
  C := FReader.PeekChar;
  if (C <> '-') and not IsDigit(C) then
    Exit(TAdvJSONState.jstNone);

  Negative := false;
  FitsInInt64 := true;
  Last := TAdvJSONNumberState.jnstNone;
  BufIndex := 0;
  Value := -1;
  repeat
    if BufIndex >= MaxNumberBuffer then
      raise EExpectedValue.Create;
    C := FReader.NextChar;
    FPeekedNumber[BufIndex] := C;
    Inc(BufIndex);
    case C of
      '-':
        if Last = TAdvJSONNumberState.jnstNone then
        begin
          Negative := true;
          Last := TAdvJSONNumberState.jnstSign;
          Continue;
        end
        else
        if Last = TAdvJSONNumberState.jnstExpE then
        begin
          Last := TAdvJSONNumberState.jnstExpSign;
          Continue;
        end
        else
          raise EExpectedValue.Create;
      '+':
        if Last = TAdvJSONNumberState.jnstExpE then
        begin
          Last := TAdvJSONNumberState.jnstExpSign;
          Continue;
        end
        else
          raise EExpectedValue.Create;
      'e', 'E':
        if Last in [TAdvJSONNumberState.jnstDigit, TAdvJSONNumberState.jnstFraction] then
        begin
          Last := TAdvJSONNumberState.jnstExpE;
          Continue;
        end
        else
          raise EExpectedValue.Create;
      '.':
        if Last = TAdvJSONNumberState.jnstDigit then
        begin
          Last := TAdvJSONNumberState.jnstDecimal;
          Continue;
        end
        else
          raise EExpectedValue.Create;
    else
      if not IsDigit(C) then
        if not IsLiteral(C) then
        begin
          FReader.Backup(C);
          Dec(BufIndex);
          Break;
        end
        else
          raise EExpectedValue.Create;

      if Last in [TAdvJSONNumberState.jnstSign, TAdvJSONNumberState.jnstNone] then
      begin
        Value := -(Ord(C) - 48);
        Last := TAdvJSONNumberState.jnstDigit
      end
      else
      if Last = TAdvJSONNumberState.jnstDigit then
      begin
        if Value = 0 then
          raise EExpectedValue.Create;
        NewValue := Value * 10 - (Ord(C) - 48);
        FitsInInt64 := FitsInInt64 and (
            (Value > MinIncompleteInteger)
             or ((Value = MinIncompleteInteger) and (NewValue < Value))
          );
        Value := NewValue;
      end
      else
      if Last = TAdvJSONNumberState.jnstDecimal then
        Last := TAdvJSONNumberState.jnstFraction
      else
      if Last in [TAdvJSONNumberState.jnstExpE, TAdvJSONNumberState.jnstExpSign] then
        Last := TAdvJSONNumberState.jnstExpDigit;
    end;
  until false;

  if (Last = TAdvJSONNumberState.jnstDigit) and FitsInInt64 and ((Value <> Low(Int64)) or Negative) then
  begin
    if Negative then
      FPeekedInt64 := Value
    else
      FPeekedInt64 := -Value;
    Exit(TAdvJSONState.jstInt64);
  end
  else
  if Last in [TAdvJSONNumberState.jnstDigit, TAdvJSONNumberState.jnstFraction, TAdvJSONNumberState.jnstExpDigit] then
  begin
    FPeekedNumber[BufIndex] := #0;
    Exit(TAdvJSONState.jstNumber);
  end
  else
    raise EExpectedValue.Create;
end;

procedure TAdvJSONReader.PushScope(const Scope: TAdvJSONScope);
begin
  if FStackSize > MaxStackSize then
    raise ETooManyDepthLevels.Create;
  FStack[FStackSize] := Scope;
  Inc(FStackSize);
end;

function TAdvJSONReader.HasNext: boolean;
begin
  Result := not (NextPeek in [TAdvJSONState.jstEndObject, TAdvJSONState.jstEndArray]);
end;

function TAdvJSONReader.IsDigit(C: Char): boolean;
begin
  Result := (C <= #255) and CharIsNumber(C);
end;

function TAdvJSONReader.IsLiteral(C: Char): boolean;
begin
  Result := not TAdvUtils.CharInSet(C, TAdvUtils.CreateCharSet('/\;#{}[]:,'' '#13#10#12#9));
end;

function TAdvJSONReader.IsNull: boolean;
begin
  Result := (Peek = TAdvJSONToken.jstoNull);
end;

function TAdvJSONReader.NextNonWhitespace: Char;
var
  s: Char;
  p: Int64;
begin
  p := FReader.FReadStream.Position;
  Result := #0;
  s := ReadChar;
  repeat
    if (s > #32) or not (Ord(s) in Wspace) then
    begin
      FReader.FReadStream.Position := p;
      Exit(s);
    end;

    s := ReadChar;
  until FReader.Eof;
  FReader.FReadStream.Position := p;
end;

function TAdvJSONReader.NextPeek: TAdvJSONState;
begin
  if FPeeked = TAdvJSONState.jstNone then
    FPeeked := DoPeek;
  Result := FPeeked;
end;

procedure TAdvJSONReader.SkipQuoted;
begin
  InternalReadQuoted(false);
end;

procedure TAdvJSONReader.SkipValue;
var
  Count: integer;
begin
  Count := 0;
  repeat
    case NextPeek of
      TAdvJSONState.jstBeginArray:
        begin
          PushScope(TAdvJSONScope.jscEmptyArray);
          Inc(Count);
        end;
      TAdvJSONState.jstBeginObject:
        begin
          PushScope(TAdvJSONScope.jscEmptyObject);
          Inc(Count);
        end;
      TAdvJSONState.jstEndArray, TAdvJSONState.jstEndObject:
        begin
          Dec(FStackSize);
          Dec(Count);
        end;
      TAdvJSONState.jstDoubleQuoted, TAdvJSONState.jstDoubleQuotedName:
        SkipQuoted;
    end;
    FPeeked := TAdvJSONState.jstNone;
  until Count <= 0;
end;

procedure TAdvJSONReader.SkipChar;
begin
  FReader.MoveNext;
end;

function TAdvJSONReader.DoPeek: TAdvJSONState;
var
  FPeekStack: TAdvJSONScope;
  C: Char;
begin
  FPeekStack := FStack[FStackSize - 1];
  if FPeekStack = TAdvJSONScope.jscEmptyArray then
    FStack[FStackSize - 1] := TAdvJSONScope.jscNonEmptyArray
  else
  if FPeekStack = TAdvJSONScope.jscNonEmptyArray then
  begin
    C := NextNonWhitespace;
    case C of
      ']':
        begin
          SkipChar;
          FPeeked := TAdvJSONState.jstEndArray;
          Exit(FPeeked);
        end;
      ',':
         SkipChar;
    else
      raise EUnterminatedArray.Create;
    end;
  end
  else
  if FPeekStack in [TAdvJSONScope.jscEmptyObject, TAdvJSONScope.jscNonEmptyObject] then
  begin
    FStack[FStackSize - 1] := TAdvJSONScope.jscDanglingName;
    if FPeekStack = TAdvJSONScope.jscNonEmptyObject then
    begin
      C := NextNonWhitespace;
      case C of
        '}':
          begin
            SkipChar;
            FPeeked := TAdvJSONState.jstEndObject;
            Exit(FPeeked);
          end;
        ',': SkipChar;
      else
        raise EUnterminatedObject.Create;
      end;
    end;
    C := NextNonWhitespace;
    case C of
      '"':
        begin
          SkipChar;
          FPeeked := TAdvJSONState.jstDoubleQuotedName;
          Exit(FPeeked);
        end;
      '}':
        if FPeekStack <> TAdvJSONScope.jscNonEmptyObject then
        begin
          SkipChar;
          FPeeked := TAdvJSONState.jstEndObject;
          Exit(FPeeked);
        end else
          raise ENameExpected.Create;
    else
      raise ENameExpected.Create;
    end;
  end
  else
  if FPeekStack = TAdvJSONScope.jscDanglingName then
  begin
    FStack[FStackSize - 1] := TAdvJSONScope.jscNonEmptyObject;
    C := NextNonWhitespace;
    if C = ':' then
      SkipChar
    else
      raise EColonExpected.Create;
  end
  else
  if FPeekStack = TAdvJSONScope.jscEmptyDocument then
    FStack[FStackSize - 1] := TAdvJSONScope.jscNonEmptyDocument
  else
  if FPeekStack = TAdvJSONScope.jscNonEmptyDocument then
  begin
    if SkipWhitespaceUntilEnd then
    begin
      FPeeked := TAdvJSONState.jstEOF;
      Exit(FPeeked);
    end else
      raise EMultipleRootNotAllowed.Create;
  end;

  C := NextNonWhitespace;
  case C of
    ']':
      if FPeekStack = TAdvJSONScope.jscEmptyArray then
      begin
        SkipChar;
        FPeeked := TAdvJSONState.jstEndArray;
        Exit(FPeeked);
      end else
        raise EExpectedValue.Create;
    '"':
      begin
        if FStackSize = 1 then
          raise EObjectOrArrayExpected.Create;
        SkipChar;
        FPeeked := TAdvJSONState.jstDoubleQuoted;
        Exit(FPeeked);
      end;
    '[':
      begin
        SkipChar;
        FPeeked := TAdvJSONState.jstBeginArray;
        Exit(FPeeked);
      end;
    '{':
      begin
        SkipChar;
        FPeeked := TAdvJSONState.jstBeginObject;
        Exit(FPeeked);
      end;
  end;

  if FStackSize = 1 then
    raise EObjectOrArrayExpected.Create;

  Result := PeekKeyword;
  if Result <> TAdvJSONState.jstNone then
    Exit;
  Result := PeekNumber;
  if Result <> TAdvJSONState.jstNone then
    Exit;
  raise EExpectedValue.Create;
end;

function TAdvJSONReader.SkipWhitespaceUntilEnd: boolean;
var
  s: Char;
  p: Int64;
begin
  p := FReader.FReadStream.Position;
  Result := True;
  s := ReadChar;
  repeat
    if (s > #32) or not (Ord(s) in Wspace) then
    begin
      FReader.FReadStream.Position := p;
      Exit(false);
    end;

    s := ReadChar;
  until FReader.Eof;
  FReader.FReadStream.Position := p;
end;

procedure TAdvJSONReader.CheckState(const State: TAdvJSONState);
begin
  if NextPeek <> State then
    raise EInvalidStateException.Create(State);
end;

procedure TAdvJSONReader.ReadBeginArray;
begin
  CheckState(TAdvJSONState.jstBeginArray);
  PushScope(TAdvJSONScope.jscEmptyArray);
  FPeeked := TAdvJSONState.jstNone;
end;

procedure TAdvJSONReader.ReadEndArray;
begin
  CheckState(TAdvJSONState.jstEndArray);
  Dec(FStackSize);
  FPeeked := TAdvJSONState.jstNone;
end;

function TAdvJSONReader.ReadBoolean: boolean;
begin
  case NextPeek of
    TAdvJSONState.jstTrue:
      Result := True;
    TAdvJSONState.jstFalse:
      Result := False;
  else
    raise EInvalidStateException.Create(TAdvJSONState.jstTrue);
  end;
  FPeeked := TAdvJSONState.jstNone;
end;

function TAdvJSONReader.ReadChar: Char;
begin
  Result := FReader.ReadChar;
end;

function TAdvJSONReader.ReadDouble: double;
begin
  case NextPeek of
    TAdvJSONState.jstInt64:
      begin
        FPeeked := TAdvJSONState.jstNone;
        Exit(FPeekedInt64);
      end;
    TAdvJSONState.jstNumber:
      begin
        if TryStrToFloat(ArrayOfCharToString(FPeekedNumber), Result) then
        begin
          FPeeked := TAdvJSONState.jstNone;
          Exit;
        end else
          FPeekedString := ArrayOfCharToString(FPeekedNumber);
      end;
    TAdvJSONState.jstDoubleQuoted:
      FPeekedString := ReadQuoted;
    TAdvJSONState.jstBuffered: ;
  else
    raise EInvalidStateException.Create(TAdvJSONState.jstNumber);
  end;

  FPeeked := TAdvJSONState.jstBuffered;
  Result := StrToFloat(FPeekedString);
  FPeekedString := '';
  FPeeked := TAdvJSONState.jstNone;
end;

function TAdvJSONReader.ReadInt64: Int64;
var
  AsDouble: double;
begin
  case NextPeek of
    TAdvJSONState.jstInt64:
      begin
        FPeeked := TAdvJSONState.jstNone;
        Exit(FPeekedInt64);
      end;
    TAdvJSONState.jstNumber:
      begin
        if TryStrToInt64(ArrayOfCharToString(FPeekedNumber), Result) then
        begin
          FPeeked := TAdvJSONState.jstNone;
          Exit;
        end else
          FPeekedString := ArrayOfCharToString(FPeekedNumber);
      end;
    TAdvJSONState.jstDoubleQuoted:
      begin
        FPeekedString := ReadQuoted;
        if TryStrToInt64(FPeekedString, Result) then
        begin
          FPeeked := TAdvJSONState.jstNone;
          Exit;
        end;
      end;
    TAdvJSONState.jstBuffered: ;
  else
    raise EInvalidStateException.Create(TAdvJSONState.jstInt64);
  end;

  FPeeked := TAdvJSONState.jstBuffered;
  AsDouble := StrToFloat(FPeekedString);
  Result := Round(AsDouble);
  if AsDouble <> Result then
    raise EInvalidStateException.Create(TAdvJSONState.jstInt64);
  FPeekedString := '';
  FPeeked := TAdvJSONState.jstNone;
end;

function TAdvJSONReader.ReadInteger: integer;
var
  AsDouble: double;
begin
  case NextPeek of
    TAdvJSONState.jstInt64:
      begin
        Result := Integer(FPeekedInt64);
        if Result <> FPeekedInt64 then
          raise EInvalidStateException.Create(TAdvJSONState.jstInt64);
        FPeeked := TAdvJSONState.jstNone;
        Exit;
      end;
    TAdvJSONState.jstNumber:
      begin
        if TryStrToInt(ArrayOfCharToString(FPeekedNumber), Result) then
        begin
          FPeeked := TAdvJSONState.jstNone;
          Exit;
        end else
          FPeekedString := ArrayOfCharToString(FPeekedNumber);
      end;
    TAdvJSONState.jstDoubleQuoted:
      begin
        FPeekedString := ReadQuoted;
        if TryStrToInt(FPeekedString, Result) then
        begin
          FPeeked := TAdvJSONState.jstNone;
          Exit;
        end;
      end;
    TAdvJSONState.jstBuffered: ;
  else
    raise EInvalidStateException.Create(TAdvJSONState.jstInt64); // todo
  end;

  FPeeked := TAdvJSONState.jstBuffered;
  AsDouble := StrToFloat(FPeekedString);
  Result := Round(AsDouble);
  if AsDouble <> Result then
    raise EInvalidStateException.Create(TAdvJSONState.jstInt64);
  FPeekedString := '';
  FPeeked := TAdvJSONState.jstNone;
end;

function TAdvJSONReader.ReadName: string;
begin
  CheckState(TAdvJSONState.jstDoubleQuotedName);
  FPeeked := TAdvJSONState.jstNone;
  Result := ReadQuoted;
end;

procedure TAdvJSONReader.ReadNull;
begin
  CheckState(TAdvJSONState.jstNull);
  FPeeked := TAdvJSONState.jstNone;
end;

procedure TAdvJSONReader.ReadBeginObject;
begin
  CheckState(TAdvJSONState.jstBeginObject);
  PushScope(TAdvJSONScope.jscEmptyObject);
  FPeeked := TAdvJSONState.jstNone;
end;

procedure TAdvJSONReader.ReadEndObject;
begin
  CheckState(TAdvJSONState.jstEndObject);
  Dec(FStackSize);
  FPeeked := TAdvJSONState.jstNone;
end;

function TAdvJSONReader.ReadQuoted: string;
begin
  Result := InternalReadQuoted(true);
end;

function TAdvJSONReader.InternalReadQuoted(const BuildString: boolean): string;
var
  c: String;
  s: string;
begin
  Result := '';
  s := '';
  while not FReader.Eof do
  begin
    c := ReadChar;
    if (c = '"') then
    begin
      if BuildString then
        Result := s;
      Break;
    end
    else
      s := s + c;
  end;
  if BuildString then
    Result := s;
end;

function TAdvJSONReader.ReadString: string;
begin
  case NextPeek of
    TAdvJSONState.jstDoubleQuoted:
      Result := ReadQuoted;
    TAdvJSONState.jstInt64:
      Result := IntToStr(FPeekedInt64);
    TAdvJSONState.jstNumber:
      Result := ArrayOfCharToString(FPeekedNumber);
    TAdvJSONState.jstBuffered:
      Result := FPeekedString;
  else
    raise EInvalidStateException.Create(TAdvJSONState.jstDoubleQuoted);
  end;
  FPeeked := TAdvJSONState.jstNone;

  Result := TAdvUtils.UnescapeString(Result);
end;

{ TAdvJSONStreamReader }

constructor TAdvJSONStreamReader.Create(const aStream: TStream);
begin
  FStream := aStream;
  FReadStream := TStringStream.Create('');
  FReadStream.CopyFrom(FStream, FStream.Size);
  FReadStream.Position := 0;
end;

destructor TAdvJSONStreamReader.Destroy;
begin
  FReadStream.Free;
  inherited;
end;

function TAdvJSONStreamReader.NextChar: char;
begin
  if (Eof) then
    raise EEndOfInputReached.Create;
  Result := ReadChar;
end;

function TAdvJSONStreamReader.PeekChar: char;
var
  p: Int64;
begin
  p := FReadStream.Position;
  Result := ReadChar;
  FReadStream.Position := p;
end;

function TAdvJSONStreamReader.ReadChar: char;
var
  i: Integer;
begin
  {$IFDEF DELPHI_LLVM}
  i := 0;
  {$ELSE}
  i := 1;
  {$ENDIF}
  if FReadStream.Position < FReadStream.Size then
    Result := FReadStream.ReadString(1)[i]
  else
    Result := #0;
end;

procedure TAdvJSONStreamReader.Backup(const c: char);
begin
  FReadStream.Position := FReadStream.Position - 1;
end;

procedure TAdvJSONStreamReader.MoveNext(const Count: integer = 1);
begin
  FReadStream.Position := FReadStream.Position + Count;
end;

function TAdvJSONStreamReader.Eof: boolean;
begin
  Result := FReadStream.Position = FReadStream.Size;
end;

{ TAdvJSONReader.EInvalidStateException }

constructor TAdvJSONReader.EInvalidStateException.Create(const AState: TAdvJSONState);
begin
  inherited CreateFmt('Invalid Json parser state. Expected state: %d', [Ord(AState)]);
end;

{ TAdvJSONReader.EUnterminatedArray }

constructor TAdvJSONReader.EUnterminatedArray.Create;
begin
  inherited Create('Unterminated array');
end;

{ TAdvJSONReader.EUnterminatedObject }

constructor TAdvJSONReader.EUnterminatedObject.Create;
begin
  inherited Create('Unterminated object');
end;

{ TAdvJSONReader.ENameExpected }

constructor TAdvJSONReader.ENameExpected.Create;
begin
  inherited Create('Name expected');
end;

{ TAdvJSONReader.EColonExpected }

constructor TAdvJSONReader.EColonExpected.Create;
begin
  inherited Create('Colon expected');
end;

{ TAdvJSONReader.EReaderClosed }

constructor TAdvJSONReader.EReaderClosed.Create;
begin
  inherited Create('Reader already closed');
end;

{ TAdvJSONReader.EMultipleRootNotAllowed }

constructor TAdvJSONReader.EMultipleRootNotAllowed.Create;
begin
  inherited Create('Multiple root values not allowed');
end;

{ TAdvJSONReader.EExpectedValue }

constructor TAdvJSONReader.EExpectedValue.Create;
begin
  inherited Create('Value expected but invalid character found');
end;

{ TAdvJSONReader.EObjectOrArrayExpected }

constructor TAdvJSONReader.EObjectOrArrayExpected.Create;
begin
  inherited Create('Object or array expected as top-level value');
end;

{ TAdvJSONReader.ETooManyDepthLevels }

constructor TAdvJSONReader.ETooManyDepthLevels.Create;
begin
  inherited Create('Maximum level of nested structured reached.');
end;

{ TAdvJSONStreamReader.EInvalidJsonInput }

constructor TAdvJSONStreamReader.EInvalidJsonInput.Create;
begin
  inherited Create('Invalid JSON Input');
end;

{ TAdvJSONStreamReader.EInternalError }

constructor TAdvJSONStreamReader.EInternalError.Create;
begin
  inherited Create('JSON stream reader internal error');
end;

{ TAdvJSONStreamReader.EEndOfInputReached }

constructor TAdvJSONStreamReader.EEndOfInputReached.Create;
begin
  inherited Create('End of JSON input reached.');
end;

{ TAdvJSONReader.EInvalidEscaped }

constructor TAdvJSONReader.EInvalidEscaped.Create;
begin
  inherited Create('Invalid escaped sequence');
end;

end.
