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

unit AdvPDFLib.General.Win;

{$I TMSDEFS.INC}

interface

uses
  Classes, Types, AdvPDFCoreLibBase,
  Graphics
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  TAdvGeneralPDFLibFontMetrics = record
    CapHeight: Integer;
    Ascent: Integer;
    Descent: Integer;
    FontBox: TRect;
    ItalicAngle: Integer;
    Fixed: Boolean;
    TrueType: Boolean;
  end;

  TAdvGeneralPDFLibInitializer = class
  private
    {$IFDEF MSWINDOWS}
    FOldFontObject: HGDIOBJ;
    FDocumentHandle: HDC;
    {$ENDIF}
  public
    constructor Create;
    procedure InitializeFontFallBackList(AList: TStrings);
    destructor Destroy; override;
  end;

  TAdvGeneralPDFLibFontInitializer = class
  private
    {$IFDEF MSWINDOWS}
    FFontDC: HFONT;
    FLogFont: TLogFont;
    FTTFCreatePackage: Boolean;
    FTTFSize: Cardinal;
    FTTFData: AnsiString;
    FTTFDataStream: TStringStream;
    FTTFlags: Word;
    FTTCIndex: Cardinal;
    {$ENDIF}
    FBase: string;
    FSize: Single;
    FStyle: TFontStyles;
    FUsedCharArray: TAdvPDFGraphicsLibUsedFontCharArray;
    FCharArray: TAdvPDFGraphicsLibFontCharArray;
    FCharWidths: TAdvPDFGraphicsLibFontCharWidths;
    FUnitsPerEm: Cardinal;
    FMainInitializer: TAdvGeneralPDFLibInitializer;
    FIsFixedWidth: Boolean;
  protected
    {$IFDEF MSWINDOWS}
    procedure InternalInitializeCharWidths(ADocumentHandle: HDC);
    function GetFontTableData(ADocHandle: HDC; ATableName: AnsiString; var AData: TWordDynArray): Pointer;
    function CompressFontString(AValue: AnsiString): TStringStream;
    {$ENDIF}
  public
    constructor Create(const AMainInitializer: TAdvGeneralPDFLibInitializer; const ABase: string; const {%H-}AStyle: TFontStyles; const ASize: Single);
    destructor Destroy; override;
    function GetFontMetrics: TAdvGeneralPDFLibFontMetrics;
    function GetCharArray: TAdvPDFGraphicsLibFontCharArray;
    function GetCharWidths: TAdvPDFGraphicsLibFontCharWidths;
    function GetUsedCharArray: TAdvPDFGraphicsLibUsedFontCharArray;
    function GetUnitsPerEm: Cardinal;
    function GetTTFDataLength: Integer;
    function GetTTFDataCompressedLength: Int64;
    function GetTTFDataCompressed: TStringStream;
    procedure CompressTTFData;
    procedure InitializeCharWidths;
    procedure InitializeFontFile;
    property IsFixedWidth: Boolean read FIsFixedWidth write FIsFixedWidth;
  end;

implementation

uses
  SysUtils, {%H-}Math
  {$IFNDEF LCLLIB}
  ,zlib
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,{%H-}zstream
  {$ENDIF}
  ;

{$IFDEF MSWINDOWS}
const
  TTFCFP_MS_PLATFORMID = 3;
  TTFCFP_UNICODE_CHAR_SET = 1;

  TTFCFP_FLAGS_SUBSET = 1;
  TTFMFP_SUBSET = 0;
  TTFCFP_FLAGS_TTC = 4;
  TTCF_TABLE = $66637474;

{$IFDEF LCLLIB}
type
  TCreateFontPackage = function(
{$ENDIF}
{$IFNDEF LCLLIB}
var
  CreateFontPackage: function(
{$ENDIF}
  puchSrcBuffer: pointer; ulSrcBufferSize: cardinal;
    var puchFontPackageBuffer: PAnsiChar;
    var pulFontPackageBufferSize: cardinal; var pulBytesWritten: cardinal;
    usFlags, usTTCIndex, usSubsetFormat, usSubsetLanguage, usSubsetPlatform,
    usSubsetEncoding: word; pusSubsetKeepList: PWordArray;
    usSubsetKeepListCount: word; lpfnAllocate, lpfnReAllocate, lpfnFree,
    reserved: pointer): cardinal; cdecl;

var
  FontSubHandle: THandle = INVALID_HANDLE_VALUE;
  {$IFDEF LCLLIB}
  CreateFontPackage: TCreateFontPackage;
  {$ENDIF}

{ TAdvGeneralPDFLibFontInitializer }

function TAdvGeneralPDFLibFontInitializer.CompressFontString(AValue: AnsiString): TStringStream;
var
  vDest: TStringStream;
  vSource: TStream;
  vCompressor: TCompressionStream;
begin
  vDest := TStringStream.Create('');
  try
  	vCompressor := TCompressionStream.Create(clMax, vDest);
    try
      vSource := TStringStream.Create(AValue);
      try
        vCompressor.CopyFrom(vSource, 0);
      finally
        vSource.Free;
      end;
    finally
      vCompressor.Free;
    end;

    vDest.Position := 0;
    Result := vDest;
  finally
  end;
end;

{$ENDIF}
procedure TAdvGeneralPDFLibFontInitializer.CompressTTFData;
begin
  {$IFDEF MSWINDOWS}
  FTTFDataStream := CompressFontString(FTTFData);
  {$ENDIF}
end;

constructor TAdvGeneralPDFLibFontInitializer.Create(const AMainInitializer: TAdvGeneralPDFLibInitializer; const ABase: string; const AStyle: TFontStyles; const ASize: Single);
{$IFDEF MSWINDOWS}
  function CodePageToCharSet(CodePage: Cardinal): Integer;
  begin
    case CodePage of
    932:  result := SHIFTJIS_CHARSET;
    949:  result := HANGEUL_CHARSET;
    936:  result := GB2312_CHARSET;
    1255: result := HEBREW_CHARSET;
    1256: result := ARABIC_CHARSET;
    1253: result := GREEK_CHARSET;
    1254: result := TURKISH_CHARSET;
    1258: result := 163;//VIETNAMESE_CHARSET;
    874:  result := THAI_CHARSET;
    1250: result := EASTEUROPE_CHARSET;
    1251: result := RUSSIAN_CHARSET;
    1257: result := BALTIC_CHARSET;
    else
      result := ANSI_CHARSET;
    end;
  end;
var
  Buffer: array [0..6] of Char;
  I: Integer;
 {$ENDIF}
begin
  FMainInitializer := AMainInitializer;
  FBase := ABase;
  FSize := ASize;
  FStyle := AStyle;
  FUsedCharArray := TAdvPDFGraphicsLibUsedFontCharArray.Create;
  {$IFDEF MSWINDOWS}
  FTTFCreatePackage := False;
  FTTFData := '';
  FTTFSize := 0;

  FillChar(FLogFont, SizeOf(FLogFont), 0);

  FLogFont.lfHeight := -1000;

  if TFontStyle.fsBold in AStyle then
    FLogFont.lfWeight := FW_BOLD
  else
    FLogFont.lfWeight := FW_NORMAL;

  FLogFont.lfItalic := Byte(TFontStyle.fsItalic in AStyle);
  FLogFont.lfUnderline := Byte(TFontStyle.fsUnderline in AStyle);
  FLogFont.lfStrikeOut := Byte(TFontStyle.fsStrikeOut in AStyle);
  GetLocaleInfo(SysLocale.DefaultLCID, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  FLogFont.lfCharSet := CodePageToCharSet(StrToIntDef(Buffer, GetACP));

  for I := 1 to Length(ABase) do
    FLogFont.lfFaceName[I - 1] := ABase[I];

  FFontDC := CreateFontIndirect(FLogFont);
  {$ENDIF}
end;

destructor TAdvGeneralPDFLibFontInitializer.Destroy;
begin
  if Assigned(FUsedCharArray) then
  begin
    FUsedCharArray.Free;
    FUsedCharArray := nil;
  end;
  {$IFDEF MSWINDOWS}
  DeleteObject(FFontDC);
  {$ENDIF}
  inherited;
end;

function TAdvGeneralPDFLibFontInitializer.GetCharArray: TAdvPDFGraphicsLibFontCharArray;
begin
  Result := FCharArray;
end;

function TAdvGeneralPDFLibFontInitializer.GetCharWidths: TAdvPDFGraphicsLibFontCharWidths;
begin
  Result := FCharWidths;
end;

function TAdvGeneralPDFLibFontInitializer.GetFontMetrics: TAdvGeneralPDFLibFontMetrics;
{$IFDEF MSWINDOWS}
var
  tm: TTextMetric;
  otm: POutlineTextmetric;
  sz: Integer;
{$ENDIF}
begin
  Result.Ascent := 0;
  Result.CapHeight := 0;
  Result.Descent := 0;
  Result.FontBox := Bounds(0, 0, 0, 0);
  Result.ItalicAngle := 0;
  Result.Fixed := False;
  Result.TrueType := False;
  {$IFDEF MSWINDOWS}
  if Assigned(FMainInitializer) then
  begin
    if FMainInitializer.FOldFontObject <> 0 then
      SelectObject(FMainInitializer.FDocumentHandle, FMainInitializer.FOldFontObject);

    FMainInitializer.FOldFontObject := SelectObject(FMainInitializer.FDocumentHandle, FFontDC);
    GetTextMetrics(FMainInitializer.FDocumentHandle, {%H-}tm);
    sz := GetOutlineTextMetrics(FMainInitializer.FDocumentHandle, SizeOf(TOutlineTextMetric), nil);
    GetMem(otm, sz);
    if Assigned(otm) then
    begin
      otm^.otmSize := sz;
      GetOutlineTextMetrics(FMainInitializer.FDocumentHandle, sz, otm);
      Result.Ascent := otm^.otmAscent;
      Result.CapHeight := otm^.otmsCapEmHeight;
      Result.ItalicAngle := otm^.otmItalicAngle;
      Result.Descent := otm^.otmDescent;
      Result.FontBox := otm^.otmrcFontBox;
      Result.Fixed := tm.tmPitchAndFamily and TMPF_FIXED_PITCH = 0;
      Result.TrueType := ((tm.tmPitchAndFamily and $0F) and TMPF_TRUETYPE) <> 0;
      Dispose(otm);
    end;
  end;
  {$ENDIF}
end;

procedure TAdvGeneralPDFLibFontInitializer.InitializeCharWidths;
begin
  {$IFDEF MSWINDOWS}
  if Assigned(FMainInitializer) then
    InternalInitializeCharWidths(FMainInitializer.FDocumentHandle);
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
function lpfnAllocate(Size: integer): pointer; cdecl;
begin
  GetMem(Result, Size);
end;

function lpfnReAllocate(Buffer: pointer; Size: integer): pointer; cdecl;
begin
  ReallocMem(Buffer, Size);
  Result := Buffer;
end;

procedure lpfnFree(Buffer: pointer); cdecl;
begin
  FreeMem(Buffer);
end;

{$ENDIF}

procedure TAdvGeneralPDFLibFontInitializer.InitializeFontFile;
{$IFDEF MSWINDOWS}
var
  used: TAdvPDFGraphicsLibFontCharArray;
  I: Integer;
  v: Integer;
  subdt: PAnsiChar;
  subMem: cardinal;
  subSize: cardinal;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if FTTFCreatePackage then
  begin
    if FontSubHandle = INVALID_HANDLE_VALUE then
    begin
      FontSubHandle := SafeLoadLibrary('FontSub.dll');
      if FontSubHandle <> 0 then
      begin
        {$IFDEF LCLLIB}
        CreateFontPackage := TCreateFontPackage(GetProcAddress(FontSubHandle, 'CreateFontPackage'));
        {$ENDIF}
        {$IFNDEF LCLLIB}
        CreateFontPackage := GetProcAddress(FontSubHandle, 'CreateFontPackage');
        {$ENDIF}
      end;
    end;

    if (FontSubHandle <> 0) and (@CreateFontPackage <> nil) then
    begin
      used.C := 0;
      for I := 0 to FUsedCharArray.Count - 1 do
      begin
        v := FUsedCharArray[I];
        used.Add(v);
      end;

      subdt := nil;
      submem := 0;
      subSize := 0;

      if CreateFontPackage(pointer(FTTFData), FTTFSize,
          subdt, subMem, subSize,
          FTTFlags, FTTCIndex ,TTFMFP_SUBSET,0,
          TTFCFP_MS_PLATFORMID,TTFCFP_UNICODE_CHAR_SET,
          pointer(Used.v),Used.c,
          @lpfnAllocate,@lpfnReAllocate,@lpfnFree,nil)=0 then
      begin
        SetString(FTTFData, subdt, subSize);
        FreeMem(subdt);
      end;
    end;
  end;
  {$ENDIF}
end;

function TAdvGeneralPDFLibFontInitializer.GetTTFDataCompressed: TStringStream;
begin
  {$IFDEF MSWINDOWS}
  Result := FTTFDataStream;
  {$ENDIF}
  {$IFNDEF MSWINDOWS}
  Result := nil;
  {$ENDIF}
end;

function TAdvGeneralPDFLibFontInitializer.GetTTFDataCompressedLength: Int64;
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  if Assigned(FTTFDataStream) then
    Result := FTTFDataStream.Size;
  {$ENDIF}
end;

function TAdvGeneralPDFLibFontInitializer.GetTTFDataLength: Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := Length(FTTFData);
  {$ENDIF}
  {$IFNDEF MSWINDOWS}
  Result := 0;
  {$ENDIF}
end;

function TAdvGeneralPDFLibFontInitializer.GetUnitsPerEm: Cardinal;
begin
  Result := FUnitsPerEm;
end;

function TAdvGeneralPDFLibFontInitializer.GetUsedCharArray: TAdvPDFGraphicsLibUsedFontCharArray;
begin
  Result := FUsedCharArray;
end;

{$IFDEF MSWINDOWS}
function TAdvGeneralPDFLibFontInitializer.GetFontTableData(ADocHandle: HDC;
  ATableName: AnsiString; var AData: TWordDynArray): Pointer;
var
  e: Cardinal;
  a: PAnsiChar;
  m: PCardinal;
  r: Pointer;
  procedure SwapBuf(P: PWordArray; PLen: Integer);
  var i: integer;
  begin
    for i := 0 to PLen-1 do
      P^[i] := Swap(P^[i]);
  end;

begin
  Result := nil;
  a := PAnsiChar(ATableName);
  m := PCardinal(a);
  e := GetFontData(ADocHandle, m^, 0, nil, 0);
  if not (e = GDI_ERROR) then
  begin
    SetLength(AData, e shr 1 + 1);
    r := Pointer(AData);
    if not (GetFontData(ADocHandle, m^, 0, r, e) = GDI_ERROR) then
    begin
      SwapBuf(r, e shr 1);
      Result := r;
    end;
  end;
end;

procedure TAdvGeneralPDFLibFontInitializer.InternalInitializeCharWidths(
  ADocumentHandle: HDC);
type
  TAdvGeneralPDFLibFontCmapHead = packed record
    v: longint;
    fr: longint;
    csa: cardinal;
    mn: cardinal;
    fl: word;
    uem: word;
    cd: Int64;
    md: Int64;
    xmin: SmallInt;
    ymin: SmallInt;
    xmax: SmallInt;
    ymax: SmallInt;
    ms: word;
    lr: word;
    fd: SmallInt;
    ilf: SmallInt;
    gdf: SmallInt;
  end;

  TAdvGeneralPDFLibFontCmapArray = packed array[byte] of packed record
    pID: word;
    psID: word;
    o: Cardinal;
  end;

  TAdvGeneralPDFLibFontCmapHeader = packed record
    v: word;
    n: word;
  end;

  TAdvGeneralPDFLibFontCmapFormat4 = packed record
    fmt: word;
    l: word;
    lng: word;
    sgX2: word;
    shrange: word;
    selr: word;
    rng: word;
  end;

  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[Byte] of SmallInt;

  function GetBit(const Bits; aIndex: Integer): Boolean;
  begin
    Result := PIntegerArray(@Bits)^[aIndex shr 5] and (1 shl (aIndex and 31)) <> 0;
  end;

var
  fd, hh: pointer;
  i, n, c, ndx: Integer;
  off: Integer;
  o: Integer;
  gidx: integer;
  id, gi: Integer;
  wl, nol: word;
  st: ^TAdvGeneralPDFLibFontCmapArray absolute fd;
  h: ^TAdvGeneralPDFLibFontCmapHeader;
  fdcm, fdhd, fdht, fdhh: TWordDynArray;
  ft: ^TAdvGeneralPDFLibFontCmapFormat4;
  ec, sc: PWordArray;
  ida: PSmallIntArray;
  idr: PWordArray;
  gia: PWordArray;
  hd: ^TAdvGeneralPDFLibFontCmapHead;
  uem: Cardinal;
  W: packed array of TABC;
  df: Integer;
  ttcNumFonts: Longword;
  ttcBytes: array of byte;
  ttidx: Word;
  tableTag: LongWord;

  function GetTTCIndex(const FontName: AnsiString; var ttcIndex: Word; const FontCount: LongWord): Boolean;
  const
    BATANG_KO = #48148#53461;
    BATANGCHE_KO = BATANG_KO + #52404;
    GUNGSUH_KO = #44417#49436;
    GUNGSUHCHE_KO = GUNGSUH_KO + #52404;
    GULIM_KO = #44404#47548;
    GULIMCHE_KO = GULIM_KO + #52404;
    DOTUM_KO = #46027#50880;
    DOTUMCHE_KO = DOTUM_KO + #52404;
    MINGLIU_CH = #32048#26126#39636;
    PMINGLIU_CH = #26032 + MINGLIU_CH;
    MINGLIU_HK_CH = MINGLIU_CH + '_hkscs';
    MINGLIU_XB_CH = MINGLIU_CH + '-extb';
    PMINGLIU_XB_CH = PMINGLIU_CH + '-extb';
    MINGLIU_XBHK_CH = MINGLIU_CH + '-extb_hkscs';
    MSGOTHIC_JA = #65325#65331#32#12468#12471#12483#12463;
    MSPGOTHIC_JA = #65325#65331#32#65328#12468#12471#12483#12463;
    MSMINCHO_JA = #65325#65331#32#26126#26397;
    MSPMINCHO_JA = #65325#65331#32#65328#26126#26397;
    SIMSUN_CHS = #23435#20307;
    NSIMSUN_CHS = #26032#23435#20307;
  var
    l: UnicodeString;
  begin
    result := True;
    l := UnicodeString(FontName);
    l := LowerCase(l);
    if (l = 'batang') or (l = BATANG_KO) then
      ttcIndex := 0
    else if (l = 'batangche') or (l = BATANGCHE_KO) then
      ttcIndex := 1
    else if (l = 'gungsuh') or (l = GUNGSUH_KO) then
      ttcIndex := 2
    else if (l = 'gungsuhche') or (l = GUNGSUHCHE_KO) then
      ttcIndex := 3
    else if l = 'cambria' then
      ttcIndex := 0
    else if l = 'cambria math' then
      ttcIndex := 1
    else if (l = 'gulim') or (l = GULIM_KO) then
      ttcIndex := 0
    else if (l = 'gulimche') or (l = GULIMCHE_KO) then
      ttcIndex := 1
    else if (l = 'dotum') or (l = DOTUM_KO) then
      ttcIndex := 2
    else if (l = 'dotumche') or (l = DOTUMCHE_KO) then
      ttcIndex := 3
    else if (l = 'mingliu') or (l = MINGLIU_CH) then
      ttcIndex := 0
    else if (l = 'pmingliu') or (l = PMINGLIU_CH) then
      ttcIndex := 1
    else if (l = 'mingliu_hkscs') or (l = MINGLIU_HK_CH) then
      ttcIndex := 2
    else if (l = 'mingliu-extb') or (l = MINGLIU_XB_CH) then
      ttcIndex := 0
    else if (l = 'pmingliu-extb') or (l = PMINGLIU_XB_CH) then
      ttcIndex := 1
    else if (l = 'mingliu_hkscs-extb') or (l = MINGLIU_XBHK_CH) then
      ttcIndex := 2
    else if (l = 'ms gothic') or (l = LowerCase(MSGOTHIC_JA)) then
      ttcIndex := 0
    else if (l = 'ms pgothic') or (l = LowerCase(MSPGOTHIC_JA)) then
        ttcIndex := 1
    else if l = 'ms ui gothic' then
      ttcIndex := 2
    else if (l = 'ms mincho') or (l = LowerCase(MSMINCHO_JA)) then
      ttcIndex := 0
    else if (l = 'ms pmincho') or (l = LowerCase(MSPMINCHO_JA)) then
      ttcIndex := 1
    else if (l = 'simsun') or (l = SIMSUN_CHS) then
      ttcIndex := 0
    else if (l = 'nsimsun') or (l = NSIMSUN_CHS) then
      ttcIndex := 1
    else
      result := False;

    if Result and (ttcIndex > (FontCount - 1)) then
      Result := False;
  end;

begin
  FTTFSize := GetFontData(ADocumentHandle, TTCF_TABLE,0,nil,0);

  if FTTFSize <> GDI_ERROR then
  begin
    SetLength(ttcBytes,4);
    if GetFontData(ADocumentHandle,TTCF_TABLE,8,pointer(ttcBytes),4) <> GDI_ERROR then
      ttcNumFonts := ttcBytes[3]
    else
      ttcNumFonts := 1;

    ttidx := 0;
    if (ttcNumFonts < 2) or not GetTTCIndex(AnsiString(FBase), ttidx,ttcNumFonts) then
      FTTCIndex := 0;

    FTTFlags := TTFCFP_FLAGS_SUBSET or TTFCFP_FLAGS_TTC;
    tableTag := TTCF_TABLE;
  end
  else
  begin
    FTTFSize := GetFontData(ADocumentHandle, 0, 0, nil, 0);
    FTTFlags := TTFCFP_FLAGS_SUBSET;
    FTTCIndex := 0;
    tableTag := 0;
  end;

  if FTTFSize <> GDI_ERROR then
  begin
    SetLength(FTTFData, FTTFSize);
    if GetFontData(ADocumentHandle, tableTag, 0, pointer(FTTFData), FTTFSize) <> GDI_ERROR then
      FTTFCreatePackage := True;
  end;

  fdcm := nil;
  fd := GetFontTableData(ADocumentHandle, 'cmap', fdcm);
  if Assigned(fd) then
  begin
    h := fd;
    {$IFDEF LCLLIB}
    inc({%H-}PtrUInt(fd), SizeOf(TAdvGeneralPDFLibFontCmapHeader));
    {$ENDIF}
    {$IFNDEF LCLLIB}
    inc(NativeInt(fd), SizeOf(TAdvGeneralPDFLibFontCmapHeader));
    {$ENDIF}
    off := 0;
    for i := 0 to h^.n - 1 do
    begin
      if st^[i].pID = 3 then
      begin
        if st^[i].psID = 0 then
          off := st^[i].o
        else if st^[i].psID = 1 then
        begin
          off := st^[i].o;
          Break;
        end;
      end;
    end;

    if (off <> 0) and (off and 1 = 0) then
    begin
      i := LongRec(off).Lo;
      LongRec(off).Lo := LongRec(off).Hi;
      LongRec(off).Hi := i;

      if off <= Length(fdcm) * 2 then
      begin
        {$IFDEF LCLLIB}
        ft := {%H-}Pointer({%H-}PtrUInt(fdcm) + off);
        {$ENDIF}
        {$IFNDEF LCLLIB}
        ft := Pointer(Integer(fdcm) + off);
        {$ENDIF}

        if ft^.fmt = 4 then
        begin
          {$IFDEF LCLLIB}
          ec := {%H-}pointer({%H-}PtrUInt(@ft^.fmt) + SizeOf(TAdvGeneralPDFLibFontCmapFormat4));
          sc := {%H-}pointer({%H-}PtrUInt(ec) + ft^.sgX2 + 2);
          ida := {%H-}pointer({%H-}PtrUInt(sc) + ft^.sgX2);
          idr := {%H-}Pointer({%H-}PtrUInt(ida) + ft^.sgX2);
          gia := {%H-}Pointer({%H-}PtrUInt(idr) + ft^.sgX2);
          {$ENDIF}
          {$IFNDEF LCLLIB}
          ec := pointer(Integer(@ft^.fmt) + SizeOf(TAdvGeneralPDFLibFontCmapFormat4));
          sc := pointer(Integer(ec) + ft^.sgX2 + 2);
          ida := pointer(Integer(sc) + ft^.sgX2);
          idr := Pointer(Integer(ida) + ft^.sgX2);
          gia := Pointer(Integer(idr) + ft^.sgX2);
          {$ENDIF}

          fdhd := nil;
          hd := GetFontTableData(ADocumentHandle, 'head', fdhd);
          if Assigned(hd) then
          begin
            fdht := nil;
            fd := GetFontTableData(ADocumentHandle, 'hmtx', fdht);
            if Assigned(fd) then
            begin
              fdhh := nil;
              hh := GetFontTableData(ADocumentHandle, 'hhea', fdhh);
              if Assigned(hh) then
              begin
                n := ft^.sgX2 shr 1;

                for i := 0 to n - 1 do
                  FCharArray.c := FCharArray.c + ec^[i] - sc^[i] + 1;

                SetLength(FCharArray.v, FCharArray.c);
                SetLength(FCharWidths, FCharArray.c);

                ndx := 0;
                for i := 0 to n - 1 do
                begin
                  id := ida^[i];
                  gi := idr^[i];
                  if gi <> 0 then
                    gi := gi shr 1 + i - n - sc^[i];

                  for c := sc^[i] to ec^[i] do
                  begin
                    FCharArray.v[ndx] := c;
                    if gi = 0 then
                      gidx := c + id
                    else
                    begin
                      gidx := gia^[gi + c];
                      if gidx <> 0 then
                        Inc(gidx, id);
                    end;

                    FCharWidths[ndx].g := gidx;
                    inc(ndx);
                  end;
                end;

                uem := hd^.uem;

                if uem <> 0 then
                begin
                  FUnitsPerEm := uem;
                  wl := (Cardinal(fdht[0]) * 1000) div uem;
                  if IsFixedWidth then
                  begin
                    for i := 0 to FCharArray.c - 1 do
                      FCharWidths[i].w := wl
                  end
                  else
                  begin
                    nol := fdhh[17];
                    for i := 0 to FCharArray.c - 1 do
                    begin
                      if FCharWidths[i].G <> 0 then
                      begin
                        if FCharWidths[i].G <= nol then
                          FCharWidths[i].w := (cardinal(fdht[FCharWidths[i].g * 2]) * 1000) div uem
                        else
                          FCharWidths[i].w := wl;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  if Length(FCharWidths) = 0 then
  begin
    o := 32;
    SetLength(W, 224);
    SetLength(FCharWidths, Length(W) + o - 1);
    GetCharABCWidthsA(ADocumentHandle, o, 255, W[0]);
    df := Integer(W[0].abcA + integer(W[0].abcB) + W[0].abcC);

    if IsFixedWidth then
    begin
      for I := o to Length(FCharWidths) - 1 do
      begin
        FCharWidths[I].w := df;
        FCharWidths[I].g := I + o;
      end;
    end
    else
    begin
      for I := o to Length(FCharWidths) - 1 do
      begin
        FCharWidths[I].w := Integer(W[I - o].abcA + Integer(W[I - o].abcB) + W[I - o].abcC);
        FCharWidths[I].g := I + o;
      end;
    end;
  end;
end;

{$ENDIF}

{ TAdvGeneralPDFLibInitializer }

constructor TAdvGeneralPDFLibInitializer.Create;
begin
  {$IFDEF MSWINDOWS}
  FDocumentHandle := CreateCompatibleDC(0);
  SetMapMode(FDocumentHandle, MM_TEXT);
  {$ENDIF}
end;

destructor TAdvGeneralPDFLibInitializer.Destroy;
begin
  {$IFDEF MSWINDOWS}
  if FOldFontObject <> 0 then
    SelectObject(FDocumentHandle, FOldFontObject);

  DeleteDC(FDocumentHandle);
  {$ENDIF}
  inherited;
end;

procedure TAdvGeneralPDFLibInitializer.InitializeFontFallBackList(
  AList: TStrings);
begin
  AList.Add('Tahoma');
  AList.Add('SimSun');
  AList.Add('Arial Unicode MS');
end;

end.

