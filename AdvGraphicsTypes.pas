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

unit AdvGraphicsTypes;

{$I TMSDEFS.INC}

{$IFDEF CMNLIB}
{$DEFINE USECOMMONCOLORS}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE USECOMMONCOLORS}
{$DEFINE CMNWEBLIB}
{$DEFINE FMXWEBLIB}
{$ENDIF}
{$IFDEF FMXLIB}
{$DEFINE FMXWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, Types, AdvTypes, Graphics
  {$IFNDEF WEBLIB}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$ENDIF}
  ;

const
  {$IFNDEF LCLLIB}
  FPC_FULLVERSION = 0;
  {$ENDIF}
  PathScale = 1E-4;
  PathFontSize = 1E-2;
  PathPosition = 1E-3;
  PathAngle = 1E-4;

  {$IFDEF FMXLIB}
  gcAlpha = $FF000000;
  gcAliceblue = gcAlpha or $F0F8FF;
  gcAntiquewhite = gcAlpha or $FAEBD7;
  gcAqua = gcAlpha or $00FFFF;
  gcAquamarine = gcAlpha or $7FFFD4;
  gcAzure = gcAlpha or $F0FFFF;
  gcBeige = gcAlpha or $F5F5DC;
  gcBisque = gcAlpha or $FFE4C4;
  gcBlack = gcAlpha or $000000;
  gcBlanchedalmond = gcAlpha or $FFEBCD;
  gcBlue = gcAlpha or $0000FF;
  gcBlueviolet = gcAlpha or $8A2BE2;
  gcBrown = gcAlpha or $A52A2A;
  gcBurlywood = gcAlpha or $DEB887;
  gcCadetblue = gcAlpha or $5F9EA0;
  gcChartreuse = gcAlpha or $7FFF00;
  gcChocolate = gcAlpha or $D2691E;
  gcCoral = gcAlpha or $FF7F50;
  gcCornflowerblue = gcAlpha or $6495ED;
  gcCornsilk = gcAlpha or $FFF8DC;
  gcCrimson = gcAlpha or $DC143C;
  gcCyan = gcAlpha or $00FFFF;
  gcDarkblue = gcAlpha or $00008B;
  gcDarkcyan = gcAlpha or $008B8B;
  gcDarkgoldenrod = gcAlpha or $B8860B;
  gcDarkgray = gcAlpha or $A9A9A9;
  gcDarkgreen = gcAlpha or $006400;
  gcDarkgrey = gcAlpha or $A9A9A9;
  gcDarkkhaki = gcAlpha or $BDB76B;
  gcDarkmagenta = gcAlpha or $8B008B;
  gcDarkolivegreen = gcAlpha or $556B2F;
  gcDarkorange = gcAlpha or $FF8C00;
  gcDarkorchid = gcAlpha or $9932CC;
  gcDarkred = gcAlpha or $8B0000;
  gcDarksalmon = gcAlpha or $E9967A;
  gcDarkseagreen = gcAlpha or $8FBC8F;
  gcDarkslateblue = gcAlpha or $483D8B;
  gcDarkslategray = gcAlpha or $2F4F4F;
  gcDarkslategrey = gcAlpha or $2F4F4F;
  gcDarkturquoise = gcAlpha or $00CED1;
  gcDarkviolet = gcAlpha or $9400D3;
  gcDeeppink = gcAlpha or $FF1493;
  gcDeepskyblue = gcAlpha or $00BFFF;
  gcDimgray = gcAlpha or $696969;
  gcDimgrey = gcAlpha or $696969;
  gcDodgerblue = gcAlpha or $1E90FF;
  gcFirebrick = gcAlpha or $B22222;
  gcFloralwhite = gcAlpha or $FFFAF0;
  gcForestgreen = gcAlpha or $228B22;
  gcFuchsia = gcAlpha or $FF00FF;
  gcGainsboro = gcAlpha or $DCDCDC;
  gcGhostwhite = gcAlpha or $F8F8FF;
  gcGold = gcAlpha or $FFD700;
  gcGoldenrod = gcAlpha or $DAA520;
  gcGray = gcAlpha or $808080;
  gcGreen = gcAlpha or $008000;
  gcGreenyellow = gcAlpha or $ADFF2F;
  gcGrey = gcAlpha or $808080;
  gcHoneydew = gcAlpha or $F0FFF0;
  gcHotpink = gcAlpha or $FF69B4;
  gcIndianred = gcAlpha or $CD5C5C;
  gcIndigo = gcAlpha or $4B0082;
  gcIvory = gcAlpha or $FFFFF0;
  gcKhaki = gcAlpha or $F0E68C;
  gcLavender = gcAlpha or $E6E6FA;
  gcLavenderblush = gcAlpha or $FFF0F5;
  gcLawngreen = gcAlpha or $7CFC00;
  gcLemonchiffon = gcAlpha or $FFFACD;
  gcLightblue = gcAlpha or $ADD8E6;
  gcLightcoral = gcAlpha or $F08080;
  gcLightcyan = gcAlpha or $E0FFFF;
  gcLightgoldenrodyellow = gcAlpha or $FAFAD2;
  gcLightgray = gcAlpha or $D3D3D3;
  gcLightgreen = gcAlpha or $90EE90;
  gcLightgrey = gcAlpha or $D3D3D3;
  gcLightpink = gcAlpha or $FFB6C1;
  gcLightsalmon = gcAlpha or $FFA07A;
  gcLightseagreen = gcAlpha or $20B2AA;
  gcLightskyblue = gcAlpha or $87CEFA;
  gcLightslategray = gcAlpha or $778899;
  gcLightslategrey = gcAlpha or $778899;
  gcLightsteelblue = gcAlpha or $B0C4DE;
  gcLightyellow = gcAlpha or $FFFFE0;
  gcLtGray = gcAlpha or $C0C0C0;
  gcMedGray = gcAlpha or $A0A0A0;
  gcDkGray = gcAlpha or $808080;
  gcMoneyGreen = gcAlpha or $C0DCC0;
  gcLegacySkyBlue = gcAlpha or $F0CAA6;
  gcCream = gcAlpha or $F0FBFF;
  gcLime = gcAlpha or $00FF00;
  gcLimegreen = gcAlpha or $32CD32;
  gcLinen = gcAlpha or $FAF0E6;
  gcMagenta = gcAlpha or $FF00FF;
  gcMaroon = gcAlpha or $800000;
  gcMediumaquamarine = gcAlpha or $66CDAA;
  gcMediumblue = gcAlpha or $0000CD;
  gcMediumorchid = gcAlpha or $BA55D3;
  gcMediumpurple = gcAlpha or $9370DB;
  gcMediumseagreen = gcAlpha or $3CB371;
  gcMediumslateblue = gcAlpha or $7B68EE;
  gcMediumspringgreen = gcAlpha or $00FA9A;
  gcMediumturquoise = gcAlpha or $48D1CC;
  gcMediumvioletred = gcAlpha or $C71585;
  gcMidnightblue = gcAlpha or $191970;
  gcMintcream = gcAlpha or $F5FFFA;
  gcMistyrose = gcAlpha or $FFE4E1;
  gcMoccasin = gcAlpha or $FFE4B5;
  gcNavajowhite = gcAlpha or $FFDEAD;
  gcNavy = gcAlpha or $000080;
  gcOldlace = gcAlpha or $FDF5E6;
  gcOlive = gcAlpha or $808000;
  gcOlivedrab = gcAlpha or $6B8E23;
  gcOrange = gcAlpha or $FFA500;
  gcOrangered = gcAlpha or $FF4500;
  gcOrchid = gcAlpha or $DA70D6;
  gcPalegoldenrod = gcAlpha or $EEE8AA;
  gcPalegreen = gcAlpha or $98FB98;
  gcPaleturquoise = gcAlpha or $AFEEEE;
  gcPalevioletred = gcAlpha or $DB7093;
  gcPapayawhip = gcAlpha or $FFEFD5;
  gcPeachpuff = gcAlpha or $FFDAB9;
  gcPeru = gcAlpha or $CD853F;
  gcPink = gcAlpha or $FFC0CB;
  gcPlum = gcAlpha or $DDA0DD;
  gcPowderblue = gcAlpha or $B0E0E6;
  gcPurple = gcAlpha or $800080;
  gcRed = gcAlpha or $FF0000;
  gcRosybrown = gcAlpha or $BC8F8F;
  gcRoyalblue = gcAlpha or $4169E1;
  gcSaddlebrown = gcAlpha or $8B4513;
  gcSalmon = gcAlpha or $FA8072;
  gcSandybrown = gcAlpha or $F4A460;
  gcSeagreen = gcAlpha or $2E8B57;
  gcSeashell = gcAlpha or $FFF5EE;
  gcSienna = gcAlpha or $A0522D;
  gcSilver = gcAlpha or $C0C0C0;
  gcSkyblue = gcAlpha or $87CEEB;
  gcSlateblue = gcAlpha or $6A5ACD;
  gcSlategray = gcAlpha or $708090;
  gcSlategrey = gcAlpha or $708090;
  gcSnow = gcAlpha or $FFFAFA;
  gcSpringgreen = gcAlpha or $00FF7F;
  gcSteelblue = gcAlpha or $4682B4;
  gcTan = gcAlpha or $D2B48C;
  gcTeal = gcAlpha or $008080;
  gcThistle = gcAlpha or $D8BFD8;
  gcTomato = gcAlpha or $FF6347;
  gcTurquoise = gcAlpha or $40E0D0;
  gcViolet = gcAlpha or $EE82EE;
  gcWheat = gcAlpha or $F5DEB3;
  gcWhite = gcAlpha or $FFFFFF;
  gcWhitesmoke = gcAlpha or $F5F5F5;
  gcYellow = gcAlpha or $FFFF00;
  gcYellowgreen = gcAlpha or $9ACD32;
  gcNull = $00000000;
  {$ENDIF}
  {$IFDEF USECOMMONCOLORS}
  gcAliceblue = $FFF8F0;
  gcAntiquewhite = $D7EBFA;
  gcAqua = $FFFF00;
  gcAquamarine = $D4FF7F;
  gcAzure = $FFFFF0;
  gcBeige = $DCF5F5;
  gcBisque = $C4E4FF;
  gcBlack = $000000;
  gcBlanchedalmond = $CDEBFF;
  gcBlue = $FF0000;
  gcBlueviolet = $E22B8A;
  gcBrown = $2A2AA5;
  gcBurlywood = $87B8DE;
  gcCadetblue = $A09E5F;
  gcChartreuse = $00FF7F;
  gcChocolate = $1E69D2;
  gcCoral = $507FFF;
  gcCornflowerblue = $ED9564;
  gcCornsilk = $DCF8FF;
  gcCrimson = $3C14DC;
  gcCyan = $FFFF00;
  gcDarkblue = $8B0000;
  gcDarkcyan = $8B8B00;
  gcDarkgoldenrod = $0B86B8;
  gcDarkgray = $A9A9A9;
  gcDarkgreen = $006400;
  gcDarkgrey = $A9A9A9;
  gcDarkkhaki = $6BB7BD;
  gcDarkmagenta = $8B008B;
  gcDarkolivegreen = $2F6B55;
  gcDarkorange = $008CFF;
  gcDarkorchid = $CC3299;
  gcDarkred = $00008B;
  gcDarksalmon = $7A96E9;
  gcDarkseagreen = $8FBC8F;
  gcDarkslateblue = $8B3D48;
  gcDarkslategray = $4F4F2F;
  gcDarkslategrey = $4F4F2F;
  gcDarkturquoise = $D1CE00;
  gcDarkviolet = $D30094;
  gcDeeppink = $9314FF;
  gcDeepskyblue = $FFBF00;
  gcDimgray = $696969;
  gcDimgrey = $696969;
  gcDodgerblue = $FF901E;
  gcFirebrick = $2222B2;
  gcFloralwhite = $F0FAFF;
  gcForestgreen = $228B22;
  gcFuchsia = $FF00FF;
  gcGainsboro = $DCDCDC;
  gcGhostwhite = $FFF8F8;
  gcGold = $00D7FF;
  gcGoldenrod = $20A5DA;
  gcGray = $808080;
  gcGreen = $008000;
  gcGreenyellow = $2FFFAD;
  gcGrey = $808080;
  gcHoneydew = $F0FFF0;
  gcHotpink = $B469FF;
  gcIndianred = $5C5CCD;
  gcIndigo = $82004B;
  gcIvory = $F0FFFF;
  gcKhaki = $8CE6F0;
  gcLavender = $FAE6E6;
  gcLavenderblush = $F5F0FF;
  gcLawngreen = $00FC7C;
  gcLemonchiffon = $CDFAFF;
  gcLightblue = $E6D8AD;
  gcLightcoral = $8080F0;
  gcLightcyan = $FFFFE0;
  gcLightgoldenrodyellow = $D2FAFA;
  gcLightgray = $D3D3D3;
  gcLightgreen = $90EE90;
  gcLightgrey = $D3D3D3;
  gcLightpink = $C1B6FF;
  gcLightsalmon = $7AA0FF;
  gcLightseagreen = $AAB220;
  gcLightskyblue = $FACE87;
  gcLightslategray = $998877;
  gcLightslategrey = $998877;
  gcLightsteelblue = $DEC4B0;
  gcLightyellow = $E0FFFF;
  gcLtGray = $C0C0C0;
  gcMedGray = $A4A0A0;
  gcDkGray = $808080;
  gcMoneyGreen = $C0DCC0;
  gcLegacySkyBlue = $F0CAA6;
  gcCream = $F0FBFF;
  gcLime = $00FF00;
  gcLimegreen = $32CD32;
  gcLinen = $E6F0FA;
  gcMagenta = $FF00FF;
  gcMaroon = $000080;
  gcMediumaquamarine = $AACD66;
  gcMediumblue = $CD0000;
  gcMediumorchid = $D355BA;
  gcMediumpurple = $DB7093;
  gcMediumseagreen = $71B33C;
  gcMediumslateblue = $EE687B;
  gcMediumspringgreen = $9AFA00;
  gcMediumturquoise = $CCD148;
  gcMediumvioletred = $8515C7;
  gcMidnightblue = $701919;
  gcMintcream = $FAFFF5;
  gcMistyrose = $E1E4FF;
  gcMoccasin = $B5E4FF;
  gcNavajowhite = $ADDEFF;
  gcNavy = $800000;
  gcOldlace = $E6F5FD;
  gcOlive = $008080;
  gcOlivedrab = $238E6B;
  gcOrange = $00A5FF;
  gcOrangered = $0045FF;
  gcOrchid = $D670DA;
  gcPalegoldenrod = $AAE8EE;
  gcPalegreen = $98FB98;
  gcPaleturquoise = $EEEEAF;
  gcPalevioletred = $9370DB;
  gcPapayawhip = $D5EFFF;
  gcPeachpuff = $B9DAFF;
  gcPeru = $3F85CD;
  gcPink = $CBC0FF;
  gcPlum = $DDA0DD;
  gcPowderblue = $E6E0B0;
  gcPurple = $800080;
  gcRed = $0000FF;
  gcRosybrown = $8F8FBC;
  gcRoyalblue = $E16941;
  gcSaddlebrown = $13458B;
  gcSalmon = $7280FA;
  gcSandybrown = $60A4F4;
  gcSeagreen = $578B2E;
  gcSeashell = $EEF5FF;
  gcSienna = $2D52A0;
  gcSilver = $C0C0C0;
  gcSkyblue = $EBCE87;
  gcSlateblue = $CD5A6A;
  gcSlategray = $908070;
  gcSlategrey = $908070;
  gcSnow = $FAFAFF;
  gcSpringgreen = $7FFF00;
  gcSteelblue = $B48246;
  gcTan = $8CB4D2;
  gcTeal = $808000;
  gcThistle = $D8BFD8;
  gcTomato = $4763FF;
  gcTurquoise = $D0E040;
  gcViolet = $EE82EE;
  gcWheat = $B3DEF5;
  gcWhite = $FFFFFF;
  gcWhitesmoke = $F5F5F5;
  gcYellow = $00FFFF;
  gcYellowgreen = $32CD9A;
  gcNull = -1;
  {$ENDIF}

type
  {$IFDEF FMXLIB}
  TAdvGraphicsColor = TAlphaColor;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TAdvGraphicsColor = TColor;
  {$ENDIF}

  TAdvGraphicsColorEntry = record
    Value: TAdvGraphicsColor;
    Name: PChar;
  end;

const
  AdvGraphicsColorCount = 154;
  {$IFNDEF WEBLIB}
  AdvGraphicsColors: array[0..AdvGraphicsColorCount - 1] of TAdvGraphicsColorEntry = (
  (Value: gcAliceblue; Name:'gcAliceblue'),
  (Value: gcAntiquewhite; Name:'gcAntiquewhite'),
  (Value: gcAqua; Name:'gcAqua'),
  (Value: gcAquamarine; Name:'gcAquamarine'),
  (Value: gcAzure; Name:'gcAzure'),
  (Value: gcBeige; Name:'gcBeige'),
  (Value: gcBisque; Name:'gcBisque'),
  (Value: gcBlack; Name:'gcBlack'),
  (Value: gcBlanchedalmond; Name:'gcBlanchedalmond'),
  (Value: gcBlue; Name:'gcBlue'),
  (Value: gcBlueviolet; Name:'gcBlueviolet'),
  (Value: gcBrown; Name:'gcBrown'),
  (Value: gcBurlywood; Name:'gcBurlywood'),
  (Value: gcCadetblue; Name:'gcCadetblue'),
  (Value: gcChartreuse; Name:'gcChartreuse'),
  (Value: gcChocolate; Name:'gcChocolate'),
  (Value: gcCoral; Name:'gcCoral'),
  (Value: gcCornflowerblue; Name:'gcCornflowerblue'),
  (Value: gcCornsilk; Name:'gcCornsilk'),
  (Value: gcCrimson; Name:'gcCrimson'),
  (Value: gcCyan; Name:'gcCyan'),
  (Value: gcDarkblue; Name:'gcDarkblue'),
  (Value: gcDarkcyan; Name:'gcDarkcyan'),
  (Value: gcDarkgoldenrod; Name:'gcDarkgoldenrod'),
  (Value: gcDarkgray; Name:'gcDarkgray'),
  (Value: gcDarkgreen; Name:'gcDarkgreen'),
  (Value: gcDarkgrey; Name:'gcDarkgrey'),
  (Value: gcDarkkhaki; Name:'gcDarkkhaki'),
  (Value: gcDarkmagenta; Name:'gcDarkmagenta'),
  (Value: gcDarkolivegreen; Name:'gcDarkolivegreen'),
  (Value: gcDarkorange; Name:'gcDarkorange'),
  (Value: gcDarkorchid; Name:'gcDarkorchid'),
  (Value: gcDarkred; Name:'gcDarkred'),
  (Value: gcDarksalmon; Name:'gcDarksalmon'),
  (Value: gcDarkseagreen; Name:'gcDarkseagreen'),
  (Value: gcDarkslateblue; Name:'gcDarkslateblue'),
  (Value: gcDarkslategray; Name:'gcDarkslategray'),
  (Value: gcDarkslategrey; Name:'gcDarkslategrey'),
  (Value: gcDarkturquoise; Name:'gcDarkturquoise'),
  (Value: gcDarkviolet; Name:'gcDarkviolet'),
  (Value: gcDeeppink; Name:'gcDeeppink'),
  (Value: gcDeepskyblue; Name:'gcDeepskyblue'),
  (Value: gcDimgray; Name:'gcDimgray'),
  (Value: gcDimgrey; Name:'gcDimgray'),
  (Value: gcDodgerblue; Name:'gcDodgerblue'),
  (Value: gcFirebrick; Name:'gcFirebrick'),
  (Value: gcFloralwhite; Name:'gcFloralwhite'),
  (Value: gcForestgreen; Name:'gcForestgreen'),
  (Value: gcFuchsia; Name:'gcFuchsia'),
  (Value: gcGainsboro; Name:'gcGainsboro'),
  (Value: gcGhostwhite; Name:'gcGhostwhite'),
  (Value: gcGold; Name:'gcGold'),
  (Value: gcGoldenrod; Name:'gcGoldenrod'),
  (Value: gcGray; Name:'gcGray'),
  (Value: gcGreen; Name:'gcGreen'),
  (Value: gcGreenyellow; Name:'gcGreenyellow'),
  (Value: gcGrey; Name:'gcGrey'),
  (Value: gcHoneydew; Name:'gcHoneydew'),
  (Value: gcHotpink; Name:'gcHotpink'),
  (Value: gcIndianred; Name:'gcIndianred'),
  (Value: gcIndigo; Name:'gcIndigo'),
  (Value: gcIvory; Name:'gcIvory'),
  (Value: gcKhaki; Name:'gcKhaki'),
  (Value: gcLavender; Name:'gcLavender'),
  (Value: gcLavenderblush; Name:'gcLavenderblush'),
  (Value: gcLawngreen; Name:'gcLawngreen'),
  (Value: gcLemonchiffon; Name:'gcLemonchiffon'),
  (Value: gcLightblue; Name:'gcLightblue'),
  (Value: gcLightcoral; Name:'gcLightcoral'),
  (Value: gcLightcyan; Name:'gcLightcyan'),
  (Value: gcLightgoldenrodyellow; Name:'gcLightgoldenrodyellow'),
  (Value: gcLightgray; Name:'gcLightgray'),
  (Value: gcLightgreen; Name:'gcLightgreen'),
  (Value: gcLightgrey; Name:'gcLightgrey'),
  (Value: gcLightpink; Name:'gcLightpink'),
  (Value: gcLightsalmon; Name:'gcLightsalmon'),
  (Value: gcLightseagreen; Name:'gcLightseagreen'),
  (Value: gcLightskyblue; Name:'gcLightskyblue'),
  (Value: gcLightslategray; Name:'gcLightslategray'),
  (Value: gcLightslategrey; Name:'gcLightslategrey'),
  (Value: gcLightsteelblue; Name:'gcLightsteelblue'),
  (Value: gcLightyellow; Name:'gcLightyellow'),
  (Value: gcLtGray; Name:'gcLtGray'),
  (Value: gcMedGray; Name:'gcMedGray'),
  (Value: gcDkGray; Name:'gcDkGray'),
  (Value: gcMoneyGreen; Name:'gcMoneyGreen'),
  (Value: gcLegacySkyBlue; Name:'gcLegacySkyBlue'),
  (Value: gcCream; Name:'gcCream'),
  (Value: gcLime; Name:'gcLime'),
  (Value: gcLimegreen; Name:'gcLimegreen'),
  (Value: gcLinen; Name:'gcLinen'),
  (Value: gcMagenta; Name:'gcMagenta'),
  (Value: gcMaroon; Name:'gcMaroon'),
  (Value: gcMediumaquamarine; Name:'gcMediumaquamarine'),
  (Value: gcMediumblue; Name:'gcMediumblue'),
  (Value: gcMediumorchid; Name:'gcMediumorchid'),
  (Value: gcMediumpurple; Name:'gcMediumpurple'),
  (Value: gcMediumseagreen; Name:'gcMediumseagreen'),
  (Value: gcMediumslateblue; Name:'gcMediumslateblue'),
  (Value: gcMediumspringgreen; Name:'gcMediumspringgreen'),
  (Value: gcMediumturquoise; Name:'gcMediumturquoise'),
  (Value: gcMediumvioletred; Name:'gcMediumvioletred'),
  (Value: gcMidnightblue; Name:'gcMidnightblue'),
  (Value: gcMintcream; Name:'gcMintcream'),
  (Value: gcMistyrose; Name:'gcMistyrose'),
  (Value: gcMoccasin; Name:'gcMoccasin'),
  (Value: gcNavajowhite; Name:'gcNavajowhite'),
  (Value: gcNavy; Name:'gcNavy'),
  (Value: gcOldlace; Name:'gcOldlace'),
  (Value: gcOlive; Name:'gcOlive'),
  (Value: gcOlivedrab; Name:'gcOlivedrab'),
  (Value: gcOrange; Name:'gcOrange'),
  (Value: gcOrangered; Name:'gcOrangered'),
  (Value: gcOrchid; Name:'gcOrchid'),
  (Value: gcPalegoldenrod; Name:'gcPalegoldenrod'),
  (Value: gcPalegreen; Name:'gcPalegreen'),
  (Value: gcPaleturquoise; Name:'gcPaleturquoise'),
  (Value: gcPalevioletred; Name:'gcPalevioletred'),
  (Value: gcPapayawhip; Name:'gcPapayawhip'),
  (Value: gcPeachpuff; Name:'gcPeachpuff'),
  (Value: gcPeru; Name:'gcPeru'),
  (Value: gcPink; Name:'gcPink'),
  (Value: gcPlum; Name:'gcPlum'),
  (Value: gcPowderblue; Name:'gcPowderblue'),
  (Value: gcPurple; Name:'gcPurple'),
  (Value: gcRed; Name:'gcRed'),
  (Value: gcRosybrown; Name:'gcRosybrown'),
  (Value: gcRoyalblue; Name:'gcRoyalblue'),
  (Value: gcSaddlebrown; Name:'gcSaddlebrown'),
  (Value: gcSalmon; Name:'gcSalmon'),
  (Value: gcSandybrown; Name:'gcSandybrown'),
  (Value: gcSeagreen; Name:'gcSeagreen'),
  (Value: gcSeashell; Name:'gcSeashell'),
  (Value: gcSienna; Name:'gcSienna'),
  (Value: gcSilver; Name:'gcSilver'),
  (Value: gcSkyblue; Name:'gcSkyblue'),
  (Value: gcSlateblue; Name:'gcSlateblue'),
  (Value: gcSlategray; Name:'gcSlategray'),
  (Value: gcSlategrey; Name:'gcSlategrey'),
  (Value: gcSnow; Name:'gcSnow'),
  (Value: gcSpringgreen; Name:'gcSpringgreen'),
  (Value: gcSteelblue; Name:'gcSteelblue'),
  (Value: gcTan; Name:'gcTan'),
  (Value: gcTeal; Name:'gcTeal'),
  (Value: gcThistle; Name:'gcThistle'),
  (Value: gcTomato; Name:'gcTomato'),
  (Value: gcTurquoise; Name:'gcTurquoise'),
  (Value: gcViolet; Name:'gcViolet'),
  (Value: gcWheat; Name:'gcWheat'),
  (Value: gcWhite; Name:'gcWhite'),
  (Value: gcWhitesmoke; Name:'gcWhitesmoke'),
  (Value: gcYellow; Name:'gcYellow'),
  (Value: gcYellowgreen; Name:'gcYellowgreen'),
  (Value: gcNull; Name:'gcNull'));
  {$ENDIF}

type
  TAdvGraphicsStrokeKind = (gskNone, gskSolid, gskDash, gskDot, gskDashDot, gskDashDotDot);
  TAdvGraphicsTextureMode = (gtmOriginal, gtmFit, gtmStretch, gtmCenter, gtmTile);
  TAdvGraphicsFillKind = (gfkNone, gfkSolid, gfkGradient, gfkTexture);
  TAdvGraphicsFillOrientation = (gfoHorizontal, gfoVertical);
  TAdvGraphicsTextAlign = (gtaCenter, gtaLeading, gtaTrailing);
  TAdvGraphicsTextTrimming = (gttNone, gttCharacter, gttWord);
  TAdvGraphicsSide = (gsLeft, gsTop, gsRight, gsBottom);
  TAdvGraphicsSides = set of TAdvGraphicsSide;
  TAdvGraphicsTextQuality = (gtqDefault, gtqAntiAliasing, gtqClearType);

const
  AllSides = [gsLeft, gsTop, gsRight, gsBottom];
  Epsilon: Single = 1E-40;

type
  TAdvGraphicsStroke = class;
  TAdvGraphicsFill = class;
  TAdvGraphicsFont = class;

  TAdvGraphicsSaveState = class(TPersistent)
  private
    {$IFDEF CMNWEBLIB}
    FSaveDC: Integer;
    {$ENDIF}
    {$IFDEF FMXLIB}
    FSaveDC: TCanvasSaveState;
    {$ENDIF}
    FFont: TAdvGraphicsFont;
    FStroke: TAdvGraphicsStroke;
    FFill: TAdvGraphicsFill;
    FCustomSaveDC: Integer;
    procedure SetStroke(const Value: TAdvGraphicsStroke);
    procedure SetFill(const Value: TAdvGraphicsFill);
    procedure SetFont(const Value: TAdvGraphicsFont);
  public
    constructor Create;
    destructor Destroy; override;
    property Stroke: TAdvGraphicsStroke read FStroke write SetStroke;
    property Fill: TAdvGraphicsFill read FFill write SetFill;
    property Font: TAdvGraphicsFont read FFont write SetFont;
    property CustomSaveDC: Integer read FCustomSaveDC write FCustomSaveDC;
    {$IFDEF CMNWEBLIB}
    property SaveDC: Integer read FSaveDC write FSaveDC;
    {$ENDIF}
    {$IFDEF FMXLIB}
    property SaveDC: TCanvasSaveState read FSaveDC write FSaveDC;
    {$ENDIF}
  end;

  TAdvCustomGraphicsFill = class(TPersistent)
  private
    FUpdateCount: Integer;
    FOnChanged: TNotifyEvent;
    FOrientation: TAdvGraphicsFillOrientation;
    FKind: TAdvGraphicsFillKind;
    FColor: TAdvGraphicsColor;
    FColorTo: TAdvGraphicsColor;
    FColorMirror: TAdvGraphicsColor;
    FColorMirrorTo: TAdvGraphicsColor;
    FOpacity: Single;
    FTexture: TAdvBitmap;
    FTextureMode: TAdvGraphicsTextureMode;
    procedure SetKind(const Value: TAdvGraphicsFillKind);
    procedure SetOrientation(const Value: TAdvGraphicsFillOrientation);
    procedure SetColor(const Value: TAdvGraphicsColor);
    procedure SetColorTo(const Value: TAdvGraphicsColor);
    procedure SetColorMirror(const Value: TAdvGraphicsColor);
    procedure SetColorMirrorTo(const Value: TAdvGraphicsColor);
    procedure SetOpacity(const Value: Single);
    function IsOpacityStored: Boolean;
    procedure SetTexture(const Value: TAdvBitmap);
    procedure SetTextureMode(const Value: TAdvGraphicsTextureMode);
  protected
    procedure Changed;
    procedure TextureChanged(Sender: TObject);
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Kind: TAdvGraphicsFillKind read FKind write SetKind default gfkSolid;
    property Orientation: TAdvGraphicsFillOrientation read FOrientation write SetOrientation default gfoVertical;
    property Color: TAdvGraphicsColor read FColor write SetColor default gcWhite;
    property ColorTo: TAdvGraphicsColor read FColorTo write SetColorTo default gcGray;
    property ColorMirror: TAdvGraphicsColor read FColorMirror write SetColorMirror default gcNull;
    property ColorMirrorTo: TAdvGraphicsColor read FColorMirrorTo write SetColorMirrorTo default gcNull;
    property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
    property Texture: TAdvBitmap read FTexture write SetTexture;
    property TextureMode: TAdvGraphicsTextureMode read FTextureMode write SetTextureMode default gtmStretch;
  public
    constructor Create(const AKind: TAdvGraphicsFillKind = gfkSolid; const AColor: TAdvGraphicsColor = gcWhite); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  end;

  TAdvGraphicsFill = class(TAdvCustomGraphicsFill)
  public
    {$IFDEF CMNWEBLIB}
    property Opacity;
    {$ENDIF}
  published
    property OnChanged;
    property Kind;
    property Orientation;
    property Color;
    property ColorTo;
    property ColorMirror;
    property ColorMirrorTo;
    property TextureMode;
    property Texture;
    {$IFDEF FMXLIB}
    property Opacity;
    {$ENDIF}
  end;

  TAdvGraphicsFont = class(TFont)
  private
    FUpdateCount: Integer;
    FOnChanged: TNotifyEvent;
  {$IFDEF FMXLIB}
    FHeight: Integer;
    FColor: TAdvGraphicsColor;
    procedure SetColor(const Value: TAdvGraphicsColor);
    procedure SetName(const Value: string);
    function GetName: string;
  {$ENDIF}
  protected
    procedure DoChanged(Sender: TObject); {$IFDEF FMXLIB}reintroduce;{$ENDIF}
  {$IFDEF CMNWEBLIB}
    {$IFDEF LCLLIB}
    procedure Changed; override;
    {$ENDIF}
  {$ENDIF}
  public
  {$IFNDEF LCLLIB}
    constructor Create; virtual;
  {$ENDIF}
  {$IFDEF LCLLIB}
    constructor Create; override;
  {$ENDIF}
    procedure Assign(Source: TPersistent); override;
    procedure AssignSource(Source: TPersistent); virtual;
  {$IFDEF FMXLIB}
    property Height: Integer read FHeight write FHeight;
  published
    property Color: TAdvGraphicsColor read FColor write SetColor default gcBlack;
    property Name: string read GetName write SetName;
  {$ENDIF}
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  end;

  TAdvCustomGraphicsStroke = class(TPersistent)
  private
    FUpdateCount: Integer;
    FOnChanged: TNotifyEvent;
    FKind: TAdvGraphicsStrokeKind;
    FColor: TAdvGraphicsColor;
    FWidth: Single;
    FOpacity: Single;
    procedure SetKind(const Value: TAdvGraphicsStrokeKind);
    procedure SetColor(const Value: TAdvGraphicsColor);
    procedure SetWidth(const Value: Single);
    procedure SetOpacity(const Value: Single);
    function IsOpacityStored: Boolean;
    function IsWidthStored: Boolean;
  protected
    procedure Changed;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Kind: TAdvGraphicsStrokeKind read FKind write SetKind default gskSolid;
    property Color: TAdvGraphicsColor read FColor write SetColor default gcSilver;
    property Width: Single read FWidth write SetWidth stored IsWidthStored nodefault;
    property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
  public
    constructor Create(const AKind: TAdvGraphicsStrokeKind = gskSolid; const AColor: TAdvGraphicsColor = gcSilver); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  end;

  TAdvGraphicsStroke = class(TAdvCustomGraphicsStroke)
  public
    {$IFDEF CMNWEBLIB}
    property Opacity;
    {$ENDIF}
  published
    property OnChanged;
    property Kind;
    property Color;
    property Width;
    {$IFDEF FMXLIB}
    property Opacity;
    {$ENDIF}
  end;

  {$IFNDEF WEBLIB}
  TAdvGraphicsVectorArray = array [0..2] of Single;

  TAdvGraphicsVector = record
    case Integer of
      0: (V: TAdvGraphicsVectorArray;);
      1: (X: Single;
          Y: Single;
          W: Single;);
  end;

  TAdvGraphicsMatrixArray = array [0..2] of TAdvGraphicsVector;

  TAdvGraphicsMatrix = record
  private
    function Scale(const AFactor: Single): TAdvGraphicsMatrix;
  public
    class function Identity: TAdvGraphicsMatrix; static;
    class function CreateRotation(const AAngle: Single): TAdvGraphicsMatrix; static;
    class function CreateScaling(const AScaleX, AScaleY: Single): TAdvGraphicsMatrix; static;
    class function CreateTranslation(const ADeltaX, ADeltaY: Single): TAdvGraphicsMatrix; static;
    {$IFDEF LCLLIB}
    class operator *(const AMatrix1, AMatrix2: TAdvGraphicsMatrix): TAdvGraphicsMatrix;
    class operator *(const APoint: TPointF; const AMatrix: TAdvGraphicsMatrix): TPointF;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    class operator Multiply(const AMatrix1, AMatrix2: TAdvGraphicsMatrix): TAdvGraphicsMatrix;
    class operator Multiply(const APoint: TPointF; const AMatrix: TAdvGraphicsMatrix): TPointF;
    {$ENDIF}

    function Determinant: Single;
    function Adjoint: TAdvGraphicsMatrix;
    function Inverse: TAdvGraphicsMatrix;

    case Integer of
      0: (M: TAdvGraphicsMatrixArray;);
      1: (m11, m12, m13: Single;
          m21, m22, m23: Single;
          m31, m32, m33: Single);
  end;
  {$ENDIF}

  {$IFDEF WEBLIB}
  TAdvGraphicsMatrix = record
    m11, m12, m13: Single;
    m21, m22, m23: Single;
    m31, m32, m33: Single;
  end;
  {$ENDIF}

  TAdvGraphicsPathPolygon = array of TPointF;

  TAdvGraphicsPathPointKind = (gppMoveTo, gppLineTo, gppCurveTo, gppClose);

  TAdvGraphicsPathCubicBezier = array[0..3] of TPointF;

  TAdvGraphicsPathPoint = record
    Kind: TAdvGraphicsPathPointKind;
    {$HINTS OFF}
    {$WARNINGS OFF}
    {$IF FPC_FULLVERSION < 30000}
    Dummy: Boolean;
    {$IFEND}
    {$HINTS ON}
    {$WARNINGS ON}
    Point: TPointF;
    {$IFDEF LCLLIB}
    class operator = (z1, z2 : TAdvGraphicsPathPoint) b : Boolean;
    {$ENDIF}
  end;

  TAdvGraphicsPathDrawMode = (pdmPolygon, pdmPolyline);

  {$IFDEF WEBLIB}
  TAdvGraphicsPathPoints = class(TList)
  private
    function GetItem(Index: Integer): TAdvGraphicsPathPoint;
    procedure SetItem(Index: Integer; const Value: TAdvGraphicsPathPoint);
  public
    property Items[Index: Integer]: TAdvGraphicsPathPoint read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvGraphicsPathPoints = class(TList<TAdvGraphicsPathPoint>);
  {$ENDIF}

  TAdvGraphicsPath = class(TPersistent)
  private
    FStartPoint: TPointF;
    FClippable: Boolean;
    FPoints: TAdvGraphicsPathPoints;
    procedure CalculateBezierCoefficients(const Bezier: TAdvGraphicsPathCubicBezier; out AX, BX, CX, AY, BY, CY: Single);
    function GetCount: Integer;
    function GetPoint(AIndex: Integer): TAdvGraphicsPathPoint;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function LastPoint: TPointF;
    function IsPointVisible(const P: TPointF): Boolean;
    function IsRectVisible(const R: TRectF): Boolean;
    function PointOnBezier(const StartPoint: TPointF; const AX, BX, CX, AY, BY, CY, T: Single): TPointF;
    function CreateBezier(const Bezier: TAdvGraphicsPathCubicBezier; const PointCount: Integer): TAdvGraphicsPathPolygon;
    function GetBounds: TRectF;
    function IsClippable: Boolean;
    procedure MoveTo(const P: TPointF);
    procedure LineTo(const P: TPointF);
    procedure CurveTo(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
    procedure SmoothCurveTo(const ControlPoint2, EndPoint: TPointF);
    procedure QuadCurveTo(const ControlPoint, EndPoint: TPointF);
    procedure AddPolygon(const APolygon: TAdvGraphicsPathPolygon);
    procedure ClosePath;
    procedure AddLine(const StartPoint, EndPoint: TPointF);
    procedure AddEllipse(const ARect: TRectF);
    procedure AddRectangle(const ARect: TRectF);
    procedure AddArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
    procedure AddPath(APath: TAdvGraphicsPath);
    procedure ApplyMatrix(AMatrix: TAdvGraphicsMatrix);
    procedure Clear;
    procedure Flatten(const Flatness: Single = 0.25);
    procedure FlattenToPolygon(var Polygon: TAdvGraphicsPathPolygon; const Flatness: Single = 0.25);
    property Count: Integer read GetCount;
    property Points[AIndex: Integer]: TAdvGraphicsPathPoint read GetPoint; default;
    property PathData: TAdvGraphicsPathPoints read FPoints;
    property Clippable: Boolean read FClippable write FClippable;
  end;

  TAdvGraphicsColorObject = class
  private
    FColor: TAdvGraphicsColor;
  public
    constructor Create(AColor: TAdvGraphicsColor);
    property Color: TAdvGraphicsColor read FColor write FColor;
  end;

  TAdvGraphicsModifyRectMode = (gcrmNone, gcrmExpandAll, gcrmShrinkAll, gcrmShiftRightAndExpandHeight, gcrmShiftDownAndExpandWidth,
    gcrmShiftRightAndShrinkHeight, gcrmShiftRightDown, gcrmShiftRightUp, gcrmShiftLeftUp, gcrmShiftLeftDown, gcrmShiftUpAndExpandWidth,
    gcrmShiftLeftAndExpandHeight);
  TAdvGraphicsModifyPointMode = (gcpmNone, gcpmLeftUp, gcpmRightDown, gcpmLeftDown, gcpmRightUp);
  TAdvGraphicsExpanderState = (gesCollapsed, gesExpanded);
  TAdvGraphicsCompactState = (gcsCollapsed, gcsExpanded);

  TAdvGraphicsCorner = (gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight);
  TAdvGraphicsCorners = set of TAdvGraphicsCorner;

function ModifyRect(const ARect: TRectF; const {%H-}ARectMode: TAdvGraphicsModifyRectMode): TRectF;
function ModifyPoint(const APoint: TPointF; const {%H-}APointMode: TAdvGraphicsModifyPointMode): TPointF;
function MakeGraphicsColor(ARed, AGreen, ABlue: Byte{$IFDEF FMXLIB};AAlpha: Byte = 255{$ENDIF}): TAdvGraphicsColor;
function Lighter(AColor: TAdvGraphicsColor; APercent: Byte): TAdvGraphicsColor;
function Darker(AColor: TAdvGraphicsColor; APercent: Byte): TAdvGraphicsColor;
function Blend(AColor1: TAdvGraphicsColor; AColor2: TAdvGraphicsColor; ALevel: Byte): TAdvGraphicsColor;
function ColorLookup: TStringList;
function PointInPoly(APoint: TPointF; APolygon: TAdvGraphicsPathPolygon): Boolean;
function RectInPoly(ARect: TRectF; APolygon: TAdvGraphicsPathPolygon): Boolean;
function PolyInRect(APolygon: TAdvGraphicsPathPolygon; ARect: TRectF): Boolean;
{$IFDEF CMNWEBLIB}
{$IFDEF WEBLIB}
procedure DrawGradient(ACanvas: TCanvas; AColor, AColorTo: TColor; ARect: TRectF; ARounding: Single; ACorners: TAdvGraphicsCorners; ADirection: Boolean);
{$ENDIF}
{$IFNDEF WEBLIB}
procedure DrawGradient(ACanvas: TCanvas; AColor, AColorTo: TColor; ARect: TRect; ARounding: Single; ACorners: TAdvGraphicsCorners; ADirection: Boolean);
{$ENDIF}
{$ENDIF}

function MatrixIdentity: TAdvGraphicsMatrix;
function MatrixCreateRotation(const AAngle: Single): TAdvGraphicsMatrix;
function MatrixCreateScaling(const AScaleX, AScaleY: Single): TAdvGraphicsMatrix;
function MatrixCreateTranslation(const ADeltaX, ADeltaY: Single): TAdvGraphicsMatrix;
function MatrixMultiply(const AMatrix1, AMatrix2: TAdvGraphicsMatrix): TAdvGraphicsMatrix; overload;
function MatrixMultiply(const APoint: TPointF; const AMatrix: TAdvGraphicsMatrix): TPointF; overload;

implementation

uses
  AdvUtils, SysUtils, Math
  {$IFDEF FMXLIB}
  ,UIConsts
  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFNDEF MSWINDOWS}
  ,LCLIntF
  {$ENDIF}
  {$ENDIF}
  ;

var
  FColorLookup: TStringList;

function MatrixIdentity: TAdvGraphicsMatrix;
begin
  Result.m11 := 1;
  Result.m12 := 0;
  Result.m13 := 0;
  Result.m21 := 0;
  Result.m22 := 1;
  Result.m23 := 0;
  Result.m31 := 0;
  Result.m32 := 0;
  Result.m33 := 1;
end;

function MatrixCreateRotation(const AAngle: Single): TAdvGraphicsMatrix;
var
  S, C: Single;
begin
  S := Sin(AAngle);
  C := Cos(AAngle);
  Result := MatrixIdentity;
  Result.m11 := C;
  Result.m12 := S;
  Result.m21 := -S;
  Result.m22 := C;
end;

function MatrixCreateScaling(const AScaleX, AScaleY: Single): TAdvGraphicsMatrix;
begin
  Result := MatrixIdentity;
  Result.m11 := AScaleX;
  Result.m22 := AScaleY;
end;

function MatrixCreateTranslation(const ADeltaX, ADeltaY: Single): TAdvGraphicsMatrix;
begin
  Result := MatrixIdentity;
  Result.m31 := ADeltaX;
  Result.m32 := ADeltaY;
end;

function MatrixMultiply(const APoint: TPointF; const AMatrix: TAdvGraphicsMatrix): TPointF;
begin
  Result.X := APoint.X * AMatrix.m11 + APoint.Y * AMatrix.m21 + AMatrix.m31;
  Result.Y := APoint.X * AMatrix.m12 + APoint.Y * AMatrix.m22 + AMatrix.m32;
end;

function MatrixMultiply(const AMatrix1, AMatrix2: TAdvGraphicsMatrix): TAdvGraphicsMatrix;
begin
  Result.m11 := AMatrix1.m11 * AMatrix2.m11 + AMatrix1.m12 * AMatrix2.m21 + AMatrix1.m13 * AMatrix2.m31;
  Result.m12 := AMatrix1.m11 * AMatrix2.m12 + AMatrix1.m12 * AMatrix2.m22 + AMatrix1.m13 * AMatrix2.m32;
  Result.m13 := AMatrix1.m11 * AMatrix2.m13 + AMatrix1.m12 * AMatrix2.m23 + AMatrix1.m13 * AMatrix2.m33;
  Result.m21 := AMatrix1.m21 * AMatrix2.m11 + AMatrix1.m22 * AMatrix2.m21 + AMatrix1.m23 * AMatrix2.m31;
  Result.m22 := AMatrix1.m21 * AMatrix2.m12 + AMatrix1.m22 * AMatrix2.m22 + AMatrix1.m23 * AMatrix2.m32;
  Result.m23 := AMatrix1.m21 * AMatrix2.m13 + AMatrix1.m22 * AMatrix2.m23 + AMatrix1.m23 * AMatrix2.m33;
  Result.m31 := AMatrix1.m31 * AMatrix2.m11 + AMatrix1.m32 * AMatrix2.m21 + AMatrix1.m33 * AMatrix2.m31;
  Result.m32 := AMatrix1.m31 * AMatrix2.m12 + AMatrix1.m32 * AMatrix2.m22 + AMatrix1.m33 * AMatrix2.m32;
  Result.m33 := AMatrix1.m31 * AMatrix2.m13 + AMatrix1.m32 * AMatrix2.m23 + AMatrix1.m33 * AMatrix2.m33;
end;

function ModifyRect(const ARect: TRectF; const ARectMode: TAdvGraphicsModifyRectMode): TRectF;
begin
  Result := ARect;
  {$IFDEF FMXWEBLIB}
  case ARectMode of
    gcrmExpandAll: Result := RectF(int(Result.Left) - 0.5, int(Result.Top) - 0.5, int(Result.Right) + 0.5, int(Result.Bottom) + 0.5);
    gcrmShrinkAll: Result := RectF(int(Result.Left) + 0.5, int(Result.Top) + 0.5, int(Result.Right) - 0.5, int(Result.Bottom) - 0.5);
    gcrmShiftRightAndExpandHeight: Result := RectF(int(Result.Left) + 0.5, int(Result.Top) - 0.5, int(Result.Right) + 0.5, int(Result.Bottom) + 0.5);
    gcrmShiftRightDown: Result := RectF(int(Result.Left) + 0.5, int(Result.Top) + 0.5, int(Result.Right) + 0.5, int(Result.Bottom) + 0.5);
    gcrmShiftRightAndShrinkHeight: Result := RectF(int(Result.Left) + 0.5, int(Result.Top) + 0.5, int(Result.Right) + 0.5, int(Result.Bottom) - 0.5);
    gcrmShiftDownAndExpandWidth: Result := RectF(int(Result.Left) - 0.5, int(Result.Top) + 0.5, int(Result.Right) + 0.5, int(Result.Bottom) + 0.5);
    gcrmShiftUpAndExpandWidth: Result := RectF(int(Result.Left) - 0.5, int(Result.Top) - 0.5, int(Result.Right) + 0.5, int(Result.Bottom) - 0.5);
    gcrmShiftRightUp: Result := RectF(Int(Result.Left) + 0.5, Int(Result.Top) - 0.5, Int(Result.Right) + 0.5, Int(Result.Bottom) - 0.5);
    gcrmShiftLeftUp: Result := RectF(Int(Result.Left) - 0.5, Int(Result.Top) - 0.5, Int(Result.Right) - 0.5, Int(Result.Bottom) - 0.5);
    gcrmShiftLeftDown: Result := RectF(Int(Result.Left) - 0.5, Int(Result.Top) + 0.5, Int(Result.Right) - 0.5, Int(Result.Bottom) + 0.5);
    gcrmShiftLeftAndExpandHeight: Result := RectF(int(Result.Left) - 0.5, int(Result.Top) - 0.5, int(Result.Right) - 0.5, int(Result.Bottom) + 0.5);
  end;
  {$ENDIF}
end;

function ModifyPoint(const APoint: TPointF; const APointMode: TAdvGraphicsModifyPointMode): TPointF;
begin
  Result := APoint;
  {$IFDEF FMXWEBLIB}
  case APointMode of
    gcpmRightDown: Result := PointF(Int(Result.X) + 0.5, Int(Result.Y) + 0.5);
    gcpmLeftUp: Result := PointF(Int(Result.X) - 0.5, Int(Result.Y) - 0.5);
    gcpmRightUp: Result := PointF(Int(Result.X) + 0.5, Int(Result.Y) - 0.5);
    gcpmLeftDown: Result := PointF(Int(Result.X) - 0.5, Int(Result.Y) + 0.5);
  end;
  {$ENDIF}
end;

function ColorLookup: TStringList;
begin
  Result := FColorLookup;
end;

function PolyInRect(APolygon: TAdvGraphicsPathPolygon; ARect: TRectF): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(APolygon) - 1 do
  begin
    if PtInRectEx(ARect, APolygon[I]) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function RectInPoly(ARect: TRectF; APolygon: TAdvGraphicsPathPolygon): Boolean;
begin
  Result := PointInPoly(PointF(ARect.Left, ARect.Top), APolygon)
    or PointInPoly(PointF(ARect.Right, ARect.Top), APolygon)
    or PointInPoly(PointF(ARect.Left, ARect.Bottom), APolygon)
    or PointInPoly(PointF(ARect.Right, ARect.Bottom), APolygon);
end;

function PointInPoly(APoint: TPointF; APolygon: TAdvGraphicsPathPolygon): Boolean;
var
  minX, maxX, minY, maxY: Double;
  I, J: Integer;
  q: TPointF;
  pt, ptn: TPointF;
  inside, chk: Boolean;
  calc: Double;
  chkc: Boolean;
begin
  Result := False;
  if Length(APolygon) = 0 then
    Exit;

  minX := APolygon[0].X;
  maxX := APolygon[0].X;
  minY := APolygon[0].Y;
  maxY := APolygon[0].Y;

  for I := 1 to Length(APolygon) - 1 do
  begin
    q := APolygon[I];
    minX := Min(q.X, minX);
    maxX := Max(q.X, maxX);
    minY := Min(q.Y, minY);
    maxY := Max(q.Y, maxY);
  end;

  if (APoint.X < minX) or (APoint.X > maxX) or (APoint.Y < minY) or (APoint.Y > maxY) then
  begin
    Result := False;
    Exit;
  end;

  inside := False;
  J := Length(APolygon) - 1;
  for I := 0 to Length(APolygon) - 1 do
  begin
    pt := APolygon[I];
    ptn := APolygon[J];
    chk := (pt.Y > APoint.Y) <> (ptn.Y > APoint.Y);
    if (ptn.Y - pt.Y) <> 0 then
    begin
      calc := (ptn.X - pt.X) * (APoint.Y - pt.Y) / (ptn.Y - pt.Y) + pt.X;
      chkc := APoint.X < calc;
      if chk and chkc then
        inside := not inside;
    end;

    j := I;
  end;
  Result := inside;
end;

function Blend(AColor1: TAdvGraphicsColor; AColor2: TAdvGraphicsColor; ALevel: Byte): TAdvGraphicsColor;
var
  c1, c2: Int64;
  r, g, b, v1, v2: byte;
begin
  ALevel := Round(2.55 * ALevel);
  {$IFDEF CMNWEBLIB}
  c1 := ColorToRGB(AColor1);
  c2 := ColorToRGB(AColor2);
  {$ENDIF}
  {$IFDEF FMXLIB}
  c1 := AColor1;
  c2 := AColor2;
  {$ENDIF}
  v1 := Byte(c1);
  v2 := Byte(c2);
  r := Max(0, Min(255, ALevel * (v1 - v2) shr 8 + v2));
  v1 := Byte(c1 shr 8);
  v2 := Byte(c2 shr 8);
  g := Max(0, Min(255, ALevel * (v1 - v2) shr 8 + v2));
  v1 := Byte(c1 shr 16);
  v2 := Byte(c2 shr 16);
  b := Max(0, Min(255, ALevel * (v1 - v2) shr 8 + v2));
  {$IFDEF CMNWEBLIB}
  Result := (b shl 16) + (g shl 8) + r;
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := MakeGraphicsColor(r, g, b);
  {$ENDIF}
end;

function Lighter(AColor: TAdvGraphicsColor; APercent: Byte): TAdvGraphicsColor;
var
  r, g, b: Byte;
begin
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  r := GetRValue(AColor);
  g := GetGValue(AColor);
  b := GetBValue(AColor);
  {$ENDIF}
  {$IFDEF FMXLIB}
  r := TAlphaColorRec(AColor).R;
  g := TAlphaColorRec(AColor).G;
  b := TAlphaColorRec(AColor).B;
  {$ENDIF}

  r := r + Min(255, TAdvUtils.MulDivInt(255 - r, APercent, 100));
  g := g + Min(255, TAdvUtils.MulDivInt(255 - g, APercent, 100));
  b := b + Min(255, TAdvUtils.MulDivInt(255 - b, APercent, 100));

  {$IFDEF FMXLIB}
  Result := MakeGraphicsColor(r, g, b, TAlphaColorRec(AColor).A);
  {$ENDIF}

  {$IFDEF CMNWEBLIB}
  Result := MakeGraphicsColor(r, g, b);
  {$ENDIF}
end;

function Darker(AColor: TAdvGraphicsColor; APercent: Byte): TAdvGraphicsColor;
var
  r, g, b: Byte;
begin
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  r := GetRValue(AColor);
  g := GetGValue(AColor);
  b := GetBValue(AColor);
  {$ENDIF}
  {$IFDEF FMXLIB}
  r := TAlphaColorRec(AColor).R;
  g := TAlphaColorRec(AColor).G;
  b := TAlphaColorRec(AColor).B;
  {$ENDIF}

  r := r - Max(0, TAdvUtils.MulDivInt(255 - r, APercent, 100));
  g := g - Max(0, TAdvUtils.MulDivInt(255 - g, APercent, 100));
  b := b - Max(0, TAdvUtils.MulDivInt(255 - b, APercent, 100));

  {$IFDEF FMXLIB}
  Result := MakeGraphicsColor(r, g, b, TAlphaColorRec(AColor).A);
  {$ENDIF}

  {$IFDEF CMNWEBLIB}
  Result := MakeGraphicsColor(r, g, b);
  {$ENDIF}
end;

{$IFDEF CMNWEBLIB}
{$IFDEF WEBLIB}
procedure DrawGradient(ACanvas: TCanvas; AColor, AColorTo: TColor;
  ARect: TRectF; ARounding: Single; ACorners: TAdvGraphicsCorners; ADirection: Boolean);
{$ENDIF}
{$IFNDEF WEBLIB}
procedure DrawGradient(ACanvas: TCanvas; AColor, AColorTo: TColor;
  ARect: TRect; ARounding: Single; ACorners: TAdvGraphicsCorners; ADirection: Boolean);
{$ENDIF}
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  si: Integer;
  rstepr,rstepg,rstepb,rstepw: Single;
  i,stepw: Word;
  oldp, oldb: TColor;
  Steps: Integer;
  {$IFDEF WEBLIB}
  R, dr: TRectF;
  iend: Single;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  R, dr: TRect;
  iend: Integer;
  {$ENDIF}
  a: Single;
  oldps: TPenStyle;
  oldbs: TBrushStyle;
begin
  R := ARect;

  {$IFDEF WEBLIB}
  if ADirection then
    Steps := Round(R.Right - R.Left)
  else
    Steps := Round(R.Bottom - R.Top);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  if ADirection then
    Steps := R.Right - R.Left
  else
    Steps := R.Bottom - R.Top;
  {$ENDIF}

  if Steps = 0 then
    Steps := 1;

  AColor := ColorToRGB(AColor);
  AColorTo := ColorToRGB(AColorTo);

  startr := (AColor and $0000FF);
  startg := (AColor and $00FF00) shr 8;
  startb := (AColor and $FF0000) shr 16;

  endr := (AColorTo and $0000FF);
  endg := (AColorTo and $00FF00) shr 8;
  endb := (AColorTo and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if ADirection then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with ACanvas do
  begin
    oldb := Brush.Color;
    oldbs := Brush.Style;
    oldp := Pen.Color;
    oldps := Pen.Style;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;

      if ADirection then
      begin
        si := Trunc(rstepw);
        iend := R.Left + stepw + si;
        iend := Min(iend, R.Right);

        dr := r;

        if (I <= ARounding) and ((gcTopLeft in ACorners) or (gcBottomLeft in ACorners)) then
        begin
          a := SQRT(Power(ARounding, 2) - Power(ARounding - I, 2));

          if (gcTopLeft in ACorners) then
            dr.Top := dr.Top + Round(ARounding - a);

          if (gcBottomLeft in ACorners) then
            dr.Bottom := dr.Bottom - Round(ARounding - a);
        end
        else if (I >= Steps - ARounding) and ((gcBottomRight in ACorners) or (gcTopRight in ACorners)) then
        begin
          a := SQRT(Power(ARounding, 2) - Power(ARounding - (Steps - I), 2));

          if (gcTopRight in ACorners) then
            dr.Top := dr.Top + Round(ARounding - a);

          if (gcBottomRight in ACorners) then
            dr.Bottom := dr.Bottom - Round(ARounding - a);
        end;

        Rectangle(dr.Left + stepw, dr.Top, iend, dr.Bottom)
      end
      else
      begin
        si := Trunc(rstepw);
        iend := R.Top + stepw + si;
        iend := Min(iend, r.Bottom);

        dr := r;

        if (I <= ARounding) and ((gcTopLeft in ACorners) or (gcTopRight in ACorners)) then
        begin
          a := SQRT(Power(ARounding, 2) - Power(ARounding - I, 2));

          if (gcTopLeft in ACorners) then
            dr.Left := dr.Left + Round(ARounding - a);

          if (gcTopRight in ACorners) then
            dr.Right := dr.Right - Round(ARounding - a);
        end
        else if (I >= Steps - ARounding) and ((gcBottomLeft in ACorners) or (gcBottomRight in ACorners)) then
        begin
          a := SQRT(Power(ARounding, 2) - Power(ARounding - (Steps - I), 2));

          if (gcBottomLeft in ACorners) then
            dr.Left := dr.Left + Round(ARounding - a);

          if (gcBottomRight in ACorners) then
            dr.Right := dr.Right - Round(ARounding - a);
        end;

        Rectangle(dr.Left, dr.Top + stepw, dr.Right, iend);
      end;
    end;
    Brush.Color := oldb;
    Brush.Style := oldbs;
    Pen.Color := oldp;
    Pen.style := oldps;
  end;
end;
{$ENDIF}

function MakeGraphicsColor(ARed, AGreen, ABlue: Byte{$IFDEF FMXLIB};AAlpha: Byte = 255{$ENDIF}): TAdvGraphicsColor;
begin
  {$IFDEF FMXLIB}
  Result := MakeColor(ARed, AGreen, ABlue, AAlpha);
  {$ENDIF}
  {$IFDEF VCLLIB}
  Result := RGB(ARed, AGreen, ABlue);
  {$ENDIF}
  {$IFDEF LCLLIB}
  Result := RGBToColor(ARed, AGreen, ABlue);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := RGB(ARed, AGreen, ABlue);
  {$ENDIF}
end;

{ TAdvGraphicsFont }

procedure TAdvGraphicsFont.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TAdvGraphicsFont.Create;
begin
  inherited Create;
  {$IFDEF FMXLIB}
  FColor := gcBlack;
  inherited OnChanged := DoChanged;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  OnChange := DoChanged;
  {$ENDIF}
end;

procedure TAdvGraphicsFont.EndUpdate;
begin
  Dec(FUpdateCount);
  DoChanged(Self);
end;

{$IFDEF CMNWEBLIB}
{$IFDEF LCLLIB}
procedure TAdvGraphicsFont.Changed;
begin
  inherited;
  DoChanged(Self);
end;
{$ENDIF}
{$ENDIF}

procedure TAdvGraphicsFont.DoChanged(Sender: TObject);
begin
  if Assigned(OnChanged) and (FUpdateCount = 0) then
    OnChanged(Self);
end;

procedure TAdvGraphicsFont.AssignSource(Source: TPersistent);
begin
  Assign(Source);
end;

procedure TAdvGraphicsFont.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  {$IFDEF FMXLIB}
  if Source is TAdvGraphicsFont then
    FColor := (Source as TAdvGraphicsFont).Color;
  {$ENDIF}
  DoChanged(Self);
end;

{$IFDEF FMXLIB}
procedure TAdvGraphicsFont.SetName(const Value: string);
begin
  Family := Value;
end;

function TAdvGraphicsFont.GetName: string;
begin
  Result := Family;
end;

procedure TAdvGraphicsFont.SetColor(const Value: TAdvGraphicsColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    {$IFDEF CMNWEBLIB}
    if FColor = clNone then
      FColor := gcNull
    else if FColor = clDefault then
      FColor := clBtnFace;
    {$ENDIF}
    DoChanged(Self);
  end;
end;

{$ENDIF}

{ TAdvGraphicsSaveState }

constructor TAdvGraphicsSaveState.Create;
begin
  inherited;
  FFill := TAdvGraphicsFill.Create;
  FStroke := TAdvGraphicsStroke.Create;
  FFont := TAdvGraphicsFont.Create;
end;

destructor TAdvGraphicsSaveState.Destroy;
begin
  FFill.Free;
  FStroke.Free;
  FFont.Free;
  inherited;
end;

procedure TAdvGraphicsSaveState.SetFill(const Value: TAdvGraphicsFill);
begin
  FFill.Assign(Value);
end;

procedure TAdvGraphicsSaveState.SetFont(const Value: TAdvGraphicsFont);
begin
  FFont.AssignSource(Value);
end;

procedure TAdvGraphicsSaveState.SetStroke(const Value: TAdvGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

{ TAdvCustomGraphicsFill }

procedure TAdvCustomGraphicsFill.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomGraphicsFill then
  begin
    FColor := (Source as TAdvCustomGraphicsFill).Color;
    FColorTo := (Source as TAdvCustomGraphicsFill).ColorTo;
    FColorMirror := (Source as TAdvCustomGraphicsFill).ColorMirror;
    FColorMirrorTo := (Source as TAdvCustomGraphicsFill).ColorMirrorTo;
    FKind := (Source as TAdvCustomGraphicsFill).Kind;
    FOrientation := (Source as TAdvCustomGraphicsFill).Orientation;
    FOpacity := (Source as TAdvCustomGraphicsFill).Opacity;
    FTexture.Assign((Source as TAdvCustomGraphicsFill).Texture);
    FTextureMode := (Source as TAdvCustomGraphicsFill).TextureMode;
  end
  else if Source is TAdvCustomGraphicsStroke then
  begin
    FColor := (Source as TAdvCustomGraphicsStroke).Color;
    FColorTo := gcNull;
    FColorMirror := gcNull;
    FColorMirrorTo := gcNull;
    FKind := gfkSolid;
    FOrientation := gfoVertical;
    FOpacity := 1;
  end;
  Changed;
end;

procedure TAdvCustomGraphicsFill.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TAdvCustomGraphicsFill.Changed;
begin
  if Assigned(OnChanged) and (FUpdateCount = 0) then
    OnChanged(Self);
end;

constructor TAdvCustomGraphicsFill.Create(const AKind: TAdvGraphicsFillKind = gfkSolid; const AColor: TAdvGraphicsColor = gcWhite);
begin
  FKind := AKind;
  FColor := AColor;
  FOpacity := 1;
  FOrientation := gfoVertical;
  FColorTo := gcGray;
  FColorMirror := gcNull;
  FColorMirrorTo := gcNull;
  FTextureMode := gtmStretch;
  FTexture := TAdvBitmap.Create;
  FTexture.OnChange := TextureChanged;
end;

destructor TAdvCustomGraphicsFill.Destroy;
begin
  FTexture.Free;
  inherited;
end;

procedure TAdvCustomGraphicsFill.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

procedure TAdvCustomGraphicsFill.SetColor(const Value: TAdvGraphicsColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    {$IFDEF CMNWEBLIB}
    if FColor = clNone then
      FColor := gcNull
    else if FColor = clDefault then
      FColor := clBtnFace;
    {$ENDIF}
    Changed;
  end;
end;

procedure TAdvCustomGraphicsFill.SetColorTo(const Value: TAdvGraphicsColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    {$IFDEF CMNWEBLIB}
    if FColorTo = clNone then
      FColorTo := gcNull
    else if FColorTo = clDefault then
      FColorTo := clBtnFace;
    {$ENDIF}
    Changed;
  end;
end;

procedure TAdvCustomGraphicsFill.SetColorMirror(const Value: TAdvGraphicsColor);
begin
  if FColorMirror <> Value then
  begin
    FColorMirror := Value;
    {$IFDEF CMNWEBLIB}
    if FColorMirror = clNone then
      FColorMirror := gcNull
    else if FColorMirror = clDefault then
      FColorMirror := clBtnFace;
    {$ENDIF}
    Changed;
  end;
end;

procedure TAdvCustomGraphicsFill.SetColorMirrorTo(const Value: TAdvGraphicsColor);
begin
  if FColorMirrorTo <> Value then
  begin
    FColorMirrorTo := Value;
    {$IFDEF CMNWEBLIB}
    if FColorMirrorTo = clNone then
      FColorMirrorTo := gcNull
    else if FColorMirrorTo = clDefault then
      FColorMirrorTo := clBtnFace;
    {$ENDIF}
    Changed;
  end;
end;

procedure TAdvCustomGraphicsFill.SetKind(const Value: TAdvGraphicsFillKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    Changed;
  end;
end;

procedure TAdvCustomGraphicsFill.SetOrientation(const Value: TAdvGraphicsFillOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Changed;
  end;
end;

procedure TAdvCustomGraphicsFill.SetTexture(const Value: TAdvBitmap);
begin
  if FTexture <> Value then
  begin
    FTexture.Assign(Value);
    Changed;
  end;
end;

procedure TAdvCustomGraphicsFill.SetTextureMode(
  const Value: TAdvGraphicsTextureMode);
begin
  if FTextureMode <> Value then
  begin
    FTextureMode := Value;
    Changed;
  end;
end;

procedure TAdvCustomGraphicsFill.TextureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvCustomGraphicsFill.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

function TAdvCustomGraphicsFill.IsOpacityStored: Boolean;
begin
  Result := Opacity <> 1;
end;

{ TAdvCustomGraphicsStroke }

procedure TAdvCustomGraphicsStroke.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomGraphicsStroke then
  begin
    FColor := (Source as TAdvCustomGraphicsStroke).Color;
    FKind := (Source as TAdvCustomGraphicsStroke).Kind;
    FOpacity := (Source as TAdvCustomGraphicsStroke).Opacity;
    FWidth := (Source as TAdvCustomGraphicsStroke).Width;
  end
  else if Source is TAdvCustomGraphicsFill then
  begin
    FColor := (Source as TAdvCustomGraphicsFill).Color;
    FKind := gskSolid;
    FOpacity := 1;
    FWidth := 1;
  end;
  Changed;
end;

procedure TAdvCustomGraphicsStroke.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvCustomGraphicsStroke.Changed;
begin
  if Assigned(OnChanged) and (FUpdateCount = 0) then
    OnChanged(Self);
end;

constructor TAdvCustomGraphicsStroke.Create(const AKind: TAdvGraphicsStrokeKind = gskSolid; const AColor: TAdvGraphicsColor = gcSilver);
begin
  FKind := AKind;
  FColor := AColor;
  FOpacity := 1;
  FWidth := 1;
end;

procedure TAdvCustomGraphicsStroke.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

function TAdvCustomGraphicsStroke.IsWidthStored: Boolean;
begin
  Result := Width <> 1;
end;

procedure TAdvCustomGraphicsStroke.SetColor(const Value: TAdvGraphicsColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    {$IFDEF CMNWEBLIB}
    if FColor = clNone then
      FColor := gcNull
    else if FColor = clDefault then
      FColor := clBtnFace;
    {$ENDIF}
    Changed;
  end;
end;

procedure TAdvCustomGraphicsStroke.SetKind(const Value: TAdvGraphicsStrokeKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    Changed;
  end;
end;

procedure TAdvCustomGraphicsStroke.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

function TAdvCustomGraphicsStroke.IsOpacityStored: Boolean;
begin
  Result := Opacity <> 1;
end;

procedure TAdvCustomGraphicsStroke.SetWidth(const Value: Single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TAdvGraphicsPath }

constructor TAdvGraphicsPath.Create;
begin
  inherited Create;
  FPoints := TAdvGraphicsPathPoints.Create;
end;

destructor TAdvGraphicsPath.Destroy;
begin
  FPoints.Free;
  inherited;
end;

procedure TAdvGraphicsPath.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TAdvGraphicsPath then
  begin
    FPoints.Count := TAdvGraphicsPath(Source).Count;
    for I := 0 to TAdvGraphicsPath(Source).Count - 1 do
      FPoints[I] := TAdvGraphicsPath(Source)[I];
  end
  else
    inherited
end;

function TAdvGraphicsPath.GetBounds: TRectF;
const
  SmallAmount = 0.001;
var
  I: Integer;
  pt: TAdvGraphicsPathPoint;
begin
  if PathData.Count < 1 then
    Exit(RectF(0, 0, 0, 0));
  Result := RectF($FFFF, $FFFF, -$FFFF, -$FFFF);
  for I := 0 to PathData.Count - 1 do
  begin
    pt := PathData[I];
    if pt.Kind = TAdvGraphicsPathPointKind.gppClose then
      Continue;

    if pt.Point.X < Result.Left then
      Result.Left := pt.Point.X;
    if pt.Point.X > Result.Right then
      Result.Right := pt.Point.X;
    if pt.Point.Y < Result.Top then
      Result.Top := pt.Point.Y;
    if pt.Point.Y > Result.Bottom then
      Result.Bottom := pt.Point.Y;
  end;
  // add small amount
  {$IFDEF WEBLIB}
  if SameValue(Result.Right - Result.Left, 0, SmallAmount) then
    Result.Right := Result.Left + SmallAmount;
  if SameValue(Result.Bottom - Result.Top, 0, SmallAmount) then
    Result.Bottom := Result.Top + SmallAmount;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  if SameValue(Result.Width, 0, SmallAmount) then
    Result.Right := Result.Left + SmallAmount;
  if SameValue(Result.Height, 0, SmallAmount) then
    Result.Bottom := Result.Top + SmallAmount;
  {$ENDIF}
end;

function TAdvGraphicsPath.GetCount: Integer;
begin
  Result := FPoints.Count;
end;

function TAdvGraphicsPath.GetPoint(AIndex: Integer): TAdvGraphicsPathPoint;
begin
  Result := TAdvGraphicsPathPoint(FPoints[AIndex]);
end;

function TAdvGraphicsPath.IsClippable: Boolean;
begin
  Result := FClippable;
end;

function TAdvGraphicsPath.IsPointVisible(const P: TPointF): Boolean;
var
  pt: TAdvGraphicsPathPolygon;
begin
  SetLength(pt, 0);
  FlattenToPolygon(pt);
  Result := PointInPoly(p, pt);
end;

function TAdvGraphicsPath.IsRectVisible(const R: TRectF): Boolean;
var
  pt: TAdvGraphicsPathPolygon;
begin
  SetLength(pt, 0);
  FlattenToPolygon(pt);
  Result := PolyInRect(pt, R);
end;

function TAdvGraphicsPath.LastPoint: TPointF;
begin
  if Count > 0 then
    Result := FPoints[FPoints.Count - 1].Point
  else
  begin
    Result := PointF(0, 0);
  end;
end;

procedure TAdvGraphicsPath.MoveTo(const P: TPointF);
var
  pt: TAdvGraphicsPathPoint;
begin
  pt.Kind := gppMoveTo;
  pt.Point := p;
  FPoints.Add(pt);
  FStartPoint := FPoints[FPoints.Count - 1].Point;
end;

procedure TAdvGraphicsPath.LineTo(const P: TPointF);
var
  pt: TAdvGraphicsPathPoint;
begin
  pt.Kind := gppLineTo;
  pt.Point := p;
  FPoints.Add(pt);
  if (P.X = FStartPoint.X) and (P.Y = FStartPoint.Y) then
    FClippable := True;
end;

procedure TAdvGraphicsPath.QuadCurveTo(const ControlPoint, EndPoint: TPointF);
const
  OneThird = 1 / 3;
  TwoThirds = 2 / 3;
var
  LP, CP1, CP2: TPointF;
begin
  LP := LastPoint;
  CP1.X := OneThird * LP.X + TwoThirds * ControlPoint.X;
  CP1.Y := OneThird * LP.Y + TwoThirds * ControlPoint.Y;
  CP2.X := TwoThirds * ControlPoint.X + OneThird * EndPoint.X;
  CP2.Y := TwoThirds * ControlPoint.Y + OneThird * EndPoint.Y;
  CurveTo(CP1, CP2, EndPoint);
end;

procedure TAdvGraphicsPath.CurveTo(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
var
  pt: TAdvGraphicsPathPoint;
begin
  pt.Kind := gppCurveTo;
  pt.Point := ControlPoint1;
  FPoints.Add(pt);
  pt.Kind := gppCurveTo;
  pt.Point := ControlPoint2;
  FPoints.Add(pt);
  pt.Kind := gppCurveTo;
  pt.Point := EndPoint;
  FPoints.Add(pt);
end;

procedure TAdvGraphicsPath.SmoothCurveTo(const ControlPoint2, EndPoint: TPointF);
var
  ControlPoint1: TPointF;
begin
  if Count > 2 then
  begin
    ControlPoint1.X := LastPoint.X + (LastPoint.X - FPoints[FPoints.Count - 2].Point.X);
    ControlPoint1.Y := LastPoint.Y + (LastPoint.Y - FPoints[FPoints.Count - 2].Point.Y);
  end
  else
    ControlPoint1 := ControlPoint2;
  CurveTo(ControlPoint1, ControlPoint2, EndPoint);
end;

procedure TAdvGraphicsPath.ClosePath;
var
  pt: TAdvGraphicsPathPoint;
begin
  pt.Kind := gppClose;
  pt.Point := FStartPoint;
  FPoints.Add(pt);
  FClippable := True;
end;

procedure TAdvGraphicsPath.AddPath(APath: TAdvGraphicsPath);
var
  I: Integer;
begin
  FPoints.Capacity := FPoints.Count + APath.Count;
  for I := 0 to APath.Count - 1 do
    FPoints.Add(APath.Points[I]);
end;

procedure TAdvGraphicsPath.AddPolygon(
  const APolygon: TAdvGraphicsPathPolygon);
var
  I: Integer;
begin
  for I := 0 to Length(APolygon) - 1 do
    LineTo(APolygon[I]);
end;

procedure TAdvGraphicsPath.Clear;
begin
  FPoints.Clear;
end;

procedure TAdvGraphicsPath.CalculateBezierCoefficients(const Bezier: TAdvGraphicsPathCubicBezier; out AX, BX, CX, AY, BY, CY: Single);
begin
  CX := 3 * (Bezier[1].X - Bezier[0].X);
  CY := 3 * (Bezier[1].Y - Bezier[0].Y);
  BX := 3 * (Bezier[2].X - Bezier[1].X) - CX;
  BY := 3 * (Bezier[2].Y - Bezier[1].Y) - CY;
  AX := Bezier[3].X - Bezier[0].X - CX - BX;
  AY := Bezier[3].Y - Bezier[0].Y - CY - BY;
end;

function TAdvGraphicsPath.PointOnBezier(const StartPoint: TPointF; const AX, BX, CX, AY, BY, CY, T: Single): TPointF;
var
  SquareT, CubeT: Single;
begin
  SquareT := T * T;
  CubeT := SquareT * T;
  Result.X := (AX * CubeT) + (BX * SquareT) + (CX * T) + StartPoint.X;
  Result.Y := (AY * CubeT) + (BY * SquareT) + (CY * T) + StartPoint.Y;
end;

function TAdvGraphicsPath.CreateBezier(const Bezier: TAdvGraphicsPathCubicBezier; const PointCount: Integer): TAdvGraphicsPathPolygon;
var
  AX, BX, CX, AY, BY, CY, DT, T: Single;
  I: Integer;
begin
  if PointCount = 0 then
    Exit;
  DT := 1 / (1 * PointCount - 1);
  T := 0;
  SetLength(Result, PointCount);
  CalculateBezierCoefficients(Bezier, AX, BX, CX, AY, BY, CY);
  for I := 0 to PointCount - 1 do
  begin
    Result[I] := PointOnBezier(Bezier[0], AX, BX, CX, AY, BY, CY, T);
    T := T + DT;
  end;
end;

procedure TAdvGraphicsPath.Flatten(const Flatness: Single);
var
  J, I: Integer;
  BPts: TAdvGraphicsPathPolygon;
  B: TAdvGraphicsPathCubicBezier;
  F, Len: Single;
  SegCount: Integer;
  OldPathData: TAdvGraphicsPathPoints;
  CurPoint: TPointF;
  x: TPointF;
begin
  if FPoints.Count > 0 then
  begin
    F := Max(Flatness, 0.05);
    OldPathData := TAdvGraphicsPathPoints.Create;
    try
      OldPathData.Count := FPoints.Count;
      for J := 0 to FPoints.Count - 1 do
        OldPathData.Add(FPoints[J]);
      FPoints.Clear;
      J := 0;
      while J < OldPathData.Count do
      begin
        case OldPathData[J].Kind of
          gppMoveTo:
            begin
              MoveTo(OldPathData[J].Point);
              CurPoint := OldPathData[J].Point;
            end;
          gppLineTo:
            begin
              LineTo(OldPathData[J].Point);
              CurPoint := OldPathData[J].Point;
            end;
          gppCurveTo:
            begin
              B[0] := CurPoint;
              B[1] := OldPathData[J].Point;
              Inc(J);
              B[2] := OldPathData[J].Point;
              Inc(J);
              B[3] := OldPathData[J].Point;
              BPts := CreateBezier(B, 6);
              Len := 0;
              for I := 0 to High(BPts) - 1 do
              begin
                x.X := BPts[I].X - BPts[I + 1].X;
                x.Y := BPts[I].Y - BPts[I + 1].Y;
                Len := Len + GetPointLength(x);
              end;
              SegCount := Round(Len / F);
              if SegCount < 2 then
                LineTo(B[3])
              else
              begin
                BPts := CreateBezier(B, SegCount);
                for I := 0 to High(BPts) do
                  LineTo(BPts[I]);
                CurPoint := OldPathData[J].Point;
              end;
            end;
          gppClose:
            ClosePath;
        end;
        Inc(J);
      end;
    finally
      OldPathData.Free;
    end;
  end;
end;

procedure TAdvGraphicsPath.FlattenToPolygon(var Polygon: TAdvGraphicsPathPolygon; const Flatness: Single = 0.25);

  procedure AddPoint(const P: TPointF);
  begin
    if (Length(Polygon) > 0) and (SameValue(P.X, Polygon[High(Polygon)].X, PathPosition) and SameValue(P.Y,
      Polygon[High(Polygon)].Y, PathPosition)) then
      Exit;
    SetLength(Polygon, Length(Polygon) + 1);
    Polygon[High(Polygon)] := P;
  end;

var
  J, I: Integer;
  BPts: TAdvGraphicsPathPolygon;
  B: TAdvGraphicsPathCubicBezier;
  SP, CurPoint, X: TPointF;
  F, Len: Single;
  SegCount: Integer;
begin
  SetLength(Polygon, 0);
  if FPoints.Count > 0 then
  begin
    F := Max(Flatness, 0.05);
    J := 0;
    while J < FPoints.Count do
    begin
      case FPoints[J].Kind of
        gppMoveTo:
          begin
            if Length(Polygon) > 0 then
              AddPoint(PointF($FFFFFF, $FFFFFF));
            AddPoint(FPoints[J].Point);
            CurPoint := FPoints[J].Point;
            SP := CurPoint;
          end;
        gppLineTo:
          begin
            AddPoint(FPoints[J].Point);
            CurPoint := FPoints[J].Point;
          end;
        gppCurveTo:
          begin
            B[0] := CurPoint;
            B[1] := FPoints[J].Point;
            Inc(J);
            B[2] := FPoints[J].Point;
            Inc(J);
            B[3] := FPoints[J].Point;
            BPts := CreateBezier(B, 6);
            Len := 0;
            for I := 0 to High(BPts) - 1 do
            begin
              x.X := BPts[I].X - BPts[I + 1].X;
              x.Y := BPts[I].Y - BPts[I + 1].Y;
              Len := Len + GetPointLength(x);
            end;
            SegCount := Round(Len / F);
            if SegCount < 2 then
            begin
              AddPoint(B[0]);
              AddPoint(B[3]);
            end
            else
            begin
              BPts := CreateBezier(B, SegCount);
              for I := 0 to High(BPts) do
                AddPoint(BPts[I]);
            end;
            CurPoint := FPoints[J].Point;
          end;
        gppClose:
          begin
            AddPoint(SP);
          end;
      end;
      Inc(J);
    end;
  end;
end;

procedure TAdvGraphicsPath.AddEllipse(const ARect: TRectF);
var
  CX, CY, PX, PY: Single;
  c: Single;
begin
  c := 0.5522847498;
  CX := (ARect.Left + ARect.Right) / 2;
  CY := (ARect.Top + ARect.Bottom) / 2;
  PX := c * ((ARect.Right - ARect.Left) / 2);
  PY := c * ((ARect.Bottom - ARect.Top) / 2);
  MoveTo(PointF(ARect.Left, CY));
  CurveTo(PointF(ARect.Left, CY - PY), PointF(CX - PX, ARect.Top), PointF(CX, ARect.Top));
  CurveTo(PointF(CX + PX, ARect.Top), PointF(ARect.Right, CY - PY), PointF(ARect.Right, CY));
  CurveTo(PointF(ARect.Right, CY + PY), PointF(CX + PX, ARect.Bottom), PointF(CX, ARect.Bottom));
  CurveTo(PointF(CX - PX, ARect.Bottom), PointF(ARect.Left, CY + PY), PointF(ARect.Left, CY));
  FClippable := True;
end;

procedure TAdvGraphicsPath.AddLine(const StartPoint, EndPoint: TPointF);
begin
  if Count = 0 then
    MoveTo(StartPoint)
  else
    LineTo(StartPoint);

  LineTo(EndPoint);
end;

procedure TAdvGraphicsPath.AddRectangle(const ARect: TRectF);
begin
  MoveTo(PointF(ARect.Left, ARect.Top));
  LineTo(PointF(ARect.Right, ARect.Top));
  LineTo(PointF(ARect.Right, ARect.Bottom));
  LineTo(PointF(ARect.Left, ARect.Bottom));
  ClosePath;
  FClippable := True;
end;

procedure TAdvGraphicsPath.ApplyMatrix(AMatrix: TAdvGraphicsMatrix);
var
  I: Integer;
  pt: TAdvGraphicsPathPoint;
begin
  if PathData.Count > 0 then
  begin
    for I := 0 to PathData.Count - 1 do
    begin
      if PathData[I].Kind in [TAdvGraphicsPathPointKind.gppMoveTo, TAdvGraphicsPathPointKind.gppLineTo, TAdvGraphicsPathPointKind.gppCurveTo] then
      begin
        pt := PathData[I];
        pt.Point := MatrixMultiply(pt.Point, AMatrix);
        PathData[I] := pt;
      end;
    end;
  end;
end;

procedure DrawArcWithBezier(Path: TAdvGraphicsPath; CenterX, CenterY, RadiusX, RadiusY, StartAngle, SweepRange: Single;
  UseMoveTo: Boolean);
var
  Coord: array of TPointF;
  Pts: array of TPointF;
  A, X, Y: Single;
  {$IFDEF LCLLIB}
  C, B, CC, SS: Extended;
  {$ELSE}
  C, B, CC, SS: Single;
  {$ENDIF}
  I: Integer;
begin
  if SweepRange = 0 then
  begin
    if UseMoveTo then
    begin
      if Path.FPoints.Count < 1 then
        Path.MoveTo(PointF(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)))
      else
        Path.LineTo(PointF(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)));
    end;
    Path.LineTo(PointF(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)));
    Exit;
  end;
  {$IFDEF WEBLIB}
  B := Sin(SweepRange / 2);
  C := Cos(SweepRange / 2);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  SinCos(SweepRange / 2, B, C);
  {$ENDIF}
  A := 1 - C;
  X := A * 4 / 3;
  Y := B - X * C / B;
  {$IFDEF WEBLIB}
  SS := Sin(StartAngle + SweepRange / 2);
  CC := Cos(StartAngle + SweepRange / 2);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  SinCos(StartAngle + SweepRange / 2, SS, CC);
  {$ENDIF}
  SetLength(Coord, 4);
  Coord[0] := PointF(C, -B);
  Coord[1] := PointF(C + X, -Y);
  Coord[2] := PointF(C + X, Y);
  Coord[3] := PointF(C, B);
  SetLength(Pts, 4);
  for I := 0 to 3 do
  begin
    Pts[I] := PointF(CenterX + RadiusX * (Coord[I].X * CC - Coord[I].Y * SS), CenterY + RadiusY * (Coord[I].X *
      SS + Coord[I].Y * CC));
  end;
  if UseMoveTo then
  begin
    if Path.FPoints.Count < 1 then
      Path.MoveTo(Pts[0])
    else
      Path.LineTo(Pts[0]);
  end;
  Path.CurveTo(Pts[1], Pts[2], Pts[3]);
end;

procedure TAdvGraphicsPath.AddArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
const
  BezierArcAngleEpsilon = 0.01;
  MinSweepAngle = 1E-10;
var
  UseMoveTo: Boolean;
  I: Integer;
  F: Single;
  TotalSweep, LocalSweep, PrevSweep: Single;
  Done: Boolean;
begin
  StartAngle := DegToRad(StartAngle);
  SweepAngle := DegToRad(SweepAngle);
  I := Trunc(StartAngle / (2 * Pi));
  F := StartAngle - (I * 2 * Pi);
  StartAngle := F;
  if SweepAngle >= 2 * Pi then
    SweepAngle := 2 * Pi;
  if SweepAngle <= -2 * Pi then
    SweepAngle := -2 * Pi;
  if Abs(SweepAngle) < MinSweepAngle then
    Exit;
  TotalSweep := 0;
  Done := False;
  UseMoveTo := True;
  repeat
    if SweepAngle < 0 then
    begin
      PrevSweep := TotalSweep;
      LocalSweep := -Pi / 2;
      TotalSweep := TotalSweep - (Pi / 2);
      if TotalSweep <= SweepAngle + BezierArcAngleEpsilon then
      begin
        LocalSweep := SweepAngle - PrevSweep;
        Done := True;
      end;
    end
    else
    begin
      PrevSweep := TotalSweep;
      LocalSweep := Pi / 2;
      TotalSweep := TotalSweep + (Pi / 2);
      if TotalSweep >= SweepAngle - BezierArcAngleEpsilon then
      begin
        LocalSweep := SweepAngle - PrevSweep;
        Done := True;
      end;
    end;
    DrawArcWithBezier(Self, Center.X, Center.Y, Radius.X, Radius.Y, StartAngle, LocalSweep, UseMoveTo);
    UseMoveTo := False;
    StartAngle := StartAngle + LocalSweep;
  until Done;
end;

{ TAdvGraphicsPathPoint }

{$IFDEF LCLLIB}
class operator TAdvGraphicsPathPoint.=(z1, z2: TAdvGraphicsPathPoint)b: boolean;
begin
  Result := z1 = z2;
end;
{$ENDIF}

{$IFNDEF WEBLIB}

{ TAdvGraphicsMatrix }

class function TAdvGraphicsMatrix.CreateRotation(const AAngle: Single): TAdvGraphicsMatrix;
var
  S, C: Single;
begin
  S := Sin(AAngle);
  C := Cos(AAngle);
  Result := Identity;
  Result.m11 := C;
  Result.m12 := S;
  Result.m21 := -S;
  Result.m22 := C;
end;

class function TAdvGraphicsMatrix.CreateScaling(const AScaleX, AScaleY: Single): TAdvGraphicsMatrix;
begin
  Result := Identity;
  Result.m11 := AScaleX;
  Result.m22 := AScaleY;
end;

class function TAdvGraphicsMatrix.CreateTranslation(const ADeltaX, ADeltaY: Single): TAdvGraphicsMatrix;
begin
  Result := Identity;
  Result.m31 := ADeltaX;
  Result.m32 := ADeltaY;
end;

function TAdvGraphicsMatrix.Scale(const AFactor: Single): TAdvGraphicsMatrix;
var
  I: Integer;
begin
  for I := 0 to 2 do
  begin
    Result.M[I].V[0] := Self.M[I].V[0] * AFactor;
    Result.M[I].V[1] := Self.M[I].V[1] * AFactor;
    Result.M[I].V[2] := Self.M[I].V[2] * AFactor;
  end;
end;

function TAdvGraphicsMatrix.Determinant: Single;
begin
  Result := Self.M[0].V[0] * (Self.M[1].V[1] * Self.M[2].V[2] - Self.M[2].V[1] * Self.M[1].V[2]) - Self.M[0].V[1]
    * (Self.M[1].V[0] * Self.M[2].V[2] - Self.M[2].V[0] * Self.M[1].V[2]) + Self.M[0].V[2] * (Self.M[1].V[0]
    * Self.M[2].V[1] - Self.M[2].V[0] * Self.M[1].V[1]);
end;

function TAdvGraphicsMatrix.Adjoint: TAdvGraphicsMatrix;
var
  a1, a2, a3, b1, b2, b3, c1, c2, c3: Single;
begin
  a1 := Self.M[0].V[0];
  a2 := Self.M[0].V[1];
  a3 := Self.M[0].V[2];
  b1 := Self.M[1].V[0];
  b2 := Self.M[1].V[1];
  b3 := Self.M[1].V[2];
  c1 := Self.M[2].V[0];
  c2 := Self.M[2].V[1];
  c3 := Self.M[2].V[2];

  Result.M[0].V[0] := (b2 * c3 - c2 * b3);
  Result.M[1].V[0] := -(b1 * c3 - c1 * b3);
  Result.M[2].V[0] := (b1 * c2 - c1 * b2);

  Result.M[0].V[1] := -(a2 * c3 - c2 * a3);
  Result.M[1].V[1] := (a1 * c3 - c1 * a3);
  Result.M[2].V[1] := -(a1 * c2 - c1 * a2);

  Result.M[0].V[2] := (a2 * b3 - b2 * a3);
  Result.M[1].V[2] := -(a1 * b3 - b1 * a3);
  Result.M[2].V[2] := (a1 * b2 - b1 * a2);
end;

class function TAdvGraphicsMatrix.Identity: TAdvGraphicsMatrix;
begin
  Result.m11 := 1;
  Result.m12 := 0;
  Result.m13 := 0;
  Result.m21 := 0;
  Result.m22 := 1;
  Result.m23 := 0;
  Result.m31 := 0;
  Result.m32 := 0;
  Result.m33 := 1;
end;

function TAdvGraphicsMatrix.Inverse: TAdvGraphicsMatrix;
const
  DefaultValue: TAdvGraphicsMatrix = (m11: 1.0; m12: 0.0; m13: 0.0; m21: 0.0; m22: 1.0; m23: 0.0; m31: 0.0; m32: 0.0; m33: 1.0);
var
  Det: Single;
begin
  Det := Self.Determinant;
  if Abs(Det) < Epsilon then
    Result := DefaultValue
  else
    Result:= Self.Adjoint.Scale(1 / Det);
end;

{$IFDEF LCLLIB}
class operator TAdvGraphicsMatrix.*(const APoint: TPointF;
  const AMatrix: TAdvGraphicsMatrix): TPointF;
{$ENDIF}
{$IFNDEF LCLLIB}
class operator TAdvGraphicsMatrix.Multiply(const APoint: TPointF;
  const AMatrix: TAdvGraphicsMatrix): TPointF;
{$ENDIF}
begin
  Result.X := APoint.X * AMatrix.M[0].V[0] + APoint.Y * AMatrix.M[1].V[0]
    + AMatrix.M[2].V[0];
  Result.Y := APoint.X * AMatrix.M[0].V[1] + APoint.Y * AMatrix.M[1].V[1]
    + AMatrix.M[2].V[1];
end;

{$IFDEF LCLLIB}
class operator TAdvGraphicsMatrix.*(const AMatrix1, AMatrix2: TAdvGraphicsMatrix): TAdvGraphicsMatrix;
{$ENDIF}
{$IFNDEF LCLLIB}
class operator TAdvGraphicsMatrix.Multiply(const AMatrix1, AMatrix2: TAdvGraphicsMatrix): TAdvGraphicsMatrix;
{$ENDIF}
begin
  Result.m11 := AMatrix1.m11 * AMatrix2.m11 + AMatrix1.m12 * AMatrix2.m21 + AMatrix1.m13 * AMatrix2.m31;
  Result.m12 := AMatrix1.m11 * AMatrix2.m12 + AMatrix1.m12 * AMatrix2.m22 + AMatrix1.m13 * AMatrix2.m32;
  Result.m13 := AMatrix1.m11 * AMatrix2.m13 + AMatrix1.m12 * AMatrix2.m23 + AMatrix1.m13 * AMatrix2.m33;
  Result.m21 := AMatrix1.m21 * AMatrix2.m11 + AMatrix1.m22 * AMatrix2.m21 + AMatrix1.m23 * AMatrix2.m31;
  Result.m22 := AMatrix1.m21 * AMatrix2.m12 + AMatrix1.m22 * AMatrix2.m22 + AMatrix1.m23 * AMatrix2.m32;
  Result.m23 := AMatrix1.m21 * AMatrix2.m13 + AMatrix1.m22 * AMatrix2.m23 + AMatrix1.m23 * AMatrix2.m33;
  Result.m31 := AMatrix1.m31 * AMatrix2.m11 + AMatrix1.m32 * AMatrix2.m21 + AMatrix1.m33 * AMatrix2.m31;
  Result.m32 := AMatrix1.m31 * AMatrix2.m12 + AMatrix1.m32 * AMatrix2.m22 + AMatrix1.m33 * AMatrix2.m32;
  Result.m33 := AMatrix1.m31 * AMatrix2.m13 + AMatrix1.m32 * AMatrix2.m23 + AMatrix1.m33 * AMatrix2.m33;
end;
{$ENDIF}

{$IFDEF WEBLIB}
function TAdvGraphicsPathPoints.GetItem(Index: Integer): TAdvGraphicsPathPoint;
begin
  Result := TAdvGraphicsPathPoint(inherited Items[Index]);
end;

procedure TAdvGraphicsPathPoints.SetItem(Index: Integer; const Value: TAdvGraphicsPathPoint);
var
  v: TAdvGraphicsPathPoint;
begin
  v := Value;
  inherited Items[Index] := v;
end;
{$ENDIF}

{ TAdvGraphicsColorObject }

constructor TAdvGraphicsColorObject.Create(AColor: TAdvGraphicsColor);
begin
  FColor := AColor;
end;

procedure DestroyColorLookup;
var
  I: Integer;
  {$IFNDEF FMXLIB}
  obj: TAdvGraphicsColorObject;
  {$ENDIF}
begin
  {$IFDEF FMXLIB}
  for I := 0 to FColorLookup.Count - 1 do
    FColorLookup.Objects[I].DisposeOf;
  {$ELSE}
  for I := 0 to FColorLookup.Count - 1 do
  begin
    obj := TAdvGraphicsColorObject(FColorLookup.Objects[I]);
    obj.Free;
  end;
  {$ENDIF}

  FColorLookup.Free;
end;

{$IFNDEF WEBLIB}
initialization
begin
  fcolorlookup := TStringList.Create;
  fcolorlookup.addobject('aliceblue', TAdvGraphicsColorObject.Create(gcaliceblue));
  fcolorlookup.addobject('antiquewhite', TAdvGraphicsColorObject.Create(gcantiquewhite));
  fcolorlookup.addobject('aqua', TAdvGraphicsColorObject.Create(gcaqua));
  fcolorlookup.addobject('aquamarine', TAdvGraphicsColorObject.Create(gcaquamarine));
  fcolorlookup.addobject('azure', TAdvGraphicsColorObject.Create(gcazure));
  fcolorlookup.addobject('beige', TAdvGraphicsColorObject.Create(gcbeige));
  fcolorlookup.addobject('bisque', TAdvGraphicsColorObject.Create(gcbisque));
  fcolorlookup.addobject('black', TAdvGraphicsColorObject.Create(gcblack));
  fcolorlookup.addobject('blanchedalmond', TAdvGraphicsColorObject.Create(gcblanchedalmond));
  fcolorlookup.addobject('blue', TAdvGraphicsColorObject.Create(gcblue));
  fcolorlookup.addobject('blueviolet', TAdvGraphicsColorObject.Create(gcblueviolet));
  fcolorlookup.addobject('brown', TAdvGraphicsColorObject.Create(gcbrown));
  fcolorlookup.addobject('burlywood', TAdvGraphicsColorObject.Create(gcburlywood));
  fcolorlookup.addobject('cadetblue', TAdvGraphicsColorObject.Create(gccadetblue));
  fcolorlookup.addobject('chartreuse', TAdvGraphicsColorObject.Create(gcchartreuse));
  fcolorlookup.addobject('chocolate', TAdvGraphicsColorObject.Create(gcchocolate));
  fcolorlookup.addobject('coral', TAdvGraphicsColorObject.Create(gccoral));
  fcolorlookup.addobject('cornflowerblue', TAdvGraphicsColorObject.Create(gccornflowerblue));
  fcolorlookup.addobject('cornsilk', TAdvGraphicsColorObject.Create(gccornsilk));
  fcolorlookup.addobject('crimson', TAdvGraphicsColorObject.Create(gccrimson));
  fcolorlookup.addobject('cyan', TAdvGraphicsColorObject.Create(gccyan));
  fcolorlookup.addobject('darkblue', TAdvGraphicsColorObject.Create(gcdarkblue));
  fcolorlookup.addobject('darkcyan', TAdvGraphicsColorObject.Create(gcdarkcyan));
  fcolorlookup.addobject('darkgoldenrod', TAdvGraphicsColorObject.Create(gcdarkgoldenrod));
  fcolorlookup.addobject('darkgray', TAdvGraphicsColorObject.Create(gcdarkgray));
  fcolorlookup.addobject('darkgreen', TAdvGraphicsColorObject.Create(gcdarkgreen));
  fcolorlookup.addobject('darkgrey', TAdvGraphicsColorObject.Create(gcdarkgrey));
  fcolorlookup.addobject('darkkhaki', TAdvGraphicsColorObject.Create(gcdarkkhaki));
  fcolorlookup.addobject('darkmagenta', TAdvGraphicsColorObject.Create(gcdarkmagenta));
  fcolorlookup.addobject('darkolivegreen', TAdvGraphicsColorObject.Create(gcdarkolivegreen));
  fcolorlookup.addobject('darkorange', TAdvGraphicsColorObject.Create(gcdarkorange));
  fcolorlookup.addobject('darkorchid', TAdvGraphicsColorObject.Create(gcdarkorchid));
  fcolorlookup.addobject('darkred', TAdvGraphicsColorObject.Create(gcdarkred));
  fcolorlookup.addobject('darksalmon', TAdvGraphicsColorObject.Create(gcdarksalmon));
  fcolorlookup.addobject('darkseagreen', TAdvGraphicsColorObject.Create(gcdarkseagreen));
  fcolorlookup.addobject('darkslateblue', TAdvGraphicsColorObject.Create(gcdarkslateblue));
  fcolorlookup.addobject('darkslategray', TAdvGraphicsColorObject.Create(gcdarkslategray));
  fcolorlookup.addobject('darkslategrey', TAdvGraphicsColorObject.Create(gcdarkslategrey));
  fcolorlookup.addobject('darkturquoise', TAdvGraphicsColorObject.Create(gcdarkturquoise));
  fcolorlookup.addobject('darkviolet', TAdvGraphicsColorObject.Create(gcdarkviolet));
  fcolorlookup.addobject('deeppink', TAdvGraphicsColorObject.Create(gcdeeppink));
  fcolorlookup.addobject('deepskyblue', TAdvGraphicsColorObject.Create(gcdeepskyblue));
  fcolorlookup.addobject('dimgray', TAdvGraphicsColorObject.Create(gcdimgray));
  fcolorlookup.addobject('dimgrey', TAdvGraphicsColorObject.Create(gcdimgrey));
  fcolorlookup.addobject('dodgerblue', TAdvGraphicsColorObject.Create(gcdodgerblue));
  fcolorlookup.addobject('firebrick', TAdvGraphicsColorObject.Create(gcfirebrick));
  fcolorlookup.addobject('floralwhite', TAdvGraphicsColorObject.Create(gcfloralwhite));
  fcolorlookup.addobject('forestgreen', TAdvGraphicsColorObject.Create(gcforestgreen));
  fcolorlookup.addobject('fuchsia', TAdvGraphicsColorObject.Create(gcfuchsia));
  fcolorlookup.addobject('gainsboro', TAdvGraphicsColorObject.Create(gcgainsboro));
  fcolorlookup.addobject('ghostwhite', TAdvGraphicsColorObject.Create(gcghostwhite));
  fcolorlookup.addobject('gold', TAdvGraphicsColorObject.Create(gcgold));
  fcolorlookup.addobject('goldenrod', TAdvGraphicsColorObject.Create(gcgoldenrod));
  fcolorlookup.addobject('gray', TAdvGraphicsColorObject.Create(gcgray));
  fcolorlookup.addobject('green', TAdvGraphicsColorObject.Create(gcgreen));
  fcolorlookup.addobject('greenyellow', TAdvGraphicsColorObject.Create(gcgreenyellow));
  fcolorlookup.addobject('grey', TAdvGraphicsColorObject.Create(gcgrey));
  fcolorlookup.addobject('honeydew', TAdvGraphicsColorObject.Create(gchoneydew));
  fcolorlookup.addobject('hotpink', TAdvGraphicsColorObject.Create(gchotpink));
  fcolorlookup.addobject('indianred', TAdvGraphicsColorObject.Create(gcindianred));
  fcolorlookup.addobject('indigo', TAdvGraphicsColorObject.Create(gcindigo));
  fcolorlookup.addobject('ivory', TAdvGraphicsColorObject.Create(gcivory));
  fcolorlookup.addobject('khaki', TAdvGraphicsColorObject.Create(gckhaki));
  fcolorlookup.addobject('lavender', TAdvGraphicsColorObject.Create(gcLavender));
  fcolorlookup.addobject('lavenderblush', TAdvGraphicsColorObject.Create(gcLavenderblush));
  fcolorlookup.addobject('lawngreen', TAdvGraphicsColorObject.Create(gcLawngreen));
  fcolorlookup.addobject('lemonchiffon', TAdvGraphicsColorObject.Create(gclemonchiffon));
  fcolorlookup.addobject('lightblue', TAdvGraphicsColorObject.Create(gclightblue));
  fcolorlookup.addobject('lightcoral', TAdvGraphicsColorObject.Create(gclightcoral));
  fcolorlookup.addobject('lightcyan', TAdvGraphicsColorObject.Create(gclightcyan));
  fcolorlookup.addobject('lightgoldenrodyellow', TAdvGraphicsColorObject.Create(gclightgoldenrodyellow));
  fcolorlookup.addobject('lightgray', TAdvGraphicsColorObject.Create(gclightgray));
  fcolorlookup.addobject('lightgreen', TAdvGraphicsColorObject.Create(gclightgreen));
  fcolorlookup.addobject('lightgrey', TAdvGraphicsColorObject.Create(gclightgrey));
  fcolorlookup.addobject('lightpink', TAdvGraphicsColorObject.Create(gclightpink));
  fcolorlookup.addobject('lightsalmon', TAdvGraphicsColorObject.Create(gclightsalmon));
  fcolorlookup.addobject('lightseagreen', TAdvGraphicsColorObject.Create(gclightseagreen));
  fcolorlookup.addobject('lightskyblue', TAdvGraphicsColorObject.Create(gclightskyblue));
  fcolorlookup.addobject('lightslategray', TAdvGraphicsColorObject.Create(gclightslategray));
  fcolorlookup.addobject('lightslategrey', TAdvGraphicsColorObject.Create(gclightslategrey));
  fcolorlookup.addobject('lightsteelblue', TAdvGraphicsColorObject.Create(gclightsteelblue));
  fcolorlookup.addobject('lightyellow', TAdvGraphicsColorObject.Create(gclightyellow));
  fcolorlookup.addobject('lime', TAdvGraphicsColorObject.Create(gclime));
  fcolorlookup.addobject('limegreen', TAdvGraphicsColorObject.Create(gclimegreen));
  fcolorlookup.addobject('linen', TAdvGraphicsColorObject.Create(gclinen));
  fcolorlookup.addobject('magenta', TAdvGraphicsColorObject.Create(gcmagenta));
  fcolorlookup.addobject('maroon', TAdvGraphicsColorObject.Create(gcmaroon));
  fcolorlookup.addobject('mediumaquamarine', TAdvGraphicsColorObject.Create(gcmediumaquamarine));
  fcolorlookup.addobject('mediumblue', TAdvGraphicsColorObject.Create(gcmediumblue));
  fcolorlookup.addobject('mediumorchid', TAdvGraphicsColorObject.Create(gcmediumorchid));
  fcolorlookup.addobject('mediumpurple', TAdvGraphicsColorObject.Create(gcmediumpurple));
  fcolorlookup.addobject('mediumseagreen', TAdvGraphicsColorObject.Create(gcmediumseagreen));
  fcolorlookup.addobject('mediumslateblue', TAdvGraphicsColorObject.Create(gcmediumslateblue));
  fcolorlookup.addobject('mediumspringgreen', TAdvGraphicsColorObject.Create(gcmediumspringgreen));
  fcolorlookup.addobject('mediumturquoise', TAdvGraphicsColorObject.Create(gcmediumturquoise));
  fcolorlookup.addobject('mediumvioletred', TAdvGraphicsColorObject.Create(gcmediumvioletred));
  fcolorlookup.addobject('midnightblue', TAdvGraphicsColorObject.Create(gcmidnightblue));
  fcolorlookup.addobject('mintcream', TAdvGraphicsColorObject.Create(gcmintcream));
  fcolorlookup.addobject('mistyrose', TAdvGraphicsColorObject.Create(gcmistyrose));
  fcolorlookup.addobject('moccasin', TAdvGraphicsColorObject.Create(gcmoccasin));
  fcolorlookup.addobject('navajowhite', TAdvGraphicsColorObject.Create(gcnavajowhite));
  fcolorlookup.addobject('navy', TAdvGraphicsColorObject.Create(gcnavy));
  fcolorlookup.addobject('oldlace', TAdvGraphicsColorObject.Create(gcoldlace));
  fcolorlookup.addobject('olive', TAdvGraphicsColorObject.Create(gcolive));
  fcolorlookup.addobject('olivedrab', TAdvGraphicsColorObject.Create(gcolivedrab));
  fcolorlookup.addobject('orange', TAdvGraphicsColorObject.Create(gcorange));
  fcolorlookup.addobject('orangered', TAdvGraphicsColorObject.Create(gcorangered));
  fcolorlookup.addobject('orchid', TAdvGraphicsColorObject.Create(gcorchid));
  fcolorlookup.addobject('palegoldenrod', TAdvGraphicsColorObject.Create(gcpalegoldenrod));
  fcolorlookup.addobject('palegreen', TAdvGraphicsColorObject.Create(gcpalegreen));
  fcolorlookup.addobject('paleturquoise', TAdvGraphicsColorObject.Create(gcpaleturquoise));
  fcolorlookup.addobject('palevioletred', TAdvGraphicsColorObject.Create(gcpalevioletred));
  fcolorlookup.addobject('papayawhip', TAdvGraphicsColorObject.Create(gcpapayawhip));
  fcolorlookup.addobject('peachpuff', TAdvGraphicsColorObject.Create(gcpeachpuff));
  fcolorlookup.addobject('peru', TAdvGraphicsColorObject.Create(gcperu));
  fcolorlookup.addobject('pink', TAdvGraphicsColorObject.Create(gcpink));
  fcolorlookup.addobject('plum', TAdvGraphicsColorObject.Create(gcplum));
  fcolorlookup.addobject('powderblue', TAdvGraphicsColorObject.Create(gcpowderblue));
  fcolorlookup.addobject('purple', TAdvGraphicsColorObject.Create(gcpurple));
  fcolorlookup.addobject('red', TAdvGraphicsColorObject.Create(gcred));
  fcolorlookup.addobject('rosybrown', TAdvGraphicsColorObject.Create(gcrosybrown));
  fcolorlookup.addobject('royalblue', TAdvGraphicsColorObject.Create(gcroyalblue));
  fcolorlookup.addobject('saddlebrown', TAdvGraphicsColorObject.Create(gcsaddlebrown));
  fcolorlookup.addobject('salmon', TAdvGraphicsColorObject.Create(gcsalmon));
  fcolorlookup.addobject('sandybrown', TAdvGraphicsColorObject.Create(gcsandybrown));
  fcolorlookup.addobject('seagreen', TAdvGraphicsColorObject.Create(gcseagreen));
  fcolorlookup.addobject('seashell', TAdvGraphicsColorObject.Create(gcseashell));
  fcolorlookup.addobject('sienna', TAdvGraphicsColorObject.Create(gcsienna));
  fcolorlookup.addobject('skyblue', TAdvGraphicsColorObject.Create(gcskyblue));
  fcolorlookup.addobject('slateblue', TAdvGraphicsColorObject.Create(gcslateblue));
  fcolorlookup.addobject('slategray', TAdvGraphicsColorObject.Create(gcslategray));
  fcolorlookup.addobject('slategrey', TAdvGraphicsColorObject.Create(gcslategrey));
  fcolorlookup.addobject('snow', TAdvGraphicsColorObject.Create(gcsnow));
  fcolorlookup.addobject('springgreen', TAdvGraphicsColorObject.Create(gcspringgreen));
  fcolorlookup.addobject('steelblue', TAdvGraphicsColorObject.Create(gcsteelblue));
  fcolorlookup.addobject('violet', TAdvGraphicsColorObject.Create(gcviolet));
  fcolorlookup.addobject('thistle', TAdvGraphicsColorObject.Create(gcthistle));
  fcolorlookup.addobject('tan', TAdvGraphicsColorObject.Create(gctan));
  fcolorlookup.addobject('tomato', TAdvGraphicsColorObject.Create(gctomato));
  fcolorlookup.addobject('turquoise', TAdvGraphicsColorObject.Create(gcturquoise));
  fcolorlookup.addobject('wheat', TAdvGraphicsColorObject.Create(gcwheat));
  fcolorlookup.addobject('whitesmoke', TAdvGraphicsColorObject.Create(gcwhitesmoke));
  fcolorlookup.addobject('yellowgreen', TAdvGraphicsColorObject.Create(gcyellowgreen));
  fcolorlookup.addobject('red', TAdvGraphicsColorObject.Create(gcred));
  fcolorlookup.addobject('black', TAdvGraphicsColorObject.Create(gcblack));
  fcolorlookup.addobject('blue', TAdvGraphicsColorObject.Create(gcblue));
  fcolorlookup.addobject('green', TAdvGraphicsColorObject.Create(gcgreen));
  fcolorlookup.addobject('aqua', TAdvGraphicsColorObject.Create(gcaqua));
  fcolorlookup.addobject('yellow', TAdvGraphicsColorObject.Create(gcyellow));
  fcolorlookup.addobject('fuchsia', TAdvGraphicsColorObject.Create(gcfuchsia));
  fcolorlookup.addobject('white', TAdvGraphicsColorObject.Create(gcwhite));
  fcolorlookup.addobject('lime', TAdvGraphicsColorObject.Create(gclime));
  fcolorlookup.addobject('silver', TAdvGraphicsColorObject.Create(gcsilver));
  fcolorlookup.addobject('gray', TAdvGraphicsColorObject.Create(gcgray));
  fcolorlookup.addobject('olive', TAdvGraphicsColorObject.Create(gcolive));
  fcolorlookup.addobject('navy', TAdvGraphicsColorObject.Create(gcnavy));
  fcolorlookup.addobject('purple', TAdvGraphicsColorObject.Create(gcpurple));
  fcolorlookup.addobject('teal', TAdvGraphicsColorObject.Create(gcteal));
  fcolorlookup.addobject('orange', TAdvGraphicsColorObject.Create(gcorange));
  fcolorlookup.addobject('maroon', TAdvGraphicsColorObject.Create(gcmaroon));
  FColorLookup.Sort;
end;

finalization
begin
  DestroyColorLookup;
end;
{$ENDIF}

end.
