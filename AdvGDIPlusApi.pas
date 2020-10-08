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

unit AdvGDIPlusApi;

{$I TMSDEFS.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
//{$DEFINE DELPHI6_UP}

interface

{$IFDEF MSWINDOWS}

{$WARNINGS OFF}

(**************************************************************************\
*
*   GDI+ public header file
*
\**************************************************************************)

uses
  Windows,
  ActiveX,
  Math
  ;

type
  INT16   = type Smallint;
  UINT16  = type Word;
  PUINT16 = ^UINT16;
  UINT32  = type Cardinal;
  TSingleDynArray = array of Single;

(**************************************************************************\
*
*   GDI+ Private Memory Management APIs
*
\**************************************************************************)

const WINGDIPDLL = 'gdiplus.dll';

//----------------------------------------------------------------------------
// Memory Allocation APIs
//----------------------------------------------------------------------------

var
{.$EXTERNALSYM GdipAlloc}
GdipAlloc: function(size: ULONG): pointer; stdcall;
{.$EXTERNALSYM GdipFree}
GdipFree: procedure(ptr: pointer); stdcall;

(**************************************************************************\
*
*   GDI+ base memory allocation class
*
\**************************************************************************)

type
  TGdiplusBase = class
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

(**************************************************************************\
*
*   GDI+ Enumeration Types
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Default bezier flattening tolerance in device pixels.
//--------------------------------------------------------------------------

const
  {.$EXTERNALSYM FlatnessDefault}
  FlatnessDefault = 0.25;

//--------------------------------------------------------------------------
// Graphics and Container State cookies
//--------------------------------------------------------------------------
type
  {.$EXTERNALSYM GraphicsState}
  GraphicsState     = UINT;
  {.$EXTERNALSYM GraphicsContainer}
  GraphicsContainer = UINT;

//--------------------------------------------------------------------------
// Fill mode constants
//--------------------------------------------------------------------------

  {.$EXTERNALSYM FillMode}
  FillMode = (
    FillModeAlternate,        // 0
    FillModeWinding           // 1
  );
  TFillMode = FillMode;

//--------------------------------------------------------------------------
// Quality mode constants
//--------------------------------------------------------------------------

{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM QualityMode}
  QualityMode = (
    QualityModeInvalid   = -1,
    QualityModeDefault   =  0,
    QualityModeLow       =  1, // Best performance
    QualityModeHigh      =  2  // Best rendering quality
  );
  TQualityMode = QualityMode;
{$ELSE}
  {.$EXTERNALSYM QualityMode}
  QualityMode = Integer;
  const
    QualityModeInvalid   = -1;
    QualityModeDefault   =  0;
    QualityModeLow       =  1; // Best performance
    QualityModeHigh      =  2; // Best rendering quality
{$ENDIF}

//--------------------------------------------------------------------------
// Alpha Compositing mode constants
//--------------------------------------------------------------------------
type
  {.$EXTERNALSYM CompositingMode}
  CompositingMode = (
    CompositingModeSourceOver,    // 0
    CompositingModeSourceCopy     // 1
  );
  TCompositingMode = CompositingMode;

//--------------------------------------------------------------------------
// Alpha Compositing quality constants
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM CompositingQuality}
  CompositingQuality = (
    CompositingQualityInvalid          = ord(QualityModeInvalid),
    CompositingQualityDefault          = ord(QualityModeDefault),
    CompositingQualityHighSpeed        = ord(QualityModeLow),
    CompositingQualityHighQuality      = ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear
  );
  TCompositingQuality = CompositingQuality;
{$ELSE}
  {.$EXTERNALSYM CompositingQuality}
  CompositingQuality = Integer;
  const
    CompositingQualityInvalid          = QualityModeInvalid;
    CompositingQualityDefault          = QualityModeDefault;
    CompositingQualityHighSpeed        = QualityModeLow;
    CompositingQualityHighQuality      = QualityModeHigh;
    CompositingQualityGammaCorrected   = 3;
    CompositingQualityAssumeLinear     = 4;

type
  TCompositingQuality = CompositingQuality;
{$ENDIF}

//--------------------------------------------------------------------------
// Unit constants
//--------------------------------------------------------------------------

 // {.$EXTERNALSYM Unit}
  Unit_ = (
    UnitWorld,      // 0 -- World coordinate (non-physical unit)
    UnitDisplay,    // 1 -- Variable -- for PageTransform only
    UnitPixel,      // 2 -- Each unit is one device pixel.
    UnitPoint,      // 3 -- Each unit is a printer's point, or 1/72 inch.
    UnitInch,       // 4 -- Each unit is 1 inch.
    UnitDocument,   // 5 -- Each unit is 1/300 inch.
    UnitMillimeter  // 6 -- Each unit is 1 millimeter.
  );
  TUnit = Unit_;

//--------------------------------------------------------------------------
// MetafileFrameUnit
//
// The frameRect for creating a metafile can be specified in any of these
// units.  There is an extra frame unit value (MetafileFrameUnitGdi) so
// that units can be supplied in the same units that GDI expects for
// frame rects -- these units are in .01 (1/100ths) millimeter units
// as defined by GDI.
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM MetafileFrameUnit}
  MetafileFrameUnit = (
    MetafileFrameUnitPixel      = ord(UnitPixel),
    MetafileFrameUnitPoint      = ord(UnitPoint),
    MetafileFrameUnitInch       = ord(UnitInch),
    MetafileFrameUnitDocument   = ord(UnitDocument),
    MetafileFrameUnitMillimeter = ord(UnitMillimeter),
    MetafileFrameUnitGdi        // GDI compatible .01 MM units
  );
  TMetafileFrameUnit = MetafileFrameUnit;
{$ELSE}
  {.$EXTERNALSYM MetafileFrameUnit}
  MetafileFrameUnit = Integer;
  const
    MetafileFrameUnitPixel      = 2;
    MetafileFrameUnitPoint      = 3;
    MetafileFrameUnitInch       = 4;
    MetafileFrameUnitDocument   = 5;
    MetafileFrameUnitMillimeter = 6;
    MetafileFrameUnitGdi        = 7; // GDI compatible .01 MM units

type
  TMetafileFrameUnit = MetafileFrameUnit;
{$ENDIF}
//--------------------------------------------------------------------------
// Coordinate space identifiers
//--------------------------------------------------------------------------

  {.$EXTERNALSYM CoordinateSpace}
  CoordinateSpace = (
    CoordinateSpaceWorld,     // 0
    CoordinateSpacePage,      // 1
    CoordinateSpaceDevice     // 2
  );
  TCoordinateSpace = CoordinateSpace;

//--------------------------------------------------------------------------
// Various wrap modes for brushes
//--------------------------------------------------------------------------

  {.$EXTERNALSYM WrapMode}
  WrapMode = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );
  TWrapMode = WrapMode;

//--------------------------------------------------------------------------
// Various hatch styles
//--------------------------------------------------------------------------

  {.$EXTERNALSYM HatchStyle}
  HatchStyle = (
    HatchStyleHorizontal,                  // = 0,
    HatchStyleVertical,                    // = 1,
    HatchStyleForwardDiagonal,             // = 2,
    HatchStyleBackwardDiagonal,            // = 3,
    HatchStyleCross,                       // = 4,
    HatchStyleDiagonalCross,               // = 5,
    HatchStyle05Percent,                   // = 6,
    HatchStyle10Percent,                   // = 7,
    HatchStyle20Percent,                   // = 8,
    HatchStyle25Percent,                   // = 9,
    HatchStyle30Percent,                   // = 10,
    HatchStyle40Percent,                   // = 11,
    HatchStyle50Percent,                   // = 12,
    HatchStyle60Percent,                   // = 13,
    HatchStyle70Percent,                   // = 14,
    HatchStyle75Percent,                   // = 15,
    HatchStyle80Percent,                   // = 16,
    HatchStyle90Percent,                   // = 17,
    HatchStyleLightDownwardDiagonal,       // = 18,
    HatchStyleLightUpwardDiagonal,         // = 19,
    HatchStyleDarkDownwardDiagonal,        // = 20,
    HatchStyleDarkUpwardDiagonal,          // = 21,
    HatchStyleWideDownwardDiagonal,        // = 22,
    HatchStyleWideUpwardDiagonal,          // = 23,
    HatchStyleLightVertical,               // = 24,
    HatchStyleLightHorizontal,             // = 25,
    HatchStyleNarrowVertical,              // = 26,
    HatchStyleNarrowHorizontal,            // = 27,
    HatchStyleDarkVertical,                // = 28,
    HatchStyleDarkHorizontal,              // = 29,
    HatchStyleDashedDownwardDiagonal,      // = 30,
    HatchStyleDashedUpwardDiagonal,        // = 31,
    HatchStyleDashedHorizontal,            // = 32,
    HatchStyleDashedVertical,              // = 33,
    HatchStyleSmallConfetti,               // = 34,
    HatchStyleLargeConfetti,               // = 35,
    HatchStyleZigZag,                      // = 36,
    HatchStyleWave,                        // = 37,
    HatchStyleDiagonalBrick,               // = 38,
    HatchStyleHorizontalBrick,             // = 39,
    HatchStyleWeave,                       // = 40,
    HatchStylePlaid,                       // = 41,
    HatchStyleDivot,                       // = 42,
    HatchStyleDottedGrid,                  // = 43,
    HatchStyleDottedDiamond,               // = 44,
    HatchStyleShingle,                     // = 45,
    HatchStyleTrellis,                     // = 46,
    HatchStyleSphere,                      // = 47,
    HatchStyleSmallGrid,                   // = 48,
    HatchStyleSmallCheckerBoard,           // = 49,
    HatchStyleLargeCheckerBoard,           // = 50,
    HatchStyleOutlinedDiamond,             // = 51,
    HatchStyleSolidDiamond,                // = 52,

    HatchStyleTotal                        // = 53,
  );

  const
    HatchStyleLargeGrid = HatchStyleCross; // 4
    HatchStyleMin       = HatchStyleHorizontal;
    HatchStyleMax       = HatchStyleSolidDiamond;

type
  THatchStyle = HatchStyle;

//--------------------------------------------------------------------------
// Dash style constants
//--------------------------------------------------------------------------

  {.$EXTERNALSYM DashStyle}
  DashStyle = (
    DashStyleSolid,          // 0
    DashStyleDash,           // 1
    DashStyleDot,            // 2
    DashStyleDashDot,        // 3
    DashStyleDashDotDot,     // 4
    DashStyleCustom          // 5
  );
  TDashStyle = DashStyle;

//--------------------------------------------------------------------------
// Dash cap constants
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM DashCap}
  DashCap = (
    DashCapFlat             = 0,
    DashCapRound            = 2,
    DashCapTriangle         = 3
  );
  TDashCap = DashCap;
{$ELSE}
  {.$EXTERNALSYM DashCap}
  DashCap = Integer;
  const
    DashCapFlat             = 0;
    DashCapRound            = 2;
    DashCapTriangle         = 3;

type
  TDashCap = DashCap;
{$ENDIF}

//--------------------------------------------------------------------------
// Line cap constants (only the lowest 8 bits are used).
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM LineCap}
  LineCap = (
    LineCapFlat             = 0,
    LineCapSquare           = 1,
    LineCapRound            = 2,
    LineCapTriangle         = 3,

    LineCapNoAnchor         = $10, // corresponds to flat cap
    LineCapSquareAnchor     = $11, // corresponds to square cap
    LineCapRoundAnchor      = $12, // corresponds to round cap
    LineCapDiamondAnchor    = $13, // corresponds to triangle cap
    LineCapArrowAnchor      = $14, // no correspondence

    LineCapCustom           = $ff, // custom cap

    LineCapAnchorMask       = $f0  // mask to check for anchor or not.
  );
  TLineCap = LineCap;
{$ELSE}
  {.$EXTERNALSYM LineCap}
  LineCap = Integer;
  const
    LineCapFlat             = 0;
    LineCapSquare           = 1;
    LineCapRound            = 2;
    LineCapTriangle         = 3;

    LineCapNoAnchor         = $10; // corresponds to flat cap
    LineCapSquareAnchor     = $11; // corresponds to square cap
    LineCapRoundAnchor      = $12; // corresponds to round cap
    LineCapDiamondAnchor    = $13; // corresponds to triangle cap
    LineCapArrowAnchor      = $14; // no correspondence

    LineCapCustom           = $ff; // custom cap

    LineCapAnchorMask       = $f0; // mask to check for anchor or not.

type
  TLineCap = LineCap;
{$ENDIF}

//--------------------------------------------------------------------------
// Custom Line cap type constants
//--------------------------------------------------------------------------

  {.$EXTERNALSYM CustomLineCapType}
  CustomLineCapType = (
    CustomLineCapTypeDefault,
    CustomLineCapTypeAdjustableArrow
  );
  TCustomLineCapType = CustomLineCapType;

//--------------------------------------------------------------------------
// Line join constants
//--------------------------------------------------------------------------

  {.$EXTERNALSYM LineJoin}
  LineJoin = (
    LineJoinMiter,
    LineJoinBevel,
    LineJoinRound,
    LineJoinMiterClipped
  );
  TLineJoin = LineJoin;

//--------------------------------------------------------------------------
// Path point types (only the lowest 8 bits are used.)
//  The lowest 3 bits are interpreted as point type
//  The higher 5 bits are reserved for flags.
//--------------------------------------------------------------------------

{$IFDEF DELPHI6_UP}
  {$Z1}
  {.$EXTERNALSYM PathPointType}
  PathPointType = (
    PathPointTypeStart           = $00, // move
    PathPointTypeLine            = $01, // line
    PathPointTypeBezier          = $03, // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask    = $07, // type mask (lowest 3 bits).
    PathPointTypeDashMode        = $10, // currently in dash mode.
    PathPointTypePathMarker      = $20, // a marker for the path.
    PathPointTypeCloseSubpath    = $80, // closed flag

    // Path types used for advanced path.
    PathPointTypeBezier3         = $03  // cubic Bezier
  );
  TPathPointType = PathPointType;
  {$Z4}
{$ELSE}
  {.$EXTERNALSYM PathPointType}
  PathPointType = Byte;
  const
    PathPointTypeStart          : Byte = $00; // move
    PathPointTypeLine           : Byte = $01; // line
    PathPointTypeBezier         : Byte = $03; // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask   : Byte = $07; // type mask (lowest 3 bits).
    PathPointTypeDashMode       : Byte = $10; // currently in dash mode.
    PathPointTypePathMarker     : Byte = $20; // a marker for the path.
    PathPointTypeCloseSubpath   : Byte = $80; // closed flag

    // Path types used for advanced path.
    PathPointTypeBezier3        : Byte = $03;  // cubic Bezier

type
  TPathPointType = PathPointType;
{$ENDIF}

//--------------------------------------------------------------------------
// WarpMode constants
//--------------------------------------------------------------------------

  {.$EXTERNALSYM WarpMode}
  WarpMode = (
    WarpModePerspective,    // 0
    WarpModeBilinear        // 1
  );
  TWarpMode = WarpMode;

//--------------------------------------------------------------------------
// LineGradient Mode
//--------------------------------------------------------------------------

  {.$EXTERNALSYM LinearGradientMode}
  LinearGradientMode = (
    LinearGradientModeHorizontal,         // 0
    LinearGradientModeVertical,           // 1
    LinearGradientModeForwardDiagonal,    // 2
    LinearGradientModeBackwardDiagonal    // 3
  );
  TLinearGradientMode = LinearGradientMode;

//--------------------------------------------------------------------------
// Region Comine Modes
//--------------------------------------------------------------------------

  {.$EXTERNALSYM CombineMode}
  CombineMode = (
    CombineModeReplace,     // 0
    CombineModeIntersect,   // 1
    CombineModeUnion,       // 2
    CombineModeXor,         // 3
    CombineModeExclude,     // 4
    CombineModeComplement   // 5 (Exclude From)
  );
  TCombineMode = CombineMode;

//--------------------------------------------------------------------------
 // Image types
//--------------------------------------------------------------------------

  {.$EXTERNALSYM ImageType}
  ImageType = (
    ImageTypeUnknown,   // 0
    ImageTypeBitmap,    // 1
    ImageTypeMetafile   // 2
  );
  TImageType = ImageType;

//--------------------------------------------------------------------------
// Interpolation modes
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM InterpolationMode}
  InterpolationMode = (
    InterpolationModeInvalid          = ord(QualityModeInvalid),
    InterpolationModeDefault          = ord(QualityModeDefault),
    InterpolationModeLowQuality       = ord(QualityModeLow),
    InterpolationModeHighQuality      = ord(QualityModeHigh),
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic
  );
  TInterpolationMode = InterpolationMode;
{$ELSE}
  {.$EXTERNALSYM InterpolationMode}
  InterpolationMode = Integer;
  const
    InterpolationModeInvalid             = QualityModeInvalid;
    InterpolationModeDefault             = QualityModeDefault;
    InterpolationModeLowQuality          = QualityModeLow;
    InterpolationModeHighQuality         = QualityModeHigh;
    InterpolationModeBilinear            = 3;
    InterpolationModeBicubic             = 4;
    InterpolationModeNearestNeighbor     = 5;
    InterpolationModeHighQualityBilinear = 6;
    InterpolationModeHighQualityBicubic  = 7;

type
  TInterpolationMode = InterpolationMode;
{$ENDIF}

//--------------------------------------------------------------------------
// Pen types
//--------------------------------------------------------------------------

  {.$EXTERNALSYM PenAlignment}
  PenAlignment = (
    PenAlignmentCenter,
    PenAlignmentInset
  );
  TPenAlignment = PenAlignment;

//--------------------------------------------------------------------------
// Brush types
//--------------------------------------------------------------------------

  {.$EXTERNALSYM BrushType}
  BrushType = (
   BrushTypeSolidColor,
   BrushTypeHatchFill,
   BrushTypeTextureFill,
   BrushTypePathGradient,
   BrushTypeLinearGradient 
  );
  TBrushType = BrushType;

//--------------------------------------------------------------------------
// Pen's Fill types
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM PenType}
  PenType = (
   PenTypeSolidColor       =  ord(BrushTypeSolidColor),
   PenTypeHatchFill        =  ord(BrushTypeHatchFill),
   PenTypeTextureFill      =  ord(BrushTypeTextureFill),
   PenTypePathGradient     =  ord(BrushTypePathGradient),
   PenTypeLinearGradient   =  ord(BrushTypeLinearGradient),
   PenTypeUnknown          = -1
  );
  TPenType = PenType;
{$ELSE}
  {.$EXTERNALSYM PenType}
  PenType = Integer;
  const
    PenTypeSolidColor       =  0;
    PenTypeHatchFill        =  1;
    PenTypeTextureFill      =  2;
    PenTypePathGradient     =  3;
    PenTypeLinearGradient   =  4;
    PenTypeUnknown          = -1;

type
  TPenType = PenType;
{$ENDIF}

//--------------------------------------------------------------------------
// Matrix Order
//--------------------------------------------------------------------------

  {.$EXTERNALSYM MatrixOrder}
  MatrixOrder = (
    MatrixOrderPrepend,
    MatrixOrderAppend
  );
  TMatrixOrder = MatrixOrder;

//--------------------------------------------------------------------------
// Generic font families
//--------------------------------------------------------------------------

  {.$EXTERNALSYM GenericFontFamily}
  GenericFontFamily = (
    GenericFontFamilySerif,
    GenericFontFamilySansSerif,
    GenericFontFamilyMonospace
  );
  TGenericFontFamily = GenericFontFamily;

//--------------------------------------------------------------------------
// FontStyle: face types and common styles
//--------------------------------------------------------------------------
type
  {.$EXTERNALSYM FontStyle}
  FontStyle = Integer;
  const
    FontStyleRegular    = Integer(0);
    FontStyleBold       = Integer(1);
    FontStyleItalic     = Integer(2);
    FontStyleBoldItalic = Integer(3);
    FontStyleUnderline  = Integer(4);
    FontStyleStrikeout  = Integer(8);
  Type
  TFontStyle = FontStyle;

//---------------------------------------------------------------------------
// Smoothing Mode
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM SmoothingMode}
  SmoothingMode = (
    SmoothingModeInvalid     = ord(QualityModeInvalid),
    SmoothingModeDefault     = ord(QualityModeDefault),
    SmoothingModeHighSpeed   = ord(QualityModeLow),
    SmoothingModeHighQuality = ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias
  );
  TSmoothingMode = SmoothingMode;
{$ELSE}
  {.$EXTERNALSYM SmoothingMode}
  SmoothingMode = Integer;
  const
    SmoothingModeInvalid     = QualityModeInvalid;
    SmoothingModeDefault     = QualityModeDefault;
    SmoothingModeHighSpeed   = QualityModeLow;
    SmoothingModeHighQuality = QualityModeHigh;
    SmoothingModeNone        = 3;
    SmoothingModeAntiAlias   = 4;

type
  TSmoothingMode = SmoothingMode;
{$ENDIF}

//---------------------------------------------------------------------------
// Pixel Format Mode
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM PixelOffsetMode}
  PixelOffsetMode = (
    PixelOffsetModeInvalid     = Ord(QualityModeInvalid),
    PixelOffsetModeDefault     = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed   = Ord(QualityModeLow),
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone,    // No pixel offset
    PixelOffsetModeHalf     // Offset by -0.5, -0.5 for fast anti-alias perf
  );
  TPixelOffsetMode = PixelOffsetMode;
{$ELSE}
  {.$EXTERNALSYM PixelOffsetMode}
  PixelOffsetMode = Integer;
  const
    PixelOffsetModeInvalid     = QualityModeInvalid;
    PixelOffsetModeDefault     = QualityModeDefault;
    PixelOffsetModeHighSpeed   = QualityModeLow;
    PixelOffsetModeHighQuality = QualityModeHigh;
    PixelOffsetModeNone        = 3;    // No pixel offset
    PixelOffsetModeHalf        = 4;    // Offset by -0.5, -0.5 for fast anti-alias perf

type
  TPixelOffsetMode = PixelOffsetMode;
{$ENDIF}

//---------------------------------------------------------------------------
// Text Rendering Hint
//---------------------------------------------------------------------------

  {.$EXTERNALSYM TextRenderingHint}
  TextRenderingHint = (
    TextRenderingHintSystemDefault,                // Glyph with system default rendering hint
    TextRenderingHintSingleBitPerPixelGridFit,     // Glyph bitmap with hinting
    TextRenderingHintSingleBitPerPixel,            // Glyph bitmap without hinting
    TextRenderingHintAntiAliasGridFit,             // Glyph anti-alias bitmap with hinting
    TextRenderingHintAntiAlias,                    // Glyph anti-alias bitmap without hinting
    TextRenderingHintClearTypeGridFit              // Glyph CT bitmap with hinting
  );
  TTextRenderingHint = TextRenderingHint;

//---------------------------------------------------------------------------
// Metafile Types
//---------------------------------------------------------------------------

  {.$EXTERNALSYM MetafileType}
  MetafileType = (
    MetafileTypeInvalid,            // Invalid metafile
    MetafileTypeWmf,                // Standard WMF
    MetafileTypeWmfPlaceable,       // Placeable WMF
    MetafileTypeEmf,                // EMF (not EMF+)
    MetafileTypeEmfPlusOnly,        // EMF+ without dual, down-level records
    MetafileTypeEmfPlusDual         // EMF+ with dual, down-level records
  );
  TMetafileType = MetafileType;

//---------------------------------------------------------------------------
// Specifies the type of EMF to record
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM EmfType}
  EmfType = (
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf),          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual)   // both EMF+ and EMF
  );
  TEmfType = EmfType;
{$ELSE}
  {.$EXTERNALSYM EmfType}
  EmfType = Integer;
  const
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf);          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly);  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual);   // both EMF+ and EMF

type
  TEmfType = EmfType;
{$ENDIF}

//---------------------------------------------------------------------------
// EMF+ Persistent object types
//---------------------------------------------------------------------------

  {.$EXTERNALSYM ObjectType}
  ObjectType = (
    ObjectTypeInvalid,
    ObjectTypeBrush,
    ObjectTypePen,
    ObjectTypePath,
    ObjectTypeRegion,
    ObjectTypeImage,
    ObjectTypeFont,
    ObjectTypeStringFormat,
    ObjectTypeImageAttributes,
    ObjectTypeCustomLineCap
  );
  TObjectType = ObjectType;

const
  ObjectTypeMax = ObjectTypeCustomLineCap;
  ObjectTypeMin = ObjectTypeBrush;

function ObjectTypeIsValid(type_: ObjectType): BOOL;

//---------------------------------------------------------------------------
// EMF+ Records
//---------------------------------------------------------------------------

  // We have to change the WMF record numbers so that they don't conflict with
  // the EMF and EMF+ record numbers.

const
  GDIP_EMFPLUS_RECORD_BASE      = $00004000;
  {.$EXTERNALSYM GDIP_EMFPLUS_RECORD_BASE}
  GDIP_WMF_RECORD_BASE          = $00010000;
  {.$EXTERNALSYM GDIP_WMF_RECORD_BASE}

// macros
function GDIP_WMF_RECORD_TO_EMFPLUS(n: integer): Integer;
function GDIP_EMFPLUS_RECORD_TO_WMF(n: integer): Integer;
function GDIP_IS_WMF_RECORDTYPE(n: integer): BOOL;

//---------------------------------------------------------------------------
// StringFormatFlags
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// String format flags
//
//  DirectionRightToLeft          - For horizontal text, the reading order is
//                                  right to left. This value is called
//                                  the base embedding level by the Unicode
//                                  bidirectional engine.
//                                  For vertical text, columns are read from
//                                  right to left.
//                                  By default, horizontal or vertical text is
//                                  read from left to right.
//
//  DirectionVertical             - Individual lines of text are vertical. In
//                                  each line, characters progress from top to
//                                  bottom.
//                                  By default, lines of text are horizontal,
//                                  each new line below the previous line.
//
//  NoFitBlackBox                 - Allows parts of glyphs to overhang the
//                                  bounding rectangle.
//                                  By default glyphs are first aligned
//                                  inside the margines, then any glyphs which
//                                  still overhang the bounding box are
//                                  repositioned to avoid any overhang.
//                                  For example when an italic
//                                  lower case letter f in a font such as
//                                  Garamond is aligned at the far left of a
//                                  rectangle, the lower part of the f will
//                                  reach slightly further left than the left
//                                  edge of the rectangle. Setting this flag
//                                  will ensure the character aligns visually
//                                  with the lines above and below, but may
//                                  cause some pixels outside the formatting
//                                  rectangle to be clipped or painted.
//
//  DisplayFormatControl          - Causes control characters such as the
//                                  left-to-right mark to be shown in the
//                                  output with a representative glyph.
//
//  NoFontFallback                - Disables fallback to alternate fonts for
//                                  characters not supported in the requested
//                                  font. Any missing characters will be
//                                  be displayed with the fonts missing glyph,
//                                  usually an open square.
//
//  NoWrap                        - Disables wrapping of text between lines
//                                  when formatting within a rectangle.
//                                  NoWrap is implied when a point is passed
//                                  instead of a rectangle, or when the
//                                  specified rectangle has a zero line length.
//
//  NoClip                        - By default text is clipped to the
//                                  formatting rectangle. Setting NoClip
//                                  allows overhanging pixels to affect the
//                                  device outside the formatting rectangle.
//                                  Pixels at the end of the line may be
//                                  affected if the glyphs overhang their
//                                  cells, and either the NoFitBlackBox flag
//                                  has been set, or the glyph extends to far
//                                  to be fitted.
//                                  Pixels above/before the first line or
//                                  below/after the last line may be affected
//                                  if the glyphs extend beyond their cell
//                                  ascent / descent. This can occur rarely
//                                  with unusual diacritic mark combinations.

//---------------------------------------------------------------------------
type
  {.$EXTERNALSYM StringFormatFlags}
  StringFormatFlags = Integer;
  const
    StringFormatFlagsDirectionRightToLeft        = $00000001;
    StringFormatFlagsDirectionVertical           = $00000002;
    StringFormatFlagsNoFitBlackBox               = $00000004;
    StringFormatFlagsDisplayFormatControl        = $00000020;
    StringFormatFlagsNoFontFallback              = $00000400;
    StringFormatFlagsMeasureTrailingSpaces       = $00000800;
    StringFormatFlagsNoWrap                      = $00001000;
    StringFormatFlagsLineLimit                   = $00002000;

    StringFormatFlagsNoClip                      = $00004000;

Type
  TStringFormatFlags = StringFormatFlags;

//---------------------------------------------------------------------------
// StringTrimming
//---------------------------------------------------------------------------

  {.$EXTERNALSYM StringTrimming}
  StringTrimming = (
    StringTrimmingNone,
    StringTrimmingCharacter,
    StringTrimmingWord,
    StringTrimmingEllipsisCharacter,
    StringTrimmingEllipsisWord,
    StringTrimmingEllipsisPath
  );
  TStringTrimming = StringTrimming;

//---------------------------------------------------------------------------
// National language digit substitution
//---------------------------------------------------------------------------

  {.$EXTERNALSYM StringDigitSubstitute}
  StringDigitSubstitute = (
    StringDigitSubstituteUser,          // As NLS setting
    StringDigitSubstituteNone,
    StringDigitSubstituteNational,
    StringDigitSubstituteTraditional
  );
  TStringDigitSubstitute = StringDigitSubstitute;
  PStringDigitSubstitute = ^TStringDigitSubstitute;

//---------------------------------------------------------------------------
// Hotkey prefix interpretation
//---------------------------------------------------------------------------

  {.$EXTERNALSYM HotkeyPrefix}
  HotkeyPrefix = (
    HotkeyPrefixNone,
    HotkeyPrefixShow,
    HotkeyPrefixHide
  );
  THotkeyPrefix = HotkeyPrefix;

//---------------------------------------------------------------------------
// String alignment flags
//---------------------------------------------------------------------------

  {.$EXTERNALSYM StringAlignment}
  StringAlignment = (
    // Left edge for left-to-right text,
    // right for right-to-left text,
    // and top for vertical
    StringAlignmentNear,
    StringAlignmentCenter,
    StringAlignmentFar
  );
  TStringAlignment = StringAlignment;

//---------------------------------------------------------------------------
// DriverStringOptions
//---------------------------------------------------------------------------

  {.$EXTERNALSYM DriverStringOptions}
  DriverStringOptions = Integer;
  const
    DriverStringOptionsCmapLookup             = 1;
    DriverStringOptionsVertical               = 2;
    DriverStringOptionsRealizedAdvance        = 4;
    DriverStringOptionsLimitSubpixel          = 8;

type
  TDriverStringOptions = DriverStringOptions;

//---------------------------------------------------------------------------
// Flush Intention flags
//---------------------------------------------------------------------------

  {.$EXTERNALSYM FlushIntention}
  FlushIntention = (
    FlushIntentionFlush,  // Flush all batched rendering operations
    FlushIntentionSync    // Flush all batched rendering operations
                          // and wait for them to complete
  );
  TFlushIntention = FlushIntention;

//---------------------------------------------------------------------------
// Image encoder parameter related types
//---------------------------------------------------------------------------

  {.$EXTERNALSYM EncoderParameterValueType}
  EncoderParameterValueType = Integer;
  const
    EncoderParameterValueTypeByte          : Integer = 1;    // 8-bit unsigned int
    EncoderParameterValueTypeASCII         : Integer = 2;    // 8-bit byte containing one 7-bit ASCII
                                                             // code. NULL terminated.
    EncoderParameterValueTypeShort         : Integer = 3;    // 16-bit unsigned int
    EncoderParameterValueTypeLong          : Integer = 4;    // 32-bit unsigned int
    EncoderParameterValueTypeRational      : Integer = 5;    // Two Longs. The first Long is the
                                                             // numerator, the second Long expresses the
                                                             // denomintor.
    EncoderParameterValueTypeLongRange     : Integer = 6;    // Two longs which specify a range of
                                                             // integer values. The first Long specifies
                                                             // the lower end and the second one
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
    EncoderParameterValueTypeUndefined     : Integer = 7;    // 8-bit byte that can take any value
                                                             // depending on field definition
    EncoderParameterValueTypeRationalRange : Integer = 8;    // Two Rationals. The first Rational
                                                             // specifies the lower end and the second
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
type
  TEncoderParameterValueType = EncoderParameterValueType;

//---------------------------------------------------------------------------
// Image encoder value types
//---------------------------------------------------------------------------

  {.$EXTERNALSYM EncoderValue}
  EncoderValue = (
    EncoderValueColorTypeCMYK,
    EncoderValueColorTypeYCCK,
    EncoderValueCompressionLZW,
    EncoderValueCompressionCCITT3,
    EncoderValueCompressionCCITT4,
    EncoderValueCompressionRle,
    EncoderValueCompressionNone,
    EncoderValueScanMethodInterlaced,
    EncoderValueScanMethodNonInterlaced,
    EncoderValueVersionGif87,
    EncoderValueVersionGif89,
    EncoderValueRenderProgressive,
    EncoderValueRenderNonProgressive,
    EncoderValueTransformRotate90,
    EncoderValueTransformRotate180,
    EncoderValueTransformRotate270,
    EncoderValueTransformFlipHorizontal,
    EncoderValueTransformFlipVertical,
    EncoderValueMultiFrame,
    EncoderValueLastFrame,
    EncoderValueFlush,
    EncoderValueFrameDimensionTime,
    EncoderValueFrameDimensionResolution,
    EncoderValueFrameDimensionPage
  );
  TEncoderValue = EncoderValue;

//---------------------------------------------------------------------------
// Conversion of Emf To WMF Bits flags
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM EmfToWmfBitsFlags}
  EmfToWmfBitsFlags = (
    EmfToWmfBitsFlagsDefault          = $00000000,
    EmfToWmfBitsFlagsEmbedEmf         = $00000001,
    EmfToWmfBitsFlagsIncludePlaceable = $00000002,
    EmfToWmfBitsFlagsNoXORClip        = $00000004
  );
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;
{$ELSE}
  {.$EXTERNALSYM EmfToWmfBitsFlags}
  EmfToWmfBitsFlags = Integer;
  const
    EmfToWmfBitsFlagsDefault          = $00000000;
    EmfToWmfBitsFlagsEmbedEmf         = $00000001;
    EmfToWmfBitsFlagsIncludePlaceable = $00000002;
    EmfToWmfBitsFlagsNoXORClip        = $00000004;
    
type
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;
{$ENDIF}
(**************************************************************************\
*
*   GDI+ Types
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Callback functions
//--------------------------------------------------------------------------

  {.$EXTERNALSYM ImageAbort}
  ImageAbort = function: BOOL; stdcall;
  {.$EXTERNALSYM DrawImageAbort}
  DrawImageAbort         = ImageAbort;
  {.$EXTERNALSYM GetThumbnailImageAbort}
  GetThumbnailImageAbort = ImageAbort;

//--------------------------------------------------------------------------
// Primitive data types
//
// NOTE:
//  Types already defined in standard header files:
//      INT8
//      UINT8
//      INT16
//      UINT16
//      INT32
//      UINT32
//      INT64
//      UINT64
//
//  Avoid using the following types:
//      LONG - use INT
//      ULONG - use UINT
//      DWORD - use UINT32
//--------------------------------------------------------------------------

const
  { from float.h }
  FLT_MAX =  3.402823466e+38; // max value
  FLT_MIN =  1.175494351e-38; // min positive value

  REAL_MAX           = FLT_MAX;
  {.$EXTERNALSYM REAL_MAX}
  REAL_MIN           = FLT_MIN;
  {.$EXTERNALSYM REAL_MIN}
  REAL_TOLERANCE     = (FLT_MIN * 100);
  {.$EXTERNALSYM REAL_TOLERANCE}
  REAL_EPSILON       = 1.192092896e-07;        // FLT_EPSILON
  {.$EXTERNALSYM REAL_EPSILON}

//--------------------------------------------------------------------------
// Status return values from GDI+ methods
//--------------------------------------------------------------------------
type
  {.$EXTERNALSYM Status}
  Status = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported
  );
  TStatus = Status;

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPSizeF = ^TGPSizeF;
  TGPSizeF = packed record
    Width  : Single;
    Height : Single;
  end;

  function MakeSize(Width, Height: Single): TGPSizeF; overload;

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PGPSize = ^TGPSize;
  TGPSize = packed record
    Width  : Integer;
    Height : Integer;
  end;

  function MakeSize(Width, Height: Integer): TGPSize; overload;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPPointF = ^TGPPointF;
  TGPPointF = packed record
    X : Single;
    Y : Single;
  end;
  TPointFDynArray = array of TGPPointF;

  function MakePoint(X, Y: Single): TGPPointF; overload;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PGPPoint = ^TGPPoint;
  TGPPoint = packed record
    X : Integer;
    Y : Integer;
  end;
  TPointDynArray = array of TGPPoint;

  function MakePoint(X, Y: Integer): TGPPoint; overload;

//--------------------------------------------------------------------------
// Represents a rectangle in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPRectF = ^TGPRectF;
  TGPRectF = packed record
    X     : Single;
    Y     : Single;
    Width : Single;
    Height: Single;
  end;
  TRectFDynArray = array of TGPRectF;

  function MakeRect(x, y, width, height: Single): TGPRectF; overload;
  function MakeRect(location: TGPPointF; size: TGPSizeF): TGPRectF; overload;

type
  PGPRect = ^TGPRect;
  TGPRect = packed record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;
  end;
  TRectDynArray = array of TGPRect;

  function MakeRect(x, y, width, height: Integer): TGPRect; overload;
  function MakeRect(location: TGPPoint; size: TGPSize): TGPRect; overload;
  function MakeRect(const Rect: TRect): TGPRect; overload;

type
  TPathData = packed class
  public
    Count  : Integer;
    Points : PGPPointF;
    Types  : PBYTE;
    constructor Create;
    destructor Destroy; override;
  end;

  PCharacterRange = ^TCharacterRange;
  TCharacterRange = packed record
    First  : Integer;
    Length : Integer;
  end;

  function MakeCharacterRange(First, Length: Integer): TCharacterRange;

(**************************************************************************
*
*   GDI+ Startup and Shutdown APIs
*
**************************************************************************)
type
  {.$EXTERNALSYM DebugEventLevel}
  DebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning
  );
  TDebugEventLevel = DebugEventLevel;

  // Callback function that GDI+ can call, on debug builds, for assertions
  // and warnings.

  {.$EXTERNALSYM DebugEventProc}
  DebugEventProc = procedure(level: DebugEventLevel; message: PChar); stdcall;

  // Notification functions which the user must call appropriately if
  // "SuppressBackgroundThread" (below) is set.

  {.$EXTERNALSYM NotificationHookProc}
  NotificationHookProc = function(out token: ULONG): Status; stdcall;
  {.$EXTERNALSYM NotificationUnhookProc}
  NotificationUnhookProc = procedure(token: ULONG); stdcall;

  // Input structure for GdiplusStartup

  {.$EXTERNALSYM GdiplusStartupInput}
  GdiplusStartupInput = packed record
    GdiplusVersion          : Cardinal;       // Must be 1
    DebugEventCallback      : DebugEventProc; // Ignored on free builds
    SuppressBackgroundThread: BOOL;           // FALSE unless you're prepared to call
                                              // the hook/unhook functions properly
    SuppressExternalCodecs  : BOOL;           // FALSE unless you want GDI+ only to use
  end;                                        // its internal image codecs.
  TGdiplusStartupInput = GdiplusStartupInput;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

  // Output structure for GdiplusStartup()

  {.$EXTERNALSYM GdiplusStartupOutput}
  GdiplusStartupOutput = packed record
    // The following 2 fields are NULL if SuppressBackgroundThread is FALSE.
    // Otherwise, they are functions which must be called appropriately to
    // replace the background thread.
    //
    // These should be called on the application's main message loop - i.e.
    // a message loop which is active for the lifetime of GDI+.
    // "NotificationHook" should be called before starting the loop,
    // and "NotificationUnhook" should be called after the loop ends.

    NotificationHook  : NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  TGdiplusStartupOutput = GdiplusStartupOutput;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

  // GDI+ initialization. Must not be called from DllMain - can cause deadlock.
  //
  // Must be called before GDI+ API's or constructors are used.
  //
  // token  - may not be NULL - accepts a token to be passed in the corresponding
  //          GdiplusShutdown call.
  // input  - may not be NULL
  // output - may be NULL only if input->SuppressBackgroundThread is FALSE.

var
  {.$EXTERNALSYM GdiplusStartup}
GdiplusStartup: function(out token: ULONG; input: PGdiplusStartupInput;
   output: PGdiplusStartupOutput): Status; stdcall;

  // GDI+ termination. Must be called before GDI+ is unloaded.
  // Must not be called from DllMain - can cause deadlock.
  //
  // GDI+ API's may not be called after GdiplusShutdown. Pay careful attention
  // to GDI+ object destructors.

  {.$EXTERNALSYM GdiplusShutdown}
GdiplusShutdown: procedure(token: ULONG); stdcall;  


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
* Module Name:
*   Gdiplus Pixel Formats
* Abstract:
*   GDI+ Pixel Formats
*
\**************************************************************************)

type
  PARGB  = ^ARGB;
  ARGB   = DWORD;
  {.$EXTERNALSYM ARGB}
  ARGB64 = Int64;
  {.$EXTERNALSYM ARGB64}

const
  ALPHA_SHIFT = 24;
  {.$EXTERNALSYM ALPHA_SHIFT}
  RED_SHIFT   = 16;
  {.$EXTERNALSYM RED_SHIFT}
  GREEN_SHIFT = 8;
  {.$EXTERNALSYM GREEN_SHIFT}
  BLUE_SHIFT  = 0;
  {.$EXTERNALSYM BLUE_SHIFT}
  ALPHA_MASK  = (ARGB($ff) shl ALPHA_SHIFT);
  {.$EXTERNALSYM ALPHA_MASK}

  // In-memory pixel data formats:
  // bits 0-7 = format index
  // bits 8-15 = pixel size (in bits)
  // bits 16-23 = flags
  // bits 24-31 = reserved

type
  PixelFormat = Integer;
  {.$EXTERNALSYM PixelFormat}
  TPixelFormat = PixelFormat;

const
  PixelFormatIndexed     = $00010000; // Indexes into a palette
  {.$EXTERNALSYM PixelFormatIndexed}
  PixelFormatGDI         = $00020000; // Is a GDI-supported format
  {.$EXTERNALSYM PixelFormatGDI}
  PixelFormatAlpha       = $00040000; // Has an alpha component
  {.$EXTERNALSYM PixelFormatAlpha}
  PixelFormatPAlpha      = $00080000; // Pre-multiplied alpha
  {.$EXTERNALSYM PixelFormatPAlpha}
  PixelFormatExtended    = $00100000; // Extended color 16 bits/channel
  {.$EXTERNALSYM PixelFormatExtended}
  PixelFormatCanonical   = $00200000;
  {.$EXTERNALSYM PixelFormatCanonical}

  PixelFormatUndefined      = 0;
  {.$EXTERNALSYM PixelFormatUndefined}
  PixelFormatDontCare       = 0;
  {.$EXTERNALSYM PixelFormatDontCare}

  PixelFormat1bppIndexed    = (1  or ( 1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat1bppIndexed}
  PixelFormat4bppIndexed    = (2  or ( 4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat4bppIndexed}
  PixelFormat8bppIndexed    = (3  or ( 8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat8bppIndexed}
  PixelFormat16bppGrayScale = (4  or (16 shl 8) or PixelFormatExtended);
  {.$EXTERNALSYM PixelFormat16bppGrayScale}
  PixelFormat16bppRGB555    = (5  or (16 shl 8) or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat16bppRGB555}
  PixelFormat16bppRGB565    = (6  or (16 shl 8) or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat16bppRGB565}
  PixelFormat16bppARGB1555  = (7  or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat16bppARGB1555}
  PixelFormat24bppRGB       = (8  or (24 shl 8) or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat24bppRGB}
  PixelFormat32bppRGB       = (9  or (32 shl 8) or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat32bppRGB}
  PixelFormat32bppARGB      = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  {.$EXTERNALSYM PixelFormat32bppARGB}
  PixelFormat32bppPARGB     = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat32bppPARGB}
  PixelFormat48bppRGB       = (12 or (48 shl 8) or PixelFormatExtended);
  {.$EXTERNALSYM PixelFormat48bppRGB}
  PixelFormat64bppARGB      = (13 or (64 shl 8) or PixelFormatAlpha  or PixelFormatCanonical or PixelFormatExtended);
  {.$EXTERNALSYM PixelFormat64bppARGB}
  PixelFormat64bppPARGB     = (14 or (64 shl 8) or PixelFormatAlpha  or PixelFormatPAlpha or PixelFormatExtended);
  {.$EXTERNALSYM PixelFormat64bppPARGB}
  PixelFormatMax            = 15;
  {.$EXTERNALSYM PixelFormatMax}

{.$EXTERNALSYM GetPixelFormatSize}
function GetPixelFormatSize(pixfmt: PixelFormat): UINT;
{.$EXTERNALSYM IsIndexedPixelFormat}
function IsIndexedPixelFormat(pixfmt: PixelFormat): BOOL;
{.$EXTERNALSYM IsAlphaPixelFormat}
function IsAlphaPixelFormat(pixfmt: PixelFormat): BOOL;
{.$EXTERNALSYM IsExtendedPixelFormat}
function IsExtendedPixelFormat(pixfmt: PixelFormat): BOOL;

//--------------------------------------------------------------------------
// Determine if the Pixel Format is Canonical format:
//   PixelFormat32bppARGB
//   PixelFormat32bppPARGB
//   PixelFormat64bppARGB
//   PixelFormat64bppPARGB
//--------------------------------------------------------------------------

{.$EXTERNALSYM IsCanonicalPixelFormat}
function IsCanonicalPixelFormat(pixfmt: PixelFormat): BOOL;

{$IFDEF DELPHI6_UP}
type
  {.$EXTERNALSYM PaletteFlags}
  PaletteFlags = (
    PaletteFlagsHasAlpha    = $0001,
    PaletteFlagsGrayScale   = $0002,
    PaletteFlagsHalftone    = $0004
  );
  TPaletteFlags = PaletteFlags;
{$ELSE}
type
  {.$EXTERNALSYM PaletteFlags}
  PaletteFlags = Integer;
  const
    PaletteFlagsHasAlpha    = $0001;
    PaletteFlagsGrayScale   = $0002;
    PaletteFlagsHalftone    = $0004;

type
  TPaletteFlags = PaletteFlags;
{$ENDIF}

  {.$EXTERNALSYM ColorPalette}
  ColorPalette = packed record
    Flags  : UINT ;                 // Palette flags
    Count  : UINT ;                 // Number of color entries
    Entries: array [0..0] of ARGB ; // Palette color entries
  end;

  TColorPalette = ColorPalette;
  PColorPalette = ^TColorPalette;

(**************************************************************************\
*
*   GDI+ Color Object
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Color mode
//----------------------------------------------------------------------------

  {.$EXTERNALSYM ColorMode}
  ColorMode = (
    ColorModeARGB32,
    ColorModeARGB64
  );
  TColorMode = ColorMode;

//----------------------------------------------------------------------------
// Color Channel flags 
//----------------------------------------------------------------------------

  {.$EXTERNALSYM ColorChannelFlags}
  ColorChannelFlags = (
    ColorChannelFlagsC,
    ColorChannelFlagsM,
    ColorChannelFlagsY,
    ColorChannelFlagsK,
    ColorChannelFlagsLast
  );
  TColorChannelFlags = ColorChannelFlags;

//----------------------------------------------------------------------------
// Color
//----------------------------------------------------------------------------

  // Common color constants
const
  aclAliceBlue            = $FFF0F8FF;
  aclAntiqueWhite         = $FFFAEBD7;
  aclAqua                 = $FF00FFFF;
  aclAquamarine           = $FF7FFFD4;
  aclAzure                = $FFF0FFFF;
  aclBeige                = $FFF5F5DC;
  aclBisque               = $FFFFE4C4;
  aclBlack                = $FF000000;
  aclBlanchedAlmond       = $FFFFEBCD;
  aclBlue                 = $FF0000FF;
  aclBlueViolet           = $FF8A2BE2;
  aclBrown                = $FFA52A2A;
  aclBurlyWood            = $FFDEB887;
  aclCadetBlue            = $FF5F9EA0;
  aclChartreuse           = $FF7FFF00;
  aclChocolate            = $FFD2691E;
  aclCoral                = $FFFF7F50;
  aclCornflowerBlue       = $FF6495ED;
  aclCornsilk             = $FFFFF8DC;
  aclCrimson              = $FFDC143C;
  aclCyan                 = $FF00FFFF;
  aclDarkBlue             = $FF00008B;
  aclDarkCyan             = $FF008B8B;
  aclDarkGoldenrod        = $FFB8860B;
  aclDarkGray             = $FFA9A9A9;
  aclDarkGreen            = $FF006400;
  aclDarkKhaki            = $FFBDB76B;
  aclDarkMagenta          = $FF8B008B;
  aclDarkOliveGreen       = $FF556B2F;
  aclDarkOrange           = $FFFF8C00;
  aclDarkOrchid           = $FF9932CC;
  aclDarkRed              = $FF8B0000;
  aclDarkSalmon           = $FFE9967A;
  aclDarkSeaGreen         = $FF8FBC8B;
  aclDarkSlateBlue        = $FF483D8B;
  aclDarkSlateGray        = $FF2F4F4F;
  aclDarkTurquoise        = $FF00CED1;
  aclDarkViolet           = $FF9400D3;
  aclDeepPink             = $FFFF1493;
  aclDeepSkyBlue          = $FF00BFFF;
  aclDimGray              = $FF696969;
  aclDodgerBlue           = $FF1E90FF;
  aclFirebrick            = $FFB22222;
  aclFloralWhite          = $FFFFFAF0;
  aclForestGreen          = $FF228B22;
  aclFuchsia              = $FFFF00FF;
  aclGainsboro            = $FFDCDCDC;
  aclGhostWhite           = $FFF8F8FF;
  aclGold                 = $FFFFD700;
  aclGoldenrod            = $FFDAA520;
  aclGray                 = $FF808080;
  aclGreen                = $FF008000;
  aclGreenYellow          = $FFADFF2F;
  aclHoneydew             = $FFF0FFF0;
  aclHotPink              = $FFFF69B4;
  aclIndianRed            = $FFCD5C5C;
  aclIndigo               = $FF4B0082;
  aclIvory                = $FFFFFFF0;
  aclKhaki                = $FFF0E68C;
  aclLavender             = $FFE6E6FA;
  aclLavenderBlush        = $FFFFF0F5;
  aclLawnGreen            = $FF7CFC00;
  aclLemonChiffon         = $FFFFFACD;
  aclLightBlue            = $FFADD8E6;
  aclLightCoral           = $FFF08080;
  aclLightCyan            = $FFE0FFFF;
  aclLightGoldenrodYellow = $FFFAFAD2;
  aclLightGray            = $FFD3D3D3;
  aclLightGreen           = $FF90EE90;
  aclLightPink            = $FFFFB6C1;
  aclLightSalmon          = $FFFFA07A;
  aclLightSeaGreen        = $FF20B2AA;
  aclLightSkyBlue         = $FF87CEFA;
  aclLightSlateGray       = $FF778899;
  aclLightSteelBlue       = $FFB0C4DE;
  aclLightYellow          = $FFFFFFE0;
  aclLime                 = $FF00FF00;
  aclLimeGreen            = $FF32CD32;
  aclLinen                = $FFFAF0E6;
  aclMagenta              = $FFFF00FF;
  aclMaroon               = $FF800000;
  aclMediumAquamarine     = $FF66CDAA;
  aclMediumBlue           = $FF0000CD;
  aclMediumOrchid         = $FFBA55D3;
  aclMediumPurple         = $FF9370DB;
  aclMediumSeaGreen       = $FF3CB371;
  aclMediumSlateBlue      = $FF7B68EE;
  aclMediumSpringGreen    = $FF00FA9A;
  aclMediumTurquoise      = $FF48D1CC;
  aclMediumVioletRed      = $FFC71585;
  aclMidnightBlue         = $FF191970;
  aclMintCream            = $FFF5FFFA;
  aclMistyRose            = $FFFFE4E1;
  aclMoccasin             = $FFFFE4B5;
  aclNavajoWhite          = $FFFFDEAD;
  aclNavy                 = $FF000080;
  aclOldLace              = $FFFDF5E6;
  aclOlive                = $FF808000;
  aclOliveDrab            = $FF6B8E23;
  aclOrange               = $FFFFA500;
  aclOrangeRed            = $FFFF4500;
  aclOrchid               = $FFDA70D6;
  aclPaleGoldenrod        = $FFEEE8AA;
  aclPaleGreen            = $FF98FB98;
  aclPaleTurquoise        = $FFAFEEEE;
  aclPaleVioletRed        = $FFDB7093;
  aclPapayaWhip           = $FFFFEFD5;
  aclPeachPuff            = $FFFFDAB9;
  aclPeru                 = $FFCD853F;
  aclPink                 = $FFFFC0CB;
  aclPlum                 = $FFDDA0DD;
  aclPowderBlue           = $FFB0E0E6;
  aclPurple               = $FF800080;
  aclRed                  = $FFFF0000;
  aclRosyBrown            = $FFBC8F8F;
  aclRoyalBlue            = $FF4169E1;
  aclSaddleBrown          = $FF8B4513;
  aclSalmon               = $FFFA8072;
  aclSandyBrown           = $FFF4A460;
  aclSeaGreen             = $FF2E8B57;
  aclSeaShell             = $FFFFF5EE;
  aclSienna               = $FFA0522D;
  aclSilver               = $FFC0C0C0;
  aclSkyBlue              = $FF87CEEB;
  aclSlateBlue            = $FF6A5ACD;
  aclSlateGray            = $FF708090;
  aclSnow                 = $FFFFFAFA;
  aclSpringGreen          = $FF00FF7F;
  aclSteelBlue            = $FF4682B4;
  aclTan                  = $FFD2B48C;
  aclTeal                 = $FF008080;
  aclThistle              = $FFD8BFD8;
  aclTomato               = $FFFF6347;
  aclTransparent          = $00FFFFFF;
  aclTurquoise            = $FF40E0D0;
  aclViolet               = $FFEE82EE;
  aclWheat                = $FFF5DEB3;
  aclWhite                = $FFFFFFFF;
  aclWhiteSmoke           = $FFF5F5F5;
  aclYellow               = $FFFFFF00;
  aclYellowGreen          = $FF9ACD32;

  // Shift count and bit mask for A, R, G, B components
  AlphaShift  = 24;
  {.$EXTERNALSYM AlphaShift}
  RedShift    = 16;
  {.$EXTERNALSYM RedShift}
  GreenShift  = 8;
  {.$EXTERNALSYM GreenShift}
  BlueShift   = 0;
  {.$EXTERNALSYM BlueShift}

  AlphaMask   = $ff000000;
  {.$EXTERNALSYM AlphaMask}
  RedMask     = $00ff0000;
  {.$EXTERNALSYM RedMask}
  GreenMask   = $0000ff00;
  {.$EXTERNALSYM GreenMask}
  BlueMask    = $000000ff;
  {.$EXTERNALSYM BlueMask}


type
{  TGPColor = class
  protected
     Argb: ARGB;
  public
    constructor Create; overload;
    constructor Create(r, g, b: Byte); overload;
    constructor Create(a, r, g, b: Byte); overload;
    constructor Create(Value: ARGB); overload;
    function GetAlpha: BYTE;
    function GetA: BYTE;
    function GetRed: BYTE;
    function GetR: BYTE;
    function GetGreen: Byte;
    function GetG: Byte;
    function GetBlue: Byte;
    function GetB: Byte;
    function GetValue: ARGB;
    procedure SetValue(Value: ARGB);
    procedure SetFromCOLORREF(rgb: COLORREF);
    function ToCOLORREF: COLORREF;
    function MakeARGB(a, r, g, b: Byte): ARGB;
  end;  }

  PGPColor = ^TGPColor;
  TGPColor = ARGB;
  TColorDynArray = array of TGPColor;

  function MakeColor(r, g, b: Byte): ARGB; overload;
  function MakeColor(a, r, g, b: Byte): ARGB; overload;
  function GetAlpha(color: ARGB): BYTE;
  function GetRed(color: ARGB): BYTE;
  function GetGreen(color: ARGB): BYTE;
  function GetBlue(color: ARGB): BYTE;
  function ColorRefToARGB(rgb: COLORREF): ARGB;
  function ARGBToColorRef(Color: ARGB): COLORREF;


(**************************************************************************\
*
*   GDI+ Metafile Related Structures
*
\**************************************************************************)

type
  {.$EXTERNALSYM ENHMETAHEADER3}
  ENHMETAHEADER3 = packed record
    iType          : DWORD;  // Record type EMR_HEADER
    nSize          : DWORD;  // Record size in bytes.  This may be greater
                             // than the sizeof(ENHMETAHEADER).
    rclBounds      : TRect;  // Inclusive-inclusive bounds in device units
    rclFrame       : TRect;  // Inclusive-inclusive Picture Frame .01mm unit
    dSignature     : DWORD;  // Signature.  Must be ENHMETA_SIGNATURE.
    nVersion       : DWORD;  // Version number
    nBytes         : DWORD;  // Size of the metafile in bytes
    nRecords       : DWORD;  // Number of records in the metafile
    nHandles       : WORD;   // Number of handles in the handle table
                             // Handle index zero is reserved.
    sReserved      : WORD;   // Reserved.  Must be zero.
    nDescription   : DWORD;  // Number of chars in the unicode desc string
                             // This is 0 if there is no description string
    offDescription : DWORD;  // Offset to the metafile description record.
                             // This is 0 if there is no description string
    nPalEntries    : DWORD;  // Number of entries in the metafile palette.
    szlDevice      : TSize;  // Size of the reference device in pels
    szlMillimeters : TSize;  // Size of the reference device in millimeters
  end;
  TENHMETAHEADER3 = ENHMETAHEADER3;
  PENHMETAHEADER3 = ^TENHMETAHEADER3;

  // Placeable WMFs

  // Placeable Metafiles were created as a non-standard way of specifying how
  // a metafile is mapped and scaled on an output device.
  // Placeable metafiles are quite wide-spread, but not directly supported by
  // the Windows API. To playback a placeable metafile using the Windows API,
  // you will first need to strip the placeable metafile header from the file.
  // This is typically performed by copying the metafile to a temporary file
  // starting at file offset 22 (0x16). The contents of the temporary file may
  // then be used as input to the Windows GetMetaFile(), PlayMetaFile(),
  // CopyMetaFile(), etc. GDI functions.

  // Each placeable metafile begins with a 22-byte header,
  //  followed by a standard metafile:

  {.$EXTERNALSYM PWMFRect16}
  PWMFRect16 = packed record
    Left   : INT16;
    Top    : INT16;
    Right  : INT16;
    Bottom : INT16;
  end;
  TPWMFRect16 = PWMFRect16;
  PPWMFRect16 = ^TPWMFRect16;

  {.$EXTERNALSYM WmfPlaceableFileHeader}
  WmfPlaceableFileHeader = packed record
    Key         : UINT32;      // GDIP_WMF_PLACEABLEKEY
    Hmf         : INT16;       // Metafile HANDLE number (always 0)
    BoundingBox : PWMFRect16;  // Coordinates in metafile units
    Inch        : INT16;       // Number of metafile units per inch
    Reserved    : UINT32;      // Reserved (always 0)
    Checksum    : INT16;       // Checksum value for previous 10 WORDs
  end;
  TWmfPlaceableFileHeader = WmfPlaceableFileHeader;
  PWmfPlaceableFileHeader = ^TWmfPlaceableFileHeader;

  // Key contains a special identification value that indicates the presence
  // of a placeable metafile header and is always 0x9AC6CDD7.

  // Handle is used to stored the handle of the metafile in memory. When written
  // to disk, this field is not used and will always contains the value 0.

  // Left, Top, Right, and Bottom contain the coordinates of the upper-left
  // and lower-right corners of the image on the output device. These are
  // measured in twips.

  // A twip (meaning "twentieth of a point") is the logical unit of measurement
  // used in Windows Metafiles. A twip is equal to 1/1440 of an inch. Thus 720
  // twips equal 1/2 inch, while 32,768 twips is 22.75 inches.

  // Inch contains the number of twips per inch used to represent the image.
  // Normally, there are 1440 twips per inch; however, this number may be
  // changed to scale the image. A value of 720 indicates that the image is
  // double its normal size, or scaled to a factor of 2:1. A value of 360
  // indicates a scale of 4:1, while a value of 2880 indicates that the image
  // is scaled down in size by a factor of two. A value of 1440 indicates
  // a 1:1 scale ratio.

  // Reserved is not used and is always set to 0.

  // Checksum contains a checksum value for the previous 10 WORDs in the header.
  // This value can be used in an attempt to detect if the metafile has become
  // corrupted. The checksum is calculated by XORing each WORD value to an
  // initial value of 0.

  // If the metafile was recorded with a reference Hdc that was a display.

const
  GDIP_EMFPLUSFLAGS_DISPLAY      = $00000001;
  {.$EXTERNALSYM GDIP_EMFPLUSFLAGS_DISPLAY}

type
  TMetafileHeader = packed class
  public
    Type_        : TMetafileType;
    Size         : UINT;           // Size of the metafile (in bytes)
    Version      : UINT;           // EMF+, EMF, or WMF version
    EmfPlusFlags : UINT;
    DpiX         : Single;
    DpiY         : Single;
    X            : Integer;        // Bounds in device units
    Y            : Integer;
    Width        : Integer;
    Height       : Integer;
    {Header       : record
      case integer of
        0: (WmfHeader: TMETAHEADER;);
        1: (EmfHeader: TENHMETAHEADER3);
      end;}
    //work around for C++ bug
    EmfHeader: TENHMETAHEADER3;

    EmfPlusHeaderSize : Integer; // size of the EMF+ header in file
    LogicalDpiX       : Integer; // Logical Dpi of reference Hdc
    LogicalDpiY       : Integer; // usually valid only for EMF+
  public
    property GetType: TMetafileType read Type_;
    property GetMetafileSize: UINT read Size;
    // If IsEmfPlus, this is the EMF+ version; else it is the WMF or EMF ver
    property GetVersion: UINT read Version;
     // Get the EMF+ flags associated with the metafile
    property GetEmfPlusFlags: UINT read EmfPlusFlags;
    property GetDpiX: Single read DpiX;
    property GetDpiY: Single read DpiY;
    procedure GetBounds(out Rect: TGPRect);
    // Is it any type of WMF (standard or Placeable Metafile)?
    function IsWmf: BOOL;
    // Is this an Placeable Metafile?
    function IsWmfPlaceable: BOOL;
    // Is this an EMF (not an EMF+)?
    function IsEmf: BOOL;
    // Is this an EMF or EMF+ file?
    function IsEmfOrEmfPlus: BOOL;
    // Is this an EMF+ file?
    function IsEmfPlus: BOOL;
    // Is this an EMF+ dual (has dual, down-level records) file?
    function IsEmfPlusDual: BOOL;
    // Is this an EMF+ only (no dual records) file?
    function IsEmfPlusOnly: BOOL;
    // If it's an EMF+ file, was it recorded against a display Hdc?
    function IsDisplay: BOOL;
    // Get the WMF header of the metafile (if it is a WMF)
    function GetWmfHeader: PMetaHeader;
    // Get the EMF header of the metafile (if it is an EMF)
    function GetEmfHeader: PENHMETAHEADER3;
  end;

(**************************************************************************\
*
*   GDI+ Imaging GUIDs
*
\**************************************************************************)

//---------------------------------------------------------------------------
// Image file format identifiers
//---------------------------------------------------------------------------

const
  ImageFormatUndefined : TGUID = '{b96b3ca9-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatUndefined}
  ImageFormatMemoryBMP : TGUID = '{b96b3caa-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatMemoryBMP}
  ImageFormatBMP       : TGUID = '{b96b3cab-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatBMP}
  ImageFormatEMF       : TGUID = '{b96b3cac-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatEMF}
  ImageFormatWMF       : TGUID = '{b96b3cad-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatWMF}
  ImageFormatJPEG      : TGUID = '{b96b3cae-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatJPEG}
  ImageFormatPNG       : TGUID = '{b96b3caf-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatPNG}
  ImageFormatGIF       : TGUID = '{b96b3cb0-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatGIF}
  ImageFormatTIFF      : TGUID = '{b96b3cb1-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatTIFF}
  ImageFormatEXIF      : TGUID = '{b96b3cb2-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatEXIF}
  ImageFormatIcon      : TGUID = '{b96b3cb5-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatIcon}

//---------------------------------------------------------------------------
// Predefined multi-frame dimension IDs
//---------------------------------------------------------------------------

  FrameDimensionTime       : TGUID = '{6aedbd6d-3fb5-418a-83a6-7f45229dc872}';
  {.$EXTERNALSYM FrameDimensionTime}
  FrameDimensionResolution : TGUID = '{84236f7b-3bd3-428f-8dab-4ea1439ca315}';
  {.$EXTERNALSYM FrameDimensionResolution}
  FrameDimensionPage       : TGUID = '{7462dc86-6180-4c7e-8e3f-ee7333a7a483}';
  {.$EXTERNALSYM FrameDimensionPage}

//---------------------------------------------------------------------------
// Property sets
//---------------------------------------------------------------------------

  FormatIDImageInformation : TGUID = '{e5836cbe-5eef-4f1d-acde-ae4c43b608ce}';
  {.$EXTERNALSYM FormatIDImageInformation}
  FormatIDJpegAppHeaders   : TGUID = '{1c4afdcd-6177-43cf-abc7-5f51af39ee85}';
  {.$EXTERNALSYM FormatIDJpegAppHeaders}

//---------------------------------------------------------------------------
// Encoder parameter sets
//---------------------------------------------------------------------------

  EncoderCompression      : TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  {.$EXTERNALSYM EncoderCompression}
  EncoderColorDepth       : TGUID = '{66087055-ad66-4c7c-9a18-38a2310b8337}';
  {.$EXTERNALSYM EncoderColorDepth}
  EncoderScanMethod       : TGUID = '{3a4e2661-3109-4e56-8536-42c156e7dcfa}';
  {.$EXTERNALSYM EncoderScanMethod}
  EncoderVersion          : TGUID = '{24d18c76-814a-41a4-bf53-1c219cccf797}';
  {.$EXTERNALSYM EncoderVersion}
  EncoderRenderMethod     : TGUID = '{6d42c53a-229a-4825-8bb7-5c99e2b9a8b8}';
  {.$EXTERNALSYM EncoderRenderMethod}
  EncoderQuality          : TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  {.$EXTERNALSYM EncoderQuality}
  EncoderTransformation   : TGUID = '{8d0eb2d1-a58e-4ea8-aa14-108074b7b6f9}';
  {.$EXTERNALSYM EncoderTransformation}
  EncoderLuminanceTable   : TGUID = '{edb33bce-0266-4a77-b904-27216099e717}';
  {.$EXTERNALSYM EncoderLuminanceTable}
  EncoderChrominanceTable : TGUID = '{f2e455dc-09b3-4316-8260-676ada32481c}';
  {.$EXTERNALSYM EncoderChrominanceTable}
  EncoderSaveFlag         : TGUID = '{292266fc-ac40-47bf-8cfc-a85b89a655de}';
  {.$EXTERNALSYM EncoderSaveFlag}

  CodecIImageBytes : TGUID = '{025d1823-6c7d-447b-bbdb-a3cbc3dfa2fc}';
  {.$EXTERNALSYM CodecIImageBytes}

type
  {.$EXTERNALSYM IImageBytes}
  IImageBytes = Interface(IUnknown)
    ['{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}']
    // Return total number of bytes in the IStream
    function CountBytes(out pcb: UINT): HRESULT; stdcall;
    // Locks "cb" bytes, starting from "ulOffset" in the stream, and returns the
    // pointer to the beginning of the locked memory chunk in "ppvBytes"
    function LockBytes(cb: UINT; ulOffset: ULONG; out ppvBytes: pointer): HRESULT; stdcall;
    // Unlocks "cb" bytes, pointed by "pvBytes", starting from "ulOffset" in the
    // stream
    function UnlockBytes(pvBytes: pointer; cb: UINT; ulOffset: ULONG): HRESULT; stdcall;
  end;

//--------------------------------------------------------------------------
// ImageCodecInfo structure
//--------------------------------------------------------------------------

  {.$EXTERNALSYM ImageCodecInfo}
  {$HINTS OFF}
  ImageCodecInfo = packed record
    Clsid             : TGUID;
    FormatID          : TGUID;
    CodecName         : PWCHAR;
    DllName           : PWCHAR;
    FormatDescription : PWCHAR;
    FilenameExtension : PWCHAR;
    MimeType          : PWCHAR;
    Flags             : DWORD;
    Version           : DWORD;
    SigCount          : DWORD;
    SigSize           : DWORD;
    SigPattern        : PBYTE;
    SigMask           : PBYTE;
  end;
  TImageCodecInfo = ImageCodecInfo;
  PImageCodecInfo = ^TImageCodecInfo;
  {$HINTS ON}

//--------------------------------------------------------------------------
// Information flags about image codecs
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM ImageCodecFlags}
  ImageCodecFlags = (
    ImageCodecFlagsEncoder            = $00000001,
    ImageCodecFlagsDecoder            = $00000002,
    ImageCodecFlagsSupportBitmap      = $00000004,
    ImageCodecFlagsSupportVector      = $00000008,
    ImageCodecFlagsSeekableEncode     = $00000010,
    ImageCodecFlagsBlockingDecode     = $00000020,

    ImageCodecFlagsBuiltin            = $00010000,
    ImageCodecFlagsSystem             = $00020000,
    ImageCodecFlagsUser               = $00040000
  );
  TImageCodecFlags = ImageCodecFlags;
{$ELSE}
  {.$EXTERNALSYM ImageCodecFlags}
  ImageCodecFlags = Integer;
  const
    ImageCodecFlagsEncoder            = $00000001;
    ImageCodecFlagsDecoder            = $00000002;
    ImageCodecFlagsSupportBitmap      = $00000004;
    ImageCodecFlagsSupportVector      = $00000008;
    ImageCodecFlagsSeekableEncode     = $00000010;
    ImageCodecFlagsBlockingDecode     = $00000020;

    ImageCodecFlagsBuiltin            = $00010000;
    ImageCodecFlagsSystem             = $00020000;
    ImageCodecFlagsUser               = $00040000;

type
  TImageCodecFlags = ImageCodecFlags;
{$ENDIF}
//---------------------------------------------------------------------------
// Access modes used when calling Image::LockBits
//---------------------------------------------------------------------------

  {.$EXTERNALSYM ImageLockMode}
  ImageLockMode = Integer;
  const
    ImageLockModeRead         = $0001;
    ImageLockModeWrite        = $0002;
    ImageLockModeUserInputBuf = $0004;
type
  TImageLockMode = ImageLockMode;

//---------------------------------------------------------------------------
// Information about image pixel data
//---------------------------------------------------------------------------

  {.$EXTERNALSYM BitmapData}
  BitmapData = packed record
    Width       : UINT;
    Height      : UINT;
    Stride      : Integer;
    PixelFormat : PixelFormat;
    Scan0       : Pointer;
    Reserved    : UINT;
  end;
  TBitmapData = BitmapData;
  PBitmapData = ^TBitmapData;

//---------------------------------------------------------------------------
// Image flags
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM ImageFlags}
  ImageFlags = (
    ImageFlagsNone                = 0,

    // Low-word: shared with SINKFLAG_x

    ImageFlagsScalable            = $0001,
    ImageFlagsHasAlpha            = $0002,
    ImageFlagsHasTranslucent      = $0004,
    ImageFlagsPartiallyScalable   = $0008,

    // Low-word: color space definition

    ImageFlagsColorSpaceRGB       = $0010,
    ImageFlagsColorSpaceCMYK      = $0020,
    ImageFlagsColorSpaceGRAY      = $0040,
    ImageFlagsColorSpaceYCBCR     = $0080,
    ImageFlagsColorSpaceYCCK      = $0100,

    // Low-word: image size info

    ImageFlagsHasRealDPI          = $1000,
    ImageFlagsHasRealPixelSize    = $2000,

    // High-word

    ImageFlagsReadOnly            = $00010000,
    ImageFlagsCaching             = $00020000
  );
  TImageFlags = ImageFlags;
{$ELSE}
  {.$EXTERNALSYM ImageFlags}
  ImageFlags = Integer;
  const
    ImageFlagsNone                = 0;

    // Low-word: shared with SINKFLAG_x

    ImageFlagsScalable            = $0001;
    ImageFlagsHasAlpha            = $0002;
    ImageFlagsHasTranslucent      = $0004;
    ImageFlagsPartiallyScalable   = $0008;

    // Low-word: color space definition

    ImageFlagsColorSpaceRGB       = $0010;
    ImageFlagsColorSpaceCMYK      = $0020;
    ImageFlagsColorSpaceGRAY      = $0040;
    ImageFlagsColorSpaceYCBCR     = $0080;
    ImageFlagsColorSpaceYCCK      = $0100;

    // Low-word: image size info

    ImageFlagsHasRealDPI          = $1000;
    ImageFlagsHasRealPixelSize    = $2000;

    // High-word

    ImageFlagsReadOnly            = $00010000;
    ImageFlagsCaching             = $00020000;

type
  TImageFlags = ImageFlags;
{$ENDIF}


{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM RotateFlipType}
  RotateFlipType = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone   = 1,
    Rotate180FlipNone  = 2,
    Rotate270FlipNone  = 3,

    RotateNoneFlipX    = 4,
    Rotate90FlipX      = 5,
    Rotate180FlipX     = 6,
    Rotate270FlipX     = 7,

    RotateNoneFlipY    = Rotate180FlipX,
    Rotate90FlipY      = Rotate270FlipX,
    Rotate180FlipY     = RotateNoneFlipX,
    Rotate270FlipY     = Rotate90FlipX,

    RotateNoneFlipXY   = Rotate180FlipNone,
    Rotate90FlipXY     = Rotate270FlipNone,
    Rotate180FlipXY    = RotateNoneFlipNone,
    Rotate270FlipXY    = Rotate90FlipNone
  );
  TRotateFlipType = RotateFlipType;
{$ELSE}
  {.$EXTERNALSYM RotateFlipType}
  RotateFlipType = (
    RotateNoneFlipNone, // = 0,
    Rotate90FlipNone,   // = 1,
    Rotate180FlipNone,  // = 2,
    Rotate270FlipNone,  // = 3,

    RotateNoneFlipX,    // = 4,
    Rotate90FlipX,      // = 5,
    Rotate180FlipX,     // = 6,
    Rotate270FlipX      // = 7,
  );
  const
    RotateNoneFlipY    = Rotate180FlipX;
    Rotate90FlipY      = Rotate270FlipX;
    Rotate180FlipY     = RotateNoneFlipX;
    Rotate270FlipY     = Rotate90FlipX;

    RotateNoneFlipXY   = Rotate180FlipNone;
    Rotate90FlipXY     = Rotate270FlipNone;
    Rotate180FlipXY    = RotateNoneFlipNone;
    Rotate270FlipXY    = Rotate90FlipNone;

type
  TRotateFlipType = RotateFlipType;
{$ENDIF}

//---------------------------------------------------------------------------
// Encoder Parameter structure
//---------------------------------------------------------------------------

  {.$EXTERNALSYM EncoderParameter}
  EncoderParameter = packed record
    Guid           : TGUID;   // GUID of the parameter
    NumberOfValues : ULONG;   // Number of the parameter values
    Type_          : ULONG;   // Value type, like ValueTypeLONG  etc.
    Value          : Pointer; // A pointer to the parameter values
  end;
  TEncoderParameter = EncoderParameter;
  PEncoderParameter = ^TEncoderParameter;

//---------------------------------------------------------------------------
// Encoder Parameters structure
//---------------------------------------------------------------------------

  {.$EXTERNALSYM EncoderParameters}
  EncoderParameters = packed record
    Count     : UINT;               // Number of parameters in this structure
    Parameter : array[0..0] of TEncoderParameter;  // Parameter values
  end;
  TEncoderParameters = EncoderParameters;
  PEncoderParameters = ^TEncoderParameters;

//---------------------------------------------------------------------------
// Property Item
//---------------------------------------------------------------------------

  {.$EXTERNALSYM PropertyItem}
  PropertyItem = record // NOT PACKED !!
    id       : PROPID;  // ID of this property
    length   : ULONG;   // Length of the property value, in bytes
    type_    : WORD;    // Type of the value, as one of TAG_TYPE_XXX
    value    : Pointer; // property value
  end;
  TPropertyItem = PropertyItem;
  PPropertyItem = ^TPropertyItem;

//---------------------------------------------------------------------------
// Image property types
//---------------------------------------------------------------------------

const
  PropertyTagTypeByte      : Integer =  1;
  {.$EXTERNALSYM PropertyTagTypeByte}
  PropertyTagTypeASCII     : Integer =  2;
  {.$EXTERNALSYM PropertyTagTypeASCII}
  PropertyTagTypeShort     : Integer =  3;
  {.$EXTERNALSYM PropertyTagTypeShort}
  PropertyTagTypeLong      : Integer =  4;
  {.$EXTERNALSYM PropertyTagTypeLong}
  PropertyTagTypeRational  : Integer =  5;
  {.$EXTERNALSYM PropertyTagTypeRational}
  PropertyTagTypeUndefined : Integer =  7;
  {.$EXTERNALSYM PropertyTagTypeUndefined}
  PropertyTagTypeSLONG     : Integer =  9;
  {.$EXTERNALSYM PropertyTagTypeSLONG}
  PropertyTagTypeSRational : Integer = 10;
  {.$EXTERNALSYM PropertyTagTypeSRational}

//---------------------------------------------------------------------------
// Image property ID tags
//---------------------------------------------------------------------------

  PropertyTagExifIFD            = $8769;
  {.$EXTERNALSYM PropertyTagExifIFD}
  PropertyTagGpsIFD             = $8825;
  {.$EXTERNALSYM PropertyTagGpsIFD}

  PropertyTagNewSubfileType     = $00FE;
  {.$EXTERNALSYM PropertyTagNewSubfileType}
  PropertyTagSubfileType        = $00FF;
  {.$EXTERNALSYM PropertyTagSubfileType}
  PropertyTagImageWidth         = $0100;
  {.$EXTERNALSYM PropertyTagImageWidth}
  PropertyTagImageHeight        = $0101;
  {.$EXTERNALSYM PropertyTagImageHeight}
  PropertyTagBitsPerSample      = $0102;
  {.$EXTERNALSYM PropertyTagBitsPerSample}
  PropertyTagCompression        = $0103;
  {.$EXTERNALSYM PropertyTagCompression}
  PropertyTagPhotometricInterp  = $0106;
  {.$EXTERNALSYM PropertyTagPhotometricInterp}
  PropertyTagThreshHolding      = $0107;
  {.$EXTERNALSYM PropertyTagThreshHolding}
  PropertyTagCellWidth          = $0108;
  {.$EXTERNALSYM PropertyTagCellWidth}
  PropertyTagCellHeight         = $0109;
  {.$EXTERNALSYM PropertyTagCellHeight}
  PropertyTagFillOrder          = $010A;
  {.$EXTERNALSYM PropertyTagFillOrder}
  PropertyTagDocumentName       = $010D;
  {.$EXTERNALSYM PropertyTagDocumentName}
  PropertyTagImageDescription   = $010E;
  {.$EXTERNALSYM PropertyTagImageDescription}
  PropertyTagEquipMake          = $010F;
  {.$EXTERNALSYM PropertyTagEquipMake}
  PropertyTagEquipModel         = $0110;
  {.$EXTERNALSYM PropertyTagEquipModel}
  PropertyTagStripOffsets       = $0111;
  {.$EXTERNALSYM PropertyTagStripOffsets}
  PropertyTagOrientation        = $0112;
  {.$EXTERNALSYM PropertyTagOrientation}
  PropertyTagSamplesPerPixel    = $0115;
  {.$EXTERNALSYM PropertyTagSamplesPerPixel}
  PropertyTagRowsPerStrip       = $0116;
  {.$EXTERNALSYM PropertyTagRowsPerStrip}
  PropertyTagStripBytesCount    = $0117;
  {.$EXTERNALSYM PropertyTagStripBytesCount}
  PropertyTagMinSampleValue     = $0118;
  {.$EXTERNALSYM PropertyTagMinSampleValue}
  PropertyTagMaxSampleValue     = $0119;
  {.$EXTERNALSYM PropertyTagMaxSampleValue}
  PropertyTagXResolution        = $011A;   // Image resolution in width direction
  {.$EXTERNALSYM PropertyTagXResolution}
  PropertyTagYResolution        = $011B;   // Image resolution in height direction
  {.$EXTERNALSYM PropertyTagYResolution}
  PropertyTagPlanarConfig       = $011C;   // Image data arrangement
  {.$EXTERNALSYM PropertyTagPlanarConfig}
  PropertyTagPageName           = $011D;
  {.$EXTERNALSYM PropertyTagPageName}
  PropertyTagXPosition          = $011E;
  {.$EXTERNALSYM PropertyTagXPosition}
  PropertyTagYPosition          = $011F;
  {.$EXTERNALSYM PropertyTagYPosition}
  PropertyTagFreeOffset         = $0120;
  {.$EXTERNALSYM PropertyTagFreeOffset}
  PropertyTagFreeByteCounts     = $0121;
  {.$EXTERNALSYM PropertyTagFreeByteCounts}
  PropertyTagGrayResponseUnit   = $0122;
  {.$EXTERNALSYM PropertyTagGrayResponseUnit}
  PropertyTagGrayResponseCurve  = $0123;
  {.$EXTERNALSYM PropertyTagGrayResponseCurve}
  PropertyTagT4Option           = $0124;
  {.$EXTERNALSYM PropertyTagT4Option}
  PropertyTagT6Option           = $0125;
  {.$EXTERNALSYM PropertyTagT6Option}
  PropertyTagResolutionUnit     = $0128;   // Unit of X and Y resolution
  {.$EXTERNALSYM PropertyTagResolutionUnit}
  PropertyTagPageNumber         = $0129;
  {.$EXTERNALSYM PropertyTagPageNumber}
  PropertyTagTransferFuncition  = $012D;
  {.$EXTERNALSYM PropertyTagTransferFuncition}
  PropertyTagSoftwareUsed       = $0131;
  {.$EXTERNALSYM PropertyTagSoftwareUsed}
  PropertyTagDateTime           = $0132;
  {.$EXTERNALSYM PropertyTagDateTime}
  PropertyTagArtist             = $013B;
  {.$EXTERNALSYM PropertyTagArtist}
  PropertyTagHostComputer       = $013C;
  {.$EXTERNALSYM PropertyTagHostComputer}
  PropertyTagPredictor          = $013D;
  {.$EXTERNALSYM PropertyTagPredictor}
  PropertyTagWhitePoint         = $013E;
  {.$EXTERNALSYM PropertyTagWhitePoint}
  PropertyTagPrimaryChromaticities = $013F;
  {.$EXTERNALSYM PropertyTagPrimaryChromaticities}
  PropertyTagColorMap           = $0140;
  {.$EXTERNALSYM PropertyTagColorMap}
  PropertyTagHalftoneHints      = $0141;
  {.$EXTERNALSYM PropertyTagHalftoneHints}
  PropertyTagTileWidth          = $0142;
  {.$EXTERNALSYM PropertyTagTileWidth}
  PropertyTagTileLength         = $0143;
  {.$EXTERNALSYM PropertyTagTileLength}
  PropertyTagTileOffset         = $0144;
  {.$EXTERNALSYM PropertyTagTileOffset}
  PropertyTagTileByteCounts     = $0145;
  {.$EXTERNALSYM PropertyTagTileByteCounts}
  PropertyTagInkSet             = $014C;
  {.$EXTERNALSYM PropertyTagInkSet}
  PropertyTagInkNames           = $014D;
  {.$EXTERNALSYM PropertyTagInkNames}
  PropertyTagNumberOfInks       = $014E;
  {.$EXTERNALSYM PropertyTagNumberOfInks}
  PropertyTagDotRange           = $0150;
  {.$EXTERNALSYM PropertyTagDotRange}
  PropertyTagTargetPrinter      = $0151;
  {.$EXTERNALSYM PropertyTagTargetPrinter}
  PropertyTagExtraSamples       = $0152;
  {.$EXTERNALSYM PropertyTagExtraSamples}
  PropertyTagSampleFormat       = $0153;
  {.$EXTERNALSYM PropertyTagSampleFormat}
  PropertyTagSMinSampleValue    = $0154;
  {.$EXTERNALSYM PropertyTagSMinSampleValue}
  PropertyTagSMaxSampleValue    = $0155;
  {.$EXTERNALSYM PropertyTagSMaxSampleValue}
  PropertyTagTransferRange      = $0156;
  {.$EXTERNALSYM PropertyTagTransferRange}

  PropertyTagJPEGProc               = $0200;
  {.$EXTERNALSYM PropertyTagJPEGProc}
  PropertyTagJPEGInterFormat        = $0201;
  {.$EXTERNALSYM PropertyTagJPEGInterFormat}
  PropertyTagJPEGInterLength        = $0202;
  {.$EXTERNALSYM PropertyTagJPEGInterLength}
  PropertyTagJPEGRestartInterval    = $0203;
  {.$EXTERNALSYM PropertyTagJPEGRestartInterval}
  PropertyTagJPEGLosslessPredictors = $0205;
  {.$EXTERNALSYM PropertyTagJPEGLosslessPredictors}
  PropertyTagJPEGPointTransforms    = $0206;
  {.$EXTERNALSYM PropertyTagJPEGPointTransforms}
  PropertyTagJPEGQTables            = $0207;
  {.$EXTERNALSYM PropertyTagJPEGQTables}
  PropertyTagJPEGDCTables           = $0208;
  {.$EXTERNALSYM PropertyTagJPEGDCTables}
  PropertyTagJPEGACTables           = $0209;
  {.$EXTERNALSYM PropertyTagJPEGACTables}

  PropertyTagYCbCrCoefficients  = $0211;
  {.$EXTERNALSYM PropertyTagYCbCrCoefficients}
  PropertyTagYCbCrSubsampling   = $0212;
  {.$EXTERNALSYM PropertyTagYCbCrSubsampling}
  PropertyTagYCbCrPositioning   = $0213;
  {.$EXTERNALSYM PropertyTagYCbCrPositioning}
  PropertyTagREFBlackWhite      = $0214;
  {.$EXTERNALSYM PropertyTagREFBlackWhite}

  PropertyTagICCProfile         = $8773;   // This TAG is defined by ICC
  {.$EXTERNALSYM PropertyTagICCProfile}
                                           // for embedded ICC in TIFF
  PropertyTagGamma                = $0301;
  {.$EXTERNALSYM PropertyTagGamma}
  PropertyTagICCProfileDescriptor = $0302;
  {.$EXTERNALSYM PropertyTagICCProfileDescriptor}
  PropertyTagSRGBRenderingIntent  = $0303;
  {.$EXTERNALSYM PropertyTagSRGBRenderingIntent}

  PropertyTagImageTitle         = $0320;
  {.$EXTERNALSYM PropertyTagImageTitle}
  PropertyTagCopyright          = $8298;
  {.$EXTERNALSYM PropertyTagCopyright}

// Extra TAGs (Like Adobe Image Information tags etc.)

  PropertyTagResolutionXUnit           = $5001;
  {.$EXTERNALSYM PropertyTagResolutionXUnit}
  PropertyTagResolutionYUnit           = $5002;
  {.$EXTERNALSYM PropertyTagResolutionYUnit}
  PropertyTagResolutionXLengthUnit     = $5003;
  {.$EXTERNALSYM PropertyTagResolutionXLengthUnit}
  PropertyTagResolutionYLengthUnit     = $5004;
  {.$EXTERNALSYM PropertyTagResolutionYLengthUnit}
  PropertyTagPrintFlags                = $5005;
  {.$EXTERNALSYM PropertyTagPrintFlags}
  PropertyTagPrintFlagsVersion         = $5006;
  {.$EXTERNALSYM PropertyTagPrintFlagsVersion}
  PropertyTagPrintFlagsCrop            = $5007;
  {.$EXTERNALSYM PropertyTagPrintFlagsCrop}
  PropertyTagPrintFlagsBleedWidth      = $5008;
  {.$EXTERNALSYM PropertyTagPrintFlagsBleedWidth}
  PropertyTagPrintFlagsBleedWidthScale = $5009;
  {.$EXTERNALSYM PropertyTagPrintFlagsBleedWidthScale}
  PropertyTagHalftoneLPI               = $500A;
  {.$EXTERNALSYM PropertyTagHalftoneLPI}
  PropertyTagHalftoneLPIUnit           = $500B;
  {.$EXTERNALSYM PropertyTagHalftoneLPIUnit}
  PropertyTagHalftoneDegree            = $500C;
  {.$EXTERNALSYM PropertyTagHalftoneDegree}
  PropertyTagHalftoneShape             = $500D;
  {.$EXTERNALSYM PropertyTagHalftoneShape}
  PropertyTagHalftoneMisc              = $500E;
  {.$EXTERNALSYM PropertyTagHalftoneMisc}
  PropertyTagHalftoneScreen            = $500F;
  {.$EXTERNALSYM PropertyTagHalftoneScreen}
  PropertyTagJPEGQuality               = $5010;
  {.$EXTERNALSYM PropertyTagJPEGQuality}
  PropertyTagGridSize                  = $5011;
  {.$EXTERNALSYM PropertyTagGridSize}
  PropertyTagThumbnailFormat           = $5012;  // 1 = JPEG, 0 = RAW RGB
  {.$EXTERNALSYM PropertyTagThumbnailFormat}
  PropertyTagThumbnailWidth            = $5013;
  {.$EXTERNALSYM PropertyTagThumbnailWidth}
  PropertyTagThumbnailHeight           = $5014;
  {.$EXTERNALSYM PropertyTagThumbnailHeight}
  PropertyTagThumbnailColorDepth       = $5015;
  {.$EXTERNALSYM PropertyTagThumbnailColorDepth}
  PropertyTagThumbnailPlanes           = $5016;
  {.$EXTERNALSYM PropertyTagThumbnailPlanes}
  PropertyTagThumbnailRawBytes         = $5017;
  {.$EXTERNALSYM PropertyTagThumbnailRawBytes}
  PropertyTagThumbnailSize             = $5018;
  {.$EXTERNALSYM PropertyTagThumbnailSize}
  PropertyTagThumbnailCompressedSize   = $5019;
  {.$EXTERNALSYM PropertyTagThumbnailCompressedSize}
  PropertyTagColorTransferFunction     = $501A;
  {.$EXTERNALSYM PropertyTagColorTransferFunction}
  PropertyTagThumbnailData             = $501B;    // RAW thumbnail bits in
  {.$EXTERNALSYM PropertyTagThumbnailData}
                                                   // JPEG format or RGB format
                                                   // depends on
                                                   // PropertyTagThumbnailFormat

  // Thumbnail related TAGs

  PropertyTagThumbnailImageWidth        = $5020;   // Thumbnail width
  {.$EXTERNALSYM PropertyTagThumbnailImageWidth}
  PropertyTagThumbnailImageHeight       = $5021;   // Thumbnail height
  {.$EXTERNALSYM PropertyTagThumbnailImageHeight}
  PropertyTagThumbnailBitsPerSample     = $5022;   // Number of bits per
  {.$EXTERNALSYM PropertyTagThumbnailBitsPerSample}
                                                   // component
  PropertyTagThumbnailCompression       = $5023;   // Compression Scheme
  {.$EXTERNALSYM PropertyTagThumbnailCompression}
  PropertyTagThumbnailPhotometricInterp = $5024;   // Pixel composition
  {.$EXTERNALSYM PropertyTagThumbnailPhotometricInterp}
  PropertyTagThumbnailImageDescription  = $5025;   // Image Tile
  {.$EXTERNALSYM PropertyTagThumbnailImageDescription}
  PropertyTagThumbnailEquipMake         = $5026;   // Manufacturer of Image
  {.$EXTERNALSYM PropertyTagThumbnailEquipMake}
                                                   // Input equipment
  PropertyTagThumbnailEquipModel        = $5027;   // Model of Image input
  {.$EXTERNALSYM PropertyTagThumbnailEquipModel}
                                                   // equipment
  PropertyTagThumbnailStripOffsets    = $5028;  // Image data location
  {.$EXTERNALSYM PropertyTagThumbnailStripOffsets}
  PropertyTagThumbnailOrientation     = $5029;  // Orientation of image
  {.$EXTERNALSYM PropertyTagThumbnailOrientation}
  PropertyTagThumbnailSamplesPerPixel = $502A;  // Number of components
  {.$EXTERNALSYM PropertyTagThumbnailSamplesPerPixel}
  PropertyTagThumbnailRowsPerStrip    = $502B;  // Number of rows per strip
  {.$EXTERNALSYM PropertyTagThumbnailRowsPerStrip}
  PropertyTagThumbnailStripBytesCount = $502C;  // Bytes per compressed
  {.$EXTERNALSYM PropertyTagThumbnailStripBytesCount}
                                                // strip
  PropertyTagThumbnailResolutionX     = $502D;  // Resolution in width
  {.$EXTERNALSYM PropertyTagThumbnailResolutionX}
                                                // direction
  PropertyTagThumbnailResolutionY     = $502E;  // Resolution in height
  {.$EXTERNALSYM PropertyTagThumbnailResolutionY}
                                                // direction
  PropertyTagThumbnailPlanarConfig    = $502F;  // Image data arrangement
  {.$EXTERNALSYM PropertyTagThumbnailPlanarConfig}
  PropertyTagThumbnailResolutionUnit  = $5030;  // Unit of X and Y
  {.$EXTERNALSYM PropertyTagThumbnailResolutionUnit}
                                                // Resolution
  PropertyTagThumbnailTransferFunction = $5031;  // Transfer function
  {.$EXTERNALSYM PropertyTagThumbnailTransferFunction}
  PropertyTagThumbnailSoftwareUsed     = $5032;  // Software used
  {.$EXTERNALSYM PropertyTagThumbnailSoftwareUsed}
  PropertyTagThumbnailDateTime         = $5033;  // File change date and
  {.$EXTERNALSYM PropertyTagThumbnailDateTime}
                                                 // time
  PropertyTagThumbnailArtist          = $5034;  // Person who created the
  {.$EXTERNALSYM PropertyTagThumbnailArtist}
                                                // image
  PropertyTagThumbnailWhitePoint      = $5035;  // White point chromaticity
  {.$EXTERNALSYM PropertyTagThumbnailWhitePoint}
  PropertyTagThumbnailPrimaryChromaticities = $5036;
  {.$EXTERNALSYM PropertyTagThumbnailPrimaryChromaticities}
                                                    // Chromaticities of
                                                    // primaries
  PropertyTagThumbnailYCbCrCoefficients = $5037; // Color space transforma-
  {.$EXTERNALSYM PropertyTagThumbnailYCbCrCoefficients}
                                                 // tion coefficients
  PropertyTagThumbnailYCbCrSubsampling = $5038;  // Subsampling ratio of Y
  {.$EXTERNALSYM PropertyTagThumbnailYCbCrSubsampling}
                                                 // to C
  PropertyTagThumbnailYCbCrPositioning = $5039;  // Y and C position
  {.$EXTERNALSYM PropertyTagThumbnailYCbCrPositioning}
  PropertyTagThumbnailRefBlackWhite    = $503A;  // Pair of black and white
  {.$EXTERNALSYM PropertyTagThumbnailRefBlackWhite}
                                                 // reference values
  PropertyTagThumbnailCopyRight       = $503B;   // CopyRight holder
  {.$EXTERNALSYM PropertyTagThumbnailCopyRight}

  PropertyTagLuminanceTable           = $5090;
  {.$EXTERNALSYM PropertyTagLuminanceTable}
  PropertyTagChrominanceTable         = $5091;
  {.$EXTERNALSYM PropertyTagChrominanceTable}

  PropertyTagFrameDelay               = $5100;
  {.$EXTERNALSYM PropertyTagFrameDelay}
  PropertyTagLoopCount                = $5101;
  {.$EXTERNALSYM PropertyTagLoopCount}

  PropertyTagPixelUnit         = $5110;  // Unit specifier for pixel/unit
  {.$EXTERNALSYM PropertyTagPixelUnit}
  PropertyTagPixelPerUnitX     = $5111;  // Pixels per unit in X
  {.$EXTERNALSYM PropertyTagPixelPerUnitX}
  PropertyTagPixelPerUnitY     = $5112;  // Pixels per unit in Y
  {.$EXTERNALSYM PropertyTagPixelPerUnitY}
  PropertyTagPaletteHistogram  = $5113;  // Palette histogram
  {.$EXTERNALSYM PropertyTagPaletteHistogram}

  // EXIF specific tag

  PropertyTagExifExposureTime  = $829A;
  {.$EXTERNALSYM PropertyTagExifExposureTime}
  PropertyTagExifFNumber       = $829D;
  {.$EXTERNALSYM PropertyTagExifFNumber}

  PropertyTagExifExposureProg  = $8822;
  {.$EXTERNALSYM PropertyTagExifExposureProg}
  PropertyTagExifSpectralSense = $8824;
  {.$EXTERNALSYM PropertyTagExifSpectralSense}
  PropertyTagExifISOSpeed      = $8827;
  {.$EXTERNALSYM PropertyTagExifISOSpeed}
  PropertyTagExifOECF          = $8828;
  {.$EXTERNALSYM PropertyTagExifOECF}

  PropertyTagExifVer           = $9000;
  {.$EXTERNALSYM PropertyTagExifVer}
  PropertyTagExifDTOrig        = $9003; // Date & time of original
  {.$EXTERNALSYM PropertyTagExifDTOrig}
  PropertyTagExifDTDigitized   = $9004; // Date & time of digital data generation
  {.$EXTERNALSYM PropertyTagExifDTDigitized}

  PropertyTagExifCompConfig    = $9101;
  {.$EXTERNALSYM PropertyTagExifCompConfig}
  PropertyTagExifCompBPP       = $9102;
  {.$EXTERNALSYM PropertyTagExifCompBPP}

  PropertyTagExifShutterSpeed  = $9201;
  {.$EXTERNALSYM PropertyTagExifShutterSpeed}
  PropertyTagExifAperture      = $9202;
  {.$EXTERNALSYM PropertyTagExifAperture}
  PropertyTagExifBrightness    = $9203;
  {.$EXTERNALSYM PropertyTagExifBrightness}
  PropertyTagExifExposureBias  = $9204;
  {.$EXTERNALSYM PropertyTagExifExposureBias}
  PropertyTagExifMaxAperture   = $9205;
  {.$EXTERNALSYM PropertyTagExifMaxAperture}
  PropertyTagExifSubjectDist   = $9206;
  {.$EXTERNALSYM PropertyTagExifSubjectDist}
  PropertyTagExifMeteringMode  = $9207;
  {.$EXTERNALSYM PropertyTagExifMeteringMode}
  PropertyTagExifLightSource   = $9208;
  {.$EXTERNALSYM PropertyTagExifLightSource}
  PropertyTagExifFlash         = $9209;
  {.$EXTERNALSYM PropertyTagExifFlash}
  PropertyTagExifFocalLength   = $920A;
  {.$EXTERNALSYM PropertyTagExifFocalLength}
  PropertyTagExifMakerNote     = $927C;
  {.$EXTERNALSYM PropertyTagExifMakerNote}
  PropertyTagExifUserComment   = $9286;
  {.$EXTERNALSYM PropertyTagExifUserComment}
  PropertyTagExifDTSubsec      = $9290;  // Date & Time subseconds
  {.$EXTERNALSYM PropertyTagExifDTSubsec}
  PropertyTagExifDTOrigSS      = $9291;  // Date & Time original subseconds
  {.$EXTERNALSYM PropertyTagExifDTOrigSS}
  PropertyTagExifDTDigSS       = $9292;  // Date & TIme digitized subseconds
  {.$EXTERNALSYM PropertyTagExifDTDigSS}

  PropertyTagExifFPXVer        = $A000;
  {.$EXTERNALSYM PropertyTagExifFPXVer}
  PropertyTagExifColorSpace    = $A001;
  {.$EXTERNALSYM PropertyTagExifColorSpace}
  PropertyTagExifPixXDim       = $A002;
  {.$EXTERNALSYM PropertyTagExifPixXDim}
  PropertyTagExifPixYDim       = $A003;
  {.$EXTERNALSYM PropertyTagExifPixYDim}
  PropertyTagExifRelatedWav    = $A004;  // related sound file
  {.$EXTERNALSYM PropertyTagExifRelatedWav}
  PropertyTagExifInterop       = $A005;
  {.$EXTERNALSYM PropertyTagExifInterop}
  PropertyTagExifFlashEnergy   = $A20B;
  {.$EXTERNALSYM PropertyTagExifFlashEnergy}
  PropertyTagExifSpatialFR     = $A20C;  // Spatial Frequency Response
  {.$EXTERNALSYM PropertyTagExifSpatialFR}
  PropertyTagExifFocalXRes     = $A20E;  // Focal Plane X Resolution
  {.$EXTERNALSYM PropertyTagExifFocalXRes}
  PropertyTagExifFocalYRes     = $A20F;  // Focal Plane Y Resolution
  {.$EXTERNALSYM PropertyTagExifFocalYRes}
  PropertyTagExifFocalResUnit  = $A210;  // Focal Plane Resolution Unit
  {.$EXTERNALSYM PropertyTagExifFocalResUnit}
  PropertyTagExifSubjectLoc    = $A214;
  {.$EXTERNALSYM PropertyTagExifSubjectLoc}
  PropertyTagExifExposureIndex = $A215;
  {.$EXTERNALSYM PropertyTagExifExposureIndex}
  PropertyTagExifSensingMethod = $A217;
  {.$EXTERNALSYM PropertyTagExifSensingMethod}
  PropertyTagExifFileSource    = $A300;
  {.$EXTERNALSYM PropertyTagExifFileSource}
  PropertyTagExifSceneType     = $A301;
  {.$EXTERNALSYM PropertyTagExifSceneType}
  PropertyTagExifCfaPattern    = $A302;
  {.$EXTERNALSYM PropertyTagExifCfaPattern}

  PropertyTagGpsVer            = $0000;
  {.$EXTERNALSYM PropertyTagGpsVer}
  PropertyTagGpsLatitudeRef    = $0001;
  {.$EXTERNALSYM PropertyTagGpsLatitudeRef}
  PropertyTagGpsLatitude       = $0002;
  {.$EXTERNALSYM PropertyTagGpsLatitude}
  PropertyTagGpsLongitudeRef   = $0003;
  {.$EXTERNALSYM PropertyTagGpsLongitudeRef}
  PropertyTagGpsLongitude      = $0004;
  {.$EXTERNALSYM PropertyTagGpsLongitude}
  PropertyTagGpsAltitudeRef    = $0005;
  {.$EXTERNALSYM PropertyTagGpsAltitudeRef}
  PropertyTagGpsAltitude       = $0006;
  {.$EXTERNALSYM PropertyTagGpsAltitude}
  PropertyTagGpsGpsTime        = $0007;
  {.$EXTERNALSYM PropertyTagGpsGpsTime}
  PropertyTagGpsGpsSatellites  = $0008;
  {.$EXTERNALSYM PropertyTagGpsGpsSatellites}
  PropertyTagGpsGpsStatus      = $0009;
  {.$EXTERNALSYM PropertyTagGpsGpsStatus}
  PropertyTagGpsGpsMeasureMode = $00A;
  {.$EXTERNALSYM PropertyTagGpsGpsMeasureMode}
  PropertyTagGpsGpsDop         = $000B;  // Measurement precision
  {.$EXTERNALSYM PropertyTagGpsGpsDop}
  PropertyTagGpsSpeedRef       = $000C;
  {.$EXTERNALSYM PropertyTagGpsSpeedRef}
  PropertyTagGpsSpeed          = $000D;
  {.$EXTERNALSYM PropertyTagGpsSpeed}
  PropertyTagGpsTrackRef       = $000E;
  {.$EXTERNALSYM PropertyTagGpsTrackRef}
  PropertyTagGpsTrack          = $000F;
  {.$EXTERNALSYM PropertyTagGpsTrack}
  PropertyTagGpsImgDirRef      = $0010;
  {.$EXTERNALSYM PropertyTagGpsImgDirRef}
  PropertyTagGpsImgDir         = $0011;
  {.$EXTERNALSYM PropertyTagGpsImgDir}
  PropertyTagGpsMapDatum       = $0012;
  {.$EXTERNALSYM PropertyTagGpsMapDatum}
  PropertyTagGpsDestLatRef     = $0013;
  {.$EXTERNALSYM PropertyTagGpsDestLatRef}
  PropertyTagGpsDestLat        = $0014;
  {.$EXTERNALSYM PropertyTagGpsDestLat}
  PropertyTagGpsDestLongRef    = $0015;
  {.$EXTERNALSYM PropertyTagGpsDestLongRef}
  PropertyTagGpsDestLong       = $0016;
  {.$EXTERNALSYM PropertyTagGpsDestLong}
  PropertyTagGpsDestBearRef    = $0017;
  {.$EXTERNALSYM PropertyTagGpsDestBearRef}
  PropertyTagGpsDestBear       = $0018;
  {.$EXTERNALSYM PropertyTagGpsDestBear}
  PropertyTagGpsDestDistRef    = $0019;
  {.$EXTERNALSYM PropertyTagGpsDestDistRef}
  PropertyTagGpsDestDist       = $001A;
  {.$EXTERNALSYM PropertyTagGpsDestDist}

(**************************************************************************\
*
*  GDI+ Color Matrix object, used with Graphics.DrawImage
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Color matrix
//----------------------------------------------------------------------------

type
  {.$EXTERNALSYM ColorMatrix}
  ColorMatrix = packed array[0..4, 0..4] of Single;
  TColorMatrix = ColorMatrix;
  PColorMatrix = ^TColorMatrix;

//----------------------------------------------------------------------------
// Color Matrix flags
//----------------------------------------------------------------------------

  {.$EXTERNALSYM ColorMatrixFlags}
  ColorMatrixFlags = (
    ColorMatrixFlagsDefault,
    ColorMatrixFlagsSkipGrays,
    ColorMatrixFlagsAltGray
  );
  TColorMatrixFlags = ColorMatrixFlags;

//----------------------------------------------------------------------------
// Color Adjust Type
//----------------------------------------------------------------------------

  {.$EXTERNALSYM ColorAdjustType}
  ColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny      // Reserved
  );
  TColorAdjustType = ColorAdjustType;

//----------------------------------------------------------------------------
// Color Map
//----------------------------------------------------------------------------

  {.$EXTERNALSYM ColorMap}
  ColorMap = packed record
    oldColor: TGPColor;
    newColor: TGPColor;
  end;
  TColorMap = ColorMap;
  PColorMap = ^TColorMap;

//---------------------------------------------------------------------------
// Private GDI+ classes for internal type checking
//---------------------------------------------------------------------------

  GpGraphics = Pointer;

  GpBrush = Pointer;
  GpTexture = Pointer;
  GpSolidFill = Pointer;
  GpLineGradient = Pointer;
  GpPathGradient = Pointer;
  GpHatch =  Pointer;

  GpPen = Pointer;
  GpCustomLineCap = Pointer;
  GpAdjustableArrowCap = Pointer;

  GpImage = Pointer;
  GpBitmap = Pointer;
  GpMetafile = Pointer;
  GpImageAttributes = Pointer;

  GpPath = Pointer;
  GpRegion = Pointer;
  GpPathIterator = Pointer;

  GpFontFamily = Pointer;
  GpFont = Pointer;
  GpStringFormat = Pointer;
  GpFontCollection = Pointer;
  GpCachedBitmap = Pointer;

  GpStatus          = TStatus;
  GpFillMode        = TFillMode;
  GpWrapMode        = TWrapMode;
  GpUnit            = TUnit;
  GpCoordinateSpace = TCoordinateSpace;
  GpPointF          = PGPPointF;
  GpPoint           = PGPPoint;
  GpRectF           = PGPRectF;
  GpRect            = PGPRect;
  GpSizeF           = PGPSizeF;
  GpHatchStyle      = THatchStyle;
  GpDashStyle       = TDashStyle;
  GpLineCap         = TLineCap;
  GpDashCap         = TDashCap;

  GpPenAlignment    = TPenAlignment;

  GpLineJoin        = TLineJoin;
  GpPenType         = TPenType;

  GpMatrix          = Pointer; 
  GpBrushType       = TBrushType;
  GpMatrixOrder     = TMatrixOrder;
  GpFlushIntention  = TFlushIntention;
  GpPathData        = TPathData;

(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
* Module Name:
*   GdiplusFlat.h
* Abstract:
*   Private GDI+ header file.
*
\**************************************************************************)

var

GdipCreatePath: function(brushMode: GPFILLMODE;
    out path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePath}

GdipCreatePath2: function(v1: GPPOINTF; v2: PBYTE; v3: Integer; v4: GPFILLMODE;  
    out path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePath2}

GdipCreatePath2I: function(v1: GPPOINT; v2: PBYTE; v3: Integer; v4: GPFILLMODE;  
    out path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePath2I}

GdipClonePath: function(path: GPPATH;  
    out clonePath: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipClonePath}

GdipDeletePath: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeletePath}

GdipResetPath: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetPath}

GdipGetPointCount: function(path: GPPATH;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPointCount}

GdipGetPathTypes: function(path: GPPATH; types: PBYTE;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathTypes}

GdipGetPathPoints: function(v1: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathPoints}

GdipGetPathPointsI: function(v1: GPPATH; points: GPPOINT;  
             count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathPointsI}

GdipGetPathFillMode: function(path: GPPATH;  
    var fillmode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathFillMode}

GdipSetPathFillMode: function(path: GPPATH;  
    fillmode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathFillMode}

GdipGetPathData: function(path: GPPATH;  
    pathData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathData}

GdipStartPathFigure: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipStartPathFigure}

GdipClosePathFigure: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipClosePathFigure}

GdipClosePathFigures: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipClosePathFigures}

GdipSetPathMarker: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPathMarker}

GdipClearPathMarkers: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipClearPathMarkers}

GdipReversePath: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipReversePath}

GdipGetPathLastPoint: function(path: GPPATH;  
    lastPoint: GPPOINTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathLastPoint}

GdipAddPathLine: function(path: GPPATH;  
    x1, y1, x2, y2: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathLine}

GdipAddPathLine2: function(path: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathLine2}

GdipAddPathArc: function(path: GPPATH; x, y, width, height, startAngle,  
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathArc}

GdipAddPathBezier: function(path: GPPATH;  
    x1, y1, x2, y2, x3, y3, x4, y4: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathBezier}

GdipAddPathBeziers: function(path: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathBeziers}

GdipAddPathCurve: function(path: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurve}

GdipAddPathCurve2: function(path: GPPATH; points: GPPOINTF; count: Integer;  
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurve2}

GdipAddPathCurve3: function(path: GPPATH; points: GPPOINTF; count: Integer;  
    offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurve3}

GdipAddPathClosedCurve: function(path: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathClosedCurve}

GdipAddPathClosedCurve2: function(path: GPPATH; points: GPPOINTF;  
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathClosedCurve2}

GdipAddPathRectangle: function(path: GPPATH; x: Single; y: Single;  
    width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathRectangle}

GdipAddPathRectangles: function(path: GPPATH; rects: GPRECTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathRectangles}

GdipAddPathEllipse: function(path: GPPATH;  x: Single; y: Single;  
    width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathEllipse}

GdipAddPathPie: function(path: GPPATH; x: Single; y: Single; width: Single;  
    height: Single; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathPie}

GdipAddPathPolygon: function(path: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathPolygon}

GdipAddPathPath: function(path: GPPATH; addingPath: GPPATH;  
    connect: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathPath}

GdipAddPathString: function(path: GPPATH; string_: PWCHAR; length: Integer;  
    family: GPFONTFAMILY; style: Integer; emSize: Single; layoutRect: PGPRectF;
    format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathString}

GdipAddPathStringI: function(path: GPPATH; string_: PWCHAR; length: Integer;  
    family: GPFONTFAMILY; style: Integer; emSize: Single; layoutRect: PGPRect;
    format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathStringI}

GdipAddPathLineI: function(path: GPPATH; x1: Integer; y1: Integer; x2: Integer;  
    y2: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathLineI}

GdipAddPathLine2I: function(path: GPPATH; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathLine2I}

GdipAddPathArcI: function(path: GPPATH; x: Integer; y: Integer; width: Integer;  
    height: Integer; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathArcI}

GdipAddPathBezierI: function(path: GPPATH; x1: Integer; y1: Integer;  
    x2: Integer; y2: Integer; x3: Integer; y3: Integer; x4: Integer;
    y4: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathBezierI}

GdipAddPathBeziersI: function(path: GPPATH; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathBeziersI}

GdipAddPathCurveI: function(path: GPPATH; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurveI}

GdipAddPathCurve2I: function(path: GPPATH; points: GPPOINT; count: Integer;  
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurve2I}

GdipAddPathCurve3I: function(path: GPPATH; points: GPPOINT; count: Integer;  
    offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurve3I}

GdipAddPathClosedCurveI: function(path: GPPATH; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathClosedCurveI}

GdipAddPathClosedCurve2I: function(path: GPPATH; points: GPPOINT;  
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathClosedCurve2I}

GdipAddPathRectangleI: function(path: GPPATH; x: Integer; y: Integer;  
    width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathRectangleI}

GdipAddPathRectanglesI: function(path: GPPATH; rects: GPRECT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathRectanglesI}

GdipAddPathEllipseI: function(path: GPPATH; x: Integer; y: Integer;  
    width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathEllipseI}

GdipAddPathPieI: function(path: GPPATH; x: Integer; y: Integer; width: Integer;  
    height: Integer; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathPieI}

GdipAddPathPolygonI: function(path: GPPATH; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathPolygonI}

GdipFlattenPath: function(path: GPPATH; matrix: GPMATRIX;  
    flatness: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFlattenPath}

GdipWindingModeOutline: function(path: GPPATH; matrix: GPMATRIX;  
    flatness: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipWindingModeOutline}

GdipWidenPath: function(nativePath: GPPATH; pen: GPPEN; matrix: GPMATRIX;  
    flatness: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipWidenPath}

GdipWarpPath: function(path: GPPATH; matrix: GPMATRIX; points: GPPOINTF;  
    count: Integer; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; warpMode: WARPMODE; flatness: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipWarpPath}

GdipTransformPath: function(path: GPPATH; matrix: GPMATRIX): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipTransformPath}

GdipGetPathWorldBounds: function(path: GPPATH; bounds: GPRECTF;  
    matrix: GPMATRIX; pen: GPPEN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathWorldBounds}

GdipGetPathWorldBoundsI: function(path: GPPATH; bounds: GPRECT;  
    matrix: GPMATRIX; pen: GPPEN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathWorldBoundsI}

GdipIsVisiblePathPoint: function(path: GPPATH; x: Single; y: Single;  
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisiblePathPoint}

GdipIsVisiblePathPointI: function(path: GPPATH; x: Integer; y: Integer;  
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisiblePathPointI}

GdipIsOutlineVisiblePathPoint: function(path: GPPATH; x: Single; y: Single;  
    pen: GPPEN; graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsOutlineVisiblePathPoint}

GdipIsOutlineVisiblePathPointI: function(path: GPPATH; x: Integer; y: Integer;  
    pen: GPPEN; graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsOutlineVisiblePathPointI}

//----------------------------------------------------------------------------
// PathIterator APIs 
//----------------------------------------------------------------------------

GdipCreatePathIter: function(out iterator: GPPATHITERATOR;  
    path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePathIter}

GdipDeletePathIter: function(iterator: GPPATHITERATOR): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeletePathIter}

GdipPathIterNextSubpath: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; var startIndex: Integer; var endIndex: Integer;
    out isClosed: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterNextSubpath}

GdipPathIterNextSubpathPath: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; path: GPPATH;
    out isClosed: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterNextSubpathPath}

GdipPathIterNextPathType: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; pathType: PBYTE; var startIndex: Integer;
    var endIndex: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterNextPathType}

GdipPathIterNextMarker: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; var startIndex: Integer;
    var endIndex: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterNextMarker}

GdipPathIterNextMarkerPath: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterNextMarkerPath}

GdipPathIterGetCount: function(iterator: GPPATHITERATOR;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterGetCount}

GdipPathIterGetSubpathCount: function(iterator: GPPATHITERATOR;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterGetSubpathCount}

GdipPathIterIsValid: function(iterator: GPPATHITERATOR;  
    out valid: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterIsValid}

GdipPathIterHasCurve: function(iterator: GPPATHITERATOR;  
    out hasCurve: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterHasCurve}

GdipPathIterRewind: function(iterator: GPPATHITERATOR): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipPathIterRewind}

GdipPathIterEnumerate: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; points: GPPOINTF; types: PBYTE;
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterEnumerate}

GdipPathIterCopyData: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; points: GPPOINTF; types: PBYTE;
    startIndex: Integer; endIndex: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterCopyData}

//----------------------------------------------------------------------------
// Matrix APIs
//----------------------------------------------------------------------------

GdipCreateMatrix: function(out matrix: GPMATRIX): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipCreateMatrix}

GdipCreateMatrix2: function(m11: Single; m12: Single; m21: Single; m22: Single;  
    dx: Single; dy: Single; out matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMatrix2}

GdipCreateMatrix3: function(rect: GPRECTF; dstplg: GPPOINTF;  
    out matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMatrix3}

GdipCreateMatrix3I: function(rect: GPRECT; dstplg: GPPOINT;  
    out matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMatrix3I}

GdipCloneMatrix: function(matrix: GPMATRIX;  
    out cloneMatrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneMatrix}

GdipDeleteMatrix: function(matrix: GPMATRIX): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteMatrix}

GdipSetMatrixElements: function(matrix: GPMATRIX; m11: Single; m12: Single;  
    m21: Single; m22: Single; dx: Single; dy: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetMatrixElements}

GdipMultiplyMatrix: function(matrix: GPMATRIX; matrix2: GPMATRIX;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyMatrix}

GdipTranslateMatrix: function(matrix: GPMATRIX; offsetX: Single;  
    offsetY: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateMatrix}

GdipScaleMatrix: function(matrix: GPMATRIX; scaleX: Single; scaleY: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScaleMatrix}

GdipRotateMatrix: function(matrix: GPMATRIX; angle: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotateMatrix}

GdipShearMatrix: function(matrix: GPMATRIX; shearX: Single; shearY: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipShearMatrix}

GdipInvertMatrix: function(matrix: GPMATRIX): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipInvertMatrix}

GdipTransformMatrixPoints: function(matrix: GPMATRIX; pts: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTransformMatrixPoints}

GdipTransformMatrixPointsI: function(matrix: GPMATRIX; pts: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTransformMatrixPointsI}

GdipVectorTransformMatrixPoints: function(matrix: GPMATRIX; pts: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipVectorTransformMatrixPoints}

GdipVectorTransformMatrixPointsI: function(matrix: GPMATRIX; pts: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipVectorTransformMatrixPointsI}

GdipGetMatrixElements: function(matrix: GPMATRIX;  
    matrixOut: PSingle): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMatrixElements}

GdipIsMatrixInvertible: function(matrix: GPMATRIX;  
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsMatrixInvertible}

GdipIsMatrixIdentity: function(matrix: GPMATRIX;  
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsMatrixIdentity}

GdipIsMatrixEqual: function(matrix: GPMATRIX; matrix2: GPMATRIX;  
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsMatrixEqual}

//----------------------------------------------------------------------------
// Region APIs
//----------------------------------------------------------------------------

GdipCreateRegion: function(out region: GPREGION): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipCreateRegion}

GdipCreateRegionRect: function(rect: GPRECTF;  
    out region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateRegionRect}

GdipCreateRegionRectI: function(rect: GPRECT;  
    out region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateRegionRectI}

GdipCreateRegionPath: function(path: GPPATH;  
    out region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateRegionPath}

GdipCreateRegionRgnData: function(regionData: PBYTE; size: Integer;  
    out region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateRegionRgnData}

GdipCreateRegionHrgn: function(hRgn: HRGN;  
    out region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateRegionHrgn}

GdipCloneRegion: function(region: GPREGION;  
    out cloneRegion: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneRegion}

GdipDeleteRegion: function(region: GPREGION): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteRegion}

GdipSetInfinite: function(region: GPREGION): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetInfinite}

GdipSetEmpty: function(region: GPREGION): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetEmpty}

GdipCombineRegionRect: function(region: GPREGION; rect: GPRECTF;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCombineRegionRect}

GdipCombineRegionRectI: function(region: GPREGION; rect: GPRECT;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCombineRegionRectI}

GdipCombineRegionPath: function(region: GPREGION; path: GPPATH;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCombineRegionPath}

GdipCombineRegionRegion: function(region: GPREGION; region2: GPREGION;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCombineRegionRegion}

GdipTranslateRegion: function(region: GPREGION; dx: Single;  
    dy: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateRegion}

GdipTranslateRegionI: function(region: GPREGION; dx: Integer;  
    dy: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateRegionI}

GdipTransformRegion: function(region: GPREGION;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTransformRegion}

GdipGetRegionBounds: function(region: GPREGION; graphics: GPGRAPHICS;  
    rect: GPRECTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionBounds}

GdipGetRegionBoundsI: function(region: GPREGION; graphics: GPGRAPHICS;  
    rect: GPRECT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionBoundsI}

GdipGetRegionHRgn: function(region: GPREGION; graphics: GPGRAPHICS;  
    out hRgn: HRGN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionHRgn}

GdipIsEmptyRegion: function(region: GPREGION; graphics: GPGRAPHICS;  
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsEmptyRegion}

GdipIsInfiniteRegion: function(region: GPREGION; graphics: GPGRAPHICS;  
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsInfiniteRegion}

GdipIsEqualRegion: function(region: GPREGION; region2: GPREGION;  
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsEqualRegion}

GdipGetRegionDataSize: function(region: GPREGION;  
    out bufferSize: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionDataSize}

GdipGetRegionData: function(region: GPREGION; buffer: PBYTE;  
    bufferSize: UINT; sizeFilled: PUINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionData}

GdipIsVisibleRegionPoint: function(region: GPREGION; x: Single; y: Single;  
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRegionPoint}

GdipIsVisibleRegionPointI: function(region: GPREGION; x: Integer; y: Integer;  
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRegionPointI}

GdipIsVisibleRegionRect: function(region: GPREGION; x: Single; y: Single;  
    width: Single; height: Single; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRegionRect}

GdipIsVisibleRegionRectI: function(region: GPREGION; x: Integer; y: Integer;  
    width: Integer; height: Integer; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRegionRectI}

GdipGetRegionScansCount: function(region: GPREGION; out count: UINT;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionScansCount}

GdipGetRegionScans: function(region: GPREGION; rects: GPRECTF;  
    out count: Integer; matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionScans}

GdipGetRegionScansI: function(region: GPREGION; rects: GPRECT;  
    out count: Integer; matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionScansI}

//----------------------------------------------------------------------------
// Brush APIs
//----------------------------------------------------------------------------

GdipCloneBrush: function(brush: GPBRUSH;  
    out cloneBrush: GPBRUSH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneBrush}

GdipDeleteBrush: function(brush: GPBRUSH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteBrush}

GdipGetBrushType: function(brush: GPBRUSH;  
    out type_: GPBRUSHTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetBrushType}

//----------------------------------------------------------------------------
// HatchBrush APIs
//----------------------------------------------------------------------------

GdipCreateHatchBrush: function(hatchstyle: Integer; forecol: ARGB;  
    backcol: ARGB; out brush: GPHATCH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateHatchBrush}

GdipGetHatchStyle: function(brush: GPHATCH;  
    out hatchstyle: GPHATCHSTYLE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetHatchStyle}

GdipGetHatchForegroundColor: function(brush: GPHATCH;  
    out forecol: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetHatchForegroundColor}

GdipGetHatchBackgroundColor: function(brush: GPHATCH;  
    out backcol: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetHatchBackgroundColor}

//----------------------------------------------------------------------------
// TextureBrush APIs
//----------------------------------------------------------------------------


GdipCreateTexture: function(image: GPIMAGE; wrapmode: GPWRAPMODE;  
    var texture: GPTEXTURE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateTexture}

GdipCreateTexture2: function(image: GPIMAGE; wrapmode: GPWRAPMODE;  
    x: Single; y: Single; width: Single; height: Single;
    out texture: GPTEXTURE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateTexture2}

GdipCreateTextureIA: function(image: GPIMAGE;  
    imageAttributes: GPIMAGEATTRIBUTES; x: Single; y: Single; width: Single;
    height: Single; out texture: GPTEXTURE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateTextureIA}

GdipCreateTexture2I: function(image: GPIMAGE; wrapmode: GPWRAPMODE; x: Integer;  
    y: Integer; width: Integer; height: Integer;
    out texture: GPTEXTURE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateTexture2I}

GdipCreateTextureIAI: function(image: GPIMAGE;  
    imageAttributes: GPIMAGEATTRIBUTES; x: Integer; y: Integer; width: Integer;
    height: Integer; out texture: GPTEXTURE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateTextureIAI}

GdipGetTextureTransform: function(brush: GPTEXTURE;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetTextureTransform}

GdipSetTextureTransform: function(brush: GPTEXTURE;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetTextureTransform}

GdipResetTextureTransform: function(brush: GPTEXTURE): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetTextureTransform}

GdipMultiplyTextureTransform: function(brush: GPTEXTURE; matrix: GPMATRIX;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyTextureTransform}

GdipTranslateTextureTransform: function(brush: GPTEXTURE; dx: Single;  
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateTextureTransform}

GdipScaleTextureTransform: function(brush: GPTEXTURE; sx: Single; sy: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScaleTextureTransform}

GdipRotateTextureTransform: function(brush: GPTEXTURE; angle: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotateTextureTransform}

GdipSetTextureWrapMode: function(brush: GPTEXTURE;  
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetTextureWrapMode}

GdipGetTextureWrapMode: function(brush: GPTEXTURE;  
    var wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetTextureWrapMode}

GdipGetTextureImage: function(brush: GPTEXTURE;  
    out image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetTextureImage}

//----------------------------------------------------------------------------
// SolidBrush APIs
//----------------------------------------------------------------------------

GdipCreateSolidFill: function(color: ARGB;  
    out brush: GPSOLIDFILL): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateSolidFill}

GdipSetSolidFillColor: function(brush: GPSOLIDFILL;  
    color: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetSolidFillColor}

GdipGetSolidFillColor: function(brush: GPSOLIDFILL;  
    out color: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetSolidFillColor}

//----------------------------------------------------------------------------
// LineBrush APIs
//----------------------------------------------------------------------------

GdipCreateLineBrush: function(point1: GPPOINTF; point2: GPPOINTF; color1: ARGB;  
    color2: ARGB; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrush}

GdipCreateLineBrushI: function(point1: GPPOINT; point2: GPPOINT; color1: ARGB;  
    color2: ARGB; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrushI}

GdipCreateLineBrushFromRect: function(rect: GPRECTF; color1: ARGB;  
    color2: ARGB; mode: LINEARGRADIENTMODE; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrushFromRect}

GdipCreateLineBrushFromRectI: function(rect: GPRECT; color1: ARGB;  
    color2: ARGB; mode: LINEARGRADIENTMODE; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrushFromRectI}

GdipCreateLineBrushFromRectWithAngle: function(rect: GPRECTF; color1: ARGB;  
    color2: ARGB; angle: Single; isAngleScalable: Bool; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrushFromRectWithAngle}

GdipCreateLineBrushFromRectWithAngleI: function(rect: GPRECT; color1: ARGB;  
    color2: ARGB; angle: Single; isAngleScalable: Bool; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrushFromRectWithAngleI}

GdipSetLineColors: function(brush: GPLINEGRADIENT; color1: ARGB;  
    color2: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineColors}

GdipGetLineColors: function(brush: GPLINEGRADIENT;  
    colors: PARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineColors}

GdipGetLineRect: function(brush: GPLINEGRADIENT;  
    rect: GPRECTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineRect}

GdipGetLineRectI: function(brush: GPLINEGRADIENT;  
    rect: GPRECT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineRectI}

GdipSetLineGammaCorrection: function(brush: GPLINEGRADIENT;  
    useGammaCorrection: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineGammaCorrection}

GdipGetLineGammaCorrection: function(brush: GPLINEGRADIENT;  
    out useGammaCorrection: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineGammaCorrection}

GdipGetLineBlendCount: function(brush: GPLINEGRADIENT;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineBlendCount}

GdipGetLineBlend: function(brush: GPLINEGRADIENT; blend: PSingle;  
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineBlend}

GdipSetLineBlend: function(brush: GPLINEGRADIENT; blend: PSingle;  
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineBlend}

GdipGetLinePresetBlendCount: function(brush: GPLINEGRADIENT;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLinePresetBlendCount}

GdipGetLinePresetBlend: function(brush: GPLINEGRADIENT; blend: PARGB;  
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLinePresetBlend}

GdipSetLinePresetBlend: function(brush: GPLINEGRADIENT; blend: PARGB;  
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLinePresetBlend}

GdipSetLineSigmaBlend: function(brush: GPLINEGRADIENT; focus: Single;  
    scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineSigmaBlend}

GdipSetLineLinearBlend: function(brush: GPLINEGRADIENT; focus: Single;  
    scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineLinearBlend}

GdipSetLineWrapMode: function(brush: GPLINEGRADIENT;  
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineWrapMode}

GdipGetLineWrapMode: function(brush: GPLINEGRADIENT;  
    out wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineWrapMode}

GdipGetLineTransform: function(brush: GPLINEGRADIENT;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineTransform}

GdipSetLineTransform: function(brush: GPLINEGRADIENT;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineTransform}

GdipResetLineTransform: function(brush: GPLINEGRADIENT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetLineTransform}

GdipMultiplyLineTransform: function(brush: GPLINEGRADIENT; matrix: GPMATRIX;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyLineTransform}

GdipTranslateLineTransform: function(brush: GPLINEGRADIENT; dx: Single;  
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateLineTransform}

GdipScaleLineTransform: function(brush: GPLINEGRADIENT; sx: Single; sy: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScaleLineTransform}

GdipRotateLineTransform: function(brush: GPLINEGRADIENT; angle: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotateLineTransform}

//----------------------------------------------------------------------------
// PathGradientBrush APIs
//----------------------------------------------------------------------------

GdipCreatePathGradient: function(points: GPPOINTF; count: Integer;  
    wrapMode: GPWRAPMODE; out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePathGradient}

GdipCreatePathGradientI: function(points: GPPOINT; count: Integer;  
    wrapMode: GPWRAPMODE; out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePathGradientI}

GdipCreatePathGradientFromPath: function(path: GPPATH;  
    out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePathGradientFromPath}

GdipGetPathGradientCenterColor: function(brush: GPPATHGRADIENT;  
    out colors: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientCenterColor}

GdipSetPathGradientCenterColor: function(brush: GPPATHGRADIENT;  
    colors: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientCenterColor}

GdipGetPathGradientSurroundColorsWithCount: function(brush: GPPATHGRADIENT;  
    color: PARGB; var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientSurroundColorsWithCount}

GdipSetPathGradientSurroundColorsWithCount: function(brush: GPPATHGRADIENT;  
    color: PARGB; var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientSurroundColorsWithCount}

GdipGetPathGradientPath: function(brush: GPPATHGRADIENT;  
    path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientPath}

GdipSetPathGradientPath: function(brush: GPPATHGRADIENT;  
    path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientPath}

GdipGetPathGradientCenterPoint: function(brush: GPPATHGRADIENT;  
    points: GPPOINTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientCenterPoint}

GdipGetPathGradientCenterPointI: function(brush: GPPATHGRADIENT;  
    points: GPPOINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientCenterPointI}

GdipSetPathGradientCenterPoint: function(brush: GPPATHGRADIENT;  
    points: GPPOINTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientCenterPoint}

GdipSetPathGradientCenterPointI: function(brush: GPPATHGRADIENT;  
    points: GPPOINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientCenterPointI}

GdipGetPathGradientRect: function(brush: GPPATHGRADIENT;  
    rect: GPRECTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientRect}

GdipGetPathGradientRectI: function(brush: GPPATHGRADIENT;  
    rect: GPRECT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientRectI}

GdipGetPathGradientPointCount: function(brush: GPPATHGRADIENT;  
    var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientPointCount}

GdipGetPathGradientSurroundColorCount: function(brush: GPPATHGRADIENT;  
    var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientSurroundColorCount}

GdipSetPathGradientGammaCorrection: function(brush: GPPATHGRADIENT;  
    useGammaCorrection: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientGammaCorrection}

GdipGetPathGradientGammaCorrection: function(brush: GPPATHGRADIENT;  
    var useGammaCorrection: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientGammaCorrection}

GdipGetPathGradientBlendCount: function(brush: GPPATHGRADIENT;  
    var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientBlendCount}

GdipGetPathGradientBlend: function(brush: GPPATHGRADIENT;  
    blend: PSingle; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientBlend}

GdipSetPathGradientBlend: function(brush: GPPATHGRADIENT;  
    blend: PSingle; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientBlend}

GdipGetPathGradientPresetBlendCount: function(brush: GPPATHGRADIENT;  
    var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientPresetBlendCount}

GdipGetPathGradientPresetBlend: function(brush: GPPATHGRADIENT;  
    blend: PARGB; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientPresetBlend}

GdipSetPathGradientPresetBlend: function(brush: GPPATHGRADIENT;  
    blend: PARGB; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientPresetBlend}

GdipSetPathGradientSigmaBlend: function(brush: GPPATHGRADIENT;  
    focus: Single; scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientSigmaBlend}

GdipSetPathGradientLinearBlend: function(brush: GPPATHGRADIENT;  
    focus: Single; scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientLinearBlend}

GdipGetPathGradientWrapMode: function(brush: GPPATHGRADIENT;  
    var wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientWrapMode}

GdipSetPathGradientWrapMode: function(brush: GPPATHGRADIENT;  
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientWrapMode}

GdipGetPathGradientTransform: function(brush: GPPATHGRADIENT;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientTransform}

GdipSetPathGradientTransform: function(brush: GPPATHGRADIENT;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientTransform}

GdipResetPathGradientTransform: function(  
    brush: GPPATHGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipResetPathGradientTransform}

GdipMultiplyPathGradientTransform: function(brush: GPPATHGRADIENT;  
    matrix: GPMATRIX; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyPathGradientTransform}

GdipTranslatePathGradientTransform: function(brush: GPPATHGRADIENT;  
    dx: Single; dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslatePathGradientTransform}

GdipScalePathGradientTransform: function(brush: GPPATHGRADIENT;  
    sx: Single; sy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScalePathGradientTransform}

GdipRotatePathGradientTransform: function(brush: GPPATHGRADIENT;  
    angle: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotatePathGradientTransform}

GdipGetPathGradientFocusScales: function(brush: GPPATHGRADIENT;  
    var xScale: Single; var yScale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientFocusScales}

GdipSetPathGradientFocusScales: function(brush: GPPATHGRADIENT;  
    xScale: Single; yScale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientFocusScales}

//----------------------------------------------------------------------------
// Pen APIs
//----------------------------------------------------------------------------

GdipCreatePen1: function(color: ARGB; width: Single; unit_: GPUNIT;  
    out pen: GPPEN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePen1}

GdipCreatePen2: function(brush: GPBRUSH; width: Single; unit_: GPUNIT;  
    out pen: GPPEN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePen2}

GdipClonePen: function(pen: GPPEN; out clonepen: GPPEN): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipClonePen}

GdipDeletePen: function(pen: GPPEN): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeletePen}

GdipSetPenWidth: function(pen: GPPEN; width: Single): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenWidth}

GdipGetPenWidth: function(pen: GPPEN; out width: Single): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetPenWidth}

GdipSetPenUnit: function(pen: GPPEN; unit_: GPUNIT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenUnit}

GdipGetPenUnit: function(pen: GPPEN; var unit_: GPUNIT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetPenUnit}

GdipSetPenLineCap197819: function(pen: GPPEN; startCap: GPLINECAP;  
    endCap: GPLINECAP; dashCap: GPDASHCAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenLineCap197819}

GdipSetPenStartCap: function(pen: GPPEN;  
    startCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenStartCap}

GdipSetPenEndCap: function(pen: GPPEN; endCap: GPLINECAP): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenEndCap}

GdipSetPenDashCap197819: function(pen: GPPEN;  
    dashCap: GPDASHCAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenDashCap197819}

GdipGetPenStartCap: function(pen: GPPEN;  
    out startCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenStartCap}

GdipGetPenEndCap: function(pen: GPPEN;  
    out endCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenEndCap}

GdipGetPenDashCap197819: function(pen: GPPEN;  
    out dashCap: GPDASHCAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenDashCap197819}

GdipSetPenLineJoin: function(pen: GPPEN;  
    lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenLineJoin}

GdipGetPenLineJoin: function(pen: GPPEN;  
    var lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenLineJoin}

GdipSetPenCustomStartCap: function(pen: GPPEN;  
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenCustomStartCap}

GdipGetPenCustomStartCap: function(pen: GPPEN;  
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenCustomStartCap}

GdipSetPenCustomEndCap: function(pen: GPPEN;  
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenCustomEndCap}

GdipGetPenCustomEndCap: function(pen: GPPEN;  
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenCustomEndCap}

GdipSetPenMiterLimit: function(pen: GPPEN;  
    miterLimit: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenMiterLimit}

GdipGetPenMiterLimit: function(pen: GPPEN;  
    out miterLimit: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenMiterLimit}

GdipSetPenMode: function(pen: GPPEN;  
    penMode: GPPENALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenMode}

GdipGetPenMode: function(pen: GPPEN;  
    var penMode: GPPENALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenMode}

GdipSetPenTransform: function(pen: GPPEN;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenTransform}

GdipGetPenTransform: function(pen: GPPEN;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenTransform}

GdipResetPenTransform: function(pen: GPPEN): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetPenTransform}

GdipMultiplyPenTransform: function(pen: GPPEN; matrix: GPMATRIX;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyPenTransform}

GdipTranslatePenTransform: function(pen: GPPEN; dx: Single; dy: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslatePenTransform}

GdipScalePenTransform: function(pen: GPPEN; sx: Single; sy: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScalePenTransform}

GdipRotatePenTransform: function(pen: GPPEN; angle: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotatePenTransform}

GdipSetPenColor: function(pen: GPPEN; argb: ARGB): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenColor}

GdipGetPenColor: function(pen: GPPEN; out argb: ARGB): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetPenColor}

GdipSetPenBrushFill: function(pen: GPPEN; brush: GPBRUSH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenBrushFill}

GdipGetPenBrushFill: function(pen: GPPEN;  
    out brush: GPBRUSH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenBrushFill}

GdipGetPenFillType: function(pen: GPPEN;  
    out type_: GPPENTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenFillType}

GdipGetPenDashStyle: function(pen: GPPEN;  
    out dashstyle: GPDASHSTYLE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenDashStyle}

GdipSetPenDashStyle: function(pen: GPPEN;  
    dashstyle: GPDASHSTYLE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenDashStyle}

GdipGetPenDashOffset: function(pen: GPPEN;  
    out offset: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenDashOffset}

GdipSetPenDashOffset: function(pen: GPPEN; offset: Single): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenDashOffset}

GdipGetPenDashCount: function(pen: GPPEN;  
    var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenDashCount}

GdipSetPenDashArray: function(pen: GPPEN; dash: PSingle;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenDashArray}

GdipGetPenDashArray: function(pen: GPPEN; dash: PSingle;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenDashArray}

GdipGetPenCompoundCount: function(pen: GPPEN;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenCompoundCount}

GdipSetPenCompoundArray: function(pen: GPPEN; dash: PSingle;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenCompoundArray}

GdipGetPenCompoundArray: function(pen: GPPEN; dash: PSingle;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenCompoundArray}

//----------------------------------------------------------------------------
// CustomLineCap APIs
//----------------------------------------------------------------------------

GdipCreateCustomLineCap: function(fillPath: GPPATH; strokePath: GPPATH;  
    baseCap: GPLINECAP; baseInset: Single;
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateCustomLineCap}

GdipDeleteCustomLineCap: function(  
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDeleteCustomLineCap}

GdipCloneCustomLineCap: function(customCap: GPCUSTOMLINECAP;  
    out clonedCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneCustomLineCap}

GdipGetCustomLineCapType: function(customCap: GPCUSTOMLINECAP;  
    var capType: CUSTOMLINECAPTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapType}

GdipSetCustomLineCapStrokeCaps: function(customCap: GPCUSTOMLINECAP;  
    startCap: GPLINECAP; endCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCustomLineCapStrokeCaps}

GdipGetCustomLineCapStrokeCaps: function(customCap: GPCUSTOMLINECAP;  
    var startCap: GPLINECAP; var endCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapStrokeCaps}

GdipSetCustomLineCapStrokeJoin: function(customCap: GPCUSTOMLINECAP;  
  lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCustomLineCapStrokeJoin}

GdipGetCustomLineCapStrokeJoin: function(customCap: GPCUSTOMLINECAP;  
  var lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapStrokeJoin}

GdipSetCustomLineCapBaseCap: function(customCap: GPCUSTOMLINECAP;  
  baseCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCustomLineCapBaseCap}

GdipGetCustomLineCapBaseCap: function(customCap: GPCUSTOMLINECAP;  
  var baseCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapBaseCap}

GdipSetCustomLineCapBaseInset: function(customCap: GPCUSTOMLINECAP;  
  inset: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCustomLineCapBaseInset}

GdipGetCustomLineCapBaseInset: function(customCap: GPCUSTOMLINECAP;  
  var inset: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapBaseInset}

GdipSetCustomLineCapWidthScale: function(customCap: GPCUSTOMLINECAP;  
  widthScale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCustomLineCapWidthScale}

GdipGetCustomLineCapWidthScale: function(customCap: GPCUSTOMLINECAP;  
  var widthScale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapWidthScale}

//----------------------------------------------------------------------------
// AdjustableArrowCap APIs
//----------------------------------------------------------------------------

GdipCreateAdjustableArrowCap: function(height: Single;  
  width: Single;
  isFilled: Bool;
  out cap: GPADJUSTABLEARROWCAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateAdjustableArrowCap}

GdipSetAdjustableArrowCapHeight: function(cap: GPADJUSTABLEARROWCAP;  
  height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetAdjustableArrowCapHeight}

GdipGetAdjustableArrowCapHeight: function(cap: GPADJUSTABLEARROWCAP;  
  var height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetAdjustableArrowCapHeight}

GdipSetAdjustableArrowCapWidth: function(cap: GPADJUSTABLEARROWCAP;  
  width: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetAdjustableArrowCapWidth}

GdipGetAdjustableArrowCapWidth: function(cap: GPADJUSTABLEARROWCAP;  
  var width: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetAdjustableArrowCapWidth}

GdipSetAdjustableArrowCapMiddleInset: function(cap: GPADJUSTABLEARROWCAP;  
  middleInset: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetAdjustableArrowCapMiddleInset}

GdipGetAdjustableArrowCapMiddleInset: function(cap: GPADJUSTABLEARROWCAP;  
  var middleInset: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetAdjustableArrowCapMiddleInset}

GdipSetAdjustableArrowCapFillState: function(cap: GPADJUSTABLEARROWCAP;  
  fillState: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetAdjustableArrowCapFillState}

GdipGetAdjustableArrowCapFillState: function(cap: GPADJUSTABLEARROWCAP;  
  var fillState: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetAdjustableArrowCapFillState}

//---------------------------------------------------------------------------- 
// Image APIs
//----------------------------------------------------------------------------

GdipLoadImageFromStream: function(stream: ISTREAM;  
  out image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipLoadImageFromStream}

GdipLoadImageFromFile: function(filename: PWCHAR;  
  out image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipLoadImageFromFile}

GdipLoadImageFromStreamICM: function(stream: ISTREAM;  
  out image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipLoadImageFromStreamICM}

GdipLoadImageFromFileICM: function(filename: PWCHAR;  
  out image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipLoadImageFromFileICM}

GdipCloneImage: function(image: GPIMAGE;  
  out cloneImage: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneImage}

GdipDisposeImage: function(image: GPIMAGE): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDisposeImage}

GdipSaveImageToFile: function(image: GPIMAGE;  
  filename: PWCHAR;
  clsidEncoder: PGUID;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSaveImageToFile}

GdipSaveImageToStream: function(image: GPIMAGE;  
  stream: ISTREAM;
  clsidEncoder: PGUID;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSaveImageToStream}

GdipSaveAdd: function(image: GPIMAGE;  
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSaveAdd}

GdipSaveAddImage: function(image: GPIMAGE;  
  newImage: GPIMAGE;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSaveAddImage}

GdipGetImageGraphicsContext: function(image: GPIMAGE;  
  out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageGraphicsContext}

GdipGetImageBounds: function(image: GPIMAGE;  
  srcRect: GPRECTF;
  var srcUnit: GPUNIT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageBounds}

GdipGetImageDimension: function(image: GPIMAGE;  
  var width: Single;
  var height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageDimension}

GdipGetImageType: function(image: GPIMAGE;  
  var type_: IMAGETYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageType}

GdipGetImageWidth: function(image: GPIMAGE;  
  var width: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageWidth}

GdipGetImageHeight: function(image: GPIMAGE;  
  var height: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageHeight}

GdipGetImageHorizontalResolution: function(image: GPIMAGE;  
  var resolution: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageHorizontalResolution}

GdipGetImageVerticalResolution: function(image: GPIMAGE;  
  var resolution: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageVerticalResolution}

GdipGetImageFlags: function(image: GPIMAGE;  
  var flags: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageFlags}

GdipGetImageRawFormat: function(image: GPIMAGE;  
  format: PGUID): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageRawFormat}

GdipGetImagePixelFormat: function(image: GPIMAGE;  
  out format: TPIXELFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImagePixelFormat}

GdipGetImageThumbnail: function(image: GPIMAGE; thumbWidth: UINT;  
    thumbHeight: UINT; out thumbImage: GPIMAGE;
    callback: GETTHUMBNAILIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageThumbnail}

GdipGetEncoderParameterListSize: function(image: GPIMAGE;  
    clsidEncoder: PGUID; out size: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetEncoderParameterListSize}

GdipGetEncoderParameterList: function(image: GPIMAGE; clsidEncoder: PGUID;  
    size: UINT; buffer: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetEncoderParameterList}

GdipImageGetFrameDimensionsCount: function(image: GPIMAGE;  
    var count: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageGetFrameDimensionsCount}

GdipImageGetFrameDimensionsList: function(image: GPIMAGE; dimensionIDs: PGUID;  
    count: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageGetFrameDimensionsList}

GdipImageGetFrameCount: function(image: GPIMAGE; dimensionID: PGUID;  
    var count: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageGetFrameCount}

GdipImageSelectActiveFrame: function(image: GPIMAGE; dimensionID: PGUID;  
    frameIndex: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageSelectActiveFrame}

GdipImageRotateFlip: function(image: GPIMAGE;  
    rfType: ROTATEFLIPTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageRotateFlip}

GdipGetImagePalette: function(image: GPIMAGE; palette: PCOLORPALETTE;  
    size: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImagePalette}

GdipSetImagePalette: function(image: GPIMAGE;  
    palette: PCOLORPALETTE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImagePalette}

GdipGetImagePaletteSize: function(image: GPIMAGE;  
    var size: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImagePaletteSize}

GdipGetPropertyCount: function(image: GPIMAGE;  
    var numOfProperty: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPropertyCount}

GdipGetPropertyIdList: function(image: GPIMAGE; numOfProperty: UINT;  
    list: PPROPID): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPropertyIdList}

GdipGetPropertyItemSize: function(image: GPIMAGE; propId: PROPID;  
    var size: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPropertyItemSize}

GdipGetPropertyItem: function(image: GPIMAGE; propId: PROPID; propSize: UINT;  
    buffer: PPROPERTYITEM): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPropertyItem}

GdipGetPropertySize: function(image: GPIMAGE; var totalBufferSize: UINT;  
    var numProperties: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPropertySize}

GdipGetAllPropertyItems: function(image: GPIMAGE; totalBufferSize: UINT;  
    numProperties: UINT; allItems: PPROPERTYITEM): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetAllPropertyItems}

GdipRemovePropertyItem: function(image: GPIMAGE;  
    propId: PROPID): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRemovePropertyItem}

GdipSetPropertyItem: function(image: GPIMAGE;  
    item: PPROPERTYITEM): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPropertyItem}

GdipImageForceValidation: function(image: GPIMAGE): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipImageForceValidation}

//---------------------------------------------------------------------------- 
// Bitmap APIs
//----------------------------------------------------------------------------

GdipCreateBitmapFromStream: function(stream: ISTREAM;  
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromStream}

GdipCreateBitmapFromFile: function(filename: PWCHAR;  
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromFile}

GdipCreateBitmapFromStreamICM: function(stream: ISTREAM;  
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromStreamICM}

GdipCreateBitmapFromFileICM: function(filename: PWCHAR;  
    var bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromFileICM}

GdipCreateBitmapFromScan0: function(width: Integer; height: Integer;  
    stride: Integer; format: PIXELFORMAT; scan0: PBYTE;
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromScan0}

GdipCreateBitmapFromGraphics: function(width: Integer; height: Integer;  
    target: GPGRAPHICS; out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromGraphics}

GdipCreateBitmapFromGdiDib: function(gdiBitmapInfo: PBitmapInfo;  
    gdiBitmapData: Pointer; out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromGdiDib}

GdipCreateBitmapFromHBITMAP: function(hbm: HBITMAP; hpal: HPALETTE;  
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromHBITMAP}

GdipCreateHBITMAPFromBitmap: function(bitmap: GPBITMAP; out hbmReturn: HBITMAP;  
    background: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateHBITMAPFromBitmap}

GdipCreateBitmapFromHICON: function(hicon: HICON;  
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromHICON}

GdipCreateHICONFromBitmap: function(bitmap: GPBITMAP;  
    out hbmReturn: HICON): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateHICONFromBitmap}

GdipCreateBitmapFromResource: function(hInstance: HMODULE;  
    lpBitmapName: PWCHAR; out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromResource}

GdipCloneBitmapArea: function(x: Single; y: Single; width: Single;  
    height: Single; format: PIXELFORMAT; srcBitmap: GPBITMAP;
    out dstBitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneBitmapArea}

GdipCloneBitmapAreaI: function(x: Integer; y: Integer; width: Integer;  
    height: Integer; format: PIXELFORMAT; srcBitmap: GPBITMAP;
    out dstBitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneBitmapAreaI}

GdipBitmapLockBits: function(bitmap: GPBITMAP; rect: GPRECT; flags: UINT;  
    format: PIXELFORMAT; lockedBitmapData: PBITMAPDATA): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBitmapLockBits}

GdipBitmapUnlockBits: function(bitmap: GPBITMAP;  
    lockedBitmapData: PBITMAPDATA): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBitmapUnlockBits}

GdipBitmapGetPixel: function(bitmap: GPBITMAP; x: Integer; y: Integer;  
    var color: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBitmapGetPixel}

GdipBitmapSetPixel: function(bitmap: GPBITMAP; x: Integer; y: Integer;  
    color: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBitmapSetPixel}

GdipBitmapSetResolution: function(bitmap: GPBITMAP; xdpi: Single;  
    ydpi: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBitmapSetResolution}

//----------------------------------------------------------------------------
// ImageAttributes APIs
//----------------------------------------------------------------------------

GdipCreateImageAttributes: function(  
    out imageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateImageAttributes}

GdipCloneImageAttributes: function(imageattr: GPIMAGEATTRIBUTES;  
    out cloneImageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneImageAttributes}

GdipDisposeImageAttributes: function(  
    imageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDisposeImageAttributes}

GdipSetImageAttributesToIdentity: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesToIdentity}

GdipResetImageAttributes: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipResetImageAttributes}

GdipSetImageAttributesColorMatrix: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool; colorMatrix: PCOLORMATRIX;
    grayMatrix: PCOLORMATRIX; flags: COLORMATRIXFLAGS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesColorMatrix}

GdipSetImageAttributesThreshold: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    threshold: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesThreshold}

GdipSetImageAttributesGamma: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool; gamma: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesGamma}

GdipSetImageAttributesNoOp: function(imageattr: GPIMAGEATTRIBUTES;  
  type_: COLORADJUSTTYPE; enableFlag: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesNoOp}

GdipSetImageAttributesColorKeys: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool; colorLow: ARGB;
    colorHigh: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesColorKeys}

GdipSetImageAttributesOutputChannel: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    channelFlags: COLORCHANNELFLAGS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesOutputChannel}

GdipSetImageAttributesOutputChannelColorProfile: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    colorProfileFilename: PWCHAR): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesOutputChannelColorProfile}

GdipSetImageAttributesRemapTable: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool; mapSize: UINT;
    map: PCOLORMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesRemapTable}

GdipSetImageAttributesWrapMode: function(imageAttr: GPIMAGEATTRIBUTES;  
    wrap: WRAPMODE; argb: ARGB; clamp: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesWrapMode}

GdipSetImageAttributesICMMode: function(imageAttr: GPIMAGEATTRIBUTES;  
    on_: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesICMMode}

GdipGetImageAttributesAdjustedPalette: function(imageAttr: GPIMAGEATTRIBUTES;  
    colorPalette: PCOLORPALETTE;
    colorAdjustType: COLORADJUSTTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageAttributesAdjustedPalette}

//----------------------------------------------------------------------------
// Graphics APIs
//----------------------------------------------------------------------------

GdipFlush: function(graphics: GPGRAPHICS;  
    intention: GPFLUSHINTENTION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFlush}

GdipCreateFromHDC: function(hdc: HDC;  
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFromHDC}

GdipCreateFromHDC2: function(hdc: HDC; hDevice: THandle;  
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFromHDC2}

GdipCreateFromHWND: function(hwnd: HWND;  
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFromHWND}

GdipCreateFromHWNDICM: function(hwnd: HWND;  
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFromHWNDICM}

GdipDeleteGraphics: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteGraphics}

GdipGetDC: function(graphics: GPGRAPHICS; var hdc: HDC): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetDC}

GdipReleaseDC: function(graphics: GPGRAPHICS; hdc: HDC): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipReleaseDC}

GdipSetCompositingMode: function(graphics: GPGRAPHICS;  
    compositingMode: COMPOSITINGMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCompositingMode}

GdipGetCompositingMode: function(graphics: GPGRAPHICS;  
    var compositingMode: COMPOSITINGMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCompositingMode}

GdipSetRenderingOrigin: function(graphics: GPGRAPHICS; x: Integer;  
    y: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetRenderingOrigin}

GdipGetRenderingOrigin: function(graphics: GPGRAPHICS; var x: Integer;  
    var y: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRenderingOrigin}

GdipSetCompositingQuality: function(graphics: GPGRAPHICS;  
    compositingQuality: COMPOSITINGQUALITY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCompositingQuality}

GdipGetCompositingQuality: function(graphics: GPGRAPHICS;  
    var compositingQuality: COMPOSITINGQUALITY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCompositingQuality}

GdipSetSmoothingMode: function(graphics: GPGRAPHICS;  
    smoothingMode: SMOOTHINGMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetSmoothingMode}

GdipGetSmoothingMode: function(graphics: GPGRAPHICS;  
    var smoothingMode: SMOOTHINGMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetSmoothingMode}

GdipSetPixelOffsetMode: function(graphics: GPGRAPHICS;  
    pixelOffsetMode: PIXELOFFSETMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPixelOffsetMode}

GdipGetPixelOffsetMode: function(graphics: GPGRAPHICS;  
    var pixelOffsetMode: PIXELOFFSETMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPixelOffsetMode}

GdipSetTextRenderingHint: function(graphics: GPGRAPHICS;  
    mode: TEXTRENDERINGHINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetTextRenderingHint}

GdipGetTextRenderingHint: function(graphics: GPGRAPHICS;  
    var mode: TEXTRENDERINGHINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetTextRenderingHint}

GdipSetTextContrast: function(graphics: GPGRAPHICS;  
    contrast: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetTextContrast}

GdipGetTextContrast: function(graphics: GPGRAPHICS;  
    var contrast: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetTextContrast}

GdipSetInterpolationMode: function(graphics: GPGRAPHICS;  
    interpolationMode: INTERPOLATIONMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetInterpolationMode}

GdipGetInterpolationMode: function(graphics: GPGRAPHICS;  
    var interpolationMode: INTERPOLATIONMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetInterpolationMode}

GdipSetWorldTransform: function(graphics: GPGRAPHICS;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetWorldTransform}

GdipResetWorldTransform: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetWorldTransform}

GdipMultiplyWorldTransform: function(graphics: GPGRAPHICS; matrix: GPMATRIX;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyWorldTransform}

GdipTranslateWorldTransform: function(graphics: GPGRAPHICS; dx: Single;  
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateWorldTransform}

GdipScaleWorldTransform: function(graphics: GPGRAPHICS; sx: Single; sy: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScaleWorldTransform}

GdipRotateWorldTransform: function(graphics: GPGRAPHICS; angle: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotateWorldTransform}

GdipGetWorldTransform: function(graphics: GPGRAPHICS;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetWorldTransform}

GdipResetPageTransform: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetPageTransform}

GdipGetPageUnit: function(graphics: GPGRAPHICS;  
    var unit_: GPUNIT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPageUnit}

GdipGetPageScale: function(graphics: GPGRAPHICS;  
    var scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPageScale}

GdipSetPageUnit: function(graphics: GPGRAPHICS;  
    unit_: GPUNIT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPageUnit}

GdipSetPageScale: function(graphics: GPGRAPHICS;  
    scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPageScale}

GdipGetDpiX: function(graphics: GPGRAPHICS;  
    var dpi: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetDpiX}

GdipGetDpiY: function(graphics: GPGRAPHICS;  
    var dpi: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetDpiY}

GdipTransformPoints: function(graphics: GPGRAPHICS;  
    destSpace: GPCOORDINATESPACE; srcSpace: GPCOORDINATESPACE;
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTransformPoints}

GdipTransformPointsI: function(graphics: GPGRAPHICS;  
    destSpace: GPCOORDINATESPACE; srcSpace: GPCOORDINATESPACE;
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTransformPointsI}

GdipGetNearestColor: function(graphics: GPGRAPHICS;  
    argb: PARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetNearestColor}

// Creates the Win9x Halftone Palette (even on NT) with correct Desktop colors

GdipCreateHalftonePalette: function: HPALETTE; stdcall;
  {.$EXTERNALSYM GdipCreateHalftonePalette}

GdipDrawLine: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Single;  
    y1: Single; x2: Single; y2: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawLine}

GdipDrawLineI: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Integer;  
    y1: Integer; x2: Integer; y2: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawLineI}

GdipDrawLines: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawLines}

GdipDrawLinesI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawLinesI}

GdipDrawArc: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single; y: Single;  
    width: Single; height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawArc}

GdipDrawArcI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;  
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawArcI}

GdipDrawBezier: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Single;  
    y1: Single; x2: Single; y2: Single; x3: Single; y3: Single; x4: Single;
    y4: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawBezier}

GdipDrawBezierI: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Integer;  
    y1: Integer; x2: Integer; y2: Integer; x3: Integer; y3: Integer;
    x4: Integer; y4: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawBezierI}

GdipDrawBeziers: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawBeziers}

GdipDrawBeziersI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawBeziersI}

GdipDrawRectangle: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single;  
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawRectangle}

GdipDrawRectangleI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;  
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawRectangleI}

GdipDrawRectangles: function(graphics: GPGRAPHICS; pen: GPPEN; rects: GPRECTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawRectangles}

GdipDrawRectanglesI: function(graphics: GPGRAPHICS; pen: GPPEN; rects: GPRECT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawRectanglesI}

GdipDrawEllipse: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single;  
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawEllipse}

GdipDrawEllipseI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;  
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawEllipseI}

GdipDrawPie: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single; y: Single;  
    width: Single;  height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawPie}

GdipDrawPieI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;  
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawPieI}

GdipDrawPolygon: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawPolygon}

GdipDrawPolygonI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawPolygonI}

GdipDrawPath: function(graphics: GPGRAPHICS; pen: GPPEN;  
    path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawPath}

GdipDrawCurve: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurve}

GdipDrawCurveI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurveI}

GdipDrawCurve2: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurve2}

GdipDrawCurve2I: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurve2I}

GdipDrawCurve3: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer; offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurve3}

GdipDrawCurve3I: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer; offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurve3I}

GdipDrawClosedCurve: function(graphics: GPGRAPHICS; pen: GPPEN;  
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawClosedCurve}

GdipDrawClosedCurveI: function(graphics: GPGRAPHICS; pen: GPPEN;  
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawClosedCurveI}

GdipDrawClosedCurve2: function(graphics: GPGRAPHICS; pen: GPPEN;  
    points: GPPOINTF; count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawClosedCurve2}

GdipDrawClosedCurve2I: function(graphics: GPGRAPHICS; pen: GPPEN;  
    points: GPPOINT; count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawClosedCurve2I}

GdipGraphicsClear: function(graphics: GPGRAPHICS;  
    color: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGraphicsClear}

GdipFillRectangle: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;  
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillRectangle}

GdipFillRectangleI: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;  
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillRectangleI}

GdipFillRectangles: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    rects: GPRECTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillRectangles}

GdipFillRectanglesI: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    rects: GPRECT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillRectanglesI}

GdipFillPolygon: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINTF; count: Integer; fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPolygon}

GdipFillPolygonI: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINT; count: Integer; fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPolygonI}

GdipFillPolygon2: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPolygon2}

GdipFillPolygon2I: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPolygon2I}

GdipFillEllipse: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;  
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillEllipse}

GdipFillEllipseI: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;  
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillEllipseI}

GdipFillPie: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;  
    y: Single; width: Single; height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPie}

GdipFillPieI: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;  
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPieI}

GdipFillPath: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPath}

GdipFillClosedCurve: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillClosedCurve}

GdipFillClosedCurveI: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillClosedCurveI}

GdipFillClosedCurve2: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINTF; count: Integer; tension: Single;
    fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillClosedCurve2}

GdipFillClosedCurve2I: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINT; count: Integer; tension: Single;
    fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillClosedCurve2I}

GdipFillRegion: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillRegion}

GdipDrawImage: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Single;  
    y: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImage}

GdipDrawImageI: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Integer;  
    y: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImageI}

GdipDrawImageRect: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Single;  
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImageRect}

GdipDrawImageRectI: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Integer;  
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImageRectI}

GdipDrawImagePoints: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    dstpoints: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePoints}

GdipDrawImagePointsI: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    dstpoints: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePointsI}

GdipDrawImagePointRect: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    x: Single; y: Single; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; srcUnit: GPUNIT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePointRect}

GdipDrawImagePointRectI: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    x: Integer; y: Integer; srcx: Integer; srcy: Integer; srcwidth: Integer;
    srcheight: Integer; srcUnit: GPUNIT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePointRectI}

GdipDrawImageRectRect: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    dstx: Single; dsty: Single; dstwidth: Single; dstheight: Single;
    srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single;
    srcUnit: GPUNIT; imageAttributes: GPIMAGEATTRIBUTES;
    callback: DRAWIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImageRectRect}

GdipDrawImageRectRectI: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    dstx: Integer; dsty: Integer; dstwidth: Integer; dstheight: Integer;
    srcx: Integer; srcy: Integer; srcwidth: Integer; srcheight: Integer;
    srcUnit: GPUNIT; imageAttributes: GPIMAGEATTRIBUTES;
    callback: DRAWIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImageRectRectI}

GdipDrawImagePointsRect: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    points: GPPOINTF; count: Integer; srcx: Single; srcy: Single;
    srcwidth: Single; srcheight: Single; srcUnit: GPUNIT;
    imageAttributes: GPIMAGEATTRIBUTES; callback: DRAWIMAGEABORT;
    callbackData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePointsRect}

GdipDrawImagePointsRectI: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    points: GPPOINT; count: Integer; srcx: Integer; srcy: Integer;
    srcwidth: Integer; srcheight: Integer; srcUnit: GPUNIT;
    imageAttributes: GPIMAGEATTRIBUTES; callback: DRAWIMAGEABORT;
    callbackData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePointsRectI}

GdipSetClipGraphics: function(graphics: GPGRAPHICS; srcgraphics: GPGRAPHICS;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipGraphics}

GdipSetClipRect: function(graphics: GPGRAPHICS; x: Single; y: Single;  
    width: Single; height: Single; combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipRect}

GdipSetClipRectI: function(graphics: GPGRAPHICS; x: Integer; y: Integer;  
    width: Integer; height: Integer;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipRectI}

GdipSetClipPath: function(graphics: GPGRAPHICS; path: GPPATH;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipPath}

GdipSetClipRegion: function(graphics: GPGRAPHICS; region: GPREGION;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipRegion}

GdipSetClipHrgn: function(graphics: GPGRAPHICS; hRgn: HRGN;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipHrgn}

GdipResetClip: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetClip}

GdipTranslateClip: function(graphics: GPGRAPHICS; dx: Single;  
    dy: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateClip}

GdipTranslateClipI: function(graphics: GPGRAPHICS; dx: Integer;  
    dy: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateClipI}

GdipGetClip: function(graphics: GPGRAPHICS;  
    region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetClip}

GdipGetClipBounds: function(graphics: GPGRAPHICS;  
    rect: GPRECTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetClipBounds}

GdipGetClipBoundsI: function(graphics: GPGRAPHICS;  
    rect: GPRECT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetClipBoundsI}

GdipIsClipEmpty: function(graphics: GPGRAPHICS;  
    result: PBool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsClipEmpty}

GdipGetVisibleClipBounds: function(graphics: GPGRAPHICS;  
    rect: GPRECTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetVisibleClipBounds}

GdipGetVisibleClipBoundsI: function(graphics: GPGRAPHICS;  
    rect: GPRECT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetVisibleClipBoundsI}

GdipIsVisibleClipEmpty: function(graphics: GPGRAPHICS;  
    var result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleClipEmpty}

GdipIsVisiblePoint: function(graphics: GPGRAPHICS; x: Single; y: Single;  
    var result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisiblePoint}

GdipIsVisiblePointI: function(graphics: GPGRAPHICS; x: Integer; y: Integer;  
    var result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisiblePointI}

GdipIsVisibleRect: function(graphics: GPGRAPHICS; x: Single; y: Single;  
    width: Single; height: Single; var result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRect}

GdipIsVisibleRectI: function(graphics: GPGRAPHICS; x: Integer; y: Integer;  
    width: Integer; height: Integer; var result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRectI}

GdipSaveGraphics: function(graphics: GPGRAPHICS;  
    var state: GRAPHICSSTATE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSaveGraphics}

GdipRestoreGraphics: function(graphics: GPGRAPHICS;  
    state: GRAPHICSSTATE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRestoreGraphics}

GdipBeginContainer: function(graphics: GPGRAPHICS; dstrect: GPRECTF;  
    srcrect: GPRECTF; unit_: GPUNIT;
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBeginContainer}

GdipBeginContainerI: function(graphics: GPGRAPHICS; dstrect: GPRECT;  
    srcrect: GPRECT; unit_: GPUNIT;
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBeginContainerI}

GdipBeginContainer2: function(graphics: GPGRAPHICS;  
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBeginContainer2}

GdipEndContainer: function(graphics: GPGRAPHICS;  
    state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEndContainer}

GdipGetMetafileHeaderFromWmf: function(hWmf: HMETAFILE;  
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    header: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileHeaderFromWmf}

GdipGetMetafileHeaderFromEmf: function(hEmf: HENHMETAFILE;  
    header: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileHeaderFromEmf}

GdipGetMetafileHeaderFromFile: function(filename: PWCHAR;  
    header: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileHeaderFromFile}

GdipGetMetafileHeaderFromStream: function(stream: ISTREAM;  
    header: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileHeaderFromStream}

GdipGetMetafileHeaderFromMetafile: function(metafile: GPMETAFILE;  
    header: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileHeaderFromMetafile}

GdipGetHemfFromMetafile: function(metafile: GPMETAFILE;  
    var hEmf: HENHMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetHemfFromMetafile}

GdipCreateStreamOnFile: function(filename: PWCHAR; access: UINT;  
    out stream: ISTREAM): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateStreamOnFile}

GdipCreateMetafileFromWmf: function(hWmf: HMETAFILE; deleteWmf: Bool;  
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMetafileFromWmf}

GdipCreateMetafileFromEmf: function(hEmf: HENHMETAFILE; deleteEmf: Bool;  
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMetafileFromEmf}

GdipCreateMetafileFromFile: function(file_: PWCHAR;  
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMetafileFromFile}

GdipCreateMetafileFromWmfFile: function(file_: PWCHAR;  
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMetafileFromWmfFile}

GdipCreateMetafileFromStream: function(stream: ISTREAM;  
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMetafileFromStream}

GdipRecordMetafile: function(referenceHdc: HDC; type_: EMFTYPE;  
    frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafile}

GdipRecordMetafileI: function(referenceHdc: HDC; type_: EMFTYPE;  
    frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT; description: PWCHAR;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafileI}

GdipRecordMetafileFileName: function(fileName: PWCHAR; referenceHdc: HDC;  
    type_: EMFTYPE; frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafileFileName}

GdipRecordMetafileFileNameI: function(fileName: PWCHAR; referenceHdc: HDC;  
    type_: EMFTYPE; frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafileFileNameI}

GdipRecordMetafileStream: function(stream: ISTREAM; referenceHdc: HDC;  
    type_: EMFTYPE; frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafileStream}

GdipRecordMetafileStreamI: function(stream: ISTREAM; referenceHdc: HDC;  
    type_: EMFTYPE; frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafileStreamI}

GdipSetMetafileDownLevelRasterizationLimit: function(metafile: GPMETAFILE;  
    metafileRasterizationLimitDpi: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetMetafileDownLevelRasterizationLimit}

GdipGetMetafileDownLevelRasterizationLimit: function(metafile: GPMETAFILE;  
    var metafileRasterizationLimitDpi: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileDownLevelRasterizationLimit}

GdipGetImageDecodersSize: function(out numDecoders: UINT;  
    out size: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageDecodersSize}

GdipGetImageDecoders: function(numDecoders: UINT; size: UINT;  
    decoders: PIMAGECODECINFO): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageDecoders}

GdipGetImageEncodersSize: function(out numEncoders: UINT;  
    out size: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageEncodersSize}

GdipGetImageEncoders: function(numEncoders: UINT; size: UINT;  
    encoders: PIMAGECODECINFO): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageEncoders}

GdipComment: function(graphics: GPGRAPHICS; sizeData: UINT;  
    data: PBYTE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipComment}

//----------------------------------------------------------------------------
// FontFamily APIs
//----------------------------------------------------------------------------

GdipCreateFontFamilyFromName: function(name: PWCHAR;  
    fontCollection: GPFONTCOLLECTION;
    out FontFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFontFamilyFromName}

GdipDeleteFontFamily: function(FontFamily: GPFONTFAMILY): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteFontFamily}

GdipCloneFontFamily: function(FontFamily: GPFONTFAMILY;  
    out clonedFontFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneFontFamily}

GdipGetGenericFontFamilySansSerif: function(  
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetGenericFontFamilySansSerif}

GdipGetGenericFontFamilySerif: function(  
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetGenericFontFamilySerif}

GdipGetGenericFontFamilyMonospace: function(  
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetGenericFontFamilyMonospace}

GdipGetFamilyName: function(family: GPFONTFAMILY; name: PWideChar;  
    language: LANGID): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFamilyName}

GdipIsStyleAvailable: function(family: GPFONTFAMILY; style: Integer;  
    var IsStyleAvailable: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsStyleAvailable}

GdipFontCollectionEnumerable: function(fontCollection: GPFONTCOLLECTION;  
    graphics: GPGRAPHICS; var numFound: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFontCollectionEnumerable}

GdipFontCollectionEnumerate: function(fontCollection: GPFONTCOLLECTION;  
    numSought: Integer; gpfamilies: array of GPFONTFAMILY;
    var numFound: Integer; graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFontCollectionEnumerate}

GdipGetEmHeight: function(family: GPFONTFAMILY; style: Integer;  
    out EmHeight: UINT16): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetEmHeight}

GdipGetCellAscent: function(family: GPFONTFAMILY; style: Integer;  
    var CellAscent: UINT16): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCellAscent}

GdipGetCellDescent: function(family: GPFONTFAMILY; style: Integer;  
    var CellDescent: UINT16): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCellDescent}

GdipGetLineSpacing: function(family: GPFONTFAMILY; style: Integer;  
    var LineSpacing: UINT16): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineSpacing}

//----------------------------------------------------------------------------
// Font APIs
//----------------------------------------------------------------------------

GdipCreateFontFromDC: function(hdc: HDC; out font: GPFONT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipCreateFontFromDC}

GdipCreateFontFromLogfontA: function(hdc: HDC; logfont: PLOGFONTA;  
    out font: GPFONT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFontFromLogfontA}

GdipCreateFontFromLogfontW: function(hdc: HDC; logfont: PLOGFONTW;  
    out font: GPFONT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFontFromLogfontW}

GdipCreateFont: function(fontFamily: GPFONTFAMILY; emSize: Single;  
    style: Integer; unit_: Integer; out font: GPFONT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFont}

GdipCloneFont: function(font: GPFONT;  
    out cloneFont: GPFONT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneFont}

GdipDeleteFont: function(font: GPFONT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteFont}

GdipGetFamily: function(font: GPFONT;  
    out family: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFamily}

GdipGetFontStyle: function(font: GPFONT;  
    var style: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFontStyle}

GdipGetFontSize: function(font: GPFONT; var size: Single): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetFontSize}

GdipGetFontUnit: function(font: GPFONT; var unit_: TUNIT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetFontUnit}

GdipGetFontHeight: function(font: GPFONT; graphics: GPGRAPHICS;  
    var height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFontHeight}

GdipGetFontHeightGivenDPI: function(font: GPFONT; dpi: Single;  
    var height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFontHeightGivenDPI}

GdipGetLogFontA: function(font: GPFONT; graphics: GPGRAPHICS;  
    var logfontA: LOGFONTA): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLogFontA}

GdipGetLogFontW: function(font: GPFONT; graphics: GPGRAPHICS;  
    var logfontW: LOGFONTW): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLogFontW}

GdipNewInstalledFontCollection: function(  
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipNewInstalledFontCollection}

GdipNewPrivateFontCollection: function(  
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipNewPrivateFontCollection}

GdipDeletePrivateFontCollection: function(  
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDeletePrivateFontCollection}

GdipGetFontCollectionFamilyCount: function(fontCollection: GPFONTCOLLECTION;  
    var numFound: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFontCollectionFamilyCount}

GdipGetFontCollectionFamilyList: function(fontCollection: GPFONTCOLLECTION;  
    numSought: Integer; gpfamilies: GPFONTFAMILY;
    var numFound: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFontCollectionFamilyList}

GdipPrivateAddFontFile: function(fontCollection: GPFONTCOLLECTION;  
    filename: PWCHAR): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPrivateAddFontFile}

GdipPrivateAddMemoryFont: function(fontCollection: GPFONTCOLLECTION;  
    memory: Pointer; length: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPrivateAddMemoryFont}

//----------------------------------------------------------------------------
// Text APIs
//----------------------------------------------------------------------------

GdipDrawString: function(graphics: GPGRAPHICS; string_: PWCHAR;  
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GPSTRINGFORMAT; brush: GPBRUSH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawString}

GdipMeasureString: function(graphics: GPGRAPHICS; string_: PWCHAR;  
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GPSTRINGFORMAT; boundingBox: PGPRectF;
    codepointsFitted: PInteger; linesFilled: PInteger): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMeasureString}

GdipMeasureCharacterRanges: function(graphics: GPGRAPHICS; string_: PWCHAR;  
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GPSTRINGFORMAT; regionCount: Integer;
    const regions: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMeasureCharacterRanges}

GdipDrawDriverString: function(graphics: GPGRAPHICS; const text: PUINT16;  
    length: Integer; const font: GPFONT; const brush: GPBRUSH;
    const positions: PGPPointF; flags: Integer;
    const matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawDriverString}

GdipMeasureDriverString: function(graphics: GPGRAPHICS; text: PUINT16;  
    length: Integer; font: GPFONT; positions: PGPPointF; flags: Integer;
    matrix: GPMATRIX; boundingBox: PGPRectF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMeasureDriverString}

//----------------------------------------------------------------------------
// String format APIs
//----------------------------------------------------------------------------

GdipCreateStringFormat: function(formatAttributes: Integer; language: LANGID;  
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateStringFormat}

GdipStringFormatGetGenericDefault: function(  
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipStringFormatGetGenericDefault}

GdipStringFormatGetGenericTypographic: function(  
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipStringFormatGetGenericTypographic}

GdipDeleteStringFormat: function(format: GPSTRINGFORMAT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteStringFormat}

GdipCloneStringFormat: function(format: GPSTRINGFORMAT;  
    out newFormat: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneStringFormat}

GdipSetStringFormatFlags: function(format: GPSTRINGFORMAT;  
    flags: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatFlags}

GdipGetStringFormatFlags: function(format: GPSTRINGFORMAT;  
    out flags: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatFlags}

GdipSetStringFormatAlign: function(format: GPSTRINGFORMAT;  
    align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatAlign}

GdipGetStringFormatAlign: function(format: GPSTRINGFORMAT;  
    out align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatAlign}

GdipSetStringFormatLineAlign: function(format: GPSTRINGFORMAT;  
    align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatLineAlign}

GdipGetStringFormatLineAlign: function(format: GPSTRINGFORMAT;  
    out align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatLineAlign}

GdipSetStringFormatTrimming: function(format: GPSTRINGFORMAT;  
    trimming: STRINGTRIMMING): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatTrimming}

GdipGetStringFormatTrimming: function(format: GPSTRINGFORMAT;  
    out trimming: STRINGTRIMMING): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatTrimming}

GdipSetStringFormatHotkeyPrefix: function(format: GPSTRINGFORMAT;  
    hotkeyPrefix: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatHotkeyPrefix}

GdipGetStringFormatHotkeyPrefix: function(format: GPSTRINGFORMAT;  
    out hotkeyPrefix: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatHotkeyPrefix}

GdipSetStringFormatTabStops: function(format: GPSTRINGFORMAT;  
    firstTabOffset: Single; count: Integer;
    tabStops: PSingle): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatTabStops}

GdipGetStringFormatTabStops: function(format: GPSTRINGFORMAT;  
    count: Integer; firstTabOffset: PSingle;
    tabStops: PSingle): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatTabStops}

GdipGetStringFormatTabStopCount: function(format: GPSTRINGFORMAT;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatTabStopCount}

GdipSetStringFormatDigitSubstitution: function(format: GPSTRINGFORMAT;  
    language: LANGID;
    substitute: STRINGDIGITSUBSTITUTE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatDigitSubstitution}

GdipGetStringFormatDigitSubstitution: function(format: GPSTRINGFORMAT;  
    language: PUINT; substitute: PSTRINGDIGITSUBSTITUTE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatDigitSubstitution}

GdipGetStringFormatMeasurableCharacterRangeCount: function(format: GPSTRINGFORMAT;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatMeasurableCharacterRangeCount}

GdipSetStringFormatMeasurableCharacterRanges: function(format: GPSTRINGFORMAT;
    rangeCount: Integer; ranges: PCHARACTERRANGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatMeasurableCharacterRanges}

//----------------------------------------------------------------------------
// Cached Bitmap APIs
//----------------------------------------------------------------------------

GdipCreateCachedBitmap: function(bitmap: GPBITMAP; graphics: GPGRAPHICS;  
    out cachedBitmap: GPCACHEDBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateCachedBitmap}

GdipDeleteCachedBitmap: function(
    cachedBitmap: GPCACHEDBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDeleteCachedBitmap}

GdipDrawCachedBitmap: function(graphics: GPGRAPHICS;
    cachedBitmap: GPCACHEDBITMAP; x: Integer;
    y: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCachedBitmap}

GdipEmfToWmfBits: function(hemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
    iMapMode: Integer; eFlags: Integer): UINT; stdcall;
  {.$EXTERNALSYM GdipEmfToWmfBits}

var
  GdipLibrary: HMODULE = 0;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

{$WARNINGS OFF}

procedure LoadGdiplus;
begin
  GdipLibrary := LoadLibrary('gdiplus.dll');
  if GdipLibrary > 0 then
  begin
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAlloc{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAlloc');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFree{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFree');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdiplusStartup{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdiplusStartup');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdiplusShutdown{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdiplusShutdown');

    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreatePath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreatePath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreatePath2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreatePath2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreatePath2I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreatePath2I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipClonePath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipClonePath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeletePath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeletePath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipResetPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipResetPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPointCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPointCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathTypes{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathTypes');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathPoints{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathPoints');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathPointsI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathPointsI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathFillMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathFillMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathFillMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathFillMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathData{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathData');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipStartPathFigure{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipStartPathFigure');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipClosePathFigure{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipClosePathFigure');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipClosePathFigures{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipClosePathFigures');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathMarker{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathMarker');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipClearPathMarkers{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipClearPathMarkers');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipReversePath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipReversePath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathLastPoint{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathLastPoint');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathLine{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathLine');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathLine2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathLine2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathArc{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathArc');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathBezier{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathBezier');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathBeziers{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathBeziers');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathCurve{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathCurve');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathCurve2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathCurve2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathCurve3{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathCurve3');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathClosedCurve{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathClosedCurve');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathClosedCurve2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathClosedCurve2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathRectangle{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathRectangle');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathRectangles{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathRectangles');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathEllipse{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathEllipse');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathPie{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathPie');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathPolygon{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathPolygon');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathString{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathString');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathStringI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathStringI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathLineI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathLineI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathLine2I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathLine2I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathArcI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathArcI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathBezierI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathBezierI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathBeziersI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathBeziersI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathCurveI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathCurveI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathCurve2I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathCurve2I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathCurve3I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathCurve3I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathClosedCurveI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathClosedCurveI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathClosedCurve2I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathClosedCurve2I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathRectangleI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathRectangleI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathRectanglesI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathRectanglesI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathEllipseI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathEllipseI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathPieI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathPieI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipAddPathPolygonI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipAddPathPolygonI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFlattenPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFlattenPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipWindingModeOutline{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipWindingModeOutline');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipWidenPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipWidenPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipWarpPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipWarpPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTransformPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTransformPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathWorldBounds{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathWorldBounds');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathWorldBoundsI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathWorldBoundsI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisiblePathPoint{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisiblePathPoint');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisiblePathPointI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisiblePathPointI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsOutlineVisiblePathPoint{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsOutlineVisiblePathPoint');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsOutlineVisiblePathPointI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsOutlineVisiblePathPointI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreatePathIter{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreatePathIter');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeletePathIter{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeletePathIter');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterNextSubpath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterNextSubpath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterNextSubpathPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterNextSubpathPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterNextPathType{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterNextPathType');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterNextMarker{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterNextMarker');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterNextMarkerPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterNextMarkerPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterGetCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterGetCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterGetSubpathCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterGetSubpathCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterIsValid{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterIsValid');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterHasCurve{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterHasCurve');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterRewind{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterRewind');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterEnumerate{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterEnumerate');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPathIterCopyData{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPathIterCopyData');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateMatrix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateMatrix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateMatrix2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateMatrix2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateMatrix3{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateMatrix3');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateMatrix3I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateMatrix3I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneMatrix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneMatrix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeleteMatrix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeleteMatrix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetMatrixElements{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetMatrixElements');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipMultiplyMatrix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipMultiplyMatrix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTranslateMatrix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTranslateMatrix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipScaleMatrix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipScaleMatrix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRotateMatrix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRotateMatrix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipShearMatrix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipShearMatrix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipInvertMatrix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipInvertMatrix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTransformMatrixPoints{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTransformMatrixPoints');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTransformMatrixPointsI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTransformMatrixPointsI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipVectorTransformMatrixPoints{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipVectorTransformMatrixPoints');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipVectorTransformMatrixPointsI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipVectorTransformMatrixPointsI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetMatrixElements{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetMatrixElements');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsMatrixInvertible{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsMatrixInvertible');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsMatrixIdentity{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsMatrixIdentity');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsMatrixEqual{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsMatrixEqual');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateRegionRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateRegionRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateRegionRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateRegionRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateRegionPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateRegionPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateRegionRgnData{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateRegionRgnData');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateRegionHrgn{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateRegionHrgn');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeleteRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeleteRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetInfinite{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetInfinite');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetEmpty{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetEmpty');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCombineRegionRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCombineRegionRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCombineRegionRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCombineRegionRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCombineRegionPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCombineRegionPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCombineRegionRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCombineRegionRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTranslateRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTranslateRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTranslateRegionI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTranslateRegionI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTransformRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTransformRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetRegionBounds{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetRegionBounds');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetRegionBoundsI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetRegionBoundsI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetRegionHRgn{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetRegionHRgn');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsEmptyRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsEmptyRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsInfiniteRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsInfiniteRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsEqualRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsEqualRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetRegionDataSize{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetRegionDataSize');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetRegionData{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetRegionData');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisibleRegionPoint{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisibleRegionPoint');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisibleRegionPointI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisibleRegionPointI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisibleRegionRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisibleRegionRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisibleRegionRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisibleRegionRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetRegionScansCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetRegionScansCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetRegionScans{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetRegionScans');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetRegionScansI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetRegionScansI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneBrush{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneBrush');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeleteBrush{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeleteBrush');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetBrushType{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetBrushType');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateHatchBrush{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateHatchBrush');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetHatchStyle{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetHatchStyle');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetHatchForegroundColor{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetHatchForegroundColor');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetHatchBackgroundColor{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetHatchBackgroundColor');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateTexture{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateTexture');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateTexture2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateTexture2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateTextureIA{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateTextureIA');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateTexture2I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateTexture2I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateTextureIAI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateTextureIAI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetTextureTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetTextureTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetTextureTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetTextureTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipResetTextureTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipResetTextureTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipMultiplyTextureTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipMultiplyTextureTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTranslateTextureTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTranslateTextureTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipScaleTextureTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipScaleTextureTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRotateTextureTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRotateTextureTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetTextureWrapMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetTextureWrapMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetTextureWrapMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetTextureWrapMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetTextureImage{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetTextureImage');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateSolidFill{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateSolidFill');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetSolidFillColor{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetSolidFillColor');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetSolidFillColor{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetSolidFillColor');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateLineBrush{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateLineBrush');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateLineBrushI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateLineBrushI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateLineBrushFromRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateLineBrushFromRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateLineBrushFromRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateLineBrushFromRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateLineBrushFromRectWithAngle{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateLineBrushFromRectWithAngle');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateLineBrushFromRectWithAngleI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateLineBrushFromRectWithAngleI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetLineColors{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetLineColors');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLineColors{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLineColors');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLineRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLineRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLineRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLineRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetLineGammaCorrection{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetLineGammaCorrection');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLineGammaCorrection{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLineGammaCorrection');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLineBlendCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLineBlendCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLineBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLineBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetLineBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetLineBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLinePresetBlendCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLinePresetBlendCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLinePresetBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLinePresetBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetLinePresetBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetLinePresetBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetLineSigmaBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetLineSigmaBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetLineLinearBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetLineLinearBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetLineWrapMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetLineWrapMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLineWrapMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLineWrapMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLineTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLineTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetLineTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetLineTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipResetLineTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipResetLineTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipMultiplyLineTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipMultiplyLineTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTranslateLineTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTranslateLineTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipScaleLineTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipScaleLineTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRotateLineTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRotateLineTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreatePathGradient{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreatePathGradient');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreatePathGradientI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreatePathGradientI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreatePathGradientFromPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreatePathGradientFromPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientCenterColor{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientCenterColor');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientCenterColor{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientCenterColor');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientSurroundColorsWithCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientSurroundColorsWithCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientSurroundColorsWithCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientSurroundColorsWithCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientCenterPoint{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientCenterPoint');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientCenterPointI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientCenterPointI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientCenterPoint{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientCenterPoint');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientCenterPointI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientCenterPointI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientPointCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientPointCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientSurroundColorCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientSurroundColorCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientGammaCorrection{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientGammaCorrection');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientGammaCorrection{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientGammaCorrection');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientBlendCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientBlendCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientPresetBlendCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientPresetBlendCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientPresetBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientPresetBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientPresetBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientPresetBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientSigmaBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientSigmaBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientLinearBlend{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientLinearBlend');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientWrapMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientWrapMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientWrapMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientWrapMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipResetPathGradientTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipResetPathGradientTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipMultiplyPathGradientTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipMultiplyPathGradientTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTranslatePathGradientTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTranslatePathGradientTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipScalePathGradientTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipScalePathGradientTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRotatePathGradientTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRotatePathGradientTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPathGradientFocusScales{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPathGradientFocusScales');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPathGradientFocusScales{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPathGradientFocusScales');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreatePen1{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreatePen1');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreatePen2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreatePen2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipClonePen{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipClonePen');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeletePen{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeletePen');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenWidth{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenWidth');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenWidth{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenWidth');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenUnit{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenUnit');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenUnit{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenUnit');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenLineCap197819{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenLineCap197819');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenStartCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenStartCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenEndCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenEndCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenDashCap197819{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenDashCap197819');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenStartCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenStartCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenEndCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenEndCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenDashCap197819{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenDashCap197819');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenLineJoin{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenLineJoin');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenLineJoin{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenLineJoin');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenCustomStartCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenCustomStartCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenCustomStartCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenCustomStartCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenCustomEndCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenCustomEndCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenCustomEndCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenCustomEndCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenMiterLimit{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenMiterLimit');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenMiterLimit{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenMiterLimit');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipResetPenTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipResetPenTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipMultiplyPenTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipMultiplyPenTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTranslatePenTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTranslatePenTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipScalePenTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipScalePenTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRotatePenTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRotatePenTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenColor{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenColor');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenColor{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenColor');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenBrushFill{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenBrushFill');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenBrushFill{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenBrushFill');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenFillType{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenFillType');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenDashStyle{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenDashStyle');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenDashStyle{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenDashStyle');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenDashOffset{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenDashOffset');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenDashOffset{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenDashOffset');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenDashCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenDashCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenDashArray{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenDashArray');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenDashArray{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenDashArray');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenCompoundCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenCompoundCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPenCompoundArray{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPenCompoundArray');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPenCompoundArray{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPenCompoundArray');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateCustomLineCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateCustomLineCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeleteCustomLineCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeleteCustomLineCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneCustomLineCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneCustomLineCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetCustomLineCapType{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapType');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetCustomLineCapStrokeCaps{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetCustomLineCapStrokeCaps');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetCustomLineCapStrokeCaps{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapStrokeCaps');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetCustomLineCapStrokeJoin{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetCustomLineCapStrokeJoin');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetCustomLineCapStrokeJoin{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapStrokeJoin');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetCustomLineCapBaseCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetCustomLineCapBaseCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetCustomLineCapBaseCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapBaseCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetCustomLineCapBaseInset{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetCustomLineCapBaseInset');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetCustomLineCapBaseInset{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapBaseInset');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetCustomLineCapWidthScale{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetCustomLineCapWidthScale');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetCustomLineCapWidthScale{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapWidthScale');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateAdjustableArrowCap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateAdjustableArrowCap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetAdjustableArrowCapHeight{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetAdjustableArrowCapHeight');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetAdjustableArrowCapHeight{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetAdjustableArrowCapHeight');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetAdjustableArrowCapWidth{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetAdjustableArrowCapWidth');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetAdjustableArrowCapWidth{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetAdjustableArrowCapWidth');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetAdjustableArrowCapMiddleInset{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetAdjustableArrowCapMiddleInset');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetAdjustableArrowCapMiddleInset{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetAdjustableArrowCapMiddleInset');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetAdjustableArrowCapFillState{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetAdjustableArrowCapFillState');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetAdjustableArrowCapFillState{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetAdjustableArrowCapFillState');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipLoadImageFromStream{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipLoadImageFromStream');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipLoadImageFromFile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipLoadImageFromFile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipLoadImageFromStreamICM{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipLoadImageFromStreamICM');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipLoadImageFromFileICM{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipLoadImageFromFileICM');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneImage{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneImage');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDisposeImage{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDisposeImage');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSaveImageToFile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSaveImageToFile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSaveImageToStream{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSaveImageToStream');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSaveAdd{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSaveAdd');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSaveAddImage{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSaveAddImage');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageGraphicsContext{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageGraphicsContext');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageBounds{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageBounds');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageDimension{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageDimension');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageType{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageType');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageWidth{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageWidth');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageHeight{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageHeight');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageHorizontalResolution{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageHorizontalResolution');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageVerticalResolution{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageVerticalResolution');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageFlags{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageFlags');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageRawFormat{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageRawFormat');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImagePixelFormat{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImagePixelFormat');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageThumbnail{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageThumbnail');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetEncoderParameterListSize{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetEncoderParameterListSize');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetEncoderParameterList{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetEncoderParameterList');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipImageGetFrameDimensionsCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipImageGetFrameDimensionsCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipImageGetFrameDimensionsList{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipImageGetFrameDimensionsList');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipImageGetFrameCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipImageGetFrameCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipImageSelectActiveFrame{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipImageSelectActiveFrame');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipImageRotateFlip{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipImageRotateFlip');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImagePalette{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImagePalette');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImagePalette{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImagePalette');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImagePaletteSize{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImagePaletteSize');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPropertyCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPropertyCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPropertyIdList{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPropertyIdList');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPropertyItemSize{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPropertyItemSize');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPropertyItem{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPropertyItem');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPropertySize{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPropertySize');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetAllPropertyItems{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetAllPropertyItems');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRemovePropertyItem{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRemovePropertyItem');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPropertyItem{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPropertyItem');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipImageForceValidation{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipImageForceValidation');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateBitmapFromStream{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromStream');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateBitmapFromFile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromFile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateBitmapFromStreamICM{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromStreamICM');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateBitmapFromFileICM{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromFileICM');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateBitmapFromScan0{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromScan0');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateBitmapFromGraphics{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromGraphics');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateBitmapFromGdiDib{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromGdiDib');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateBitmapFromHBITMAP{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromHBITMAP');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateHBITMAPFromBitmap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateHBITMAPFromBitmap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateBitmapFromHICON{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromHICON');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateHICONFromBitmap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateHICONFromBitmap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateBitmapFromResource{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromResource');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneBitmapArea{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneBitmapArea');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneBitmapAreaI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneBitmapAreaI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipBitmapLockBits{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipBitmapLockBits');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipBitmapUnlockBits{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipBitmapUnlockBits');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipBitmapGetPixel{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipBitmapGetPixel');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipBitmapSetPixel{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipBitmapSetPixel');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipBitmapSetResolution{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipBitmapSetResolution');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateImageAttributes{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateImageAttributes');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneImageAttributes{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneImageAttributes');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDisposeImageAttributes{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDisposeImageAttributes');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesToIdentity{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesToIdentity');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipResetImageAttributes{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipResetImageAttributes');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesColorMatrix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesColorMatrix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesThreshold{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesThreshold');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesGamma{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesGamma');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesNoOp{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesNoOp');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesColorKeys{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesColorKeys');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesOutputChannel{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesOutputChannel');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesOutputChannelColorProfile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesOutputChannelColorProfile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesRemapTable{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesRemapTable');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesWrapMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesWrapMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetImageAttributesICMMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetImageAttributesICMMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageAttributesAdjustedPalette{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageAttributesAdjustedPalette');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFlush{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFlush');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateFromHDC{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateFromHDC');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateFromHDC2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateFromHDC2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateFromHWND{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateFromHWND');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateFromHWNDICM{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateFromHWNDICM');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeleteGraphics{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeleteGraphics');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetDC{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetDC');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipReleaseDC{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipReleaseDC');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetCompositingMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetCompositingMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetCompositingMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetCompositingMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetRenderingOrigin{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetRenderingOrigin');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetRenderingOrigin{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetRenderingOrigin');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetCompositingQuality{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetCompositingQuality');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetCompositingQuality{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetCompositingQuality');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetSmoothingMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetSmoothingMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetSmoothingMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetSmoothingMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPixelOffsetMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPixelOffsetMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPixelOffsetMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPixelOffsetMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetTextRenderingHint{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetTextRenderingHint');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetTextRenderingHint{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetTextRenderingHint');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetTextContrast{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetTextContrast');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetTextContrast{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetTextContrast');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetInterpolationMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetInterpolationMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetInterpolationMode{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetInterpolationMode');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetWorldTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetWorldTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipResetWorldTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipResetWorldTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipMultiplyWorldTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipMultiplyWorldTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTranslateWorldTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTranslateWorldTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipScaleWorldTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipScaleWorldTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRotateWorldTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRotateWorldTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetWorldTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetWorldTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipResetPageTransform{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipResetPageTransform');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPageUnit{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPageUnit');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetPageScale{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetPageScale');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPageUnit{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPageUnit');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetPageScale{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetPageScale');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetDpiX{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetDpiX');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetDpiY{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetDpiY');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTransformPoints{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTransformPoints');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTransformPointsI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTransformPointsI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetNearestColor{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetNearestColor');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateHalftonePalette{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateHalftonePalette');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawLine{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawLine');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawLineI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawLineI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawLines{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawLines');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawLinesI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawLinesI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawArc{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawArc');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawArcI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawArcI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawBezier{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawBezier');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawBezierI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawBezierI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawBeziers{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawBeziers');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawBeziersI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawBeziersI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawRectangle{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawRectangle');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawRectangleI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawRectangleI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawRectangles{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawRectangles');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawRectanglesI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawRectanglesI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawEllipse{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawEllipse');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawEllipseI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawEllipseI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawPie{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawPie');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawPieI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawPieI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawPolygon{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawPolygon');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawPolygonI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawPolygonI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawCurve{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawCurve');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawCurveI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawCurveI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawCurve2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawCurve2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawCurve2I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawCurve2I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawCurve3{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawCurve3');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawCurve3I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawCurve3I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawClosedCurve{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawClosedCurve');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawClosedCurveI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawClosedCurveI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawClosedCurve2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawClosedCurve2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawClosedCurve2I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawClosedCurve2I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGraphicsClear{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGraphicsClear');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillRectangle{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillRectangle');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillRectangleI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillRectangleI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillRectangles{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillRectangles');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillRectanglesI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillRectanglesI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillPolygon{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillPolygon');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillPolygonI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillPolygonI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillPolygon2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillPolygon2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillPolygon2I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillPolygon2I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillEllipse{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillEllipse');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillEllipseI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillEllipseI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillPie{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillPie');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillPieI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillPieI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillClosedCurve{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillClosedCurve');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillClosedCurveI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillClosedCurveI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillClosedCurve2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillClosedCurve2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillClosedCurve2I{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillClosedCurve2I');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFillRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFillRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImage{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImage');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImageI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImageI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImageRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImageRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImageRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImageRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImagePoints{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImagePoints');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImagePointsI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImagePointsI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImagePointRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImagePointRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImagePointRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImagePointRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImageRectRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImageRectRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImageRectRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImageRectRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImagePointsRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImagePointsRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawImagePointsRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawImagePointsRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetClipGraphics{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetClipGraphics');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetClipRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetClipRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetClipRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetClipRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetClipPath{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetClipPath');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetClipRegion{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetClipRegion');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetClipHrgn{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetClipHrgn');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipResetClip{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipResetClip');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTranslateClip{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTranslateClip');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipTranslateClipI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipTranslateClipI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetClip{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetClip');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetClipBounds{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetClipBounds');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetClipBoundsI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetClipBoundsI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsClipEmpty{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsClipEmpty');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetVisibleClipBounds{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetVisibleClipBounds');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetVisibleClipBoundsI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetVisibleClipBoundsI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisibleClipEmpty{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisibleClipEmpty');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisiblePoint{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisiblePoint');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisiblePointI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisiblePointI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisibleRect{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisibleRect');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsVisibleRectI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsVisibleRectI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSaveGraphics{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSaveGraphics');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRestoreGraphics{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRestoreGraphics');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipBeginContainer{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipBeginContainer');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipBeginContainerI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipBeginContainerI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipBeginContainer2{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipBeginContainer2');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipEndContainer{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipEndContainer');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetMetafileHeaderFromWmf{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetMetafileHeaderFromWmf');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetMetafileHeaderFromEmf{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetMetafileHeaderFromEmf');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetMetafileHeaderFromFile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetMetafileHeaderFromFile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetMetafileHeaderFromStream{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetMetafileHeaderFromStream');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetMetafileHeaderFromMetafile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetMetafileHeaderFromMetafile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetHemfFromMetafile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetHemfFromMetafile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateStreamOnFile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateStreamOnFile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateMetafileFromWmf{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateMetafileFromWmf');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateMetafileFromEmf{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateMetafileFromEmf');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateMetafileFromFile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateMetafileFromFile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateMetafileFromWmfFile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateMetafileFromWmfFile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateMetafileFromStream{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateMetafileFromStream');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRecordMetafile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRecordMetafile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRecordMetafileI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRecordMetafileI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRecordMetafileFileName{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRecordMetafileFileName');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRecordMetafileFileNameI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRecordMetafileFileNameI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRecordMetafileStream{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRecordMetafileStream');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipRecordMetafileStreamI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipRecordMetafileStreamI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetMetafileDownLevelRasterizationLimit{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetMetafileDownLevelRasterizationLimit');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetMetafileDownLevelRasterizationLimit{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetMetafileDownLevelRasterizationLimit');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageDecodersSize{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageDecodersSize');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageDecoders{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageDecoders');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageEncodersSize{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageEncodersSize');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetImageEncoders{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetImageEncoders');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipComment{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipComment');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateFontFamilyFromName{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateFontFamilyFromName');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeleteFontFamily{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeleteFontFamily');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneFontFamily{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneFontFamily');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetGenericFontFamilySansSerif{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetGenericFontFamilySansSerif');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetGenericFontFamilySerif{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetGenericFontFamilySerif');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetGenericFontFamilyMonospace{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetGenericFontFamilyMonospace');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetFamilyName{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetFamilyName');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipIsStyleAvailable{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipIsStyleAvailable');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFontCollectionEnumerable{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFontCollectionEnumerable');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipFontCollectionEnumerate{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipFontCollectionEnumerate');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetEmHeight{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetEmHeight');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetCellAscent{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetCellAscent');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetCellDescent{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetCellDescent');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLineSpacing{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLineSpacing');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateFontFromDC{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateFontFromDC');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateFontFromLogfontA{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateFontFromLogfontA');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateFontFromLogfontW{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateFontFromLogfontW');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateFont{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateFont');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneFont{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneFont');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeleteFont{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeleteFont');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetFamily{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetFamily');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetFontStyle{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetFontStyle');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetFontSize{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetFontSize');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetFontUnit{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetFontUnit');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetFontHeight{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetFontHeight');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetFontHeightGivenDPI{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetFontHeightGivenDPI');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLogFontA{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLogFontA');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetLogFontW{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetLogFontW');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipNewInstalledFontCollection{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipNewInstalledFontCollection');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipNewPrivateFontCollection{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipNewPrivateFontCollection');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeletePrivateFontCollection{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeletePrivateFontCollection');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetFontCollectionFamilyCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetFontCollectionFamilyCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetFontCollectionFamilyList{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetFontCollectionFamilyList');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPrivateAddFontFile{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPrivateAddFontFile');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipPrivateAddMemoryFont{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipPrivateAddMemoryFont');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawString{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawString');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipMeasureString{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipMeasureString');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipMeasureCharacterRanges{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipMeasureCharacterRanges');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawDriverString{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawDriverString');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipMeasureDriverString{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipMeasureDriverString');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateStringFormat{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateStringFormat');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipStringFormatGetGenericDefault{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipStringFormatGetGenericDefault');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipStringFormatGetGenericTypographic{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipStringFormatGetGenericTypographic');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeleteStringFormat{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeleteStringFormat');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCloneStringFormat{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCloneStringFormat');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetStringFormatFlags{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetStringFormatFlags');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetStringFormatFlags{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetStringFormatFlags');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetStringFormatAlign{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetStringFormatAlign');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetStringFormatAlign{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetStringFormatAlign');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetStringFormatLineAlign{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetStringFormatLineAlign');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetStringFormatLineAlign{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetStringFormatLineAlign');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetStringFormatTrimming{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetStringFormatTrimming');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetStringFormatTrimming{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetStringFormatTrimming');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetStringFormatHotkeyPrefix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetStringFormatHotkeyPrefix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetStringFormatHotkeyPrefix{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetStringFormatHotkeyPrefix');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetStringFormatTabStops{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetStringFormatTabStops');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetStringFormatTabStops{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetStringFormatTabStops');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetStringFormatTabStopCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetStringFormatTabStopCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetStringFormatDigitSubstitution{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetStringFormatDigitSubstitution');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetStringFormatDigitSubstitution{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetStringFormatDigitSubstitution');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipGetStringFormatMeasurableCharacterRangeCount{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipGetStringFormatMeasurableCharacterRangeCount');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipSetStringFormatMeasurableCharacterRanges{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipSetStringFormatMeasurableCharacterRanges');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipCreateCachedBitmap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipCreateCachedBitmap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDeleteCachedBitmap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDeleteCachedBitmap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipDrawCachedBitmap{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipDrawCachedBitmap');
    {$IFDEF LCLLIB}Pointer({$ENDIF}GdipEmfToWmfBits{$IFDEF LCLLIB}){$ENDIF} :=GetProcAddress(GdipLibrary, 'GdipEmfToWmfBits');
  end;
end;

procedure FreeGdiplus;
begin
  if (GdipLibrary <> 0) then
  begin
    FreeLibrary(GdipLibrary);
    GdipLibrary := 0;

    GdipAlloc := nil;
    GdipFree := nil;
    GdiplusStartup := nil;
    GdiplusShutdown := nil;

    GdipCreatePath := nil;
    GdipCreatePath2 := nil;
    GdipCreatePath2I := nil;
    GdipClonePath := nil;
    GdipDeletePath := nil;
    GdipResetPath := nil;
    GdipGetPointCount := nil;
    GdipGetPathTypes := nil;
    GdipGetPathPoints := nil;
    GdipGetPathPointsI := nil;
    GdipGetPathFillMode := nil;
    GdipSetPathFillMode := nil;
    GdipGetPathData := nil;
    GdipStartPathFigure := nil;
    GdipClosePathFigure := nil;
    GdipClosePathFigures := nil;
    GdipSetPathMarker := nil;
    GdipClearPathMarkers := nil;
    GdipReversePath := nil;
    GdipGetPathLastPoint := nil;
    GdipAddPathLine := nil;
    GdipAddPathLine2 := nil;
    GdipAddPathArc := nil;
    GdipAddPathBezier := nil;
    GdipAddPathBeziers := nil;
    GdipAddPathCurve := nil;
    GdipAddPathCurve2 := nil;
    GdipAddPathCurve3 := nil;
    GdipAddPathClosedCurve := nil;
    GdipAddPathClosedCurve2 := nil;
    GdipAddPathRectangle := nil;
    GdipAddPathRectangles := nil;
    GdipAddPathEllipse := nil;
    GdipAddPathPie := nil;
    GdipAddPathPolygon := nil;
    GdipAddPathPath := nil;
    GdipAddPathString := nil;
    GdipAddPathStringI := nil;
    GdipAddPathLineI := nil;
    GdipAddPathLine2I := nil;
    GdipAddPathArcI := nil;
    GdipAddPathBezierI := nil;
    GdipAddPathBeziersI := nil;
    GdipAddPathCurveI := nil;
    GdipAddPathCurve2I := nil;
    GdipAddPathCurve3I := nil;
    GdipAddPathClosedCurveI := nil;
    GdipAddPathClosedCurve2I := nil;
    GdipAddPathRectangleI := nil;
    GdipAddPathRectanglesI := nil;
    GdipAddPathEllipseI := nil;
    GdipAddPathPieI := nil;
    GdipAddPathPolygonI := nil;
    GdipFlattenPath := nil;
    GdipWindingModeOutline := nil;
    GdipWidenPath := nil;
    GdipWarpPath := nil;
    GdipTransformPath := nil;
    GdipGetPathWorldBounds := nil;
    GdipGetPathWorldBoundsI := nil;
    GdipIsVisiblePathPoint := nil;
    GdipIsVisiblePathPointI := nil;
    GdipIsOutlineVisiblePathPoint := nil;
    GdipIsOutlineVisiblePathPointI := nil;
    GdipCreatePathIter := nil;
    GdipDeletePathIter := nil;
    GdipPathIterNextSubpath := nil;
    GdipPathIterNextSubpathPath := nil;
    GdipPathIterNextPathType := nil;
    GdipPathIterNextMarker := nil;
    GdipPathIterNextMarkerPath := nil;
    GdipPathIterGetCount := nil;
    GdipPathIterGetSubpathCount := nil;
    GdipPathIterIsValid := nil;
    GdipPathIterHasCurve := nil;
    GdipPathIterRewind := nil;
    GdipPathIterEnumerate := nil;
    GdipPathIterCopyData := nil;
    GdipCreateMatrix := nil;
    GdipCreateMatrix2 := nil;
    GdipCreateMatrix3 := nil;
    GdipCreateMatrix3I := nil;
    GdipCloneMatrix := nil;
    GdipDeleteMatrix := nil;
    GdipSetMatrixElements := nil;
    GdipMultiplyMatrix := nil;
    GdipTranslateMatrix := nil;
    GdipScaleMatrix := nil;
    GdipRotateMatrix := nil;
    GdipShearMatrix := nil;
    GdipInvertMatrix := nil;
    GdipTransformMatrixPoints := nil;
    GdipTransformMatrixPointsI := nil;
    GdipVectorTransformMatrixPoints := nil;
    GdipVectorTransformMatrixPointsI := nil;
    GdipGetMatrixElements := nil;
    GdipIsMatrixInvertible := nil;
    GdipIsMatrixIdentity := nil;
    GdipIsMatrixEqual := nil;
    GdipCreateRegion := nil;
    GdipCreateRegionRect := nil;
    GdipCreateRegionRectI := nil;
    GdipCreateRegionPath := nil;
    GdipCreateRegionRgnData := nil;
    GdipCreateRegionHrgn := nil;
    GdipCloneRegion := nil;
    GdipDeleteRegion := nil;
    GdipSetInfinite := nil;
    GdipSetEmpty := nil;
    GdipCombineRegionRect := nil;
    GdipCombineRegionRectI := nil;
    GdipCombineRegionPath := nil;
    GdipCombineRegionRegion := nil;
    GdipTranslateRegion := nil;
    GdipTranslateRegionI := nil;
    GdipTransformRegion := nil;
    GdipGetRegionBounds := nil;
    GdipGetRegionBoundsI := nil;
    GdipGetRegionHRgn := nil;
    GdipIsEmptyRegion := nil;
    GdipIsInfiniteRegion := nil;
    GdipIsEqualRegion := nil;
    GdipGetRegionDataSize := nil;
    GdipGetRegionData := nil;
    GdipIsVisibleRegionPoint := nil;
    GdipIsVisibleRegionPointI := nil;
    GdipIsVisibleRegionRect := nil;
    GdipIsVisibleRegionRectI := nil;
    GdipGetRegionScansCount := nil;
    GdipGetRegionScans := nil;
    GdipGetRegionScansI := nil;
    GdipCloneBrush := nil;
    GdipDeleteBrush := nil;
    GdipGetBrushType := nil;
    GdipCreateHatchBrush := nil;
    GdipGetHatchStyle := nil;
    GdipGetHatchForegroundColor := nil;
    GdipGetHatchBackgroundColor := nil;
    GdipCreateTexture := nil;
    GdipCreateTexture2 := nil;
    GdipCreateTextureIA := nil;
    GdipCreateTexture2I := nil;
    GdipCreateTextureIAI := nil;
    GdipGetTextureTransform := nil;
    GdipSetTextureTransform := nil;
    GdipResetTextureTransform := nil;
    GdipMultiplyTextureTransform := nil;
    GdipTranslateTextureTransform := nil;
    GdipScaleTextureTransform := nil;
    GdipRotateTextureTransform := nil;
    GdipSetTextureWrapMode := nil;
    GdipGetTextureWrapMode := nil;
    GdipGetTextureImage := nil;
    GdipCreateSolidFill := nil;
    GdipSetSolidFillColor := nil;
    GdipGetSolidFillColor := nil;
    GdipCreateLineBrush := nil;
    GdipCreateLineBrushI := nil;
    GdipCreateLineBrushFromRect := nil;
    GdipCreateLineBrushFromRectI := nil;
    GdipCreateLineBrushFromRectWithAngle := nil;
    GdipCreateLineBrushFromRectWithAngleI := nil;
    GdipSetLineColors := nil;
    GdipGetLineColors := nil;
    GdipGetLineRect := nil;
    GdipGetLineRectI := nil;
    GdipSetLineGammaCorrection := nil;
    GdipGetLineGammaCorrection := nil;
    GdipGetLineBlendCount := nil;
    GdipGetLineBlend := nil;
    GdipSetLineBlend := nil;
    GdipGetLinePresetBlendCount := nil;
    GdipGetLinePresetBlend := nil;
    GdipSetLinePresetBlend := nil;
    GdipSetLineSigmaBlend := nil;
    GdipSetLineLinearBlend := nil;
    GdipSetLineWrapMode := nil;
    GdipGetLineWrapMode := nil;
    GdipGetLineTransform := nil;
    GdipSetLineTransform := nil;
    GdipResetLineTransform := nil;
    GdipMultiplyLineTransform := nil;
    GdipTranslateLineTransform := nil;
    GdipScaleLineTransform := nil;
    GdipRotateLineTransform := nil;
    GdipCreatePathGradient := nil;
    GdipCreatePathGradientI := nil;
    GdipCreatePathGradientFromPath := nil;
    GdipGetPathGradientCenterColor := nil;
    GdipSetPathGradientCenterColor := nil;
    GdipGetPathGradientSurroundColorsWithCount := nil;
    GdipSetPathGradientSurroundColorsWithCount := nil;
    GdipGetPathGradientPath := nil;
    GdipSetPathGradientPath := nil;
    GdipGetPathGradientCenterPoint := nil;
    GdipGetPathGradientCenterPointI := nil;
    GdipSetPathGradientCenterPoint := nil;
    GdipSetPathGradientCenterPointI := nil;
    GdipGetPathGradientRect := nil;
    GdipGetPathGradientRectI := nil;
    GdipGetPathGradientPointCount := nil;
    GdipGetPathGradientSurroundColorCount := nil;
    GdipSetPathGradientGammaCorrection := nil;
    GdipGetPathGradientGammaCorrection := nil;
    GdipGetPathGradientBlendCount := nil;
    GdipGetPathGradientBlend := nil;
    GdipSetPathGradientBlend := nil;
    GdipGetPathGradientPresetBlendCount := nil;
    GdipGetPathGradientPresetBlend := nil;
    GdipSetPathGradientPresetBlend := nil;
    GdipSetPathGradientSigmaBlend := nil;
    GdipSetPathGradientLinearBlend := nil;
    GdipGetPathGradientWrapMode := nil;
    GdipSetPathGradientWrapMode := nil;
    GdipGetPathGradientTransform := nil;
    GdipSetPathGradientTransform := nil;
    GdipResetPathGradientTransform := nil;
    GdipMultiplyPathGradientTransform := nil;
    GdipTranslatePathGradientTransform := nil;
    GdipScalePathGradientTransform := nil;
    GdipRotatePathGradientTransform := nil;
    GdipGetPathGradientFocusScales := nil;
    GdipSetPathGradientFocusScales := nil;
    GdipCreatePen1 := nil;
    GdipCreatePen2 := nil;
    GdipClonePen := nil;
    GdipDeletePen := nil;
    GdipSetPenWidth := nil;
    GdipGetPenWidth := nil;
    GdipSetPenUnit := nil;
    GdipGetPenUnit := nil;
    GdipSetPenLineCap197819 := nil;
    GdipSetPenStartCap := nil;
    GdipSetPenEndCap := nil;
    GdipSetPenDashCap197819 := nil;
    GdipGetPenStartCap := nil;
    GdipGetPenEndCap := nil;
    GdipGetPenDashCap197819 := nil;
    GdipSetPenLineJoin := nil;
    GdipGetPenLineJoin := nil;
    GdipSetPenCustomStartCap := nil;
    GdipGetPenCustomStartCap := nil;
    GdipSetPenCustomEndCap := nil;
    GdipGetPenCustomEndCap := nil;
    GdipSetPenMiterLimit := nil;
    GdipGetPenMiterLimit := nil;
    GdipSetPenMode := nil;
    GdipGetPenMode := nil;
    GdipSetPenTransform := nil;
    GdipGetPenTransform := nil;
    GdipResetPenTransform := nil;
    GdipMultiplyPenTransform := nil;
    GdipTranslatePenTransform := nil;
    GdipScalePenTransform := nil;
    GdipRotatePenTransform := nil;
    GdipSetPenColor := nil;
    GdipGetPenColor := nil;
    GdipSetPenBrushFill := nil;
    GdipGetPenBrushFill := nil;
    GdipGetPenFillType := nil;
    GdipGetPenDashStyle := nil;
    GdipSetPenDashStyle := nil;
    GdipGetPenDashOffset := nil;
    GdipSetPenDashOffset := nil;
    GdipGetPenDashCount := nil;
    GdipSetPenDashArray := nil;
    GdipGetPenDashArray := nil;
    GdipGetPenCompoundCount := nil;
    GdipSetPenCompoundArray := nil;
    GdipGetPenCompoundArray := nil;
    GdipCreateCustomLineCap := nil;
    GdipDeleteCustomLineCap := nil;
    GdipCloneCustomLineCap := nil;
    GdipGetCustomLineCapType := nil;
    GdipSetCustomLineCapStrokeCaps := nil;
    GdipGetCustomLineCapStrokeCaps := nil;
    GdipSetCustomLineCapStrokeJoin := nil;
    GdipGetCustomLineCapStrokeJoin := nil;
    GdipSetCustomLineCapBaseCap := nil;
    GdipGetCustomLineCapBaseCap := nil;
    GdipSetCustomLineCapBaseInset := nil;
    GdipGetCustomLineCapBaseInset := nil;
    GdipSetCustomLineCapWidthScale := nil;
    GdipGetCustomLineCapWidthScale := nil;
    GdipCreateAdjustableArrowCap := nil;
    GdipSetAdjustableArrowCapHeight := nil;
    GdipGetAdjustableArrowCapHeight := nil;
    GdipSetAdjustableArrowCapWidth := nil;
    GdipGetAdjustableArrowCapWidth := nil;
    GdipSetAdjustableArrowCapMiddleInset := nil;
    GdipGetAdjustableArrowCapMiddleInset := nil;
    GdipSetAdjustableArrowCapFillState := nil;
    GdipGetAdjustableArrowCapFillState := nil;
    GdipLoadImageFromStream := nil;
    GdipLoadImageFromFile := nil;
    GdipLoadImageFromStreamICM := nil;
    GdipLoadImageFromFileICM := nil;
    GdipCloneImage := nil;
    GdipDisposeImage := nil;
    GdipSaveImageToFile := nil;
    GdipSaveImageToStream := nil;
    GdipSaveAdd := nil;
    GdipSaveAddImage := nil;
    GdipGetImageGraphicsContext := nil;
    GdipGetImageBounds := nil;
    GdipGetImageDimension := nil;
    GdipGetImageType := nil;
    GdipGetImageWidth := nil;
    GdipGetImageHeight := nil;
    GdipGetImageHorizontalResolution := nil;
    GdipGetImageVerticalResolution := nil;
    GdipGetImageFlags := nil;
    GdipGetImageRawFormat := nil;
    GdipGetImagePixelFormat := nil;
    GdipGetImageThumbnail := nil;
    GdipGetEncoderParameterListSize := nil;
    GdipGetEncoderParameterList := nil;
    GdipImageGetFrameDimensionsCount := nil;
    GdipImageGetFrameDimensionsList := nil;
    GdipImageGetFrameCount := nil;
    GdipImageSelectActiveFrame := nil;
    GdipImageRotateFlip := nil;
    GdipGetImagePalette := nil;
    GdipSetImagePalette := nil;
    GdipGetImagePaletteSize := nil;
    GdipGetPropertyCount := nil;
    GdipGetPropertyIdList := nil;
    GdipGetPropertyItemSize := nil;
    GdipGetPropertyItem := nil;
    GdipGetPropertySize := nil;
    GdipGetAllPropertyItems := nil;
    GdipRemovePropertyItem := nil;
    GdipSetPropertyItem := nil;
    GdipImageForceValidation := nil;
    GdipCreateBitmapFromStream := nil;
    GdipCreateBitmapFromFile := nil;
    GdipCreateBitmapFromStreamICM := nil;
    GdipCreateBitmapFromFileICM := nil;
    GdipCreateBitmapFromScan0 := nil;
    GdipCreateBitmapFromGraphics := nil;
    GdipCreateBitmapFromGdiDib := nil;
    GdipCreateBitmapFromHBITMAP := nil;
    GdipCreateHBITMAPFromBitmap := nil;
    GdipCreateBitmapFromHICON := nil;
    GdipCreateHICONFromBitmap := nil;
    GdipCreateBitmapFromResource := nil;
    GdipCloneBitmapArea := nil;
    GdipCloneBitmapAreaI := nil;
    GdipBitmapLockBits := nil;
    GdipBitmapUnlockBits := nil;
    GdipBitmapGetPixel := nil;
    GdipBitmapSetPixel := nil;
    GdipBitmapSetResolution := nil;
    GdipCreateImageAttributes := nil;
    GdipCloneImageAttributes := nil;
    GdipDisposeImageAttributes := nil;
    GdipSetImageAttributesToIdentity := nil;
    GdipResetImageAttributes := nil;
    GdipSetImageAttributesColorMatrix := nil;
    GdipSetImageAttributesThreshold := nil;
    GdipSetImageAttributesGamma := nil;
    GdipSetImageAttributesNoOp := nil;
    GdipSetImageAttributesColorKeys := nil;
    GdipSetImageAttributesOutputChannel := nil;
    GdipSetImageAttributesOutputChannelColorProfile := nil;
    GdipSetImageAttributesRemapTable := nil;
    GdipSetImageAttributesWrapMode := nil;
    GdipSetImageAttributesICMMode := nil;
    GdipGetImageAttributesAdjustedPalette := nil;
    GdipFlush := nil;
    GdipCreateFromHDC := nil;
    GdipCreateFromHDC2 := nil;
    GdipCreateFromHWND := nil;
    GdipCreateFromHWNDICM := nil;
    GdipDeleteGraphics := nil;
    GdipGetDC := nil;
    GdipReleaseDC := nil;
    GdipSetCompositingMode := nil;
    GdipGetCompositingMode := nil;
    GdipSetRenderingOrigin := nil;
    GdipGetRenderingOrigin := nil;
    GdipSetCompositingQuality := nil;
    GdipGetCompositingQuality := nil;
    GdipSetSmoothingMode := nil;
    GdipGetSmoothingMode := nil;
    GdipSetPixelOffsetMode := nil;
    GdipGetPixelOffsetMode := nil;
    GdipSetTextRenderingHint := nil;
    GdipGetTextRenderingHint := nil;
    GdipSetTextContrast := nil;
    GdipGetTextContrast := nil;
    GdipSetInterpolationMode := nil;
    GdipGetInterpolationMode := nil;
    GdipSetWorldTransform := nil;
    GdipResetWorldTransform := nil;
    GdipMultiplyWorldTransform := nil;
    GdipTranslateWorldTransform := nil;
    GdipScaleWorldTransform := nil;
    GdipRotateWorldTransform := nil;
    GdipGetWorldTransform := nil;
    GdipResetPageTransform := nil;
    GdipGetPageUnit := nil;
    GdipGetPageScale := nil;
    GdipSetPageUnit := nil;
    GdipSetPageScale := nil;
    GdipGetDpiX := nil;
    GdipGetDpiY := nil;
    GdipTransformPoints := nil;
    GdipTransformPointsI := nil;
    GdipGetNearestColor := nil;
    GdipCreateHalftonePalette := nil;
    GdipDrawLine := nil;
    GdipDrawLineI := nil;
    GdipDrawLines := nil;
    GdipDrawLinesI := nil;
    GdipDrawArc := nil;
    GdipDrawArcI := nil;
    GdipDrawBezier := nil;
    GdipDrawBezierI := nil;
    GdipDrawBeziers := nil;
    GdipDrawBeziersI := nil;
    GdipDrawRectangle := nil;
    GdipDrawRectangleI := nil;
    GdipDrawRectangles := nil;
    GdipDrawRectanglesI := nil;
    GdipDrawEllipse := nil;
    GdipDrawEllipseI := nil;
    GdipDrawPie := nil;
    GdipDrawPieI := nil;
    GdipDrawPolygon := nil;
    GdipDrawPolygonI := nil;
    GdipDrawPath := nil;
    GdipDrawCurve := nil;
    GdipDrawCurveI := nil;
    GdipDrawCurve2 := nil;
    GdipDrawCurve2I := nil;
    GdipDrawCurve3 := nil;
    GdipDrawCurve3I := nil;
    GdipDrawClosedCurve := nil;
    GdipDrawClosedCurveI := nil;
    GdipDrawClosedCurve2 := nil;
    GdipDrawClosedCurve2I := nil;
    GdipGraphicsClear := nil;
    GdipFillRectangle := nil;
    GdipFillRectangleI := nil;
    GdipFillRectangles := nil;
    GdipFillRectanglesI := nil;
    GdipFillPolygon := nil;
    GdipFillPolygonI := nil;
    GdipFillPolygon2 := nil;
    GdipFillPolygon2I := nil;
    GdipFillEllipse := nil;
    GdipFillEllipseI := nil;
    GdipFillPie := nil;
    GdipFillPieI := nil;
    GdipFillPath := nil;
    GdipFillClosedCurve := nil;
    GdipFillClosedCurveI := nil;
    GdipFillClosedCurve2 := nil;
    GdipFillClosedCurve2I := nil;
    GdipFillRegion := nil;
    GdipDrawImage := nil;
    GdipDrawImageI := nil;
    GdipDrawImageRect := nil;
    GdipDrawImageRectI := nil;
    GdipDrawImagePoints := nil;
    GdipDrawImagePointsI := nil;
    GdipDrawImagePointRect := nil;
    GdipDrawImagePointRectI := nil;
    GdipDrawImageRectRect := nil;
    GdipDrawImageRectRectI := nil;
    GdipDrawImagePointsRect := nil;
    GdipDrawImagePointsRectI := nil;
    GdipSetClipGraphics := nil;
    GdipSetClipRect := nil;
    GdipSetClipRectI := nil;
    GdipSetClipPath := nil;
    GdipSetClipRegion := nil;
    GdipSetClipHrgn := nil;
    GdipResetClip := nil;
    GdipTranslateClip := nil;
    GdipTranslateClipI := nil;
    GdipGetClip := nil;
    GdipGetClipBounds := nil;
    GdipGetClipBoundsI := nil;
    GdipIsClipEmpty := nil;
    GdipGetVisibleClipBounds := nil;
    GdipGetVisibleClipBoundsI := nil;
    GdipIsVisibleClipEmpty := nil;
    GdipIsVisiblePoint := nil;
    GdipIsVisiblePointI := nil;
    GdipIsVisibleRect := nil;
    GdipIsVisibleRectI := nil;
    GdipSaveGraphics := nil;
    GdipRestoreGraphics := nil;
    GdipBeginContainer := nil;
    GdipBeginContainerI := nil;
    GdipBeginContainer2 := nil;
    GdipEndContainer := nil;
    GdipGetMetafileHeaderFromWmf := nil;
    GdipGetMetafileHeaderFromEmf := nil;
    GdipGetMetafileHeaderFromFile := nil;
    GdipGetMetafileHeaderFromStream := nil;
    GdipGetMetafileHeaderFromMetafile := nil;
    GdipGetHemfFromMetafile := nil;
    GdipCreateStreamOnFile := nil;
    GdipCreateMetafileFromWmf := nil;
    GdipCreateMetafileFromEmf := nil;
    GdipCreateMetafileFromFile := nil;
    GdipCreateMetafileFromWmfFile := nil;
    GdipCreateMetafileFromStream := nil;
    GdipRecordMetafile := nil;
    GdipRecordMetafileI := nil;
    GdipRecordMetafileFileName := nil;
    GdipRecordMetafileFileNameI := nil;
    GdipRecordMetafileStream := nil;
    GdipRecordMetafileStreamI := nil;
    GdipSetMetafileDownLevelRasterizationLimit := nil;
    GdipGetMetafileDownLevelRasterizationLimit := nil;
    GdipGetImageDecodersSize := nil;
    GdipGetImageDecoders := nil;
    GdipGetImageEncodersSize := nil;
    GdipGetImageEncoders := nil;
    GdipComment := nil;
    GdipCreateFontFamilyFromName := nil;
    GdipDeleteFontFamily := nil;
    GdipCloneFontFamily := nil;
    GdipGetGenericFontFamilySansSerif := nil;
    GdipGetGenericFontFamilySerif := nil;
    GdipGetGenericFontFamilyMonospace := nil;
    GdipGetFamilyName := nil;
    GdipIsStyleAvailable := nil;
    GdipFontCollectionEnumerable := nil;
    GdipFontCollectionEnumerate := nil;
    GdipGetEmHeight := nil;
    GdipGetCellAscent := nil;
    GdipGetCellDescent := nil;
    GdipGetLineSpacing := nil;
    GdipCreateFontFromDC := nil;
    GdipCreateFontFromLogfontA := nil;
    GdipCreateFontFromLogfontW := nil;
    GdipCreateFont := nil;
    GdipCloneFont := nil;
    GdipDeleteFont := nil;
    GdipGetFamily := nil;
    GdipGetFontStyle := nil;
    GdipGetFontSize := nil;
    GdipGetFontUnit := nil;
    GdipGetFontHeight := nil;
    GdipGetFontHeightGivenDPI := nil;
    GdipGetLogFontA := nil;
    GdipGetLogFontW := nil;
    GdipNewInstalledFontCollection := nil;
    GdipNewPrivateFontCollection := nil;
    GdipDeletePrivateFontCollection := nil;
    GdipGetFontCollectionFamilyCount := nil;
    GdipGetFontCollectionFamilyList := nil;
    GdipPrivateAddFontFile := nil;
    GdipPrivateAddMemoryFont := nil;
    GdipDrawString := nil;
    GdipMeasureString := nil;
    GdipMeasureCharacterRanges := nil;
    GdipDrawDriverString := nil;
    GdipMeasureDriverString := nil;
    GdipCreateStringFormat := nil;
    GdipStringFormatGetGenericDefault := nil;
    GdipStringFormatGetGenericTypographic := nil;
    GdipDeleteStringFormat := nil;
    GdipCloneStringFormat := nil;
    GdipSetStringFormatFlags := nil;
    GdipGetStringFormatFlags := nil;
    GdipSetStringFormatAlign := nil;
    GdipGetStringFormatAlign := nil;
    GdipSetStringFormatLineAlign := nil;
    GdipGetStringFormatLineAlign := nil;
    GdipSetStringFormatTrimming := nil;
    GdipGetStringFormatTrimming := nil;
    GdipSetStringFormatHotkeyPrefix := nil;
    GdipGetStringFormatHotkeyPrefix := nil;
    GdipSetStringFormatTabStops := nil;
    GdipGetStringFormatTabStops := nil;
    GdipGetStringFormatTabStopCount := nil;
    GdipSetStringFormatDigitSubstitution := nil;
    GdipGetStringFormatDigitSubstitution := nil;
    GdipGetStringFormatMeasurableCharacterRangeCount := nil;
    GdipSetStringFormatMeasurableCharacterRanges := nil;
    GdipCreateCachedBitmap := nil;
    GdipDeleteCachedBitmap := nil;
    GdipDrawCachedBitmap := nil;
    GdipEmfToWmfBits := nil;
  end;
end;

// -----------------------------------------------------------------------------
// TGdiplusBase class
// -----------------------------------------------------------------------------

  class function TGdiplusBase.NewInstance: TObject;
  begin
    Result := InitInstance(GdipAlloc(ULONG(instanceSize)));
  end;

  procedure TGdiplusBase.FreeInstance;
  begin
    CleanupInstance;
    GdipFree(Self);
  end;

// -----------------------------------------------------------------------------
// macros
// -----------------------------------------------------------------------------

function ObjectTypeIsValid(type_: ObjectType): BOOL;
begin
  result :=  ((type_ >= ObjectTypeMin) and (type_ <= ObjectTypeMax));
end;

function GDIP_WMF_RECORD_TO_EMFPLUS(n: integer): Integer;
begin
  result := (n or GDIP_WMF_RECORD_BASE);
end;

function GDIP_EMFPLUS_RECORD_TO_WMF(n: integer): Integer;
begin
  result := n and (not GDIP_WMF_RECORD_BASE);
end;

function GDIP_IS_WMF_RECORDTYPE(n: integer): BOOL;
begin
  result := ((n and GDIP_WMF_RECORD_BASE) <> 0);
end;


//--------------------------------------------------------------------------
// TGPPoint Util
//--------------------------------------------------------------------------

  function MakePoint(X, Y: Integer): TGPPoint;
  begin
    result.X := X;
    result.Y := Y;
  end;

  function MakePoint(X, Y: Single): TGPPointF;
  begin
    Result.X := X;
    result.Y := Y;
  end;

//--------------------------------------------------------------------------
// TGPSize Util
//--------------------------------------------------------------------------

  function MakeSize(Width, Height: Single): TGPSizeF;
  begin
    result.Width := Width;
    result.Height := Height;
  end;

  function MakeSize(Width, Height: Integer): TGPSize;
  begin
    result.Width := Width;
    result.Height := Height;
  end;

//--------------------------------------------------------------------------
// TCharacterRange Util
//--------------------------------------------------------------------------

  function MakeCharacterRange(First, Length: Integer): TCharacterRange;
  begin
    result.First  := First;
    result.Length := Length;
  end;

// -----------------------------------------------------------------------------
// RectF class
// -----------------------------------------------------------------------------

  function MakeRect(x, y, width, height: Single): TGPRectF; overload;
  begin
    Result.X      := x;
    Result.Y      := y;
    Result.Width  := width;
    Result.Height := height;
  end;

  function MakeRect(location: TGPPointF; size: TGPSizeF): TGPRectF; overload;
  begin
    Result.X      := location.X;
    Result.Y      := location.Y;
    Result.Width  := size.Width;
    Result.Height := size.Height;
  end;

// -----------------------------------------------------------------------------
// Rect class
// -----------------------------------------------------------------------------

  function MakeRect(x, y, width, height: Integer): TGPRect; overload;
  begin
    Result.X      := x;
    Result.Y      := y;
    Result.Width  := width;
    Result.Height := height;
  end;

  function MakeRect(location: TGPPoint; size: TGPSize): TGPRect; overload;
  begin
    Result.X      := location.X;
    Result.Y      := location.Y;
    Result.Width  := size.Width;
    Result.Height := size.Height;
  end;

  function MakeRect(const Rect: TRect): TGPRect;
  begin
    Result.X := rect.Left;
    Result.Y := Rect.Top;
    Result.Width := Rect.Right-Rect.Left;
    Result.Height:= Rect.Bottom-Rect.Top;
  end;

// -----------------------------------------------------------------------------
// PathData class
// -----------------------------------------------------------------------------

  constructor TPathData.Create;
  begin
    Count := 0;
    Points := nil;
    Types := nil;
  end;

  destructor TPathData.destroy;
  begin
    if assigned(Points) then freemem(Points);
    if assigned(Types) then freemem(Types);
  end;


function GetPixelFormatSize(pixfmt: PixelFormat): UINT;
begin
  result := (pixfmt shr 8) and $ff;
end;

function IsIndexedPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatIndexed) <> 0;
end;

function IsAlphaPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatAlpha) <> 0;
end;

function IsExtendedPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatExtended) <> 0;
end;

function IsCanonicalPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatCanonical) <> 0;
end;

// -----------------------------------------------------------------------------
// Color class
// -----------------------------------------------------------------------------

{  constructor TGPColor.Create;
  begin
    Argb := DWORD(Black);
  end;

  // Construct an opaque Color object with
  // the specified Red, Green, Blue values.
  //
  // Color values are not premultiplied.

  constructor TGPColor.Create(r, g, b: Byte);
  begin
    Argb := MakeARGB(255, r, g, b);
  end;

  constructor TGPColor.Create(a, r, g, b: Byte);
  begin
    Argb := MakeARGB(a, r, g, b);
  end;

  constructor TGPColor.Create(Value: ARGB);
  begin
    Argb := Value;
  end;

  function TGPColor.GetAlpha: BYTE;
  begin
    result := BYTE(Argb shr AlphaShift);
  end;

  function TGPColor.GetA: BYTE;
  begin
    result := GetAlpha;
  end;

  function TGPColor.GetRed: BYTE;
  begin
    result := BYTE(Argb shr RedShift);
  end;

  function TGPColor.GetR: BYTE;
  begin
    result := GetRed;
  end;

  function TGPColor.GetGreen: Byte;
  begin
    result := BYTE(Argb shr GreenShift);
  end;

  function TGPColor.GetG: Byte;
  begin
    result := GetGreen;
  end;

  function TGPColor.GetBlue: Byte;
  begin
    result := BYTE(Argb shr BlueShift);
  end;

  function TGPColor.GetB: Byte;
  begin
    result := GetBlue;
  end;

  function TGPColor.GetValue: ARGB;
  begin
    result := Argb;
  end;

  procedure TGPColor.SetValue(Value: ARGB);
  begin
    Argb := Value;
  end;

  procedure TGPColor.SetFromCOLORREF(rgb: COLORREF);
  begin
    Argb := MakeARGB(255, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
  end;

  function TGPColor.ToCOLORREF: COLORREF;
  begin
    result := RGB(GetRed, GetGreen, GetBlue);
  end;

  function TGPColor.MakeARGB(a, r, g, b: Byte): ARGB;
  begin
    result := ((DWORD(b) shl  BlueShift) or
               (DWORD(g) shl GreenShift) or
               (DWORD(r) shl   RedShift) or
               (DWORD(a) shl AlphaShift));
  end;  }

  function MakeColor(r, g, b: Byte): ARGB; overload;
  begin
    result := MakeColor(255, r, g, b);
  end;

  function MakeColor(a, r, g, b: Byte): ARGB; overload;
  begin
    result := ((DWORD(b) shl  BlueShift) or
               (DWORD(g) shl GreenShift) or
               (DWORD(r) shl   RedShift) or
               (DWORD(a) shl AlphaShift));
  end;

  function GetAlpha(color: ARGB): BYTE;
  begin
    result := BYTE(color shr AlphaShift);
  end;

  function GetRed(color: ARGB): BYTE;
  begin
    result := BYTE(color shr RedShift);
  end;

  function GetGreen(color: ARGB): BYTE;
  begin
    result := BYTE(color shr GreenShift);
  end;

  function GetBlue(color: ARGB): BYTE;
  begin
    result := BYTE(color shr BlueShift);
  end;

  function ColorRefToARGB(rgb: COLORREF): ARGB;
  begin
    result := MakeColor(255, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
  end;

  function ARGBToColorRef(Color: ARGB): COLORREF;
  begin
    result := RGB(GetRed(Color), GetGreen(Color), GetBlue(Color));
  end;


// -----------------------------------------------------------------------------
// MetafileHeader class
// -----------------------------------------------------------------------------

  procedure TMetafileHeader.GetBounds(out Rect: TGPRect);
  begin
    rect.X      := X;
    rect.Y      := Y;
    rect.Width  := Width;
    rect.Height := Height;
  end;

  function TMetafileHeader.IsWmf: BOOL;
  begin
    result :=  ((Type_ = MetafileTypeWmf) or (Type_ = MetafileTypeWmfPlaceable));
  end;

  function TMetafileHeader.IsWmfPlaceable: BOOL;
  begin
    result := (Type_ = MetafileTypeWmfPlaceable);
  end;

  function TMetafileHeader.IsEmf: BOOL;
  begin
    result := (Type_ = MetafileTypeEmf);
  end;

  function TMetafileHeader.IsEmfOrEmfPlus: BOOL;
  begin
    result := (Type_ >= MetafileTypeEmf);
  end;

  function TMetafileHeader.IsEmfPlus: BOOL;
  begin
    result := (Type_ >= MetafileTypeEmfPlusOnly)
  end;

  function TMetafileHeader.IsEmfPlusDual: BOOL;
  begin
    result := (Type_ = MetafileTypeEmfPlusDual)
  end;

  function TMetafileHeader.IsEmfPlusOnly: BOOL;
  begin
    result := (Type_ = MetafileTypeEmfPlusOnly)
  end;

  function TMetafileHeader.IsDisplay: BOOL;
  begin
    result := (IsEmfPlus and ((EmfPlusFlags and GDIP_EMFPLUSFLAGS_DISPLAY) <> 0));
  end;

  function TMetafileHeader.GetWmfHeader: PMetaHeader;
  begin
    {if IsWmf then result :=  @Header.WmfHeader
             else result := nil;}
    result := nil;
  end;

  function TMetafileHeader.GetEmfHeader: PENHMETAHEADER3;
  begin
    if IsEmfOrEmfPlus then result := @EmfHeader
                      else result := nil;
  end;

initialization
  LoadGdiplus;
finalization
  FreeGdiplus;

{$ENDIF}

end.
