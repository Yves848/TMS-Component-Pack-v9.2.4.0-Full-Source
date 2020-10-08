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

unit AdvPDFGraphicsLibHTMLEngine;

{$I TMSDEFS.INC}

//{$DEFINE USEGRAPHICSHTMLENGINE}

interface

uses
  AdvTypes, Graphics, PictureContainer, AdvGraphics, AdvPDFLib,
  AdvGraphicsTypes
  {$IFNDEF LCLLIB}
  ,Types, Generics.Collections
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ;

type
  TAdvPDFGraphicsLibHTMLLine = class
  private
    FTextColor: TAdvGraphicsColor;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FLineBreak: Boolean;
    FBitmap: TAdvBitmap;
    FHasImage: Boolean;
    FFontName: string;
    FIsURL: Boolean;
    FText: UnicodeString;
    FURL: UnicodeString;
    FBackgroundColor: TAdvGraphicsColor;
    FBitmapHeight: Single;
    FBitmapWidth: Single;
    FSuperscript: Boolean;
    FSubscript: Boolean;
    FTextAlign: TAdvGraphicsTextAlign;
    FOffset: Single;
    FBullet: Boolean;
    FLineColor: TAdvGraphicsColor;
    FIsLine: Boolean;
    procedure SetBitmap(const Value: TAdvBitmap);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset;
    property LineBreak: Boolean read FLineBreak write FLineBreak;
    property LineColor: TAdvGraphicsColor read FLineColor write FLineColor;
    property BackgroundColor: TAdvGraphicsColor read FBackgroundColor write FBackgroundColor;
    property TextColor: TAdvGraphicsColor read FTextColor write FTextColor;
    property HasImage: Boolean read FHasImage write FHasImage;
    property IsUrl: Boolean read FIsURL write FIsURL;
    property Bitmap: TAdvBitmap read FBitmap write SetBitmap;
    property BitmapWidth: Single read FBitmapWidth write FBitmapWidth;
    property BitmapHeight: Single read FBitmapHeight write FBitmapHeight;
    property FontName: string read FFontName write FFontName;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property FontSize: Integer read FFontSize write FFontSize;
    property Text: UnicodeString read FText write FText;
    property URL: UnicodeString read FURL write FURL;
    property Bullet: Boolean read FBullet write FBullet;
    property Offset: Single read FOffset write FOffset;
    property TextAlign: TAdvGraphicsTextAlign read FTextAlign write FTextAlign;
    property Subscript: Boolean read FSubscript write FSubscript;
    property Superscript: Boolean read FSuperscript write FSuperscript;
    property IsLine: Boolean read FIsLine write FIsLine;
  end;

  TAdvPDFGraphicsLibHTMLLineBreak = class(TAdvPDFGraphicsLibHTMLLine)
  public
    constructor Create; override;
  end;

  TAdvPDFGraphicsLibHTMLImage = class(TAdvPDFGraphicsLibHTMLLine)
  public
    constructor Create; override;
  end;

  TAdvPDFGraphicsLibHTMLLines = TObjectList<TAdvPDFGraphicsLibHTMLLine>;

  TAdvPDFGraphicsLibHTMLEngine = class
  public
    class function ParseHTML(AHTML: string; APictureContainer: TPictureContainer = nil): TAdvPDFGraphicsLibHTMLLines;
    class function DrawHTMLText(APDFLib: IAdvCustomPDFLib; Text: UnicodeString; Rect: TRectF; APictureContainer: TPictureContainer = nil; Paging: Boolean = False; AScale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    class function DrawHTMLText(APDFLib: IAdvCustomPDFLib; Text: UnicodeString; Point: TPointF; APictureContainer: TPictureContainer = nil; AScale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
  end;

implementation

uses
  Classes, SysUtils, AdvPDFCoreLibBase,
  AdvUtils, Math
  {$IFDEF USEGRAPHICSHTMLENGINE}
  , AdvGraphicsPDFEngine
  {$ENDIF}
  ;

var
  FColorLookup: TStringList;

type
  TAdvPDFCoreLibBaseColor = class
  private
    FColor: TAdvGraphicsColor;
  public
    constructor Create(AColor: TAdvGraphicsColor);
    property Color: TAdvGraphicsColor read FColor write FColor;
  end;

{ TAdvPDFGraphicsLibHTMLEngine }

class function TAdvPDFGraphicsLibHTMLEngine.DrawHTMLText(APDFLib: IAdvCustomPDFLib; Text: UnicodeString; Rect: TRectF;
  APictureContainer: TPictureContainer; Paging: Boolean; AScale: Single;
  Calculate: Boolean): TRectF;
{$IFDEF USEGRAPHICSHTMLENGINE}
var
  g: TAdvGraphicsPDFEngine;
  tr: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(APDFLib) then
    Exit;

  g := TAdvGraphicsPDFEngine.Create(APDFLib);
  try
    g.PictureContainer := APictureContainer;
    tr := g.CalculateText(Text, Rect, True);
    if not Calculate then
      g.DrawText(Rect, Text, True, gtaLeading, gtaLeading);

    Result := RectF(Rect.Left, Rect.Top, Rect.Left + tr.Width, Rect.Top + tr.Height);
  finally
    g.Free;
  end;
end;

{$ENDIF}
{$IFNDEF USEGRAPHICSHTMLENGINE}
var
  lst: TAdvPDFGraphicsLibHTMLLines;
  l: TAdvPDFGraphicsLibHTMLLine;
  tr: TRectF;
  s: UnicodeString;
  x, y, yp: Single;
  fts, ftsurl: TAdvPDFGraphicsLibFont;
  mx, my: Single;
  su: UnicodeString;
  rs: array of UnicodeString;
  rsr: TAdvPDFGraphicsLibRectArray;
  I, K: Integer;
  th: Single;
  trbg: TRectF;
  fc, fcto, stc: TAdvGraphicsColor;
  trd, trc: TRectF;
  lbm: TAdvPDFGraphicsLibLineBreakMode;
  applyoffset: Boolean;
  lw: Single;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(APDFLib) then
    Exit;

  lbm := APDFLib.Graphics.LineBreakMode;
  APDFLib.Graphics.LineBreakMode := bmLineBreakModeWordWrap;

  {$IFDEF LCLLIB}
  lst := ParseHTML(UTF8Encode(Text), APictureContainer);
  {$ENDIF}
  {$IFNDEF LCLLIB}
  lst := ParseHTML(Text, APictureContainer);
  {$ENDIF}
  x := Rect.Left;
  y := Rect.Top;
  mx := 0;
  my := 0;
  stc := APDFLib.Graphics.Stroke.Color;
  fc := APDFLib.Graphics.Fill.Color;
  fcto := APDFLib.Graphics.Fill.ColorTo;
  lw := APDFLib.Graphics.Stroke.Width;

  applyoffset := True;
  fts := TAdvPDFGraphicsLibFont.Create;
  ftsurl := TAdvPDFGraphicsLibFont.Create;
  try
    fts.Assign(APDFLib.Graphics.Font);
    ftsurl.Assign(APDFLib.Graphics.Font);
    if Assigned(lst) then
    begin
      th := APDFLib.Graphics.Font.Size * PDFLHFACTOR;
      for l in lst do
      begin
        if l is TAdvPDFGraphicsLibHTMLImage then
        begin
          if Assigned(l.Bitmap) then
          begin
            if (y + l.BitmapHeight * AScale > Rect.Bottom) and Paging and not Calculate then
            begin
              Y := Rect.Top;
              APDFLib.&Initialization.NotifyNewPage;
            end;

            if not Calculate then
            begin
              case l.TextAlign of
                gtaCenter: APDFLib.Graphics.DrawImage(l.Bitmap, RectF(Rect.Left + (Rect.Width - l.BitmapWidth * AScale) / 2, y, Rect.Left + (Rect.Width - l.BitmapWidth * AScale) / 2 + l.BitmapWidth * AScale, y + l.BitmapHeight * AScale));
                gtaLeading: APDFLib.Graphics.DrawImage(l.Bitmap, RectF(x, y, x + l.BitmapWidth * AScale, y + l.BitmapHeight * AScale));
                gtaTrailing: APDFLib.Graphics.DrawImage(l.Bitmap, RectF(Rect.Right - l.BitmapWidth * AScale, y, Rect.Right, y + l.BitmapHeight * AScale));
              end;
            end;

            x := x + l.BitmapWidth * AScale;
            th := l.BitmapHeight * AScale;
            my := Max(my, th);
          end;
        end
        else
        begin
          APDFLib.Graphics.Font.BeginUpdate;
          APDFLib.Graphics.Font.Color := l.TextColor;
          if l.FontSize > 0 then
            APDFLib.Graphics.Font.Size := l.FontSize * AScale
          else
            APDFLib.Graphics.Font.SizeNoScale := fts.Size;

          if l.Subscript or l.Superscript then
            APDFLib.Graphics.Font.SizeNoScale := Round(APDFLib.Graphics.Font.SizeNoScale * 0.7);

          APDFLib.Graphics.Font.Style := l.FontStyle;
          if l.FontName <> '' then
            APDFLib.Graphics.Font.Name := l.FontName
          else
            APDFLib.Graphics.Font.Name := fts.Name;
          APDFLib.Graphics.Font.EndUpdate;

          APDFLib.Graphics.URLFont.BeginUpdate;
          APDFLib.Graphics.URLFont.Assign(APDFLib.Graphics.Font);
          APDFLib.Graphics.URLFont.EndUpdate;

          if l.LineBreak then
          begin
            if l.IsLine then
            begin
              APDFLib.Graphics.Stroke.Color := l.LineColor;
              APDFLib.Graphics.Stroke.Width := 1;
              APDFLib.Graphics.DrawLine(PointF(Rect.Left, y), PointF(mx, y));
              my := my + 2;
            end;

            Y := Y + th;

            th := APDFLib.Graphics.Font.Size * PDFLHFACTOR;

            x := Rect.Left;

            applyoffset := True;
          end
          else
          begin
            if applyoffset then
            begin
              x := x + l.Offset;
              if l.Bullet then
                x := x - APDFLib.Graphics.DrawText(l.Text, PointF(0, 0), True).Width;

              applyoffset := False;
            end;

            s := l.Text;
            SetLength(rs, 1);
            SetLength(rsr, 1);
            rsr[0] := RectF(X, Y, Rect.Right, Y + APDFLib.Graphics.Font.Size * PDFLHFACTOR);
            trc := APDFLib.Graphics.DrawText(s, RectF(0, 0, 10000, 10000), True);
            rsr[0] := RectF(X, Y, Rect.Right, Y + trc.Height);
            yp := y + trc.Height;
            k := APDFLib.Graphics.DrawText(s, rsr, 0, True);
            su := Copy(s, 1, Length(s) - k);
            rs[Length(rs) - 1] := su;
            Delete(s, 1, Length(su));

            th := Max(th, rsr[0].Height);

            while k > 0 do
            begin
              SetLength(rsr, Length(rsr) + 1);
              rsr[Length(rsr) - 1] := RectF(Rect.Left + l.Offset, yp, Rect.Right, yp + APDFLib.Graphics.Font.Size * PDFLHFACTOR);
              trc := APDFLib.Graphics.DrawText(s, RectF(0, 0, 10000, 10000), True);
              rsr[Length(rsr) - 1] := RectF(Rect.Left + l.Offset, yp, Rect.Right, yp + trc.Height);
              yp := yp + trc.Height;
              th := Max(th, trc.Height);
              k := APDFLib.Graphics.DrawText(s, rsr[Length(rsr) - 1], 1, 0, True);
              su := Copy(s, 1, Length(s) - k);
              SetLength(rs, Length(rs) + 1);
              rs[Length(rs) - 1] := su;
              Delete(s, 1, Length(su));
            end;

            for I := 0 to Length(rs) - 1 do
            begin
              if (rsr[I].Bottom > Rect.Bottom) and Paging and not Calculate then
              begin
                Y := Rect.Top;
                for K := I to Length(rsr) - 1 do
                begin
                  rsr[K] := RectF(rsr[K].Left, Y, rsr[K].Right, Y + rsr[K].Height);
                  Y := Y + rsr[K].Height;
                end;

                APDFLib.&Initialization.NotifyNewPage;
              end;

              tr := APDFLib.Graphics.DrawText(rs[I], rsr[I], True);

              case l.TextAlign of
                gtaLeading: trbg := RectF(rsr[I].Left, rsr[I].Top, rsr[I].Left + tr.Width, rsr[I].Top + tr.Height);
                gtaCenter: trbg := RectF(rsr[I].Left + (rsr[I].Width - tr.Width) / 2, rsr[I].Top, rsr[I].Left + (rsr[I].Width - tr.Width) / 2 + tr.Width, rsr[I].Top + tr.Height);
                gtaTrailing: trbg := RectF(rsr[I].Right - tr.Width, rsr[I].Top, rsr[I].Right, rsr[I].Top + tr.Height);
              end;

              if not Calculate then
              begin
                if (l.BackgroundColor <> gcNull) then
                begin
                  APDFLib.Graphics.Stroke.Color := gcNull;
                  APDFLib.Graphics.Fill.Color := l.BackgroundColor;
                  APDFLib.Graphics.Fill.ColorTo := gcNull;
                  APDFLib.Graphics.DrawRectangle(trbg);
                end;

                if l.Subscript then
                  trd := RectF(rsr[I].Left, rsr[I].Bottom - rsr[I].Height * 0.3, rsr[I].Right, rsr[I].Bottom - rsr[I].Height * 0.3 + rsr[I].Height)
                else if l.Superscript then
                  trd := RectF(rsr[I].Left, rsr[I].Top - rsr[I].Height * 0.3, rsr[I].Right, rsr[I].Top - rsr[I].Height * 0.3 + rsr[I].Height)
                else
                  trd := rsr[I];

                case l.TextAlign of
                  gtaCenter: APDFLib.Graphics.Alignment := gtaCenter;
                  gtaLeading: APDFLib.Graphics.Alignment := gtaLeading;
                  gtaTrailing: APDFLib.Graphics.Alignment := gtaTrailing;
                end;

                if l.IsUrl then
                  APDFLib.Graphics.AddURL(rs[I], l.URL, trd)
                else
                  APDFLib.Graphics.DrawText(rs[I], trd);
              end;

              X := rsr[I].Left + tr.Width;
              Y := rsr[I].Top;

              mx := Max(mx, x);
              my := Max(my, y + tr.Height);
              th := Max(th, tr.Height);
            end;
          end;
        end;
      end;
      lst.Free;
    end;
  finally
    APDFLib.Graphics.LineBreakMode := lbm;
    APDFLib.Graphics.Font.BeginUpdate;
    APDFLib.Graphics.Font.Assign(fts);
    APDFLib.Graphics.URLFont.Assign(ftsurl);
    APDFLib.Graphics.Font.EndUpdate;
    APDFLib.Graphics.Fill.Color := fc;
    APDFLib.Graphics.Fill.ColorTo := fcto;
    APDFLib.Graphics.Stroke.Color := stc;
    APDFLib.Graphics.Stroke.Width := lw;
    fts.Free;
    ftsurl.Free;
  end;

  Result := RectF(Rect.Left, Rect.Top, mx, my);
end;
{$ENDIF}

class function TAdvPDFGraphicsLibHTMLEngine.DrawHTMLText(APDFLib: IAdvCustomPDFLib; Text: UnicodeString;
  Point: TPointF; APictureContainer: TPictureContainer; AScale: Single;
  Calculate: Boolean): TRectF;
begin
  Result := DrawHTMLText(APDFLib, Text, RectF(Point.X, Point.Y, Point.X + 10000, Point.Y + 10000), APictureContainer, False, AScale, Calculate);
end;

class function TAdvPDFGraphicsLibHTMLEngine.ParseHTML(AHTML: string; APictureContainer: TPictureContainer = nil): TAdvPDFGraphicsLibHTMLLines;
var
  Su: string;
  FoundTag: Boolean;
  FLines: TAdvPDFGraphicsLibHTMLLines;

  function HTML2Color(s: string): TAdvGraphicsColor;
  begin
    if Pos('#',s) = 1 then
      Result := TAdvGraphics.HTMLToColor(s)
    else
      Result := TAdvGraphics.TextToColor(s);
  end;

  function IStrToInt(S: string): Integer;
  var
    {%H-}Err, Res: Integer;
  begin
    Val(S, Res, Err);
    Result := Res;
  end;

  function ConvertHTMLLine(var S: string): string;
  var
    Su, Res, TagProp, Prop, Tagp, LineText, LType: string;
    Linebreak, Imgbreak: Boolean;
    TagPos,SpacePos: Integer;
    WordLen: Integer;
    TagChar: Char;
    LengthFits: Boolean;
    Anchor: Boolean;
    LastAnchor: string;
    IsImg: Boolean;
    ListIndex: Integer;
    Indent, err: integer;
    Invisible: Boolean;
//    FrstBullet: Boolean;
    HTML: TAdvPDFGraphicsLibHTMLLine;
    HTMLIMG: TAdvPDFGraphicsLibHTMLImage;
    Bmp: TAdvBitmap;
    imgw, imgh: Single;
    TagWidth, TagHeight: Integer;
    X: Integer;
    bmpcreated: Boolean;
    imgal: TAdvGraphicsTextAlign;
    c: TAdvGraphicsColor;
    lb: TAdvPDFGraphicsLibHTMLLineBreak;
    bl: String;
  begin
    Anchor := False;
//    FrstBullet := False;
    Invisible := False;
    ListIndex := 0;
    SpacePos := 0;
    LastAnchor := '';
    bl := '';

    Result := '';
    LineText := '';

    Linebreak := False;
    Imgbreak := False;
    Res := '';

    HTML := TAdvPDFGraphicsLibHTMLLine.Create;

    while (Length(S) > 0) and not linebreak and not imgbreak do
    begin
      IsImg := False;
      TagPos := Pos('<', S);

      if (TagPos > 0) and ((SpacePos > TagPos) or (SpacePos = 0)) then
      begin
        Su := Copy(S, 1, TagPos - 1);
      end
      else
      begin
        if SpacePos > 0 then
          Su := Copy(S, 1, SpacePos)
        else
          Su := S;
      end;

      WordLen := Length(Su);

      while Pos('&nbsp;', Su) > 0 do
      begin
        TAdvUtils.TagReplaceString('&nbsp;', ' ', Su);
      end;

      while Pos('&lt;', Su) > 0 do
      begin
        TAdvUtils.TagReplaceString('&lt;', '<', Su);
      end;

      while Pos('&gt;', Su) > 0 do
      begin
        TAdvUtils.TagReplaceString('&gt;', '>', Su);
      end;

      if WordLen > 0 then
      begin
        if Invisible then
          Delete(S, 1, WordLen);

        if not Invisible then
        begin
          HTML.IsUrl := Anchor;
          {$IFDEF LCLLIB}
          HTML.Text := UTF8Decode(bl + Su);
          {$ENDIF}
          {$IFNDEF LCLLIB}
          HTML.Text := bl + Su;
          {$ENDIF}
          HTML.HasImage := IsImg;

          if Anchor then
          begin
            HTML.TextColor := gcBlue;
            {$IFDEF LCLLIB}
            HTML.Url := UTF8Decode(LastAnchor);
            {$ENDIF}
            {$IFNDEF LCLLIB}
            HTML.Url := LastAnchor;
            {$ENDIF}
            HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsUnderline];
          end;

          if (HTML.Text <> '') or (HTML.HasImage) then
          begin
            FLines.Add(HTML);
            HTML := TAdvPDFGraphicsLibHTMLLine.Create;
          end;

          if bl <> '' then
          begin
            FLines.Add(TAdvPDFGraphicsLibHTMLLineBreak.Create);
            bl := '';
          end;

          LengthFits := True;
          LineText := LineText + Su;

          if LengthFits then
          begin
            Res := Res + Copy(S, 1, WordLen);

            if not LengthFits and (LineText <> Su) then
              S := '';

            Delete(S, 1, WordLen);
          end
          else
          begin
            Linebreak := True;
            FLines.Add(TAdvPDFGraphicsLibHTMLLine.Create);
          end;
        end;
      end;

      TagPos := Pos('<', S);

      if (TagPos = 1) and (Length(S) <= 2) then
        S := '';

      {$IFDEF DELPHI_LLVM}
      X := 1;
      {$ELSE}
      X := 0;
      {$ENDIF}
      if not Linebreak and (TagPos = 1) and (Length(S) > 2) then
      begin
        if (S[2 - X] = '/') and (Length(S) > 3) then
        begin
          case UpCase(S[3 - X]) of
            'A':
              begin
                if Anchor then
                  Anchor := False;
              end;
            'B':
             begin
              if s[4 - X] <> '>' then
              else
                HTML.FontStyle := HTML.FontStyle - [TFontStyle.fsBold];
             end;
            'S':
             begin
              TagChar := UpCase(s[4 - X]);

              if (TagChar = 'U') then
              begin
                HTML.Superscript := False;
                HTML.Subscript := False;
              end
              else
              begin
               if (TagChar = 'H') then
               begin

               end
               else
                HTML.FontStyle := HTML.FontStyle - [TFontStyle.fsStrikeOut];
              end;
             end;
            'F':
              begin
                HTML.Reset;
              end;
            'I':
              begin
                if S[3 - X] <> '>' then
                  Linebreak := True
                else
                  HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsItalic];
              end;
            'L', 'P':
              begin
                Linebreak := True;
                FLines.Add(TAdvPDFGraphicsLibHTMLLineBreak.Create);
              end;
            'U':
              begin
                if (S[4 - X] <> '>') and (ListIndex > 0) then
                begin
                end
                else
                  HTML.FontStyle := HTML.FontStyle - [TFontStyle.fsUnderline, TFontStyle.fsStrikeOut];
              end;
            'Z': Invisible := False;
          end;
        end
        else
        begin
          case UpCase(S[2 - X]) of
            'A':
              begin
                TagProp := Uppercase(Copy(S, 3, Pos('>', S) - 1));
                if (TAdvUtils.VarPos('HREF', TagProp, TagPos) > 0) then
                begin
                  TagProp := Copy(S, 3, Pos('>', S) - 1);
                  Prop := Copy(TagProp, TagPos + 4, Length(TagProp));
                  Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                  Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                  LastAnchor := Prop;
                  Anchor := True;
                end;
              end;
            'B':
              begin
                TagChar := UpCase(S[3 - X]);
                if TagChar = '>' then
                begin
                  HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsBold];
                end
                else if TagChar = 'R' then
                begin
                  Linebreak := True;
                  FLines.Add(TAdvPDFGraphicsLibHTMLLineBreak.Create);
                end
                else
                begin
                  if TagChar = 'O' then
                  begin
                    Res := Res + Copy(S, 1, Pos('>', S));
                    TagProp := Uppercase(Copy(S, 6, Pos('>', S) - 1));

                    if (Pos('BGCOLOR', TagProp) > 0) then
                    begin
                      Prop := Copy(TagProp, Pos('BGCOLOR', TagProp) + 7,
                        Length(TagProp));
                      Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                      Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    end;
                  end;
                end;
              end;
            'H':
              begin
                case UpCase(S[3 - X]) of
                 'R':
                  begin
                    Linebreak := True;
                    TagProp := Copy(s,4,Pos('>',s) - 1);

                    c := gcBlack;
                    if TAdvUtils.VarPos('COLOR',Uppercase(TagProp),TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',prop)-1);
                      c := HTML2Color(Prop);
                    end;

                    lb := TAdvPDFGraphicsLibHTMLLineBreak.Create;
                    lb.LineColor := c;
                    lb.IsLine := True;
                    FLines.Add(lb);
                  end;
                end;
              end;
          'I':begin
                TagChar := Upcase(s[3 - X]);

                if TagChar = '>' then // <I> tag
                  HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsItalic]
                else
                if TagChar = 'N' then  // <IND> tag
                begin
                  TagProp := Copy(s,3,pos('>',s) - 1);

                  Prop := Copy(TagProp,TAdvUtils.ipos('x',TagProp) + 2,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                  val(Prop, indent, err);
                  if err = 0 then
                    HTML.Offset := Indent;
                end
                else
                  if TagChar = 'M' then
                  begin
                    TagProp := Copy(s,3,Pos('>',s) - 1);
                    Prop := Copy(TagProp,Pos('SRC',Uppercase(TagProp)) + 4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    TagProp := Uppercase(TagProp);

                    TagWidth := 0;
                    TagHeight := 0;

                    imgal := gtaLeading;

                    if TAdvUtils.VarPos('ALIGN', TagProp, TagPos) > 0 then
                    begin
                      Tagp := Copy(TagProp, TagPos + 5, Length(TagProp));
                      Tagp := Copy(Tagp, Pos('"', Tagp) + 1, Length(Tagp));
                      Tagp := Copy(Tagp, 1, Pos('"', Tagp) - 1);
                      if Pos('RIGHT',Tagp) > 0 then
                        imgal := gtaTrailing;
                      if Pos('LEFT',Tagp) > 0 then
                        imgal := gtaLeading;
                      if Pos('CENTER',Tagp) > 0 then
                        imgal := gtaCenter;
                    end;

                    if Pos('WIDTH',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,Pos('WIDTH',TagProp) + 6,Length(TagProp));
                      Tagp := Copy(Tagp,Pos('"',tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,Pos('"',tagp) - 1);
                      Val(Tagp,TagWidth,Err);
                    end;

                    if Pos('HEIGHT',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,TAdvUtils.ipos('HEIGHT',TagProp) + 7,Length(TagProp));
                      Tagp := Copy(Tagp,pos('"',Tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,pos('"',Tagp) - 1);
                      Val(Tagp,TagHeight,Err);
                    end;

                    bmp := nil;
                    bmpcreated := false;

                    if (Pos('FILE://',Uppercase(Prop)) > 0)  then
                    begin
                      Delete(Prop,1,7);

                      if FileExists(Prop) then
                      begin
                        bmp := TAdvBitmap.Create;
                        bmp.LoadFromFile(Prop);
                        bmpcreated := true;
                      end;
                    end;

                    if (Pos(':',Prop) = 0) and Assigned(APictureContainer) then
                    begin
                      bmp := APictureContainer.FindBitmap(Prop);
                    end;

                    imgw := 0;
                    imgh := 0;

                    if Assigned(bmp) then
                    begin
                      if not IsBitmapEmpty(bmp) then
                      begin
                        if (TagWidth > 0) and (TagHeight > 0) then
                        begin
                          imgw := TagWidth;
                          imgh := TagHeight;
                        end;
                        if (TagWidth > 0) and (TagHeight = 0) then
                        begin
                          imgw := TagWidth;
                          imgh := Round(TagWidth/bmp.Width * bmp.Height);
                        end;
                        if (TagWidth = 0) and (TagHeight > 0) then
                        begin
                          imgw := Round(TagHeight/bmp.Height * bmp.Width);
                          imgh := TagHeight;
                        end;
                        if (TagWidth = 0) and (TagHeight = 0) then
                        begin
                          imgw := bmp.Width;
                          imgh := bmp.Height;
                        end;

                        HTMLIMG := TAdvPDFGraphicsLibHTMLImage.Create;
                        HTMLIMG.Bitmap.Assign(Bmp);
                        HTMLIMG.BitmapWidth := imgw;
                        HTMLIMG.BitmapHeight := imgh;
                        HTMLIMG.TextAlign := imgal;
                        FLines.Add(HTMLIMG);
                      end;

                      if bmpcreated then
                        bmp.Free;
                    end;
                  end;
                end;
            'L':
              begin
                bl := '';

                if (TAdvUtils.VarPos('>',s,TagPos)>0) then
                begin
                  TagProp := Uppercase(Copy(s,3,TagPos-1));

                  if TAdvUtils.VarPos('TYPE',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);
                    LType := Prop;
                  end;
                end;

                {$IFDEF LCLLIB}
                if LType = 'CIRCLE' then
                  bl := UTF8Encode(UnicodeString(#$2022))
                else if LType = 'SQUARE' then
                  bl := UTF8Encode(UnicodeString(#$25AA))
                else if LType = 'STAR' then
                  bl := UTF8Encode(UnicodeString(#$2605))
                else if LType = 'ARROW' then
                  bl := UTF8Encode(UnicodeString(#$2192))
                else if LType = 'TICK' then
                  bl := UTF8Encode(UnicodeString(#$2713))
                {$ENDIF}
                {$IFNDEF LCLLIB}
                if LType = 'CIRCLE' then
                  bl := #$2022
                else if LType = 'SQUARE' then
                  bl := #$25AA
                else if LType = 'STAR' then
                  bl := #$2605
                else if LType = 'ARROW' then
                  bl := #$2192
                else if LType = 'TICK' then
                  bl := #$2713
                {$ENDIF}
                else
                  bl := '-';

                bl := bl + ' ';
              end;
            'U':
              begin
                if S[3 - X] <> '>' then
                begin
                  Inc(ListIndex);
                  Linebreak := True;
                end
                else
                  HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsUnderline];
              end;
            'P':
              begin
                if (TAdvUtils.VarPos('>', S, TagPos) > 0) then
                begin
                  TagProp := Uppercase(Copy(S, 3, TagPos - 1));

                  if TAdvUtils.VarPos('ALIGN', TagProp, TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp, TagPos + 5, Length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    if Pos('RIGHT',Prop) > 0 then
                      HTML.TextAlign := gtaTrailing;
                    if Pos('LEFT',Prop) > 0 then
                      HTML.TextAlign := gtaLeading;
                    if Pos('CENTER',Prop) > 0 then
                      HTML.TextAlign := gtaCenter;
                  end;

                  if TAdvUtils.VarPos('INDENT', TagProp, TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp, TagPos + 6, Length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    HTML.Offset := IStrToInt(Prop);
                  end;

                  if TAdvUtils.VarPos('BGCOLOR', TagProp, TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp, TagPos + 5, Length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    HTML.BackgroundColor := HTML2Color(Prop);
                  end;
                end;
              end;
            'F':
              begin
                if (TAdvUtils.VarPos('>', S, TagPos) > 0) then
                begin
                  TagProp := UpperCase(Copy(s,6,TagPos-6));

                  if TAdvUtils.VarPos('ALIGN', TagProp, TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp, TagPos + 5, Length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    if Pos('RIGHT',Prop) > 0 then
                      HTML.TextAlign := gtaTrailing;
                    if Pos('LEFT',Prop) > 0 then
                      HTML.TextAlign := gtaLeading;
                    if Pos('CENTER',Prop) > 0 then
                      HTML.TextAlign := gtaCenter;
                  end;

                  if (TAdvUtils.VarPos('FACE',TagProp,TagPos) > 0) then
                  begin
                    Prop := Copy(s,TagPos+4,Length(TagProp));
                    Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                    Prop := Copy(prop,1,pos('"',prop)-1);
                    HTML.FontName := Prop;
                  end;

                  if TAdvUtils.VarPos('INDENT', TagProp, TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp, TagPos + 6, Length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    HTML.Offset := IStrToInt(Prop);
                  end;

                  if TAdvUtils.VarPos('BULLET', TagProp, TagPos) > 0 then
                    HTML.Bullet := True;

                  if (TAdvUtils.VarPos(' COLOR',TagProp,TagPos) > 0) then
                  begin
                    Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);

                    if Length(Prop) > 0 then
                      HTML.TextColor := HTML2Color(Prop)
                  end;

                  if (TAdvUtils.VarPos('BGCOLOR',TagProp,TagPos)>0) then
                  begin
                    Prop := Copy(TagProp,TagPos+7,Length(TagProp));
                    Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                    Prop := Copy(prop,1,pos('"',prop)-1);

                    if Length(Prop) > 0 then
                      HTML.BackgroundColor := HTML2Color(Prop)
                  end;

                  if (TAdvUtils.VarPos('SIZE',TagProp,TagPos)>0) then
                  begin
                    Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                    Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                    Prop := Copy(Prop,Pos('"',Prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    case IStrToInt(Prop) of
                    1: HTML.FontSize := 8;
                    2: HTML.FontSize := 10;
                    3: HTML.FontSize := 12;
                    4: HTML.FontSize := 14;
                    5: HTML.FontSize := 16;
                    else
                      HTML.FontSize := IStrToInt(Prop);
                    end;
                  end;
                end;
              end;
          'S':begin
                TagChar := Upcase(s[3 - X]);

                if TagChar = '>' then
                  HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsStrikeOut]
                else
                begin
                  if TagChar = 'H' then
                  else
                  begin
                    if TAdvUtils.ipos('<SUB>',s)=1 then
                      HTML.Subscript := True
                    else
                      if TAdvUtils.ipos('<SUP>',s)=1 then
                        HTML.Superscript := True;
                  end;
                end;
              end;
            'Z': Invisible := True;
          end;
        end;
        if (TAdvUtils.VarPos('>', S, TagPos) > 0) and not Imgbreak then
        begin
          Res := Res + Copy(S, 1, TagPos);
          Delete(S, 1, TagPos);
        end
        else
        begin
          if not Imgbreak then
            Delete(S, 1, Length(S));
        end;
      end;
    end;

    Result := Res;

    if FLines.IndexOf(HTML) = -1 then
      HTML.Free;
  end;

  function DBTagStrip(s:string):string;
  var
    i,j: Integer;
  begin
    i := Pos('<#',s);
    if i > 0 then
    begin
      Result := Copy(s,1,i - 1);
      Delete(s,1,i);
      j := Pos('>',s);
      if j > 0 then
        Delete(s,j,1);
      Result := Result + s;
    end
    else
      Result := s;
  end;

  function CRLFStrip(s:string;break:boolean):string;
  var
    i: Integer;
  begin
    Result := '';
    {$IFDEF DELPHI_LLVM}
    for i := 0 to Length(s) - 1 do
    {$ELSE}
    for i := 1 to Length(s) do
    {$ENDIF}
    begin
      if not ( (s[i] = #13) or (s[i] = #10)) then
        Result := Result + s[i]
      else
        if (s[i] = #13) and break then
          Result := Result + '<BR>';
    end;
  end;

begin
  FLines := TAdvPDFGraphicsLibHTMLLines.Create;

  if Pos('&', AHTML) > 0 then
  begin
    repeat
      Foundtag := False;
      if TAdvUtils.TagReplacestring('&amp;', '&', AHTML) then Foundtag := True;
      if TAdvUtils.TagReplacestring('&quot;', '"', AHTML) then Foundtag := True;
      if TAdvUtils.TagReplacestring('&sect;', '§', AHTML) then Foundtag := True;
      if TAdvUtils.TagReplacestring('&permil;', '®‰', AHTML) then Foundtag := True;
      if TAdvUtils.TagReplacestring('&reg;', '®', AHTML) then Foundtag := True;
      if TAdvUtils.TagReplacestring('&copy;', '©', AHTML) then Foundtag := True;
      if TAdvUtils.TagReplacestring('&para;', '¶', AHTML) then Foundtag := True;
      if TAdvUtils.TagReplacestring('&trade;', '™', AHTML) then Foundtag := True;
      if TAdvUtils.TagReplacestring('&euro;', '€', AHTML) then Foundtag := True;
      if TAdvUtils.TagReplacestring('&pound;', '£', AHTML) then Foundtag := True;
      if TAdvUtils.TagReplacestring('&dollar;', '$', AHTML) then Foundtag := True;


    until not Foundtag;
  end;

  AHTML := DBTagStrip(AHTML);
  AHTML := CRLFStrip(AHTML, True);

  Su := '';
  while Length(AHTML) > 0 do
    Su := Su + ConvertHTMLLine(AHTML);

  Result := FLines;
end;

{ TAdvPDFGraphicsLibHTMLLine }

constructor TAdvPDFGraphicsLibHTMLLine.Create;
begin
  FBitmap := TAdvBitmap.Create;
  Reset;
end;

destructor TAdvPDFGraphicsLibHTMLLine.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TAdvPDFGraphicsLibHTMLLine.Reset;
begin
  FLineBreak := False;
  FBackgroundColor := gcNull;
  FTextColor := gcBlack;
  FHasImage := False;
  FTextAlign := gtaLeading;
  FSubscript := False;
  FBullet := False;
  FSuperscript := False;
  FIsUrl := False;
  FFontName := '';
  FFontStyle := [];
  FFontSize := -1;
  FText := '';
  FURL := '';
  FOffset := 0;
  FBitmapWidth := 0;
  FBitmapHeight := 0;
end;

procedure TAdvPDFGraphicsLibHTMLLine.SetBitmap(const Value: TAdvBitmap);
begin
  FBitmap.Assign(Value);
end;

{ TAdvPDFGraphicsLibHTMLLineBreak }

constructor TAdvPDFGraphicsLibHTMLLineBreak.Create;
begin
  inherited;
  LineBreak := True;
end;

{ TAdvPDFGraphicsLibHTMLImage }

constructor TAdvPDFGraphicsLibHTMLImage.Create;
begin
  inherited;
  HasImage := True;
end;

procedure DestroyColorLookup;
var
  I: Integer;
begin
  {$IFDEF FMXLIB}
  for I := 0 to FColorLookup.Count - 1 do
    FColorLookup.Objects[I].DisposeOf;
  {$ELSE}
  for I := 0 to FColorLookup.Count - 1 do
    FColorLookup.Objects[I].Free;
  {$ENDIF}

  FColorLookup.Free;
end;

{ TAdvPDFCoreLibBaseColor }

constructor TAdvPDFCoreLibBaseColor.Create(AColor: TAdvGraphicsColor);
begin
  FColor := AColor;
end;

initialization
begin
  fcolorlookup := TStringList.Create;
  fcolorlookup.addobject('aliceblue', TAdvPDFCoreLibBaseColor.Create(gcaliceblue));
  fcolorlookup.addobject('antiquewhite', TAdvPDFCoreLibBaseColor.Create(gcantiquewhite));
  fcolorlookup.addobject('aqua', TAdvPDFCoreLibBaseColor.Create(gcaqua));
  fcolorlookup.addobject('aquamarine', TAdvPDFCoreLibBaseColor.Create(gcaquamarine));
  fcolorlookup.addobject('azure', TAdvPDFCoreLibBaseColor.Create(gcazure));
  fcolorlookup.addobject('beige', TAdvPDFCoreLibBaseColor.Create(gcbeige));
  fcolorlookup.addobject('bisque', TAdvPDFCoreLibBaseColor.Create(gcbisque));
  fcolorlookup.addobject('black', TAdvPDFCoreLibBaseColor.Create(gcblack));
  fcolorlookup.addobject('blanchedalmond', TAdvPDFCoreLibBaseColor.Create(gcblanchedalmond));
  fcolorlookup.addobject('blue', TAdvPDFCoreLibBaseColor.Create(gcblue));
  fcolorlookup.addobject('blueviolet', TAdvPDFCoreLibBaseColor.Create(gcblueviolet));
  fcolorlookup.addobject('brown', TAdvPDFCoreLibBaseColor.Create(gcbrown));
  fcolorlookup.addobject('burlywood', TAdvPDFCoreLibBaseColor.Create(gcburlywood));
  fcolorlookup.addobject('cadetblue', TAdvPDFCoreLibBaseColor.Create(gccadetblue));
  fcolorlookup.addobject('chartreuse', TAdvPDFCoreLibBaseColor.Create(gcchartreuse));
  fcolorlookup.addobject('chocolate', TAdvPDFCoreLibBaseColor.Create(gcchocolate));
  fcolorlookup.addobject('coral', TAdvPDFCoreLibBaseColor.Create(gccoral));
  fcolorlookup.addobject('cornflowerblue', TAdvPDFCoreLibBaseColor.Create(gccornflowerblue));
  fcolorlookup.addobject('cornsilk', TAdvPDFCoreLibBaseColor.Create(gccornsilk));
  fcolorlookup.addobject('crimson', TAdvPDFCoreLibBaseColor.Create(gccrimson));
  fcolorlookup.addobject('cyan', TAdvPDFCoreLibBaseColor.Create(gccyan));
  fcolorlookup.addobject('darkblue', TAdvPDFCoreLibBaseColor.Create(gcdarkblue));
  fcolorlookup.addobject('darkcyan', TAdvPDFCoreLibBaseColor.Create(gcdarkcyan));
  fcolorlookup.addobject('darkgoldenrod', TAdvPDFCoreLibBaseColor.Create(gcdarkgoldenrod));
  fcolorlookup.addobject('darkgray', TAdvPDFCoreLibBaseColor.Create(gcdarkgray));
  fcolorlookup.addobject('darkgreen', TAdvPDFCoreLibBaseColor.Create(gcdarkgreen));
  fcolorlookup.addobject('darkgrey', TAdvPDFCoreLibBaseColor.Create(gcdarkgrey));
  fcolorlookup.addobject('darkkhaki', TAdvPDFCoreLibBaseColor.Create(gcdarkkhaki));
  fcolorlookup.addobject('darkmagenta', TAdvPDFCoreLibBaseColor.Create(gcdarkmagenta));
  fcolorlookup.addobject('darkolivegreen', TAdvPDFCoreLibBaseColor.Create(gcdarkolivegreen));
  fcolorlookup.addobject('darkorange', TAdvPDFCoreLibBaseColor.Create(gcdarkorange));
  fcolorlookup.addobject('darkorchid', TAdvPDFCoreLibBaseColor.Create(gcdarkorchid));
  fcolorlookup.addobject('darkred', TAdvPDFCoreLibBaseColor.Create(gcdarkred));
  fcolorlookup.addobject('darksalmon', TAdvPDFCoreLibBaseColor.Create(gcdarksalmon));
  fcolorlookup.addobject('darkseagreen', TAdvPDFCoreLibBaseColor.Create(gcdarkseagreen));
  fcolorlookup.addobject('darkslateblue', TAdvPDFCoreLibBaseColor.Create(gcdarkslateblue));
  fcolorlookup.addobject('darkslategray', TAdvPDFCoreLibBaseColor.Create(gcdarkslategray));
  fcolorlookup.addobject('darkslategrey', TAdvPDFCoreLibBaseColor.Create(gcdarkslategrey));
  fcolorlookup.addobject('darkturquoise', TAdvPDFCoreLibBaseColor.Create(gcdarkturquoise));
  fcolorlookup.addobject('darkviolet', TAdvPDFCoreLibBaseColor.Create(gcdarkviolet));
  fcolorlookup.addobject('deeppink', TAdvPDFCoreLibBaseColor.Create(gcdeeppink));
  fcolorlookup.addobject('deepskyblue', TAdvPDFCoreLibBaseColor.Create(gcdeepskyblue));
  fcolorlookup.addobject('dimgray', TAdvPDFCoreLibBaseColor.Create(gcdimgray));
  fcolorlookup.addobject('dimgrey', TAdvPDFCoreLibBaseColor.Create(gcdimgrey));
  fcolorlookup.addobject('dodgerblue', TAdvPDFCoreLibBaseColor.Create(gcdodgerblue));
  fcolorlookup.addobject('firebrick', TAdvPDFCoreLibBaseColor.Create(gcfirebrick));
  fcolorlookup.addobject('floralwhite', TAdvPDFCoreLibBaseColor.Create(gcfloralwhite));
  fcolorlookup.addobject('forestgreen', TAdvPDFCoreLibBaseColor.Create(gcforestgreen));
  fcolorlookup.addobject('fuchsia', TAdvPDFCoreLibBaseColor.Create(gcfuchsia));
  fcolorlookup.addobject('gainsboro', TAdvPDFCoreLibBaseColor.Create(gcgainsboro));
  fcolorlookup.addobject('ghostwhite', TAdvPDFCoreLibBaseColor.Create(gcghostwhite));
  fcolorlookup.addobject('gold', TAdvPDFCoreLibBaseColor.Create(gcgold));
  fcolorlookup.addobject('goldenrod', TAdvPDFCoreLibBaseColor.Create(gcgoldenrod));
  fcolorlookup.addobject('gray', TAdvPDFCoreLibBaseColor.Create(gcgray));
  fcolorlookup.addobject('green', TAdvPDFCoreLibBaseColor.Create(gcgreen));
  fcolorlookup.addobject('greenyellow', TAdvPDFCoreLibBaseColor.Create(gcgreenyellow));
  fcolorlookup.addobject('grey', TAdvPDFCoreLibBaseColor.Create(gcgrey));
  fcolorlookup.addobject('honeydew', TAdvPDFCoreLibBaseColor.Create(gchoneydew));
  fcolorlookup.addobject('hotpink', TAdvPDFCoreLibBaseColor.Create(gchotpink));
  fcolorlookup.addobject('indianred', TAdvPDFCoreLibBaseColor.Create(gcindianred));
  fcolorlookup.addobject('indigo', TAdvPDFCoreLibBaseColor.Create(gcindigo));
  fcolorlookup.addobject('ivory', TAdvPDFCoreLibBaseColor.Create(gcivory));
  fcolorlookup.addobject('khaki', TAdvPDFCoreLibBaseColor.Create(gckhaki));
  fcolorlookup.addobject('lavender', TAdvPDFCoreLibBaseColor.Create(gcLavender));
  fcolorlookup.addobject('lavenderblush', TAdvPDFCoreLibBaseColor.Create(gcLavenderblush));
  fcolorlookup.addobject('lawngreen', TAdvPDFCoreLibBaseColor.Create(gcLawngreen));
  fcolorlookup.addobject('lemonchiffon', TAdvPDFCoreLibBaseColor.Create(gclemonchiffon));
  fcolorlookup.addobject('lightblue', TAdvPDFCoreLibBaseColor.Create(gclightblue));
  fcolorlookup.addobject('lightcoral', TAdvPDFCoreLibBaseColor.Create(gclightcoral));
  fcolorlookup.addobject('lightcyan', TAdvPDFCoreLibBaseColor.Create(gclightcyan));
  fcolorlookup.addobject('lightgoldenrodyellow', TAdvPDFCoreLibBaseColor.Create(gclightgoldenrodyellow));
  fcolorlookup.addobject('lightgray', TAdvPDFCoreLibBaseColor.Create(gclightgray));
  fcolorlookup.addobject('lightgreen', TAdvPDFCoreLibBaseColor.Create(gclightgreen));
  fcolorlookup.addobject('lightgrey', TAdvPDFCoreLibBaseColor.Create(gclightgrey));
  fcolorlookup.addobject('lightpink', TAdvPDFCoreLibBaseColor.Create(gclightpink));
  fcolorlookup.addobject('lightsalmon', TAdvPDFCoreLibBaseColor.Create(gclightsalmon));
  fcolorlookup.addobject('lightseagreen', TAdvPDFCoreLibBaseColor.Create(gclightseagreen));
  fcolorlookup.addobject('lightskyblue', TAdvPDFCoreLibBaseColor.Create(gclightskyblue));
  fcolorlookup.addobject('lightslategray', TAdvPDFCoreLibBaseColor.Create(gclightslategray));
  fcolorlookup.addobject('lightslategrey', TAdvPDFCoreLibBaseColor.Create(gclightslategrey));
  fcolorlookup.addobject('lightsteelblue', TAdvPDFCoreLibBaseColor.Create(gclightsteelblue));
  fcolorlookup.addobject('lightyellow', TAdvPDFCoreLibBaseColor.Create(gclightyellow));
  fcolorlookup.addobject('lime', TAdvPDFCoreLibBaseColor.Create(gclime));
  fcolorlookup.addobject('limegreen', TAdvPDFCoreLibBaseColor.Create(gclimegreen));
  fcolorlookup.addobject('linen', TAdvPDFCoreLibBaseColor.Create(gclinen));
  fcolorlookup.addobject('magenta', TAdvPDFCoreLibBaseColor.Create(gcmagenta));
  fcolorlookup.addobject('maroon', TAdvPDFCoreLibBaseColor.Create(gcmaroon));
  fcolorlookup.addobject('mediumaquamarine', TAdvPDFCoreLibBaseColor.Create(gcmediumaquamarine));
  fcolorlookup.addobject('mediumblue', TAdvPDFCoreLibBaseColor.Create(gcmediumblue));
  fcolorlookup.addobject('mediumorchid', TAdvPDFCoreLibBaseColor.Create(gcmediumorchid));
  fcolorlookup.addobject('mediumpurple', TAdvPDFCoreLibBaseColor.Create(gcmediumpurple));
  fcolorlookup.addobject('mediumseagreen', TAdvPDFCoreLibBaseColor.Create(gcmediumseagreen));
  fcolorlookup.addobject('mediumslateblue', TAdvPDFCoreLibBaseColor.Create(gcmediumslateblue));
  fcolorlookup.addobject('mediumspringgreen', TAdvPDFCoreLibBaseColor.Create(gcmediumspringgreen));
  fcolorlookup.addobject('mediumturquoise', TAdvPDFCoreLibBaseColor.Create(gcmediumturquoise));
  fcolorlookup.addobject('mediumvioletred', TAdvPDFCoreLibBaseColor.Create(gcmediumvioletred));
  fcolorlookup.addobject('midnightblue', TAdvPDFCoreLibBaseColor.Create(gcmidnightblue));
  fcolorlookup.addobject('mintcream', TAdvPDFCoreLibBaseColor.Create(gcmintcream));
  fcolorlookup.addobject('mistyrose', TAdvPDFCoreLibBaseColor.Create(gcmistyrose));
  fcolorlookup.addobject('moccasin', TAdvPDFCoreLibBaseColor.Create(gcmoccasin));
  fcolorlookup.addobject('navajowhite', TAdvPDFCoreLibBaseColor.Create(gcnavajowhite));
  fcolorlookup.addobject('navy', TAdvPDFCoreLibBaseColor.Create(gcnavy));
  fcolorlookup.addobject('oldlace', TAdvPDFCoreLibBaseColor.Create(gcoldlace));
  fcolorlookup.addobject('olive', TAdvPDFCoreLibBaseColor.Create(gcolive));
  fcolorlookup.addobject('olivedrab', TAdvPDFCoreLibBaseColor.Create(gcolivedrab));
  fcolorlookup.addobject('orange', TAdvPDFCoreLibBaseColor.Create(gcorange));
  fcolorlookup.addobject('orangered', TAdvPDFCoreLibBaseColor.Create(gcorangered));
  fcolorlookup.addobject('orchid', TAdvPDFCoreLibBaseColor.Create(gcorchid));
  fcolorlookup.addobject('palegoldenrod', TAdvPDFCoreLibBaseColor.Create(gcpalegoldenrod));
  fcolorlookup.addobject('palegreen', TAdvPDFCoreLibBaseColor.Create(gcpalegreen));
  fcolorlookup.addobject('paleturquoise', TAdvPDFCoreLibBaseColor.Create(gcpaleturquoise));
  fcolorlookup.addobject('palevioletred', TAdvPDFCoreLibBaseColor.Create(gcpalevioletred));
  fcolorlookup.addobject('papayawhip', TAdvPDFCoreLibBaseColor.Create(gcpapayawhip));
  fcolorlookup.addobject('peachpuff', TAdvPDFCoreLibBaseColor.Create(gcpeachpuff));
  fcolorlookup.addobject('peru', TAdvPDFCoreLibBaseColor.Create(gcperu));
  fcolorlookup.addobject('pink', TAdvPDFCoreLibBaseColor.Create(gcpink));
  fcolorlookup.addobject('plum', TAdvPDFCoreLibBaseColor.Create(gcplum));
  fcolorlookup.addobject('powderblue', TAdvPDFCoreLibBaseColor.Create(gcpowderblue));
  fcolorlookup.addobject('purple', TAdvPDFCoreLibBaseColor.Create(gcpurple));
  fcolorlookup.addobject('red', TAdvPDFCoreLibBaseColor.Create(gcred));
  fcolorlookup.addobject('rosybrown', TAdvPDFCoreLibBaseColor.Create(gcrosybrown));
  fcolorlookup.addobject('royalblue', TAdvPDFCoreLibBaseColor.Create(gcroyalblue));
  fcolorlookup.addobject('saddlebrown', TAdvPDFCoreLibBaseColor.Create(gcsaddlebrown));
  fcolorlookup.addobject('salmon', TAdvPDFCoreLibBaseColor.Create(gcsalmon));
  fcolorlookup.addobject('sandybrown', TAdvPDFCoreLibBaseColor.Create(gcsandybrown));
  fcolorlookup.addobject('seagreen', TAdvPDFCoreLibBaseColor.Create(gcseagreen));
  fcolorlookup.addobject('seashell', TAdvPDFCoreLibBaseColor.Create(gcseashell));
  fcolorlookup.addobject('sienna', TAdvPDFCoreLibBaseColor.Create(gcsienna));
  fcolorlookup.addobject('skyblue', TAdvPDFCoreLibBaseColor.Create(gcskyblue));
  fcolorlookup.addobject('slateblue', TAdvPDFCoreLibBaseColor.Create(gcslateblue));
  fcolorlookup.addobject('slategray', TAdvPDFCoreLibBaseColor.Create(gcslategray));
  fcolorlookup.addobject('slategrey', TAdvPDFCoreLibBaseColor.Create(gcslategrey));
  fcolorlookup.addobject('snow', TAdvPDFCoreLibBaseColor.Create(gcsnow));
  fcolorlookup.addobject('springgreen', TAdvPDFCoreLibBaseColor.Create(gcspringgreen));
  fcolorlookup.addobject('steelblue', TAdvPDFCoreLibBaseColor.Create(gcsteelblue));
  fcolorlookup.addobject('violet', TAdvPDFCoreLibBaseColor.Create(gcviolet));
  fcolorlookup.addobject('thistle', TAdvPDFCoreLibBaseColor.Create(gcthistle));
  fcolorlookup.addobject('tan', TAdvPDFCoreLibBaseColor.Create(gctan));
  fcolorlookup.addobject('tomato', TAdvPDFCoreLibBaseColor.Create(gctomato));
  fcolorlookup.addobject('turquoise', TAdvPDFCoreLibBaseColor.Create(gcturquoise));
  fcolorlookup.addobject('wheat', TAdvPDFCoreLibBaseColor.Create(gcwheat));
  fcolorlookup.addobject('whitesmoke', TAdvPDFCoreLibBaseColor.Create(gcwhitesmoke));
  fcolorlookup.addobject('yellowgreen', TAdvPDFCoreLibBaseColor.Create(gcyellowgreen));
  fcolorlookup.addobject('red', TAdvPDFCoreLibBaseColor.Create(gcred));
  fcolorlookup.addobject('black', TAdvPDFCoreLibBaseColor.Create(gcblack));
  fcolorlookup.addobject('blue', TAdvPDFCoreLibBaseColor.Create(gcblue));
  fcolorlookup.addobject('green', TAdvPDFCoreLibBaseColor.Create(gcgreen));
  fcolorlookup.addobject('aqua', TAdvPDFCoreLibBaseColor.Create(gcaqua));
  fcolorlookup.addobject('yellow', TAdvPDFCoreLibBaseColor.Create(gcyellow));
  fcolorlookup.addobject('fuchsia', TAdvPDFCoreLibBaseColor.Create(gcfuchsia));
  fcolorlookup.addobject('white', TAdvPDFCoreLibBaseColor.Create(gcwhite));
  fcolorlookup.addobject('lime', TAdvPDFCoreLibBaseColor.Create(gclime));
  fcolorlookup.addobject('silver', TAdvPDFCoreLibBaseColor.Create(gcsilver));
  fcolorlookup.addobject('gray', TAdvPDFCoreLibBaseColor.Create(gcgray));
  fcolorlookup.addobject('olive', TAdvPDFCoreLibBaseColor.Create(gcolive));
  fcolorlookup.addobject('navy', TAdvPDFCoreLibBaseColor.Create(gcnavy));
  fcolorlookup.addobject('purple', TAdvPDFCoreLibBaseColor.Create(gcpurple));
  fcolorlookup.addobject('teal', TAdvPDFCoreLibBaseColor.Create(gcteal));
  fcolorlookup.addobject('orange', TAdvPDFCoreLibBaseColor.Create(gcorange));
  fcolorlookup.addobject('maroon', TAdvPDFCoreLibBaseColor.Create(gcmaroon));
  FColorLookup.Sort;
end;

finalization
begin
  DestroyColorLookup;
end;


end.


