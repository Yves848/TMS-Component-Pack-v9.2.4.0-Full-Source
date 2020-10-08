{********************************************************************}
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

unit AdvGraphicsHTMLEngine;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvGraphics, Types, Graphics,
  AdvTypes, PictureContainer, AdvGraphicsTypes
  {$IFDEF CMNLIB}
  , ImgList
  {$ENDIF}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

var
  AdvHTMLENGINE_ATTR_DELIM: char = '"';

function HTMLDrawEx(AGraphics: TAdvGraphics; s:string; fr:TRectF;
                    XPos,YPos,FocusLink,HoverLink,ShadowOffset: Integer;
                    CheckHotSpot,CheckHeight,{%H-}Print,Selected,Blink,HoverStyle,WordWrap: Boolean;
                    {%H-}ResFactor:Double;
                    URLColor,HoverColor,HoverFontColor,ShadowColor: TAdvGraphicsColor;
                    var AnchorVal,StripVal,FocusAnchor: string;
                    var XSize,YSize: single; var HyperLinks,MouseLink: Integer;
                    var HoverRect:TRectF;var LineCount: Integer; LineSpacing: Integer;
                    PictureContainer: TPictureContainer; {%H-}Opacity: Single; HyperLinkUnderline: Boolean = True{$IFDEF CMNLIB}; ImageList: TCustomImageList = nil{$ENDIF};
                    HighlightColor: TAdvGraphicsColor = gcBlue;
                    HighlightTextColor: TAdvGraphicsColor = gcWhite;
                    HighlightTextStyle: TFontStyles = []
                    ): Boolean; overload;

function HTMLDrawEx(AGraphics: TAdvGraphics; s:string; fr:TRectF;
                    XPos,YPos,FocusLink,HoverLink,ShadowOffset: Integer;
                    CheckHotSpot,CheckHeight,{%H-}Print,Selected,Blink,HoverStyle,WordWrap, Down: Boolean;
                    DownID: string;
                    {%H-}ResFactor:Double;
                    URLColor,HoverColor,HoverFontColor,ShadowColor: TAdvGraphicsColor;
                    var AnchorVal,StripVal,FocusAnchor: string;
                    var XSize,YSize: single; var HyperLinks,MouseLink: Integer;
                    var HoverRect, ControlRect:TRectF;var CID,CV,CT: string; var LineCount: Integer; LineSpacing: Integer;
                    PictureContainer: TPictureContainer; {%H-}Opacity: Single; HyperLinkUnderline: Boolean = True{$IFDEF CMNLIB}; ImageList: TCustomImageList = nil{$ENDIF};
                    HighlightColor: TAdvGraphicsColor = gcBlue;
                    HighlightTextColor: TAdvGraphicsColor = gcWhite;
                    HighlightTextStyle: TFontStyles = []
                    ): Boolean; overload;

function HiLight(s,h,tag:string;DoCase:boolean):string;
function UnHiLight(s,tag:string):string;
function GetControlValue(HTML,ControlID:string;var ControlValue:String): Boolean;
function GetControlProp(HTML,ControlID:string): string;
function GetControlMaxLen(HTML,ControlID:string): integer;
function SetControlValue(var HTML:string;ControlID,ControlValue:string): Boolean;
function ClearRadioControls(HTML: string): string;
function GetNextControlID(HTML:string; ControlID: string): string;
function HasHTMLControl(HTML: string): boolean;
procedure ParseControl(Tag: string; var ControlType,ControlID,ControlValue,ControlWidth,ControlHeight,ControlProp,ControlLen:string);

implementation

uses
  SysUtils, AdvUtils
  {$IFDEF LCLLIB}
  ,LCLIntF
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,Windows
  {$ENDIF}
  ;

function VarIPos(su,s:string;var Res:Integer):Integer;
begin
  Res := Pos(su, Uppercase(s));
  Result := Res;
end;

function StripPos2HTMLPos(s:string; i: Integer): Integer;
var
  j,k: Integer;
  Skip: Boolean;
begin
  Result := 0;
  k := 1;
  Skip := False;

  {$IFDEF DELPHI_LLVM}
  for j := 0 to Length(s) - 1 do
  {$ELSE}
  for j := 1 to Length(s) do
  {$ENDIF}
  begin
    if s[j] = '<' then
      Skip := True;

    if k = i then
    begin
      Result := j;
      Exit;
    end;

    if not Skip then
      Inc(k);

    if s[j] = '>' then
      Skip := False;

  end;

  if k = i then
  begin
    Result := Length(s) + 1;
  end;
end;


function PosFrom(su,s:string; h: Integer;DoCase: boolean; var Res: Integer): Integer;
var
  r: Integer;
begin
  Result := 0;
  Res := 0;

  if h > 0 then
    Delete(s,1,h);

  if DoCase then
    r := Pos(su,s)
  else
    r := Pos(AnsiUpperCase(su), AnsiUpperCase(s));

  if r > 0 then
  begin
    Res := h + r;
    Result := Res;
  end;
end;


function HiLight(s,h,tag:string;DoCase:boolean):string;
var
  hs: string;
  l,k: Integer;
begin
  hs := TAdvUtils.HTMLStrip(s);

  l := 0;

  if (pos(h,s) > 0) and ((pos('<',hs) >0) or (pos('>',hs) > 0)) then
  begin
    hs := StringReplace(hs,'<','&lt;',[rfReplaceAll]);
    hs := StringReplace(hs,'>','&gt;',[rfReplaceAll]);
    s := hs;
    h := StringReplace(h,'<','&lt;',[rfReplaceAll]);
    h := StringReplace(h,'>','&gt;',[rfReplaceAll]);
  end;

  k := 0;
  while PosFrom(h,hs,l,DoCase,k) > 0 do
  begin
    l := k + Length(h);
    Insert('<'+tag+'>',s,StripPos2HTMLPos(s,k));
    Insert('</'+tag+'>',s,StripPos2HTMLPos(s,l));
  end;

  Result := s;
end;

function UnHiLight(s,tag:string):string;
begin
  Result := '';
  // replace line breaks by linefeeds
  tag := Uppercase(tag);
  while Pos('<'+tag+'>',Uppercase(s)) > 0 do s := StringReplace(s,'<'+tag+'>','',[rfIgnoreCase]);
  while Pos('</'+tag+'>',Uppercase(s)) > 0 do s := StringReplace(s,'</'+tag+'>','',[rfIgnoreCase]);

  s := StringReplace(s,'&lt;','<',[rfReplaceAll]);
  s := StringReplace(s,'&gt;','>',[rfReplaceAll]);

  Result := s;
end;

function IStrToInt(s:string):Integer;
var
  {%H-}Err,Res: Integer;
begin
  Val(s,Res,Err);
  Result := Res;
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

procedure ParseControl(Tag: string; var ControlType,ControlID,ControlValue,ControlWidth,ControlHeight,ControlProp,ControlLen:string);
var
  Prop: string;
  vp: integer;
begin
  ControlType := '';
  ControlWidth := '';
  ControlHeight := '';
  ControlValue := '';
  ControlID := '';
  ControlProp := '';
  ControlLen := '';

  vp := 0;
  if VarIPos('TYPE=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) - 1);
    ControlType := Uppercase(Prop);
  end;

  if VarIPos('WIDTH=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) - 1);
    ControlWidth := Prop;
  end;

  if VarIPos('HEIGHT=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) - 1);
    ControlHeight := Prop;
  end;

  if VarIPos('ID=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) - 1);
    ControlID := Prop;
  end;

  if VarIPos('VALUE=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) - 1);
    ControlValue := TAdvUtils.UnFixMarkup(Prop);
  end;

  if VarIPos('PROP=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) - 1);
    ControlProp := TAdvUtils.UnFixMarkup(Prop);
  end;

  if VarIPos('MAXLEN=',Tag,vp) > 0 then
  begin
    Prop := Copy(Tag,vp + 1,Length(Tag));
    Prop := Copy(Prop,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) + 1,Length(Prop));
    Prop := Copy(Prop,1,Pos(AdvHTMLENGINE_ATTR_DELIM,Prop) - 1);
    ControlLen := Prop;
  end;
end;

function HasHTMLControl(HTML: string): boolean;
var
  lp: Integer;
begin
  lp := 0;
  Result := VarIPos('<CONTROL', HTML,lp) > 0;
end;

function GetNextControlID(HTML, ControlID:string): string;
var
  lp: Integer;
  Tag,CType,CID,CV,CW,CH,CP,CL: string;
  flg: boolean;
begin
  Result := '';
  flg := ControlID = '';

  lp := 0;
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    CType := '';
    CID := '';
    CV := '';
    CW := '';
    CH := '';
    CP := '';
    CL := '';
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);

    if flg and (CType <> 'BUTTON') and (CType <> 'CHECK') and (CType <> 'RADIO') then
    begin
      Result := CID;
      Exit;
    end;

    if (ControlID = CID) then
      flg := true;
  end;
end;

function GetControlValue(HTML,ControlID:string;var ControlValue:String): Boolean;
var
  lp: Integer;
  Tag,CType,CID,CV,CW,CH,CP,CL: string;
begin
  Result := False;
  lp := 0;
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    CType := '';
    CID := '';
    CV := '';
    CW := '';
    CH := '';
    CP := '';
    CL := '';
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);
    if (ControlID = CID) then
    begin
      ControlValue := CV;
      Result := True;
      Exit;
    end;
  end;
end;

function GetControlProp(HTML,ControlID:string): string;
var
  lp: Integer;
  Tag,CType,CID,CV,CW,CH,CP,CL: string;
begin
  Result := '';
  lp := 0;
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    CType := '';
    CID := '';
    CV := '';
    CW := '';
    CH := '';
    CP := '';
    CL := '';
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);
    if (ControlID = CID) then
    begin
      Result := CP;
      Exit;
    end;
  end;
end;

function GetControlMaxLen(HTML,ControlID:string): integer;
var
  lp, {%H-}e: Integer;
  Tag,CType,CID,CV,CW,CH,CP,CL: string;
begin
  Result := 0;
  lp := 0;
  e := 0;
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    CType := '';
    CID := '';
    CV := '';
    CW := '';
    CH := '';
    CP := '';
    CL := '';
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);

    if (ControlID = CID) then
    begin
      val(CL,Result,e);
      Exit;
    end;
  end;
end;

function SetControlValue(var HTML:string;ControlID,ControlValue:string): Boolean;
var
  lp: Integer;
  Tag,Temp,CType,CID,CV,CW,CH,CP,CL: string;
begin
  Result := False;
  Temp := '';
  ControlValue := TAdvUtils.FixMarkup(ControlValue);
  lp := 0;
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Temp := Temp + Copy(html,1,lp);
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    CType := '';
    CID := '';
    CV := '';
    CW := '';
    CH := '';
    CP := '';
    CL := '';
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);
    if (ControlID = CID) then
    begin
      Temp := Temp + 'CONTROL ID='+AdvHTMLENGINE_ATTR_DELIM+ControlID+AdvHTMLENGINE_ATTR_DELIM+' VALUE='+AdvHTMLENGINE_ATTR_DELIM+ControlValue+AdvHTMLENGINE_ATTR_DELIM+' WIDTH='+AdvHTMLENGINE_ATTR_DELIM+CW+AdvHTMLENGINE_ATTR_DELIM+' TYPE=' + AdvHTMLENGINE_ATTR_DELIM +CType+AdvHTMLENGINE_ATTR_DELIM;
      if (CP <> '') then
        Temp := Temp + ' PROP='+AdvHTMLENGINE_ATTR_DELIM+TAdvUtils.FixMarkup(CP)+AdvHTMLENGINE_ATTR_DELIM;
      if (CL <> '') then
        Temp := Temp + ' MAXLEN='+AdvHTMLENGINE_ATTR_DELIM+CL+AdvHTMLENGINE_ATTR_DELIM;

      Temp := Temp + '>';
      html := Temp + Copy(html,pos('>',html)+1,Length(html));
      Result := True;
      Exit;
    end;
  end;
end;

function ClearRadioControls(HTML: string): string;
var
  lp: Integer;
  Tag,CType,CID,CV,CW,CH,CP,CL: string;
  sl: TStringList;
  s: string;
begin
  Result := '';

  s := HTML;

  sl := TStringList.Create;
  lp := 0;
  while VarIPos('<CONTROL ',html,lp) > 0 do
  begin
    Delete(html,1,lp);
    Tag := Copy(html,1,Pos('>',html));
    CType := '';
    CID := '';
    CV := '';
    CW := '';
    CH := '';
    CP := '';
    CL := '';
    ParseControl(Tag,CType,CID,CV,CW,CH,CP,CL);

    if CType = 'RADIO' then
      sl.Add(CID);
  end;

  for lp := 0 to sl.Count - 1 do
  begin
    SetControlValue(s, sl.Strings[lp], 'FALSE');
  end;

  sl.Free;
  Result := s;
end;

function HTMLDrawEx(AGraphics: TAdvGraphics; s:string; fr:TRectF;
                    XPos,YPos,FocusLink,HoverLink,ShadowOffset: Integer;
                    CheckHotSpot,CheckHeight,{%H-}Print,Selected,Blink,HoverStyle,WordWrap, Down: Boolean;
                    DownID: string;
                    {%H-}ResFactor:Double;
                    URLColor,HoverColor,HoverFontColor,ShadowColor: TAdvGraphicsColor;
                    var AnchorVal,StripVal,FocusAnchor: string;
                    var XSize,YSize: single; var HyperLinks,MouseLink: Integer;
                    var HoverRect, ControlRect:TRectF;var CID,CV,CT: string; var LineCount: Integer; LineSpacing: Integer;
                    PictureContainer: TPictureContainer; {%H-}Opacity: Single; HyperLinkUnderline: Boolean = True{$IFDEF CMNLIB}; ImageList: TCustomImageList = nil{$ENDIF};
                    HighlightColor: TAdvGraphicsColor = gcBlue;
                    HighlightTextColor: TAdvGraphicsColor = gcWhite;
                    HighlightTextStyle: TFontStyles = []
                    ): Boolean;

type
  TFloatStyle = (fsNone, fsLeft, fsRight);

var
  su: string;
  r,dr: TRectF;
  hr,rr: TRectF;
  htmlwidth,htmlheight,txtheight, ch: Single;
  rh: TRectF;
  Align: TAlignment;
  PIndent: Integer;
  OldFont: TAdvGraphicsFont;
  OldFontColor: TAdvGraphicsColor;
  CalcFont: TAdvGraphicsFont;
  CalcFontColor: TAdvGraphicsColor;
  DrawFont: TAdvGraphicsFont;
  DrawFontCOlor: TAdvGraphicsColor;
  FontColor: TAdvGraphicsColor;
  OldPenColor: TAdvGraphicsColor;
  OldCalcFont: TAdvGraphicsFont;
  OldCalcFontColor: TAdvGraphicsColor;
  BkColor,BGColor: TAdvGraphicsColor;
  OldDrawFont: TAdvGraphicsFont;
  OldDrawFontColor: TAdvGraphicsColor;
  Hotspot, ImageHotspot: Boolean;
  Anchor,OldAnchor,MouseInAnchor,Error: Boolean;
  paracolor,pencolor,blnkcolor,hifcol,hibcol: TAdvGraphicsColor;
  LastAnchor,OldAnchorVal,LType: string;
  IMGSize: TPoint;
  isSup,isSub,isPara,isShad: Boolean;
  hlcount,licount: Integer;
  imgali: single;
  subh, suph: Single;
  ListIndex: Integer;
  Invisible: Boolean;
  FoundTag: Boolean;
  nnFit: Integer;
  inspoint: Integer;
  hifStyles: TFontStyles;
  AltImg,ImgIdx,OldImgIdx: Integer;
  ColL,ColB: TAdvGraphicsColor;
  ofsx,newofsx: integer;
  FHot: Boolean;
  floatrect: TRect;
  floatpt: TPoint;
  txtfloat,txtfloatbefore: TFloatStyle;
  imgfloat: TFloatStyle;


  function HTMLDrawLine(AGraphics: TAdvGraphics; var s:string;r: TRectF;Calc:Boolean;
                        var w,h,subh,suph,imgali:Single;var Align:TAlignment; var PIndent: Integer;
                        XPos,YPos:Integer;var Hotspot,ImageHotSpot:Boolean;OffsetX: integer; var NewOffsetX: integer; var floatrect: TRect; var txtfloat: TFloatStyle):string;
  var
    su,Res,TagProp,Prop,Tagp,LineText,dsu:string;
    cr, ir: TRectF;
    {$IFDEF CMNLIB}
    crc: TRect;
    {$ENDIF}
    linebreak,imgbreak,linkbreak: Boolean;
    indent,bmpx,bmpy,imgw,imgh: Integer;
    err: integer;
    sw, th, errs: Single;
    rh: TRectF;
    TagPos,SpacePos,o,l: Integer;
    bmp: TAdvBitmap;
    bmpcreated: boolean;
    NewColor: TAdvGraphicsColor;
    TagWidth,TagHeight,WordLen: Integer;
    WordWidth: Single;
    TagChar: Char;
    LengthFits: Boolean;
    ptf1,ptf2: TPointF;
    imgalign,imgoffs, imgth: integer;
    ControlType,ControlWidth,ControlHeight,ControlID,ControlValue,ControlProp,ControlLen: string;
  begin
    Result := '';
    LineText := '';
    WordWidth := 0;
    r.Bottom := r.Bottom - Subh;

    w := 0;
    sw := 0;

    LineBreak := False;
    ImgBreak := False;
    LinkBreak := False;
    HotSpot := False;
    ImageHotSpot := False;

//    r.Left := r.Left + offsetX;

    cr := r;
    res := '';

    if not Calc then
      cr.Left := cr.Left + OffsetX;

    if isPara and not Calc then
    begin
      Pencolor := AGraphics.Stroke.Color;
      AGraphics.Stroke.Color := AGraphics.Fill.Color;
      AGraphics.DrawRectangle(Rectf(fr.Left,r.Top,fr.Right,r.Top + h));
    end;

    while (Length(s) > 0) and not LineBreak and not ImgBreak do
    begin
      // get next word or till next HTML tag
      TagPos := Pos('<',s);

      if WordWrap then
        SpacePos := Pos(' ',s)
      else
        SpacePos := 0;

      if (Tagpos > 0) and ((SpacePos > TagPos) or (SpacePos = 0)) then
        su := Copy(s,1,TagPos - 1)
      else
      begin
        if SpacePos > 0 then
          su := Copy(s,1,SpacePos)
        else
          su := s;
      end;

      WordLen := Length(su);

      while Pos('&nbsp;',su) > 0 do
      begin
        TAdvUtils.TagReplacestring('&nbsp;',' ',su);
      end;

      while Pos('&lt;',su) > 0 do
      begin
        TAdvUtils.TagReplacestring('&lt;','<',su);
      end;

      while Pos('&gt;',su) > 0 do
      begin
        TAdvUtils.TagReplacestring('&gt;','>',su);
      end;

      //WordLenEx := Length(su);

      if WordLen > 0 then
      begin
        rh := AGraphics.CalculateText(su, RectF(0, 0, 10000, 10000), False, False);
        th := rh.Bottom - rh.Top;

        if isSub and (subh < (th / 4)) then subh := th / 4;
        if isSup and (suph < (th / 4)) then suph := th / 4;

        if th > h then
          h := th;

        StripVal := StripVal + su;

        if Invisible then
          Delete(s,1,WordLen);

        if not Invisible then
        begin
          // draw mode
          if not Calc then
          begin
            if isSup then
              cr.Bottom := cr.Bottom - suph;

            if isSub then
              cr.Bottom := cr.Bottom + subh;

            cr.Bottom := cr.Bottom - imgali;

            rh := AGraphics.CalculateText(su, RectF(0, 0, 10000, 10000), False, False);
            errs := rh.Right - rh.Left;

            if BkColor <> gcNull then
            begin
              AGraphics.Fill.Color := BkColor;
              AGraphics.Stroke.Color := BkColor;
              rh := AGraphics.CalculateText('gh');
              AGraphics.DrawRectangle(RectF(cr.Left, cr.Top, cr.Left + errs, cr.Top + (rh.Bottom - rh.Top)));
            end;

            BKColor := gcNull;

            if isShad then
            begin
              OffsetRectEx(cr,ShadowOffset,ShadowOffset);
              AGraphics.Font.Color := ShadowColor;
              AGraphics.DrawText(cr, su, False, gtaLeading, gtaCenter, gttNone, 0, -1, -1{$IFNDEF LIMITEDGRAPHICSMODE}, False{$ENDIF});
              OffsetRectEx(cr,-ShadowOffset,-ShadowOffset);
              AGraphics.Font.Color := OldFontColor;
            end;

            //errs := cr.Right;

            {$IFDEF DELPHI_LLVM}
            if (su <> '') and (su[length(su)-1] = ' ') and Anchor then
            {$ELSE}
            if (su <> '') and (su[length(su)] = ' ') and Anchor then
            {$ENDIF}
              dsu := su + #0
            else
              dsu := su;

            AGraphics.Font.Color := FontColor;
            AGraphics.DrawText(cr, dsu, False, gtaLeading, gtaCenter, gttNone, 0, -1, -1{$IFNDEF LIMITEDGRAPHICSMODE}, False{$ENDIF});

            cr.Right := cr.Left + errs;

            if Anchor and (Hyperlinks - 1 = FocusLink) then
              FocusAnchor := LastAnchor;

            if Error then
            begin
              AGraphics.Stroke.Color := gcRed;
              AGraphics.Stroke.Kind := gskSolid;
              AGraphics.Stroke.Width := 1;

              l := Round((cr.Left / 2) * 2);
              if (l mod 4)=0 then o := 2 else o := 0;

              while l < cr.Right do
              begin
                ptf1 := PointF(l,r.Bottom + o - 1);
                if o = 2 then o := 0 else o := 2;
                ptf2 := PointF(l + 2, r.Bottom + o - 1);
                AGraphics.DrawLine(ptf1, ptf2);
                Inc(l,2);
              end;
            end;

            cr.Left := cr.Right;
            cr.Right := r.Right;
            cr.Bottom := r.Bottom;
            cr.Top := r.Top;
          end
        else
          begin
            cr := r; //reinitialized each time !

            rh := AGraphics.CalculateText(su, RectF(0, 0, 10000, 10000), False, False);
            cr.Right := cr.Left + (rh.Right - rh.Left);

            // preparations for editing purposes
            if (ypos > cr.Top) and (ypos < cr.bottom) and (xpos > w) then {scan charpos here}
            begin
              //this will get the character pos of the insertion point
              nnfit := 0;
              if nnfit = WordLen then
                InsPoint := InsPoint + WordLen
              else
                InsPoint := InsPoint + nnfit;
            end;

            //end of preparations for editing purposes

            // Calculated text width
            WordWidth := cr.Right - cr.Left;
            w := w + WordWidth;

            //errs := XPos - cr.Left;
            //errs := r.Right - XPos - cr.Left;

            if (XPos - cr.Left  >= w - WordWidth) and (XPos - cr.Left <= w) and Anchor then
            begin
              HotSpot := True;
              if (YPos > cr.Top){ and (YPos < cr.Bottom)} then
              begin
                Anchorval := LastAnchor;
                MouseInAnchor := True;
              end;
            end;
          end;

          LengthFits := (w < r.Right - r.Left - OfsX) or (r.Right - r.Left - OfsX <= WordWidth);

//          if not LengthFits and
//            ((Length(LineText) > 0) and (LineText[Length(LineText)] <> ' ')) then
//            LengthFits := True;

          LineText := LineText + su;

//          if not LengthFits and not Wordwrap then
//          begin
//            LineBreak := true;
//          end;

          if LengthFits or not WordWrap then
          begin
            Res := Res + Copy(s,1,WordLen);

            //if not LengthFits and Calc and (LineText <> su) then
            //  s := '';

            Delete(s,1,WordLen);

            if Length(su) >= WordLen then
            begin

              if Copy(su, WordLen, 1) = ' ' then
              begin
                rh := AGraphics.CalculateText(' ');
                sw := (rh.Right - rh.Left);
              end
              else
                sw := 0;
            end
            else
              sw := 0;
          end
          else
          begin
            LineBreak := True;
            w := w - WordWidth;
          end;
        end;
      end;

      TagPos := Pos('<',s);

      if (TagPos = 1) and (Length(s) <= 2) then
        s := '';

      if not LineBreak and (TagPos = 1) and (Length(s) > 2) then
      begin
        {$IFDEF DELPHI_LLVM}
        if (s[1] = '/') and (Length(s) > 3) then
        {$ELSE}
        if (s[2] = '/') and (Length(s) > 3) then
        {$ENDIF}
        begin
          {$IFDEF DELPHI_LLVM}
          case UpCase(s[2]) of
          {$ELSE}
          case UpCase(s[3]) of
          {$ENDIF}
          'A':begin
                if (not HoverStyle or (Hoverlink = Hyperlinks)) and not Calc then
                begin
                  AGraphics.Font.Style := AGraphics.Font.Style - [TFontStyle.fsUnderline];
                  if Hovercolor <> gcNull then
                    BkColor := HoverColor;
                  if HoverFontColor <> gcNull then
                    FontColor := HoverFontColor;
                end;

                if not Selected then
                  FontColor := OldFontColor;

                Anchor := False;

                if MouseInAnchor then
                begin
                  hr.Bottom := r.Bottom;
                  hr.Right := r.Left + w;
                  if r.Top <> hr.Top then
                  begin
                    hr.Left := r.Left;
                    hr.Top := r.Top;
                  end;

                  HoverRect := hr;
                  MouseLink := HyperLinks;
                  MouseInAnchor := False;
                end;

                if Focuslink = Hyperlinks - 1 then
                begin
                  rr.Right := cr.Left;
                  rr.Bottom := cr.Bottom - ImgAli;
                  rh := AGraphics.CalculateText('gh');
                  rr.Top := rr.Bottom - (rh.Bottom - rh.Top);
                  InflateRectEx(rr,1,0);
                  //if not Calc then AGraphics.DrawFocusRect(rr);
                end;
              end;
          'E':begin
                if not Calc then
                  Error := False;
              end;
          'B':begin
                {$IFDEF DELPHI_LLVM}
                if s[3] <> '>' then
                {$ELSE}
                if s[4] <> '>' then
                {$ENDIF}
                  FontColor := OldFontColor
                else
                  AGraphics.Font.Style := AGraphics.Font.Style - [TFontStyle.fsBold];
              end;
          'S':begin
                {$IFDEF DELPHI_LLVM}
                TagChar := UpCase(s[3]);
                {$ELSE}
                TagChar := UpCase(s[4]);
                {$ENDIF}

                if (TagChar = 'U') then
                begin
                  isSup := False;
                  isSub := False;
                end
                else
                 if (TagChar = 'H') then
                  isShad := False
                 else
                  AGraphics.Font.Style := AGraphics.Font.Style - [TFontStyle.fsStrikeOut];
              end;
          'F':begin
                AGraphics.Font.Name := OldFont.Name;
                AGraphics.Font.Size := OldFont.Size;
                if not Calc and not Selected then
                begin
                  FontColor := OldFontColor;
                  AGraphics.Fill.Color := BGColor;
                end;
              end;
          'H':begin
                if not Calc then
                begin
                  FontColor := hifCol;
                  BkColor := hibCol;
                  AGraphics.Font.Style := hifStyles;
                  AGraphics.Fill.Color := hibCol;
                end;
              end;
          'I':begin
                AGraphics.Font.Style := AGraphics.Font.Style - [TFontStyle.fsItalic];
              end;
          'L':begin
                LineBreak := True;
              end;
          'O':begin
                NewOffsetX := 0;
              end;
          'P':begin
                LineBreak := True;
                if not Calc then
                begin
                  AGraphics.Fill.Color := ParaColor;
                  isPara := false;
                end;
              end;
          'U':begin
                {$IFDEF DELPHI_LLVM}
                if (s[3] <> '>') and (ListIndex > 0) then
                {$ELSE}
                if (s[4] <> '>') and (ListIndex > 0) then
                {$ENDIF}
                  Dec(Listindex)
                else
                  AGraphics.Font.Style := AGraphics.Font.Style - [TFontStyle.fsUnderline];
              end;
          'R':begin
                //EndRotated(Canvas);
              end;
          'Z':Invisible := False;
          end;
        end
        else
        begin
          {$IFDEF DELPHI_LLVM}
          case Upcase(s[1]) of
          {$ELSE}
          case Upcase(s[2]) of
          {$ENDIF}
          'A':begin
                // only do this when at hover position in xpos,ypos
                if (FocusLink = HyperLinks) and not Calc then
                begin
                  rr.Left := cr.Left;
                  rr.Top := cr.Top;
                end;

                Inc(HyperLinks);
                if (not HoverStyle or (Hoverlink = HyperLinks)) and not Calc then
                begin
                  if HyperLinkUnderline then
                    AGraphics.Font.Style := AGraphics.Font.Style + [TFontStyle.fsUnderline];

                  if (Hovercolor <> gcNull) and not Calc then
                    BkColor := HoverColor;

                  if HoverFontColor <> gcNull then
                    FontColor := HoverFontColor;
                end;

                if not Selected and ((HoverFontColor = gcNull) or (HoverLink <> HyperLinks) or not HoverStyle) then
                begin
                  OldFontColor := FontColor;
                  FontColor := URLColor;
                end;

                TagProp := Copy(s,3,Pos('>',s) - 1);  // <A href="anchor">
                Prop := Copy(TagProp,Pos('"',TagProp) + 1,Length(TagProp));
                Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                LastAnchor := Prop;
                Anchor := True;

                hr.Left := w;
                hr.Top := r.Top;
              end;
          'B':begin
                {$IFDEF DELPHI_LLVM}
                TagChar := Upcase(s[2]);
                {$ELSE}
                TagChar := Upcase(s[3]);
                {$ENDIF}
                case TagChar of
                '>': AGraphics.Font.Style := AGraphics.Font.Style + [TFontStyle.fsBold]; // <B> tag
                'R': // <BR> tag
                   begin
                    LineBreak := true;
                    StripVal := StripVal + #13;
                   end;
                'L': if not Blink then
                   FontColor := BlnkColor; // <BLINK> tag
                'O':
                  begin

                    bmpy := 0;
                    Res := Res + Copy(s,1,pos('>',s));
                    if not Calc and not Selected then
                    begin
                      TagProp := Uppercase(Copy(s,6,pos('>',s)-1));

                      if (Pos('BACKGROUND',TagProp) > 0) then
                      begin
                        Prop := Copy(TagProp,Pos('BACKGROUND',TagProp)+10,Length(TagProp));
                        Prop := Copy(Prop,Pos('"',Prop)+1,Length(prop));
                        Prop := Copy(Prop,1,Pos('"',Prop)-1);

                        if Pos('IDX:', UpperCase(Prop)) > 0 then
                          begin
                            Delete(Prop, 1, 4);
                            (*
                            if Assigned(FImages) and (IStrToInt(Prop) < FImages.Count) then
                              begin
                                IMGSize.X := MulDiv(FImages.Width, GetDeviceCaps(AGraphics.Handle, LOGPIXELSX), 96);
                                IMGSize.Y := MulDiv(FImages.Height, GetDeviceCaps(AGraphics.Handle, LOGPIXELSY), 96);

                                if not Calc and not Print then
                                  FImages.Draw(Canvas, CR.Left, CR.Top, IStrToInt(Prop), True);

                                if not Calc and Print then
                                  begin
                                    CR.Right := CR.Left + Round(resfactor * FImages.Width);
                                    CR.Bottom := CR.Top + Round(resfactor * FImages.Height);

                                    ABitmap := TBitmap.Create;
                                    FImages.GetBitmap(IStrToInt(Prop), ABitmap);
                                    PrintBitmap(Canvas, CR, ABitmap);
                                    ABitmap.Free;
                                    CR := r;
                                  end;
                              end;
                            *)
                          end;

                        bmp := nil;
                        bmpcreated := false;

                        if (Pos(':',Prop) = 0) and Assigned(PictureContainer) then
                        begin
                          bmp := PictureContainer.FindBitmap(Prop);
                        end;

                        {$IFNDEF WEBLIB}
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
                        {$ENDIF}
                        {$IFDEF WEBLIB}
                        if (Pos('URL://',Uppercase(Prop)) > 0)  then
                        begin
                          Delete(Prop,1,6);
                          bmp := TAdvBitmap(TAdvBitmap.CreateFromURL(Prop));
                          bmpcreated := true;
                        end;
                        {$ENDIF}

                        if Assigned(bmp) then
                        begin
                          if not IsBitmapEmpty(bmp) and (bmp.Width > 0) and (bmp.Height > 0) then
                          begin
                            // do the tiling here
                            //hrgn := CreateRectRgn(fr.left, fr.top, fr.right,fr.bottom);
                            //SelectClipRgn(AGraphics.Handle, hrgn);
                            while (bmpy < fr.bottom-fr.top) do
                            begin
                              bmpx := 0;
                              while (bmpx < fr.right - fr.left) do
                              begin
                                AGraphics.DrawBitmap(RectF(Round(fr.left+bmpx),Round(fr.top+bmpy), Round(fr.Left + bmpx) + bmp.Width,Round(fr.top+bmpy) + bmp.Height), BitmapToDrawBitmap(bmp));
                                //AGraphics.Draw(fr.left+bmpx,fr.top+bmpy,bmp);
                                bmpx := bmpx + bmp.width;
                              end;
                              bmpy := bmpy + bmp.height;
                            end;
                            //SelectClipRgn(AGraphics.handle, 0);
                            //DeleteObject(hrgn);
                          end;

                          if bmpcreated then
                            bmp.Free;
                        end; //end of bmp <> nil
                      end; //end of background

                      if (Pos('BGTOPLEFT', TagProp) > 0) then
                        begin
                          Prop := Copy(TagProp, Pos('BGTOPLEFT', TagProp) + 10, Length(TagProp));
                          Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                          Prop := Copy(Prop, 1, Pos('"', Prop) - 1);

                          bmp := nil;

                          if (Pos(':', Prop) = 0) and Assigned(PictureContainer) then
                            begin
                              bmp := PictureContainer.FindBitmap(Prop);
                            end;

                          if Assigned(bmp) then
                          begin
                            if not IsBitmapEmpty(bmp) and (bmp.Width > 0) and (bmp.Height > 0) then
                            begin
                              AGraphics.DrawBitmap(RectF(Round(fr.Left), Round(fr.Top + bmpy),Round(fr.Left) + bmp.Width ,Round(fr.Top) + bmp.Height), BitmapToDrawBitmap(bmp));
                            end;
                          end;
                        end; //end of bgtopleft

                      if (Pos('BGTOPRIGHT', TagProp) > 0) then
                        begin
                          Prop := Copy(TagProp, Pos('BGTOPRIGHT', TagProp) + 10, Length(TagProp));
                          Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                          Prop := Copy(Prop, 1, Pos('"', Prop) - 1);

                          bmp := nil;

                          if (Pos(':', Prop) = 0) and Assigned(PictureContainer) then
                            begin
                              bmp := PictureContainer.FindBitmap(Prop);
                            end;


                          if Assigned(bmp) then
                          begin
                            if not IsBitmapEmpty(bmp) and (bmp.Width > 0) and (bmp.Height > 0) then
                            begin
                              AGraphics.DrawBitmap(RectF(Round(fr.Right) - bmp.Width, Round(fr.Top + bmpy), Round(fr.Right), Round(fr.Top) + bmp.Height ), BitmapToDrawBitmap(bmp));
                            end;
                          end;
                        end; //end of bgtopright

                      if TAdvUtils.VarPos('BGCOLOR',TagProp,TagPos) > 0 then
                      begin
                        Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                        Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                        Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                        NewColor := gcNull;

                        if Length(Prop) > 0 then
                        begin
                          {$IFDEF DELPHI_LLVM}
                          if Prop[0] = '#' then
                          {$ELSE}
                          if Prop[1] = '#' then
                          {$ENDIF}
                            NewColor := TAdvGraphics.HTMLToColor(Prop)
                          else
                          begin
                            {$IFDEF WEBLIB}
                            NewColor := TAdvGraphics.TextToColor(LowerCase(prop));
                            {$ENDIF}
                            {$IFNDEF WEBLIB}
                            NewColor := TAdvGraphics.TextToColor(AnsiLowerCase(prop));
                            {$ENDIF}
                          end;
                        end;

                        if TAdvUtils.VarPos('BGCOLORTO',TagProp,TagPos) > 0 then
                        begin
                          Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                          Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                          Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                          Prop := 'H';
                          if TAdvUtils.VarPos('DIR',TagProp,TagPos) > 0 then
                          begin
                            Prop := Copy(TagProp,TagPos + 3,Length(TagProp));
                            Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                            Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                          end;

                          AGraphics.Stroke.Color := Newcolor;
                          //DrawHTMLGradient(Canvas,NewColor,NewColorTo,clNone,64,Rect(fr.left,fr.top,fr.right,fr.bottom),Prop = 'H');
                          //AGraphics.Brush.Style := bsClear
                        end
                        else
                        begin
                          BGColor := AGraphics.Fill.Color;
                          AGraphics.Fill.Color := NewColor;
                          PenColor:=AGraphics.Stroke.Color;
                          AGraphics.Stroke.Color := Newcolor;
                          AGraphics.DrawRectangle(Rectf(fr.left - 2,fr.top,fr.right,fr.bottom));
                          AGraphics.Stroke.Color := PenColor;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
          'E':begin
                if not Calc then
                  Error := True;
              end;
          'C':begin
                TagProp := Copy(s,9,pos('>',s)-1);
                ControlType := '';
                ControlID := '';
                ControlValue := '';
                ControlWidth := '';
                ControlHeight := '';
                ControlProp := '';
                ControlLen := '';
                ParseControl(TagProp,ControlType,ControlID,ControlValue,ControlWidth,ControlHeight,ControlProp,ControlLen);

                ch := 25;

                if ControlHeight <> '' then
                begin
                  val(ControlHeight,ch,err);
                  if err <> 0 then
                    ch := 25;
                end;

                if (ControlWidth <> '') or (ControlType = 'CHECK') or (ControlType = 'RADIO') then
                begin
                  val(ControlWidth,Indent,err);

                  if err = 0 then
                  begin
                    IMGSize.x := Indent;
                    IMGSize.y := Round(AGraphics.CalculateTextHeight('gh')) + 10;
                  end;

                  if (ControlType = 'CHECK') or (ControlType = 'RADIO') then
                    h := 16
                  else
                    h := 23;

                  if (ch > 25) then
                    h := ch - 2;

                  if not Calc then
                  begin
                    if (ControlType = 'EDIT') or (ControlType = 'PASSWORD') or (ControlType = 'MASK') then
                    begin
                    end;

                    if ControlType = 'COMBO' then
                    begin
                    end;

                    if ControlType = 'CHECK' then
                    begin
                      IMGSize.x := Round(16 * ResFactor) + 2;
                      IMGSize.y := IMGSize.x;
                      Indent := IMGSize.x;
                      ir := RectF(cr.Left + 1, cr.Top + ((cr.Bottom - cr.Top) - (ImgSize.Y - 2)) / 2, cr.Left + IMGSize.X - 1, cr.Top + ((cr.Bottom - cr.Top) - (ImgSize.Y - 2)) / 2 + (IMGSize.Y - 2));
                      ir := RectF(Round(ir.Left), Round(ir.Top), Round(ir.Right), Round(ir.Bottom));
                      AGraphics.DrawCheckBox(ir, UpperCase(ControlValue) = 'TRUE', (Down and (DownID = ControlID)), True);
                    end;

                    if ControlType = 'RADIO' then
                    begin
                      IMGSize.x := Round(16 * ResFactor) + 2;
                      IMGSize.y := IMGSize.x;
                      Indent := IMGSize.x;
                      ir := RectF(cr.Left + 1, cr.Top + ((cr.Bottom - cr.Top) - (ImgSize.Y - 2)) / 2, cr.Left + IMGSize.X - 1, cr.Top + ((cr.Bottom - cr.Top) - (ImgSize.Y - 2)) / 2 + (IMGSize.Y - 2));
                      ir := RectF(Round(ir.Left), Round(ir.Top), Round(ir.Right), Round(ir.Bottom));
                      AGraphics.DrawRadioButton(ir, UpperCase(ControlValue) = 'TRUE', (Down and (DownID = ControlID)), True);
                    end;

                    if ControlType = 'BUTTON' then
                    begin
                      IMGSize.y := 22;
                      ir := RectF(cr.Left + 2,cr.Top + ((cr.Bottom - cr.Top) - IMGSize.Y) / 2,cr.Left + Indent -2, cr.Top + ((cr.Bottom - cr.Top) - IMGSize.Y) / 2+ IMGSize.Y);
                      FHot := PtInRectEx(ir, PointF(XPos, YPos));
                      ir := RectF(Round(ir.Left), Round(ir.Top), Round(ir.Right), Round(ir.Bottom));
                      AGraphics.DrawButton(ir, Down and (DownID = ControlID), FHot, True);
                      InflateRectEx(ir,-2,-2);
                      AGraphics.DrawText(ir, ControlValue, False, gtaCenter, gtaCenter);
                    end;
                  end;

                  if (ControlType = 'BUTTON') then
                    IMGSize.y := 22;

                  if (ControlType = 'COMBO') then
                     IMGSize.y := 25;

                  if (ControlType = 'CHECK') or (ControlType = 'RADIO') then
                  begin
                    IMGSize.x := Round(16 * ResFactor) + 2;
                    IMGSize.y := IMGSize.X;
                  end;

                  if ((XPos - r.Left > w) and (XPos - r.Left < w + IMGSize.x) and
                     (YPos > cr.Top) and (YPos < cr.Top + IMGSize.Y)) or
                     (CheckHotSpot and (CID = ControlID)) then
                  begin
                    ImageHotSpot := True;
                    AnchorVal := 'ctrl';
                    AltImg := ImgIdx;

                    ir.Left := r.left + w;
                    ir.Right := ir.Left + ImgSize.X;
                    ir.Top := cR.Top;
                    ir.Bottom := cr.top + ImgSize.Y;

                    ControlRect := ir;
                    CV := ControlValue;
                    CID := ControlID;
                    CT := ControlType;
                  end;

                  if (w + IMGSize.x > r.Right-r.Left) and
                     (IMGSize.x < r.Right - r.Left) then
                  begin
                    ImgBreak := True;
                  end
                  else
                  begin
                    w := w + IMGSize.x;
                    cr.left := cr.left + IMGSize.x;
                    if IMGSize.y > h then
                      h := IMGSize.y;
                  end;
                end;
              end;
          'H':begin
                {$IFDEF DELPHI_LLVM}
                case Upcase(s[2]) of
                {$ELSE}
                case Upcase(s[3]) of
                {$ENDIF}
                'R':
                begin
                  LineBreak := True;
                  if not Calc then
                  begin
                    TagProp := Copy(s,4,Pos('>',s) - 1);

                    NewColor := gcBlack;

                    if TAdvUtils.VarPos('COLOR',Uppercase(TagProp),TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',prop)-1);

                      if Length(Prop) > 0 then
                      begin
                        {$IFDEF DELPHI_LLVM}
                        if Prop[0] = '#' then
                        {$ELSE}
                        if Prop[1] = '#' then
                        {$ENDIF}
                          NewColor := TAdvGraphics.HTMLToColor(Prop)
                        else
                        begin
                          {$IFDEF WEBLIB}
                          NewColor := TAdvGraphics.TextToColor(LowerCase(prop));
                          {$ENDIF}
                          {$IFNDEF WEBLIB}
                          NewColor := TAdvGraphics.TextToColor(AnsiLowerCase(prop));
                          {$ENDIF}
                        end;
                      end;
                    end;

                    AGraphics.Stroke.Kind := gskSolid;
                    Pencolor := AGraphics.Stroke.color;
                    AGraphics.Stroke.Color := Newcolor;
                    ptf1 := PointF(r.Left, cr.Bottom  + 1);
                    ptf2 := PointF(r.Right, cr.Bottom + 1);
                    AGraphics.DrawLine(ptf1,ptf2);
                    AGraphics.Stroke.Color := Pencolor;
                  end;
                end;
                'I':
                begin
                  if not Calc then
                  begin
                    hifCol := FontColor;
                    hibCol := BkColor;
                    hifStyles := AGraphics.Font.Style;
                    BkColor := HighlightColor;
                    FontColor := HighlightTextColor;
                  end;
                  AGraphics.Font.Style := HighlightTextStyle;
                end;
                end;
              end;
          'I':begin
                {$IFDEF DELPHI_LLVM}
                TagChar := Upcase(s[2]);
                {$ELSE}
                TagChar := Upcase(s[3]);
                {$ENDIF}

                if TagChar = '>' then // <I> tag
                  AGraphics.Font.Style := AGraphics.Font.Style + [TFontStyle.fsItalic]
                else
                if TagChar = 'N' then  // <IND> tag
                begin
                  TagProp := Copy(s,3,pos('>',s) - 1);

                  Prop := Copy(TagProp,TAdvUtils.ipos('x',TagProp) + 2,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                  val(Prop,indent,err);
                  if err = 0 then
                  begin
                    if indent > w then
                     begin
                       w := Indent;
                       cr.left := fr.left + Indent;
                     end;
                  end;
                end
                else
                  if TagChar = 'M' then
                  begin
                    imgalign := 0;
                    imgoffs := 0;
                    imgfloat := fsNone;
                    imgth := Round(AGraphics.CalculateTextHeight('gh'));
                    inc(ImgIdx);

                    ir := cr;

                    TagProp := Copy(s,3,Pos('>',s) - 1);
                    Prop := Copy(TagProp,Pos('SRC',Uppercase(TagProp)) + 4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    TagProp := Uppercase(TagProp);

                    if (Pos('ALT',TagProp) > 0) and (AltImg = ImgIdx) then
                    begin
                      Prop := Copy(TagProp,Pos('ALT',TagProp) + 4,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                    end;

                    TagWidth := 0;
                    TagHeight := 0;

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

                    if Pos('ALIGN',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,TAdvUtils.ipos('ALIGN',TagProp) + 6,Length(TagProp));
                      Tagp := Copy(Tagp,pos('"',Tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,pos('"',Tagp) - 1);

                      if TagP = 'MIDDLE' then
                        imgalign := 1;

                      if TagP = 'TOP' then
                        imgalign := 2;
                    end;

                    if Pos('FLOAT',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,TAdvUtils.ipos('FLOAT',TagProp) + 6,Length(TagProp));
                      Tagp := Copy(Tagp,pos('"',Tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,pos('"',Tagp) - 1);

                      if TagP = 'LEFT' then
                      begin
                        imgfloat := fsLeft;
                        ir.left := fr.Left;
                      end;

                      if TagP = 'RIGHT' then
                        imgfloat := fsRight;
                    end;

                    IMGSize.x := 0;
                    IMGSize.y := 0;

                    if Pos('IDX:',Uppercase(Prop)) > 0 then
                    begin
                      Delete(Prop,1,4);
                      {$IFDEF CMNLIB}
                      if Assigned(ImageList) and (IStrToInt(Prop) < ImageList.Count) then
                      begin
                        {$IFDEF FMXLIB}
                        {$ENDIF}
                        {$IFDEF CMNLIB}
                        {$IFDEF VCLLIB}
                        IMGSize.x := MulDiv(ImageList.Width,GetDeviceCaps(AGraphics.Canvas.Handle,LOGPIXELSX),96);
                        IMGSize.y := MulDiv(ImageList.Height,GetDeviceCaps(AGraphics.Canvas.Handle,LOGPIXELSY),96);
                        {$ENDIF}
                        {$IFDEF LCLLIB}
                        IMGSize.x := ImageList.Width;
                        IMGSize.y := ImageList.Height;
                        {$ENDIF}

                        if (imgth > ImageList.Height) then
                        begin
                          if imgalign = 0 then // bottom
                            imgoffs := (imgth - ImageList.Height);

                          if imgalign = 1 then // center
                            imgoffs := (imgth - ImageList.Height) div 2;
                        end;

                        if not Calc and not Print then
                        begin
                          crc := ConvertToRect(ir);
                          ImageList.Draw(AGraphics.Canvas,crc.Left,crc.Top + imgoffs,IStrToInt(Prop),True);
                        end;
                        {$ENDIF}
                      end;
                      {$ENDIF}
                    end;

                    bmp := nil;
                    bmpcreated := false;

                    if (Pos(':',Prop) = 0) and Assigned(PictureContainer) then
                    begin
                      bmp := PictureContainer.FindBitmap(Prop);
                    end;

                    {$IFNDEF WEBLIB}
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
                    {$ENDIF}
                    {$IFDEF WEBLIB}
                    if (Pos('URL://',Uppercase(Prop)) > 0)  then
                    begin
                      Delete(Prop,1,6);
                      bmp := TAdvBitmap(TAdvBitmap.CreateFromURL(Prop));
                      bmpcreated := true;
                    end;
                    {$ENDIF}

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

                          if imgfloat = fsRight then
                            ir.Left := r.Right - imgw;

                          if not Calc {and not Print} then
                          begin
                            if (TagWidth > 0) or (TagHeight > 0) then
                            begin
                              AGraphics.DrawBitmap(RectF(Round(ir.Left),Round(ir.Top),Round(ir.Left) + imgw,Round(ir.Top) + imgh), BitmapToDrawBitmap(bmp));
                            end
                            else
                            if (TagWidth > 0) and (TagHeight = 0) then
                            begin
                              AGraphics.DrawBitmap(RectF(Round(ir.Left),Round(ir.Top),Round(ir.Left) + TagWidth,Round(ir.Top) + TagHeight),BitmapToDrawBitmap(bmp));
                            end
                            else
                            begin
                              AGraphics.DrawBitmap(RectF(Round(ir.Left),Round(ir.Top),Round(ir.Left) + bmp.Width, Round(ir.Top) + bmp.Height),BitmapToDrawBitmap(bmp));
                            end;
                          end;

                          if (TagWidth > 0) or (TagHeight > 0) then
                          begin
                            IMGSize.x := imgw;
                            IMGSize.y := imgh;
                          end
                          else
                          begin
                            IMGSize.x := bmp.Width;
                            IMGSize.y := bmp.Height;
                          end;
                        end;

                        if bmpcreated then
                          bmp.Free;
                      end;

                    if (XPos - r.Left > w) and (XPos - r.Left < w + IMGSize.x) and
                       (YPos > cr.Top) and (YPos < cr.Top + IMGSize.Y) and Anchor then
                    begin
                      ImageHotSpot := True;
                      AnchorVal := LastAnchor;
                      AltImg := ImgIdx;
                    end;

                    if (w + IMGSize.x > r.Right-r.Left) and
                       (IMGSize.x < r.Right - r.Left) then
                    begin
                      ImgBreak := True;
                    end
                    else
                      begin
                        w := w + IMGSize.x;
                        cr.left := cr.left + IMGSize.x;
                        if IMGSize.y > h then
                          h := IMGSize.y;
                      end;

                    if Pos('ALIGN',TagProp) > 0 then
                    begin
                      if Pos('"TOP',TagProp) > 0 then
                      begin
                        rh := AGraphics.CalculateText('gh');
                        ImgAli := h - (rh.Bottom - rh.Top);
                      end
                      else
                      begin
                        if Pos('"MIDDLE',TagProp) > 0 then
                        begin
                          rh := AGraphics.CalculateText('gh');
                          ImgAli := (h - (rh.Bottom - rh.Top)) / 2;
                        end;
                      end;
                    end;

                    if (Pos('WRAP',TagProp) > 0) then
                    begin
                      rh := AGraphics.CalculateText('gh');
                      h := rh.Bottom - rh.Top;
                      ImgAli := 0;
                    end;

                    if imgfloat = fsLeft then
                    begin
                      rh := AGraphics.CalculateText('gh');
                      ImgAli := h - (rh.Bottom - rh.Top);
                      floatrect.left := imgw;
                      floatrect.bottom := imgh;
                      if not calc then
                      begin
                        rh := AGraphics.CalculateText('gh');
                        h := (rh.Bottom - rh.Top);
                      end;
                    end;

                    if imgfloat = fsRight then
                    begin
                      rh := AGraphics.CalculateText('gh');
                      ImgAli := h - (rh.Bottom - rh.Top);
                      floatrect.right := imgw;
                      floatrect.bottom := imgh;
                      if not calc then
                      begin
                        rh := AGraphics.CalculateText('gh');
                        h := rh.Bottom - rh.Top;
                      end;
                      w := fr.Right;
                    end;

                  end;
                end;
          'L':begin
                NewColor := gcNull;
                LType := '';

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
                  if TAdvUtils.VarPos('COLOR',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);

                    if Length(Prop) > 0 then
                    begin
                      {$IFDEF DELPHI_LLVM}
                      if Prop[0] = '#' then
                      {$ELSE}
                      if Prop[1] = '#' then
                      {$ENDIF}
                        NewColor := TAdvGraphics.HTMLToColor(Prop)
                      else
                        NewColor := TAdvGraphics.TextToColor(prop);
                    end;
                  end;
                end;

                w := w + 12 * ListIndex;
                if Linkbreak then
                  Imgbreak := True else Linkbreak := True;

                cr.left := cr.left + 12 * (ListIndex - 1);

                if not calc and not Invisible then
                begin
                  if LType = '' then
                  begin
                    if (NewColor <> gcNull) then
                    begin
                      ColL := AGraphics.Font.Color;
                      FontColor := NewColor;
                    end;
                    Prop := AGraphics.Font.Name;
                    AGraphics.Font.Name := 'Symbol';

                    if Odd(ListIndex) then
                      AGraphics.DrawText(cr, '·', False, gtaLeading, gtaCenter, gttNone, 0, -1, -1{$IFNDEF LIMITEDGRAPHICSMODE}, False{$ENDIF})
                    else
                      AGraphics.DrawText(cr, 'o', False, gtaLeading, gtaCenter, gttNone, 0, -1, -1{$IFNDEF LIMITEDGRAPHICSMODE}, False{$ENDIF});

                    AGraphics.Font.Name := prop;
                    if NewColor <> gcNull then
                      FontColor := ColL;
                  end
                  else
                  begin
                    ColB := gcNull;

                    if LTYPE = 'SQUARE' then
                    begin
                      ColB := AGraphics.Fill.Color;
                      AGraphics.Stroke.Color := NewColor;
                      AGraphics.Fill.Color := NewColor;
                      AGraphics.DrawRectangle(RectF(cr.Left, cr.Top + 2, cr.Left + 8, cr.Top + 10));
                      AGraphics.Fill.Color := ColB;
                    end;

                    if LTYPE = 'CIRCLE' then
                    begin
                      ColB := AGraphics.Fill.Color;
                      AGraphics.Stroke.Color := NewColor;
                      AGraphics.Fill.Color := NewColor;
                      AGraphics.DrawEllipse(RectF(cr.Left, cr.Top + 2, cr.Left + 8, cr.Top + 10));
                      AGraphics.Fill.Color := ColB;
                    end;
                  end;
                end;
                cr.Left := cr.Left + 12;
              end;
          'U':begin
                {$IFDEF DELPHI_LLVM}
                if s[2] <> '>' then
                {$ELSE}
                if s[3] <> '>' then
                {$ENDIF}
                begin
                  Inc(ListIndex);
                  LineBreak := true;
                end
                else
                  AGraphics.Font.Style := AGraphics.Font.Style + [TFontStyle.fsUnderline];
              end;
          'O':begin
                {$IFDEF DELPHI_LLVM}
                TagChar := Upcase(s[2]);
                {$ELSE}
                TagChar := Upcase(s[3]);
                {$ENDIF}
                if TagChar = 'F' then  // <OFS> tag
                begin
                  TagProp := Copy(s,3,pos('>',s) - 1);
                  Prop := Copy(TagProp,TAdvUtils.ipos('x',TagProp) + 2,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                  val(Prop,NewOffsetX,err);
                  cr.Left := NewOffsetX;
                  w := NewOffsetX;
                end
              end;
          'P':begin
                if (TAdvUtils.VarPos('>',s,TagPos)>0) then
                begin
                  TagProp := Uppercase(Copy(s,3,TagPos-1));

                  if TAdvUtils.VarPos('ALIGN',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);

                    if Pos('RIGHT',Prop) > 0 then Align := taRightJustify;
                    if Pos('LEFT',Prop) > 0 then Align := taLeftJustify;
                    if Pos('CENTER',Prop) > 0 then Align := taCenter;
                  end;

                  if TAdvUtils.VarPos('INDENT',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);
                    PIndent := IStrToInt(Prop);
                  end;

                  if TAdvUtils.VarPos('FLOAT',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);

                    if Prop = 'LEFT' then
                      txtfloat := fsLeft;

                    if Prop = 'RIGHT' then
                      txtfloat := fsRight;

                    floatpt := Point(Round(r.Left), Round(r.Top));
                  end;

                  if TAdvUtils.VarPos('BGCOLOR',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    NewColor := gcNull;

                    if Length(Prop) > 0 then
                    begin
                      {$IFDEF DELPHI_LLVM}
                      if Prop[0] = '#' then
                      {$ELSE}
                      if Prop[1] = '#' then
                      {$ENDIF}
                        NewColor := TAdvGraphics.HTMLToColor(Prop)
                      else
                        NewColor := TAdvGraphics.TextToColor(prop);
                    end;

                    if TAdvUtils.VarPos('BGCOLORTO',TagProp,TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                      if not Calc then
                      begin
                        isPara := True;
                        AGraphics.Stroke.Color := Newcolor;

                        //DrawHTMLGradient(Canvas,NewColor,NewColorTo,clNone,64,Rect(fr.left,r.top,fr.right,r.bottom+2),true);
                        //AGraphics.Brush.Style := bsClear
                      end;
                    end
                    else
                    begin
                      if not Calc then
                      begin
                        isPara := True;
                        paracolor := AGraphics.Fill.Color;
                        //if AGraphics.Brush.Style = bsClear then ParaColor := clNone;
                        AGraphics.Fill.color := NewColor;
                        PenColor := AGraphics.Stroke.Color;
                        AGraphics.Stroke.Color := Newcolor;
                        AGraphics.DrawRectangle(RectF(fr.left,r.top,fr.right,r.bottom));
                      end;
                    end;
                  end;
                end;
            end;
        'F':begin
              if (TAdvUtils.VarPos('>',s,TagPos)>0) then
              begin
                TagProp := UpperCase(Copy(s,6,TagPos-6));

                if (TAdvUtils.VarPos('FACE',TagProp,TagPos) > 0) then
                begin
                  Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                  Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos('"',prop)-1);
                  AGraphics.Font.Name := Prop;
                end;

                if (TAdvUtils.VarPos(' COLOR',TagProp,TagPos) > 0) and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',prop)+1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',prop)-1);
                  if AGraphics.Fill.Color <> gcNull then
                    OldFontColor := AGraphics.Font.Color
                  else
                    OldFontColor := gcBlack;

                  if Length(Prop) > 0 then
                  begin
                    {$IFDEF DELPHI_LLVM}
                    if Prop[0] = '#' then
                    {$ELSE}
                    if Prop[1] = '#' then
                    {$ENDIF}
                      FontColor := TAdvGraphics.HTMLToColor(Prop)
                    else
                      FontColor := TAdvGraphics.TextToColor(Prop);
                  end;
                end;

                if (TAdvUtils.VarPos('BGCOLOR',TagProp,TagPos)>0) and not Calc and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+7,Length(TagProp));
                  Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos('"',prop)-1);

                  if Length(Prop) > 0 then
                  begin
                    {$IFDEF DELPHI_LLVM}
                    if Prop[0] = '#' then
                    {$ELSE}
                    if Prop[1] = '#' then
                    {$ENDIF}
                      BkColor := TAdvGraphics.HTMLToColor(Prop)
                    else
                      BkColor := TAdvGraphics.TextToColor(prop);
                  end;

                end;

                if (TAdvUtils.VarPos('SIZE',TagProp,TagPos)>0) then
                begin
                  Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                  Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                  Prop := Copy(Prop,Pos('"',Prop)+1,Length(Prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                  case IStrToInt(Prop) of
                  1: TAdvUtils.SetFontSize(AGraphics.Font, 8);
                  2: TAdvUtils.SetFontSize(AGraphics.Font, 10);
                  3: TAdvUtils.SetFontSize(AGraphics.Font, 12);
                  4: TAdvUtils.SetFontSize(AGraphics.Font, 14);
                  5: TAdvUtils.SetFontSize(AGraphics.Font, 16);
                  else
                    TAdvUtils.SetFontSize(AGraphics.Font, IStrToInt(Prop));
                  end;
                end;
              end;
            end;
        'S':begin
              {$IFDEF DELPHI_LLVM}
              TagChar := Upcase(s[2]);
              {$ELSE}
              TagChar := Upcase(s[3]);
              {$ENDIF}

              if TagChar = '>' then
                AGraphics.Font.Style := AGraphics.Font.Style + [TFontStyle.fsStrikeOut]
              else
              begin
                if TagChar = 'H' then
                  isShad := True
                else
                begin
                  if TAdvUtils.ipos('<SUB>',s)=1 then
                    isSub := True
                  else
                    if TAdvUtils.ipos('<SUP>',s)=1 then
                      isSup := True;
                end;
              end;
            end;
        'R':begin
              TagProp := Copy(s,3,pos('>',s)-1);
              prop := Copy(TagProp,TAdvUtils.ipos('a',TagProp)+2,Length(TagProp));
              prop := Copy(prop,pos('"',prop)+1,Length(prop));
              prop := Copy(prop,1,pos('"',prop)-1);
              //StartRotated(Canvas,indent);
            end;
        'Z':Invisible := True;
        end;
      end;

      if (TAdvUtils.VarPos('>',s,TagPos)>0) and not ImgBreak then
      begin
        Res := Res + Copy(s,1,TagPos);
        Delete(s,1,TagPos);
      end
      else
        if not Imgbreak then
          Delete(s,1,Length(s));
    end;
  end;

    w := w - sw;

    if w > xsize then
      xsize := w + 2;

    if (FocusLink = Hyperlinks - 1) and Anchor and not Calc then
    begin
      rr.Right := cr.Left;
      rr.Bottom := cr.Bottom;
      InflateRectEx(rr,1,0);
      //if not Calc then
      //  AGraphics.DrawFocusRect(rr);
      rr.Left := r.Left + 1;
      rr.Top := rr.Bottom;
    end;

    Result := Res;
  end;

begin
  Anchor := False;
  Error := False;
  OldFont := TAdvGraphicsFont.Create;
  OldFont.AssignSource(AGraphics.Font);
  OldFontColor := AGraphics.Font.Color;
  DrawFont := TAdvGraphicsFont.Create;
  DrawFont.AssignSource(AGraphics.Font);
  DrawFontColor := AGraphics.Font.Color;
  CalcFont := TAdvGraphicsFont.Create;
  CalcFont.AssignSource(AGraphics.Font);
  CalcFontColor := AGraphics.Font.Color;
  OldDrawfont := TAdvGraphicsFont.Create;
  OldDrawFont.AssignSource(AGraphics.Font);
  OldDrawFontColor := AGraphics.Font.Color;
  OldCalcFont := TAdvGraphicsFont.Create;
  OldCalcFont.AssignSource(AGraphics.Font);
  OldCalcFontColor := AGraphics.Font.Color;
  BlnkColor := AGraphics.Fill.color;
  OldPenColor := AGraphics.Stroke.Color;
  BGColor := gcNull;
  BkColor := gcNull;
  ParaColor := gcNull;
  isPara := False;
  isShad := False;
  Invisible := False;

  ControlRect := RectF(0,0,0,0);
  CV := '';
  CT := '';
  if not CheckHotSpot then
    CID := '';

  OfsX := 0;
  NewOfsX := 0;

  txtfloat := fsNone;
  floatrect := Rect(0,0,0,0);

  Result := False;

  r := fr;

  Align := taLeftJustify;
  PIndent := 0;

  XSize := 0;
  YSize := 0;
  HyperLinks := 0;
  HlCount := 0;
  ListIndex := 0;
  LiCount := 0;
  StripVal := '';
  FocusAnchor := '';
  MouseLink := -1;
  MouseInAnchor := False;

  ImgIdx := 0;
  AltImg := -1;

  if Pos('&',s) > 0 then
  begin
    s :=  TAdvUtils.UnFixMarkup(s, true);
    repeat
      Foundtag := False;
      if TAdvUtils.TagReplacestring('&amp;','&',s) then Foundtag := True;
    until not Foundtag;
  end;

  s := DBTagStrip(s);
  s := CRLFStrip(s,True);

  InsPoint := 0;
  LineCount := 0;

  while Length(s) > 0 do
  begin
    //calculate part of the HTML text fitting on the next line
    OldFont.AssignSource(OldCalcFont);
    OldFontColor := OldCalcFontColor;
    AGraphics.Font.AssignSource(CalcFont);
    FontColor := CalcFontColor;
    OldAnchor := Anchor;
    OldAnchorVal := LastAnchor;
    suph := 0;
    subh := 0;
    imgali := 0;
    isSup := False;
    isSub := False;
    HotSpot := False;
    ImageHotSpot := False;
    HTMLWidth := 0;
    rh := AGraphics.CalculateText('gh');
    HtmlHeight := rh.Bottom - rh.Top;
    txtHeight := HtmlHeight;

    OldImgIdx := ImgIdx;

    s := Trim(s);

    txtfloatbefore := txtfloat;

    su := HTMLDrawLine(AGraphics,s,r,True,HtmlWidth,HtmlHeight,subh,suph,imgali,Align,PIndent,XPos,YPos,HotSpot,ImageHotSpot,ofsx,newofsx, floatrect,txtfloat);
    Inc(LineCount);

    Anchor := OldAnchor;
    LastAnchor := OldAnchorVal;

    CalcFont.AssignSource(AGraphics.Font);
    CalcFontColor := AGraphics.Font.Color;
    OldCalcFont.AssignSource(OldFont);
    OldCalcFontColor := OldFontColor;

    HTMLHeight := HTMLHeight + LineSpacing;

    dr := r;

    case Align of
    taCenter:if (r.right - r.left - htmlwidth > 0) then
               dr.left := r.left+((r.right - r.left - htmlwidth) / 2);
    taRightJustify:if r.right - htmlwidth > r.left then
                       dr.left := r.right - htmlwidth;
    end;

    dr.Left := dr.Left + PIndent;
    dr.Right := dr.Right + PIndent;

    dr.Bottom := dr.Top + HtmlHeight + Subh + Suph;

    if not CheckHeight then
    begin
      OldFont.AssignSource(OldDrawFont);
      OldFontColor := OldDrawFontColor;
      AGraphics.Font.AssignSource(DrawFont);
      FontColor := DrawFontColor;

      HyperLinks := HlCount;
      ListIndex := LiCount;
      ImgIdx := OldImgIdx;

      HTMLDrawLine(AGraphics,su,dr,CheckHotSpot,HtmlWidth,HtmlHeight,subh,suph,ImgAli,Align,PIndent,XPos,YPos,HotSpot,ImageHotspot,ofsx,newofsx, floatrect,txtfloat);

      HlCount := HyperLinks;
      LiCount := ListIndex;

      r.left := fr.Left + floatrect.left;

      r.right := fr.Right - floatrect.right;

      if dr.Top > floatrect.bottom then
      begin
        floatrect.Left := 0;
        floatrect.Right := 0;
        r.Left := fr.Left;
      end;

      rh := AGraphics.CalculateText('gh');

      if (HotSpot and
         (YPos > dr.Bottom - ImgAli - (rh.Bottom - rh.Top)) and
         (YPos < dr.Bottom - ImgAli)) or ImageHotSpot then
      begin
        Result := True;
      end;

      ofsx := newofsx;

      DrawFont.AssignSource(AGraphics.Font);
      DrawFontColor := FontColor;
      OldDrawFont.AssignSource(OldFont);
      OldDrawFontColor := OldFontColor;
    end;

    if (txtfloat <> fsNone) and (txtfloat <> txtfloatbefore) then
    begin
      r.Left := floatpt.x;
      r.Top := floatpt.y;
      txtfloat := fsNone;
    end
    else
    begin
      r.top := r.top + HtmlHeight + subh + suph;
      ysize := ysize + HtmlHeight + subh + suph;
    end;

    //do not draw below bottom
    if (r.top + TxtHeight > r.bottom + 1) and not CheckHeight then
      s := '';
  end;

  if (ysize = 0) then
  begin
    rh := AGraphics.CalculateText('gh');
    ysize := rh.Bottom - rh.Top;
  end;

  //ysize := ysize + 2;

  InsPoint := InsPoint shr 1;

  AGraphics.Stroke.Color := OldPenColor;
  AGraphics.Fill.Color := BlnkColor;
  AGraphics.Font.AssignSource(OldFont);
  OldFont.Free;
  DrawFont.Free;
  CalcFont.Free;
  OldDrawfont.Free;
  OldCalcfont.Free;
end;

function HTMLDrawEx(AGraphics: TAdvGraphics; s:string; fr:TRectF;
                    XPos,YPos,FocusLink,HoverLink,ShadowOffset: Integer;
                    CheckHotSpot,CheckHeight,{%H-}Print,Selected,Blink,HoverStyle,WordWrap: Boolean;
                    {%H-}ResFactor:Double;
                    URLColor,HoverColor,HoverFontColor,ShadowColor: TAdvGraphicsColor;
                    var AnchorVal,StripVal,FocusAnchor: string;
                    var XSize,YSize: single; var HyperLinks,MouseLink: Integer;
                    var HoverRect: TRectF; var LineCount: Integer; LineSpacing: Integer;
                    PictureContainer: TPictureContainer; {%H-}Opacity: Single; HyperLinkUnderline: Boolean = True{$IFDEF CMNLIB}; ImageList: TCustomImageList = nil{$ENDIF};
                    HighlightColor: TAdvGraphicsColor = gcBlue;
                    HighlightTextColor: TAdvGraphicsColor = gcWhite;
                    HighlightTextStyle: TFontStyles = []
                    ): Boolean;
var
  cr: TRectF;
  cid: string;
  cv: string;
  ct: string;
begin
  cid := '';
  cv := '';
  ct := '';
  cr := RectF(0, 0, 0, 0);
  Result := HTMLDrawEx(AGraphics, s, fr, XPos, YPos, FocusLink, HoverLink, ShadowOffset, CheckHotSpot, CheckHeight, Print, Selected, Blink, HoverStyle, WordWrap, False, '',
    ResFactor, URLColor, HoverColor, HoverFontColor, ShadowColor, AnchorVal, StripVal, FocusAnchor, XSize, YSize, HyperLinks, MouseLink, HoverRect, cr, cid, cv, ct, LineCount, LineSpacing, PictureContainer,
    Opacity, HyperLinkUnderline{$IFDEF CMNLIB}, ImageList{$ENDIF}, HighlightColor, HighlightTextColor, HighlightTextStyle);
end;

end.
