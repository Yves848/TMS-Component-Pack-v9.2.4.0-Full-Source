{**************************************************************************}
{ Mini HTML rendering engine                                               }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright � 1999-2019                                         }
{            Email : info@tmssoftware.com                                  }
{            Website : https://www.tmssoftware.com/                        }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

{$I TMSDEFS.INC}

procedure PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);
var
  BitmapHeader:  pBitmapInfo;
  BitmapImage :  POINTER;
  HeaderSize  :  DWORD;
  ImageSize   :  DWORD;
begin
  GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
  GetMem(BitmapHeader, HeaderSize);
  GetMem(BitmapImage,  ImageSize);
  try
    GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
    StretchDIBits(Canvas.Handle,
                  DestRect.Left, DestRect.Top,     // Destination Origin
                  DestRect.Right  - DestRect.Left, // Destination Width
                  DestRect.Bottom - DestRect.Top,  // Destination Height
                  0, 0,                            // Source Origin
                  Bitmap.Width, Bitmap.Height,     // Source Width & Height
                  BitmapImage,
                  TBitmapInfo(BitmapHeader^),
                  DIB_RGB_COLORS,
                  SRCCOPY)
  finally
    FreeMem(BitmapHeader);
    FreeMem(BitmapImage)
  end;
end;

function DirExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function SysImage(Canvas: TCanvas;x,y:Integer;APath:string;large,draw,print:boolean;resfactor:double):TPoint;
var
  SFI: TSHFileInfo;
  i,Err: Integer;
  imglsthandle: THandle;
  rx,ry: Integer;
  bmp: TBitmap;
  r: TRect;
begin
  Val(APath,i,Err);

  FillChar(SFI,Sizeof(SFI),0);
  if (APath <> '') and (Err <> 0) then
  begin
    if FileExists(APath) or DirExists(APath) then
    // If the file or directory exists, just let Windows figure out it's attrs.
      SHGetFileInfo(PChar(APath), 0, SFI, SizeOf(TSHFileInfo),
                    SHGFI_SYSICONINDEX {or OPEN_FLAG[Open] or SELECTED_FLAG[Selected]})
    else
    // File doesn't exist, so Windows doesn't know what to do with it.  We have
    // to tell it by passing the attributes we want, and specifying the
    // SHGFI_USEFILEATTRIBUTES flag so that the function knows to use them.
      SHGetFileInfo(PChar(APath), 0, SFI, SizeOf(TSHFileInfo),
                    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES {or OPEN_FLAG[Open] or SELECTED_FLAG[Selected]});
    i := SFI.iIcon;
  end;

  if Large then
    imglsthandle := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
                            SHGFI_SYSICONINDEX or SHGFI_LARGEICON)
  else
    imglsthandle := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
                            SHGFI_SYSICONINDEX or SHGFI_SMALLICON);


  ImageList_GetIconSize(imglsthandle,rx,ry);

  Result := Point(rx,ry);
 
  if Draw and not Print then
    ImageList_Draw(imglsthandle,i,Canvas.Handle,x,y, ILD_TRANSPARENT);

  if Draw and Print then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Width := rx;
      bmp.Height := ry;
      ImageList_Draw(imglsthandle,i,bmp.Canvas.handle,0,0,ILD_NORMAL);
      r.left := x;
      r.top := y;
      r.right := x + Round(rx * ResFactor);
      r.bottom := y + Round(ry * ResFactor);
      PrintBitmap(Canvas,r,bmp);
    finally
      bmp.Free;
    end;
  end;
end;

procedure DrawHTMLGradient(Canvas: TCanvas; FromColor,ToColor,BorderColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  iend: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;
begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
      begin
        iend := R.Left + stepw + Trunc(rstepw) + 1;
        if iend > R.Right then
          iend := R.Right;
        Rectangle(R.Left + stepw,R.Top,iend,R.Bottom)
      end
      else
      begin
        iend := R.Top + stepw + Trunc(rstepw)+1;
        if iend > r.Bottom then
          iend := r.Bottom;
        Rectangle(R.Left,R.Top + stepw,R.Right,iend);
      end;
    end;

    if BorderColor <> clNone then
    begin
      Brush.Style := bsClear;
      Pen.Color := BorderColor;
      Rectangle(R.Left,R.Top,R.Right,R.Bottom);
    end;
  end;
end;

function Text2Color(s:string):tcolor;
begin
  Result := clBlack;

  if (s='clred') then result:=clred else
  if (s='clblack') then result:=clblack else
  if (s='clblue') then result:=clblue else
  if (s='clgreen') then result:=clgreen else
  if (s='claqua') then result:=claqua else
  if (s='clyellow') then result:=clyellow else
  if (s='clfuchsia') then result:=clfuchsia else
  if (s='clwhite') then result:=clwhite else
  if (s='cllime') then result:=cllime else
  if (s='clsilver') then result:=clsilver else
  if (s='clgray') then result:=clgray else
  if (s='clolive') then result:=clolive else
  if (s='clnavy') then result:=clnavy else
  if (s='clpurple') then result:=clpurple else
  if (s='clteal') then result:=clteal else
  if (s='clmaroon') then result:=clmaroon;

  if Result <> clBlack then Exit;

  if (s='clbackground') then result:=clbackground else
  if (s='clactivecaption') then result:=clactivecaption else
  if (s='clinactivecaption') then result:=clinactivecaption else
  if (s='clmenu') then result:=clmenu else
  if (s='clwindow') then result:=clwindow else
  if (s='clwindowframe') then result:=clwindowframe else
  if (s='clmenutext') then result:=clmenutext else
  if (s='clwindowtext') then result:=clwindowtext else
  if (s='clcaptiontext') then result:=clcaptiontext else
  if (s='clactiveborder') then result:=clactiveborder else
  if (s='clinactiveborder') then result:=clinactiveborder else
  if (s='clappworkspace') then result:=clappworkspace else
  if (s='clhighlight') then result:=clhighlight else
  if (s='clhighlighttext') then result:=clhighlighttext else
  if (s='clbtnface') then result:=clbtnface else
  if (s='clbtnshadow') then result:=clbtnshadow else
  if (s='clgraytext') then result:=clgraytext else
  if (s='clbtntext') then result:=clbtntext else
  if (s='clinactivecaptiontext') then result:=clinactivecaptiontext else
  if (s='clbtnhighlight') then result:=clbtnhighlight else
  if (s='cl3ddkshadow') then result:=clgraytext else
  if (s='cl3dlight') then result:=cl3dlight else
  if (s='clinfotext') then result:=clinfotext else
  if (s='clinfobk') then result:=clinfobk;
end;

function HexVal(s:string): Integer;
var
  i,j: Integer;
begin
  if Length(s) < 2 then
  begin
    Result := 0;
    Exit;
  end;

  if s[1] >= 'A' then
    i := ord(s[1]) - ord('A') + 10
  else
    i := ord(s[1]) - ord('0');

  if s[2] >= 'A' then
    j := ord(s[2]) - ord('A') + 10
  else
    j := ord(s[2]) - ord('0');

  Result := i shl 4 + j;
end;

function Hex2Color(s:string): TColor;
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(s,2,2));
  g := Hexval(Copy(s,4,2)) shl 8;
  b := Hexval(Copy(s,6,2)) shl 16;
  Result := TColor(b + g + r);
end;

function IPos(su,s:string):Integer;
begin
  Result := Pos(AnsiUpperCase(su),AnsiUpperCase(s));
end;

function IStrToInt(s: string):Integer;
var
  Err,Res: Integer;
begin
  Val(s,Res,Err);
  Result := Res;
end;

function IStrToFloat(s: string):Single;
var
  Err,i: Integer;
  Res: single;
begin
  for i := 1 to Length(s) do
    if s[i] = ',' then
     s[i] := '.';


  Val(s,Res,Err);
  Result := Res;
end;

function UnicodeTagStrip(s: string): string;
var
  i,j,e,nn: Integer;
  su,res: string;
  w: word;

  function NonNum(aval: string): integer;
  var
    i: integer;
  begin
    Result := 3;
    for i := 3 to length(aval) do
      begin
        if not ((aval[i] >= '0') and (aval[i] <='9')) then
        begin
          Result := i;
          break;
        end;
      end;
  end;

begin
  res := '';
  repeat
    i := Pos('&#',s);
    if (i > 0) then
    begin
      res := res + Copy(s,1,i - 1);
      Delete(s,1,i - 1);
      j := Pos(';',s);


      if j > 7 then
      begin
        nn := NonNum(s);
        j := nn;
      end;

      if (j > 0) then
      begin
        su := Copy(s,3,j - 3);
        val(su, w, e);

        if (length(su)> 0) and (length(su) <= 6) and (e = 0) then
        begin
          res := res + chr(w);
          Delete(s,1,j);
        end;
      end
      else
      begin
        res := res + '&#';
        Delete(s,1,2);
      end;
    end
    else
      res := res + s;
  until i <= 0;

  Result := res;
end;

function DBTagStrip(s: string): string;
var
  i,j: Integer;
begin
  repeat
    i := Pos('<#',s);
    if i > 0 then
    begin
      Result := Copy(s,1,i - 1);
      Delete(s,1,i);
      j := Pos('>',s);
      if j > 0 then
        Delete(s,j,1);
      s := Result + s;
    end
    else
      Result := s;
  until i <= 0;
end;

function CRLFStrip(s:string;break:boolean):string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if not ( (s[i] = #13) or (s[i] = #10)) then
      Result := Result + s[i]
    else
      if (s[i] = #13) and break then
        Result := Result + '<BR>';
  end;
end;

function VarPos(su,s:string;var Res:Integer):Integer;
begin
  Res := Pos(su,s);
  Result := Res;
end;

function TagReplaceString(const Srch,Repl:string;var Dest:string):Boolean;
var
  i: Integer;
begin
  i := IPos(srch,dest);
  if i > 0 then
  begin
    Result := True;
    Delete(Dest,i,Length(Srch));
    Dest := Copy(Dest,1,i-1) + Repl + Copy(Dest,i,Length(Dest));
  end
  else
    Result := False;
end;


function ConvertHTMLSpecialChars(s: string): string;
const
  NumSpecialChar = 72;

  HTMLEncodedChar : array[1..72] of ansistring = ('�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','�','�',
                                             '�','�','$','�',
                                             '�','�','"','''',
                                             '�','�','�','�',
                                             '�','�','�','�');

  HTMLSpecialChar : array[1..72] of ansistring = ('&eacute;','&egrave;','&euml;','&ecirc;',
                                             '&oacute;','&ograve;','&ouml;','&ocirc;',
                                             '&iacute;','&igrave;','&iuml;','&icirc;',
                                             '&uacute;','&ugrave;','&uuml;','&ucirc;',
                                             '&aacute;','&agrave;','&auml;','&acirc;',
                                             '&Eacute;','&Egrave;','&Euml;','&Ecirc;',
                                             '&Oacute;','&Ograve;','&Ouml;','&Ocirc;',
                                             '&Iacute;','&Igrave;','&Iuml;','&Icirc;',
                                             '&Uacute;','&Ugrave;','&Uuml;','&Ucirc;',
                                             '&Aacute;','&Agrave;','&Auml;','&Acirc;',
                                             '&ccedil;','&Ccedil;','&oslash;','&Oslash;',
                                             '&aring;','&Aring;','&copy;','&reg;',
                                             '&euro;','&laquo;','&raquo;','&atilde;',
                                             '&Atilde;','&otilde;','&Otilde','&szlig;',
                                             '&trade;','&pound;','&dollar;','&sect;',
                                             '&permil;','&para;','&quot;','&apos;',
                                             '&yen;','&curren;','&cent;','&plusmn;',
                                             '&iexcl;','&laquo;','&raquo;','&deg;');

var
  i: integer;
begin
  for i := 1 to NumSpecialChar do
  begin
    while Pos(string(HTMLSpecialChar[i]),s) > 0 do
    begin
      TagReplacestring(string(HTMLSpecialChar[i]),string(HTMLEncodedChar[i]),s);
    end;
  end;

  Result := s;
end;

{$WARNINGS OFF}
function HTMLDrawEx(Canvas:TCanvas; s: string; fr: TRect;
                    FImages: TCustomImageList;
                    XPos,YPos,FocusLink,HoverLink,ShadowOffset: Integer;
                    CheckHotSpot,CheckHeight,Print,Selected,Blink,HoverStyle,WordWrap,Scaled: Boolean;
                    ResFactor:Double;
                    URLColor,HoverColor,HoverFontColor,ShadowColor:TColor;
                    var AnchorVal,StripVal,FocusAnchor: string;
                    var XSize,YSize,HyperLinks,MouseLink: Integer;
                    var HoverRect:TRect;ic: THTMLPictureCache; pc: TPictureContainer; LineSpacing: Integer;
                    BidiMode: TBidiMode = bdLeftToRight;
                    HighlightColor: TColor = clHighlight;
                    HighlightTextColor: TColor = clHighlightText;
                    HighlightTextStyle: TFontStyles = []; ADPIScale: single = -1): Boolean;

type
  TFloatStyle = (fsNone, fsLeft, fsRight);
var
  su: string;
  r,dr,hr,rr,er,dcr: TRect;
  htmlwidth,htmlheight,txtheight: Integer;
  Align: TAlignment;
  PIndent: Integer;
  OldFont: TFont;
  CalcFont: TFont;
  DrawFont: TFont;
  OldCalcFont: TFont;
  OldDrawFont: TFont;
  Hotspot, ImageHotspot: Boolean;
  Anchor,OldAnchor,MouseInAnchor,Error: Boolean;
  bgcolor,paracolor,hvrcolor,hvrfntcolor,pencolor,blnkcolor,hifcol,hibcol: TColor;
  hifstyle: TFontStyles;
  LastAnchor,OldAnchorVal,LType, HTitle: string;
  IMGSize: TPoint;
  isSup,isSub,isPara,isShad,oldisShad,isURL: Boolean;
  subh,suph,imgali,hlcount,licount: Integer;
  hrgn,holdfont: THandle;
  ListIndex,OListIndex: Integer;
  OLIST: boolean;
  OListIndexStr: string;
  dtp: TDrawTextParams;
  Invisible: Boolean;
  FoundTag: Boolean;
  nnFit: Integer;
  nnSize: TSize;
  inspoint: Integer;
  AltImg,ImgIdx,OldImgIdx: Integer;
  DrawStyle: DWord;
  ColL,ColB: TColor;
  ofsx,newofsx: integer;
  floatrect: TRect;
  floatpt: TPoint;
  txtfloat,txtfloatbefore: TFloatStyle;
  imgfloat: TFloatStyle;
  lhf,scale: single;
  LISTINDENT: integer;
  imgalign: integer;

  procedure StartRotated(Canvas:TCanvas;Angle: Integer);
  var
    LFont:TLogFont;
  begin
    GetObject(Canvas.Font.Handle,SizeOf(LFont),Addr(LFont));
    LFont.lfEscapement := Angle * 10;
    LFont.lfOrientation := Angle * 10;
    hOldFont := SelectObject(Canvas.Handle,CreateFontIndirect(LFont));
  end;

  procedure EndRotated(Canvas:TCanvas);
  begin
    DeleteObject(SelectObject(Canvas.Handle,hOldFont));
  end;

  function HTMLDrawLine(Canvas: TCanvas; var s: string;r: TRect; Calc: Boolean;
                        var w, h, subh, suph, imgali: Integer;var Align: TAlignment; var PIndent: Integer;
                        XPos,YPos: Integer;var Hotspot,ImageHotSpot: Boolean; OffsetX: integer; var NewOffsetX: integer; var floatrect: TRect; var txtfloat: TFloatStyle): string;
  var
    su,Res,TagProp,Prop,Tagp,LineText:string;
    cr,ir: TRect;
    linebreak,imgbreak,linkbreak: Boolean;
    th,sw,indent,err,bmpx,bmpy,imgw,imgh: Integer;
    TagPos,SpacePos,o,l: Integer;
    bmp: TGraphic;
    ABitmap: TBitmap;
    NewColor,NewColorTo: TColor;
    TagWidth,TagHeight,WordLen,WordLenEx,WordWidth: Integer;
    TagChar: Char;
    LengthFits: Boolean;
    imgoffs, imgth,txth: integer;
    origh: integer;
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
    HTitle := '';
    origh := h;
    h := 0;

//    r.Left := r.Left + offsetX;

    cr := r;
    res := '';

    if not Calc then
      cr.Left := cr.Left + OffsetX;

    if isPara and not Calc then
    begin
      Pencolor := Canvas.Pen.Color;
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Rectangle(fr.Left,r.Top,fr.Right,r.Top + origh);
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
      begin
        su := Copy(s,1,TagPos - 1);
      end
      else
      begin
        if SpacePos > 0 then
          su := Copy(s,1,SpacePos)
        else
          su := s;
      end;

      {$IFDEF TMSDEBUG}
      TMSLogger.Info(su+ '.');
      {$ENDIF}

      WordLen := Length(su);

      while Pos('&nbsp;',su) > 0 do
      begin
        TagReplacestring('&nbsp;',' ',su);
      end;

      while Pos('&lt;',su) > 0 do
      begin
        TagReplacestring('&lt;','<',su);
      end;

      while Pos('&gt;',su) > 0 do
      begin
        TagReplacestring('&gt;','>',su);
      end;

      WordLenEx := Length(su);

      if WordLen > 0 then
      begin
        if h = 0 then
          h := origh;

        th := Canvas.TextHeight(su);

        if isSub and (subh < (th shr 2)) then subh := th shr 2;
        if isSup and (suph < (th shr 2)) then suph := th shr 2;

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

            if isShad then
            begin
              OffsetRect(cr,ShadowOffset,ShadowOffset);
              NewColor := Canvas.Font.Color;
              Canvas.Font.Color := ShadowColor;
              DrawTextEx(Canvas.Handle,PChar(su),WordLenEx,cr,DrawStyle and not DT_EXTERNALLEADING,nil);
              Offsetrect(cr,-ShadowOffset,-ShadowOffset);
              Canvas.Font.Color := NewColor;
            end;

            err := cr.Right;

            dcr := cr;

            if (fsItalic in Canvas.Font.Style) then
              dcr.Right := cr.Right + 2;

            DrawTextEx(Canvas.Handle, PChar(su), WordLenEx, cr, DrawStyle or DT_CALCRECT,nil);

            if WordWrap or (not WordWrap and (cr.Left < r.Right - Canvas.TextWidth('l'))) then
            begin
              DrawTextEx(Canvas.Handle, PChar(su), WordLenEx, dcr, DrawStyle and not DT_EXTERNALLEADING,nil);
            end;

            if Anchor and (Hyperlinks - 1 = FocusLink) then
            begin
              FocusAnchor := LastAnchor;
            end;

            {$IFDEF TMSDEBUG}
            if Anchor then
            begin
              TMSLogger.Info('drawrect for '+anchorval+' = ['+inttostr(cr.Left)+':'+inttostr(cr.Top)+'] ['+inttostr(cr.right)+':'+inttostr(cr.bottom)+'] @ ['+inttostr(xpos)+':'+inttostr(ypos));
            end;
            {$ENDIF}

            if Error then
            begin
              Canvas.Pen.Color := clRed;
              Canvas.Pen.Width := 1;

              l := (cr.Left div 2) * 2;
              if (l mod 4)=0 then o := 2 else o := 0;

              Canvas.MoveTo(l,r.Bottom + o - 1);
              while l < cr.Right do
              begin
                if o = 2 then o := 0 else o := 2;
                Canvas.LineTo(l + 2,r.bottom + o - 1);
                Inc(l,2);
              end;
              // if o = 2 then o := 0 else o := 2;
              // Canvas.LineTo(l + 2,r.Bottom + o - 1);
            end;

            // change to Bidimode
            if DrawStyle and DT_RTLREADING <> DT_RTLREADING then
            begin
              cr.Left := cr.Right;
              cr.Right := r.Right;
            end
            else
            begin
              cr.Right := err - cr.Right + cr.Left;
            end;

            cr.Bottom := r.Bottom;
            cr.Top := r.Top;
          end
        else
          begin
            cr := r; //reinitialized each time !

            DrawTextEx(Canvas.Handle,PChar(su),WordLenEx,cr,DrawStyle or DT_CALCRECT,nil);

            // preparations for editing purposes
            if (ypos > cr.Top) and (ypos < cr.bottom) and (xpos > w) then {scan charpos here}
            begin
              er := rect(w,cr.top,xpos,cr.bottom);
              Fillchar(dtp,sizeof(dtp),0);
              dtp.cbSize:=sizeof(dtp);

              GetTextExtentExPoint(Canvas.Handle,pChar(su),WordLenEx,xpos-w,@nnfit,nil,nnSize);

              //this will get the character pos of the insertion point
              if nnfit = WordLen then
                InsPoint := InsPoint + WordLen
              else
                InsPoint := InsPoint + nnfit;
            end;
            // end of preparations for editing purposes

            // Calculated text width
            WordWidth := cr.Right - cr.Left;
            w := w + WordWidth;

            if DrawStyle and DT_RTLREADING = DT_RTLREADING then
              err := r.Right - XPos - cr.Left
            else
              err := XPos - cr.Left;

            if (XPos - cr.Left  >= w - WordWidth) and (XPos - cr.Left <= w) and Anchor then
            begin
              HotSpot := True;
              if (YPos > cr.Top){ and (YPos < cr.Bottom)} then
              begin
                Anchorval := LastAnchor;
                FocusAnchor := Htitle;
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
              if System.Copy(su, WordLen, 1) = ' ' then
                sw := Canvas.TextWidth(' ')
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
        if (s[2] = '/') and (Length(s) > 3) then
        begin
          case UpCase(s[3]) of
          'A':begin
                isURL := false;
                if (not HoverStyle or (Hoverlink = Hyperlinks)) and not Calc then
                begin
                  Canvas.Font.Style := Canvas.Font.Style - [fsUnderline];
                  if Hovercolor <> clNone then
                  begin
                    Canvas.Brush.Color := HvrColor;
                    if HvrColor = clNone then
                      Canvas.Brush.Style := bsClear;
                  end;
                  if HoverFontColor <> clNone then
                    Canvas.Font.Color := HoverFontColor;
                end;

                if not Selected then
                  Canvas.Font.Color := Oldfont.Color;

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
                  {$IFDEF TMSDEBUG}
                  TMSLogger.LogSeparator;
                  TMSLogger.Info('hotspot anchor '+lastanchor);
                  TMSLogger.Info(hr.Left);
                  TMSLogger.Info(hr.Top);
                  TMSLogger.Info(hr.Right);
                  TMSLogger.Info(hr.Bottom);
                  TMSLogger.LogSeparator;
                  {$ENDIF}
                  MouseInAnchor := False;
                end;

                if Focuslink = Hyperlinks - 1 then
                begin
                  rr.Right := cr.Left;
                  rr.Bottom := cr.Bottom - ImgAli;
                  rr.Top := rr.Bottom - Canvas.TextHeight('gh');
                  InflateRect(rr,1,0);
                  if not Calc then Canvas.DrawFocusRect(rr);
                end;
              end;
          'E':begin
                if not Calc then
                  Error := False;
              end;
          'B':begin
                if s[4] <> '>' then
                  Canvas.Font.Color := OldFont.Color
                else
                  Canvas.Font.Style := Canvas.Font.Style - [fsBold];
              end;
          'S':begin
                TagChar := UpCase(s[4]);

                if (TagChar = 'T') then // STRIKE , STRONG
                begin
                  if (Length(s) > 5) and (Upcase(s[6]) = 'O') then // STRONG
                    Canvas.Font.Style := Canvas.Font.Style - [fsBold]
                  else
                    Canvas.Font.Style := Canvas.Font.Style - [fsStrikeOut];
                end
                else
                if (TagChar = 'U') then // SUB
                begin
                  isSup := False;
                  isSub := False;
                end
                else
                if (TagChar = 'P') then  // SPAN
                begin
                  Canvas.Brush.Color := BGColor;
                  Canvas.Brush.Style := bsClear;
                end
                else
                if (TagChar = 'H') then  // SHADOW
                begin
                  isShad := False
                end
                else
                  Canvas.Font.Style := Canvas.Font.Style - [fsStrikeOut];
              end;
          'F':begin
                Canvas.Font.Name := OldFont.Name;
                Canvas.Font.Size := OldFont.Size;

                if not Calc and not Selected then
                begin
                  Canvas.Font.Color := OldFont.Color;
                  Canvas.Brush.Color := BGColor;
                  if BGColor = clNone then
                    Canvas.Brush.Style := bsClear;
                end;
              end;
          'H':begin
                if Upcase(s[4])= 'I' then
                begin
                  if not Calc then
                  begin
                    Canvas.Font.Color := hifCol;
                    Canvas.Font.Style := hifStyle;
                    Canvas.Brush.Color := hibCol;

                    if hibCol = clNone then
                      Canvas.Brush.Style := bsClear;
                  end;
                end;
              end;
          'I':begin
                Canvas.Font.Style := Canvas.Font.Style - [fsItalic];
              end;
          'L':begin
                LineBreak := True;
              end;
          'O':begin
                if (s[4] <> '>') and (ListIndex > 0) then
                begin
                  Dec(Listindex);
                  if not calc then
                    OList := false;
                end
                else
                  NewOffsetX := 0;
              end;
          'P':begin
                LineBreak := true;

                if not Calc then
                 lhf := 1;

                if not Calc then
                  isPara := false;

                if not (Calc or CheckHotSpot) then
                begin
                  //Align := taLeftJustify;

                  if not Selected then
                  begin
                    Canvas.Brush.Color := ParaColor;
                    if ParaColor = clNone then
                      Canvas.Brush.Style := bsClear;
                  end;
                end;
              end;
          'U':begin
                if (s[4] <> '>') and (ListIndex > 0) then
                  Dec(Listindex)
                else
                  Canvas.Font.Style := Canvas.Font.Style - [fsUnderline];
              end;
          'R':begin
                EndRotated(Canvas);
              end;
          'Z':Invisible := False;
          end;
        end
        else
        begin
          case Upcase(s[2]) of
          'A':begin
                isURL := true;
                OldFont.Color := Canvas.Font.Color;
                { only do this when at hover position in xpos,ypos }
                if (FocusLink = HyperLinks) and not Calc then
                begin
                  rr.Left := cr.Left;
                  rr.Top := cr.Top;
                end;

                Inc(HyperLinks);
                if (not HoverStyle or (Hoverlink = HyperLinks)) and not Calc then
                begin
                  Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
                  if (Hovercolor <> clNone) and not Calc then
                  begin
                    HvrColor := Canvas.Brush.Color;
                    if Canvas.Brush.Style = bsClear then
                      HvrColor := clNone;
                    Canvas.Brush.Color := HoverColor;
                  end;

                  if HoverFontColor <> clNone then
                  begin
                    hvrfntcolor := Canvas.Font.Color;
                    Canvas.Font.Color := HoverFontColor;
                  end;
                end;

                if not Selected and ((HoverFontColor = clNone) or (HoverLink <> HyperLinks) or not HoverStyle) then
                  Canvas.Font.Color := URLColor;

                TagProp := Uppercase(Copy(s,3,Pos('>',s) - 1));  // <A href="anchor">

                if (VarPos('HREF',TagProp,TagPos) > 0) then
                begin
                  TagProp := Copy(s,3,Pos('>',s) - 1);
                  Prop := Copy(TagProp,TagPos + 4,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                  LastAnchor := Prop;
                  Anchor := True;
                end;

                TagProp := Uppercase(Copy(s,3,Pos('>',s) - 1));  // <A href="anchor">

                if (VarPos('TITLE',TagProp,TagPos) > 0) then
                begin
                  TagProp := Copy(s,3,Pos('>',s) - 1);  // <A href="anchor">
                  Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                  HTitle := Prop;
                end
                else
                  HTitle := '';

                hr.Left := w;
                hr.Top := r.Top;
              end;
          'B':begin
                TagChar := Upcase(s[3]);
                case TagChar of
                '>': Canvas.Font.Style := Canvas.Font.Style + [fsBold]; // <B> tag
                'R': // <BR> tag
                   begin
                    LineBreak := true;
                    if h = 0 then
                      h := origh;
                    StripVal := StripVal + #13;
                   end;
                'L': if not Blink then
                   Canvas.Font.Color := BlnkColor; // <BLINK> tag
                'O':
                  begin
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
                            if Assigned(FImages) and (IStrToInt(Prop) < FImages.Count) then
                              begin
                                //IMGSize.X := MulDiv(FImages.Width, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 96);
                                //IMGSize.Y := MulDiv(FImages.Height, GetDeviceCaps(Canvas.Handle, LOGPIXELSY), 96);
                                IMGSize.X := FImages.Width;
                                IMGSize.Y := FImages.Height;

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
                          end;

                        if Pos('SSYS:', UpperCase(Prop)) > 0 then
                          begin
                            Delete(Prop, 1, 5);
                            IMGSize := SysImage(Canvas, CR.Left, CR.Top, Prop, False, not Calc, Print, resfactor);

//                            IMGSize.X := MulDiv(IMGSize.X, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 96);
//                            IMGSize.Y := MulDiv(IMGSize.Y, GetDeviceCaps(Canvas.Handle, LOGPIXELSY), 96);
                          end;

                        if Pos('LSYS:', UpperCase(Prop)) > 0 then
                          begin
                            Delete(Prop, 1, 5);
                            IMGSize := SysImage(Canvas, CR.Left, CR.Top, Prop, True, not Calc, Print, resfactor);

//                            IMGSize.X := MulDiv(IMGSize.X, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 96);
//                            IMGSize.Y := MulDiv(IMGSize.Y, GetDeviceCaps(Canvas.Handle, LOGPIXELSY), 96);
                          end;

                        bmp := nil;

                        if (Pos(':',Prop) = 0) and Assigned(pc) then
                        begin
                          bmp := pc.FindPicture(Prop);
                        end;

                        if (Pos('://',Prop) > 0) and Assigned(ic) then
                        begin
                          if ic.FindPicture(Prop) = nil then
                          with ic.AddPicture do
                          begin
                            Asynch := False;
                            LoadFromURL(Prop);
                          end;
                          bmp := ic.FindPicture(Prop) as THTMLPicture;
                        end;

                        if bmp <> nil then
                        begin
                          if not bmp.Empty and (bmp.Width > 0) and (bmp.Height > 0) then
                          begin
                            // do the tiling here
                            bmpy := 0;
                            hrgn := CreateRectRgn(fr.left, fr.top, fr.right,fr.bottom);
                            SelectClipRgn(Canvas.Handle, hrgn);
                            while (bmpy < fr.bottom-fr.top) do
                            begin
                              bmpx := 0;
                              while (bmpx < fr.right - fr.left) do
                              begin
                                Canvas.Draw(fr.left+bmpx,fr.top+bmpy,bmp);
                                bmpx := bmpx + bmp.width;
                              end;
                              bmpy := bmpy + bmp.height;
                            end;
                            SelectClipRgn(Canvas.handle, 0);
                            DeleteObject(hrgn);
                          end;
                        end; //end of bmp <> nil
                      end; //end of background

                      if (Pos('BGTOPLEFT', TagProp) > 0) then
                        begin
                          Prop := Copy(TagProp, Pos('BGTOPLEFT', TagProp) + 10, Length(TagProp));
                          Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                          Prop := Copy(Prop, 1, Pos('"', Prop) - 1);

                          if Pos('IDX:', UpperCase(Prop)) > 0 then
                            begin
                              Delete(Prop, 1, 4);
                              if Assigned(FImages) and (IStrToInt(Prop) < FImages.Count) then
                                begin
//                                  IMGSize.X := MulDiv(FImages.Width, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 96);
//                                  IMGSize.Y := MulDiv(FImages.Height, GetDeviceCaps(Canvas.Handle, LOGPIXELSY), 96);

                                  IMGSize.X := FImages.Width;
                                  IMGSize.Y := FImages.Height;

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
                            end;

                          if Pos('SSYS:', UpperCase(Prop)) > 0 then
                            begin
                              Delete(Prop, 1, 5);
                              IMGSize := SysImage(Canvas, CR.Left, CR.Top, Prop, False, not Calc, Print, resfactor);

//                              IMGSize.X := MulDiv(IMGSize.X, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 96);
//                              IMGSize.Y := MulDiv(IMGSize.Y, GetDeviceCaps(Canvas.Handle, LOGPIXELSY), 96);
                            end;

                          if Pos('LSYS:', UpperCase(Prop)) > 0 then
                            begin
                              Delete(Prop, 1, 5);
                              IMGSize := SysImage(Canvas, CR.Left, CR.Top, Prop, True, not Calc, Print, resfactor);

//                              IMGSize.X := MulDiv(IMGSize.X, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 96);
//                              IMGSize.Y := MulDiv(IMGSize.Y, GetDeviceCaps(Canvas.Handle, LOGPIXELSY), 96);
                            end;

                          bmp := nil;

                          if (Pos(':', Prop) = 0) and Assigned(pc) then
                            begin
                              bmp := pc.FindPicture(Prop);
                            end;

                          if (Pos('://', Prop) > 0) and Assigned(iC) then
                            begin
                              if iC.FindPicture(Prop) = nil then
                                with iC.AddPicture do
                                  begin
                                    ASYNCH := False;
                                    LoadFromURL(Prop);
                                  end;
                              bmp := iC.FindPicture(Prop) as THTMLPicture;
                            end;

                          if bmp <> nil then
                            begin
                              if not bmp.Empty and (bmp.Width > 0) and (bmp.Height > 0) then
                                begin
                                  // do the tiling here
                                  bmpy := 0;
                                  HRgn := CreateRectRgn(fr.Left, fr.Top, fr.Right, fr.Bottom);
                                  SelectClipRgn(Canvas.Handle, HRgn);
                                  while (bmpy < fr.Bottom - fr.Top) do
                                    begin
                                      bmpx := 0;
                                      while (bmpx < fr.Right - fr.Left) do
                                        begin
                                          Canvas.Draw(fr.Left + bmpx, fr.Top + bmpy, bmp);
                                          bmpx := bmpx + bmp.Width;
                                        end;
                                      bmpy := bmpy + bmp.Height;
                                    end;
                                  SelectClipRgn(Canvas.Handle, 0);
                                  DeleteObject(HRgn);
                                end;
                            end; //end of bmp <> nil
                        end; //end of bgtopleft

                      if (Pos('BGTOPRIGHT', TagProp) > 0) then
                        begin
                          Prop := Copy(TagProp, Pos('BGTOPRIGHT', TagProp) + 10, Length(TagProp));
                          Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                          Prop := Copy(Prop, 1, Pos('"', Prop) - 1);

                          if Pos('IDX:', UpperCase(Prop)) > 0 then
                            begin
                              Delete(Prop, 1, 4);
                              if Assigned(FImages) and (IStrToInt(Prop) < FImages.Count) then
                                begin
//                                  IMGSize.X := MulDiv(FImages.Width, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 96);
//                                  IMGSize.Y := MulDiv(FImages.Height, GetDeviceCaps(Canvas.Handle, LOGPIXELSY), 96);
                                  IMGSize.X := FImages.Width;
                                  IMGSize.Y := FImages.Height;

                                  if not Calc and not Print then
                                    FImages.Draw(Canvas, CR.Right - FImages.Width, CR.Top, IStrToInt(Prop), True);

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
                            end;

                          if Pos('SSYS:', UpperCase(Prop)) > 0 then
                            begin
                              Delete(Prop, 1, 5);
                              IMGSize := SysImage(Canvas, CR.Right - FImages.Width, CR.Top, Prop, False, not Calc, Print, resfactor);

//                              IMGSize.X := MulDiv(IMGSize.X, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 96);
//                              IMGSize.Y := MulDiv(IMGSize.Y, GetDeviceCaps(Canvas.Handle, LOGPIXELSY), 96);
                            end;

                          if Pos('LSYS:', UpperCase(Prop)) > 0 then
                            begin
                              Delete(Prop, 1, 5);
                              IMGSize := SysImage(Canvas, CR.Right - FImages.Width, CR.Top, Prop, True, not Calc, Print, resfactor);

//                              IMGSize.X := MulDiv(IMGSize.X, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 96);
//                              IMGSize.Y := MulDiv(IMGSize.Y, GetDeviceCaps(Canvas.Handle, LOGPIXELSY), 96);
                            end;

                          bmp := nil;

                          if (Pos(':', Prop) = 0) and Assigned(pc) then
                            begin
                              bmp := pc.FindPicture(Prop);
                            end;

                          if (Pos('://', Prop) > 0) and Assigned(iC) then
                            begin
                              if iC.FindPicture(Prop) = nil then
                                with iC.AddPicture do
                                  begin
                                    ASYNCH := False;
                                    LoadFromURL(Prop);
                                  end;
                              bmp := iC.FindPicture(Prop) as THTMLPicture;
                            end;

                          if bmp <> nil then
                            begin
                              if not bmp.Empty and (bmp.Width > 0) and (bmp.Height > 0) then
                                begin
                                  // do the printing here
                                  bmpy := 0;
                                  HRgn := CreateRectRgn(fr.Left, fr.Top, fr.Right, fr.Bottom);
                                  SelectClipRgn(Canvas.Handle, HRgn);
                                  while (bmpy < fr.Bottom - fr.Top) do
                                    begin
                                      bmpx := 0;
                                      while (bmpx < fr.Right - fr.Left) do
                                        begin
                                          Canvas.Draw(fr.Right - bmp.Width, fr.Top + bmpy, bmp);
                                          bmpx := bmpx + bmp.Width;
                                        end;
                                      bmpy := bmpy + bmp.Height;
                                    end;
                                  SelectClipRgn(Canvas.Handle, 0);
                                  DeleteObject(HRgn);
                                end;
                            end; //end of bmp <> nil
                        end; //end of bgtopright

                      if VarPos('BGCOLOR',TagProp,TagPos) > 0 then
                      begin
                        Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                        Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                        Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                        NewColor := clNone;

                        if Length(Prop) > 0 then
                        begin
                          if Prop[1] = '#' then
                            NewColor := Hex2Color(Prop)
                          else
                            NewColor := Text2Color(AnsiLowerCase(prop));
                        end;

                        if VarPos('BGCOLORTO',TagProp,TagPos) > 0 then
                        begin
                          Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                          Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                          Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                          NewColorTo := clNone;

                          if Length(Prop) > 0 then
                          begin
                            if Prop[1] = '#' then
                              NewColorTo := Hex2Color(Prop)
                            else
                              NewColorTo := Text2Color(AnsiLowerCase(prop));
                          end;

                          Prop := 'H';
                          if VarPos('DIR',TagProp,TagPos) > 0 then
                          begin
                            Prop := Copy(TagProp,TagPos + 3,Length(TagProp));
                            Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                            Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                          end;

                          Canvas.Pen.Color := Newcolor;
                          DrawHTMLGradient(Canvas,NewColor,NewColorTo,clNone,64,Rect(fr.left,fr.top,fr.right,fr.bottom),Prop = 'H');
                          Canvas.Brush.Style := bsClear
                        end
                        else
                        begin
                          BGColor := Canvas.Brush.Color;                        
                          Canvas.Brush.color := NewColor;
                          PenColor:=Canvas.Pen.Color;
                          Canvas.Pen.Color := Newcolor;
                          Canvas.Rectangle(fr.left - 2,fr.top,fr.right,fr.bottom);
                          Canvas.Pen.Color := PenColor;
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
          'H':begin
                case Upcase(s[3]) of
                'R':
                begin
                  LineBreak := True;
                  if not Calc then
                  begin
                    TagProp := Copy(s,4,Pos('>',s) - 1);

                    NewColor := clBlack;

                    if VarPos('COLOR',Uppercase(TagProp),TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',prop)-1);

                      if Length(Prop) > 0 then
                      begin
                        if Prop[1] = '#' then
                          NewColor := Hex2Color(Prop)
                        else
                          NewColor := Text2Color(AnsiLowerCase(prop));
                      end;
                    end;

                    Pencolor := Canvas.Pen.color;
                    Canvas.Pen.Width := 1;
                    Canvas.Pen.Color := Newcolor;
                    Canvas.MoveTo(fr.left,cr.Bottom + 1);
                    Canvas.Lineto(fr.right,cr.Bottom + 1);
                    Canvas.Pen.Color := Pencolor;
                  end;
                end;
                'I':
                begin
                  if not Calc then
                  begin
                    hifCol := Canvas.Font.Color;
                    hibCol := Canvas.Brush.Color;
                    hifStyle := Canvas.Font.Style;
                    if Canvas.Brush.Style = bsClear then
                      hibCol := clNone;

                    Canvas.Brush.Color := HighlightColor;
                    if HighLightColor = clNone then
                      Canvas.Brush.Style := bsClear;

                    Canvas.Font.Color := HighLightTextColor;
                    Canvas.Font.Style := HighLightTextStyle;
                  end;
                end;
                end;
              end;
          'I':begin
                TagChar := Upcase(s[3]);

                if TagChar = '>' then // <I> tag
                  Canvas.Font.Style := Canvas.Font.Style + [fsItalic]
                else
                if TagChar = 'N' then  // <IND> tag
                begin
                  TagProp := Copy(s,3,pos('>',s) - 1);

                  Prop := Copy(TagProp,ipos('x',TagProp) + 2,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                  val(Prop,Indent,err);
                  if err = 0 then
                  begin
                    if Indent * scale > w then
                     begin
                       w := Round(Indent * scale);
                       cr.left := fr.left + Round(Indent * scale);
                     end;
                  end;
                end
                else
                  if TagChar = 'M' then
                  begin
                    imgalign := 0;
                    imgoffs := 0;
                    imgfloat := fsNone;
                    imgth := Canvas.TextHeight('gh');

                    inc(ImgIdx);

                    ir := cr;

                    TagProp := Copy(s,3,Pos('>',s) - 1);
                    Prop := Copy(TagProp,Pos('SRC',Uppercase(TagProp)) + 4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    TagProp := Uppercase(TagProp);

                    if (Pos('ALT',TagProp) > 0) and (AltImg = ImgIdx) then
                    begin
                      TagP := Copy(TagProp,Pos('ALT',TagProp) + 4,Length(TagProp));
                      TagP := Copy(TagP,Pos('"',TagP) + 1,Length(TagP));
                      TagP := Copy(TagP,1,Pos('"',TagP) - 1);
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
                      Tagp := Copy(TagProp,ipos('HEIGHT',TagProp) + 7,Length(TagProp));
                      Tagp := Copy(Tagp,pos('"',Tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,pos('"',Tagp) - 1);
                      Val(Tagp,TagHeight,Err);
                    end;

                    if Pos('ALIGN',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,ipos('ALIGN',TagProp) + 6,Length(TagProp));
                      Tagp := Copy(Tagp,pos('"',Tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,pos('"',Tagp) - 1);

                      if TagP = 'MIDDLE' then
                        imgalign := 1;

                      if TagP = 'TOP' then
                        imgalign := 2;
                    end;

                    if Pos('FLOAT',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,ipos('FLOAT',TagProp) + 6,Length(TagProp));
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
                      if Assigned(FImages) and (IStrToInt(Prop) < FImages.Count) then
                      begin
                        //IMGSize.x := MulDiv(FImages.Width,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                        //IMGSize.y := MulDiv(FImages.Height,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                        IMGSize.X := FImages.Width;
                        IMGSize.Y := FImages.Height;

                        if (imgth > FImages.Height) then
                        begin
                          if imgalign = 0 then // bottom
                            imgoffs := (imgth - FImages.Height);

                          if imgalign = 1 then // center
                            imgoffs := (imgth - FImages.Height) div 2;
                        end;

                        if not Calc and not Print then
                          FImages.Draw(Canvas,ir.Left, ir.Top + imgoffs,IStrToInt(Prop),True);

                        if not Calc and Print then
                        begin
                          ir.Right := ir.Left + Round(ResFactor * FImages.Width);
                          ir.Bottom := ir.Top + Round(ResFactor * FImages.Height);

                          ABitmap := TBitmap.Create;
                          FImages.GetBitmap(IStrToInt(Prop),ABitmap);
                          PrintBitmap(Canvas,ir,ABitmap);
                          ABitmap.Free;
                        end;

                      end;
                    end;

                    if Pos('SSYS:',Uppercase(Prop)) > 0 then
                    begin
                      Delete(Prop,1,5);
                      IMGSize := SysImage(Canvas,ir.Left,ir.Top,Prop,False,not Calc,Print,ResFactor);

//                      IMGSize.x := MulDiv(IMGSize.X, GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
//                      IMGSize.y := MulDiv(IMGSize.Y, GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                    end;

                    if Pos('LSYS:',Uppercase(Prop)) > 0 then
                    begin
                      Delete(Prop,1,5);
                      IMGsize := SysImage(Canvas,ir.Left,ir.Top,Prop,True,not Calc,Print,ResFactor);

//                      IMGSize.x := MulDiv(IMGSize.X, GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
//                      IMGSize.y := MulDiv(IMGSize.Y, GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                    end;

                    bmp := nil;

                    if (Pos(':',Prop) = 0) and Assigned(pc) then
                    begin
                      bmp := pc.FindPicture(Prop);
                    end;

                    if (Pos('://',Prop) > 0) and Assigned(ic) then
                    begin
                      if ic.FindPicture(Prop) = nil then
                        with ic.AddPicture do
                        begin
                          Asynch := False;
                          LoadFromURL(Prop);
                        end;

                      bmp := ic.FindPicture(Prop) as THTMLPicture;
                    end;

                      if bmp <> nil then
                      begin
                        if not bmp.Empty then
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
                              if (bmp is THTMLPicture) then
                                (bmp as THTMLPicture).Stretch := True;
                              Canvas.StretchDraw(Rect(ir.Left,ir.Top,ir.Left + imgw,ir.Top + imgh),bmp)
                            end
                            else
                            if (TagWidth > 0) and (TagHeight = 0) then
                            begin
                              if (bmp is THTMLPicture) then
                                (bmp as THTMLPicture).Stretch := True;
                              Canvas.StretchDraw(Rect(ir.Left,ir.Top,ir.Left + TagWidth,ir.Top + TagHeight),bmp)
                            end
                            else
                            begin
                              // need for animation - redraw background
                              if (bmp is THTMLPicture) then
                              begin
                                if (bmp as THTMLPicture).FrameCount > 1 then
                                begin
                                  Canvas.Pen.Color := BlnkColor;
                                  Canvas.Brush.Color := BlnkColor;
                                  Canvas.Rectangle(ir.Left,ir.Top,ir.Left + (bmp as THTMLPicture).MaxWidth,ir.Top + (bmp as THTMLPicture).MaxHeight);
                                end;

                                if (imgth > (bmp as THTMLPicture).Height) then
                                begin
                                  if imgalign = 0 then // bottom
                                    imgoffs := (imgth - (bmp as THTMLPicture).Height);

                                  if imgalign = 1 then // center
                                    imgoffs := (imgth - (bmp as THTMLPicture).Height) div 2;
                                end;

                                Canvas.Draw(ir.Left + (bmp as THTMLPicture).FrameXPos,ir.Top + (bmp as THTMLPicture).FrameYPos + imgoffs,bmp);
                              end
                              else
                              begin
                                if (imgth > bmp.Height) then
                                begin
                                  if imgalign = 0 then // bottom
                                    imgoffs := (imgth - bmp.Height);

                                  if imgalign = 1 then // center
                                    imgoffs := (imgth - bmp.Height) div 2;
                                end;

                                Canvas.Draw(ir.Left,ir.Top + imgoffs,bmp);
                              end;
                            end;

                          end;

                          if (TagWidth > 0) or (TagHeight > 0) then
                          begin
                            if ADPIScale > 0 then
                            begin
                              IMGSize.x := Round(imgw*ADPIScale);
                              IMGSize.y := Round(imgh*ADPIScale);
                            end
                            else
                            begin
                              IMGSize.x := MulDiv(imgw,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                              IMGSize.y := MulDiv(imgh,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);
                            end;
                          end
                          else
                          begin
                            if (bmp is THTMLPicture) then
                            begin
                              //IMGSize.x := MulDiv((bmp as THTMLPicture).MaxWidth,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                              //IMGSize.y := MulDiv((bmp as THTMLPicture).MaxHeight,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);

                              ImgSize.x := (bmp as THTMLPicture).MaxWidth;
                              ImgSize.y := (bmp as THTMLPicture).MaxHeight;
                            end
                            else
                            begin
                              //IMGSize.x := MulDiv(bmp.Width,GetDeviceCaps(Canvas.Handle,LOGPIXELSX),96);
                              //IMGSize.y := MulDiv(bmp.Height,GetDeviceCaps(Canvas.Handle,LOGPIXELSY),96);

                              IMGSize.x := bmp.Width;
                              IMGSize.y := bmp.Height;
                            end;
                          end;
                        end;
                      end;

                    if (XPos - r.Left > w) and (XPos - r.Left < w + IMGSize.x) and
                       (YPos > cr.Top) and (YPos < cr.Top + IMGSize.Y) and Anchor then
                    begin
                      ImageHotSpot := True;
                      AnchorVal := LastAnchor;
                      if HTitle <> '' then
                        FocusAnchor := HTitle;
                      AltImg := ImgIdx;
                    end;

                    if Print then
                    begin
                      //IMGSize.x := Round(IMGSize.x * ResFactor);
                      //IMGSize.y := Round(IMGSize.y * ResFactor);
                      {$IFDEF TMSDEBUG}
                      TMSLogger.LogSeparator;
                      TMSLogger.Info('bmp : ');
                      TMSLogger.Info(IMGSize.x);
                      TMSLogger.Info(IMGSize.y);
                      TMSLogger.LogSeparator;
                      {$ENDIF}
                    end;

                    if (w + IMGSize.x > r.Right-r.Left) and
                       (IMGSize.x < r.Right - r.Left) then
                    begin
                      ImgBreak := True;
                    end
                    else
                      begin
                        w := w + IMGSize.x + 2;
                        cr.left := cr.left + IMGSize.x + 2;
                        if IMGSize.y > h then
                          h := IMGSize.y;
                      end;

                    txth := Canvas.TextHeight('gh');

                    if Pos('ALIGN',TagProp) > 0 then
                    begin
                      if Pos('"TOP',TagProp) > 0 then
                      begin
                        if h > txth then
                          ImgAli := h - txth
                        else
                          ImgAli := 0;
                      end
                      else
                      begin
                        if Pos('"MIDDLE',TagProp) > 0 then
                        begin
                          if (h > txth) then
                            ImgAli := (h - txth) shr 1
                          else
                            ImgAli := 0;
                        end;
                      end;
                    end;

                    if (Pos('WRAP',TagProp) > 0) then
                    begin
                      h := txth;
                      ImgAli := 0;
                    end;

                    if imgfloat = fsLeft then
                    begin
                      ImgAli := h - txth;
                      imgoffs := 0;
                      floatrect.left := imgw + 2;
                      floatrect.bottom := imgh;
                      if not calc then
                        h := txth;
                    end;

                    if imgfloat = fsRight then
                    begin
                      if h > txth then
                        ImgAli := h - txth
                      else
                        ImgAli := 0;

                      imgoffs := 0;
                      floatrect.right := imgw + 2;
                      floatrect.bottom := imgh;
                      if not calc then
                        h := txth;
                      w := fr.Right;
                    end;

                  end;
                end;
          'L':begin
                NewColor := clNone;
                LType := '';

                if (VarPos('>',s,TagPos)>0) then
                begin
                  TagProp := Uppercase(Copy(s,3,TagPos-1));

                  if VarPos('TYPE',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);
                    LType := Prop;
                  end;
                  if VarPos('COLOR',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);

                    if Length(Prop) > 0 then
                    begin
                      if Prop[1] = '#' then
                        NewColor := Hex2Color(Prop)
                      else
                        NewColor := Text2Color(AnsiLowerCase(prop));
                    end;
                  end;
                end;

                w := w + LISTINDENT * ListIndex;
                if Linkbreak then
                  Imgbreak := True else Linkbreak := True;

                cr.left := cr.left + LISTINDENT * (ListIndex - 1);

                if not calc and not Invisible then
                begin
                  if LType = '' then
                  begin
                    if NewColor <> clNone then
                    begin
                      ColL := Canvas.Font.Color;
                      Canvas.Font.Color := NewColor;
                    end;
                    Prop := Canvas.Font.Name;
                    Canvas.Font.Name := 'Symbol';

                    if not OList then
                    begin
                      if Odd(ListIndex) then
                        DrawText(Canvas.Handle,'�',1,cr,0)
                      else
                        DrawText(Canvas.Handle,'o',1,cr,0);
                    end
                    else
                    begin
                      OListIndexStr := inttostr(OListIndex)+'. ';
                      Inc(OListIndex);

                      DrawText(Canvas.Handle,PChar(OListIndexStr),Length(OListIndexStr),cr,0);
                      cr.Left := cr.Left + Canvas.TextWidth(OListIndexStr) - 12;
                    end;

                    Canvas.Font.Name := prop;
                    if NewColor <> clNone then
                      Canvas.Font.Color := ColL;
                  end
                  else
                  begin
                    ColB := clNone;

                    if LTYPE = 'SQUARE' then
                    begin
                      ColB := Canvas.Brush.Color;
                      Canvas.Pen.Color := NewColor;
                      Canvas.Brush.Color := NewColor;
                      Canvas.Brush.Style := bsSolid;
                      Canvas.Rectangle(cr.Left, cr.Top + 2, cr.Left + 8, cr.Top + 10);
                      Canvas.Brush.Color := ColB;
                      if ColB = clNone then
                        Canvas.Brush.Style := bsClear;
                    end;

                    if LTYPE = 'CIRCLE' then
                    begin
                      ColB := Canvas.Brush.Color;
                      Canvas.Pen.Color := NewColor;
                      Canvas.Brush.Color := NewColor;
                      Canvas.Brush.Style := bsSolid;
                      Canvas.Ellipse(cr.Left, cr.Top + 2, cr.Left + 8, cr.Top + 10);
                      Canvas.Brush.Color := ColB;
                      if ColB = clNone then
                        Canvas.Brush.Style := bsClear;
                    end;

                    Canvas.Brush.Style := bsClear;
                  end;
                end;
                cr.Left := cr.Left + LISTINDENT;
              end;
          'U':begin
                if s[3] <> '>' then
                begin
                  Inc(ListIndex);
                  if w > 0 then
                    LineBreak := true;
                  OList := false;
                end
                else
                  Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
              end;
          'O':begin
                TagChar := Upcase(s[3]);
                if TagChar = 'L' then  // <OFS> tag
                begin
                  Inc(ListIndex);
                  if w > 0 then
                    LineBreak := true;
                  OList := true;
                  OListIndex := 1;
                end;

                if TagChar = 'F' then  // <OFS> tag
                begin
                  TagProp := Copy(s,3,pos('>',s) - 1);
                  Prop := Copy(TagProp,ipos('x',TagProp) + 2,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                  val(Prop,NewOffsetX,err);
                  cr.Left := fr.Left + NewOffsetX;
                  w := NewOffsetX;
                end
              end;
          'P':begin
                //if calc then
                //  Linebreak := true
                //else
                begin
                  if (VarPos('>',s,TagPos)>0) then
                  begin
                    TagProp := Uppercase(Copy(s,3,TagPos-1));

                    if VarPos('ALIGN',TagProp,TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',prop)-1);

                      if Pos('RIGHT',Prop) > 0 then Align := taRightJustify;
                      if Pos('LEFT',Prop) > 0 then Align := taLeftJustify;
                      if Pos('CENTER',Prop) > 0 then Align := taCenter;
                    end;

                    if VarPos('INDENT',TagProp,TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',prop)-1);
                      PIndent := IStrToInt(Prop);
                    end;

                    if VarPos('LINE-HEIGHT',TagProp,TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',prop)-1);

                      lhf := IStrToFloat(Prop);
                      if lhf = 0 then
                        lhf := 1;
                    end;

                    if VarPos('FLOAT',TagProp,TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',prop)-1);

                      if Prop = 'LEFT' then
                        txtfloat := fsLeft;

                      if Prop = 'RIGHT' then
                        txtfloat := fsRight;

                      floatpt := Point(r.Left, r.Top);
                    end;

                    if (VarPos('BGCOLOR',TagProp,TagPos) > 0) and not Selected then
                    begin
                      Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                      NewColor := clNone;

                      if Length(Prop) > 0 then
                      begin
                        if Prop[1] = '#' then
                          NewColor := Hex2Color(Prop)
                        else
                          NewColor := Text2Color(AnsiLowerCase(prop));
                      end;

                      if (VarPos('BGCOLORTO',TagProp,TagPos) > 0) and not Selected then
                      begin
                        Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                        Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                        Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                        NewColorTo := clNone;

                        if Length(Prop) > 0 then
                        begin
                          if Prop[1] = '#' then
                            NewColorTo := Hex2Color(Prop)
                          else
                            NewColorTo := Text2Color(AnsiLowerCase(prop));
                        end;
                        if not Calc then
                        begin
                          isPara := True;
                          Canvas.Pen.Color := Newcolor;
                          DrawHTMLGradient(Canvas,NewColor,NewColorTo,clNone,64,Rect(fr.left,r.top,fr.right,r.bottom+2),true);
                          Canvas.Brush.Style := bsClear
                        end;
                      end
                      else
                      begin
                        if not Calc then
                        begin
                          isPara := True;
                          paracolor := Canvas.Brush.Color;
                          if Canvas.Brush.Style = bsClear then ParaColor := clNone;
                          Canvas.Brush.color := NewColor;
                          PenColor := Canvas.Pen.Color;
                          Canvas.Pen.Color := Newcolor;
                          Canvas.Rectangle(fr.left,r.top,fr.right,r.bottom);
                        end;
                      end;
                    end;
                  end;
                end;
            end;
        'F':begin
              if (VarPos('>',s,TagPos)>0) then
              begin
                if Canvas.Brush.Style = bsClear then
                  BGColor := clNone
                else
                  BGColor := Canvas.Brush.Color;

                TagProp := UpperCase(Copy(s,6,TagPos-6));

                if (VarPos('FACE',TagProp,TagPos) > 0) then
                begin
                  Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                  Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos('"',prop)-1);
                  Canvas.Font.Name := Prop;
                end;

                if (VarPos(' COLOR',TagProp,TagPos) > 0) and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',prop)+1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',prop)-1);

                  if not Calc then
                  begin
                    if not isURL then
                      OldFont.Color := Canvas.Font.Color;

                    if Length(Prop) > 0 then
                    begin
                      if Prop[1] = '#' then
                        Canvas.Font.Color := Hex2Color(Prop)
                      else
                        Canvas.Font.Color := Text2Color(AnsiLowerCase(prop));
                    end;
                  end;
                end;

                if (VarPos('BGCOLOR',TagProp,TagPos)>0) and not Calc and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+7,Length(TagProp));
                  Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos('"',prop)-1);

                  if Length(Prop) > 0 then
                  begin
                    if not Calc then
                    begin
                      if Prop[1] = '#' then
                        Canvas.Brush.Color := Hex2Color(Prop)
                      else
                        Canvas.Brush.Color := Text2Color(AnsiLowerCase(prop));
                    end;
                  end;
                end;

                if (VarPos('STYLE',TagProp,TagPos)>0) then
                begin
                  Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                  Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                  Prop := Copy(Prop,Pos('"',Prop)+1,Length(Prop));
                  // font-size:14pt
                  if Pos('FONT-SIZE:', Uppercase(Prop)) > 0 then
                  begin
                    Delete(Prop, 1, 10);

                    if pos('PT', UpperCase(Prop)) > 0 then
                    begin
                      Prop := Trim(Copy(Prop,1, Pos('PT',Uppercase(Prop))-1));
                      Canvas.Font.Size := IStrToInt(Prop);
                    end;
                  end;
                end
                else
                if (VarPos('SIZE',TagProp,TagPos) > 0) then
                begin
                  Prop := Copy(TagProp,TagPos + 4,Length(TagProp));
                  Prop := Copy(Prop,Pos('=',Prop) + 1,Length(Prop));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));

                  case IStrToInt(Prop) of
                  1:Canvas.Font.Size := OldFont.Size + 1;
                  2:Canvas.Font.Size := OldFont.Size + 2;
                  3:Canvas.Font.Size := OldFont.Size + 4;
                  4:Canvas.Font.Size := OldFont.Size + 6;
                  5:Canvas.Font.Size := OldFont.Size + 8;
                  else
                    if Scaled then
                    begin
                      if ADPIScale <> -1 then
                       Canvas.Font.Height := - Round(IStrToInt(Prop)* ADPIScale*96/72)
                     else
                       Canvas.Font.Height := - Round(IStrToInt(Prop)* GetDeviceCaps(Canvas.Handle, LOGPIXELSX)/72)
                    end
                    else
                      Canvas.Font.Height := - Round(IStrToInt(Prop)* 96/72);
                  end;

                end;
              end;
            end;
        'S':begin
              TagChar := Upcase(s[3]);

              if TagChar = '>' then
                Canvas.Font.Style := Canvas.Font.Style + [fsStrikeOut]
              else
              begin
                if TagChar = 'T' then  // STRIKE, STRONG
                begin
                  if (Length(s) > 4) and (Upcase(s[5]) = 'O') then // STRONG
                    Canvas.Font.Style := Canvas.Font.Style + [fsBold]
                  else
                    Canvas.Font.Style := Canvas.Font.Style + [fsStrikeOut]
                end
                else
                if TagChar = 'H' then  // SHADOW
                begin
                  isShad := True
                end
                else
                if TagChar = 'P' then  // SPAN
                begin
                  if (VarPos('>',s,TagPos)>0) then
                  begin

                    TagProp := UpperCase(Copy(s,6,TagPos-6));
                    if (VarPos('STYLE',TagProp,TagPos)>0) then
                    begin
                      Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                      Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                      Prop := Copy(Prop,Pos('"',Prop)+1,Length(Prop));
                      // font-size:14pt
                      if Pos('BACKGROUND-COLOR:', Uppercase(Prop)) > 0 then
                      begin
                        Delete(Prop, 1, 17);

                        if pos('#', UpperCase(Prop)) = 1 then
                        begin
                          BGColor := Canvas.Brush.Color;
                          Canvas.Brush.Color := Hex2Color(Prop);
                        end;
                      end;
                    end;
                  end;
                end
                else
                begin
                  if ipos('<SUB>',s)=1 then
                    isSub := True
                  else
                    if ipos('<SUP>',s)=1 then
                      isSup := True;
                end;
              end;
            end;
        'R':begin
              TagProp := Copy(s,3,pos('>',s)-1);
              prop := Copy(TagProp,ipos('a',TagProp)+2,Length(TagProp));
              prop := Copy(prop,pos('"',prop)+1,Length(prop));
              prop := Copy(prop,1,pos('"',prop)-1);
              Val(prop,Indent,err);
              StartRotated(Canvas,indent);
            end;
        'Z':Invisible := True;
        end;
      end;

      if (VarPos('>',s,TagPos)>0) and not ImgBreak then
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
      InflateRect(rr,1,0);
      if not Calc then
        Canvas.DrawFocusRect(rr);
      rr.Left := r.Left + 1;
      rr.Top := rr.Bottom;
    end;

    Result := Res;
  end;

  function DrawTextBidimodeFlags (DrawStyle : integer ; BidiMode : TBidiMode) : integer;
  var
    bool : boolean;

  begin
    bool := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
    Result := DrawStyle;
    // do not change center alignment
    if bool then
      if Result and DT_RIGHT = DT_RIGHT then
        Result := Result and not DT_RIGHT { removing DT_RIGHT, makes it DT_LEFT }
      else
      if not (Result and DT_CENTER = DT_CENTER) then
        Result := Result or DT_RIGHT;

    bool := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);

    if bool then
      Result := Result or DT_RTLREADING;
  end;

begin
  if ADPIScale > 0 then
    scale := ADPIScale
  else
    scale := GetDeviceCaps(Canvas.Handle, LOGPIXELSY) / 96;

  Anchor := False;
  Error := False;
  OldFont := TFont.Create;
  OldFont.Assign(Canvas.Font);
  DrawFont := TFont.Create;
  DrawFont.Assign(Canvas.Font);
  CalcFont := TFont.Create;
  CalcFont.Assign(Canvas.Font);
  OldDrawfont := TFont.Create;
  OldDrawFont.Assign(Canvas.Font);
  OldCalcFont := TFont.Create;
  OldCalcFont.Assign(Canvas.Font);
  BlnkColor := Canvas.Brush.color;
  Canvas.Brush.Color := clNone;
  BGColor := clNone;
  ParaColor := clNone;
  isPara := False;
  isShad := False;
  isURL := false;
  Invisible := False;

  OfsX := 0;
  NewOfsX := 0;

  Result := False;

  txtfloat := fsNone;
  floatrect := Rect(0,0,0,0);
  r := fr;
//  r.Left := r.Left + 1; {required to add offset for DrawText problem with first capital W letter}

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
  lhf := 1;

  LISTINDENT := MulDiv(12, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 96);

  ImgIdx := 0;
  AltImg := -1;

  SetBKMode(Canvas.Handle,TRANSPARENT);

  DrawStyle := DT_LEFT or DT_SINGLELINE or DT_EXTERNALLEADING or DT_BOTTOM  or DT_EXPANDTABS; // or DT_NOPREFIX;

  DrawStyle := DrawTextBidimodeFlags(DrawStyle,bidiMode);

  if Pos(' & ',s) > 0 then
    DrawStyle := DrawStyle or DT_NOPREFIX;


  if not WordWrap then
    DrawStyle := DrawStyle or DT_END_ELLIPSIS;

  if Pos('&',s) > 0 then
  begin
    s := ConvertHTMLSpecialChars(s);

    repeat
      Foundtag := False;
      if TagReplacestring('&amp;','&&',s) then Foundtag := True;
    until not Foundtag;
  end;

  s := DBTagStrip(s);
  s := UnicodeTagStrip(s);
  s := CRLFStrip(s,True);

  InsPoint := 0;
  imgali := 0;

  while Length(s) > 0 do
  begin
    {calculate part of the HTML text fitting on the next line}
    Oldfont.Assign(OldCalcFont);
    Canvas.Font.Assign(CalcFont);
    Oldanchor := Anchor;
    OldAnchorVal := LastAnchor;
    suph := 0;
    subh := 0;
    isSup := False;
    isSub := False;
    HtmlHeight := Canvas.TextHeight('gh');

    txtHeight := HtmlHeight;

    OldImgIdx := ImgIdx;

    s := Trim(s);

    oldisShad := isShad;

    txtfloatbefore := txtfloat;

    su := HTMLDrawLine(Canvas,s,r,True,HtmlWidth,HtmlHeight,subh,suph,imgali,Align,PIndent,XPos,YPos,HotSpot,ImageHotSpot,ofsx,newofsx, floatrect, txtfloat);

    isShad := oldisShad;

    Anchor := OldAnchor;
    LastAnchor := OldAnchorVal;

    CalcFont.Assign(Canvas.Font);
    OldCalcFont.Assign(OldFont);

    HTMLHeight := round(lhf * (HTMLHeight + LineSpacing));

    dr := r;

    case Align of
    taCenter:if (r.right - r.left - htmlwidth > 0) then
               dr.left := r.left + ((r.right - r.left - htmlwidth) div 2);
    taRightJustify:if r.right - htmlwidth > r.left then
                       dr.left := r.right - htmlwidth;
    end;

    dr.Left := dr.Left + PIndent;

    dr.Bottom := dr.Top + HtmlHeight + Subh + Suph;

    if not CheckHeight then
    begin
      OldFont.Assign(OldDrawFont);
      Canvas.Font.Assign(DrawFont);

      HyperLinks := HlCount;
      ListIndex := LiCount;
      ImgIdx := OldImgIdx;

      HTMLDrawLine(Canvas,su,dr,CheckHotSpot,HtmlWidth,HtmlHeight,subh,suph,ImgAli,Align,PIndent,XPos,YPos,HotSpot,ImageHotspot,ofsx,newofsx, floatrect,txtfloat);

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

      if (HotSpot and
         (YPos > dr.Bottom - ImgAli - Canvas.TextHeight('gh')) and
         (YPos < dr.Bottom - ImgAli)) or ImageHotSpot then
      begin
        Result := True;
      end;

      DrawFont.Assign(Canvas.Font);
      OldDrawFont.Assign(OldFont);
    end;

    ofsx := newofsx;

    if (txtfloat <> fsNone) and (txtfloat <> txtfloatbefore) then
    begin
      r.Left := floatpt.x;
      r.Top := floatpt.y;
      txtfloat := fsNone;
    end
    else
    begin
      //HtmlHeight := Canvas.TextHeight('gh');
      imgali := 0;
      r.top := r.top + HtmlHeight + subh + suph;
      ysize := ysize + HtmlHeight + subh + suph;
    end;

    //do not draw below bottom
    if (r.top + TxtHeight > r.bottom) and not CheckHeight then
      s := '';
  end;

  if (ysize = 0) then
    ysize := Canvas.TextHeight('gh');

  //ysize := ysize + 2;

  InsPoint := InsPoint shr 1;

  Canvas.Brush.Color := BlnkColor;
  Canvas.Font.Assign(OldFont);
  OldFont.Free;
  DrawFont.Free;
  CalcFont.Free;
  OldDrawfont.Free;
  OldCalcfont.Free;
end;
{$WARNINGS ON}

{$IFNDEF REMOVEDRAW}
function HTMLDraw(Canvas:TCanvas;s:string;fr:trect;
                                 FImages:TImageList;
                                 xpos,ypos:integer;
                                 checkhotspot,checkheight,print,selected,blink:boolean;
                                 resfactor:double;
                                 URLColor:tcolor;
                                 var Anchorval,StripVal:string;
                                 var XSize,YSize:integer):boolean;
var
  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  r: TRect;
begin
  Result := HTMLDrawEx(Canvas,s,fr,FImages,xpos,ypos,-1,-1,1,checkhotspot,checkheight,print,selected,blink,false,
                       False,true,resfactor,URLColor,clNone,clNone,clGray,anchorval,stripval,focusanchor,xsize,ysize,HyperLinks,MouseLink,r,nil,nil,0);
end;

{$IFNDEF REMOVEIPOSFROM}
function IPosFrom(su,s:string;frm:integer):Integer;
var
  i:Integer;
begin
  i := Pos(AnsiUpperCase(su),AnsiUpperCase(s));
  if i > frm then
    Result := i
  else
    Result := 0;
end;
{$ENDIF}

{$ENDIF}

{$IFNDEF REMOVESTRIP}

function HTMLStrip(s:string):string;
var
  TagPos: integer;
begin
  Result := '';
  // replace line breaks by linefeeds
  while (pos('<BR>',uppercase(s))>0) do s := StringReplace(s,'<BR>',chr(13)+chr(10),[rfIgnoreCase]);
  while (pos('<HR>',uppercase(s))>0) do s := StringReplace(s,'<HR>',chr(13)+chr(10),[rfIgnoreCase]);

  while (VarPos('<z>',s,TagPos) > 0) do
  begin
    Result := Result + Copy(s,1,TagPos - 1); // copy till Z tag
    if (VarPos('</z>',s,TagPos) > 0) then
      Delete(s,1,TagPos + 3)
    else
      Break;
  end;

  while (VarPos('<Z>',s,TagPos) > 0) do
  begin
    Result := Result + Copy(s,1,TagPos - 1); // copy till Z tag
    if (VarPos('</Z>',s,TagPos) > 0) then
      Delete(s,1,TagPos + 3)
    else
      Break;
  end;


  // remove all other tags
  while (VarPos('<',s,TagPos) > 0) do
  begin
    Result := Result + Copy(s,1,TagPos - 1);
    if (VarPos('>',s,TagPos)>0) then
      Delete(s,1,TagPos)
    else
      Break;
  end;
  Result := Result + s;
end;
{$ENDIF}

{$IFDEF HILIGHT}

function HTMLStripAll(s: string): string;
var
  TagPos: integer;
begin
  Result := '';

  // remove all tags
  while (VarPos('<',s,TagPos) > 0) do
  begin
    Result := Result + Copy(s,1,TagPos-1);
    VarPos('>',s,TagPos);

    if (TagPos > 0) then
      Delete(s,1,TagPos)
    else
      if (TagPos = 0) then
        break;
  end;

  Result := Result + s;
end;

function StripPos2HTMLPos(s:string; i: Integer): Integer;
var
  j,k: Integer;
  Skip: Boolean;
begin
  Result := 0;
  k := 1;
  Skip := False;

  for j := 1 to Length(s) do
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
    r := Pos(AnsiUpperCase(su),AnsiUpperCase(s));

  if r > 0 then
  begin
    Res := h + r;
    Result := Res;
  end;
end;

function HiLight(s,h,tag:string;DoCase:boolean): string;
var
  hs: string;
  l,k: Integer;
begin
  hs := HTMLStripAll(s);

  l := 0;

  while PosFrom(h,hs,l,DoCase,k) > 0 do
  begin
    l := k + Length(h);
    Insert('<'+tag+'>',s,StripPos2HTMLPos(s,k));
    Insert('</'+tag+'>',s,StripPos2HTMLPos(s,l));
  end;

  Result := s;
end;

function UnHiLight(s,tag:string): string;
begin
  Result := '';
  // replace line breaks by linefeeds
  tag := Uppercase(tag);
  while Pos('<'+tag+'>',Uppercase(s)) > 0 do s := StringReplace(s,'<'+tag+'>','',[rfIgnoreCase]);
  while Pos('</'+tag+'>',Uppercase(s)) > 0 do s := StringReplace(s,'</'+tag+'>','',[rfIgnoreCase]);
  Result := s;
end;

{$ENDIF}

{$IFDEF PARAMS}

function IPosv(su,s:string;var vp:integer):integer;
begin
  vp := pos(AnsiUpperCase(su),AnsiUpperCase(s));
  Result := vp;
end;


function GetHREFValue(html,href:string;var value:string):boolean;
var
  lp: Integer;
begin
  Result := False;
  while IPosv('href="',html,lp) > 0 do
  begin
    Delete(html,1,lp+5); {delete all before}
    if IPosv('"',html,lp) > 0 then
    begin
      if CompareText(href,copy(html,1,lp-1))=0 then
      begin
        {href match - get the value now}
        Delete(html,1,lp);
        if (iposv('>',html,lp)>0) then
        begin
          Delete(html,1,lp);
          if (iposv('<',html,lp)>0) then
          begin
            Value := Copy(html,1,lp-1);
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  end;
end;


function SetHREFValue(var html:string;href,value:string):boolean;
var
  h:string;
  p:string;
begin
  {get current value and do a stringreplace}

  Result := False;
  if GetHREFValue(html,href,h) then
  begin
    p := Copy(html,pos('href="' + href,html),Length(html));

    p := StringReplace(p,'>' + h + '</A','>' + value + '</A',[rfIgnoreCase]);

    html := Copy(html,1,pos('href="'+href,html)-1)+p;
    Result := True;
  end;
end;

{$ENDIF}



