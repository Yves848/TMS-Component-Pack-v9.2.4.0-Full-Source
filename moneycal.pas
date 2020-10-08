{***************************************************************************}
{ TCALCOMP component CalForm                                                }
{ for Delphi  & C++Builder                                                  }
{                                                                           }
{ written by TMS Software                                                   }
{          copyright © 1998 - 2019                                          }
{          Email : info@tmssoftware.com                                     }
{          Website : https://www.tmssoftware.com                            }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit Moneycal;

{$I TMSDEFS.INC}
interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Types;

const
  adaysinmonth:array[1..13] of word = (31,28,31,30,31,30,31,31,30,31,30,31,29);
  monames:array[1..12] of string[5] = ('JAN','FEB','MAR','APR','MAY','JUNE','JULY','AUG','SEP','OCT','NOV','DEC');
  selstr = 'Select month';
  labelx = 30;
  labelw = 65;

type
  TDayStr = string;
  TMonthStr = string;

  TDayArray = array[1..14] of TDayStr;
  TMonthArray = array[1..12] of TMonthStr;

  TCalForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    xoffset, yoffset: integer;
    days: TDayArray;
    months: TMonthArray;
    seldate: tdatetime;
    thedate: tdatetime;
    initdate: tdatetime;
    startday: word;
    frstday, frstmonth: integer;
    dx, dy: word;
    lblx1, lblx2: word;
    xposin, yposin: integer;
    flgl, flgr, flgla: boolean;
    daymode: boolean;
    labels: string;
    txtcolor, txtselcolor, txtinvcolor, txtwkndcolor: tcolor;
    formxunit: integer;
    formyunit: integer;
    weekmode: boolean;
    FDPIScale: single;
    { Private declarations }
    procedure SetLabel(mo, ye: word);
    procedure ChangeMonth(dx: integer);
    procedure ChangeYear(dx: integer);
    function DaysInMonth(mo, ye: word): word;
    function DateToWeek(da, mo, ye: word): integer;
    procedure PaintArrowLeft;
    procedure PaintArrowRight;
    procedure PaintLabel;
    procedure PaintProc;
    procedure ToggleDayMode;
  protected
    procedure CreateParams(var Params:TCreateParams); override;
  public
    fromdate: tdatetime;
    todate: tdatetime;

    constructor Create(AOwner: TComponent); override;
    procedure SetDate(da, mo, ye: word);
    procedure GetDate(var da, mo, ye: word);
    procedure SetStartDay(aday: word);
    procedure SetColors(textcolor, selectcolor, inverscolor, weekendcolor: tcolor);
    procedure SetNameofDays(d: TDayArray);
    procedure SetNameofMonths(m: TMonthArray);
    procedure SetWeeks(onoff: boolean);
    procedure SetStarts(d, m: integer);
    { Public declarations }
  published
    property DPIScale: single read FDPIScale write FDPIScale;
  end;

var
  CalForm: TCalForm;

implementation

{$R *.DFM}

uses
  AdvStyleIF;

constructor TCalForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  xoffset := 0;
  yoffset := 16;
  weekmode := false;
end;

procedure TCalForm.CreateParams(var Params:TCreateParams);
begin
  inherited CreateParams(Params);
{
 params.style:=params.style or WS_THICKFRAME;
}
  params.style:=params.style or WS_DLGFRAME;
end;

procedure TCalForm.SetLabel(mo, ye: word);
begin
  if daymode then
    labels := months[mo] + ' ' + inttostr(ye)
  else
    labels := inttostr(ye);
end;

function TCalForm.DaysInMonth(mo, ye: word): word;
begin
  if (mo <> 2) then
    DaysInMonth := adaysinmonth[mo]
  else
  begin
    if (ye mod 4 = 0) then
      DaysInMonth := 29
    else
      DaysInMonth := 28;
    if (ye mod 100 = 0) then
      DaysInMonth := 28;
    if (ye mod 400 = 0) then
      DaysInmonth := 29;
  end;
end;


function TCalForm.DateToWeek(da, mo, ye: word): integer;
var
  days1, days2: real;
  d1, d2: tdatetime;
  fday: integer;

begin
  d2 := encodedate(ye, mo, da);
  if (frstmonth > mo) then
    dec(ye)
  else
    if ((frstmonth = mo) and (frstday > da)) then
      dec(ye);

  d1 := encodedate(ye, frstmonth, frstday);
  fday := dayofweek(d1);

  days2 := int(d2);
  days1 := int(d1);

  days1 := (days2 - days1) + (fday - 2);

  DateToWeek := (trunc(days1) div 7) + 1;
end;


procedure TCalForm.SetWeeks(onoff: boolean);
begin

  with Canvas do
  begin
    if onoff then
      formxunit := (textwidth('99') + 6)
    else
      formxunit := (textwidth('99') + 6);
    formyunit := (textheight('99') + 4);
  end;

  if onoff then
  begin
    xoffset := 20;
    Width := formxunit * 8 + 10;
    weekmode := true;
    height := formyunit * 7 + 20;
  end
  else
  begin
    xoffset := 0;
    Width := formxunit * 7 + 10;
    weekmode := false;
    height := formyunit * 7 + 20;
  end;
end;

procedure TCalForm.SetNameofDays(d: TDayArray);
begin
  days := d;
  days[8] := days[1];
  days[9] := days[2];
  days[10] := days[3];
  days[11] := days[4];
  days[12] := days[5];
  days[13] := days[6];
  days[14] := days[7];
end;

procedure TCalForm.SetNameofMonths(M: TMonthArray);
begin
  months := m;
end;

procedure TCalForm.SetStarts(D, M: integer);
begin
  frstday := d;
  frstmonth := m;
end;

procedure TCalForm.SetColors(textcolor,selectcolor,inverscolor,weekendcolor:tcolor);
begin
  txtcolor := textcolor;
  txtselcolor := selectcolor;
  txtinvcolor := inverscolor;
  txtwkndcolor := weekendcolor;
end;

procedure TCalForm.ChangeMonth(DX: integer);
var
  ye, mo, da: word;
begin

  decodedate(thedate, ye, mo, da);
  mo := mo + dx;

  if (mo = 13) then
  begin
    inc(ye);
    mo := 1;
  end;

  if (mo = 0) then
  begin
    dec(ye);
    mo := 12;
  end;

  if (da > daysinmonth(mo, ye)) then
    da := daysinmonth(mo, ye);

  thedate := encodedate(ye, mo, da);
  seldate := thedate;
  SetLabel(mo, ye);
  Repaint;
end;


procedure TCalForm.ChangeYear(dx:integer);
var
  ye, mo, da: word;

begin
  decodedate(thedate, ye, mo, da);
  ye := ye + dx;

  thedate := encodedate(ye, mo, da);
  seldate := thedate;
  SetLabel(mo, ye);

  Repaint;
end;

procedure TCalForm.PaintArrowLeft;
begin

  with Canvas do
  begin
    if flgl then
    begin
      Brush.Color := txtselcolor;
      Pen.Color := txtselcolor;
    end
    else
    begin
      Brush.Color := txtcolor;
      Pen.Color := txtcolor;
    end;

    if daymode then
      polygon([Point(Round(FDPIScale * (xoffset + 10)), 0), Point(Round(FDPIScale * (xoffset + 4)), Round(FDPIScale * 6)),Point(Round(FDPIScale * (xoffset + 10)), Round(FDPIScale * 12))])
    else
     polygon([Point(Round(FDPIScale *10), 0), Point(Round(FDPIScale * 4), Round(FDPIScale * 6)),Point(Round(FDPIScale * 10), Round(FDPIScale * 12))]);

    Brush.Color := Color;
   end;
end;

procedure TCalForm.PaintArrowRight;
var
  RectRight: integer;
begin
  with Canvas do
  begin
    if flgr then
    begin
      Brush.Color := txtselcolor;
      Pen.Color := txtselcolor;
    end
    else
    begin
      Brush.Color := txtcolor;
      Pen.Color := txtcolor;
    end;

    RectRight := GetClientRect.Right - 2;

    Polygon([Point(RectRight - Round(FDPIScale * 10), 0), Point(RectRight - Round(FDPIScale * 4), Round(FDPIScale * 6)),Point(RectRight - Round(FDPIScale * 10), Round(FDPIScale * 12))]);
    Brush.Color := Color;
   end;
end;

procedure TCalForm.PaintLabel;
var
  buf: array[0..12] of char;
  l: longint;
  xofs: integer;
begin
  with Canvas do
  begin
    strpcopy(buf, labels);

    l := textwidth(labels);

    if flgla then
      Font.Color := txtselcolor
    else
      Font.Color := txtcolor;
    setbkmode(handle, TRANSPARENT);

   if daymode then
     xofs := xoffset
   else
     xofs := 0;

   textout(xofs + ((Width - loword(l) - xofs) shr 1), 0, labels);
   Font.Color := txtcolor;
   lblx1 := (Width - loword(l) - xofs) shr 1;
   lblx2 := lblx1 + loword(l);
  end;
end;

procedure TCalForm.PaintProc;
var
  i, j: word;
  d: tdatetime;
  da, mo, ye, pmo, pye, sda, cda, cmo, cye: word;
  fd: integer;
  tmpstr: string;

  function SmallCaps(s: string): string;
  var
    buf:array[0..10] of char;
  begin
    strpcopy(buf, s);
    strlower(buf);
    s := strpas(buf);
    s[1] := upcase(s[1]);
    SmallCaps := s;
  end;

begin
  decodedate(seldate, ye, mo, sda);
  decodedate(thedate, ye, mo, da);
  decodedate(now, cye, cmo, cda);

  with Canvas do
  begin
   dx := TextWidth('99') + Round(FDPIScale * 6);
   dy := TextHeight('99') + Round(FDPIScale * 4);
  end;

  PaintArrowLeft;
  PaintArrowRight;
  PaintLabel;

  if daymode then
  begin
    d := encodedate(ye, mo, 1);

   {first day of the month}

    fd := dayofweek(d) - 1 - startday;

    if (fd < 0) then
      fd := fd + 7;

    if (mo = 1) then
    begin
      pmo := 12;
      pye := ye - 1;
    end
    else
    begin
      pmo := mo - 1;
      pye := ye;
    end;

    with Canvas do
    begin
      Font.Color := txtcolor;
      setbkmode(handle, TRANSPARENT);

      if weekmode then
        begin
          Pen.Color := txtcolor;
          moveto(18, dy - 3 + Round(FDPIScale * yoffset));
          lineto(18, height);

          j := datetoweek(1, mo, ye);

          for i := 1 to 6 do
          begin
            if (i + j - 1) > 53 then
              tmpstr := inttostr(i + j - 1 - 53)
            else
              tmpstr := inttostr(i + j - 1);
            textout(2, i * dy + Round(FDPIScale * yoffset), tmpstr);
          end;
        end;

     {draw days here}
      settextalign(handle, TA_RIGHT);
      for i:=1 to 7 do
        textout((i) * dx + Round(FDPIScale * xoffset) , Round(FDPIScale * yoffset), days[i + startday - 1]);


      settextalign(handle,TA_RIGHT);
      {draw line under days}
      Pen.Color := clBlack;
      MoveTo(Round(FDPIScale * xoffset), dy - 5 + Round(FDPIScale * yoffset));
      LineTo(Round(FDPIScale * xoffset) + 7 * dx + 6, dy - 5 + Round(FDPIScale * yoffset));
      Pen.Color := clWhite;
      MoveTo(Round(FDPIScale * xoffset), dy - 4 + Round(FDPIScale * yoffset));
      LineTo(Round(FDPIScale * xoffset) + 7 * dx + 6, dy - 4 + Round(FDPIScale * yoffset));
      Pen.Color := clBlack;

      {draw numbers}
      for i := 1 to 7 do
        for j := 1 to 6 do
        begin
          if (fd >= (i + (j - 1) * 7)) then
          begin
            Font.Color := txtinvcolor;
            SetBkMode(handle, TRANSPARENT);

            TextOut(Round(FDPIScale * xoffset) + i * dx, j * dy + Round(FDPIScale * yoffset), inttostr(daysinmonth(pmo, pye) - (fd - i)));
          end
          else
          begin
            if ((i + (j - 1) * 7 - fd) > DaysInMonth(mo, ye)) {and ( j * dy + Round(FDPIScale * yoffset)} then
            begin
              Font.Color := txtinvcolor;
              SetBkMode(handle, TRANSPARENT);
              TextOut(Round(FDPIScale * xoffset) + i * dx, j * dy + Round(FDPIScale * yoffset), inttostr(i + (j - 1) * 7 - fd - daysinmonth(mo, ye)));
            end
            else
            begin
              if (sda = i + (j - 1) * 7 - fd) then
              begin
                Brush.Color := txtselcolor;
                Pen.Color := txtselcolor;
                Font.Color := txtinvcolor;
                textout(Round(FDPIScale * xoffset) + i * dx, j * dy + Round(FDPIScale * yoffset), inttostr(i + (j - 1) * 7 - fd));
                Brush.Color := Color;
                Pen.Color := txtcolor;
              end
              else
              begin {check to see if weekend day here}
                if (dayofweek(encodedate(ye, mo, i + (j - 1) * 7 - fd)) in [1, 7]) then
                  Font.Color := txtwkndcolor
                else
                  Font.Color := txtcolor;
                SetBkMode(handle, TRANSPARENT);
                TextOut(Round(FDPIScale * xoffset) + i * dx, j * dy + Round(FDPIScale * yoffset), inttostr(i + (j - 1) * 7 - fd));
               end;

             end;
           end;

          if (cda = i + (j - 1) * 7 - fd) and (cmo = mo) and (cye = ye) then
          begin
            Pen.Color := clGray;
            MoveTo(Round(FDPIScale * xoffset) + (i - 1) * dx + 2, (j + 1) * dy - 4 + Round(FDPIScale * yoffset));
            LineTo(Round(FDPIScale * xoffset) + (i - 1) * dx + 2, j * dy - 2 + Round(FDPIScale * yoffset));
            LineTo(Round(FDPIScale * xoffset) + i * dx, j * dy - 2 + Round(FDPIScale * yoffset));
            Pen.Color := clWhite;
            LineTo(Round(FDPIScale * xoffset) + i * dx, (j + 1) * dy - 4 + Round(FDPIScale * yoffset));
            LineTo(Round(FDPIScale * xoffset) + (i - 1) * dx + 2, (j + 1) * dy - 4 + Round(FDPIScale * yoffset));
            Pen.Color := clBlack;
          end;
        end;
    end;
  end
 else
  begin
    with Canvas do
    begin
      {draw days here}
      TextOut((Width-TextWidth(selstr)) shr 1, Round(FDPIScale * yoffset), selstr);

      {draw line under days}
      Pen.Color := clBlack;
      MoveTo(0, dy - 5 + Round(FDPIScale * yoffset));
      LineTo(width, dy - 5 + Round(FDPIScale * yoffset));
      Pen.Color := clWhite;
      MoveTo(0, dy - 4 + Round(FDPIScale * yoffset));
      LineTo(width, dy - 4 + Round(FDPIScale * yoffset));
      Pen.Color := clBlack;

      dx := width div 4;
      dy := (height - Round(FDPIScale * yoffset)) div 4;

      for i := 1 to 3 do
        for j := 1 to 4 do
        begin
          if j + (i - 1) * 4 = mo then
          begin
            Brush.Color := txtselcolor;
            Pen.Color := txtselcolor;
            Font.Color := txtinvcolor;
            TextOut((j - 1) * dx + 5, Round(FDPIScale * yoffset) - 10 + dy * i, SmallCaps(months[j + (i - 1) * 4]));
            Brush.Color := Color;
            Pen.Color := txtcolor;
            Font.Color := txtcolor;
            SetBkMode(handle, TRANSPARENT);
          end
          else
            TextOut((j - 1) * dx + 5, dy * i + Round(FDPIScale * yoffset) - 10, SmallCaps(months[j + (i - 1) * 4]));
        end;
    end;
  end;
end;

procedure TCalForm.FormCreate(Sender: TObject);
begin
  thedate := now;
  seldate := thedate;
  daymode := true;
  ChangeMonth(0);
  flgl := false;
  flgr := false;
  flgla := false;
end;

procedure TCalForm.ToggleDayMode;
var
  da, mo, ye: word;
begin
  daymode := not daymode;
  decodedate(seldate, ye, mo, da);
  SetLabel(mo, ye);
  Repaint;
end;

procedure TCalForm.SetStartDay(aday: word);
begin
  startday := aday;
  Repaint;
end;

procedure TCalForm.SetDate(da, mo, ye: word);
begin
  thedate := encodedate(ye,mo,da);
  seldate := theDate;
  Repaint;
  Setlabel(mo, ye);
  initdate := seldate;
end;

procedure TCalForm.GetDate(var da, mo, ye: word);
begin
  DecodeDate(seldate, ye, mo, da);
end;

procedure TCalForm.FormShow(Sender: TObject);
begin
  SetCursor(LoadCursor(0, IDC_ARROW));
  SetCapture(Handle);
end;

procedure TCalForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ReleaseCapture;
end;

procedure TCalForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  RectArrowRight: integer;
begin
  if daymode then
    x := x - Round(FDPIScale * xoffset);

  if (x >= lblx1) and (x <= lblx2) and (y > 0) and (y < 15) then
  begin
    if flgla = false then
    begin
      flgla := true;
      PaintLabel;
    end;
  end
  else
    if flgla then
    begin
      flgla := false;
      PaintLabel;
    end;

  if (x > 5) and (x < Round(FDPIScale * (xoffset + 10))) and (y > 0) and (y < Round(FDPIScale * 12)) then
  begin
    if flgl=false then
      begin
        flgl:=true;
        PaintArrowLeft;
      end;
  end
  else
    if flgl then
    begin
      flgl:=false;
      PaintArrowLeft;
    end;

  RectArrowRight := GetClientRect.Right - 2;

  if (x > (RectArrowRight - Round(FDPIScale * 10))) and ( x < RectArrowRight) and (y > 0) and (y < Round(FDPIScale * 12)) then
  begin
    if flgr = false then
    begin
      flgr := true;
      PaintArrowRight;
    end;
  end
  else
    if flgr then
    begin
      flgr := false;
      PaintArrowRight;
    end;
end;

procedure TCalForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ye, mo, da: word;
  xcal, ycal: integer;
  sda, fd: integer;
  xofs: integer;

begin
  if (Button <> mbLeft) then
    exit;

  xposin := $7fff;
  yposin := $7fff;

  if (x < 0) or (y < 0) or (x >= self.width) or (y >= self.height - 4) then
  begin
    Close;
    Exit;
  end;

  if daymode then
    x := x - Round(FDPIScale * xoffset);

  if daymode then
    xofs := 0
  else
    xofs := Round(FDPIScale * xoffset);

  if (x >= lblx1) and (x <= lblx2) and (y > 0) and (y < 15) then
  begin
    ToggleDayMode;
    Exit;
  end;

  if (x > 5) and (x < 15) and (y > 0) and (y < 15) then
  begin
    if daymode then
      ChangeMonth(-1)
    else
      ChangeYear(-1);
    Exit;
  end;

  if (x > (formxunit shr 1) * 13 + xofs) and (x < (formxunit shr 1) * 13 + 10 + xofs) and (y > 0) and (y < 15) then
  begin
    if daymode then
      ChangeMonth(1)
    else
      ChangeYear(1);
    Exit;
  end;

  if daymode then
  begin
    if (y > dy + Round(FDPIScale * yoffset)) then
    begin
      xposin := x;
      yposin := y;

      decodedate(seldate, ye, mo, da);

      seldate := encodedate(ye, mo, 1);
      fd := dayofweek(seldate) - 1 - startday;

      if (fd < 0) then
        fd := fd + 7;

      xcal := x div dx;
      ycal := ((y - Round(FDPIScale * yoffset)) - dy) div dy;

      if (xcal > 6) then
        xcal := 6;
      if (ycal > 5) then
        ycal := 5;

      sda := xcal + 7 * ycal - fd + 1;

      if (sda < 1) then
         begin
          dec(mo);
          if (mo = 0) then
             begin
               mo := 12;
               dec(ye);
             end;
          sda := daysinmonth(mo, ye) + sda;
          changemonth(-1);
         end;

       if (sda > daysinmonth(mo, ye)) then
         begin
          sda := sda - daysinmonth(mo,ye);
          inc(mo);
          if (mo > 12) then
             begin
               mo := 1;
               inc(ye);
             end;
          changemonth(+1);
         end;

      da := sda;

      seldate := encodedate(ye, mo, da);
      thedate := seldate;

      Repaint;
    end;
  end
  else
  begin
    with Self do
    begin
      if (y > Round(FDPIScale * yoffset) + 12) then
      begin
        DecodeDate(seldate, ye, mo, da);

        fd := 1 + (x div (width div 4));
        ycal := 4 * ((y - Round(FDPIScale * yoffset) + 10) div ((height - Round(FDPIScale * yoffset)) div 4) - 1);
        if ycal < 0 then
          ycal := 0;

        fd := fd + ycal;
        mo := fd;
        if (mo > 12) then mo := mo - 4;

        if (da > daysinmonth(mo, ye)) then
          da := daysinmonth(mo, ye);

        seldate := encodedate(ye, mo, da);

        thedate := seldate;

        daymode := true;

        SetLabel(mo, ye);
        Repaint;
       end;
     end;
  end;

end;

procedure TCalForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    seldate := initdate;
    Close;
  end;

  if daymode then
    x := x - Round(FDPIScale * xoffset);

  if (abs(x - xposin) < 4) and (abs(y - yposin) < 4) then
  begin
    ModalResult := mrOK;
  end
  else
    SetCapture(Handle);
end;

procedure TCalForm.FormPaint(Sender: TObject);
begin
  PaintProc;
end;

procedure TCalForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (key = #32) then
  begin
    ToggleDayMode;
  end;
  if (key = #27) then
  begin
    seldate := initdate;
    Close;
  end;
  if (key= #13) then
  begin
    ModalResult := mrOK;
  end;
end;

procedure TCalForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  da, mo, ye: word;
begin
  if daymode then
  begin
    if (key = vk_left) then
    begin
      thedate := thedate - 1;
      seldate := thedate;
      DecodeDate(thedate, ye, mo, da);
      SetLabel(mo, ye);
      Repaint;
    end;
    if (key = vk_right) then
    begin
      thedate := thedate + 1;
      seldate := thedate;
      DecodeDate(thedate, ye, mo, da);
      SetLabel(mo, ye);
      Repaint;
      end;
    if (key = vk_up) then
    begin
      thedate := thedate - 7;
      seldate := thedate;
      DecodeDate(thedate, ye, mo, da);
      SetLabel(mo, ye);
      Repaint;
      end;
    if (key = vk_down) then
    begin
      thedate := thedate + 7;
      seldate := thedate;
      decodedate(thedate, ye, mo, da);
      SetLabel(mo, ye);
      Repaint;
    end;
  end;

  if (key = vk_prior) then
  begin
    ChangeMonth(-1);
  end;
  if (key = vk_next) then
  begin
    ChangeMonth(+1);
  end;
end;
end.
