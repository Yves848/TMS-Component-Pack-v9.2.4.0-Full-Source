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

unit AdvPDFImageBook;

{$I TMSDEFS.INC}

interface

uses
  Classes, SysUtils, AdvPDFLib, AdvPDFGraphicsLib;

type
  TAdvPDFImageBookPageNumberPosition = (pnNone, pnTopLeft, pnTopCenter, pnTopRight, pnBottomLeft, pnBottomCenter, pnBottomRight);

  TAdvPDFImageBookPageInfoEvent = procedure(Sender: TObject; PageIndex: integer; var Header, Footer: UnicodeString) of object;

  TAdvPDFImageBookPageDrawEvent = procedure(Sender: TObject; PageIndex: integer; Graphics: IAdvCustomPDFGraphicsLib) of object;

  TAdvPDFImageBook = class
  private
    FPageNumberFormat: Unicodestring;
    FPageNumberPosition: TAdvPDFImageBookPageNumberPosition;
    FTitle: Unicodestring;
    FOnGetPageInfo: TAdvPDFImageBookPageInfoEvent;
    FOnAfterPageDraw: TAdvPDFImageBookPageDrawEvent;
    FOnBeforePageDraw: TAdvPDFImageBookPageDrawEvent;
  protected
    procedure DoGetPageInfo(PageIndex: integer; var Header, Footer: UnicodeString); virtual;
    procedure DoBeforePageDraw(PageIndex: integer; Graphics: IAdvCustomPDFGraphicsLib); virtual;
    procedure DoAfterPageDraw(PageIndex: integer; Graphics: IAdvCustomPDFGraphicsLib); virtual;
  public
    constructor Create;
    procedure GenerateBook(FileName: string; Images: TStrings);

    property Title: Unicodestring read FTitle write FTitle;
    property PageNumberFormat: Unicodestring read FPageNumberFormat write FPageNumberFormat;
    property PageNumberPosition: TAdvPDFImageBookPageNumberPosition read FPageNumberPosition write FPageNumberPosition default pnBottomCenter;
    property OnGetPageInfo: TAdvPDFImageBookPageInfoEvent read FOnGetPageInfo write FOnGetPageInfo;
    property OnBeforePageDraw: TAdvPDFImageBookPageDrawEvent read FOnBeforePageDraw write FOnBeforePageDraw;
    property OnAfterPageDraw: TAdvPDFImageBookPageDrawEvent read FOnAfterPageDraw write FOnAfterPageDraw;
  end;

implementation

uses
  AdvGraphicsTypes, AdvTypes
  {$IFNDEF LCLLIB}
  ,Types
  {$ENDIF}
  ;

{ TAdvPDFImageBook }

constructor TAdvPDFImageBook.Create;
begin
  inherited;
  FPageNumberPosition := pnBottomCenter;
  FPageNumberFormat := 'Page %d of %d';
  FTitle := '';
end;

procedure TAdvPDFImageBook.DoAfterPageDraw(PageIndex: integer;
  Graphics: IAdvCustomPDFGraphicsLib);
begin
  if Assigned(OnAfterPageDraw) then
    OnAfterPageDraw(Self, PageIndex, Graphics);
end;

procedure TAdvPDFImageBook.DoBeforePageDraw(PageIndex: integer;
  Graphics: IAdvCustomPDFGraphicsLib);
begin
  if Assigned(OnBeforePageDraw) then
    OnBeforePageDraw(Self, PageIndex, Graphics);
end;

procedure TAdvPDFImageBook.DoGetPageInfo(PageIndex: integer; var Header,
  Footer: UnicodeString);
begin
  if Assigned(OnGetPageInfo) then
    OnGetPageInfo(Self, PageIndex, Header, Footer);
end;

procedure TAdvPDFImageBook.GenerateBook(FileName: string; Images: TStrings);
var
  pdf: TAdvPDFLib;
  i: integer;
  r: TRectF;
  pagnr, hdr, ftr: UnicodeString;
  topmargin, bottommargin, th: integer;
begin
  if Images.Count = 0 then
    raise Exception.Create('No images specified');

  pdf := TAdvPDFLib.Create;

  try
    pdf.BeginDocument(FileName);

    case PageNumberPosition of
      pnTopLeft: pdf.HeaderAlignment := gtaLeading;
      pnTopCenter: pdf.HeaderAlignment := gtaCenter;
      pnTopRight: pdf.HeaderAlignment := gtaTrailing;
      pnBottomLeft: pdf.HeaderAlignment := gtaLeading;
      pnBottomCenter: pdf.HeaderAlignment := gtaCenter;
      pnBottomRight: pdf.HeaderAlignment := gtaTrailing;
    end;


    for i := 0 to Images.Count - 1 do
    begin
      {$IFDEF LCLLIB}
      pagnr := UTF8Decode(Format(UTF8Encode(PageNumberFormat), [i+1, Images.Count]));
      {$eNDIF}
      {$IFNDEF LCLLIB}
      pagnr := Format(PageNumberFormat,[i+1, Images.Count]);
      {$ENDIF}
      topmargin := 0;
      bottommargin := 0;
      hdr := '';
      ftr := '';

      if PageNumberPosition in [pnTopLeft, pnTopCenter, pnTopRight] then
      begin
        hdr := pagnr;
        ftr := '';
      end;

      if PageNumberPosition in [pnBottomLeft, pnBottomCenter, pnBottomRight] then
      begin
        if (i = 0) and (Title <> '') then
        begin
          hdr := Title;
          pdf.HeaderAlignment := gtaCenter;
        end;
        ftr := pagnr;
      end;

      DoGetPageInfo(i, hdr, ftr);

      pdf.Header := hdr;
      pdf.Footer := ftr;

      pdf.NewPage;


      th := Round(pdf.Graphics.CalculateText('gh').Height);

      if pdf.Header <> '' then
      begin
        topmargin := 2 * th;
      end;

      if pdf.Footer <> '' then
      begin
        bottommargin := 2 * th;
      end;

      r := RectF(0, topmargin, pdf.PageWidth, pdf.PageHeight - bottommargin);

      DoBeforePageDraw(i, pdf.Graphics);

      pdf.Graphics.DrawImageFromFile(Images.Strings[i], r);

      DoAfterPageDraw(i, pdf.Graphics);
    end;

    pdf.EndDocument;
  finally
    pdf.Free;
  end;

end;

end.
