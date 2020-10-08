{********************************************************************}
{ TAdvPdfLib demo                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvPdfLibSampleDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, PictureContainer, ShellAPI;

type
  TForm130 = class(TForm)
    Button1: TButton;
    PictureContainer1: TPictureContainer;
    Label16: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Label16Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form130: TForm130;

implementation

{$R *.dfm}

uses
  AdvPDFLib, Types, AdvGraphicsTypes,
  AdvPDFCoreLibBase;

procedure TForm130.Button1Click(Sender: TObject);
var
  p: TAdvPDFLib;
  s: string;
  r, rh, tr: TRectF;
  st: TStringList;
  pth: string;
begin
  pth := '';
  p := TAdvPDFLib.Create;
  try
    s := 'Lorem Ipsum is simply <b>dummy</b> text of the printing and typesetting industry. Lorem Ipsum has been the industry''s standard dummy'
      + 'text ever since the 1500s, when an <i><font color="gcOrange">unknown printer took a galley</font></i> of type and scrambled it to make a type specimen book. '
      + 'It has survived not only five <font size="12" color="gcRed">centuries</font>, but also the <sup>leap</sup> <sub>into</sub> <u>electronic</u> typeset'
      + 'ting,<br/><img width="64" src="TMSLogo.ico"/><br/>remaining essentially <s>unchanged</s>. It'
      + ' was popularised in the 1960s with the <font size="8"><a href="https://tmssoftware.com/site/tmsvcluipack.asp">release</a></font> of Letraset sheets containing Lorem Ipsum passages, and more recently with des'
      + 'ktop publishing software like <font bgcolor="gcSteelBlue" color="gcWhite">Aldus PageMaker</font> including versions of Lorem Ipsum.';

    p.PictureContainer := PictureContainer1;
    p.BeginDocument(pth + 'TMS PDF Library.pdf');
    p.Header := 'TMS PDF Library';
    p.Footer := 'Page 1/1';
    p.NewPage;
    rh := p.GetHeaderRect;

    p.Graphics.Font.SizeNoScale := 12;
    p.Graphics.Font.Color := gcSteelblue;
    p.Graphics.Font.Style := [TFontStyle.fsBold, TFontStyle.fsUnderline];
    tr := p.Graphics.DrawText('Drawing primitives', PointF(rh.Left, rh.Bottom + 10));

    p.Graphics.Fill.Color := gcOrange;
    p.Graphics.Stroke.Color := gcGray;
    p.Graphics.DrawRectangle(RectF(rh.Left + 10, tr.Bottom + 10, rh.Left + 50, tr.Bottom + 50));

    p.Graphics.Fill.Color := gcBlanchedalmond;
    p.Graphics.Fill.ColorTo:= gcLightgray;
    p.Graphics.Stroke.Color := gcBlack;
    p.Graphics.Fill.Kind := gfkGradient;
    p.Graphics.DrawEllipse(RectF(rh.Left + 75, tr.Bottom + 10, rh.Left + 150, tr.Bottom + 50));

    p.Graphics.Stroke.Color := gcDarkmagenta;
    p.Graphics.Stroke.Kind := gskSolid;
    p.Graphics.DrawLine(PointF(rh.Left + 180, tr.Bottom + 10), PointF(rh.Left + 250, tr.Bottom + 10));
    p.Graphics.Stroke.Kind := gskDash;
    p.Graphics.DrawLine(PointF(rh.Left + 180, tr.Bottom + 20), PointF(rh.Left + 250, tr.Bottom + 20));
    p.Graphics.Stroke.Kind := gskDot;
    p.Graphics.DrawLine(PointF(rh.Left + 180, tr.Bottom + 30), PointF(rh.Left + 250, tr.Bottom + 30));
    p.Graphics.Stroke.Kind := gskDashDot;
    p.Graphics.DrawLine(PointF(rh.Left + 180, tr.Bottom + 40), PointF(rh.Left + 250, tr.Bottom + 40));
    p.Graphics.Stroke.Kind := gskDashDotDot;
    p.Graphics.DrawLine(PointF(rh.Left + 180, tr.Bottom + 50), PointF(rh.Left + 250, tr.Bottom + 50));

    p.Graphics.Stroke.Color := gcDarkseagreen;
    p.Graphics.Stroke.Width := 3;
    p.Graphics.Stroke.Kind := gskSolid;
    p.Graphics.DrawPathBegin;
    p.Graphics.DrawPathMoveToPoint(PointF(350, tr.Bottom - 10));
    p.Graphics.DrawPathAddCurveToPoint(PointF(310, 130), PointF(445, 50), PointF(398, tr.Bottom + 65));
    p.Graphics.DrawPathEnd(dmPathStroke);

    p.Graphics.Stroke.Width := 0.5;
    p.Graphics.Stroke.Color := gcBlack;
    p.Graphics.Fill.Color := gcNull;
    p.Graphics.Fill.Kind := gfkSolid;
    p.Graphics.DrawLine(PointF(350, tr.Bottom - 10), PointF(310, 130));
    p.Graphics.DrawLine(PointF(445, 50), PointF(398, tr.Bottom + 65));

    p.Graphics.DrawRectangle(RectF(442.5, 47.5, 447.5, 52.5));
    p.Graphics.DrawRectangle(RectF(395.5, tr.Bottom + 62.5, 400.5, tr.Bottom + 67.5));
    p.Graphics.DrawRectangle(RectF(347.5, tr.Bottom - 12.5, 352.5, tr.Bottom - 7.5));
    p.Graphics.DrawRectangle(RectF(307.5, 127.5, 312.5, 132.5));

    p.Graphics.Font.SizeNoScale := 12;
    p.Graphics.Font.Color := gcSteelblue;
    p.Graphics.Font.Style := [TFontStyle.fsBold, TFontStyle.fsUnderline];
    tr := p.Graphics.DrawText('HTML formatted text support', PointF(rh.Left, tr.Bottom + 75));
    p.Graphics.Font.SizeNoScale := 10;
    rh.Top := rh.Top + 40;
    rh.Bottom := rh.Bottom + 40;
    r := RectF(rh.Left + 10, tr.Bottom + 10, rh.Right, 300);
    tr := p.Graphics.DrawHTMLText(s, r);

    p.Graphics.Font.SizeNoScale := 12;
    p.Graphics.Font.Color := gcSteelblue;
    p.Graphics.Font.Style := [TFontStyle.fsBold, TFontStyle.fsUnderline];
    tr := p.Graphics.DrawText('Text flow in multiple columns', PointF(rh.Left, tr.Bottom + 10));

    st := TStringList.Create;
    try
      st.LoadFromFile(pth + 'test2.txt');
      p.Graphics.Font.SizeNoScale := 10;
      p.Graphics.Font.Color := gcBlack;
      p.Graphics.Font.Style := [];
      p.Graphics.DrawText(st.Text, RectF(rh.Left + 10, tr.Bottom + 10, rh.Right, tr.Bottom + 150), 3);
    finally
      st.Free;
    end;

    p.Graphics.Font.SizeNoScale := 12;
    p.Graphics.Font.Color := gcSteelblue;
    p.Graphics.Font.Style := [TFontStyle.fsBold, TFontStyle.fsUnderline];
    tr := p.Graphics.DrawText('Image drawing and quality', PointF(rh.Left, tr.Bottom + 150));

    p.Graphics.DrawImageFromFile(pth + 'Sample.jpg', RectF(rh.Left + 10, tr.Bottom + 10, rh.Left + 110, tr.Bottom + 110));
    p.Graphics.DrawImageFromFile(pth + 'Sample.jpg', RectF(rh.Left + 120, tr.Bottom + 10, rh.Left + 220, tr.Bottom + 110), True, True, itJPG, 0.1);

    p.Graphics.DrawImageFromFile(pth + 'Sample2.jpg', PointF(rh.Left + 240, tr.Bottom + 10));
    p.Graphics.DrawImageFromFile(pth + 'Sample2.jpg', RectF(rh.Left + 420, tr.Bottom + 10, rh.Left + 520, tr.Bottom + 110), True, True, itJPG, 0.1);
//
    p.Graphics.Font.SizeNoScale := 12;
    p.Graphics.Font.Color := gcSteelblue;
    p.Graphics.Font.Style := [TFontStyle.fsBold, TFontStyle.fsUnderline];
    tr := p.Graphics.DrawText('Unicode text support', PointF(rh.Left, tr.Bottom + 150));
    p.Graphics.Font.SizeNoScale := 10;
    p.Graphics.Font.Color := gcBlack;
    p.Graphics.Font.style := [];
    p.Graphics.DrawHTMLText('<b><font color="gcDarkOrange">Belarusian</font></b>: ?????????? ??????', PointF(rh.Left + 10, tr.Bottom + 10));
    p.Graphics.DrawHTMLText('<b><font color="gcDarkOrange">Chinese</font></b>: ??,??', PointF(rh.Left + 10, tr.Bottom + 30));
    p.Graphics.DrawHTMLText('<b><font color="gcDarkOrange">Thai</font></b>: ????????????', PointF(rh.Left + 10, tr.Bottom + 50));

    p.Graphics.DrawHTMLText('<b><font color="gcDarkOrange">Kyrgyz</font></b>: ????? ?????', PointF(rh.Left + 210, tr.Bottom + 10));
    p.Graphics.DrawHTMLText('<b><font color="gcDarkOrange">Japanese</font></b>: ???????', PointF(rh.Left + 210, tr.Bottom + 30));
    p.Graphics.DrawHTMLText('<b><font color="gcDarkOrange">Telugu</font></b>: ??? ??????', PointF(rh.Left + 210, tr.Bottom + 50));

    p.Graphics.DrawHTMLText('<b><font color="gcDarkOrange">Vietnamese</font></b>: Chào th? gi?i', PointF(rh.Left + 410, tr.Bottom + 10));
    p.Graphics.DrawHTMLText('<b><font color="gcDarkOrange">Greek</font></b>: Ge?? s?? ??sµe', PointF(rh.Left + 410, tr.Bottom + 30));
    p.Graphics.DrawHTMLText('<b><font color="gcDarkOrange">Lao</font></b>: ?????????????????', PointF(rh.Left + 410, tr.Bottom + 50));


    p.EndDocument(True);
  finally
    p.Free;
  end;
end;

procedure TForm130.Label16Click(Sender: TObject);
begin
  ShellExecute(handle, 'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
