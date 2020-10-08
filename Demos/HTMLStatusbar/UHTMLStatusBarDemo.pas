{********************************************************************}
{ TMS THTMLStatusBar Demo                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UHTMLStatusBarDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  HTMLStatusBar, StdCtrls, ShellAPI, Buttons, ImgList, ExtCtrls,
  System.ImageList
  ;

type
  TForm1 = class(TForm)
    HTMLStatusBar1: THTMLStatusBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    SpeedButton1: TSpeedButton;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Timer1: TTimer;
    procedure Label4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HTMLStatusBar1PanelClick(Sender: TObject;
      PanelIndex: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Label4Click(Sender: TObject);
begin
  ShellExecute(self.handle,'open',pchar(label4.caption),nil,nil,SW_NORMAL);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  Showmessage('Sample control in the statusbar');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  htmlstatusbar1.Panels[6].ImageIndexes[0] := 0;
  htmlstatusbar1.Panels[6].ImageIndexes[1] := 1;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  HTMLStatusBar1.Panels[5].Progress.Position := HTMLStatusBar1.Panels[5].Progress.Position + 2;
  if   HTMLStatusBar1.Panels[5].Progress.Position > 100 then
    HTMLStatusBar1.Panels[5].Progress.Position := 0;
end;

procedure TForm1.HTMLStatusBar1PanelClick(Sender: TObject;
  PanelIndex: Integer);
begin
  if PanelIndex = 7 then
    HTMLStatusBar1.Panels[7].Animated := not    HTMLStatusBar1.Panels[7].Animated;

end;

end.
