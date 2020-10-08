{********************************************************************}
{ TMS TAdvGlassButton Demo                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UAdvGlassButtonDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvGDIP, AdvGlassButton, GDIPicture, StdCtrls, jpeg, ExtCtrls,
  ImgList, Menus, AdvOfficeHint, ShellAPI, System.ImageList;
type
  TForm2 = class(TForm)
    Image1: TImage;
    PopupMenu1: TPopupMenu;
    Open1: TMenuItem;
    Save1: TMenuItem;
    ImageList1: TImageList;
    AdvGlassButton2: TAdvGlassButton;
    AdvGlassButton3: TAdvGlassButton;
    AdvGlassButton4: TAdvGlassButton;
    AdvGlassButton5: TAdvGlassButton;
    AdvGlassButton6: TAdvGlassButton;
    Timer1: TTimer;
    AdvGlassButton7: TAdvGlassButton;
    Label1: TLabel;
    procedure AdvGlassButton5Click(Sender: TObject);
    procedure NextImage(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RepaintButtons(i: Integer);
    procedure Label1Click(Sender: TObject);
    procedure PreviousImage(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;
  i: Integer;

implementation

{$R *.dfm}

procedure TForm2.AdvGlassButton5Click(Sender: TObject);
begin
  if AdvGlassButton5.BackGroundSymbol=TBackGroundSymbol.bsPlay then
  begin
    AdvGlassButton5.BackGroundSymbol:=TBackGroundSymbol.bsPause;
    AdvGlassButton5.BackColor:=clGreen;
    AdvGlassButton5.GlowColor:=clLime;
    Timer1.Enabled:=true;
  end
  else
  begin
    AdvGlassButton5.BackGroundSymbol:=TBackGroundSymbol.bsPlay;
    AdvGlassButton5.BackColor:=clRed;
    AdvGlassButton5.GlowColor:=clRed;
    Timer1.Enabled:=false;
  end;

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  i:=0;
end;

procedure TForm2.Label1Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm2.NextImage(Sender: TObject);
begin
  if( i = 2) then
    i:=0
  else
    Inc(i);
  RepaintButtons(i);
end;

procedure TForm2.PreviousImage(Sender: TObject);
begin
  if( i = 0) then
    i:=2
  else
    Dec(i);
  RepaintButtons(i);
end;

procedure TForm2.RepaintButtons(i: integer);
begin
  case i of
    0: Image1.Picture.LoadFromFile('.\bar.jpg');
    1: Image1.Picture.LoadFromFile('.\wedding.jpg');
    2: Image1.Picture.LoadFromFile('.\elephant.jpg');
  end;
  AdvGlassButton4.Repaint;
  AdvGlassButton2.Repaint;
  AdvGlassButton3.Repaint;
  AdvGlassButton5.Repaint;
  AdvGlassButton6.Repaint;
  AdvGlassButton7.Repaint;
end;

end.
