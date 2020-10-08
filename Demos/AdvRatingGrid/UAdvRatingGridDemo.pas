{********************************************************************}
{ TMS TAdvRatingGrid Demo                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UAdvRatingGridDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvRatingGrid, StdCtrls, pngimage, ShellAPI;

type
  TForm1 = class(TForm)
    AdvRatingGrid1: TAdvRatingGrid;
    AdvRatingGrid2: TAdvRatingGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label16: TLabel;
    procedure Label16Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Label16Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
