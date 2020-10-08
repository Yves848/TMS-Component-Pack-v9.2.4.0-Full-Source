{********************************************************************}
{ TAdvPageControl demo                                               }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2003-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvPageControlLayoutDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, AdvPageControl, ComCtrls, ImgList, StdCtrls, AdvTabSet, ShellAPI,
  System.ImageList;

type
  TForm1 = class(TForm)
    AdvPageControl1: TAdvPageControl;
    AdvTabSheet1: TAdvTabSheet;
    AdvTabSheet2: TAdvTabSheet;
    AdvTabSheet3: TAdvTabSheet;
    AdvPageControl2: TAdvPageControl;
    AdvTabSheet4: TAdvTabSheet;
    AdvTabSheet5: TAdvTabSheet;
    AdvTabSheet6: TAdvTabSheet;
    ImageList1: TImageList;
    AdvPageControl3: TAdvPageControl;
    AdvTabSheet7: TAdvTabSheet;
    AdvTabSheet8: TAdvTabSheet;
    AdvTabSheet9: TAdvTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    labelen: TLabel;
    Label9: TLabel;
    AdvPageControl4: TAdvPageControl;
    AdvTabSheet15: TAdvTabSheet;
    AdvTabSheet16: TAdvTabSheet;
    AdvTabSheet17: TAdvTabSheet;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    AdvTabSet1: TAdvTabSet;
    AdvTabSet2: TAdvTabSet;
    AdvTabSet3: TAdvTabSet;
    AdvTabSet4: TAdvTabSet;
    Label15: TLabel;
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
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
