{********************************************************************}
{ TAdvPdfLib demo                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvPdfLibPlannerDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Planner, StdCtrls, Generics.Collections, Math, AdvPdfLib, AdvPDFIO, AdvPlannerPdfIO,
  ImgList, ExtCtrls, AdvCustomComponent, System.ImageList;

type
  TForm1 = class(TForm)
    Planner1: TPlanner;
    ImageList1: TImageList;
    Panel1: TPanel;
    AdvPlannerPDFIO1: TAdvPlannerPDFIO;
    Button2: TButton;
    RadioGroup1: TRadioGroup;
    SaveDialog1: TSaveDialog;
    Panel2: TPanel;
    Label1: TLabel;
    Label16: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Label16Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ShellAPI;

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
var
  fn: string;
begin
  if SaveDialog1.Execute then
  begin
    fn := SaveDialog1.FileName;

    if ExtractFileExt(fn) = '' then
      fn := fn + '.PDF';

    AdvPlannerPDFIO1.Save(fn);
    ShellExecute(0,'open',PChar(fn),nil,nil,SW_NORMAL);
  end;
end;

procedure TForm1.Label16Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp',nil,nil,SW_SHOWNORMAL);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
  0: Planner1.Sidebar.Position := spLeft;
  1: Planner1.Sidebar.Position := spTop;
  end;
end;

end.
