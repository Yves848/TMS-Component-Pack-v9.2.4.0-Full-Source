{***************************************************************************}
{ TMS TTreeList Demo                                                        }
{                                                                           }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2000 - 2019                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                              }
{                                                                           }
{***************************************************************************}

unit UTreeListLoadDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, TreeList, StdCtrls, ExtCtrls, System.ImageList, Vcl.ImgList, ShellAPI
  {$IFDEF VER120} , ImgList {$ENDIF}
  {$IFDEF VER130} , ImgList {$ENDIF}
  ;

type
  TForm1 = class(TForm)
    TreeList1: TTreeList;
    ImageList1: TImageList;
    Label19: TLabel;
    Label18: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  TreeList1.LoadFromFile('cars.tl');

  for i := 1 to TreeList1.Items.Count do
  begin
    TreeList1.Items.Item[i - 1].ImageIndex := Random(2);
    TreeList1.Items.Item[i - 1].SelectedIndex := TreeList1.Items.Item[i - 1].ImageIndex;
  end;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
