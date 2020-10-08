{********************************************************************}
{ THTMLTreeList DEMO application                                     }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2002 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}


unit UHTMLTreeListDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, HTMLTreeList, StdCtrls, ShellApi, ImgList, System.ImageList;

type
  TForm1 = class(TForm)
    HTMLTreeList1: THTMLTreeList;
    ImageList1: TImageList;
    Label1: TLabel;
    Label19: TLabel;
    Label18: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure HTMLTreeList1AnchorEnter(Sender: TObject; Node: TTreeNode;
      anchor: String);
    procedure HTMLTreeList1AnchorExit(Sender: TObject; Node: TTreeNode;
      anchor: String);
    procedure HTMLTreeList1AnchorClick(Sender: TObject; Node: TTreeNode;
      anchor: String);
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
begin
  htmltreelist1.loadfromfile('cars.tl');
end;

procedure TForm1.HTMLTreeList1AnchorEnter(Sender: TObject; Node: TTreeNode;
  anchor: String);
begin
  Label1.Caption := Anchor;
end;

procedure TForm1.HTMLTreeList1AnchorExit(Sender: TObject; Node: TTreeNode;
  anchor: String);
begin
  Label1.Caption := '';
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.HTMLTreeList1AnchorClick(Sender: TObject; Node: TTreeNode;
  anchor: String);
begin
  ShellExecute(0,'open',pchar(anchor),nil,nil,SW_NORMAL);
end;

end.
