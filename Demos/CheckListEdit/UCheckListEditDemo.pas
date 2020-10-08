{********************************************************************}
{ TMS TCHECKLISTEDIT DEMO                                            }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UCheckListEditDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, clisted, ShellAPI;

type
  TForm1 = class(TForm)
    CheckListEdit1: TCheckListEdit;
    CheckListEdit2: TCheckListEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckListEdit3: TCheckListEdit;
    Label19: TLabel;
    Label18: TLabel;
    procedure CheckListEdit3TextToCheckListItem(sender: TObject;
      var aItem: String);
    procedure CheckListEdit3CheckListItemToText(sender: TObject;
      var aText: String);
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

procedure TForm1.CheckListEdit3TextToCheckListItem(sender: TObject;
  var aItem: String);
var
 i:integer;
begin
 for i:=1 to CheckListEdit3.Items.Count do
  if pos(aItem,CheckListEdit3.Items[i-1])=1 then
    begin
     aItem:=CheckListEdit3.Items[i-1];
     break;
    end;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.CheckListEdit3CheckListItemToText(sender: TObject;
  var aText: String);
begin
 aText:=copy(aText,1,3);
end;

end.
