{********************************************************************}
{ TEDITLIST DEMO                                                     }
{                                                                    }
{ written by TMS Software                                            }
{            copyright � 1998-2012                                   }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UEditListBoxDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, editlist, ExtCtrls;

type
  TForm1 = class(TForm)
    EditListBox1: TEditListBox;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RadioGroup1: TRadioGroup;
    Label19: TLabel;
    Label18: TLabel;
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure EditListBox1EditStart(Sender: TObject; itemindex: Integer;
      var allow: Boolean);
    procedure Label19Click(Sender: TObject);
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

{$R *.DFM}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
 editlistbox1.AllowEdit:=checkbox1.checked;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
 case radiogroup1.itemindex of
 0:editlistbox1.scrollstyle:=ssNormal;
 1:editlistbox1.scrollstyle:=ssFlat;
 2:editlistbox1.scrollstyle:=ssEncarta;
 end;
end;

procedure TForm1.EditListBox1EditStart(Sender: TObject; itemindex: Integer;
  var allow: Boolean);
begin
 showmessage('here');
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
