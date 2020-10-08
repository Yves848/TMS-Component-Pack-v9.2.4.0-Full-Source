{********************************************************************}
{ TMS TRTFLABEL DEMO                                                 }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1998-2019                                   }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit URTFLabelDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, rtflabel, ExtCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    RTFLabel1: TRTFLabel;
    GroupBox2: TGroupBox;
    RTFLabel2: TRTFLabel;
    Timer1: TTimer;
    Label19: TLabel;
    procedure Timer1Timer(Sender: TObject);
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
procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  //  ShowTime;
  RTFLabel2.RichText := 'Date is: ' + DateToStr(Now) + #13'Time is: ' + TimeToStr(Now);

  RTFLabel2.BeginUpdate;

  RTFLabel2.RTF.SelStart := 9;
  RTFLabel2.RTF.SelLength := Length(DateToStr(Now));
  RTFLabel2.RTF.SelAttributes.Color := clBlue;
  RTFLabel2.RTF.SelAttributes.Style := [fsBold];
  RTFLabel2.RTF.SelStart := 19 + Length(DateToStr(Now));
  RTFLabel2.RTF.SelLength := Length(TimeToStr(Now));
  RTFLabel2.RTF.SelAttributes.Color := clGreen;
  RTFLabel2.RTF.SelAttributes.Style := [fsBold];

  RTFLabel2.EndUpdate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

end.
