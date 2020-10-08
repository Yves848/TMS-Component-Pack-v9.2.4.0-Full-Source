{*************************************************************************}
{ TAdvMemo, TDBAdvMemo demo application                                   }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1998-2019                                        }
{            Email : info@tmssoftware.com                                 }
{            Web : https://www.tmssoftware.com                            }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit UAdvMemoSpellCheckDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TMSSpellCheck, StdCtrls, ExtCtrls, AdvMemo, AdvMemoSpellCheck,
  TMSSpellCheckCorrectLinesForm, ShellAPI;

type
  TForm5 = class(TForm)
    AdvMemo1: TAdvMemo;
    RadioGroup1: TRadioGroup;
    AdvSpellCheck1: TAdvSpellCheck;
    AdvMemoSpellChecker1: TAdvMemoSpellChecker;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label4: TLabel;
    procedure RadioGroup1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

procedure TForm5.CheckBox1Click(Sender: TObject);
begin
  AdvMemoSpellChecker1.ShowDialog := CheckBox1.Checked;
end;

procedure TForm5.Label4Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm5.RadioGroup1Click(Sender: TObject);
begin
  case radiogroup1.ItemIndex of
  0: AdvMemoSpellChecker1.AutoCorrectType := acWordCheck;
  1: AdvMemoSpellChecker1.AutoCorrectType := acWordCorrect;
  2: AdvMemoSpellChecker1.AutoCorrectType := acLineCheck;
  3: AdvMemoSpellChecker1.AutoCorrectType := acLineCorrect;
  end;
end;

end.
