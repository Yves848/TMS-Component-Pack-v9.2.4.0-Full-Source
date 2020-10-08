unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, TaskDialog, Vcl.Buttons;

type
  TForm1 = class(TForm)
    AdvInputTaskDialog1: TAdvInputTaskDialog;
    Button1: TButton;
    AdvInputTaskDialog2: TAdvInputTaskDialog;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure AdvInputTaskDialog1ValidateInputText(Sender: TObject;
      var NewValue: string; const Data, ModalResult: Integer;
      var IsValid: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure AdvInputTaskDialog2ValidateInputText(Sender: TObject;
      var NewValue: string; const Data, ModalResult: Integer;
      var IsValid: Boolean);
    procedure AdvInputTaskDialog2DialogClose(Sender: TObject;
      var CanClose: Boolean);
    procedure AdvInputTaskDialog1DialogClose(Sender: TObject;
      var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  CommCtrl,System.RegularExpressions;

procedure TForm1.AdvInputTaskDialog1DialogClose(Sender: TObject;
  var CanClose: Boolean);
begin
  Edit1.Text := AdvInputTaskDialog1.InputText;
end;

procedure TForm1.AdvInputTaskDialog1ValidateInputText(Sender: TObject;
  var NewValue: string; const Data, ModalResult: Integer; var IsValid: Boolean);
begin
  isValid := TRegEx.IsMatch(NewValue, '^([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5})$');

  if not isValid then
  begin
    AdvInputTaskDialog1.InvalidEntryTitle := 'Input error';
    AdvInputTaskDialog1.InvalidEntryText := 'Value entered is not a valid email address';
    AdvInputTaskDialog1.InvalidEntryIcon := tieError;
  end;
end;

procedure TForm1.AdvInputTaskDialog2DialogClose(Sender: TObject;
  var CanClose: Boolean);
begin
  Edit2.Text := AdvInputTaskDialog2.InputText;
end;

procedure TForm1.AdvInputTaskDialog2ValidateInputText(Sender: TObject;
  var NewValue: string; const Data, ModalResult: Integer; var IsValid: Boolean);
begin
  isValid := TRegEx.IsMatch(NewValue, '(?=^.{8,}$)(?=.*\d)(?=.*[!@#$%^&*]+)(?![.\n])(?=.*[A-Z])(?=.*[a-z]).*$');

  if not isValid then
  begin
    AdvInputTaskDialog2.InvalidEntryTitle := 'Invalid password';
    AdvInputTaskDialog2.InvalidEntryText := 'Please enter a password meeting the security requirements';
    AdvInputTaskDialog2.InvalidEntryIcon := tieError;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  AdvInputTaskDialog1.Title := 'Enter email address';
  AdvInputTaskDialog1.Content := 'Email:';
  AdvInputTaskDialog1.Icon := tiShield;
  AdvInputTaskDialog1.ExpandedDefault := true;
  AdvInputTaskDialog1.ExpandedText := 'Your email address will be used to login';
  AdvInputTaskDialog1.Execute;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  AdvInputTaskDialog2.Title := 'Enter a new password';
  AdvInputTaskDialog2.Content := 'Password:';
  AdvInputTaskDialog2.Icon := tiQuestion;
  AdvInputTaskDialog2.InputType := itPassword;
  AdvInputTaskDialog2.ExpandedDefault := true;
  AdvInputTaskDialog2.ExpandedText := 'The password must'#13
    + ' - have a lengh greater than or equal to 8'#13
    + '- contain one or more uppercase characters'#13
    + '- contain one or more lowercase characters'#13
    + '- contain one or more numeric values'#13
    + '- contain one or more special characters';

  AdvInputTaskDialog2.Execute;
end;

end.
