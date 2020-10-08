program asg83;

uses
  Vcl.Forms,
  Uasg83 in 'Uasg83.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
