program Demo;

uses
  Vcl.Forms,
  UDemo in 'UDemo.pas' {Form56};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm56, Form56);
  Application.Run;
end.
