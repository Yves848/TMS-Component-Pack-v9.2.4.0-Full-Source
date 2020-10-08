program Demo;

uses
  Vcl.Forms,
  UDemo in 'UDemo.pas' {Form47};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm47, Form47);
  Application.Run;
end.
