program ImageControlsDemo;

uses
  Forms,
  UImageControlsDemo in 'UImageControlsDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
