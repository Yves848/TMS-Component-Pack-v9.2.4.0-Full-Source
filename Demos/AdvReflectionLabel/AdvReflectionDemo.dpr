program AdvReflectionDemo;

uses
  Forms,
  UAdvReflectionDemo in 'UAdvReflectionDemo.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  TStyleManager.TrySetStyle('Glossy');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
