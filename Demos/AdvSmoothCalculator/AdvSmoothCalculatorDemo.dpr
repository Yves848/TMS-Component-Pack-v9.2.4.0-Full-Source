program AdvSmoothCalculatorDemo;

uses
  Forms,
  UAdvSmoothCalculatorDemo in 'UAdvSmoothCalculatorDemo.pas' {Form343};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm343, Form343);
  Application.Run;
end.
