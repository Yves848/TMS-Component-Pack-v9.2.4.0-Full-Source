program AdvFancyLabelDemo;

uses
  Forms,
  UAdvFancyLabelDemo in 'UAdvFancyLabelDemo.pas' {Form92},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm92, Form92);
  Application.Run;
end.
