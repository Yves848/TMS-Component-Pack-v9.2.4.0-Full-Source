program AdvListBoxDemo;



uses
  Forms,
  UAdvListBoxDemo in 'UAdvListBoxDemo.pas' {Form6},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Glossy');
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
