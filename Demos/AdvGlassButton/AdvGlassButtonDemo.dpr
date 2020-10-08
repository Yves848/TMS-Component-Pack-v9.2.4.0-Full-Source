program AdvGlassButtonDemo;

uses
  Forms,
  UAdvGlassButtonDemo in 'UAdvGlassButtonDemo.pas' {Form2},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
