program AdvOfficePagerDemo;

uses
  Forms,
  UAdvOfficePagerDemo in 'UAdvOfficePagerDemo.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
