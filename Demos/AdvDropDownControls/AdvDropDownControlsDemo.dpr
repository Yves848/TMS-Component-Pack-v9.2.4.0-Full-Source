program AdvDropDownControlsDemo;

uses
  Forms,
  UAdvDropDownControlsDemo in 'UAdvDropDownControlsDemo.pas' {Form316},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm316, Form316);
  Application.Run;
end.
