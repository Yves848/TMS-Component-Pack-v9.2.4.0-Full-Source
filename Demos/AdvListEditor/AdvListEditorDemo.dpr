program AdvListEditorDemo;

uses
  Forms,
  UAdvListEditorDemo in 'UAdvListEditorDemo.pas' {Form4},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
