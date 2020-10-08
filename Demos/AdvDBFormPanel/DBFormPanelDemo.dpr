program DBFormPanelDemo;

uses
  Forms,
  UDBFormPanelDemo in 'UDBFormPanelDemo.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
