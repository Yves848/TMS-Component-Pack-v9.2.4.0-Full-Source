program AdvPanelDemo;

uses
  Forms,
  UAdvPanelDemo in 'UAdvPanelDemo.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
