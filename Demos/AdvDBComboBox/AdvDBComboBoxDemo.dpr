program AdvDBComboBoxDemo;

uses
  Forms,
  UAdvDBComboBoxDemo in 'UAdvDBComboBoxDemo.pas' {Form433},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm433, Form433);
  Application.Run;
end.
