program AdvGaugeDemo;

uses
  Forms,
  UAdvGaugeDemo in 'UAdvGaugeDemo.pas' {Form93},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm93, Form93);
  Application.Run;
end.
