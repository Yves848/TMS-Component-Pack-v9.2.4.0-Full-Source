program AdvMetroControlsDemo;

uses
  Forms,
  UAdvMetroControlsDemo in 'UAdvMetroControlsDemo.pas' {TMSForm1: TAdvMetroForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTMSForm1, TMSForm1);
  Application.Run;
end.