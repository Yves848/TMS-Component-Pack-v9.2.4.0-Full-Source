program AdvSmoothPanelDemo;

uses
  Forms,
  UAdvSmoothPanelDemo in 'UAdvSmoothPanelDemo.pas' {Form93};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm93, Form93);
  Application.Run;
end.
