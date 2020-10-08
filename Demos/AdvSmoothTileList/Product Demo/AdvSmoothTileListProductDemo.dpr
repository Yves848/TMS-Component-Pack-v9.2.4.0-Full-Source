program AdvSmoothTileListProductDemo;

uses
  Forms,
  UAdvSmoothTileListProductDemo in 'UAdvSmoothTileListProductDemo.pas' {Form327};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm327, Form327);
  Application.Run;
end.
