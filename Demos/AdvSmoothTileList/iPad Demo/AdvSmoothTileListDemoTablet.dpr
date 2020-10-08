program AdvSmoothTileListDemoTablet;

uses
  Forms,
  UAdvSmoothTileListDemoTablet in 'UAdvSmoothTileListDemoTablet.pas' {Form330};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm330, Form330);
  Application.Run;
end.
