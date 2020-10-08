program AdvSmoothPopupDemo;

uses
  Forms,
  UAdvSmoothPopupDemo in 'UAdvSmoothPopupDemo.pas' {Form205};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm205, Form205);
  Application.Run;
end.
