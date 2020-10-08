program AdvSmoothSlideShowDemo;

uses
  Forms,
  UAdvSmoothSlideShowDemo in 'UAdvSmoothSlideShowDemo.pas' {Form674};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm674, Form674);
  Application.Run;
end.
