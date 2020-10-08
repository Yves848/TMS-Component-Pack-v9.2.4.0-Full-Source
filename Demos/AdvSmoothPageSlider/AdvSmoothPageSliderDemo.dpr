program AdvSmoothPageSliderDemo;

uses
  Forms,
  UAdvSmoothPageSliderDemo in 'UAdvSmoothPageSliderDemo.pas' {Form600};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm600, Form600);
  Application.Run;
end.
