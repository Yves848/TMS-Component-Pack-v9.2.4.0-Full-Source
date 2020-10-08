program AdvSmoothMenuDemo;

uses
  Forms,
  UAdvSmoothMenuDemo in 'UAdvSmoothMenuDemo.pas' {Form93};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm93, Form93);
  Application.Run;
end.
