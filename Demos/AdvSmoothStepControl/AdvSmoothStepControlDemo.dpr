program AdvSmoothStepControlDemo;

uses
  Forms,
  UAdvSmoothStepControlDemo in 'UAdvSmoothStepControlDemo.pas' {Form252};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm252, Form252);
  Application.Run;
end.
