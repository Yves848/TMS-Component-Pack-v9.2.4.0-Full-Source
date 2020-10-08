program AdvPolyListOverviewDemo;

uses
  Forms,
  UAdvPolyListOverviewDemo in 'UAdvPolyListOverviewDemo.pas' {Form665};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm665, Form665);
  Application.Run;
end.
