program PlannerRangeSelectorDemo;

uses
  Forms,
  UPlannerRangeSelectorDemos in 'UPlannerRangeSelectorDemos.pas' {Form92};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm92, Form92);
  Application.Run;
end.
