program AdvTaskDialogDemo;

uses
  Forms,
  UAdvTaskDialogDemo in 'UAdvTaskDialogDemo.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
