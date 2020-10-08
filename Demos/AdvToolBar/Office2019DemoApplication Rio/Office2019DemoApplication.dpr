program Office2019DemoApplication;

uses
  Vcl.Forms,
  UOffice2019DemoApplication in 'UOffice2019DemoApplication.pas' {Form6},
  Office2019Frame in 'Office2019Frame.pas' {Office2019Frame1: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
