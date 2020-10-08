program AdvOfficeMDITableSetDemo;

uses
  Forms,
  UAdvOfficeMDITableSetDemo in 'UAdvOfficeMDITableSetDemo.pas' {Form1},
  UOfficeMDITablePage in 'UOfficeMDITablePage.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
