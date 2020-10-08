program AdvMsgBoxExplorerDemo;

uses
  Forms,
  UAdvMsgBoxExplorerDemo in 'UAdvMsgBoxExplorerDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
