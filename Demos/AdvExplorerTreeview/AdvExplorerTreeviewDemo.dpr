program AdvExplorerTreeviewDemo;

uses
  Forms,
  UAdvExplorerTreeviewDemo in 'UAdvExplorerTreeviewDemo.pas' {Form146},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm146, Form146);
  Application.Run;
end.
