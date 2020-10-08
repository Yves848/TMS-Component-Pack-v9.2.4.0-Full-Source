program AdvSmoothMessageDialogDemo;

uses
  Forms,
  UAdvSmoothMessageDialogDemo in 'UAdvSmoothMessageDialogDemo.pas' {Form8};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
