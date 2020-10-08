program AdvTreeViewDemo;

uses
  Vcl.Forms,
  UAdvTreeViewDemo in 'UAdvTreeViewDemo.pas' {Form98};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm98, Form98);
  Application.Run;
end.
