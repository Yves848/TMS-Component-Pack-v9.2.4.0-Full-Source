program HTMLFormDemo;

uses
  Forms,
  UHTMLFormDemo in 'UHTMLFormDemo.pas' {Form369};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm369, Form369);
  Application.Run;
end.
