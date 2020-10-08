program FormStylerDemo;

uses
  Forms,
  UFormStylerDemo in 'UFormStylerDemo.pas' {Form225};

{$R *.res}

begin
  Application.Initialize;
  
  Application.CreateForm(TForm225, Form225);
  Application.Run;
end.
