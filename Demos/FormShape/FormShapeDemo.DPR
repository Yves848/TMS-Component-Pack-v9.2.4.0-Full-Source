program FormShapeDemo;

uses
  Forms,
  UFormShapeDemo in 'UFormShapeDemo.PAS' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
