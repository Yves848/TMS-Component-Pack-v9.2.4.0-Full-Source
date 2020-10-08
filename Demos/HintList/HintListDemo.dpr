program HintListDemo;

uses
  Forms,
  UHintListDemo in 'UHintListDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
