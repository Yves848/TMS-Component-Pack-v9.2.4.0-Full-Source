program DBTodoListDemo;

uses
  Forms,
  UDBTodoListDemo in 'UDBTodoListDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
