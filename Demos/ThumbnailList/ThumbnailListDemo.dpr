program ThumbnailListDemo;

uses
  Forms,
  UThumbnailListDemo in 'UThumbnailListDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
