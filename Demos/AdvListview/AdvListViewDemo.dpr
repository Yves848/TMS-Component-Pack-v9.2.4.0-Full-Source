program AdvListViewDemo;

uses
  Forms,
  UAdvListViewDemo in 'UAdvListViewDemo.pas' {Form1},
  uAdvListViewprev in 'uAdvListViewprev.pas' {Form2},
  Vcl.Themes,
  Vcl.Styles;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
