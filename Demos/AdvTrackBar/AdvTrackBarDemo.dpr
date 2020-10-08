program AdvTrackBarDemo;

uses
  Forms,
  UAdvTrackBarDemo in 'UAdvTrackBarDemo.pas' {Form2},
  UDesignTrackBar in 'UDesignTrackBar.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
