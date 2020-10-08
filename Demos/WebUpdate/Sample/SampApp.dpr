program SampApp;

uses
  Forms,
  UWebUpdateSampleDemo in 'UWebUpdateSampleDemo.pas' {Form1},
  uproxy in 'uproxy.pas' {proxy},
  uselcomp in 'uselcomp.pas' {selcomp},
  wucrc32 in '..\..\..\Comp\wucrc32.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(Tproxy, proxy);
  Application.CreateForm(Tselcomp, selcomp);
  Application.Run;
end.
