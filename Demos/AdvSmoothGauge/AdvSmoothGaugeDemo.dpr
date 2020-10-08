program AdvSmoothGaugeDemo;

uses
  Forms,
  UAdvSmoothGaugeDemo in 'UAdvSmoothGaugeDemo.pas' {Form185};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm185, Form185);
  Application.Run;
end.
