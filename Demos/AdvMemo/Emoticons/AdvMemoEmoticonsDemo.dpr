program AdvMemoEmoticonsDemo;

uses
  Forms,
  UAdvMemoEmoticonsDemo in 'UAdvMemoEmoticonsDemo.pas' {FAdvMemoDemo06};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFAdvMemoDemo06, FAdvMemoDemo06);
  Application.Run;
end.
