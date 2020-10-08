program AdvMemoAutoCorrectionDemo;

uses
  Forms,
  UAdvMemoAutoCorrectionDemo in 'UAdvMemoAutoCorrectionDemo.pas' {FAdvMemoDemo02},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFAdvMemoDemo02, FAdvMemoDemo02);
  Application.Run;
end.
