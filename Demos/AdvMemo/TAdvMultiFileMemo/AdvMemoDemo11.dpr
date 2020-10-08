program AdvMemoDemo11;

uses
  Vcl.Forms,
  UAdvMemoDemo11 in 'UAdvMemoDemo11.pas' {TFAdvMultiFileMemoDemo01};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTFAdvMultiFileMemoDemo01, TFAdvMultiFileMemoDemo01);
  Application.Run;
end.
