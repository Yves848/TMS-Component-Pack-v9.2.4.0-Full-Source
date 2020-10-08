program AdvMemoAutoCompleteListDemo;

uses
  Forms,
  UAdvMemoAutoCompleteListDemo in 'UAdvMemoAutoCompleteListDemo.pas' {FAdvMemoDemo05};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFAdvMemoDemo05, FAdvMemoDemo05);
  Application.Run;
end.
