program AdvSmoothTouchKeyBoardDemo;

uses
  Forms,
  UAdvSmoothTouchKeyBoardDemo in 'UAdvSmoothTouchKeyBoardDemo.pas' {Form171};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm171, Form171);
  Application.Run;
end.
