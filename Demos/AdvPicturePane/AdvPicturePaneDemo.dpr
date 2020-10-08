program AdvPicturePaneDemo;

uses
  Forms,
  UAdvPicturePane in 'UAdvPicturePane.pas' {Form93},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm93, Form93);
  Application.Run;
end.
