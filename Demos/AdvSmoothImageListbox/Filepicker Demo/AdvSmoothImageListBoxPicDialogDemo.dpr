program AdvSmoothImageListBoxPicDialogDemo;

uses
  Forms,
  UAdvSmoothImageListBoxPicDialogDemo in 'UAdvSmoothImageListBoxPicDialogDemo.pas' {Form132},
  UPickerDialog in 'UPickerDialog.pas' {PickerDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm132, Form132);
  Application.Run;
end.
