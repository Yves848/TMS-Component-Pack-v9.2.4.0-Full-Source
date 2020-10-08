unit UDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Planner,
  AdvCustomComponent, AdvPDFIO, AdvPlannerPDFIO, PictureContainer, Vcl.ExtCtrls;

type
  TForm47 = class(TForm)
    Planner1: TPlanner;
    Button1: TButton;
    AdvPlannerPDFIO1: TAdvPlannerPDFIO;
    PictureContainer1: TPictureContainer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form47: TForm47;

implementation

{$R *.dfm}

uses
  AdvPDFLib;

procedure TForm47.Button1Click(Sender: TObject);
begin
  AdvPlannerPDFIO1.Save('PlannerExport.pdf', [Planner1]);
end;

procedure TForm47.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  AdvPlannerPDFIO1.Options.PageOrientation := poLandScape;
  AdvPlannerPDFIO1.Options.OpenInPDFReader := True;

  Planner1.Sidebar.Width := 50;
  Planner1.Positions := 7;
  with Planner1.Items.Add do
  begin
    ItemBegin := 8;
    ItemEnd := 40;
    CaptionText := 'Meeting with John';
    Text.Add('<u>Necessities</u><br><ul><li>Notebook<li>Digital lineout<li>Model artwork</ul>');
    CaptionType := ctText;
    CaptionAlign := taCenter;
    Color := clWebLemonChiffon;
    ColorTo := clNone;
  end;

  with Planner1.Items.Add do
  begin
    ItemBegin := 10;
    ItemEnd := 20;
    ItemPos := 1;
    CaptionType := ctTimeText;
    CaptionText := 'Meeting';
    Text.Add('<img src="1"></img>Meeting with sponsors for 2017')
  end;

  with Planner1.Items.Add do
  begin
    ItemBegin := 5;
    ItemEnd := 15;
    ItemPos := 2;
    CaptionText := 'Audi - Mercedes fusion';
    CaptionType := ctText;
    Text.Add('<img src="2"></img>Meeting with Bruno Fierens for approval<br><a href="http://www.tmssoftware.com">http://www.tmssoftware.com</a>');
    Color := clWebGreenYellow;
    ColorTo := clNone;
  end;

  with Planner1.Items.Add do
  begin
    ItemBegin := 5;
    ItemEnd := 25;
    ItemPos := 3;
    CaptionType := ctTime;
    Text.Add('<b><i><u>Lorem Ipsum</u></i></b> is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry''s <a href="http://www.tmssotware.com">standard dummy text</a> ever '+
      'since the 1500s, when an <font color="clRed">unknown printer</font> took a galley of type and scrambled it to make a type specimen book.');
  end;

  with Planner1.Items.Add do
  begin
    ItemBegin := 10;
    ItemEnd := 30;
    ItemPos := 4;
    Text.Add('Test drive of the new BMW i8<BR/><img width="100" src="BMW"></img>');
  end;

  with Planner1.Items.Add do
  begin
    ItemBegin := 10;
    ItemEnd := 16;
    ItemPos := 6;
    CaptionType := ctText;
    CaptionText := 'Audi Conditions';
    Text.Add('<ul><li>Update iOS application<li>Change Audi packs<li>Change online terms</ul>');
  end;

  Planner1.Mode.PlannerType := plDay;
  for I := 0 to Planner1.Positions - 1 do
    Planner1.Header.Captions[I + 1] := 'Position ' + IntToStr(I);

  Planner1.Header.Alignment := taCenter;
end;

end.
