{********************************************************************}
{ TMS TAdvSmoothComboBox Demo                                        }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothComboBoxDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvSmoothComboBox, StdCtrls, AdvSmoothListBox, AdvStyleIF,
  AdvSmoothEdit, AdvSmoothEditButton, AdvSmoothDatePicker, ShellAPI,
  AdvSmoothImageListBoxPicker;

type
  TForm194 = class(TForm)
    ComboBox1: TComboBox;
    Label9: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    AdvSmoothComboBox2: TAdvSmoothComboBox;
    Label1: TLabel;
    AdvSmoothComboBox3: TAdvSmoothComboBox;
    AdvSmoothComboBox1: TAdvSmoothComboBox;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure AdvSmoothComboBox2ItemButtonClick(Sender: TObject;
      itemindex: Integer);
    procedure AdvSmoothComboBox1ItemCheckClick(Sender: TObject;
      itemindex: Integer; checked: Boolean);
    procedure AdvSmoothComboBox3ItemAnchorClick(Sender: TObject; Anchor: string;
      ItemIndex: Integer);
    procedure Label3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form194: TForm194;

implementation

{$R *.dfm}

procedure TForm194.AdvSmoothComboBox1ItemCheckClick(Sender: TObject;
  itemindex: Integer; checked: Boolean);
var
  I: Integer;
  count: integer;
begin
  if checked then
  begin
    AdvSmoothComboBox1.Items[itemindex].GraphicLeftType := gtCommonImage;
    AdvSmoothComboBox1.Items[itemindex].Notes := '<font color="clGreen">Task finished</font>';
    AdvSmoothComboBox1.Items[itemindex].ProgressValue := 100;
  end
  else
  begin
    AdvSmoothComboBox1.Items[itemindex].Notes := '<font color="clRed">Task unfinished</font>';
    AdvSmoothComboBox1.Items[itemindex].GraphicLeftType := gtImage;
    AdvSmoothComboBox1.Items[itemindex].ProgressValue := 0;
  end;

  Count := 0;
  for I := 0 to AdvSmoothComboBox1.Items.Count - 1 do
  begin
    if AdvSmoothComboBox1.Items[I].Checked then
      Inc(Count);
  end;
  AdvSmoothComboBox1.Footer.Caption := 'Finished tasks : ' + '<font size="10" color="clBlack">'+inttostr(count)+'</font>';

end;

procedure TForm194.AdvSmoothComboBox2ItemButtonClick(Sender: TObject;
  itemindex: Integer);
begin
  if AdvSmoothComboBox2.Items[itemindex].Expanded then
    AdvSmoothComboBox2.Items[itemindex].ButtonCaption := '-'
  else
    AdvSmoothComboBox2.Items[itemindex].ButtonCaption := '+';
end;

procedure TForm194.AdvSmoothComboBox3ItemAnchorClick(Sender: TObject;
  Anchor: string; ItemIndex: Integer);
begin
  outputdebugstring(pchar(Anchor));
end;

procedure TForm194.ComboBox1Change(Sender: TObject);
var
  AStyle: TTMSStyle;
begin
  AdvSmoothComboBox1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
end;

procedure TForm194.FormCreate(Sender: TObject);
var
  i: integer;
begin
  ReportMemoryLeaksOnShutdown := true;

  FillStyleList(ComboBox1.Items);
  AdvSmoothComboBox1.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[2]));
  ComboBox1.ItemIndex := 2;

  //TODOList
  for I := 0 to 20 do
  begin
    with AdvSmoothComboBox1.Items.Add do
    begin
      Caption := 'Task ' + inttostr(I + 1);
      GraphicRightType := gtCheckBox;
      Info := 'Task description';
      Notes := '<font color="clred">Task unfinished</font>';
      NotesLocation := plTopCenter;
      GraphicLeftMargin := 15;
      ProgressVisible := true;
    end;
  end;
  AdvSmoothComboBox1.Footer.Caption := 'Finished tasks : ' + '<font size="10" color="clBlack">0</font>';

  with AdvSmoothComboBox2.Items.Add do
  begin
    Caption := 'Mercedes';
    GraphicLeftType := gtSmoothButton;
    ButtonCaption := '-';
    ButtonBevelColor := clBlack;
    ButtonColor := $00FFFAD1;
    GraphicLeftWidth := 25;
    Info := 'Total : 100%';
  end;
  with AdvSmoothComboBox2.Items.Add do
  begin
    Level := 1;
    Caption := 'Mercedes SLK Roadster';
    Indent := 30;
    ProgressValue := 10;
  end;
  with AdvSmoothComboBox2.Items.Add do
  begin
    Level := 1;
    Caption := 'Mercedes SLR Coupé';
    Indent := 30;
    ProgressValue := 70;
  end;
  with AdvSmoothComboBox2.Items.Add do
  begin
    Level := 1;
    Caption := 'Mercedes GLK 4x4';
    Indent := 30;
    ProgressValue := 20;
  end;

  with AdvSmoothComboBox2.Items.Add do
  begin
    Caption := 'BMW';
    GraphicLeftType := gtSmoothButton;
    ButtonCaption := '-';
    ButtonBevelColor := clBlack;
    ButtonColor := $00FFFAD1;
    GraphicLeftWidth := 25;
    Info := 'Total : 80%';
  end;
  with AdvSmoothComboBox2.Items.Add do
  begin
    Level := 1;
    Caption := 'BMW M3';
    Indent := 30;
    ProgressValue := 40;
  end;
  with AdvSmoothComboBox2.Items.Add do
  begin
    Level := 1;
    Caption := 'BMW Z4';
    Indent := 30;
    ProgressValue := 15;
  end;
  with AdvSmoothComboBox2.Items.Add do
  begin
    Level := 1;
    Caption := 'BMW X5';
    Indent := 30;
    ProgressValue := 25;    
  end;

  with AdvSmoothComboBox2.Items.Add do
  begin
    Caption := 'Land Rover';
    GraphicLeftType := gtSmoothButton;
    ButtonCaption := '-';
    ButtonBevelColor := clBlack;
    ButtonColor := $00FFFAD1;
    GraphicLeftWidth := 25;
    Info := 'Total : 90%';
  end;
  with AdvSmoothComboBox2.Items.Add do
  begin
    Level := 1;
    Caption := 'Land Rover Defender 90';
    Indent := 30;
    ProgressValue := 80;
  end;
  with AdvSmoothComboBox2.Items.Add do
  begin
    Level := 1;
    Caption := 'Land Rover Series III';
    Indent := 30;
    ProgressValue := 5;
  end;
  with AdvSmoothComboBox2.Items.Add do
  begin
    Level := 1;
    Caption := 'Range Rover Sport V8';
    Indent := 30;
    ProgressValue := 5;    
  end;

  for I := 0 to AdvSmoothComboBox2.Items.Count - 1 do
  begin
    with AdvSmoothComboBox2.Items[I] do
    begin
      if Level > 0 then
      begin
        ProgressVisible := true;
        ProgressPosition := plCenterRight;
        ProgressWidth := 50;
      end;
    end;
  end;

  AdvSmoothComboBox3.DropDownWidth := AdvSmoothComboBox3.Width;

end;

procedure TForm194.Label3Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
