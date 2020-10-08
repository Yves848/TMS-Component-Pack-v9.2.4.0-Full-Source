{********************************************************************}
{ TMS TAdvSmoothCalendar Demo                                        }
{                                                                    }
{ written by TMS Software                                            }
{            copyright � 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothCalendarDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvSmoothEdit, AdvSmoothEditButton, AdvSmoothDatePicker,
  AdvSmoothCalendar, AdvStyleIF, DateUtils, ComCtrls, GDIPFill, AdvGDIP,
  ShellAPI, Vcl.Mask;

type
  TForm93 = class(TForm)
    AdvSmoothCalendar1: TAdvSmoothCalendar;
    AdvSmoothDatePicker1: TAdvSmoothDatePicker;
    AdvSmoothDatePicker2: TAdvSmoothDatePicker;
    ComboBox1: TComboBox;
    Label9: TLabel;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox2: TCheckBox;
    AdvSmoothCalendar2: TAdvSmoothCalendar;
    Label3: TLabel;
    Label4: TLabel;
    CheckBox3: TCheckBox;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AdvSmoothCalendar1MonthChanged(Sender: TObject; Month: Integer);
    procedure AdvSmoothCalendar1SelectDate(Sender: TObject;
      Mode: TAdvSmoothCalendarDateMode; Date: TDateTime);
    procedure CheckBox1Click(Sender: TObject);
    procedure AdvSmoothCalendar1SelectMultiDate(Sender: TObject;
      Mode: TAdvSmoothCalendarDateMode; StartDate, EndDate: TDateTime);
    procedure CheckBox2Click(Sender: TObject);
    procedure AdvSmoothCalendar2DateStatus(Sender: TObject; Date: TDateTime;
      var StatusMessage: string; Fill: TGDIPStatus; var OffsetX,
      OffsetY: Integer);
    procedure AdvSmoothCalendar1DateStatus(Sender: TObject; Date: TDateTime;
      var StatusMessage: string; Fill: TGDIPStatus; var OffsetX,
      OffsetY: Integer);
    procedure AdvSmoothCalendar2DateFill(Sender: TObject; AFill: TGDIPFill;
      AFont: TFont; Date: TDateTime; DateKind: TAdvSmoothCalendarDateKind);
    procedure Label3Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CheckSeason(Month: integer);
  end;

var
  Form93: TForm93;

implementation

{$R *.dfm}
{$R UDemo.res}

procedure TForm93.AdvSmoothCalendar1DateStatus(Sender: TObject; Date: TDateTime;
  var StatusMessage: string; Fill: TGDIPStatus; var OffsetX, OffsetY: Integer);
begin
  if Date = int(Now) then
    StatusMessage := 'Today';
end;

procedure TForm93.AdvSmoothCalendar1MonthChanged(Sender: TObject;
  Month: Integer);
begin
  CheckSeason(Month);
end;

procedure TForm93.AdvSmoothCalendar1SelectDate(Sender: TObject;
  Mode: TAdvSmoothCalendarDateMode; Date: TDateTime);
begin
  Label1.Caption := 'Start date: ' + FormatDateTime('dd/mm/yyyy', AdvSmoothCalendar1.StartDate);
  Label2.Caption := 'End date: ' + FormatDateTime('dd/mm/yyyy', AdvSmoothCalendar1.EndDate);
  CheckSeason(MonthOf(Date));
end;

procedure TForm93.AdvSmoothCalendar1SelectMultiDate(Sender: TObject;
  Mode: TAdvSmoothCalendarDateMode; StartDate, EndDate: TDateTime);
begin
  Label1.Caption := 'Start date: ' + FormatDateTime('dd/mm/yyyy', StartDate);
  Label2.Caption := 'End date: ' + FormatDateTime('dd/mm/yyyy', EndDate);
end;

procedure TForm93.AdvSmoothCalendar2DateFill(Sender: TObject; AFill: TGDIPFill;
  AFont: TFont; Date: TDateTime; DateKind: TAdvSmoothCalendarDateKind);
begin
  if Date = int(Now) - 10 then
  begin
    AFill.Opacity := 255;
    AFill.Color := clTeal;
    AFill.OpacityTo := 100;
    AFill.ColorTo := clGreen;
    AFill.ColorMirror := clNone;
    AFill.GradientMirrorType := gtNone;
    AFont.Color := clWhite;
    AFont.Size := 12;
  end;
end;

procedure TForm93.AdvSmoothCalendar2DateStatus(Sender: TObject; Date: TDateTime;
  var StatusMessage: string; Fill: TGDIPStatus; var OffsetX, OffsetY: Integer);
begin
  if Date = int(Now) then
  begin
    Fill.Fill.Color := clgreen;
    Fill.Fill.ColorTo := clgreen;
    StatusMessage := 'Today';
  end;

  if Date = int(Now) - 10 then
  begin
    Fill.Fill.Color := clRed;
    Fill.Fill.ColorTo := clRed;
    Fill.Font.Color := clBlack;
    StatusMessage := 'Meeting';
  end;
end;

procedure TForm93.CheckBox1Click(Sender: TObject);
begin
  AdvSmoothCalendar1.MultiSelect := CheckBox1.Checked;
end;

procedure TForm93.CheckBox2Click(Sender: TObject);
begin
  AdvSmoothcalendar1.Animation := CheckBox2.Checked;
end;

procedure TForm93.CheckBox3Click(Sender: TObject);
begin
  if CheckBox3.Checked then
  begin
    AdvSmoothCalendar1.DateAppearance.WeekendFill.Color := clGray;
  end
  else
  begin
    AdvSmoothCalendar1.DateAppearance.WeekendFill.Color := AdvSmoothCalendar1.DateAppearance.DayOfWeekFill.Color;
  end;
end;

procedure TForm93.CheckSeason(Month: integer);
begin
  AdvSmoothCalendar1.Fill.BackGroundPicture := nil;
    
  case Month of
    1:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'winter');
    2:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'winter');
    3:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'spring');
    4:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'spring');
    5:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'spring');
    6:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'summer');
    7:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'summer');
    8:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'summer');
    9:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'autumn');
    10:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'autumn');
    11:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'autumn');
    12:AdvSmoothCalendar1.Fill.BackGroundPicture.LoadFromResourceName(hinstance, 'winter');
  end;
  AdvSmoothDatePicker1.Calendar.Fill.Assign(AdvSmoothCalendar1.Fill);
end;

procedure TForm93.ComboBox1Change(Sender: TObject);
begin
  AdvSmoothCalendar2.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
  AdvSmoothDatePicker2.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
end;

procedure TForm93.FormCreate(Sender: TObject);
begin
  (* force US names
  ShortDayNames[1] := 'Su';
  ShortDayNames[2] := 'Mo';
  ShortDayNames[3] := 'Tu';
  ShortDayNames[4] := 'We';
  ShortDayNames[5] := 'Th';
  ShortDayNames[6] := 'Fr';
  ShortDayNames[7] := 'Sa';
  LongMonthNames[7] := 'July';
  *)

  AdvSmoothDatePicker1.Calendar.Fill.Assign(AdvSmoothCalendar1.Fill);
  AdvSmoothDatePicker1.Calendar.DateAppearance.Assign(AdvSmoothCalendar1.DateAppearance);
  AdvSmoothDatePicker1.Calendar.Footer.Assign(AdvSmoothCalendar1.Footer);
  AdvSmoothDatePicker1.Calendar.Header.Assign(AdvSmoothCalendar1.Header);

  FillStyleList(ComboBox1.Items);
  ComboBox1.ItemIndex := 1;
  AdvSmoothCalendar2.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[1]));
  AdvSmoothDatePicker2.SetComponentStyle(TTMSStyle(ComboBox1.Items.Objects[1]));

  AdvSmoothCalendar1.Year := YearOf(Now);
  AdvsmoothCalendar2.Year := YearOf(Now);
  AdvSmoothCalendar1.Month := MonthOf(Now);
  AdvsmoothCalendar2.Month := MonthOf(Now);
  AdvSmoothCalendar1.SelectedDate := Now;
  CheckSeason(MonthOf(Now));

  AdvSmoothCalendar1.Footer.Caption := 'Today : ' + FormatDateTime('dd/mm/yyyy', now);    
end;

procedure TForm93.Label3Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
