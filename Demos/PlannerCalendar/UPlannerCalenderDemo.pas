{********************************************************************}
{ PlannerCalendar DEMO application                                   }
{   TPlannerCalendar                                                 }
{   TPlannerDatePicker                                               }
{   TPlannerCalendarGroup                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2003-2004                                   }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UPlannerCalenderDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, PlannerCal, AdvPageControl, ComCtrls,
  AdvEdit, AdvEdBtn, PlannerDatePicker;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    PlannerCalendar2: TPlannerCalendar;
    PlannerCalendarGroup1: TPlannerCalendarGroup;
    Label19: TLabel;
    Label18: TLabel;
    PlannerDatePicker1: TPlannerDatePicker;
    PlannerCalendar1: TPlannerCalendar;
    procedure PlannerCalendar1DaySelect(Sender: TObject;
      SelDate: TDateTime);
    procedure FormCreate(Sender: TObject);
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  ShellAPI;

{$R *.dfm}

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.PlannerCalendar1DaySelect(Sender: TObject;
  SelDate: TDateTime);
begin
  Edit1.Text := DateTimeToStr(SelDate);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PlannerCalendar1.Date := Date;
  PlannerCalendar2.Date := Date;
  PlannerCalendarGroup1.Date := Date;
  PlannerCalendarGroup1.Date := Date;

  with PlannerCalendar1.Events.Add do
  begin
    Date := Now;
    Hint := 'Hello world';
    FontColor := ClBlue;
    Color := ClYellow;
    Shape := evsTriangle;
  end;

  with PlannerCalendar1.Events.Add do
  begin
    Date := Now - 10;
    Hint := 'Event 2';
    FontColor := ClWhite;
    Color := ClRed;
    Shape := evsCircle;
  end;

  with PlannerCalendar1.Events.Add do
  begin
    Date := Now + 13;
    Hint := 'Event 3';
    FontColor := ClWhite;
    Color := ClLime;
    Shape := evsRectangle;
  end;
    with PlannerCalendar1.Events.Add do
  begin
    Date := Now + 36;
    Hint := 'Event 3';
    FontColor := ClWhite;
    Color := ClLime;
    Shape := evsRectangle;
  end;

  with plannercalendarGroup1.Events.Add do
  begin
    Date := Now - 20;
    Hint := 'Send Invitations';
    FontColor := ClWhite;
    Color := ClGreen;
    Shape := evsRectangle;
  end;
  with plannercalendarGroup1.Events.Add do
  begin
    Date := Now + 24;
    Hint := 'Release';
    FontColor := ClBlack;
    Color := ClYellow;
    Shape := evsTriangle;
  end;

  with plannercalendarGroup1.Events.Add do
  begin
    Date := now + 38;
    Hint := 'New Meeting';
    FontColor := ClBlack;
    Color := ClLime;
    Shape := evsCircle;
  end;

  with plannercalendarGroup1.Events.Add do
  begin
    Date := Now - 3;
    Hint := 'Holiday';
    FontColor := ClBlack;
    Shape := evsCircle;
  end;
end;

end.
