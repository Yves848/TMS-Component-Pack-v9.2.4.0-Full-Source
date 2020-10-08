{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2016 - 2019                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvPlannerPDFIO;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvPDFLib, Planner, AdvPDFIO, Graphics,
  AdvTypes, Types, PictureContainer, AdvGraphicsTypes;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 ; Fixed : Issue with retrieving item text via OnItemText event

resourcestring
  sTMSPlannerPDFIOPlannerNotAssigned = 'Planner Not Assigned';

type
  TAdvPlannerPDFIO = class;

  TAdvPlannerPDFIOOptions = class(TAdvPDFIOOptions)
  private
    FFitToPage: Boolean;
    FRepeatHeader: Boolean;
    FRepeatSideBar: Boolean;
  protected
    property FitToPage: Boolean read FFitToPage write FFitToPage default True;
    property RepeatHeader: Boolean read FRepeatHeader write FRepeatHeader default True;
    property RepeatSideBar: Boolean read FRepeatSideBar write FRepeatSideBar default True;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
  end;

  TAdvPlannerPDFIORowIsPageBreakEvent = procedure(Sender: TObject; ARow: Integer; var IsPageBreak: Boolean) of object;

  TAdvPlannerPDFIOCellRange = record
    StartCol, StartRow, EndCol, EndRow: Integer;
  end;

  TAdvCustomPlannerPDFIO = class(TAdvCustomPDFIO)
  private
    FOnRowIsPageBreak: TAdvPlannerPDFIORowIsPageBreakEvent;
    function GetOptions: TAdvPlannerPDFIOOptions;
    procedure SetOptions(const Value: TAdvPlannerPDFIOOptions);
    function GetPlanner: TCustomPlanner;
    procedure SetPlanner(const Value: TCustomPlanner);
  protected
    function GetVersion: String; override;
    function CreateOptions: TAdvPDFIOOptions; override;
    procedure DrawCell(APDFIO: TAdvCustomPlannerPDFIO; APDFLib: TAdvPDFLib; APlanner: TCustomPlanner;
      ACol, ARow, AColOff, ARowOff: Integer; AStart: Boolean; ARect: TRectF; ADrawText, ADrawBackground: Boolean; AScale: Single);
    procedure DoPDFExport(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject); override;
    procedure DoPDFPlannerExport(const APDFLib: TAdvPDFLib; const APlanner: TCustomPlanner; const ASelection: TAdvPlannerPDFIOCellRange); virtual;
    procedure DoRowIsPageBreak(ARow: Integer; var IsPageBreak: Boolean); virtual;
    property Planner: TCustomPlanner read GetPlanner write SetPlanner;
    property Version: String read GetVersion;
    property Options: TAdvPlannerPDFIOOptions read GetOptions write SetOptions;
    property OnRowIsPageBreak: TAdvPlannerPDFIORowIsPageBreakEvent read FOnRowIsPageBreak write FOnRowIsPageBreak;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvPlannerPDFIO = class(TAdvCustomPlannerPDFIO)
  published
    property Planner;
    property Version;
    property Options;
    property Information;
    property OnGetHeader;
    property OnGetFooter;
    property OnBeforeDrawHeader;
    property OnAfterDrawHeader;
    property OnBeforeDrawFooter;
    property OnAfterDrawFooter;
    property OnBeforeDrawContent;
    property OnAfterDrawContent;
    property OnRowIsPageBreak;
  end;

implementation

uses
  SysUtils, ImgList, AdvUtils, Forms, Math, AdvGraphics,
  AdvPDFCoreLibBase, PlanUtil
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  ;

type
  TPlannerItemsOpen = class(TPlannerItems);
  TPlannerItemOpen = class(TPlannerItem);
  TCustomPlannerOpen = class(TCustomPlanner);

function CellRange(AStartCol, AStartRow, AEndCol,
  AEndRow: Integer): TAdvPlannerPDFIOCellRange;
begin
  Result.StartCol := AStartCol;
  Result.StartRow := AStartRow;
  Result.EndCol := AEndCol;
  Result.EndRow := AEndRow;
end;

{ TAdvCustomPlannerPDFIO }

function TAdvCustomPlannerPDFIO.GetPlanner: TCustomPlanner;
begin
  Result := TCustomPlanner(inherited ExportObject);
end;

function TAdvCustomPlannerPDFIO.GetOptions: TAdvPlannerPDFIOOptions;
begin
  Result := TAdvPlannerPDFIOOptions(inherited Options);
end;

function TAdvCustomPlannerPDFIO.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomPlannerPDFIO.DrawCell(APDFIO: TAdvCustomPlannerPDFIO; APDFLib: TAdvPDFLib; APlanner: TCustomPlanner;
  ACol, ARow, AColOff, ARowOff: Integer; AStart: Boolean; ARect: TRectF; ADrawText, ADrawBackground: Boolean; AScale: Single);

procedure DrawSideRow(ARect: TRectF; AColumn, APos: Integer; Occupied: Boolean; DefColWidth: Integer);
var
  Line1, Line2, Line3, DT: string;
  HorizontalAlign: Integer;
  OldSize: Single;
  OnTheOur: Boolean;
  MinorLineWidth: Single;
  MajorLineWidth: Single;
  HS: Boolean;
  DNum, delta: Integer;
  DRect, HRect, PRect: TRectF;
  dday: Integer;
  p: double;
  t1, t2: TDateTime;
  Mins: Integer;
  OCol: Integer;
  colw: Single;
  tr: TRectF;

  procedure DrawTimeIndicator(ARect: TRectF; po: double);
  var
    offset: Single;
    pic: TPicture;
  begin
    offset := Max(0, po - 2);

    if (APlanner.Sidebar.TimeIndicatorType in [tiLine, tiLineGlyph]) then
    begin
      APDFLib.Graphics.Stroke.Color := APlanner.Sidebar.TimeIndicatorColor;
      APDFLib.Graphics.Stroke.Width := 3 * AScale;
      APDFLib.Graphics.DrawLine(PointF(ARect.Left + offset, ARect.Top), PointF(ARect.Left + offset, ARect.Bottom));
      APDFLib.Graphics.Stroke.Width := 1 * AScale;
    end;

    if (APlanner.Sidebar.TimeIndicatorType in [tiLineGlyph, tiGlyph]) and not APlanner.Sidebar.TimeIndicatorGlyph.Empty then
    begin
      APlanner.Sidebar.TimeIndicatorGlyph.Transparent := true;
      pic := TPicture.Create;
      pic.Assign(APlanner.Sidebar.TimeIndicatorGlyph);
      try
        APDFLib.Graphics.DrawImage(pic, PointF(ARect.Left + offset - (APlanner.Sidebar.TimeIndicatorGlyph.Width / 2), ARect.Top));
      finally
        pic.Free;
      end;
    end;
  end;

begin
  APlanner.GetSideBarText(AColumn, APos, Line1, Line2, Line3, HS);

  MinorLineWidth := APDFLib.Graphics.DrawText(Line2, PointF(0, 0), True).Height;

  PRect := ARect;
  OCol := AColumn;

  HorizontalAlign := AlignToFlag(APlanner.Sidebar.Alignment);

  if (APlanner.Display.DisplayText > 0) then
  begin
    delta := (APlanner.Display.DisplayStart + AColumn) mod APlanner.Display.DisplayText;

    if (delta <> 0) then
    begin
      AColumn := AColumn - delta;
      ARect.Left := ARect.Left - delta * (ARect.Right - ARect.Left + 2);
      APlanner.GetSideBarText(AColumn, APos, Line1, Line2, Line3, HS);
    end
    else
      Line2 := '';

    ARect.Right := ARect.Left + (APlanner.GridControl.DefaultColWidth);
  end
  else if (APlanner.Mode.PlannerType = plTimeLine) then
  begin
    if Line2 <> '' then
      Line1 := Line1 + FormatSettings.TimeSeparator + Line2;
  end;

  if APlanner.Sidebar.AMPMPos = apNone then
    Line3 := '';

  if Occupied then
    APDFLib.Graphics.Font.Color := APlanner.Sidebar.OccupiedFontColor;

  if (APlanner.Mode.PlannerType = plHalfDayPeriod) then
  begin
    DRect := ARect;

    if not Odd(AColumn) then
      ARect.Right := ARect.Right + DefColWidth
    else
      ARect.Left := ARect.Left - DefColWidth;

    APDFLib.Graphics.Stroke.Color := APlanner.Sidebar.SeparatorLineColor;
    APDFLib.Graphics.DrawLine(PointF(ARect.Right + 3, ARect.Top), PointF(ARect.Right + 3, ARect.Bottom));

    if (Line3 <> '') and (not APlanner.Sidebar.RotateOnTop) then
      ARect.Bottom := ARect.Bottom - APDFLib.Graphics.DrawText('gh', PointF(0, 0), True).Height;

    APDFLib.Graphics.Alignment := gtaCenter;
    tr := APDFLib.Graphics.DrawText(Line3, DRect, True);
    tr.Width := tr.Width + 1 * AScale;
    tr.Height := tr.Height + 1 * AScale;
    APDFLib.Graphics.DrawText(Line3, RectF(DRect.Left, DRect.Bottom - tr.Height, DRect.Right, DRect.Bottom));
    Line3 := '';
  end;

  if (APlanner.Mode.PlannerType = plTimeLine) then
  begin
    dday := (MININDAY div APlanner.Display.DisplayUnit) - (APlanner.Mode.TimeLineNVUBegin + APlanner.Mode.TimeLineNVUEnd);
    if dday = 0 then
    begin
      delta := 0;
      DNum := 1;
    end
    else
    begin
      delta := AColumn mod dday;
      DNum := AColumn div dday;
    end;

    if APlanner.Sidebar.DateTimeFormat <> '' then
      DT := FormatDateTime(APlanner.Sidebar.DateTimeFormat, APlanner.Mode.TimeLineStart + DNum)
    else
      DT := DateToStr(APlanner.Mode.TimeLineStart + DNum);

    if (delta = dday - 1) then
    begin
      DRect := ARect;

      colw := (ARect.Right - ARect.Left);

      DRect.Left := Max(0, ARect.Left - delta * colw);
      DRect.Right := DRect.Left + (dday * colw);

      tr := APDFLib.Graphics.DrawText(DT, DRect, True);
      tr.Width := tr.Width + 1 * AScale;
      tr.Height := tr.Height + 1 * AScale;

      case HorizontalAlign of
        0: APDFLib.Graphics.DrawText(DT, RectF(DRect.Left, DRect.Top, DRect.Left + tr.Width, DRect.Top + tr.Height));
        2: APDFLib.Graphics.DrawText(DT, RectF(DRect.Right - tr.Width, DRect.Top, DRect.Right, DRect.Top + tr.Height));
        1: APDFLib.Graphics.DrawText(DT, RectF(DRect.Left + (DRect.Width - tr.Width) / 2, DRect.Top, DRect.Left + (DRect.Width - tr.Width) / 2 + tr.Width, DRect.Top + tr.Height));
      end;

      if (APlanner.Sidebar.TimeIndicator) then
      begin
        APlanner.CellToAbsTime(OCol, t1, t2);
        if (Now >= t1) and (Now <= t2) then
        begin
          p := APlanner.GridControl.ColWidths[OCol] * ((Now - t1) / (t2 - t1));

          DrawTimeIndicator(PRect, p);
        end;
      end;
    end;

    if (delta = 0) then
    begin
      DRect := ARect;
      APDFLib.Graphics.Stroke.Color := APlanner.GridLineColor;
      APDFLib.Graphics.DrawLine(PointF(DRect.Left - 1, ARect.Top), PointF(DRect.Left - 1, ARect.Bottom));
    end;

    ARect.Top := ARect.Top + (ARect.Bottom - ARect.Top) / 2;

    if APlanner.Display.DisplayText >= 0 then
    begin
      Line2 := Line1;
      ARect.Right := ARect.Right + APlanner.Display.DisplayText * APlanner.GridControl.DefaultColWidth;
    end;

    Line1 := '';
  end;

  if (APlanner.Mode.PlannerType = plDay) or ((Line1 <> '') and not APlanner.Sidebar.RotateOnTop) then
  begin
    OnTheOur := False;
    OldSize := APDFLib.Graphics.Font.Size;
    if (APlanner.Mode.PlannerType = plDay) then
      APDFLib.Graphics.Font.Size := APDFLib.Graphics.Font.Size * APlanner.Sidebar.HourFontRatio - 1;
    try
      MajorLineWidth := APDFLib.Graphics.DrawText(Line1, PointF(0, 0), True).Height;
      if APlanner.Mode.PlannerType = plDay then
      begin
        OnTheOur := Line2 = '00';
        if Line3 <> '' then
          Line2 := Line2 + #13#10 + Line3;
      end;

      HRect := ARect;

      if (APlanner.Mode.PlannerType = plDay) then
      begin
        Mins := (AColumn + APlanner.Display.DisplayStart) * APlanner.Display.DisplayUnit + APlanner.Display.DisplayOffset;
        delta := Round(((Mins mod 60) / APlanner.Display.DisplayUnit));
        if (delta > 0) then
          HRect.Left := HRect.Left - delta * DefColWidth;

        HRect.Right := HRect.Right + delta * DefColWidth;

        if DefColWidth * Abs(delta) < APDFLib.Graphics.DrawText(Line1, PointF(0, 0), True).Width then
          HS := true;
      end;

      if OnTheOur or HS then
      begin
        if APlanner.Sidebar.Alignment = taLeftJustify then
          hrect.Right := hrect.Right + 100;

        tr := APDFLib.Graphics.DrawText(Line1, HRect, True);
        tr.Width := tr.Width + 1 * AScale;
        tr.Height := tr.Height + 1 * AScale;

        case HorizontalAlign of
          0: APDFLib.Graphics.DrawText(Line1, RectF(HRect.Left, HRect.Top, HRect.Right, HRect.Top + tr.Height));
          2: APDFLib.Graphics.DrawText(Line1, RectF(HRect.Right - tr.Width, HRect.Top, HRect.Right, HRect.Top + tr.Height));
          1: APDFLib.Graphics.DrawText(Line1, RectF(HRect.Left + (HRect.Width - tr.Width) / 2, HRect.Top, HRect.Left + (HRect.Width - tr.Width) / 2 + tr.Width, HRect.Top + tr.Height));
        end;
      end;

      if (AColumn > 0) and (AColumn <= APlanner.GridControl.ColCount - 1) and (APlanner.Mode.PlannerType = plDay) then
      begin
        APDFLib.Graphics.Stroke.Color := APlanner.Sidebar.LineColor;

        if OnTheOur then
          APDFLib.Graphics.DrawLine(PointF(ARect.Left - 1, ARect.Bottom - 24), PointF(ARect.Left - 1, ARect.Bottom))
        else
          APDFLib.Graphics.DrawLine(PointF(ARect.Left - 1, ARect.Bottom - 12), PointF(ARect.Left - 1, ARect.Bottom));
      end;

      if (APlanner.Sidebar.TimeIndicator) then
      begin
        APlanner.CellToAbsTime(OCol, t1, t2);

        if (Frac(Now) >= Frac(t1)) and (Frac(Now) <= Frac(t2)) then
        begin
          p := APlanner.Display.DisplayScale *
            ((Frac(Now) - Frac(t1)) / (Frac(t2) - Frac(t1)));

          DrawTimeIndicator(PRect, p);
        end;
      end;

      ARect.Top := ARect.Top + MajorLineWidth;
    finally
      APDFLib.Graphics.Font.Size := OldSize;
    end;
  end
  else
  begin
    if (Line2 = '') then
    begin
      Line2 := Line1;
      Line1 := '';
    end;
  end;

  if (Line2 <> '') then
    case APlanner.Mode.PlannerType of
       plDay, plWeek, plTimeLine:
       begin
         tr := APDFLib.Graphics.DrawText(Line2, ARect, True);
         tr.Width := tr.Width + 1 * AScale;
         tr.Height := tr.Height + 1 * AScale;

         case HorizontalAlign of
           0: APDFLib.Graphics.DrawText(Line2, RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Top + tr.Height));
           2: APDFLib.Graphics.DrawText(Line2, RectF(ARect.Right - tr.Width, ARect.Top, ARect.Right, ARect.Top + tr.Height));
           1: APDFLib.Graphics.DrawText(Line2, RectF(ARect.Left + (ARect.Width - tr.Width) / 2, ARect.Top, ARect.Left + (ARect.Width - tr.Width) / 2 + tr.Width, ARect.Top + tr.Height));
         end;
       end;

        plMonth, plDayPeriod, plHalfDayPeriod, plActiveDayPeriod, plMultiMonth, plCustomList, plCustom:
        begin
          if MinorLineWidth >= ARect.Right - ARect.Left then
            MinorLineWidth := 0
          else
            case HorizontalAlign of
              0: MinorLineWidth := 0;
              2: MinorLineWidth := ARect.Right - ARect.Left - MinorLineWidth;
              1: MinorLineWidth := Round(ARect.Right - ARect.Left - MinorLineWidth) shr 1;
            end;
          try
            if APlanner.Sidebar.RotateOnTop then
            begin
              if (Line1 <> '') then
              begin
                APDFLib.Graphics.DrawText(Line1, RectF(ARect.Left + MinorLineWidth, ARect.Bottom - 4, ARect.Left + MinorLineWidth + ARect.Width, ARect.Bottom - 4 + ARect.Height));
                ARect.Left := ARect.Left + APDFLib.Graphics.DrawText('gh', PointF(0, 0), True).Height;
              end;

              APDFLib.Graphics.DrawText(Line2, RectF(ARect.Left + MinorLineWidth, ARect.Bottom - 4, ARect.Left + MinorLineWidth + ARect.Width, ARect.Bottom - 4 + ARect.Height));
              ARect.Left := ARect.Left + APDFLib.Graphics.DrawText('gh', PointF(0, 0), True).Height;
              if Line3 <> '' then
                APDFLib.Graphics.DrawText(Line3, RectF(ARect.Left + MinorLineWidth, ARect.Bottom - 4, ARect.Left + MinorLineWidth + ARect.Width, ARect.Bottom - 4 + ARect.Height));
            end
            else
            begin
              tr := APDFLib.Graphics.DrawText(Line2, ARect, True);
              tr.Width := tr.Width + 1 * AScale;
              tr.Height := tr.Height + 1 * AScale;
              case HorizontalAlign of
                0: APDFLib.Graphics.DrawText(Line2, RectF(ARect.Left, ARect.Top, ARect.Left + tr.Width, ARect.Top + tr.Height));
                2: APDFLib.Graphics.DrawText(Line2, RectF(ARect.Right - tr.Width, ARect.Top, ARect.Right, ARect.Top + tr.Height));
                1: APDFLib.Graphics.DrawText(Line2, RectF(ARect.Left + (ARect.Width - tr.Width) / 2, ARect.Top, ARect.Left + (ARect.Width - tr.Width) / 2 + tr.Width, ARect.Top + tr.Height));
              end;
            end;

          finally
          end;
        end;
    end;
end;

procedure DrawSideCol(ARect: TRectF; ARow, APos, Offs: Integer; Occupied: Boolean);
var
  Line1, Line2, Line3, DT: string;
  HorizontalAlign: Integer;
  MinutesWidth, HoursWidth, MinutesHeight: Single;
  OldSize: Single;
  OnTheHour: Boolean;
  HS, IsDay: Boolean;
  DNum, delta: Integer;
  DRect: TRectF;
  dday: Integer;
  BTM: dword;
  t1, t2: TDateTime;
  p: double;
  PRect: TRectF;
  ORow: Integer;
  tr: TRectF;

  procedure DrawTimeIndicator(ARect: TRectF; po: double);
  var
    offset: Single;
    pic: TPicture;
  begin
    offset := po;
    if (APlanner.Sidebar.TimeIndicatorType in [tiLine, tiLineGlyph]) then
    begin
      APDFLib.Graphics.Stroke.Color := APlanner.Sidebar.TimeIndicatorColor;
      APDFLib.Graphics.Stroke.Width := 3 * AScale;
      APDFLib.Graphics.DrawLine(PointF(ARect.Left, ARect.Top + offset), PointF(ARect.Right, ARect.Top + offset));
      APDFLib.Graphics.Stroke.Width := 1 * AScale;
    end;

    if (APlanner.Sidebar.TimeIndicatorType in [tiLineGlyph, tiGlyph]) and not APlanner.Sidebar.TimeIndicatorGlyph.Empty then
    begin
      APlanner.Sidebar.TimeIndicatorGlyph.Transparent := true;
      pic := TPicture.Create;
      pic.Assign(APlanner.Sidebar.TimeIndicatorGlyph);
      try
        APDFLib.Graphics.DrawImage(pic, PointF(ARect.Left, ARect.Top + offset - (APlanner.Sidebar.TimeIndicatorGlyph.Height / 2)));
      finally
        pic.Free;
      end;
    end;
  end;

begin
  HS := False;

  PRect := ARect;
  ORow := ARow;

  if (APlanner.Display.DisplayText > 0) then
  begin
    delta := (APlanner.Display.DisplayStart + ARow) mod APlanner.Display.DisplayText;

    if (delta <> 0) then
    begin
      ARow := ARow - delta;

      ARect.Top := ARect.Top - delta * (ARect.Bottom - ARect.Top + 2);
    end;
  end;

  APlanner.GetSideBarText(ARow + Offs, APos, Line1, Line2, Line3, HS);
  tr := APDFLib.Graphics.DrawText(Line2, PointF(0, 0), True);
  tr.Width := tr.Width + 1 * AScale;
  tr.Height := tr.Height + 1 * AScale;
  MinutesWidth := tr.Width;
  MinutesHeight := tr.Height;

  if Occupied then
    APDFLib.Graphics.Font.Color := APlanner.Sidebar.OccupiedFontColor;

  HorizontalAlign := AlignToFlag(APlanner.Sidebar.Alignment);

  if (APlanner.Mode.PlannerType = plHalfDayPeriod) then
  begin
    DRect := ARect;

    if not Odd(ARow) then
      ARect.Bottom := ARect.Bottom + APlanner.GridControl.DefaultRowHeight
    else
      ARect.Top := ARect.Top - APlanner.GridControl.DefaultRowHeight;

    APDFLib.Graphics.Stroke.Color := APlanner.Sidebar.SeparatorLineColor;

    APDFLib.Graphics.DrawLine(PointF(ARect.Left - 1, ARect.Bottom), PointF(ARect.Right, ARect.Bottom));

    if APlanner.Sidebar.AMPMPos = apNone then
      Line3 := '';

    BTM := 0;
    if Odd(ARow) then
      BTM := 8;

    case BTM of
      0: APDFLib.Graphics.DrawText(Line3, RectF(DRect.Right - tr.Width, DRect.Top, DRect.Right, DRect.Top + tr.Height));
      8: APDFLib.Graphics.DrawText(Line3, RectF(DRect.Right - tr.Width, DRect.Bottom - tr.Height, DRect.Right, DRect.Bottom));
    end;

    Line3 := '';
  end;

  if (APlanner.Mode.PlannerType = plTimeLine) then
  begin
    if Line2 <> '' then
      Line1 := Line1 + FormatSettings.TimeSeparator + Line2;

    dday := (MININDAY div APlanner.Display.DisplayUnit) -
      (APlanner.Mode.TimeLineNVUEnd + APlanner.Mode.TimeLineNVUBegin);

    if dday = 0 then
      dday := 1;

    delta := ARow mod dday;
    DNum := ARow div dday;

    begin
      if APlanner.Sidebar.DateTimeFormat <> '' then
        DT := FormatDateTime(APlanner.Sidebar.DateTimeFormat,
          APlanner.Mode.TimeLineStart + DNum)
      else
        DT := DateToStr(APlanner.Mode.TimeLineStart + DNum);

      DRect := PRect;

      DRect.Top := PRect.Top - delta * (PRect.Bottom - PRect.Top + 2);
      DRect.Bottom := DRect.Top + dday * (PRect.Bottom - PRect.Top + 2);

      APDFLib.Graphics.DrawText(DT, PointF(DRect.Left, DRect.Bottom - 4));
    end;

    if (delta = 0) and (ARow > 0) then
    begin
      DRect.Top := ARow * APlanner.GridControl.DefaultRowHeight;
      APDFLib.Graphics.Stroke.Color := APlanner.GridLineColor;
      APDFLib.Graphics.DrawLine(PointF(ARect.Left - 1, ARect.Top - 1), PointF(ARect.Right - 1, ARect.Top - 1));
    end;

    MinutesWidth := APDFLib.Graphics.DrawText(Line1, PointF(0, 0), True).Width;
    ARect.Left := ARect.Right - MinutesWidth;

    Line2 := Line1;
    Line1 := '';

    if (APlanner.Sidebar.TimeIndicator) then
    begin
      APlanner.CellToAbsTime(ORow, t1, t2);

      if (Now >= t1) and (Now <= t2) then
      begin
        p := APlanner.Display.DisplayScale * ((Now - t1) / (t2 - t1));
        DrawTimeIndicator(PRect, p);
      end;
    end;
  end;

  if (ARect.Bottom - ARect.Top < MinutesHeight * 2) and
    (APlanner.Mode.PlannerType <> plDay) then
    Line1 := '';

  if (APlanner.Mode.PlannerType in [plDay]) or (Line1 <> '') then
  begin
    OnTheHour := False;
    IsDay := APlanner.Mode.PlannerType = plDay;

    if (APlanner.Sidebar.TimeIndicator) then
    begin
      APlanner.CellToAbsTime(ORow, t1, t2);

      if (Frac(Now) >= Frac(t1)) and (Frac(Now) <= Frac(t2)) then
      begin
        p := APlanner.Display.DisplayScale *
          ((Frac(Now) - Frac(t1)) / (Frac(t2) - Frac(t1)));

        DrawTimeIndicator(PRect, p);
      end;
    end;

    HS := HS or not IsDay;

    begin
      if APlanner.Mode.PlannerType = plDay then
      begin
        OnTheHour := Pos('00', Line2) > 0;

        if APlanner.Sidebar.AMPMPos = apNone then
          Line3 := '';

        if Line3 <> '' then
        begin
          if APlanner.Sidebar.AMPMPos = apUnderTime then
            Line2 := Line2 + #13#10 + Line3
          else
          begin
            Line2 := Line2 + ' ' + Line3;
            MinutesWidth := APDFLib.Graphics.DrawText(Line2, PointF(0, 0), True).Width;
          end;
        end;
      end;

      if APlanner.Mode.PlannerType = plHalfDayPeriod then
      begin
        if APlanner.Sidebar.AMPMPos = apNone then
          Line3 := '';

        if APlanner.Sidebar.AMPMPos = apUnderTime then
          Line2 := Line2 + #13#10 + Line3
        else
          Line2 := Line2 + ' ' + Line3
      end;

      if APlanner.Sidebar.Flat and (APlanner.Mode.PlannerType = plDay) and
        (ARow <> APlanner.GridControl.TopRow) then
      begin
        APDFLib.Graphics.Stroke.Color := APlanner.Sidebar.LineColor;

        if OnTheHour then
          APDFLib.Graphics.DrawLine(PointF(ARect.Left, ARect.Top - 1), PointF(ARect.Right - 1, ARect.Top - 1))
        else
          APDFLib.Graphics.DrawLine(PointF(ARect.Right - MinutesWidth, ARect.Top - 1), PointF(ARect.Right - 1, ARect.Top - 1));

      end;
    end;

    OldSize := APDFLib.Graphics.Font.Size;
    try

      if IsDay then
        APDFLib.Graphics.Font.Size := APDFLib.Graphics.Font.Size * APlanner.Sidebar.HourFontRatio
      else
        MinutesWidth := 0;

      HoursWidth := APDFLib.Graphics.DrawText(Line1, PointF(0, 0), True).Width;

      case APlanner.Sidebar.Alignment of
      taLeftJustify:
        begin
          if HS then
            APDFLib.Graphics.DrawText(Line1, PointF(ARect.Left, ARect.Top));

          if IsDay then
            ARect.Left := ARect.Left + HoursWidth + 4
        end;
      taRightJustify:
        begin
          if HS then
            APDFLib.Graphics.DrawText(Line1, PointF(ARect.Right - MinutesWidth - 4 - HoursWidth, ARect.Top));
        end;
      taCenter:
        begin
          if HS then
            APDFLib.Graphics.DrawText(Line1, PointF(ARect.Left + (ARect.Right - ARect.Left - HoursWidth - MinutesWidth) / 2, ARect.Top));
          if IsDay then
            ARect.Left := ARect.Left + HoursWidth + 4 + (ARect.Right - ARect.Left - HoursWidth - MinutesWidth) / 2;
        end;
      end;

      if not IsDay then
        ARect.Top := ARect.Top + MinutesHeight + 2;

    finally
      if IsDay then
        APDFLib.Graphics.Font.Size := OldSize;
    end;
  end;

  if (APlanner.Display.DisplayText > 0) then
  begin
    if ((APlanner.Display.DisplayStart + ARow) mod APlanner.Display.DisplayText <> 0) then
    begin
      Line2 := '';
    end;
  end;

  { Painting }
  if (Line2 <> '') then
  begin
    tr := APDFLib.Graphics.DrawText(Line2, ARect, True);
    tr.Width := tr.Width + 1 * AScale;
    tr.Height := tr.Height + 1 * AScale;

    case HorizontalAlign of
      0: APDFLib.Graphics.DrawText(Line2, RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Top + tr.Height));
      2: APDFLib.Graphics.DrawText(Line2, RectF(ARect.Right - tr.Width, ARect.Top, ARect.Right, ARect.Top + tr.Height));
      1: APDFLib.Graphics.DrawText(Line2, RectF(ARect.Left + (ARect.Width - tr.Width) / 2, ARect.Top, ARect.Left + (ARect.Width - tr.Width) / 2 + tr.Width, ARect.Top + tr.Height));
    end;
  end;
end;

function ConvertToPlainText(AText: string): string;
begin
  Result := AText;
  if (Pos('{\rtf', LowerCase(AText)) > 0) then
  begin
    APlanner.TextToRich(AText);
    Result := APlanner.RichEdit.Text;
  end;
end;

var
  dr: TRectF;
  oc: Boolean;
  pi: TPlannerItem;
  b: TBrush;
  s: string;
  capr, capdr, tr: TRectF;
begin
  APDFLib.Graphics.Stroke.Width := 1 * AScale;
  APDFLib.Graphics.Stroke.Color := gcNull;
  APDFLib.Graphics.Fill.Color := gcNull;
  APDFLib.Graphics.Fill.ColorTo := gcNull;
  APDFLib.Graphics.Fill.Kind := gfkNone;

  if ((ARow < ARowOff) or (ACol < AColOff)) and APlanner.Header.Visible then
  begin
    APDFLib.Graphics.Fill.Color := APlanner.Header.Color;
    APDFLib.Graphics.Fill.ColorTo := APlanner.Header.ColorTo;
    APDFLib.Graphics.Fill.Kind := gfkGradient;
    APDFLib.Graphics.DrawRectangle(ARect);
    APDFLib.Graphics.Stroke.Color := APlanner.Header.LineColor;
    dr := ARect;
    if APlanner.Sidebar.Position in [spTop] then
    begin
      InflateRectEx(dr, -1, 0);
      APDFLib.Graphics.Fill.Orientation := gfoHorizontal;
    end
    else
    begin
      InflateRectEx(dr, 0, -1);
      APDFLib.Graphics.Fill.Orientation := gfoVertical;
    end;

    APDFLib.Graphics.DrawRectangle(dr);

    APDFLib.Graphics.Font.BeginUpdate;
    APDFLib.Graphics.Font.Name := APlanner.Header.Font.Name;
    APDFLib.Graphics.Font.Color := APlanner.Header.Font.Color;
    APDFLib.Graphics.Font.Style := APlanner.Header.Font.Style;
    APDFLib.Graphics.Font.Size := APlanner.Header.Font.Size * AScale;
    APDFLib.Graphics.Font.EndUpdate;

    APDFLib.Graphics.URLFont.BeginUpdate;
    APDFLib.Graphics.URLFont.Name := APlanner.Header.Font.Name;
    APDFLib.Graphics.URLFont.Color := APlanner.URLColor;
    APDFLib.Graphics.URLFont.Size := APlanner.Header.Font.Size * AScale;
    APDFLib.Graphics.URLFont.EndUpdate;

    if APlanner.Sidebar.Position in [spTop] then
      s := APlanner.Header.Captions[ARow - ARowOff]
    else
      s := APlanner.Header.Captions[ACol - AColOff];

    s := ConvertToPlainText(s);
    capr := dr;
    if s <> '' then
    begin
      case APlanner.Header.Alignment of
        taLeftJustify: APDFLib.Graphics.Alignment := gtaLeading;
        taRightJustify: APDFLib.Graphics.Alignment := gtaTrailing;
        taCenter: APDFLib.Graphics.Alignment := gtaCenter;
      end;

      if TAdvUtils.IsHTML(s) then
        tr := APDFLib.Graphics.DrawHTMLText(s, PointF(0, 0), AScale, True)
      else
        tr := APDFLib.Graphics.DrawText(s, PointF(0, 0), True);


      tr.Height := tr.Height + 4 * AScale;

      if not TAdvUtils.IsHTML(s) then
      begin
        case APlanner.Header.VAlignment of
          vtaCenter: capr := RectF(capr.Left, capr.Top + (capr.Height - tr.Height) / 2, capr.Right, capr.Top + (capr.Height - tr.Height) / 2 + tr.Height);
          vtaTop: capr := RectF(capr.Left, capr.Top, capr.Right, capr.Top + tr.Height);
          vtaBottom: capr := RectF(capr.Left, capr.Bottom - tr.Height, capr.Right, capr.Bottom);
        end;
      end;

      capdr := capr;
      InflateRectEx(capdr, -2 * AScale, -2 * AScale);

      if TAdvUtils.IsHTML(s) then
        APDFLib.Graphics.DrawHTMLText(s, capdr, False, AScale)
      else
        APDFLib.Graphics.DrawText(s, capdr);

      APDFLib.Graphics.Alignment := gtaLeading;
    end;
  end
  else
  begin
    ARow := ARow - ARowOff;
    ACol := ACol - AColOff;
    case APlanner.Sidebar.Orientation of
      soHorizontal:
      begin
        if (ARow < APlanner.GridControl.FixedRows) and APlanner.Sidebar.Visible then
        begin
          pi := TPlannerItemsOpen(APlanner.Items).FindItemIdx(ACol);
          oc := (pi <> nil) and APlanner.Sidebar.ShowOccupied;
          APDFLib.Graphics.Font.Name := APlanner.Sidebar.Font.Name;
          APDFLib.Graphics.Font.Size := APlanner.Sidebar.Font.Size * AScale;
          APDFLib.Graphics.Font.Color := APlanner.Sidebar.Font.Color;
          APDFLib.Graphics.Font.Style := APlanner.Sidebar.Font.Style;

          APDFLib.Graphics.Fill.Color := APlanner.Sidebar.Background;
          APDFLib.Graphics.Fill.ColorTo := APlanner.Sidebar.BackgroundTo;
          APDFLib.Graphics.Fill.Kind := gfkGradient;
          APDFLib.Graphics.Fill.Orientation := gfoVertical;
          dr := ARect;
          APDFLib.Graphics.DrawRectangle(dr);

          DrawSideRow(dr, ACol, ARow, oc, APlanner.GridControl.DefaultColWidth);

          if not (APlanner.Mode.PlannerType in [plDay, plTimeLine, plHalfDayPeriod]) then
          begin
            APDFLib.Graphics.Stroke.Color := APlanner.Sidebar.SeparatorLineColor;
            APDFLib.Graphics.DrawLine(PointF(dr.Right - 1 * AScale, dr.Bottom - 1 * AScale), PointF(dr.Right - 1 * AScale, dr.Top));
          end;
        end
        else
        begin
          b := TBrush.Create;
          TCustomPlannerOpen(APlanner).GetCellBrush(ARow, ACol, b);
          APDFLib.Graphics.Fill.Color := b.Color;
          APDFLib.Graphics.Fill.Kind := gfkSolid;
          b.Free;

          dr := ARect;
          APDFLib.Graphics.DrawRectangle(dr);

          if (APlanner.Mode.PlannerType = plDay) then
          begin
            if ((ACol + 1 + APlanner.Display.DisplayStart) * APlanner.Display.DisplayUnit mod 60 = 0) then
              APDFLib.Graphics.Stroke.Color := APlanner.Display.HourLineColor
            else
              APDFLib.Graphics.Stroke.Color := APlanner.GridLineColor;
          end
          else
            APDFLib.Graphics.Stroke.Color := APlanner.GridLineColor;

          APDFLib.Graphics.DrawLine(PointF(dr.Left - 1, dr.Top), PointF(dr.Left - 1, dr.Bottom));
          APDFLib.Graphics.Stroke.Color := APlanner.GridLineColor;
          APDFLib.Graphics.DrawLine(PointF(dr.Left, dr.Top), PointF(dr.Right, dr.Top));
          APDFLib.Graphics.DrawLine(PointF(dr.Left, dr.Bottom), PointF(dr.Right, dr.Bottom));
          if ACol = APlanner.GridControl.ColCount - 1 then
            APDFLib.Graphics.DrawLine(PointF(dr.Right, dr.Top), PointF(dr.Right, dr.Bottom));
        end;
      end;
      soVertical:
      begin
        if (ACol < APlanner.GridControl.FixedCols) and APlanner.Sidebar.Visible then
        begin
          pi := TPlannerItemsOpen(APlanner.Items).FindItemIdx(ARow);
          oc := (pi <> nil) and APlanner.Sidebar.ShowOccupied;
          APDFLib.Graphics.Font.Name := APlanner.Sidebar.Font.Name;
          APDFLib.Graphics.Font.Size := APlanner.Sidebar.Font.Size * AScale;
          APDFLib.Graphics.Font.Color := APlanner.Sidebar.Font.Color;
          APDFLib.Graphics.Font.Style := APlanner.Sidebar.Font.Style;

          APDFLib.Graphics.Fill.Color := APlanner.Sidebar.Background;
          APDFLib.Graphics.Fill.ColorTo := APlanner.Sidebar.BackgroundTo;
          APDFLib.Graphics.Fill.Kind := gfkGradient;
          APDFLib.Graphics.Fill.Orientation := gfoHorizontal;
          dr := ARect;
          APDFLib.Graphics.DrawRectangle(dr);

          DrawSideCol(dr, ARow, 0, 0, oc);

          if not (APlanner.Mode.PlannerType in [plDay, plTimeLine, plHalfDayPeriod]) then
          begin
            APDFLib.Graphics.Stroke.Color := APlanner.Sidebar.SeparatorLineColor;
            APDFLib.Graphics.DrawLine(PointF(dr.Left, dr.Bottom - 1 * AScale), PointF(dr.Right - 1 * AScale, dr.Bottom - 1 * AScale));
          end;
        end
        else
        begin
          b := TBrush.Create;
          TCustomPlannerOpen(APlanner).GetCellBrush(ACol, ARow, b);
          APDFLib.Graphics.Fill.Color := b.Color;
          APDFLib.Graphics.Fill.Kind := gfkSolid;
          b.Free;

          dr := ARect;
          if AStart then
            dr.Top := dr.Top - 1;
          APDFLib.Graphics.DrawRectangle(dr);

          if (APlanner.Mode.PlannerType = plDay) then
          begin
            if ((ARow + 1 + APlanner.Display.DisplayStart) * APlanner.Display.DisplayUnit mod 60 = 0) then
              APDFLib.Graphics.Stroke.Color := APlanner.Display.HourLineColor
            else
              APDFLib.Graphics.Stroke.Color := APlanner.GridLineColor;
          end
          else
            APDFLib.Graphics.Stroke.Color := APlanner.GridLineColor;

          APDFLib.Graphics.DrawLine(PointF(dr.Left, dr.Bottom - 1), PointF(dr.Right, dr.Bottom - 1));
          APDFLib.Graphics.Stroke.Color := APlanner.GridLineColor;
          APDFLib.Graphics.DrawLine(PointF(dr.Right, dr.Top), PointF(dr.Right, dr.Bottom));
          APDFLib.Graphics.DrawLine(PointF(dr.Left, dr.Top), PointF(dr.Left, dr.Bottom));
          if AStart and (APlanner.Header.Visible = False) then
            APDFLib.Graphics.DrawLine(PointF(dr.Left, dr.Top), PointF(dr.Right, dr.Top));
        end;
      end;
    end;
  end;
end;

constructor TAdvCustomPlannerPDFIO.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  if Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for I := 0 to AOwner.ComponentCount - 1 do
    begin
      if (AOwner.Components[i] is TCustomPlanner) then
      begin
        Planner := AOwner.Components[i] as TCustomPlanner;
        Break;
      end;
    end;
  end;
end;

function TAdvCustomPlannerPDFIO.CreateOptions: TAdvPDFIOOptions;
begin
  Result := TAdvPlannerPDFIOOptions.Create;
end;

procedure TAdvCustomPlannerPDFIO.DoRowIsPageBreak(ARow: integer;
  var IsPageBreak: boolean);
begin
  if Assigned(OnRowIsPageBreak) then
    OnRowIsPageBreak(Self, Arow, IsPageBreak);
end;

procedure TAdvCustomPlannerPDFIO.DoPDFExport(const APDFLib: TAdvPDFLib; const AExportObject: TAdvPDFIOExportObject);
var
  pgb: array of TAdvPlannerPDFIOCellRange;
  I: Integer;
  pb: Boolean;
  pi: integer;
  g: TCustomPlanner;
  rc, cc: Integer;
begin
  if not Assigned(AExportObject) or (Assigned(AExportObject) and not (AExportObject is TCustomPlanner)) then
    raise Exception.Create(sTMSPlannerPDFIOPlannerNotAssigned);

  g := AExportObject as TCustomPlanner;
  rc := g.GridControl.RowCount;
  cc := g.GridControl.ColCount;

  if (g.Sidebar.Position in [spLeft, spRight, spLeftRight]) and g.Header.Visible then
    rc := rc + 1;

  if (g.Sidebar.Position in [spTop]) and g.Header.Visible then
    cc := cc + 1;

  pi := 0;
  pgb := nil;
  for I := 0 to rc - 1 do
  begin
    pb := False;
    DoRowIsPageBreak(I, pb);
    if pb then
    begin
      SetLength(pgb, Length(pgb) + 1);
      pgb[Length(pgb) - 1] := CellRange(0, pi, cc - 1, I - 1);
      pi := I;
    end;
  end;

  if pi < rc - 1 then
  begin
    SetLength(pgb, Length(pgb) + 1);
    pgb[Length(pgb) - 1] := CellRange(0, pi, cc - 1, rc - 1);
  end;

  for I := 0 to Length(pgb) - 1 do
    DoPDFPlannerExport(APDFLib, g, pgb[I]);
end;

procedure TAdvCustomPlannerPDFIO.DOPDFPlannerExport(const APDFLib: TAdvPDFLib; const APlanner: TCustomPlanner; const ASelection: TAdvPlannerPDFIOCellRange);
var
  c, r: Integer;
  cwr, rhr, cw, rh, cwrt, rhrt: Single;
  I, K: Integer;
  xr, yr: Single;
  bc, br: Integer;
  cellr: TRectF;
  x, y, xoff, xoffw, xoffc: Single;
  rSt, csave, cSt: Integer;
  chk: Boolean;
  FFixedRowsSave, FFixedColumnsSave: Integer;
  FCSave, FRSave: Integer;
  FCSaveEx, FRSaveEx: Integer;
  FStartY: Single;
  FFixedRowGet, FFixedColGet: Boolean;
  plIt: TPlannerItemOpen;
  FSetRSave, FSetCSave: Boolean;
  xs, ys: Single;
  ph, pw: Single;
  xts, yts: Single;
  FSaveXBounds: Single;
  FSaveYBounds: Single;
  w, h: Single;
  sc: Single;
  CurrentPlanner: TCustomPlanner;
  hh, ww, caph: Single;
  offc, offr: Integer;
  st: Boolean;
  dtr, tr: TRectF;
  bmp: TPictureContainer;
  it: TPlannerItem;
  s: string;
  itr: TRect;
  ypos, yposx: Single;
  itrd, capr, capdr, notr, notdr: TRectF;
  FFirstPage: Boolean;

  function IsHorizontal: Boolean;
  begin
    Result := CurrentPlanner.Sidebar.Position in [spTop];
  end;

  function GetFixedPrintWidth: Single;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to offc - 1 do
      Result := Result + CurrentPlanner.GridControl.ColWidths[I] * sc;

    if IsHorizontal and CurrentPlanner.Header.Visible then
      Result := Result + CurrentPlanner.Header.Height * sc;
  end;

  function GetFixedPrintHeight: Single;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to offr - 1 do
      Result := Result + CurrentPlanner.GridControl.RowHeights[I] * sc;
  end;

  function GetScale: Single;
  var
    K: Integer;
  begin
    Result := 1;
    if Options.FitToPage then
    begin
      w := 0;
      for K := 0 to CurrentPlanner.GridControl.ColCount - 1 do
        w := w + Round(CurrentPlanner.GridControl.ColWidths[K]);

      if IsHorizontal and CurrentPlanner.Header.Visible then
        w := w + CurrentPlanner.Header.Height;

      if w > 0 then
        Result := (APDFLib.MediaBox.Width - Options.Margins.Left - Options.Margins.Right) / w;
    end;
  end;

  function ConvertToPlainText(AText: string): string;
  begin
    Result := AText;
    if (Pos('{\rtf', LowerCase(AText)) > 0) then
    begin
      CurrentPlanner.TextToRich(AText);
      Result := CurrentPlanner.RichEdit.Text;
    end;
  end;

  function IsItemInList(AItem: TPlannerItem): Boolean;
  var
    mp, pi: Integer;
    co: Integer;
    itc: TPlannerItem;
    ri: Integer;
    K: Integer;
    offs, offsx: Integer;
  begin
    Result := False;
    if not IsHorizontal then
    begin
      co := c - offc - CurrentPlanner.GridControl.FixedCols;
      mp := TPlannerItemsOpen(CurrentPlanner.Items).MaxItemsInPos(co);
      for pi := 0 to mp - 1 do
      begin
        for ri := FRSaveEx to r - offr do
        begin
          itc := TPlannerItemsOpen(CurrentPlanner.Items).FindItemPosIdx(ri, co, pi);
          if AItem = itc then
          begin
            Result := True;
            Break;
          end;
        end;
      end
    end
    else if IsHorizontal then
    begin
      offs := 0;
      offsx := 0;
      if not FFirstPage then
        offs := 1
      else
        offsx := 1;

      for K := FRSaveEx - offs to r - offs - offsx do
      begin
        mp := TPlannerItemsOpen(CurrentPlanner.Items).MaxItemsInPos(K);
        for pi := 0 to mp - 1 do
        begin
          for ri := FCSaveEx to ASelection.EndCol do
          begin
            itc := TPlannerItemsOpen(CurrentPlanner.Items).FindItemPosIdx(ri, K, pi);
            if AItem = itc then
            begin
              Result := True;
              Break;
            end;
          end;
        end
      end;
    end;
  end;

  procedure DrawItems;
  var
    I, K: Integer;
  begin
    if ((c - offc > 0) and not IsHorizontal) then
    begin
      for I := 0 to CurrentPlanner.Items.Count - 1 do
      begin
        it := CurrentPlanner.Items[I];
        if Assigned(it) and IsItemInList(it) then
        begin
          itr := it.ItemRect;
          ypos := FStartY;
          for K := FRSaveEx to it.ItemBegin do
            ypos := ypos + CurrentPlanner.GridControl.RowHeights[K] * sc;

          yposx := ypos;
          for K := Max(FRSaveEx, it.ItemBegin + 1) to Min(R, it.ItemEnd) do
            yposx := yposx + CurrentPlanner.GridControl.RowHeights[K] * sc;

          xoff := 0;
          xoffw := 0;
          if it.Conflicts > 0 then
          begin
            xoffw := (cw / it.Conflicts);
            xoff := xoffw * it.ConflictPos;
          end;

          itrd := RectF(x + xoff, ypos - 1, x + xoff + xoffw - (CurrentPlanner.ItemGap * sc), yposx - 1);
          InflateRectEx(itrd, -1 * sc, -1 * sc);

          APDFLib.Graphics.Stroke.Color := it.BorderColor;
          APDFLib.Graphics.Fill.Color := it.Color;
          APDFLib.Graphics.Fill.ColorTo := it.ColorTo;
          APDFLib.Graphics.Fill.Kind := gfkGradient;
          APDFLib.Graphics.DrawRectangle(itrd);

          if (it.ItemBegin >= FRSaveEx - offr) and (it.ItemBegin <= r - offr) then
          begin
            APDFLib.Graphics.Font.BeginUpdate;
            APDFLib.Graphics.Font.Name := it.CaptionFont.Name;
            APDFLib.Graphics.Font.Color := it.CaptionFont.Color;
            APDFLib.Graphics.Font.Style := it.CaptionFont.Style;
            APDFLib.Graphics.Font.Size := it.CaptionFont.Size * sc;
            APDFLib.Graphics.Font.EndUpdate;

            APDFLib.Graphics.URLFont.BeginUpdate;
            APDFLib.Graphics.URLFont.Name := it.CaptionFont.Name;
            APDFLib.Graphics.URLFont.Color := CurrentPlanner.URLColor;
            APDFLib.Graphics.URLFont.Size := it.CaptionFont.Size * sc;
            APDFLib.Graphics.URLFont.EndUpdate;

            s := '';
            case it.CaptionType of
              ctText: s := it.CaptionText;
              ctTime: s := PlannerGetIdCol(Planner, it.ItemBegin, it.ItemPos) + ' - ' + PlannerGetIdCol(Planner, it.ItemEnd, it.ItemPos);
              ctTimeText: s := PlannerGetIdCol(Planner, it.ItemBegin, it.ItemPos) + ' - ' + PlannerGetIdCol(Planner, it.ItemEnd, it.ItemPos) + ' ' + it.CaptionText;
            end;

            s := ConvertToPlainText(s);
            capr := RectF(itrd.Left, itrd.Top, itrd.Right, itrd.Top);
            if s <> '' then
            begin
              case it.CaptionAlign of
                taLeftJustify: APDFLib.Graphics.Alignment := gtaLeading;
                taRightJustify: APDFLib.Graphics.Alignment := gtaTrailing;
                taCenter: APDFLib.Graphics.Alignment := gtaCenter;
              end;

              if TAdvUtils.IsHTML(s) then
                tr := APDFLib.Graphics.DrawHTMLText(s, PointF(0, 0), sc, True)
              else
                tr := APDFLib.Graphics.DrawText(s, PointF(0, 0), True);

              tr.Height := tr.Height + 2 * sc;

              APDFLib.Graphics.Fill.Color := it.CaptionBkg;
              APDFLib.Graphics.Fill.ColorTo := it.CaptionBkgTo;
              APDFLib.Graphics.Fill.Kind := gfkGradient;
              case it.CaptionBkgDirection of
                gdHorizontal: APDFLib.Graphics.Fill.Orientation := gfoHorizontal;
                gdVertical: APDFLib.Graphics.Fill.Orientation := gfoVertical;
              end;

              capr := RectF(itrd.Left, itrd.Top, itrd.Right, itrd.Top + tr.Height);
              APDFLib.Graphics.DrawRectangle(capr);

              capdr := capr;
              InflateRectEx(capdr, -2 * sc, -2 * sc);
              if TAdvUtils.IsHTML(s) then
                APDFLib.Graphics.DrawHTMLText(s, capdr, False, sc)
              else
                APDFLib.Graphics.DrawText(s, capdr);
            end;

            APDFLib.Graphics.Font.BeginUpdate;
            APDFLib.Graphics.Font.Name := it.Font.Name;
            APDFLib.Graphics.Font.Color := it.Font.Color;
            APDFLib.Graphics.Font.Style := it.Font.Style;
            APDFLib.Graphics.Font.Size := it.Font.Size * sc;
            APDFLib.Graphics.Font.EndUpdate;

            APDFLib.Graphics.URLFont.BeginUpdate;
            APDFLib.Graphics.URLFont.Name := it.Font.Name;
            APDFLib.Graphics.URLFont.Color := Planner.URLColor;
            APDFLib.Graphics.URLFont.Size := it.Font.Size * sc;
            APDFLib.Graphics.URLFont.EndUpdate;

            s := it.NotesText;
            s := ConvertToPlainText(s);

            if s <> '' then
            begin
              case it.Alignment of
                taLeftJustify: APDFLib.Graphics.Alignment := gtaLeading;
                taRightJustify: APDFLib.Graphics.Alignment := gtaTrailing;
                taCenter: APDFLib.Graphics.Alignment := gtaCenter;
              end;

              if TAdvUtils.IsHTML(s) then
                tr := APDFLib.Graphics.DrawHTMLText(s, PointF(0, 0), sc, True)
              else
                tr := APDFLib.Graphics.DrawText(s, PointF(0, 0), True);

              notr := RectF(itrd.Left, capr.Bottom, itrd.Right, itrd.Bottom);
              notdr := notr;
              InflateRectEx(notdr, -2 * sc, -2 * sc);

              if TAdvUtils.IsHTML(s) then
                APDFLib.Graphics.DrawHTMLText(s, notdr, False, sc)
              else
                APDFLib.Graphics.DrawText(s, notdr);
            end;
          end;
        end;
      end;
    end
    else if IsHorizontal then
    begin
      for I := 0 to CurrentPlanner.Items.Count - 1 do
      begin
        it := CurrentPlanner.Items[I];
        if Assigned(it) and IsItemInList(it) then
        begin
          itr := it.ItemRect;
          ypos := Options.Margins.Left + ww;
          for K := FCSaveEx + 1 to it.ItemBegin do
            ypos := ypos + CurrentPlanner.GridControl.ColWidths[K] * sc;

          yposx := ypos;
          for K := Max(FCSaveEx, it.ItemBegin + 1) to Min(ASelection.EndCol, it.ItemEnd) do
            yposx := yposx + CurrentPlanner.GridControl.ColWidths[K] * sc;

          if not FFirstPage and CurrentPlanner.Sidebar.Visible then
          begin
            xoffc := CurrentPlanner.Sidebar.Width * sc;
            for K := FRSaveEx - 1 to it.ItemPos - 1 do
              xoffc := xoffc + CurrentPlanner.GridControl.RowHeights[K] * sc;
          end
          else
          begin
            xoffc := 0;
            for K := FRSaveEx to it.ItemPos do
              xoffc := xoffc + CurrentPlanner.GridControl.RowHeights[K] * sc;
          end;

          xoff := 0;
          xoffw := 0;
          if it.Conflicts > 0 then
          begin
            xoffw := ((CurrentPlanner.GridControl.RowHeights[it.ItemPos + 1] * sc) / it.Conflicts);
            xoff := xoffw * it.ConflictPos;
          end;

          if FFirstPage then
            itrd := RectF(ypos, Options.Margins.Top + caph + xoffc + xoff, yposx, Options.Margins.Top + caph + xoffc + xoff + xoffw - (CurrentPlanner.ItemGap * sc))
          else
            itrd := RectF(ypos, Options.Margins.Top + xoffc + xoff, yposx, Options.Margins.Top + xoffc + xoff + xoffw - (CurrentPlanner.ItemGap * sc));

          InflateRectEx(itrd, 0, -1 * sc);
          itrd.Left := itrd.Left - 1 * sc;
          itrd.Right := itrd.Right - 1 * sc;

          APDFLib.Graphics.Stroke.Color := it.BorderColor;
          APDFLib.Graphics.Fill.Color := it.Color;
          APDFLib.Graphics.Fill.ColorTo := it.ColorTo;
          APDFLib.Graphics.Fill.Kind := gfkGradient;
          APDFLib.Graphics.DrawRectangle(itrd);

          APDFLib.Graphics.Font.BeginUpdate;
          APDFLib.Graphics.Font.Name := it.CaptionFont.Name;
          APDFLib.Graphics.Font.Color := it.CaptionFont.Color;
          APDFLib.Graphics.Font.Style := it.CaptionFont.Style;
          APDFLib.Graphics.Font.Size := it.CaptionFont.Size * sc;
          APDFLib.Graphics.Font.EndUpdate;

          APDFLib.Graphics.URLFont.BeginUpdate;
          APDFLib.Graphics.URLFont.Name := it.CaptionFont.Name;
          APDFLib.Graphics.URLFont.Color := CurrentPlanner.URLColor;
          APDFLib.Graphics.URLFont.Size := it.CaptionFont.Size * sc;
          APDFLib.Graphics.URLFont.EndUpdate;

          s := '';
          case it.CaptionType of
            ctText: s := it.CaptionText;
            ctTime: s := PlannerGetIdCol(Planner, it.ItemBegin, it.ItemPos) + ' - ' + PlannerGetIdCol(Planner, it.ItemEnd, it.ItemPos);
            ctTimeText: s := PlannerGetIdCol(Planner, it.ItemBegin, it.ItemPos) + ' - ' + PlannerGetIdCol(Planner, it.ItemEnd, it.ItemPos) + ' ' + it.CaptionText;
          end;

          s := ConvertToPlainText(s);
          capr := RectF(itrd.Left, itrd.Top, itrd.Right, itrd.Top);
          if s <> '' then
          begin
            case it.CaptionAlign of
              taLeftJustify: APDFLib.Graphics.Alignment := gtaLeading;
              taRightJustify: APDFLib.Graphics.Alignment := gtaTrailing;
              taCenter: APDFLib.Graphics.Alignment := gtaCenter;
            end;

            if TAdvUtils.IsHTML(s) then
              tr := APDFLib.Graphics.DrawHTMLText(s, PointF(0, 0), sc, True)
            else
              tr := APDFLib.Graphics.DrawText(s, PointF(0, 0), True);

            tr.Height := tr.Height + 2 * sc;

            APDFLib.Graphics.Fill.Color := it.CaptionBkg;
            APDFLib.Graphics.Fill.ColorTo := it.CaptionBkgTo;
            APDFLib.Graphics.Fill.Kind := gfkGradient;
            case it.CaptionBkgDirection of
              gdHorizontal: APDFLib.Graphics.Fill.Orientation := gfoHorizontal;
              gdVertical: APDFLib.Graphics.Fill.Orientation := gfoVertical;
            end;

            capr := RectF(itrd.Left, itrd.Top, itrd.Right, itrd.Top + tr.Height);
            APDFLib.Graphics.DrawRectangle(capr);

            capdr := capr;
            InflateRectEx(capdr, -2 * sc, -2 * sc);
            if TAdvUtils.IsHTML(s) then
              APDFLib.Graphics.DrawHTMLText(s, capdr, False, sc)
            else
              APDFLib.Graphics.DrawText(s, capdr);
          end;

          APDFLib.Graphics.Font.BeginUpdate;
          APDFLib.Graphics.Font.Name := it.Font.Name;
          APDFLib.Graphics.Font.Color := it.Font.Color;
          APDFLib.Graphics.Font.Style := it.Font.Style;
          APDFLib.Graphics.Font.Size := it.Font.Size * sc;
          APDFLib.Graphics.Font.EndUpdate;

          APDFLib.Graphics.URLFont.BeginUpdate;
          APDFLib.Graphics.URLFont.Name := it.Font.Name;
          APDFLib.Graphics.URLFont.Color := Planner.URLColor;
          APDFLib.Graphics.URLFont.Size := it.Font.Size * sc;
          APDFLib.Graphics.URLFont.EndUpdate;

          s := it.NotesText;

          if Assigned(CurrentPlanner.OnItemText) then
            CurrentPlanner.OnItemText(CurrentPlanner, it, s);

          s := ConvertToPlainText(s);

          if s <> '' then
          begin
            case it.Alignment of
              taLeftJustify: APDFLib.Graphics.Alignment := gtaLeading;
              taRightJustify: APDFLib.Graphics.Alignment := gtaTrailing;
              taCenter: APDFLib.Graphics.Alignment := gtaCenter;
            end;

            if TAdvUtils.IsHTML(s) then
              tr := APDFLib.Graphics.DrawHTMLText(s, PointF(0, 0), sc, True)
            else
              tr := APDFLib.Graphics.DrawText(s, PointF(0, 0), True);

            notr := RectF(itrd.Left, capr.Bottom, itrd.Right, itrd.Bottom);
            notdr := notr;
            InflateRectEx(notdr, -2 * sc, -2 * sc);

            if TAdvUtils.IsHTML(s) then
              APDFLib.Graphics.DrawHTMLText(s, notdr, False, sc)
            else
              APDFLib.Graphics.DrawText(s, notdr);
          end;
        end;
      end;
    end;
  end;

begin
  CurrentPlanner := APlanner;
  APDFLib.PictureContainer := CurrentPlanner.PictureContainer;

  sc := GetScale;
  FFirstPage := True;
  NewPage(APDFLib, CurrentPlanner);
  st := True;

  hh := 0;
  offr := 0;
  if not IsHorizontal and CurrentPlanner.Header.Visible then
  begin
    hh := hh + CurrentPlanner.Header.Height * sc;
    Inc(offr);
  end;

  ww := 0;
  offc := 0;
  if IsHorizontal and CurrentPlanner.Header.Visible then
  begin
    ww := ww + CurrentPlanner.Header.Height * sc;
    Inc(offc);
  end;

  caph := 0;
  if CurrentPlanner.Caption.Visible then
    caph := CurrentPlanner.Caption.Height * sc;

  w := APDFLib.MediaBox.width;
  h := APDFLib.MediaBox.height;

  x := Options.Margins.Left + ww;
  y := Options.Margins.Top + caph;

  rSt := ASelection.StartRow;
  FFixedRowsSave := 0;
  FFixedColumnsSave := -1;
  cSave := -1;
  cSt := -1;
  FFixedRowGet := False;
  FFixedColGet := False;
  FSetRSave := True;
  FSetCSave := True;

  pw := GetFixedPrintWidth;
  ph := GetFixedPrintHeight;

  FSaveXBounds := Options.Margins.Left + ww;

  for I := ASelection.StartCol to ASelection.EndCol - offc do
  begin
    if (CompareValue(Floor(FSaveXBounds + CurrentPlanner.GridControl.ColWidths[I] * sc), w - Options.Margins.Right) = GreaterThanValue) then
      Break;

    FSaveXBounds := FSaveXBounds + CurrentPlanner.GridControl.ColWidths[I] * sc;
  end;

  FSaveYBounds := Options.Margins.Top + caph + hh;

  for I := ASelection.StartRow to ASelection.EndRow - offr do
  begin
    if (CompareValue(Floor(FSaveYBounds + CurrentPlanner.GridControl.RowHeights[I] * sc), h - Options.Margins.Bottom) = GreaterThanValue) then
      Break;

    FSaveYBounds := FSaveYBounds + CurrentPlanner.GridControl.RowHeights[I] * sc;
  end;

  if CurrentPlanner.Caption.Visible then
  begin
    APDFLib.Graphics.Stroke.Color := gcNull;
    APDFLib.Graphics.Fill.Color := CurrentPlanner.Caption.Background;
    APDFLib.Graphics.Fill.ColorTo := CurrentPlanner.Caption.BackgroundTo;
    APDFLib.Graphics.Fill.Kind := gfkGradient;

    case APlanner.Caption.GradientDirection of
      gdHorizontal: APDFLib.Graphics.Fill.Orientation := gfoHorizontal;
      gdVertical: APDFLib.Graphics.Fill.Orientation := gfoVertical;
    end;
    APDFLib.Graphics.Fill.Kind := gfkGradient;

    tr := RectF(X - ww, Y - caph, Min(FSaveXBounds, X - ww + w - Options.Margins.Right - Options.Margins.Left), Y);
    APDFLib.Graphics.DrawRectangle(tr);
    if CurrentPlanner.Caption.Title <> '' then
    begin
      APDFLib.Graphics.Font.Name := CurrentPlanner.Caption.Font.Name;
      APDFLib.Graphics.Font.Size := CurrentPlanner.Caption.Font.Size * sc;
      APDFLib.Graphics.Font.Color := CurrentPlanner.Caption.Font.Color;
      APDFLib.Graphics.Font.Style := CurrentPlanner.Caption.Font.Style;

      if TAdvUtils.IsHTML(CurrentPlanner.Caption.Title) then
        APDFLib.Graphics.DrawHTMLText(CurrentPlanner.Caption.Title, tr)
      else
      begin
        dtr := APDFLib.Graphics.DrawText(CurrentPlanner.Caption.Title, tr, True);
        dtr.Width := dtr.Width + 1 * sc;
        dtr.Height := dtr.Height + 1 * sc;
        case CurrentPlanner.Caption.Alignment of
          taLeftJustify: tr := RectF(tr.Left, tr.Top + (tr.Height - dtr.Height) / 2, tr.Left + dtr.Width, tr.Top + (tr.Height - dtr.Height) / 2 + dtr.Height);
          taCenter: tr := RectF(tr.Left + (tr.Width - dtr.Width) / 2, tr.Top + (tr.Height - dtr.Height) / 2, tr.Left + (tr.Width - dtr.Width) / 2 + dtr.Width, tr.Top + (tr.Height - dtr.Height) / 2 + dtr.Height);
          taRightJustify: tr := RectF(tr.Right - dtr.Width, tr.Top + (tr.Height - dtr.Height) / 2, tr.Right, tr.Top + (tr.Height - dtr.Height) / 2 + dtr.Height);
        end;

        APDFLib.Graphics.DrawText(CurrentPlanner.Caption.Title, tr);
      end;
    end;
  end;

  c := ASelection.StartCol;

  FCSave := -1;
  FCSaveEx := c;
  while c <= ASelection.EndCol do
  begin
    if cSt = -1 then
      cSt := c;

    if (Options.RepeatSideBar and not IsHorizontal) or (Options.RepeatHeader and (offc > 0) and IsHorizontal) then
    begin
      if ((FFixedColumnsSave = 0) and not IsHorizontal) or ((FFixedColumnsSave < offc - 1) and IsHorizontal) then
      begin
        FFixedColGet := True;
        if FSetCSave then
        begin
          FSetCSave := False;
          FCSave := cSt;
        end;

        cst := FFixedColumnsSave;
        c := cst;
        Inc(FFixedColumnsSave);
      end
      else if FFixedColGet then
      begin
        FFixedColGet := False;
        cSt := FCSave;
        c := FCSave;

        if cst < offc then
        begin
          cst := offc;
          c := cst;
        end;
      end;
    end;

    if c < offc then
      cw := ww
    else
    begin
      if IsHorizontal and (FStartY = -1) then
        FStartY := X;

      cw := CurrentPlanner.GridControl.ColWidths[c - offc] * sc;
    end;

    r := rst;
    FRSaveEx := rst;
    FStartY := -1;
    while r <= ASelection.EndRow do
    begin
      if (Options.RepeatHeader and (offr > 0) and not IsHorizontal) or (Options.RepeatSideBar and IsHorizontal) then
      begin
        if ((FFixedRowsSave < offr) and not IsHorizontal) or ((FFixedRowsSave = 0) and IsHorizontal and not FFirstPage) then
        begin
          FFixedRowGet := True;
          if FSetRSave then
          begin
            FSetRSave := False;
            FRSave := rSt;
          end;

          rSt := FFixedRowsSave;
          r := rst;
          Inc(FFixedRowsSave);
        end
        else if FFixedRowGet then
        begin
          FFixedRowGet := False;
          rSt := FRSave;
          r := FRSave;

          if rst < offr then
          begin
            rst := offr;
            r := rst;
          end;
        end;
      end;

      bc := c;
      br := r;

      if r < offr then
        rh := hh
      else
      begin
        if not IsHorizontal and (FStartY = -1) then
          FStartY := Y;
        rh := CurrentPlanner.GridControl.RowHeights[r - offr] * sc;
      end;

      cwr := cw;
      rhr := rh;

      xts := x - ww;
      yts := y;
      xs := xts + cwr;
      ys := yts + rhr;

      if FSaveXBounds > -1 then
        xs := Min(FSaveXBounds, xs);

      if (FFirstPage or Options.RepeatSideBar) and (c > offc) then
        xts := Max(Options.Margins.Left + pw, xts)
      else
        xts := Max(Options.Margins.Left, xts);

      if FSaveYBounds > -1 then
        ys := Min(FSaveYBounds, ys);

      if (FFirstPage or Options.RepeatHeader) and (r > offr) then
        yts := Max(Options.Margins.Top + ph, yts)
      else
        yts := Max(Options.Margins.Top, yts);

      cellr := RectF(xts, yts, xs, ys);
      DrawCell(Self, APDFLib, CurrentPlanner, bc, br, offc, offr, st, cellr, True, True, sc);
      st := False;

      y := y + rh;

      if r < offr then
      begin
        if CompareValue(y + hh, h - Options.Margins.Bottom) = GreaterThanValue then
        begin
          FSaveYBounds := Y + hh;
          Break;
        end;
      end
      else
      begin
        if CompareValue(Floor(y + CurrentPlanner.GridControl.RowHeights[r - offr + 1] * sc), h - Options.Margins.Bottom) = GreaterThanValue then
        begin
          FSaveYBounds := Y + CurrentPlanner.GridControl.RowHeights[r - offr + 1] * sc;
          Break;
        end;
      end;

      Inc(r);
    end;

    if not IsHorizontal then
      DrawItems;

    if FFirstPage then
      y := Options.Margins.Top + caph
    else
      y := Options.Margins.Top;

    x := x + cw;
    Inc(c);
    st := True;
    FFixedRowsSave := 0;

    if (CompareValue(Floor(x - ww + CurrentPlanner.GridControl.ColWidths[c - offc] * sc), w - Options.Margins.Right) = GreaterThanValue) or (c = ASelection.EndCol + 1) then
    begin
      FSaveXBounds := x - ww + CurrentPlanner.GridControl.ColWidths[c - offc] * sc;

      rst := r + 1;

      if cSave = -1 then
        cSave := c;

      if r = ASelection.EndRow + 1 then
      begin
        c := cSave;
        cst := -1;
        rst := ASelection.StartRow;
        cSave := -1;
      end
      else
        c := cst;

      FFixedRowsSave := 0;
      if (c > ASelection.StartCol) and not IsHorizontal then
        FFixedColumnsSave := 0
      else
        FFixedColumnsSave := -1;

      FSetRSave := True;
      FSetCSave := True;

      if IsHorizontal then
        DrawItems;

      if (c < ASelection.EndCol + 1) then
      begin
        NewPage(APDFLib, CurrentPlanner);
        x := Options.Margins.Left + ww;
        y := Options.Margins.Top;
        FFirstPage := False;
      end;
    end;
  end;
end;

procedure TAdvCustomPlannerPDFIO.SetPlanner(const Value: TCustomPlanner);
begin
  inherited ExportObject := Value;
end;

procedure TAdvCustomPlannerPDFIO.SetOptions(const Value: TAdvPlannerPDFIOOptions);
begin
  Options.Assign(Value);
end;

{ TAdvPlannerPDFIOOptions }

procedure TAdvPlannerPDFIOOptions.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvPlannerPDFIOOptions then
  begin
    FFitToPage := (Source as TAdvPlannerPDFIOOptions).FitToPage;
    FRepeatHeader := (Source as TAdvPlannerPDFIOOptions).RepeatHeader;
    FRepeatSideBar := (Source as TAdvPlannerPDFIOOptions).RepeatSideBar;
  end;
end;

constructor TAdvPlannerPDFIOOptions.Create;
begin
  inherited;
  FFitToPage := True;
  FRepeatHeader := True;
  FRepeatSideBar := True;
end;

end.


