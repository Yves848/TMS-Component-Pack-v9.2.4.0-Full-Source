object Form93: TForm93
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'AdvSmoothCalendar Demo'
  ClientHeight = 436
  ClientWidth = 594
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001002020000001002000A81000001600000028000000200000004000
    0000010020000000000000100000640000006400000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    00002A2A2A061F1F24311F1F24311F1F24311F1F24311F1F24311F1F24311F1F
    24311F1F24311F1F24311F1F24311F1F24311F1F24311F1F24311F1F24311F1F
    24311F1F24311F1F24311F1F24311F1F24311F1F24311F1F24311F1F24311F1F
    24311F1F24311F1F24311F1F24311F1F24311F1F24312121262E000000000000
    00001E1E1E21201F23FF201F23FF201F23FF201F23FF201F23FF201F23FF201F
    23FF201F23FF201F23FF201F23FF201F23FF201F23FF201F23FF201F23FF201F
    23FF201F23FF201F23FF201F23FF201F23FF201F23FF201F23FF201F23FF201F
    23FF201F23FF201F23FF201F23FF201F23FF201F23FF232225EF000000000000
    00001C1C1C09211E2144211E2144211E2144211E2144211E2144211E2144211E
    2144211E2144211E2144211E2144211E2144211E2144211E2144211E2144211E
    2144211E2144211E2144211E2144211E2144211E2144211E2144211E2144211E
    2144211E2144211E2144211E2144211E2144212124A9232225EF000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000002321268A232225EF000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000002321268A232225EF000000000000
    00000000000000000001242024462320256D242424310000000026262C282525
    285825252A36000000000000000027272B402525285827272720000000000000
    000028282C4B2525285826262614000000003F3F3F04222226422121256C2222
    246122222E160000000000000000000000002321268A232225EF000000000000
    00000000000021212492201F23FF201F23FF201F23FE2323274123202574201F
    23FF201E239D0000000000000000212123B9201F23FF201E235D000000000000
    0000212025DB201F23FF1E1E223B22222734212023DF201F23FF201F23FF201F
    23FF212023EF2323283200000000000000002323264F24222884000000000000
    00003333330F201F23FC201F23FF21212579212125963333330523202574201F
    23FF201E239D0000000000000000212123B9201F23FF201E235D000000000000
    0000212025DB201F23FF1E1E223B2A2A2A12202024C921212469252525292222
    2575201F23FF212124A20000000000000000D4AA0006F0B40F11000000000000
    00001E1E2621201F23FF212024F500000000000000000000000023202574201F
    23FF201E239D0000000000000000212123B9201F23FF201E235D000000000000
    0000212025DB201F23FF1E1E223B00000000000000002626261422222468211F
    23D8201F23FF2120259E00000000E6A90015EFAE01E1EEAE00FCEEAC025D0000
    00001E1E1E21201F23FF212024F500000000000000000000000023202574201F
    23FF2020239F0000000000000000211F23B9201F23FF2020235E000000000000
    0000212024DB201F23FF1E1E223B0000000024212663201F23F5201F23FF201F
    23FF202024C3271F272000000000ECAE0346EFAE00FFEFAE00FFEEAD00A90000
    00001E1E1E21201F23FF212024F500000000000000000000000023202574201F
    23FF202024CB0000000100000000212023CF201F23FF2121248B000000002A2A
    2A06201F24F0201F23FF2520253755555503201F23F6201F23FC232125822727
    2727000000010000000000000000DF9F0008EEAE01AEEFAE01D3EDAB003A0000
    00002422267D201F23FF201F23F92424286A2626294F0000000023202574201F
    23FF201F23FF202024C2211F24AA201F23FF201F22F8201F23FC211F24A8211F
    23C1201F23FF201F23F42727270D00000001201F23EA201F23F8211F23722220
    257C212024D4271F272000000000000000000000000000000000000000000000
    0000201F23FF201F23FF201F23FF201F23FF222125C00000000023202574201F
    23FF1F1F23C8201F23E4201F23FF201F23ED2520253E21202596201F23FC201F
    23FF201F23F32222255800000000000000002222254A201F23EA201F23FF201F
    23FF212023DF2424283F00000000000000000000000000000000000000000000
    00001E1E2333201F23FF212024F6242424151F1F1F10000000001919190A2424
    24152727270D1F1F3F08251F25302727270D000000000000000022222E162424
    24312A2A2A0C000000000000000000000000000000002A2A2A062121272D2626
    2628000000020000000000000000000000000000000000000000000000000000
    00001E1E1E21201F23FF212024F5000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    00001919330A2323264F2424284C000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    594
    436)
  PixelsPerInch = 96
  TextHeight = 13
  object Label9: TLabel
    Left = 11
    Top = 83
    Width = 28
    Height = 13
    Caption = 'Style:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 320
    Top = 88
    Width = 53
    Height = 13
    Caption = 'Start date:'
  end
  object Label2: TLabel
    Left = 458
    Top = 88
    Width = 47
    Height = 13
    Caption = 'End date:'
  end
  object Label3: TLabel
    Left = 180
    Top = 415
    Width = 148
    Height = 13
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'https://www.tmssoftware.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label3Click
  end
  object Label4: TLabel
    Left = 11
    Top = 415
    Width = 163
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'For more information please visit: '
  end
  object AdvSmoothCalendar1: TAdvSmoothCalendar
    Left = 320
    Top = 107
    Width = 257
    Height = 249
    Year = 2012
    Month = 7
    Fill.ColorMirror = clNone
    Fill.ColorMirrorTo = clNone
    Fill.GradientType = gtTexture
    Fill.GradientMirrorType = gtSolid
    Fill.BackGroundPicturePosition = ppStretched
    Fill.BorderColor = clBlack
    Fill.BorderOpacity = 99
    Fill.Rounding = 0
    Fill.ShadowOffset = 0
    Fill.Glow = gmNone
    DateAppearance.DateFont.Charset = DEFAULT_CHARSET
    DateAppearance.DateFont.Color = clWindowText
    DateAppearance.DateFont.Height = -11
    DateAppearance.DateFont.Name = 'Tahoma'
    DateAppearance.DateFont.Style = [fsBold]
    DateAppearance.DateFill.Color = clWhite
    DateAppearance.DateFill.ColorTo = clWhite
    DateAppearance.DateFill.ColorMirror = clNone
    DateAppearance.DateFill.ColorMirrorTo = clNone
    DateAppearance.DateFill.GradientType = gtVertical
    DateAppearance.DateFill.GradientMirrorType = gtSolid
    DateAppearance.DateFill.Opacity = 102
    DateAppearance.DateFill.OpacityTo = 167
    DateAppearance.DateFill.BorderColor = 10066329
    DateAppearance.DateFill.BorderOpacity = 148
    DateAppearance.DateFill.Rounding = 0
    DateAppearance.DateFill.ShadowOffset = 0
    DateAppearance.DateFill.Glow = gmNone
    DateAppearance.DayOfWeekFont.Charset = DEFAULT_CHARSET
    DateAppearance.DayOfWeekFont.Color = clWindowText
    DateAppearance.DayOfWeekFont.Height = -16
    DateAppearance.DayOfWeekFont.Name = 'Tahoma'
    DateAppearance.DayOfWeekFont.Style = [fsBold]
    DateAppearance.DayOfWeekFill.Color = clWhite
    DateAppearance.DayOfWeekFill.ColorTo = clWhite
    DateAppearance.DayOfWeekFill.ColorMirror = clNone
    DateAppearance.DayOfWeekFill.ColorMirrorTo = clNone
    DateAppearance.DayOfWeekFill.GradientType = gtVertical
    DateAppearance.DayOfWeekFill.GradientMirrorType = gtSolid
    DateAppearance.DayOfWeekFill.Opacity = 128
    DateAppearance.DayOfWeekFill.OpacityTo = 191
    DateAppearance.DayOfWeekFill.BorderColor = clBlack
    DateAppearance.DayOfWeekFill.BorderOpacity = 71
    DateAppearance.DayOfWeekFill.Rounding = 0
    DateAppearance.DayOfWeekFill.ShadowOffset = 0
    DateAppearance.DayOfWeekFill.Glow = gmNone
    DateAppearance.SelectedDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.SelectedDateFont.Color = clWindowText
    DateAppearance.SelectedDateFont.Height = -11
    DateAppearance.SelectedDateFont.Name = 'Tahoma'
    DateAppearance.SelectedDateFont.Style = [fsBold]
    DateAppearance.SelectedDateFill.Color = 52377
    DateAppearance.SelectedDateFill.ColorTo = 52377
    DateAppearance.SelectedDateFill.ColorMirror = clNone
    DateAppearance.SelectedDateFill.ColorMirrorTo = clNone
    DateAppearance.SelectedDateFill.GradientType = gtVertical
    DateAppearance.SelectedDateFill.GradientMirrorType = gtSolid
    DateAppearance.SelectedDateFill.Opacity = 127
    DateAppearance.SelectedDateFill.OpacityTo = 190
    DateAppearance.SelectedDateFill.BorderColor = clGray
    DateAppearance.SelectedDateFill.Rounding = 0
    DateAppearance.SelectedDateFill.ShadowOffset = 0
    DateAppearance.SelectedDateFill.Glow = gmNone
    DateAppearance.CurrentDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.CurrentDateFont.Color = clWindowText
    DateAppearance.CurrentDateFont.Height = -12
    DateAppearance.CurrentDateFont.Name = 'Tahoma'
    DateAppearance.CurrentDateFont.Style = [fsBold]
    DateAppearance.CurrentDateFill.Color = 16763904
    DateAppearance.CurrentDateFill.ColorTo = 16764057
    DateAppearance.CurrentDateFill.ColorMirror = clNone
    DateAppearance.CurrentDateFill.ColorMirrorTo = clNone
    DateAppearance.CurrentDateFill.GradientType = gtVertical
    DateAppearance.CurrentDateFill.GradientMirrorType = gtSolid
    DateAppearance.CurrentDateFill.Opacity = 127
    DateAppearance.CurrentDateFill.OpacityTo = 192
    DateAppearance.CurrentDateFill.BorderColor = clNavy
    DateAppearance.CurrentDateFill.BorderOpacity = 217
    DateAppearance.CurrentDateFill.Rounding = 0
    DateAppearance.CurrentDateFill.ShadowOffset = 0
    DateAppearance.CurrentDateFill.Glow = gmNone
    DateAppearance.WeekendFill.Color = clWhite
    DateAppearance.WeekendFill.ColorTo = clWhite
    DateAppearance.WeekendFill.ColorMirror = clNone
    DateAppearance.WeekendFill.ColorMirrorTo = clNone
    DateAppearance.WeekendFill.GradientType = gtVertical
    DateAppearance.WeekendFill.GradientMirrorType = gtSolid
    DateAppearance.WeekendFill.Opacity = 102
    DateAppearance.WeekendFill.OpacityTo = 167
    DateAppearance.WeekendFill.BorderColor = 10066329
    DateAppearance.WeekendFill.BorderOpacity = 148
    DateAppearance.WeekendFill.Rounding = 0
    DateAppearance.WeekendFill.ShadowOffset = 0
    DateAppearance.WeekendFill.Glow = gmNone
    DateAppearance.WeekendFont.Charset = DEFAULT_CHARSET
    DateAppearance.WeekendFont.Color = clWindowText
    DateAppearance.WeekendFont.Height = -11
    DateAppearance.WeekendFont.Name = 'Tahoma'
    DateAppearance.WeekendFont.Style = []
    DateAppearance.HoverDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.HoverDateFont.Color = clWindowText
    DateAppearance.HoverDateFont.Height = -11
    DateAppearance.HoverDateFont.Name = 'Tahoma'
    DateAppearance.HoverDateFont.Style = [fsBold]
    DateAppearance.HoverDateFill.Color = 16772829
    DateAppearance.HoverDateFill.ColorTo = 16764057
    DateAppearance.HoverDateFill.ColorMirror = clNone
    DateAppearance.HoverDateFill.ColorMirrorTo = clNone
    DateAppearance.HoverDateFill.GradientType = gtVertical
    DateAppearance.HoverDateFill.GradientMirrorType = gtSolid
    DateAppearance.HoverDateFill.Opacity = 128
    DateAppearance.HoverDateFill.OpacityTo = 191
    DateAppearance.HoverDateFill.BorderColor = clWhite
    DateAppearance.HoverDateFill.Rounding = 0
    DateAppearance.HoverDateFill.ShadowOffset = 0
    DateAppearance.HoverDateFill.Glow = gmNone
    DateAppearance.MonthDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.MonthDateFont.Color = clWindowText
    DateAppearance.MonthDateFont.Height = -11
    DateAppearance.MonthDateFont.Name = 'Tahoma'
    DateAppearance.MonthDateFont.Style = []
    DateAppearance.YearDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.YearDateFont.Color = clWindowText
    DateAppearance.YearDateFont.Height = -11
    DateAppearance.YearDateFont.Name = 'Tahoma'
    DateAppearance.YearDateFont.Style = []
    DateAppearance.WeekNumbers.Visible = True
    DateAppearance.WeekNumbers.Font.Charset = DEFAULT_CHARSET
    DateAppearance.WeekNumbers.Font.Color = clWindowText
    DateAppearance.WeekNumbers.Font.Height = -11
    DateAppearance.WeekNumbers.Font.Name = 'Tahoma'
    DateAppearance.WeekNumbers.Font.Style = [fsBold]
    DateAppearance.WeekNumbers.Fill.Color = clWhite
    DateAppearance.WeekNumbers.Fill.ColorTo = clWhite
    DateAppearance.WeekNumbers.Fill.ColorMirror = clNone
    DateAppearance.WeekNumbers.Fill.ColorMirrorTo = clNone
    DateAppearance.WeekNumbers.Fill.GradientType = gtHorizontal
    DateAppearance.WeekNumbers.Fill.GradientMirrorType = gtSolid
    DateAppearance.WeekNumbers.Fill.Opacity = 128
    DateAppearance.WeekNumbers.Fill.OpacityTo = 190
    DateAppearance.WeekNumbers.Fill.BorderColor = 10066329
    DateAppearance.WeekNumbers.Fill.BorderOpacity = 154
    DateAppearance.WeekNumbers.Fill.Rounding = 0
    DateAppearance.WeekNumbers.Fill.ShadowOffset = 0
    DateAppearance.WeekNumbers.Fill.Glow = gmNone
    DateAppearance.WeekNumbers.Width = 20
    DateAppearance.DisabledDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.DisabledDateFont.Color = clWindowText
    DateAppearance.DisabledDateFont.Height = -11
    DateAppearance.DisabledDateFont.Name = 'Tahoma'
    DateAppearance.DisabledDateFont.Style = []
    DateAppearance.DisabledDateFill.ColorMirror = clNone
    DateAppearance.DisabledDateFill.ColorMirrorTo = clNone
    DateAppearance.DisabledDateFill.GradientType = gtVertical
    DateAppearance.DisabledDateFill.GradientMirrorType = gtSolid
    DateAppearance.DisabledDateFill.BorderColor = clNone
    DateAppearance.DisabledDateFill.Rounding = 0
    DateAppearance.DisabledDateFill.ShadowOffset = 0
    DateAppearance.DisabledDateFill.Glow = gmNone
    DateAppearance.DateBeforeFill.ColorMirror = clNone
    DateAppearance.DateBeforeFill.ColorMirrorTo = clNone
    DateAppearance.DateBeforeFill.GradientType = gtVertical
    DateAppearance.DateBeforeFill.GradientMirrorType = gtSolid
    DateAppearance.DateBeforeFill.BorderColor = clNone
    DateAppearance.DateBeforeFill.Rounding = 0
    DateAppearance.DateBeforeFill.ShadowOffset = 0
    DateAppearance.DateBeforeFill.Glow = gmNone
    DateAppearance.DateAfterFill.ColorMirror = clNone
    DateAppearance.DateAfterFill.ColorMirrorTo = clNone
    DateAppearance.DateAfterFill.GradientType = gtVertical
    DateAppearance.DateAfterFill.GradientMirrorType = gtSolid
    DateAppearance.DateAfterFill.BorderColor = clNone
    DateAppearance.DateAfterFill.Rounding = 0
    DateAppearance.DateAfterFill.ShadowOffset = 0
    DateAppearance.DateAfterFill.Glow = gmNone
    DateAppearance.DateBeforeFont.Charset = DEFAULT_CHARSET
    DateAppearance.DateBeforeFont.Color = clWindowText
    DateAppearance.DateBeforeFont.Height = -11
    DateAppearance.DateBeforeFont.Name = 'Tahoma'
    DateAppearance.DateBeforeFont.Style = []
    DateAppearance.DateAfterFont.Charset = DEFAULT_CHARSET
    DateAppearance.DateAfterFont.Color = clWindowText
    DateAppearance.DateAfterFont.Height = -11
    DateAppearance.DateAfterFont.Name = 'Tahoma'
    DateAppearance.DateAfterFont.Style = []
    StatusAppearance.Fill.Color = 52377
    StatusAppearance.Fill.ColorTo = 52377
    StatusAppearance.Fill.ColorMirror = clNone
    StatusAppearance.Fill.ColorMirrorTo = clNone
    StatusAppearance.Fill.GradientType = gtVertical
    StatusAppearance.Fill.GradientMirrorType = gtSolid
    StatusAppearance.Fill.Opacity = 163
    StatusAppearance.Fill.OpacityTo = 95
    StatusAppearance.Fill.BorderColor = clBlack
    StatusAppearance.Fill.Rounding = 0
    StatusAppearance.Fill.ShadowOffset = 0
    StatusAppearance.Fill.Glow = gmNone
    StatusAppearance.Font.Charset = DEFAULT_CHARSET
    StatusAppearance.Font.Color = clWindowText
    StatusAppearance.Font.Height = -11
    StatusAppearance.Font.Name = 'Tahoma'
    StatusAppearance.Font.Style = []
    Header.Fill.ColorTo = clWhite
    Header.Fill.ColorMirror = clNone
    Header.Fill.ColorMirrorTo = clNone
    Header.Fill.GradientType = gtVertical
    Header.Fill.GradientMirrorType = gtSolid
    Header.Fill.Opacity = 123
    Header.Fill.OpacityTo = 89
    Header.Fill.BorderColor = 10066329
    Header.Fill.BorderOpacity = 151
    Header.Fill.Rounding = 0
    Header.Fill.ShadowOffset = 0
    Header.Fill.Glow = gmNone
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWhite
    Header.Font.Height = -16
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = [fsBold]
    Footer.Fill.Color = clWhite
    Footer.Fill.ColorTo = clSilver
    Footer.Fill.ColorMirror = clNone
    Footer.Fill.ColorMirrorTo = clNone
    Footer.Fill.GradientType = gtVertical
    Footer.Fill.GradientMirrorType = gtSolid
    Footer.Fill.Opacity = 86
    Footer.Fill.OpacityTo = 138
    Footer.Fill.BorderColor = 10066329
    Footer.Fill.BorderOpacity = 145
    Footer.Fill.Rounding = 0
    Footer.Fill.ShadowOffset = 0
    Footer.Fill.Glow = gmNone
    Footer.Font.Charset = DEFAULT_CHARSET
    Footer.Font.Color = clWindowText
    Footer.Font.Height = -13
    Footer.Font.Name = 'Tahoma'
    Footer.Font.Style = []
    Footer.CurrentDateCaption = False
    Version = '2.3.1.0'
    OnSelectDate = AdvSmoothCalendar1SelectDate
    OnSelectMultiDate = AdvSmoothCalendar1SelectMultiDate
    OnMonthChanged = AdvSmoothCalendar1MonthChanged
    OnDateStatus = AdvSmoothCalendar1DateStatus
    TabOrder = 0
    TMSStyle = 0
  end
  object AdvSmoothDatePicker1: TAdvSmoothDatePicker
    Left = 320
    Top = 362
    Width = 257
    Height = 21
    Flat = False
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clWindowText
    LabelFont.Height = -11
    LabelFont.Name = 'Tahoma'
    LabelFont.Style = []
    Lookup.Separator = ';'
    Color = clWindow
    Enabled = True
    ReadOnly = False
    TabOrder = 1
    Text = '01/01/2020'
    Visible = True
    Version = '2.4.0.0'
    ButtonStyle = bsDropDown
    ButtonWidth = 16
    Etched = False
    Glyph.Data = {
      DA020000424DDA0200000000000036000000280000000D0000000D0000000100
      200000000000A402000000000000000000000000000000000000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F00000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000000000000000000000000000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F0000000000000000000000000000000000000000000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F0000000000000000000000000000000
      0000000000000000000000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000}
    HideCalendarAfterSelection = True
    Calendar.Fill.ColorMirror = clNone
    Calendar.Fill.ColorMirrorTo = clNone
    Calendar.Fill.GradientType = gtVertical
    Calendar.Fill.GradientMirrorType = gtSolid
    Calendar.Fill.BorderColor = clNone
    Calendar.Fill.Rounding = 0
    Calendar.Fill.ShadowOffset = 0
    Calendar.Fill.Glow = gmNone
    Calendar.Animation = True
    Calendar.ShowCurrentDate = True
    Calendar.DateAppearance.DateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.DateFont.Color = clWindowText
    Calendar.DateAppearance.DateFont.Height = -11
    Calendar.DateAppearance.DateFont.Name = 'Tahoma'
    Calendar.DateAppearance.DateFont.Style = []
    Calendar.DateAppearance.DateFill.ColorMirror = clNone
    Calendar.DateAppearance.DateFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.DateFill.GradientType = gtVertical
    Calendar.DateAppearance.DateFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.DateFill.BorderColor = clNone
    Calendar.DateAppearance.DateFill.Rounding = 0
    Calendar.DateAppearance.DateFill.ShadowOffset = 0
    Calendar.DateAppearance.DateFill.Glow = gmNone
    Calendar.DateAppearance.DayOfWeekFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.DayOfWeekFont.Color = clWindowText
    Calendar.DateAppearance.DayOfWeekFont.Height = -11
    Calendar.DateAppearance.DayOfWeekFont.Name = 'Tahoma'
    Calendar.DateAppearance.DayOfWeekFont.Style = []
    Calendar.DateAppearance.DayOfWeekFill.ColorMirror = clNone
    Calendar.DateAppearance.DayOfWeekFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.DayOfWeekFill.GradientType = gtVertical
    Calendar.DateAppearance.DayOfWeekFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.DayOfWeekFill.BorderColor = clNone
    Calendar.DateAppearance.DayOfWeekFill.Rounding = 0
    Calendar.DateAppearance.DayOfWeekFill.ShadowOffset = 0
    Calendar.DateAppearance.DayOfWeekFill.Glow = gmNone
    Calendar.DateAppearance.SelectedDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.SelectedDateFont.Color = clWindowText
    Calendar.DateAppearance.SelectedDateFont.Height = -11
    Calendar.DateAppearance.SelectedDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.SelectedDateFont.Style = []
    Calendar.DateAppearance.SelectedDateFill.ColorMirror = clNone
    Calendar.DateAppearance.SelectedDateFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.SelectedDateFill.GradientType = gtVertical
    Calendar.DateAppearance.SelectedDateFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.SelectedDateFill.BorderColor = clNone
    Calendar.DateAppearance.SelectedDateFill.Rounding = 0
    Calendar.DateAppearance.SelectedDateFill.ShadowOffset = 0
    Calendar.DateAppearance.SelectedDateFill.Glow = gmNone
    Calendar.DateAppearance.CurrentDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.CurrentDateFont.Color = clWindowText
    Calendar.DateAppearance.CurrentDateFont.Height = -11
    Calendar.DateAppearance.CurrentDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.CurrentDateFont.Style = []
    Calendar.DateAppearance.CurrentDateFill.ColorMirror = clNone
    Calendar.DateAppearance.CurrentDateFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.CurrentDateFill.GradientType = gtVertical
    Calendar.DateAppearance.CurrentDateFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.CurrentDateFill.BorderColor = clNone
    Calendar.DateAppearance.CurrentDateFill.Rounding = 0
    Calendar.DateAppearance.CurrentDateFill.ShadowOffset = 0
    Calendar.DateAppearance.CurrentDateFill.Glow = gmNone
    Calendar.DateAppearance.WeekendFill.ColorMirror = clNone
    Calendar.DateAppearance.WeekendFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.WeekendFill.GradientType = gtVertical
    Calendar.DateAppearance.WeekendFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.WeekendFill.BorderColor = clNone
    Calendar.DateAppearance.WeekendFill.Rounding = 0
    Calendar.DateAppearance.WeekendFill.ShadowOffset = 0
    Calendar.DateAppearance.WeekendFill.Glow = gmNone
    Calendar.DateAppearance.WeekendFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.WeekendFont.Color = clWindowText
    Calendar.DateAppearance.WeekendFont.Height = -11
    Calendar.DateAppearance.WeekendFont.Name = 'Tahoma'
    Calendar.DateAppearance.WeekendFont.Style = []
    Calendar.DateAppearance.HoverDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.HoverDateFont.Color = clWindowText
    Calendar.DateAppearance.HoverDateFont.Height = -11
    Calendar.DateAppearance.HoverDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.HoverDateFont.Style = []
    Calendar.DateAppearance.HoverDateFill.ColorMirror = clNone
    Calendar.DateAppearance.HoverDateFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.HoverDateFill.GradientType = gtVertical
    Calendar.DateAppearance.HoverDateFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.HoverDateFill.BorderColor = clNone
    Calendar.DateAppearance.HoverDateFill.Rounding = 0
    Calendar.DateAppearance.HoverDateFill.ShadowOffset = 0
    Calendar.DateAppearance.HoverDateFill.Glow = gmNone
    Calendar.DateAppearance.MonthDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.MonthDateFont.Color = clWindowText
    Calendar.DateAppearance.MonthDateFont.Height = -11
    Calendar.DateAppearance.MonthDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.MonthDateFont.Style = []
    Calendar.DateAppearance.YearDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.YearDateFont.Color = clWindowText
    Calendar.DateAppearance.YearDateFont.Height = -11
    Calendar.DateAppearance.YearDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.YearDateFont.Style = []
    Calendar.DateAppearance.WeekNumbers.Font.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.WeekNumbers.Font.Color = clWindowText
    Calendar.DateAppearance.WeekNumbers.Font.Height = -11
    Calendar.DateAppearance.WeekNumbers.Font.Name = 'Tahoma'
    Calendar.DateAppearance.WeekNumbers.Font.Style = []
    Calendar.DateAppearance.WeekNumbers.Fill.ColorMirror = clNone
    Calendar.DateAppearance.WeekNumbers.Fill.ColorMirrorTo = clNone
    Calendar.DateAppearance.WeekNumbers.Fill.GradientType = gtVertical
    Calendar.DateAppearance.WeekNumbers.Fill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.WeekNumbers.Fill.BorderColor = clNone
    Calendar.DateAppearance.WeekNumbers.Fill.Rounding = 0
    Calendar.DateAppearance.WeekNumbers.Fill.ShadowOffset = 0
    Calendar.DateAppearance.WeekNumbers.Fill.Glow = gmNone
    Calendar.DateAppearance.DisabledDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.DisabledDateFont.Color = clWindowText
    Calendar.DateAppearance.DisabledDateFont.Height = -11
    Calendar.DateAppearance.DisabledDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.DisabledDateFont.Style = []
    Calendar.DateAppearance.DisabledDateFill.ColorMirror = clNone
    Calendar.DateAppearance.DisabledDateFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.DisabledDateFill.GradientType = gtVertical
    Calendar.DateAppearance.DisabledDateFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.DisabledDateFill.BorderColor = clNone
    Calendar.DateAppearance.DisabledDateFill.Rounding = 0
    Calendar.DateAppearance.DisabledDateFill.ShadowOffset = 0
    Calendar.DateAppearance.DisabledDateFill.Glow = gmNone
    Calendar.DateAppearance.DateBeforeFill.ColorMirror = clNone
    Calendar.DateAppearance.DateBeforeFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.DateBeforeFill.GradientType = gtVertical
    Calendar.DateAppearance.DateBeforeFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.DateBeforeFill.BorderColor = clNone
    Calendar.DateAppearance.DateBeforeFill.Rounding = 0
    Calendar.DateAppearance.DateBeforeFill.ShadowOffset = 0
    Calendar.DateAppearance.DateBeforeFill.Glow = gmNone
    Calendar.DateAppearance.DateAfterFill.ColorMirror = clNone
    Calendar.DateAppearance.DateAfterFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.DateAfterFill.GradientType = gtVertical
    Calendar.DateAppearance.DateAfterFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.DateAfterFill.BorderColor = clNone
    Calendar.DateAppearance.DateAfterFill.Rounding = 0
    Calendar.DateAppearance.DateAfterFill.ShadowOffset = 0
    Calendar.DateAppearance.DateAfterFill.Glow = gmNone
    Calendar.DateAppearance.DateBeforeFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.DateBeforeFont.Color = clWindowText
    Calendar.DateAppearance.DateBeforeFont.Height = -11
    Calendar.DateAppearance.DateBeforeFont.Name = 'Tahoma'
    Calendar.DateAppearance.DateBeforeFont.Style = []
    Calendar.DateAppearance.DateAfterFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.DateAfterFont.Color = clWindowText
    Calendar.DateAppearance.DateAfterFont.Height = -11
    Calendar.DateAppearance.DateAfterFont.Name = 'Tahoma'
    Calendar.DateAppearance.DateAfterFont.Style = []
    Calendar.StatusAppearance.Fill.ColorMirror = clNone
    Calendar.StatusAppearance.Fill.ColorMirrorTo = clNone
    Calendar.StatusAppearance.Fill.GradientType = gtVertical
    Calendar.StatusAppearance.Fill.GradientMirrorType = gtSolid
    Calendar.StatusAppearance.Fill.BorderColor = clNone
    Calendar.StatusAppearance.Fill.Rounding = 0
    Calendar.StatusAppearance.Fill.ShadowOffset = 0
    Calendar.StatusAppearance.Fill.Glow = gmNone
    Calendar.StatusAppearance.Font.Charset = DEFAULT_CHARSET
    Calendar.StatusAppearance.Font.Color = clWindowText
    Calendar.StatusAppearance.Font.Height = -11
    Calendar.StatusAppearance.Font.Name = 'Tahoma'
    Calendar.StatusAppearance.Font.Style = []
    Calendar.Footer.Fill.ColorMirror = clNone
    Calendar.Footer.Fill.ColorMirrorTo = clNone
    Calendar.Footer.Fill.GradientType = gtVertical
    Calendar.Footer.Fill.GradientMirrorType = gtSolid
    Calendar.Footer.Fill.BorderColor = clNone
    Calendar.Footer.Fill.Rounding = 0
    Calendar.Footer.Fill.ShadowOffset = 0
    Calendar.Footer.Fill.Glow = gmNone
    Calendar.Footer.Font.Charset = DEFAULT_CHARSET
    Calendar.Footer.Font.Color = clWindowText
    Calendar.Footer.Font.Height = -11
    Calendar.Footer.Font.Name = 'Tahoma'
    Calendar.Footer.Font.Style = []
    Calendar.Header.Fill.ColorMirror = clNone
    Calendar.Header.Fill.ColorMirrorTo = clNone
    Calendar.Header.Fill.GradientType = gtVertical
    Calendar.Header.Fill.GradientMirrorType = gtSolid
    Calendar.Header.Fill.BorderColor = clNone
    Calendar.Header.Fill.Rounding = 0
    Calendar.Header.Fill.ShadowOffset = 0
    Calendar.Header.Fill.Glow = gmNone
    Calendar.Header.Font.Charset = DEFAULT_CHARSET
    Calendar.Header.Font.Color = clWindowText
    Calendar.Header.Font.Height = -11
    Calendar.Header.Font.Name = 'Tahoma'
    Calendar.Header.Font.Style = []
    Calendar.Width = 257
    Calendar.Height = 249
    Calendar.ShowHint = False
    Date = 43831.000000000000000000
    TMSStyle = 0
  end
  object AdvSmoothDatePicker2: TAdvSmoothDatePicker
    Left = 11
    Top = 362
    Width = 257
    Height = 21
    Flat = False
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clWindowText
    LabelFont.Height = -11
    LabelFont.Name = 'Tahoma'
    LabelFont.Style = []
    Lookup.Separator = ';'
    Color = clWindow
    Enabled = True
    ReadOnly = False
    TabOrder = 2
    Text = '01/01/2020'
    Visible = True
    Version = '2.4.0.0'
    ButtonStyle = bsDropDown
    ButtonWidth = 16
    Etched = False
    Glyph.Data = {
      DA020000424DDA0200000000000036000000280000000D0000000D0000000100
      200000000000A402000000000000000000000000000000000000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F00000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000000000000000000000000000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F0000000000000000000000000000000000000000000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F0000000000000000000000000000000
      0000000000000000000000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000}
    HideCalendarAfterSelection = True
    Calendar.Fill.ColorMirror = clNone
    Calendar.Fill.ColorMirrorTo = clNone
    Calendar.Fill.GradientType = gtVertical
    Calendar.Fill.GradientMirrorType = gtSolid
    Calendar.Fill.BorderColor = clNone
    Calendar.Fill.Rounding = 0
    Calendar.Fill.ShadowOffset = 0
    Calendar.Fill.Glow = gmNone
    Calendar.Animation = True
    Calendar.ShowCurrentDate = True
    Calendar.DateAppearance.DateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.DateFont.Color = clWindowText
    Calendar.DateAppearance.DateFont.Height = -11
    Calendar.DateAppearance.DateFont.Name = 'Tahoma'
    Calendar.DateAppearance.DateFont.Style = []
    Calendar.DateAppearance.DateFill.ColorMirror = clNone
    Calendar.DateAppearance.DateFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.DateFill.GradientType = gtVertical
    Calendar.DateAppearance.DateFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.DateFill.BorderColor = clNone
    Calendar.DateAppearance.DateFill.Rounding = 0
    Calendar.DateAppearance.DateFill.ShadowOffset = 0
    Calendar.DateAppearance.DateFill.Glow = gmNone
    Calendar.DateAppearance.DayOfWeekFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.DayOfWeekFont.Color = clWindowText
    Calendar.DateAppearance.DayOfWeekFont.Height = -11
    Calendar.DateAppearance.DayOfWeekFont.Name = 'Tahoma'
    Calendar.DateAppearance.DayOfWeekFont.Style = []
    Calendar.DateAppearance.DayOfWeekFill.ColorMirror = clNone
    Calendar.DateAppearance.DayOfWeekFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.DayOfWeekFill.GradientType = gtVertical
    Calendar.DateAppearance.DayOfWeekFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.DayOfWeekFill.BorderColor = clNone
    Calendar.DateAppearance.DayOfWeekFill.Rounding = 0
    Calendar.DateAppearance.DayOfWeekFill.ShadowOffset = 0
    Calendar.DateAppearance.DayOfWeekFill.Glow = gmNone
    Calendar.DateAppearance.SelectedDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.SelectedDateFont.Color = clWindowText
    Calendar.DateAppearance.SelectedDateFont.Height = -11
    Calendar.DateAppearance.SelectedDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.SelectedDateFont.Style = []
    Calendar.DateAppearance.SelectedDateFill.ColorMirror = clNone
    Calendar.DateAppearance.SelectedDateFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.SelectedDateFill.GradientType = gtVertical
    Calendar.DateAppearance.SelectedDateFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.SelectedDateFill.BorderColor = clNone
    Calendar.DateAppearance.SelectedDateFill.Rounding = 0
    Calendar.DateAppearance.SelectedDateFill.ShadowOffset = 0
    Calendar.DateAppearance.SelectedDateFill.Glow = gmNone
    Calendar.DateAppearance.CurrentDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.CurrentDateFont.Color = clWindowText
    Calendar.DateAppearance.CurrentDateFont.Height = -11
    Calendar.DateAppearance.CurrentDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.CurrentDateFont.Style = []
    Calendar.DateAppearance.CurrentDateFill.ColorMirror = clNone
    Calendar.DateAppearance.CurrentDateFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.CurrentDateFill.GradientType = gtVertical
    Calendar.DateAppearance.CurrentDateFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.CurrentDateFill.BorderColor = clNone
    Calendar.DateAppearance.CurrentDateFill.Rounding = 0
    Calendar.DateAppearance.CurrentDateFill.ShadowOffset = 0
    Calendar.DateAppearance.CurrentDateFill.Glow = gmNone
    Calendar.DateAppearance.WeekendFill.ColorMirror = clNone
    Calendar.DateAppearance.WeekendFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.WeekendFill.GradientType = gtVertical
    Calendar.DateAppearance.WeekendFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.WeekendFill.BorderColor = clNone
    Calendar.DateAppearance.WeekendFill.Rounding = 0
    Calendar.DateAppearance.WeekendFill.ShadowOffset = 0
    Calendar.DateAppearance.WeekendFill.Glow = gmNone
    Calendar.DateAppearance.WeekendFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.WeekendFont.Color = clWindowText
    Calendar.DateAppearance.WeekendFont.Height = -11
    Calendar.DateAppearance.WeekendFont.Name = 'Tahoma'
    Calendar.DateAppearance.WeekendFont.Style = []
    Calendar.DateAppearance.HoverDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.HoverDateFont.Color = clWindowText
    Calendar.DateAppearance.HoverDateFont.Height = -11
    Calendar.DateAppearance.HoverDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.HoverDateFont.Style = []
    Calendar.DateAppearance.HoverDateFill.ColorMirror = clNone
    Calendar.DateAppearance.HoverDateFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.HoverDateFill.GradientType = gtVertical
    Calendar.DateAppearance.HoverDateFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.HoverDateFill.BorderColor = clNone
    Calendar.DateAppearance.HoverDateFill.Rounding = 0
    Calendar.DateAppearance.HoverDateFill.ShadowOffset = 0
    Calendar.DateAppearance.HoverDateFill.Glow = gmNone
    Calendar.DateAppearance.MonthDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.MonthDateFont.Color = clWindowText
    Calendar.DateAppearance.MonthDateFont.Height = -11
    Calendar.DateAppearance.MonthDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.MonthDateFont.Style = []
    Calendar.DateAppearance.YearDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.YearDateFont.Color = clWindowText
    Calendar.DateAppearance.YearDateFont.Height = -11
    Calendar.DateAppearance.YearDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.YearDateFont.Style = []
    Calendar.DateAppearance.WeekNumbers.Font.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.WeekNumbers.Font.Color = clWindowText
    Calendar.DateAppearance.WeekNumbers.Font.Height = -11
    Calendar.DateAppearance.WeekNumbers.Font.Name = 'Tahoma'
    Calendar.DateAppearance.WeekNumbers.Font.Style = []
    Calendar.DateAppearance.WeekNumbers.Fill.ColorMirror = clNone
    Calendar.DateAppearance.WeekNumbers.Fill.ColorMirrorTo = clNone
    Calendar.DateAppearance.WeekNumbers.Fill.GradientType = gtVertical
    Calendar.DateAppearance.WeekNumbers.Fill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.WeekNumbers.Fill.BorderColor = clNone
    Calendar.DateAppearance.WeekNumbers.Fill.Rounding = 0
    Calendar.DateAppearance.WeekNumbers.Fill.ShadowOffset = 0
    Calendar.DateAppearance.WeekNumbers.Fill.Glow = gmNone
    Calendar.DateAppearance.DisabledDateFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.DisabledDateFont.Color = clWindowText
    Calendar.DateAppearance.DisabledDateFont.Height = -11
    Calendar.DateAppearance.DisabledDateFont.Name = 'Tahoma'
    Calendar.DateAppearance.DisabledDateFont.Style = []
    Calendar.DateAppearance.DisabledDateFill.ColorMirror = clNone
    Calendar.DateAppearance.DisabledDateFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.DisabledDateFill.GradientType = gtVertical
    Calendar.DateAppearance.DisabledDateFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.DisabledDateFill.BorderColor = clNone
    Calendar.DateAppearance.DisabledDateFill.Rounding = 0
    Calendar.DateAppearance.DisabledDateFill.ShadowOffset = 0
    Calendar.DateAppearance.DisabledDateFill.Glow = gmNone
    Calendar.DateAppearance.DateBeforeFill.ColorMirror = clNone
    Calendar.DateAppearance.DateBeforeFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.DateBeforeFill.GradientType = gtVertical
    Calendar.DateAppearance.DateBeforeFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.DateBeforeFill.BorderColor = clNone
    Calendar.DateAppearance.DateBeforeFill.Rounding = 0
    Calendar.DateAppearance.DateBeforeFill.ShadowOffset = 0
    Calendar.DateAppearance.DateBeforeFill.Glow = gmNone
    Calendar.DateAppearance.DateAfterFill.ColorMirror = clNone
    Calendar.DateAppearance.DateAfterFill.ColorMirrorTo = clNone
    Calendar.DateAppearance.DateAfterFill.GradientType = gtVertical
    Calendar.DateAppearance.DateAfterFill.GradientMirrorType = gtSolid
    Calendar.DateAppearance.DateAfterFill.BorderColor = clNone
    Calendar.DateAppearance.DateAfterFill.Rounding = 0
    Calendar.DateAppearance.DateAfterFill.ShadowOffset = 0
    Calendar.DateAppearance.DateAfterFill.Glow = gmNone
    Calendar.DateAppearance.DateBeforeFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.DateBeforeFont.Color = clWindowText
    Calendar.DateAppearance.DateBeforeFont.Height = -11
    Calendar.DateAppearance.DateBeforeFont.Name = 'Tahoma'
    Calendar.DateAppearance.DateBeforeFont.Style = []
    Calendar.DateAppearance.DateAfterFont.Charset = DEFAULT_CHARSET
    Calendar.DateAppearance.DateAfterFont.Color = clWindowText
    Calendar.DateAppearance.DateAfterFont.Height = -11
    Calendar.DateAppearance.DateAfterFont.Name = 'Tahoma'
    Calendar.DateAppearance.DateAfterFont.Style = []
    Calendar.StatusAppearance.Fill.ColorMirror = clNone
    Calendar.StatusAppearance.Fill.ColorMirrorTo = clNone
    Calendar.StatusAppearance.Fill.GradientType = gtVertical
    Calendar.StatusAppearance.Fill.GradientMirrorType = gtSolid
    Calendar.StatusAppearance.Fill.BorderColor = clNone
    Calendar.StatusAppearance.Fill.Rounding = 0
    Calendar.StatusAppearance.Fill.ShadowOffset = 0
    Calendar.StatusAppearance.Fill.Glow = gmNone
    Calendar.StatusAppearance.Font.Charset = DEFAULT_CHARSET
    Calendar.StatusAppearance.Font.Color = clWindowText
    Calendar.StatusAppearance.Font.Height = -11
    Calendar.StatusAppearance.Font.Name = 'Tahoma'
    Calendar.StatusAppearance.Font.Style = []
    Calendar.Footer.Fill.ColorMirror = clNone
    Calendar.Footer.Fill.ColorMirrorTo = clNone
    Calendar.Footer.Fill.GradientType = gtVertical
    Calendar.Footer.Fill.GradientMirrorType = gtSolid
    Calendar.Footer.Fill.BorderColor = clNone
    Calendar.Footer.Fill.Rounding = 0
    Calendar.Footer.Fill.ShadowOffset = 0
    Calendar.Footer.Fill.Glow = gmNone
    Calendar.Footer.Font.Charset = DEFAULT_CHARSET
    Calendar.Footer.Font.Color = clWindowText
    Calendar.Footer.Font.Height = -11
    Calendar.Footer.Font.Name = 'Tahoma'
    Calendar.Footer.Font.Style = []
    Calendar.Header.Fill.ColorMirror = clNone
    Calendar.Header.Fill.ColorMirrorTo = clNone
    Calendar.Header.Fill.GradientType = gtVertical
    Calendar.Header.Fill.GradientMirrorType = gtSolid
    Calendar.Header.Fill.BorderColor = clNone
    Calendar.Header.Fill.Rounding = 0
    Calendar.Header.Fill.ShadowOffset = 0
    Calendar.Header.Fill.Glow = gmNone
    Calendar.Header.Font.Charset = DEFAULT_CHARSET
    Calendar.Header.Font.Color = clWindowText
    Calendar.Header.Font.Height = -11
    Calendar.Header.Font.Name = 'Tahoma'
    Calendar.Header.Font.Style = []
    Calendar.Width = 257
    Calendar.Height = 249
    Calendar.ShowHint = False
    Date = 43831.000000000000000000
    TMSStyle = 0
  end
  object ComboBox1: TComboBox
    Left = 75
    Top = 80
    Width = 193
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = ComboBox1Change
    Items.Strings = (
      'Windows 8')
  end
  object CheckBox1: TCheckBox
    Left = 320
    Top = 8
    Width = 129
    Height = 17
    Caption = 'Multiselect (with Shift)'
    TabOrder = 4
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 320
    Top = 31
    Width = 97
    Height = 17
    Caption = 'Animation'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox2Click
  end
  object AdvSmoothCalendar2: TAdvSmoothCalendar
    Left = 11
    Top = 107
    Width = 257
    Height = 249
    Year = 2012
    Month = 12
    Fill.Color = 15984090
    Fill.ColorTo = 15785680
    Fill.ColorMirror = clNone
    Fill.ColorMirrorTo = clNone
    Fill.GradientType = gtVertical
    Fill.GradientMirrorType = gtSolid
    Fill.BorderColor = clNone
    Fill.Rounding = 0
    Fill.ShadowOffset = 0
    Fill.Glow = gmNone
    DateAppearance.DateFont.Charset = DEFAULT_CHARSET
    DateAppearance.DateFont.Color = clWindowText
    DateAppearance.DateFont.Height = -11
    DateAppearance.DateFont.Name = 'Tahoma'
    DateAppearance.DateFont.Style = []
    DateAppearance.DateFill.Color = 16773091
    DateAppearance.DateFill.ColorTo = 16768452
    DateAppearance.DateFill.ColorMirror = 16765357
    DateAppearance.DateFill.ColorMirrorTo = 16767936
    DateAppearance.DateFill.GradientType = gtVertical
    DateAppearance.DateFill.GradientMirrorType = gtVertical
    DateAppearance.DateFill.BorderColor = clNone
    DateAppearance.DateFill.Rounding = 0
    DateAppearance.DateFill.ShadowOffset = 0
    DateAppearance.DateFill.Glow = gmNone
    DateAppearance.DayOfWeekFont.Charset = DEFAULT_CHARSET
    DateAppearance.DayOfWeekFont.Color = clWindowText
    DateAppearance.DayOfWeekFont.Height = -11
    DateAppearance.DayOfWeekFont.Name = 'Tahoma'
    DateAppearance.DayOfWeekFont.Style = []
    DateAppearance.DayOfWeekFill.Color = 15984090
    DateAppearance.DayOfWeekFill.ColorTo = 15785680
    DateAppearance.DayOfWeekFill.ColorMirror = clNone
    DateAppearance.DayOfWeekFill.ColorMirrorTo = clNone
    DateAppearance.DayOfWeekFill.GradientType = gtVertical
    DateAppearance.DayOfWeekFill.GradientMirrorType = gtSolid
    DateAppearance.DayOfWeekFill.BorderColor = clNone
    DateAppearance.DayOfWeekFill.Rounding = 0
    DateAppearance.DayOfWeekFill.ShadowOffset = 0
    DateAppearance.DayOfWeekFill.Glow = gmNone
    DateAppearance.SelectedDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.SelectedDateFont.Color = clWindowText
    DateAppearance.SelectedDateFont.Height = -11
    DateAppearance.SelectedDateFont.Name = 'Tahoma'
    DateAppearance.SelectedDateFont.Style = []
    DateAppearance.SelectedDateFill.Color = 11196927
    DateAppearance.SelectedDateFill.ColorTo = 7257087
    DateAppearance.SelectedDateFill.ColorMirror = 4370174
    DateAppearance.SelectedDateFill.ColorMirrorTo = 8053246
    DateAppearance.SelectedDateFill.GradientType = gtVertical
    DateAppearance.SelectedDateFill.GradientMirrorType = gtVertical
    DateAppearance.SelectedDateFill.BorderColor = 4370174
    DateAppearance.SelectedDateFill.Rounding = 0
    DateAppearance.SelectedDateFill.ShadowOffset = 0
    DateAppearance.SelectedDateFill.Glow = gmNone
    DateAppearance.CurrentDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.CurrentDateFont.Color = clWindowText
    DateAppearance.CurrentDateFont.Height = -11
    DateAppearance.CurrentDateFont.Name = 'Tahoma'
    DateAppearance.CurrentDateFont.Style = []
    DateAppearance.CurrentDateFill.Color = 7778289
    DateAppearance.CurrentDateFill.ColorTo = 4296947
    DateAppearance.CurrentDateFill.ColorMirror = 946929
    DateAppearance.CurrentDateFill.ColorMirrorTo = 5021693
    DateAppearance.CurrentDateFill.GradientType = gtVertical
    DateAppearance.CurrentDateFill.GradientMirrorType = gtVertical
    DateAppearance.CurrentDateFill.BorderColor = 4548219
    DateAppearance.CurrentDateFill.Rounding = 0
    DateAppearance.CurrentDateFill.ShadowOffset = 0
    DateAppearance.CurrentDateFill.Glow = gmNone
    DateAppearance.WeekendFill.Color = 16773091
    DateAppearance.WeekendFill.ColorTo = 16768452
    DateAppearance.WeekendFill.ColorMirror = 16765357
    DateAppearance.WeekendFill.ColorMirrorTo = 16767936
    DateAppearance.WeekendFill.GradientType = gtVertical
    DateAppearance.WeekendFill.GradientMirrorType = gtVertical
    DateAppearance.WeekendFill.BorderColor = clNone
    DateAppearance.WeekendFill.Rounding = 0
    DateAppearance.WeekendFill.ShadowOffset = 0
    DateAppearance.WeekendFill.Glow = gmNone
    DateAppearance.WeekendFont.Charset = DEFAULT_CHARSET
    DateAppearance.WeekendFont.Color = clWindowText
    DateAppearance.WeekendFont.Height = -11
    DateAppearance.WeekendFont.Name = 'Tahoma'
    DateAppearance.WeekendFont.Style = []
    DateAppearance.HoverDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.HoverDateFont.Color = clWindowText
    DateAppearance.HoverDateFont.Height = -11
    DateAppearance.HoverDateFont.Name = 'Tahoma'
    DateAppearance.HoverDateFont.Style = []
    DateAppearance.HoverDateFill.Color = 15465983
    DateAppearance.HoverDateFill.ColorTo = 11332863
    DateAppearance.HoverDateFill.ColorMirror = 5888767
    DateAppearance.HoverDateFill.ColorMirrorTo = 10807807
    DateAppearance.HoverDateFill.GradientType = gtVertical
    DateAppearance.HoverDateFill.GradientMirrorType = gtVertical
    DateAppearance.HoverDateFill.BorderColor = 10079963
    DateAppearance.HoverDateFill.Rounding = 0
    DateAppearance.HoverDateFill.ShadowOffset = 0
    DateAppearance.HoverDateFill.Glow = gmNone
    DateAppearance.MonthDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.MonthDateFont.Color = clWindowText
    DateAppearance.MonthDateFont.Height = -11
    DateAppearance.MonthDateFont.Name = 'Tahoma'
    DateAppearance.MonthDateFont.Style = []
    DateAppearance.YearDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.YearDateFont.Color = clWindowText
    DateAppearance.YearDateFont.Height = -11
    DateAppearance.YearDateFont.Name = 'Tahoma'
    DateAppearance.YearDateFont.Style = []
    DateAppearance.WeekNumbers.Font.Charset = DEFAULT_CHARSET
    DateAppearance.WeekNumbers.Font.Color = clWindowText
    DateAppearance.WeekNumbers.Font.Height = -11
    DateAppearance.WeekNumbers.Font.Name = 'Tahoma'
    DateAppearance.WeekNumbers.Font.Style = []
    DateAppearance.WeekNumbers.Fill.Color = 15984090
    DateAppearance.WeekNumbers.Fill.ColorTo = 15785680
    DateAppearance.WeekNumbers.Fill.ColorMirror = clNone
    DateAppearance.WeekNumbers.Fill.ColorMirrorTo = clNone
    DateAppearance.WeekNumbers.Fill.GradientType = gtVertical
    DateAppearance.WeekNumbers.Fill.GradientMirrorType = gtSolid
    DateAppearance.WeekNumbers.Fill.BorderColor = clNone
    DateAppearance.WeekNumbers.Fill.Rounding = 0
    DateAppearance.WeekNumbers.Fill.ShadowOffset = 0
    DateAppearance.WeekNumbers.Fill.Glow = gmNone
    DateAppearance.DisabledDateFont.Charset = DEFAULT_CHARSET
    DateAppearance.DisabledDateFont.Color = clGray
    DateAppearance.DisabledDateFont.Height = -11
    DateAppearance.DisabledDateFont.Name = 'Tahoma'
    DateAppearance.DisabledDateFont.Style = []
    DateAppearance.DisabledDateFill.Color = 15921906
    DateAppearance.DisabledDateFill.ColorTo = 11974326
    DateAppearance.DisabledDateFill.ColorMirror = 11974326
    DateAppearance.DisabledDateFill.ColorMirrorTo = 15921906
    DateAppearance.DisabledDateFill.GradientType = gtVertical
    DateAppearance.DisabledDateFill.GradientMirrorType = gtVertical
    DateAppearance.DisabledDateFill.BorderColor = clNone
    DateAppearance.DisabledDateFill.Rounding = 0
    DateAppearance.DisabledDateFill.ShadowOffset = 0
    DateAppearance.DisabledDateFill.Glow = gmNone
    DateAppearance.DateBeforeFill.ColorMirror = clNone
    DateAppearance.DateBeforeFill.ColorMirrorTo = clNone
    DateAppearance.DateBeforeFill.GradientType = gtVertical
    DateAppearance.DateBeforeFill.GradientMirrorType = gtSolid
    DateAppearance.DateBeforeFill.BorderColor = clNone
    DateAppearance.DateBeforeFill.Rounding = 0
    DateAppearance.DateBeforeFill.ShadowOffset = 0
    DateAppearance.DateBeforeFill.Glow = gmNone
    DateAppearance.DateAfterFill.ColorMirror = clNone
    DateAppearance.DateAfterFill.ColorMirrorTo = clNone
    DateAppearance.DateAfterFill.GradientType = gtVertical
    DateAppearance.DateAfterFill.GradientMirrorType = gtSolid
    DateAppearance.DateAfterFill.BorderColor = clNone
    DateAppearance.DateAfterFill.Rounding = 0
    DateAppearance.DateAfterFill.ShadowOffset = 0
    DateAppearance.DateAfterFill.Glow = gmNone
    DateAppearance.DateBeforeFont.Charset = DEFAULT_CHARSET
    DateAppearance.DateBeforeFont.Color = clWindowText
    DateAppearance.DateBeforeFont.Height = -11
    DateAppearance.DateBeforeFont.Name = 'Tahoma'
    DateAppearance.DateBeforeFont.Style = []
    DateAppearance.DateAfterFont.Charset = DEFAULT_CHARSET
    DateAppearance.DateAfterFont.Color = clWindowText
    DateAppearance.DateAfterFont.Height = -11
    DateAppearance.DateAfterFont.Name = 'Tahoma'
    DateAppearance.DateAfterFont.Style = []
    StatusAppearance.Fill.Color = clRed
    StatusAppearance.Fill.ColorMirror = clNone
    StatusAppearance.Fill.ColorMirrorTo = clNone
    StatusAppearance.Fill.GradientType = gtSolid
    StatusAppearance.Fill.GradientMirrorType = gtSolid
    StatusAppearance.Fill.BorderColor = clGray
    StatusAppearance.Fill.Rounding = 0
    StatusAppearance.Fill.ShadowOffset = 0
    StatusAppearance.Fill.Glow = gmNone
    StatusAppearance.Font.Charset = DEFAULT_CHARSET
    StatusAppearance.Font.Color = clWhite
    StatusAppearance.Font.Height = -11
    StatusAppearance.Font.Name = 'Tahoma'
    StatusAppearance.Font.Style = []
    Header.Fill.Color = 16773091
    Header.Fill.ColorTo = 16765615
    Header.Fill.ColorMirror = clNone
    Header.Fill.ColorMirrorTo = clNone
    Header.Fill.GradientType = gtVertical
    Header.Fill.GradientMirrorType = gtSolid
    Header.Fill.BorderColor = 16765615
    Header.Fill.Rounding = 0
    Header.Fill.ShadowOffset = 0
    Header.Fill.Glow = gmNone
    Header.ArrowColor = 7485192
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = 7485192
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Footer.Fill.Color = 16773091
    Footer.Fill.ColorTo = 16765615
    Footer.Fill.ColorMirror = clNone
    Footer.Fill.ColorMirrorTo = clNone
    Footer.Fill.GradientType = gtVertical
    Footer.Fill.GradientMirrorType = gtSolid
    Footer.Fill.BorderColor = 16765615
    Footer.Fill.Rounding = 0
    Footer.Fill.ShadowOffset = 0
    Footer.Fill.Glow = gmNone
    Footer.Font.Charset = DEFAULT_CHARSET
    Footer.Font.Color = 7485192
    Footer.Font.Height = -11
    Footer.Font.Name = 'Tahoma'
    Footer.Font.Style = []
    Version = '2.3.1.0'
    OnDateFill = AdvSmoothCalendar2DateFill
    OnDateStatus = AdvSmoothCalendar2DateStatus
    TabOrder = 6
    TMSStyle = 0
  end
  object CheckBox3: TCheckBox
    Left = 320
    Top = 54
    Width = 129
    Height = 17
    Caption = 'Darken Weekend'
    TabOrder = 7
    OnClick = CheckBox3Click
  end
end
