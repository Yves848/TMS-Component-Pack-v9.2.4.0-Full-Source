object FormMain: TFormMain
  Left = 601
  Top = 197
  Caption = 'CDS edit & search sample'
  ClientHeight = 394
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 101
    Top = 299
    Width = 35
    Height = 15
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 14
    Top = 331
    Width = 35
    Height = 15
    Caption = 'Name:'
  end
  object Label3: TLabel
    Left = 14
    Top = 361
    Width = 40
    Height = 15
    Caption = 'Capital:'
  end
  object GridCountry: TDBAdvGrid
    Left = 8
    Top = 40
    Width = 473
    Height = 249
    Cursor = crDefault
    ColCount = 6
    DefaultRowHeight = 23
    DrawingStyle = gdsClassic
    RowCount = 2
    FixedRows = 1
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing]
    ScrollBars = ssBoth
    TabOrder = 0
    HoverRowCells = [hcNormal, hcSelected]
    OnGetEditorType = GridCountryGetEditorType
    OnGetEditorProp = GridCountryGetEditorProp
    ActiveCellFont.Charset = DEFAULT_CHARSET
    ActiveCellFont.Color = clWindowText
    ActiveCellFont.Height = -12
    ActiveCellFont.Name = 'Segoe UI'
    ActiveCellFont.Style = [fsBold]
    Bands.Active = True
    CellNode.TreeColor = clSilver
    ControlLook.FixedGradientHoverFrom = clGray
    ControlLook.FixedGradientHoverTo = clWhite
    ControlLook.FixedGradientDownFrom = clGray
    ControlLook.FixedGradientDownTo = clSilver
    ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
    ControlLook.DropDownHeader.Font.Color = clWindowText
    ControlLook.DropDownHeader.Font.Height = -12
    ControlLook.DropDownHeader.Font.Name = 'Segoe UI'
    ControlLook.DropDownHeader.Font.Style = []
    ControlLook.DropDownHeader.Visible = True
    ControlLook.DropDownHeader.Buttons = <>
    ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
    ControlLook.DropDownFooter.Font.Color = clWindowText
    ControlLook.DropDownFooter.Font.Height = -12
    ControlLook.DropDownFooter.Font.Name = 'Segoe UI'
    ControlLook.DropDownFooter.Font.Style = []
    ControlLook.DropDownFooter.Visible = True
    ControlLook.DropDownFooter.Buttons = <>
    Filter = <>
    FilterDropDown.Font.Charset = DEFAULT_CHARSET
    FilterDropDown.Font.Color = clWindowText
    FilterDropDown.Font.Height = -12
    FilterDropDown.Font.Name = 'Segoe UI'
    FilterDropDown.Font.Style = []
    FilterDropDownClear = '(All)'
    FilterEdit.TypeNames.Strings = (
      'Starts with'
      'Ends with'
      'Contains'
      'Not contains'
      'Equal'
      'Not equal'
      'Larger than'
      'Smaller than'
      'Clear')
    FixedColWidth = 20
    FixedRowHeight = 23
    FixedFont.Charset = DEFAULT_CHARSET
    FixedFont.Color = clWindowText
    FixedFont.Height = -12
    FixedFont.Name = 'Segoe UI'
    FixedFont.Style = [fsBold]
    FloatFormat = '%.2f'
    HoverButtons.Buttons = <>
    HoverButtons.Position = hbLeftFromColumnLeft
    HTMLSettings.ImageFolder = 'images'
    HTMLSettings.ImageBaseName = 'img'
    Navigation.AppendOnArrowDown = True
    PrintSettings.DateFormat = 'dd/mm/yyyy'
    PrintSettings.Font.Charset = DEFAULT_CHARSET
    PrintSettings.Font.Color = clWindowText
    PrintSettings.Font.Height = -12
    PrintSettings.Font.Name = 'Segoe UI'
    PrintSettings.Font.Style = []
    PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
    PrintSettings.FixedFont.Color = clWindowText
    PrintSettings.FixedFont.Height = -12
    PrintSettings.FixedFont.Name = 'Segoe UI'
    PrintSettings.FixedFont.Style = []
    PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
    PrintSettings.HeaderFont.Color = clWindowText
    PrintSettings.HeaderFont.Height = -12
    PrintSettings.HeaderFont.Name = 'Segoe UI'
    PrintSettings.HeaderFont.Style = []
    PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
    PrintSettings.FooterFont.Color = clWindowText
    PrintSettings.FooterFont.Height = -12
    PrintSettings.FooterFont.Name = 'Segoe UI'
    PrintSettings.FooterFont.Style = []
    PrintSettings.PageNumSep = '/'
    ScrollWidth = 16
    SearchFooter.FindNextCaption = 'Find next'
    SearchFooter.FindPrevCaption = 'Find previous'
    SearchFooter.Font.Charset = DEFAULT_CHARSET
    SearchFooter.Font.Color = clWindowText
    SearchFooter.Font.Height = -12
    SearchFooter.Font.Name = 'Segoe UI'
    SearchFooter.Font.Style = []
    SearchFooter.HighLightCaption = 'Highlight'
    SearchFooter.HintClose = 'Close'
    SearchFooter.HintFindNext = 'Find next occurence'
    SearchFooter.HintFindPrev = 'Find previous occurence'
    SearchFooter.HintHighlight = 'Highlight occurences'
    SearchFooter.MatchCaseCaption = 'Match case'
    SearchFooter.ResultFormat = '(%d of %d)'
    SortSettings.DefaultFormat = ssAutomatic
    VAlignment = vtaCenter
    Version = '2.4.4.10'
    AutoCreateColumns = True
    AutoRemoveColumns = True
    Columns = <
      item
        Alignment = taCenter
        Borders = []
        BorderPen.Color = clSilver
        ButtonHeight = 18
        CheckFalse = 'N'
        CheckTrue = 'Y'
        Color = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -12
        HeaderFont.Name = 'Segoe UI'
        HeaderFont.Style = []
        PrintBorders = [cbTop, cbLeft, cbRight, cbBottom]
        PrintFont.Charset = DEFAULT_CHARSET
        PrintFont.Color = clWindowText
        PrintFont.Height = -12
        PrintFont.Name = 'Segoe UI'
        PrintFont.Style = []
        Width = 20
      end
      item
        Borders = []
        BorderPen.Color = clSilver
        ButtonHeight = 18
        CheckFalse = 'N'
        CheckTrue = 'Y'
        Color = clWindow
        FieldName = 'Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -12
        HeaderFont.Name = 'Segoe UI'
        HeaderFont.Style = [fsBold]
        PrintBorders = [cbTop, cbLeft, cbRight, cbBottom]
        PrintFont.Charset = DEFAULT_CHARSET
        PrintFont.Color = clWindowText
        PrintFont.Height = -12
        PrintFont.Name = 'Segoe UI'
        PrintFont.Style = []
        ShowBands = True
        Width = 80
      end
      item
        Borders = []
        BorderPen.Color = clSilver
        ButtonHeight = 18
        CheckFalse = 'N'
        CheckTrue = 'Y'
        Color = clWindow
        FieldName = 'Capital'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -12
        HeaderFont.Name = 'Segoe UI'
        HeaderFont.Style = [fsBold]
        PrintBorders = [cbTop, cbLeft, cbRight, cbBottom]
        PrintFont.Charset = DEFAULT_CHARSET
        PrintFont.Color = clWindowText
        PrintFont.Height = -12
        PrintFont.Name = 'Segoe UI'
        PrintFont.Style = []
        ShowBands = True
        Width = 92
      end
      item
        Borders = []
        BorderPen.Color = clSilver
        ButtonHeight = 18
        CheckFalse = 'N'
        CheckTrue = 'Y'
        Color = clWindow
        FieldName = 'Continent'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -12
        HeaderFont.Name = 'Segoe UI'
        HeaderFont.Style = []
        PrintBorders = [cbTop, cbLeft, cbRight, cbBottom]
        PrintFont.Charset = DEFAULT_CHARSET
        PrintFont.Color = clWindowText
        PrintFont.Height = -12
        PrintFont.Name = 'Segoe UI'
        PrintFont.Style = []
        ShowBands = True
        Width = 96
      end
      item
        Borders = []
        BorderPen.Color = clSilver
        ButtonHeight = 18
        CheckFalse = 'N'
        CheckTrue = 'Y'
        Color = clWindow
        FieldName = 'Area'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -12
        HeaderFont.Name = 'Segoe UI'
        HeaderFont.Style = []
        PrintBorders = [cbTop, cbLeft, cbRight, cbBottom]
        PrintFont.Charset = DEFAULT_CHARSET
        PrintFont.Color = clWindowText
        PrintFont.Height = -12
        PrintFont.Name = 'Segoe UI'
        PrintFont.Style = []
        ShowBands = True
        Width = 95
      end
      item
        Borders = []
        BorderPen.Color = clSilver
        ButtonHeight = 18
        CheckFalse = 'N'
        CheckTrue = 'Y'
        Color = clWindow
        FieldName = 'Population'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -12
        HeaderFont.Name = 'Segoe UI'
        HeaderFont.Style = []
        PrintBorders = [cbTop, cbLeft, cbRight, cbBottom]
        PrintFont.Charset = DEFAULT_CHARSET
        PrintFont.Color = clWindowText
        PrintFont.Height = -12
        PrintFont.Name = 'Segoe UI'
        PrintFont.Style = []
        ShowBands = True
        Width = 78
      end>
    DataSource = SrcCountry
    InvalidPicture.Data = {
      055449636F6E0000010001002020040000000000E80200001600000028000000
      2000000040000000010004000000000000020000000000000000000000000000
      0000000000000000000080000080000000808000800000008000800080800000
      80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
      FFFFFF000000000000777777777777000000000000000000777788FFFF887777
      000000000000007778F8887117788F877700000000000778F87111111111178F
      877000000000778871111111111999178877000000077F811111111111199999
      18F7700000778811111111111119999991887700007881111111111111119199
      99188700077F711111811111111198719997F7700788111118FF111111118FF7
      1991887077F71111888FF1111118FFFF19997F77778111118888FF1111888FF8
      911918777881111118888FF1188888811111188778811111118888FF88888811
      111117877F7111111118888888888111111117F77F7999111111888888881111
      111111F77F7999991111788888F71111111111F77F7999999917888888FF7111
      111117F778879999917FFF88888FF111111117877887999997FFFFF88888FF11
      11111887778799997FFFFFF798888FF11111187777F87997FFFFFF71178F88FF
      71117F7707887997FFFFF7999978F88871118870077F87997FFF799999978F87
      1117F77000788879978799999999787111188700007788879999999999999999
      1188770000077F88799999999999999778F77000000077888879999999999778
      8877000000000778F88877799777788F877000000000007778F8888878888F87
      7700000000000000777788FFFF88777700000000000000000077777777777700
      00000000FFC003FFFF0000FFFC00003FF800001FF000000FE0000007C0000003
      C000000380000001800000010000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000080000001
      80000001C0000003C0000003E0000007F000000FF800001FFC00003FFF0000FF
      FFC003FF}
    ShowUnicode = False
    ColWidths = (
      20
      80
      92
      96
      95
      78)
    RowHeights = (
      23
      23)
  end
  object DBNavigatorCountry: TDBNavigator
    Left = 8
    Top = 8
    Width = 240
    Height = 25
    DataSource = SrcCountry
    TabOrder = 1
  end
  object CheckBoxAdvanceOnEnter: TCheckBox
    Left = 497
    Top = 80
    Width = 125
    Height = 17
    Caption = 'AdvanceOnEnter'
    TabOrder = 2
    OnClick = CheckBoxAdvanceOnEnterClick
  end
  object CheckBoxAdvanceInsert: TCheckBox
    Left = 497
    Top = 112
    Width = 125
    Height = 17
    Caption = 'AdvanceInsert'
    TabOrder = 3
    OnClick = CheckBoxAdvanceInsertClick
  end
  object CheckBoxAllowEditing: TCheckBox
    Left = 497
    Top = 48
    Width = 125
    Height = 17
    Caption = 'Allow Editing'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckBoxAllowEditingClick
  end
  object CheckBoxAllowInsertRow: TCheckBox
    Left = 497
    Top = 144
    Width = 125
    Height = 17
    Caption = 'AllowInsertRow'
    TabOrder = 5
    OnClick = CheckBoxAllowInsertRowClick
  end
  object CheckBoxAllowDeleteRow: TCheckBox
    Left = 497
    Top = 176
    Width = 125
    Height = 17
    Caption = 'AllowDeleteRow'
    TabOrder = 6
    OnClick = CheckBoxAllowDeleteRowClick
  end
  object BtnCopy: TButton
    Left = 344
    Top = 8
    Width = 49
    Height = 25
    Caption = 'Copy'
    TabOrder = 7
    OnClick = BtnCopyClick
  end
  object BtnPaste: TButton
    Left = 400
    Top = 8
    Width = 49
    Height = 25
    Caption = 'Paste'
    TabOrder = 8
    OnClick = BtnPasteClick
  end
  object Button3: TButton
    Left = 8
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Search '
    TabOrder = 9
    OnClick = Button3Click
  end
  object EditSearch: TEdit
    Left = 144
    Top = 296
    Width = 121
    Height = 23
    TabOrder = 10
  end
  object BtnPreview: TButton
    Left = 456
    Top = 8
    Width = 57
    Height = 25
    Caption = 'Preview'
    TabOrder = 11
    OnClick = BtnPreviewClick
  end
  object EditName: TDBEdit
    Left = 64
    Top = 328
    Width = 121
    Height = 23
    DataField = 'Name'
    DataSource = SrcCountry
    TabOrder = 12
  end
  object EditCapital: TDBEdit
    Left = 64
    Top = 360
    Width = 121
    Height = 23
    DataField = 'Capital'
    DataSource = SrcCountry
    TabOrder = 13
  end
  object BtnOpenClose: TButton
    Left = 520
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Close dataset'
    TabOrder = 14
    OnClick = BtnOpenCloseClick
  end
  object SrcCountry: TDataSource
    DataSet = CdsCountry
    Left = 280
    Top = 296
  end
  object AdvPreviewDialog1: TAdvPreviewDialog
    CloseAfterPrint = False
    DialogCaption = 'Preview'
    DialogPrevBtn = 'Previous'
    DialogNextBtn = 'Next'
    DialogPrintBtn = 'Print'
    DialogCloseBtn = 'Close'
    Grid = GridCountry
    PreviewFast = False
    PreviewWidth = 350
    PreviewHeight = 300
    PreviewLeft = 100
    PreviewTop = 100
    PreviewCenter = False
    Left = 232
    Top = 344
  end
  object CdsCountry: TClientDataSet
    Aggregates = <>
    FileName = 'country.xml'
    Params = <>
    Left = 360
    Top = 296
    object CdsCountryName: TStringField
      FieldName = 'Name'
      Size = 24
    end
    object CdsCountryCapital: TStringField
      FieldName = 'Capital'
      Size = 24
    end
    object CdsCountryContinent: TStringField
      FieldName = 'Continent'
      Size = 24
    end
    object CdsCountryArea: TFloatField
      FieldName = 'Area'
    end
    object CdsCountryPopulation: TFloatField
      FieldName = 'Population'
    end
  end
  object test: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 488
    Top = 336
  end
end
