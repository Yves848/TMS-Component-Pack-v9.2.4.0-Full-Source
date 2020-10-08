object FormMain: TFormMain
  Left = 542
  Top = 221
  Caption = 'Clientdataset grid with lookup editors'
  ClientHeight = 452
  ClientWidth = 775
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 111
    Height = 15
    Caption = 'DBGrid - Employees'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 240
    Width = 133
    Height = 15
    Caption = 'DBAdvGrid - Employees'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 552
    Top = 8
    Width = 105
    Height = 15
    Caption = 'DBGrid - Countries'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 552
    Top = 168
    Width = 126
    Height = 15
    Caption = 'DBGrid - Departments'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object DBGridEmployees: TDBGrid
    Left = 8
    Top = 32
    Width = 513
    Height = 180
    DataSource = SrcEmployee
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'NAME'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'AGE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'COUNTRYNAME'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DEPARTMENT'
        Visible = True
      end>
  end
  object DBAdvGridEmployees: TDBAdvGrid
    Left = 8
    Top = 264
    Width = 513
    Height = 180
    Cursor = crDefault
    ColCount = 5
    DefaultRowHeight = 23
    DrawingStyle = gdsClassic
    RowCount = 4
    FixedRows = 1
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing]
    ScrollBars = ssBoth
    TabOrder = 1
    HoverRowCells = [hcNormal, hcSelected]
    ActiveCellFont.Charset = DEFAULT_CHARSET
    ActiveCellFont.Color = clWindowText
    ActiveCellFont.Height = -12
    ActiveCellFont.Name = 'Segoe UI'
    ActiveCellFont.Style = [fsBold]
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
    Version = '2.4.4.10'
    AutoCreateColumns = True
    AutoRemoveColumns = True
    Columns = <
      item
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
        EditLength = 20
        FieldName = 'NAME'
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
        Width = 116
      end
      item
        Borders = []
        BorderPen.Color = clSilver
        ButtonHeight = 18
        CheckFalse = 'N'
        CheckTrue = 'Y'
        Color = clWindow
        EditLength = 20
        FieldName = 'AGE'
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
        Width = 64
      end
      item
        Borders = []
        BorderPen.Color = clSilver
        ButtonHeight = 18
        CheckFalse = 'N'
        CheckTrue = 'Y'
        Color = clWindow
        EditLength = 20
        FieldName = 'COUNTRYNAME'
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
        Width = 127
      end
      item
        Borders = []
        BorderPen.Color = clSilver
        ButtonHeight = 18
        CheckFalse = 'N'
        CheckTrue = 'Y'
        Color = clWindow
        EditLength = 20
        FieldName = 'DEPARTMENT'
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
        Width = 125
      end>
    DataSource = SrcEmployee
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
      116
      64
      127
      125)
    RowHeights = (
      23
      23
      23
      23)
  end
  object DBGridCountries: TDBGrid
    Left = 552
    Top = 32
    Width = 217
    Height = 121
    DataSource = SrcCountry
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object DBGridDepartments: TDBGrid
    Left = 552
    Top = 192
    Width = 217
    Height = 113
    DataSource = SrcDepartment
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object SrcCountry: TDataSource
    DataSet = CdsCountry
    Left = 56
    Top = 72
  end
  object SrcDepartment: TDataSource
    DataSet = CdsDepartment
    Left = 128
    Top = 72
  end
  object SrcEmployee: TDataSource
    DataSet = CdsEmployee
    Left = 200
    Top = 72
  end
  object CdsCountry: TClientDataSet
    Aggregates = <>
    FileName = 'country.xml'
    Params = <>
    Left = 56
    Top = 144
    object CdsCountryID: TAutoIncField
      DisplayLabel = 'Id'
      DisplayWidth = 10
      FieldName = 'ID'
      ReadOnly = True
    end
    object CdsCountryCOUNTRYNAME: TStringField
      DisplayLabel = 'Countryname'
      DisplayWidth = 19
      FieldName = 'COUNTRYNAME'
      Size = 32
    end
  end
  object CdsDepartment: TClientDataSet
    Aggregates = <>
    FileName = 'department.xml'
    Params = <>
    Left = 128
    Top = 144
    object CdsDepartmentID: TAutoIncField
      DisplayLabel = 'Id'
      DisplayWidth = 10
      FieldName = 'ID'
      ReadOnly = True
    end
    object CdsDepartmentDEPARTMENT: TStringField
      DisplayLabel = 'Department'
      DisplayWidth = 19
      FieldName = 'DEPARTMENT'
      Size = 32
    end
  end
  object CdsEmployee: TClientDataSet
    Aggregates = <>
    FileName = 'employee.xml'
    Params = <>
    Left = 200
    Top = 144
    object CdsEmployeeNAME: TStringField
      DisplayLabel = 'Name'
      DisplayWidth = 19
      FieldName = 'NAME'
      Size = 64
    end
    object CdsEmployeeAGE: TIntegerField
      DisplayLabel = 'Age'
      DisplayWidth = 10
      FieldName = 'AGE'
    end
    object CdsEmployeeDEP: TIntegerField
      DisplayLabel = 'Dep'
      DisplayWidth = 10
      FieldName = 'DEP'
    end
    object CdsEmployeeCOUNTRY: TIntegerField
      DisplayLabel = 'Country'
      DisplayWidth = 10
      FieldName = 'COUNTRY'
    end
    object CdsEmployeeCOUNTRYNAME: TStringField
      DisplayLabel = 'Countryname'
      DisplayWidth = 20
      FieldKind = fkLookup
      FieldName = 'COUNTRYNAME'
      LookupDataSet = CdsCountry
      LookupKeyFields = 'ID'
      LookupResultField = 'COUNTRYNAME'
      KeyFields = 'COUNTRY'
      Lookup = True
    end
    object CdsEmployeeDEPARTMENT: TStringField
      DisplayLabel = 'Department'
      DisplayWidth = 20
      FieldKind = fkLookup
      FieldName = 'DEPARTMENT'
      LookupDataSet = CdsDepartment
      LookupKeyFields = 'ID'
      LookupResultField = 'DEPARTMENT'
      KeyFields = 'DEP'
      Lookup = True
    end
  end
end
