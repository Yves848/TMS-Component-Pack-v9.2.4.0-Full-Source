object Demo: TDemo
  Left = 308
  Top = 217
  BorderStyle = bsSingle
  Caption = 'TAdvStringGrid demo 4'
  ClientHeight = 526
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 651
    Height = 526
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Text'
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 641
        Height = 105
        Hint = 'Performs printing of the grid'
        Caption = 'Printing'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object Label7: TLabel
          Left = 8
          Top = 19
          Width = 26
          Height = 13
          Caption = 'Title :'
        end
        object Label8: TLabel
          Left = 383
          Top = 65
          Width = 36
          Height = 13
          Caption = 'Borders'
        end
        object CheckBox1: TCheckBox
          Left = 8
          Top = 64
          Width = 73
          Height = 17
          Caption = 'Print date'
          TabOrder = 0
        end
        object CheckBox2: TCheckBox
          Left = 8
          Top = 84
          Width = 73
          Height = 17
          Caption = 'Print time'
          TabOrder = 1
        end
        object CheckBox3: TCheckBox
          Left = 80
          Top = 64
          Width = 113
          Height = 17
          Caption = 'Print page numbers'
          TabOrder = 2
        end
        object CheckBox4: TCheckBox
          Left = 80
          Top = 84
          Width = 97
          Height = 17
          Caption = 'Print borders'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object Button8: TButton
          Left = 383
          Top = 16
          Width = 89
          Height = 25
          Caption = 'Print font'
          TabOrder = 4
          OnClick = Button8Click
        end
        object CheckBox5: TCheckBox
          Left = 200
          Top = 65
          Width = 97
          Height = 17
          Caption = 'Fit to page'
          TabOrder = 5
        end
        object CheckBox6: TCheckBox
          Left = 200
          Top = 84
          Width = 97
          Height = 17
          Caption = 'Print centered'
          Checked = True
          State = cbChecked
          TabOrder = 6
        end
        object RadioButton1: TRadioButton
          Left = 472
          Top = 81
          Width = 45
          Height = 17
          Caption = 'Vert.'
          TabOrder = 7
          OnClick = RadioButton1Click
        end
        object RadioButton2: TRadioButton
          Left = 424
          Top = 81
          Width = 46
          Height = 17
          Caption = 'Horz.'
          TabOrder = 8
          OnClick = RadioButton2Click
        end
        object RadioButton3: TRadioButton
          Left = 383
          Top = 81
          Width = 41
          Height = 17
          Caption = 'All'
          Checked = True
          TabOrder = 9
          TabStop = True
          OnClick = RadioButton3Click
        end
        object CheckBox7: TCheckBox
          Left = 296
          Top = 65
          Width = 73
          Height = 17
          Caption = 'Auto size'
          Checked = True
          State = cbChecked
          TabOrder = 10
        end
        object Memo1: TMemo
          Left = 40
          Top = 16
          Width = 337
          Height = 41
          Lines.Strings = (
            'This is the titletext'
            'for the demo of TAdvStringGrid')
          TabOrder = 11
        end
      end
      object GroupBox3: TGroupBox
        Left = 0
        Top = 108
        Width = 641
        Height = 85
        Hint = 'Controls sorting & sizing'
        Caption = 'Varia'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        object Label1: TLabel
          Left = 9
          Top = 14
          Width = 210
          Height = 13
          Caption = 'Press Insert / Delete to insert or delete a row'
        end
        object Label2: TLabel
          Left = 9
          Top = 29
          Width = 306
          Height = 13
          Caption = 
            'Press Enter after edit to select next cell (incl. jump over read' +
            '-only)'
        end
        object Button3: TButton
          Left = 492
          Top = 13
          Width = 60
          Height = 29
          Caption = 'Autosize'
          TabOrder = 0
          OnClick = Button3Click
        end
        object Button6: TButton
          Left = 564
          Top = 13
          Width = 60
          Height = 29
          Caption = 'Full sort'
          TabOrder = 1
          OnClick = Button6Click
        end
        object RadioGroup1: TRadioGroup
          Left = 328
          Top = 8
          Width = 121
          Height = 53
          Caption = 'Direction'
          ItemIndex = 0
          Items.Strings = (
            'Ascending'
            'Descending')
          TabOrder = 2
          OnClick = RadioGroup1Click
        end
        object fixedsort: TCheckBox
          Left = 328
          Top = 64
          Width = 121
          Height = 17
          Caption = 'Sort fixed columns'
          TabOrder = 3
          OnClick = fixedsortClick
        end
        object multiline: TCheckBox
          Left = 8
          Top = 64
          Width = 73
          Height = 17
          Caption = 'Multiline'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = multilineClick
        end
        object colro: TCheckBox
          Left = 8
          Top = 45
          Width = 105
          Height = 17
          Caption = 'Col 3 is read only'
          TabOrder = 5
        end
        object colfix: TCheckBox
          Left = 128
          Top = 45
          Width = 97
          Height = 17
          Caption = 'Col 3 is fixed'
          TabOrder = 6
          OnClick = colfixClick
        end
        object CheckBox9: TCheckBox
          Left = 128
          Top = 64
          Width = 97
          Height = 17
          Caption = 'Show full URL'
          TabOrder = 7
          OnClick = CheckBox9Click
        end
      end
      object AdvStringGrid1: TAdvStringGrid
        Left = 0
        Top = 197
        Width = 641
        Height = 276
        Cursor = crDefault
        DefaultRowHeight = 32
        DrawingStyle = gdsClassic
        RowCount = 5
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans serif'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing]
        ParentFont = False
        ParentShowHint = False
        ScrollBars = ssBoth
        ShowHint = True
        TabOrder = 2
        HoverRowCells = [hcNormal, hcSelected]
        OnGetCellColor = AdvStringGrid1GetCellColor
        OnGetCellPrintColor = AdvStringGrid1GetCellColor
        OnGetAlignment = AdvStringGrid1GetAlignment
        OnGetFormat = AdvStringGrid1GetFormat
        OnGridHint = AdvStringGrid1GridHint
        OnPrintPage = AdvStringGrid1PrintPage
        OnPrintStart = AdvStringGrid1PrintStart
        OnAutoInsertRow = AdvStringGrid1AutoInsertRow
        OnAutoDeleteRow = AdvStringGrid1AutoDeleteRow
        OnClickSort = AdvStringGrid1ClickSort
        OnCanSort = AdvStringGrid1CanSort
        OnCanEditCell = AdvStringGrid1CanEditCell
        OnIsFixedCell = AdvStringGrid1IsFixedCell
        HintColor = clLime
        ActiveCellFont.Charset = DEFAULT_CHARSET
        ActiveCellFont.Color = clWindowText
        ActiveCellFont.Height = -11
        ActiveCellFont.Name = 'MS Sans Serif'
        ActiveCellFont.Style = [fsBold]
        CellNode.ShowTree = False
        CellNode.TreeColor = clSilver
        ControlLook.FixedGradientHoverFrom = clGray
        ControlLook.FixedGradientHoverTo = clWhite
        ControlLook.FixedGradientDownFrom = clGray
        ControlLook.FixedGradientDownTo = clSilver
        ControlLook.ControlStyle = csClassic
        ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownHeader.Font.Color = clWindowText
        ControlLook.DropDownHeader.Font.Height = -11
        ControlLook.DropDownHeader.Font.Name = 'Tahoma'
        ControlLook.DropDownHeader.Font.Style = []
        ControlLook.DropDownHeader.Visible = True
        ControlLook.DropDownHeader.Buttons = <>
        ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownFooter.Font.Color = clWindowText
        ControlLook.DropDownFooter.Font.Height = -11
        ControlLook.DropDownFooter.Font.Name = 'Tahoma'
        ControlLook.DropDownFooter.Font.Style = []
        ControlLook.DropDownFooter.Visible = True
        ControlLook.DropDownFooter.Buttons = <>
        EnhTextSize = True
        EnhRowColMove = False
        Filter = <>
        FilterDropDown.Font.Charset = DEFAULT_CHARSET
        FilterDropDown.Font.Color = clWindowText
        FilterDropDown.Font.Height = -11
        FilterDropDown.Font.Name = 'Tahoma'
        FilterDropDown.Font.Style = []
        FilterDropDownClear = '(All)'
        FilterEdit.TypeNames.Strings = (
          'Starts with'
          'Ends with'
          'Contains'
          'Not contains'
          'Equal'
          'Not equal'
          'Clear')
        FixedRowHeight = 24
        FixedFont.Charset = DEFAULT_CHARSET
        FixedFont.Color = clWindowText
        FixedFont.Height = -11
        FixedFont.Name = 'MS Sans Serif'
        FixedFont.Style = []
        FloatFormat = '%.2f'
        GridImages = ImageList1
        HoverButtons.Buttons = <>
        HoverButtons.Position = hbLeftFromColumnLeft
        HTMLSettings.ImageFolder = 'images'
        HTMLSettings.ImageBaseName = 'img'
        IntelliPan = ipBoth
        Lookup = True
        LookupCaseSensitive = True
        LookupHistory = True
        MouseActions.CaretPositioning = True
        MouseActions.DirectEdit = True
        MouseActions.RowSelect = True
        Multilinecells = True
        Navigation.AllowInsertRow = True
        Navigation.AdvanceOnEnter = True
        Navigation.AdvanceInsert = True
        Navigation.AllowClipboardShortCuts = True
        Navigation.CursorWalkEditor = True
        PrintSettings.HeaderSize = 120
        PrintSettings.Time = ppBottomCenter
        PrintSettings.Date = ppTopCenter
        PrintSettings.DateFormat = 'dd/mm/yyyy'
        PrintSettings.Title = ppTopCenter
        PrintSettings.Font.Charset = DEFAULT_CHARSET
        PrintSettings.Font.Color = clWindowText
        PrintSettings.Font.Height = -11
        PrintSettings.Font.Name = 'Arial'
        PrintSettings.Font.Style = []
        PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
        PrintSettings.FixedFont.Color = clWindowText
        PrintSettings.FixedFont.Height = -11
        PrintSettings.FixedFont.Name = 'MS Sans Serif'
        PrintSettings.FixedFont.Style = []
        PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
        PrintSettings.HeaderFont.Color = clPurple
        PrintSettings.HeaderFont.Height = -19
        PrintSettings.HeaderFont.Name = 'MS Sans Serif'
        PrintSettings.HeaderFont.Style = [fsBold]
        PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
        PrintSettings.FooterFont.Color = clLime
        PrintSettings.FooterFont.Height = -11
        PrintSettings.FooterFont.Name = 'MS Sans Serif'
        PrintSettings.FooterFont.Style = [fsItalic]
        PrintSettings.ColumnSpacing = 10
        PrintSettings.Orientation = poLandscape
        PrintSettings.PagePrefix = 'page'
        PrintSettings.FixedWidth = 200
        PrintSettings.JobName = 'TAdvStringGrid demo'
        PrintSettings.PageNumSep = '/'
        ScrollType = ssFlat
        ScrollWidth = 16
        SearchFooter.FindNextCaption = 'Find next'
        SearchFooter.FindPrevCaption = 'Find previous'
        SearchFooter.Font.Charset = DEFAULT_CHARSET
        SearchFooter.Font.Color = clWindowText
        SearchFooter.Font.Height = -11
        SearchFooter.Font.Name = 'Tahoma'
        SearchFooter.Font.Style = []
        SearchFooter.HighLightCaption = 'Highlight'
        SearchFooter.HintClose = 'Close'
        SearchFooter.HintFindNext = 'Find next occurence'
        SearchFooter.HintFindPrev = 'Find previous occurence'
        SearchFooter.HintHighlight = 'Highlight occurences'
        SearchFooter.MatchCaseCaption = 'Match case'
        SelectionColor = clHighlight
        SelectionTextColor = clWhite
        SortSettings.DefaultFormat = ssAutomatic
        SortSettings.Show = True
        SortSettings.UpGlyph.Data = {
          72020000424D720200000000000036000000280000000E0000000D0000000100
          1800000000003C02000000000000000000000000000000000000C6C6C6C6C6C6
          C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6FF6363FF6363FF6363FF6363FF3131C6C6
          C6C6C6C60000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6FF9C00FF9C00FFFF
          FFFFFFFFFF6363FF6363FF3131C6C6C60000C6C6C6C6C6C6C6C6C6C6C6C6C6C6
          C6FF9C00FFCE63FFFFFFFF6300FF3131FF3131FF3131FF3131FF31310000C6C6
          C6C6C6C6C6C6C6C6C6C6FF9C00FFCE00FFCE00FF9C00FF6300C6C6C6C6C6C6C6
          C6C6630000FF63000000C6C6C6C6C6C6C6C6C6C6C6C6FF9C00FFCE00FF9C00FF
          9C00FF6300C6C6C6C6C6C6C6C6C6C6C6C66300000000C6C6C6C6C6C6C6C6C6FF
          9C00FFFF00FFCE00FF9C00FF6300C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6
          0000C6C6C6C6C6C6C6C6C6FF9C00FFFF00FFFF00FFCE00FF6300C6C6C6C6C6C6
          C6C6C6C6C6C6C6C6C6C6C6C60000000000949494949494949494FFFF00FFFF00
          FFFF00949494949494949494000000C6C6C6C6C6C6C6C6C60000C6C6C6000000
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00000000C6C6C6C6C6C6C6C6
          C6C6C6C60000C6C6C6C6C6C6000000FFFF00FFFF00FFFF00FFFF00FFFF000000
          00C6C6C6C6C6C6C6C6C6C6C6C6C6C6C60000C6C6C6C6C6C6C6C6C6000000FFFF
          00FFFF00FFFF00000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C60000C6C6
          C6C6C6C6C6C6C6C6C6C6000000FFFF00000000C6C6C6C6C6C6C6C6C6C6C6C6C6
          C6C6C6C6C6C6C6C60000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6000000C6C6C6C6
          C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C60000}
        SortSettings.DownGlyph.Data = {
          72020000424D720200000000000036000000280000000E0000000D0000000100
          1800000000003C02000000000000000000000000000000000000C6C6C6C6C6C6
          C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6000000C6C6C6C6C6C6C6C6C6C6C6
          C6C6C6C60000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6000000FFFF
          00000000C6C6C6C6C6C6C6C6C6C6C6C60000C6C6C6C6C6C6C6C6C6C6C6C6C6C6
          C6C6C6C6000000FFFF00FFFF00FFFF00000000C6C6C6C6C6C6C6C6C60000C6C6
          C6C6C6C6C6C6C6C6C6C6C6C6C6000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          0000C6C6C6C6C6C60000C6C6C6C6C6C6C6C6C6C6C6C6000000FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00000000C6C6C60000C6C6C6C6C6C6C6C6C600
          0000848484848484848484FFFF00FFFF00FFFF00FFFF00848484848484000000
          0000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6FF6300FFCE00FFFF00FFFF00
          FF9C00C6C6C6C6C6C6C6C6C60000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6
          FF6300FFCE00FF9C00FFCE00FF9C00C6C6C6C6C6C6C6C6C60000C6C6C6C6C6C6
          C6C6C6C6C6C6C6C6C6FF6300FF9C00FF9C00FF9C00FF6363FF9C00C6C6C6C6C6
          C6C6C6C60000C6C6C6C6C6C6C6C6C6C6C6C6FF3131CE6300FF6300FF9C00FFCE
          00FF6363FF9C00C6C6C6C6C6C6C6C6C60000FF3131FF3131FF3131FF3131FF31
          31FF3131FF6300FFFFFFFF6363FF6363C6C6C6C6C6C6C6C6C6C6C6C60000C6C6
          C6FF3131FF6363FF6363FF6363FFFFFFFFFFFFFF6363FF6363C6C6C6C6C6C6C6
          C6C6C6C6C6C6C6C60000C6C6C6C6C6C6FF3131FF6363FF6363FF6363FF6363C6
          C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C60000}
        URLShow = True
        URLEdit = True
        Version = '8.0.5.5'
        WordWrap = False
        ColWidths = (
          64
          64
          64
          64
          64)
        RowHeights = (
          24
          32
          32
          32
          32)
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Graphics'
      object Label3: TLabel
        Left = 0
        Top = 8
        Width = 254
        Height = 13
        Caption = 'Demo of how to add graphics && text to your stringgrid. '
      end
      object Label4: TLabel
        Left = 0
        Top = 24
        Width = 358
        Height = 13
        Caption = 
          'Control both horizontal and vertical alignment of text with resp' +
          'ect to graphics'
      end
      object Label5: TLabel
        Left = 0
        Top = 40
        Width = 189
        Height = 13
        Caption = 'Fixed rows at bottom of grid are allowed.'
      end
      object AdvStringGrid2: TAdvStringGrid
        Left = 0
        Top = 56
        Width = 641
        Height = 417
        Cursor = crDefault
        Hint = 'TAdvStringGrid with graphics'
        ColCount = 9
        DefaultRowHeight = 64
        DrawingStyle = gdsClassic
        RowCount = 7
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing]
        ParentFont = False
        ParentShowHint = False
        ScrollBars = ssBoth
        ShowHint = True
        TabOrder = 0
        HoverRowCells = [hcNormal, hcSelected]
        OnGetAlignment = AdvStringGrid2GetAlignment
        OnButtonClick = AdvStringGrid2ButtonClick
        HintColor = clYellow
        ActiveCellFont.Charset = DEFAULT_CHARSET
        ActiveCellFont.Color = clWindowText
        ActiveCellFont.Height = -11
        ActiveCellFont.Name = 'MS Sans Serif'
        ActiveCellFont.Style = [fsBold]
        CellNode.ShowTree = False
        CellNode.TreeColor = clSilver
        ControlLook.FixedGradientHoverFrom = clGray
        ControlLook.FixedGradientHoverTo = clWhite
        ControlLook.FixedGradientDownFrom = clGray
        ControlLook.FixedGradientDownTo = clSilver
        ControlLook.ControlStyle = csClassic
        ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownHeader.Font.Color = clWindowText
        ControlLook.DropDownHeader.Font.Height = -11
        ControlLook.DropDownHeader.Font.Name = 'Tahoma'
        ControlLook.DropDownHeader.Font.Style = []
        ControlLook.DropDownHeader.Visible = True
        ControlLook.DropDownHeader.Buttons = <>
        ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownFooter.Font.Color = clWindowText
        ControlLook.DropDownFooter.Font.Height = -11
        ControlLook.DropDownFooter.Font.Name = 'Tahoma'
        ControlLook.DropDownFooter.Font.Style = []
        ControlLook.DropDownFooter.Visible = True
        ControlLook.DropDownFooter.Buttons = <>
        ControlLook.NoDisabledButtonLook = True
        EnhRowColMove = False
        Filter = <>
        FilterDropDown.Font.Charset = DEFAULT_CHARSET
        FilterDropDown.Font.Color = clWindowText
        FilterDropDown.Font.Height = -11
        FilterDropDown.Font.Name = 'Tahoma'
        FilterDropDown.Font.Style = []
        FilterDropDownClear = '(All)'
        FilterEdit.TypeNames.Strings = (
          'Starts with'
          'Ends with'
          'Contains'
          'Not contains'
          'Equal'
          'Not equal'
          'Clear')
        FixedFooters = 1
        FixedRowHeight = 64
        FixedFont.Charset = DEFAULT_CHARSET
        FixedFont.Color = clWindowText
        FixedFont.Height = -11
        FixedFont.Name = 'Arial'
        FixedFont.Style = []
        FloatFormat = '%.2f'
        GridImages = ImageList1
        HoverButtons.Buttons = <>
        HoverButtons.Position = hbLeftFromColumnLeft
        HTMLSettings.ImageFolder = 'images'
        HTMLSettings.ImageBaseName = 'img'
        MouseActions.AllSelect = True
        MouseActions.ColSelect = True
        MouseActions.RowSelect = True
        Navigation.AllowInsertRow = True
        Navigation.AllowDeleteRow = True
        Navigation.AdvanceOnEnter = True
        Navigation.AdvanceInsert = True
        Navigation.AllowClipboardShortCuts = True
        PrintSettings.DateFormat = 'dd/mm/yyyy'
        PrintSettings.Font.Charset = ANSI_CHARSET
        PrintSettings.Font.Color = clWindowText
        PrintSettings.Font.Height = -11
        PrintSettings.Font.Name = 'Arial'
        PrintSettings.Font.Style = []
        PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
        PrintSettings.FixedFont.Color = clWindowText
        PrintSettings.FixedFont.Height = -11
        PrintSettings.FixedFont.Name = 'MS Sans Serif'
        PrintSettings.FixedFont.Style = []
        PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
        PrintSettings.HeaderFont.Color = clWindowText
        PrintSettings.HeaderFont.Height = -11
        PrintSettings.HeaderFont.Name = 'MS Sans Serif'
        PrintSettings.HeaderFont.Style = []
        PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
        PrintSettings.FooterFont.Color = clWindowText
        PrintSettings.FooterFont.Height = -11
        PrintSettings.FooterFont.Name = 'MS Sans Serif'
        PrintSettings.FooterFont.Style = []
        PrintSettings.PagePrefix = 'page'
        PrintSettings.PageNumSep = '/'
        PrintSettings.PrintGraphics = True
        ScrollWidth = 16
        SearchFooter.FindNextCaption = 'Find next'
        SearchFooter.FindPrevCaption = 'Find previous'
        SearchFooter.Font.Charset = DEFAULT_CHARSET
        SearchFooter.Font.Color = clWindowText
        SearchFooter.Font.Height = -11
        SearchFooter.Font.Name = 'Tahoma'
        SearchFooter.Font.Style = []
        SearchFooter.HighLightCaption = 'Highlight'
        SearchFooter.HintClose = 'Close'
        SearchFooter.HintFindNext = 'Find next occurence'
        SearchFooter.HintFindPrev = 'Find previous occurence'
        SearchFooter.HintHighlight = 'Highlight occurences'
        SearchFooter.MatchCaseCaption = 'Match case'
        SelectionColor = clHighlight
        SelectionTextColor = clWhite
        SortSettings.DefaultFormat = ssAutomatic
        Version = '8.0.5.5'
        WordWrap = False
        ColWidths = (
          64
          64
          64
          64
          64
          64
          64
          64
          64)
        RowHeights = (
          64
          64
          64
          64
          64
          64
          64)
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Editors'
      object Label6: TLabel
        Left = 0
        Top = 24
        Width = 344
        Height = 13
        Caption = 
          'Demo of inplace combobox, spineditor, datepicker, ellipseditor, ' +
          'checkbox'
      end
      object editgrid: TAdvStringGrid
        Left = 0
        Top = 42
        Width = 641
        Height = 433
        Cursor = crDefault
        BiDiMode = bdLeftToRight
        DrawingStyle = gdsClassic
        RowCount = 5
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
        ParentBiDiMode = False
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        HoverRowCells = [hcNormal, hcSelected]
        OnGetEditorType = editgridGetEditorType
        OnEllipsClick = editgridEllipsClick
        HintColor = clYellow
        ActiveCellFont.Charset = DEFAULT_CHARSET
        ActiveCellFont.Color = clWindowText
        ActiveCellFont.Height = -11
        ActiveCellFont.Name = 'MS Sans Serif'
        ActiveCellFont.Style = [fsBold]
        CellNode.ShowTree = False
        CellNode.TreeColor = clSilver
        ControlLook.FixedGradientHoverFrom = clGray
        ControlLook.FixedGradientHoverTo = clWhite
        ControlLook.FixedGradientDownFrom = clGray
        ControlLook.FixedGradientDownTo = clSilver
        ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownHeader.Font.Color = clWindowText
        ControlLook.DropDownHeader.Font.Height = -11
        ControlLook.DropDownHeader.Font.Name = 'Tahoma'
        ControlLook.DropDownHeader.Font.Style = []
        ControlLook.DropDownHeader.Visible = True
        ControlLook.DropDownHeader.Buttons = <>
        ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownFooter.Font.Color = clWindowText
        ControlLook.DropDownFooter.Font.Height = -11
        ControlLook.DropDownFooter.Font.Name = 'Tahoma'
        ControlLook.DropDownFooter.Font.Style = []
        ControlLook.DropDownFooter.Visible = True
        ControlLook.DropDownFooter.Buttons = <>
        EnhRowColMove = False
        Filter = <>
        FilterDropDown.Font.Charset = DEFAULT_CHARSET
        FilterDropDown.Font.Color = clWindowText
        FilterDropDown.Font.Height = -11
        FilterDropDown.Font.Name = 'Tahoma'
        FilterDropDown.Font.Style = []
        FilterDropDownClear = '(All)'
        FilterEdit.TypeNames.Strings = (
          'Starts with'
          'Ends with'
          'Contains'
          'Not contains'
          'Equal'
          'Not equal'
          'Clear')
        FixedRowHeight = 22
        FixedFont.Charset = DEFAULT_CHARSET
        FixedFont.Color = clWindowText
        FixedFont.Height = -11
        FixedFont.Name = 'MS Sans Serif'
        FixedFont.Style = []
        FloatFormat = '%.2f'
        HoverButtons.Buttons = <>
        HoverButtons.Position = hbLeftFromColumnLeft
        HTMLSettings.ImageFolder = 'images'
        HTMLSettings.ImageBaseName = 'img'
        MouseActions.DirectEdit = True
        Navigation.AdvanceOnEnter = True
        Navigation.AutoComboDropSize = True
        PrintSettings.DateFormat = 'dd/mm/yyyy'
        PrintSettings.Font.Charset = DEFAULT_CHARSET
        PrintSettings.Font.Color = clWindowText
        PrintSettings.Font.Height = -11
        PrintSettings.Font.Name = 'MS Sans Serif'
        PrintSettings.Font.Style = []
        PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
        PrintSettings.FixedFont.Color = clWindowText
        PrintSettings.FixedFont.Height = -11
        PrintSettings.FixedFont.Name = 'MS Sans Serif'
        PrintSettings.FixedFont.Style = []
        PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
        PrintSettings.HeaderFont.Color = clWindowText
        PrintSettings.HeaderFont.Height = -11
        PrintSettings.HeaderFont.Name = 'MS Sans Serif'
        PrintSettings.HeaderFont.Style = []
        PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
        PrintSettings.FooterFont.Color = clWindowText
        PrintSettings.FooterFont.Height = -11
        PrintSettings.FooterFont.Name = 'MS Sans Serif'
        PrintSettings.FooterFont.Style = []
        PrintSettings.Borders = pbNoborder
        PrintSettings.Centered = False
        PrintSettings.PagePrefix = 'page'
        PrintSettings.PageNumSep = '/'
        ScrollWidth = 16
        SearchFooter.FindNextCaption = 'Find next'
        SearchFooter.FindPrevCaption = 'Find previous'
        SearchFooter.Font.Charset = DEFAULT_CHARSET
        SearchFooter.Font.Color = clWindowText
        SearchFooter.Font.Height = -11
        SearchFooter.Font.Name = 'Tahoma'
        SearchFooter.Font.Style = []
        SearchFooter.HighLightCaption = 'Highlight'
        SearchFooter.HintClose = 'Close'
        SearchFooter.HintFindNext = 'Find next occurence'
        SearchFooter.HintFindPrev = 'Find previous occurence'
        SearchFooter.HintHighlight = 'Highlight occurences'
        SearchFooter.MatchCaseCaption = 'Match case'
        SelectionColor = clHighlight
        SelectionTextColor = clHighlightText
        SortSettings.DefaultFormat = ssAutomatic
        URLColor = clBlack
        Version = '8.0.5.5'
        WordWrap = False
        ColWidths = (
          64
          60
          64
          64
          64)
        RowHeights = (
          22
          22
          22
          22
          22)
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Nodes'
      object AdvStringGrid3: TAdvStringGrid
        Left = 0
        Top = 48
        Width = 641
        Height = 425
        Cursor = crDefault
        Color = clWhite
        DefaultRowHeight = 21
        DrawingStyle = gdsClassic
        RowCount = 5
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        HoverRowCells = [hcNormal, hcSelected]
        HintColor = clYellow
        ActiveCellFont.Charset = DEFAULT_CHARSET
        ActiveCellFont.Color = clWindowText
        ActiveCellFont.Height = -11
        ActiveCellFont.Name = 'MS Sans Serif'
        ActiveCellFont.Style = [fsBold]
        CellNode.NodeType = cnGlyph
        CellNode.NodeColor = clBlue
        CellNode.ExpandGlyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          080000000000000100000000000000000000000100000001000000000000009C
          9C0063CECE009CCEFF009CFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00050505050505
          0505050505050505050505050500000000000000000000000000050501010101
          0101010101010101000005050105030403040303030302010000050105040403
          0403040303030200010005010504040404040304030301000100010504040404
          0304040304020002020001050404040404040304030200020200010101010101
          0101010101010204020005010504040404040404040404040200050105040404
          0404040405050505020005010504040404040501010101010105050501050505
          0505010505050505050505050501010101010505050505050505050505050505
          0505050505050505050505050505050505050505050505050505}
        CellNode.ContractGlyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          0800000000000001000000000000000000000001000000010000000000008484
          8400009C9C0063CECE00F7F7F7009CCEFF009CFFFF00CEFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00080808080808
          0808080808080808080808080808080808080808080808080808080000000000
          0000000000000000000802030303030303030303030303030008020705060506
          0506050605050503000802070606060506050605060505030008020706060606
          0606050605060503000802070606060606050605060506030008020706060606
          0606060605060503000802070606060606060605060506030008020706060606
          0606060606060503000802070707070707070707070706030008020303030303
          0303020202020202080808020407070606020008080808080808080802020202
          0200080808080808080808080808080808080808080808080808}
        CellNode.TreeColor = clSilver
        ControlLook.FixedGradientHoverFrom = clGray
        ControlLook.FixedGradientHoverTo = clWhite
        ControlLook.FixedGradientDownFrom = clGray
        ControlLook.FixedGradientDownTo = clSilver
        ControlLook.ControlStyle = csClassic
        ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownHeader.Font.Color = clWindowText
        ControlLook.DropDownHeader.Font.Height = -11
        ControlLook.DropDownHeader.Font.Name = 'Tahoma'
        ControlLook.DropDownHeader.Font.Style = []
        ControlLook.DropDownHeader.Visible = True
        ControlLook.DropDownHeader.Buttons = <>
        ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownFooter.Font.Color = clWindowText
        ControlLook.DropDownFooter.Font.Height = -11
        ControlLook.DropDownFooter.Font.Name = 'Tahoma'
        ControlLook.DropDownFooter.Font.Style = []
        ControlLook.DropDownFooter.Visible = True
        ControlLook.DropDownFooter.Buttons = <>
        EnhRowColMove = False
        Filter = <>
        FilterDropDown.Font.Charset = DEFAULT_CHARSET
        FilterDropDown.Font.Color = clWindowText
        FilterDropDown.Font.Height = -11
        FilterDropDown.Font.Name = 'Tahoma'
        FilterDropDown.Font.Style = []
        FilterDropDownClear = '(All)'
        FilterEdit.TypeNames.Strings = (
          'Starts with'
          'Ends with'
          'Contains'
          'Not contains'
          'Equal'
          'Not equal'
          'Clear')
        FixedColWidth = 20
        FixedFont.Charset = DEFAULT_CHARSET
        FixedFont.Color = clWindowText
        FixedFont.Height = -11
        FixedFont.Name = 'MS Sans Serif'
        FixedFont.Style = []
        FloatFormat = '%.2f'
        HoverButtons.Buttons = <>
        HoverButtons.Position = hbLeftFromColumnLeft
        HTMLSettings.ImageFolder = 'images'
        HTMLSettings.ImageBaseName = 'img'
        PrintSettings.DateFormat = 'dd/mm/yyyy'
        PrintSettings.Font.Charset = DEFAULT_CHARSET
        PrintSettings.Font.Color = clWindowText
        PrintSettings.Font.Height = -11
        PrintSettings.Font.Name = 'MS Sans Serif'
        PrintSettings.Font.Style = []
        PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
        PrintSettings.FixedFont.Color = clWindowText
        PrintSettings.FixedFont.Height = -11
        PrintSettings.FixedFont.Name = 'MS Sans Serif'
        PrintSettings.FixedFont.Style = []
        PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
        PrintSettings.HeaderFont.Color = clWindowText
        PrintSettings.HeaderFont.Height = -11
        PrintSettings.HeaderFont.Name = 'MS Sans Serif'
        PrintSettings.HeaderFont.Style = []
        PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
        PrintSettings.FooterFont.Color = clWindowText
        PrintSettings.FooterFont.Height = -11
        PrintSettings.FooterFont.Name = 'MS Sans Serif'
        PrintSettings.FooterFont.Style = []
        PrintSettings.Borders = pbNoborder
        PrintSettings.Centered = False
        PrintSettings.PageNumSep = '/'
        ScrollWidth = 16
        SearchFooter.FindNextCaption = 'Find next'
        SearchFooter.FindPrevCaption = 'Find previous'
        SearchFooter.Font.Charset = DEFAULT_CHARSET
        SearchFooter.Font.Color = clWindowText
        SearchFooter.Font.Height = -11
        SearchFooter.Font.Name = 'Tahoma'
        SearchFooter.Font.Style = []
        SearchFooter.HighLightCaption = 'Highlight'
        SearchFooter.HintClose = 'Close'
        SearchFooter.HintFindNext = 'Find next occurence'
        SearchFooter.HintFindPrev = 'Find previous occurence'
        SearchFooter.HintHighlight = 'Highlight occurences'
        SearchFooter.MatchCaseCaption = 'Match case'
        SelectionColor = clHighlight
        SelectionTextColor = clHighlightText
        SortSettings.DefaultFormat = ssAutomatic
        Version = '8.0.5.5'
        WordWrap = False
        ColWidths = (
          20
          64
          64
          64
          64)
        RowHeights = (
          21
          21
          21
          21
          21)
      end
      object Button1: TButton
        Left = 0
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Expand all'
        TabOrder = 1
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 88
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Contract all'
        TabOrder = 2
        OnClick = Button2Click
      end
      object RadioGroup2: TRadioGroup
        Left = 264
        Top = 5
        Width = 281
        Height = 34
        Caption = 'Node style'
        Columns = 3
        ItemIndex = 2
        Items.Strings = (
          'Flat'
          '3D'
          'Glyph')
        TabOrder = 3
        OnClick = RadioGroup2Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Filter'
      object Label9: TLabel
        Left = 0
        Top = 8
        Width = 68
        Height = 13
        Caption = 'Filter column 1'
      end
      object Label10: TLabel
        Left = 112
        Top = 8
        Width = 68
        Height = 13
        Caption = 'Filter column 3'
      end
      object AdvStringGrid4: TAdvStringGrid
        Left = 0
        Top = 56
        Width = 641
        Height = 417
        Cursor = crDefault
        DefaultRowHeight = 21
        DrawingStyle = gdsClassic
        RowCount = 5
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        HoverRowCells = [hcNormal, hcSelected]
        HintColor = clYellow
        ActiveCellFont.Charset = DEFAULT_CHARSET
        ActiveCellFont.Color = clWindowText
        ActiveCellFont.Height = -11
        ActiveCellFont.Name = 'MS Sans Serif'
        ActiveCellFont.Style = [fsBold]
        Bands.Active = True
        CellNode.ShowTree = False
        CellNode.TreeColor = clSilver
        ControlLook.FixedGradientHoverFrom = clGray
        ControlLook.FixedGradientHoverTo = clWhite
        ControlLook.FixedGradientDownFrom = clGray
        ControlLook.FixedGradientDownTo = clSilver
        ControlLook.ControlStyle = csClassic
        ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownHeader.Font.Color = clWindowText
        ControlLook.DropDownHeader.Font.Height = -11
        ControlLook.DropDownHeader.Font.Name = 'Tahoma'
        ControlLook.DropDownHeader.Font.Style = []
        ControlLook.DropDownHeader.Visible = True
        ControlLook.DropDownHeader.Buttons = <>
        ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownFooter.Font.Color = clWindowText
        ControlLook.DropDownFooter.Font.Height = -11
        ControlLook.DropDownFooter.Font.Name = 'Tahoma'
        ControlLook.DropDownFooter.Font.Style = []
        ControlLook.DropDownFooter.Visible = True
        ControlLook.DropDownFooter.Buttons = <>
        EnhRowColMove = False
        Filter = <>
        FilterDropDown.Font.Charset = DEFAULT_CHARSET
        FilterDropDown.Font.Color = clWindowText
        FilterDropDown.Font.Height = -11
        FilterDropDown.Font.Name = 'Tahoma'
        FilterDropDown.Font.Style = []
        FilterDropDownClear = '(All)'
        FilterEdit.TypeNames.Strings = (
          'Starts with'
          'Ends with'
          'Contains'
          'Not contains'
          'Equal'
          'Not equal'
          'Clear')
        FixedRowAlways = True
        FixedFont.Charset = DEFAULT_CHARSET
        FixedFont.Color = clWindowText
        FixedFont.Height = -11
        FixedFont.Name = 'MS Sans Serif'
        FixedFont.Style = []
        FloatFormat = '%.2f'
        HoverButtons.Buttons = <>
        HoverButtons.Position = hbLeftFromColumnLeft
        HTMLSettings.ImageFolder = 'images'
        HTMLSettings.ImageBaseName = 'img'
        PrintSettings.DateFormat = 'dd/mm/yyyy'
        PrintSettings.Font.Charset = DEFAULT_CHARSET
        PrintSettings.Font.Color = clWindowText
        PrintSettings.Font.Height = -11
        PrintSettings.Font.Name = 'MS Sans Serif'
        PrintSettings.Font.Style = []
        PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
        PrintSettings.FixedFont.Color = clWindowText
        PrintSettings.FixedFont.Height = -11
        PrintSettings.FixedFont.Name = 'MS Sans Serif'
        PrintSettings.FixedFont.Style = []
        PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
        PrintSettings.HeaderFont.Color = clWindowText
        PrintSettings.HeaderFont.Height = -11
        PrintSettings.HeaderFont.Name = 'MS Sans Serif'
        PrintSettings.HeaderFont.Style = []
        PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
        PrintSettings.FooterFont.Color = clWindowText
        PrintSettings.FooterFont.Height = -11
        PrintSettings.FooterFont.Name = 'MS Sans Serif'
        PrintSettings.FooterFont.Style = []
        PrintSettings.Borders = pbNoborder
        PrintSettings.Centered = False
        PrintSettings.PageNumSep = '/'
        ScrollWidth = 16
        SearchFooter.FindNextCaption = 'Find next'
        SearchFooter.FindPrevCaption = 'Find previous'
        SearchFooter.Font.Charset = DEFAULT_CHARSET
        SearchFooter.Font.Color = clWindowText
        SearchFooter.Font.Height = -11
        SearchFooter.Font.Name = 'Tahoma'
        SearchFooter.Font.Style = []
        SearchFooter.HighLightCaption = 'Highlight'
        SearchFooter.HintClose = 'Close'
        SearchFooter.HintFindNext = 'Find next occurence'
        SearchFooter.HintFindPrev = 'Find previous occurence'
        SearchFooter.HintHighlight = 'Highlight occurences'
        SearchFooter.MatchCaseCaption = 'Match case'
        SelectionColor = clHighlight
        SelectionTextColor = clHighlightText
        SortSettings.DefaultFormat = ssAutomatic
        Version = '8.0.5.5'
        WordWrap = False
        ColWidths = (
          64
          64
          64
          64
          66)
        RowHeights = (
          21
          21
          21
          21
          21)
      end
      object ComboBox1: TComboBox
        Left = 0
        Top = 24
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = '*'
        OnChange = ComboBox1Change
        Items.Strings = (
          '*'
          'A*;M*'
          '>B & <M'
          'BMW'
          '!BMW'
          '!A* & !M*'
          '')
      end
      object ComboBox2: TComboBox
        Left = 113
        Top = 24
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        Text = '*'
        OnChange = ComboBox1Change
        Items.Strings = (
          '*'
          '4'
          '>4 & <12'
          '>4'
          '4 ^ 6')
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Multisort'
      object Label11: TLabel
        Left = 0
        Top = 8
        Width = 207
        Height = 13
        Caption = 'Left click column header for main sort index.'
      end
      object Label12: TLabel
        Left = 0
        Top = 24
        Width = 216
        Height = 13
        Caption = 'Add secondary sort indexes with shift left click'
      end
      object AdvStringGrid5: TAdvStringGrid
        Left = 0
        Top = 48
        Width = 641
        Height = 425
        Cursor = crDefault
        ColCount = 8
        DefaultRowHeight = 21
        DrawingStyle = gdsClassic
        RowCount = 5
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        HoverRowCells = [hcNormal, hcSelected]
        OnGetFormat = AdvStringGrid5GetFormat
        HintColor = clYellow
        ActiveCellFont.Charset = DEFAULT_CHARSET
        ActiveCellFont.Color = clWindowText
        ActiveCellFont.Height = -11
        ActiveCellFont.Name = 'MS Sans Serif'
        ActiveCellFont.Style = [fsBold]
        CellNode.ShowTree = False
        CellNode.TreeColor = clSilver
        ControlLook.FixedGradientHoverFrom = clGray
        ControlLook.FixedGradientHoverTo = clWhite
        ControlLook.FixedGradientDownFrom = clGray
        ControlLook.FixedGradientDownTo = clSilver
        ControlLook.ControlStyle = csClassic
        ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownHeader.Font.Color = clWindowText
        ControlLook.DropDownHeader.Font.Height = -11
        ControlLook.DropDownHeader.Font.Name = 'Tahoma'
        ControlLook.DropDownHeader.Font.Style = []
        ControlLook.DropDownHeader.Visible = True
        ControlLook.DropDownHeader.Buttons = <>
        ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownFooter.Font.Color = clWindowText
        ControlLook.DropDownFooter.Font.Height = -11
        ControlLook.DropDownFooter.Font.Name = 'Tahoma'
        ControlLook.DropDownFooter.Font.Style = []
        ControlLook.DropDownFooter.Visible = True
        ControlLook.DropDownFooter.Buttons = <>
        EnhRowColMove = False
        Filter = <>
        FilterDropDown.Font.Charset = DEFAULT_CHARSET
        FilterDropDown.Font.Color = clWindowText
        FilterDropDown.Font.Height = -11
        FilterDropDown.Font.Name = 'Tahoma'
        FilterDropDown.Font.Style = []
        FilterDropDownClear = '(All)'
        FilterEdit.TypeNames.Strings = (
          'Starts with'
          'Ends with'
          'Contains'
          'Not contains'
          'Equal'
          'Not equal'
          'Clear')
        FixedFont.Charset = DEFAULT_CHARSET
        FixedFont.Color = clWindowText
        FixedFont.Height = -11
        FixedFont.Name = 'MS Sans Serif'
        FixedFont.Style = []
        FloatFormat = '%.2f'
        HoverButtons.Buttons = <>
        HoverButtons.Position = hbLeftFromColumnLeft
        HTMLSettings.ImageFolder = 'images'
        HTMLSettings.ImageBaseName = 'img'
        PrintSettings.DateFormat = 'dd/mm/yyyy'
        PrintSettings.Font.Charset = DEFAULT_CHARSET
        PrintSettings.Font.Color = clWindowText
        PrintSettings.Font.Height = -11
        PrintSettings.Font.Name = 'MS Sans Serif'
        PrintSettings.Font.Style = []
        PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
        PrintSettings.FixedFont.Color = clWindowText
        PrintSettings.FixedFont.Height = -11
        PrintSettings.FixedFont.Name = 'MS Sans Serif'
        PrintSettings.FixedFont.Style = []
        PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
        PrintSettings.HeaderFont.Color = clWindowText
        PrintSettings.HeaderFont.Height = -11
        PrintSettings.HeaderFont.Name = 'MS Sans Serif'
        PrintSettings.HeaderFont.Style = []
        PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
        PrintSettings.FooterFont.Color = clWindowText
        PrintSettings.FooterFont.Height = -11
        PrintSettings.FooterFont.Name = 'MS Sans Serif'
        PrintSettings.FooterFont.Style = []
        PrintSettings.Borders = pbNoborder
        PrintSettings.Centered = False
        PrintSettings.PageNumSep = '/'
        ScrollWidth = 16
        SearchFooter.FindNextCaption = 'Find next'
        SearchFooter.FindPrevCaption = 'Find previous'
        SearchFooter.Font.Charset = DEFAULT_CHARSET
        SearchFooter.Font.Color = clWindowText
        SearchFooter.Font.Height = -11
        SearchFooter.Font.Name = 'Tahoma'
        SearchFooter.Font.Style = []
        SearchFooter.HighLightCaption = 'Highlight'
        SearchFooter.HintClose = 'Close'
        SearchFooter.HintFindNext = 'Find next occurence'
        SearchFooter.HintFindPrev = 'Find previous occurence'
        SearchFooter.HintHighlight = 'Highlight occurences'
        SearchFooter.MatchCaseCaption = 'Match case'
        SelectionColor = clHighlight
        SelectionTextColor = clHighlightText
        SortSettings.DefaultFormat = ssAutomatic
        SortSettings.Show = True
        SortSettings.IndexShow = True
        Version = '8.0.5.5'
        WordWrap = False
        ColWidths = (
          64
          64
          64
          64
          64
          64
          64
          64)
        RowHeights = (
          21
          21
          21
          21
          21)
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Cell merging'
      object Label13: TLabel
        Left = 8
        Top = 24
        Width = 268
        Height = 13
        Caption = 'Static merged cells and merged cells with inplace edirtors'
      end
      object asg: TAdvStringGrid
        Left = 8
        Top = 40
        Width = 625
        Height = 425
        Cursor = crDefault
        DrawingStyle = gdsClassic
        RowCount = 5
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected]
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        HoverRowCells = [hcNormal, hcSelected]
        OnCanEditCell = asgCanEditCell
        OnAnchorClick = asgAnchorClick
        OnGetEditorType = asgGetEditorType
        ActiveCellFont.Charset = DEFAULT_CHARSET
        ActiveCellFont.Color = clWindowText
        ActiveCellFont.Height = -11
        ActiveCellFont.Name = 'MS Sans Serif'
        ActiveCellFont.Style = [fsBold]
        CellNode.ShowTree = False
        CellNode.TreeColor = clSilver
        ControlLook.FixedGradientHoverFrom = clGray
        ControlLook.FixedGradientHoverTo = clWhite
        ControlLook.FixedGradientDownFrom = clGray
        ControlLook.FixedGradientDownTo = clSilver
        ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownHeader.Font.Color = clWindowText
        ControlLook.DropDownHeader.Font.Height = -11
        ControlLook.DropDownHeader.Font.Name = 'Tahoma'
        ControlLook.DropDownHeader.Font.Style = []
        ControlLook.DropDownHeader.Visible = True
        ControlLook.DropDownHeader.Buttons = <>
        ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
        ControlLook.DropDownFooter.Font.Color = clWindowText
        ControlLook.DropDownFooter.Font.Height = -11
        ControlLook.DropDownFooter.Font.Name = 'Tahoma'
        ControlLook.DropDownFooter.Font.Style = []
        ControlLook.DropDownFooter.Visible = True
        ControlLook.DropDownFooter.Buttons = <>
        EnhRowColMove = False
        Filter = <>
        FilterDropDown.Font.Charset = DEFAULT_CHARSET
        FilterDropDown.Font.Color = clWindowText
        FilterDropDown.Font.Height = -11
        FilterDropDown.Font.Name = 'Tahoma'
        FilterDropDown.Font.Style = []
        FilterDropDownClear = '(All)'
        FilterEdit.TypeNames.Strings = (
          'Starts with'
          'Ends with'
          'Contains'
          'Not contains'
          'Equal'
          'Not equal'
          'Clear')
        FixedRowHeight = 22
        FixedFont.Charset = DEFAULT_CHARSET
        FixedFont.Color = clWindowText
        FixedFont.Height = -11
        FixedFont.Name = 'Tahoma'
        FixedFont.Style = []
        FloatFormat = '%.2f'
        GridImages = ImageList2
        HoverButtons.Buttons = <>
        HoverButtons.Position = hbLeftFromColumnLeft
        HTMLSettings.ImageFolder = 'images'
        HTMLSettings.ImageBaseName = 'img'
        PrintSettings.DateFormat = 'dd/mm/yyyy'
        PrintSettings.Font.Charset = DEFAULT_CHARSET
        PrintSettings.Font.Color = clWindowText
        PrintSettings.Font.Height = -11
        PrintSettings.Font.Name = 'MS Sans Serif'
        PrintSettings.Font.Style = []
        PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
        PrintSettings.FixedFont.Color = clWindowText
        PrintSettings.FixedFont.Height = -11
        PrintSettings.FixedFont.Name = 'MS Sans Serif'
        PrintSettings.FixedFont.Style = []
        PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
        PrintSettings.HeaderFont.Color = clWindowText
        PrintSettings.HeaderFont.Height = -11
        PrintSettings.HeaderFont.Name = 'MS Sans Serif'
        PrintSettings.HeaderFont.Style = []
        PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
        PrintSettings.FooterFont.Color = clWindowText
        PrintSettings.FooterFont.Height = -11
        PrintSettings.FooterFont.Name = 'MS Sans Serif'
        PrintSettings.FooterFont.Style = []
        PrintSettings.Borders = pbNoborder
        PrintSettings.Centered = False
        PrintSettings.PageNumSep = '/'
        ScrollWidth = 16
        SearchFooter.FindNextCaption = 'Find next'
        SearchFooter.FindPrevCaption = 'Find previous'
        SearchFooter.Font.Charset = DEFAULT_CHARSET
        SearchFooter.Font.Color = clWindowText
        SearchFooter.Font.Height = -11
        SearchFooter.Font.Name = 'Tahoma'
        SearchFooter.Font.Style = []
        SearchFooter.HighLightCaption = 'Highlight'
        SearchFooter.HintClose = 'Close'
        SearchFooter.HintFindNext = 'Find next occurence'
        SearchFooter.HintFindPrev = 'Find previous occurence'
        SearchFooter.HintHighlight = 'Highlight occurences'
        SearchFooter.MatchCaseCaption = 'Match case'
        SelectionColor = clHighlight
        SelectionTextColor = clHighlightText
        SortSettings.DefaultFormat = ssAutomatic
        Version = '8.0.5.5'
        ColWidths = (
          64
          64
          64
          64
          64)
        RowHeights = (
          22
          22
          22
          22
          22)
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'System'
    Font.Style = []
    Left = 308
    Top = 420
  end
  object MainMenu1: TMainMenu
    Left = 388
    Top = 428
    object Loaddata1: TMenuItem
      Caption = 'File'
      object Savetofile1: TMenuItem
        Caption = 'Save to file'
        OnClick = Savetofile1Click
      end
      object Loadfromfile1: TMenuItem
        Caption = 'Load from file'
        OnClick = Loadfromfile1Click
      end
      object SavetoCSV1: TMenuItem
        Caption = 'Save to CSV'
        OnClick = SavetoCSV1Click
      end
      object LoadfromCSV1: TMenuItem
        Caption = 'Load from CSV'
        OnClick = LoadfromCSV1Click
      end
      object SaveasXLS1: TMenuItem
        Caption = 'Save as XLS'
        OnClick = SaveasXLS1Click
      end
      object LoadfromXLS1: TMenuItem
        Caption = 'Load from XLS'
        OnClick = LoadfromXLS1Click
      end
      object SavetoHTML1: TMenuItem
        Caption = 'Save to HTML'
        OnClick = SavetoHTML1Click
      end
      object SavetoASCII1: TMenuItem
        Caption = 'Save to ASCII'
        OnClick = SavetoASCII1Click
      end
      object SavetoDOC1: TMenuItem
        Caption = 'Save to DOC'
        OnClick = SavetoDOC1Click
      end
      object SavetoXML1: TMenuItem
        Caption = 'Save to XML'
        OnClick = SavetoXML1Click
      end
      object SavetoRTF1: TMenuItem
        Caption = 'Save to RTF'
        OnClick = SavetoRTF1Click
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Clipboard1: TMenuItem
      Caption = 'Clipboard'
      object Copyselectiontoclipboard1: TMenuItem
        Caption = 'Copy selection to clipboard'
        OnClick = Copyselectiontoclipboard1Click
      end
      object Copytoclipboard2: TMenuItem
        Caption = 'Copy all to clipboard'
        OnClick = Copytoclipboard2Click
      end
      object Pastefromclipboard2: TMenuItem
        Caption = 'Paste selection from clipboard'
        OnClick = Pastefromclipboard2Click
      end
      object Pastefromclipboard1: TMenuItem
        Caption = 'Paste all from clipboard'
        OnClick = Pastefromclipboard1Click
      end
    end
    object Print1: TMenuItem
      Caption = 'Print'
      object Print2: TMenuItem
        Caption = 'Print'
        OnClick = Print2Click
      end
      object Printpreview1: TMenuItem
        Caption = 'Print preview'
        OnClick = Printpreview1Click
      end
      object Setselection1: TMenuItem
        Caption = 'Print selection'
        OnClick = Setselection1Click
      end
      object Printwithgraphics1: TMenuItem
        Caption = 'Print with graphics'
        OnClick = Printwithgraphics1Click
      end
      object Printpreviewwithgraphics1: TMenuItem
        Caption = 'Print preview with graphics'
        OnClick = Printpreviewwithgraphics1Click
      end
    end
    object Hide1: TMenuItem
      Caption = 'Hide'
      object Column11: TMenuItem
        Tag = 1
        Caption = 'Column 1'
        OnClick = Column11Click
      end
      object Column21: TMenuItem
        Tag = 2
        Caption = 'Column 2'
        OnClick = Column11Click
      end
      object Column31: TMenuItem
        Tag = 3
        Caption = 'Column 3'
        OnClick = Column11Click
      end
      object Column41: TMenuItem
        Tag = 4
        Caption = 'Column 4'
        OnClick = Column11Click
      end
      object Column51: TMenuItem
        Tag = 5
        Caption = 'Column 5'
        OnClick = Column11Click
      end
      object Column61: TMenuItem
        Tag = 6
        Caption = 'Column 6'
        OnClick = Column11Click
      end
      object Column71: TMenuItem
        Tag = 7
        Caption = 'Column 7'
        OnClick = Column11Click
      end
    end
    object Search1: TMenuItem
      Caption = 'Search'
      object Findfirst1: TMenuItem
        Caption = 'Find first'
        OnClick = Findfirst1Click
      end
      object Findnext1: TMenuItem
        Caption = 'Find next'
        ShortCut = 114
        OnClick = Findnext1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object ImageList1: TImageList
    Left = 324
    Top = 344
    Bitmap = {
      494C010105000A00440010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF0000007B7B7B00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFF
      FF0000FFFF00FFFFFF007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF0000007B7B7B00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF00FFFFFF00FFFFFF007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF0000007B7B7B00FFFFFF0000FFFF007B0000007B0000007B00
      00007B7B7B00FFFFFF007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF0000007B7B7B00FFFFFF007B7B0000FF000000FF000000FF00
      00007B000000FFFFFF007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF0000007B7B7B00FFFFFF007B7B00007B7B7B00007B0000FF00
      00007B000000FFFFFF007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000007B0000007B00007B7B7B00FFFFFF007B7B0000FFFFFF007B7B7B00007B
      00007B000000FFFFFF007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000007B
      0000007B0000007B00007B7B7B00FFFFFF0000FFFF007B7B00007B7B00007B7B
      00007B7B7B00FFFFFF007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000007B
      0000007B0000007B00007B7B7B00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF0000000000
      0000007B0000007B00007B7B7B00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFF
      FF007B7B7B00FFFFFF007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FF000000FF00
      0000FFFFFF00000000007B7B7B00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF007B7B7B007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FF000000FFFF
      FF0000000000FFFFFF007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B00FF00
      0000FF00000000000000FFFFFF0000000000007B0000007B0000007B0000007B
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B00FF000000FF000000FFFFFF0000000000007B0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B00000000000000
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
      00000000000000007B00FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000FFFF0000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B00BDBDBD0000FFFF00BDBDBD00BDBDBD00000000007B7B7B007B7B7B007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      7B00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000FFFF0000000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B0000000000000000000000000000000000000000007B7B
      7B00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF000000000000FFFF00BDBD
      BD007B7B7B007B7B7B000000000000000000000000000000000000007B000000
      7B0000007B00FFFFFF0000000000000000000000000000000000000000000000
      000000007B00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000FFFF0000000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000007B7B7B00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD007B7B7B0000000000000000000000000000000000000000000000
      00007B7B7B0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF0000000000FFFF
      FF0000FFFF00BDBDBD007B7B7B0000000000000000000000000000007B000000
      7B0000007B00FFFFFF0000000000000000000000000000000000000000000000
      7B00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000FFFF0000000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000007B7B7B00007B7B00BDBD
      BD00BDBDBD00BDBDBD00007B7B00007B7B00007B7B00BDBDBD00BDBDBD00BDBD
      BD00007B7B007B7B7B0000000000000000000000000000000000000000000000
      00007B7B7B00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF0000000000FFFF
      FF0000000000000000007B7B7B00000000000000000000000000000000000000
      7B0000007B0000007B00FFFFFF0000000000000000000000000000007B000000
      7B00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000007B7B7B0000FFFF00007B
      7B00BDBDBD00007B7B00000000000000000000000000007B7B00BDBDBD00007B
      7B00BDBDBD007B7B7B0000000000000000000000000000000000000000000000
      0000000000007B7B7B00FFFFFF00FFFFFF00FFFFFF0000FFFF007B7B7B000000
      0000FFFFFF0000FFFF00BDBDBD00000000000000000000000000000000000000
      000000007B0000007B0000007B00FFFFFF000000000000007B0000007B00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000007B7B7B00FFFFFF00FFFF
      FF00007B7B0000000000FFFFFF00FFFFFF0000FFFF0000000000007B7B00BDBD
      BD00BDBDBD007B7B7B0000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF00FFFFFF00FFFFFF00FFFFFF0000FF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000000000000000
      00000000000000007B0000007B0000007B0000007B0000007B00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000000000007B7B7B0000FFFF00BDBD
      BD0000000000FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000000000007B
      7B00BDBDBD007B7B7B0000000000000000000000000000000000000000000000
      0000000000007B7B00007B7B000000000000FFFFFF0000FFFF00FFFFFF00FFFF
      FF00FFFFFF0000FFFF00BDBDBD00000000000000000000000000000000000000
      0000000000000000000000007B0000007B0000007B00FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF0000FFFF0000000000000000007B7B7B00BDBDBD000000
      000000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF000000
      0000007B7B007B7B7B000000000000000000000000007B7B00007B7B00000000
      000000000000000000007B7B000000000000FFFFFF00FFFFFF00FFFFFF0000FF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000000000000000
      00000000000000007B0000007B0000007B0000007B0000007B00FFFFFF000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000FFFF0000FFFF000000000000000000000000007B7B7B0000000000FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF0000000000007B7B00000000000000000000000000FFFF00007B7B00000000
      0000FFFF0000000000007B7B0000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000FFFF00BDBDBD00000000000000000000000000000000000000
      000000007B0000007B0000007B00FFFFFF000000000000007B00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000000000FFFFFF00FFFFFF00FFFFFF000000000000FF
      FF0000FFFF00000000000000000000000000000000007B7B7B00FFFFFF00FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFF
      FF0000FFFF0000000000000000000000000000000000FFFF0000FFFF00000000
      000000000000FFFF0000FFFF000000000000FFFFFF00FFFFFF00FFFFFF0000FF
      FF00FFFFFF00FFFFFF00BDBDBD0000000000000000000000000000007B000000
      7B0000007B0000007B00FFFFFF0000000000000000000000000000007B000000
      7B00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000000000FFFFFF000000000000FFFF0000FF
      FF00000000000000FF000000000000000000000000007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFF
      FF00FFFFFF0000FFFF00BDBDBD00000000000000000000007B0000007B000000
      7B0000007B00FFFFFF0000000000000000000000000000000000000000000000
      7B0000007B00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000FFFF0000FFFF000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000007B0000007B00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000007B0000007B00FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00FC00000000000000F800000000000000
      E000000000000000C00000000000000080000000000000008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      100100000000000084030000000000008803000000000000C507000000000000
      E08F000000000000F83F000000000000FFFFFFFFFFFFC001FFFFF00FFFF9C001
      8001E003E7FFC0018001E001C3F3C0018001F000C3E7C0018001F000E1C7C001
      8001F800F08FC0018001F800F81F800080019000FC3F800080010000F81FC001
      80010000F09FE00180010000C1C7F0018001980083E3F801FFFFF8008FF1FC11
      FFFFF800FFFFFE31FFFFFFFFFFFFFF7F00000000000000000000000000000000
      000000000000}
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 508
    Top = 56
  end
  object ImageList2: TImageList
    Left = 440
    Top = 360
    Bitmap = {
      494C010101000500440010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BCBCBC00B6B6B600BABABA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C3B5B500BA8A8A008E71710096969600B9B9B90000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C3B5
      B500CB716800C21E0000B4382500916E6E0097979700B8B8B800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5C3B50093A8
      6800813E000000800000455D0000AE3C2700936C6C0097979700B8B8B8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B6C3B60067D4670000C0
      0000008000000080000000800000485B0000AA3E2800946B6B0098989800B7B7
      B700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A4C9A40000E600000080
      00000080000000900000009E0000008000004A5A0000A7402900946B6B009898
      9800B7B7B7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A4C9A40000E600000080
      00001F7000005A73120021C12100009F0000008000004C590000A6412900946B
      6B00AAAAAA000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A4C9A40000E600000080
      0000485B0000D42A2A0071A5710034D13400009F00000080000091360000B04F
      4F0096969600B7B7B70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B2C4B20051D8510000B2
      0000485B0000DF363600ADADAD0098CC980034D03400009F0000455D0000A63F
      25009669690095959500B8B8B800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B0C5B00053D6
      53001FB9000088A14000000000000000000098CC980034D13400009F0000435E
      0000AA3C2400946B6B0094949400B9B9B9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AEC5
      AE0077D777008FCF8F0000000000000000000000000099CC990032D23200009D
      0000405F0000AF392200926D6D00939393000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009BCC9B0030D4
      3000009C00003C610000B6342000906F6F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009DCB
      9D002DD72D0000990000C21E0000986767000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A1C9A10029DB2900C2350000BF8F8F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A6C8A600C0A79B00C2B9B9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000F8FF000000000000
      F07F000000000000E03F000000000000C01F000000000000800F000000000000
      8007000000000000800700000000000080030000000000008001000000000000
      C300000000000000E380000000000000FFC0000000000000FFE0000000000000
      FFF0000000000000FFF800000000000000000000000000000000000000000000
      000000000000}
  end
  object AdvGridRTFIO1: TAdvGridRTFIO
    AdvStringGrid = AdvStringGrid2
    GridStartRow = 0
    GridStartCol = 0
    Options.ExportBackGround = True
    Options.ExportOverwrite = omAlways
    Options.ExportOverwriteMessage = 'File already exists'#13'Ok to overwrite ?'
    Options.ExportShowInWord = True
    Options.ExportMsWordFeatures = True
    Options.ExportImages = True
    Options.ConvertHTML = True
    Options.ExportRTFCell = True
    Version = '1.4.0.0'
    Left = 500
    Top = 344
  end
  object AdvPreviewDialog1: TAdvPreviewDialog
    CloseAfterPrint = False
    DialogCaption = 'Preview'
    DialogPrevBtn = 'Previous'
    DialogNextBtn = 'Next'
    DialogPrintBtn = 'Print'
    DialogCloseBtn = 'Close'
    Grid = AdvStringGrid1
    PreviewFast = False
    PreviewWidth = 450
    PreviewHeight = 600
    PreviewLeft = 100
    PreviewTop = 100
    PreviewCenter = False
    PrinterSetupDialog = True
    Left = 472
    Top = 264
  end
  object PrintDialog1: TPrintDialog
    Left = 312
    Top = 272
  end
end
