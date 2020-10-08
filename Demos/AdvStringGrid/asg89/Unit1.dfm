object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TAdvStringGrid with TAdvSearchEdit inplace editor'
  ClientHeight = 315
  ClientWidth = 653
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 195
    Height = 13
    Caption = 'Assign the right continent to the country'
  end
  object AdvStringGrid1: TAdvStringGrid
    Left = 24
    Top = 43
    Width = 608
    Height = 250
    Cursor = crDefault
    ColCount = 3
    DefaultColWidth = 32
    DrawingStyle = gdsClassic
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    HoverRowCells = [hcNormal, hcSelected]
    OnGetEditorType = AdvStringGrid1GetEditorType
    ActiveCellFont.Charset = DEFAULT_CHARSET
    ActiveCellFont.Color = clWindowText
    ActiveCellFont.Height = -11
    ActiveCellFont.Name = 'Tahoma'
    ActiveCellFont.Style = [fsBold]
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
    DefaultEditor = edCustom
    Filter = <>
    FilterDropDown.Font.Charset = DEFAULT_CHARSET
    FilterDropDown.Font.Color = clWindowText
    FilterDropDown.Font.Height = -11
    FilterDropDown.Font.Name = 'Tahoma'
    FilterDropDown.Font.Style = []
    FilterDropDown.TextChecked = 'Checked'
    FilterDropDown.TextUnChecked = 'Unchecked'
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
    FixedColWidth = 32
    FixedRowHeight = 22
    FixedFont.Charset = DEFAULT_CHARSET
    FixedFont.Color = clWindowText
    FixedFont.Height = -11
    FixedFont.Name = 'Tahoma'
    FixedFont.Style = [fsBold]
    FloatFormat = '%.2f'
    HoverButtons.Buttons = <>
    HoverButtons.Position = hbLeftFromColumnLeft
    HTMLSettings.ImageFolder = 'images'
    HTMLSettings.ImageBaseName = 'img'
    PrintSettings.DateFormat = 'dd/mm/yyyy'
    PrintSettings.Font.Charset = DEFAULT_CHARSET
    PrintSettings.Font.Color = clWindowText
    PrintSettings.Font.Height = -11
    PrintSettings.Font.Name = 'Tahoma'
    PrintSettings.Font.Style = []
    PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
    PrintSettings.FixedFont.Color = clWindowText
    PrintSettings.FixedFont.Height = -11
    PrintSettings.FixedFont.Name = 'Tahoma'
    PrintSettings.FixedFont.Style = []
    PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
    PrintSettings.HeaderFont.Color = clWindowText
    PrintSettings.HeaderFont.Height = -11
    PrintSettings.HeaderFont.Name = 'Tahoma'
    PrintSettings.HeaderFont.Style = []
    PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
    PrintSettings.FooterFont.Color = clWindowText
    PrintSettings.FooterFont.Height = -11
    PrintSettings.FooterFont.Name = 'Tahoma'
    PrintSettings.FooterFont.Style = []
    PrintSettings.PageNumSep = '/'
    SearchFooter.FindNextCaption = 'Find &next'
    SearchFooter.FindPrevCaption = 'Find &previous'
    SearchFooter.Font.Charset = DEFAULT_CHARSET
    SearchFooter.Font.Color = clWindowText
    SearchFooter.Font.Height = -11
    SearchFooter.Font.Name = 'Tahoma'
    SearchFooter.Font.Style = []
    SearchFooter.HighLightCaption = 'Highlight'
    SearchFooter.HintClose = 'Close'
    SearchFooter.HintFindNext = 'Find next occurrence'
    SearchFooter.HintFindPrev = 'Find previous occurrence'
    SearchFooter.HintHighlight = 'Highlight occurrences'
    SearchFooter.MatchCaseCaption = 'Match case'
    SortSettings.DefaultFormat = ssAutomatic
    Version = '8.0.5.3'
    ColWidths = (
      32
      197
      209)
    RowHeights = (
      22
      22
      22
      22
      22
      22
      22
      22
      22
      22)
  end
  object AdvSearchEditEditLink1: TAdvSearchEditEditLink
    Tag = 0
    WantKeyLeftRight = True
    WantKeyUpDown = False
    WantKeyHomeEnd = True
    WantKeyPriorNext = False
    Appearance.CategoryFont.Charset = DEFAULT_CHARSET
    Appearance.CategoryFont.Color = clWindowText
    Appearance.CategoryFont.Height = -11
    Appearance.CategoryFont.Name = 'Tahoma'
    Appearance.CategoryFont.Style = []
    Appearance.DescriptionFont.Charset = DEFAULT_CHARSET
    Appearance.DescriptionFont.Color = clWindowText
    Appearance.DescriptionFont.Height = -11
    Appearance.DescriptionFont.Name = 'Tahoma'
    Appearance.DescriptionFont.Style = []
    Appearance.DescriptionControlFont = False
    Appearance.FilterCountFont.Charset = DEFAULT_CHARSET
    Appearance.FilterCountFont.Color = clWindowText
    Appearance.FilterCountFont.Height = -11
    Appearance.FilterCountFont.Name = 'Tahoma'
    Appearance.FilterCountFont.Style = []
    Appearance.FilterCountFormat = '(%d)'
    Appearance.ItemCategoryFont.Charset = DEFAULT_CHARSET
    Appearance.ItemCategoryFont.Color = 42495
    Appearance.ItemCategoryFont.Height = -11
    Appearance.ItemCategoryFont.Name = 'Tahoma'
    Appearance.ItemCategoryFont.Style = []
    Appearance.ItemCategoryFormat = 'in %s'
    Categories = <>
    CategoryButton.Appearance.ColorChecked = 16111818
    CategoryButton.Appearance.ColorCheckedTo = 16367008
    CategoryButton.Appearance.ColorDisabled = 15921906
    CategoryButton.Appearance.ColorDisabledTo = 15921906
    CategoryButton.Appearance.ColorDown = 16111818
    CategoryButton.Appearance.ColorDownTo = 16367008
    CategoryButton.Appearance.ColorHot = 16117985
    CategoryButton.Appearance.ColorHotTo = 16372402
    CategoryButton.Appearance.ColorMirrorHot = 16107693
    CategoryButton.Appearance.ColorMirrorHotTo = 16775412
    CategoryButton.Appearance.ColorMirrorDown = 16102556
    CategoryButton.Appearance.ColorMirrorDownTo = 16768988
    CategoryButton.Appearance.ColorMirrorChecked = 16102556
    CategoryButton.Appearance.ColorMirrorCheckedTo = 16768988
    CategoryButton.Appearance.ColorMirrorDisabled = 11974326
    CategoryButton.Appearance.ColorMirrorDisabledTo = 15921906
    Columns = <>
    DropDownFooter.Font.Charset = DEFAULT_CHARSET
    DropDownFooter.Font.Color = clWindowText
    DropDownFooter.Font.Height = -11
    DropDownFooter.Font.Name = 'Tahoma'
    DropDownFooter.Font.Style = []
    DropDownFooter.Visible = True
    DropDownFooter.Buttons = <>
    DropDownHeader.Font.Charset = DEFAULT_CHARSET
    DropDownHeader.Font.Color = clWindowText
    DropDownHeader.Font.Height = -11
    DropDownHeader.Font.Name = 'Tahoma'
    DropDownHeader.Font.Style = []
    DropDownHeader.Visible = True
    DropDownHeader.Buttons = <>
    DropDownShadow = False
    DropDownSizable = False
    EmptyText = 'Search ...'
    FilterCondition.AutoSelect = False
    Items = <>
    SearchButton.Appearance.ColorChecked = 16111818
    SearchButton.Appearance.ColorCheckedTo = 16367008
    SearchButton.Appearance.ColorDisabled = 15921906
    SearchButton.Appearance.ColorDisabledTo = 15921906
    SearchButton.Appearance.ColorDown = 16111818
    SearchButton.Appearance.ColorDownTo = 16367008
    SearchButton.Appearance.ColorHot = 16117985
    SearchButton.Appearance.ColorHotTo = 16372402
    SearchButton.Appearance.ColorMirrorHot = 16107693
    SearchButton.Appearance.ColorMirrorHotTo = 16775412
    SearchButton.Appearance.ColorMirrorDown = 16102556
    SearchButton.Appearance.ColorMirrorDownTo = 16768988
    SearchButton.Appearance.ColorMirrorChecked = 16102556
    SearchButton.Appearance.ColorMirrorCheckedTo = 16768988
    SearchButton.Appearance.ColorMirrorDisabled = 11974326
    SearchButton.Appearance.ColorMirrorDisabledTo = 15921906
    Left = 544
    Top = 128
  end
  object AdvSearchEditEditLink2: TAdvSearchEditEditLink
    Tag = 0
    WantKeyLeftRight = True
    WantKeyUpDown = False
    WantKeyHomeEnd = True
    WantKeyPriorNext = False
    Appearance.CategoryFont.Charset = DEFAULT_CHARSET
    Appearance.CategoryFont.Color = clWindowText
    Appearance.CategoryFont.Height = -11
    Appearance.CategoryFont.Name = 'Tahoma'
    Appearance.CategoryFont.Style = []
    Appearance.DescriptionFont.Charset = DEFAULT_CHARSET
    Appearance.DescriptionFont.Color = clWindowText
    Appearance.DescriptionFont.Height = -11
    Appearance.DescriptionFont.Name = 'Tahoma'
    Appearance.DescriptionFont.Style = []
    Appearance.DescriptionControlFont = False
    Appearance.FilterCountFont.Charset = DEFAULT_CHARSET
    Appearance.FilterCountFont.Color = clWindowText
    Appearance.FilterCountFont.Height = -11
    Appearance.FilterCountFont.Name = 'Tahoma'
    Appearance.FilterCountFont.Style = []
    Appearance.FilterCountFormat = '(%d)'
    Appearance.ItemCategoryFont.Charset = DEFAULT_CHARSET
    Appearance.ItemCategoryFont.Color = 42495
    Appearance.ItemCategoryFont.Height = -11
    Appearance.ItemCategoryFont.Name = 'Tahoma'
    Appearance.ItemCategoryFont.Style = []
    Appearance.ItemCategoryFormat = 'in %s'
    Categories = <>
    CategoryButton.Appearance.ColorChecked = 16111818
    CategoryButton.Appearance.ColorCheckedTo = 16367008
    CategoryButton.Appearance.ColorDisabled = 15921906
    CategoryButton.Appearance.ColorDisabledTo = 15921906
    CategoryButton.Appearance.ColorDown = 16111818
    CategoryButton.Appearance.ColorDownTo = 16367008
    CategoryButton.Appearance.ColorHot = 16117985
    CategoryButton.Appearance.ColorHotTo = 16372402
    CategoryButton.Appearance.ColorMirrorHot = 16107693
    CategoryButton.Appearance.ColorMirrorHotTo = 16775412
    CategoryButton.Appearance.ColorMirrorDown = 16102556
    CategoryButton.Appearance.ColorMirrorDownTo = 16768988
    CategoryButton.Appearance.ColorMirrorChecked = 16102556
    CategoryButton.Appearance.ColorMirrorCheckedTo = 16768988
    CategoryButton.Appearance.ColorMirrorDisabled = 11974326
    CategoryButton.Appearance.ColorMirrorDisabledTo = 15921906
    Columns = <>
    DropDownFooter.Font.Charset = DEFAULT_CHARSET
    DropDownFooter.Font.Color = clWindowText
    DropDownFooter.Font.Height = -11
    DropDownFooter.Font.Name = 'Tahoma'
    DropDownFooter.Font.Style = []
    DropDownFooter.Visible = True
    DropDownFooter.Buttons = <>
    DropDownHeader.Font.Charset = DEFAULT_CHARSET
    DropDownHeader.Font.Color = clWindowText
    DropDownHeader.Font.Height = -11
    DropDownHeader.Font.Name = 'Tahoma'
    DropDownHeader.Font.Style = []
    DropDownHeader.Visible = True
    DropDownHeader.Buttons = <>
    DropDownShadow = False
    DropDownSizable = False
    EmptyText = 'Search ...'
    FilterCondition.AutoSelect = False
    Items = <>
    SearchButton.Appearance.ColorChecked = 16111818
    SearchButton.Appearance.ColorCheckedTo = 16367008
    SearchButton.Appearance.ColorDisabled = 15921906
    SearchButton.Appearance.ColorDisabledTo = 15921906
    SearchButton.Appearance.ColorDown = 16111818
    SearchButton.Appearance.ColorDownTo = 16367008
    SearchButton.Appearance.ColorHot = 16117985
    SearchButton.Appearance.ColorHotTo = 16372402
    SearchButton.Appearance.ColorMirrorHot = 16107693
    SearchButton.Appearance.ColorMirrorHotTo = 16775412
    SearchButton.Appearance.ColorMirrorDown = 16102556
    SearchButton.Appearance.ColorMirrorDownTo = 16768988
    SearchButton.Appearance.ColorMirrorChecked = 16102556
    SearchButton.Appearance.ColorMirrorCheckedTo = 16768988
    SearchButton.Appearance.ColorMirrorDisabled = 11974326
    SearchButton.Appearance.ColorMirrorDisabledTo = 15921906
    Left = 544
    Top = 64
  end
end
