object Form5: TForm5
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'AdvSearchList and AdvSearchEdit demo'
  ClientHeight = 769
  ClientWidth = 869
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
  OnCreate = FormCreate
  DesignSize = (
    869
    769)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 751
    Width = 163
    Height = 13
    Caption = 'For more information please visit: '
  end
  object Label4: TLabel
    Left = 177
    Top = 750
    Width = 160
    Height = 13
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Caption = 'https://www.tmssoftware.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label4Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 24
    Width = 281
    Height = 337
    Caption = 'Dictionary lookup'
    TabOrder = 0
    object Edit1: TEdit
      Left = 15
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 0
      OnChange = Edit1Change
    end
    object AdvSearchList1: TAdvSearchList
      Left = 15
      Top = 58
      Width = 249
      Height = 199
      HorzScrollBar.Tracking = True
      VertScrollBar.Range = 20
      VertScrollBar.Tracking = True
      TabOrder = 1
      Appearance.BandColorOdd = clInactiveCaption
      Appearance.CategoryControlFont = False
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
      Appearance.SelectionColor = clCream
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
      FilterCondition.AutoSelect = False
      BorderColor = clNone
      Categories = <>
      Columns = <
        item
          ControlFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          WordWrap = False
        end>
      Items = <>
      Version = '1.0.7.1'
      OnItemChange = AdvSearchList1ItemChange
    end
    object AdvSearchEdit1: TAdvSearchEdit
      Left = 15
      Top = 300
      Width = 249
      Height = 23
      Appearance.CategoryControlFont = False
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
      CategoryButton.Appearance.BorderColor = 13948116
      CategoryButton.Appearance.BorderColorHot = 15917525
      CategoryButton.Appearance.BorderColorCheckedHot = 14925219
      CategoryButton.Appearance.BorderColorDown = 14925219
      CategoryButton.Appearance.BorderColorChecked = 15914434
      CategoryButton.Appearance.BorderColorDisabled = 13948116
      CategoryButton.Appearance.ColorTo = clNone
      CategoryButton.Appearance.ColorChecked = 15914434
      CategoryButton.Appearance.ColorCheckedTo = clNone
      CategoryButton.Appearance.ColorDisabled = clWhite
      CategoryButton.Appearance.ColorDisabledTo = clNone
      CategoryButton.Appearance.ColorDown = 14925219
      CategoryButton.Appearance.ColorDownTo = clNone
      CategoryButton.Appearance.ColorHot = 15917525
      CategoryButton.Appearance.ColorHotTo = clNone
      CategoryButton.Appearance.ColorMirror = clWhite
      CategoryButton.Appearance.ColorMirrorTo = clNone
      CategoryButton.Appearance.ColorMirrorHot = 15917525
      CategoryButton.Appearance.ColorMirrorHotTo = clNone
      CategoryButton.Appearance.ColorMirrorDown = 14925219
      CategoryButton.Appearance.ColorMirrorDownTo = clNone
      CategoryButton.Appearance.ColorMirrorChecked = 15914434
      CategoryButton.Appearance.ColorMirrorCheckedTo = clNone
      CategoryButton.Appearance.ColorMirrorDisabled = clWhite
      CategoryButton.Appearance.ColorMirrorDisabledTo = clNone
      CategoryButton.Appearance.GradientHot = ggVertical
      CategoryButton.Appearance.GradientMirrorHot = ggVertical
      CategoryButton.Appearance.GradientDown = ggVertical
      CategoryButton.Appearance.GradientMirrorDown = ggVertical
      CategoryButton.Appearance.GradientChecked = ggVertical
      CategoryButton.Appearance.TextColorChecked = 5263440
      CategoryButton.Appearance.TextColorDown = 5263440
      CategoryButton.Appearance.TextColorHot = 5263440
      CategoryButton.Visible = False
      Columns = <
        item
          ControlFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          WordWrap = False
        end>
      DropDownHeader.Color = clWhite
      DropDownHeader.ColorTo = clNone
      DropDownHeader.Font.Charset = DEFAULT_CHARSET
      DropDownHeader.Font.Color = 5263440
      DropDownHeader.Font.Height = -11
      DropDownHeader.Font.Name = 'Tahoma'
      DropDownHeader.Font.Style = []
      DropDownHeader.Visible = False
      DropDownHeader.BorderColor = 13948116
      DropDownHeader.Buttons = <>
      DropDownHeight = 0
      DropDownFooter.Color = clWhite
      DropDownFooter.ColorTo = clNone
      DropDownFooter.Font.Charset = DEFAULT_CHARSET
      DropDownFooter.Font.Color = 5263440
      DropDownFooter.Font.Height = -11
      DropDownFooter.Font.Name = 'Tahoma'
      DropDownFooter.Font.Style = []
      DropDownFooter.Visible = False
      DropDownFooter.BorderColor = 13948116
      DropDownFooter.Buttons = <>
      DropDownShadow = True
      DropDownSizable = True
      DropDownWidth = 0
      EmptyText = 'Search ...'
      EmptyTextFocused = False
      EmptyTextStyle = []
      FilterCondition.AutoSelect = True
      FocusFontColor = clBlack
      ItemHeight = 20
      Items = <>
      SearchButton.Appearance.BorderColor = 13948116
      SearchButton.Appearance.BorderColorHot = 15917525
      SearchButton.Appearance.BorderColorCheckedHot = 14925219
      SearchButton.Appearance.BorderColorDown = 14925219
      SearchButton.Appearance.BorderColorChecked = 15914434
      SearchButton.Appearance.BorderColorDisabled = 13948116
      SearchButton.Appearance.ColorTo = clNone
      SearchButton.Appearance.ColorChecked = 15914434
      SearchButton.Appearance.ColorCheckedTo = clNone
      SearchButton.Appearance.ColorDisabled = clWhite
      SearchButton.Appearance.ColorDisabledTo = clNone
      SearchButton.Appearance.ColorDown = 14925219
      SearchButton.Appearance.ColorDownTo = clNone
      SearchButton.Appearance.ColorHot = 15917525
      SearchButton.Appearance.ColorHotTo = clNone
      SearchButton.Appearance.ColorMirror = clWhite
      SearchButton.Appearance.ColorMirrorTo = clNone
      SearchButton.Appearance.ColorMirrorHot = 15917525
      SearchButton.Appearance.ColorMirrorHotTo = clNone
      SearchButton.Appearance.ColorMirrorDown = 14925219
      SearchButton.Appearance.ColorMirrorDownTo = clNone
      SearchButton.Appearance.ColorMirrorChecked = 15914434
      SearchButton.Appearance.ColorMirrorCheckedTo = clNone
      SearchButton.Appearance.ColorMirrorDisabled = clWhite
      SearchButton.Appearance.ColorMirrorDisabledTo = clNone
      SearchButton.Appearance.GradientHot = ggVertical
      SearchButton.Appearance.GradientMirrorHot = ggVertical
      SearchButton.Appearance.GradientDown = ggVertical
      SearchButton.Appearance.GradientMirrorDown = ggVertical
      SearchButton.Appearance.GradientChecked = ggVertical
      SearchButton.Appearance.TextColorChecked = 5263440
      SearchButton.Appearance.TextColorDown = 5263440
      SearchButton.Appearance.TextColorHot = 5263440
      SearchButton.Visible = False
      TabOrder = 2
      Text = ''
      Version = '1.0.9.0'
    end
    object ckSearchLimit: TCheckBox
      Left = 15
      Top = 263
      Width = 179
      Height = 17
      Caption = 'Limit search results to:'
      TabOrder = 3
      OnClick = ckSearchLimitClick
    end
  end
  object SpinEdit1: TSpinEdit
    Left = 192
    Top = 287
    Width = 80
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 10
    OnChange = SpinEdit1Change
  end
  object GroupBox2: TGroupBox
    Left = 295
    Top = 24
    Width = 549
    Height = 337
    Caption = 'Car lookup'
    TabOrder = 2
    DesignSize = (
      549
      337)
    object AdvSearchList2: TAdvSearchList
      Left = 16
      Top = 58
      Width = 377
      Height = 236
      HorzScrollBar.Tracking = True
      VertScrollBar.Increment = 20
      VertScrollBar.Range = 70
      VertScrollBar.Tracking = True
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = []
      TabOrder = 0
      Appearance.CategoryControlFont = False
      Appearance.CategoryFont.Charset = DEFAULT_CHARSET
      Appearance.CategoryFont.Color = clWindowText
      Appearance.CategoryFont.Height = -13
      Appearance.CategoryFont.Name = 'Verdana'
      Appearance.CategoryFont.Style = []
      Appearance.DescriptionFont.Charset = DEFAULT_CHARSET
      Appearance.DescriptionFont.Color = clWindowText
      Appearance.DescriptionFont.Height = -11
      Appearance.DescriptionFont.Name = 'Tahoma'
      Appearance.DescriptionFont.Style = []
      Appearance.DescriptionControlFont = False
      Appearance.FilterCount = fcShow
      Appearance.FilterCountFont.Charset = DEFAULT_CHARSET
      Appearance.FilterCountFont.Color = clWindowText
      Appearance.FilterCountFont.Height = -11
      Appearance.FilterCountFont.Name = 'Tahoma'
      Appearance.FilterCountFont.Style = []
      Appearance.FilterCountFormat = 'Match:%d  '
      Appearance.ItemCategoryFont.Charset = DEFAULT_CHARSET
      Appearance.ItemCategoryFont.Color = 42495
      Appearance.ItemCategoryFont.Height = -11
      Appearance.ItemCategoryFont.Name = 'Tahoma'
      Appearance.ItemCategoryFont.Style = []
      Appearance.ItemCategoryFormat = 'in %s'
      FilterCondition.AutoSelect = False
      BorderColor = clNone
      Categories = <>
      Columns = <
        item
          ControlFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          WordWrap = False
        end>
      ItemHeight = 70
      Items = <>
      Version = '1.0.7.1'
    end
    object CheckListBox1: TCheckListBox
      Left = 407
      Top = 58
      Width = 129
      Height = 143
      OnClickCheck = CheckListBox1ClickCheck
      ItemHeight = 13
      TabOrder = 1
    end
    object AdvSearchEdit2: TAdvSearchEdit
      Left = 16
      Top = 300
      Width = 377
      Height = 23
      Appearance.CategoryControlFont = False
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
      CategoryButton.Appearance.BorderColor = 13948116
      CategoryButton.Appearance.BorderColorHot = 15917525
      CategoryButton.Appearance.BorderColorCheckedHot = 14925219
      CategoryButton.Appearance.BorderColorDown = 14925219
      CategoryButton.Appearance.BorderColorChecked = 15914434
      CategoryButton.Appearance.BorderColorDisabled = 13948116
      CategoryButton.Appearance.ColorTo = clNone
      CategoryButton.Appearance.ColorChecked = 15914434
      CategoryButton.Appearance.ColorCheckedTo = clNone
      CategoryButton.Appearance.ColorDisabled = clWhite
      CategoryButton.Appearance.ColorDisabledTo = clNone
      CategoryButton.Appearance.ColorDown = 14925219
      CategoryButton.Appearance.ColorDownTo = clNone
      CategoryButton.Appearance.ColorHot = 15917525
      CategoryButton.Appearance.ColorHotTo = clNone
      CategoryButton.Appearance.ColorMirror = clWhite
      CategoryButton.Appearance.ColorMirrorTo = clNone
      CategoryButton.Appearance.ColorMirrorHot = 15917525
      CategoryButton.Appearance.ColorMirrorHotTo = clNone
      CategoryButton.Appearance.ColorMirrorDown = 14925219
      CategoryButton.Appearance.ColorMirrorDownTo = clNone
      CategoryButton.Appearance.ColorMirrorChecked = 15914434
      CategoryButton.Appearance.ColorMirrorCheckedTo = clNone
      CategoryButton.Appearance.ColorMirrorDisabled = clWhite
      CategoryButton.Appearance.ColorMirrorDisabledTo = clNone
      CategoryButton.Appearance.GradientHot = ggVertical
      CategoryButton.Appearance.GradientMirrorHot = ggVertical
      CategoryButton.Appearance.GradientDown = ggVertical
      CategoryButton.Appearance.GradientMirrorDown = ggVertical
      CategoryButton.Appearance.GradientChecked = ggVertical
      CategoryButton.Appearance.TextColorChecked = 5263440
      CategoryButton.Appearance.TextColorDown = 5263440
      CategoryButton.Appearance.TextColorHot = 5263440
      CategoryButton.Visible = False
      Columns = <
        item
          ControlFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          WordWrap = False
        end>
      DropDownHeader.Color = clWhite
      DropDownHeader.ColorTo = clNone
      DropDownHeader.Font.Charset = DEFAULT_CHARSET
      DropDownHeader.Font.Color = 5263440
      DropDownHeader.Font.Height = -11
      DropDownHeader.Font.Name = 'Tahoma'
      DropDownHeader.Font.Style = []
      DropDownHeader.Visible = False
      DropDownHeader.BorderColor = 13948116
      DropDownHeader.Buttons = <>
      DropDownHeight = 0
      DropDownFooter.Color = clWhite
      DropDownFooter.ColorTo = clNone
      DropDownFooter.Font.Charset = DEFAULT_CHARSET
      DropDownFooter.Font.Color = 5263440
      DropDownFooter.Font.Height = -11
      DropDownFooter.Font.Name = 'Tahoma'
      DropDownFooter.Font.Style = []
      DropDownFooter.Visible = False
      DropDownFooter.BorderColor = 13948116
      DropDownFooter.Buttons = <>
      DropDownShadow = True
      DropDownSizable = True
      DropDownWidth = 0
      EmptyText = 'Search ...'
      EmptyTextFocused = False
      EmptyTextStyle = []
      FilterCondition.AutoSelect = True
      FocusFontColor = clBlack
      ItemHeight = 20
      Items = <>
      SearchButton.Appearance.BorderColor = 13948116
      SearchButton.Appearance.BorderColorHot = 15917525
      SearchButton.Appearance.BorderColorCheckedHot = 14925219
      SearchButton.Appearance.BorderColorDown = 14925219
      SearchButton.Appearance.BorderColorChecked = 15914434
      SearchButton.Appearance.BorderColorDisabled = 13948116
      SearchButton.Appearance.ColorTo = clNone
      SearchButton.Appearance.ColorChecked = 15914434
      SearchButton.Appearance.ColorCheckedTo = clNone
      SearchButton.Appearance.ColorDisabled = clWhite
      SearchButton.Appearance.ColorDisabledTo = clNone
      SearchButton.Appearance.ColorDown = 14925219
      SearchButton.Appearance.ColorDownTo = clNone
      SearchButton.Appearance.ColorHot = 15917525
      SearchButton.Appearance.ColorHotTo = clNone
      SearchButton.Appearance.ColorMirror = clWhite
      SearchButton.Appearance.ColorMirrorTo = clNone
      SearchButton.Appearance.ColorMirrorHot = 15917525
      SearchButton.Appearance.ColorMirrorHotTo = clNone
      SearchButton.Appearance.ColorMirrorDown = 14925219
      SearchButton.Appearance.ColorMirrorDownTo = clNone
      SearchButton.Appearance.ColorMirrorChecked = 15914434
      SearchButton.Appearance.ColorMirrorCheckedTo = clNone
      SearchButton.Appearance.ColorMirrorDisabled = clWhite
      SearchButton.Appearance.ColorMirrorDisabledTo = clNone
      SearchButton.Appearance.GradientHot = ggVertical
      SearchButton.Appearance.GradientMirrorHot = ggVertical
      SearchButton.Appearance.GradientDown = ggVertical
      SearchButton.Appearance.GradientMirrorDown = ggVertical
      SearchButton.Appearance.GradientChecked = ggVertical
      SearchButton.Appearance.TextColorChecked = 5263440
      SearchButton.Appearance.TextColorDown = 5263440
      SearchButton.Appearance.TextColorHot = 5263440
      SearchButton.Visible = False
      TabOrder = 2
      Text = ''
      Version = '1.0.9.0'
    end
    object Edit2: TEdit
      Left = 16
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 3
      OnChange = Edit2Change
    end
    object ckDescr: TCheckBox
      Left = 160
      Top = 26
      Width = 121
      Height = 17
      Caption = 'Filter also description'
      TabOrder = 4
      OnClick = ckDescrClick
    end
    object Panel1: TPanel
      Left = 368
      Top = 22
      Width = 25
      Height = 24
      TabOrder = 5
      OnClick = Panel1Click
      object Shape1: TShape
        Left = 0
        Top = 0
        Width = 25
        Height = 24
        Hint = 'Highlight color'
        Brush.Color = clRed
        Enabled = False
        ParentShowHint = False
        ShowHint = True
      end
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 376
    Width = 836
    Height = 337
    Caption = 'TMS products lookup'
    TabOrder = 3
    object AdvSearchList3: TAdvSearchList
      Left = 15
      Top = 64
      Width = 665
      Height = 257
      HorzScrollBar.Tracking = True
      VertScrollBar.Range = 20
      VertScrollBar.Tracking = True
      TabOrder = 0
      Appearance.CategoryControlFont = False
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
      FilterCondition.AutoSelect = False
      BorderColor = clNone
      Categories = <>
      Columns = <
        item
          ControlFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          WordWrap = False
        end>
      Items = <>
      Version = '1.0.7.1'
    end
    object CheckListBox2: TCheckListBox
      Left = 694
      Top = 64
      Width = 129
      Height = 129
      OnClickCheck = CheckListBox2ClickCheck
      ItemHeight = 13
      Items.Strings = (
        'ALL'
        'VCL'
        'FMX'
        'FNC'
        'BIZ'
        'DEV'
        'IW'
        '.NET')
      TabOrder = 1
    end
    object AdvSearchEdit3: TAdvSearchEdit
      Left = 15
      Top = 24
      Width = 665
      Height = 26
      Appearance.CategoryControlFont = False
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
      CategoryButton.Appearance.BorderColor = 13948116
      CategoryButton.Appearance.BorderColorHot = 15917525
      CategoryButton.Appearance.BorderColorCheckedHot = 14925219
      CategoryButton.Appearance.BorderColorDown = 14925219
      CategoryButton.Appearance.BorderColorChecked = 15914434
      CategoryButton.Appearance.BorderColorDisabled = 13948116
      CategoryButton.Appearance.ColorTo = clNone
      CategoryButton.Appearance.ColorChecked = 15914434
      CategoryButton.Appearance.ColorCheckedTo = clNone
      CategoryButton.Appearance.ColorDisabled = clWhite
      CategoryButton.Appearance.ColorDisabledTo = clNone
      CategoryButton.Appearance.ColorDown = 14925219
      CategoryButton.Appearance.ColorDownTo = clNone
      CategoryButton.Appearance.ColorHot = 15917525
      CategoryButton.Appearance.ColorHotTo = clNone
      CategoryButton.Appearance.ColorMirror = clWhite
      CategoryButton.Appearance.ColorMirrorTo = clNone
      CategoryButton.Appearance.ColorMirrorHot = 15917525
      CategoryButton.Appearance.ColorMirrorHotTo = clNone
      CategoryButton.Appearance.ColorMirrorDown = 14925219
      CategoryButton.Appearance.ColorMirrorDownTo = clNone
      CategoryButton.Appearance.ColorMirrorChecked = 15914434
      CategoryButton.Appearance.ColorMirrorCheckedTo = clNone
      CategoryButton.Appearance.ColorMirrorDisabled = clWhite
      CategoryButton.Appearance.ColorMirrorDisabledTo = clNone
      CategoryButton.Appearance.GradientHot = ggVertical
      CategoryButton.Appearance.GradientMirrorHot = ggVertical
      CategoryButton.Appearance.GradientDown = ggVertical
      CategoryButton.Appearance.GradientMirrorDown = ggVertical
      CategoryButton.Appearance.GradientChecked = ggVertical
      CategoryButton.Appearance.TextColorChecked = 5263440
      CategoryButton.Appearance.TextColorDown = 5263440
      CategoryButton.Appearance.TextColorHot = 5263440
      CategoryButton.Picture.Data = {
        89504E470D0A1A0A0000000D494844520000001400000010080600000016185F
        1B0000000473424954080808087C086488000000097048597300000B1300000B
        1301009A9C180000012449444154388DDD924D4A034110855FF54C14DC4C22D9
        4802A38857D033487023222167D0A54710B20B7807C16DAEE02EA028B8F3274D
        B723593A2128429C2A373668D24946E3CAB77CAFF85E5114ADC5F189021DE20F
        2482160108D6E3D53680DA3C30166E6B6B7715806CF175A92E8CEB3978976FC3
        610300937336AAD5CA7B10761450F9E1668F19B065ADED018072C15D923C0590
        1D015E72C39807E0A0E660DF8000706FCC9508D501700E5EA614EDEB44DF7C35
        83D1A9B49FDE96A2624A84EDA9DB410EBAC69C8DFA63400078EEA79D52312A13
        68D3978BA0A5AD39F665E4335D99EF9DDC7B60C25994CFFC5426AC9A632D44CD
        49B059C05FE99F0145F81C85C2C3B4993027ABC7C247DADAD35983DE3F742A2F
        472B2C18A88570AFABF5459EE60F44E17257C6F2EDD50000000049454E44AE42
        6082}
      CategoryButton.Width = 30
      Columns = <
        item
          ControlFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          WordWrap = False
        end>
      DropDownHeader.Color = clWhite
      DropDownHeader.ColorTo = clNone
      DropDownHeader.Caption = 'TMS products overview'
      DropDownHeader.Font.Charset = DEFAULT_CHARSET
      DropDownHeader.Font.Color = 5263440
      DropDownHeader.Font.Height = -11
      DropDownHeader.Font.Name = 'Tahoma'
      DropDownHeader.Font.Style = []
      DropDownHeader.Visible = True
      DropDownHeader.BorderColor = 13948116
      DropDownHeader.Buttons = <>
      DropDownHeight = 0
      DropDownFooter.Color = clWhite
      DropDownFooter.ColorTo = clNone
      DropDownFooter.Font.Charset = DEFAULT_CHARSET
      DropDownFooter.Font.Color = 5263440
      DropDownFooter.Font.Height = -11
      DropDownFooter.Font.Name = 'Tahoma'
      DropDownFooter.Font.Style = []
      DropDownFooter.Visible = True
      DropDownFooter.BorderColor = 13948116
      DropDownFooter.Buttons = <>
      DropDownShadow = True
      DropDownSizable = True
      DropDownWidth = 0
      EmptyText = 'Search ...'
      EmptyTextFocused = False
      EmptyTextStyle = []
      FilterCondition.AutoSelect = True
      FocusFontColor = clBlack
      ItemHeight = 20
      Items = <>
      SearchButton.Appearance.BorderColor = 13948116
      SearchButton.Appearance.BorderColorHot = 15917525
      SearchButton.Appearance.BorderColorCheckedHot = 14925219
      SearchButton.Appearance.BorderColorDown = 14925219
      SearchButton.Appearance.BorderColorChecked = 15914434
      SearchButton.Appearance.BorderColorDisabled = 13948116
      SearchButton.Appearance.ColorTo = clNone
      SearchButton.Appearance.ColorChecked = 15914434
      SearchButton.Appearance.ColorCheckedTo = clNone
      SearchButton.Appearance.ColorDisabled = clWhite
      SearchButton.Appearance.ColorDisabledTo = clNone
      SearchButton.Appearance.ColorDown = 14925219
      SearchButton.Appearance.ColorDownTo = clNone
      SearchButton.Appearance.ColorHot = 15917525
      SearchButton.Appearance.ColorHotTo = clNone
      SearchButton.Appearance.ColorMirror = clWhite
      SearchButton.Appearance.ColorMirrorTo = clNone
      SearchButton.Appearance.ColorMirrorHot = 15917525
      SearchButton.Appearance.ColorMirrorHotTo = clNone
      SearchButton.Appearance.ColorMirrorDown = 14925219
      SearchButton.Appearance.ColorMirrorDownTo = clNone
      SearchButton.Appearance.ColorMirrorChecked = 15914434
      SearchButton.Appearance.ColorMirrorCheckedTo = clNone
      SearchButton.Appearance.ColorMirrorDisabled = clWhite
      SearchButton.Appearance.ColorMirrorDisabledTo = clNone
      SearchButton.Appearance.GradientHot = ggVertical
      SearchButton.Appearance.GradientMirrorHot = ggVertical
      SearchButton.Appearance.GradientDown = ggVertical
      SearchButton.Appearance.GradientMirrorDown = ggVertical
      SearchButton.Appearance.GradientChecked = ggVertical
      SearchButton.Appearance.TextColorChecked = 5263440
      SearchButton.Appearance.TextColorDown = 5263440
      SearchButton.Appearance.TextColorHot = 5263440
      SearchButton.Visible = False
      TabOrder = 2
      Text = ''
      Version = '1.0.9.0'
    end
  end
  object AdvSearchEdit4: TAdvSearchEdit
    Left = 208
    Top = 717
    Width = 300
    Height = 23
    Appearance.CategoryControlFont = False
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
    CategoryButton.Appearance.BorderColor = 13948116
    CategoryButton.Appearance.BorderColorHot = 15917525
    CategoryButton.Appearance.BorderColorCheckedHot = 14925219
    CategoryButton.Appearance.BorderColorDown = 14925219
    CategoryButton.Appearance.BorderColorChecked = 15914434
    CategoryButton.Appearance.BorderColorDisabled = 13948116
    CategoryButton.Appearance.ColorTo = clNone
    CategoryButton.Appearance.ColorChecked = 15914434
    CategoryButton.Appearance.ColorCheckedTo = clNone
    CategoryButton.Appearance.ColorDisabled = clWhite
    CategoryButton.Appearance.ColorDisabledTo = clNone
    CategoryButton.Appearance.ColorDown = 14925219
    CategoryButton.Appearance.ColorDownTo = clNone
    CategoryButton.Appearance.ColorHot = 15917525
    CategoryButton.Appearance.ColorHotTo = clNone
    CategoryButton.Appearance.ColorMirror = clWhite
    CategoryButton.Appearance.ColorMirrorTo = clNone
    CategoryButton.Appearance.ColorMirrorHot = 15917525
    CategoryButton.Appearance.ColorMirrorHotTo = clNone
    CategoryButton.Appearance.ColorMirrorDown = 14925219
    CategoryButton.Appearance.ColorMirrorDownTo = clNone
    CategoryButton.Appearance.ColorMirrorChecked = 15914434
    CategoryButton.Appearance.ColorMirrorCheckedTo = clNone
    CategoryButton.Appearance.ColorMirrorDisabled = clWhite
    CategoryButton.Appearance.ColorMirrorDisabledTo = clNone
    CategoryButton.Appearance.GradientHot = ggVertical
    CategoryButton.Appearance.GradientMirrorHot = ggVertical
    CategoryButton.Appearance.GradientDown = ggVertical
    CategoryButton.Appearance.GradientMirrorDown = ggVertical
    CategoryButton.Appearance.GradientChecked = ggVertical
    CategoryButton.Appearance.TextColorChecked = 5263440
    CategoryButton.Appearance.TextColorDown = 5263440
    CategoryButton.Appearance.TextColorHot = 5263440
    Columns = <
      item
        ControlFont = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        WordWrap = False
      end>
    DropDownHeader.Color = clWhite
    DropDownHeader.ColorTo = clNone
    DropDownHeader.Font.Charset = DEFAULT_CHARSET
    DropDownHeader.Font.Color = 5263440
    DropDownHeader.Font.Height = -11
    DropDownHeader.Font.Name = 'Tahoma'
    DropDownHeader.Font.Style = []
    DropDownHeader.Visible = True
    DropDownHeader.BorderColor = 13948116
    DropDownHeader.Buttons = <>
    DropDownHeight = 0
    DropDownFooter.Color = clWhite
    DropDownFooter.ColorTo = clNone
    DropDownFooter.Font.Charset = DEFAULT_CHARSET
    DropDownFooter.Font.Color = 5263440
    DropDownFooter.Font.Height = -11
    DropDownFooter.Font.Name = 'Tahoma'
    DropDownFooter.Font.Style = []
    DropDownFooter.Visible = True
    DropDownFooter.BorderColor = 13948116
    DropDownFooter.Buttons = <>
    DropDownShadow = True
    DropDownSizable = True
    DropDownWidth = 0
    EmptyText = 'Search ...'
    EmptyTextFocused = False
    EmptyTextStyle = []
    FilterCondition.AutoSelect = True
    FocusFontColor = clBlack
    ItemHeight = 20
    Items = <>
    SearchButton.Appearance.BorderColor = 13948116
    SearchButton.Appearance.BorderColorHot = 15917525
    SearchButton.Appearance.BorderColorCheckedHot = 14925219
    SearchButton.Appearance.BorderColorDown = 14925219
    SearchButton.Appearance.BorderColorChecked = 15914434
    SearchButton.Appearance.BorderColorDisabled = 13948116
    SearchButton.Appearance.ColorTo = clNone
    SearchButton.Appearance.ColorChecked = 15914434
    SearchButton.Appearance.ColorCheckedTo = clNone
    SearchButton.Appearance.ColorDisabled = clWhite
    SearchButton.Appearance.ColorDisabledTo = clNone
    SearchButton.Appearance.ColorDown = 14925219
    SearchButton.Appearance.ColorDownTo = clNone
    SearchButton.Appearance.ColorHot = 15917525
    SearchButton.Appearance.ColorHotTo = clNone
    SearchButton.Appearance.ColorMirror = clWhite
    SearchButton.Appearance.ColorMirrorTo = clNone
    SearchButton.Appearance.ColorMirrorHot = 15917525
    SearchButton.Appearance.ColorMirrorHotTo = clNone
    SearchButton.Appearance.ColorMirrorDown = 14925219
    SearchButton.Appearance.ColorMirrorDownTo = clNone
    SearchButton.Appearance.ColorMirrorChecked = 15914434
    SearchButton.Appearance.ColorMirrorCheckedTo = clNone
    SearchButton.Appearance.ColorMirrorDisabled = clWhite
    SearchButton.Appearance.ColorMirrorDisabledTo = clNone
    SearchButton.Appearance.GradientHot = ggVertical
    SearchButton.Appearance.GradientMirrorHot = ggVertical
    SearchButton.Appearance.GradientDown = ggVertical
    SearchButton.Appearance.GradientMirrorDown = ggVertical
    SearchButton.Appearance.GradientChecked = ggVertical
    SearchButton.Appearance.TextColorChecked = 5263440
    SearchButton.Appearance.TextColorDown = 5263440
    SearchButton.Appearance.TextColorHot = 5263440
    TabOrder = 4
    Text = ''
    Version = '1.0.9.0'
  end
  object ColorDialog1: TColorDialog
    Left = 768
    Top = 176
  end
  object AdvFormStyler1: TAdvFormStyler
    Style = tsOffice2016White
    Left = 784
    Top = 616
  end
end
