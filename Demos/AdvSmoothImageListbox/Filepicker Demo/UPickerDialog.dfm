object PickerDialog: TPickerDialog
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Dialog'
  ClientHeight = 692
  ClientWidth = 1041
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 648
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      201
      648)
    object Label1: TLabel
      Left = 15
      Top = 591
      Width = 66
      Height = 13
      Anchors = [akBottom]
      Caption = 'Preview size :'
    end
    object lblSize: TLabel
      Left = 91
      Top = 591
      Width = 18
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = '200'
    end
    object DriveComboBox1: TDriveComboBox
      Left = 10
      Top = 13
      Width = 181
      Height = 19
      DirList = DirectoryListBox1
      TabOrder = 1
    end
    object DirectoryListBox1: TDirectoryListBox
      Left = 10
      Top = 36
      Width = 181
      Height = 547
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      OnChange = DirectoryListBox1Change
    end
    object TrackBar1: TTrackBar
      Left = 3
      Top = 606
      Width = 195
      Height = 36
      Anchors = [akRight, akBottom]
      Max = 500
      Min = 64
      Frequency = 25
      Position = 200
      TabOrder = 2
      OnChange = TrackBar1Change
    end
  end
  object AdvSmoothImageListBox1: TAdvSmoothImageListBox
    Left = 201
    Top = 0
    Width = 840
    Height = 648
    ScrollType = stNormal
    ZoomAnimationFactor = 1.500000000000000000
    SelectedItemIndex = 0
    Items = <>
    TopLayerItems = <>
    ItemAppearance.ItemWidth = 200
    ItemAppearance.ItemHeight = 200
    ItemAppearance.Fill.Color = clWhite
    ItemAppearance.Fill.ColorTo = clBtnFace
    ItemAppearance.Fill.ColorMirror = clNone
    ItemAppearance.Fill.ColorMirrorTo = clNone
    ItemAppearance.Fill.GradientType = gtVertical
    ItemAppearance.Fill.GradientMirrorType = gtVertical
    ItemAppearance.Fill.BorderColor = clBlack
    ItemAppearance.Fill.Rounding = 0
    ItemAppearance.Fill.ShadowOffset = 0
    ItemAppearance.Fill.Glow = gmNone
    ItemAppearance.SelectedFill.Color = clInactiveCaption
    ItemAppearance.SelectedFill.ColorTo = clInactiveCaption
    ItemAppearance.SelectedFill.ColorMirror = clNone
    ItemAppearance.SelectedFill.ColorMirrorTo = clNone
    ItemAppearance.SelectedFill.GradientType = gtVertical
    ItemAppearance.SelectedFill.GradientMirrorType = gtVertical
    ItemAppearance.SelectedFill.BorderColor = clBlack
    ItemAppearance.SelectedFill.Rounding = 0
    ItemAppearance.SelectedFill.ShadowOffset = 0
    ItemAppearance.SelectedFill.Glow = gmNone
    ItemAppearance.DisabledFill.Color = 11974326
    ItemAppearance.DisabledFill.ColorTo = 11974326
    ItemAppearance.DisabledFill.ColorMirror = clNone
    ItemAppearance.DisabledFill.ColorMirrorTo = clNone
    ItemAppearance.DisabledFill.GradientType = gtVertical
    ItemAppearance.DisabledFill.GradientMirrorType = gtVertical
    ItemAppearance.DisabledFill.BorderColor = clBlack
    ItemAppearance.DisabledFill.Rounding = 0
    ItemAppearance.DisabledFill.ShadowOffset = 0
    ItemAppearance.DisabledFill.Glow = gmNone
    ItemAppearance.HoverFill.Color = clInactiveCaptionText
    ItemAppearance.HoverFill.ColorTo = clInactiveCaptionText
    ItemAppearance.HoverFill.ColorMirror = clNone
    ItemAppearance.HoverFill.ColorMirrorTo = clNone
    ItemAppearance.HoverFill.GradientType = gtVertical
    ItemAppearance.HoverFill.GradientMirrorType = gtVertical
    ItemAppearance.HoverFill.BorderColor = clBlack
    ItemAppearance.HoverFill.Rounding = 0
    ItemAppearance.HoverFill.ShadowOffset = 0
    ItemAppearance.HoverFill.Glow = gmNone
    ItemAppearance.Splitter.Spacing = 30
    ItemAppearance.Splitter.Size = 2
    ItemAppearance.Splitter.Fill.Color = clInactiveCaption
    ItemAppearance.Splitter.Fill.ColorTo = clInactiveCaption
    ItemAppearance.Splitter.Fill.ColorMirror = clNone
    ItemAppearance.Splitter.Fill.ColorMirrorTo = clNone
    ItemAppearance.Splitter.Fill.GradientType = gtHorizontal
    ItemAppearance.Splitter.Fill.GradientMirrorType = gtSolid
    ItemAppearance.Splitter.Fill.BorderColor = clBlack
    ItemAppearance.Splitter.Fill.Rounding = 0
    ItemAppearance.Splitter.Fill.ShadowOffset = 0
    ItemAppearance.Splitter.Fill.Glow = gmNone
    ItemAppearance.Splitter.TextFont.Charset = DEFAULT_CHARSET
    ItemAppearance.Splitter.TextFont.Color = clWhite
    ItemAppearance.Splitter.TextFont.Height = -16
    ItemAppearance.Splitter.TextFont.Name = 'Tahoma'
    ItemAppearance.Splitter.TextFont.Style = []
    ItemAppearance.Splitter.Expander = True
    ItemAppearance.Splitter.ExpanderColor = clBtnFace
    ItemAppearance.Splitter.ExpanderDownColor = clInactiveCaption
    ItemAppearance.Splitter.ExpanderHoverColor = clInactiveCaption
    ItemAppearance.Stretch = isShrinkOnly
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clBlack
    Header.Font.Height = -13
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Fill.Color = clBtnFace
    Header.Fill.ColorTo = clBtnFace
    Header.Fill.ColorMirror = clNone
    Header.Fill.ColorMirrorTo = clNone
    Header.Fill.GradientType = gtVertical
    Header.Fill.GradientMirrorType = gtSolid
    Header.Fill.BorderColor = clNone
    Header.Fill.Rounding = 0
    Header.Fill.ShadowOffset = 0
    Header.Fill.Glow = gmNone
    Header.Navigator.Visible = False
    Header.Navigator.Color = clBtnFace
    Header.Navigator.HintNext = 'Next Item'
    Header.Navigator.HintPrevious = 'Previous Item'
    Header.Navigator.HintNextPage = 'Next Page'
    Header.Navigator.HintPreviousPage = 'Previous Page'
    Header.Navigator.DisabledColor = clGray
    Header.Navigator.HoverColor = clInactiveCaption
    Header.Navigator.DownColor = clInactiveCaptionText
    Header.Navigator.BorderColor = clBlack
    Footer.Font.Charset = DEFAULT_CHARSET
    Footer.Font.Color = clBlack
    Footer.Font.Height = -13
    Footer.Font.Name = 'Tahoma'
    Footer.Font.Style = []
    Footer.Fill.Color = clBtnFace
    Footer.Fill.ColorTo = clBtnFace
    Footer.Fill.ColorMirror = clNone
    Footer.Fill.ColorMirrorTo = clNone
    Footer.Fill.GradientType = gtVertical
    Footer.Fill.GradientMirrorType = gtSolid
    Footer.Fill.BorderColor = clNone
    Footer.Fill.Rounding = 0
    Footer.Fill.ShadowOffset = 0
    Footer.Fill.Glow = gmNone
    Footer.Navigator.Visible = True
    Footer.Navigator.Color = clBtnFace
    Footer.Navigator.HintNext = 'hint next'
    Footer.Navigator.HintPrevious = 'hint previous'
    Footer.Navigator.HintNextPage = 'hint next page'
    Footer.Navigator.HintPreviousPage = 'hint previous page'
    Footer.Navigator.DisabledColor = clGray
    Footer.Navigator.HoverColor = clInactiveCaption
    Footer.Navigator.DownColor = clInactiveCaption
    Footer.Navigator.BorderColor = clBlack
    Fill.Color = 11974326
    Fill.ColorTo = 11974326
    Fill.ColorMirror = clNone
    Fill.ColorMirrorTo = clNone
    Fill.GradientType = gtVertical
    Fill.GradientMirrorType = gtSolid
    Fill.BorderColor = clNone
    Fill.Rounding = 0
    Fill.ShadowOffset = 0
    Fill.Glow = gmNone
    DefaultHTMLText.Location = cpTopLeft
    DefaultHTMLText.Font.Charset = DEFAULT_CHARSET
    DefaultHTMLText.Font.Color = clWindowText
    DefaultHTMLText.Font.Height = -11
    DefaultHTMLText.Font.Name = 'Tahoma'
    DefaultHTMLText.Font.Style = []
    ZoomMode = zmAspectRatio
    OnItemSelect = AdvSmoothImageListBox1ItemSelect
    Align = alClient
    TabOrder = 1
    ParentShowHint = False
    ShowHint = True
    TMSStyle = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 648
    Width = 1041
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      1041
      44)
    object Button1: TButton
      Left = 962
      Top = 6
      Width = 75
      Height = 31
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 881
      Top = 6
      Width = 75
      Height = 31
      Anchors = [akRight, akBottom]
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
