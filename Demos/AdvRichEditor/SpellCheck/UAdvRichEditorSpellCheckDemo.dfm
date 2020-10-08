object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'AdvRichEditor Demo AdvSpellCheck'
  ClientHeight = 536
  ClientWidth = 1069
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
  PixelsPerInch = 96
  TextHeight = 13
  object AdvDockPanel1: TAdvDockPanel
    Left = 0
    Top = 0
    Width = 1069
    Height = 43
    MinimumSize = 3
    LockHeight = False
    Persistence.Location = plRegistry
    Persistence.Enabled = False
    ToolBarStyler = AdvToolBarOfficeStyler1
    UseRunTimeHeight = False
    Version = '6.7.1.4'
    object AdvRichEditorEditToolBar1: TAdvRichEditorEditToolBar
      Left = 3
      Top = 1
      Width = 201
      Height = 28
      AllowFloating = True
      Caption = ''
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'Tahoma'
      CaptionFont.Style = []
      CompactImageIndex = -1
      ShowRightHandle = False
      TextAutoOptionMenu = 'Add or Remove Buttons'
      TextOptionMenu = 'Options'
      ToolBarStyler = AdvToolBarOfficeStyler1
      ParentOptionPicture = True
      ToolBarIndex = -1
      Hints.FileOpenTitle = 'Open (Ctrl+O)'
      Hints.FileOpenContent = 'Open new document from file'
      Hints.FileSaveTitle = 'Save (Ctrl+S)'
      Hints.FileSaveContent = 'Save document to file'
      Hints.CutTitle = 'Cut (Ctrl+X)'
      Hints.CutContent = 'Remove the selection to the clipboard'
      Hints.CopyTitle = 'Copy (Ctrl+C)'
      Hints.CopyContent = 'Put a copy of the selection on the clipboard'
      Hints.PasteTitle = 'Paste (Ctrl+V)'
      Hints.PasteContent = 'Add content on the clipboard to your document'
      Hints.UndoTitle = 'Undo (Ctrl+Z)'
      Hints.UndoContent = 'Undo typing'
      Hints.RedoTitle = 'Redo (Ctrl+Y)'
      Hints.RedoContent = 'Redo typing'
      Options = [btFileOpen, btFileSave, btCopy, btPaste, btCut, btUndo, btRedo]
    end
    object AdvRichEditorFormatToolBar1: TAdvRichEditorFormatToolBar
      Left = 206
      Top = 1
      Width = 716
      Height = 28
      AllowFloating = True
      Caption = ''
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'Tahoma'
      CaptionFont.Style = []
      CompactImageIndex = -1
      ShowRightHandle = False
      TextAutoOptionMenu = 'Add or Remove Buttons'
      TextOptionMenu = 'Options'
      ToolBarStyler = AdvToolBarOfficeStyler1
      ParentOptionPicture = True
      ToolBarIndex = -1
      RichEditor = AdvRichEditor1
      Hints.BoldTitle = 'Bold (Ctrl+B)'
      Hints.BoldContent = 'Select bold font style'
      Hints.ItalicTitle = 'Italic (Ctrl+I)'
      Hints.ItalicContent = 'Select italic font style'
      Hints.UnderlineTitle = 'Underline (Ctrl+U)'
      Hints.UnderlineContent = 'Select underline font style'
      Hints.StrikeThroughTitle = 'Strikethrough'
      Hints.StrikeThroughContent = 'Select strikethrough font style'
      Hints.SubScriptTitle = 'Subscript'
      Hints.SubScriptContent = 'Set subscript text'
      Hints.SuperScriptTitle = 'Superscript'
      Hints.SuperScriptContent = 'Set superscript text'
      Hints.InsertPictureTitle = 'Insert picture'
      Hints.InsertPictureContent = 'Insert a picture from file'
      Hints.InsertSpecialCharTitle = 'Insert special character'
      Hints.InsertSpecialCharContent = 'Insert a special character'
      Hints.BulletTitle = 'Insert bullet'
      Hints.BulletContent = 'Insert a bullet for list'
      Hints.NumberedBulletTitle = 'Start list'
      Hints.NumberedBulletContent = 'Start a numbered list'
      Hints.TextColorTitle = 'Text color'
      Hints.TextColorContent = 'Set selection text color'
      Hints.BackgroundColorTitle = 'Background color'
      Hints.BackgroundColorContent = 'Set selection background color'
      Hints.AlignLeftTitle = 'Align text left (Ctrl+L)'
      Hints.AlignLeftContent = 'Align the text to left'
      Hints.AlignCenterTitle = 'Align center (Ctrl+E)'
      Hints.AlignCenterContent = 'Center text'
      Hints.AlignRightTitle = 'Align text right (Ctrl+R)'
      Hints.AlignRightContent = 'Align the text to right'
      Hints.InsertHyperlinkTitle = 'Set hyperlink'
      Hints.InsertHyperlinkContent = 'Set hyperlink for text'
      Hints.IndentTitle = 'Increase indent'
      Hints.IndentContent = 'Increase the indent level of the paragraph'
      Hints.UnIndentTitle = 'Decrease indent'
      Hints.UnIndentContent = 'Decrease the indent level of the paragraph'
      Hints.FontSizeUpTitle = 'Increase font size'
      Hints.FontSizeUpContent = 'Make your text a bit bigger'
      Hints.FontSizeDownTitle = 'Decrease font size'
      Hints.FontSizeDownContent = 'Make your text a bit smaller'
      Options = [btBold, btItalic, btUnderline, btStrikeThrough, btSubscript, btSuperScript, btInsertPicture, btInsertspecialChar, btBullet, btNumberedBullet, btTextColor, btBackgroundColor, btAlignLeft, btAlignCenter, btAlignRight, btInsertHyperlink, btIndent, btUnindent]
    end
    object AdvRichEditorProofingToolBar1: TAdvRichEditorProofingToolBar
      Left = 924
      Top = 1
      Width = 109
      Height = 28
      AllowFloating = True
      Caption = ''
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'Tahoma'
      CaptionFont.Style = []
      CompactImageIndex = -1
      ShowRightHandle = False
      TextAutoOptionMenu = 'Add or Remove Buttons'
      TextOptionMenu = 'Options'
      ToolBarStyler = AdvToolBarOfficeStyler1
      ParentOptionPicture = True
      ToolBarIndex = -1
      RichEditor = AdvRichEditor1
      Hints.CheckDocumentTitle = 'Spelling'
      Hints.CheckDocumentContent = 'Check the spelling of the current document'
      Hints.ClearSpellCheckTitle = 'Clear spellcheck'
      Hints.ClearSpellCheckContent = 'Clear all marked spell check errors'
      Hints.ConfigSpellCheckTitle = 'Configuration'
      Hints.ConfigSpellCheckContent = 'Configure options and language for the spell check engine'
      Hints.SelectLanguageTitle = 'Language'
      Hints.SelectLanguageContent = 'Select language for the current document'
      Options = [btCheckDocument, btClearSpellCheck, btConfigureSpellCheck, btSelectLanguage]
      SpellCheck = AdvRichEditorSpellCheck1
    end
  end
  object AdvRichEditor1: TAdvRichEditor
    Left = 0
    Top = 43
    Width = 768
    Height = 425
    Cursor = crIBeam
    HorzScrollBar.Tracking = True
    VertScrollBar.Range = 6
    VertScrollBar.Tracking = True
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 1
    TabStop = True
    AutoCorrect.OldValue.Strings = (
      'BF')
    AutoCorrect.NewValue.Strings = (
      'Bruno Fierens')
    ParentFont = False
    Version = '1.8.0.0'
    OnCorrectWord = AdvRichEditor1CorrectWord
  end
  object Panel1: TPanel
    Left = 0
    Top = 468
    Width = 1069
    Height = 68
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      1069
      68)
    object Label1: TLabel
      Left = 16
      Top = 4
      Width = 159
      Height = 13
      Caption = 'Spell check engine during editing:'
    end
    object Label2: TLabel
      Left = 222
      Top = 4
      Width = 160
      Height = 13
      Caption = 'Document spell check correct via:'
    end
    object Label3: TLabel
      Left = 494
      Top = 6
      Width = 50
      Height = 13
      Caption = 'Load text:'
    end
    object Label4: TLabel
      Left = 185
      Top = 49
      Width = 148
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
    object Label5: TLabel
      Left = 16
      Top = 50
      Width = 163
      Height = 13
      Caption = 'For more information please visit: '
    end
    object autocorrect: TCheckBox
      Left = 95
      Top = 20
      Width = 97
      Height = 17
      Caption = 'Auto correct'
      TabOrder = 0
    end
    object spellcheck: TCheckBox
      Left = 16
      Top = 20
      Width = 73
      Height = 17
      Caption = 'Spell check'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object ComboBox1: TComboBox
      Left = 558
      Top = 6
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      OnChange = ComboBox1Change
      Items.Strings = (
        'English text'
        'German text'
        'French text'
        'Dutch text')
    end
    object RadioButton1: TRadioButton
      Left = 222
      Top = 20
      Width = 73
      Height = 17
      Caption = 'Panel'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RadioButton1Click
    end
    object RadioButton2: TRadioButton
      Left = 301
      Top = 20
      Width = 58
      Height = 17
      Caption = 'Dialog'
      TabOrder = 4
      OnClick = RadioButton2Click
    end
  end
  object AdvRichEditorSpellCheckPanel1: TAdvRichEditorSpellCheckPanel
    Left = 768
    Top = 43
    Width = 301
    Height = 425
    Align = alRight
    TabOrder = 4
    UI.ShowIgnore = True
    UI.ShowIgnoreAll = True
    UI.ShowAdd = True
    UI.ShowChange = True
    UI.ShowChangeAll = True
    UI.ShowNext = True
    UI.ShowPrevious = True
    UI.CaptionIgnore = '&Ignore'
    UI.CaptionIgnoreAll = 'I&gnore all'
    UI.CaptionAdd = '&Add'
    UI.CaptionChange = '&Change'
    UI.CaptionChangeAll = 'C&hange all'
    UI.CaptionNext = '&Next'
    UI.CaptionPrevious = '&Previous'
    UI.HintIgnore = 'Ignore current spell check error'
    UI.HintIgnoreAll = 'Ignore all spell check error'
    UI.HintAdd = 'Add new word to word list'
    UI.HintChange = 'Change spelling of current word'
    UI.HintChangeAll = 'Change spelling of all words'
    UI.HintNext = 'Go to next spell check error'
    UI.HintPrevious = 'Go to previous spell check error'
    UI.QueryAdd = 'Add new word'
    UI.QueryWord = 'Word'
    SpellCheck = AdvRichEditorSpellCheck1
    RichEditor = AdvRichEditor1
  end
  object AdvRichEditorSpellCheck1: TAdvRichEditorSpellCheck
    OnRequestsProcessed = AdvRichEditorSpellCheck1RequestsProcessed
    AutoUpdate = True
    Languages = <
      item
        AffixFileName = 'english.aff'
        SoundexProcess = spStandard
        SoundexName = 'ENGLISH'
        Guid = '{2D6A9E89-92B4-4BF6-B7BF-DAF810EB9A2E}'
        Enabled = True
        LanguageCode = lcEnglish
        SourceFileName = 'english.lat'
        Description = 'English Dictionary'
      end
      item
        AffixFileName = 'dutch.aff'
        SoundexProcess = spDutch
        SoundexName = 'DUTCH'
        Guid = '{319768EE-774D-45D2-AC56-BDEFBE665BC7}'
        Enabled = True
        LanguageCode = lcDutch
        SourceFileName = 'dutch.lat'
        Description = 'Dutch dictionary'
      end
      item
        AffixFileName = 'german.aff'
        SoundexProcess = spGerman
        SoundexName = 'GERMAN'
        Guid = '{301E99E1-9636-407C-AAD6-2288DADC4DF2}'
        Enabled = True
        LanguageCode = lcGerman
        SourceFileName = 'german.lat'
        Description = 'German dictionary'
      end
      item
        AffixFileName = 'french.aff'
        SoundexProcess = spFrench
        SoundexName = 'FRENCH'
        Guid = '{21E2151E-5504-4D62-9778-B42E4BF40A4D}'
        Enabled = True
        LanguageCode = lcFrench
        SourceFileName = 'french.lat'
        Description = 'French dictionary'
      end>
    InMemoryDatabaseName = 'TMSSPELLCHECK'
    DatabaseFilename = 'TMSSPELLCHECK'
    StoreElements = [sseSpellcheckDB, sseIgnoreList]
    Active = True
    Version = '1.0.6.5'
    MultiLanguageValidation = False
    ContextMenuIgnoreText = '&Ignore'
    RichEditor = AdvRichEditor1
    Left = 560
    Top = 144
  end
  object AdvToolBarOfficeStyler1: TAdvToolBarOfficeStyler
    AppColor.AppButtonColor = 13005312
    AppColor.AppButtonHoverColor = 16755772
    AppColor.TextColor = clWhite
    AppColor.HoverColor = 16246477
    AppColor.HoverTextColor = clBlack
    AppColor.HoverBorderColor = 15187578
    AppColor.SelectedColor = 15187578
    AppColor.SelectedTextColor = clBlack
    AppColor.SelectedBorderColor = 15187578
    Style = bsWindows10
    BorderColor = 14404026
    BorderColorHot = 14404026
    ButtonAppearance.Color = 16575452
    ButtonAppearance.ColorTo = 16571329
    ButtonAppearance.ColorChecked = 16645114
    ButtonAppearance.ColorCheckedTo = 16643051
    ButtonAppearance.ColorDown = 16575452
    ButtonAppearance.ColorDownTo = 16571329
    ButtonAppearance.ColorHot = 16645114
    ButtonAppearance.ColorHotTo = 16643051
    ButtonAppearance.BorderDownColor = 13542013
    ButtonAppearance.BorderHotColor = 16504504
    ButtonAppearance.BorderCheckedColor = 16504504
    ButtonAppearance.CaptionFont.Charset = DEFAULT_CHARSET
    ButtonAppearance.CaptionFont.Color = clWindowText
    ButtonAppearance.CaptionFont.Height = -12
    ButtonAppearance.CaptionFont.Name = 'Segoe UI'
    ButtonAppearance.CaptionFont.Style = []
    CaptionAppearance.CaptionColor = 16115676
    CaptionAppearance.CaptionColorTo = 16115676
    CaptionAppearance.CaptionTextColor = 10060659
    CaptionAppearance.CaptionColorHot = 16512491
    CaptionAppearance.CaptionColorHotTo = 16512491
    CaptionAppearance.CaptionTextColorHot = 10060659
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWindowText
    CaptionFont.Height = -11
    CaptionFont.Name = 'Segoe UI'
    CaptionFont.Style = []
    ContainerAppearance.LineColor = clBtnShadow
    ContainerAppearance.Line3D = True
    Color.Color = 16250613
    Color.ColorTo = 16250613
    Color.Direction = gdVertical
    Color.Mirror.Color = 16776699
    Color.Mirror.ColorTo = 16379877
    Color.Mirror.ColorMirror = 16379877
    Color.Mirror.ColorMirrorTo = 16115676
    ColorHot.Color = 16776699
    ColorHot.ColorTo = 16776699
    ColorHot.Direction = gdVertical
    ColorHot.Mirror.Color = 16777213
    ColorHot.Mirror.ColorTo = 16512491
    ColorHot.Mirror.ColorMirror = 16512491
    ColorHot.Mirror.ColorMirrorTo = 16512491
    CompactGlowButtonAppearance.BorderColor = 13811376
    CompactGlowButtonAppearance.BorderColorHot = 47103
    CompactGlowButtonAppearance.BorderColorDown = 2726850
    CompactGlowButtonAppearance.BorderColorChecked = 3904194
    CompactGlowButtonAppearance.Color = 16513270
    CompactGlowButtonAppearance.ColorTo = 16117478
    CompactGlowButtonAppearance.ColorChecked = 10014198
    CompactGlowButtonAppearance.ColorCheckedTo = 9425148
    CompactGlowButtonAppearance.ColorDisabled = clNone
    CompactGlowButtonAppearance.ColorDisabledTo = clNone
    CompactGlowButtonAppearance.ColorDown = 9161198
    CompactGlowButtonAppearance.ColorDownTo = 8046581
    CompactGlowButtonAppearance.ColorHot = 14807293
    CompactGlowButtonAppearance.ColorHotTo = 6934271
    CompactGlowButtonAppearance.ColorMirror = 15853791
    CompactGlowButtonAppearance.ColorMirrorTo = 16050913
    CompactGlowButtonAppearance.ColorMirrorHot = 13369343
    CompactGlowButtonAppearance.ColorMirrorHotTo = 6934271
    CompactGlowButtonAppearance.ColorMirrorDown = 9694195
    CompactGlowButtonAppearance.ColorMirrorDownTo = 5684213
    CompactGlowButtonAppearance.ColorMirrorChecked = 6210815
    CompactGlowButtonAppearance.ColorMirrorCheckedTo = 8640511
    CompactGlowButtonAppearance.ColorMirrorDisabled = clNone
    CompactGlowButtonAppearance.ColorMirrorDisabledTo = clNone
    CompactGlowButtonAppearance.GradientHot = ggVertical
    CompactGlowButtonAppearance.GradientMirrorHot = ggVertical
    CompactGlowButtonAppearance.GradientDown = ggVertical
    CompactGlowButtonAppearance.GradientMirrorDown = ggVertical
    CompactGlowButtonAppearance.GradientChecked = ggVertical
    DockColor.Color = 16250613
    DockColor.ColorTo = 16250613
    DockColor.Direction = gdHorizontal
    DockColor.Steps = 128
    FloatingWindowBorderColor = 13542013
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    GlowButtonAppearance.BorderColor = 16250613
    GlowButtonAppearance.BorderColorHot = 16371364
    GlowButtonAppearance.BorderColorDown = 14983778
    GlowButtonAppearance.BorderColorChecked = 14983778
    GlowButtonAppearance.BorderColorDisabled = 16250613
    GlowButtonAppearance.Color = 16250613
    GlowButtonAppearance.ColorTo = clNone
    GlowButtonAppearance.ColorChecked = 16244937
    GlowButtonAppearance.ColorCheckedTo = clNone
    GlowButtonAppearance.ColorDisabled = 16250613
    GlowButtonAppearance.ColorDisabledTo = clNone
    GlowButtonAppearance.ColorDown = 16244937
    GlowButtonAppearance.ColorDownTo = clNone
    GlowButtonAppearance.ColorHot = 16248808
    GlowButtonAppearance.ColorHotTo = clNone
    GlowButtonAppearance.ColorMirror = clNone
    GlowButtonAppearance.ColorMirrorTo = clNone
    GlowButtonAppearance.ColorMirrorHot = clNone
    GlowButtonAppearance.ColorMirrorHotTo = clNone
    GlowButtonAppearance.ColorMirrorDown = clNone
    GlowButtonAppearance.ColorMirrorDownTo = clNone
    GlowButtonAppearance.ColorMirrorChecked = clNone
    GlowButtonAppearance.ColorMirrorCheckedTo = clNone
    GlowButtonAppearance.ColorMirrorDisabled = clNone
    GlowButtonAppearance.ColorMirrorDisabledTo = clNone
    GlowButtonAppearance.GradientHot = ggVertical
    GlowButtonAppearance.GradientMirrorHot = ggVertical
    GlowButtonAppearance.GradientDown = ggVertical
    GlowButtonAppearance.GradientMirrorDown = ggVertical
    GlowButtonAppearance.GradientChecked = ggVertical
    GroupAppearance.Background = clInfoBk
    GroupAppearance.BorderColor = 47103
    GroupAppearance.Color = 47103
    GroupAppearance.ColorTo = 47103
    GroupAppearance.ColorMirror = 47103
    GroupAppearance.ColorMirrorTo = 47103
    GroupAppearance.Font.Charset = DEFAULT_CHARSET
    GroupAppearance.Font.Color = clWindowText
    GroupAppearance.Font.Height = -12
    GroupAppearance.Font.Name = 'Segoe UI'
    GroupAppearance.Font.Style = []
    GroupAppearance.Gradient = ggVertical
    GroupAppearance.GradientMirror = ggVertical
    GroupAppearance.TextColor = 5978398
    GroupAppearance.CaptionAppearance.CaptionColor = 16115676
    GroupAppearance.CaptionAppearance.CaptionColorTo = 16115676
    GroupAppearance.CaptionAppearance.CaptionTextColor = 10060659
    GroupAppearance.CaptionAppearance.CaptionColorHot = 16512491
    GroupAppearance.CaptionAppearance.CaptionColorHotTo = 16512491
    GroupAppearance.CaptionAppearance.CaptionTextColorHot = 10060659
    GroupAppearance.PageAppearance.BorderColor = 14404026
    GroupAppearance.PageAppearance.Color = 16776699
    GroupAppearance.PageAppearance.ColorTo = 16379877
    GroupAppearance.PageAppearance.ColorMirror = 16379877
    GroupAppearance.PageAppearance.ColorMirrorTo = 16115676
    GroupAppearance.PageAppearance.Gradient = ggVertical
    GroupAppearance.PageAppearance.GradientMirror = ggVertical
    GroupAppearance.PageAppearance.ShadowColor = clSilver
    GroupAppearance.PageAppearance.HighLightColor = clNone
    GroupAppearance.TabAppearance.BorderColor = 14404026
    GroupAppearance.TabAppearance.BorderColorHot = 47103
    GroupAppearance.TabAppearance.BorderColorSelected = 47103
    GroupAppearance.TabAppearance.BorderColorSelectedHot = 47103
    GroupAppearance.TabAppearance.BorderColorDisabled = clGray
    GroupAppearance.TabAppearance.BorderColorDown = 14404026
    GroupAppearance.TabAppearance.Color = clWhite
    GroupAppearance.TabAppearance.ColorTo = clWhite
    GroupAppearance.TabAppearance.ColorSelected = 16776699
    GroupAppearance.TabAppearance.ColorSelectedTo = 16776699
    GroupAppearance.TabAppearance.ColorDisabled = 16119285
    GroupAppearance.TabAppearance.ColorDisabledTo = 16119285
    GroupAppearance.TabAppearance.ColorHot = 16380138
    GroupAppearance.TabAppearance.ColorHotTo = 16380138
    GroupAppearance.TabAppearance.ColorMirror = clWhite
    GroupAppearance.TabAppearance.ColorMirrorTo = clWhite
    GroupAppearance.TabAppearance.ColorMirrorHot = 16380138
    GroupAppearance.TabAppearance.ColorMirrorHotTo = 16380138
    GroupAppearance.TabAppearance.ColorMirrorSelected = 16776699
    GroupAppearance.TabAppearance.ColorMirrorSelectedTo = 16776699
    GroupAppearance.TabAppearance.ColorMirrorDisabled = 16119285
    GroupAppearance.TabAppearance.ColorMirrorDisabledTo = 16119285
    GroupAppearance.TabAppearance.Font.Charset = DEFAULT_CHARSET
    GroupAppearance.TabAppearance.Font.Color = clWindowText
    GroupAppearance.TabAppearance.Font.Height = -11
    GroupAppearance.TabAppearance.Font.Name = 'Segoe UI'
    GroupAppearance.TabAppearance.Font.Style = []
    GroupAppearance.TabAppearance.Gradient = ggVertical
    GroupAppearance.TabAppearance.GradientMirror = ggVertical
    GroupAppearance.TabAppearance.GradientHot = ggVertical
    GroupAppearance.TabAppearance.GradientMirrorHot = ggVertical
    GroupAppearance.TabAppearance.GradientSelected = ggVertical
    GroupAppearance.TabAppearance.GradientMirrorSelected = ggVertical
    GroupAppearance.TabAppearance.GradientDisabled = ggVertical
    GroupAppearance.TabAppearance.GradientMirrorDisabled = ggVertical
    GroupAppearance.TabAppearance.TextColor = 5978398
    GroupAppearance.TabAppearance.TextColorHot = 5978398
    GroupAppearance.TabAppearance.TextColorSelected = 9126421
    GroupAppearance.TabAppearance.TextColorDisabled = clGray
    GroupAppearance.TabAppearance.ShadowColor = 16115676
    GroupAppearance.TabAppearance.HighLightColor = clNone
    GroupAppearance.TabAppearance.HighLightColorHot = clNone
    GroupAppearance.TabAppearance.HighLightColorSelected = clNone
    GroupAppearance.TabAppearance.HighLightColorSelectedHot = clNone
    GroupAppearance.TabAppearance.HighLightColorDown = clNone
    GroupAppearance.ToolBarAppearance.BorderColor = 13811376
    GroupAppearance.ToolBarAppearance.BorderColorHot = 13811376
    GroupAppearance.ToolBarAppearance.Color.Color = 16776699
    GroupAppearance.ToolBarAppearance.Color.ColorTo = 16115676
    GroupAppearance.ToolBarAppearance.Color.Direction = gdHorizontal
    GroupAppearance.ToolBarAppearance.ColorHot.Color = 16776699
    GroupAppearance.ToolBarAppearance.ColorHot.ColorTo = 16512491
    GroupAppearance.ToolBarAppearance.ColorHot.Direction = gdHorizontal
    PageAppearance.BorderColor = 14404026
    PageAppearance.Color = 16776699
    PageAppearance.ColorTo = 16379877
    PageAppearance.ColorMirror = 16379877
    PageAppearance.ColorMirrorTo = 16115676
    PageAppearance.Gradient = ggVertical
    PageAppearance.GradientMirror = ggVertical
    PageAppearance.ShadowColor = clSilver
    PageAppearance.HighLightColor = clNone
    PagerCaption.BorderColor = 14404026
    PagerCaption.Color = 16116191
    PagerCaption.ColorTo = 16116191
    PagerCaption.ColorMirror = 16116191
    PagerCaption.ColorMirrorTo = 16116191
    PagerCaption.Gradient = ggVertical
    PagerCaption.GradientMirror = ggVertical
    PagerCaption.TextColor = clGray
    PagerCaption.TextColorExtended = clBlue
    PagerCaption.Font.Charset = DEFAULT_CHARSET
    PagerCaption.Font.Color = clWindowText
    PagerCaption.Font.Height = -13
    PagerCaption.Font.Name = 'Segoe UI'
    PagerCaption.Font.Style = []
    QATAppearance.BorderColor = 13811634
    QATAppearance.Color = 16116191
    QATAppearance.ColorTo = 16116191
    QATAppearance.FullSizeBorderColor = 13811634
    QATAppearance.FullSizeColor = 16116191
    QATAppearance.FullSizeColorTo = 16116191
    RightHandleColor = 15527147
    RightHandleColorTo = 15527147
    RightHandleColorHot = 16248808
    RightHandleColorHotTo = 16248808
    RightHandleColorDown = 16244937
    RightHandleColorDownTo = 16244937
    TabAppearance.BorderColor = 14404026
    TabAppearance.BorderColorHot = 47103
    TabAppearance.BorderColorSelected = 14404026
    TabAppearance.BorderColorSelectedHot = 47103
    TabAppearance.BorderColorDisabled = clGray
    TabAppearance.BorderColorDown = 14404026
    TabAppearance.Color = clWhite
    TabAppearance.ColorTo = clWhite
    TabAppearance.ColorSelected = 16776699
    TabAppearance.ColorSelectedTo = 16776699
    TabAppearance.ColorDisabled = 16119285
    TabAppearance.ColorDisabledTo = 16119285
    TabAppearance.ColorHot = 16380138
    TabAppearance.ColorHotTo = 16380138
    TabAppearance.ColorMirror = clWhite
    TabAppearance.ColorMirrorTo = clWhite
    TabAppearance.ColorMirrorHot = 16380138
    TabAppearance.ColorMirrorHotTo = 16380138
    TabAppearance.ColorMirrorSelected = 16776699
    TabAppearance.ColorMirrorSelectedTo = 16776699
    TabAppearance.ColorMirrorDisabled = 16119285
    TabAppearance.ColorMirrorDisabledTo = 16119285
    TabAppearance.Font.Charset = DEFAULT_CHARSET
    TabAppearance.Font.Color = clWindowText
    TabAppearance.Font.Height = -11
    TabAppearance.Font.Name = 'Segoe UI'
    TabAppearance.Font.Style = []
    TabAppearance.Gradient = ggVertical
    TabAppearance.GradientMirror = ggVertical
    TabAppearance.GradientHot = ggVertical
    TabAppearance.GradientMirrorHot = ggVertical
    TabAppearance.GradientSelected = ggVertical
    TabAppearance.GradientMirrorSelected = ggVertical
    TabAppearance.GradientDisabled = ggVertical
    TabAppearance.GradientMirrorDisabled = ggVertical
    TabAppearance.TextColor = 5978398
    TabAppearance.TextColorHot = 5978398
    TabAppearance.TextColorSelected = 9126421
    TabAppearance.TextColorDisabled = clGray
    TabAppearance.ShadowColor = 16115676
    TabAppearance.HighLightColor = clNone
    TabAppearance.HighLightColorHot = clNone
    TabAppearance.HighLightColorSelected = clNone
    TabAppearance.HighLightColorSelectedHot = clNone
    TabAppearance.HighLightColorDown = clNone
    TabAppearance.BackGround.Color = 16116191
    TabAppearance.BackGround.ColorTo = 16116191
    TabAppearance.BackGround.Direction = gdHorizontal
    Left = 560
    Top = 264
  end
  object AdvOfficeHint1: TAdvOfficeHint
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    HintHelpText = 'Press F1 for more help.'
    Version = '1.8.1.0'
    Left = 560
    Top = 200
  end
  object AdvRichEditorSpellCheckDialog1: TAdvRichEditorSpellCheckDialog
    Caption = 'Correct words'
    SpellCheck = AdvRichEditorSpellCheck1
    UI.ShowIgnore = True
    UI.ShowIgnoreAll = True
    UI.ShowAdd = True
    UI.ShowChange = True
    UI.ShowChangeAll = True
    UI.ShowNext = True
    UI.ShowPrevious = True
    UI.CaptionIgnore = '&Ignore'
    UI.CaptionIgnoreAll = 'I&gnore all'
    UI.CaptionAdd = '&Add'
    UI.CaptionChange = '&Change'
    UI.CaptionChangeAll = 'C&hange all'
    UI.CaptionNext = '&Next'
    UI.CaptionPrevious = '&Previous'
    UI.HintIgnore = 'Ignore current spell check error'
    UI.HintIgnoreAll = 'Ignore all spell check error'
    UI.HintAdd = 'Add new word to word list'
    UI.HintChange = 'Change spelling of current word'
    UI.HintChangeAll = 'Change spelling of all words'
    UI.HintNext = 'Go to next spell check error'
    UI.HintPrevious = 'Go to previous spell check error'
    UI.QueryAdd = 'Add new word'
    UI.QueryWord = 'Word'
    RichEditor = AdvRichEditor1
    Left = 560
    Top = 328
  end
end
