object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'AdvSpellCheck Demo'
  ClientHeight = 568
  ClientWidth = 748
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
    748
    568)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 21
    Width = 309
    Height = 13
    Caption = 'Performs spell check for each word entered + spell check on exit'
  end
  object Label2: TLabel
    Left = 16
    Top = 77
    Width = 201
    Height = 13
    Caption = 'Performs auto correct of sentence on exit'
  end
  object Label3: TLabel
    Left = 16
    Top = 133
    Width = 339
    Height = 13
    Caption = 
      'Performs sentence correct on exit in TAdvSpellCheckCorrectLinesP' +
      'anel'
  end
  object Label4: TLabel
    Left = 16
    Top = 189
    Width = 342
    Height = 13
    Caption = 
      'Performs sentence correct on exit in TAdvSpellCheckCorrectLinesD' +
      'ialog'
  end
  object Label5: TLabel
    Left = 16
    Top = 239
    Width = 148
    Height = 13
    Caption = 'Rich edit control for spell check'
  end
  object Label7: TLabel
    Left = 185
    Top = 549
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
    OnClick = Label7Click
  end
  object Label6: TLabel
    Left = 16
    Top = 550
    Width = 163
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'For more information please visit: '
  end
  object Edit1: TEdit
    Left = 16
    Top = 40
    Width = 369
    Height = 21
    TabOrder = 0
    OnExit = Edit1Exit
    OnKeyPress = Edit1KeyPress
  end
  object Edit2: TEdit
    Left = 16
    Top = 96
    Width = 369
    Height = 21
    TabOrder = 1
    OnExit = Edit2Exit
  end
  object AdvSpellCheckCorrectLinesPanel1: TAdvSpellCheckCorrectLinesPanel
    Left = 416
    Top = 152
    Width = 305
    Height = 361
    TabOrder = 2
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
    SpellCheck = AdvSpellCheck1
    OnSpellCheckComplete = AdvSpellCheckCorrectLinesPanel1SpellCheckComplete
  end
  object Edit3: TEdit
    Left = 16
    Top = 152
    Width = 369
    Height = 21
    TabOrder = 3
    OnExit = Edit3Exit
  end
  object Edit4: TEdit
    Left = 16
    Top = 208
    Width = 369
    Height = 21
    TabOrder = 4
    OnExit = Edit4Exit
  end
  object RichEdit1: TRichEdit
    Left = 16
    Top = 258
    Width = 369
    Height = 255
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      
        'Wendsday, March 26 -- Too Atlantic bottle-nosed dolphins were pu' +
        't in to '
      
        'active duty today by the Navy. The two specially traned dolphins' +
        ' are '
      
        'searching the waters for explosives around the port citty of Umm' +
        ' Qasr.'
      
        'Makai, 33, and Tacoma, 22, both mails, use their natural ability' +
        'es to '
      
        'locate explosives and mark them with floats. The dolfins are wor' +
        'king to '
      'clear a path for ships carrying humanitarian aid to Iraq.'
      
        'The Navy has 20 trained dolphins as part of the Marine Mammal Pr' +
        'oject '
      
        'based in San Diego, California. Nine of those dolphins where flo' +
        'wn to the '
      
        'Persian Gulf recently. They are staying in specially bilt tanks ' +
        'abord a U.S. '
      'warship. '
      
        'The Navy dolphins are tauhgt to avoid touching the explasives. A' +
        'ccording '
      
        'to won bomb expert, the dolphins are more at risk from local dol' +
        'phins than '
      
        'the explosives. Dolphins are teritorial an cud drive away the tw' +
        'o '
      'newcomers.')
    ParentFont = False
    TabOrder = 5
    Zoom = 100
  end
  object Button1: TButton
    Left = 16
    Top = 519
    Width = 369
    Height = 25
    Caption = 'Spell check document'
    TabOrder = 6
    OnClick = Button1Click
  end
  object AdvSpellCheck1: TAdvSpellCheck
    AutoUpdate = True
    Languages = <
      item
        AffixFileName = 'english.aff'
        SoundexProcess = spStandard
        SoundexName = 'ENGLISH'
        Guid = '{6518498E-5E8F-4C52-A715-A135EE64752B}'
        Enabled = True
        LanguageCode = lcEnglish
        SourceFileName = 'english.lat'
        Description = 'English dictionary'
      end>
    InMemoryDatabaseName = 'TMSSPELLCHECK'
    DatabaseFilename = 'TMSSPELLCHECK'
    StoreElements = [sseSpellcheckDB, sseIgnoreList]
    Active = False
    Version = '1.0.6.1'
    MultiLanguageValidation = False
    Left = 520
    Top = 32
  end
  object AdvSpellCheckCorrectLinesDialog1: TAdvSpellCheckCorrectLinesDialog
    Caption = 'Correct'
    SpellCheck = AdvSpellCheck1
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
    Left = 624
    Top = 32
  end
end
