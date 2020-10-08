object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'DBAdvSearchList & DBAdvSearchEdit Demo'
  ClientHeight = 457
  ClientWidth = 592
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
  object Label1: TLabel
    Left = 16
    Top = 21
    Width = 48
    Height = 13
    Caption = 'Search ...'
  end
  object Label19: TLabel
    Left = 185
    Top = 434
    Width = 148
    Height = 13
    Cursor = crHandPoint
    Caption = 'https://www.tmssoftware.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label19Click
  end
  object Label18: TLabel
    Left = 16
    Top = 434
    Width = 163
    Height = 13
    Caption = 'For more information please visit: '
  end
  object DBAdvSearchList1: TDBAdvSearchList
    Left = 16
    Top = 67
    Width = 553
    Height = 305
    HorzScrollBar.Range = 517
    HorzScrollBar.Tracking = True
    VertScrollBar.Increment = 20
    VertScrollBar.Range = 3020
    VertScrollBar.Tracking = True
    TabOrder = 0
    Appearance.Banding = True
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
        DataField = 'NAME'
      end
      item
        ControlFont = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        DataField = 'COUNTRY'
      end
      item
        ControlFont = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        DataField = 'PEOPLE'
      end>
    Items = <
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Tokyo'#8211'Yokohama'
          end
          item
            Caption = 'Japan'
          end
          item
            Caption = '37843000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Jakarta'#160'(Jabodetabek)'
          end
          item
            Caption = 'Indonesia'
          end
          item
            Caption = '30539000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Delhi'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '24998000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Manila'#160'(Metro Manila)'
          end
          item
            Caption = 'Philippines'
          end
          item
            Caption = '24123000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Seoul'#8211'Gyeonggi'#8211'Incheon'#160'(Sudogwon)'
          end
          item
            Caption = 'South Korea'
          end
          item
            Caption = '23480000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Shanghai'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '23416000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Karachi'
          end
          item
            Caption = 'Pakistan'
          end
          item
            Caption = '22123000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Beijing'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '21009000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'New York City'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '20630000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Guangzhou'#8211'Foshan'#160'(Guangfo)'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '20597000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'S'#227'o Paulo'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '20365000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Mexico City'#160'(Valley of Mexico)'
          end
          item
            Caption = 'Mexico'
          end
          item
            Caption = '20063000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Mumbai'#160
          end
          item
            Caption = 'India'
          end
          item
            Caption = '17712000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Osaka'#8211'Kobe'#8211'Kyoto'#160'(Keihanshin)'
          end
          item
            Caption = 'Japan'
          end
          item
            Caption = '17444000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Moscow'
          end
          item
            Caption = 'Russia'
          end
          item
            Caption = '16170000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dhaka'
          end
          item
            Caption = 'Bangladesh'
          end
          item
            Caption = '15669000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Greater Cairo'
          end
          item
            Caption = 'Egypt'
          end
          item
            Caption = '15600000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Los Angeles'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '15058000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Bangkok'
          end
          item
            Caption = 'Thailand'
          end
          item
            Caption = '14998000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kolkata'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '14667000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Buenos Aires'
          end
          item
            Caption = 'Argentina'
          end
          item
            Caption = '14122000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Tehran'
          end
          item
            Caption = 'Iran'
          end
          item
            Caption = '13532000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Istanbul'
          end
          item
            Caption = 'Turkey'
          end
          item
            Caption = '13287000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Lagos'
          end
          item
            Caption = 'Nigeria'
          end
          item
            Caption = '13123000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Shenzhen'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '12084000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Rio de Janeiro'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '11727000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kinshasa'
          end
          item
            Caption = 'Democratic Republic of the Congo'
          end
          item
            Caption = '11587000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Tianjin'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '10920000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Paris'
          end
          item
            Caption = 'France'
          end
          item
            Caption = '10858000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Lima'
          end
          item
            Caption = 'Peru'
          end
          item
            Caption = '10750000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Chengdu'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '10376000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'London'
          end
          item
            Caption = 'United Kingdom'
          end
          item
            Caption = '10236000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Nagoya'#160
          end
          item
            Caption = 'Japan'
          end
          item
            Caption = '10177000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Lahore'
          end
          item
            Caption = 'Pakistan'
          end
          item
            Caption = '10052000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Chennai'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '9714000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Chicago'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '9156000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Bogot'#225
          end
          item
            Caption = 'Colombia'
          end
          item
            Caption = '8991000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Ho Chi Minh City'#160'(Saigon)'
          end
          item
            Caption = 'Vietnam'
          end
          item
            Caption = '8957000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Hyderabad'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '8754000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Bengaluru'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '8728906'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dongguan'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '8442000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Johannesburg'#8211'East Rand'
          end
          item
            Caption = 'South Africa'
          end
          item
            Caption = '8432000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Wuhan'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '7509000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Taipei'
          end
          item
            Caption = 'Taiwan'
          end
          item
            Caption = '7438000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Hangzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '7275000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Hong Kong'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '7246000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Chongqing'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '7217000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Ahmedabad'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '7186000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kuala Lumpur'#160'(Klang Valley)'
          end
          item
            Caption = 'Malaysia'
          end
          item
            Caption = '7088000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Quanzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '6710000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Essen'#8211'D'#252'sseldorf'#160'(Ruhr Area)'
          end
          item
            Caption = 'Germany'
          end
          item
            Caption = '6679000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Baghdad'
          end
          item
            Caption = 'Iraq'
          end
          item
            Caption = '6625000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Toronto'
          end
          item
            Caption = 'Canada'
          end
          item
            Caption = '6456000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Santiago'
          end
          item
            Caption = 'Chile'
          end
          item
            Caption = '6225000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dallas'#8211'Fort Worth'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '6174000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Madrid'
          end
          item
            Caption = 'Spain'
          end
          item
            Caption = '6171000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Nanjing'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '6155000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Shenyang'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '6078000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Xi'#39'an'#8211'Xianyang'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '5977000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'San Francisco'#8211'San Jose'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '5929000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Luanda'
          end
          item
            Caption = 'Angola'
          end
          item
            Caption = '5899000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Qingdao'#8211'Jimo'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '5816000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Houston'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '5764000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Miami'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '5764000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Bandung'
          end
          item
            Caption = 'Indonesia'
          end
          item
            Caption = '5695000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Riyadh'
          end
          item
            Caption = 'Saudi Arabia'
          end
          item
            Caption = '5666000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Pune'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '5631000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Singapore'
          end
          item
            Caption = 'Singapore'
          end
          item
            Caption = '5624000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Philadelphia'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '5570000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Surat'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '5447000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Milan'
          end
          item
            Caption = 'Italy'
          end
          item
            Caption = '5257000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Suzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '5246000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Saint Petersburg'
          end
          item
            Caption = 'Russia'
          end
          item
            Caption = '5126000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Khartoum'
          end
          item
            Caption = 'Sudan'
          end
          item
            Caption = '5125000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Atlanta'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '5015000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Zhengzhou'#8211'Xingyang'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '4942000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Washington, D.C.'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '4889000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Surabaya'
          end
          item
            Caption = 'Indonesia'
          end
          item
            Caption = '4881000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Harbin'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '4815000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Abidjan'
          end
          item
            Caption = 'Ivory Coast'
          end
          item
            Caption = '4800000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Yangon'#160'(Rangoon)'
          end
          item
            Caption = 'Myanmar'
          end
          item
            Caption = '4800000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Nairobi'
          end
          item
            Caption = 'Kenya'
          end
          item
            Caption = '4738000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Barcelona'
          end
          item
            Caption = 'Spain'
          end
          item
            Caption = '4693000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Alexandria'
          end
          item
            Caption = 'Egypt'
          end
          item
            Caption = '4689000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kabul'
          end
          item
            Caption = 'Afghanistan'
          end
          item
            Caption = '4635000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Guadalajara'
          end
          item
            Caption = 'Mexico'
          end
          item
            Caption = '4603000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Ankara'
          end
          item
            Caption = 'Turkey'
          end
          item
            Caption = '4538000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Belo Horizonte'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '4517000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Boston'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '4478000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Xiamen'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '4420000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kuwait City'
          end
          item
            Caption = 'Kuwait'
          end
          item
            Caption = '4283000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dar es Salaam'
          end
          item
            Caption = 'Tanzania'
          end
          item
            Caption = '4219000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Phoenix'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '4194000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dalian'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '4183000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Accra'
          end
          item
            Caption = 'Ghana'
          end
          item
            Caption = '4145000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Monterrey'
          end
          item
            Caption = 'Mexico'
          end
          item
            Caption = '4083000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Berlin'
          end
          item
            Caption = 'Germany'
          end
          item
            Caption = '4069000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Sydney'
          end
          item
            Caption = 'Australia'
          end
          item
            Caption = '4036000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Fuzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3962000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Medan'
          end
          item
            Caption = 'Indonesia'
          end
          item
            Caption = '3942000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dubai'
          end
          item
            Caption = 'United Arab Emirates'
          end
          item
            Caption = '3933000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Melbourne'
          end
          item
            Caption = 'Australia'
          end
          item
            Caption = '3906000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Rome'
          end
          item
            Caption = 'Italy'
          end
          item
            Caption = '3906000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Busan'
          end
          item
            Caption = 'South Korea'
          end
          item
            Caption = '3906000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Cape Town'
          end
          item
            Caption = 'South Africa'
          end
          item
            Caption = '3812000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Jinan'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3789000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Ningbo'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3753000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Hanoi'
          end
          item
            Caption = 'Vietnam'
          end
          item
            Caption = '3715000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Naples'
          end
          item
            Caption = 'Italy'
          end
          item
            Caption = '3706000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Taiyuan'#8212'Yuci'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3702000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Jeddah'
          end
          item
            Caption = 'Saudi Arabia'
          end
          item
            Caption = '3677000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Detroit'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '3672000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Hefei'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3665000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Changsha'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3657000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kunming'#8211'Anning'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3649000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Wuxi'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3597000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Medell'#237'n'
          end
          item
            Caption = 'Colombia'
          end
          item
            Caption = '3568000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Faisalabad'
          end
          item
            Caption = 'Pakistan'
          end
          item
            Caption = '3560000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Aleppo'
          end
          item
            Caption = 'Syria'
          end
          item
            Caption = '3560000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kano'
          end
          item
            Caption = 'Nigeria'
          end
          item
            Caption = '3550000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Montreal'
          end
          item
            Caption = 'Canada'
          end
          item
            Caption = '3407963'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dakar'
          end
          item
            Caption = 'Senegal'
          end
          item
            Caption = '3520000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Athens'
          end
          item
            Caption = 'Greece'
          end
          item
            Caption = '3484000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Changzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3425000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Durban'
          end
          item
            Caption = 'South Africa'
          end
          item
            Caption = '3421000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Porto Alegre'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '3413000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Jaipur'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '3409000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Fortaleza'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '3401000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Addis Ababa'
          end
          item
            Caption = 'Ethiopia'
          end
          item
            Caption = '3376000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Changchun'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3368000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Shijiazhuang'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3367000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Recife'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '3347000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Mashhad'
          end
          item
            Caption = 'Iran'
          end
          item
            Caption = '3294000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Seattle'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '3218000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Casablanca'
          end
          item
            Caption = 'Morocco'
          end
          item
            Caption = '3211000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Salvador'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '3190000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = #220'r'#252'mqi'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3184000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Lucknow'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '3184000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Chittagong'
          end
          item
            Caption = 'Bangladesh'
          end
          item
            Caption = '3176000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Wenzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3169000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Ibadan'
          end
          item
            Caption = 'Nigeria'
          end
          item
            Caption = '3160000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Izmir'
          end
          item
            Caption = 'Turkey'
          end
          item
            Caption = '3112000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Curitiba'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '3102000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'San Diego'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '3086000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Yaound'#233
          end
          item
            Caption = 'Cameroon'
          end
          item
            Caption = '3060000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Zhangjiagang'#8211'Jiangyin'#8211'Jingjiang'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3056000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kanpur'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '3037000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Zhongshan'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3031000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Sana'#39'a'
          end
          item
            Caption = 'Yemen'
          end
          item
            Caption = '2980000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Tel Aviv'
          end
          item
            Caption = 'Israel'
          end
          item
            Caption = '2979000'
          end>
      end>
    Version = '1.0.8.1'
    DataSource = DataSource1
  end
  object Edit1: TEdit
    Left = 16
    Top = 40
    Width = 209
    Height = 21
    TabOrder = 1
    OnChange = Edit1Change
  end
  object DBAdvSearchEdit1: TDBAdvSearchEdit
    Left = 16
    Top = 392
    Width = 553
    Height = 21
    Appearance.Banding = True
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
        DataField = 'NAME'
      end
      item
        ControlFont = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        DataField = 'COUNTRY'
      end
      item
        ControlFont = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        DataField = 'PEOPLE'
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
    Items = <
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Tokyo'#8211'Yokohama'
          end
          item
            Caption = 'Japan'
          end
          item
            Caption = '37843000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Jakarta'#160'(Jabodetabek)'
          end
          item
            Caption = 'Indonesia'
          end
          item
            Caption = '30539000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Delhi'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '24998000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Manila'#160'(Metro Manila)'
          end
          item
            Caption = 'Philippines'
          end
          item
            Caption = '24123000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Seoul'#8211'Gyeonggi'#8211'Incheon'#160'(Sudogwon)'
          end
          item
            Caption = 'South Korea'
          end
          item
            Caption = '23480000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Shanghai'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '23416000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Karachi'
          end
          item
            Caption = 'Pakistan'
          end
          item
            Caption = '22123000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Beijing'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '21009000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'New York City'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '20630000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Guangzhou'#8211'Foshan'#160'(Guangfo)'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '20597000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'S'#227'o Paulo'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '20365000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Mexico City'#160'(Valley of Mexico)'
          end
          item
            Caption = 'Mexico'
          end
          item
            Caption = '20063000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Mumbai'#160
          end
          item
            Caption = 'India'
          end
          item
            Caption = '17712000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Osaka'#8211'Kobe'#8211'Kyoto'#160'(Keihanshin)'
          end
          item
            Caption = 'Japan'
          end
          item
            Caption = '17444000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Moscow'
          end
          item
            Caption = 'Russia'
          end
          item
            Caption = '16170000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dhaka'
          end
          item
            Caption = 'Bangladesh'
          end
          item
            Caption = '15669000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Greater Cairo'
          end
          item
            Caption = 'Egypt'
          end
          item
            Caption = '15600000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Los Angeles'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '15058000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Bangkok'
          end
          item
            Caption = 'Thailand'
          end
          item
            Caption = '14998000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kolkata'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '14667000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Buenos Aires'
          end
          item
            Caption = 'Argentina'
          end
          item
            Caption = '14122000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Tehran'
          end
          item
            Caption = 'Iran'
          end
          item
            Caption = '13532000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Istanbul'
          end
          item
            Caption = 'Turkey'
          end
          item
            Caption = '13287000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Lagos'
          end
          item
            Caption = 'Nigeria'
          end
          item
            Caption = '13123000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Shenzhen'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '12084000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Rio de Janeiro'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '11727000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kinshasa'
          end
          item
            Caption = 'Democratic Republic of the Congo'
          end
          item
            Caption = '11587000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Tianjin'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '10920000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Paris'
          end
          item
            Caption = 'France'
          end
          item
            Caption = '10858000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Lima'
          end
          item
            Caption = 'Peru'
          end
          item
            Caption = '10750000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Chengdu'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '10376000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'London'
          end
          item
            Caption = 'United Kingdom'
          end
          item
            Caption = '10236000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Nagoya'#160
          end
          item
            Caption = 'Japan'
          end
          item
            Caption = '10177000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Lahore'
          end
          item
            Caption = 'Pakistan'
          end
          item
            Caption = '10052000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Chennai'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '9714000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Chicago'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '9156000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Bogot'#225
          end
          item
            Caption = 'Colombia'
          end
          item
            Caption = '8991000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Ho Chi Minh City'#160'(Saigon)'
          end
          item
            Caption = 'Vietnam'
          end
          item
            Caption = '8957000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Hyderabad'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '8754000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Bengaluru'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '8728906'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dongguan'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '8442000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Johannesburg'#8211'East Rand'
          end
          item
            Caption = 'South Africa'
          end
          item
            Caption = '8432000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Wuhan'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '7509000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Taipei'
          end
          item
            Caption = 'Taiwan'
          end
          item
            Caption = '7438000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Hangzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '7275000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Hong Kong'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '7246000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Chongqing'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '7217000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Ahmedabad'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '7186000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kuala Lumpur'#160'(Klang Valley)'
          end
          item
            Caption = 'Malaysia'
          end
          item
            Caption = '7088000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Quanzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '6710000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Essen'#8211'D'#252'sseldorf'#160'(Ruhr Area)'
          end
          item
            Caption = 'Germany'
          end
          item
            Caption = '6679000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Baghdad'
          end
          item
            Caption = 'Iraq'
          end
          item
            Caption = '6625000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Toronto'
          end
          item
            Caption = 'Canada'
          end
          item
            Caption = '6456000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Santiago'
          end
          item
            Caption = 'Chile'
          end
          item
            Caption = '6225000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dallas'#8211'Fort Worth'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '6174000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Madrid'
          end
          item
            Caption = 'Spain'
          end
          item
            Caption = '6171000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Nanjing'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '6155000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Shenyang'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '6078000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Xi'#39'an'#8211'Xianyang'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '5977000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'San Francisco'#8211'San Jose'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '5929000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Luanda'
          end
          item
            Caption = 'Angola'
          end
          item
            Caption = '5899000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Qingdao'#8211'Jimo'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '5816000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Houston'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '5764000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Miami'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '5764000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Bandung'
          end
          item
            Caption = 'Indonesia'
          end
          item
            Caption = '5695000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Riyadh'
          end
          item
            Caption = 'Saudi Arabia'
          end
          item
            Caption = '5666000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Pune'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '5631000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Singapore'
          end
          item
            Caption = 'Singapore'
          end
          item
            Caption = '5624000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Philadelphia'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '5570000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Surat'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '5447000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Milan'
          end
          item
            Caption = 'Italy'
          end
          item
            Caption = '5257000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Suzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '5246000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Saint Petersburg'
          end
          item
            Caption = 'Russia'
          end
          item
            Caption = '5126000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Khartoum'
          end
          item
            Caption = 'Sudan'
          end
          item
            Caption = '5125000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Atlanta'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '5015000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Zhengzhou'#8211'Xingyang'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '4942000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Washington, D.C.'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '4889000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Surabaya'
          end
          item
            Caption = 'Indonesia'
          end
          item
            Caption = '4881000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Harbin'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '4815000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Abidjan'
          end
          item
            Caption = 'Ivory Coast'
          end
          item
            Caption = '4800000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Yangon'#160'(Rangoon)'
          end
          item
            Caption = 'Myanmar'
          end
          item
            Caption = '4800000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Nairobi'
          end
          item
            Caption = 'Kenya'
          end
          item
            Caption = '4738000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Barcelona'
          end
          item
            Caption = 'Spain'
          end
          item
            Caption = '4693000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Alexandria'
          end
          item
            Caption = 'Egypt'
          end
          item
            Caption = '4689000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kabul'
          end
          item
            Caption = 'Afghanistan'
          end
          item
            Caption = '4635000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Guadalajara'
          end
          item
            Caption = 'Mexico'
          end
          item
            Caption = '4603000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Ankara'
          end
          item
            Caption = 'Turkey'
          end
          item
            Caption = '4538000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Belo Horizonte'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '4517000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Boston'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '4478000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Xiamen'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '4420000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kuwait City'
          end
          item
            Caption = 'Kuwait'
          end
          item
            Caption = '4283000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dar es Salaam'
          end
          item
            Caption = 'Tanzania'
          end
          item
            Caption = '4219000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Phoenix'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '4194000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dalian'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '4183000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Accra'
          end
          item
            Caption = 'Ghana'
          end
          item
            Caption = '4145000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Monterrey'
          end
          item
            Caption = 'Mexico'
          end
          item
            Caption = '4083000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Berlin'
          end
          item
            Caption = 'Germany'
          end
          item
            Caption = '4069000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Sydney'
          end
          item
            Caption = 'Australia'
          end
          item
            Caption = '4036000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Fuzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3962000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Medan'
          end
          item
            Caption = 'Indonesia'
          end
          item
            Caption = '3942000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dubai'
          end
          item
            Caption = 'United Arab Emirates'
          end
          item
            Caption = '3933000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Melbourne'
          end
          item
            Caption = 'Australia'
          end
          item
            Caption = '3906000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Rome'
          end
          item
            Caption = 'Italy'
          end
          item
            Caption = '3906000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Busan'
          end
          item
            Caption = 'South Korea'
          end
          item
            Caption = '3906000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Cape Town'
          end
          item
            Caption = 'South Africa'
          end
          item
            Caption = '3812000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Jinan'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3789000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Ningbo'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3753000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Hanoi'
          end
          item
            Caption = 'Vietnam'
          end
          item
            Caption = '3715000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Naples'
          end
          item
            Caption = 'Italy'
          end
          item
            Caption = '3706000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Taiyuan'#8212'Yuci'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3702000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Jeddah'
          end
          item
            Caption = 'Saudi Arabia'
          end
          item
            Caption = '3677000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Detroit'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '3672000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Hefei'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3665000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Changsha'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3657000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kunming'#8211'Anning'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3649000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Wuxi'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3597000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Medell'#237'n'
          end
          item
            Caption = 'Colombia'
          end
          item
            Caption = '3568000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Faisalabad'
          end
          item
            Caption = 'Pakistan'
          end
          item
            Caption = '3560000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Aleppo'
          end
          item
            Caption = 'Syria'
          end
          item
            Caption = '3560000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kano'
          end
          item
            Caption = 'Nigeria'
          end
          item
            Caption = '3550000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Montreal'
          end
          item
            Caption = 'Canada'
          end
          item
            Caption = '3407963'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Dakar'
          end
          item
            Caption = 'Senegal'
          end
          item
            Caption = '3520000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Athens'
          end
          item
            Caption = 'Greece'
          end
          item
            Caption = '3484000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Changzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3425000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Durban'
          end
          item
            Caption = 'South Africa'
          end
          item
            Caption = '3421000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Porto Alegre'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '3413000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Jaipur'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '3409000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Fortaleza'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '3401000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Addis Ababa'
          end
          item
            Caption = 'Ethiopia'
          end
          item
            Caption = '3376000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Changchun'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3368000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Shijiazhuang'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3367000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Recife'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '3347000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Mashhad'
          end
          item
            Caption = 'Iran'
          end
          item
            Caption = '3294000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Seattle'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '3218000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Casablanca'
          end
          item
            Caption = 'Morocco'
          end
          item
            Caption = '3211000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Salvador'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '3190000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = #220'r'#252'mqi'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3184000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Lucknow'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '3184000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Chittagong'
          end
          item
            Caption = 'Bangladesh'
          end
          item
            Caption = '3176000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Wenzhou'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3169000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Ibadan'
          end
          item
            Caption = 'Nigeria'
          end
          item
            Caption = '3160000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Izmir'
          end
          item
            Caption = 'Turkey'
          end
          item
            Caption = '3112000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Curitiba'
          end
          item
            Caption = 'Brazil'
          end
          item
            Caption = '3102000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'San Diego'
          end
          item
            Caption = 'United States of America'
          end
          item
            Caption = '3086000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Yaound'#233
          end
          item
            Caption = 'Cameroon'
          end
          item
            Caption = '3060000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Zhangjiagang'#8211'Jiangyin'#8211'Jingjiang'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3056000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Kanpur'
          end
          item
            Caption = 'India'
          end
          item
            Caption = '3037000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Zhongshan'
          end
          item
            Caption = 'China'
          end
          item
            Caption = '3031000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Sana'#39'a'
          end
          item
            Caption = 'Yemen'
          end
          item
            Caption = '2980000'
          end>
      end
      item
        CategoryID = 0
        Columns = <
          item
            Caption = 'Tel Aviv'
          end
          item
            Caption = 'Israel'
          end
          item
            Caption = '2979000'
          end>
      end>
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
    Version = '1.0.9.1'
    ListSource = DataSource1
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 432
    Top = 16
  end
  object ClientDataSet1: TClientDataSet
    PersistDataPacket.Data = {
      102300009619E0BD010000001800000005009600000003000000AC0702494404
      00010004000000044E414D450100490004000100055749445448020002003200
      07434F554E54525901004900040001000557494454480200020032000650454F
      504C450100490004000100055749445448020002003200075049435455524501
      0049000400010005574944544802000200320001000A4348414E47455F4C4F47
      04008200C2010000010000000000000004000000020000000000000004000000
      0300000000000000040000000400000000000000040000000500000000000000
      0400000006000000000000000400000007000000000000000400000008000000
      00000000040000000900000000000000040000000A0000000000000004000000
      0B00000000000000040000000C00000000000000040000000D00000000000000
      040000000E00000000000000040000000F000000000000000400000010000000
      0000000004000000110000000000000004000000120000000000000004000000
      1300000000000000040000001400000000000000040000001500000000000000
      0400000016000000000000000400000017000000000000000400000018000000
      00000000040000001900000000000000040000001A0000000000000004000000
      1B00000000000000040000001C00000000000000040000001D00000000000000
      040000001E00000000000000040000001F000000000000000400000020000000
      0000000004000000210000000000000004000000220000000000000004000000
      2300000000000000040000002400000000000000040000002500000000000000
      0400000026000000000000000400000027000000000000000400000028000000
      00000000040000002900000000000000040000002A0000000000000004000000
      2B00000000000000040000002C00000000000000040000002D00000000000000
      040000002E00000000000000040000002F000000000000000400000030000000
      0000000004000000310000000000000004000000320000000000000004000000
      3300000000000000040000003400000000000000040000003500000000000000
      0400000036000000000000000400000037000000000000000400000038000000
      00000000040000003900000000000000040000003A0000000000000004000000
      3B00000000000000040000003C00000000000000040000003D00000000000000
      040000003E00000000000000040000003F000000000000000400000040000000
      0000000004000000410000000000000004000000420000000000000004000000
      4300000000000000040000004400000000000000040000004500000000000000
      0400000046000000000000000400000047000000000000000400000048000000
      00000000040000004900000000000000040000004A0000000000000004000000
      4B00000000000000040000004C00000000000000040000004D00000000000000
      040000004E00000000000000040000004F000000000000000400000050000000
      0000000004000000510000000000000004000000520000000000000004000000
      5300000000000000040000005400000000000000040000005500000000000000
      0400000056000000000000000400000057000000000000000400000058000000
      00000000040000005900000000000000040000005A0000000000000004000000
      5B00000000000000040000005C00000000000000040000005D00000000000000
      040000005E00000000000000040000005F000000000000000400000060000000
      0000000004000000610000000000000004000000620000000000000004000000
      6300000000000000040000006400000000000000040000006500000000000000
      0400000066000000000000000400000067000000000000000400000068000000
      00000000040000006900000000000000040000006A0000000000000004000000
      6B00000000000000040000006C00000000000000040000006D00000000000000
      040000006E00000000000000040000006F000000000000000400000070000000
      0000000004000000710000000000000004000000720000000000000004000000
      7300000000000000040000007400000000000000040000007500000000000000
      0400000076000000000000000400000077000000000000000400000078000000
      00000000040000007900000000000000040000007A0000000000000004000000
      7B00000000000000040000007C00000000000000040000007D00000000000000
      040000007E00000000000000040000007F000000000000000400000080000000
      0000000004000000810000000000000004000000820000000000000004000000
      8300000000000000040000008400000000000000040000008500000000000000
      0400000086000000000000000400000087000000000000000400000088000000
      00000000040000008900000000000000040000008A0000000000000004000000
      8B00000000000000040000008C00000000000000040000008D00000000000000
      040000008E00000000000000040000008F000000000000000400000090000000
      0000000004000000910000000000000004000000920000000000000004000000
      9300000000000000040000009400000000000000040000009500000000000000
      04000000960000000000000004000000040000010000000E546F6B796F96596F
      6B6F68616D61054A6170616E08333738343330303009746F6B796F2E6A706704
      000002000000154A616B61727461A0284A61626F6465746162656B2909496E64
      6F6E657369610833303533393030300B6A616B617274612E6A70670400000300
      00000544656C686905496E6469610832343939383030300964656C68692E6A70
      6704000004000000154D616E696C61A0284D6574726F204D616E696C61290B50
      68696C697070696E65730832343132333030300B6D616E696C6C612E6A706704
      0000050000002153656F756C964779656F6E67676996496E6368656F6EA02853
      75646F67776F6E290B536F757468204B6F726561083233343830303030097365
      6F756C2E6A706704000006000000085368616E67686169054368696E61083233
      3431363030300C7368616E676861692E6A706704000007000000074B61726163
      68690850616B697374616E0832323132333030300B6B6172616368692E6A7067
      04000008000000074265696A696E67054368696E610832313030393030300B62
      65696A696E672E6A7067040000090000000D4E657720596F726B204369747918
      556E6974656420537461746573206F6620416D65726963610832303633303030
      30076E65772E6A70670400000A0000001A4775616E677A686F7596466F736861
      6EA0284775616E67666F29054368696E610832303539373030300D6775616E67
      7A686F752E6A70670400000B0000000953E36F205061756C6F064272617A696C
      0832303336353030300773E36F2E6A70670400000C0000001E4D657869636F20
      43697479A02856616C6C6579206F66204D657869636F29064D657869636F0832
      303036333030300A6D657869636F2E6A70670400000D000000074D756D626169
      A005496E6469610831373731323030300A6D756D6261692E6A70670400000E00
      00001D4F73616B61964B6F6265964B796F746FA0284B656968616E7368696E29
      054A6170616E083137343434303030096F73616B612E6A70670400000F000000
      064D6F73636F77065275737369610831363137303030300A6D6F73636F772E6A
      706704000010000000054468616B610A42616E676C6164657368083135363639
      303030096468616B612E6A7067040000110000000D4772656174657220436169
      726F0545677970740831353630303030300B677265617465722E6A7067040000
      120000000B4C6F7320416E67656C657318556E6974656420537461746573206F
      6620416D6572696361083135303538303030076C6F732E6A7067040000130000
      000742616E676B6F6B08546861696C616E640831343939383030300B62616E67
      6B6F6B2E6A706704000014000000074B6F6C6B61746105496E64696108313436
      36373030300B6B6F6C6B6174612E6A7067040000150000000C4275656E6F7320
      416972657309417267656E74696E610831343132323030300A6275656E6F732E
      6A7067040000170000000654656872616E044972616E0831333533323030300A
      74656872616E2E6A70670400001700000008497374616E62756C065475726B65
      790831333238373030300C697374616E62756C2E6A706704000018000000054C
      61676F73074E696765726961083133313233303030096C61676F732E6A706704
      000019000000085368656E7A68656E054368696E610831323038343030300C73
      68656E7A68656E2E6A70670400001A0000000E52696F206465204A616E656972
      6F064272617A696C0831313732373030300772696F2E6A70670400001B000000
      084B696E73686173612044656D6F6372617469632052657075626C6963206F66
      2074686520436F6E676F0831313538373030300C6B696E73686173612E6A7067
      0400001C000000075469616E6A696E054368696E610831303932303030300B74
      69616E6A696E2E6A70670400001D000000055061726973064672616E63650831
      303835383030300970617269732E6A70670400001E000000044C696D61045065
      7275083130373530303030086C696D612E6A70670400001F000000074368656E
      676475054368696E610831303337363030300B6368656E6764752E6A70670400
      0020000000064C6F6E646F6E0E556E69746564204B696E67646F6D0831303233
      363030300A6C6F6E646F6E2E6A706704000021000000074E61676F7961A0054A
      6170616E0831303137373030300A6E61676F79612E6A70670400002200000006
      4C61686F72650850616B697374616E0831303035323030300A6C61686F72652E
      6A706704000023000000074368656E6E616905496E6469610739373134303030
      0B6368656E6E61692E6A706704000024000000074368696361676F18556E6974
      656420537461746573206F6620416D657269636107393135363030300B636869
      6361676F2E6A70670400002500000006426F676F74E108436F6C6F6D62696107
      383939313030300A626F676F74E12E6A70670400002600000019486F20436869
      204D696E682043697479A028536169676F6E2907566965746E616D0738393537
      30303006686F2E6A7067040000270000000948796465726162616405496E6469
      6107383735343030300D6879646572616261642E6A7067040000290000000942
      656E67616C75727505496E64696107383732383930360D62656E67616C757275
      2E6A70670400002900000008446F6E676775616E054368696E61073834343230
      30300C646F6E676775616E2E6A70670400002A000000164A6F68616E6E657362
      75726796456173742052616E640C536F75746820416672696361073834333230
      3030106A6F68616E6E6573627572672E6A70670400002B00000005577568616E
      054368696E61073735303930303009777568616E2E6A70670400002C00000006
      5461697065690654616977616E07373433383030300A7461697065692E6A7067
      0400002D0000000848616E677A686F75054368696E6107373237353030300C68
      616E677A686F752E6A70670400002E00000009486F6E67204B6F6E6705436869
      6E61073732343630303008686F6E672E6A70670400002F0000000943686F6E67
      71696E67054368696E6107373231373030300D63686F6E6771696E672E6A7067
      040000300000000941686D65646162616405496E64696107373138363030300D
      61686D6564616261642E6A7067040000310000001B4B75616C61204C756D7075
      72A0284B6C616E672056616C6C657929084D616C617973696107373038383030
      300F6B75616C616C756D7075722E6A706704000032000000085175616E7A686F
      75054368696E6107363731303030300C7175616E7A686F752E6A706704000033
      0000001C457373656E9644FC7373656C646F7266A02852756872204172656129
      074765726D616E79073636373930303013657373656E64757373656C64757266
      2E6A706704000034000000074261676864616404497261710736363235303030
      0B626167686461642E6A70670400003500000007546F726F6E746F0643616E61
      646107363435363030300B746F726F6E746F2E6A706704000036000000085361
      6E746961676F054368696C6507363232353030300C73616E746961676F2E6A70
      67040000370000001144616C6C617396466F727420576F72746818556E697465
      6420537461746573206F6620416D657269636107363137343030300A64616C6C
      61732E6A706704000038000000064D616472696405537061696E073631373130
      30300A6D61647269642E6A706704000039000000074E616E6A696E6705436869
      6E6107363135353030300B6E616E6A696E672E6A70670400003A000000085368
      656E79616E67054368696E6107363037383030300C7368656E79616E672E6A70
      670400003B0000000E586927616E965869616E79616E67054368696E61073539
      373730303009786927616E2E6A70670400003C0000001653616E204672616E63
      6973636F9653616E204A6F736518556E6974656420537461746573206F662041
      6D657269636107353932393030301173616E206672616E636973636F2E6A7067
      0400003D000000064C75616E646106416E676F6C6107353839393030300A6C75
      616E64612E6A70670400003E0000000C51696E6764616F964A696D6F05436869
      6E6107353831363030300B71696E6764616F2E6A70670400003F00000007486F
      7573746F6E18556E6974656420537461746573206F6620416D65726963610735
      3736343030300B686F7573746F6E2E6A70670400003F000000054D69616D6918
      556E6974656420537461746573206F6620416D65726963610735373634303030
      096D69616D692E6A7067040000410000000742616E64756E6709496E646F6E65
      73696107353639353030300B62616E64756E672E6A7067040000420000000652
      69796164680C53617564692041726162696107353636363030300A7269796164
      682E6A7067040000430000000450756E6505496E646961073536333130303008
      70756E652E6A7067040000440000000953696E6761706F72650953696E676170
      6F726507353632343030300D73696E6761706F72652E6A706704000045000000
      0C5068696C6164656C7068696118556E6974656420537461746573206F662041
      6D65726963610735353730303030107068696C6164656C706869612E6A706704
      00004600000005537572617405496E6469610735343437303030097375726174
      2E6A706704000047000000054D696C616E054974616C79073532353730303009
      6D696C616E2E6A7067040000480000000653757A686F75054368696E61073532
      34363030300A73757A686F752E6A706704000049000000105361696E74205065
      7465727362757267065275737369610735313236303030097361696E742E6A70
      670400004A000000084B686172746F756D05537564616E07353132353030300C
      6B686172746F756D2E6A70670400004B0000000741746C616E746118556E6974
      656420537461746573206F6620416D657269636107353031353030300B61746C
      616E74612E6A70670400004C000000125A68656E677A686F759658696E677961
      6E67054368696E6107343934323030300D7A68656E677A686F752E6A70670400
      004D0000001057617368696E67746F6E2C20442E432E18556E69746564205374
      61746573206F6620416D657269636107343838393030300E77617368696E6774
      6F6E2E6A70670400004E00000008537572616261796109496E646F6E65736961
      07343838313030300C73757261626179612E6A70670400004F00000006486172
      62696E054368696E6107343831353030300A68617262696E2E6A706704000050
      00000007416269646A616E0B49766F727920436F61737407343830303030300B
      616269646A616E2E6A7067040000500000001059616E676F6EA02852616E676F
      6F6E29074D79616E6D617207343830303030300A79616E676F6E2E6A70670400
      0052000000074E6169726F6269054B656E796107343733383030300B6E616972
      6F62692E6A7067040000530000000942617263656C6F6E6105537061696E0734
      3639333030300D62617263656C6F6E612E6A7067040000540000000A416C6578
      616E6472696105456779707407343638393030300E616C6578616E647269612E
      6A706704000055000000054B6162756C0B41666768616E697374616E07343633
      35303030096B6162756C2E6A7067040000560000000B47756164616C616A6172
      61064D657869636F07343630333030300F67756164616C616A6172612E6A7067
      0400005700000006416E6B617261065475726B657907343533383030300A616E
      6B6172612E6A7067040000580000000E42656C6F20486F72697A6F6E74650642
      72617A696C07343531373030300862656C6F2E6A70670400005900000006426F
      73746F6E18556E6974656420537461746573206F6620416D6572696361073434
      37383030300A626F73746F6E2E6A70670400005A000000065869616D656E0543
      68696E6107343432303030300A7869616D656E2E6A70670400005B0000000B4B
      75776169742043697479064B757761697407343238333030300A6B7577616974
      2E6A70670400005C0000000D4461722065732053616C61616D0854616E7A616E
      69610734323139303030076461722E6A70670400005D0000000750686F656E69
      7818556E6974656420537461746573206F6620416D6572696361073431393430
      30300B70686F656E69782E6A70670400005E0000000644616C69616E05436869
      6E6107343138333030300A64616C69616E2E6A70670400005F00000005416363
      7261054768616E6107343134353030300961636372612E6A7067040000600000
      00094D6F6E746572726579064D657869636F07343038333030300D6D6F6E7465
      727265792E6A706704000061000000064265726C696E074765726D616E790734
      3036393030300A6265726C696E2E6A706704000062000000065379646E657909
      4175737472616C696107343033363030300A7379646E65792E6A706704000063
      0000000646757A686F75054368696E6107333936323030300A66757A686F752E
      6A706704000064000000054D6564616E09496E646F6E65736961073339343230
      3030096D6564616E2E6A70670400006500000005447562616914556E69746564
      204172616220456D69726174657307333933333030300964756261692E6A7067
      04000066000000094D656C626F75726E65094175737472616C69610733393036
      3030300D6D656C626F75726E652E6A70670400006600000004526F6D65054974
      616C79073339303630303008726F6D652E6A7067040000660000000542757361
      6E0B536F757468204B6F726561073339303630303009627573616E2E6A706704
      000069000000094361706520546F776E0C536F75746820416672696361073338
      313230303008636170652E6A70670400006A000000054A696E616E054368696E
      610733373839303030096A696E616E2E6A70670400006B000000064E696E6762
      6F054368696E6107333735333030300A6E696E67626F2E6A70670400006C0000
      000548616E6F6907566965746E616D07333731353030300968616E6F692E6A70
      670400006D000000064E61706C6573054974616C7907333730363030300A6E61
      706C65732E6A70670400006E0000000C5461697975616E975975636905436869
      6E6107333730323030300B7461697975616E2E6A70670400006F000000064A65
      646461680C53617564692041726162696107333637373030300A6A6564646168
      2E6A70670400007000000007446574726F697418556E69746564205374617465
      73206F6620416D657269636107333637323030300B646574726F69742E6A7067
      04000071000000054865666569054368696E6107333636353030300968656665
      692E6A706704000072000000084368616E67736861054368696E610733363537
      3030300C6368616E677368612E6A7067040000730000000E4B756E6D696E6796
      416E6E696E67054368696E6107333634393030300B6B756E6D696E672E6A7067
      040000740000000457757869054368696E61073335393730303008777578692E
      6A706704000075000000084D6564656C6CED6E08436F6C6F6D62696107333536
      383030300C6D6564656C6CED6E2E6A7067040000760000000A46616973616C61
      6261640850616B697374616E07333536303030300E66616973616C616261642E
      6A70670400007600000006416C6570706F05537972696107333536303030300A
      616C6570706F2E6A706704000078000000044B616E6F074E6967657269610733
      353530303030086B616E6F2E6A706704000079000000084D6F6E747265616C06
      43616E61646107333430373936330C6D6F6E747265616C2E6A70670400007A00
      00000544616B61720753656E6567616C07333532303030300964616B61722E6A
      70670400007B00000006417468656E730647726565636507333438343030300A
      617468656E732E6A70670400007C000000094368616E677A686F75054368696E
      6107333432353030300D6368616E677A686F752E6A70670400007D0000000644
      757262616E0C536F7574682041667269636107333432313030300A6475726261
      6E2E6A70670400007E0000000C506F72746F20416C65677265064272617A696C
      073334313330303009706F72746F2E6A70670400007F000000064A6169707572
      05496E64696107333430393030300A6A61697075722E6A706704000080000000
      09466F7274616C657A61064272617A696C07333430313030300D666F7274616C
      657A612E6A7067040000810000000B416464697320416261626108457468696F
      70696107333337363030300961646469732E6A70670400008200000009436861
      6E676368756E054368696E6107333336383030300D6368616E676368756E2E6A
      7067040000830000000C5368696A69617A6875616E67054368696E6107333336
      37303030107368696A69617A6875616E672E6A70670400008400000006526563
      696665064272617A696C07333334373030300A7265636966652E6A7067040000
      85000000074D617368686164044972616E07333239343030300B6D6173686861
      642E6A7067040000860000000753656174746C6518556E697465642053746174
      6573206F6620416D657269636107333231383030300B73656174746C652E6A70
      67040000870000000A43617361626C616E6361074D6F726F63636F0733323131
      3030300E63617361626C616E63612E6A7067040000880000000853616C766164
      6F72064272617A696C07333139303030300C73616C7661646F722E6A70670400
      008900000006DC72FC6D7169054368696E6107333138343030300AFC72FC6D71
      692E6A706704000089000000074C75636B6E6F7705496E646961073331383430
      30300B6C75636B6E6F772E6A70670400008B0000000A436869747461676F6E67
      0A42616E676C616465736807333137363030300E636869747461676F6E672E6A
      70670400008C0000000757656E7A686F75054368696E6107333136393030300B
      77656E7A686F752E6A70670400008D0000000649626164616E074E6967657269
      6107333136303030300A69626164616E2E6A70670400008E00000005497A6D69
      72065475726B6579073331313230303009697A6D69722E6A70670400008F0000
      00084375726974696261064272617A696C07333130323030300C637572697469
      62612E6A7067040000900000000953616E20446965676F18556E697465642053
      7461746573206F6620416D657269636107333038363030300773616E2E6A7067
      040000910000000759616F756E64E90843616D65726F6F6E0733303630303030
      0B79616F756E64E92E6A7067040000920000001F5A68616E676A696167616E67
      964A69616E6779696E964A696E676A69616E67054368696E6107333035363030
      30107A68616E676A696167616E672E6A706704000093000000064B616E707572
      05496E64696107333033373030300A6B616E7075722E6A706704000094000000
      095A686F6E677368616E054368696E6107333033313030300D7A686F6E677368
      616E2E6A7067040000950000000653616E6127610559656D656E073239383030
      30300A73616E6127612E6A7067040000960000000854656C2041766976064973
      7261656C07323937393030300774656C2E6A7067}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 352
    Top = 16
  end
end
