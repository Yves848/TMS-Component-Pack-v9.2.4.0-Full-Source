object Form56: TForm56
  Left = 0
  Top = 0
  Caption = 'TMS TAdvStringGrid PDF Export'
  ClientHeight = 416
  ClientWidth = 576
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
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 556
    Height = 39
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alTop
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object Button2: TButton
      Left = 8
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Export'
      TabOrder = 0
      OnClick = Button2Click
    end
  end
  object AdvStringGrid1: TAdvStringGrid
    AlignWithMargins = True
    Left = 10
    Top = 59
    Width = 556
    Height = 347
    Cursor = crDefault
    Margins.Left = 10
    Margins.Top = 0
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alClient
    DrawingStyle = gdsClassic
    ScrollBars = ssBoth
    TabOrder = 1
    HoverRowCells = [hcNormal, hcSelected]
    OnGetCellColor = AdvStringGrid1GetCellColor
    OnGetAlignment = AdvStringGrid1GetAlignment
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
    SearchFooter.ResultFormat = '(%d of %d)'
    SortSettings.DefaultFormat = ssAutomatic
    Version = '8.2.3.0'
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
      22
      22
      22
      22
      22
      22)
  end
  object AdvGridPDFIO1: TAdvGridPDFIO
    Left = 368
    Top = 216
    Width = 26
    Height = 26
    Visible = True
    Grid = AdvStringGrid1
    Options.DefaultFont.Name = 'Arial'
    Options.Header = 'TMS PDF Header'
    Options.Footer = 'TMS PDF Footer'
    Options.Margins.Left = 20.000000000000000000
    Options.Margins.Top = 50.000000000000000000
    Options.Margins.Right = 20.000000000000000000
    Options.Margins.Bottom = 50.000000000000000000
    Options.HeaderFont.Name = 'Arial'
    Options.FooterFont.Name = 'Arial'
    Options.HeaderMargins.Left = 5.000000000000000000
    Options.HeaderMargins.Top = 5.000000000000000000
    Options.HeaderMargins.Right = 5.000000000000000000
    Options.HeaderMargins.Bottom = 5.000000000000000000
    Options.FooterMargins.Left = 5.000000000000000000
    Options.FooterMargins.Top = 5.000000000000000000
    Options.FooterMargins.Right = 5.000000000000000000
    Options.FooterMargins.Bottom = 5.000000000000000000
    OnGetFooter = AdvGridPDFIO1GetFooter
  end
  object PictureContainer1: TPictureContainer
    Items = <
      item
        Picture.Stretch = False
        Picture.Frame = 1
        Picture.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          026B49444154384FA5523D68535114FEEE4BD2C6FCD1D6A194A04206977611AB
          601554A82228E8A29B932268519146105C1C1C426D845AEBA44BBB99C54171F1
          071C049B56DBB4A2D6166930A626CD4F5FD2262F2F2FCF736E5E4CEBEA0787CB
          79E77CDFF9CEBD0FFF0B619D7F7172187B2B55844C13FD26E5DCA028804DC12B
          45E0D68B20A665A3852D02C78730B6DDE3BB72FEF02574EFEA81AE17912FADE0
          773E8ED88F1944173FA3523122CF6FE29C45690A9C18C2537F47D7D987172610
          4B8C6376791CD51A3550878DC2E908A058F2E26D6C0E1B5AED113919609E1460
          DB5EA76B6AE4E204DE7C1D40B6B8226DCBA23582569291CB7720BA90856EA097
          D7A13640D371EFD4BED398FBF51819B2ACD88967AB07A8A34A47C9002EF79970
          38B3E86C076A0686992B05E8728E063A776031F352DAAE5018348D0E54E92C93
          C28D437CA5B48A0BF0B98963C311CEA50097CA860A8D18D769CAB503264A2C44
          059E3C68911976877C11B90E430AF045A9EBA9BA650B832452A0C941126CE0FE
          07013BAF67E58CC60AC8A849B4907A78AA59BE7DB0490E4705B6B5129918464D
          C8A10C29502D61F27B7209B5721BDC4E6AFEB87906E5D375B2A30558CB091436
          88B38E49AE4981EC1784BE2552C8A44D081AEA2191D198C0C88CC08359011791
          D99D412BA55240326722BD801073E5D6C979C4FDBDD89DD3B46E4557E0B099F0
          7881568BC83FD25A46E0E7B2C052D2849A46E4FD18C244D5377BDD79EC0E9EB8
          DBD0DFD52EE0A5E76222839F946DF3E4C22A22AFEF22489FE35CDBBA2C10E839
          833EFF1E5CB5BBB1BFF1547C61BC73E21346E79FE11D7D9264C6BF020C9A0D3F
          854F664DA814090AF2D200F0073B8FE789E1A3B7CB0000000049454E44AE4260
          82}
        Name = '1'
        Tag = 0
      end
      item
        Picture.Stretch = False
        Picture.Frame = 1
        Picture.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          01C249444154384F6364C00136EF3C349D9189298311A8821148303333AF7577
          B008814AC301560336ED3A7C90E1DF3FBB5BF71E317CFFF99B81E1FF7F062626
          26868F5FBED474D5E4B642958101139446012C4C4C40CD8F192E5EBE34A9B630
          C9E4CBA7379338D9D91898FFFD36842A8103AC0630333331FCF8F59361F1F4EE
          7620F7ECA9833BFAE4652519D8383883212A1000C5802327CFFFAFEF9AFE1FE8
          69867FFFC1422F40848888C813161616B0003A4031E0E397AF50D67F6010FC81
          B2191856AF5EFD9711AB5B7178E13F30D0FEFCF90B66C35CC5CAC20CE6A3031C
          E682031E0CDE7FFA0CA69998883200A2EBC7CF5F400D60260230424D4403585D
          70EFE113065B0B2386EDFB8EFCFFFEFD074410E6243480620028C58140517A8C
          C9D113E7194E9CB9CC70F5C65D86C6B24C13ECDA8169064A83C1DFDF90800382
          B3204D5036089C85790F1DA0B8E0D3E7CF7740F49235DB40AA819A2078EB9E23
          FF1F3D7EC1F0EEE327B03C3240CF0B5AC028BB2A272DCE202723054C0BFFC05E
          7FF4F439C3D3E7AF405ED106AAB906510A01D83293716E75C70A217E7E15289F
          E1ED87B7A7A7B4D56402992017A100ACB91108B4809813C204035062B8056122
          03060600A983A2EE44C1904D0000000049454E44AE426082}
        Name = '2'
        Tag = 0
      end
      item
        Picture.Stretch = False
        Picture.Frame = 1
        Picture.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          018749444154384F9592C92F03511CC7BFA3B84A136B225CAAC452B15CC63FE0
          D088E5E0C60989F00788A4D1C452E122A31249A597DE25B869286A09A2B19C38
          20D222D356A95AAAC3F3DE68269129C6E730F366E6F7F9CCE465F01F4E27BCE4
          647C85242F65D292E73F19B2D94856750948513E4E26579588A6C0E0D834E9EF
          EDC3ECA1132F620CFED83B7C967939F26760D7524B063B8C10669C303736C1FD
          B18EC463025258949F73F2F107B62D3CE15B7380A01F17995D70ED13982A0DF0
          EC78200C8FD7D39103DDD7A81A596ECBA7F21D1089427FBB047D412516F7FC98
          1A1D91653697F20B14590C533902C41EE8DD57B80E0BD169F7293243B5076A39
          4A5F2361EEB8542533BE0552CB09388E0CE816BC2A99A104B67E917B848D9432
          430E7807EA49430BDD6D31F42F99C105179A49760D0F9CB969E01A787AA25949
          93CC4823F177203D0F6FC5ED405CA2C9B86699A12BABE2AD37F71C8CB9E9E0B8
          0C38D6087AEC9B9A6486CE54516E0D5D9EA3E0F90A818004F3E8B26699C17EA4
          BAAFA5826619003E01BC2BBA1CDB3E82D90000000049454E44AE426082}
        Name = '3'
        Tag = 0
      end>
    Version = '2.0.0.0'
    Left = 280
    Top = 216
  end
end
