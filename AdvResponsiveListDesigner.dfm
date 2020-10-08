object AdvResponsiveListDesignerForm: TAdvResponsiveListDesignerForm
  Left = 0
  Top = 0
  Caption = 'ResponsiveList Designer'
  ClientHeight = 635
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBtn: TPanel
    Left = 0
    Top = 594
    Width = 784
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      784
      41)
    object Button1: TButton
      Left = 604
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 685
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object pnlProp: TPanel
    Left = 0
    Top = 0
    Width = 240
    Height = 594
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 9
      Top = 17
      Width = 50
      Height = 13
      Caption = 'Conditions'
    end
    object SpeedButton1: TSpeedButton
      Left = 96
      Top = 8
      Width = 23
      Height = 22
      Hint = 'New condition'
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9F989390898490898490898490
        8984908984908984908984908984796F67FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF948C
        87FFFFFFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF948C87FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFF7A736DFFFFFFFFFFFFFFFFFF948C
        87FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFF
        FFFF7A736DFFFFFFFFFFFFFFFFFF948C87FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF908984FFFFFFFFFFFF7A736D7A736D3B342E7A736D7A736DFFFFFF948C
        87FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFF
        FFFF7A736DFFFFFFFFFFFFFFFFFF948C87FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFF7A736DFFFFFFFFFFFFFFFFFF948C
        87FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF948C87FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEAE8E7DCDAD8DCDAD8877E
        78FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFF908984B3ADA9A39D98695E58FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF908984EFEEED867C76FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF908883DFDDDBDFDDDBDFDDDBDF
        DDDBDFDDDB857C76867C76FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFCCC9C6756A637C736E7A736D766C647C736EB0AAA6FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 125
      Top = 8
      Width = 23
      Height = 22
      Hint = 'Delete condition'
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000130B0000130B00000000000000000000FFFFFFFFFFFF
        5353535353535353535353535353535353535353535353535353535353535353
        53575757FFFFFFFFFFFFFFFFFFFFFFFF535353FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF575757FFFFFFFFFFFFFFFFFFFFFFFF
        535353FFFFFFFFFFFF9C9C9CFFFFFFCBCBCBCECECEFFFFFF9A9A9AFFFFFFFFFF
        FF575757FFFFFFFFFFFFFFFFFFFFFFFF535353FFFFFFFFFFFF575757FFFFFFA7
        A7A7ABABABFFFFFF535353FFFFFFFFFFFF575757FFFFFFFFFFFFFFFFFFFFFFFF
        535353FFFFFFFFFFFF575757FFFFFFA7A7A7ABABABFFFFFF535353FFFFFFFFFF
        FF575757FFFFFFFFFFFFFFFFFFFFFFFF535353FFFFFFFFFFFF575757FFFFFFA7
        A7A7ABABABFFFFFF535353FFFFFFFFFFFF575757FFFFFFFFFFFFFFFFFFFFFFFF
        535353FFFFFFFFFFFF575757FFFFFFA7A7A7ABABABFFFFFF535353FFFFFFFFFF
        FF575757FFFFFFFFFFFFFFFFFFFFFFFF535353FFFFFFFFFFFF575757FFFFFFA7
        A7A7ABABABFFFFFF535353FFFFFFFFFFFF575757FFFFFFFFFFFFFFFFFFFFFFFF
        535353FFFFFFFFFFFF575757FFFFFFA7A7A7ABABABFFFFFF535353FFFFFFFFFF
        FF575757FFFFFFFFFFFFFFFFFFFFFFFF535353FFFFFFFFFFFF575757FFFFFFA7
        A7A7ABABABFFFFFF535353FFFFFFFFFFFF575757FFFFFFFFFFFFFFFFFFFFFFFF
        535353FFFFFFFFFFFF575757FFFFFFA7A7A7ABABABFFFFFF535353FFFFFFFFFF
        FF575757FFFFFFFFFFFFFFFFFFFFFFFF535353FFFFFFFFFFFFD1D1D1FFFFFFE6
        E6E6E8E8E8FFFFFFD0D0D0FFFFFFFFFFFF575757FFFFFFFFFFFFFFFFFFFFFFFF
        535353FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF575757FFFFFFFFFFFF5353535353532D2D2D1B1B1B09090909090909090909
        09090909090909090909090909091B1B1B2E2E2E535353535353FFFFFFFFFFFF
        FFFFFFFFFFFF535353FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF575757FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8B8B8B53535353535353
        53535353535353535353538E8E8EFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton2Click
    end
    object Label2: TLabel
      Left = 9
      Top = 139
      Width = 57
      Height = 13
      Caption = 'Width from:'
    end
    object Label3: TLabel
      Left = 9
      Top = 167
      Width = 45
      Height = 13
      Caption = 'Width to:'
    end
    object Label4: TLabel
      Left = 9
      Top = 195
      Width = 60
      Height = 13
      Caption = 'Height from:'
    end
    object Label5: TLabel
      Left = 9
      Top = 223
      Width = 48
      Height = 13
      Caption = 'Height to:'
    end
    object Label6: TLabel
      Left = 9
      Top = 251
      Width = 44
      Height = 13
      Caption = 'Columns:'
    end
    object Label7: TLabel
      Left = 9
      Top = 279
      Width = 25
      Height = 13
      Caption = 'Row:'
    end
    object Label8: TLabel
      Left = 9
      Top = 307
      Width = 55
      Height = 13
      Caption = 'Item width:'
    end
    object Label9: TLabel
      Left = 9
      Top = 335
      Width = 59
      Height = 13
      Caption = 'Item height:'
    end
    object Label10: TLabel
      Left = 9
      Top = 363
      Width = 55
      Height = 13
      Caption = 'Margin left:'
    end
    object Label11: TLabel
      Left = 9
      Top = 391
      Width = 55
      Height = 13
      Caption = 'Margin top:'
    end
    object Label12: TLabel
      Left = 9
      Top = 419
      Width = 61
      Height = 13
      Caption = 'Margin right:'
    end
    object Label13: TLabel
      Left = 9
      Top = 447
      Width = 73
      Height = 13
      Caption = 'Margin bottom:'
    end
    object Label14: TLabel
      Left = 9
      Top = 476
      Width = 49
      Height = 13
      Caption = 'Category:'
    end
    object SpeedButton3: TSpeedButton
      Left = 154
      Top = 8
      Width = 23
      Height = 22
      Hint = 'Clone condition'
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000130B0000130B00000000000000000000FFFFFF9F9893
        908984908984908984908984908984908984908984908984796F67FDFDFDFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF948C87FBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF908984
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF948C87B1ACA8B3AD
        A9B3ADA9CFCCC9FFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF948C87DDDBD9DFDDDBDFDDDB938B85FFFFFFFFFFFF908984
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF948C87FBFBFBFFFF
        FFFFFFFF948C87FFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF948C87FBFBFBFFFFFFFFFFFF948C87FFFFFFFFFFFF908984
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF948C87FBFBFBFFFF
        FFFFFFFF948C87FFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF948C87FBFBFBFFFFFFFFFFFF948C87FFFFFFFFFFFF908984
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF948C87FBFBFBFFFF
        FFFFFFFF948C87FFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEA
        E8E7DCDAD8DCDAD8877E78FBFBFBFFFFFFFFFFFF948C87FFFFFFFFFFFF908984
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF908984B3ADA9A39D98695E58FEFEFEFFFF
        FFFFFFFF948C87FFFFFFFFFFFF908984FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF90
        8984EFEEED867C76EDECEBFFFFFFFFFFFFFFFFFF948C87FFFFFFFFFFFF908883
        DFDDDBDFDDDBDFDDDBDFDDDBDFDDDB857C76867C76EDECEBFFFFFF847B75948C
        878B827D938B85FFFFFFFFFFFFCCC9C6B0AAA6B0AAA6AEA9A5766C64B0AAA6B0
        AAA6EDECEBFFFFFFFFFFFF948C87F1F0EF877F79ECEBEAFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFBFBFB978F8AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF877E78877F
        79ECEBEAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFD79706A948C8794
        8C87948C87948C87948C87766C64ECEBEAFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton3Click
    end
    object SpeedButton4: TSpeedButton
      Left = 183
      Top = 8
      Width = 23
      Height = 22
      Hint = 'Move up'
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000130B0000130B00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3F3F3FAFAFAFFFFFFFF
        FFFFFFFFFFFFFFFFF9F9F9F3F3F3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF474747515151FAFAFAFFFFFFFFFFFFF9F9F94E4E4E484848FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C60D0D0D505050FA
        FAFAF9F9F94E4E4E0D0D0DC6C6C6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC6C6C60D0D0D4E4E4E4E4E4E0D0D0DC6C6C6FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C60D
        0D0D0D0D0DC6C6C6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C6C6C6C6FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton4Click
    end
    object SpeedButton5: TSpeedButton
      Left = 211
      Top = 8
      Width = 23
      Height = 22
      Hint = 'Move down'
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000130B0000130B00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9
        E9E9E9E9E9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFE9E9E92C2C2C2C2C2CE9E9E9FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9E92C2C2C21
        21212121212C2C2CE9E9E9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFE9E9E92C2C2C212121E1E1E1E0E0E02121212C2C2CE9E9E9FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF414141222222E1E1E1FF
        FFFFFFFFFFE0E0E0212121414141FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFD2D2D2E2E2E2FFFFFFFFFFFFFFFFFFFFFFFFE0E0E0D4D4D4FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton5Click
    end
    object lstConditions: TListView
      Left = 8
      Top = 36
      Width = 226
      Height = 92
      Columns = <
        item
          Caption = 'W from'
          MaxWidth = 50
          MinWidth = 50
        end
        item
          Caption = 'W to'
          MaxWidth = 50
          MinWidth = 50
        end
        item
          Caption = 'H from'
          MaxWidth = 50
          MinWidth = 50
        end
        item
          Caption = 'H to'
          MaxWidth = 50
          MinWidth = 50
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lstConditionsChange
    end
    object SpinEdit1: TSpinEdit
      Left = 96
      Top = 136
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = -1
      TabOrder = 1
      Value = 0
      OnChange = SpinEdit1Change
      OnExit = SpinEdit1Exit
    end
    object SpinEdit2: TSpinEdit
      Left = 96
      Top = 164
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = -1
      TabOrder = 2
      Value = 0
      OnChange = SpinEdit2Change
      OnExit = SpinEdit1Exit
    end
    object SpinEdit3: TSpinEdit
      Left = 96
      Top = 192
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = -1
      TabOrder = 3
      Value = 0
      OnChange = SpinEdit3Change
      OnExit = SpinEdit1Exit
    end
    object SpinEdit4: TSpinEdit
      Left = 96
      Top = 220
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = -1
      TabOrder = 4
      Value = 0
      OnChange = SpinEdit4Change
      OnExit = SpinEdit1Exit
    end
    object SpinEdit5: TSpinEdit
      Left = 96
      Top = 248
      Width = 73
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnExit = SpinEdit5Exit
    end
    object SpinEdit6: TSpinEdit
      Left = 96
      Top = 276
      Width = 73
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 6
      Value = 0
      OnExit = SpinEdit6Exit
    end
    object SpinEdit7: TSpinEdit
      Left = 96
      Top = 304
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = -1
      TabOrder = 7
      Value = 0
      OnChange = SpinEdit7Change
      OnExit = SpinEdit5Exit
    end
    object SpinEdit8: TSpinEdit
      Left = 96
      Top = 332
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = -1
      TabOrder = 8
      Value = 0
      OnChange = SpinEdit8Change
      OnExit = SpinEdit6Exit
    end
    object btnTemplate: TButton
      Left = 9
      Top = 501
      Width = 160
      Height = 25
      Caption = 'Template'
      TabOrder = 14
      OnClick = btnTemplateClick
    end
    object SpinEdit9: TSpinEdit
      Left = 96
      Top = 360
      Width = 73
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 9
      Value = 0
      OnExit = SpinEdit5Exit
    end
    object SpinEdit10: TSpinEdit
      Left = 96
      Top = 388
      Width = 73
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 10
      Value = 0
      OnExit = SpinEdit5Exit
    end
    object SpinEdit11: TSpinEdit
      Left = 96
      Top = 416
      Width = 73
      Height = 22
      MaxValue = 100
      MinValue = -1
      TabOrder = 11
      Value = 0
      OnExit = SpinEdit5Exit
    end
    object SpinEdit12: TSpinEdit
      Left = 96
      Top = 444
      Width = 73
      Height = 22
      MaxValue = 100
      MinValue = -1
      TabOrder = 12
      Value = 0
      OnExit = SpinEdit5Exit
    end
    object SpinEdit13: TSpinEdit
      Left = 96
      Top = 472
      Width = 73
      Height = 22
      MaxValue = 100
      MinValue = -1
      TabOrder = 13
      Value = 0
      OnExit = SpinEdit5Exit
    end
    object Button3: TButton
      Left = 9
      Top = 532
      Width = 160
      Height = 25
      Caption = 'Header template'
      TabOrder = 15
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 9
      Top = 563
      Width = 160
      Height = 25
      Caption = 'Footer template'
      TabOrder = 16
      OnClick = Button4Click
    end
    object ckHFull: TCheckBox
      Left = 175
      Top = 334
      Width = 49
      Height = 17
      Hint = 'Set items to use full control height'
      Caption = 'Full'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 17
      OnClick = ckHFullClick
    end
    object ckWFull: TCheckBox
      Left = 175
      Top = 306
      Width = 49
      Height = 17
      Hint = 'Set items to use full control width'
      Caption = 'Full'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 18
      OnClick = ckWFullClick
    end
    object ckWF: TCheckBox
      Left = 175
      Top = 138
      Width = 97
      Height = 17
      Hint = 'Set width to -1 to ignore width value'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      OnClick = ckWFClick
    end
    object ckWT: TCheckBox
      Left = 175
      Top = 166
      Width = 55
      Height = 17
      Hint = 'Set width to -1 to ignore width value'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 20
      OnClick = ckWTClick
    end
    object ckHF: TCheckBox
      Left = 175
      Top = 194
      Width = 55
      Height = 17
      Hint = 'Set height to -1 to ignore height value'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 21
      OnClick = ckHFClick
    end
    object ckHT: TCheckBox
      Left = 175
      Top = 222
      Width = 55
      Height = 17
      Hint = 'Set height to -1 to ignore height value'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 22
      OnClick = ckHTClick
    end
  end
  object Panel1: TPanel
    Left = 240
    Top = 0
    Width = 544
    Height = 594
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object pbVert: TPaintBox
      Left = 0
      Top = 6
      Width = 6
      Height = 588
      Align = alLeft
      OnPaint = pbVertPaint
      ExplicitHeight = 381
    end
    object pbHorz: TPaintBox
      Left = 0
      Top = 0
      Width = 544
      Height = 6
      Align = alTop
      OnPaint = pbHorzPaint
      ExplicitWidth = 458
    end
    object List: TAdvResponsiveList
      Left = 6
      Top = 6
      Width = 538
      Height = 588
      Cursor = crDefault
      HorzScrollBar.Range = 531
      HorzScrollBar.Tracking = True
      VertScrollBar.Range = 583
      VertScrollBar.Tracking = True
      Align = alClient
      TabOrder = 0
      BorderColor = clNone
      Conditions = <
        item
          Category = 0
          Columns = 3
          HeightFrom = -1
          WidthFrom = 200
          Tag = 0
        end>
      Items = <
        item
          Content = 'item 1'
          HeaderColor = clHighlight
          Visible = True
          ControlName = ''
        end
        item
          Content = 'item 2'
          HeaderColor = clHighlight
          Visible = True
          ControlName = ''
        end
        item
          Content = 'item 3'
          HeaderColor = clHighlight
          Visible = True
          ControlName = ''
        end>
      Version = '1.1.1.0'
      OnResize = ListResize
    end
  end
end
