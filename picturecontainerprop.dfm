object ContainerEditor: TContainerEditor
  Left = 474
  Top = 172
  Caption = 'Picture Container editor'
  ClientHeight = 400
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    432
    400)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 36
    Height = 13
    Caption = 'Lookup'
  end
  object HTMListBox: THTMListBox
    Left = 8
    Top = 40
    Width = 320
    Height = 352
    Anchors = [akLeft, akTop, akRight, akBottom]
    EnableBlink = True
    HTMLHint = True
    ParentShowHint = False
    PictureContainer = PictureContainer
    ShowHint = True
    Sorted = False
    TabOrder = 0
    OnDblClick = HTMListBoxDblClick
    OnKeyDown = HTMListBoxKeyDown
    Version = '2.2.4.0'
  end
  object AddBtn: TButton
    Left = 336
    Top = 8
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add'
    TabOrder = 1
    OnClick = AddBtnClick
  end
  object RemoveBtn: TButton
    Left = 336
    Top = 40
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Remove'
    TabOrder = 2
    OnClick = RemoveBtnClick
  end
  object OkBtn: TButton
    Left = 336
    Top = 369
    Width = 89
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object ChangeName: TButton
    Left = 336
    Top = 72
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Change name'
    TabOrder = 4
    OnClick = ChangeNameClick
  end
  object CancelBtn: TButton
    Left = 336
    Top = 337
    Width = 89
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object Button1: TButton
    Left = 336
    Top = 103
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    TabOrder = 6
    OnClick = Button1Click
  end
  object edLookup: TEdit
    Left = 56
    Top = 13
    Width = 272
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    OnChange = edLookupChange
  end
  object PictureContainer: TPictureContainer
    Items = <>
    Version = '2.0.0.0'
    Left = 376
    Top = 216
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'All (*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf;*.gif;*.png)|*.jpg;*.j' +
      'peg;*.bmp;*.ico;*.emf;*.wmf;*.gif;*.png|JPEG Image File (*.jpg)|' +
      '*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|Icon' +
      's (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wm' +
      'f)|*.wmf|GIF File (*.gif)|*.gif|PNG file (*.png)|*.png'
    Options = [ofHideReadOnly, ofNoChangeDir, ofAllowMultiSelect, ofEnableSizing]
    Left = 376
    Top = 272
  end
  object SaveDialog1: TSaveDialog
    Filter = 
      'All (*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf;*.gif;*.png)|*.jpg;*.j' +
      'peg;*.bmp;*.ico;*.emf;*.wmf;*.gif;*.png|JPEG Image File (*.jpg)|' +
      '*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|Icon' +
      's (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wm' +
      'f)|*.wmf|GIF File (*.gif)|*.gif|PNG file (*.png)|*.png'
    Left = 376
    Top = 152
  end
end
