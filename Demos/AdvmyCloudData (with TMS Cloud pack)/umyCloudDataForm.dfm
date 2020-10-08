object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'myCloudData form demo'
  ClientHeight = 642
  ClientWidth = 580
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
    Left = 8
    Top = 39
    Width = 561
    Height = 594
    Caption = 'Panel1'
    TabOrder = 0
    object btnOpenClose: TButton
      Left = 16
      Top = 13
      Width = 75
      Height = 25
      Caption = '&Open'
      TabOrder = 0
      OnClick = btnOpenCloseClick
    end
    object DBNavigator2: TDBNavigator
      Left = 113
      Top = 13
      Width = 240
      Height = 25
      DataSource = DataSource1
      TabOrder = 1
    end
    object AdvmyCloudDataFormPanel1: TAdvmyCloudDataFormPanel
      Left = 16
      Top = 44
      Width = 529
      Height = 409
      DataSource = DataSource1
      DoubleBuffered = True
      Layout.Columns = 2
      Layout.Descriptions.Font.Charset = DEFAULT_CHARSET
      Layout.Descriptions.Font.Color = clBtnShadow
      Layout.Descriptions.Font.Height = -11
      Layout.Descriptions.Font.Name = 'Tahoma'
      Layout.Descriptions.Font.Style = [fsItalic]
      Layout.Descriptions.Kind = dkLabel
      Layout.Labels.Format = '%s:'
      Layout.Labels.Font.Charset = DEFAULT_CHARSET
      Layout.Labels.Font.Color = clWindowText
      Layout.Labels.Font.Height = -11
      Layout.Labels.Font.Name = 'Tahoma'
      Layout.Labels.Font.Style = []
      Layout.Labels.Position = lsTop
      Layout.Labels.Size = 0
      Layout.Items = <>
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      CustomItems = False
    end
    object DBGrid1: TDBGrid
      Left = 16
      Top = 459
      Width = 529
      Height = 120
      DataSource = DataSource1
      TabOrder = 3
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
    object ckDescr: TCheckBox
      Left = 434
      Top = 17
      Width = 122
      Height = 17
      Caption = 'Description as hint'
      TabOrder = 4
      OnClick = ckDescrClick
    end
  end
  object btnConnect: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Connect'
    TabOrder = 1
    OnClick = btnConnectClick
  end
  object DataSource1: TDataSource
    DataSet = AdvMyCloudDataDataSet1
    Left = 280
    Top = 8
  end
  object AdvMyCloudDataDataSet1: TAdvMyCloudDataDataSet
    App.CallBackURL = 'http://127.0.0.1:8888'
    App.CallBackPort = 8888
    Connection = AdvMyCloudDataConnection1
    Filter = <
      item
        FieldName = 'SEX'
        FieldValue = 'M'
      end>
    PersistTokens.Location = plIniFile
    Table = 'CONTACTS'
    TableID = 80
    Left = 488
    Top = 8
    Tokens = 
      'yQjCGVk0kaSe/HN6sWuUHQM8TZHXHDBqvm2Yz0/Xt6OHP5u9RfSsfB6CeLmeP+w3' +
      'NFU52KAUyakwvz+Is86hK4ytWh+ZdQw66hsyzCw6ERZu/TK5n7Ta9iJAG4wqKChy' +
      '9wvJHuMROrzi+cF+9O6KvjJMhbNPomkU8w59zsdnWnkhscC6p+ea0RKIacUgZlVD' +
      'L+i+iw+WL40Hp5rZmb7qDVnWuvvmT0gB9rP6+WDMvSB9BYZVQYoRDxLFerKmpklE' +
      'W1BfLBclH6trfDoyOrVH7vi2QqMfR+bbmS+K1Rpc9kDughhzhvur8I5bUEZADOEY' +
      '/BinDsxaJQVU/71gUWkekru0n88Zh6dg/ViQaNrV2yxPhaqG+WlAa7oy2aidARnY' +
      'ZzZXiMPVpJIQXB3NZgEmboYeMAXX26cKuRFuCuDNKkaxprnBs4Quw5/pl3YZOmH8' +
      'YupHybXYTV3SdisIttSCh2KBk0cx1tZ2Whu3cLJwRZCvbtIED/WGDDO3IEwBevED' +
      'fdM'
  end
  object AdvMyCloudDataConnection1: TAdvMyCloudDataConnection
    Connected = False
    App.CallBackURL = 'http://127.0.0.1:8888'
    App.CallBackPort = 8888
    PersistTokens.Location = plIniFile
    OnConnected = AdvMyCloudDataConnection1Connected
    Left = 384
    Top = 8
    Tokens = ''
  end
end
