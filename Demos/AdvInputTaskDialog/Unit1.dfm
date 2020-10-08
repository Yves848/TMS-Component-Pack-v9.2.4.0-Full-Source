object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TAdvInputTaskDialog'
  ClientHeight = 390
  ClientWidth = 603
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 168
    Top = 109
    Width = 65
    Height = 13
    Caption = 'Entered email'
  end
  object Label2: TLabel
    Left = 168
    Top = 253
    Width = 87
    Height = 13
    Caption = 'Entered password'
  end
  object Button1: TButton
    Left = 168
    Top = 64
    Width = 257
    Height = 33
    Caption = 'Example for capturing an email address'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 168
    Top = 208
    Width = 257
    Height = 33
    Caption = 'Example for capturing a secure password'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 168
    Top = 128
    Width = 257
    Height = 21
    TabOrder = 2
  end
  object Edit2: TEdit
    Left = 168
    Top = 272
    Width = 257
    Height = 21
    TabOrder = 3
  end
  object AdvInputTaskDialog1: TAdvInputTaskDialog
    CommonButtons = []
    DefaultButton = 0
    FooterColor = 15790320
    FooterTextColor = clWindowText
    OnDialogClose = AdvInputTaskDialog1DialogClose
    OnValidateInputText = AdvInputTaskDialog1ValidateInputText
    Left = 280
    Top = 16
  end
  object AdvInputTaskDialog2: TAdvInputTaskDialog
    CommonButtons = []
    DefaultButton = 0
    FooterColor = 15790320
    FooterTextColor = clWindowText
    OnDialogClose = AdvInputTaskDialog2DialogClose
    OnValidateInputText = AdvInputTaskDialog2ValidateInputText
    Left = 288
    Top = 160
  end
end
