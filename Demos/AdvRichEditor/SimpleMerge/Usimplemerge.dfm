object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'AdvRichEditor simple merge demo'
  ClientHeight = 343
  ClientWidth = 640
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
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 241
    Height = 13
    Caption = 'Click on a grid row to merge the selected row data'
  end
  object AdvRichEditor1: TAdvRichEditor
    Left = 8
    Top = 32
    Width = 400
    Height = 300
    Cursor = crIBeam
    HorzScrollBar.Tracking = True
    VertScrollBar.Range = 6
    VertScrollBar.Tracking = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    TabOrder = 0
    TabStop = True
    ParentFont = False
    Version = '1.7.7.2'
  end
  object StringGrid1: TStringGrid
    Left = 414
    Top = 32
    Width = 217
    Height = 300
    ColCount = 2
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    TabOrder = 1
    OnClick = StringGrid1Click
    ColWidths = (
      109
      100)
  end
end
