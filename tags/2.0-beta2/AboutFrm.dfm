object AboutForm: TAboutForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #20851#20110
  ClientHeight = 128
  ClientWidth = 238
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    238
    128)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 16
    Top = 16
    Width = 32
    Height = 32
    AutoSize = True
    Picture.Data = {
      055449636F6E0000010001002020100001000400E80200001600000028000000
      2000000040000000010004000000000080020000000000000000000000000000
      0000000000000000000080000080000000808000800000008000800080800000
      C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
      FFFFFF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000BB3330000000000000
      000000000000030BBBB3300CCCCC0CCCCC000000000C0B30BBBBB00EEEE00EEE
      EEC00000000C0BB30BBBB0300000EEEEEEEC0000000C0BBB30BB0BB3000EEEEE
      EEEEC000000C0BBBB30B0BB30EEEEEEEEEEEC000000CE0BBBB30BBBB00EEEEEE
      EEEEC000000C00BBB0030BBB00000EEEEEEEC000000C00000BBB30B0B30000EE
      EEEEC000000000E0BBBBB300B30EE00EEEEEC00000000EEE0BBBB000BB0EEE0E
      EEEEC000000CEEEE00000BB300019EE0EEEEC000000CEEEE00E00BBB00B11EEE
      EEEEC000000CEEEE00EEE0000BBB0EEEEEEEC000000C0EE00EEE00EE11B00EEE
      EEEEC000000CE000EEEE00EE9100E00EEEEEC000000CEEEEEEEEE00EEEE0EEE0
      EEEEC000000CEEEEEEEEEE00EEE0EEEEEEEEC000000CEEEEEEEEEEEE0EE0EEEE
      EEEEC000000CEEEEEEEEEEEEEEEEEEEEEEEEC000000CEEEEEEEEEEE0000EEEEE
      EE000000000CEEEEEEEEEE077EE0EEEEE077EF000000CEE0EEEEE07E777E0EE0
      07E777F000000C08EEEEE0E7EE7E0E080F7FF7F00000000CCCCCC0FFFF770C0C
      0FFFF77000000000000000FFFFF700000FFFFF70000000000000080FFF708000
      80FFF80800000008000800800008000008000080000000000800000000000000
      08000000FFFCFFFFFFFCFFFFF810EFFFF001F7FFF000003FE000001FE000000F
      E0000007E0000007E0000007600000072000000780000007C0000007E0000007
      E0000007E0000007E0000007E0000007E0000007E0000007E0000007E0000007
      E0000003E0000001F0000000F8000000FC000000FDF80500FDE00000FC8C0D81
      FE3FFE3F}
  end
  object Button1: TButton
    Left = 155
    Top = 99
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = #20851#38381
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 72
    Top = 16
    Width = 158
    Height = 65
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'BugFree Helper'
      ''
      'author:cnwinds'
      'email:cnwinds@163.com'
      'date:2008-1-20')
    ReadOnly = True
    TabOrder = 1
  end
end