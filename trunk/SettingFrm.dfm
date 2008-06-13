object SettingForm: TSettingForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #21442#25968#37197#32622
  ClientHeight = 226
  ClientWidth = 309
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    309
    226)
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 78
    Height = 12
    Caption = 'BugFree'#20027#39029#65306
  end
  object Label2: TLabel
    Left = 52
    Top = 64
    Width = 48
    Height = 12
    Caption = #29992#25143#21517#65306
  end
  object Label3: TLabel
    Left = 64
    Top = 96
    Width = 36
    Height = 12
    Caption = #23494#30721#65306
  end
  object Label4: TLabel
    Left = 16
    Top = 134
    Width = 84
    Height = 12
    Caption = #26356#26032#38388#38548#26102#38388#65306
  end
  object Label5: TLabel
    Left = 277
    Top = 134
    Width = 12
    Height = 12
    Caption = #31186
  end
  object Edit1: TEdit
    Left = 106
    Top = 21
    Width = 183
    Height = 20
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 106
    Top = 61
    Width = 183
    Height = 20
    TabOrder = 1
    OnChange = Edit1Change
  end
  object Edit3: TEdit
    Left = 106
    Top = 93
    Width = 183
    Height = 20
    PasswordChar = '*'
    TabOrder = 2
    OnChange = Edit1Change
  end
  object Button1: TButton
    Left = 118
    Top = 197
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = #20445#23384
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 216
    Top = 197
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = #20851#38381
    TabOrder = 4
    OnClick = Button2Click
  end
  object SpinEdit1: TSpinEdit
    Left = 106
    Top = 131
    Width = 165
    Height = 21
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 0
    OnChange = Edit1Change
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 168
    Width = 111
    Height = 17
    Caption = #38543#30005#33041#33258#21160#21551#21160
    TabOrder = 6
  end
end
