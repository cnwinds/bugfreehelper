object BugFreeHelperForm: TBugFreeHelperForm
  Left = 0
  Top = 0
  Caption = 'BugFreeHelper'
  ClientHeight = 206
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainTrayIcon: TTrayIcon
    Animate = True
    Icons = ImageList2
    IconIndex = -1
    PopupMenu = MainPopupMenu
    Left = 16
    Top = 16
  end
  object MainPopupMenu: TPopupMenu
    AutoHotkeys = maManual
    OnPopup = MainPopupMenuPopup
    Left = 56
    Top = 16
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 96
    Top = 16
  end
  object ImageList1: TImageList
    Left = 16
    Top = 56
    Bitmap = {
      494C010107000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001001000000000000018
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A014E07FE07F
      8073000000000000000000000000000000000000000000000000A014E07FE07F
      8073000000000000000000000000000000000000000000000000A014E07FE07F
      8073000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C05A4008E07F
      E07F000040080000000000000000000000000000000000000000C05A4008E07F
      E07F000040080000000000000000000000000000000000000000C05A4008E07F
      E07F000040080000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E07F406B8010
      E07BE07FE07F0000000000000000000000000000000000000000E07F406B8010
      E07BE07FE07F0000000000000000000000000000000000000000E07F406B8010
      E07BE07FE07F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E07FE07FE07F
      2025E07FA0770000000000000000000000000000000000000000E07FE07FE07F
      2025E07FA0770000000000000000000000000000000000000000E07FE07FE07F
      2025E07FA0770000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000004008E07F
      E07FC018600CC07700000000000000000000000000000000000000004008E07F
      E07FC018600CC07700000000000000000000000000000000000000004008E07F
      E07FC018600CC077000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E07F
      E07F00000000E07F00000000000000000000000000000000000000000000E07F
      E07F00000000E07F00000000000000000000000000000000000000000000E07F
      E07F00000000E07F000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E07FE07F0000000000000000000000000000000000000000000000000000
      0000E07FE07F0000000000000000000000000000000000000000000000000000
      0000E07FE07F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000E07F00000000000000000000000000000000000000000000
      0000000000000000E07F00000000000000000000000000000000000000000000
      0000000000000000E07F00000000000000000000000000000000000000000000
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
      0000EF3D00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EF3D00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000EF3D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000EF3D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EF3DEF3D0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E07F
      E07FE07FEF3D0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000EF3DEF3DEF3DEF3D
      EF3DEF3DEF3D00000000EF3D0000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000EF3DEF3DEF3DE07F
      E07FE07FEF3D00000000EF3D0000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000EF3D4A294A294A29
      4A29EF3D000000000000EF3D0000000000000000000000000000A014E07FE07F
      807300000000000000000000000000000000000000000000EF3D4A294A29E07F
      E07FE07F000000000000EF3D0000000000000000000000000000A014E07FE07F
      807300000000000000000000000000000000000000000000EF3D4A294A294A29
      4A29EF3DEF3DEF3DEF3DEF3D0000000000000000000000000000C05A4008E07F
      E07F00004008000000000000000000000000000000000000EF3D4A294A294A29
      4A29EF3DEF3DEF3DEF3DEF3D0000000000000000000000000000C05A4008E07F
      E07F00004008000000000000000000000000000000000000EF3D4A294A294A29
      4A294A294A29EF3D000000000000000000000000000000000000E07F406B8010
      E07BE07FE07F000000000000000000000000000000000000EF3D4A294A29E07F
      E07FE07F4A29EF3D000000000000000000000000000000000000E07F406B8010
      E07BE07FE07F000000000000000000000000000000000000EF3D4A294A294A29
      4A294A294A29EF3D000000000000000000000000000000000000E07FE07FE07F
      2025E07FA077000000000000000000000000000000000000EF3D4A294A29E07F
      E07FE07F4A29EF3D000000000000000000000000000000000000E07FE07FE07F
      2025E07FA077000000000000000000000000EF3D00000000EF3DEF3D4A294A29
      4A294A294A294A29EF3DEF3DEF3D00000000000000000000000000004008E07F
      E07FC018600CC07700000000000000000000EF3D00000000EF3DEF3D4A29E07F
      E07FE07F4A294A29EF3DEF3DEF3D00000000000000000000000000004008E07F
      E07FC018600CC077000000000000000000000000EF3DEF3DEF3D0000EF3D4A29
      4A294A294A294A29EF3D00000000EF3D0000000000000000000000000000E07F
      E07F00000000E07F000000000000000000000000EF3DEF3DEF3D0000EF3DE07F
      E07FE07F4A294A29EF3D00000000EF3D0000000000000000000000000000E07F
      E07F00000000E07F0000000000000000000000000000000000000000EF3DEF3D
      EF3D4A294A29EF3DEF3DEF3D0000000000000000000000000000000000000000
      0000E07FE07F00000000000000000000000000000000000000000000EF3DE07F
      E07FE07F4A29EF3DEF3DEF3D0000000000000000000000000000000000000000
      0000E07FE07F00000000000000000000000000000000000000000000EF3D0000
      0000EF3DEF3DEF3D4A29EF3D0000000000000000000000000000000000000000
      0000000000000000E07F000000000000000000000000000000000000EF3DE07F
      E07FE07FEF3DEF3D4A29EF3D0000000000000000000000000000000000000000
      0000000000000000E07F0000000000000000000000000000EF3DEF3DEF3D0000
      0000EF3D0000EF3DEF3DEF3DEF3DEF3D00000000000000000000000000000000
      000000000000000000000000000000000000000000000000EF3DEF3DEF3DE07F
      E07FE07F0000EF3DEF3DEF3DEF3DEF3D00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EF3D000000000000EF3D0000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E07F
      E07FE07F000000000000EF3D0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000EF3D00000000EF3D0000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E07F
      E07FE07FEF3D00000000EF3D0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF7FFFFFFF7F0000FFBFFFDFFFBF0000
      FF3FFFDFFF3F0000E037E03FE0370000E077E07BE0770000E007E007E0070000
      E01FE01FE01F0000E01FE01FE01F00006003600360030000880D880D880D0000
      F807F807F8070000FB07FB07FB070000E341E341E3410000FF77FF77FF770000
      FFB7FFB7FFB70000FFFFFFFFFFFF0000FF7FFF7FFF7FFF7FFFBFFFBFFFBFFFBF
      FF3FFF3FFC3FFF3FE037E037E037E037E077E077E077E077E007E007E007E007
      E01FE01FE01FE01FE01FE01FE01FE01F600360036003E003880D880D880DE80D
      F807F807F807D807FB07FB07F807BB07E341E341E041FB41FF77FF77FC77F777
      FFB7FFB7FC37FFB7FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object TrayTimer: TTimer
    Interval = 300
    OnTimer = TrayTimerTimer
    Left = 56
    Top = 56
  end
  object ImageList2: TImageList
    Left = 16
    Top = 96
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001001000000000000018
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
      0000000000000000000000000000000000000000000000000000A014E07FE07F
      8073000000000000000000000000000000000000000000000000A014E07FE07F
      8073000000000000000000000000000000000000000000000000A014E07FE07F
      8073000000000000000000000000000000000000000000000000A014E07FE07F
      8073000000000000000000000000000000000000000000000000C05A4008E07F
      E07F000040080000000000000000000000000000000000000000C05A4008E07F
      E07F000040080000000000000000000000000000000000000000C05A4008E07F
      E07F000040080000000000000000000000000000000000000000C05A4008E07F
      E07F000040080000000000000000000000000000000000000000E07F406B8010
      E07BE07FE07F0000000000000000000000000000000000000000E07F406B8010
      E07BE07FE07F0000000000000000000000000000000000000000E07F406B8010
      E07BE07FE07F0000000000000000000000000000000000000000E07F406B8010
      E07BE07FE07F0000000000000000000000000000000000000000E07FE07FE07F
      2025E07FA0770000000000000000000000000000000000000000E07FE07FE07F
      2025E07FA0770000000000000000000000000000000000000000E07FE07FE07F
      2025E07FA0770000000000000000000000000000000000000000E07FE07FE07F
      2025E07FA077000000000000000000000000000000000000000000004008E07F
      E07FC018600CC07700000000000000000000000000000000000000004008E07F
      E07FC018600CC07700000000000000000000000000000000000000004008E07F
      E07FC018600CC07700000000000000000000000000000000000000004008E07F
      E07FC018600CC07700000000000000000000000000000000000000000000E07F
      E07F00000000E07F00000000000000000000000000000000000000000000E07F
      E07F00000000E07F00000000000000000000000000000000000000000000E07F
      E07F00000000E07F00000000000000000000000000000000000000000000E07F
      E07F00000000E07F000000000000000000000000000000000000000000000000
      0000E07FE07F0000000000000000000000000000000000000000000000000000
      0000E07FE07F0000000000000000000000000000000000000000000000000000
      0000E07FE07F0000000000000000000000000000000000000000000000000000
      0000E07FE07F0000000000000000000000000000000000000000000000000000
      0000000000000000E07F00000000000000000000000000000000000000000000
      0000000000000000E07F00000000000000000000000000000000000000000000
      0000000000000000E07F00000000000000000000000000000000000000000000
      0000000000000000E07F00000000000000000000000000000000000000000000
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
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF7FFF7FFFFFFF7FFFBFFFBFFFDFFFBF
      FF3FFF3FFFDFFF3FE037E037E03FE037E077E077E07BE077E007E007E007E007
      E01FE01FE01FE01FE01FE01FE01FE01FE003600360036003E80D880D880D880D
      D807F807F807F807BB07FB07FB07FB07FB41E341E341E341F777FF77FF77FF77
      FFB7FFB7FFB7FFB7FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end
