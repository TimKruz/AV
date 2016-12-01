object MainFrm: TMainFrm
  Left = 575
  Top = 216
  AlphaBlend = True
  AlphaBlendValue = 95
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsNone
  Caption = 'Amiga Virtual'
  ClientHeight = 263
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0AA22AA22AA22AA22AA22AA000000002288888888888888888888882200000A8
    8888888888888888888888888A00028888888888888888888888888888200298
    8888888888889D88888888888820A5D8888888888888D58888888888888AA8D9
    888888888889D88888C88888888A285D88888888888D588884E48888888228DD
    88888888888DD8888ECE88888882A885D5D88888889588888EEE8888888AA88D
    9D95D88888DD8888CE4EC888888A2885D88D95D888D58884EE8EE48888822888
    D9888D95D5D8888EC888CE888882A8885D88888D9D58888EE888EE88888AA888
    DD8888888DD888CE48884EC8888A288885988888958884EE88888EE488822888
    8DD88888DD888EC8888888CE8882A88885D88888D5888EE8888888EE888AA888
    88D98889D888CE488888884EC88A2888885D888D5884EE888888888EE4822888
    88DD888DD88EC88888888888C882A88888859895888EE88888888888888AA888
    888DD8DD88CE488888888888888A28888885D8D584EE88888888888888822888
    8888D9D88EC88888888888888882A88888885D588EE8888888888888888AA888
    8888DDD8CE48888888888888888A028888888584EE8888888888888888200288
    88888D88C888888888888888882000A88888888888888888888888888A000002
    288888888888888888888882200000000AA22AA22AA22AA22AA22AA00000F800
    001FE0000007C000000380000001800000010000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000008000000180000001C0000003E0000007F800001F}
  OldCreateOrder = False
  PopupMenu = MainMenu
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 2
    Top = 20
    Width = 369
    Height = 241
    Cursor = crCross
    Align = alCustom
    BorderStyle = bsNone
    Color = 14666463
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    PopupMenu = MainMenu
    ReadOnly = True
    ShowHint = False
    TabOrder = 0
    Visible = False
    OnExit = MemoExit
    OnKeyPress = MemoKeyPress
  end
  object MenuPanel: TPanel
    Left = 2
    Top = 20
    Width = 369
    Height = 241
    BevelOuter = bvLowered
    BevelWidth = 2
    TabOrder = 1
    object VirtList: TListBox
      Left = 4
      Top = 5
      Width = 260
      Height = 80
      AutoComplete = False
      BevelKind = bkTile
      BorderStyle = bsNone
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
    end
    object LoadBtn: TButton
      Left = 264
      Top = 5
      Width = 100
      Height = 20
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = LoadBtnClick
    end
    object NewBtn: TButton
      Left = 264
      Top = 45
      Width = 100
      Height = 20
      Caption = #1057#1086#1079#1076#1072#1090#1100
      TabOrder = 3
      OnClick = NewBtnClick
    end
    object DelBtn: TButton
      Left = 264
      Top = 65
      Width = 100
      Height = 20
      Caption = #1059#1076#1072#1083#1080#1090#1100
      TabOrder = 4
      OnClick = DelBtnClick
    end
    object RenBtn: TButton
      Left = 264
      Top = 25
      Width = 100
      Height = 20
      Caption = #1055#1077#1088#1077#1080#1084#1077#1085#1086#1074#1072#1090#1100
      TabOrder = 2
      OnClick = RenBtnClick
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 30
    OnTimer = TimerTimer
  end
  object MainMenu: TPopupMenu
    Left = 30
    object SettingsGrp: TMenuItem
      Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' AV'
      object CloseAVBtn: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1080' '#1079#1072#1082#1088#1099#1090#1100' '#1090#1077#1082#1091#1097#1091#1102' AV'
        ShortCut = 27
        OnClick = CloseAVBtnClick
      end
      object SaveAVBtn: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1090#1077#1082#1091#1097#1091#1102' AV'
        ShortCut = 16467
        OnClick = SaveAVBtnClick
      end
      object DontSaveBtn: TMenuItem
        Caption = #1047#1072#1082#1088#1099#1090#1100' '#1090#1077#1082#1091#1097#1091#1102' AV'
        ShortCut = 49240
        OnClick = DontSaveBtnClick
      end
      object ChgAvatarBtn: TMenuItem
        Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1072#1074#1072#1090#1072#1088' AV'
        ShortCut = 16454
        OnClick = ChgAvatarBtnClick
      end
    end
    object EditGrp: TMenuItem
      Caption = #1058#1077#1082#1089#1090' '#1076#1080#1072#1083#1086#1075#1072
      object CopyBtn: TMenuItem
        Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1074#1089#1105
        ShortCut = 32833
        OnClick = CopyBtnClick
      end
      object ClearBtn: TMenuItem
        Caption = #1059#1076#1072#1083#1080#1090#1100' '#1074#1089#1105
        ShortCut = 32835
        OnClick = ClearBtnClick
      end
    end
    object HelpGrp: TMenuItem
      Caption = #1057#1087#1088#1072#1074#1082#1072
      object FAQbtn: TMenuItem
        Caption = #1063#1072#1042#1054
        Enabled = False
        ShortCut = 112
        OnClick = FAQbtnClick
      end
      object About: TMenuItem
        Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077'...'
        OnClick = AboutClick
      end
    end
    object QuitBtn: TMenuItem
      Caption = #1042#1099#1093#1086#1076
      ShortCut = 16465
      OnClick = QuitBtnClick
    end
  end
end
