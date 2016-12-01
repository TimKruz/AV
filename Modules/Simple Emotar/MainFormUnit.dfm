object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Emotar'
  ClientHeight = 200
  ClientWidth = 200
  Color = 16512700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object EmotarImage: TImage
    Left = 0
    Top = 24
    Width = 100
    Height = 150
  end
  object FormCaption: TLabel
    Left = 6
    Top = 5
    Width = 53
    Height = 16
    Caption = 'EMOTAR'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object GenerateBtn: TButton
    Left = 0
    Top = 175
    Width = 100
    Height = 25
    Caption = 'Generate Emotion'
    Default = True
    TabOrder = 0
    WordWrap = True
    OnClick = GenerateBtnClick
  end
  object HeadTypeBtn: TBitBtn
    Left = 100
    Top = 24
    Width = 100
    Height = 25
    Caption = 'Head'
    TabOrder = 1
    OnClick = ButtonClick
  end
  object HairTypeBtn: TBitBtn
    Left = 100
    Top = 48
    Width = 100
    Height = 25
    Caption = 'Hair'
    TabOrder = 2
    OnClick = ButtonClick
  end
  object MouthTypeBtn: TBitBtn
    Left = 100
    Top = 72
    Width = 100
    Height = 25
    Caption = 'Mouth'
    TabOrder = 3
    OnClick = ButtonClick
  end
  object EyesTypeBtn: TBitBtn
    Left = 100
    Top = 96
    Width = 100
    Height = 25
    Caption = 'Eyes'
    TabOrder = 4
    OnClick = ButtonClick
  end
  object PupilsTypeBtn: TBitBtn
    Left = 100
    Top = 120
    Width = 100
    Height = 25
    Caption = 'Pupils'
    TabOrder = 5
    OnClick = ButtonClick
  end
  object EyebrowsTypeBtn: TBitBtn
    Left = 100
    Top = 144
    Width = 100
    Height = 25
    Caption = 'Eyebrows'
    TabOrder = 6
    OnClick = ButtonClick
  end
  object SignTypeBtn: TBitBtn
    Left = 100
    Top = 168
    Width = 100
    Height = 25
    Caption = 'Sign'
    TabOrder = 7
    OnClick = ButtonClick
  end
  object PopupMenu: TPopupMenu
    Left = 2
    Top = 28
    object N11: TMenuItem
      Caption = '1'
    end
    object N21: TMenuItem
      Caption = '2'
    end
    object N31: TMenuItem
      Caption = '3'
    end
    object N41: TMenuItem
      Caption = '4'
    end
  end
  object Timer: TTimer
    Left = 32
    Top = 28
  end
end
