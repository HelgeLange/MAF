object fResourceBaseEditorForm: TfResourceBaseEditorForm
  Left = 0
  Top = 0
  Caption = 'fResourceBaseEditorForm'
  ClientHeight = 288
  ClientWidth = 370
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
    Left = 0
    Top = 232
    Width = 370
    Height = 56
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 0
    ExplicitTop = 246
    ExplicitWidth = 380
    object Panel3: TPanel
      Left = 183
      Top = 2
      Width = 185
      Height = 52
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 193
      DesignSize = (
        185
        52)
      object btnClose: TButton
        Left = 60
        Top = 12
        Width = 113
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Close'
        TabOrder = 0
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 165
    Width = 370
    Height = 67
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 179
    ExplicitWidth = 380
    object Label4: TLabel
      Left = 16
      Top = 42
      Width = 52
      Height = 13
      Caption = 'Category :'
    end
    object Label5: TLabel
      Left = 16
      Top = 12
      Width = 41
      Height = 13
      Caption = 'Module :'
    end
    object cbCategory: TComboBox
      Tag = 123
      Left = 160
      Top = 36
      Width = 209
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'None')
    end
    object cbModule: TComboBox
      Tag = 321
      Left = 160
      Top = 9
      Width = 209
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
  end
end
