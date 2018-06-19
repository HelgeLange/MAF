object fResourceFileInfo: TfResourceFileInfo
  Left = 0
  Top = 0
  Caption = 'ResourceFile - Info'
  ClientHeight = 269
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 16
    Top = 24
    Width = 353
    Height = 185
    Caption = ' File Info '
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 24
      Width = 100
      Height = 13
      Caption = 'Items in resource file'
    end
    object Label2: TLabel
      Left = 24
      Top = 51
      Width = 43
      Height = 13
      Caption = 'Encoding'
    end
    object lItemCount: TLabel
      Left = 208
      Top = 24
      Width = 129
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
    end
    object cbEncoding: TComboBox
      Left = 192
      Top = 48
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'AnsiStrings'
      Items.Strings = (
        'AnsiStrings'
        'WideStrings')
    end
  end
  object Button1: TButton
    Left = 256
    Top = 224
    Width = 113
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = Button1Click
  end
end
