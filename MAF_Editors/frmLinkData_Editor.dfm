object fLinkDataEditor: TfLinkDataEditor
  Left = 0
  Top = 0
  Caption = 'Link Data Editor'
  ClientHeight = 204
  ClientWidth = 331
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
    Left = 16
    Top = 8
    Width = 169
    Height = 13
    AutoSize = False
    Caption = 'LinkIDs'
  end
  object Label2: TLabel
    Left = 212
    Top = 24
    Width = 101
    Height = 77
    AutoSize = False
    Caption = 
      'Simply place for every LinkID the number in a line in the Memo o' +
      'n the left side. Press Save, when done. '
    WordWrap = True
  end
  object mLinkIDs: TMemo
    Left = 16
    Top = 24
    Width = 177
    Height = 161
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 212
    Top = 160
    Width = 105
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnSave: TButton
    Left = 212
    Top = 124
    Width = 105
    Height = 25
    Caption = 'Save'
    TabOrder = 2
    OnClick = btnSaveClick
  end
end
