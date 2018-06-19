object fEditName_Generic: TfEditName_Generic
  Left = 332
  Top = 264
  Caption = 'Enter a new Hook description...'
  ClientHeight = 104
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 9
    Width = 161
    Height = 13
    Caption = 'New description (max. 250 chars)'
  end
  object eHookDesc: TEdit
    Left = 16
    Top = 28
    Width = 349
    Height = 21
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 260
    Top = 68
    Width = 105
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 144
    Top = 68
    Width = 105
    Height = 25
    Caption = 'Apply'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
