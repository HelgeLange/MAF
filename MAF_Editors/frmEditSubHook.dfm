object fEditSubHook: TfEditSubHook
  Left = 31
  Top = 22
  Caption = 'New / Edit SubHook'
  ClientHeight = 236
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 125
    Width = 161
    Height = 13
    Caption = 'New description (max. 250 chars)'
  end
  object Label2: TLabel
    Left = 16
    Top = 55
    Width = 60
    Height = 13
    Caption = 'SubHookID :'
  end
  object Label3: TLabel
    Left = 16
    Top = 91
    Width = 59
    Height = 13
    Caption = 'Module       :'
  end
  object Label4: TLabel
    Left = 16
    Top = 28
    Width = 60
    Height = 13
    Caption = 'HookID       :'
  end
  object btnCancel: TButton
    Left = 260
    Top = 184
    Width = 105
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object btnOk: TButton
    Left = 144
    Top = 184
    Width = 105
    Height = 25
    Caption = 'Apply'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object eSubHookDesc: TEdit
    Left = 16
    Top = 144
    Width = 349
    Height = 21
    TabOrder = 3
  end
  object eSubHookID: TEdit
    Left = 244
    Top = 52
    Width = 121
    Height = 21
    TabOrder = 1
    OnKeyPress = eSubHookIDKeyPress
  end
  object cbModules: TComboBox
    Left = 152
    Top = 88
    Width = 213
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object eHookID: TEdit
    Left = 244
    Top = 25
    Width = 121
    Height = 21
    TabOrder = 0
    OnKeyPress = eSubHookIDKeyPress
  end
end
