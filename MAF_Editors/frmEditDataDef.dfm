object fEditDataDef: TfEditDataDef
  Left = 0
  Top = 0
  Caption = 'Edit Data Definition'
  ClientHeight = 160
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 27
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 16
    Top = 67
    Width = 48
    Height = 13
    Caption = 'Data type'
  end
  object eName: TEdit
    Left = 200
    Top = 24
    Width = 145
    Height = 21
    TabOrder = 0
    OnChange = eNameChange
  end
  object cbDataType: TComboBox
    Left = 200
    Top = 64
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 1
    Text = 'Integer'
    OnChange = cbDataTypeChange
    Items.Strings = (
      'Integer'
      'DateTime'
      'Int64'
      'String'
      'MediaItem')
  end
  object Panel1: TPanel
    Left = -11
    Top = 104
    Width = 388
    Height = 73
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object btnCancel: TButton
      Left = 248
      Top = 16
      Width = 108
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object btnSave: TButton
      Left = 120
      Top = 16
      Width = 108
      Height = 25
      Caption = 'Save'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
end
