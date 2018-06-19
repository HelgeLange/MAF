object fParameter_Edit: TfParameter_Edit
  Left = 0
  Top = 0
  Caption = 'Parameter Editor'
  ClientHeight = 298
  ClientWidth = 459
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
  object Label1: TLabel
    Left = 16
    Top = 13
    Width = 43
    Height = 13
    Caption = 'Variables'
  end
  object lbVariables: TListBox
    Left = 16
    Top = 32
    Width = 121
    Height = 205
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbVariablesClick
  end
  object GroupBox1: TGroupBox
    Left = 159
    Top = 32
    Width = 275
    Height = 205
    Caption = ' Editor '
    Enabled = False
    TabOrder = 1
    object Label2: TLabel
      Left = 12
      Top = 35
      Width = 68
      Height = 13
      Caption = 'Variable Name'
    end
    object Label3: TLabel
      Left = 12
      Top = 75
      Width = 48
      Height = 13
      Caption = 'Data type'
    end
    object Label4: TLabel
      Left = 12
      Top = 154
      Width = 64
      Height = 13
      Caption = 'Default Value'
    end
    object Label5: TLabel
      Left = 12
      Top = 115
      Width = 75
      Height = 13
      Caption = 'Parameter type'
    end
    object eName: TEdit
      Left = 94
      Top = 32
      Width = 163
      Height = 21
      TabOrder = 0
    end
    object eValue: TEdit
      Left = 94
      Top = 151
      Width = 163
      Height = 21
      TabOrder = 1
    end
    object cbType: TComboBox
      Left = 94
      Top = 72
      Width = 163
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'String'
      OnChange = cbTypeChange
      Items.Strings = (
        'String'
        'Integer'
        'DateTime'
        'Boolean'
        'Pointer'
        'TObject'
        'Callback (TNotifyEvent)')
    end
    object cbParamType: TComboBox
      Left = 94
      Top = 112
      Width = 163
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'In Parameter'
      OnChange = cbTypeChange
      Items.Strings = (
        'In Parameter'
        'Out Parameter')
    end
  end
  object btnNew: TButton
    Left = 349
    Top = 256
    Width = 85
    Height = 25
    Caption = 'New'
    TabOrder = 2
    OnClick = btnNewClick
  end
  object btnEdit: TButton
    Left = 253
    Top = 256
    Width = 85
    Height = 25
    Caption = 'Edit'
    Enabled = False
    TabOrder = 3
    OnClick = btnEditClick
  end
  object btnDelete: TButton
    Left = 159
    Top = 256
    Width = 85
    Height = 25
    Caption = 'Delete'
    Enabled = False
    TabOrder = 4
    OnClick = btnDeleteClick
  end
end
