object fCodeGroupEdit: TfCodeGroupEdit
  Left = 0
  Top = 0
  Caption = 'Code Group Editor'
  ClientHeight = 428
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 71
    Height = 13
    Caption = 'Code Group ID'
  end
  object Label2: TLabel
    Left = 24
    Top = 83
    Width = 57
    Height = 13
    Caption = 'Public Name'
  end
  object Label3: TLabel
    Left = 24
    Top = 112
    Width = 53
    Height = 13
    Caption = 'Description'
  end
  object Label7: TLabel
    Left = 24
    Top = 55
    Width = 110
    Height = 13
    Caption = 'Minimum Security Level'
  end
  object edCodeGroupID: TEdit
    Left = 304
    Top = 21
    Width = 121
    Height = 21
    Enabled = False
    ReadOnly = True
    TabOrder = 0
  end
  object edCodeGroupName: TEdit
    Left = 168
    Top = 80
    Width = 257
    Height = 21
    TabOrder = 1
  end
  object mCodeGroupDesc: TMemo
    Left = 24
    Top = 131
    Width = 401
    Height = 70
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 321
    Top = 382
    Width = 104
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnSave: TButton
    Left = 200
    Top = 382
    Width = 104
    Height = 25
    Caption = 'Save'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 207
    Width = 401
    Height = 162
    Caption = ' Public Names for Admin Panels '
    TabOrder = 5
    object Label4: TLabel
      Left = 16
      Top = 21
      Width = 369
      Height = 81
      AutoSize = False
      Caption = 
        'CodeGroups are used to limit the access to function to certain u' +
        'ser groups.  As there will be an editor needed in the main appli' +
        'cation for an admin to assign access rights and we shouldn'#39't giv' +
        'e any access to the CodeGroups itself, all informations needed t' +
        'o display the CodeGroups in an AdminPanel can be set here.'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 16
      Top = 99
      Width = 89
      Height = 13
      Caption = 'Public Name ResID'
    end
    object Label6: TLabel
      Left = 16
      Top = 126
      Width = 115
      Height = 13
      Caption = 'Public Description ResID'
    end
    object edNameID: TEdit
      Left = 229
      Top = 96
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edDescID: TEdit
      Left = 229
      Top = 123
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object btnSearchMediaNameID: TButton
      Left = 356
      Top = 92
      Width = 29
      Height = 25
      Caption = '[...]'
      TabOrder = 2
      OnClick = btnSearchMediaNameIDClick
    end
    object Button2: TButton
      Left = 356
      Top = 120
      Width = 29
      Height = 25
      Caption = '[...]'
      TabOrder = 3
    end
  end
  object cbMinSL: TComboBox
    Left = 253
    Top = 52
    Width = 172
    Height = 21
    Style = csDropDownList
    ItemIndex = 9
    TabOrder = 6
    Text = '10 - Maximum, only Developer'
    Items.Strings = (
      '1 - Minimum, almost all users'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      '10 - Maximum, only Developer')
  end
end
