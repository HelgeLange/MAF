object fFilesAdd_Wizard: TfFilesAdd_Wizard
  Left = 0
  Top = 0
  Caption = 'Add files...'
  ClientHeight = 395
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnAddSelected: TButton
    Left = 318
    Top = 329
    Width = 121
    Height = 25
    Caption = 'Save'
    TabOrder = 0
    OnClick = btnAddSelectedClick
  end
  object ListBox1: TListBox
    Left = 16
    Top = 160
    Width = 289
    Height = 225
    DragMode = dmAutomatic
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
    OnClick = ListBox1Click
    OnDragOver = ListBox1DragOver
  end
  object btnClose: TButton
    Left = 318
    Top = 360
    Width = 121
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = btnCloseClick
  end
  object btnAdd: TButton
    Left = 318
    Top = 160
    Width = 121
    Height = 25
    Caption = 'Add'
    TabOrder = 3
    OnClick = btnAddClick
  end
  object btnDelete: TButton
    Left = 318
    Top = 191
    Width = 121
    Height = 25
    Caption = 'Delete'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 8
    Width = 423
    Height = 146
    Caption = ' Image info '
    TabOrder = 5
    object Image1: TImage
      Left = 5
      Top = 14
      Width = 128
      Height = 128
      Center = True
      Proportional = True
    end
    object Label1: TLabel
      Left = 152
      Top = 14
      Width = 86
      Height = 13
      Caption = 'File size              : '
    end
    object lFileSize: TLabel
      Left = 216
      Top = 14
      Width = 191
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
    end
    object lHeight: TLabel
      Left = 216
      Top = 33
      Width = 191
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
    end
    object Label3: TLabel
      Left = 152
      Top = 33
      Width = 86
      Height = 13
      Caption = 'Height                : '
    end
    object lWidth: TLabel
      Left = 216
      Top = 52
      Width = 191
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
    end
    object Label5: TLabel
      Left = 152
      Top = 52
      Width = 86
      Height = 13
      Caption = 'Width                 : '
    end
    object Label2: TLabel
      Left = 152
      Top = 83
      Width = 84
      Height = 13
      Caption = 'Assign category :'
    end
    object Label4: TLabel
      Left = 152
      Top = 110
      Width = 84
      Height = 13
      Caption = 'Assign module    :'
    end
    object cbCategories: TComboBox
      Left = 262
      Top = 80
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object cbModules: TComboBox
      Left = 262
      Top = 107
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object OD: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 336
    Top = 216
  end
end
