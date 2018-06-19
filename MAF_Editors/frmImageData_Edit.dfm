inherited fImageData_Edit: TfImageData_Edit
  Caption = 'Image Data Editor'
  ClientWidth = 390
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage [2]
    Left = 16
    Top = 16
    Width = 128
    Height = 128
    Center = True
    Transparent = True
  end
  object Label1: TLabel [3]
    Left = 160
    Top = 48
    Width = 38
    Height = 13
    Caption = 'Height :'
  end
  object Label2: TLabel [4]
    Left = 160
    Top = 67
    Width = 35
    Height = 13
    Caption = 'Width :'
  end
  object Label3: TLabel [5]
    Left = 160
    Top = 104
    Width = 49
    Height = 13
    Caption = 'Filename :'
  end
  object Label6: TLabel [6]
    Left = 160
    Top = 19
    Width = 66
    Height = 13
    Caption = 'Resource ID :'
  end
  object lHeight: TLabel [7]
    Left = 248
    Top = 48
    Width = 121
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lHeight'
  end
  object lWidth: TLabel [8]
    Left = 248
    Top = 67
    Width = 121
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lWidth'
  end
  inherited cbCategory: TComboBox
    TabOrder = 3
  end
  inherited cbModule: TComboBox
    TabOrder = 4
  end
  object edFileName: TEdit
    Left = 160
    Top = 123
    Width = 209
    Height = 21
    TabOrder = 0
    OnExit = edResIDExit
  end
  object edResID: TEdit
    Left = 248
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 1
    OnExit = edResIDExit
  end
end
