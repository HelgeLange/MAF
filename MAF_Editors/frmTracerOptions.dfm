object fTracerOptions: TfTracerOptions
  Left = 0
  Top = 0
  Caption = 'fTracerOptions'
  ClientHeight = 299
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 16
    Width = 423
    Height = 73
    Caption = ' Options '
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 35
      Width = 67
      Height = 13
      Caption = 'Tracer LogFile'
    end
    object edMAFLogfile: TEdit
      Left = 128
      Top = 32
      Width = 273
      Height = 21
      TabOrder = 0
    end
  end
  object cbUseTracer: TCheckBox
    Left = 304
    Top = 15
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = ' Use Global Tracer'
    TabOrder = 1
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 95
    Width = 423
    Height = 178
    Caption = ' Components '
    TabOrder = 2
    object cbHookManager: TCheckBox
      Left = 16
      Top = 32
      Width = 129
      Height = 17
      Caption = 'TmafHookManager'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 55
      Width = 145
      Height = 17
      Caption = 'TmafResourceManager'
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 78
      Width = 129
      Height = 17
      Caption = 'TmafDataStorage'
      TabOrder = 2
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 101
      Width = 97
      Height = 17
      Caption = 'TmafFileDB'
      TabOrder = 3
    end
    object CheckBox5: TCheckBox
      Left = 16
      Top = 124
      Width = 129
      Height = 17
      Caption = 'TmafModuleController'
      TabOrder = 4
    end
    object CheckBox6: TCheckBox
      Left = 16
      Top = 147
      Width = 113
      Height = 17
      Caption = 'TmafGlobalVars'
      TabOrder = 5
    end
    object CheckBox7: TCheckBox
      Left = 272
      Top = 32
      Width = 129
      Height = 17
      Alignment = taLeftJustify
      Caption = 'TmafWindowManager'
      TabOrder = 6
    end
    object CheckBox8: TCheckBox
      Left = 288
      Top = 55
      Width = 113
      Height = 17
      Alignment = taLeftJustify
      Caption = 'TmafLinkManager'
      TabOrder = 7
    end
    object CheckBox9: TCheckBox
      Left = 288
      Top = 78
      Width = 113
      Height = 17
      Alignment = taLeftJustify
      Caption = 'TmafUserSecurity'
      TabOrder = 8
    end
    object CheckBox10: TCheckBox
      Left = 296
      Top = 101
      Width = 105
      Height = 17
      Alignment = taLeftJustify
      Caption = 'TmafHookClient'
      TabOrder = 9
    end
    object CheckBox11: TCheckBox
      Left = 272
      Top = 124
      Width = 129
      Height = 17
      Alignment = taLeftJustify
      Caption = 'TmafResourceClient'
      TabOrder = 10
    end
    object CheckBox12: TCheckBox
      Left = 304
      Top = 147
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'TmafLinkClient'
      TabOrder = 11
    end
  end
end
