object fCreateDevAccount: TfCreateDevAccount
  Left = 0
  Top = 0
  Caption = 'Create Developer Account'
  ClientHeight = 368
  ClientWidth = 447
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
  object Label4: TLabel
    Left = 16
    Top = 20
    Width = 409
    Height = 85
    AutoSize = False
    Caption = 'LabelText'
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 120
    Width = 409
    Height = 161
    Caption = ' Account Data '
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 31
      Width = 59
      Height = 13
      Caption = 'Group Name'
    end
    object Label2: TLabel
      Left = 12
      Top = 71
      Width = 25
      Height = 13
      Caption = 'Login'
    end
    object Label3: TLabel
      Left = 12
      Top = 98
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label5: TLabel
      Left = 12
      Top = 125
      Width = 74
      Height = 13
      Caption = 'Login Password'
    end
    object edGroup: TEdit
      Left = 216
      Top = 28
      Width = 177
      Height = 21
      TabOrder = 0
      Text = 'Developer'
    end
    object edLogin: TEdit
      Left = 216
      Top = 68
      Width = 177
      Height = 21
      TabOrder = 1
    end
    object edPassword: TEdit
      Left = 216
      Top = 95
      Width = 177
      Height = 21
      TabOrder = 2
    end
    object edProgrammPassword: TEdit
      Left = 216
      Top = 122
      Width = 177
      Height = 21
      TabOrder = 3
    end
  end
  object btnCreate: TButton
    Left = 288
    Top = 308
    Width = 137
    Height = 25
    Caption = 'Create Account'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnCreateClick
  end
  object btnCancel: TButton
    Left = 16
    Top = 308
    Width = 137
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
