object fFileLoggerEditor: TfFileLoggerEditor
  Left = 0
  Top = 0
  Caption = 'TmafFileLogger Property Editor'
  ClientHeight = 232
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 12
    Width = 305
    Height = 29
    AutoSize = False
    Caption = 
      'TmafFileLogger can use its macros to replace parts of the filena' +
      'me with actual informations. Those macros are :'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 52
    Width = 55
    Height = 13
    Caption = '%DATE%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 16
    Top = 72
    Width = 109
    Height = 13
    Caption = '%WORKSTATION%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 16
    Top = 92
    Width = 55
    Height = 13
    Caption = '%USER%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 136
    Top = 52
    Width = 185
    Height = 13
    AutoSize = False
    Caption = 'Actual date'
  end
  object Label6: TLabel
    Left = 136
    Top = 72
    Width = 185
    Height = 13
    AutoSize = False
    Caption = 'Workstation name'
  end
  object Label7: TLabel
    Left = 136
    Top = 92
    Width = 185
    Height = 13
    AutoSize = False
    Caption = 'User name, who is logged in'
  end
  object Label8: TLabel
    Left = 16
    Top = 111
    Width = 305
    Height = 29
    AutoSize = False
    Caption = 
      'If loFilePerType is set, the filename will always start with the' +
      ' prefix [Type]'
    WordWrap = True
  end
  object edLogfileName: TEdit
    Left = 16
    Top = 144
    Width = 305
    Height = 21
    TabOrder = 0
    OnChange = edLogfileNameChange
  end
  object Panel1: TPanel
    Left = -12
    Top = 180
    Width = 365
    Height = 97
    BevelInner = bvLowered
    TabOrder = 1
    object btnCancel: TButton
      Left = 232
      Top = 16
      Width = 101
      Height = 25
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object btnOk: TButton
      Left = 125
      Top = 16
      Width = 101
      Height = 25
      Caption = 'Okay'
      Default = True
      TabOrder = 1
      OnClick = btnOkClick
    end
  end
end
