inherited fStringData_Editor: TfStringData_Editor
  Caption = 'fStringData_Editor'
  ClientHeight = 322
  ClientWidth = 394
  ExplicitWidth = 400
  ExplicitHeight = 346
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    AlignWithMargins = True
    Left = 3
    Top = 116
    Width = 388
    Height = 13
    Align = alTop
    Caption = 'Text :'
    ExplicitWidth = 29
  end
  inherited Panel1: TPanel
    Top = 265
    Width = 394
    Height = 57
    TabOrder = 1
    ExplicitTop = 265
    ExplicitWidth = 383
    ExplicitHeight = 57
    inherited Panel3: TPanel
      Left = 205
      Width = 187
      Height = 53
      ExplicitLeft = 196
      ExplicitWidth = 187
      ExplicitHeight = 53
      inherited btnClose: TButton
        Width = 111
        OnClick = btnCloseClick
        ExplicitWidth = 109
      end
    end
    object btnAddNext: TButton
      Left = 17
      Top = 16
      Width = 115
      Height = 25
      Hint = 
        'Adds a new entry with the next free ID behind the current and ke' +
        'eps Category and Module'
      Caption = 'Add Next'
      TabOrder = 1
      OnClick = btnAddNextClick
    end
  end
  inherited Panel2: TPanel
    Top = 0
    Width = 394
    Height = 113
    Align = alTop
    TabOrder = 2
    ExplicitTop = 0
    ExplicitWidth = 383
    ExplicitHeight = 113
    inherited Label4: TLabel
      Left = 17
      Top = 51
      ExplicitLeft = 17
      ExplicitTop = 51
    end
    inherited Label5: TLabel
      Left = 17
      Top = 86
      ExplicitLeft = 17
      ExplicitTop = 86
    end
    object lStringID: TLabel [2]
      Left = 17
      Top = 16
      Width = 42
      Height = 13
      Caption = 'String ID'
    end
    inherited cbCategory: TComboBox
      Top = 48
      ExplicitTop = 48
    end
    inherited cbModule: TComboBox
      Top = 83
      ExplicitTop = 83
    end
    object edStringID: TEdit
      Left = 248
      Top = 13
      Width = 121
      Height = 21
      TabOrder = 2
      OnExit = edStringIDExit
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 132
    Width = 394
    Height = 133
    Align = alClient
    TabOrder = 0
    OnExit = edStringIDExit
    ExplicitWidth = 383
  end
end
