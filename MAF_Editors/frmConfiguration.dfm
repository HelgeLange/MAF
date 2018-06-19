object fConfiguration: TfConfiguration
  Left = 0
  Top = 0
  Caption = 'MAF Components Configuration'
  ClientHeight = 364
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnSave: TButton
    Left = 331
    Top = 328
    Width = 119
    Height = 25
    Caption = 'Save'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object btnCancel: TButton
    Left = 187
    Top = 328
    Width = 119
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 441
    Height = 305
    ActivePage = tsSynchro
    TabOrder = 2
    object tsSynchro: TTabSheet
      Caption = 'Synchro Options'
      object GroupBox1: TGroupBox
        Left = 3
        Top = 88
        Width = 424
        Height = 138
        Caption = ' ResourceManager '
        TabOrder = 0
        object Label1: TLabel
          Left = 25
          Top = 38
          Width = 129
          Height = 13
          AutoSize = False
          Caption = 'FileResource FileName'
        end
        object Label2: TLabel
          Left = 25
          Top = 66
          Width = 129
          Height = 13
          AutoSize = False
          Caption = 'StringResource FileName'
        end
        object Label3: TLabel
          Left = 25
          Top = 94
          Width = 129
          Height = 13
          AutoSize = False
          Caption = 'SQLResource FileName'
        end
        object edFileRes: TEdit
          Left = 160
          Top = 34
          Width = 216
          Height = 21
          TabOrder = 0
        end
        object btnSearchFileRes: TButton
          Tag = 1
          Left = 382
          Top = 32
          Width = 25
          Height = 25
          Caption = '[...]'
          TabOrder = 1
          OnClick = btnSearchFileResClick
        end
        object edStringRes: TEdit
          Left = 160
          Top = 63
          Width = 216
          Height = 21
          TabOrder = 2
        end
        object btnSearchStringRes: TButton
          Tag = 2
          Left = 382
          Top = 61
          Width = 25
          Height = 25
          Caption = '[...]'
          TabOrder = 3
          OnClick = btnSearchFileResClick
        end
        object edSQLRes: TEdit
          Left = 160
          Top = 92
          Width = 216
          Height = 21
          TabOrder = 4
        end
        object btnSearchSQLRes: TButton
          Tag = 3
          Left = 382
          Top = 88
          Width = 25
          Height = 25
          Caption = '[...]'
          TabOrder = 5
          OnClick = btnSearchFileResClick
        end
      end
      object gbSettings: TGroupBox
        Left = 3
        Top = 12
        Width = 423
        Height = 77
        Caption = ' Dynamic Function Table (DFT) Synchronization '
        TabOrder = 1
        object cbSynchro: TCheckBox
          Left = 16
          Top = 32
          Width = 391
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Enable DFT Synchronization through XML file '
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
      end
    end
  end
  object XML: TXMLDocument
    Left = 64
    Top = 272
    DOMVendorDesc = 'MSXML'
  end
  object OD: TOpenDialog
    DefaultExt = '*.maf'
    FileName = '*.maf'
    Filter = '*.maf|MAF Data files|*.*|All files'
    FilterIndex = 0
    Left = 332
    Top = 8
  end
end
