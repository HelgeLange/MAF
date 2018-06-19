object fResourceClient_Editor: TfResourceClient_Editor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'ResourceClient Editor'
  ClientHeight = 446
  ClientWidth = 445
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 349
    Width = 157
    Height = 13
    Caption = 'ResourceID for selected element'
  end
  object Label2: TLabel
    Left = 16
    Top = 16
    Width = 401
    Height = 65
    AutoSize = False
    Caption = 
      'Check all components that should be updated automatically by the' +
      ' ResClient. To have a component updated you'#39'll need  to assign a' +
      ' ResourceID. To have images updated by the IconSkin event you ha' +
      've to check those, too. If a component isn'#39't shown here, the Res' +
      'Client doesn'#39't knowit and therefore cannot update it. Use the Af' +
      'terLanguageChange event to do it yourself.'
    WordWrap = True
  end
  object lvComponents: TListView
    Left = 16
    Top = 112
    Width = 401
    Height = 217
    Checkboxes = True
    Columns = <
      item
        Caption = 'Component Name'
        Width = 200
      end
      item
        Alignment = taRightJustify
        Caption = 'ResID'
        Width = 70
      end
      item
        Alignment = taRightJustify
        Caption = 'Component Type'
        Width = 110
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = lvComponentsClick
    OnSelectItem = lvComponentsSelectItem
  end
  object edResID: TEdit
    Left = 224
    Top = 346
    Width = 153
    Height = 21
    TabOrder = 1
    OnChange = edResIDChange
  end
  object Panel1: TPanel
    Left = -16
    Top = 379
    Width = 465
    Height = 65
    BevelInner = bvLowered
    TabOrder = 2
    object btnClose: TButton
      Left = 320
      Top = 16
      Width = 113
      Height = 25
      Caption = 'Close'
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
  object Button1: TButton
    Left = 383
    Top = 344
    Width = 34
    Height = 25
    Caption = '[...]'
    TabOrder = 3
    OnClick = Button1Click
  end
end
