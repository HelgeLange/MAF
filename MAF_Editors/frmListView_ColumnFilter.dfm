object fListView_ColumnFilter: TfListView_ColumnFilter
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'fListView_ColumnFilter'
  ClientHeight = 78
  ClientWidth = 151
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object clbFilterItems: TCheckListBox
    Left = 0
    Top = 80
    Width = 151
    Height = 69
    OnClickCheck = clbFilterItemsClickCheck
    ItemHeight = 13
    TabOrder = 0
  end
  object btnApply: TButton
    Left = 0
    Top = 53
    Width = 151
    Height = 25
    Align = alBottom
    Caption = 'Apply'
    TabOrder = 1
    OnClick = btnApplyClick
  end
  object cbShowAll: TCheckBox
    Left = 0
    Top = 0
    Width = 151
    Height = 17
    Align = alTop
    Caption = 'Show All'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = cbShowAllClick
  end
  object edText: TEdit
    Left = 0
    Top = 23
    Width = 121
    Height = 21
    TabOrder = 3
    OnChange = edTextChange
  end
end
