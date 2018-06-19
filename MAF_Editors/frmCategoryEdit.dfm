object fCategoryEdit: TfCategoryEdit
  Left = 0
  Top = 0
  Caption = 'Edit categories'
  ClientHeight = 279
  ClientWidth = 307
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
  object ListBox1: TListBox
    Left = 16
    Top = 32
    Width = 161
    Height = 228
    ItemHeight = 13
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 16
    Top = 239
    Width = 161
    Height = 21
    TabOrder = 1
    Visible = False
  end
  object btnDelete: TButton
    Left = 183
    Top = 237
    Width = 106
    Height = 25
    Caption = 'Delete'
    TabOrder = 2
    OnClick = btnDeleteClick
  end
  object btnEdit: TButton
    Left = 183
    Top = 206
    Width = 106
    Height = 25
    Caption = 'Edit'
    TabOrder = 3
    OnClick = btnEditClick
  end
  object btnAdd: TButton
    Left = 183
    Top = 175
    Width = 106
    Height = 25
    Caption = 'Add'
    TabOrder = 4
    OnClick = btnAddClick
  end
end
