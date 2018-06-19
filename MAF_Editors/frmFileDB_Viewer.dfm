object fFileDB_Viewer: TfFileDB_Viewer
  Left = 0
  Top = 0
  Caption = 'fFileDB_Viewer'
  ClientHeight = 351
  ClientWidth = 602
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
  object tvDirectory: TTreeView
    Left = 16
    Top = 40
    Width = 177
    Height = 242
    DragMode = dmAutomatic
    HideSelection = False
    Indent = 19
    MultiSelectStyle = []
    TabOrder = 0
    OnChange = tvDirectoryChange
  end
  object lvFileRes: TListView
    Left = 208
    Top = 40
    Width = 369
    Height = 242
    Columns = <
      item
        Caption = 'ID'
        Width = 60
      end
      item
        Caption = 'Name'
        Tag = 1
        Width = 150
      end
      item
        Caption = 'Size'
        Tag = 2
        Width = 65
      end>
    DragMode = dmAutomatic
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnSelectItem = lvFileResSelectItem
  end
  object btnDelete: TButton
    Left = 464
    Top = 288
    Width = 113
    Height = 25
    Caption = 'Delete'
    Enabled = False
    TabOrder = 2
    OnClick = btnDeleteClick
  end
end
