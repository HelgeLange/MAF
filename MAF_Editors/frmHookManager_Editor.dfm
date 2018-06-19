object fDFTEdit: TfDFTEdit
  Left = 202
  Top = 81
  Caption = 'DFT Editor'
  ClientHeight = 545
  ClientWidth = 787
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 787
    Height = 545
    ActivePage = tsLibraries
    Align = alClient
    TabOrder = 0
    object tsLibraries: TTabSheet
      Caption = 'Libraries'
      ImageIndex = 2
      object Label4: TLabel
        Left = 12
        Top = 8
        Width = 69
        Height = 13
        Caption = 'Module path : '
      end
      object Label6: TLabel
        Left = 87
        Top = 8
        Width = 3
        Height = 13
      end
      object lvModules: TListView
        Left = 12
        Top = 27
        Width = 273
        Height = 358
        Columns = <
          item
            Caption = 'Module Name'
            Width = 250
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = lvModulesSelectItem
      end
      object GroupBox1: TGroupBox
        Left = 291
        Top = 21
        Width = 254
        Height = 247
        Caption = ' Module information '
        TabOrder = 1
        object Label1: TLabel
          Left = 16
          Top = 24
          Width = 54
          Height = 13
          Caption = 'ID             :'
        end
        object lblID: TLabel
          Left = 72
          Top = 24
          Width = 167
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
        end
        object lblVersion: TLabel
          Left = 72
          Top = 43
          Width = 167
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
        end
        object Label3: TLabel
          Left = 16
          Top = 43
          Width = 54
          Height = 13
          Caption = 'Version     :'
        end
        object lblCopyright: TLabel
          Left = 72
          Top = 62
          Width = 167
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
        end
        object Label5: TLabel
          Left = 16
          Top = 62
          Width = 54
          Height = 13
          Caption = 'Copyright :'
        end
        object lblBuildDate: TLabel
          Left = 72
          Top = 81
          Width = 167
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
        end
        object Label7: TLabel
          Left = 16
          Top = 81
          Width = 54
          Height = 13
          Caption = 'Build date :'
        end
        object lblAuthor: TLabel
          Left = 72
          Top = 100
          Width = 167
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
        end
        object Label9: TLabel
          Left = 16
          Top = 100
          Width = 55
          Height = 13
          Caption = 'Author      :'
        end
        object Label2: TLabel
          Left = 16
          Top = 129
          Width = 60
          Height = 13
          Caption = 'Description :'
        end
        object mDescription: TMemo
          Left = 16
          Top = 148
          Width = 225
          Height = 89
          Enabled = False
          ReadOnly = True
          TabOrder = 0
        end
      end
      object btnAddModule: TButton
        Left = 291
        Top = 329
        Width = 98
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddModuleClick
      end
      object btnDelete: TButton
        Left = 291
        Top = 360
        Width = 98
        Height = 25
        Caption = 'Delete'
        TabOrder = 3
        OnClick = btnDeleteClick
      end
      object GroupBox2: TGroupBox
        Left = 551
        Top = 21
        Width = 222
        Height = 324
        Caption = ' XML Import / Export '
        TabOrder = 4
        object Panel1: TPanel
          Left = -29
          Top = -9
          Width = 280
          Height = 218
          BevelInner = bvLowered
          TabOrder = 0
          object btnXML_Import: TButton
            Left = 40
            Top = 159
            Width = 201
            Height = 25
            Caption = 'Import from XML'
            TabOrder = 0
            OnClick = btnXML_ImportClick
          end
          object cbClearCurrentData: TCheckBox
            Left = 40
            Top = 32
            Width = 161
            Height = 17
            Hint = 
              'All data about modules, function and descriptions will be delete' +
              'd prior importing  the XML'
            Caption = 'Clear Current Data'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = cbClearCurrentDataClick
          end
          object cbImportDescriptions: TCheckBox
            Left = 40
            Top = 55
            Width = 169
            Height = 17
            Caption = 'Import Descriptions'
            Checked = True
            State = cbChecked
            TabOrder = 2
          end
          object cbUpdateExistingData: TCheckBox
            Left = 40
            Top = 78
            Width = 177
            Height = 17
            Caption = 'Update Existing Data'
            TabOrder = 3
          end
        end
        object Panel2: TPanel
          Left = -13
          Top = 203
          Width = 246
          Height = 126
          BevelInner = bvLowered
          TabOrder = 1
          object btnXML_Export: TButton
            Left = 24
            Top = 76
            Width = 201
            Height = 25
            Caption = 'Export to XML'
            TabOrder = 0
            OnClick = btnXML_ExportClick
          end
          object cbExportDescriptions: TCheckBox
            Left = 24
            Top = 21
            Width = 193
            Height = 17
            Caption = 'Export Descriptions'
            TabOrder = 1
          end
        end
      end
      object Button2: TButton
        Left = 562
        Top = 360
        Width = 201
        Height = 25
        Caption = 'Clear Garbage'
        TabOrder = 5
        OnClick = Button2Click
      end
    end
    object tsEdit: TTabSheet
      Caption = 'Editor'
      OnShow = tsEditShow
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 779
        Height = 517
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Splitter1: TSplitter
          Left = 0
          Top = 253
          Width = 779
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          ExplicitLeft = 3
          ExplicitTop = 305
        end
        object Panel5: TPanel
          Left = 0
          Top = 256
          Width = 779
          Height = 261
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          object Panel9: TPanel
            Left = 663
            Top = 0
            Width = 116
            Height = 261
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 0
            object btnUp: TBitBtn
              Left = 13
              Top = 24
              Width = 42
              Height = 25
              Caption = 'Up'
              TabOrder = 0
              OnClick = btnUpDownClick
            end
            object btnDown: TBitBtn
              Left = 13
              Top = 55
              Width = 41
              Height = 25
              Caption = 'Down'
              TabOrder = 1
              OnClick = btnUpDownClick
            end
            object Panel14: TPanel
              Left = 0
              Top = 108
              Width = 116
              Height = 153
              Align = alBottom
              BevelOuter = bvNone
              TabOrder = 2
              object btnNewSubHook: TButton
                Left = 15
                Top = 44
                Width = 90
                Height = 25
                Caption = 'New SubHook'
                TabOrder = 0
                OnClick = btnEditSubHookClick
              end
              object btnEditSubHook: TButton
                Left = 15
                Top = 81
                Width = 90
                Height = 25
                Caption = 'Edit SubHook'
                Enabled = False
                TabOrder = 1
                OnClick = btnEditSubHookClick
              end
              object btnDeleteSubHook: TButton
                Left = 15
                Top = 112
                Width = 90
                Height = 25
                Caption = 'Delete SubHook'
                TabOrder = 2
                OnClick = btnDeleteSubHookClick
              end
            end
          end
          object Panel4: TPanel
            Left = 0
            Top = 0
            Width = 663
            Height = 261
            Align = alClient
            TabOrder = 1
            object lvSubHooks: TListView
              Left = 1
              Top = 21
              Width = 661
              Height = 239
              Align = alClient
              Checkboxes = True
              Columns = <
                item
                  Caption = 'UniqueID'
                  Width = 70
                end
                item
                  Caption = 'SubHookID'
                  Width = 70
                end
                item
                  Caption = 'Module'
                  Width = 120
                end
                item
                  Caption = 'Description'
                  Width = 260
                end
                item
                  Caption = 'CodeGroup'
                  Width = 100
                end>
              HideSelection = False
              ReadOnly = True
              RowSelect = True
              PopupMenu = PopupMenu1
              TabOrder = 0
              ViewStyle = vsReport
              OnClick = lvSubHooksClick
            end
            object Panel11: TPanel
              Left = 1
              Top = 1
              Width = 661
              Height = 20
              Align = alTop
              TabOrder = 1
              object Panel12: TPanel
                Left = 1
                Top = 1
                Width = 312
                Height = 18
                Align = alLeft
                BevelOuter = bvNone
                TabOrder = 0
                object Label11: TLabel
                  Left = 0
                  Top = 0
                  Width = 312
                  Height = 18
                  Align = alClient
                  Caption = 'SubHooks'
                  ExplicitWidth = 47
                  ExplicitHeight = 13
                end
              end
              object Panel13: TPanel
                Left = 313
                Top = 1
                Width = 347
                Height = 18
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 1
                object bFilterSubHooks: TCheckBox
                  Left = 0
                  Top = 0
                  Width = 347
                  Height = 18
                  Align = alClient
                  Alignment = taLeftJustify
                  Caption = 'Apply module filter for SubHooks'
                  TabOrder = 0
                end
              end
            end
          end
        end
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 779
          Height = 253
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object Label10: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 40
            Width = 773
            Height = 13
            Align = alTop
            Caption = 'Dynamic functions (Hooks) :'
            ExplicitWidth = 134
          end
          object lvHooks: TListView
            Left = 0
            Top = 56
            Width = 663
            Height = 197
            Align = alClient
            Columns = <
              item
                Caption = 'HookID'
                Width = 70
              end
              item
                Caption = 'SubHooks'
                Width = 70
              end
              item
                Caption = 'Description'
                Width = 480
              end>
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnClick = lvHooksClick
            OnSelectItem = lvHooksSelectItem
          end
          object Panel7: TPanel
            Left = 0
            Top = 0
            Width = 779
            Height = 37
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object Label8: TLabel
              Left = 12
              Top = 13
              Width = 66
              Height = 13
              Caption = 'Module filter :'
            end
            object cbModules: TComboBox
              Left = 304
              Top = 9
              Width = 357
              Height = 21
              Style = csDropDownList
              TabOrder = 0
            end
          end
          object Panel8: TPanel
            Left = 663
            Top = 56
            Width = 116
            Height = 197
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 2
            object Panel10: TPanel
              Left = 0
              Top = 156
              Width = 116
              Height = 41
              Align = alBottom
              BevelOuter = bvNone
              TabOrder = 0
              object btnEditHook: TButton
                Left = 13
                Top = 4
                Width = 90
                Height = 25
                Caption = 'Edit Hook'
                Enabled = False
                TabOrder = 0
                OnClick = btnEditHookClick
              end
            end
          end
        end
      end
    end
    object tsCodeGroups: TTabSheet
      Caption = 'Code Groups'
      ImageIndex = 2
      object pCodeGroups: TPanel
        Left = 3
        Top = 17
        Width = 758
        Height = 192
        BevelInner = bvLowered
        BevelOuter = bvSpace
        TabOrder = 0
        object Label12: TLabel
          Left = 9
          Top = 9
          Width = 62
          Height = 13
          Caption = 'Code Groups'
        end
        object lvCodeGroups: TListView
          Left = 9
          Top = 28
          Width = 616
          Height = 150
          Columns = <
            item
              Caption = 'ID'
            end
            item
              Caption = 'Name'
              Width = 120
            end
            item
              Caption = 'Description'
              Width = 260
            end
            item
              Caption = 'SecurityLevel'
              Width = 80
            end
            item
              Caption = 'Member'
            end>
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnSelectItem = lvCodeGroupsSelectItem
        end
        object btnCG_Delete: TButton
          Left = 639
          Top = 154
          Width = 106
          Height = 25
          Caption = 'Delete'
          Enabled = False
          TabOrder = 1
          OnClick = btnCG_DeleteClick
        end
        object btnCG_Edit: TButton
          Left = 639
          Top = 123
          Width = 106
          Height = 25
          Caption = 'Edit'
          Enabled = False
          TabOrder = 2
          OnClick = btnCG_EditClick
        end
        object btnCG_Add: TButton
          Left = 639
          Top = 92
          Width = 106
          Height = 25
          Caption = 'Add'
          TabOrder = 3
          OnClick = btnCG_AddClick
        end
      end
      object pHooks: TPanel
        Left = 3
        Top = 215
        Width = 758
        Height = 249
        BevelInner = bvLowered
        TabOrder = 1
        object Label13: TLabel
          Left = 9
          Top = 13
          Width = 105
          Height = 13
          Caption = 'Unassigned SubHooks'
        end
        object Label14: TLabel
          Left = 376
          Top = 13
          Width = 93
          Height = 13
          Caption = 'Assigned SubHooks'
        end
        object lvUnassignedSubHooks: TListView
          Left = 9
          Top = 32
          Width = 296
          Height = 209
          Columns = <
            item
              Caption = 'HookID'
              Width = 60
            end
            item
              Caption = 'SubHookID'
              Width = 70
            end
            item
              Caption = 'Module'
              Width = 100
            end>
          ReadOnly = True
          RowSelect = True
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          ViewStyle = vsReport
          OnInfoTip = lvUnassignedSubHooksInfoTip
          OnSelectItem = lvUnassignedSubHooksSelectItem
        end
        object lvAssignedSubHooks: TListView
          Left = 432
          Top = 32
          Width = 313
          Height = 209
          Columns = <
            item
              Caption = 'HookID'
              Width = 60
            end
            item
              Caption = 'SubHookID'
              Width = 70
            end
            item
              Caption = 'Module'
              Width = 100
            end>
          ReadOnly = True
          RowSelect = True
          TabOrder = 1
          ViewStyle = vsReport
          OnSelectItem = lvAssignedSubHooksSelectItem
        end
        object btnCG_HookAdd: TButton
          Left = 323
          Top = 49
          Width = 92
          Height = 25
          Caption = 'Add -->'
          Enabled = False
          TabOrder = 2
          OnClick = btnCG_HookAddClick
        end
        object btnCG_HookRemove: TButton
          Left = 323
          Top = 208
          Width = 92
          Height = 25
          Caption = '<-- Remove'
          Enabled = False
          TabOrder = 3
          OnClick = btnCG_HookRemoveClick
        end
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.dll'
    Title = 'Select a module...'
    Left = 504
    Top = 8
  end
  object XML: TXMLDocument
    FileName = 'D:\Delphi\TCA\TCA_DFT.xml'
    Left = 544
    Top = 8
    DOMVendorDesc = 'MSXML'
  end
  object SD: TSaveDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Title = 'Save DFT as XML...'
    Left = 464
    Top = 8
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 168
    Top = 354
    object mForce_uID_Change: TMenuItem
      Caption = 'Force uID Change'
      OnClick = mForce_uID_ChangeClick
    end
  end
end
