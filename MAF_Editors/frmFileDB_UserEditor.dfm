object fFileDB_UserEditor: TfFileDB_UserEditor
  Left = 0
  Top = 0
  Caption = 'ERPFileDB User Editor'
  ClientHeight = 388
  ClientWidth = 477
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 477
    Height = 388
    ActivePage = tsUsers
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 485
    ExplicitHeight = 392
    object tsUsers: TTabSheet
      Caption = 'Users'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 8
        Top = 25
        Width = 41
        Height = 13
        Caption = 'User List'
      end
      object Label8: TLabel
        Left = 261
        Top = 11
        Width = 79
        Height = 13
        Caption = 'Encryption Key :'
      end
      object ListBox1: TListBox
        Left = 8
        Top = 44
        Width = 173
        Height = 265
        ItemHeight = 13
        TabOrder = 0
      end
      object GroupBox1: TGroupBox
        Left = 196
        Top = 40
        Width = 273
        Height = 225
        Caption = ' Editor '
        TabOrder = 1
        object Label2: TLabel
          Left = 12
          Top = 35
          Width = 25
          Height = 13
          Caption = 'Login'
        end
        object Label3: TLabel
          Left = 12
          Top = 67
          Width = 46
          Height = 13
          Caption = 'Password'
        end
        object Label4: TLabel
          Left = 12
          Top = 99
          Width = 84
          Height = 13
          Caption = 'Repeat Password'
        end
        object Label5: TLabel
          Left = 12
          Top = 126
          Width = 51
          Height = 13
          Caption = 'First Name'
        end
        object Label6: TLabel
          Left = 12
          Top = 153
          Width = 50
          Height = 13
          Caption = 'Last Name'
        end
        object Label7: TLabel
          Left = 12
          Top = 188
          Width = 54
          Height = 13
          Caption = 'User Group'
        end
        object edLogin: TEdit
          Left = 140
          Top = 32
          Width = 121
          Height = 21
          Enabled = False
          TabOrder = 0
        end
        object edPassword: TEdit
          Left = 140
          Top = 64
          Width = 121
          Height = 21
          Enabled = False
          TabOrder = 1
        end
        object edPassword2: TEdit
          Left = 140
          Top = 96
          Width = 121
          Height = 21
          Enabled = False
          TabOrder = 2
        end
        object edFirstName: TEdit
          Left = 140
          Top = 123
          Width = 121
          Height = 21
          Enabled = False
          TabOrder = 3
        end
        object edLastName: TEdit
          Left = 140
          Top = 150
          Width = 121
          Height = 21
          Enabled = False
          TabOrder = 4
        end
        object cbEditGroup: TComboBox
          Left = 116
          Top = 184
          Width = 145
          Height = 21
          Style = csDropDownList
          Enabled = False
          TabOrder = 5
        end
      end
      object btnDelete: TButton
        Left = 196
        Top = 284
        Width = 75
        Height = 25
        Caption = 'Delete'
        Enabled = False
        TabOrder = 2
      end
      object btnEdit: TButton
        Left = 296
        Top = 284
        Width = 75
        Height = 25
        Caption = 'Edit'
        Enabled = False
        TabOrder = 3
      end
      object btnNew: TButton
        Left = 394
        Top = 284
        Width = 75
        Height = 25
        Caption = 'New'
        TabOrder = 4
        OnClick = btnNewClick
      end
      object edEncryptionKey: TEdit
        Left = 348
        Top = 8
        Width = 121
        Height = 21
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Text = 'X9@zulu$$kffs87#%'
      end
    end
    object tsGroups: TTabSheet
      Caption = 'Groups'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
