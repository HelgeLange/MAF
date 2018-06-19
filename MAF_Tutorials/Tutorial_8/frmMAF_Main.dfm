object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 356
  ClientWidth = 491
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 116
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 320
    Top = 22
    Width = 153
    Height = 25
    Caption = 'DynamicFunction 12000'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 168
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object Button2: TButton
    Left = 320
    Top = 53
    Width = 153
    Height = 25
    Caption = 'DynamicFunction 12001'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Edit2: TEdit
    Left = 168
    Top = 55
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object mafFramePanel1: TmafFramePanel
    Left = 24
    Top = 144
    Width = 449
    Height = 181
    Caption = 'mafFramePanel1'
    TabOrder = 4
    UniqueID = 0
    HookID = 0
    SubHookID = 0
    HookClient = mafHookClient1
    ControlOptions = []
  end
  object mafManagerLoader1: TmafManagerLoader
    Manager.Strings = (
      'HookManager'
      'ResourceManager')
    AutoLoad = True
    ManagerSubDirectory = 'Modules'
    OnDataLoad = mafManagerLoader1DataLoad
    Left = 44
    Top = 20
  end
  object mafHookClient1: TmafHookClient
    Streamer.Attributes = 0
    Streamer.TemplateID = 0
    AfterExecHook = mafHookClient1AfterExecHook
    BeforeCallRouter = mafHookClient1BeforeCallRouter
    AfterCallRouter = mafHookClient1AfterCallRouter
    Left = 40
    Top = 72
  end
  object MainMenu1: TMainMenu
    Left = 104
    object ests1: TMenuItem
      Caption = 'Tests'
      object Tutorial5Form1oldfashioned1: TMenuItem
        Tag = 12002
        Caption = 'Tutorial 5 - Form1 old fashioned'
        OnClick = Tutorial5Form1oldfashioned1Click
      end
    end
  end
  object mafResourceClient1: TmafResourceClient
    Left = 92
    Top = 68
    RC_Data = {1900000075030000010000000200000000}
  end
end
