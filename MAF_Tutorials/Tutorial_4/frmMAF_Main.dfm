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
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 312
    Top = 78
    Width = 153
    Height = 25
    Caption = 'DynamicFunction 12000'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 160
    Top = 80
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object Button2: TButton
    Left = 312
    Top = 109
    Width = 153
    Height = 25
    Caption = 'DynamicFunction 12001'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Edit2: TEdit
    Left = 160
    Top = 111
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object mafManagerLoader1: TmafManagerLoader
    Manager.Strings = (
      'HookManager')
    AutoLoad = True
    ManagerSubDirectory = 'Modules'
    OnDataLoad = mafManagerLoader1DataLoad
    Left = 44
    Top = 20
  end
  object mafHookClient1: TmafHookClient
    AfterExecHook = mafHookClient1AfterExecHook
    BeforeCallRouter = mafHookClient1BeforeCallRouter
    AfterCallRouter = mafHookClient1AfterCallRouter
    Left = 40
    Top = 72
  end
end
