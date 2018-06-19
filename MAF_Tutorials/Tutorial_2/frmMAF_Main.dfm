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
  object mafManagerLoader1: TmafManagerLoader
    Manager.Strings = (
      'HookManager')
    AutoLoad = True
    ManagerSubDirectory = 'Modules'
    OnDataLoad = mafManagerLoader1DataLoad
    Left = 44
    Top = 20
  end
end
