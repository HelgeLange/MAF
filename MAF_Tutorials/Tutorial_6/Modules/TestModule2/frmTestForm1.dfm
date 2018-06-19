object fTestForm1: TfTestForm1
  Left = 0
  Top = 0
  Caption = 'fTestForm1'
  ClientHeight = 294
  ClientWidth = 482
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object mafPageControl1: TmafPageControl
    Left = 16
    Top = 26
    Width = 289
    Height = 193
    TabOrder = 0
    HookClient = mafHookClient1
    HookID = 12003
    PageControlOptions = [pcoAutoLoad]
  end
  object mafWindowController1: TmafWindowController
    FormID = 0
    WindowOptions = []
    InitOptions = []
    LoadSaveControl.DataID = 0
    CaptionID = 0
    Left = 408
    Top = 16
  end
  object mafHookClient1: TmafHookClient
    Left = 368
    Top = 56
  end
end
