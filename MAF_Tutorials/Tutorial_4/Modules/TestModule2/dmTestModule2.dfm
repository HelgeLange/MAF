object dmTM2: TdmTM2
  OldCreateOrder = False
  Height = 150
  Width = 215
  object mafModuleController1: TmafModuleController
    ModuleInfo.ModuleName = 'TestModule2.dll'
    ModuleInfo.VersionString = '1.0.000'
    ModuleInfo.ModuleID = 3
    ModuleInfo.ModuleFlags = []
    OnSubHook = mafModuleController1SubHook
    Left = 64
    Top = 40
    DataDefs = {00000000}
    MC_Data = {41FF0800FF020000010000000200000000}
    InstallData = {41FF080000030000010000000200000000}
  end
end
