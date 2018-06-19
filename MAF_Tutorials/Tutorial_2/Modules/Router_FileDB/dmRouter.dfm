object DMR: TDMR
  OldCreateOrder = False
  Height = 317
  Width = 323
  object mafModuleController1: TmafModuleController
    ModuleInfo.ModuleName = 'Router_FileDB.dll'
    ModuleInfo.VersionString = '1.0.000'
    ModuleInfo.ModuleID = 1
    ModuleInfo.ModuleFlags = []
    OnInitialized = mafModuleController1Initialized
    Left = 52
    Top = 20
    DataDefs = {00000000}
    MC_Data = {41FF0800FF020000010000000200000000}
    InstallData = {41FF080000030000010000000200000000}
  end
  object HookManager: TmafHookManager
    ModuleController = mafModuleController1
    DynamicFuntionTable.ModulePath = 'D:\Delphi\MAFramework\MAF_Tutorials\Tutorial_2\Modules\'
    BaseDB = mafFileDB1
    DataStorage = mafDataStorage1
    Left = 52
    Top = 76
  end
  object mafFileDB1: TmafFileDB
    Connected = True
    DatabaseName = 'D:\Delphi\MAFramework\MAF_Tutorials\Tutorial_2\DemoDB.maf'
    Options = [dboAutoCommit, dboAutoCreate]
    User = 'SYSDBA'
    Password = 'masterkey'
    Left = 52
    Top = 208
  end
  object mafDataStorage1: TmafDataStorage
    ModuleController = mafModuleController1
    TableName = 'MAF_DataStorage'
    BaseDB = mafFileDB1
    Left = 52
    Top = 144
  end
end
