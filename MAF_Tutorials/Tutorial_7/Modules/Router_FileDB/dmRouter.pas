unit dmRouter;

interface

uses
  SysUtils, Classes, uMAF_CustomBaseDB, uMAF_FileDB,
  uMAF_Core, uMAF_HookManager, uMAF_ModuleController, uMAF_DataStorage,
  uMAF_ResourceManager;

type
  TDMR = class(TDataModule)
    mafModuleController1: TmafModuleController;
    HookManager: TmafHookManager;
    mafFileDB1: TmafFileDB;
    mafDataStorage1: TmafDataStorage;
    ResourceManager: TmafResourceManager;
    procedure mafModuleController1Initialized(Sender: TObject; var ErrCode: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMR: TDMR;

implementation

uses Dialogs;

{$R *.dfm}

procedure TDMR.mafModuleController1Initialized(Sender: TObject; var ErrCode: Integer);
begin
  HookManager.DynamicFuntionTable.ModulePath := ExtractFilePath(ParamStr(0)) + 'Modules\';
  HookManager.Connected := True;
end;

end.
