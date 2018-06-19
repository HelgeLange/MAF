unit dmTestModule;

interface

uses
  SysUtils, Classes, uMAF_Core, uMAF_ModuleController;

type
  TDMTM = class(TDataModule)
    mafModuleController1: TmafModuleController;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMTM: TDMTM;

implementation

{$R *.dfm}

end.
