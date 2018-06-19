unit dmTestModule2;

interface

uses
  SysUtils, Classes, uMAF_Core, uMAF_ModuleController;

type
  TdmTM2 = class(TDataModule)
    mafModuleController1: TmafModuleController;
    procedure mafModuleController1SubHook(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmTM2: TdmTM2;

implementation

{$R *.dfm}

procedure TdmTM2.mafModuleController1SubHook(SubHookID: Integer; QHS: pQHS;
  UserParam: Pointer; var ErrCode: Integer);
begin
  Case SubHookID Of
    101000 : QHS^.ResVal := QHS^.ResVal + 15;
  end;
end;

end.
