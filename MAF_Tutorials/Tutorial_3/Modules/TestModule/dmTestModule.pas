unit dmTestModule;

interface

uses
  SysUtils, Classes, uMAF_Core, uMAF_ModuleController, uMAF_Globals;

type
  TDMTM = class(TDataModule)
    mafModuleController1: TmafModuleController;
    procedure mafModuleController1SubHook(SubHookID: Integer; QHS: pQHS;
      UserParam: Pointer; var ErrCode: Integer);
  private
    procedure __FreePChar(pData: Pointer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMTM: TDMTM;

implementation

uses uMAF_Tools;

{$R *.dfm}

procedure TDMTM.__FreePChar(pData: Pointer);
begin
  FreePChar(PChar(pData));
end;

procedure TDMTM.mafModuleController1SubHook(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  Case SubHookID Of
    100000 : begin
               StrToPChar('Hello world !', PChar(QHS^.pChildObj));
               QHS^.pFreeMemFunc := __FreePChar;
             end;
  end;
end;

end.
