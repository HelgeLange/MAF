unit dmTestModule;

interface

uses
  SysUtils, Classes, uMAF_Core, uMAF_ModuleController;

type
  TDMTM = class(TDataModule)
    mafModuleController1: TmafModuleController;
    procedure mafModuleController1SubHook(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
    procedure mafModuleController1Loaded(Sender: TObject);
  private
    procedure SubHook_100001(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
    procedure __FreePChar(pData: Pointer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMTM: TDMTM;

implementation

uses uMAF_Tools, frmPageControl1, frmPageControl3;

{$R *.dfm}

procedure TDMTM.__FreePChar(pData: Pointer);
begin
  FreePChar(PChar(pData));
end;

procedure TDMTM.mafModuleController1Loaded(Sender: TObject);
begin
  mafModuleController1.RegisterSubHookEvent(100001, SubHook_100001);
  mafModuleController1.RegisterFormClass(100002, TfPageControl1);
  mafModuleController1.RegisterFormClass(100003, TfPageControl3);
end;

procedure TDMTM.mafModuleController1SubHook(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  Case SubHookID Of
    100000 : begin
               StrToPChar('Hello world !', PChar(QHS^.pChildObj));
               QHS^.pFreeMemFunc := __FreePChar;
             end;
    100001 : QHS^.ResVal := QHS^.ResVal + 5;
  end;
end;

procedure TDMTM.SubHook_100001(SubHookID: Integer; QHS: pQHS;
  UserParam: Pointer; var ErrCode: Integer);
begin
  QHS^.ResVal := QHS^.ResVal + 13;
end;

end.
