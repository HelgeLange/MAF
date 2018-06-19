unit dmTestModule2;

interface

uses
  SysUtils, Classes, uMAF_Core, uMAF_ModuleController;

type
  TdmTM2 = class(TDataModule)
    mafModuleController1: TmafModuleController;
    procedure mafModuleController1SubHook(SubHookID: Integer; QHS: pQHS;
      UserParam: Pointer; var ErrCode: Integer);
    procedure mafModuleController1Loaded(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmTM2: TdmTM2;

implementation

uses frmTestForm1;

{$R *.dfm}

procedure TdmTM2.mafModuleController1Loaded(Sender: TObject);
begin
  mafModuleController1.RegisterFormClass(101001, TfTestForm1);
end;

procedure TdmTM2.mafModuleController1SubHook(SubHookID: Integer; QHS: pQHS;
  UserParam: Pointer; var ErrCode: Integer);
var frm : TfTestForm1;
begin
  Case SubHookID Of
    101000 : QHS^.ResVal := QHS^.ResVal + 15;
    101001 : begin
               frm := TfTestForm1.Create(self);
               try
                 frm.ShowModal;
               finally
                 frm.Free;
               end;
             end;
  end;
end;

end.
