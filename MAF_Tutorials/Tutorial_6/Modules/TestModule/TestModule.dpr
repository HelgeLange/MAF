library TestModule;

uses
  SysUtils,
  Classes,
  uModuleService in '..\..\..\..\uModuleService.pas',
  dmTestModule in 'dmTestModule.pas' {DMTM: TDataModule},
  frmPageControl1 in 'frmPageControl1.pas' {fPageControl1},
  frmPageControl3 in 'frmPageControl3.pas' {fPageControl3};

{$R *.res}

begin
  __RegisterDMClass(TDMTM);
end.
