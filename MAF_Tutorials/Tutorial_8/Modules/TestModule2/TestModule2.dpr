library TestModule2;

uses
  SysUtils,
  Classes,
  uModuleService in '..\..\..\..\uModuleService.pas',
  dmTestModule2 in 'dmTestModule2.pas' {dmTM2: TDataModule},
  frmTestForm1 in 'frmTestForm1.pas' {fTestForm1},
  frmPageControl2 in 'frmPageControl2.pas' {fPageControl2},
  frmFrame1 in 'frmFrame1.pas' {Frame1: TFrame};

{$R *.res}

begin
  __RegisterDMClass(TdmTM2);
end.
