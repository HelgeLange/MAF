library TestModule;

uses
  SysUtils,
  Classes,
  uModuleService in '..\..\..\..\uModuleService.pas',
  dmTestModule in 'dmTestModule.pas' {DMTM: TDataModule};

{$R *.res}

begin
  __RegisterDMClass(TDMTM);
end.
