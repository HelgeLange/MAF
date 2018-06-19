library TestModule2;

uses
  SysUtils,
  Classes,
  uModuleService in '..\..\..\..\uModuleService.pas',
  dmTestModule2 in 'dmTestModule2.pas' {dmTM2: TDataModule};

{$R *.res}

begin
  __RegisterDMClass(TdmTM2);
end.
