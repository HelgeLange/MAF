library Router_FileDB;

uses
  SysUtils,
  Classes,
  dmRouter in 'dmRouter.pas' {DMR: TDataModule},
  uModuleService in '..\..\..\..\uModuleService.pas';

{$R *.res}

begin
  __RegisterDMClass(TDMR);
end.
