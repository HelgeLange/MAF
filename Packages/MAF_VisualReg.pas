unit MAF_VisualReg;

interface

{$I ..\MAF_Base\MAFramework.inc}

uses {$IFDEF D15+}
     VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     Controls, Forms, Dialogs,
     {$ENDIF}
     Classes;

procedure Register;

implementation

uses uMAF_PageControl, uMAF_WizardPanel, uMAF_ConfigDlgManager, uMAF_MainMenuInterface;

const Pal_Examples : String = 'MAF Examples';

procedure Register;
begin
  RegisterComponents(Pal_Examples, [TmafPageControl]);
  RegisterComponents(Pal_Examples, [TmafWizardPanel, TmafFramePanel]);
  RegisterComponents(Pal_Examples, [TmafTreeViewManager, TmafTreeViewConfigManager, TmafListViewManager]);
  RegisterComponents(Pal_Examples, [TmafMainMenuInterface]);
end;

end.
