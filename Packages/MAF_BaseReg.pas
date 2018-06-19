unit MAF_BaseReg;

interface

uses Windows, Classes,
     {$IFDEF VER230}
     VCL.Controls, VCL.Forms, VCL.Dialogs;
     {$ELSE}
     Controls, Forms, Dialogs;
     {$ENDIF}

procedure Register;

implementation

uses uMAF_HookClient, uMAF_LinkClient, uMAF_ResourceClient, uMAF_ManagerLoader,
     {uRouterDataSetConnector, }uMAF_WindowManager, uMAF_ResourceManager,
     uMAF_WindowController, uMAF_Core,
     uMAF_CustomBaseDB, uMAF_HookManager, uMAF_GlobalVars,
     uMAF_DataStorage, uMAF_FileDB, uMAF_CustomFileResource,
     uMAF_LinkManager, uMAF_UserSecurity, uMAF_VarController,
     uMAF_Logger, uMAF_ModuleController, uMAF_Parameters,

     uMAF_PropertyEditors;

//{$R uLinkClient.dcr}
//{$R ERP_Base_MBC.res}
//{$R ERP_BaseDesign.dcr}



const Pal_Manager  : String = 'MAF Manager';
      Pal_Clients  : String = 'MAF Clients';
      Pal_Database : String = 'MAF Database';
      Pal_Examples : String = 'MAF Examples';

procedure Register;
begin
  RegisterComponents(Pal_Manager, [TmafManagerLoader]);
  RegisterComponents(Pal_Manager, [TmafFileLogger]);
  RegisterComponents(Pal_Manager, [TmafWindowManager]);
  RegisterComponents(Pal_Manager, [TmafHookManager]);
//  RegisterComponents(Pal_Manager, [THookSecurityLayer]);
  RegisterComponents(Pal_Manager, [TmafUserSecurity]);
  RegisterComponents(Pal_Manager, [TmafGlobalVars]);
  RegisterComponents(Pal_Manager, [TmafDataStorage]);
  RegisterComponents(Pal_Manager, [TmafResourceManager]);
  RegisterComponents(Pal_Manager, [TmafLinkManager]);

  RegisterComponents(Pal_Clients, [TmafModuleController]);
  RegisterComponents(Pal_Clients, [TmafHookClient]);
  RegisterComponents(Pal_Clients, [TmafLinkClient]);
  RegisterComponents(Pal_Clients, [TmafWindowController]);
//  RegisterComponents(Pal_Clients, [TmafInstallAPI]);
  RegisterComponents(Pal_Clients, [TmafResourceClient]);
  //RegisterComponents(Pal_Clients, [TmafINIVarController]);


//  RegisterComponents(Pal_Database, [TRouterDirectSQL, TRouterResourceIDSQL]);
  RegisterComponents(Pal_Database, [TmafFileDB]);
  RegisterComponents(Pal_Database, [TmafFileResource]);

  RegisterComponents(Pal_Database, [TmafCustomBaseDB2]);

  RegisterClass(TParameterCollection);
  RegisterClass(TParameterItem);
  RegisterComponents(Pal_Clients, [TmafParameters]);
end;

end.
