{*******************************************************************************
Name         : uMAF_DynamicFunctionTable.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2002-2011 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com                                                                               
Date         : 27.06.2009
Last Update  : 22.11.2010
Version      : 1.0.017
Purpose      : administrates modules and the dynamic code table. Code comes in
               part from uMAF_HookManager and was moved here
Last Changes :

1.0.017 (22.11.2010) -----------------------------------------------------------
- [CHG] when initializing of a module fails the message box now includes the name
        of the module. Also the error box is added now when not all modules
        will be loaded (which happens in runtime  when the current user has no
        access at all to any function of a module)
1.0.016 (08.07.2010) -----------------------------------------------------------
- [FIX] in ExecuteHook the QueryHandlerStruct wasn't prepared correctly when
        a uID was executed. That is fixed
1.0.015 (28.04.2010) -----------------------------------------------------------
- [ADD] added support to execute UniqueIDs instead of the combination HookID/SubHookID
        which makes more sense anyway, as SubHookIDs can change when a new module
        overwrites an existing SubHook
1.0.014 (30.11.2009) -----------------------------------------------------------
- [FIX] __Set_CodeGroup_Rights didn't free own items correctly and under some
        circumstances it added the access data from the caller instead of own.
        That is fixed now.
1.0.013 (27.11.2009) -----------------------------------------------------------
- [ADD] added deletion of Codegroup Access Rights for user and groups
1.0.012 (22.11.2009) -----------------------------------------------------------
- [FIX] when registering a new module ad the BaseDB was connected a parameter
        was passed wrong, causing the new module to overwrite a property in the
        BaseDB, that is fixed now
- [ADD] the TmafModuleController for a registered SubHook, that is being deleted
        will now be informed in any case after notifying all function observers
        That makes it possible to close open windows, that are not closed by a
        function observer (like top-level windows in an MDI environment)
1.0.011 (14.11.2009) -----------------------------------------------------------
- [ADD] added calling event TmafHookManager.BeforeLoadingManager
- [FIX] fixed an issue in RegisterSubHook, when the Subhook was overwriting an
        existing one, the deletion was not reported to the function observer
- [ADD] When setting the property ModulePath the trailing path delimiter is added
        in any case
1.0.010 (07.11.2009) -----------------------------------------------------------
- [ADD] when uninstalling a module the TmafModuleController now gets a final
        message to clean up open windows
- [ADD] added a check, if an uID already exists when installing a SubHook and
        the InsertDirection isn't hidOverwrite. This avoids that a function is
        there twice
1.0.009 (04.11.2009) -----------------------------------------------------------
- [FIX] fixed a behaviour where for access rights weren't saved only a white list
        but access rights for all items. That wasn't intended and now all items
        that indicate no-access are removed from the list before saving it
1.0.008 (27.10.2009) -----------------------------------------------------------
- [ADD] Function observer to observe single SubHooks or whole DynamicFunctions
- [ADD] Notify to function observer when installing or de-installing
1.0.007 (23.09.2009) -----------------------------------------------------------
- [FIX] fixed the __RegisterSubHook for cases hidBefore and hidAfter, where the
        new SubHook wasn't added when the Relative_uID wasn't found
- [FIX] fixed in __RegisterSubHook the hidOverwrite where the new SubHook wasn't
        added, when the old uID wasn't found
- [ADD] __RegisterSubHook and __UnRegisterSubHook now have a preset parameter
        to tell the methods if they save the DFT or not
- [ADD] __RegisterModule now can add the functions registered for install in the
        ModuleController of the newly installed module
- [ADD] the PmafInstallToken got a new action to bring the DFT into lockdown
1.0.006 (13.09.2009) -----------------------------------------------------------
- [ADD] CodeGroups are now sorted and use a fast find function
1.0.005 (01.09.2009) -----------------------------------------------------------
- [FIX] fixed a bug where a newly added module wasn't added to the module list
- [CHG] changed in the template reader/writer methods for Modules, Hooks and
        InstallHistory the Create and Free methods for the Template token
- [ADD] added the code for the lockdown mode, where modules and the DFT are re-
        initialized with all modules and code loaded before changes on the DFT
        are allowed. If the component isn't in lockdown mode, no changes can be
        made and while in lockdown mode no dynamic code can be executed (due to
        security reasons)
- [ADD] added the private method __FreeUnusedModules wich unloads all modules
        that don't export functions for the current DFT
1.0.004 (24.07.2009) -----------------------------------------------------------
- [FIX] fixed the pFreeMemProc in ExecuteHook, wich didn't delete the pointers
        for the proc itself nor the pChildObj (where the record was saved)
1.0.003 (18.07.2009) -----------------------------------------------------------
- [FIX] fixed a bug where the new FreeMemory function from the QueryHandlerStruct
        was only called, when an AfterExecHook event was assigned in the calling
        HookClient
- [CHG] use now of the new QueryHandlerStruct member CallerComponent, wich allows
        us access to Before/AfterExecHook events
1.0.002 (06.07.2009) -----------------------------------------------------------
- [CHG] moved the record definitions and its memory allocation/deallocation
        routines to uMAF_HookManager_Helper.pas to keep this file a bit cleaner
- [ADD] install history routines
1.0.001 (01.07.2009) -----------------------------------------------------------
- [ADD] RSubHook record got a new member "uID", wich represents an unique ID to
        identify a SubHook. That makes it easier to target a certain SubHook to
        be replaced, as the previous mode, where a secondary key made from HookID,
        SubHookID and ModuleID isn't be found once replaced by another module
- [ADD] implemented the reader/write methods to save and load Module-Info and
        Hook-Info from Template streams instead from own database tables
- [ADD] CodeGroup support code from TmafHookManager
1.0.000 (27.06.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_DynamicFunctionTable;

interface

uses Windows, SysUtils, Classes,
     // Modular Application Framework Components units
     uMAF_Tracer, uMAF_Globals, uMAF_Tools, uMAF_ModuleManager, uMAF_TemplateStreamer,
     uMAF_HookManager_Helper, uMAF_Core;

Type TModuleLoadMethod = (mlmDatabase, mlmDirectory);

     TLoaderProgress = procedure(Sender: TObject; MaxVal, CurrentVal: Integer) Of Object;

     TTableState = (tsLoading, tsCleaning, tsLockdown, tsUnloading);
     TTableStates = Set of TTableState;

     TmafDynamicFuntionTable = class(TPersistent)
     private
       FpModules : TList;    // List of TmafModuleManager
       FpDFT : TList;        // List of PDynamicFunction
       FpCodeGroups : TList; // list of all code groups
       FpTheoreticalCodeGroups : TList; // list of codegroups the user theoretically would have access to
       FpInstallHistory : TList; // Install history for SubHooks, a list of PInstallHistory records
       FpFunctionObserver : TList; // List of registered function observer
       FpCG_Rights : TList;  // list with the current access rights
       FpAddedHooks : TList;   // list of SubHooks added during a module installation
       FpDeletedHooks : TList; // list of SubHooks deleted/overwritten during a module instalation
       FpTemplateStreamer : TTemplateStreamer; // used to read/write the module list and the DFT from/to a template
       FbLockdown : Boolean; // lockdown is true, when we install/uninstall Hooks/Modules, no Hooks can be executed during this time
       FnHighCodeGroupID : Integer;
       FModuleLoadMethod : TModuleLoadMethod; // how shall we load the modules
       FsModulePath : String;
       FhApplicationHandle : HWND;
       FbUseInstallHistory : Boolean;
       FOnModuleLoading : TLoaderProgress;
       FOnDFTLoading : TLoaderProgress;
       FTableState : TTableStates;
       // module loader functions
       procedure __LoadModules;
       procedure __FreeModules;
       procedure __FreeUnusedModules;
       procedure __Initialize_UsedModules(bAllModules: Boolean = False);
       procedure __TemplateReadWrite_Modules(AAction: TDataStorageAction);
       function __RegisterModule(LibName: String; bInstallSubHooks: Boolean = True): Integer;
       function __UnregisterModule(nID: Integer): Integer;
       procedure __Reset_ExportedFunctionCount;

       // dynamic funtion table functions
       procedure __LoadDynamicFunctions;
       procedure __FreeDynamicFunctions;
       procedure __FreeFunctionObserver;
       procedure __TemplateReadWrite_DynamicFunctions(AAction: TDataStorageAction);
       function __GetSubHook(nSubHookID: Integer; pToken: PDynamicFunction = nil): PSubHook;
       function __ExecuteDynamicFunction(pToken: PDynamicFunction; QHS: pQHS; pUserParam: Pointer): Integer;
       function __ExecuteSubHook(pToken: PSubHook; QHS: pQHS; pUserParam: Pointer): Integer;
       function __ActivateSubHook(uID: Integer; bActive: Boolean): Integer;
       function __RegisterSubHook(pToken: PMAFInstallToken; bSave: Boolean = True): Integer;
       function __UnregisterSubHook(pToken: PMAFInstallToken; bSave: Boolean = True): Integer;
       function __Find_FunctionObserver(nHookID, nSubHookID: Integer; Sender: TComponent): PFunctionObserver;
       function __Create_SubHookChange_Item(nHookID, nSubHookID: Integer; Action: Byte): PSubHookChange;
       procedure __Notify_FunctionObserver(AType: Byte = 3);

       // stream loader events
       procedure __WriteStreamAttribute(Sender: TObject; ID: Integer);
       procedure __ReadStreamAttribute(Sender: TObject; ID: Integer);

       // CodeGroup function
       procedure __FreeCodeGroups;
       procedure __FreeCodeGroupRights;
       procedure __CodeGroup_AddFunction(pToken: PSubHook);
       procedure __CodeGroup_DeleteFunction(pToken: PSubHook);
       function __ChangeCodeGroup(pToken: PMAFInstallToken): Integer;
       function __CodeGroup_Find(nID: Integer): PCodeGroup;
       procedure __TemplateReadWrite_CodeGroupRights(AAction: TDataStorageAction);
       function __Get_CodeGroup_Right(nCodegroupID: Integer): PAccessData;

       // Install history
       procedure Initialize_InstallHistory;
       procedure Free_InstallHistory;
       procedure __TemplateReadWrite_InstallHistory(AAction: TDataStorageAction);
       procedure HistoryToken_Add(var pToken: PSubHook; InstallerModuleID : Integer);
       function HistoryToken_Get(pToken: PSubHook; bRemove: Boolean = True): PSubHook;
       function __GetHistoryCount: Integer;

       // helper functions
       function __GetSubHookCount: Integer;
       procedure __PrepareQueryHandlerStruct(QHS: pQHS);
       procedure __SetLockdown(const Value: Boolean);
       procedure __SetModulePath(const Value: String);
     protected
     public
       FpHookManager : Pointer;      // Pointer to the HookManager
       constructor Create;
       destructor Destroy; override;

       procedure Initialize;
       procedure Close;
       function ExecuteHook(nHookID: Integer; QHS: pQHS; pUserParam: Pointer): Integer;
       function Query_InstallAPI(pToken: PMAFInstallToken): Integer;

       // module functions
       procedure EnumRegisteredModules(QHS: pQHS);
       function GetModuleManager(nID: Integer): TmafModuleManager;

       // DyncamicFunctionTable functions
       function GetSubHookToken(nHookID, SubHookID, ModuleID: Integer): PSubHook;
       function GetDynamicFunction(nHookID : Integer): PDynamicFunction;
       function __GetSubHook_uID(uID: Integer): PSubHook;
       function __Add_FunctionObserver(nHookID, nSubHookID: Integer; Sender: TComponent): Integer;
       function __Delete_FunctionObserver(nHookID, nSubHookID: Integer; Sender: TComponent): Integer;

       // CodeGroup functions
       procedure DeleteCodeGroup(nCodeGroupID: Integer);
       procedure CodeGroups_CheckDescriptions(AList: TList);
       function __Set_CodeGroup_Rights(ATarget: Integer; pData: PGroupData): Integer;
       function __Get_CodeGroup_Rights(ATarget: Integer; pData: PGroupData): Integer;
       function __Del_CodeGroup_Rights(ATarget: Integer; pData: PGroupData): Integer;

       property ApplicationHandle: HWND read FhApplicationHandle write FhApplicationHandle;
       property HighCodeGroupID : Integer read FnHighCodeGroupID write FnHighCodeGroupID;
       property Lockdown : Boolean read FbLockdown write __SetLockdown;
       property Modules : TList read FpModules;
       property DFT : TList read FpDFT write FpDFT;
       property CodeGroups : TList read FpCodeGroups;
     published
       property ModuleLoadMethod: TModuleLoadMethod read FModuleLoadMethod write FModuleLoadMethod default mlmDatabase;
       property ModulePath : String read FsModulePath write __SetModulePath;
       property UseInstallHistory : Boolean read FbUseInstallHistory write FbUseInstallHistory default True;
       // events
       property OnModuleLoading : TLoaderProgress read FOnModuleLoading write FOnModuleLoading;
       property OnDFTLoading : TLoaderProgress read FOnDFTLoading write FOnDFTLoading;
     end;

implementation

uses uMAF_HookManager, uMAF_HookClient, Dialogs;

const HOOKMANAGER_DATA_CATEGORY     = 'MAF_SystemData';
      HOOKMANAGER_DATA_MODULES      = 'ModuleData';
      HOOKMANAGER_DATA_HOOKS        = 'DFTData';
      HOOKMANAGER_DFT_HISTORY       = 'DFTHistory';
      HOOKMANAGER_CG_RIGHTS_GROUP   = 'CG_GROUP_';
      HOOKMANAGER_CG_RIGHTS_USER    = 'CG_USER_';
      MODULE_STREAM_ID              = 122;
      DFT_STREAM_ID                 = 123;
      DFT_HISTORY_ID                = 124;
      DFT_CG_RIGHTS                 = 125;

      sUserNotAuthorized_Error      = 'User tried to switch to lockdown and was not authorized to do that';

{ TmafDynamicFuntionTable }

constructor TmafDynamicFuntionTable.Create;
begin
  FpAddedHooks := nil;  // will be initialized when going into lockdown
  FpDeletedHooks := nil; // see above
  FbUseInstallHistory := True;
  FbLockdown := False;
  FnHighCodeGroupID := 0;
  FpModules := TList.Create;
  FpDFT := TList.Create;
  FpCodeGroups := TList.Create;
  FpTheoreticalCodeGroups := TList.Create;
  FpCG_Rights := TList.Create;
  FpFunctionObserver := TList.Create;
  FpTemplateStreamer := TTemplateStreamer.Create;
  FpTemplateStreamer.OnStreamReadAttribute := __ReadStreamAttribute;
  FpTemplateStreamer.OnStreamWriteAttribute := __WriteStreamAttribute;
end;

destructor TmafDynamicFuntionTable.Destroy;
begin
  Close;
  FreeAndNil(FpCodeGroups);
  FreeAndNil(FpTheoreticalCodeGroups);
  FreeAndNil(FpDFT);
  FreeAndNil(FpModules);
  FreeAndNil(FpTemplateStreamer);
  FreeAndNil(FpFunctionObserver);
  FreeAndNil(FpCG_Rights);
  inherited;
end;

procedure TmafDynamicFuntionTable.Initialize;
begin
  __LoadModules;
  __LoadDynamicFunctions;  // CodeGroups are created automatically when the function are loaded from the DFT
end;

procedure TmafDynamicFuntionTable.Close;
begin
  __FreeCodeGroups;        // free the CodeGroups
  __FreeDynamicFunctions;  // free the DFT
  __FreeModules;           // free the loaded modules
  __FreeFunctionObserver;  // free the function observer
end;

procedure TmafDynamicFuntionTable.__SetLockdown(const Value: Boolean);
begin
  FbLockdown := Value;
  If FbLockdown Then begin
    //in lockdown only load the not loaded modules again
    Include(FTableState, tsLockdown);
    If TmafHookManager(FpHookManager).Connected Then begin
      __Initialize_UsedModules(True);
      __LoadDynamicFunctions; // in Lockdown we load DFT fresh and full
    end;
    If FbUseInstallHistory Then
      Initialize_InstallHistory;
    FpAddedHooks := TList.Create;
    FpDeletedHooks := TList.Create;
  end else begin
    __FreeUnusedModules;
    __LoadDynamicFunctions; // without lockdown we load only the functions the user has access to
    Exclude(FTableState, tsLockdown);
    __Notify_FunctionObserver;
    If FbUseInstallHistory Then
      Free_InstallHistory;
    TmafHookManager(FpHookManager).__TriggerDFTChange;
    FpAddedHooks.Free;
    FpDeletedHooks.Free;
  end;  //  --  If FbLockdown Then
end;

procedure TmafDynamicFuntionTable.__SetModulePath(const Value: String);
begin
  FsModulePath := IncludeTrailingPathDelimiter(Value);
end;

function TmafDynamicFuntionTable.Query_InstallAPI(pToken: PMAFInstallToken): Integer;
begin
  If pToken = nil Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;

  If pToken^.nAction = iaLockDown Then begin
    Result := ERR_USER_NOT_AUTHORIZED;

    // TODO : check, if the user can go into lockdown
    Lockdown := (pToken^.bActive > 0);
    If ((LockDown) And ((pToken^.bActive > 0))) Then
      Result := ERR_NO_ERROR;

    If ((Not LockDown) And ((pToken^.bActive = 0))) Then
      Result := ERR_NO_ERROR;

    If ((Result = ERR_USER_NOT_AUTHORIZED) And (TmafHookManager(FpHookManager).Logger <> nil)) Then begin
      TmafHookManager(FpHookManager).Logger.WriteLog(ltError, sUserNotAuthorized_Error);
      Exit;
    end;
  end;

  If Not Lockdown Then begin   // only in lockdown mode we can register new modules
    Result := ERR_HM_NOT_IN_LOCKDOWN;
    Exit;
  end;

  Result := ERR_UNKNOWN_ERROR;
  Case pToken^.nAction Of
    iaInsert           : Result := __RegisterSubHook(pToken);
    iaDelete           : Result := __UnregisterSubHook(pToken);
    iaActivate         : Result := __ActivateSubHook(pToken^.uID, (pToken^.bActive > 0));
    iaRegisterModule   : Result := __RegisterModule(String(pToken^.sDescription), (pToken^.bActive > 0));
    iaUnregisterModule : Result := __UnregisterModule(pToken^.nModuleID);
    iaSetCodeGroup     : Result := __ChangeCodeGroup(pToken);
    iaSaveChanges      : __TemplateReadWrite_DynamicFunctions(taWrite);
  end; 
end;

// ********************************* Comments **********************************
// Description : preparing basic settings for the QueryHandlerStruct before
//               calling a Hook
// Param (in)  : Pointer to a QueryHandlerStruct
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 25.02.2002
// Last Update : 21.03.2007
// *****************************************************************************
procedure TmafDynamicFuntionTable.__PrepareQueryHandlerStruct(QHS: pQHS);
begin
  QHS^.bStop := False;
  QHS^.bSkipNext := False;
  QHS^.nNextSubHookID := -1;

  // default database, if nothing else is set
  If ((QHS^.pDB = nil) And (TmafHookManager(FpHookManager).BaseDB <> nil)) Then
    QHS^.pDB := TmafHookManager(FpHookManager).BaseDB.DataBase;
  // default transaction, Reason : take a look above ;-)
  If ((QHS^.pTransaction = nil) And (TmafHookManager(FpHookManager).BaseDB <> nil)) Then
    QHS^.pTransaction := TmafHookManager(FpHookManager).BaseDB.Transaction;
  // default updatetransaction
  If ((QHS^.pUpdateTransaction = nil) And (TmafHookManager(FpHookManager).BaseDB <> nil)) Then
    QHS^.pUpdateTransaction := TmafHookManager(FpHookManager).BaseDB.UpdateTransaction;

  // if no parent handle is set, the parent will be the application
  If QHS^.FhParentHandle = 0 Then
    QHS^.FhParentHandle := ApplicationHandle;
end; // __PrepareQueryHandlerStruct

// ********************************* Comments **********************************
// Description : executes a Dynamic function
// Param (in)  : nHookID = HookID
//               QHS = Pointer to a valid TQueryHandlerStruct
//               pUserParam = user defined pointer, we don't touch it
// Param (out) : ErrCode
// Coding by   : Helge Lange
// Date        : 25.02.2002
// Last Update : 08.07.2010
// *****************************************************************************
function TmafDynamicFuntionTable.ExecuteHook(nHookID: Integer; QHS: pQHS; pUserParam: Pointer): Integer;
var pDF : PDynamicFunction;
    pSH : PSubHook;
    bCanContinue: Boolean;
    pSHC : PSubHookChange;
    i : Integer;
begin
  Result := ERR_NO_ERROR;
  bCanContinue := Not (tsLockDown in FTableState);
  If tsCleaning in FTableState Then begin  // we know, that tsCleaning is only there, if a SubHook is being deleted
    If Assigned(FpDeletedHooks) Then begin // check, if the List exists.. if not, we're not really deleting and somebody screws aroud
      If FpDeletedHooks.Count = 0 Then       // if there are no
        Exit;
    end else
      Exit;                                // so if somebody screws around, we quit, too

    For i := 0 To FpDeletedHooks.Count - 1 Do begin
      pSHC := PSubHookChange(FpDeletedHooks.Items[i]);
      If ((pSHC^.nHookID = nHookID) And (pSHC^.nSubHookID = QHS^.SubHookID)) Then begin
        bCanContinue := True;
        Break;
      end;
    end;
  end;

  If Not bCanContinue Then
    Exit;

  __PrepareQueryHandlerStruct(QHS); // prepare the QueryHandlerStruct
  QHS^.bStop := False;
  QHS^.bSkipNext := False;

  If QHS^.uID > 0 Then begin
    pSH := __GetSubHook_uID(QHS^.uID);
    If Assigned(pSH) Then
      Result := __ExecuteSubHook(pSH, QHS, pUserParam);
    Exit;
  end;  //  --  If QHS^.uID > 0 Then

  pDF := GetDynamicFunction(nHookID);
  If pDF = nil Then                 // it's not an error, if a function doesn't exists
    Exit;

  If QHS^.SubHookID > 0 Then begin
    pSH := __GetSubHook(QHS^.SubHookID, pDF);
    If Assigned(pSH) Then
      Result := __ExecuteSubHook(pSH, QHS, pUserParam)
  end else
    Result := __ExecuteDynamicFunction(pDF, QHS, pUserParam);
  {$IFDEF Tracer}
  MAFTracer.Leave;  // leave for either  __ExecuteSubHook or __ExecuteDynamicFunction
  {$ENDIF}
end;

// ********************************* Comments **********************************
// Description : executes a complete Dynamic function with all SubHooks
// Param (in)  : pToken = PDynamicFunction record from DFT
//               QHS = Pointer to a valid TQueryHandlerStruct
//               pUserParam = user defined pointer, we don't touch it
// Param (out) : ErrCode
// Coding by   : Helge Lange
// Date        : 25.02.2002
// Last Update : 03.07.2009
// *****************************************************************************
function TmafDynamicFuntionTable.__ExecuteDynamicFunction(pToken: PDynamicFunction; QHS: pQHS; pUserParam: Pointer): Integer;
var i : Integer;
    pData : PSubHook;
    CanAccess : Boolean;
begin
  {$IFDEF Tracer}
  MAFTracer.Enter('TmafDynamicFuntionTable.__ExecuteDynamicFunction');
  MAFTracer.Log_Integer('HookID', pToken^.HookID);
  {$ENDIF}
  Result := ERR_NO_ERROR;
  For i := 0 To pToken^.FpSubHooks.Count - 1 Do begin
    pData := PSubHook(pToken^.FpSubHooks.Items[i]);
    CanAccess := pData^.bActive;
    QHS^.SubHookID := pData^.SubHookID;
    QHS^.uID := pData^.uID;
{    If ((TmafHookManager(FpHookManager).SecurityLayer <> nil) And (CanAccess)) Then
      CanAccess := TmafHookManager(FpHookManager).SecurityLayer.CheckAccess(pToken^.HookID, QHS^.SubHookID); }
    If ((Not QHS^.bSkipNext) And (CanAccess)) Then begin
      // fire event BeforeExecHook
      If ((QHS^.EventFlags And 4) = 4) Then
        If Assigned(TmafHookClient(QHS^.CallerComponent).BeforeExecHook) Then
          TmafHookClient(QHS^.CallerComponent).BeforeExecHook(pToken^.HookID, QHS, pUserParam, Result);
      // we call the SubHook
      {$IFDEF Tracer}
      MAFTracer.Log_Integer('SubHookID', QHS^.SubHookID);
      {$ENDIF}
      Result := pData^.pModuleManager.CallDLL(QHS^.SubHookID, QHS, pUserParam);
    end Else  //  --  If not QHS^.bSkipNext Then
      QHS^.bSkipNext := False;
    If CanAccess Then begin
      If i < (pToken^.FpSubHooks.Count - 1) Then
        QHS^.nNextSubHookID := PSubHook(pToken^.FpSubHooks.Items[i+1])^.SubHookID
      Else
        QHS^.nNextSubHookID := -1;
      // fire event AfterExecHook
      If ((QHS^.EventFlags And 8) = 8) Then
        If Assigned(TmafHookClient(QHS^.CallerComponent).AfterExecHook) Then
          TmafHookClient(QHS^.CallerComponent).AfterExecHook(pToken^.HookID, QHS, pUserParam, Result);
        If Assigned(QHS^.pFreeMemFunc) Then begin
          QHS^.pFreeMemFunc(QHS^.pChildObj);
          QHS^.pFreeMemFunc := nil; // we have to delete that, in case the next SubHook doesn't set one
          QHS^.pChildObj := nil;    // and delete the record pointer, as it was disposed anyway
        end;  //  --  If Assigned(QHS^.pFreeMemFunc) Then
      If QHS^.bStop Then
        Exit;
    end;  //  --  If CanAccess Then
  end;  //  --  For i := 0 To pToken^.FpSubHooks.Count - 1 Do
  {$IFDEF Tracer}
  MAFTracer.Leave;
  {$ENDIF}
end;

// ********************************* Comments **********************************
// Description : executes a single SubHook from a Dynamic Function
// Param (in)  : pToken = PSubHook record from SubHook list of a Dynamic Function
//               QHS = Pointer to a valid TQueryHandlerStruct
//               pUserParam = user defined pointer, we don't touch it
// Param (out) : ErrCode
// Coding by   : Helge Lange
// Date        : 25.02.2002
// Last Update : 03.07.2009
// *****************************************************************************
function TmafDynamicFuntionTable.__ExecuteSubHook(pToken: PSubHook; QHS: pQHS; pUserParam: Pointer): Integer;
var CanAccess : Boolean;
begin
  {$IFDEF Tracer}
  MAFTracer.Enter('TmafDynamicFuntionTable.__ExecuteSubHook');
  MAFTracer.Log_Integer('HookID', pToken^.HookID);
  MAFTracer.Log_Integer('SubHookID', pToken^.SubHookID);
  {$ENDIF}
  Result := ERR_NO_ERROR;
  QHS^.HookID := pToken^.HookID;
  QHS^.SubHookID := pToken^.SubHookID;
  QHS^.uID := pToken^.uID;
  CanAccess := pToken^.bActive;
{  If ((TmafHookManager(FpHookManager).SecurityLayer <> nil) And (CanAccess)) Then
    CanAccess := TmafHookManager(FpHookManager).SecurityLayer.CheckAccess(pToken^.HookID, pToken^.SubHookID); }
  If CanAccess Then begin
    // fire event BeforeExecHook
    If ((QHS^.EventFlags And 4) = 4) Then
      If Assigned(TmafHookClient(QHS^.CallerComponent).BeforeExecHook) Then
        TmafHookClient(QHS^.CallerComponent).BeforeExecHook(pToken^.HookID, QHS, pUserParam, Result);
    // we call the SubHook
    Result := pToken^.pModuleManager.CallDLL(QHS^.SubHookID, QHS, pUserParam);
    // fire event AfterExecHook
    If ((QHS^.EventFlags And 8) = 8) Then
      If Assigned(TmafHookClient(QHS^.CallerComponent).AfterExecHook) Then
        TmafHookClient(QHS^.CallerComponent).AfterExecHook(pToken^.HookID, QHS, pUserParam, Result);
    If Assigned(QHS^.pFreeMemFunc) Then begin
      QHS^.pFreeMemFunc(QHS^.pChildObj);
      QHS^.pFreeMemFunc := nil; // we have to delete that, in case the next SubHook doesn't set one
      QHS^.pChildObj := nil;    // and delete the record pointer, as it was disposed anyway
    end;  //  --  If Assigned(QHS^.pFreeMemFunc) Then
  end;  //  --  If CanAccess Then
  {$IFDEF Tracer}
  MAFTracer.Leave;
  {$ENDIF}
end;




{ *********************** Module  Functions ********************************** }



function TmafDynamicFuntionTable.GetModuleManager(nID: Integer): TmafModuleManager;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpModules.Count - 1 Do
    If TmafModuleManager(FpModules.Items[i]).ModuleID = nID Then begin
      Result := TmafModuleManager(FpModules.Items[i]);
      Break;
    end;  //  --  If TmafModuleManager(FpModules.Items[i]).ModuleID = nID Then
end;

function TmafDynamicFuntionTable.__RegisterModule(LibName: String; bInstallSubHooks: Boolean = True): Integer;
var pModule : TmafModuleManager;
    i : Integer;
    pData : PmafInstallToken;
    bCanSave : Boolean;
    S : String;
    QHS : pQHS;
begin
  Result := ERR_UNKNOWN_ERROR;
  For i := 0 To FpModules.Count - 1 Do
    If LibName = TmafModuleManager(FpModules.Items[i]).ModuleController.ModuleInfo.ModuleName Then
      Exit;

  S := ExtractFilePath(LibName);
  If ((S <> '') And (IncludeTrailingPathDelimiter(S) <> FsModulePath)) Then
    CopyFile(PChar(LibName), PChar(FsModulePath + ExtractFileName(LibName)), True);

  // so that module isn't already registered...
  pModule := TmafModuleManager.Create(FsModulePath + ExtractFileName(LibName));
  If Assigned(TmafHookManager(FpHookManager).BaseDB) Then begin
    QHS := __Create_QueryHandlerStruct;
    QHS^.pDB := TmafHookManager(FpHookManager).BaseDB.Database;
    QHS^.pTransaction := TmafHookManager(FpHookManager).BaseDB.Transaction;
    pModule.Initialize(QHS, nil);
    __Free_QueryHandlerStruct(QHS);
  end Else
    pModule.Initialize(nil, nil);
  FpModules.Add(pModule);
  __TemplateReadWrite_Modules(taWrite);  // and saving the new module list

  bCanSave := True;
  If ((bInstallSubHooks) And (pModule.ModuleController.InstallData.Count > 0)) Then begin
    For i := 0 To pModule.ModuleController.InstallData.Count - 1 Do begin
       pData := PmafInstallToken(pModule.ModuleController.InstallData.Items[i]);
       If pData^.bActive > 0 Then
         If __RegisterSubHook(pData, False) <> ERR_NO_ERROR Then // we don't save inside for now
           bCanSave := False;
    end;
    If bCanSave Then
      __TemplateReadWrite_DynamicFunctions(taWrite);
  end;
end;

function TmafDynamicFuntionTable.__UnregisterModule(nID: Integer): Integer;
var pModule : TmafModuleManager;
    i, nPos : Integer;
//    pDF : PDynamicFunction;
    bCanSave : Boolean;
    pData : PmafInstallToken;
begin
  Result := ERR_UNKNOWN_ERROR;
  pModule := GetModuleManager(nID);
  If Assigned(pModule) Then begin
    nPos := FpModules.IndexOf(pModule);

    If pModule.ExportedFunctions > 0 Then begin
      bCanSave := True;
      If pModule.ModuleController.InstallData.Count > 0 Then begin
        For i := 0 To pModule.ModuleController.InstallData.Count - 1 Do begin
           pData := PmafInstallToken(pModule.ModuleController.InstallData.Items[i]);
           pData^.nAction := iaDelete;
           If pData^.bActive > 0 Then
             If __UnregisterSubHook(pData, False) <> ERR_NO_ERROR Then // we don't save inside for now
               bCanSave := False;
        end;
        If bCanSave Then
          __TemplateReadWrite_DynamicFunctions(taWrite);
      end;
      SendComponentMessage(pModule.ModuleController, WM_MODULE_CLOSE, nil, nil);
    end;
    // first we have to see, if there are Hooks open
    pModule.Free_DLL;
    FreeAndNil(pModule);
    { TODO -oHelge : Add notify code for Hooks that may want to know about the removal of that module }
    If nPos > -1 Then
      FpModules.Delete(nPos);
    { TODO -oHelge : Remove all Hooks from this module ? }
    __TemplateReadWrite_Modules(taWrite);  // and saving the new module list
  end;  //  --  If Assigned(pModule) Then
end;

procedure TmafDynamicFuntionTable.EnumRegisteredModules(QHS: pQHS);
var i : Integer;
    ClientHC : TmafHookClient;
    p : Pointer;
begin
  ClientHC := TmafHookClient(QHS^.pChildObj);
  For i := 0 To FpModules.Count - 1 Do begin
      If Assigned(ClientHC.AfterExecHook) Then begin
        P := Pointer(TmafModuleManager(FpModules.Items[i]).ModuleController);
        ClientHC.AfterExecHook(QHS^.HookID, QHS, P, ERR_NO_ERROR);
      end;
  end;  //  --  For i := 0 To FpModules.Count - 1 Do
end;

procedure TmafDynamicFuntionTable.__FreeModules;
var i : Integer;
begin
  For i := FpModules.Count - 1 DownTo 0 Do
    TmafModuleManager(FpModules.Items[i]).Free;
  FpModules.Clear;
end;

procedure TmafDynamicFuntionTable.__FreeUnusedModules;
var i : Integer;
begin
  If FbLockdown Then
    Exit;

  For i := FpModules.Count - 1 DownTo 0 Do begin
    If TmafModuleManager(FpModules.Items[i]).ExportedFunctions = 0 Then begin
      TmafModuleManager(FpModules.Items[i]).Free_DLL;
//      FpModules.Delete(i);
    end;
  end;
end;

procedure TmafDynamicFuntionTable.__Initialize_UsedModules(bAllModules: Boolean = False);
var i : Integer;
    QHS : pQHS;
    ErrCode : Integer;
begin
  QHS := __Create_QueryHandlerStruct;
  // owner of the HookManager is the datamadule which has the mainform as owner
  QHS^.pParent := TmafHookManager(FpHookManager).Owner.Owner;
  If bAllModules Then begin
    For i := 0 To FpModules.Count - 1 Do begin
      ErrCode := TmafModuleManager(FpModules.Items[i]).Initialize(QHS, nil);
      If ErrCode <> ERR_NO_ERROR Then
        MessageDlg('Module (' + TmafModuleManager(FpModules.Items[i]).ModuleName + ') initialization error code : ' + IntToStr(ErrCode), mtError, [mbOk], 0);
    end;
  end else
    For i := 0 To FpModules.Count - 1 Do
      If TmafModuleManager(FpModules.Items[i]).ExportedFunctions > 0 Then begin
        Try
          ErrCode := TmafModuleManager(FpModules.Items[i]).Initialize(QHS, nil);
        Except
          Exception.Create('Error initializing module : ' + TmafModuleManager(FpModules.Items[i]).ModuleName);
        End;
        If ErrCode <> ERR_NO_ERROR Then
          MessageDlg('Module (' + TmafModuleManager(FpModules.Items[i]).ModuleName + ') initialization error code : ' + IntToStr(ErrCode), mtError, [mbOk], 0);
      end;
  __Free_QueryHandlerStruct(QHS);
end;

procedure TmafDynamicFuntionTable.__LoadModules;
var SearchRec : TSearchRec;
    pModule : TmafModuleManager;
begin
  __FreeModules;  // always free first the list

  // do we load from a template saved in a database ?
  If FModuleLoadMethod = mlmDatabase Then begin
    If Not Assigned(TmafHookManager(FpHookManager).BaseDB) Then begin
      Raise EComponentError.Create('ModuleLoadMethod is mlmDatbase, but no BaseDB is connected to the HookManager');
      Exit;
    end;
    // so we got a database connection, now we check the TemplateHandler
    If Not Assigned(TmafHookManager(FpHookManager).DataStorage) Then begin
      Raise EComponentError.Create('ModuleLoadMethod is mlmDatbase, but no TemplateHandler is connected to the HookManager');
      Exit;
    end;
    // we should be good to go
    __TemplateReadWrite_Modules(taRead);
  end;
  // or do we simply load all DLLs stored in the given directory ?
  If ModuleLoadMethod = mlmDirectory Then begin
    If FindFirst(FsModulePath + '*.dll', faAnyFile, SearchRec) = 0 Then begin
      pModule := TmafModuleManager.Create(FsModulePath + SearchRec.Name);
      FpModules.Add(pModule);
      While FindNext(SearchRec) = 0 Do begin
        pModule := TmafModuleManager.Create(FsModulePath + SearchRec.Name);
        FpModules.Add(pModule);
      end;  //  --  While FindNext(SearchRec) = 0 Do
      FindClose(SearchRec);
    end;  //  --  If FindFirst(FsModulePath + '*.dll', faAnyFile, SearchRec) = 0 Then
  end;
end;

procedure TmafDynamicFuntionTable.__TemplateReadWrite_Modules(AAction: TDataStorageAction);
var pToken  : PDataStorageQuery;
begin
  FpTemplateStreamer.Attributes := 1;
  FpTemplateStreamer.Stream.Size := 0;
  pToken := __Create_DataStorageQuery(HOOKMANAGER_DATA_CATEGORY, HOOKMANAGER_DATA_MODULES);
  pToken^.aObj := FpTemplateStreamer.Stream;  // we use the internal stream of the TempleateStreamer
  pToken^.Action := AAction;
  Case AAction Of
    taRead  : begin
                TmafHookManager(FpHookManager).DataStorage.Query(pToken);
                If FpTemplateStreamer.Stream.Size > 0 Then
                  FpTemplateStreamer.ReadStream;
              end; // taRead
    taWrite : begin
                FpTemplateStreamer.WriteStream(MODULE_STREAM_ID);
                TmafHookManager(FpHookManager).DataStorage.Query(pToken);
              end; // taWrite
  end;  //  --  Case AAction Of
  __Free_DataStorageQuery(pToken);
end;





{ ******************** Install History Functions ***************************** }





procedure TmafDynamicFuntionTable.__TemplateReadWrite_InstallHistory(AAction: TDataStorageAction);
var pToken  : PDataStorageQuery;
begin
  FpTemplateStreamer.Attributes := 1;
  FpTemplateStreamer.Stream.Size := 0;
  pToken := __Create_DataStorageQuery(HOOKMANAGER_DATA_CATEGORY, HOOKMANAGER_DFT_HISTORY);
  pToken^.aObj := FpTemplateStreamer.Stream;  // we use the internal stream of the TempleateStreamer
  pToken^.Action := AAction;
  Case AAction Of
    taRead  : begin
                TmafHookManager(FpHookManager).DataStorage.Query(pToken);
                If FpTemplateStreamer.Stream.Size > 0 Then
                  FpTemplateStreamer.ReadStream;
              end; // taRead
    taWrite : begin
                FpTemplateStreamer.WriteStream(DFT_HISTORY_ID);
                TmafHookManager(FpHookManager).DataStorage.Query(pToken);
              end; // taWrite
  end;  //  --  Case AAction Of
  __Free_DataStorageQuery(pToken);
end;

function TmafDynamicFuntionTable.__GetHistoryCount: Integer;
var i : Integer;
begin
  Result := 0;
  For i := 0 To FpInstallHistory.Count - 1 Do
    Result := Result + PInstallHistory(FpInstallHistory.Items[i])^.FpHistory.Count;
end;

procedure TmafDynamicFuntionTable.Initialize_InstallHistory;
begin
  FpInstallHistory := TList.Create;
  __TemplateReadWrite_InstallHistory(taRead);
end;

procedure TmafDynamicFuntionTable.Free_InstallHistory;
var pToken : PInstallHistory;
    pSH : PInstallRecord;
begin
  __TemplateReadWrite_InstallHistory(taWrite);
  While FpInstallHistory.Count > 0 Do begin
    pToken := PInstallHistory(FpInstallHistory.Items[0]);
    While pToken^.FpHistory.Count > 0 Do begin
      pSH := PInstallRecord(pToken^.FpHistory.Items[0]);
      __Free_InstallRecord(pSH);
      pToken^.FpHistory.Delete(0);
    end;  //  --  While pToken^.FpHistory.Count Do
    __Free_InstallHistory(pToken);
    FpInstallHistory.Delete(0);
  end;  //  --  While FpInstallHistory.Count > 0 Do
  FreeAndNil(FpInstallHistory);
end;

procedure TmafDynamicFuntionTable.HistoryToken_Add(var pToken: PSubHook; InstallerModuleID : Integer);
var i : Integer;
    pIH : PInstallHistory;
    pIR : PInstallRecord;
begin
  pIR := __Create_InstallRecord;
  pIR^.HookID := pToken^.HookID;
  pIR^.SubHookID := pToken^.SubHookID;
  pIR^.ModuleID := pToken^.nModuleID;
  pIR^.CodegroupID := pToken^.nCodeGroupID;
  pIR^.InstallerModuleID := InstallerModuleID;
  For i := 0 To FpInstallHistory.Count - 1 Do
    If PInstallHistory(FpInstallHistory.Items[i])^.Hook_uID = pToken^.uID Then begin
      PInstallHistory(FpInstallHistory.Items[i])^.FpHistory.Add(pIR); // place it at the end
      Exit;
    end;  //  --  If PDynamicFunction(FpInstallHistory.Items[i])^.HookID = pToken^.HookID Then

  // if we get here, there was no previous entry for that Hook.. we create one
  pIH := __Create_InstallHistory;       // create new install record
  pIH^.Hook_uID := pToken^.uID;         // assign the uID, as in the InstallHistory all is sorted by UniqueID
  pIH^.FpHistory.Add(pIR);              // add the SubHook
  FpInstallHistory.Add(pIH);            // and add the InstallHistory
end;

function TmafDynamicFuntionTable.HistoryToken_Get(pToken: PSubHook; bRemove: Boolean): PSubHook;
var i : Integer;
    pIH : PInstallHistory;
    pIR : PInstallRecord;
begin
  Result := nil;
  pIH := nil;
  For i := 0 To FpInstallHistory.Count - 1 Do
    // the HookID in the DynamicFunction record actually is the uID
    // every uID has it's own list in the IntallHistory
    If PInstallHistory(FpInstallHistory.Items[i])^.Hook_uID = pToken^.uID Then begin
      pIH := PInstallHistory(FpInstallHistory.Items[i]);
      Break;
    end;  //  --  If PInstallHistory(FpInstallHistory.Items[i])^.uID_Hook = pToken^.uID Then
    
  If pIH = nil Then
    Exit;

  // now we take the last item from the history stack, if the InstallerModuleID is the currently uninstalling module
  If pIH^.FpHistory.Count > 0 Then begin
    pIR := PInstallRecord(pIH^.FpHistory.Items[pIH^.FpHistory.Count - 1]);
    If pIR^.InstallerModuleID = pToken^.nModuleID Then begin
      // the last history record we create when we installed the module currently de-installed
      // so we can return it to the previous state
      Result := __Create_SubHookToken;
      Result^.uID := pIH^.Hook_uID;
      Result^.HookID := pIR^.HookID;
      Result^.SubHookID := pIR^.SubHookID;
      Result^.nModuleID := pIR^.ModuleID;
      Result^.nCodeGroupID := pIR^.CodegroupID;
      Result^.bActive := pToken^.bActive;
      Result^.pModuleManager := GetModuleManager(Result^.nModuleID);
      If bRemove Then begin
        __Free_InstallRecord(pIR);
        pIH^.FpHistory.Delete(pIH^.FpHistory.Count - 1);
      end;  //  --  If bRemove Then
    end else begin
      // obviously the last history record wasn't from the currently uninstalling module
      // so we have to check all records for the ones with our ModuleID and just delete it
      For i := pIH^.FpHistory.Count - 1 DownTo 0 Do
        If PInstallRecord(pIH^.FpHistory.Items[i])^.InstallerModuleID = pToken^.nModuleID Then begin
          pIR := PInstallRecord(pIH^.FpHistory.Items[i]);
          __Free_InstallRecord(pIR);
          pIH^.FpHistory.Delete(i);
        end;
    end;
  end;
  // finally we check, if the install history for this uID still has record
  // and delete it, if needed
  If pIH^.FpHistory.Count = 0 Then begin
    FpInstallHistory.Delete(FpInstallHistory.IndexOf(pIH));
    __Free_InstallHistory(pIH);
  end;  //  --  If pIH^.FpHistory.Count = 0 Then
end;





{ *********************** Dynamic Functions ********************************** }




function __DoSort_Items(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  // return -1 if Item1 < Item2
  //         0 if equal (shouldn't happen anyway)
  //         1 if Item1 > Item2
  If PDynamicFunction(Item1)^.HookID > PDynamicFunction(Item2)^.HookID Then begin
    Result := 1;
    Exit;
  end;

  If PDynamicFunction(Item1)^.HookID < PDynamicFunction(Item2)^.HookID Then begin
    Result := -1;
    Exit;
  end;
end;

function TmafDynamicFuntionTable.GetSubHookToken(nHookID, SubHookID, ModuleID: Integer): PSubHook;
var i, j: Integer;
begin
  Result := nil;
  For i := 0 To FpDFT.Count - 1 Do
    If PDynamicFunction(FpDFT.Items[i])^.HookID = nHookID Then begin
      For j := 0 To PDynamicFunction(FpDFT.Items[i])^.FpSubHooks.Count - 1 Do
        If PSubHook(PDynamicFunction(FpDFT.Items[i])^.FpSubHooks.Items[j])^.SubHookID = SubHookID Then
          If PSubHook(PDynamicFunction(FpDFT.Items[i])^.FpSubHooks.Items[j])^.nModuleID = ModuleID Then begin
            Result := PSubHook(PDynamicFunction(FpDFT.Items[i])^.FpSubHooks.Items[j]);
            Exit;
          end;  //  --  If PSubHook(PDynamicFunction(FpDFT.Items[i])^.FpSubHooks.Items[j])^.nModuleID = ModuleID Then
    end;  //  --  If PDynamicFunction(FpDFT.Items[i])^.HookID = nHookID Then
end; // GetSubHookToken

procedure TmafDynamicFuntionTable.__FreeDynamicFunctions;
var //pSH  : PSubHook;
    pDF  : PDynamicFunction;
    i : Integer;
begin
  For i := FpDFT.Count - 1 DownTo 0 Do begin
    pDF := PDynamicFunction(FpDFT.Items[i]);
    __FreeDynamicFunctionToken(pDF);
  end;  //  --  While FpDFT.Count > 0 Do
  FpDFT.Clear;
end;

procedure TmafDynamicFuntionTable.__FreeFunctionObserver;
var i: Integer;
begin
  For i := FpFunctionObserver.Count - 1 DownTo 0 Do
    Dispose(PFunctionObserver(FpFunctionObserver.Items[i]));
  FpFunctionObserver.Clear;
end;

procedure TmafDynamicFuntionTable.__LoadDynamicFunctions;
begin
  __FreeCodeGroups;                             // free the Codegroups
  __FreeDynamicFunctions;                       // free the DFT entries
  If Not FbLockdown Then
    __TemplateReadWrite_CodeGroupRights(taRead);// read the access rights
  __TemplateReadWrite_DynamicFunctions(taRead); // load the DFT
  __FreeCodeGroupRights;                        // throw the access rights away, we don't need them anymore
  FpCodeGroups.Sort(__DoSort_Items);            // PDynamicFunction and PCodeGroup have same record structure, so we can use the same sort function
  __Initialize_UsedModules(FbLockdown);         // initialize the modules used atm
  If Not FbLockdown Then
    __FreeUnusedModules;                        // unload all modules that don't export functions for the current DFT
end;

procedure TmafDynamicFuntionTable.__TemplateReadWrite_DynamicFunctions(AAction: TDataStorageAction);
var pToken  : PDataStorageQuery;
begin
  FpTemplateStreamer.Attributes := 1;
  FpTemplateStreamer.Stream.Size := 0;
  pToken := __Create_DataStorageQuery(HOOKMANAGER_DATA_CATEGORY, HOOKMANAGER_DATA_HOOKS);
  pToken^.aObj := FpTemplateStreamer.Stream;  // we use the internal stream of the TempleateStreamer
  pToken^.Action := AAction;
  Case AAction Of
    taRead  : begin
                TmafHookManager(FpHookManager).DataStorage.Query(pToken);
                If FpTemplateStreamer.Stream.Size > 0 Then
                  FpTemplateStreamer.ReadStream;
              end; // taRead
    taWrite : begin
                FpDFT.Sort(__DoSort_Items);
                FpTemplateStreamer.WriteStream(DFT_STREAM_ID);
                TmafHookManager(FpHookManager).DataStorage.Query(pToken);
              end; // taWrite
  end;  //  --  Case AAction Of
  __Free_DataStorageQuery(pToken);
end;

function TmafDynamicFuntionTable.__ActivateSubHook(uID: Integer; bActive: Boolean): Integer;
var i, j : Integer;
begin
  Result := ERR_SUBHOOK_NOT_FOUND;
  For i := 0 To FpDFT.Count - 1 Do
    For j := 0 To PDynamicFunction(FpDFT.Items[i]).FpSubHooks.Count - 1 do
      If PSubHook(PDynamicFunction(FpDFT.Items[i]).FpSubHooks.Items[j])^.uID = uID Then begin
        PSubHook(PDynamicFunction(FpDFT.Items[i]).FpSubHooks.Items[j])^.bActive := bActive;
        __TemplateReadWrite_DynamicFunctions(taWrite);
        Result := ERR_NO_ERROR;
        Break;
      end;  //  --  If PSubHook(PDynamicFunction(FpDFT.Items[i]).FpSubHooks.Items[j])^.uID = uID Then
end;

function TmafDynamicFuntionTable.__Find_FunctionObserver(nHookID, nSubHookID: Integer; Sender: TComponent): PFunctionObserver;
var i : Integer;
    pObserver : PFunctionObserver;
begin
  Result := nil;
  For i := 0 To FpFunctionObserver.Count - 1 Do begin
    pObserver := PFunctionObserver(FpFunctionObserver.Items[i]);
    If ((pObserver^.nHookID = nHookID) And (pObserver^.nSubHookID = nSubHookID) And (pObserver^.pCallBack = Sender)) Then begin
      Result := pObserver;
      Break;
    end;
  end;
end;

// AType defines, which type of event should be processed
// 1 = only deletion
// 2 = only add
// 3 = both
procedure TmafDynamicFuntionTable.__Notify_FunctionObserver(AType: Byte = 3);
var i, j : Integer;
    pObserver : PFunctionObserver;
    bWholeDF : Boolean;
    pDF : PDynamicFunction;
    pSH : PSubHook;
begin
  For i := 0 To FpFunctionObserver.Count - 1 Do begin
    pObserver := PFunctionObserver(FpFunctionObserver.Items[i]);
    bWholeDF := (pObserver^.nSubHookID = 0);
    pDF := GetDynamicFunction(pObserver^.nHookID);  // let's see, if that DynamicFunction is still there

    If ((Atype = 1) or (AType = 3)) Then begin
      // First we check for deleted functions
      For j := FpDeletedHooks.Count - 1 DownTo 0 Do begin
        If PSubHookChange(FpDeletedHooks.Items[j])^.nHookID = pObserver^.nHookID Then begin
          // ok, somebody is watching something here
          If bWholeDF Then
            SendComponentMessage(pObserver^.pCallBack, MSG_FUNCTION_OBSERVER_DEL, Pointer(pObserver^.nHookID), Pointer(PSubHookChange(FpDeletedHooks.Items[j])^.nSubHookID), False)
          Else
            If PSubHookChange(FpDeletedHooks.Items[j])^.nSubHookID = pObserver^.nSubHookID Then
              SendComponentMessage(pObserver^.pCallBack, MSG_FUNCTION_OBSERVER_DEL, Pointer(pObserver^.nHookID), Pointer(pObserver^.nSubHookID), False);
        end;  //  --  If PSubHookChange(FpDeletedHooks.Items[j])^.nHookID = pObserver^.nHookID Then
      end;  //  --  For j := FpDeletedHooks.Count - 1 DownTo 0 Do
    end;  //  --  If ((Atype = 1) or (AType = 3)) Then

    If ((Atype = 2) or (AType = 3)) Then begin
      // now we register all added functions
      For j := FpAddedHooks.Count - 1 DownTo 0 Do begin
        If PSubHookChange(FpAddedHooks.Items[j])^.nHookID = pObserver^.nHookID Then begin
          // ok, somebody is watching something here
          If pDF = nil Then  // if the DynamicFunction isn't there anymore, the user might not have access to it, so no notify needed
            Continue;
          pSH := __GetSubHook(PSubHookChange(FpAddedHooks.Items[j])^.nSubHookID, pDF);
          // if the SubHook isn't there, the user has no access, no notify needed
          If pSH = nil Then
            Continue;

          If bWholeDF Then
            SendComponentMessage(pObserver^.pCallBack, MSG_FUNCTION_OBSERVER_ADD, Pointer(pObserver^.nHookID), Pointer(pSH^.SubHookID), False)
          Else
            If PSubHookChange(FpDeletedHooks.Items[j])^.nSubHookID = pObserver^.nSubHookID Then
              SendComponentMessage(pObserver^.pCallBack, MSG_FUNCTION_OBSERVER_ADD, Pointer(pObserver^.nHookID), Pointer(pObserver^.nSubHookID), False);
        end;  //  --  If PSubHookChange(FpDeletedHooks.Items[j])^.nHookID = pObserver^.nHookID Then
      end;  //  --  For j := FpAddedHooks.Count - 1 DownTo 0 Do
    end;  //  --  If ((Atype = 2) or (AType = 3)) Then
  end;  //  --  For i := 0 To FpFunctionObserver.Count - 1 Do

  // Now we don't need the information about added and deleted subhooks anymore
  If ((Atype = 2) or (AType = 3)) Then begin
    For i := 0 To FpAddedHooks.Count - 1 Do
      Dispose(PSubHookChange(FpAddedHooks.Items[i]));
    FpAddedHooks.Count := 0;
  end;
  If ((Atype = 1) or (AType = 3)) Then begin
    For i := 0 To FpDeletedHooks.Count - 1 Do
      Dispose(PSubHookChange(FpDeletedHooks.Items[i]));
    FpDeletedHooks.Count := 0;
  end;
end;

function TmafDynamicFuntionTable.__Add_FunctionObserver(nHookID, nSubHookID: Integer; Sender: TComponent): Integer;
var pObserver : PFunctionObserver;
begin
  Result := ERR_NO_ERROR;
  If __Find_FunctionObserver(nHookID, nSubHookID, Sender) <> nil Then begin
    Result := ERR_NOTHING_TO_DO;
    Exit;
  end;

  If Sender = nil Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;

  New(pObserver);
  pObserver^.nHookID := nHookID;
  pObserver^.nSubHookID := nSubHookID;
  pObserver^.pCallBack := Sender;
  pObserver^.nModuleID := 0; // not needed right now
  FpFunctionObserver.Add(pObserver);
end;

function TmafDynamicFuntionTable.__Delete_FunctionObserver(nHookID, nSubHookID: Integer; Sender: TComponent): Integer;
var pObserver : PFunctionObserver;
begin
  Result := ERR_NOTHING_TO_DO;
  pObserver := __Find_FunctionObserver(nHookID, nSubHookID, Sender);
  If pObserver <> nil Then begin
    FpFunctionObserver.Delete(FpFunctionObserver.IndexOf(pObserver));
    Dispose(pObserver);
    Result := ERR_NO_ERROR;
  end;
end;

function TmafDynamicFuntionTable.__Create_SubHookChange_Item(nHookID, nSubHookID: Integer; Action: Byte): PSubHookChange;
begin
  New(Result);
  Result^.nHookID := nHookID;
  Result^.nSubHookID := nSubHookID;
  Result^.bChange := Action;
end;

function TmafDynamicFuntionTable.__RegisterSubHook(pToken: PMAFInstallToken; bSave: Boolean = True): Integer;
var pDF : PDynamicFunction;
    pSH, pSH2 : PSubHook;
    aList : TList;
    i : Integer;
    pModule : TmafModuleManager;
    QHS : pQHS;
begin
  Result := ERR_NO_ERROR;
  If pToken = nil Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;  //  --  If pToken = nil Then

  If pToken^.nAction = iaDelete Then begin
    __UnregisterSubHook(pToken);
    Exit;
  end;  //  --  If pToken^.nAction = iaDelete Then

  // if the following uID isn't going to overwrite anything, it will be placed
  // somewhere, so we better check, if not by any chance the user tries to install
  // an old module where the current function already was overwritten
  // so if the uID exists, we do nothing
  If pToken^.InsertDir <> hidOverwrite Then begin
    pSH := __GetSubHook_uID(pToken^.uID);
    If Assigned(pSH) Then begin
      Result := ERR_SUBHOOK_UID_EXISTS;
      Exit;
    end;  //  --  If Assigned(pSH) Then
  end;  //  --  If pToken^.InsertDir <> hidOverwrite Then

  pDF := GetDynamicFunction(pToken^.nHookID);
  If pDF = nil Then begin
    // we don't have that dynamic function yet, so we need to create it...
    pDF := __CreateDynamicFunctionToken;
    pDF^.HookID := pToken^.nHookID;
    FpDFT.Add(pDF);
  end;  //  --  If pDF = nil Then

  // now we have a dynamic function for sure
  pSH := __Create_SubHookToken;

  // uID handling : if the editor creates a new SubHook the new uID needs to be
  //                created by the ModuleController inside the module where the
  //                function will be registered
  If pToken^.uID < 1 Then begin
    // first we need the module
    pModule := GetModuleManager(pToken^.nModuleID);
    If Assigned(pModule) Then begin
      If Assigned(pModule.ModuleController) Then begin
        QHS := __Create_QueryHandlerStruct;
        // owner of the HookManager is the datamadule which has the mainform as owner
        QHS^.pParent := TmafHookManager(FpHookManager).Owner.Owner;
        If pModule.Initialize(QHS, nil) = ERR_NO_ERROR Then begin
          If Not Assigned(pModule.ModuleController) Then
            Raise EComponentError.Create('TmafDynamicFuntionTable.__RegisterSubHook: Cannot find TmafModuleController in module ' + pModule.ModuleName);
          pSH^.uID := pModule.ModuleController.GetNext_uID;
//          ShowMessage('MC uID: ' + IntToStr(pSH^.uID));
          // Ok, now we got what the moduleController thinks is the next uID
          // but it can be, that the ModuleController isn't up to date
          // in order to be up to date, we would have to import the XML (or register
          // the previoulsy created dynamic functions manually) and then recompile
          // the DLL
          // so what we do is checking our own records for higher uIDs
          If pSH^.uID < pModule.FnMax_uID Then begin
            pSH^.uID := pModule.Max_uID;
//            ShowMessage('pModule uID: ' + IntToStr(pSH^.uID));
          end;
          // and we update the Max_uID for the module in case the user wants to create several subhooks without closing the editor
          If pModule.FnMax_uID < pSH^.uID Then
            pModule.FnMax_uID := pSH^.uID;
        end;
        __Free_QueryHandlerStruct(QHS);
      end;

    end;
  end Else
    pSH^.uID := pToken^.uID;
  pSH^.HookID := pToken^.nHookID;
  pSH^.SubHookID := pToken^.nSubHookID;
  pSH^.nModuleID := pToken^.nModuleID;
  pSH^.nCodeGroupID := pToken^.nCodeGroupID;
  pSH^.bActive := (pToken^.bActive > 0);
  pSH^.pModuleManager := GetModuleManager(pToken^.nModuleID);
  aList := TList.Create;
  Case pToken^.InsertDir Of
    hidLast  : begin
                 pDF^.FpSubHooks.Add(pSH);
                 // we register the add to inform the function observer about it
                 FpAddedHooks.Add(__Create_SubHookChange_Item(pSH^.HookID, pSH^.SubHookID, 1));
               end;
    hidFirst : begin
                 While pDF^.FpSubHooks.Count > 0 Do begin
                   aList.Add(pDF^.FpSubHooks.Items[0]);
                   pDF^.FpSubHooks.Delete(0);
                 end;  //  --  While pDF^.FpSubHooks.Count > 0 Do
                 pDF^.FpSubHooks.Add(pSH);
                 // we register the add to inform the function observer about it
                 FpAddedHooks.Add(__Create_SubHookChange_Item(pSH^.HookID, pSH^.SubHookID, 1));
                 While aList.Count > 0 Do begin
                   pDF^.FpSubHooks.Add(aList.Items[0]);
                   aList.Delete(0);
                 end;  //  --  While aList.Count > 0 Do
               end; // hidFirst
    hidAfter,
    hidBefore: begin
                 While pDF^.FpSubHooks.Count > 0 Do begin
                   If PSubHook(pDF^.FpSubHooks.Items[0])^.uID = pToken^.nRelative_uID Then begin
                     If pToken^.InsertDir = hidBefore Then begin
                       aList.Add(pSH);
                       aList.Add(pDF^.FpSubHooks.Items[0]);
                     end;  //  --  If pToken^.InsertDir = hidBefore Then
                     If pToken^.InsertDir = hidAfter Then begin
                       aList.Add(pDF^.FpSubHooks.Items[0]);
                       aList.Add(pSH);
                     end;  //  --  If pToken^.InsertDir = hidAfter Then
//                     aList.Delete(0);
                   end else
                     aList.Add(pDF^.FpSubHooks.Items[0]);
                   pDF^.FpSubHooks.Delete(0);
                 end;  //  --  While pDF^.FpSubHooks.Count > 0 Do

                 // in case the Relative_uID wasn't found, we add the SubHook at the end
                 If aList.IndexOf(pSH) = -1 Then
                   aList.Add(pSH);
                 // we register the add to inform the function observer about it
                 FpAddedHooks.Add(__Create_SubHookChange_Item(pSH^.HookID, pSH^.SubHookID, 1));

                 While aList.Count > 0 Do begin
                   pDF^.FpSubHooks.Add(aList.Items[0]);
                   aList.Delete(0);
                 end;  //  --  While aList.Count > 0 Do
               end; // hidAfter / hidBefore
    hidOverwrite : begin
                     For i := 0 To pDF^.FpSubHooks.Count - 1 Do
                       If PSubHook(pDF^.FpSubHooks.Items[i])^.uID = pToken^.uID Then begin
                         pSH2 := PSubHook(pDF^.FpSubHooks.Items[i]);  // save the old SubHook for backup
                         If FbUseInstallHistory Then
                           HistoryToken_Add(pSH2, pToken^.nModuleID);  // add it to the history record
                         // now we register the delete to inform the function observer about the deletion
                         FpDeletedHooks.Add(__Create_SubHookChange_Item(pSH2^.HookID, pSH2^.SubHookID, 2));
                         __Notify_FunctionObserver(1);
                         // now it can be, that the hook being uninstalled is an open top-level-window, so we inform
                         // the ModuleController, that the SubHook goes
                         pModule := GetModuleManager(pSH2^.nModuleID);
                         If Assigned(pModule) Then
                           SendComponentMessage(pModule.ModuleController, WM_SUBHOOK_CLOSE, Pointer(pSH2^.HookID), Pointer(pSH2^.SubHookID), False);

                         __Free_SubHookToken(pSH2);
                         pDF^.FpSubHooks.Items[i] := pSH;
                         // we register the add to inform the function observer about it
                         FpAddedHooks.Add(__Create_SubHookChange_Item(pSH^.HookID, pSH^.SubHookID, 1));
                         pSH := nil;
                         Break;
                       end;
                     If pSH <> nil Then begin     // in case the function to overwrite wasn't there
                       pDF^.FpSubHooks.Add(pSH);  // we add it just at the end
                       // we register the add to inform the function observer about it
                       FpAddedHooks.Add(__Create_SubHookChange_Item(pSH^.HookID, pSH^.SubHookID, 1));
                     end;
                   end;
  end;  //  --  Case pToken^.InsertDir Of
  aList.Free;
  If bSave Then
    __TemplateReadWrite_DynamicFunctions(taWrite);
end;

procedure TmafDynamicFuntionTable.__Reset_ExportedFunctionCount;
var i: Integer;
begin
  For i := 0 To FpModules.Count - 1 Do
    TmafModuleManager(FpModules.Items[i]).ExportedFunctions := 0;
end;

function TmafDynamicFuntionTable.__UnregisterSubHook(pToken: PmafInstallToken; bSave: Boolean = True): Integer;
var pDF : PDynamicFunction;
    pSH, pSH2 : PSubHook;
    idx : Integer;
    bDelete : Boolean;
    pModule : TmafModuleManager;
begin
  bDelete := True;
  Result := ERR_SUBHOOK_NOT_FOUND;
  pDF := GetDynamicFunction(pToken^.nHookID);    // getting the Dynamic Function
  If pToken^.uID > 0 Then
    pSH := __GetSubHook_uID(pToken^.uID)          // searches for a SubHook by its unique ID
  Else
    pSH := __GetSubHook(pToken^.nSubHookID, pDF);
  If pSH = nil Then
    Exit;
  idx := pDF^.FpSubHooks.IndexOf(pSH);           // the current entry

  // to inform the FunctionObserver about the deletion, we save the SubHookInfo and notify the observer
  // immediately, because later the module will be unloaded and closing an open window from that module
  // will be impossible
  // also we need to do it now before restoring something from the InstallHistory, as this will change
  // the currently active Master-DFT and the function would be already gone and cannot be executed
  Include(FTableState, tsCleaning);
  FpDeletedHooks.Add(__Create_SubHookChange_Item(pSH^.HookID, pSH^.SubHookID, 2));
  __Notify_FunctionObserver(1);
  
  // now it can be, that the hook being uninstalled is an open top-level-window, so we inform
  // the ModuleController, that the SubHook goes
  pModule := GetModuleManager(pSH^.nModuleID);
  If Assigned(pModule) Then
    SendComponentMessage(pModule.ModuleController, WM_SUBHOOK_CLOSE, Pointer(pSH^.HookID), Pointer(pSH^.SubHookID), False);

  Exclude(FTableState, tsCleaning);

  // if the hook was overwriting another one, we restore the last one
  If ((pToken^.InsertDir = hidOverwrite) And (FbUseInstallHistory)) Then begin
    pSH2 := nil;
    If pSH^.nModuleID = pToken^.nModuleID Then  // but only, if the uninstalling module is still the active one
      pSH2 := HistoryToken_Get(pSH, True);
    If Assigned(pSH2) Then begin
      pDF^.FpSubHooks.Items[idx] := pSH2;       // install the entry from the install history
      // we remember the Hook was added again and inform the function observer later on
      FpAddedHooks.Add(__Create_SubHookChange_Item(pSH2^.HookID, pSH2^.SubHookID, 1));
      bDelete := False;
    end;
  end;

  If bDelete Then                           // if it wasn't replaced by an item from the InstallHistory
    If idx > -1 Then                        // and we found the SubHook
      pDF.FpSubHooks.Delete(idx)            // then we delete it from the SubHook list
    Else begin
      Result := ERR_UNKNOWN_ERROR;
      Exit;
    end;

  __Free_SubHookToken(pSH);                 // free the old one

  If pDF^.FpSubHooks.Count = 0 Then begin   // now we check, if the Dynamic function is empty
    FpDFT.Delete(FpDFT.IndexOf(pDF));       // and delete it from the DFT, if so
    __FreeDynamicFunctionToken(pDF);
  end;  //  --  If pDF^.FpSubHooks.Count = 0 Then
  Result := ERR_NO_ERROR;
  If bSave Then
    __TemplateReadWrite_DynamicFunctions(taWrite);
end;

function TmafDynamicFuntionTable.__GetSubHook_uID(uID: Integer): PSubHook;
var i, j : Integer;
begin
  Result := nil;
  For i := 0 To FpDFT.Count - 1 Do
    For j := 0 To PDynamicFunction(FpDFT.Items[i]).FpSubHooks.Count - 1 Do
      If PSubHook(PDynamicFunction(FpDFT.Items[i])^.FpSubHooks.Items[j])^.uID = uID Then begin
        Result := PSubHook(PDynamicFunction(FpDFT.Items[i])^.FpSubHooks.Items[j]);
        Break;
      end;
end;

function TmafDynamicFuntionTable.GetDynamicFunction(nHookID: Integer): PDynamicFunction;
var nLower, nUpper, nMid : Integer;
begin
  Result := nil;
  If FpDFT.Count < 2 Then begin
    If FpDFT.Count = 1 Then
      If PDynamicFunction(FpDFT.Items[0])^.HookID = nHookID Then
        Result := PDynamicFunction(FpDFT.Items[0]);
    Exit;
  end;  //  --  If Data.Count < 2 Then

  nLower := 0;
  nUpper := FpDFT.Count - 1;
  While True Do begin
    nMid := (nLower + nUpper) div 2;
    If nHookID < PDynamicFunction(FpDFT.Items[nMid])^.HookID Then
      nUpper := nMid - 1
    Else
      If nHookID > PDynamicFunction(FpDFT.Items[nMid])^.HookID Then
        nLower := nMid + 1
      Else begin
        Result := PDynamicFunction(FpDFT.Items[nMid]);
        Exit;
      end;
    If (nLower > nUpper) Then
      Exit;
  end;  //  --  While True Do
end;

function TmafDynamicFuntionTable.__GetSubHook(nSubHookID: Integer; pToken: PDynamicFunction = nil): PSubHook;
var i, j : Integer;
begin
  Result := nil;
  If pToken = nil Then begin
    For j := 0 To FpDFT.Count - 1 Do begin
      pToken := PDynamicFunction(FpDFT.Items[j]);
      For i := 0 To pToken^.FpSubHooks.Count - 1 do
        If PSubHook(pToken^.FpSubHooks.Items[i])^.SubHookID = nSubHookID Then begin
          Result := PSubHook(pToken^.FpSubHooks.Items[i]);
          Break;
        end;  //  --  If PSubHook(pToken^.FpSubHooks.Items[i])^.SubHookID = nSubHookID Then
    end;  //  --  For j := 0 To FpDFT.Count - 1 Do
  end else begin
    For i := 0 To pToken^.FpSubHooks.Count - 1 do
      If PSubHook(pToken^.FpSubHooks.Items[i])^.SubHookID = nSubHookID Then begin
        Result := PSubHook(pToken^.FpSubHooks.Items[i]);
        Break;
      end;
  end;
end;

function TmafDynamicFuntionTable.__GetSubHookCount: Integer;
var i : Integer;
begin
  Result := 0;
  For i := 0 To FpDFT.Count - 1 Do
    Result := Result + PDynamicFunction(FpDFT.Items[i])^.FpSubHooks.Count;
end;




{ *********************** CodeGroup Functions ******************************** }




function TmafDynamicFuntionTable.__ChangeCodeGroup(pToken: PMAFInstallToken): Integer;
var SubHook : PSubHook;
begin
  If pToken = nil Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;
  Result := ERR_NO_ERROR;
  SubHook := __GetSubHook_uID(pToken^.uID);
  If Assigned(SubHook) Then begin
    If pToken^.nCodeGroupID > -1 Then begin    // if there was a codegroup set
      If SubHook^.nCodeGroupID > -1 Then       // if the SubHook was assigned to a CodeGroup
        __CodeGroup_DeleteFunction(SubHook);   // remove the SubHook from the CodeGroup
      SubHook^.nCodeGroupID := pToken^.nCodeGroupID; // assign the new CodeGroupID to the SubHook
      __CodeGroup_AddFunction(SubHook);      // assign to its new CodeGroup
    end Else
      __CodeGroup_DeleteFunction(SubHook);
    __TemplateReadWrite_DynamicFunctions(taWrite); // and save changes  
  end;  //  --  If Result = ERR_NO_ERROR Then
end;

procedure TmafDynamicFuntionTable.__FreeCodeGroupRights;
var i : Integer;
begin
  For i := 0 To FpCG_Rights.Count - 1 do
    Dispose(PAccessData(FpCG_Rights.Items[i]));
  FpCG_Rights.Clear;  
end;

procedure TmafDynamicFuntionTable.__FreeCodeGroups;
var pToken : PCodeGroup;
begin
  While FpCodeGroups.Count > 0 Do begin
    pToken := PCodeGroup(FpCodeGroups.Items[0]);
    __FreeCodeGroupToken(pToken);
    FpCodeGroups.Delete(0);
  end;  //  --  While FpCodeGroups.Count > 0 Do
end; // __FreeCodeGroups

procedure TmafDynamicFuntionTable.DeleteCodeGroup(nCodeGroupID: Integer);
var i : Integer;
    pToken : PCodeGroup;
    pData : PSubHook;
begin
  pToken := nil;
  For i := 0 To FpCodeGroups.Count - 1 Do
    If PCodeGroup(FpCodeGroups.Items[i])^.nCodeGroupID = nCodeGroupID Then begin
      pToken := PCodeGroup(FpCodeGroups.Items[i]);
      Break;
    end;
  If pToken <> nil Then
    While pToken^.FpSubHooks.Count > 0 Do begin
      pData := PSubHook(pToken^.FpSubHooks.Items[0]);
      __CodeGroup_DeleteFunction(pData); // deletes the CodeGroup, when it runs out of members
    end;  //  --  While pToken^.FpSubHooks.Count > 0 Do
end;

procedure TmafDynamicFuntionTable.__CodeGroup_AddFunction(pToken: PSubHook);
var i : Integer;
    bAdded : Boolean;
    pCG : PCodeGroup;
begin
  If pToken^.nCodeGroupID = -1 Then  // if not assigned into a CodeGroup...
    Exit;

  bAdded := False;
  For i := 0 To FpCodeGroups.Count - 1 Do
    If PCodeGroup(FpCodeGroups.Items[i])^.nCodeGroupID = pToken^.nCodeGroupID Then begin
      PCodeGroup(FpCodeGroups.Items[i])^.FpSubHooks.Add(pToken);
      bAdded := True;
      Break;
    end;  //  --  If PCodeGroup(FpCodeGroups.Items[i])^.nCodeGroupID = pToken^.nCodeGroupID Then
  If Not bAdded Then begin
    pCG := __CreateCodeGroupToken;
    pCG^.nCodeGroupID := pToken^.nCodeGroupID;
    pCG^.FpSubHooks.Add(pToken);
    FpCodeGroups.Add(pCG);
    // and check, if we found a higher CodeGroupID than we had until now
    If FnHighCodeGroupID < pCG^.nCodeGroupID Then
      FnHighCodeGroupID := pCG^.nCodeGroupID;
  end;  //  --  If Not bAdded Then
end;

procedure TmafDynamicFuntionTable.__CodeGroup_DeleteFunction(pToken: PSubHook);
var i, nPos : Integer;
    pData  : PCodeGroup;
begin
  For i := 0 To FpCodeGroups.Count - 1 Do
    If PCodeGroup(FpCodeGroups.Items[i])^.nCodeGroupID = pToken^.nCodeGroupID Then begin
      pData := PCodeGroup(FpCodeGroups.Items[i]);
      nPos := pData^.FpSubHooks.IndexOf(pToken);
      If nPos > -1 Then begin
        pData^.FpSubHooks.Delete(nPos);
        pToken^.nCodeGroupID := -1;
        If pData^.FpSubHooks.Count = 0 Then begin
          __FreeCodeGroupToken(pData);
          FpCodeGroups.Delete(i);
        end;  //  --  If pData^.FpSubHooks.Count = 0 Then
      end;  //  --  If nPos > -1 Then
    end;  //  --  If PCodeGroup(FpCodeGroups.Items[i])^.nCodeGroupID = pToken^.nCodeGroupID Then
end;

function TmafDynamicFuntionTable.__CodeGroup_Find(nID: Integer): PCodeGroup;
var nLower, nUpper, nMid : Integer;
begin
  Result := nil;
  If FpCodeGroups.Count < 2 Then begin
    If FpCodeGroups.Count = 1 Then
      If PCodeGroup(FpCodeGroups.Items[0])^.nCodeGroupID = nID Then
        Result := PCodeGroup(FpCodeGroups.Items[0]);
    Exit;
  end;  //  --  If FpCodeGroups.Count < 2 Then

  nLower := 0;
  nUpper := FpCodeGroups.Count - 1;
  While True Do begin
    nMid := (nLower + nUpper) div 2;
    If nID < PCodeGroup(FpCodeGroups.Items[nMid])^.nCodeGroupID Then
      nUpper := nMid - 1
    Else
      If nID > PCodeGroup(FpCodeGroups.Items[nMid])^.nCodeGroupID Then
        nLower := nMid + 1
      Else begin
        Result := PCodeGroup(FpCodeGroups.Items[nMid]);
        Exit;
      end;
    If (nLower > nUpper) Then
      Exit;
  end;  //  --  While True Do
end;

procedure TmafDynamicFuntionTable.CodeGroups_CheckDescriptions(AList: TList);
var i : Integer;
    pCG_Data : PCodeGroupData;
//    pCG : PCodeGroup;
begin
  If AList = nil Then
    Exit;

  For i := AList.Count - 1 DownTo 0 Do begin
    pCG_Data := PCodeGroupData(AList.Items[i]);  // the current CodeGroup description data
    If TmafHookManager(FpHookManager).BaseDB.ConnectionData.SecurityLevel < pCG_Data^.MinSL Then begin
      AList.Delete(i);
    end;

{    pCG := __CodeGroup_Find(pCG_Data^.nID);      // let's find the CodeGroup entry
    If pCG = nil Then                            // if it isn't there, it does not exist for this user
      AList.Delete(i);                           // so we don't show the description to the user }
  end;
end;

var pGroupUserData : PGroupData;
    StreamTarget   : Integer;

function TmafDynamicFuntionTable.__Get_CodeGroup_Right(nCodegroupID: Integer): PAccessData;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpCG_Rights.Count - 1 Do
    If PAccessData(FpCG_Rights.Items[i])^.nCodeGroupID = nCodegroupID Then begin
      Result := PAccessData(FpCG_Rights.Items[i]);
      Break;
    end;
end;

function TmafDynamicFuntionTable.__Del_CodeGroup_Rights(ATarget: Integer; pData: PGroupData): Integer;
var i : Integer;
begin
  Result := ERR_NO_ERROR;
  pGroupUserData := pData;
  StreamTarget := ATarget;
  __TemplateReadWrite_CodeGroupRights(taRead);
  For i := 0 to FpCG_Rights.Count - 1 Do
    Dispose(PAccessData(FpCG_Rights.Items[i]));
  FpCG_Rights.Clear;
  __TemplateReadWrite_CodeGroupRights(taWrite);
end;

function TmafDynamicFuntionTable.__Get_CodeGroup_Rights(ATarget: Integer; pData: PGroupData): Integer;
var i : Integer;
    pAD : PAccessData;
begin
  Result := ERR_NO_ERROR;
  pGroupUserData := pData;
  StreamTarget := ATarget;
  __TemplateReadWrite_CodeGroupRights(taRead);
  For i := 0 To pData^.AccessList.Count - 1 Do begin
    pAD := __Get_CodeGroup_Right(PAccessData(pData^.AccessList.Items[i])^.nCodeGroupID);
    If Assigned(pAD) Then
      PAccessData(pData^.AccessList.Items[i])^.nAccess := pAD^.nAccess;
  end;
  __FreeCodeGroupRights;
  pGroupUserData := nil;
  StreamTarget := 0;
end;

function TmafDynamicFuntionTable.__Set_CodeGroup_Rights(ATarget: Integer; pData: PGroupData): Integer;
var i : Integer;
    pAD : PAccessData;
begin
  Result := ERR_NO_ERROR;
  pGroupUserData := pData;
  StreamTarget := ATarget;
  __TemplateReadWrite_CodeGroupRights(taRead); // read the users current rights
  // we copy the data to set into the empty list
  For i := 0 To pData^.AccessList.Count - 1 Do begin
    pAD := __Get_CodeGroup_Right(PAccessData(pData^.AccessList.Items[i])^.nCodeGroupID);
    // we update the ones the user had access to and leave all others untouched
    If Assigned(pAD) Then
      pAD^.nAccess := PAccessData(pData^.AccessList.Items[i])^.nAccess
    Else begin
      New(pAD);
      pAD^.nCodeGroupID := PAccessData(pData^.AccessList.Items[i])^.nCodeGroupID;
      pAD^.nAccess := PAccessData(pData^.AccessList.Items[i])^.nAccess;
      FpCG_Rights.Add(pAD);
    end;
  end;

  // and now we delete all with no access as we safe only a white list
  For i := FpCG_Rights.Count - 1 DownTo 0 Do
    If PAccessData(FpCG_Rights.Items[i])^.nAccess = 13 Then begin
      Dispose(PAccessData(FpCG_Rights.Items[i]));
      FpCG_Rights.Delete(i);
    end;


  __TemplateReadWrite_CodeGroupRights(taWrite);
  __FreeCodeGroupRights;
  pGroupUserData := nil;
  StreamTarget := 0;
end;

// CodeGroupRight data is saved under UserID_X and GroupID_X in the data name
procedure TmafDynamicFuntionTable.__TemplateReadWrite_CodeGroupRights(AAction: TDataStorageAction);
var pToken  : PDataStorageQuery;
begin
  FpTemplateStreamer.Attributes := 1;
  FpTemplateStreamer.StreamVersion := 2;
  FpTemplateStreamer.Stream.Size := 0;
  If pGroupUserData <> nil Then begin
    // if pGroupUserData <> nil then we come from an admin dialog and save or load, otherwise we load the data for DFT compiling
    Case StreamTarget Of
      1 : pToken := __Create_DataStorageQuery(HOOKMANAGER_DATA_CATEGORY, HOOKMANAGER_CG_RIGHTS_USER + IntToStr(pGroupUserData^.nGroupID));
      2 : pToken := __Create_DataStorageQuery(HOOKMANAGER_DATA_CATEGORY, HOOKMANAGER_CG_RIGHTS_GROUP + IntToStr(pGroupUserData^.nGroupID));
      Else Exit;
    end;
  end else
    pToken := __Create_DataStorageQuery(HOOKMANAGER_DATA_CATEGORY, HOOKMANAGER_CG_RIGHTS_USER + IntToStr(TmafHookManager(FpHookManager).BaseDB.ConnectionData.UserID));
  pToken^.aObj := FpTemplateStreamer.Stream;  // we use the internal stream of the TempleateStreamer
  pToken^.Action := AAction;
  Case AAction Of
    taRead  : begin
                TmafHookManager(FpHookManager).DataStorage.Query(pToken);
                If FpTemplateStreamer.Stream.Size = 0 Then begin
                  // the user stream wasn't found, we try the group stream
                  FreePChar(pToken^.sName);
                  StrToPChar(HOOKMANAGER_CG_RIGHTS_GROUP + IntToStr(TmafHookManager(FpHookManager).BaseDB.ConnectionData.GroupID), pToken^.sName);
                  TmafHookManager(FpHookManager).DataStorage.Query(pToken);
                end;  //  --  If ((StreamTarget = 1) And (FpTemplateStreamer.Stream.Size)) Then 
                If FpTemplateStreamer.Stream.Size > 0 Then
                  FpTemplateStreamer.ReadStream;
              end; // taRead
    taWrite : begin
                FpTemplateStreamer.WriteStream(DFT_CG_RIGHTS);
                TmafHookManager(FpHookManager).DataStorage.Query(pToken);
              end; // taWrite
  end;  //  --  Case AAction Of
  __Free_DataStorageQuery(pToken);
end;




{ *********************** Streamer Functions ********************************* }



procedure TmafDynamicFuntionTable.__WriteStreamAttribute(Sender: TObject; ID: Integer);
var i, j : Integer;
    pData : PSubHook;
    pIR   : PInstallRecord;
    B : Byte;
begin
  {$IFDEF Trial}
    if bTamperedWith then
      Exit;
  {$ENDIF}
  Case FpTemplateStreamer.StreamID Of
    MODULE_STREAM_ID : begin
                         FpTemplateStreamer.WriteInteger(FpModules.Count);
                         For i := 0 To FpModules.Count - 1 Do begin
                           FpTemplateStreamer.WriteInteger(TmafModuleManager(FpModules.Items[i]).ModuleID);
                           FpTemplateStreamer.WriteString(ExtractFileName(TmafModuleManager(FpModules.Items[i]).ModuleName));
                         end;
                       end; // MODULE_STREAM_ID
    DFT_STREAM_ID    : begin
                         FpTemplateStreamer.WriteInteger(__GetSubHookCount);
                         FpTemplateStreamer.WriteInteger(FpDFT.Count); // write the Hook count
                         For i := 0 To FpDFT.Count - 1 Do begin
                           FpTemplateStreamer.WriteInteger(PDynamicFunction(FpDFT.Items[i])^.HookID);           // write the HookID for the following SubHooks
                           FpTemplateStreamer.WriteInteger(PDynamicFunction(FpDFT.Items[i])^.FpSubHooks.Count); // write the amount of SubHooks in that particular Hook
                           For j := 0 To PDynamicFunction(FpDFT.Items[i])^.FpSubHooks.Count - 1 Do begin
                             pData := PSubHook(PDynamicFunction(FpDFT.Items[i]).FpSubHooks.Items[j]);
                             FpTemplateStreamer.WriteInteger(pData^.uID);
                             FpTemplateStreamer.WriteInteger(pData^.SubHookID);
                             FpTemplateStreamer.WriteInteger(pData^.nModuleID);
                             FpTemplateStreamer.WriteInteger(pData^.nCodeGroupID);
                             If pData^.bActive Then B := 1
                                               Else B := 0;
                             FpTemplateStreamer.WriteByte(B);
                           end;  //  --  For j := 0 To PDynamicFunction(FpDFT.Items[i]).FpSubHooks.Count - 1 Do
                         end;  //  --  For i := 0 To FpDFT.Count - 1 Do
                       end; // DFT_STREAM_ID
    DFT_HISTORY_ID   : begin
                         FpTemplateStreamer.WriteInteger(__GetHistoryCount);      // count of all history items
                         FpTemplateStreamer.WriteInteger(FpInstallHistory.Count); // write the Hook count
                         For i := 0 To FpInstallHistory.Count - 1 Do begin
                           FpTemplateStreamer.WriteInteger(PInstallHistory(FpInstallHistory.Items[i])^.Hook_uID);           // write the HookID for the following SubHooks
                           FpTemplateStreamer.WriteInteger(PInstallHistory(FpInstallHistory.Items[i])^.FpHistory.Count); // write the amount of SubHooks in that particular Hook
                           For j := 0 To PInstallHistory(FpInstallHistory.Items[i])^.FpHistory.Count - 1 Do begin
                             pIR := PInstallRecord(PInstallHistory(FpInstallHistory.Items[i]).FpHistory.Items[j]);
                             FpTemplateStreamer.WriteInteger(pIR^.HookID);
                             FpTemplateStreamer.WriteInteger(pIR^.SubHookID);
                             FpTemplateStreamer.WriteInteger(pIR^.ModuleID);
                             FpTemplateStreamer.WriteInteger(pIR^.CodeGroupID);
                             FpTemplateStreamer.WriteInteger(pIR^.InstallerModuleID);
                           end;  //  --  For j := 0 To PInstallHistory(FpInstallHistory.Items[i])^.FpHistory.Count - 1 Do
                         end;  //  --  For i := 0 To FpInstallHistory.Count - 1 Do
                       end; // DFT_HISTORY_ID
    DFT_CG_RIGHTS    : begin
                         FpTemplateStreamer.WriteInteger(FpCG_Rights.Count);
                         For i := 0 To FpCG_Rights.Count - 1 Do begin
                           FpTemplateStreamer.WriteInteger(PAccessData(FpCG_Rights.Items[i])^.nCodeGroupID);
                           FpTemplateStreamer.WriteByte(PAccessData(FpCG_Rights.Items[i])^.nAccess);
                         end;
                       end; // DFT_CG_RIGHTS
  end;
end;

procedure TmafDynamicFuntionTable.__ReadStreamAttribute(Sender: TObject; ID: Integer);
var i, j, k, nCount, nID, nCountSubHooks : Integer;
    sModuleName : String;
    pModule : TmafModuleManager;
    pDF : PDynamicFunction;
    pSH : PSubHook;
    pIH : PInstallHistory;
    pIR : PInstallRecord;
    pAD : PAccessData;
    b : Byte;
begin
  {$IFDEF Trial}
    if bTamperedWith then
      Exit;
  {$ENDIF}
  Case FpTemplateStreamer.StreamID Of
    MODULE_STREAM_ID : begin
                         FpTemplateStreamer.ReadInteger(nCount);
                         For i := 0 To nCount - 1 Do begin
                           If Assigned(FOnModuleLoading) Then
                             FOnModuleLoading(TObject(FpHookManager), nCount, i);
                           FpTemplateStreamer.ReadInteger(nID);
                           FpTemplateStreamer.ReadString(sModuleName);
                           pModule := GetModuleManager(nID);
                           If pModule = nil Then begin
                             pModule := TmafModuleManager.Create(FsModulePath + sModulename);
                             pModule.ModuleID := nID;
                             FpModules.Add(pModule);
                           end else begin
                             sModuleName := ExtractFileName(pModule.ModuleName);
                             If Assigned(TmafHookManager(FpHookManager).BeforeLoadingModule) Then
                               TmafHookManager(FpHookManager).BeforeLoadingModule(TObject(FpHookManager), pModule.ModuleID, sModuleName);
                             pModule.ModuleName := FsModulePath + sModuleName;
                           end;
                         end;
                       end;
    DFT_STREAM_ID    : begin
                         __Reset_ExportedFunctionCount; // set in all ModuleManager the ExportedFunction to 0
                         k := 0;
                         FpTemplateStreamer.ReadInteger(nID);     // amount of SubHooks in total
                         FpTemplateStreamer.ReadInteger(nCount);  // Hook Count
                         For i := 1 To nCount Do begin
                           pDF := __CreateDynamicFunctionToken;   // create the DF
                           FpTemplateStreamer.ReadInteger(pDF^.HookID);
                           FpTemplateStreamer.ReadInteger(nCountSubHooks); // SubHook count
                           For j := 1 To nCountSubHooks Do begin
                             Inc(k);
                             If Assigned(FOnDFTLoading) Then
                               FOnDFTLoading(TObject(FpHookManager), nID, k);
                             pSH := __Create_SubHookToken;              // create the SubHook
                             pSH^.HookID := pDF^.HookID;
                             FpTemplateStreamer.ReadInteger(pSH^.uID);
                             FpTemplateStreamer.ReadInteger(pSH^.SubHookID);
                             FpTemplateStreamer.ReadInteger(pSH^.nModuleID);
                             pSH^.pModuleManager := GetModuleManager(pSH^.nModuleID); // assign the ModuleManager
                             // we save in every ModuleManager the biggest Max_uID we can find in our data 
                             If pSH^.uID > pSH^.pModuleManager.FnMax_uID Then
                               pSH^.pModuleManager.Max_uID := pSH^.uID;
                             FpTemplateStreamer.ReadInteger(pSH^.nCodeGroupID);
                             If pSH^.nCodegroupID < 1 Then
                               pSH^.nCodeGroupID := -1;
                             // we add the code group to a list of theoretical codegroups for that user
                             If pSH^.nCodeGroupID > 0 Then
                               If FpTheoreticalCodeGroups.IndexOf(Pointer(pSH^.nCodeGroupID)) = -1 Then
                                 FpTheoreticalCodeGroups.Add(Pointer(pSH^.nCodeGroupID));
                             FpTemplateStreamer.ReadByte(b);
                             pSH^.bActive := (b = 1);
                             pAD := Pointer(1);  // that it is <> nil
                             If Not FbLockdown Then
                               If pSH^.nCodeGroupID > 0 Then
                                 pAD := __Get_CodeGroup_Right(pSH^.nCodeGroupID);
                             // we take the SubHook only, if it's active or we're in lockdown
                             If (((FbLockdown) Or (pSH^.bActive)) And (Assigned(pSH^.pModuleManager)) And (Assigned(pAD))) Then begin
                               // add the SubHook to the DynamicFunction
                               pDF^.FpSubHooks.Add(pSH);
                               // register the SubHook  with the ModuleManager
                                 pSH^.pModuleManager.ExportedFunctions := pSH^.pModuleManager.ExportedFunctions + 1;
                               // and register the SubHook with the CodeGroups
                               If pSH^.nCodeGroupID > -1 Then
                                 __CodeGroup_AddFunction(pSH);
                             end Else
                               Dispose(pSH);
                           end;  //  --  For j := 1 To nCountSubHooks Do
                           // we add the DF only if there are SubHooks registered and loaded
                           If pDF^.FpSubHooks.Count > 0 Then
                             FpDFT.Add(pDF)
                           Else
                             __FreeDynamicFunctionToken(pDF);
                         end;  //  --  For i := 1 To nCount Do
                       end;
    DFT_HISTORY_ID   : begin
                         FpTemplateStreamer.ReadInteger(nID);     // amount of SubHooks in total
                         FpTemplateStreamer.ReadInteger(nCount);  // Hook Count
                         For i := 1 To nCount Do begin
                           pIH := __Create_InstallHistory;
                           FpTemplateStreamer.ReadInteger(pIH^.Hook_uID);
                           FpTemplateStreamer.ReadInteger(nCountSubHooks);
                           For j := 1 To nCountSubHooks Do begin
                             pIR := __Create_InstallRecord;
                             FpTemplateStreamer.ReadInteger(pIR^.HookID);
                             FpTemplateStreamer.ReadInteger(pIR^.SubHookID);
                             FpTemplateStreamer.ReadInteger(pIR^.ModuleID);
                             FpTemplateStreamer.ReadInteger(pIR^.CodeGroupID);
                             FpTemplateStreamer.ReadInteger(pIR^.InstallerModuleID);
                             pIH^.FpHistory.Add(pIR);
                           end;  //  --  For j := 1 To nCountSubHooks Do
                           FpInstallHistory.Add(pIH);
                         end;  //  --  For i := 1 To nCount Do
                       end;
    DFT_CG_RIGHTS    : begin
                         FpTemplateStreamer.ReadInteger(nCount);
                         For i := 1 To nCount Do begin
                           New(pAD);
                           FpTemplateStreamer.ReadInteger(pAD^.nCodeGroupID);
                           FpTemplateStreamer.ReadByte(pAD^.nAccess);
                           FpCG_Rights.Add(pAD);
                         end;
                       end; // DFT_CG_RIGHTS
  end;  //  --  Case FpTemplateStreamer.StreamID Of
end;

initialization
  pGroupUserData := nil;
  StreamTarget := 0;

end.
