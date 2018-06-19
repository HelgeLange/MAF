unit uMAF_HookManager_Helper;

interface

uses Windows, SysUtils, Classes,
     // Modular Application Framework Components units
     uMAF_ModuleManager;

Type RSubHook = packed record
       uID : Integer;           // unique ID for the SubHook
       HookID : Integer;        // the HookID for this function
       SubHookID : Integer;     // the SubHookID to identify the funtion within a module
       nModuleID : Integer;     // real ModuleID
       nCodeGroupID : Integer;  // CodeGroupID for the funtion
//       Flags : Integer;         // Flags for the SubHook (explained below)
       bActive : Boolean;       // is the funtion activated ?
       pModuleManager : TmafModuleManager; // direct access to the ModuleManager hosting the function
     end; // RSubHook
     PSubHook = ^RSubHook;

     RDynamicFunction = packed record
       HookID: Integer;
       FpSubHooks : TList;  // list of PSubHook
     end; // RDynamicFunction
     PDynamicFunction = ^RDynamicFunction;

     RInstallHistory = packed record
       Hook_uID : Integer;               // the uID for this History entry
       FpHistory : TList;                // List of PInstallRecord
     end;
     PInstallHistory = ^RInstallHistory;

     RInstallRecord = packed record
       HookID : Integer;                 // HookID of the previously installed Hook
       SubHookID : Integer;              // SubHookID of the previously installed Hook
       ModuleID : Integer;               // ModuleID of the previously installed Hook
       CodegroupID : Integer;            // CodeGroupID of the previously installed Hook
       InstallerModuleID : Integer;      // the module, that overwrote that Hook and caused it to be in the InstallHistory
     end;
     PInstallRecord = ^RInstallRecord;

     RCodeGroup = packed record
       nCodeGroupID : Integer;
       FpSubHooks : TList;
     end;
     PCodeGroup = ^RCodeGroup;

     RAccessData = packed record
       nCodeGroupID : Integer;
       nAccess : Byte;
     end;
     PAccessData = ^RAccessData;

     RGroupData = packed record
       nGroupID : Integer;
       AccessList : TList;  // List of PAccessData
     end;
     PGroupData = ^RGroupData;

     RSubHookChange = packed record
       nHookID : Integer;
       nSubHookID : Integer;
       bChange : Byte; // 1 = add, 2 = delete
     end;
     PSubHookChange = ^RSubHookChange;

     THookInsertDirection = (hidBefore, hidAfter, hidFirst, hidLast, hidOverwrite);
     TmafInstallAction   = (iaInsert, iaDelete, iaActivate, iaRegisterModule, iaUnregisterModule, iaSetCodeGroup, iaSaveChanges, iaLockDown);

     RmafInstallToken = packed record
       nAction    : TmafInstallAction;
       uID        : Integer;
       nHookID    : Integer;
       nSubHookID : Integer;
       nModuleID  : Integer;
       nCodeGroupID : Integer;       // ID of the code group the function belongs to
       sDescription : PChar;
       InsertDir  : THookInsertDirection;
       nRelative_uID : Integer;      // uID of the SubHook the new SubHook has to put relative to with hidBefore/hidAfter
       nRelativeHook  : Integer;     // SubHookID relative, when InsertDir = hidAfter or hidBefore
       nRelativeModule : Integer;
       bActive : Integer;
       dtTimeStamp : TDateTime;
     end; // RmafInstallToken
     PmafInstallToken = ^RmafInstallToken;

function __Create_SubHookToken: PSubHook;
procedure __Free_SubHookToken(var pToken: PSubHook);
function __CreateDynamicFunctionToken: PDynamicFunction;
procedure __FreeDynamicFunctionToken(var pToken: PDynamicFunction);
function __CreateCodeGroupToken : PCodeGroup;
procedure __FreeCodeGroupToken(var pToken: PCodeGroup);
function __Create_InstallRecord: PInstallRecord;
procedure __Free_InstallRecord(var pToken: PInstallRecord);
function __Create_InstallHistory: PInstallHistory;
procedure __Free_InstallHistory(var pToken: PInstallHistory);
function __Create_InstallToken(AAction: TmafInstallAction): PMAFInstallToken;
procedure __Free_InstallToken(var pToken: PMAFInstallToken);

implementation

uses uMAF_Tools, dialogs;

function __Create_SubHookToken: PSubHook;
begin
  New(Result);
  FillChar(Result^, SizeOf(RSubHook), 0);
//  Result^.bSecurityInit := False; // security isn't initialized yet
  Result^.bActive := True;        // function is active by default
  Result^.uID := -1;              // doesn't have an uID assigned yet
  Result^.nCodeGroupID := -1;     // doesn't belong to a CodeGroup yet
end;

procedure __Free_SubHookToken(var pToken: PSubHook);
begin
  Dispose(pToken);
  pToken := nil;
end;

function __CreateDynamicFunctionToken: PDynamicFunction;
begin
  New(Result);
  Result^.HookID := 0;
  Result^.FpSubHooks := TList.Create;
end;

procedure __FreeDynamicFunctionToken(var pToken: PDynamicFunction);
var pSubHookToken : PSubHook;
    i : Integer;
begin
  For i := 0 To pToken^.FpSubHooks.Count - 1 Do begin
    pSubHookToken := pToken^.FpSubHooks.Items[i];
    __Free_SubHookToken(pSubHookToken);
  end;  //  --  For i := 0 To pToken^.FpSubHooks.Count - 1 Do
  pToken^.FpSubHooks.Clear;
  pToken^.FpSubHooks.Free;
  Dispose(pToken);
  pToken := nil;
end;

function __CreateCodeGroupToken : PCodeGroup;
begin
  New(Result);
  Result^.nCodeGroupID := -1;
  Result^.FpSubHooks := TList.Create;
end;

procedure __FreeCodeGroupToken(var pToken: PCodeGroup);
begin
  If pToken = nil Then
    Exit;
  pToken^.FpSubHooks.Clear;
  pToken^.FpSubHooks.Free;
  Dispose(pToken);
  pToken := nil;
end;

function __Create_InstallRecord: PInstallRecord;
begin
  New(Result);
  FillChar(Result^, SizeOf(RInstallRecord), 0);
end;

procedure __Free_InstallRecord(var pToken: PInstallRecord);
begin
  Dispose(pToken);
  pToken := nil;
end;

function __Create_InstallHistory: PInstallHistory;
begin
  New(Result);
  Result^.Hook_uID := 0;
  Result^.FpHistory := TList.Create;
end;

procedure __Free_InstallHistory(var pToken: PInstallHistory);
var pIR : PInstallRecord;
begin
  While pToken^.FpHistory.Count > 0 Do begin
    pIR := pToken^.FpHistory.Items[0];
    __Free_InstallRecord(pIR);
    pToken^.FpHistory.Delete(0);
  end;  //  --  While pToken^.FpHistory.Count > 0 Do
  pToken^.FpHistory.Free;
  Dispose(pToken);
  pToken := nil;
end;

function __Create_InstallToken(AAction: TmafInstallAction): PMAFInstallToken;
begin
  New(Result);
  FillChar(Result^, SizeOf(RMAFInstallToken), 0);
  Result^.nAction := AAction;
  Result^.InsertDir := hidLast;
end;

procedure __Free_InstallToken(var pToken: PMAFInstallToken);
begin
  If Assigned(pToken) Then begin
    If pToken^.sDescription <> nil Then
      FreePChar(pToken^.sDescription);
    Dispose(pToken);
    pToken := nil;
  end;  //  --  If Assigned(pToken) Then 
end;

end.
