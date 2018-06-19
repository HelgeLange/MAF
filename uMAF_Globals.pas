{*******************************************************************************
Name         : uMAF_Globals.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007-2011 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 28.02.2007
Last Update  : 03.11.2011
Version      : 1.0.007
Purpose      : global constants and variables for MAF components
Last Changes :

1.0.007 (03.11.2011) -----------------------------------------------------------
- [ADD] added ErrorCode ERR_STREAM_EMPTY (142)
1.0.006 (21.09.2009) -----------------------------------------------------------
- [ADD] added flag mfNoUnistall in the ModuleOptions
1.0.005 (31.08.2009) -----------------------------------------------------------
- [CHG] moved the RHookInstallToken definition to uMAFHookManager_Helper.pas as
        it is needed only in limited locations
- [CHG] moved the definition for TOnRightChange event to uSecurityLayer.pas as
        it is used only there
1.0.004 (18.07.2009) -----------------------------------------------------------
- [CHG] QueryHandlerStruct got a new member CallerComponent wich contains ALWAYS
        the calling TmafHookClient. Therefore also the 2 members FBeforeExecHook
        and FAfterExecHook were removed, as the TmafHookManager can now access
        the events directly in the calling HookClient
- [CHG] TExecuteHook now uses a pointer of the pQHS type for the QHS parameter
        instead of pointer without type. That means, all existing code using this
        event has to be changed, but in the future now type conversation will be
        necessary anymore
1.0.003 (20.06.2009) -----------------------------------------------------------
- [ADD] QueryHandlerStruct got a new member pFreeMemFunc wich is used internally
        to free memory allocated in another module than the callers (memory
        should always be freed in the module where it was allocated!). It will
        be called automaticalle by TmafHookClient and TmafHookManager
- [DEL] QueryHandlerStruct lost the member "Param" (type ShortString) as it was
        obsolete and shouldn't be used in Delphi 2009+ anyway
1.0.002 (13.06.2009) -----------------------------------------------------------
- [ADD] MAN_REGISTER_SELF and MAN_UNREGISTER_SELF constants added to support
        the new (Un)RegisterSelf functions of the ERPBaseComponent
1.0.002 (13.10.2008) -----------------------------------------------------------
- [ADD] TGenericDBQuery class for visual components
1.0.001 (28.02.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_Globals;

interface

uses Windows, Classes, Messages, SysUtils;

Type // for global variables
     TLoadSave = (lsINIFile, lsDatabase, lsRegistry, lsComponent, lsDefault, lsNone);


     RFunctionObserver = packed record
       nHookID : Integer;
       nSubHookID : Integer;
       nModuleID : Integer;
       pCallBack : TComponent;
     end;
     PFunctionObserver = ^RFunctionObserver;

     RCodeGroupData = packed record
       nID : Integer;          // identifier
       Name : String;          // name a plain text
       Desc : String;          // description as plain text
       NameResID : Cardinal;   // ResID for public use with localization
       DescResID : Cardinal;   // ResID for public use with localization
       MinSL     : Integer;    // minimum security level to to be able to give rights on that group
     end; // RCodeGroupData
     PCodeGroupData = ^RCodeGroupData;

     RmafMediaItem = packed record
       MediaID   : Cardinal; // ID of the media
       MediaType : Byte;     // contains emtXXXX constant
     end;
     PmafMediaItem = ^RmafMediaItem;


     // mfNoUninstall    = Module cannot be uninstalled and any attempt will be refused by HookManager
     // mfManagerModule  = Module contains a manager component and won't be unloaded by HookManager but by the ManagerContainer
     // mfStartParams    = Manager module accepts start parameter from TmafManagerLoader
     TModuleFlag = (mfNoUninstall, mfManagerModule, mfStartParams);
     TModuleFlags = Set Of TModuleFlag;

     RAccessRightStreamData = packed record
       ID         : Integer;
       AccessType : Byte;
     end; // RAccessRightStreamData
     PAccessRightStreamData = ^RAccessRightStreamData;

Type TVarFlag = (vfSave, vfReadOnly);
     TVarFlags = Set Of TVarFlag;

     TGlobalVarNotify = procedure(Sender: TObject; VarName: String) of object;
     TOnMAFError = procedure(Sender: TObject; ErrorMsg: String) Of Object;

     RCallBackRegister = packed record
       VarName : PChar;
       AHookClient : TObject;
     end;
     PCallBackRegister = ^RCallBackRegister;

     TGenericDBQuery = class(TPersistent)
     private
       FnID : Integer;
       FnID_Type : Integer;
       FsValue : String;
       FsTBL_ID : String;
       FsTBL_Data_Field : String;
       FsTBL_Table_Name : String;
     published
       property ID : Integer read FnID write FnID;
       property ID_Type : Integer read FnID_Type write FnID_Type;
       property Value : String read FsValue write FsValue;
       property TBL_ID : String read FsTBL_ID write FsTBL_ID;
       property TBL_Data_Field : String read FsTBL_Data_Field write FsTBL_Data_Field;
       property TBL_Table_Name : String read FsTBL_Table_Name write FsTBL_Table_Name;
     end; // TGenericDBQuery

     RGenericDBRecord = packed record
       ID : Integer;
       ID_Type : Integer;
       Value : PChar;
       TBL_ID : PChar;
       TBL_Data_Field : PChar;
       TBL_Table_Name : PChar;
     end; // RGenericDBRecord
     PGenericDBRecord = ^RGenericDBRecord;

     RProgramParam = packed record
       Tag : Array [1..2] Of Char;   // i.e. '-p'
       Parameter : String;            // everything behind the Tag
     end;
     PProgramParam = ^RProgramParam;

     RUserDataRec = packed record
       ID : Integer;
       GroupID : Integer;
       Login : String;
       Password : String;
       Password2 : String;
       FirstName : String;
       LastName : String;
       Flags : Integer;
       SL    : Byte;
     end;
     PUserDataRec = ^RUserDataRec;

     TGroupFlag = (gfVisibleToLowerSL);
     TGroupFlags = Set Of TGroupFlag;

     RGroupDataRec = packed record
       GroupID : Integer;
       GroupName : String;
       Flags : TGroupFlags;
       SL : Byte;
     end;
     PGroupDataRec = ^RGroupDataRec;

     RResQueryStruct = packed record
       nCommand : Integer;
       ResID    : Cardinal;
       pResult  : Pointer;
       ResName  : PChar;
     end; // RResQueryStruct
     PResQueryStruct = ^RResQueryStruct;

     TLogType = (ltError, ltInfo, ltWarning);

     RLogMessage = packed record
       aType : TLogType;
       Msg   : PChar;
     end; // RLogMessage
     PLogMessage = ^RLogMessage;

const MODULE_SERVICE_PROC  = '__ModuleServiceProc';
      MANAGER_SERVICE_PROC = '__ManagerModuleServiceProc';
      MODULE_INIT_PROC     = '__ModuleInitServiceProc';

      // Manager type consts... MUST be unique within the system
      MT_ROUTER             = 13021974;
      MT_LINK_MANAGER       = MT_ROUTER + 1;
      MT_WINDOW_MANAGER     = MT_LINK_MANAGER + 1;
      MT_RESOURCE_MANAGER   = MT_WINDOW_MANAGER + 1;
      MT_GLOBAL_VAR_MANAGER = MT_RESOURCE_MANAGER + 1;
      MT_TEMPLATE_MANAGER   = MT_GLOBAL_VAR_MANAGER + 1;
      MT_USER_SECURITY      = MT_TEMPLATE_MANAGER + 1;

      OCM__BASE                   = WM_USER + $1C00;
      MSG_GET_MANAGER_DATA        = OCM__BASE + 1;
      MSG_MANAGERTYPE             = MSG_GET_MANAGER_DATA + 1;
      MSG_SET_FORM_DATA           = MSG_MANAGERTYPE + 1;
      MSG_FORM_INITIALIZED        = MSG_SET_FORM_DATA + 1;
      LC_TEXT_MESSAGE             = MSG_FORM_INITIALIZED + 1;        // TLinkServer/Client
      LC_DATA_MESSAGE             = LC_TEXT_MESSAGE + 1;
      MSG_FUNCTION_OBSERVER_ADD   = LC_DATA_MESSAGE + 1;             // FunctionObserver notify
      MSG_FUNCTION_OBSERVER_DEL   = MSG_FUNCTION_OBSERVER_ADD + 1;
      WM_SET_NUMBER               = MSG_FUNCTION_OBSERVER_DEL + 1;
      MSG_GLOBALVAR_CHANGE        = WM_SET_NUMBER + 1;
      WM_CAN_CLOSE                = MSG_GLOBALVAR_CHANGE + 1;
      WM_SAVE_DATA                = WM_CAN_CLOSE + 1;
      WM_LANGUAGE_CHANGE          = WM_SAVE_DATA + 1;
      WM_LANGUAGE_CHANGED         = WM_LANGUAGE_CHANGE + 1;
      MSG_DFT_CHANGE              = WM_LANGUAGE_CHANGED + 1;
      MSG_BEFORE_FORM_CLOSE       = MSG_DFT_CHANGE + 1;
      WM_ICON_SKIN_CHANGE         = MSG_BEFORE_FORM_CLOSE + 1;
      WM_MEDIA_RESOURCE_CHANGE    = WM_ICON_SKIN_CHANGE + 1;
      WM_MEDIA_RESOURCE_CHANGED   = WM_MEDIA_RESOURCE_CHANGE + 1;
      WM_MANAGER_CLOSE            = WM_MEDIA_RESOURCE_CHANGED + 1;
      WM_START_PARAMS             = WM_MANAGER_CLOSE + 1;
      WM_MODULE_CLOSE             = WM_START_PARAMS + 1;
      WM_INIT_DONE                = WM_MODULE_CLOSE + 1;
      WM_SUBHOOK_CLOSE            = WM_INIT_DONE + 1;
      WM_REGISTER_NOTIFY          = WM_SUBHOOK_CLOSE + 1;
      MSG_ON_IDLE                 = WM_REGISTER_NOTIFY + 1;

      // Manager standard communication routines
      MAN_CREATE_MANAGER        = 01; // call to create the needed objects inside the the Manager
      MAN_CLOSE_MANAGER         = 02; // info that the manager is about to be closed
      MAN_CLOSE_COMPONENT       = 04; // inform the manager to shutdown
      MAN_GET_MANAGER_TYPE      = 09; // returns the MT_XXX constant
      MAN_REGISTER_SELF         = 10; // registers a client with its Manager component
      MAN_UNREGISTER_SELF       = 11; // unregisters a client from its Manager component

      // standard message constants for modules
      HM_CREATE                 = 100;  // tells the module to create its stuff
      HM_CLOSE                  = 101;  // tells the module, that it will be shut down
//      HM_GET_HAC_COUNT          = 102;  // aks the module, how many TERPHookAccessContainer it has
//      HM_GET_HAC                = 103;  // asks for a TERPHookAccessContainer
      HM_GET_HM_STATISTIC       = 104;  // asks the HM for the statistic object (defined in uMAF_HookManager.pas)
      HM_GET_SECURITY_LEVEL     = 105;  // asks fr the current security level
//      HM_QUERY                  = 106;  // TmafHookManager Query
      HM_USER_SECURITY          = 107;  // TmafUserSecurity query
      HM_LINKMANAGER_QUERY      = 108;  // query the Linkmanager API
      HM_WINDOWMANAGER_QUERY    = 109;
      HM_RESOURCEMANAGER_QUERY  = 110;  // query to the TmafResourceManager

      // HookManager private constants HM_XXXX
      HM_GLOBAL_VAR_QUERY           = 03; // access to global variables inside the Router Manager
      HM_INIT_MODULE                = 05; // Inits a module in design time
      HM_EXEC_HOOK                  = 06; // executes a hook
      HM_GET_HOOK_TOKEN             = 07; // returns a PDynamicFunction token
      HM_GET_HOOKMANAGER            = 08; // HM returns "self"
      MAN_INIT_DONE                 = 12; // the TmafManagerLoader loaded all manager and sends MAN_INIT_DONE
      HM_INSTALL_API                = 13; // calls the InstallAPI
      HM_SAVE_CG_RIGHTS             = 14;
      HM_LOAD_CG_RIGHTS             = 15; 
      HM_ADD_FUNCTION_OBSERVER      = 16; // installs a function observer
      HM_DELETE_FUNCTION_OBSERVER   = 17; // removes a function observer
      HM_LOAD_CG_DESCRIPTIONS       = 18; // returns a list of codegroup description for public use (admin panel)
      HM_DELETE_CG_RIGHTS           = 19; // deletes the right for a group or user from the DataStorage
      HM_GET_HOOK_ACTIVE_STATUS     = 20; // reads if a hook is active or not
      HM_GET_SESSION_ID             = 21; // returns the current SessionID
      MAN_STATUS                    = 22; // returns the status of a manager components
      HM_APPLICATION_IDLE           = 23; // Application.OnIdle is send to the TmafHookManager
      HM_TOGGLE_APPLICATION_IDLE    = 24; // attaches or detaches the client on the Application.OnIdle event handler
      HM_GET_SUBHOOK_LIST           = 27; // returns a list of SubHooks assinged to a Hook
      HM_DS_GET_CATEGORIES          = 28; // returns the list of categories in the DataStorage
      HM_DS_GET_NAMES_IN_CATEGORY   = 29; // returns the list of names in a category
      HM_DELETE_TEMPLATE            = 30; // deletes a template
      HM_LOG_MESSAGE                = 36; // logs a message through the Router
      HM_LOAD_TEMPLATE              = 37; // will load a template
      HM_SAVE_TEMPLATE              = 38; // will save a template
      HM_GET_TEMPLATE_ID            = 39; // gets the ID of a template referenced by category + name
      HM_GET_GLOBALVAR_MANAGER      = 40; // GlobalVar Manager returns "self"

      // SQL component support
      HM_CREATE_DATASET         = 41;
      HM_DESTROY_DATASET        = 42;
      HM_SET_SQL                = 43;
      HM_SET_SQL_RESOURCE_ID    = 45;
      HM_SET_SQL_PARAMS         = 46;
      HM_READ_BLOB              = 47;
      HM_WRITE_BLOB             = 48;

      MOD_INSTALL_MODULE         = 60; // tells the TERPInstallAPI to install itself
      MOD_DEINSTALL_MODULE       = 61; // tells the ERPInstallAPI to deinstall themself

      // User security functions
      US_GET_USERLIST           = 85;  // returns user list
      US_CREATE_USER            = 86;  // create a new user
      US_DELETE_USER            = 87;  // deletes an existing user
      US_MODIFY_USER            = 88;  // modifies an existing user
      US_GET_GROUPLIST          = 89;  // returns group list
      US_GET_USER_DATA          = 90;  // returns the saved data for a user
      US_CREATE_GROUP           = 91;  // creates a user group
      US_UPDATE_GROUP           = 92;  // changes a group name
      US_DELETE_GROUP           = 93;  // deletes a group
      US_GET_GROUP_DATA         = 94;  // returns the group data

      // ResourceManager private constants RM_XXXX
      RM_GET_FILE_STREAM     = 1300;
      RM_RELEASE_FILE_STREAM = 1301;
      RM_GET_GRAPHIC         = 1302;
      RM_RELEASE_GRAPHIC     = 1303;
      RM_GET_STRING          = 1304;
      RM_GET_SQL             = 1305;
      RM_GET_LANGUAGE_LIST   = 1306;
      RM_GET_ACTIVE_LANGUAGE = 1307;
      RM_SET_ACTIVE_LANGUAGE = 1308;
      RM_GET_ICON_SKIN       = 1309;
      RM_SET_ICON_SKIN       = 1310;
      RM_GET_SKIN_LIST       = 1311;
      RM_GET_FILERES_NAME    = 1312;  // must go through HM_RESOURCEMANAGER_QUERY
      RM_GET_STRINGRES_NAME  = 1313;  // must go through HM_RESOURCEMANAGER_QUERY
      RM_GET_SQLRES_NAME     = 1314;  // must go through HM_RESOURCEMANAGER_QUERY

      // GlobalVarManager private consts GV_XXXX
      GV_SET_INTEGER         =  301;
      GV_GET_INTEGER         =  302;
      GV_DELETE_VAR          =  303;
      GV_SET_STRING          =  304;
      GV_GET_STRING          =  305;
      GV_SET_OBJECT          =  306;
      GV_GET_OBJECT          =  307;
      GV_SET_BOOLEAN         =  308;
      GV_GET_BOOLEAN         =  309;
      GV_REGISTER_CALLBACK   =  310;
      GV_UNREGISTER_CALLBACK =  311;

      // LinkManager private constants LM_XXXX
      LM_ADD_LINK_CLIENT        = 1500;
      LM_DEL_LINK_CLIENT        = 1501;
      LM_GET_LINK_LIST          = 1503;
      LM_ADD_LINK_ID            = 1504;
      LM_DEL_LINK_ID            = 1505;
      LM_SEND_TEXT              = 1506;
      LM_SEND_DATA              = 1507;
      LM_REQUEST_CHANNEL_ID     = 1508;

      // WindowManager private constants WM__XXXX
      WM__ADD_WINDOW           = 200;   // adds the current client to the WindowManager
      WM__REMOVE_WINDOW        = 201;   // removes the current client from the WindowManager
      WM__SHOW_LIST            = 202;
      WM__REMOVE_CLIENTS       = 203;   // removes the clients
      WM__GET_CLIENT_COUNT     = 204;   // returns, how many client windows the current window has
      WM__DIRTY                = 205;   // a window gets dirty
      WM__MSG_TO_PARENT        = 206;   // sends a msg to its parent window
      WM__GET_WINDOW_NO        = 207;   // requests an window number for the caption
      WM__GET_WINDOWINFO       = 208;   // requests window flags from WindowController
      WM__FIND_WINDOW_ID       = 209;   // requests to find a window with a certain FormID
      HM_GET_WINDOWMANAGER     = 210;   // WindowManager returns "Self"
      WM__FIND_WINDOW_BY_HOOK  = 211;   // finds a window by its given HookID/SubHookID
      WM__SET_MAINFORM         = 212;   // sets the actual main window
      WM__GET_MAINFORM         = 213;   // returns the actual mainform
      WM__GET_WINDOWMANAGER    = 214;   // returns the TmafWindowManager object
      WM__REGISTER_FORM_ID     = 215;   // registers a Hook/SubHook pair to a FormID
      WM__UNREGISTER_FORM_ID   = 216;
      WM__GET_FORM_ID_DATA     = 217;   // returns HookID/SubHookID registered for a FormID

      // constants for internal use in TmafPageControl and TmafWizardPanel
      RES_GetDescription         = 50; // returns window description only
      RES_FreeWindow             = 51; // release the window
      RES_GetWindow              = 53; // creates the window and returns its pointer in pQHS^.pChildObj

{-------------------------------------------------------------------------------

                       MAF ERROR CONSTANTS

-------------------------------------------------------------------------------}
      ERR_NO_ERROR               = 100;
      ERR_LOAD_LIBRARY_FAILED    = 101;
      ERR_SERVICE_PROC_FAILED    = 102;
      ERR_NOTHING_TO_DO          = 103;
      ERR_DLL_NOT_LOADED         = 104;
      ERR_HOOKCLIENT_NO_ROUTER   = 105;
      ERR_NO_MODULE_CONTROLLER   = 106;
      ERR_NO_DB                  = 107;
      ERR_LOG_FAILURE            = 108;
      ERR_PARAM_FAILURE          = 109;
      ERR_SUBHOOK_UID_EXISTS     = 110;  // occurs when trying to install a SubHook with an uID that exists already and hidOverwrite isn't set
      ERR_DS_NOT_FOUND           = 111;
      ERR_UNKNOWN_ERROR          = 112;
      ERR_DS_SAVE_ERROR          = 113;
      ERR_MODULE_NOT_INITIALIZED = 114;
      ERR_SUBHOOK_NOT_FOUND      = 115;
      ERR_SQL_STRING_NOT_FOUND   = 116;
      ERR_CANCEL_CLOSE           = 117;
      ERR_NO_SQL_DATA            = 118;
      ERR_NO_DB_SPECIFIED        = 119;
      ERR_NO_USER_SPECIFIED      = 120;
      ERR_NO_PASSWORD_SPECIFIED  = 121;
      ERR_SQL_RESOURCE_MISSING   = 122;
      ERR_SQL_UNKNOWN            = 123;
      ERR_RIGHT_TOKEN_ID_EXIST   = 124;
      ERR_RIGHT_TOKEN_NOT_EXIST  = 125;
      ERR_GLOBAL_VAR_NOT_EXIST   = 126;
      ERR_MODULE_UNKNOWN         = 127;
      ERR_MODULE_EXISTS          = 128;
      ERR_TEMPLATE_READ_ERROR    = 129;
      ERR_TEMPLATE_WRITE_ERROR   = 130;
      ERR_TEMPLATE_NOT_FOUND     = 131;
      ERR_USER_ACTION            = 132;  // error id an user action (insert, delete or modify) isn't allowed 
      ERR_COMPONENT_SETUP_FAILURE = 133;
      ERR_HM_NOT_IN_LOCKDOWN     = 134;
      ERR_MC_FORMTYPE_UNKNOWN    = 135;
      ERR_MC_FORM_CREATION       = 136;
      ERR_MC_FORM_INIT           = 137;
      ERR_RESOURCEFILE_CLOSED    = 138;
      ERR_RESOURCE_NOT_FOUND     = 139;
      ERR_SKINS_DISABLED         = 140;
      ERR_USER_NOT_AUTHORIZED    = 141;
      ERR_STREAM_EMPTY           = 142;

// StreamDataTypes for TTemplateStreamer and mafModuleControllerToken
const sdtString          = 01;
      sdtInteger         = 02;
      sdtPAnsiChar       = 03;
      sdtPWideChar       = 04;
      sdtDateTime        = 05;
      sdtReal            = 06;
      sdtByte            = 07;
      sdtWord            = 08;
      sdtInt64           = 09;
      sdtFloat           = 10;
      sdtShortString     = 11;
      sdtCardinal        = 12;
      sdtWideString      = 13;
      sdtBoolean         = 14;
      sdtMediaItem       = 15;
      sdtPointer         = 16; // although a pointer cannot really be saved
      sdtObject          = 17; // same here
      sdtObjectFunction  = 18; // same here, is to define a callback into an object (like events), which uses 8 byte

      // ResourceManager resource type constants
      emtTextFile = 01;
      emtPicture  = 02;
      emtVideo    = 03;
      emtString   = 04;
      emtSQL      = 05;


const MAX_SQL_SIZE = 2000;           // max memory reserved for a SQL query in TRouterDataSetConnector

var pOldApplication : Pointer;
    bRunMode : Boolean;

function __Create_GenericDBRecord: PGenericDBRecord;
procedure __Free_GenericDBRecord(var pData: PGenericDBRecord);

procedure CopyGenericDBRecordClassToRecord(var pData: PGenericDBRecord; aClass: TGenericDBQuery);
function NewProgramParam: PProgramParam;
function NewLogMessage: PLogMessage;
procedure FreeLogMessage(var pData: PLogMessage);

function __Create_CodeGroupDataToken: PCodeGroupData;

function NewResQueryStruct: PResQueryStruct;

implementation

uses uMAF_Tools;

function __Create_GenericDBRecord: PGenericDBRecord;
begin
  New(Result);
  FillChar(Result^, SizeOf(RGenericDBRecord), 0);
  Result^.ID_Type := -1;  // not defined
end;

procedure __Free_GenericDBRecord(var pData: PGenericDBRecord);
begin
  If pData = nil Then
    Exit;
  If pData^.Value <> nil Then
    FreePChar(pData^.Value);
  If pData^.TBL_ID <> nil Then
    FreePChar(pData^.TBL_ID);
  If pData^.TBL_Data_Field <> nil Then
    FreePChar(pData^.TBL_Data_Field);
  If pData^.TBL_Table_Name <> nil Then
    FreePChar(pData^.TBL_Table_Name);
  Dispose(pData);
  pData := nil;
end;

procedure CopyGenericDBRecordClassToRecord(var pData: PGenericDBRecord; aClass: TGenericDBQuery);
begin
  If aClass = nil Then
    Exit;
  If pData = nil Then
    pData := __Create_GenericDBRecord;

  pData^.ID := aClass.ID;
  pData^.ID_Type := aClass.ID_Type;
  StrToPChar(aClass.Value, pData^.Value);
  StrToPChar(aClass.TBL_ID, pData^.TBL_ID);
  StrToPChar(aClass.TBL_Data_Field, pData^.TBL_Data_Field);
  StrToPChar(aClass.TBL_Table_Name, pData^.TBL_Table_Name);
end;

function NewProgramParam: PProgramParam;
begin
  New(Result);
  FillChar(Result^, SizeOf(RProgramParam), 0);
end;

function NewLogMessage: PLogMessage;
begin
  New(Result);
  FillChar(Result^, SizeOf(RLogMessage), 0);
  Result^.aType := ltError;
end;

procedure FreeLogMessage(var pData: PLogMessage);
begin
  If pData^.Msg <> nil Then
    FreeMem(pData^.Msg, StrLen(pData^.Msg) + SizeOf(Char));
  Dispose(pData);
  pData := nil;
end;

function __Create_CodeGroupDataToken: PCodeGroupData;
begin
  New(Result);
  Result^.nID := -1;
  Result^.Name := '';
  Result^.Desc := '';
  Result^.NameResID := 0;
  Result^.DescResID := 0;
  Result^.MinSL := 1;
end;

function NewResQueryStruct: PResQueryStruct;
begin
  New(Result);
  FillChar(Result^, SizeOf(RResQueryStruct), 0);
end; // NewResQueryStruct

initialization
  bRunMode := False;

end.
