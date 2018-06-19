{*******************************************************************************
Name         : uMAF_Core.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2009-2011 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 24.07.2009
Last Update  : 10.11.2011
Version      : 1.0.008
Purpose      : base components for both manager and client components, that
               implement already basic needs to work within the MAF system
Last Changes :

1.0.008 (10.11.2010)------------------------------------------------------------
- [ADD] global variable gManagerLoader that contains in runtime the pointer
        to the TmafManagerLoader object (which is and should be unique in every
        application).
        TmafBaseComponent.__GetManagerBaseContainer now uses the variable
        gManagerLoader to find the ManagerLoader faster. Only if this variable
        is nil, the "old" method will be used
1.0.007 (11.09.2010)------------------------------------------------------------
- [ADD] TmafParameter can now store an additional information if it's an in- or
        out-parameter. This does not have any affect on the variable, but can
        be used to determine in the called code, how to handle different
        variables dynamically
1.0.006 (30.08.2010)------------------------------------------------------------
- [ADD] changed the basic way to accept messages for the manager, doesn't use an
        own constant anymore (like HM_QUERY for TmafHookManager), but uses
        instead the MT_XXXX constant for the manager (MT_ROUTER for
        TmafHookManager)
- [ADD] TmafParameter can now save stored variables to the dfm file
1.0.005 (28.07.2010)------------------------------------------------------------
- [ADD] new object TmafParameter that supports named and indexed parameters
        TQueryHandlerStruct.pParams now uses this type to pass on the parameters
- [ADD] the Parameter with their values can now be stored to a stream and loaded
        from a stream again. Obviously it wont work with all types (i.e. Pointer,
        objects)
1.0.004 (02.06.2010)------------------------------------------------------------
- [CHG] moved TVariableStack from uMAF_HookClient.pas to uMAF_Core.pas
- [ADD] record TQueryHandlerStruct (QHS) now contains a new member "pParams"
        which can contain variables send with any call to a dynamic function
1.0.003 (16.03.2010)------------------------------------------------------------
- [ADD] added MAN_STATUS message response to inform the manager loader on
        request, that the manager component in question is indeed running
- [ADD] TQueryHandlerStruct now delivers the Update-Transaction as well
1.0.002 (22.11.2009)------------------------------------------------------------
- [ADD] added message handler for WM_INIT_DONE that client components in general
        can receive the message and register itself, if it didn't work in the
        Loaded method. That happened when a client component was placed on the
        TDataModule of a manager module that is automatically created when the
        module is loaded by the TmafManagerLoader during the start-up process.
        At this point the manager of the client components were not available yet
1.0.001 (28.09.2009)------------------------------------------------------------
- [ADD] added a message to all registered client components (the ones that are
        still active in the program) that the manager is closing, so that they
        can either do still something or at least know, that there is no need to
        call from now on. The message is already received by the TmafBaseComponent
1.0.000 (24.07.2009)------------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_Core;

interface

{$I MAFramework.inc}

uses Windows, Classes, Messages, Variants, Dialogs,
     {$IFDEF D16+}
      System.SysUtils,
     {$ELSE}
      SysUtils,
     {$ENDIF}
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_Tools, uMAF_Parameters;

Type TManagerOption = (moAutoRegister, moRegisterInitDone);
     TManagerOptions = Set of TManagerOption;

     TResourceFileType = (rftUnknown, rftMedia, rftString, rftSQL, rftFileDB);

     TEncodingType = (etANSI, etWideString);

     // hcoProcessOnIdle      = HookCLient gets OnIdle event calls from TmafHookManager
     // hcoClearVariableStack = HookClient clears the parameter stack before leaving "ExecuteHook", "ExecuteUniqueID"
     THookClientOption = (hcoProcessOnIdle, hcoClearParamStack);
     THookClientOptions = Set Of THookClientOption;

     TFreeMemFunc = procedure(pData: Pointer) Of Object;

     TFunctionObserverAction = (foaAdd, foaDelete);
     TFunctionObserverCallBack = procedure(Sender: TObject; nHookID, nSubHookID : Integer; Action: TFunctionObserverAction) Of Object;

     // msLoading    = the maf component loads something (a window for example)
     // msUnloading  = the maf component unloads something
     // msDestroying = the maf component is destroying itself
     // msQuerying   = the maf component is querying the DynamicFunction
     // msListing    = the maf component lists data from a DynamicFunction
     TmafState = (msLoading, msUnloading, msDestroying, msQuerying, msListing);
     TmafStates = Set Of TmafState;

     TVarType = (vtString, vtInteger, vtPointer, vtObject, vtBoolean, vtDateTime, vtCallback);

     // dwFlags : Bit  0 : 1 = save; 0 = not save
     RVarRec = packed record
       Name     : PChar;
       Typ      : TVarType;
       Var1     : Pointer;     // PChar or just an Integer, depends on type
       dwFlags  : TVarFlags;
       LoadSave : TLoadSave;
       CallBackList : TList;   // List of HookClients to inform
     end; // RVarRec
     PVarRec = ^RVarRec;

     // TVarControllerOptions controls when the component reads and writes
     // variables to and from its destination
     //
     // vcfSaveOnClose    = Variables will be written, when the component is
     //                     being destroyed, otherwise when the variable changes
     // vcfReadOnLoaded   = Variable will be read, when the loaded method is
     //                     called, otherwise when the variable is requested
     TVarControllerOption = (vcfSaveOnClose, vcfReadOnLoaded);
     TVarControllerOptions = Set Of TVarControllerOption;



Type TVarStackToken = packed record
       FType  : TVarType;
       pToken : Pointer;
     end; // TVarStackToken
     PVarStackToken = ^TVarStackToken;

     TmafVariable = class(TObject)
     private
       FsName : String;
       FType  : TVarType;
       pToken : Pointer;
       FIndex : Integer;
       FCallback : TNotifyEvent;
       FVarParamType : TVarParamType;
       procedure __SetAsString(const Value: String);
       function __GetAsString: String;
       function __GetAsInteger: Integer;
       procedure __SetAsInteger(const Value: Integer);
       function __GetAsPointer: Pointer;
       procedure __SetAsPointer(const Value: Pointer);
       function __GetAsObject: TObject;
       procedure __SetAsObjects(const Value: TObject);
       function __GetValue: Variant;
       procedure __SetValue(const Value: Variant);
       function __GetAsDateTime: TDateTime;
       procedure __SetAsDateTime(const Value: TDateTime);
       function __GetBoolean: Boolean;
       procedure __SetBoolean(const Value: Boolean);
       function __GetAsCallback: TNotifyEvent;
       procedure __SetAsCallback(const Value: TNotifyEvent);
     public
       constructor Create;
       destructor Destroy; override;
       property VarName : String read FsName;
       property VarType : TVarType read FType;
       property ParamType : TVarParamType read FVarParamType write FVarParamType;
       property AsString : String read __GetAsString write __SetAsString;
       property AsInteger : Integer read __GetAsInteger write __SetAsInteger;
       property AsPointer : Pointer read __GetAsPointer write __SetAsPointer;
       property AsObject : TObject read __GetAsObject write __SetAsObjects;
       property AsDateTime : TDateTime read __GetAsDateTime write __SetAsDateTime;
       property AsBoolean : Boolean read __GetBoolean write __SetBoolean;
       property AsCallback : TNotifyEvent read __GetAsCallback write __SetAsCallback;
       property Value : Variant read __GetValue write __SetValue;
       property Index: Integer read FIndex;
     end;

     TVariableStack = class(TObject)
     private
       FpStack : TList;
       function __GetInteger: Integer;
       procedure __SetInteger(const Value: Integer);
       function __GetString: String;
       procedure __SetString(const Value: String);
       function __GetPointer: Pointer;
       procedure __SetPointer(const Value: Pointer);
       function __GetObject: TObject;
       procedure __SetObject(const Value: TObject);
       function __GetCount: Integer;
     protected
     public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;

       property Count : Integer read __GetCount;
       property Stack : TList read FpStack;
       property AsInteger : Integer read __GetInteger write __SetInteger;
       property AsString : String read __GetString write __SetString;
       property AsPointer : Pointer read __GetPointer write __SetPointer;
       property AsObject : TObject read __GetObject write __SetObject;
     end; // TVariableStack

     TmafParameter = class(TPersistent)
     private
       FpVarList : TList;
       function __GetByName(ParamName: String): TmafVariable;
       function __GetByIndex(Index: Integer): TmafVariable;
       function __GetCount: Integer;
     protected
       procedure __ReadStreamData(Sender: TObject; ID: Integer);
       procedure __WriteStreamData(Sender: TObject; ID: Integer);
       procedure ReadParameters(aStream: TStream);
       procedure WriteParameters(aStream: TStream);
     public
       constructor Create;
       destructor Destroy; override;
       procedure DefineProperties(Filer: TFiler); override;
       function Add(AName: String; AType: TVarType): Integer;
       procedure Delete(idx: Integer);
       procedure Clear;

       procedure SaveToStream(aStream: TStream);
       procedure LoadFromStream(aStream: TStream);

       property ParamByName[ParamName: String]: TmafVariable read __GetByName;
       property Params[Index: Integer]: TmafVariable read __GetByIndex;
     published
       property Count : Integer read __GetCount;
     end;


     // records
     TQueryHandlerStruct = packed record
       HookID            : Integer;     // called Hook
       SubHookID         : Integer;     // 0, to execute all SubHooks, otherwise the SubHookID
       CommandID         : Integer;     // used by MA Framework components
       Command           : Integer;     // used by MA framework components
       ResVal            : Integer;     // Integer result var for caller
       FhParentHandle    : HWND;        // HWND of the parent
       pParent           : TComponent;  // Pointer to Parent
       pChildObj         : Pointer;     // Pointer to ChildObject in module
       pDB               : Pointer;     // Zeiger auf Database component
       pTransaction      : Pointer;     // Zeiger auf Transaction component
       pUpdateTransaction: Pointer;     // Pointer to update transaction
       EventFlags        : Byte;        // Flags, wich event will be triggered (Before/AfterExecHook)
       UserData1         : Pointer;
       UserData2         : Pointer;
       nNextSubHookID    : Integer;     // next SubHook to be called, -1, if is the current the last
       bStop             : Boolean;     // set True, if the execution should be stopped completly
       bSkipNext         : Boolean;     // set True to skip the next SubHook
       Reserved1         : Integer;     // Don't use if possible, will be often used by components
       Reserved2         : Integer;
       PReserved1        : Pointer;
       PReserved2        : Pointer;

       // function is used automatically used by compnents to free memory allocated
       // in another modul, do not use until you know, what you're doing !!!
       // it is called after every TmafHookClient.AfterExecHook-Event in  (if the event is used)
       // and at the end after the event TmafHookClient.AfterCallRouter
       // If a component decides to use it, it MUST be set to nil after freeing the memory
       // otherwise it will be called over and over again
       pFreeMemFunc      : TFreeMemFunc;
       CallerComponent   : TObject;     // contains ALWAYS the component that called the Dynamic Function
       pDataBlock        : Pointer;     // datablock coming from TmafModuleController, will be freed automatically
       uID               : Integer;     // the uID of the SubHook currently executed. If uID <> 0 when calling HM_EXEC_HOOK, the only the uID will be executed
       pParams           : TmafParameters; // a dynamic named parameter list
     end;
     pQHS = ^TQueryHandlerStruct;

     TModuleServiceProc = function(nSubHookID: Integer; QHS: pQHS; pUserParam: Pointer): Integer; stdcall;
     TExecuteHook = procedure(nCommand: Integer; QHS : pQHS; var pUserParam : Pointer; ErrCode: Integer) Of Object;
     TCallRouter  = procedure(nCommand: Integer; QHS : pQHS; var pUserParam : Pointer) Of Object;
     TOnSubHook   = procedure(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer) Of Object;
     TOnRegisterSubHook = procedure(SubHookID: Integer; FEvent: TOnSubHook) Of Object;

     RManagerItem = packed record
       FsLibraryName : String;           // Manager filename including path
       dwHandle      : THandle;          // Handle from LoadLibrary
       FpEntryPoint  : TModuleServiceProc; // EntryPoint for __ModuleServiceProc
       FbLoaded      : Boolean;          // True, if already loaded
       nManagerType  : Integer;          // hold the MT_XXX const
     end; // RManagerItem
     PManagerItem = ^RManagerItem;

     TDataStorageAction = (taNoAction, taRead, taWrite, taListCategories, taListNames, taGetID, taDelete);

     RDataStorageQuery = packed record
       Action : TDataStorageAction;
       nID : Integer;
       sCategory : PChar;
       sName : PChar;
       // in action = taRead/taWrite it must be a TMemoryStream
       // in action = taListXXX aObj must be a TStringList
       aObj : TObject;
       // FreeMemFunc is used to free the memory for the PChars just in case they
       // were created in another module
       pFreeMemFunc : TFreeMemFunc;
     end;
     PDataStorageQuery = ^RDataStorageQuery;

     TEventPriority = (epUltraHigh, epHigh, epNormal, epLow);

     REventToken = packed record
       nSubHookID : Integer;
       OnSubHook : TOnSubHook;
       aPriority : TEventPriority;
     end; // REventToken
     PEventToken = ^REventToken;

Type TOnManagerContainerError = procedure(Sender: TObject; var ManagerContainer: TComponent) Of Object;
     TClientOption = (coRegisterSelf);
     TClientOptions = Set Of TClientOption;

     TmafBaseComponent = class(TComponent)
     private
       FpManagerItem  : PManagerItem; // as long as this is nil, we didn't connect to the ManagerContainer
       FbCalledConnect : Boolean;
       FOnManagerContainerError : TOnManagerContainerError;
       FOnLoaded : TNotifyEvent;
       FbExecOnLoaded : Boolean;
       FClientOptions : TClientOptions;
       FbRegistered : Boolean;
       FbManagerClosed : Boolean;
       function __GetLoaded: Boolean;
     protected
       FManagerType : Integer;
       AContainer : TComponent;
       AManagerQueryID : Integer;
       procedure RegisterSelf; virtual;
       procedure UnRegisterSelf; virtual;
       procedure __ConnectToManager; virtual;
       procedure __DisconnectFromManager; virtual;
       procedure InitDone; virtual;
       procedure MSG_IsManagerType(var Msg: TMessage); message MSG_MANAGERTYPE;
       procedure WM_MangerClose(var Msg: TMessage); message WM_MANAGER_CLOSE;
       procedure WM_InitDone(var Msg: TMessage); message WM_INIT_DONE;
       procedure WM_RegisterNotify(var Msg: TMessage); message WM_REGISTER_NOTIFY;
       function __GetManagerBaseContainer: TComponent;
       function __AdvQuery_Manager(nCommand : Integer; QHS: pQHS; pUserParam : Pointer): Integer; virtual;
       property Connected : Boolean read FbCalledConnect;
       property ClientOptions : TClientOptions read FClientOptions write FClientOptions;
       property OnLoaded : TNotifyEvent read FOnLoaded write FOnLoaded;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       function CallManager(nCommand: Integer; lParam, wParam : Pointer): Integer; virtual;
       function CallForeignManager(ManagerType, nCommand: Integer; lParam, wParam : Pointer): Integer; virtual;
       property IsLoaded : Boolean read __GetLoaded;
     published
       property OnManagerContainerError : TOnManagerContainerError read FOnManagerContainerError write FOnManagerContainerError;
     end; // TmafBaseComponent

     TmafCustomModuleController = class(TmafBaseComponent)
     private
       FpManagerList : TList;
     protected
       function __GetRegisteredManager: Integer;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure RegisterSubHookEvent(nSubHookID: Integer; FEvent: TOnSubHook; EP: TEventPriority = epNormal); virtual; abstract;
       procedure UnregisterSubHookEvent(nSubHookID: Integer; FEvent: TOnSubHook); virtual; abstract;
       procedure RegisterManager(AManager: TComponent); virtual;
       procedure UnregisterManager(AManager: TComponent); virtual;
       function Execute(nSubHookID: Integer; aQHS, pUserParam: Pointer): Integer; virtual; abstract;
     published
       property RegisteredManager : Integer read __GetRegisteredManager;
     end;

     TmafCustomManagerComponent = class(TComponent)
     private
       FManagerType : Integer;
       FpClients : TList;
       FpModuleController : TmafCustomModuleController;
       FManagerOptions : TManagerOptions;
       FpManagerLoader : TComponent; // pointer to access the TmafManagerLoader
       FOnInitDone : TNotifyEvent;
       FOnError : TOnMAFError;
       procedure __SetModuleController(const Value: TmafCustomModuleController);
     protected
       FbInitDoneNotified : Boolean;
       procedure __RegisterAPI; virtual;     // do not call by yourself, it will be done by RegisterAPI
       procedure __UnRegisterAPI; virtual;   // do not call by yourself, it will be done by UnRegisterAPI
       procedure ClientAdded(aClient: TComponent; var CanAdd: Boolean); virtual;
       procedure ClientRemoved(aClient: TComponent); virtual;
       procedure CloseManager; virtual;
       procedure InitDone; virtual;
       procedure RegisterAPI;
       procedure UnRegisterAPI;
       procedure __DoManagerEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer); virtual;
       procedure __OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer); virtual;
       property Clients : TList read FpClients write FpClients;
       property OnError : TOnMAFError read FOnError write FOnError;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       property ManagerType : Integer read FManagerType write FManagerType;
       property ManagerOptions : TManagerOptions read FManagerOptions write FManagerOptions default [moAutoRegister];
     published
       property ModuleController : TmafCustomModuleController read FpModuleController write __SetModuleController;
       property OnInitDone: TNotifyEvent read FOnInitDone write FOnInitDone;
     end;

type PBytes = ^Byte;
     TMaskTable = array [0..517] of LongWord;

function __Create_QueryHandlerStruct: pQHS;
procedure __Free_QueryHandlerStruct(var QHS: pQHS);

function __Create_DataStorageQuery(ACategory, AName: String): PDataStorageQuery;
procedure __Free_DataStorageQuery(var pToken: PDataStorageQuery);
procedure __Apply_DataStorageNames(ACategory, AName : String; pQuery: PDataStorageQuery);

function CheckIfProcPatched(const ProcPos: Pointer): Boolean;
function CheckIfNopInProc(const ProcPos: Pointer): Boolean;
function CheckIfBPInProc(const ProcPos: Pointer): Boolean;

function CreateVarRec(VarName : String; VarFlags: TVarFlags; ALoadSave: TLoadSave): PVarRec;
procedure FreeVarRec(var pData : PVarRec);

var MaskTable: TMaskTable = (
$00004000, $00004000, $00004000, $00004000,
$00008000, $00008000, $00000000, $00000000,
$00004000, $00004000, $00004000, $00004000,
$00008000, $00008000, $00000000, $00000000,
$00004000, $00004000, $00004000, $00004000,
$00008000, $00008000, $00000000, $00000000,
$00004000, $00004000, $00004000, $00004000,
$00008000, $00008000, $00000000, $00000000,
$00004000, $00004000, $00004000, $00004000,
$00008000, $00008000, $00000008, $00000000,
$00004000, $00004000, $00004000, $00004000,
$00008000, $00008000, $00000008, $00000000,
$00004000, $00004000, $00004000, $00004000,
$00008000, $00008000, $00000008, $00000000,
$00004000, $00004000, $00004000, $00004000,
$00008000, $00008000, $00000008, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00004000, $00004000,
$00000008, $00000008, $00001008, $00000018,
$00002000, $00006000, $00000100, $00004100,
$00000000, $00000000, $00000000, $00000000,
$00000100, $00000100, $00000100, $00000100,
$00000100, $00000100, $00000100, $00000100,
$00000100, $00000100, $00000100, $00000100,
$00000100, $00000100, $00000100, $00000100,
$00004100, $00006000, $00004100, $00004100,
$00004000, $00004000, $00004000, $00004000,
$00004000, $00004000, $00004000, $00004000,
$00004000, $00004000, $00004000, $00004000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00002002, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000020, $00000020, $00000020, $00000020,
$00000000, $00000000, $00000000, $00000000,
$00000100, $00002000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000100, $00000100, $00000100, $00000100,
$00000100, $00000100, $00000100, $00000100,
$00002000, $00002000, $00002000, $00002000,
$00002000, $00002000, $00002000, $00002000,
$00004100, $00004100, $00000200, $00000000,
$00004000, $00004000, $00004100, $00006000,
$00000300, $00000000, $00000200, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00004000, $00004000, $00004000, $00004000,
$00000100, $00000100, $00000000, $00000000,
$00004000, $00004000, $00004000, $00004000,
$00004000, $00004000, $00004000, $00004000,
$00000100, $00000100, $00000100, $00000100,
$00000100, $00000100, $00000100, $00000100,
$00002000, $00002000, $00002002, $00000100,
$00000000, $00000000, $00000000, $00000000,
$00000008, $00000000, $00000008, $00000008,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00004000, $00004000,
$00004000, $00004000, $00004000, $00004000,
$FFFFFFFF, $FFFFFFFF, $00000000, $FFFFFFFF,
$00000000, $00000000, $00000000, $00000000,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$00002000, $00002000, $00002000, $00002000,
$00002000, $00002000, $00002000, $00002000,
$00002000, $00002000, $00002000, $00002000,
$00002000, $00002000, $00002000, $00002000,
$00004000, $00004000, $00004000, $00004000,
$00004000, $00004000, $00004000, $00004000,
$00004000, $00004000, $00004000, $00004000,
$00004000, $00004000, $00004000, $00004000,
$00000000, $00000000, $00000000, $00004000,
$00004100, $00004000, $FFFFFFFF, $FFFFFFFF,
$00000000, $00000000, $00000000, $00004000,
$00004100, $00004000, $FFFFFFFF, $00004000,
$00004000, $00004000, $00004000, $00004000,
$00004000, $00004000, $00004000, $00004000,
$FFFFFFFF, $FFFFFFFF, $00004100, $00004000,
$00004000, $00004000, $00004000, $00004000,
$00004000, $00004000, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$00000000, $00000000, $00000000, $00000000,
$00000000, $00000000, $00000000, $00000000,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$00000000, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
$FFFFFFFF, $FFFFFFFF);

  // the TmafManagerLoader exists only once in every application
  // so it stores the Self pointer here free to access from every client
  // to find its own Manager
  gManagerLoader : TComponent;

implementation

uses Forms, uMAF_ManagerLoader, uMAF_TemplateStreamer;

const MAX_EVENTS = 6;

var ManagerEvents : array[1..MAX_EVENTS] of Integer = (
      MAN_GET_MANAGER_TYPE,
      MAN_REGISTER_SELF,
      MAN_UNREGISTER_SELF,
      MAN_CLOSE_COMPONENT,
      MAN_INIT_DONE,
      MAN_STATUS
    );

    EventPriorities : array[1..MAX_EVENTS] of TEventPriority = (
      epLow, epLow, epLow, epLow, epLow, epLow
      );

function __Create_QueryHandlerStruct: pQHS;
begin
  New(Result);
  FillChar(Result^, SizeOf(TQueryhandlerStruct), 0);
end;

procedure __Free_QueryHandlerStruct(var QHS: pQHS);
begin
  If Assigned(QHS) Then
    Dispose(QHS);
  QHS := nil;
end;

function __Create_DataStorageQuery(ACategory, AName: String): PDataStorageQuery;
begin
  New(Result);
  FillChar(Result^, SizeOf(RDataStorageQuery), 0);
  If ACategory <> '' Then
    StrToPChar(ACategory, Result^.sCategory);
  If AName <> '' Then
    StrToPChar(AName, Result^.sName);
end;

procedure __Free_DataStorageQuery(var pToken: PDataStorageQuery);
begin
  If pToken = nil Then
    Exit;

  If Assigned(pToken^.pFreeMemFunc) Then
    pToken^.pFreeMemFunc(pToken);          // will free only the Category and Templatename PChars
  If pToken^.sCategory <> nil Then
    FreePChar(pToken^.sCategory);
  If pToken^.sName <> nil Then
    FreePChar(pToken^.sName);
  Dispose(pToken);
  pToken := nil;
end;

procedure __Apply_DataStorageNames(ACategory, AName : String; pQuery: PDataStorageQuery);
begin
  FreePChar(pQuery^.sCategory);
  StrToPChar(ACategory, pQuery^.sCategory);
  FreePChar(pQuery^.sName);
  StrToPChar(AName, pQuery^.sName);
end;

function CreateVarRec(VarName : String; VarFlags: TVarFlags; ALoadSave: TLoadSave): PVarRec;
begin
  New(Result);
  FillChar(Result^, SizeOf(RVarRec), 0);
  StrToPChar(VarName, Result^.Name);
  Result^.dwFlags := VarFlags;
  If vfSave in VarFlags Then
    Result^.LoadSave := ALoadSave      // if save, where...
  Else
    Result^.LoadSave := lsNone;
end; // CreateVarRec

procedure FreeVarRec(var pData : PVarRec);
begin
  If Not Assigned(pData) Then
    Exit;

  FreePChar(pData^.Name);
  If pData^.Typ = vtString Then
    FreePChar(PChar(pData^.Var1));
  If pData^.CallBackList <> nil Then
    pData^.CallBackList.Free;
  Dispose(pData);
  pData := nil;
end; // FreeVarRec

function GetOpCodeSize(Start: Pointer; Tlb: TMaskTable): integer;
var pOPCode: PBytes;
    t, c: LongWord;
    dh, dl, al: byte;
begin
  result := -1;
  t := 0;
  pOPCode := Start;
  repeat
    t := t and $F7;
    c := pOPCode^;
    pOpCode := Pointer((DWORD(pOpCode) + 1));
    t := t or Tlb[c];
  until ((t and $000000FF) and 8) = 0;

  if (c = $0F6) or (c = $0F7) then begin
    t := t or $00004000;
    if (pOpCode^ and $38) = 0 then
      t := t or $00008000;
  end else
    if (c = $0CD) then begin
      t := t or $00000100;
      if pOpCode^ = $20 then
        t := t or $00000400;
    end else
      if (c = $0F) then begin
        al := pOpCode^;
        pOpCode := Pointer((DWORD(pOpCode) + 1));
        t := t or Tlb[al + $100];
        if t = $FFFFFFFF then
          Exit;
      end;
  if (((t and $0000FF00) shr 8) and $80) <> 0 then begin
    dh := (t and $0000FF00) shr 8;
    dh := dh xor $20;
    if (c and 1) = 0 then
      dh := dh xor $21;
    t := t and $FFFF00FF;
    t := t or (dh shl 8);
  end;

  if (((t and $0000FF00) shr 8) and $40) <> 0 then begin
    al := pOpCode^;
    pOpCode := Pointer((DWORD(pOpCode) + 1));
    c := al;
    c := c or (al shl 8);
    c := c and $C007;
    if (c and $0000FF00) <> $C000 then begin
      if ((t and $000000FF) and $10) = 0 then begin
        if (c and $000000FF) = 4 then begin
          al := pOpCode^;
          pOpCode := Pointer((DWORD(pOpCode) + 1));
          al := al and 7;
          c := c and $0000FF00;
          c := c or al;
        end;
        if (c and $0000FF00) <> $4000 then begin
          if (c and $0000FF00) = $8000 then begin
            t := t or 4;
          end else
            if c = 5 then
              t := t or 4;
        end else begin
            t := t or 1;
      end;
    end else begin
      if (c <> 6) then begin
        if (c and $0000FF00) = $4000 then
          t := t or 1
        else
          if (c and $0000FF00) = $8000 then
            t := t or 2;
        end else
        t := t or 2;
      end;
    end;
  end;
  if (((t and $000000FF)) and $20) <> 0 then begin
    dl := (t and $000000FF);
    dl := dl xor 2;
    t := t and $FFFFFF00;
    t := t or dl;
    if (dl and $10) = 0 then begin
      dl := dl xor 6;
      t := t and $FFFFFF00;
      t := t or dl;
    end;
  end;
  if (((t and $0000FF00) shr 8) and $20) <> 0 then begin
    dh := (t and $0000FF00) shr 8;
    dh := dh xor 2;
    t := t and $FFFF00FF;
    t := t or (dh shl 8);
    if (dh and $10) = 0 then begin
      dh := dh xor 6;
      t := t and $FFFFFF00;
      t := t or dh;
    end;
  end;
  result := DWORD(pOPCode) - DWORD(Start);
  t := t and $707;
  result := result + (t and $000000FF);
  result := result + ((t and $0000FF00) shr 8);
end;

function CheckIfProcPatched(const ProcPos: Pointer): Boolean;
begin
  Result := False;
  if ProcPos = nil then
    Exit;

  if PByte(ProcPos)^ = $C3 then
    Result := True;
end;

function CheckIfNopInProc(const ProcPos: Pointer): Boolean;
var ProcData: PByte;
begin
  Result := False;
  if ProcPos = nil then
    Exit;

  ProcData := PByte(ProcPos);
  while True do begin
    if ProcData^ = $C3 then
      Break;

    if ProcData^ = $90 then
      Result := True;

    Inc(ProcData, GetOpCodeSize(ProcData, MaskTable));
  end;
end;

function CheckIfBPInProc(const ProcPos: Pointer): Boolean;
var ProcData: PByte;
begin
  Result := False;
  if ProcPos = nil then
    Exit;

  ProcData := PByte(ProcPos);
  while True do begin
    if ProcData^ = $C3 then
      Break;

    if ProcData^ = $CC then
      Result := True;

    Inc(ProcData, GetOpCodeSize(ProcData, MaskTable));
  end;
end;

{ TmafCustomManagerComponent }

constructor TmafCustomManagerComponent.Create(AOwner: TComponent);
begin
  inherited;
  FManagerType := -1;
  FpModuleController := nil;
  FpClients := TList.Create;
  FManagerOptions := [moAutoRegister];
  FpManagerLoader := nil;
  FbInitDoneNotified := False;
end;

destructor TmafCustomManagerComponent.Destroy;
begin
  If FpModuleController <> nil Then
    __UnRegisterAPI;
  FpClients.Free;
  inherited;
end;

procedure TmafCustomManagerComponent.InitDone;
begin
  If Not FbInitDoneNotified Then begin    // making sure, that we always fire that event just once
    If Assigned(FpModuleController) Then
      SendComponentMessage(ModuleController, WM_INIT_DONE, nil, nil);
    If Assigned(FOnInitDone) Then
      FOnInitDone(Self);
    FbInitDoneNotified := True;
  end;
end;

procedure TmafCustomManagerComponent.Loaded;
begin
  inherited;
  RegisterAPI;
end;

procedure TmafCustomManagerComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  If Operation = opRemove Then
    If AComponent = FpModuleController Then
      FpModuleController := nil;
end;

procedure TmafCustomManagerComponent.__SetModuleController(const Value: TmafCustomModuleController);
begin
  If Value = FpModuleController Then
    Exit;

  If FpModuleController <> nil Then begin
    FpModuleController.RemoveFreeNotification(Self);
    UnRegisterAPI;
  end;  //  --  If FpModuleController <> nil Then

  FpModuleController := Value;
  If Assigned(FpModuleController) Then begin
    FpModuleController.FreeNotification(Self);
    RegisterAPI;
  end;  //  --  If Assigned(FpModuleController) Then 
end; // __SetModuleController

// is called in __OnEvent, if a manager specific messages comes in, must be overwritten
// handle specific events
procedure TmafCustomManagerComponent.__DoManagerEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  // Do nothing as it is for an specific manager event
end;

procedure TmafCustomManagerComponent.__OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
var bCanAdd : Boolean;
    idx : Integer;
begin
  // handles messages to specific MT_XXXX constants, allows us to
  If SubHookID = ManagerType Then begin
    __DoManagerEvent(SubHookID, QHS, UserParam, ErrCode);
    Exit;
  end;

  Case SubHookID Of
    MAN_GET_MANAGER_TYPE : If Assigned(QHS) Then
                             If String(PChar(QHS^.pChildObj)) = Self.Name Then
                               QHS^.ResVal := FManagerType;
    MAN_REGISTER_SELF    : If QHS^.Reserved1 = FManagerType Then begin // only "our" MAN_REGISTER_SELF
                             idx := FpClients.IndexOf(QHS^.pChildObj);
                             If ((idx = -1) And (QHS^.pChildObj <> nil)) Then begin
                               bCanAdd := True;
                               ClientAdded(TComponent(QHS^.pChildObj), bCanAdd);
                               If bCanAdd Then
                                 FpClients.Add(QHS^.pChildObj);
                             end;
                           end;
    MAN_UNREGISTER_SELF  : If QHS^.Reserved1 = FManagerType Then  // only "our" MAN_UNREGISTER_SELF
                             If ((FpClients.IndexOf(QHS^.pChildObj) > -1) And (QHS^.pChildObj <> nil)) Then begin
                               ClientRemoved(TComponent(QHS^.pChildObj));
                               FpClients.Delete(FpClients.IndexOf(QHS^.pChildObj));
                             end;
    MAN_CLOSE_COMPONENT  : begin
                             If PChar(UserParam) = Self.Name Then begin
                               CloseManager;
                               If Assigned(FpModuleController) Then
                                 QHS^.Reserved1 := FpModuleController.RegisteredManager;
                             end;  //  --  If PChar(UserParam) = Self.Name Then
                           end;
    MAN_INIT_DONE        : If QHS^.Reserved1 = FManagerType Then
                             InitDone;
    MAN_STATUS           : If PChar(UserParam) = Self.Name Then
                             QHS^.Reserved2 := FManagerType;
  end;
end;

procedure TmafCustomManagerComponent.__RegisterAPI;
var i : Integer;
    bCanRegister : Boolean;
begin
  If Assigned(FpModuleController) Then begin
    If moAutoRegister in FManagerOptions Then
      FpModuleController.RegisterManager(Self);
    FpModuleController.RegisterSubHookEvent(FManagerType, __OnEvent, epHigh); // we register the manager event handler
    For i := 1 To MAX_EVENTS Do begin
      bCanRegister := True;
      Case ManagerEvents[i] Of
        MAN_CLOSE_COMPONENT : bCanRegister := (moAutoRegister in FManagerOptions);
        MAN_INIT_DONE       : bCanRegister := (moRegisterInitDone in FManagerOptions);
      end;  //  --  Case ManagerEvents[i] Of
      If bCanRegister Then
        FpModuleController.RegisterSubHookEvent(ManagerEvents[i], __OnEvent, EventPriorities[i]);
    end;  //  --  For i := 1 To MAX_EVENTS Do
  end;  //  --  If Assigned(FpModuleController) Then
end;

procedure TmafCustomManagerComponent.__UnRegisterAPI;
var i : Integer;
begin
  If Assigned(FpModuleController) Then
    For i := 1 To MAX_EVENTS Do
      FpModuleController.UnRegisterSubHookEvent(ManagerEvents[i], __OnEvent);
end;

procedure TmafCustomManagerComponent.RegisterAPI;
begin
  If csLoading in ComponentState Then
    Exit;
  __RegisterAPI;
end;

procedure TmafCustomManagerComponent.UnRegisterAPI;
begin
  If csLoading in ComponentState Then
    Exit;
  __UnRegisterAPI;
end;

procedure TmafCustomManagerComponent.ClientAdded(aClient: TComponent; var CanAdd: Boolean);
begin
  CanAdd := True;
end;

procedure TmafCustomManagerComponent.ClientRemoved(aClient: TComponent);
begin
  // place holder
end;

procedure TmafCustomManagerComponent.CloseManager;
var i : Integer;
begin
  For i := 0 To FpClients.Count - 1 do
    SendComponentMessage(TComponent(FpClients.Items[i]), WM_MANAGER_CLOSE, nil, nil);

  If ((moAutoRegister in FManagerOptions) And (Assigned(FpModuleController))) Then
    FpModuleController.UnregisterManager(Self);
  FbInitDoneNotified := False;
end;

{ TmafBaseComponent }

// ********************************* Comments **********************************
// Description : constructor
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.02.2007
// Last Update : 26.07.2009
// *****************************************************************************
constructor TmafBaseComponent.Create(AOwner: TComponent);
begin
  FbManagerClosed := False;
  inherited;
  FClientOptions := [];
  FpManagerItem := nil;
  FbCalledConnect := False;
  AContainer := nil;
  FbExecOnLoaded := False;
  FbRegistered := False;
end; // Create

// ********************************* Comments **********************************
// Description : destructor
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.02.2007
// Last Update : 28.02.2007
// *****************************************************************************
destructor TmafBaseComponent.Destroy;
begin
  __DisconnectFromManager;
  inherited;
end; // Destroy

procedure TmafBaseComponent.Loaded;
begin
  inherited;
  If ((csDesigning in ComponentState) or (Not bRunMode)) Then
    Exit;

  If coRegisterSelf in FClientOptions Then
    RegisterSelf;
  If Assigned(FOnLoaded) Then
    FOnLoaded(Self);
  FbExecOnLoaded := True;
end;

procedure TmafBaseComponent.InitDone;
begin
  If coRegisterSelf in FClientOptions Then
    If Not FbRegistered Then
      RegisterSelf;
end;

// ********************************* Comments **********************************
// Description : returns, if the Manager is loaded or not
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.02.2007
// Last Update : 28.02.2007
// *****************************************************************************
function TmafBaseComponent.__GetLoaded: Boolean;
begin
  Result := False;
  If csDesigning in ComponentState Then
    Exit;
  If Assigned(FpManagerItem) Then
    Result := FpManagerItem^.FbLoaded;
end; // __GetLoaded

// ********************************* Comments **********************************
// Description : we get informed, when the ManagerBaseContainer gets deleted
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 01.03.2007
// Last Update : 01.03.2007
// *****************************************************************************
procedure TmafBaseComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  If Operation = opRemove Then
    If AComponent = AContainer Then
      __DisconnectFromManager;
end; // Notification

procedure TmafBaseComponent.MSG_IsManagerType(var Msg: TMessage);
begin
  If Msg.WParam = FManagerType Then
    Msg.Result := 1
  Else
    Msg.Result := 0;
end; // WM_IsManagerType

procedure TmafBaseComponent.RegisterSelf;
var QHS: pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.pChildObj := Self;
  QHS^.Reserved1 := FManagerType;
  If CallManager(MAN_REGISTER_SELF, QHS, Self) = ERR_NO_ERROR Then
    FbRegistered := True;
  __Free_QueryHandlerStruct(QHS);
end;

procedure TmafBaseComponent.UnRegisterSelf;
var QHS: pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.pChildObj := Self;
  QHS^.Reserved1 := FManagerType;
  CallManager(MAN_UNREGISTER_SELF, QHS, Self);
  __Free_QueryHandlerStruct(QHS);
  FbRegistered := False;
end;

procedure TmafBaseComponent.WM_InitDone(var Msg: TMessage);
begin
  InitDone;
  FbManagerClosed := False;
end;

procedure TmafBaseComponent.WM_MangerClose(var Msg: TMessage);
begin
  FbRegistered := False;
  FpManagerItem := nil;
  FbManagerClosed := True;
end;

procedure TmafBaseComponent.WM_RegisterNotify(var Msg: TMessage);
begin
  If coRegisterSelf in FClientOptions Then
    RegisterSelf;
end;

// ********************************* Comments **********************************
// Description : base implementation for a connect method
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.02.2007
// Last Update : 28.02.2007
// *****************************************************************************
procedure TmafBaseComponent.__ConnectToManager;
begin
  If ((Not FbRegistered) And (coRegisterSelf in FClientOptions)) Then
    RegisterSelf;

  FbCalledConnect := True;
end; // __ConnectToManager

// ********************************* Comments **********************************
// Description : base implementation for a disconnect method
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.02.2007
// Last Update : 26.07.2009
// *****************************************************************************
procedure TmafBaseComponent.__DisconnectFromManager;
begin
  If ((coRegisterSelf in FClientOptions) And (FbRegistered)) Then
    UnRegisterSelf;
  FpManagerItem := nil;
end; // __DisconnectFromManager

function TmafBaseComponent.__GetManagerBaseContainer: TComponent;
var i : Integer;
begin
  if gManagerLoader <> nil then begin
    Result := gManagerLoader;
    Exit;
  end;

  Result := nil;
  If Application.MainForm = nil Then begin
    If Application.Tag > 0 Then
      Result := TComponent(Application.Tag)
    Else
      If Assigned(FOnManagerContainerError) Then
        FOnManagerContainerError(Self, Result);
  end else begin
    For i := 0 To Application.MainForm.ComponentCount - 1 Do
      If IsClass(Application.MainForm.Components[i], TmafManagerLoader) Then begin
        Result := Application.MainForm.Components[i];
        Break;
      end;  //  --  If IsClass(Application.MainForm.Components[i], TERPManagerContainer) Then
  end;
end;

function TmafBaseComponent.__AdvQuery_Manager(nCommand: Integer; QHS: pQHS; pUserParam: Pointer): Integer;
begin
  If QHS = nil Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;
  QHS^.HookID := AManagerQueryID;
  QHS^.SubHookID := nCommand;
  Result := CallManager(AManagerQueryID, QHS, pUserParam);
end;

// ********************************* Comments **********************************
// Description : Calls the own Manager
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.02.2007
// Last Update : 28.07.2009
// *****************************************************************************
function TmafBaseComponent.CallManager(nCommand: Integer; lParam, wParam: Pointer): Integer;
var j : Integer;
begin
  If FbManagerClosed Then begin
    Result := ERR_MANAGER_NOT_LOADED;
    Exit;
  end;
  If csDesigning in ComponentState Then begin
    Result := 0;
    Exit;
  end;  //  --  If csDesigning in ComponentState Then
  Result := ERR_MANAGER_NOT_LOADED;
  {$B-}  // Boolean short-circuit evaluation off
  If ((FpManagerItem = nil) Or (Not FpManagerItem^.FbLoaded)) Then begin
  {$B+}
    If csDestroying in ComponentState Then
      Exit;

    AContainer := __GetManagerBaseContainer;
    If AContainer = nil Then
      Raise EComponentError.Create('ManagerBaseContainer not found !');

    j := SendComponentMessage(AContainer, MSG_GET_MANAGER_DATA, Pointer(FManagerType), Self);
    If j > 0 Then begin
      FpManagerItem := PManagerItem(j);
      If FpManagerItem^.FbLoaded Then
        __ConnectToManager;
    end;
  end;

  {$B-}  // Boolean short-circuit evaluation off
  If ((FpManagerItem = nil) Or (Not FpManagerItem^.FbLoaded)) Then begin
  {$B+}
    Result := ERR_DLL_NOT_LOADED;
    Exit;
  end;  //  --  If ((FpManagerItem = nil) Or (Not FpManagerItem^.FbLoaded)) Then

  If Assigned(FpManagerItem^.FpEntryPoint) Then
    Result := FpManagerItem^.FpEntryPoint(nCommand, lParam, wParam);
end; // CallManager

function TmafBaseComponent.CallForeignManager(ManagerType, nCommand: Integer; lParam, wParam: Pointer): Integer;
var MBC : TComponent;
    j : Integer;
    MI : PManagerItem;
begin
  Result := ERR_DLL_NOT_LOADED;
  If csDestroying in ComponentState Then
    Exit;
  MBC := __GetManagerBaseContainer;
  If MBC = nil Then
    Raise EComponentError.Create('ManagerBaseContainer not found !');

  j := SendComponentMessage(MBC, MSG_GET_MANAGER_DATA, Pointer(ManagerType), Self);
  If j > 0 Then begin
    MI := PManagerItem(j);
    If ((MI^.FbLoaded) And (Assigned(MI^.FpEntryPoint))) Then
      Result := MI^.FpEntryPoint(nCommand, lParam, wParam);
  end;
end;

{ TmafCustomModuleController }

constructor TmafCustomModuleController.Create(AOwner: TComponent);
begin
  inherited;
  FpManagerList := TList.Create;
end;

destructor TmafCustomModuleController.Destroy;
begin
  FreeAndNil(FpManagerList);
  inherited;
end;

function TmafCustomModuleController.__GetRegisteredManager: Integer;
begin
  Result := FpManagerList.Count;
end;

procedure TmafCustomModuleController.RegisterManager(AManager: TComponent);
begin
  If FpManagerList.IndexOf(AManager) = -1 Then
    FpManagerList.Add(AManager);
end;

procedure TmafCustomModuleController.UnregisterManager(AManager: TComponent);
var i : Integer;
begin
  i := FpManagerList.IndexOf(AManager);
  If i > -1 Then
    FpManagerList.Delete(i);
end;

{ TVariableStack }

procedure TVariableStack.Clear;
var pToken : PVarStackToken;
begin
  While FpStack.Count > 0 Do begin
    pToken := PVarStackToken(FpStack.Items[0]);
    Case pToken^.FType Of
      vtString  : FreeMem(pToken^.pToken, StrLen(PChar(pToken^.pToken)+1));
      vtObject  : TObject(pToken^.pToken).Free;
      vtPointer : begin // we can't free pointer !!!!
                    {$ifdef Debug}
                    OutputDebugString(PChar('TVariableStack.WARNING : Pointer left on the stack !'));
                    {$endif}
                  end;
    end;
    Dispose(pToken);
    FpStack.Delete(0);
  end;  //  --  While FpStack.Count > 0 Do
end;

constructor TVariableStack.Create;
begin
  FpStack := TList.Create;
end;

destructor TVariableStack.Destroy;
begin
  // actually there shouldn't be anything on that stack at this point, because
  // every real programmer knows, that if you push, you have to pop ;)
  {$ifdef Debug}
    If FpStack.Count > 0 Then
      OutputDebugString(PChar('TVariableStack.WARNING : There are '+IntToStr(FpStack.Count)+' left on the stack'));
  {$endif}
  FpStack.Clear;
  FpStack.Free;
  inherited;
end; // Destroy

function TVariableStack.__GetCount: Integer;
begin
  Result := FpStack.Count;
end;

procedure TVariableStack.__SetInteger(const Value: Integer);
var pToken : PVarStackToken;
begin
  New(pToken);
  pToken^.FType := vtInteger;
  pToken^.pToken := Pointer(Value);
  FpStack.Add(pToken);
end; // __SetInteger

function TVariableStack.__GetInteger: Integer;
var pToken : PVarStackToken;
begin
  Result := 0;
  If FpStack.Count = 0 Then
    Exit;
  pToken := PVarStackToken(FpStack.Items[FpStack.Count-1]);
  If pToken^.FType <> vtInteger Then
    Exit;     { TODO : Raise exception einbauen }
  Result := Integer(pToken^.pToken);
  Dispose(pToken);
  FpStack.Delete(FpStack.Count-1);
end; // __GetInteger

// ********************************* Comments **********************************
// Description : pushes a string to the stack
// Param (in)  : value=String to push
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 16.02.2007
// Last Update : 16.02.2007
// *****************************************************************************
procedure TVariableStack.__SetString(const Value: String);
var pToken : PVarStackToken;
    PC : PChar;
begin
  New(pToken);
  pToken^.FType := vtString;
  PC := nil;
  StrToPChar(Value, PC);
  pToken^.pToken := PC;
  FpStack.Add(pToken);
end; // __SetString

// ********************************* Comments **********************************
// Description : read a string from the stack
// Param (in)  : N/A
// Param (out) : string wich was stacked before
// Coding by   : Helge Lange
// Date        : 16.02.2007
// Last Update : 16.02.2007
// *****************************************************************************
function TVariableStack.__GetString: String;
var pToken : PVarStackToken;
begin
  Result := '';
  If FpStack.Count = 0 Then
    Exit;
  pToken := PVarStackToken(FpStack.Items[FpStack.Count-1]);
  If pToken^.FType <> vtString Then
    Exit;
  Result := PChar(pToken^.pToken);
  FreeMem(pToken^.pToken, StrLen(PChar(pToken^.pToken)+1));
  Dispose(pToken);
  FpStack.Delete(FpStack.Count-1);
end; // __GetString

// ********************************* Comments **********************************
// Description : writes a pointer to the stack
// Param (in)  : Value=Pointer
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 16.02.2007
// Last Update : 16.02.2007
// *****************************************************************************
procedure TVariableStack.__SetPointer(const Value: Pointer);
var pToken : PVarStackToken;
begin
  New(pToken);
  pToken^.FType := vtPointer;
  pToken^.pToken := Value;
  FpStack.Add(pToken);
end; // __SetPointer

// ********************************* Comments **********************************
// Description : reads a pointer from the stack
// Param (in)  : N/A
// Param (out) : Pointer
// Coding by   : Helge Lange
// Date        : 16.02.2007
// Last Update : 16.02.2007
// *****************************************************************************
function TVariableStack.__GetPointer: Pointer;
var pToken : PVarStackToken;
begin
  Result := nil;
  If FpStack.Count = 0 Then
    Exit;
  pToken := PVarStackToken(FpStack.Items[FpStack.Count-1]);
  If pToken^.FType <> vtPointer Then
    Exit;
  Result := pToken^.pToken;
  Dispose(pToken);
  FpStack.Delete(FpStack.Count-1);
end; // __GetPointer

// ********************************* Comments **********************************
// Description : writes an TObject to the stack
// Param (in)  : Value=TObject
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 16.02.2007
// Last Update : 16.02.2007
// *****************************************************************************
procedure TVariableStack.__SetObject(const Value: TObject);
var pToken : PVarStackToken;
begin
  New(pToken);
  pToken^.FType := vtObject;
  pToken^.pToken := Pointer(Value);
  FpStack.Add(pToken);
end; // __SetObject

// ********************************* Comments **********************************
// Description : reads an object from the stack
// Param (in)  : N/A
// Param (out) : TObject
// Coding by   : Helge Lange
// Date        : 16.02.2007
// Last Update : 16.02.2007
// *****************************************************************************
function TVariableStack.__GetObject: TObject;
var pToken : PVarStackToken;
begin
  Result := nil;
  If FpStack.Count = 0 Then
    Exit;
  pToken := PVarStackToken(FpStack.Items[FpStack.Count-1]);
  If pToken^.FType <> vtObject Then
    Exit;
  Result := TObject(pToken^.pToken);
  Dispose(pToken);
  FpStack.Delete(FpStack.Count-1);
end; // __GetObject

{ TmafVariable }

constructor TmafVariable.Create;
begin
  inherited;
  pToken := nil;
  FIndex := -1;
  FVarParamType := vptIn;
end;

destructor TmafVariable.Destroy;
begin
  Case FType Of
    vtString  : FreePChar(PChar(pToken));
    vtPointer : ;
    vtObject  : TObject(pToken).Free;
  end;
  inherited;
end;

function TmafVariable.__GetAsString: String;
begin
  Case FType Of
    vtString : Result := String(PChar(pToken));
    vtInteger: Result := IntToStr(Integer(pToken));
    vtPointer: Result := IntToHex(Integer(pToken), 2);
    vtBoolean: If pToken = nil Then Result := 'FALSE'
                               Else Result := 'TRUE';
    vtDateTime : Result := FormatDateTime({$IFDEF D16+}FormatSettings.ShortDateFormat{$ELSE}ShortDateFormat{$ENDIF}, TDateTime(pToken^));
    Else Result := '';
  end;
end;

procedure TmafVariable.__SetAsString(const Value: String);
begin
  Case FType Of
    vtString   : begin
                   If pToken <> nil Then
                     FreePChar(PChar(pToken));
                   StrToPChar(Value, PChar(pToken));
                 end;
    vtInteger  : begin
                   pToken := Pointer(StrToIntDef(Value, 0));
                 end;
    vtPointer,
    vtObject   : Raise Exception.Create('Cannot set Pointer or TObject as String');
    vtBoolean  : If UpperCase(Value) = 'TRUE' Then
                   pToken := Pointer(1)
                 Else
                   pToken := nil;
    vtDateTime: ;
  end;
end;

function TmafVariable.__GetBoolean: Boolean;
begin
  Result := False;
  Case FType Of
    vtString   : Result := (__GetAsString = 'TRUE');
    vtBoolean,
    vtInteger  : Result := (Integer(pToken) = 1);
    vtPointer  : ;
    vtObject   : ;
    vtDateTime : ;
  end;
end;

procedure TmafVariable.__SetBoolean(const Value: Boolean);
begin
  Case FType Of
    vtString   : If Value Then __SetAsString('TRUE')
                          Else __SetAsString('FALSE');
    vtBoolean,
    vtInteger  : If Value Then pToken := Pointer(1)
                          Else pToken := nil;
    vtPointer  : ;
    vtObject   : ;
    vtDateTime : ;
  end;
end;

function TmafVariable.__GetAsInteger: Integer;
begin
  Result := 0;
  Case FType Of
    vtString   : Result := StrToIntDef(String(PChar(pToken)), 0);
    vtBoolean,   // return 0 when false,  1 for True
    vtPointer,
    vtObject,
    vtInteger  : Result := Integer(pToken);
    vtDateTime : ;
  end;
end;

procedure TmafVariable.__SetAsInteger(const Value: Integer);
begin
  Case FType Of
    vtString   : begin
                   If pToken <> nil Then
                     FreePChar(PChar(pToken));
                   StrToPChar(IntToStr(Value), PChar(pToken));
                 end;
    vtPointer,
    vtObject,
    vtBoolean,
    vtInteger  : pToken := Pointer(Value);
    vtDateTime : ;
  end;
end;

function TmafVariable.__GetAsPointer: Pointer;
begin
  Result := nil;
  Case FType Of
    vtString: ;
    vtInteger,
    vtPointer  : Result := pToken;
    vtObject: ;
    vtBoolean: ;
    vtDateTime: ;
  end;
end;

procedure TmafVariable.__SetAsPointer(const Value: Pointer);
begin
  Case FType Of
    vtString: ;
    vtInteger,
    vtObject,
    vtPointer  : pToken := Value;
    vtBoolean: ;
    vtDateTime : ;
  end;
end;

function TmafVariable.__GetAsObject: TObject;
begin
  Result := nil;
  Case FType Of
    vtString: ;
    vtInteger: ;
    vtPointer: ;
    vtObject   : Result := TObject(pToken);
    vtBoolean: ;
    vtDateTime: ;
  end;
end;

procedure TmafVariable.__SetAsObjects(const Value: TObject);
begin
  Case FType Of
    vtString: ;
    vtInteger: ;
    vtPointer: ;
    vtObject   : pToken := Pointer(Value);
    vtBoolean: ;
    vtDateTime: ;
  end;
end;

procedure TmafVariable.__SetValue(const Value: Variant);
begin
  Case FType Of
    vtString   : __SetAsString(String(Value));
    vtInteger  : __SetAsInteger(Integer(Value));
    vtPointer: ;
    vtObject: ;
    vtBoolean: ;
    vtDateTime: ;
  end;
end;

function TmafVariable.__GetValue: Variant;
begin
  Case FType Of
    vtString   : Result := String(PChar(pToken));
    vtInteger  : Result := Integer(pToken);
    vtPointer  : ;
    vtObject   : ;//Result := TObject(pToken);
    vtBoolean  : Result := (pToken = nil);
    vtDateTime : ;
  end;
end;

procedure TmafVariable.__SetAsDateTime(const Value: TDateTime);
begin
  Case FType Of
    vtString: ;
    vtInteger: ;
    vtPointer: ;
    vtObject: ;
    vtBoolean: ;
    vtDateTime : begin
                   If pToken = nil Then
                     GetMem(pToken, SizeOf(TDateTime));
                   TDateTime(pToken^) := Value;
                 end;
  end;
end;

procedure TmafVariable.__SetAsCallback(const Value: TNotifyEvent);
begin
  Case FType Of
    vtString,
    vtInteger,
    vtPointer,
    vtObject,
    vtBoolean,
    vtDateTime : Raise Exception.Create('Impossible data conversation!');
    vtCallback : FCallback := Value;
  end;
end;

function TmafVariable.__GetAsCallback: TNotifyEvent;
begin
  Case FType Of
    vtString,
    vtInteger,
    vtPointer,
    vtObject,
    vtBoolean,
    vtDateTime : Raise Exception.Create('Impossible data conversation!');
    vtCallback : Result := FCallback;
  end;
end;

function TmafVariable.__GetAsDateTime: TDateTime;
begin
  Case FType Of
    vtDateTime : Result := TDateTime(pToken^);
  end;
end;

{ TmafParameter }

constructor TmafParameter.Create;
begin
  FpVarList := TList.Create;
end;

destructor TmafParameter.Destroy;
begin
  Clear;
  FpVarList.Free;
  inherited;
end;

procedure TmafParameter.LoadFromStream(aStream: TStream);
var FpStreamer : TTemplateStreamer;
begin
  FpStreamer := TTemplateStreamer.Create;
  FpStreamer.OnStreamReadAttribute := __ReadStreamData;
  FpStreamer.Attributes := 1;
  FpStreamer.StreamVersion := 1;
  FpStreamer.ReadStream(TMemoryStream(aStream));
//  aStream.Position := 0;
  FpStreamer.Free;
end;

procedure TmafParameter.SaveToStream(aStream: TStream);
var FpStreamer : TTemplateStreamer;
begin
  FpStreamer := TTemplateStreamer.Create;
  FpStreamer.OnStreamWriteAttribute := __WriteStreamData;
  FpStreamer.Attributes := 1;
  FpStreamer.StreamVersion := 1;
  FpStreamer.WriteStream(25879, TMemoryStream(aStream));
//  aStream.Position := 0;
  FpStreamer.Free;
end;

procedure TmafParameter.ReadParameters(aStream: TStream);
begin
  LoadFromStream(aStream);
end;

procedure TmafParameter.WriteParameters(aStream: TStream);
begin
  SaveToStream(aStream);
end;

function TmafParameter.__GetByIndex(Index: Integer): TmafVariable;
begin
  Result := nil;
  If FpVarList.Count > 0 Then
    If ((Index > -1) And (Index < FpVarList.Count)) Then
      Result := TmafVariable(FpVarList.Items[Index]);
{  If Result = nil Then
    Raise Exception.Create('TmafParameter.__GetByIndex: Index out of bounds!');}
end;

function TmafParameter.__GetByName(ParamName: String): TmafVariable;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpVarList.Count - 1 Do
    If TmafVariable(FpVarList.Items[i]).FsName = ParamName Then begin
      Result := TmafVariable(FpVarList.Items[i]);
      Break;
    end;
{  If Result = nil Then
    Raise Exception.Create('TmafParameter.__GetByName: ParamName not found!');}
end;

function TmafParameter.__GetCount: Integer;
begin
  Result := FpVarList.Count;
end;

procedure TmafParameter.__ReadStreamData(Sender: TObject; ID: Integer);
var i, j, idx, k : Integer;
    S : String;
    bType : Byte;
    dtVal : TDateTime;
    boolVal : Boolean;
begin
  If TTemplateStreamer(Sender).StreamID = 25879 Then begin
    TTemplateStreamer(Sender).ReadInteger(j);
    For i := 1 To j Do begin
      TTemplateStreamer(Sender).ReadString(S);
      TTemplateStreamer(Sender).ReadByte(bType);
      Case bType Of
        sdtString  : begin
                       idx := Add(S, vtString);
                       TTemplateStreamer(Sender).ReadString(S);
                       Params[idx].AsString := S;
                     end;
        sdtInteger : begin
                       idx := Add(S, vtInteger);
                       TTemplateStreamer(Sender).ReadInteger(k);
                       Params[idx].AsInteger := k;
                     end;
        sdtBoolean : begin
                       idx := Add(S, vtBoolean);
                       TTemplateStreamer(Sender).ReadBoolean(boolVal);
                       Params[idx].AsBoolean := boolVal;
                     end;
        sdtDateTime: begin
                       idx := Add(S, vtDateTime);
                       TTemplateStreamer(Sender).ReadDateTime(dtVal);
                       Params[idx].AsDateTime := dtVal;
                     end;
        sdtPointer : begin
                       idx := Add(S, vtPointer);
                     end;
        sdtObject  : begin
                       idx := Add(S, vtObject);
                     end;
        sdtObjectFunction : idx := Add(S, vtCallback);
      end;
      If TTemplateStreamer(Sender).StreamVersion > 0 Then begin
        TTemplateStreamer(Sender).ReadByte(bType);
        Case bType Of
          1 : TmafVariable(FpVarList.Items[idx]).FVarParamType := vptIn;
          2 : TmafVariable(FpVarList.Items[idx]).FVarParamType := vptOut;
        end;  //  --  Case bType Of
      end;  //  --  If TTemplateStreamer(Sender).StreamVersion > 0 Then
    end;  //  --  For i := 1 To j Do
  end;  //  --  If TTemplateStreamer(Sender).StreamID = 25879 Then
end;

procedure TmafParameter.__WriteStreamData(Sender: TObject; ID: Integer);
var i : Integer;
    b : Byte;
begin
  If TTemplateStreamer(Sender).StreamID = 25879 Then begin
    TTemplateStreamer(Sender).WriteInteger(FpVarList.Count);
    For i := 0 To FpVarList.Count - 1 Do begin
      TTemplateStreamer(Sender).WriteString(TmafVariable(FpVarList.Items[i]).VarName);
      Case TmafVariable(FpVarList.Items[i]).FType Of
        vtString   : begin
                       TTemplateStreamer(Sender).WriteByte(sdtString);
                       TTemplateStreamer(Sender).WriteString(String(PChar(TmafVariable(FpVarList.Items[i]).pToken)));
                     end;
        vtInteger  : begin
                       TTemplateStreamer(Sender).WriteByte(sdtInteger);
                       TTemplateStreamer(Sender).WriteInteger(Integer(TmafVariable(FpVarList.Items[i]).pToken));
                     end;
        vtBoolean  : begin
                       TTemplateStreamer(Sender).WriteByte(sdtBoolean);
                       TTemplateStreamer(Sender).WriteBoolean(TmafVariable(FpVarList.Items[i]).pToken <> nil);
                     end;
        vtDateTime : begin
                       TTemplateStreamer(Sender).WriteByte(sdtDateTime);
                       TTemplateStreamer(Sender).WriteDateTime(TDateTime(TmafVariable(FpVarList.Items[i]).pToken^));
                     end;
        vtPointer  : begin
                       TTemplateStreamer(Sender).WriteByte(sdtPointer);
                     end;
        vtObject   : begin
                       TTemplateStreamer(Sender).WriteByte(sdtObject);
                     end;
        vtCallback : begin
                       TTemplateStreamer(Sender).WriteByte(sdtObjectFunction);
                     end;
      end;
      Case TmafVariable(FpVarList.Items[i]).FVarParamType Of
        vptIn  : b := 1;
        vptOut : b := 2;
      end;
      TTemplateStreamer(Sender).WriteByte(b);
    end;
  end;
end;

function TmafParameter.Add(AName: String; AType: TVarType): Integer;
var pToken : TmafVariable;
begin
  pToken := ParamByName[AName];
  If pToken = nil Then begin
    pToken := TmafVariable.Create;
    pToken.FsName := AName;
    pToken.FType := AType;
    pToken.FVarParamType := vptIn;
    pToken.FIndex := FpVarList.Add(pToken);  // returns index of the variable
  end;
  Result := pToken.Index;
end;

procedure TmafParameter.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Parameters', ReadParameters, WriteParameters, True);
end;

procedure TmafParameter.Delete(idx: Integer);
var pToken: TmafVariable;
begin
  If ((idx > -1) And (idx < FpVarList.Count)) Then begin
    pToken := TmafVariable(FpVarList.Items[idx]);
    pToken.Free;
    FpVarList.Delete(idx);
  end;
end;

procedure TmafParameter.Clear;
var i: Integer;
    aVar : TmafVariable;
begin
  For i := 0 To FpVarList.Count - 1 Do begin
    aVar := TmafVariable(FpVarList.Items[i]);
    aVar.Free;
  end;  //  --  For i := 0 To FpVarList.Count - 1 Do
  FpVarList.Clear;
end;

initialization
  gManagerLoader := nil;

end.
