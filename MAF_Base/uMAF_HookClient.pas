{*******************************************************************************
Name         : uMAF_HookClient.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2002-2014 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 25.02.2002
Last Update  : 11.10.2014
Version      : 1.2.041
Last Changes :

1.2.041 (11.10.2014) -----------------------------------------------------------
- [ADD] most TemplateStreamer ReadXXX got an overloade function to directly
        return the value instead of a var-parameter
1.2.040 (26.01.2012) -----------------------------------------------------------
- [ADD] added DeleteTemplate
1.2.039 (07.11.2011) -----------------------------------------------------------
- [FIX] in LoadTemplate and SaveTemplate (the version with parameters) the
        original stream will be restored when another stream was used
- [FIX] in LoadTemplate and SaveTemplate (the versions without parameters) will
        always use the internal stream and switch the used stream in the
        QueryToken to the original internal stream
- [FIX] fixed the propert setter __SetHookClientOptions which was calling
        with an "old" method to TmafHookManager, causing him to do nothing
        therefore in runtime enabled/disabled hcoProcessOnIdle did actually
        nothing. That is fixed now
1.2.038 (03.11.2011) -----------------------------------------------------------
- [ADD] added ErrorCode as Result to LoadTemplate and SaveTemplate
1.2.037 (08.09.2010) -----------------------------------------------------------
- [FIX] fixed a bug in TmafHookClient.Destroy, where a "List out of bounds"
        occured when trying to delete a registered callback due to the fact
        that UnregisterCallBack was called, where the element already was deleted
- [ADD] added overloaded version of GetGlobalInteger, that returns the variable
        as function result
1.2.036 (30.08.2010) -----------------------------------------------------------
- [CHG] changed all calls to the manager from HM_QUERY to MT_ROUTER
- [CHG] Parameter is now a published property and shows the count of variables
        declared through the new editor
1.2.035 (28.07.2010) -----------------------------------------------------------
- [ADD] change of the parameter stack to a new Object TmafParameter, which
        supports named params and indexed params (pretty much like used in
        database queries etc.)
- [FIX] fixed a bug, where the FreeMemFunc was only called, when an event was
        assigned to handle AfterXXXX
1.2.034 (06.07.2010) -----------------------------------------------------------
- [ADD] added a new (overloaded) GetGlobalString, which returns the variable
        value directly as function result instead through a var parameter. The
        old method is still available, but declared as deprecated and will be
        removed in the future
1.2.033 (02.06.2010) -----------------------------------------------------------
- [CHG] moved TVariableStack to uMAF_Core.pas to reuse it without adding
        uMAF_HookClient.pas to the uses clause
- [ADD] new HookClientOption hcoClearParamStack to clear the Parameter stack
        after ExecuteHook and ExecuteUniqueID
- [ADD] Parameter stack introduced, which works pretty much as the variable stack
        but will be given through the QHS^.pParams to the called function(s)
1.2.032 (31.05.2010) -----------------------------------------------------------
- [ADD] support for OnIdle events from the application :
        - new Event OnIdle, which is fired, when the TmafHookClient is registered
          to accept those events
        - HookClientOptions.hcoProcessOnIdle to attach on or detach from the
          OnIdle event handler   
1.2.031 (28.04.2010) -----------------------------------------------------------
- [ADD] new function ExecuteUniqueID to execute a SubHook through its UniqueID
1.2.030 (25.03.2010) -----------------------------------------------------------
- [FIX] LoadTemplate and SaveTemplate didn't set the StreamID from the parameters
        into the QueryToken and prevented the system from loading / saving streams
        through their ID. That is fixed now.
1.2.029 (01.10.2009) -----------------------------------------------------------
- [ADD] added new method to query the HookManager, that will step by step remove
        the need for the DirectRouterCall. It also uses 2 published events,
        "BeforeQuery" and "AfterQuery"
- [ADD] added new method to query manager that don't have an own client like
        TmafGlobalVars, TmafDataStorage or TmafUserSecurity
- [CHG] changes to the handling to set global variables due to the changes in
        the component. Supports now read-only variables 
1.2.028 (13.09.2009) -----------------------------------------------------------
- [CHG] moved the TemplateStreamerClass due to its popularity to the unit
        uMAF_TemplateStreamer.pas so that not everyone has to put the HookClient
        into the uses clause, which also could cause problems
1.2.027 (30.08.2009) -----------------------------------------------------------
- [ADD] added support to register/unregister a notification for changes in
        a global variable
1.2.026 (24.07.2009) -----------------------------------------------------------
- [FIX] fixed a bug in ExecuteHook and DirectRouterCall, where the pFreeMemProc
        wasn't set to nil and also the pChildObj wasn't set to nil.
1.2.025 (09.07.2009) -----------------------------------------------------------
- [ADD] TemplateStreamer got its own PTemplateQuery token and sets all values
        by itself through properties (TemplateID, Name and Category), assigns
        the stream used (internal or external) and the token can be used to be
        sent to the TemplateManager. It can be accessed through the public
        property TemplateQueryToken.
- [ADD] TTemplateStreamer.ResetQueryToken added wich resets the values of the token
        to default values
- [CHG] TmafHookClient.LoadTemplate and TmafHookClient.SaveTemplate now use the
        internal PTemplateQuery token and don't create their own anymore
1.2.024 (27.06.2009) -----------------------------------------------------------
- [ADD] added Streamer events to TTemplateStreamer itself, wich are used
        when no HookClient is assigned. Now it can be used as a standalone,
        too. If no HookClient is assigned, the events must be set within the code
        when creating the component. If a HookClient is set, the internal events
        are overwritten with the events set in the HookClient component
- [CHG] ReadStream/WriteStream in TTemplateStreamer don't use HookClient.PushVar
        and HookClient.PopVar anymmore to save the internal stream pointer when
        they are forced to use another stream. Now the TTemplateStreamer is
        independent fro the HookClient
1.2.023 (20.06.2009) -----------------------------------------------------------
- [ADD] support for the pFreeMemFunc in the QueryHandlerStruct. It's called after
        executing the AfterCallRouter event after executing the whole Hook, if the
        event is assigned and marked for execution.
        It is also executed after the AfterDirectRouterCall event in
        TmafHookClient.DirectRouterCall
1.2.022 (15.06.2009) -----------------------------------------------------------
- [ADD] New Even "TmafHookClient.OnDFTChanged" wich is triggered by the HookManager
        whenever the Dynamic Function Table (DFT) is changed after the initial
        load. During loading the program this event wont be triggered
1.2.021 (03.11.2008) -----------------------------------------------------------
- [ADD] New class TTemplateStreamer that should help to write Template streams
        with Delphi 2009 and versions below
1.2.020 (27.10.2008) -----------------------------------------------------------
- [ADD] LogErrorMessage, LogInfoMessage and LogWarningMessage, that the programmer
        doesn't have to include ERP_Globals.pas to send a log message
- [CHG] rewrite of LogMessage method to support TERPLogger components
1.2.019 (12.10.2008) -----------------------------------------------------------
- [ADD] GetTemplateID added to ask just for the ID of a Template by giving its
        category and name
1.2.018 (26.03.2007) -----------------------------------------------------------
- [CHG] changed the template support to use the new TERPTemplate component
1.2.017 (06.03.2007) -----------------------------------------------------------
- [CHG] changed definition for events BeforeExecHook and AfterExecHook, they
        return now error code for every SubHook they call
1.2.016 (02.03.2007) -----------------------------------------------------------
- [ADD] added global variable support for global string and integer values, wich
        can also be saved at the end of the session and restored next time the
        program starts
1.2.015 (28.02.2007) -----------------------------------------------------------
- [CHG] Changed from TBaseRouterConnector to the new base class
        TERPBaseComponent, wich connects through TManagerBaseContainer
1.1.014 (18.02.2007) -----------------------------------------------------------
- [FIX] fixed a bug that leftover-stack tokens where not really released when
        destroying the stack. It's not really a bug, bc there shouldn't be any
        items on the stack in the first place, but obviously it can happen, if
        a "programmer" forgets to POP after PUSH
1.1.013 (16.02.2007) -----------------------------------------------------------
- [ADD] added stack for variable, wich makes it easier to pass parameters to
        the events. Idea is, if you execute a hook and you have to set variables
        inside the BeforeCallRouter i.e., but you don't have them handy without
        declaring a variable for them inside your object, then you can push them
        to the stack and pop them in BeforeCallRouter event.
        The stack works like a normal stack, when pushing, new variables will be
        added on the top of the stack, when popping, they will be released and
        they are NOT available anymore. So be careful in your code
        Usage : MyInt := TmafHookClient.PopVar.AsInteger;
                TmafHookClient.PushVar.AsString := MyString;
1.1.012 (14.02.2007) -----------------------------------------------------------
- [ADD] added public events BeforeDirectRouterCall and AfterDirectRouterCall,
        wich cannot be assigned in DesignTime and are ONLY for other components,
        who needs to communicate with the router
- [CHG] CallSpecialFunction to DirectRouterCall to match with the event names
- [ADD] EventStack, that components have an easy way to save and restore events
1.1.011 (13.01.2007) -----------------------------------------------------------
- [ADD] start adding PageSecurity feature
1.1.010 (15.09.2006) -----------------------------------------------------------
- [CHG] update to use TBaseRouterConnector as base component
1.0.009 (29.08.2004) -----------------------------------------------------------
- [ADD] added ServiceFunc property, so the main program cana sk for it
1.0.008 (23.05.2004) -----------------------------------------------------------
- [ADD] LoadTemplate and SaveTemplate-support
1.0.007 (26.04.2004) -----------------------------------------------------------
- [ADD] added LogMessage support
1.0.006-------------------------------------------------------------------------
- [DEL] removed Dispatch
- [ADD] communication now through VCL-Message-Handling
1.0.005-------------------------------------------------------------------------
- [CHG] Connect/DisConnect-Handler-Request using now Dispatch
1.0.004-------------------------------------------------------------------------
- [ADD] added GetSubHookList
1.0.003-------------------------------------------------------------------------
- [FIX] fixed bug in Set_RouterServiceFunc, IsLoaded is set correct now and
        OnConnect will be fired
1.0.002-------------------------------------------------------------------------
- [ADD] added SessionID
1.0.001-------------------------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_HookClient;

interface

uses Windows, Forms, Classes, SysUtils, Messages,
     // Modular Application Framework Components units
     uMAF_Core, uMAF_Globals, uMAF_TemplateStreamer, uMAF_Parameters;

Type TEventStackToken = packed record
       FBeforeExecHook         : TExecuteHook;
       FAfterExecHook          : TExecuteHook;
       FBeforeCallRouter       : TCallRouter;
       FAfterCallRouter        : TCallRouter;
       FBeforeDirectRouterCall : TCallRouter;
       FAfterDirectRouterCall  : TCallRouter;
       FBeforeQuery            : TCallRouter;
       FAfterQuery             : TCallRouter;
     end; // TEventStackToken
     PEventStackToken = ^TEventStackToken;

     TmafHookClient = Class(TmafBaseComponent)
     private
       FBeforeExecHook,
       FAfterExecHook     : TExecuteHook;
       FBeforeCallRouter,
       FAfterCallRouter   : TCallRouter;
       // internal events for connected components, DO NOT USE unless from another component, wich uses the HookClient
       FBeforeDirectRouterCall : TCallRouter;
       FAfterDirectRouterCall : TCallRouter;
       FBeforeQuery : TCallRouter;
       FAfterQuery : TCallRouter;
       // Streamer events
       FOnStreamVersionError : TOnStreamVersionError;
       FOnStreamVersion : TNotifyEvent;
       FOnStreamReadAttribute : TOnAttribute;
       FOnStreamWriteAttribute : TOnAttribute;
       FpGlobalVarNotify : TStringList;
       FpEventStack : TList;
       FpVariableStack : TVariableStack;
       FpParameters : TmafParameters;
       FpStreamer : TTemplateStreamer;
       FOnDFTChanged : TNotifyEvent;
       FOnGlobalVarChange : TGlobalVarNotify;
       FOnGlobalVarDeleted : TGlobalVarNotify;
       FOnFunctionObserver : TFunctionObserverCallBack;
       FHookClientOptions : THookClientOptions;
       FOnIdle : TNotifyEvent;
       FOnIdleNotifyList : TList;
       function GetSessionID: Integer;
       function Get(Index: Integer): PVarStackToken; overload;
       function __GetLastVar: Integer;
       procedure __SetHookClientOptions(const Value: THookClientOptions);
     protected
       procedure MSG_IsManagerType(var Msg: TMessage); message MSG_MANAGERTYPE;
       procedure MSG_DFTChanged(var Msg: TMessage); message MSG_DFT_CHANGE;
       procedure MSG_GlobalVar_Change(var Msg: TMessage); message MSG_GLOBALVAR_CHANGE;
       procedure MSG_FunctionObserver_Add(var Msg: TMessage); message MSG_FUNCTION_OBSERVER_ADD;
       procedure MSG_FunctionObserver_Delete(var Msg: TMessage); message MSG_FUNCTION_OBSERVER_DEL;
       procedure MSG_OnIdle(var Msg: TMessage); message MSG_ON_IDLE;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       function ExecuteHook(HookID: Integer; SubHookID: Integer = 0; bClearEvents: Boolean = False; EventFlags: Byte = 15): Integer;
       function ExecuteUniqueID(uID: Integer; EventFlags: Byte = 15): Integer;
       procedure PushEvents;
       procedure PopEvents;
       function PushVar: TVariableStack;
       function PopVar: TVariableStack;
//       function Parameter: TmafParameter;
       // Non-Hook-Funktions... DirectRouterCalls
       function DirectRouterCall(nCommand : Integer) : Integer;
       procedure GetSubHookList(HookNr: Integer; const List: TStrings);
       function __Query_HookManager(nCommand: Integer): Integer;
       function __Query_Manager(nManagerQuery, nCommand : Integer): Integer;
       function __Add_FunctionObserver(nHookID, nSubHookID: Integer; AReceiver: TComponent = nil): Integer;
       function __Delete_FunctionObserver(nHookID, nSubHookID: Integer; AReceiver: TComponent = nil): Integer;
       // error log support
       function LogMessage(aLogType: TLogType; Msg: String): Integer;
       function LogInfoMessage(Msg: String): Integer;
       function LogErrorMessage(Msg: String): Integer;
       function LogWarningMessage(Msg: String): Integer;
       // template support
       function LoadTemplate(var nID: Integer; Category, TemplateName: String; aStream: TMemoryStream = nil): Integer; overload;
       function SaveTemplate(var nID: Integer; Category, TemplateName: String; aStream: TMemoryStream = nil): Integer; overload;
       function LoadTemplate: Integer; overload;
       function SaveTemplate: Integer; overload;
       function DeleteTemplate(Category, TemplateName: String): Integer;
       function GetTemplateID(Category, TemplateName: String): Integer;
       procedure __FreeQueryNames(pData: Pointer);
       // global variable support
       function GetGlobalInteger(VarName: String; var Value: Integer): Integer; overload;
       function GetGlobalInteger(VarName: String): Integer; overload;
       procedure SetGlobalInteger(VarName: String; Value: Integer; VarFlags: TVarFlags=[]; ALoadSave: TLoadSave = lsINIFile);
       function GetGlobalString(VarName: String; var Value: String): Integer; overload; deprecated;
       function GetGlobalString(VarName: String): String; overload;
       procedure SetGlobalString(VarName: String; Value: String; VarFlags: TVarFlags=[]; ALoadSave: TLoadSave = lsINIFile);
       function DeleteGlobalVar(VarName: String): Integer;
       procedure SetGlobalObject(VarName: String; Value: Pointer);
       function GetGlobalObject(VarName: String; var Value: Pointer): Integer;
       procedure SetGlobalBoolean(VarName: String; Value: Boolean; VarFlags: TVarFlags=[]; ALoadSave: TLoadSave = lsINIFile);
       function GetGlobalBoolean(VarName: String; var Value: Boolean): Integer; overload;
       function GetGlobalBoolean(VarName: String): Boolean; overload;
       function RegisterCallBack(VarName: String): Integer;
       function UnRegisterCallBack(VarName: String): Integer;
       // OnIdle Notify function
       procedure RegisterOnIdleNotifier(AComponent: TComponent);
       procedure UnregisterOnIdleNotifier(AComponent: TComponent);
       // properties
       property SessionID : Integer read GetSessionID;
       property VarStack[Index: Integer]: PVarStackToken read Get;
       property LastVar : Integer read __GetLastVar;
       // events
       property BeforeDirectRouterCall : TCallRouter read FBeforeDirectRouterCall write FBeforeDirectRouterCall;
       property AfterDirectRouterCall : TCallRouter read FAfterDirectRouterCall write FAfterDirectRouterCall;
     published
       property Parameter : TmafParameters read FpParameters write FpParameters;
       property Streamer : TTemplateStreamer read FpStreamer write FpStreamer;
       property HookClientOptions : THookClientOptions read FHookClientOptions write __SetHookClientOptions default [];
       property BeforeExecHook : TExecuteHook read FBeforeExecHook write FBeforeExecHook;
       property AfterExecHook : TExecuteHook read FAfterExecHook write FAfterExecHook;
       property BeforeCallRouter : TCallRouter read FBeforeCallRouter write FBeforeCallRouter;
       property AfterCallRouter : TCallRouter read FAfterCallRouter write FAfterCallRouter;
       property OnDFTChanged : TNotifyEvent read FOnDFTChanged write FOnDFTChanged;
       property OnGlobalVarChange : TGlobalVarNotify read FOnGlobalVarChange write FOnGlobalVarChange;
       property OnGlobalVarDeleted : TGlobalVarNotify read FOnGlobalVarDeleted write FOnGlobalVarDeleted;
       property BeforeQuery : TCallRouter read FBeforeQuery write FBeforeQuery;
       property AfterQuery : TCallRouter read FAfterQuery write FAfterQuery;
       property OnFunctionObserver : TFunctionObserverCallBack read FOnFunctionObserver write FOnFunctionObserver;
       property OnIdle : TNotifyEvent read FOnIdle write FOnIdle;
       // Streamer events
       property OnStreamVersionError : TOnStreamVersionError read FOnStreamVersionError write FOnStreamVersionError;
       property OnStreamReadAttribute : TOnAttribute read FOnStreamReadAttribute write FOnStreamReadAttribute;
       property OnStreamWriteAttribute : TOnAttribute read FOnStreamWriteAttribute write FOnStreamWriteAttribute;
       property OnStreamVersion : TNotifyEvent read FOnStreamVersion write FOnStreamVersion;
     end;

implementation

uses uMAF_Tools;

{ TmafHookClient }

// ********************************* Comments **********************************
// Description : component create
// Param (in)  : AOwner=Owner component
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 15.02.2007
// Last Update : 30.08.2009
// *****************************************************************************
constructor TmafHookClient.Create(AOwner: TComponent);
begin
  inherited;
  AManagerQueryID := MT_ROUTER;
  FpGlobalVarNotify := nil;                 // will be initialized when an entry is added
  FManagerType := MT_ROUTER;
  FpEventStack := TList.Create;
  FpVariableStack := TVariableStack.Create;
  FpStreamer := TTemplateStreamer.Create;
  ClientOptions := [coRegisterSelf];        // HookClient has to register itself in the HookManager
  FHookClientOptions := [];                 
end; // Create

// ********************************* Comments **********************************
// Description : component destroy
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 15.02.2007
// Last Update : 08.09.2010
// *****************************************************************************
destructor TmafHookClient.Destroy;
begin
  If FOnIdleNotifyList <> nil Then
    FOnIdleNotifyList.Free;         // no need to free the pointer as they're components that will be freed automatically
  FOnIdleNotifyList := nil;
  HookClientOptions := [];          // causes to unregister from the OnIdle
  If Assigned(FpGlobalVarNotify) Then begin
    While FpGlobalVarNotify.Count > 0 Do begin
      UnRegisterCallBack(FpGlobalVarNotify.Strings[0]);
//      FpGlobalVarNotify.Delete(0);
    end;  //  --  While FpGlobalVarNotify.Count > 0 Do
    FreeAndNil(FpGlobalVarNotify);
  end;  //  --  If Assigned(FpGlobalVarNotify) Then 

  While FpEventStack.Count > 0 Do begin
    Dispose(PEventStackToken(FpEventStack.Items[0]));
    FpEventStack.Delete(0);
  end;  //  --  While FpEventStack.Count > 0 Do
  FpEventStack.Free;
  FpVariableStack.Free;
  FpStreamer.Free;
  inherited;
end; // Destroy

// ********************************* Comments **********************************
// Description : component Loaded
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 09.07.2009
// Last Update : 09.07.2009
// *****************************************************************************
procedure TmafHookClient.Loaded;
begin
  inherited;
  FpStreamer.HookClient := Self;
  HookClientOptions := FHookClientOptions; // causes the hookclient to register OnIdle
end;

function TmafHookClient.ExecuteHook(HookID: Integer; SubHookID: Integer = 0; bClearEvents: Boolean = False; EventFlags: Byte = 15): Integer;
var QHS : pQHS;
    UserPointer : Pointer;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.pParams := FpParameters;
  UserPointer := nil;
  QHS^.HookID := HookID;
  QHS^.SubHookID := SubHookID; // if 0, all functions will be called
  QHS^.CallerComponent := Self;
//  QHS^.FBeforeExecHook := FBeforeExecHook;
//  QHS^.FAfterExecHook := FAfterExecHook;
  // Bit 0 = BeforeCallRouter (1)        All          = 1+2+4+8=15
  // Bit 1 = AfterCallRouter  (2)        only BCR+ACR = 1+2=3
  // Bit 2 = BeforeExecHook   (4)        only BEH+AEH = 4+8=12
  // Bit 3 = AfterExecHook    (8)
  QHS^.EventFlags := EventFlags;

  If (EventFlags And 1=1) Then
    If Assigned(FBeforeCallRouter) Then
      FBeforeCallRouter(HookID, QHS, UserPointer);

  Result := CallManager(HM_EXEC_HOOK, QHS, UserPointer);

  If (EventFlags And 2=2) Then
    If Assigned(FAfterCallRouter) Then
      FAfterCallRouter(HookID, QHS, UserPointer);

  If Assigned(QHS^.pFreeMemFunc) Then begin
    QHS^.pFreeMemFunc(QHS^.pChildObj);
    QHS^.pFreeMemFunc := nil;
    QHS^.pChildObj := nil;
  end;  //  --  If Assigned(QHS^.pFreeMemFunc) Then

  __Free_QueryHandlerStruct(QHS);
  If bClearEvents Then begin
    FBeforeCallRouter := nil;
    FAfterCallRouter := nil;
    FBeforeExecHook := nil;
    FAfterExecHook := nil;
  end;
  If hcoClearParamStack in FHookClientOptions Then
    FpParameters.Data.Clear;
end; // ExecuteHook

function TmafHookClient.ExecuteUniqueID(uID: Integer; EventFlags: Byte): Integer;
var QHS : pQHS;
    UserPointer : Pointer;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.pParams := FpParameters;
  UserPointer := nil;
  QHS^.uID := uID;
  QHS^.CallerComponent := Self;
//  QHS^.FBeforeExecHook := FBeforeExecHook;
//  QHS^.FAfterExecHook := FAfterExecHook;
  // Bit 0 = BeforeCallRouter (1)        All          = 1+2+4+8=15
  // Bit 1 = AfterCallRouter  (2)        only BCR+ACR = 1+2=3
  // Bit 2 = BeforeExecHook   (4)        only BEH+AEH = 4+8=12
  // Bit 3 = AfterExecHook    (8)
  QHS^.EventFlags := EventFlags;

  If (EventFlags And 1=1) Then
    If Assigned(FBeforeCallRouter) Then
      FBeforeCallRouter(uID, QHS, UserPointer);

  Result := CallManager(HM_EXEC_HOOK, QHS, UserPointer);

  If (EventFlags And 2=2) Then
    If Assigned(FAfterCallRouter) Then
      FAfterCallRouter(uID, QHS, UserPointer);

  If Assigned(QHS^.pFreeMemFunc) Then begin
    QHS^.pFreeMemFunc(QHS^.pChildObj);
    QHS^.pFreeMemFunc := nil;
    QHS^.pChildObj := nil;
  end;  //  --  If Assigned(QHS^.pFreeMemFunc) Then

  __Free_QueryHandlerStruct(QHS);
  If hcoClearParamStack in FHookClientOptions Then
    FpParameters.Data.Clear;
end;

function TmafHookClient.DirectRouterCall(nCommand: Integer): Integer;
var QHS : pQHS;
    UserPointer : Pointer;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.HookID := nCommand;
  QHS^.CallerComponent := Self;
  UserPointer := nil;
  If Assigned(FBeforeDirectRouterCall) Then
    FBeforeDirectRouterCall(nCommand, QHS, UserPointer);
  Result := CallManager(nCommand, QHS, UserPointer);
  If Assigned(FAfterDirectRouterCall) Then
    FAfterDirectRouterCall(nCommand, QHS, UserPointer);
  If Assigned(QHS^.pFreeMemFunc) Then begin
    QHS^.pFreeMemFunc(QHS^.pChildObj);
    QHS^.pFreeMemFunc := nil;
    QHS^.pChildObj := nil;
  end;  //  --  If Assigned(QHS^.pFreeMemFunc) Then
  __Free_QueryHandlerStruct(QHS);
end;

function TmafHookClient.__Query_HookManager(nCommand: Integer): Integer;
begin
  Result := __Query_Manager(MT_ROUTER, nCommand);
end;

function TmafHookClient.__Query_Manager(nManagerQuery, nCommand: Integer): Integer;
var QHS : pQHS;
    UserPointer : Pointer;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.HookID := nManagerQuery;
  QHS^.SubHookID := nCommand;
  QHS^.CallerComponent := Self;
  UserPointer := nil;
  If Assigned(FBeforeQuery) Then
    FBeforeQuery(nCommand, QHS, UserPointer);
  Result := CallManager(nManagerQuery, QHS, UserPointer);
  If Assigned(FAfterQuery) Then
    FAfterQuery(nCommand, QHS, UserPointer);
  If Assigned(QHS^.pFreeMemFunc) Then begin
    QHS^.pFreeMemFunc(QHS^.pChildObj);
    QHS^.pFreeMemFunc := nil;
    QHS^.pChildObj := nil;
  end;  //  --  If Assigned(QHS^.pFreeMemFunc) Then
  __Free_QueryHandlerStruct(QHS);
end;

procedure TmafHookClient.__SetHookClientOptions(const Value: THookClientOptions);
var QHS : pQHS;
begin
  If ((csDesigning in ComponentState) Or (csLoading in ComponentState)) Then begin
    FHookClientOptions := Value;
    Exit;
  end;  //  --  If ((csDesigning in ComponentState) Or (csLoading in ComponentState)) Then 

  If FHookClientOptions <> Value Then begin
    QHS := __Create_QueryHandlerStruct;
    QHS^.HookID := MT_ROUTER;
    QHS^.SubHookID := HM_TOGGLE_APPLICATION_IDLE;
    If hcoProcessOnIdle in Value Then
      QHS^.Reserved1 := 1; // 1 to attach, 0 to detach (Reserved1 is by default 0)
    QHS^.pChildObj := Self;  
    __AdvQuery_Manager(HM_TOGGLE_APPLICATION_IDLE, QHS, nil);
//    __AdvQuery_Manager(MT_ROUTER, QHS, nil);
    __Free_QueryHandlerStruct(QHS);
  end;  //  --  If FHookClientOptions <> Value Then
  FHookClientOptions := Value;
end; // __SetHookClientOptions

// ********************************* Comments **********************************
// Description : returns global SessionID
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 01.02.2004
// Last Update : 01.03.2007
// *****************************************************************************
function TmafHookClient.GetSessionID: Integer;
var QHS : pQHS;
begin
  GetMem(QHS, SizeOf(TQueryHandlerStruct));
  CallManager(HM_GET_SESSION_ID, QHS, nil);
  Result := QHS^.ResVal;
  FreeMem(QHS, SizeOf(TQueryHandlerStruct));
end; // GetSessionID

// ********************************* Comments **********************************
// Description : returns if we're a certain ManagerType
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 01.02.2004
// Last Update : 01.03.2007
// *****************************************************************************
procedure TmafHookClient.MSG_IsManagerType(var Msg: TMessage);
begin
  If ((Msg.WParam = MT_ROUTER) Or (Msg.WParam = MT_GLOBAL_VAR_MANAGER)) Then
    Msg.Result := 1
  Else
    Msg.Result := 0;
end; // WM_IsManagerType

// ********************************* Comments **********************************
// Description : MSG_DFTChanged
// Param (in)  : Msg sent by HookManager
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 15.06.2009
// Last Update : 15.06.2009
// *****************************************************************************
procedure TmafHookClient.MSG_DFTChanged(var Msg: TMessage);
begin
  If Assigned(FOnDFTChanged) Then
    FOnDFTChanged(Self);
end; // MSG_DFTChanged

// ********************************* Comments **********************************
// Description : MSG_FunctionObserver, a Subhook in a function observed by this
//               component, has been added
// Param (in)  : Msg sent by TmafHookManager
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 27.10.2009
// Last Update : 27.10.2009
// *****************************************************************************
procedure TmafHookClient.MSG_FunctionObserver_Add(var Msg: TMessage);
begin
  If Assigned(FOnFunctionObserver) Then
    FOnFunctionObserver(Self, Msg.WParam, Msg.LParam, foaAdd);
end;

// ********************************* Comments **********************************
// Description : MSG_FunctionObserver, a Subhook in a function observed by this
//               component, has been deleted
// Param (in)  : Msg sent by TmafHookManager
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 27.10.2009
// Last Update : 27.10.2009
// *****************************************************************************
procedure TmafHookClient.MSG_FunctionObserver_Delete(var Msg: TMessage);
begin
  If Assigned(FOnFunctionObserver) Then
    FOnFunctionObserver(Self, Msg.WParam, Msg.LParam, foaDelete);
end;

procedure TmafHookClient.MSG_OnIdle(var Msg: TMessage);
var i : Integer;
begin
  If hcoProcessOnIdle in HookClientOptions Then
    If Assigned(FOnIdle) Then
      FOnIdle(Self);
  If FOnIdleNotifyList <> nil Then
    For i := 0 To FOnIdleNotifyList.Count - 1 Do
      SendComponentMessage(TComponent(FOnIdleNotifyList.Items[i]), MSG_ON_IDLE, nil, nil);
end;

procedure TmafHookClient.MSG_GlobalVar_Change(var Msg: TMessage);
var VarName : String;
begin
  VarName := String(PChar(Msg.WParam));
  Case Msg.LParam Of
    0 : If Assigned(FOnGlobalVarChange) Then
          FOnGlobalVarChange(Self, VarName);
    1 : If Assigned(FOnGlobalVarDeleted) Then
          FOnGlobalVarChange(Self, VarName);
  end;
end;

// ********************************* Comments **********************************
// Description : GetSubHookList
// Param (in)  : Liste, in welchem die SubHookNr gespeichert werden
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 06.03.2004
// Last Update : 01.03.2007
// *****************************************************************************
procedure TmafHookClient.GetSubHookList(HookNr: Integer; const List: TStrings);
begin
  List.Clear;
  CallManager(HM_GET_SUBHOOK_LIST, Pointer(HookNr), Pointer(List));
end; // GetSubHookList

// ********************************* Comments **********************************
// Description : sends a logmessage to TERPLogger through HookManager
// Param (in)  : aLogType =type of the message
//               Msg      =Msg to log
// Param (out) : Error code
// Coding by   : Helge Lange
// Date        : 26.04.2004
// Last Update : 27.10.2008
// *****************************************************************************
function TmafHookClient.LogMessage(aLogType: TLogType; Msg: String): Integer;
var pData : PLogMessage;
begin
  pData := NewLogMessage;
  StrToPChar(Msg, pData^.Msg);
  Result := CallManager(HM_LOG_MESSAGE, nil, pData);
  FreeLogMessage(pData);
end; // LogMessage

function TmafHookClient.LogErrorMessage(Msg: String): Integer;
begin
  Result := LogMessage(ltError, Msg);
end;

function TmafHookClient.LogInfoMessage(Msg: String): Integer;
begin
  Result := LogMessage(ltInfo, Msg);
end;

function TmafHookClient.LogWarningMessage(Msg: String): Integer;
begin
  Result := LogMessage(ltWarning, Msg);
end;

function TmafHookClient.__Add_FunctionObserver(nHookID, nSubHookID: Integer; AReceiver: TComponent = nil): Integer;
var QHS : pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.Reserved1 := nHookID;
  QHS^.Reserved2 := nSubHookID;
  If AReceiver = nil Then
    AReceiver := Self;
  Result := __AdvQuery_Manager(HM_ADD_FUNCTION_OBSERVER, QHS, AReceiver);
  __Free_QueryHandlerStruct(QHS);
end;

function TmafHookClient.__Delete_FunctionObserver(nHookID, nSubHookID: Integer; AReceiver: TComponent = nil): Integer;
var QHS : pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.Reserved1 := nHookID;
  QHS^.Reserved2 := nSubHookID;
  If AReceiver = nil Then
    AReceiver := Self;
  Result := __AdvQuery_Manager(HM_DELETE_FUNCTION_OBSERVER, QHS, AReceiver);
  __Free_QueryHandlerStruct(QHS);
end;

procedure TmafHookClient.__FreeQueryNames(pData: Pointer);
begin
  If Not Assigned(pData) Then
    Exit;
  If PDataStorageQuery(pData)^.sCategory <> nil Then
    FreePChar(PDataStorageQuery(pData)^.sCategory);
  If PDataStorageQuery(pData)^.sName <> nil Then
    FreePChar(PDataStorageQuery(pData)^.sName);
  PDataStorageQuery(pData)^.pFreeMemFunc := nil;
end;

// ********************************* Comments **********************************
// Description : requests a template ID from the router identified by category and name
// Param (in)  : Category=Category of the template
//               TemplateName=Name of the template
// Param (out) : ID of the template
// Coding by   : Helge Lange
// Date        : 12.10.2008
// Last Update : 12.10.2008
// *****************************************************************************
function TmafHookClient.GetTemplateID(Category, TemplateName: String): Integer;
var pQuery: PDataStorageQuery;
begin
  pQuery := __Create_DataStorageQuery(Category, TemplateName);
  pQuery^.Action := taGetID;
  pQuery^.nID := 0;
  pQuery^.aObj := Pointer(1); // dummy pointer
  pQuery^.pFreeMemFunc := __FreeQueryNames;
  CallManager(HM_LOAD_TEMPLATE, nil, pQuery);
  Result := pQuery^.nID;
  __Free_DataStorageQuery(pQuery);
end; // GetTemplateID

function TmafHookClient.DeleteTemplate(Category, TemplateName: String): Integer;
begin
  Streamer.ResetQueryToken;
  Streamer.DataStorageQueryToken^.Action := taDelete;
  Streamer.TemplateCategory := Category;
  Streamer.TemplateName := TemplateName;
  Result := CallManager(HM_DELETE_TEMPLATE, nil, Streamer.DataStorageQueryToken);
end;

function TmafHookClient.LoadTemplate: Integer;
begin
  Streamer.DataStorageQueryToken^.Action := taRead;
  Streamer.DataStorageQueryToken^.nID := 0;
  Streamer.DataStorageQueryToken^.aObj := Streamer.Stream;  // in this Loadtemplate we always use our internal stream
  Result := CallManager(HM_LOAD_TEMPLATE, nil, Streamer.DataStorageQueryToken);
  FpStreamer.ReadStream;   // reading the stream
end;

function TmafHookClient.SaveTemplate: Integer;
begin
  Streamer.DataStorageQueryToken^.Action := taWrite;
  Streamer.DataStorageQueryToken^.nID := 0;
  Streamer.DataStorageQueryToken^.aObj := Streamer.Stream;  // in this Savetemplate we always use our internal stream
  Result := CallManager(HM_SAVE_TEMPLATE, nil, Streamer.DataStorageQueryToken);
end;

// ********************************* Comments **********************************
// Description : requests a template from the router
// Param (in)  : Category=Category of the template
//               TemplateName=Name of the template
//               aStream=MemoryStream to save the data
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 23.05.2004
// Last Update : 07.11.2011
// *****************************************************************************
function TmafHookClient.LoadTemplate(var nID: Integer; Category, TemplateName: String; aStream: TMemoryStream = nil): Integer;
begin
  Streamer.ResetQueryToken;
  Streamer.DataStorageQueryToken^.Action := taRead;
  Streamer.TemplateCategory := Category;
  Streamer.TemplateName := TemplateName;
  Streamer.DataStorageQueryToken^.nID := nID;
  If aStream <> nil Then
    Streamer.DataStorageQueryToken^.aObj := aStream;
  TMemoryStream(Streamer.DataStorageQueryToken^.aObj).Size := 0;
  Streamer.DataStorageQueryToken^.pFreeMemFunc := __FreeQueryNames;
  Result := CallManager(HM_LOAD_TEMPLATE, nil, Streamer.DataStorageQueryToken);
  nID := Streamer.DataStorageQueryToken^.nID;
  If aStream = nil Then      // it was our internal stream, so we can assume
    FpStreamer.ReadStream    // that the user also uses our internal read methods
  else
    Streamer.DataStorageQueryToken^.aObj := Streamer.Stream; // if we didn't use the internal stream, we need to restore out original stream
end; // LoadTemplate

// ********************************* Comments **********************************
// Description : saves a template through the TERPTemplate within the router
// Param (in)  : Category=Category of the template
//               TemplateName=Name of the template
//               aStream=MemoryStream to save the data
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 23.05.2004
// Last Update : 07.11.2011
// *****************************************************************************
function TmafHookClient.SaveTemplate(var nID: Integer; Category, TemplateName: String; aStream: TMemoryStream): Integer;
begin
  Streamer.ResetQueryToken;
  Streamer.DataStorageQueryToken^.Action := taWrite;
  Streamer.TemplateCategory := Category;
  Streamer.TemplateName := TemplateName;
  Streamer.DataStorageQueryToken^.nID := nID;
  Streamer.DataStorageQueryToken^.pFreeMemFunc := __FreeQueryNames;
  If aStream <> nil Then
    Streamer.DataStorageQueryToken^.aObj := aStream;
  Result := CallManager(HM_SAVE_TEMPLATE, nil, Streamer.DataStorageQueryToken);
  nID := Streamer.DataStorageQueryToken^.nID;
  Streamer.DataStorageQueryToken^.aObj := Streamer.Stream; // restore our internal stream
end; // SaveTemplate

// ********************************* Comments **********************************
// Description : all events will be saved on the stack
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 15.02.2007
// Last Update : 01.10.2009
// *****************************************************************************
procedure TmafHookClient.PushEvents;
var pToken : PEventStackToken;
begin
  New(pToken);
  pToken^.FBeforeExecHook := FBeforeExecHook;
  FBeforeExecHook := nil;
  pToken^.FAfterExecHook := FAfterExecHook;
  FAfterExecHook := nil;
  pToken^.FBeforeCallRouter := FBeforeCallRouter;
  FBeforeCallRouter := nil;
  pToken^.FAfterCallRouter := FAfterCallRouter;
  FAfterCallRouter := nil;
  pToken^.FBeforeDirectRouterCall := FBeforeDirectRouterCall;
  FBeforeDirectRouterCall := nil;
  pToken^.FAfterDirectRouterCall := FAfterDirectRouterCall;
  FAfterDirectRouterCall := nil;
  pToken^.FBeforeQuery := FBeforeQuery;
  pToken^.FBeforeQuery := nil;
  pToken^.FAfterQuery := FAfterQuery;
  pToken^.FAfterQuery := nil;
  FpEventStack.Add(pToken);
end; // PushEvents

// ********************************* Comments **********************************
// Description : loads last pushed events from the stack and removes then from there
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 15.02.2007
// Last Update : 01.10.2009
// *****************************************************************************
procedure TmafHookClient.PopEvents;
var pToken : PEventStackToken;
begin
  If FpEventStack.Count > 0 Then begin
    pToken := PEventStackToken(FpEventStack.Items[FpEventStack.Count-1]);
    FBeforeExecHook := pToken^.FBeforeExecHook;
    FAfterExecHook := pToken^.FAfterExecHook;
    FBeforeCallRouter := pToken^.FBeforeCallRouter;
    FAfterCallRouter := pToken^.FAfterCallRouter;
    FBeforeDirectRouterCall := pToken^.FBeforeDirectRouterCall;
    FAfterDirectRouterCall := pToken^.FAfterDirectRouterCall;
    FBeforeQuery := pToken^.FBeforeQuery;
    FAfterQuery := pToken^.FAfterQuery;
    Dispose(pToken);
    FpEventStack.Delete(FpEventStack.Count-1);
  end;  //  --  If FpEventStack.Count > 0 Then
end; // PopEvents

// ********************************* Comments **********************************
// Description : returns the variable stack for pushging a variable
// Param (in)  : N/A
// Param (out) : FpVariableStack
// Coding by   : Helge Lange
// Date        : 16.02.2007
// Last Update : 16.02.2007
// *****************************************************************************
function TmafHookClient.PushVar: TVariableStack;
begin
  Result := FpVariableStack;
end; // PushVar

// ********************************* Comments **********************************
// Description : returns the variable stack for popping a variable
// Param (in)  : N/A
// Param (out) : FpVariableStack
// Coding by   : Helge Lange
// Date        : 16.02.2007
// Last Update : 16.02.2007
// *****************************************************************************
function TmafHookClient.PopVar: TVariableStack;
begin
  Result := FpVariableStack;
end; // PopVar

function TmafHookClient.Get(Index: Integer): PVarStackToken;
begin
  If ((Index < 0) Or (Index > FpVariableStack.Stack.Count -1)) Then begin
    Raise EComponentError.Create('TVariableStack: Index out of range !');
    Exit;
  end;

  Result := FpVariableStack.Stack.Items[Index];
end;

function TmafHookClient.__GetLastVar: Integer;
begin
  Result := FpVariableStack.Stack.Count - 1;
end;

{function TmafHookClient.Parameter: TmafParameter;
begin
  Result := FpParameterStack;
end;}



// ********************************* Comments **********************************
// Description : reads a global integer variable
// Param (in)  : VarName=unique name of the variable
//               Value=Value saved in this variable
// Param (out) : ErrCode
// Coding by   : Helge Lange
// Date        : 02.03.2007
// Last Update : 22.10.2008
// *****************************************************************************
function TmafHookClient.GetGlobalInteger(VarName: String; var Value: Integer): Integer;
var pData: PVarRec;
begin
  pData := CreateVarRec(VarName, [], lsNone);
  pData^.Typ := vtInteger;
  Result := CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_GET_INTEGER), pData);
  If Result = ERR_NO_ERROR Then
    Value := Integer(pData^.Var1);
  FreeVarRec(pData);
end; // GetGlobalInteger

function TmafHookClient.GetGlobalInteger(VarName: String): Integer;
var nVar : Integer;
begin
  nVar := -1;
  If GetGlobalInteger(VarName, nVar) = ERR_NO_ERROR Then
    Result := nVar;
end;

function TmafHookClient.GetGlobalBoolean(VarName: String; var Value: Boolean): Integer;
var pData: PVarRec;
begin
  Value := False;
  pData := CreateVarRec(VarName, [], lsNone);
  pData^.Typ := vtBoolean;
  Result := CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_GET_BOOLEAN), pData);
  Case Result Of
    ERR_NO_ERROR : Value := (pData^.Var1 <> nil);
    ERR_GLOBAL_VAR_NOT_EXIST : Value := False;
  End;
  FreeVarRec(pData);
end;

function TmafHookClient.GetGlobalBoolean(VarName: String): Boolean;
var B : Boolean;
begin
  GetGlobalBoolean(VarName, b);
  Result := b;
end;

function TmafHookClient.GetGlobalObject(VarName: String; var Value: Pointer): Integer;
var pData: PVarRec;
begin
  pData := CreateVarRec(VarName, [], lsNone);
  pData^.Typ := vtPointer;
  Result := CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_GET_OBJECT), pData);
  If Result = ERR_NO_ERROR Then
    Value := pData^.Var1;
  FreeVarRec(pData);
end;

// ********************************* Comments **********************************
// Description : writes a global integer variable
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 02.03.2007
// Last Update : 01.10.2009
// *****************************************************************************
procedure TmafHookClient.SetGlobalInteger(VarName: String; Value: Integer; VarFlags: TVarFlags=[]; ALoadSave: TLoadSave = lsINIFile);
var pData: PVarRec;
begin
  pData := CreateVarRec(VarName, VarFlags, ALoadSave);
  pData^.Typ := vtInteger;
  pData^.Var1 := Pointer(Value);
  CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_SET_INTEGER), pData);
  FreeVarRec(pData);
end; // SetGlobalInteger

procedure TmafHookClient.SetGlobalBoolean(VarName: String; Value: Boolean; VarFlags: TVarFlags=[]; ALoadSave: TLoadSave = lsINIFile);
var pData: PVarRec;
begin
  pData := CreateVarRec(VarName, VarFlags, ALoadSave);
  pData^.Typ := vtBoolean;
  If Value Then pData^.Var1 := Pointer(1)
           Else pData^.Var1 := nil;
  CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_SET_BOOLEAN), pData);
  FreeVarRec(pData);
end;

procedure TmafHookClient.SetGlobalObject(VarName: String; Value: Pointer);
var pData: PVarRec;
begin
  pData := CreateVarRec(VarName, [], lsNone);
  pData^.Typ := vtPointer;
  pData^.Var1 := Pointer(Value);
  CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_SET_OBJECT), pData);
  FreeVarRec(pData);
end;


// ********************************* Comments **********************************
// Description : read a global String variable
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 01.03.2007
// Last Update : 22.10.2008
// *****************************************************************************
function TmafHookClient.GetGlobalString(VarName: String; var Value: String): Integer;
var pData, p: PVarRec;
begin
  pData := CreateVarRec(VarName, [], lsNone);
  pData^.Typ := vtString;
  Result := CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_GET_STRING), pData);
  // our string returns as PVarRec inside Var1, it's a live pointer of the Manager,
  // so don't touch it !!!!!!!
  p := PVarRec(pData^.Var1);
  If Result = ERR_NO_ERROR Then
    Value := String(PChar(p^.Var1));
  // Since FreeVarRec release Var1 as String normally here, but we put there the
  // live pointer, we change the type to vtInteger and FreeVarRec doesn't try to
  // release the data behind PVarRec^.Var1
  pData^.Typ := vtInteger;
  FreeVarRec(pData);
end; // GetGlobalString

function TmafHookClient.GetGlobalString(VarName: String): String;
var pData, p: PVarRec;
    ErrCode : Integer;
begin
  pData := CreateVarRec(VarName, [], lsNone);
  pData^.Typ := vtString;
  ErrCode := CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_GET_STRING), pData);
  // our string returns as PVarRec inside Var1, it's a live pointer of the Manager,
  // so don't touch it !!!!!!!
  p := PVarRec(pData^.Var1);
  If ErrCode = ERR_NO_ERROR Then
    Result := String(PChar(p^.Var1));
  // Since FreeVarRec release Var1 as String normally here, but we put there the
  // live pointer, we change the type to vtInteger and FreeVarRec doesn't try to
  // release the data behind PVarRec^.Var1
  pData^.Typ := vtInteger;
  FreeVarRec(pData);
end;

// ********************************* Comments **********************************
// Description : writes a global string variable
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 01.03.2007
// Last Update : 22.10.2008
// *****************************************************************************
procedure TmafHookClient.SetGlobalString(VarName, Value: String; VarFlags: TVarFlags=[]; ALoadSave: TLoadSave = lsINIFile);
var pData: PVarRec;
begin
  pData := CreateVarRec(VarName, VarFlags, ALoadSave);
  pData^.Typ := vtString;
  StrToPChar(Value, PChar(pData^.Var1));
  CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_SET_STRING), pData);
  FreeVarRec(pData);
end; // SetGlobalString

function TmafHookClient.DeleteGlobalVar(VarName: String): Integer;
var pData: PVarRec;
begin
  pData := CreateVarRec(VarName, [], lsNone);
  Result := CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_DELETE_VAR), pData);
  FreeVarRec(pData);
end; // DeleteGlobalVar

function TmafHookClient.RegisterCallBack(VarName: String): Integer;
var pData : PCallBackRegister;
begin
  New(pData);
  FillChar(pData^, SizeOf(RCallBackRegister), 0);
  StrToPChar(VarName, pData^.VarName);
  pData^.AHookClient := Self;
  Result := CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_REGISTER_CALLBACK), pData);
  FreePChar(pData^.VarName);
  Dispose(pData);
  If ((FpGlobalVarNotify = nil) And (Result = ERR_NO_ERROR)) Then begin
    FpGlobalVarNotify := TStringList.Create;
    FpGlobalVarNotify.CaseSensitive := False;
    FpGlobalVarNotify.Duplicates := dupIgnore;
  end;
  FpGlobalVarNotify.Add(VarName);
end;

function TmafHookClient.UnRegisterCallBack(VarName: String): Integer;
var pData : PCallBackRegister;
    idx : Integer;
begin
  New(pData);
  FillChar(pData^, SizeOf(RCallBackRegister), 0);
  StrToPChar(VarName, pData^.VarName);
  pData^.AHookClient := Self;
  Result := CallManager(HM_GLOBAL_VAR_QUERY, Pointer(GV_UNREGISTER_CALLBACK), pData);
  FreePChar(pData^.VarName);
  Dispose(pData);
  If FpGlobalVarNotify <> nil Then begin
    idx := FpGlobalVarNotify.IndexOf(VarName);
    If idx > -1 Then
      FpGlobalVarNotify.Delete(idx);
  end;  //  --  If FpGlobalVarNotify <> nil Then
end;

procedure TmafHookClient.RegisterOnIdleNotifier(AComponent: TComponent);
begin
  If FOnIdleNotifyList = nil Then begin
    FOnIdleNotifyList := TList.Create;
    Include(FHookClientOptions, hcoProcessOnIdle);
  end;
  If FOnIdleNotifyList.IndexOf(AComponent) = -1 Then
    FOnIdleNotifyList.Add(AComponent);
end;

procedure TmafHookClient.UnregisterOnIdleNotifier(AComponent: TComponent);
var idx : Integer;
begin
  If FOnIdleNotifyList = nil Then
    Exit;
  idx := FOnIdleNotifyList.IndexOf(AComponent);
  If idx > -1 Then
    FOnIdleNotifyList.Delete(idx);
end;

initialization
  RegisterClass(TmafHookClient);

finalization
  UnRegisterClass(TmafHookClient);

end.
