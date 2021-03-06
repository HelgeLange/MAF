{*******************************************************************************
Name         : uMAF_HookManager.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2002-2011 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 25.02.2002
Last Update  : 01.12.2011
Version      : 1.0.018
Purpose      :
Last Changes :

1.0.018 (01.12.2011) -----------------------------------------------------------
- [ADD] added Event BeforeConnect that is called, before the DFT loads when
        the property Connected is set to True
- [ADD] new property DFTLoader, where one can choose between 2 DFT loading
        methods :
        1.) the usual BaseDB loading method, where the DFT stream is stored in
            the TmafDataStorage
        2.) a XML file, which can be stored as a file next to the executable
            (or any other place, use property XMLFileName) or in TmafDataStorage
            as stream but as XML
1.0.017 (07.11.2011) -----------------------------------------------------------
- [FIX] when a TmafHookClient registered itself on creation and used the option
        hcoProcessOnIdle, then it the FreeNotification wasn't set, causing an
        access violation, when the TmafHookClient was gone and OnIdle occured
1.0.016 (30.08.2010) -----------------------------------------------------------
- [CHG] moved the code for HM_QUERY in __OnEvent to new virtual method
        __DoManagerEvent, that reacts on MT_ROUTER messages
1.0.015 (31.05.2010) -----------------------------------------------------------
- [ADD] accepts now registrations from TmafHookClients for OnIdle events
1.0.014 (14.11.2009) -----------------------------------------------------------
- [ADD] added event BeforeLoadingModule, that is fired before the module will
        be loading and makes it possible to change the module name (i.e.
        switching the Router module for different databases)
1.0.013 (24.07.2009) -----------------------------------------------------------
- [CHG] switched from HookAccessContainer to TmafModuleController
- [ADD] added code, that prevents the connected property to be loaded from the
        DFM file as True while in design time. That caused trouble when trying
        to open the DFT editor because it was trying to initialize the library
        with the HookManager inside over and over again, because the object
        variable (in that case DMR) wasn't set yet
- [CHG] Component now inherits from TERPCustomManagerComponent, was TComponent
        before. This new class implements basic functions all manager components
        should have and helps to keep the code cleaner makes makes centralized
        changes easier
1.0.012 (01.07.2009) -----------------------------------------------------------
- [DEL] property HookAction removed, as it was for obsolete with the new XML
        support to import/export dynamic functions etc.
- [CHG] CodeGroup support moved to TERPDynamicFunctionTable object
1.0.011 (20.06.2009) -----------------------------------------------------------
- [ADD] support for the pFreeMemFunc in the QueryHandlerStruct. It's called after
        executing the AfterExecHook event after every single SubHook, if the
        event is assigned and marked for execution.
1.0.010 (16.06.2009) -----------------------------------------------------------
- [ADD] Code group support
- [ADD] THookManagerStatistic object, that holds all "nice to have" statistic data
        collected in a read-only object for Display purpose only
- [ADD] TMAFHookManager.__TriggerDFTChange to inform all TmafHookClients loaded
        at the moment about a change in the DFT
- [DEL] unstored properties that held statistic informations before, additional
        code to display those data
- [CHG] RSubHook record stores now the HookID itself, too. That was necessary for
        the CodeGroups implementation
1.0.009 (03.11.2008) -----------------------------------------------------------
- [ADD] property Connected, can be used to load and unload the DFT and modules
- [DEL] property AutoLoad, because functions shouldn't be automatically loaded
        when the component is loaded from stream, but after all components are
        loaded and we're really ready to load the DFT and the modules
1.0.008 (29.10.2008) -----------------------------------------------------------
- [FIX] __OnEnumDynamicFunctions now sets the bActive flag correct
1.0.007 (27.10.2008) -----------------------------------------------------------
- [ADD] added TERPLogger support
1.0.006 (15.10.2008) -----------------------------------------------------------
- [CHANGE] FreeRegisteredModules changed the unload order
           "last loaded, first unloaded", was "first loaded, first unloaded"
           That keeps Manager modules (wich are usually loaded first) until
           last moment loaded
1.0.005 (10.10.2008) -----------------------------------------------------------
- [FIX] EnumModules won't throw a ComponentError anymore, if the Database is not
        connected, but Autoload is True
1.0.004 (26.09.2008) -----------------------------------------------------------
- [ADD] added call of FreeNotification for all connected components to get
        informed, when they get destroyed
1.0.003 (25.05.2007) -----------------------------------------------------------
- [ADD] property HookAction to access CopyHookData in design time
- [ADD] CopyHookData to copy data from one BaseDB to another in design or runtime
1.0.002 (04.04.2007) -----------------------------------------------------------
- [ADD] will now register its interface to TERPHookAccessContainer, also new
        property HookAccessContainer
- [ADD] TERPHookAccessContainer can now register here and their published SubHooks
        will be used to built our DFT, if we don't use a database
        (property SaveDFT=False)
- [ADD] added pack/unpack for DFT to support registry version of the BaseDB,
        wich will need a packed stream to load/save the DFT into the registry
1.0.001 (25.02.2002) -----------------------------------------------------------
- initial version
*******************************************************************************}


// ideas
// - through ModuleController one can register FormID at single Hooks (shouldn't be
//   Hooks with more than 1 SubHook inside) and one can open a window by its FormID
// - function observer. every hookclient can create a function observer and as
//   soon as one funtion changes or the rights to access change, those Hookclients
//   get informed
// - XML import/export f�r HookManager, sowie ModuleController
//   - all done
// - Hook backup install stack. every time a hook gets overwritten by another
//   module, it will be moved with a timestamp to a backup table. when the module
//   gets uninstalled, the backup item will be used to restore the previous
//   connection to the function used before, if not the function is already used
//   by another module than the uninstalling one
// - moving the multi-function interface in OnEvent to a single-query interface
//   to reduce amount of events in the modulecontroller
// - FileDB using the base from the FileResource
//   - done
// - when switching to lockdown save a list of open hooks from windowmanager and
//   as long as the session goes we keep that list and check it when a hook is
//   called that isn't there. That way we should be able to close all open windows
//   without problems
// - local and remote BaseDB for templates and global variables
unit uMAF_HookManager;

interface

uses Windows, SysUtils, Classes, {$IFDEF Trial} Dialogs, {$ENDIF}
     // Modular Application Framework Components units
     {$IFDEF Tracer} uMAF_Tracer, {$ENDIF}
     uMAF_Globals, uMAF_ModuleManager, uMAF_CustomBaseDB, uMAF_DataStorage,
     uMAF_ModuleController, uMAF_Logger, uMAF_DynamicFunctionTable,
     uMAF_HookManager_Helper, uMAF_Core, uMAF_TemplateStreamer, uMAF_License;

Type
     TmafHookManager = class;

     TDFTLoader = (dlBaseDB, dlXML);

     TmafManagerStatistic = class(TPersistent)
     private
       FnHookClientCount : Integer;
       function __GetCodegroupCount: Integer;
       function __GetModuleCount: Integer;
       function __GetHookCount: Integer;
       function __GetSubHookCount: Integer;
       function __GetHookClientCount: Integer;
     public
       FpHookManager : TmafHookManager;
       constructor Create;
     published
       property HookClientCount : Integer read __GetHookClientCount stored False;
       property CodeGroupCount : Integer read __GetCodegroupCount stored False;
       property ModuleCount : Integer read __GetModuleCount stored False;
       property HookCount : Integer read __GetHookCount stored False;
       property SubHookCount : Integer read __GetSubHookCount stored False;
     end; // TmafHookManagerStatistic

     TBeforeLoadingModule = procedure(Sender: TObject; nModuleID: Integer; var ModuleName: String) Of Object;

     TmafHookManager = class(TmafCustomManagerComponent)
     private
       FpBaseDB : TmafCustomBaseDB;           // Base DB access, add descendant components only !
       FpTemplateHandler : TmafDataStorage;      // connected TmafTemplate
       FbConnected : Boolean;
       FpClientList : TList;        // List of all TmafModuleController loaded within the system
       FpLogger : TmafCustomLogger;
       FpHookManagerStatistic : TmafManagerStatistic;
       FpDynamicFuntionTable : TmafDynamicFuntionTable;
       Streamer : TTemplateStreamer;
       FBeforeLoadingModule : TBeforeLoadingModule;
       FpOnIdleList : TList;
       FpLicenseManager : TmafLicenseManager;
       FBeforeConnect : TNotifyEvent;

       procedure __SetBaseDB(const Value: TmafCustomBaseDB);
       // events
       procedure __SetConnected(const Value: Boolean);
       procedure __SetTemplateHandler(const Value: TmafDataStorage);
       procedure __SetLogger(const Value: TmafCustomLogger);
       // other
       procedure __FreeCodeGroupData(pData: Pointer);
       function __Process_ApplicationIdle: Integer;
       procedure __Toggle_ApplicationIdle(aClient: TComponent; bAttach: Boolean);
       procedure __SetLicenseManager(const Value: TmafLicenseManager);
     protected
       // stream loader events
       procedure __WriteStreamAttribute(Sender: TObject; ID: Integer);
       procedure __ReadStreamAttribute(Sender: TObject; ID: Integer);
       function __Get_PublicCodeGroups(QHS: pQHS): Integer;
       procedure __RegisterAPI; override;
       procedure __UnRegisterAPI; override;
       procedure __DoManagerEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer); override;
       procedure __OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer); override;
       procedure CloseManager; override;
       procedure InitDone; override;
       procedure ClientAdded(aClient: TComponent; var CanAdd: Boolean); override;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;


       // InstallAPI
       procedure AddHookAccessContainer(AContainer: Pointer);
       procedure RemoveHookAccessContainer(AContainer: Pointer);
       procedure __TriggerDFTChange;

     published
       property DynamicFuntionTable : TmafDynamicFuntionTable read FpDynamicFuntionTable write FpDynamicFuntionTable;
       property BaseDB : TmafCustomBaseDB read FpBaseDB write __SetBaseDB;
       property Logger : TmafCustomLogger read FpLogger write __SetLogger;
       property DataStorage : TmafDataStorage read FpTemplateHandler write __SetTemplateHandler;
       property Connected : Boolean read FbConnected write __SetConnected default False;
       property LicenseManager : TmafLicenseManager read FpLicenseManager write __SetLicenseManager;
//       property SecurityLayer : THookSecurityLayer read FpSecurityLayer write __SetSecurityLayer;
       property HookManagerStatistic : TmafManagerStatistic read FpHookManagerStatistic write FpHookManagerStatistic;
       property BeforeLoadingModule : TBeforeLoadingModule read FBeforeLoadingModule write FBeforeLoadingModule;
       property OnError;
       property BeforeConnect : TNotifyEvent read FBeforeConnect write FBeforeConnect;
     end; // TmafHookManager

// Helper functions
function HookToInstallToken(pData: PSubHook): PMAFInstallToken;

{$IFDEF Trial}
procedure ShowNagScreen;
var bTamperedWith : Boolean;
{$ENDIF}

const CodeGroupDataVer  = 3000;
      HOOKMANAGER_TEMPLATE_CODEGROUP = 'CodeGroup_Data';
      ID_CodeGroupDesc  = 545;



implementation

uses Messages, uMAF_HookClient, uMAF_Tools;

const MAX_EVENTS = 3;
      HOOKMANAGER_TEMPLATE_CATEGORY = 'HookData';
      HOOKMANAGER_TEMPLATE_MODULES  = 'ModuleData';

var ManagerEvents : array[1..MAX_EVENTS] of Integer = (
      HM_EXEC_HOOK,
//      HM_QUERY,
      HM_INSTALL_API,
      HM_LOG_MESSAGE);

    EventPriorities : array[1..MAX_EVENTS] of TEventPriority = (
      epUltraHigh, {epNormal,} epLow, epLow
      );

function HookToInstallToken(pData: PSubHook): PMAFInstallToken;
begin
  Result := nil;
  If Not Assigned(pData) Then
    Exit;

  Result := __Create_InstallToken(iaInsert);
  Result^.uID := pData^.uID;
  Result^.nHookID := pData^.HookID;
  Result^.nSubHookID := pData^.SubHookID;
  Result^.nModuleID := pData^.nModuleID;
  Result^.nCodeGroupID := pData^.nCodeGroupID;
  If pData^.bActive Then Result^.bActive := 1
                    Else Result^.bActive := 0;
end;

{$IFDEF Trial}
procedure ShowNagScreen;
begin
  MessageDlg('This program is using the trial version of the' + #13#10 +
             'Modular Application Framework Components' + #13#10 +
             'Please register your copy at http://www.maf-components.com', mtInformation, [mbOk], 0);
end;
{$ENDIF}

{ TmafHookManagerStatistic }

constructor TmafManagerStatistic.Create;
begin
  inherited;
  FnHookClientCount := 0;
end;

function TmafManagerStatistic.__GetCodegroupCount: Integer;
begin
  Result := FpHookManager.DynamicFuntionTable.CodeGroups.Count;
end;

function TmafManagerStatistic.__GetHookClientCount: Integer;
begin
  Result := FpHookManager.Clients.Count;
end;

function TmafManagerStatistic.__GetHookCount: Integer;
begin
  Result := FpHookManager.DynamicFuntionTable.DFT.Count;
end;

function TmafManagerStatistic.__GetModuleCount: Integer;
begin
  Result := FpHookManager.DynamicFuntionTable.Modules.Count;
end;

function TmafManagerStatistic.__GetSubHookCount: Integer;
var i : Integer;
begin
  Result := 0;
  For i := 0 To FpHookManager.DynamicFuntionTable.DFT.Count - 1 Do
    Result := Result + PDynamicFunction(FpHookManager.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Count;
end;

{ TmafHookManager }

constructor TmafHookManager.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF Trial} ShowNagScreen; {$ENDIF}
  ManagerType := MT_ROUTER;  // must be registered in Create!
  ManagerOptions := [moAutoRegister, moRegisterInitDone];
  FpDynamicFuntionTable := TmafDynamicFuntionTable.Create;
  FpDynamicFuntionTable.FpHookManager := Self;
  FpClientList := TList.Create;
  FpOnIdleList := TList.Create;
  FbConnected := False;
  FpHookManagerStatistic := TmafManagerStatistic.Create;
  FpHookManagerStatistic.FpHookManager := Self;
end;

destructor TmafHookManager.Destroy;
begin
  FreeAndNil(FpOnIdleList);
  FreeAndNil(FpClientList);
  FpHookManagerStatistic.Free;
  FreeAndNil(FpDynamicFuntionTable);
  inherited;
end;

procedure TmafHookManager.InitDone;
var i : Integer;
begin
  If FbInitDoneNotified Then
    Exit;
  inherited;
  If FbConnected Then
    Connected := True;
  For i := 0 To FpDynamicFuntionTable.Modules.Count - 1 Do
    If TmafModuleManager(FpDynamicFuntionTable.Modules.Items[i]).ModuleController <> nil Then
      SendComponentMessage(TmafModuleManager(FpDynamicFuntionTable.Modules.Items[i]).ModuleController, WM_INIT_DONE, nil, nil);
end;

procedure TmafHookManager.Loaded;
begin
  inherited;
  {$IFDEF Tracer}
    MAFTracer.Enter('TmafHookManager.Loaded');
  {$ENDIF}
{  If csDesigning in ComponentState Then  // we want to avoid, that connected is loaded
    FbConnected := False;                // as True in the IDE
  Connected := FbConnected; }
  {$IFDEF Tracer}
    MAFTracer.Leave;
  {$ENDIF}
end;

procedure TmafHookManager.ClientAdded(aClient: TComponent; var CanAdd: Boolean);
begin
  inherited;
  // if the HookClient wants to be informed about OnIdle events, we add him here
  If hcoProcessOnIdle in TmafHookClient(aClient).HookClientOptions Then
    __Toggle_ApplicationIdle(aClient, True);
end;

procedure TmafHookManager.CloseManager;
var i: Integer;
    pModule : TmafModuleManager;
begin
  inherited;
  For i := 0 To FpDynamicFuntionTable.Modules.Count - 1 Do begin
    pModule := TmafModuleManager(FpDynamicFuntionTable.Modules.Items[i]);
    If Assigned(pModule.ModuleController) Then
      SendComponentMessage(pModule.ModuleController, WM_MANAGER_CLOSE, nil, nil);
  end;
  Connected := False;
end;

procedure TmafHookManager.Notification(AComponent: TComponent; Operation: TOperation);
var idx : Integer;
begin
  inherited;
  If Operation = opRemove Then begin
    If AComponent = FpBaseDB Then
      FpBaseDB := nil;
    If AComponent = FpTemplateHandler Then
      FpTemplateHandler := nil;
    If AComponent = FpLogger Then
      FpLogger := nil;
    If AComponent = FpLicenseManager Then
      FpLicenseManager := nil;
    idx := FpOnIdleList.IndexOf(AComponent);
    If idx > -1 Then
      FpOnIdleList.Delete(idx);
  end;  //  --  If Operation = opRemove Then
end; // Notification

procedure TmafHookManager.__SetConnected(const Value: Boolean);
begin
  If Value = FbConnected Then
    Exit;                      // we don't have anything to do

  FbConnected := Value;
  If not (csLoading in ComponentState) Then begin
    If FbConnected Then begin
      If Assigned(FBeforeConnect) Then
        FBeforeConnect(Self);
      Try
        FpDynamicFuntionTable.Initialize;
      Except
        On E: Exception Do
          If Assigned(OnError) Then
            OnError(Self, E.Message);
      End;
    end else begin
      FpDynamicFuntionTable.Close;
    end;
  end;  //  --  If not (csLoading in ComponentState) Then
end;

procedure TmafHookManager.__SetBaseDB(const Value: TmafCustomBaseDB);
begin
  FpBaseDB := Value;
  If FpBaseDB <> nil Then begin
    FpBaseDB.FreeNotification(Self);
{    If Assigned(FpSecurityLayer) Then           // if we have a SecurityLayer...
      FpSecurityLayer.BaseDB := FpBaseDB;       // setting the new BaseDB there, too }
  end;  //  --  If FpBaseDB <> nil Then
end;

// ********************************* Comments **********************************
// Description : register our events
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 04.04.2007
// Last Update : 04.04.2007
// *****************************************************************************
procedure TmafHookManager.__RegisterAPI;
var i : Integer;
begin
  inherited;
  If Assigned(ModuleController) Then
    For i := 1 To MAX_EVENTS Do
      ModuleController.RegisterSubHookEvent(ManagerEvents[i], __OnEvent);
end; // __RegisterAPI

procedure TmafHookManager.__UnRegisterAPI;
var i : Integer;
begin
  If Assigned(ModuleController) Then
    For i := 1 To MAX_EVENTS Do
      ModuleController.UnRegisterSubHookEvent(ManagerEvents[i], __OnEvent);
  inherited;
end;

procedure TmafHookManager.__SetLicenseManager(const Value: TmafLicenseManager);
begin
  FpLicenseManager := Value;
  If FpLicenseManager <> nil Then begin
    FpLicenseManager.HookManager := Self;
    FpLicenseManager.FreeNotification(Self);
  end;  //  --  If FpLicenseManager <> nil Then
end;

procedure TmafHookManager.__SetLogger(const Value: TmafCustomLogger);
begin
  FpLogger := Value;
  If FpLogger <> nil Then begin
    FpLogger.HookManager := Self;
    FpLogger.FreeNotification(Self);
  end;  //  --  If FpLogger <> nil Then
end;


var FpCodeGroupDescList : TList;

procedure TmafHookManager.__ReadStreamAttribute(Sender: TObject; ID: Integer);
var i, nCount : Integer;
    pCG_Data : PCodeGroupData;
begin
  Case Streamer.StreamID Of
    ID_CodeGroupDesc : begin
                         FpCodeGroupDescList.Clear;
                         Streamer.ReadInteger(nCount);
                         FpCodeGroupDescList.Capacity := nCount;
                         For i := 1 To nCount Do begin
                           pCG_Data := __Create_CodeGroupDataToken;
                           Streamer.ReadInteger(pCG_Data^.nID);
                           Streamer.ReadString(pCG_Data^.Name);
                           Streamer.ReadString(pCG_Data^.Desc);
                           Streamer.ReadCardinal(pCG_Data^.NameResID);
                           Streamer.ReadCardinal(pCG_Data^.DescResID);
                           Streamer.ReadInteger(pCG_Data^.MinSL);
                           If ((pCG_Data^.Name = '') And (pCG_Data^.Desc = '')) Then  // filter empty ones out
                             Dispose(pCG_Data)
                           Else
                             FpCodeGroupDescList.Add(pCG_Data);
                         end;
                       end; // ID_CodeGroupDesc
  end;
end;

procedure TmafHookManager.__WriteStreamAttribute(Sender: TObject; ID: Integer);
begin

end;

procedure TmafHookManager.__FreeCodeGroupData(pData: Pointer);
var i : Integer;
begin
  FpCodeGroupDescList := TList(pData);
  For i := FpCodeGroupDescList.Count - 1 DownTo 0 Do
    Dispose(PCodeGroupData(FpCodeGroupDescList.Items[i]));
  FpCodeGroupDescList.Free;
end;

// ********************************* Comments **********************************
// Description : returns the CodeGroup infos for the currently available CodeGroups
//               all CodeGroups not accessible by the current user are filtered
//               out
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 13.09.2009
// Last Update : 13.09.2009
// *****************************************************************************
function TmafHookManager.__Get_PublicCodeGroups(QHS: pQHS): Integer;
begin
  Result := ERR_COMPONENT_SETUP_FAILURE;
  If ((QHS <> nil) And (FpTemplateHandler <> nil)) Then begin
    Streamer := TTemplateStreamer.Create;
    Streamer.Attributes := 1;                      // there is only one attribute
    Streamer.Stream.Size := 0;                     // no data yet
    Streamer.DataStorageQueryToken^.Action := taRead; // we read here
    Streamer.DataStorageQueryToken^.nID := 0;         // we don't know the ID
    Streamer.TemplateCategory := 'HookDesc';       // the category for templates
    Streamer.TemplateName := HOOKMANAGER_TEMPLATE_CODEGROUP; // the template name
    Streamer.OnStreamReadAttribute := __ReadStreamAttribute;
//    Streamer.StreamVersion := CodeGroupDataVer;    // Stream version for CodeGroup Descriptions

    Result := FpTemplateHandler.Query(Streamer.DataStorageQueryToken);
    If ((Streamer.Stream.Size > 0) And (Result = ERR_NO_ERROR)) Then begin
      FpCodeGroupDescList := TList.Create; // the list, where the ReadStream reads the Codegroup descriptions into
      QHS^.pChildObj := FpCodeGroupDescList; // and we return that list
      QHS^.pFreeMemFunc := __FreeCodeGroupData;
      Streamer.ReadStream;
      FpDynamicFuntionTable.CodeGroups_CheckDescriptions(FpCodeGroupDescList);
    end;
    Streamer.Free;
    Result := ERR_NO_ERROR;
  end;
end;

procedure TmafHookManager.__DoManagerEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  Case QHS^.SubHookID Of
    HM_GET_SECURITY_LEVEL : If Assigned(FpBaseDB) Then QHS^.Reserved1 := FpBaseDB.ConnectionData.SecurityLevel
                                                  Else QHS^.Reserved1 := 1;
    HM_SAVE_CG_RIGHTS     : ErrCode := FpDynamicFuntionTable.__Set_CodeGroup_Rights(QHS^.Reserved1, PGroupData(UserParam));
    HM_LOAD_CG_RIGHTS     : ErrCode := FpDynamicFuntionTable.__Get_CodeGroup_Rights(QHS^.Reserved1, PGroupData(UserParam));
    HM_DELETE_CG_RIGHTS   : ErrCode := FpDynamicFuntionTable.__Del_CodeGroup_Rights(QHS^.Reserved1, PGroupData(UserParam));
    HM_LOAD_CG_DESCRIPTIONS : ErrCode := __Get_PublicCodeGroups(QHS);
    HM_GET_HM_STATISTIC   : QHS^.pChildObj := FpHookManagerStatistic;
    HM_GET_HOOKMANAGER    : QHS^.pChildObj := Self;
    HM_ADD_FUNCTION_OBSERVER    : ErrCode := FpDynamicFuntionTable.__Add_FunctionObserver(QHS^.Reserved1, QHS^.Reserved2, UserParam);
    HM_DELETE_FUNCTION_OBSERVER : ErrCode := FpDynamicFuntionTable.__Delete_FunctionObserver(QHS^.Reserved1, QHS^.Reserved2, UserParam);
    HM_APPLICATION_IDLE   : QHS^.ResVal := __Process_ApplicationIdle;
    HM_TOGGLE_APPLICATION_IDLE : __Toggle_ApplicationIdle(TComponent(QHS^.pChildObj), (QHS^.Reserved1 > 0));
  end;
end;

procedure TmafHookManager.__OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  Case SubHookID Of
    HM_EXEC_HOOK        : ErrCode := FpDynamicFuntionTable.ExecuteHook(QHS^.HookID, QHS, UserParam);
    HM_LOG_MESSAGE      : If ((Assigned(FpLogger)) And (UserParam <> nil)) Then
                            ErrCode := FpLogger.WriteLog(PLogMessage(UserParam)^.aType, String(PLogMessage(UserParam)^.Msg))
                          Else
                            ErrCode := ERR_PARAM_FAILURE;

    HM_INSTALL_API      : ErrCode := FpDynamicFuntionTable.Query_InstallAPI(UserParam);

    Else inherited __OnEvent(SubHookID, QHS, UserParam, ErrCode);
  end;  //  --  Case SubHookID Of
end; // __OnEvent

procedure TmafHookManager.__Toggle_ApplicationIdle(aClient: TComponent; bAttach: Boolean);
var idx : Integer;
begin
  idx := FpOnIdleList.IndexOf(aClient);
  If bAttach Then begin
    If idx = -1 Then begin
      FpOnIdleList.Add(aClient);
      aClient.FreeNotification(Self);
    end;
  end else begin
    If idx > -1 Then begin
      FpOnIdleList.Delete(idx);
      aClient.RemoveFreeNotification(Self);
    end;
  end;
end;

function TmafHookManager.__Process_ApplicationIdle: Integer;
var i : Integer;
begin
  // we count backwards in case a client wants to remove itself inside the OnIdle
  For i := FpOnIdleList.Count - 1 DownTo 0 Do
    SendComponentMessage(TComponent(FpOnIdleList.Items[i]), MSG_ON_IDLE, nil, nil);
  Result := 1;
end;

procedure TmafHookManager.__SetTemplateHandler(const Value: TmafDataStorage);
begin
  FpTemplateHandler := Value;
  If FpTemplateHandler <> nil Then
    FpTemplateHandler.FreeNotification(Self);
end;

procedure TmafHookManager.__TriggerDFTChange;
var i : Integer;
begin
  For i := 0 To Clients.Count - 1 Do
    SendComponentMessage(TComponent(Clients.Items[i]), MSG_DFT_CHANGE, nil, nil);
end; // __TriggerDFTChange

procedure TmafHookManager.AddHookAccessContainer(AContainer: Pointer);
begin
  If ((AContainer <> nil) And (FpClientList.IndexOf(AContainer) = -1)) Then
    FpClientList.Add(AContainer);
end; // AddHookAccessContainer

procedure TmafHookManager.RemoveHookAccessContainer(AContainer: Pointer);
var i : Integer;
begin
  i := FpClientList.IndexOf(AContainer);
  If i > -1 Then
    FpClientList.Delete(i);
end; // RemoveHookAccessContainer

initialization
  {$IFDEF Trial}
  bTamperedWith := ((CheckIfProcPatched(@ShowNagScreen) = False) And (CheckIfNopInProc(@ShowNagScreen)) And (CheckIfBPInProc(@ShowNagScreen)));
  {$ENDIF}

end.
