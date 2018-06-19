{*******************************************************************************
Name         : uMAF_ModuleController.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2009-2015 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 12.07.2009
Last Update  : 26.06.2015
Version      : 1.0.011
Purpose      : Controller component for modules, created as a new solution for
               FormAccessManager and HookAccessContainer, it offers access to
               unique Data records requested by Hooks, forms published in the
               module and manages the functions published in the module
Last Changes :


1.0.011 (26.06.2015) -----------------------------------------------------------
- [FIX] a bug was fixed when a form was called that had a data record assigned
        that the record was created twice
- [ADD] When creating a record now you can give an object TmafParameters to
        create additionally the record data inside the Tmafparameters
1.0.010 (04.04.2013) -----------------------------------------------------------
- [ADD] added new event OnBeforeReturnData, which can be used to determine if
        the data should be returned or no. The caller MUST check if
        QHS^.pChildObj is <> nil
1.0.009 (07.07.2011) -----------------------------------------------------------
- [FIX] fixed GetNext_uID to increment the variable FnCurrent_uID currently for
        the current session
- [ADD] when the ModuleID is set and RangeID is 0, then it will be set to 999 as
        default
- [ADD] when ModuleID is set and Start_uID is 9, it will be set to ModuleID * 1000
        as default
1.0.008 (04.08.2010) -----------------------------------------------------------
- [ADD] added Start_uID and Range_uID do define, where uIDs will start and what
        range is reserved. This is necessary, that uID can be made unique
        throughout several projects using the same modules. The information will
        be used by the HookManager-Editor
1.0.007 (08.07.2010) -----------------------------------------------------------
- [ADD] event to inform before a form is created (for example can a user/password
        dialog can pop-up, if the current user doesn't have sufficient rights to
        access the form)
1.0.006 (22.11.2009) -----------------------------------------------------------
- [ADD] Event priorities to priorize SubHook events, that often used and/or
        time critical events can be executed faster
- [ADD] ModuleController now gets informed about every SubHook de-installed to
        check for any open forms. That was necessary to close open forms not only
        when the module gets un-istaled, but also when an existing SubHook for a
        window gets overwritten or deleted while the window is still open
1.0.005 (07.11.2009) -----------------------------------------------------------
- [ADD] the ModuleController now receives a message from the DFT that the module
        is being closed (uninstalled) and can clean up all open forms
1.0.004 (19.10.2009) -----------------------------------------------------------
- [ADD] added new message procedure to accept a parameter list delivered by the
        TmafManagerLoader when mfStartParams is set in ModuleInfo
1.0.003 (21.09.2009) -----------------------------------------------------------
- [ADD] added install info list, wich contains all infos to install / deinstall
        the module
1.0.002 (11.09.2009) -----------------------------------------------------------
- [ADD] added Event OnInitialized wich is fired by the new uModuleService.pas
        after the datamodule/form was created and the create returned to the
        caller. At this point, the form variable like Form1 := TForm1.Create(Self)
        has been set and it's safe to work with it.
1.0.001 (02.08.2009) -----------------------------------------------------------
- [FIX] OnDestroy was never called in TmafModuleController.Execute, because
        .Execute was never called from the module when HM_CLOSE came in. Instead
        of adding an extra line in the mdule code to call .Execute before .Free
        it is now it is called in TmafModuleController.Destroy
1.0.000 (12.07.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_ModuleController;

interface

uses Windows, Classes, SysUtils, Messages,
     // Modular Application Framework Components units
     uMAF_Tracer, uMAF_Globals, uMAF_Core, uMAF_TemplateStreamer, uMAF_Parameters,
     uMAF_ModuleController_DataHelper;

Type THookAccessOption  = (haoAlwaysStandardEvent, haoAllowMultipleEvents, haoReplaceEvent);
     THookAccessOptions = Set Of THookAccessOption;
     TOnDatabase = procedure(nSubHookID: Integer; QHS: TQueryHandlerStruct) Of Object;
     TBeforeFormNotifyEvent = procedure(Sender: TObject; QHS: pQHS; nSubHookID: Integer; pData: Pointer; var CanContinue: Boolean) Of Object;
     TFormNotifyEvent = procedure(nSubHookID: Integer; Frm: TObject) Of Object;
     TNotifyErrorEvent = procedure(Sender: TObject; var ErrCode: Integer) Of Object;
     TStartParamEvent = procedure(Sender: TObject; AStartParams: TList) Of Object;

     TmafModuleController = class(TmafCustomModuleController)
     private
       FpDataDefs : TList;                         // list of TmafModuleControllerDataTypes objects
       FpData     : TList;                         // list of PDataRecord
       FpEventList : TList;                        // List of registered events created on the fly when the module is loaded
       FpFormTypeList : TList;                     // List of registered Forms
       FpMemoryList : TList;
       FpInstallData : TList;                      // List of the data to install / uninstall the module
       FpStreamer : TTemplateStreamer;
       FHookAccessOptions : THookAccessOptions;
       FOnSubHook : TOnSubHook;                    // standard subhook event
       FOnDatabase : TOnDatabase;                  // fired when the QHS contains a database/transaction info
       FOnCreate : TNotiFyEvent;                   // fired when HM_CREATED or MAN_CREATE_MANAGER command comes in
       FOnDestroy : TNotifyEvent;                  // fired when HM_CLOSE or MAN_CLOSE_MANAGER command comes in
       FOnManagerLoadingDone : TNotifyEvent;       // fired when all manager are available
       FOnInitialized : TNotifyErrorEvent;         // fired, when the DataModule/Form was created
       FBeforeFormCreate : TBeforeFormNotifyEvent; // fired before a form is created
       FOnFormCreated : TFormNotifyEvent;          // fired when a form was created
       FOnFormDestroy : TFormNotifyEvent;          // fired when a form will be destroyed
       FOnStartParams : TStartParamEvent;          // fired when accepting start params and there are some delivered by the TmafManagerLoader
       FBeforeReturnData : TBeforeFormNotifyEvent; // fired before return data is executed, set CanContinue to False and no data will be returned
       FpModuleInfo : TModuleInfo;
       FpCreatedForms : TList;                     // List of all forms we created and that are running right now
       FnStart_uID : Integer;                      // at which number the uIDs for this module will start
       FnRange_uID : Integer;                      // how many numbers will be reserved (should be big enough, 1000 should suffice)
       FnCurrent_uID: Integer;                     // the current uID that was given last
       function __GetDataDefs_HighID: Integer;
       procedure ReadDataDefs(aStream: TStream);
       procedure WriteDataDefs(aStream: TStream);
       procedure Read_StreamedData(aStream: TStream);
       procedure WriteData(aStream: TStream);
       procedure WriteInstallData(aStream: TStream);
       procedure Setup_Streamer;
       procedure __ReadStreamData(Sender: TObject; ID: Integer);
       procedure __WriteStreamData(Sender: TObject; ID: Integer);
       procedure __SetHookAccessOptions(const Value: THookAccessOptions);
       function __EventExists(nSubHookID: Integer; FEvent: TOnSubHook): PEventToken;
       procedure __Register_InternalEvents;
       procedure __OnCreateEnumWindow(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
       procedure __OnCreateWindow(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
       procedure __OnReturnData(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
       procedure __CreateForm(aFormClass: TComponentClass; var Reference; AOwner: TComponent);
       function __GetFormByHookID(HookID, SubHookID: Integer): Pointer;
       function __GetOpenFormCount: Integer;
       function __FindDataDefs(nDataId: Integer): TmafModuleControllerDataTypes;
       procedure OnModuleIDChange(Sender: TObject);
     protected
       procedure WMStartParams(var Msg: TMessage); message WM_START_PARAMS;
       procedure WMModuleClose(var Msg: TMessage); message WM_MODULE_CLOSE;
       procedure WMSubHookClose(var Msg: TMessage); message WM_SUBHOOK_CLOSE;
       procedure WM_MangerClose(var Msg: TMessage); message WM_MANAGER_CLOSE;
       procedure InitDone; override;
       function CreateForm(SubHookID: Integer; QHS: pQHS; pUserData: Pointer): Integer;
       procedure DestroyForm(SubHookID: Integer; QHS: pQHS; pUserData: Pointer);
       procedure DefineProperties(Filer: TFiler); override;
       function __GetDataSize(pData: PDataRecord): Integer;
       function __Create_DynamicRecord(pData: PDataRecord; mp: TmafParameters = nil): Pointer;
       procedure __Free_DynamicRecord(pData: Pointer);
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       function Execute(nSubHookID: Integer; aQHS, pUserParam: Pointer): Integer; override;
       procedure RegisterSubHookEvent(nSubHookID: Integer; FEvent: TOnSubHook; EP: TEventPriority = epNormal); override;
       procedure UnregisterSubHookEvent(nSubHookID: Integer; FEvent: TOnSubHook); override;
       function RegisterFormClass(SubHookID: Integer; aFormClass: TComponentClass): Boolean;
       procedure RegisterFormID(nFormID, nHookID : Integer; nSubHookID: Integer = 0);
       function GetDataDef(Idx: Integer): TmafModuleControllerDataTypes;
       function GetDataToken(nHookID, nSubHookID : Integer): PDataRecord;
       function GetEventToken(nSubHookID: Integer): PEventToken;
       function GetNext_uID : Cardinal;
       function __DoInitialize: Integer;  // just executes the event OnInitialized, do not call it on your own
       procedure __ReCalc_uID;            // called internally from the editor, do not call (although it doesn't hurt either!)
       property DataDefs : TList read FpDataDefs;
       property Data : TList read FpData;
       property InstallData : TList read FpInstallData;
       property DataDefs_HighID : Integer read __GetDataDefs_HighID;
       property OpenFormCount : Integer read __GetOpenFormCount;
     published
       property ModuleInfo : TModuleInfo read FpModuleInfo write FpModuleInfo;
       property HookAccessOptions : THookAccessOptions read FHookAccessOptions write __SetHookAccessOptions default [haoAllowMultipleEvents];
       property Start_uID : Integer read FnStart_uID write FnStart_uID default 0;
       property Range_uID : Integer read FnRange_uID write FnRange_uID default 0;
       property Current_uID : Integer read FnCurrent_uID ;
       // events
       property OnFormCreated : TFormNotifyEvent read FOnFormCreated write FOnFormCreated;
       property OnFormDestroy : TFormNotifyEvent read FOnFormDestroy write FOnFormDestroy;
       property OnLoaded;
       property OnSubHook: TOnSubHook read FOnSubHook write FOnSubHook;
       property OnDatabase : TOnDatabase read FOnDatabase write FOnDatabase;
       property OnHM_CREATE : TNotifyEvent read FOnCreate write FOnCreate;
       property OnHM_CLOSE : TNotifyEvent read FOnDestroy write FOnDestroy;
       property OnInitialized : TNotifyErrorEvent read FOnInitialized write FOnInitialized;
       property OnStartParams : TStartParamEvent read FOnStartParams write FOnStartParams;
       property OnManagerLoadingDone : TNotifyEvent read FOnManagerLoadingDone write FOnManagerLoadingDone;
       property OnBeforeFormCreate : TBeforeFormNotifyEvent read FBeforeFormCreate write FBeforeFormCreate;
       property OnBeforeReturnData : TBeforeFormNotifyEvent read FBeforeReturnData write FBeforeReturnData;
     end;

implementation

uses Forms, Controls, //Dialogs,
     uMAF_Tools, uMAF_WindowController, uMAF_HookManager_Helper;

{ TmafModuleController }

constructor TMAFModuleController.Create(AOwner: TComponent);
begin
  inherited;
  HookAccessOptions := [haoAllowMultipleEvents];
  FManagerType := MT_ROUTER;
  FpDataDefs := TList.Create;
  FpData := TList.Create;
  FpEventList := TList.Create;
  FpFormTypeList := TList.Create;
  FpMemoryList := TList.Create;
  FpInstallData := TList.Create;
  FpCreatedForms := TList.Create;
  FpStreamer := nil;
  FpModuleInfo := TModuleInfo.Create;
  FpModuleInfo.OnModuleIDChange := OnModuleIDChange;
  FnCurrent_uID := 0;
  FnStart_uID := 0;
  FnRange_uID := 0;
end;

destructor TMAFModuleController.Destroy;
var pData : PDataRecord;
    pEToken : PEventToken;
    pID : PMAFInstallToken;
    i : integer;
begin
  If Assigned(FOnDestroy) Then
    FOnDestroy(Self);
  For i := 0 To FpInstallData.Count - 1 Do begin
    pID := PmafInstallToken(FpInstallData.Items[i]);
    __Free_InstallToken(pID);
  end;
  FreeAndNil(FpInstallData);
  For i := 0 To FpMemoryList.Count - 1 Do
    Dispose(PMemoryToken(FpMemoryList.Items[i]));
  FreeAndNil(FpMemoryList);
  For i := 0 To FpFormTypeList.Count - 1 do
    Dispose(PFormRegisterData(FpFormTypeList.Items[i]));
  FreeAndNil(FpFormTypeList);
  For i := 0 To FpEventList.Count - 1 Do begin
    pEToken := PEventToken(FpEventList.Items[i]);
    __Free_EventToken(pEToken);
  end;  //  --  While FpEventList.Count > 0 Do
  FreeAndNil(FpEventList);
  For i := 0 To FpData.Count - 1 Do begin
    pData := PDataRecord(FpData.Items[i]);
    __Free_DataRecord(pData);
  end;  //  --  While FpData.Count > 0 Do
  FreeAndNil(FpData);
  For i := 0 To FpDataDefs.Count - 1 do
    TmafModuleControllerDataTypes(FpDataDefs.Items[i]).Free;
  FreeAndNil(FpDataDefs);
  FreeAndNil(FpModuleInfo);
  FreeAndNil(FpCreatedForms);
  inherited;
end;

procedure TmafModuleController.Loaded;
begin
  {$IFDEF Tracer}
    MAFTracer.Enter('TmafModuleController.Loaded');
  {$ENDIF}
  __Register_InternalEvents;  // we run that before we call "inherited", wich triggers the OnLoaded Event
  inherited;
  //{$message hint 'TmafModuleController.Loaded'}
  {$IFDEF Tracer}
    MAFTracer.Leave;
  {$ENDIF}
end;

procedure TmafModuleController.Notification(AComponent: TComponent; Operation: TOperation);
var idx : Integer;
begin
  inherited;
  If Operation = opRemove Then begin
    If FpCreatedForms.Count = 0 Then
      Exit;

    idx := FpCreatedForms.IndexOf(AComponent);
    If idx > -1 Then
      FpCreatedForms.Delete(idx);
  end;
end;

procedure TmafModuleController.OnModuleIDChange(Sender: TObject);
begin
  If FnRange_uID = 0 Then
    FnRange_uID := 999;
  If FnStart_uID = 0 Then
    FnStart_uID := FpModuleInfo.ModuleID * 1000;
end;

procedure TmafModuleController.InitDone;
var Msg : TMessage;
    i : Integer;
begin
  inherited;
  MSG.Msg := WM_INIT_DONE;
  If Owner <> nil then
    For i := 0 to Owner.ComponentCount - 1 Do
      If Owner.Components[i] <> Self Then
        Owner.Components[i].Dispatch(MSG);

  If Assigned(FOnManagerLoadingDone) Then
    FOnManagerLoadingDone(Self);
end;

procedure TmafModuleController.WMModuleClose(var Msg: TMessage);
var QHS : pQHS;
begin
  If OpenFormCount > 0 Then begin
    While FpCreatedForms.Count > 0 Do begin
      QHS := __Create_QueryHandlerStruct;
      QHS^.HookID := TmafWindowController(FpCreatedForms.Items[0]).HookID;
      QHS^.SubHookID := TmafWindowController(FpCreatedForms.Items[0]).SubHookID;
      QHS^.pChildObj := TmafWindowController(FpCreatedForms.Items[0]).Owner;
      DestroyForm(QHS^.SubHookID, QHS, nil);
      __Free_QueryHandlerStruct(QHS);
    end;
  end;
end;

procedure TmafModuleController.WMStartParams(var Msg: TMessage);
begin
  if Msg.WParam > 0 then
    If mfStartParams in FpModuleInfo.ModuleFlags then
      if Assigned(FOnStartParams) then
        FOnStartParams(Self, TList(Msg.WParam));
end;

// this occurs when a SubHook gets closed in this module and we check, if a
// window is still open (top-level-windows might tend to be like that)
procedure TmafModuleController.WMSubHookClose(var Msg: TMessage);
var i: Integer;
    AController : TmafWindowController;
    QHS : pQHS;
begin
  For i := 0 to FpCreatedForms.Count - 1 Do begin
    AController := TmafWindowController(FpCreatedForms.Items[i]);
    If ((AController.HookID = Msg.WParam) And (AController.SubHookID = Msg.LParam)) Then begin
      QHS := __Create_QueryHandlerStruct;
      QHS^.pChildObj := AController.Owner;
      DestroyForm(Msg.LParam, QHS, nil);
      __Free_QueryHandlerStruct(QHS);
      Break;
    end;
  end;
end;

procedure TmafModuleController.WM_MangerClose(var Msg: TMessage);
begin
  If Assigned(FOnDestroy) Then
    FOnDestroy(Self);
  inherited;
end;

procedure TMAFModuleController.__Register_InternalEvents;
var i : Integer;
    pToken : PEventToken;
begin
  For i := 0 To FpData.Count - 1 Do begin
    If PDataRecord(FpData.Items[i])^.Action = daRouteThrough Then
      Continue;                             // daRouteThrough we can ignore

    pToken := __Create_EventToken;
    pToken^.nSubHookID := PDataRecord(FpData.Items[i])^.SubHookID;
    Case PDataRecord(FpData.Items[i])^.Action Of
      daCreateWindowShow,
      daCreateWindowShowModal,
      daCreateWindow     : pToken^.OnSubHook := __OnCreateWindow;
      daReturnData       : pToken^.OnSubHook := __OnReturnData;
      daCreateEnumWindowModal,
      daCreateEnumWindow : pToken^.OnSubHook := __OnCreateEnumWindow;
    end;  //  --  Case PDataRecord(FpData.Items[i])^.Action Of
    FpEventList.Add(pToken);
  end;  //  --  For i := 0 To FpData.Count - 1 Do
end;

// ********************************* Comments **********************************
// Description : making sure, that haoReplaceEvent and haoAllowMultipleEvents are
//               not there the same time
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 04.04.2007
// Last Update : 04.04.2007
// *****************************************************************************
procedure TMAFModuleController.__SetHookAccessOptions(const Value: THookAccessOptions);
var tempValue: THookAccessOptions;
begin
  tempValue := Value;
  If (haoReplaceEvent in Value) and (haoAllowMultipleEvents in Value) then
  Begin
    //Widerspruch: Nutzen wir doch einfach den bisherigen Wert:
    tempValue := tempValue - [haoAllowMultipleEvents, haoReplaceEvent];
    If haoReplaceEvent in FHookAccessOptions then
      Include(tempValue, haoAllowMultipleEvents);
    If haoAllowMultipleEvents in FHookAccessOptions Then
      Include(tempValue, haoReplaceEvent);
  end;
  FHookAccessOptions := tempValue;
end; // __SetHookAccessOptions

function TMAFModuleController.GetDataDef(Idx: Integer): TmafModuleControllerDataTypes;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpDataDefs.Count - 1 do
    If TmafModuleControllerDataTypes(FpDataDefs.Items[i]).TokenID = Idx Then begin
      Result := TmafModuleControllerDataTypes(FpDataDefs.Items[i]);
      Exit;
    end;  //  --  If TERPModuleControllerDataTypes(FpDataDefs.Items[i]).TokenID = Idx Then
end;

function TMAFModuleController.__GetDataDefs_HighID: Integer;
var i : Integer;
begin
  Result := 0;
  For i := 0 To FpDataDefs.Count - 1 Do
    If TmafModuleControllerDataTypes(FpDataDefs.Items[i]).TokenID > Result Then
      Result := TmafModuleControllerDataTypes(FpDataDefs.Items[i]).TokenID;
end;

function TMAFModuleController.__GetDataSize(pData: PDataRecord): Integer;
var i : Integer;
    pToken : PDataToken;
begin
  Result := 0;
  For i := 0 To pData^.FpDataTokens.Count - 1 Do begin
    pToken := PDataToken(pData^.FpDataTokens.Items[i]);
    Case pToken^.aType Of
      sdtString : Result := Result + SizeOf(Pointer);
      sdtDateTime : Result := Result + SizeOf(TDateTime);
      sdtInteger : Result := Result + SizeOf(Integer);
      sdtInt64 : Result := Result + SizeOf(Int64);
      sdtMediaItem : Result := Result + SizeOf(Cardinal) + SizeOf(Byte);
    end;  //  --  Case pToken^.aType Of
  end;  //  --  For i := 0 To pData^.FpDataTokens.Count - 1 Do
end;

function TMAFModuleController.__DoInitialize: Integer;
begin
  Result := ERR_NO_ERROR;
  If Assigned(FOnInitialized) Then
    FOnInitialized(Self, Result);
end;

function TmafModuleController.__FindDataDefs(nDataId: Integer): TmafModuleControllerDataTypes;
var i: Integer;
begin
  For i := 0 To DataDefs.Count - 1 Do
    If TmafModuleControllerDataTypes(DataDefs.Items[i]).TokenID = nDataId Then begin
      Result := TmafModuleControllerDataTypes(DataDefs.Items[i]);
      Break;
    end;
end;

function TMAFModuleController.__Create_DynamicRecord(pData: PDataRecord; mp: TmafParameters = nil): Pointer;
var i, nSize : Integer;
    StringVar : PChar;
    pCurrVar : ^Byte;
    pToken : PDataToken;
    pMT : PMemoryToken;
    aToken : TmafModuleControllerDataTypes;
begin
  If pData = nil Then begin
    Result := nil;
    Exit;
  end;

  nSize := __GetDataSize(pData);
  GetMem(Result, nSize);
  FillChar(Result^, nSize, 0);
  pCurrVar := Result;
  aToken := __FindDataDefs(pData^.DataID);
  For i := 0 To pData^.FpDataTokens.Count - 1 Do begin
    pToken := PDataToken(pData^.FpDataTokens.Items[i]);
    Case pToken^.aType Of
      sdtString   : begin
                      GetMem(StringVar, pToken^.nLength);
                      Move(pToken^.pData^, StringVar^, pToken^.nLength);
                      Move(StringVar, pCurrVar^, SizeOf(Pointer));
                      Inc(pCurrVar, SizeOf(Pointer));
                      If Assigned(mp) Then
                        mp.Add(PRecordDef(aToken.Token.Items[i])^.sName, String(StringVar));
                    end;
      sdtDateTime : begin
                      Move(pToken^.pData^, pCurrVar^, SizeOf(TDateTime));
                      Inc(pCurrVar, SizeOf(TDateTime));
                    end;
      sdtInt64    : begin
                      Move(pToken^.pData^, pCurrVar^, SizeOf(Int64));
                      Inc(pCurrVar, SizeOf(Int64));
                    end;
      sdtInteger  : begin
                      Move(pToken^.pData^, pCurrVar^, SizeOf(Integer));
                      Inc(pCurrVar, SizeOf(Integer));
                      If Assigned(mp) Then
                        mp.Add(PRecordDef(aToken.Token.Items[i])^.sName, Integer(pToken^.pData^));
                    end;
      sdtMediaItem: begin
                      Move(pToken^.pData^, pCurrVar^, SizeOf(Cardinal) + SizeOf(Byte));
                      Inc(pCurrVar, SizeOf(Cardinal) + SizeOf(Byte));
                    end;
    end;  //  --  Case pToken^.aType Of
  end;
  New(pMT);
  pMT^.pDR := pData;     // we know, wich data types where used
  pMT^.pData := Result;  // we know, where the data record is located
  FpMemoryList.Add(pMT); // we save the info in a list that we can find it later on with just the pointer
end;

procedure TMAFModuleController.__Free_DynamicRecord(pData: Pointer);
var pMT : PMemoryToken;
    i, j, nSize : Integer;
    pDT : PDataToken;
    pCurrVar : ^Byte;
    StringVar : PChar;
begin
  For i := FpMemoryList.Count - 1 DownTo 0 Do begin
    pMT := PMemoryToken(FpMemoryList.Items[i]);
    If pMT^.pData = pData Then begin
      nSize := __GetDataSize(pMT^.pDR);
      // free any PChars allocated...
      pCurrVar := pMT^.pData;
      For j := 0 To pMT^.pDR^.FpDataTokens.Count - 1 Do begin
        pDT := PDataToken(pMT^.pDR^.FpDataTokens.Items[j]);
        Case pDT^.aType Of
          sdtString   : begin
                          Move(pCurrVar^, StringVar, SizeOf(Pointer));
                          FreeMem(StringVar, pDT^.nLength);
                          Inc(pCurrVar, SizeOf(Pointer));
                        end;
          sdtDateTime : Inc(pCurrVar, SizeOf(TDateTime));
          sdtInt64    : Inc(pCurrVar, SizeOf(Int64));
          sdtInteger  : Inc(pCurrVar, SizeOf(Integer));
          sdtMediaItem: Inc(pCurrVar, SizeOf(Cardinal) + SizeOf(Byte));
        end;  //  --  Case pDT^.aType Of
      end;
      FreeMem(pMT^.pData, nSize);
      Dispose(pMT);
      FpMemoryList.Delete(i);
      Break;
    end;  //  --  If pMT^.pData = pQHS(QHS)^.pChildObj Then
  end;  //  --  For i := FpMemoryList.Count - 1 DownTo 0 Do 
end;

procedure TMAFModuleController.Setup_Streamer;
begin
  FpStreamer := TTemplateStreamer.Create;
  FpStreamer.OnStreamReadAttribute := __ReadStreamData;
  FpStreamer.OnStreamWriteAttribute := __WriteStreamData;
  FpStreamer.Attributes := 1;
  FpStreamer.StreamVersion := 589633;
end;

procedure TMAFModuleController.__ReadStreamData(Sender: TObject; ID: Integer);
var i, j, nCount1, nCount2 : Integer;
    pData : PDataRecord;
    pToken : PDataToken;
    aType : Byte;
    S : String;
    pID : PMAFInstallToken;
    bActive : Boolean;
begin
  Case FpStreamer.StreamID Of
    767 : begin
            FpStreamer.ReadInteger(nCount1);  // number of entries
            FpData.Capacity := nCount1;
            For i := 1 To nCount1 Do begin
              pData := __Create_DataRecord;
              FpStreamer.ReadInteger(pData^.ID);
              FpStreamer.ReadInteger(pData^.DataID);
              FpStreamer.ReadInteger(pData^.HookID);
              FpStreamer.ReadInteger(pData^.SubHookID);
              FpStreamer.ReadString(pData^.Description);
              If FpStreamer.StreamVersion > 589633 Then
                FpStreamer.ReadBoolean(pData^.bUseDatablock); // <-- new in StreamVersion 589634
              If FpStreamer.StreamVersion > 589634 Then begin
                FpStreamer.ReadCardinal(pData^.uID);          // <-- new in StreamVersion 589635
                If FnCurrent_uID < pData^.uID Then            // determinating the highest uID used
                  FnCurrent_uID := pData^.uID;
              end;
              FpStreamer.ReadByte(Byte(pData^.Action));
              FpStreamer.ReadInteger(nCount2);
              pData^.FpDataTokens.Capacity := nCount2;
              For j := 1 To nCount2 Do begin
                FpStreamer.ReadByte(aType);
                pToken := __Create_DataToken(aType);  // reserves memory for the data itself already and sets the right type etc.
                FpStreamer.ReadInteger(pToken^.nLength);
                Case pToken^.aType Of
                  sdtInteger     : FpStreamer.ReadInteger(Integer(pToken^.pData^));
                  sdtDateTime    : FpStreamer.ReadDateTime(TDateTime(pToken^.pData^));
                  sdtInt64       : FpStreamer.ReadInt64(Int64(pToken^.pData^));
                  sdtString      : begin
                                     FpStreamer.ReadString(S);
                                     pToken^.nLength := StrToPChar(S, PChar(pToken^.pData));
                                   end;
                  sdtMediaItem   : begin
                                     FpStreamer.ReadByte(PmafMediaItem(pToken^.pData)^.MediaType);
                                     FpStreamer.ReadCardinal(PmafMediaItem(pToken^.pData)^.MediaID);
                                   end;
                end;  //  --  Case pToken^.aType Of
                pData^.FpDataTokens.Add(pToken);
              end;  //  --  For j := 1 To nCount2 Do
              FpData.Add(pData);
            end;  //  --  For i := 1 To nCount1 Do
          end; // 767
    768 : begin
            FpStreamer.ReadInteger(nCount1);
            FpInstallData.Capacity := nCount1;
            For i := 1 To nCount1 Do begin
              pID := __Create_InstallToken(iaInsert);
              FpStreamer.ReadInteger(pID^.uID);
              FpStreamer.ReadInteger(pID^.nHookID);
              FpStreamer.ReadInteger(pID^.nSubHookID);
              FpStreamer.ReadInteger(pID^.nCodeGroupID);
              FpStreamer.ReadInteger(pID^.nRelative_uID);
              FpStreamer.ReadByte(aType);
              Case aType Of
                1 : pID^.InsertDir := hidFirst;
                2 : pID^.InsertDir := hidLast;
                3 : pID^.InsertDir := hidBefore;
                4 : pID^.InsertDir := hidAfter;
                5 : pID^.InsertDir := hidOverwrite;
                Else pID^.InsertDir := hidLast;
              end;
              FpStreamer.ReadBoolean(bActive);
              If bActive Then pID^.bActive := 1
                         Else pID^.bActive := 0;
              pID^.nModuleID := FpModuleInfo.ModuleID;
              FpInstallData.Add(pID);
            end;  //  --  For i := 1 To nCount1 Do
          end;
  end;
end;

procedure TMAFModuleController.__WriteStreamData(Sender: TObject; ID: Integer);
var i, j : Integer;
    pData : PDataRecord;
    pToken : PDataToken;
    pID : PMAFInstallToken;
    b : Byte;
begin
  Case FpStreamer.StreamID Of
    767 : begin
            FpStreamer.WriteInteger(FpData.Count);    // number of entries
            For i := 0 To FpData.Count - 1 Do begin
              pData := PDataRecord(FpData.Items[i]);
              FpStreamer.WriteInteger(pData^.ID);
              FpStreamer.WriteInteger(pData^.DataID);
              FpStreamer.WriteInteger(pData^.HookID);
              FpStreamer.WriteInteger(pData^.SubHookID);
              FpStreamer.WriteString(pData^.Description);
              FpStreamer.WriteBoolean(pData^.bUseDatablock); // <-- new in StreamVersion 589634
              FpStreamer.WriteCardinal(pData^.uID);          // <-- new in StreamVersion 589635
              FpStreamer.WriteByte(Byte(pData^.Action));
              FpStreamer.WriteInteger(pData^.FpDataTokens.Count);
              For j := 0 To pData^.FpDataTokens.Count - 1 Do begin
                pToken := PDataToken(pData^.FpDataTokens.Items[j]);
                FpStreamer.WriteByte(pToken^.aType);
                FpStreamer.WriteInteger(pToken^.nLength);
                Case pToken^.aType Of
                  sdtInteger     : FpStreamer.WriteInteger(Integer(pToken^.pData^));
                  sdtDateTime    : FpStreamer.WriteDateTime(TDateTime(pToken^.pData^));
                  sdtInt64       : FpStreamer.WriteInt64(Int64(pToken^.pData^));
                  sdtString      : FpStreamer.WriteString(String(PChar(pToken^.pData)));
                  sdtMediaItem   : begin
                                     FpStreamer.WriteByte(PmafMediaItem(pToken^.pData)^.MediaType);
                                     FpStreamer.WriteCardinal(PmafMediaItem(pToken^.pData)^.MediaID);
                                   end;
                end;  //  --  Case pToken^.aType Of
              end;  //  --  For j := 0 To pData^.FpDataTokens.Count - 1 Do
            end;  //  --  For i := 0 To FpData.Count - 1 Do
          end; // 767
    768 : begin
            FpStreamer.WriteInteger(FpInstallData.Count);
            For i := 0 To FpInstallData.Count - 1 Do begin
              pID := PMAFInstallToken(FpInstallData.Items[i]);
              FpStreamer.WriteInteger(pID^.uID);
              FpStreamer.WriteInteger(pID^.nHookID);
              FpStreamer.WriteInteger(pID^.nSubHookID);
              FpStreamer.WriteInteger(pID^.nCodeGroupID);
              FpStreamer.WriteInteger(pID^.nRelative_uID);
              Case pID^.InsertDir Of
                hidFirst     : b := 1;
                hidLast      : b := 2;
                hidBefore    : b := 3;
                hidAfter     : b := 4;
                hidOverwrite : b := 5;
                Else b := 2;
              end;
              FpStreamer.WriteByte(b);
              FpStreamer.WriteBoolean((pID^.bActive > 0));
            end;
          end; // 768
  end;
end;

procedure TMAFModuleController.ReadDataDefs(aStream: TStream);
var i, nCount : Integer;
    aToken : TmafModuleControllerDataTypes;
begin
  aStream.Read(nCount, SizeOf(Integer));
  For i := 1 To nCount Do begin
    aToken := TmafModuleControllerDataTypes.Create;
    aToken.ReadDataDefs(aStream);
    FpDataDefs.Add(aToken);
  end;  //  --  For i := 1 To nCount Do
end;

procedure TMAFModuleController.WriteDataDefs(aStream: TStream);
var i, nCount : Integer;
begin
  nCount := FpDataDefs.Count;
  aStream.Write(nCount, SizeOf(Integer));
  For i := 0 To FpDataDefs.Count - 1 Do
    TmafModuleControllerDataTypes(FpDataDefs.Items[i]).WriteDataDefs(aStream);
end;

procedure TMAFModuleController.WriteInstallData(aStream: TStream);
begin
  Setup_Streamer;
  FpStreamer.StreamVersion := 589633;
  FpStreamer.WriteStream(768, TMemoryStream(aStream));
  FreeAndNil(FpStreamer);
end;

procedure TMAFModuleController.Read_StreamedData(aStream: TStream);
begin
  Setup_Streamer;
  FpStreamer.ReadStream(TMemoryStream(aStream));
  FreeAndNil(FpStreamer);
end;

procedure TMAFModuleController.WriteData(aStream: TStream);
begin
  Setup_Streamer;
  FpStreamer.StreamVersion := 589635;
  FpStreamer.WriteStream(767, TMemoryStream(aStream));
  FreeAndNil(FpStreamer);
end;

procedure TMAFModuleController.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('DataDefs', ReadDataDefs, WriteDataDefs, True);
  Filer.DefineBinaryProperty('MC_Data', Read_StreamedData, WriteData, True);
  Filer.DefineBinaryProperty('InstallData', Read_StreamedData, WriteInstallData, True);
end;

// ********************************* Comments **********************************
// Description : checks, if an the combination SubHookID and EventHandler already
//               exist
// Param (in)  : nSubHookID=ID of the SubHook
//               FEvent=Event handler for this SubHook
// Param (out) : the first token,that matches, found
// Coding by   : Helge Lange
// Date        : 04.04.2007
// Last Update : 04.04.2007
// *****************************************************************************
function TMAFModuleController.__EventExists(nSubHookID: Integer; FEvent: TOnSubHook): PEventToken;
var i : Integer;
    pToken: PEventToken;
begin
  Result := nil;
  For i := 0 To FpEventList.Count - 1 Do begin
    pToken := PEventToken(FpEventList.Items[i]);
    If Not (haoAllowMultipleEvents in FHookAccessOptions) Then begin
      // no multiple handler are allowed, so it's enough to find one item with
      // the same SubHookID
      If pToken^.nSubHookID = nSubHookID Then begin // same subHookID ?
        Result := pToken;
        Exit;
      end;  //  --  If @pToken^.OnSubHook = @FEvent Then
    end else begin
      // multiple handler are allowed, so we just see, if we have the same handler
      // assigned to the same SubHook, because after we don't want to call a handler
      // twice !
      If pToken^.nSubHookID = nSubHookID Then       // same subHookID ?
        If @pToken^.OnSubHook = @FEvent Then begin  // and same event handler ?
          Result := pToken;
          Exit;
        end;  //  --  If @pToken^.OnSubHook = @FEvent Then
    end;  //  --  If (Not haoAllowMultipleEvents in FHookAccessOptions) Then
  end;  //  --  For i := 0 To FpEventList.Count - 1 Do
end; // __EventExists

function TMAFModuleController.RegisterFormClass(SubHookID: Integer; aFormClass: TComponentClass): Boolean;
var i : Integer;
    pData : PFormRegisterData;
begin
  // SubHookID can be registered just once !
  For i := 0 To FpFormTypeList.Count - 1 Do
    If PFormRegisterData(FpFormTypeList.Items[i])^.nSubHookID = SubHookID Then begin
      // if exist, we just replace the currently saved FormClass
      PFormRegisterData(FpFormTypeList.Items[i])^.FormType := aFormClass;
      Result := (PFormRegisterData(FpFormTypeList.Items[i])^.FormData <> nil);
      Exit;
    end;  //  --  If PFormRegisterData(FpFormTypeList.Items[i])^.nSubHookID = SubHookID Then

  // obviously the SubHookID isn't registered yet
  New(pData);
  pData^.nSubHookID := SubHookID;
  pData^.FormType := aFormClass;
  pData^.FormData := nil;
  FpFormTypeList.Add(pData);

  // now we get the data connected to the subhook 
  For i := 0 To FpData.Count - 1 Do
    If PDataRecord(FpData.Items[i])^.SubHookID = SubHookID Then begin
      pData^.FormData := FpData.Items[i];
      Break;
    end;  //  --  If PDataRecord(FpData.Items[i])^.SubHookID = SubHookID Then
  Result := (pData^.FormData <> nil);
end;

procedure TmafModuleController.RegisterFormID(nFormID, nHookID : Integer; nSubHookID: Integer = 0);
var QHS: pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.HookID := HM_WINDOWMANAGER_QUERY;
  QHS^.SubHookID := WM__REGISTER_FORM_ID;
  QHS^.Reserved1 := nFormID;
  QHS^.Reserved2 := nHookID;
  QHS^.PReserved1 := Pointer(nSubHookID);
  CallForeignManager(MT_WINDOW_MANAGER, HM_WINDOWMANAGER_QUERY, QHS, nil);
  __Free_QueryHandlerStruct(QHS);
end;

// ********************************* Comments **********************************
// Description : adds an event to the event list, if it's not there already
// Param (in)  : nSubHookID=ID of the SubHook
//               FEvent=Event handler for this SubHook
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 04.04.2007
// Last Update : 04.04.2007
// *****************************************************************************
procedure TMAFModuleController.RegisterSubHookEvent(nSubHookID: Integer; FEvent: TOnSubHook; EP: TEventPriority = epNormal);
var pToken: PEventToken;
    i : Integer;
begin
  If Assigned(FEvent) Then begin        // just to be sure
    pToken := __EventExists(nSubHookID, FEvent);
    // just replace
    If ((haoReplaceEvent in FHookAccessOptions) And (pToken <> nil)) Then begin
      pToken^.OnSubHook := FEvent;
      Exit;
    end;
    // now the token was not found or we cannot just replace the event
    // so let's see, if we got a token, but we're not allowed to place another
    If ((Not (haoAllowMultipleEvents in FHookAccessOptions)) And (pToken <> nil)) Then
      Exit; // nope.. so let's go

    // now we 're allowed to place another token or we don't have the token yet
    New(pToken);
    pToken^.nSubHookID := nSubHookID;
    pToken^.OnSubHook := FEvent;
    pToken^.aPriority := EP;
    Case EP Of
      epUltraHigh : If FpEventList.Count > 0 Then begin // ultrahigh comes first to be executed fast
                      FpEventList.Insert(0, pToken);
                      Exit;
                    end;
      epHigh      : For i := 0 To FpEventList.Count - 1 Do
                      If PEventToken(FpEventList.Items[i])^.aPriority <> epUltraHigh Then begin
                        FpEventList.Insert(i, pToken);
                        Exit;
                      end;
      epNormal    : For i := 0 To FpEventList.Count - 1 Do
                      If ((PEventToken(FpEventList.Items[i])^.aPriority <> epUltraHigh) And (PEventToken(FpEventList.Items[i])^.aPriority <> epHigh)) Then begin
                        FpEventList.Insert(i, pToken);
                        Exit;
                      end;
    end;  //  --  Case EP Of
    // if it wasn't added above, we can add it behind all current items 
    FpEventList.Add(pToken);
  end;  //  --  If Assigned(FEvent) Then
end; // RegisterSubHookEvent

procedure TMAFModuleController.UnregisterSubHookEvent(nSubHookID: Integer; FEvent: TOnSubHook);
var pToken: PEventToken;
begin
  pToken := __EventExists(nSubHookID, FEvent);
  If pToken <> nil Then begin
    FpEventList.Delete(FpEventList.IndexOf(pToken));
    Dispose(pToken);
  end;  //  --  If pToken <> nil Then
end;

// ********************************* Comments **********************************
// Description : executes event(s) assigned to a SubHook
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 03.04.2007
// Last Update : 25.06.2009
// *****************************************************************************
function TMAFModuleController.Execute(nSubHookID: Integer; aQHS, pUserParam: Pointer): Integer;
var ErrCode, i : Integer;
    bExecuted  : Boolean;
    pData : PDataRecord;
begin
  {$IFDEF Tracer}
  MAFTracer.Enter('TmafModuleController.Execute');
  MAFTracer.Log_Integer('SubHookID', nSubHookID);
  {$ENDIF}
  Result := Err_NO_ERROR;
  Case nSubHookID Of
    HM_CREATE,
    MAN_CREATE_MANAGER : begin
                           If aQHS <> nil Then
                             pQHS(aQHS)^.pChildObj := Self;  // we return a pointer to ourselfs
                           If Assigned(FOnCreate) Then
                             FOnCreate(Self);
                           Exit;
                         end;
  end;  //  --  Case nSubHookID Of
  bExecuted := False;
  ErrCode := ERR_SUBHOOK_NOT_FOUND;
  If Assigned(FOnDatabase) Then
    FOnDatabase(nSubHookID, pQHS(aQHS)^);
  ErrCode := ERR_NO_ERROR;

  If nSubHookID > 1000 Then begin
    pData := GetDataToken(pQHS(aQHS)^.HookID, nSubHookID);
    If Assigned(pData) Then
      If pData^.bUseDatablock Then
        pQHS(aQHS)^.pDataBlock := __Create_DynamicRecord(pData, pQHS(aQHS)^.pParams);  // returns nil, if pData is nil
  end;

  // First we check the registered events from our event list
  // those come from connected components and when the user explicitly registers
  // another event handler than the standard one
  // With the component registered SubHooks go also here and according to the
  // defined action they call their respective event handlers
  For i := 0 To FpEventList.Count - 1 Do
    If PEventToken(FpEventList.Items[i])^.nSubHookID = nSubHookID Then begin
      {$IFDEF Tracer}
      MAFTracer.CheckPoint('Executing special OnSubHook event...');
      {$ENDIF}
      PEventToken(FpEventList.Items[i])^.OnSubHook(nSubHookID, pQHS(aQHS), pUserParam, ErrCode);
      bExecuted := True;
      If Not (haoAllowMultipleEvents in FHookAccessOptions) Then
        Break;  // if there is just 1 event per hook anyway, then we can stop right now
    end;  //  --  If PEventToken(FpEventList.Items[i])^.nSubHookID = nSubHookID Then

  // check, if we should always execute the standard event
  If (haoAlwaysStandardEvent in FHookAccessOptions) Then
    bExecuted := False;

  // if no special event is registered, we fire the standard event handler (if available)
  If Not bExecuted Then
    If Assigned(FOnSubHook) Then begin
      {$IFDEF Tracer} MAFTracer.CheckPoint('Executing standard OnSubHook event'); {$ENDIF}
      FOnSubHook(nSubHookID, pQHS(aQHS), pUserParam, ErrCode);
    end;
  Result := ErrCode;
  {$IFDEF Tracer} MAFTracer.Leave; {$ENDIF}
end; // Execute

function TMAFModuleController.GetDataToken(nHookID, nSubHookID: Integer): PDataRecord;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpData.Count - 1 Do
    If ((PDataRecord(Fpdata.Items[i])^.HookID = nHookID) And (PDataRecord(Fpdata.Items[i])^.SubHookID = nSubHookID)) Then begin
      Result := PDataRecord(Fpdata.Items[i]);
      Break;
    end;
end;

function TMAFModuleController.GetEventToken(nSubHookID: Integer): PEventToken;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpEventList.Count - 1 Do
    If PEventToken(FpEventList.Items[i])^.nSubHookID = nSubHookID Then begin
      Result := PEventToken(FpEventList.Items[i]);
      Break;  // it's enough to find one to know, that there are events registered
    end;
end;

function TmafModuleController.GetNext_uID: Cardinal;
begin
  // make sure, the current (although greater than 0) is within the limits of the lower range
  // this is just for compatibility for older versions
  If FnCurrent_uID > 0 Then begin
    Inc(FnCurrent_uID);
    Result := FnCurrent_uID;
  end Else
    Result := FnStart_uID;
  If Result = 0 Then
    Raise EComponentError.Create('TmafModuleController setup failure : No Start_uID defined!');
  If Result > (FnStart_uID + FnRange_uID) Then
    Raise EComponentError.Create('TmafModuleController uID Range Error : End of Range reached!');
end;

procedure TMAFModuleController.__CreateForm(aFormClass: TComponentClass; var Reference; AOwner: TComponent);
var Instance: TComponent;
begin
  Instance := TComponent(aFormClass.NewInstance);
  TComponent(Reference) := Instance;
  If AOwner = nil Then
    AOwner := Self;
  try
    Instance.Create(AOwner);
  except
    TComponent(Reference) := nil;
  end;  //  --  try... except
end;

function TMAFModuleController.__GetFormByHookID(HookID, SubHookID: Integer): Pointer;
var QHS: pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.HookID := HookID;
  QHS^.SubHookID := SubHookID;
  CallForeignManager(MT_WINDOW_MANAGER, WM__FIND_WINDOW_BY_HOOK, QHS, nil);
  Result := QHS^.pChildObj;
  __Free_QueryHandlerStruct(QHS);
end;

function TmafModuleController.__GetOpenFormCount: Integer;
begin
  Result := FpCreatedForms.Count;
end;

function TMAFModuleController.CreateForm(SubHookID: Integer; QHS: pQHS; pUserData: Pointer): Integer;
var pToken: PFormRegisterData;
    aComp: TComponent;
    i : Integer;
    aController : TmafWindowController;
    pData : PDataRecord;
    bContinue : Boolean;
begin
  {$IFDEF Tracer}
  MAFTracer.Enter('TmafModuleController.CreateForm');
  {$ENDIF}
  Result := ERR_MC_FORMTYPE_UNKNOWN;
  pToken := nil;
  For i := 0 To FpFormTypeList.Count - 1 Do
    If PFormRegisterData(FpFormTypeList.Items[i])^.nSubHookID = SubHookID Then begin
      pToken := PFormRegisterData(FpFormTypeList.Items[i]);
      Break;
    end;  //  --  If PFormRegisterData(FpFormTypeList.Items[i])^.nSubHookID = SubHookID Then


  // ask windowmanager, if there is a window with this HookID and SubHookID  and if it
  // is allowed to create another one
  aController := __GetFormByHookID(QHS^.HookID, QHS^.SubHookID);
  If Assigned(aController) Then begin
    If woSingleWindow in aController.WindowOptions Then begin
      If daSingleWindowClose in aController.DefaultActions Then begin
        QHS^.pChildObj := aController.Owner;
        DestroyForm(SubHookID, QHS, pUserData);
      end else
        TForm(aController.Owner).BringToFront;
      Result := ERR_NO_ERROR;
      Exit;
    end;  //  --  If woSingleWindow in aController.WindowOptions Then
  end;  //  --  If Assigned(aController) Then

  // If there are data attached...
  If pToken <> nil Then begin
    pData := GetDataToken(QHS^.HookID, SubHookID);
    If Not Assigned(QHS^.pDataBlock) Then
      If pData^.bUseDatablock Then
        QHS^.pDataBlock := __Create_DynamicRecord(pData, QHS^.pParams);  // returns nil, if pData is nil

  // we inform that a form is about to be created
  bContinue := True;
  If Assigned(FBeforeFormCreate) Then
    FBeforeFormCreate(Self, QHS, QHS^.SubHookID, QHS^.pDataBlock, bContinue);
  // If the user decides, that we cannot continue, we leave...
  If Not bContinue Then begin
    If QHS^.pDataBlock <> nil Then begin
      __Free_DynamicRecord(pData);
      QHS^.pDataBlock := nil;
    end;
    Exit;
  end;

    Result := ERR_MC_FORM_CREATION;
    __CreateForm(pToken^.FormType, aComp, TComponent(QHS^.pParent));

    If aComp = nil Then
      Raise EComponentError.Create('Couldn''t create class "'+pToken^.FormType.ClassName+'"');

    aController := GetController(aComp);
    If aController <> nil Then begin
      If QHS^.Reserved1 = 1 Then begin   // means, it will run as standalone now
        If IsClass(aComp, TForm) Then begin
          TForm(aComp).BorderStyle := bsDialog;
          TForm(aComp).BorderIcons := [biSystemMenu, biMinimize, biMaximize];
          TForm(aComp).Align := alNone;
        end;
      end;
      TScrollingWinControl(aComp).Left := QHS^.Reserved1;
      TScrollingWinControl(aComp).Top := QHS^.Reserved2;
      aController.FreeNotification(Self);               // we want to know, when this form goes
      aController.ModuleID := FpModuleInfo.ModuleID;    // we set the ModuleID, HookID and SubHookID will be set by the WindowController
      Try
        SendComponentMessage(aComp, MSG_SET_FORM_DATA, pUserData, QHS, True);
        SendComponentMessage(aComp, MSG_FORM_INITIALIZED, pUserData, QHS, True);
        QHS^.pChildObj := aComp;
        If Assigned(OnFormCreated) Then
          OnFormCreated(pToken^.nSubHookID, TObject(QHS^.pChildObj));
        If Assigned(pData) Then begin
          If pData^.Action = daCreateWindowShow Then
            If IsClass(aComp, TForm) Then
              TScrollingWinControl(aComp).Show;
          If ((pData^.Action = daCreateWindowShowModal) Or (pData^.Action = daCreateEnumWindowModal)) Then begin
            If IsClass(aComp, TForm) Then begin
              QHS^.ResVal := TForm(aComp).ShowModal;
              If Assigned(QHS^.pParams) Then
                If QHS^.pParams.ParamByName['ModalResult'] <> nil Then
                  QHS^.pParams.ParamByName['ModalResult'].Value := QHS^.ResVal
                Else
                  QHS^.pParams.Add('ModalResult', QHS^.ResVal);
              TForm(aComp).Free;
            end else
              aComp.Free;
            aController := nil;
            QHS^.pChildObj := nil;
          end;
        end;
        Result := ERR_NO_ERROR;
        If aController <> nil Then
          FpCreatedForms.Add(aController);
      Except
        Result := ERR_MC_FORM_INIT;
      End;
    end;
    If ((Not pData^.bUseDatablock) And (QHS^.pDataBlock <> nil)) Then
      __Free_DynamicRecord(QHS^.pDataBlock);
  end;  //  --  If pToken <> nil Then
  {$IFDEF Tracer} MAFTracer.Leave; {$ENDIF}
end;

procedure TMAFModuleController.DestroyForm(SubHookID: Integer; QHS: pQHS; pUserData: Pointer);
var aComp: TComponent;
    aController : TmafWindowController;
    idx : Integer;
begin
  {$IFDEF Tracer}
  MAFTracer.Enter('TmafModuleController.DestroyForm');
  {$ENDIF}
  aComp := TComponent(QHS^.pChildObj);
  aController := GetController(aComp);
  If Assigned(FOnFormDestroy) Then
    FOnFormDestroy(SubHookID, TObject(QHS^.pChildObj));
  If aController <> nil Then begin
    If woSave in aController.WindowOptions Then
      aController.Close; // triggering OnCloseForm event
    SendComponentMessage(aController, MSG_BEFORE_FORM_CLOSE, nil, nil, False);
  end;
  Try
    TScrollingWinControl(QHS^.pChildObj).Free;
    idx := FpCreatedForms.IndexOf(aController); // it should have been done already with the destroy of the windowcontroller
    If idx > -1 Then                            // but better safe than sorry :)
      FpCreatedForms.Delete(idx);
  except
    { $message hint 'error in TmafModuleController.DestroyForm'}
  End;

  QHS^.pChildObj := nil;
  {$IFDEF Tracer} MAFTracer.Leave; {$ENDIF}
end;

// ********************************* Comments **********************************
// Description : special event for the option "Create Window" in the component
//               editor.
// Param (in)  : standard OnSubHook event
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 18.07.2009
// Last Update : 24.07.2009
// *****************************************************************************
procedure TMAFModuleController.__OnCreateWindow(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  {$IFDEF Tracer}
  MAFTracer.Enter('TmafModuleController.__OnCreateWindow');
  {$ENDIF}
  If QHS^.pChildObj = nil Then
    ErrCode := CreateForm(SubHookID, QHS, UserParam)
  Else
    DestroyForm(SubHookID, QHS, UserParam);
  If haoAlwaysStandardEvent in FHookAccessOptions Then
    If Assigned(FOnSubHook) Then
      FOnSubHook(SubHookID, QHS, UserParam, ErrCode);
  {$IFDEF Tracer} MAFTracer.Leave; {$ENDIF}
end;

procedure TMAFModuleController.__OnCreateEnumWindow(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  {$IFDEF Tracer}
  MAFTracer.Enter('TmafModuleController.__OnCreateEnumWindow');
  {$ENDIF}
  If ((QHS^.pParent = nil) And (QHS^.pChildObj = nil)) Then
    __OnReturnData(SubHookID, QHS, UserParam, ErrCode)
  else
    __OnCreateWindow(SubHookID, QHS, UserParam, ErrCode);
  {$IFDEF Tracer} MAFTracer.Leave; {$ENDIF}
end;

// ********************************* Comments **********************************
// Description : special event for the option "Return Data" in the component
//               editor.
// Param (in)  : standard OnSubHook event
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 18.07.2009
// Last Update : 04.04.2013
// *****************************************************************************
procedure TMAFModuleController.__OnReturnData(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
var pData : PDataRecord;
    CanContinue: Boolean;
begin
  {$IFDEF Tracer}
  MAFTracer.Enter('TmafModuleController.__OnReturnData');
  {$ENDIF}
  CanContinue := True;
  If Assigned(FBeforeReturnData) Then
    FBeforeReturnData(Self, QHS, QHS^.SubHookID,  UserParam, CanContinue);

  If CanContinue Then begin
    pData := GetDataToken(QHS^.HookID, SubHookID);
    QHS^.pChildObj := __Create_DynamicRecord(pData);  // returns nil, if pData is nil
    QHS^.pFreeMemFunc := __Free_DynamicRecord;        // to free the memory after delivery
  end;
  {$IFDEF Tracer} MAFTracer.Leave; {$ENDIF}
end;

procedure TmafModuleController.__ReCalc_uID;
var i: Integer;
begin
  For i := 0 To FpData.Count - 1 Do
    If PDataRecord(FpData.Items[i])^.uID > FnCurrent_uID Then
      FnCurrent_uID := PDataRecord(FpData.Items[i])^.uID;
end;

end.
