{*******************************************************************************
Name         : uMAF_WindowController.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2004-2012 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 28.08.2004
Last Update  : 21.06.2012
Version      : 1.1.012
Last Changes :

1.1.012 (21.06.2012)------------------------------------------------------------
- [ADD] support for resource based Form icon
1.1.011 (23.09.2009)------------------------------------------------------------
- [ADD] added OnSetDatabase event, which is fired before the formluar is shown
        and occurs, when a database and transaction is set by the caller or
        the HookManager
1.1.010 (16.09.2009)------------------------------------------------------------
- [ADD] coRegisterFormID added, where the user can register a form with a unique
        FormID with the WindowController and open that Window by it's FormID only
        there would be no need to know the HookID
1.1.009 (12.09.2009)------------------------------------------------------------
- [CHG] while not in design mode setting the property CaptionID will change
        the caption instantly 
1.1.008 (28.07.2009)------------------------------------------------------------
- [ADD] properties HookID and SubHookID to identify, wich function exactly was
        called to open the window we're sitting on. Filled by MSG_SetFormData
        and read-only
- [ADD] message procedure for MSG_BEFORE_FORM_CLOSE, wich is called by the
        ModuleController, after TERPWindowController.Close is called and before
        the window is released
- [ADD] DefaultAction property, that defines, what to do, when woSingle is set in
        WindowOptions and a window is called to be opened a 2nd time
1.1.007 (06.11.2008)------------------------------------------------------------
- [ADD] function GetController, that finds the WindowController on a component
        and returns it
1.1.006 (03.11.2008)------------------------------------------------------------
- [ADD] TLoadSaveControl to have better and more visible control over automatic
        load/save functionality
1.1.005 (11.10.2008)------------------------------------------------------------
- [ADD] ioToggle in InitOptions added, where the FormAccessManager can decide,
        wether he creates the Window or even destroyes an existing one
- [ADD] function FindWindowByFormID added, wich finds a window with the same
        FormID
1.1.004 (26.09.2008)------------------------------------------------------------
- [ADD] window status flags field "FStatusFlags" and first status WF_DIRTY
- [DEL] boolean variable FbDirty has been removed and replaced by 32-bit status
        flag member (see above)
1.1.003 (14.04.2007)------------------------------------------------------------
- [ADD] property ControllerOptions
- [ADD] property WindowOptions
- [ADD] property HookClient
- [ADD] Load/SaveData with events and data pointer support
1.1.002 (01.03.2007)------------------------------------------------------------
- [CHANGE] changed to use TManagerBaseContainer
1.0.001 (28.08.2004)------------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_WindowController;

interface

uses Windows, Messages, SysUtils, Classes, Forms, dialogs, Graphics,
     // Modular Application Framework Components units
     uMAF_Core, uMAF_Globals, uMAF_HookClient, uMAF_ResourceClient;

     // coAutoRegister  = will (un)register the window in the WindowManager
     // coAutoLoadData  = will manage load/save of data through the HookClient
     // coRegisterMainForm = the current form will be set as main form
Type TControllerOption  = (coAutoRegister, coAutoLoadData, coRegisterMainForm);
     TControllerOptions = Set Of TControllerOption;

     // woLoadSave      = WC can be used to load / save data automatically
     //                 = ParentWindow controller can set an ID to load data
     // woSingleWindow  = only one type of the window is allowed in the system,
     //                   creation of a 2nd one will be prohibited to the FAM
     // woWindowNumber  = adds a window number to the caption
     // woRootWindow    = set only for main form of the application
     // woShowInList    = set ths flag, if a window should be visible in a WindowList
     TWindowOption      = (woLoad, woSave, woSingleWindow, woWindowNumber, woRootWindow, woShowInList);
     TWindowOptions     = Set of TWindowOption;

     // ioSetParentHandle = ParentHandle will be set when form created
     // ioSetParent       = the parent will be set (the parent window must be visible at this time!!!)
     // ioToggle          = If the window type (FormID) exists, it will be
     //                     destroyed and the new not created
     TInitOption        = (ioSetParentHandle, ioSetParent, ioToggle);
     TInitOptions       = Set of TInitOption;

     TLoadSaveEvent = procedure(Sender: TObject; pData: Pointer; ErrCode: Integer) Of Object;
     TDataPointerEvent = procedure(Sender: TObject; var pData: Pointer) Of Object;
     TDatabaseEvent = procedure(Sender: TObject; ADataBase: TComponent; ATransaction: TComponent) Of Object;
     TOnInitForm = procedure(Sender: TObject; QHS: pQHS; pUserParam: Pointer; var ErrCode: Integer) Of Object;

     // lsoSameHookID = for load/save/free the same HookID will be used and
     // QHS^.Reserved1 will be set to 1=Load, 2=Save and 3=free data
     // the reasopn for a "free" is that memory should be released, where it was
     // allocated, so we have a method to send memory created somewhere else back
     // to its module of creation and let them release the memory
     //
     // lsoUseDataID = DataID will be send to receiver in QHS^.CommandID to load
     // a record for example from a database, that needs an ID
     // Useful, if one has a UserList window for example and wants to edit, then
     // the UserID can be send when creating the window with the WindowController
     // in QHS^.Reserved2 and it will be set automatically into DataID and we can
     // load the userdata without a line of code more
     TLoadSaveOption = (lsoSameHookID, lsoUseDataID);
     TLoadSaveOptions = Set Of TLoadSaveOption;

     // daSingleWindowClose = If a window is marked as woSingleWindow and called
     //                       a 2nd time, this defines, how the ModuleController
     //                       should react. If daSingleWindow is set, the window
     //                       will be freed, otherwise brought to front 
     TDefaultAction = (daSingleWindowClose);
     TDefaultActions = Set Of TDefaultAction;

     TLoadSaveControl = class(TPersistent)
     private
       FnLoadHookID : Integer;  // HookID to load data
       FnSaveHookID : Integer;  // HookID to save data
       FnFreeHookID : Integer;  // HookID to free data
       FnSubHookID  : Integer;  // SubHookID in case one uses a general Hook for several loads/saves
       FnDataID     : Integer;  // DataID, if needed
       FLoadSaveOptions : TLoadSaveOptions;
       procedure __SetFreeHookID(const Value: Integer);
       procedure __SetLoadHookID(const Value: Integer);
       procedure __SetSaveHookID(const Value: Integer);
     public
       constructor Create;
     published
       property LoadSaveOptions : TLoadSaveOptions read FLoadSaveOptions write FLoadSaveOptions default [lsoSameHookID];
       property DataID : Integer read FnDataID write FnDataID;
       property SubHookID : Integer read FnSubHookID write FnSubHookID default 0;
       property LoadHookID : Integer read FnLoadHookID write __SetLoadHookID default 0;
       property SaveHookID : Integer read FnSaveHookID write __SetSaveHookID default 0;
       property FreeHookID : Integer read FnFreeHookID write __SetFreeHookID default 0;
     end;

     TmafWindowController = class(TmafBaseComponent)
     private
       FpHookClient : TmafHookClient;            // Hook access
       FpResClient : TmafResourceClient;         // access to localized resources
       FpParentController : TmafWindowController; // the Controller above us in the hierarchy
       FControllerOptions : TControllerOptions;  // controller options
       FDefaultActions    : TDefaultActions;     // set's the default actions
       FWindowOptions     : TWindowOptions;      // window options
       FInitOptions       : TInitOptions;        // controller init options
       FLoadSaveControl   : TLoadSaveControl;    // load/save informations
       FStatusFlags       : DWORD;               // window status flags (WF_ prefix)
       FnFormID : Integer;                       // unique ID for the form
       FnDataID : Integer;                       // unique ID for the data loaded atm
       FnHookID : Integer;                       // The HookID that opened the window
       FnFormIconID : Integer;                   // ID to load an icon from resource file
       FnSubHookID : Integer;                    // the SubHookID that opened this window
       FnModuleID : Integer;                     // the module we're in
       FnCaptionID : Integer;                    // Titel caption ID for localized captions
       FOnGetDataPointer,                        // Event to let the form create the data pointer
       FOnFreeDataPointer : TDataPointerEvent;   // Event to release the memory reserved in FOnGetDataPointer
       FOnLoadData,                              // OnLoadData event
       FOnSaveData  : TLoadSaveEvent;            // OnSaveData event
       FOnInitForm : TOnInitForm;                // InInitForm event
       FOnCloseForm : TNotifyEvent;              // OnCloseForm event
       FOnSetDatabase : TDatabaseEvent;          // OnSetDatabase event
       FInternalFlags : DWORD;                   // internal flags
       FNotifyList : TList;                      // list of components to notify about stuff
       FOnCloseQuery_Save : TCloseQueryEvent;    // the forms CloseQuery event
       function __GetClientCount: Integer;
       procedure __SetDirty(const Value: Boolean);
       function __GetDirty: Boolean;
       function __GetDataID: Integer;
       procedure __SetDataID(const Value: Integer);
       function __CanClose: Boolean;
       procedure __SetResClient(const Value: TmafResourceClient);
       function __FindParentController: TmafWindowController;
       function __GetClosing: Boolean;
       procedure __SetClosing(const Value: Boolean);
       procedure __SetCaptionID(const Value: Integer);
       procedure __SetControllerOptions(const Value: TControllerOptions);
       procedure __SetWindowOptions(const Value: TWindowOptions);
       procedure __BeforeOpenForm(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
       procedure __AfterOpenForm(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
       procedure __SetFormIconID(const Value: Integer);
     protected
       FData : Pointer;
       procedure BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
       procedure MSG_SetFormData(var Msg: TMessage); message MSG_SET_FORM_DATA;
       procedure WMForm_Initialized(var Message: TMessage); message MSG_FORM_INITIALIZED;
       procedure MSG_LanguageChange(var Msg: TMessage); message WM_LANGUAGE_CHANGE;
       procedure MSG_BeforeFormClose(var Msg: TMessage); message MSG_BEFORE_FORM_CLOSE;
       procedure OwnerForm_CloseQuery(Sender: TObject; var CanClose: Boolean);
       procedure __Set_MainForm;
     public
       FnWindowNumber : Integer;
       FsOriginalCaption : String;               // the original caption of the form
       constructor Create(aOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       function LoadData: Integer;
       function SaveData: Integer;
       function FreeData: Integer;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       procedure AddWindow(aParent: TComponent);
       function RemoveWindow: Boolean;
       procedure RemoveClients;
       procedure Close;
       function FindWindowByFormID(nFormID: Integer): Pointer;
       function GetMainWindow: TForm;
       function OpenForm(nFormID: Integer): TForm;
       function ShowWindowList(bShow: Boolean; aParent: HWND): Pointer;
       procedure Dispatch_Message_To_Parent(Msg: Pointer);
       procedure AddToNotifyList(aComponent: TComponent);
       procedure RemoveFromNotifyList(aComponent: TComponent);
       property ClientCount : Integer read __GetClientCount;
       property bDirty : Boolean read __GetDirty write __SetDirty;
       property bClosing : Boolean read __GetClosing write __SetClosing;
       property Data : Pointer read FData write FData;
       property CanClose : Boolean read __CanClose;
       property ParentController : TmafWindowController read FpParentController;
       property ModuleID : Integer read FnModuleID write FnModuleID;
     published
       property HookID : Integer read FnHookID stored False;
       property SubHookID : Integer read FnSubHookID stored False;
       property HookClient : TmafHookClient read FpHookClient write FpHookClient;
       property ResClient : TmafResourceClient read FpResClient write __SetResClient;
       property FormID : Integer read FnFormID write FnFormID;
       property FormIconID : Integer read FnFormIconID write __SetFormIconID default 0;
       property ControllerOptions : TControllerOptions read FControllerOptions write __SetControllerOptions default [coAutoRegister];
       property WindowOptions : TWindowOptions read FWindowOptions write __SetWindowOptions;
       property DefaultActions : TDefaultActions read FDefaultActions write FDefaultActions default [daSingleWindowClose];
       property InitOptions : TInitOptions read FInitOptions write FInitOptions;
       property LoadSaveControl : TLoadSaveControl read FLoadSaveControl write FLoadSaveControl;
       property DataID : Integer read __GetDataID write __SetDataID default 0;
       property CaptionID : Integer read FnCaptionID write __SetCaptionID;
       property OnGetDataPointer : TDataPointerEvent read FOnGetDataPointer write FOnGetDataPointer;
       property OnFreeDataPointer : TDataPointerEvent read FOnFreeDataPointer write FOnFreeDataPointer;
       property OnLoadData : TLoadSaveEvent read FOnLoadData write FOnLoadData;
       property OnSaveData : TLoadSaveEvent read FOnSaveData write FOnSaveData;
       property OnInitForm : TOnInitForm read FOnInitForm write FOnInitForm;
       property OnCloseForm : TNotifyEvent read FOnCloseForm write FOnCloseForm;
       property OnSetDatabase : TDatabaseEvent read FOnSetDatabase write FOnSetDatabase;
     end;

const WF_DIRTY               = 1;
      WF_CLOSING             = 2;

function GetController(aForm: TComponent): TmafWindowController;

implementation

uses uMAF_Tools;

function GetController(aForm: TComponent): TmafWindowController;
var i : Integer;
begin
  Result := nil;
  For i := 0 to aForm.ComponentCount - 1 Do
    If IsCLass(aForm.Components[i], TmafWindowController) Then begin
      Result := TmafWindowController(aForm.Components[i]);
      Break;
    end;
end;

{ TLoadSaveControl }

constructor TLoadSaveControl.Create;
begin
  FnFreeHookID := 0;
  FnLoadHookID := 0;
  FnSaveHookID := 0;
  FnSubHookID := 0;
  FLoadSaveOptions := [lsoSameHookID];
end;

procedure TLoadSaveControl.__SetFreeHookID(const Value: Integer);
begin
  FnFreeHookID := Value;
  If lsoSameHookID in FLoadSaveOptions Then begin
    FnLoadHookID := Value;
    FnSaveHookID := Value;
  end;  //  --  If lsoSameHookID in FLoadSaveOptions Then
end;

procedure TLoadSaveControl.__SetLoadHookID(const Value: Integer);
begin
  FnLoadHookID := Value;
  If lsoSameHookID in FLoadSaveOptions Then begin
    FnFreeHookID := Value;
    FnSaveHookID := Value;
  end;  //  --  If lsoSameHookID in FLoadSaveOptions Then
end;

procedure TLoadSaveControl.__SetSaveHookID(const Value: Integer);
begin
  FnSaveHookID := Value;
  If lsoSameHookID in FLoadSaveOptions Then begin
    FnLoadHookID := Value;
    FnFreeHookID := Value;
  end;  //  --  If lsoSameHookID in FLoadSaveOptions Then
end;

{ TmafWindowController }

constructor TmafWindowController.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  AManagerQueryID := HM_WINDOWMANAGER_QUERY;
  FInternalFlags := 0;
  FManagerType := MT_WINDOW_MANAGER;
  FControllerOptions := [coAutoRegister];
  FDefaultActions := [daSingleWindowClose];
  FpHookClient := nil;
  FnDataID := 0;
  FnWindowNumber := 0;
  FData := nil;
  FpParentController := nil;
  FLoadSaveControl := TLoadSaveControl.Create;
  FNotifyList := TList.Create;
  FsOriginalCaption := '';
end; // Create

destructor TmafWindowController.Destroy;
begin
  If ((FData <> nil) And (Assigned(FOnFreeDataPointer))) Then
    FOnFreeDataPointer(Self, FData);
  If coAutoRegister in FControllerOptions Then
    RemoveWindow;
  FreeAndNil(FLoadSaveControl);
  FreeAndNil(FNotifyList);
  inherited;
end; // Destroy

procedure TmafWindowController.Loaded;
begin
  inherited;
  If csDesigning in ComponentState Then
    Exit;

  If IsClass(Owner, TForm) Then
    FsOriginalCaption := TForm(Owner).Caption;
  // we're going to set a new OnCloseQuery event and save the original to call it ourselfs
  If IsClass(Owner, TForm) Then begin
    If Assigned(TForm(Owner).OnCloseQuery) Then
      FOnCloseQuery_Save := TForm(Owner).OnCloseQuery
    Else
      FOnCloseQuery_Save := nil;
    TForm(Owner).OnCloseQuery := OwnerForm_CloseQuery;
  end;

  ResClient := FpResClient;
  If coAutoRegister in FControllerOptions Then
    AddWindow(Owner);
  If coRegisterMainForm in FControllerOptions Then
    __Set_MainForm;
  If FnFormIconID > 0 Then
    FormIconID := FnFormIconID;
end;

procedure TmafWindowController.MSG_BeforeFormClose(var Msg: TMessage);
var i : Integer;
begin
  If Pointer(Msg.WParam) <> Self Then
    For i := 0 To FNotifyList.Count - 1 Do
      SendComponentMessage(TComponent(FNotifyList.Items[i]), MSG_BEFORE_FORM_CLOSE, nil, nil, False)
end;

procedure TmafWindowController.MSG_LanguageChange(var Msg: TMessage);
begin
  If Not IsClass(Owner, TForm) Then
    Exit;                              // we set only form captions

  If ((FnCaptionID <> 0) And (FpResClient <> nil)) Then
    TForm(Owner).Caption := FpResClient.GetString(FnCaptionID);
  If ((woWindowNumber in FWindowOptions) And Not (woSingleWindow in FWindowOptions)) Then
    TForm(Owner).Caption := TForm(Owner).Caption + ' ' + IntToStr(FnWindowNumber);
end; // MSG_LanguageChange
                                  
procedure TmafWindowController.MSG_SetFormData(var Msg: TMessage);
var Frm: TScrollingWinControl;
    AMsg : TMessage;
    {$IFDEF CPUX64}
    AResult : Integer;
    {$ELSE}
    AResult : Integer;
    {$ENDIF}
begin
  If FInternalFlags And 1=1 Then begin
    If woLoad in FWindowOptions Then // this we do in the 2nd step only
      LoadData;
    Exit;
  end;  //  --  If FInternalFlags And 1=1 Then
  FInternalFlags := FInternalFlags + 1;  // and this only in the 1st step

  If ioToggle in FInitOptions Then begin
    Msg.Result := 0; // FAM can create the window
    Repeat
      Frm := TScrollingWinControl(FindWindowByFormID(FnFormID));
      If Frm = Self.Owner Then
        Break;
      If ((Frm <> nil) And (Frm <> Owner)) Then begin
        SendMessage(Frm.Handle, WM_CLOSE, 0, 0);
        Msg.Result := 1; // FAM cannot create the window
      end else
        Frm := nil;
    Until Frm = nil;

  end;  //  --  If ioToggle in FInitOptions Then
  If ioSetParentHandle in FInitOptions Then
    TScrollingWinControl(Owner).ParentWindow := TScrollingWinControl(pQHS(Msg.LParam)^.pParent).Handle; // doesn't fire FormShow !
  If ioSetParent in FInitOptions Then
    TScrollingWinControl(Owner).Parent := TScrollingWinControl(pQHS(Msg.LParam)^.pParent);
  If lsoUseDataID in FLoadSaveControl.LoadSaveOptions Then
    FLoadSaveControl.DataID := pQHS(Msg.LParam)^.Reserved2;

  FnHookID := pQHS(Msg.LParam)^.HookID;
  FnSubHookID := pQHS(Msg.LParam)^.SubHookID;

  MSG_LanguageChange(AMsg);
end; // MSG_SetFormData

procedure TmafWindowController.WMForm_Initialized(var Message: TMessage);
var ErrCode : Integer;
begin
  If Assigned(FOnInitForm) Then begin
    ErrCode := Message.Result;
    FOnInitForm(Self, pQHS(Message.LParam), Pointer(Message.WParam), ErrCode);
    Message.Result := ErrCode;
  end;

  If ((pQHS(Message.LParam)^.pDB <> nil) And (pQHS(Message.LParam)^.pTransaction <> nil)) Then
    If Assigned(FOnSetDatabase) Then
      FOnSetDatabase(Self, TComponent(pQHS(Message.LParam)^.pDB), TComponent(pQHS(Message.LParam)^.pTransaction));

  If woLoad in FWindowOptions Then
    LoadData;
end;

procedure TmafWindowController.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  If Operation = opRemove Then begin
    If AComponent = FpHookClient Then
      FpHookClient := nil;
    If AComponent = FpResClient Then
      FpResClient := nil;
    RemoveFreeNotification(AComponent);
  end;
end;

function TmafWindowController.OpenForm(nFormID: Integer): TForm;
var QHS : pQHS;
begin
  Result := FindWindowByFormID(nFormID);
  If Result = nil Then begin
    QHS := __Create_QueryHandlerStruct;
    QHS^.Reserved1 := nFormID;
    __AdvQuery_Manager(WM__GET_FORM_ID_DATA, QHS, nil);
    If QHS^.Reserved2 > 0 Then         // if it's > 0 then a FormID was registered
      If Assigned(FpHookClient) Then begin
        FpHookClient.PushEvents;
        FpHookClient.BeforeCallRouter := __BeforeOpenForm;
        FpHookClient.AfterCallRouter := __AfterOpenForm;
        FpHookClient.ExecuteHook(QHS^.Reserved2, Integer(QHS^.PReserved1));
        Result := FpHookClient.PopVar.AsPointer;
        FpHookClient.PopEvents;
      end;
    __Free_QueryHandlerStruct(QHS);
  end;

end;

procedure TmafWindowController.OwnerForm_CloseQuery(Sender: TObject; var CanClose: Boolean);
begin
{  If bClosing Then begin
    CanClose := True;
    If Assigned(FOnCloseQuery_Save) Then
      FOnCloseQuery_Save(Sender, CanClose); // we call the original OnCloseQuery
    If Not CanClose Then                  // if the form says, we can't
      bClosing := False;                  // then we have to believe it
    Exit;
  end;

  If SendComponentMessage(TComponent(Owner), MSG_BEFORE_FORM_CLOSE, Self, nil, True) = 0 Then begin
    bClosing := True;
    CanClose := False;
    PostMessage(TForm(Owner).Handle, WM_CLOSE, 0, 0);
  end;}
  CanClose := True;
  If SendComponentMessage(TComponent(Owner), MSG_BEFORE_FORM_CLOSE, Self, nil, True) = 0 Then begin
    If Assigned(FOnCloseQuery_Save) Then
      FOnCloseQuery_Save(Sender, CanClose); // we call the original OnCloseQuery
  end;
end;

procedure TmafWindowController.Close;
begin
  If woSave in FWindowOptions Then
    SaveData;
  If Assigned(FOnCloseForm) Then
    FOnCloseForm(Self);
end; // Close

procedure TmafWindowController.AddToNotifyList(aComponent: TComponent);
begin
  If FNotifyList.IndexOf(aComponent) = -1 Then
    FNotifyList.Add(aComponent);
end;

procedure TmafWindowController.RemoveFromNotifyList(aComponent: TComponent);
begin
  If FNotifyList.IndexOf(aComponent) > -1 Then
    FNotifyList.Delete(FNotifyList.IndexOf(aComponent));
end;

procedure TmafWindowController.AddWindow(aParent: TComponent);
var aC : TComponent;
    FoundForm : TScrollingWinControl;
    QHS : pQHS;
begin
  FoundForm := nil;
  aC := aParent;
  Repeat              // Owner-Liste durchgehen, bis das nächst-höhere Fenster gefunden wurde
    If aC <> nil Then begin
      If IsClass(aC, TScrollingWinControl) Then
        FoundForm := TScrollingWinControl(aC)
      Else
        aC := aC.Owner;
    end;
  Until (aC = nil) Or (FoundForm <> nil);
  FpParentController := __FindParentController;
  QHS := __Create_QueryHandlerStruct;
  QHS^.pChildObj := FoundForm;
  __AdvQuery_Manager(WM__ADD_WINDOW, QHS, Self);
  __Free_QueryHandlerStruct(QHS);
end;

function TmafWindowController.RemoveWindow: Boolean;
var QHS : pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  Result := (__AdvQuery_Manager(WM__REMOVE_WINDOW, QHS, Self) = ERR_NO_ERROR);
  __Free_QueryHandlerStruct(QHS);
end;

procedure TmafWindowController.RemoveClients;
begin
  CallManager(WM__REMOVE_CLIENTS, Self, nil);
end;

function TmafWindowController.__FindParentController: TmafWindowController;
var pOwner : TComponent;
    i : Integer;
begin
  Result := nil;
  pOwner := Owner.Owner;  // the owner is our form and we would just find us
  If pOwner = nil Then
    Exit;
  Repeat
    For i := 0 To pOwner.ComponentCount - 1 Do
      If IsClass(pOwner.Components[i], TmafWindowController) Then begin
        Result := TmafWindowController(pOwner.Components[i]);
        Break;
      end;  //  --  If IsClass(pOwner.Components[i], TERPWindowController) Then
    pOwner := pOwner.Owner; // we take the owner form of the found controller
  Until ((Result <> nil) Or (pOwner = nil));
end;

function TmafWindowController.FindWindowByFormID(nFormID: Integer): Pointer;
var QHS : pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.ResVal := nFormID;
  __AdvQuery_Manager(WM__FIND_WINDOW_ID, QHS, nil);
  Result := QHS^.pChildObj;
  __Free_QueryHandlerStruct(QHS);
end; // FindWindowByFormID

procedure TmafWindowController.__AfterOpenForm(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin
  FpHookClient.PushVar.AsPointer := QHS^.pChildObj;
end;

procedure TmafWindowController.__BeforeOpenForm(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin
  QHS^.pParent := GetMainWindow;
end;

function TmafWindowController.__CanClose: Boolean;
begin
  Result := Not (((FStatusFlags and WF_DIRTY) = WF_DIRTY) and (woSave in FWindowOptions));
end;

function TmafWindowController.__GetClientCount: Integer;
begin
  Result := CallManager(WM__GET_CLIENT_COUNT, Self, nil);
end;

function TmafWindowController.ShowWindowList(bShow: Boolean; aParent: HWND): Pointer;
var p : Pointer;
begin
  If bShow Then p := Self
           Else p := nil;
  Result := Pointer(CallManager(WM__SHOW_LIST, Pointer(aParent), p));
end;

procedure TmafWindowController.__SetDataID(const Value: Integer);
var OldID: Integer;
begin
  OldID := FnDataID;
  FnDataID := Value;
  If coAutoLoadData in FControllerOptions Then
    If LoadData <> ERR_NO_ERROR Then
      FnDataID := OldID;
end;

function TmafWindowController.__GetDataID: Integer;
begin
  Result := FnDataID;
end;

function TmafWindowController.__GetClosing: Boolean;
begin
  Result := ((FStatusFlags And WF_CLOSING) = WF_CLOSING);
end;

procedure TmafWindowController.__SetCaptionID(const Value: Integer);
begin
  FnCaptionID := Value;
  If Not (csDesigning in ComponentState) Then
    If Assigned(FpResClient) Then
      If IsClass(Owner, TForm) Then
        TForm(Owner).Caption := FpResClient.GetString(FnCaptionID);
end;

procedure TmafWindowController.__SetClosing(const Value: Boolean);
begin
  If bClosing Then
    Exit;

  If Value Then
    FStatusFlags := FStatusFlags + WF_CLOSING  // set local closing flag
  Else
    FStatusFlags := FStatusFlags - WF_CLOSING; // remove local closing flag
end;

procedure TmafWindowController.__SetControllerOptions(const Value: TControllerOptions);
//var FBaseOptions : TClientOptions;
begin
  FControllerOptions := Value;
{  FBaseOptions := ClientOptions;
  If coRegisterMainForm in FControllerOptions Then
    Include(FBaseOptions, coRegisterSelf)
  Else
    Exclude(FBaseOptions, coRegisterSelf);
  ClientOptions := FBaseOptions; }
end;

function TmafWindowController.__GetDirty: Boolean;
begin
  Result := ((FStatusFlags And WF_DIRTY) = WF_DIRTY);
end;

procedure TmafWindowController.__SetDirty(const Value: Boolean);
var p : Pointer;
begin
  If bDirty = Value Then        // if it isn't new to us...
    Exit;                       // ... nothing more to do for us!

  If Value Then p := Owner      // if dirty, we send our owner
           Else p := nil;       // otherwise we send nil

  CallManager(WM__DIRTY, Pointer(Self), p);  // set flag in WindowManager
  If Value Then
    FStatusFlags := FStatusFlags + WF_DIRTY  // set local dirty flag
  Else
    FStatusFlags := FStatusFlags - WF_DIRTY; // remove local dirty flag
end; // __SetDirty

procedure TmafWindowController.__SetFormIconID(const Value: Integer);
var pIcon : TGraphic;
begin
  FnFormIconID := Value;
  If (csLoading in ComponentState) Then
    Exit;                                   // we will handle it on loaded
  If ((Assigned(FpResClient)) And (FnFormIconID > 0) And IsClass(Self.Owner, TForm)) Then begin
    pIcon := FpResClient.GetGraphic(FnFormIconID);
    If Assigned(pIcon) Then begin
      TForm(Self.Owner).Icon.Assign(pIcon);
      pIcon.Free;
    end;  //  --  If Assigned(pIcon) Then
  end;  //  --  If ((Assigned(FpResClient)) And (FnFormIconID > 0) And IsClass(Self.Owner, TForm)) Then 
end;

procedure TmafWindowController.__SetResClient(const Value: TmafResourceClient);
begin
  If ((FpResClient = Value) Or (csLoading in ComponentState)) Then begin
    FpResClient := Value;
    Exit;
  end;

  If FpResClient <> nil Then
    FpResClient.DeleteNotifyComponent(Self);

  FpResClient := Value;
  If FpResClient <> nil Then
    FpResClient.AddNotifyComponent(Self);
end;

procedure TmafWindowController.__SetWindowOptions(const Value: TWindowOptions);
//var QHS : pQHS;
//    bRequestWindowNo : Boolean;
begin
//  bRequestWindowNo := (woWindowNumber in FWindowOptions);
  FWindowOptions := Value;

  If csDesigning in ComponentState Then
    Exit;

  If woSingleWindow in FWindowOptions Then
    Exclude(FWindowOptions, woWindowNumber);

{  If ((bRequestWindowNo) And (woWindowNumber in FWindowOptions)) Then begin
    QHS := __Create_QueryHandlerStruct;
    QHS^.Reserved1 := FnHookID;
    QHS^.Reserved2 := FnSubHookID;
    __AdvQuery_Manager(WM__GET_WINDOW_NO, QHS, nil);
    FnWindowNumber := QHS^.ResVal;
    If ((FpResClient <> nil) And (CaptionID > 0)) Then
      TForm(Owner).Caption := FpResClient.GetString(FnCaptionID) + ' ' + IntToStr(FnWindowNumber)
    Else
      TForm(Owner).Caption := FsOriginalCaption + ' ' + IntToStr(FnWindowNumber);
    __Free_QueryHandlerStruct(QHS);
  end;                             }
end;

procedure TmafWindowController.__Set_MainForm;
var QHS : pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  __AdvQuery_Manager(WM__SET_MAINFORM, QHS, Self.Owner);
  __Free_QueryHandlerStruct(QHS);
end;

// ********************************* Comments **********************************
// Description : Sendet eine Nachricht zum Parent des Formulars
// Param (in)  : Msg=Pointer auf die Nachricht
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 07.11.2004
// Last Update : 07.11.2004
// *****************************************************************************
procedure TmafWindowController.Dispatch_Message_To_Parent(Msg: Pointer);
begin
  CallManager(WM__MSG_TO_PARENT, Self, Msg);
end; // Dispatch_Message_To_Parent

procedure TmafWindowController.BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin
  QHS^.CommandID := FpHookClient.PopVar.AsInteger;  // UserID
  QHS^.pChildObj := FpHookClient.PopVar.AsPointer;  // data pointer
  If lsoSameHookID in FLoadSaveControl.LoadSaveOptions Then
    QHS^.Reserved1 := FpHookClient.PopVar.AsInteger;
end;

function TmafWindowController.LoadData: Integer;
var pData: Pointer;
begin
  Result := ERR_COMPONENT_SETUP_FAILURE; // we assume, the component wasn't parameterised
  If FpHookClient <> nil Then begin // are we set ?
    If FData = nil Then
      If Assigned(FOnGetDataPointer) Then begin
        FOnGetDataPointer(Self, pData);
        FData := pData;
      end;  //  --  If Assigned(FOnGetDataPointer) Then
    Result := ERR_PARAM_FAILURE;
    // now we should have a data pointer, otherwise the user is doing smth wrong ;)
    If FData <> nil Then begin
      FpHookClient.PushEvents; // save the forms event handler
      FpHookClient.BeforeCallRouter := BCR;
      If lsoSameHookID in FLoadSaveControl.LoadSaveOptions Then
        FpHookClient.PushVar.AsInteger := 1;

      FpHookClient.PushVar.AsPointer := FData;
      FpHookClient.PushVar.AsInteger := FnDataID;
      Result := FpHookClient.ExecuteHook(FLoadSaveControl.LoadHookID, FLoadSaveControl.SubHookID);
      FpHookClient.PopEvents;  // restore the forms event handler
      If Assigned(FOnLoadData) Then
        FOnLoadData(Self, FData, Result);
      bDirty := False;
    end;  //  --  If FData <> nil Then
  end;  //  --  If ((FpHookClient <> nil) And (FnDataID > 0)) Then
end;

function TmafWindowController.SaveData: Integer;
begin
  Result := ERR_COMPONENT_SETUP_FAILURE; // we assume, the component wasn't parameterised
  If FpHookClient <> nil Then begin // are we set ?
    Result := ERR_PARAM_FAILURE;
    // now we should have a data pointer, otherwise the user is doing smth wrong ;)
    If FData <> nil Then begin
      FpHookClient.PushEvents; // save the forms event handler
      FpHookClient.BeforeCallRouter := BCR;
      If lsoSameHookID in FLoadSaveControl.LoadSaveOptions Then
        FpHookClient.PushVar.AsInteger := 2;
      FpHookClient.PushVar.AsPointer := FData;
      FpHookClient.PushVar.AsInteger := FnDataID;
      If Assigned(FOnSaveData) Then
        FOnSaveData(Self, FData, Result);
      Result := FpHookClient.ExecuteHook(FLoadSaveControl.SaveHookID, FLoadSaveControl.SubHookID);
      FpHookClient.PopEvents;  // restore the forms event handler
    end;  //  --  If FData <> nil Then
    bDirty := False;  // remove the dirty flag
  end;  //  --  If ((FpHookClient <> nil) And (FnDataID > 0)) Then
end;

function TmafWindowController.FreeData: Integer;
begin
  Result := ERR_COMPONENT_SETUP_FAILURE; // we assume, the component wasn't parameterised
  If FpHookClient <> nil Then begin // are we set ?
    Result := ERR_PARAM_FAILURE;
    // now we should have a data pointer, otherwise the user is doing smth wrong ;)
    If FData <> nil Then begin
      FpHookClient.PushEvents; // save the forms event handler
      FpHookClient.BeforeCallRouter := BCR;
      If lsoSameHookID in FLoadSaveControl.LoadSaveOptions Then
        FpHookClient.PushVar.AsInteger := 3;
      FpHookClient.PushVar.AsPointer := FData;
      FpHookClient.PushVar.AsInteger := FnDataID;
      Result := FpHookClient.ExecuteHook(FLoadSaveControl.FreeHookID, FLoadSaveControl.SubHookID);
      FpHookClient.PopEvents;  // restore the forms event handler
    end;  //  --  If FData <> nil Then
    bDirty := False;  // remove the dirty flag
  end;  //  --  If ((FpHookClient <> nil) And (FnDataID > 0)) Then
end;

function TmafWindowController.GetMainWindow: TForm;
var QHS: pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  __AdvQuery_Manager(WM__GET_MAINFORM, QHS, nil);
  Result := TForm(QHS^.pChildObj);
  __Free_QueryHandlerStruct(QHS);
end;

end.
