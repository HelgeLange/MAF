{*******************************************************************************
Name         : uMAF_GlobalVars.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2004-2010 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 24.08.2004
Last Update  : 04.02.2011
Version      : 1.0.012
Last Changes :


1.0.012 (04.02.2011) -----------------------------------------------------------
- [ADD] added new Event BeforeConnect, which will allow to set the correct path
        for the used controller database (like ini file)
1.0.011 (07.09.2010) -----------------------------------------------------------
- [FIX] fixed a bug, where the callback for an object variable never was called
        when the variable existed
1.0.010 (30.07.2010) -----------------------------------------------------------
- [CHG] moved the classes TCustomVarController and TINIVarController under the
        name TmafCustomVarController to a new unit named uMAF_VarController.pas
1.0.009 (01.10.2009) -----------------------------------------------------------
- [CHG] changed several parts in order to use the new TVarFlags
- [ADD] CloseManager added wich is automatically called by the BaseManager comp
        and now saves and clears variables there
1.0.008 (15.09.2009) -----------------------------------------------------------
- [DEL] removed the possibility to save into the dfm file itself for now as
        I want to change it to the Streamhelper eventually, if it is needed
- [CHG] the component doesn't write variables in design mode anymore
1.0.007 (12.09.2009) -----------------------------------------------------------
- [FIX] fixed a small bug in GetString, where the untyped pointer was directly
        casted to String instead to a PChar first
- [FIX] fixed a bug in TERPGlobalVars.Loaded where also in design mode the
        component tried to connect to the INI file and load the stored variables 
1.0.006 (30.08.2009) -----------------------------------------------------------
- [ADD] Callback functionality for every global variable (except for objects of
        course). Through the HookClient one can now register a notify to get
        informed, when the variable changes or gets deleted
- [ADD] added property DefaultController where a default VarController can be set
1.0.005 (24.07.2009) -----------------------------------------------------------
- [CHG] changed TERPGlobalVars to inherit from TERPCustomManagerComponent
        therefore the obsolet HookAccessContainer stuff was deleted and we now
        use the ModuleController defined in the new CustomClass
1.0.004 (22.10.2008) -----------------------------------------------------------
- [ADD] added VarController, wich can read from and write to INI files, Registry
        or database
- [ADD] support for global boolean variables
1.0.003 (02.03.2007) -----------------------------------------------------------
- [CHANGE] moved FreeVarRec to interface unit uGlobalVars_Shared.pas
- [CHANGE] Code cleanup
- [FIX] fixed the bug, that in DeleteVar always the record of the first variable
        in the list was freed
- [FIX] fixed a bug, where in FreeVarRec 1 byte less was released from
        PVarRec^.Name and 1 byte less from PVarRec^.Var1, if it was a String
- [ADD] interface TVarClass.Query now also accepts Strings as global variables
1.0.002 (02.09.2004) -----------------------------------------------------------
- [ADD] added interface TVarClass.Query
1.0.001 (30.08.2004)------------------------------------------------------------
- [ADD] TVarClass.DeleteVar
- [ADD] OnChange-Event for realtime update
1.0.000 (24.08.2004) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_GlobalVars;

interface

uses Windows, Classes, SysUtils, IniFiles,
     // Modular Application Framework Components units
     uMAF_Tools, uMAF_Globals, uMAF_Core, uMAF_DataStorage, uMAF_VarController;

{ TODO : - build fast access list for saved variables }

Type TDefaultController = (dcIniVarController);

     TBeforeControllerConnect = procedure(Sender: TObject; ControllerType: TLoadSave) Of Object;

     TmafGlobalVars = class(TmafCustomManagerComponent)
     private
       FVarList : TList;
       FOnChange : TNotifyEvent;
       FpIniVarController : TmafINIVarController;
       FDefaultController : TDefaultController;
       FBeforeConnect : TBeforeControllerConnect;
       procedure __TriggerOnChange(pData: PVarRec);
       function __GetCount: Integer;
       function __GetDefaultLoadSave: TLoadSave;
       procedure __SetVarDefaults(pData: PVarRec; ALoadSave: TLoadSave);
       procedure __SaveVariable(pData: PVarRec);

       procedure __OnEnumString(Sender: TObject; VarName, Value: String; ALoadSave: TLoadSave);
       procedure __OnEnumInteger(Sender: TObject; VarName: String; Value: Integer; ALoadSave: TLoadSave);
       procedure __OnEnumBoolean(Sender: TObject; VarName: String; Value: Boolean; ALoadSave: TLoadSave);
       procedure __OnBeforeConnect(Sender: TObject);
     protected
       procedure __RegisterAPI; override;
       procedure __OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer); override;
       procedure CloseManager; override;

       function GetChildOwner: TComponent; override;
       procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       procedure Clear;
       procedure Save;
       function Query(nAction: Integer; pData: Pointer): Integer;
       function GetVar(VarName: String): PVarRec;
       procedure AddString(VarName, Value : String; VarFlags: TVarFlags; ALoadSave: TLoadSave);
       procedure AddInteger(VarName: String; Value: Integer; VarFlags: TVarFlags; ALoadSave: TLoadSave);
       procedure AddBoolean(VarName : String; Value : Boolean; VarFlags: TVarFlags; ALoadSave: TLoadSave);
       procedure AddObject(VarName: String; Value: Pointer);
       function GetInteger(VarName: String; var bFound: Boolean): Integer;
       function GetString(VarName: String; var bFound: Boolean): String;
       function GetObject(VarName: String; var bFound: Boolean): Pointer;
       function GetBoolean(VarName: String; var bFound: Boolean): Boolean;
       function DeleteVar(VarName: String): Integer;
       function RegisterCallBack(VarName: String; AHookClient: TObject): Integer;
       function UnRegisterCallBack(VarName: String; AHookClient: TObject): Integer;

       property VarList : TList read FVarList;
     published  
       property Count : Integer read __GetCount;
       property IniVarController : TmafINIVarController read FpIniVarController write FpIniVarController;
       property DefaultController : TDefaultController read FDefaultController write FDefaultController default dcIniVarController;
       property OnChange : TNotifyEvent read FOnChange write FOnChange;
       property BeforeConnect : TBeforeControllerConnect read FBeforeConnect write FBeforeConnect;
     end; // TmafGlobalVars

implementation

uses Forms, Messages;

const StreamVer : Integer = 1000;
      MAX_EVENTS = 2;

var ManagerEvents : array[1..MAX_EVENTS] of Integer = (
      HM_GLOBAL_VAR_QUERY,
      HM_GET_GLOBALVAR_MANAGER);

{ TmafGlobalVars }

constructor TmafGlobalVars.Create;
begin
  inherited;
  FDefaultController := dcIniVarController;
  FVarList := TList.Create;
  ManagerType := MT_GLOBAL_VAR_MANAGER;
  FpIniVarController := TmafINIVarController.Create;
  FpIniVarController.OnEnumString := __OnEnumString;
  FpIniVarController.OnEnumInteger := __OnEnumInteger;
  FpIniVarController.OnEnumBoolean := __OnEnumBoolean;
  FpIniVarController.BeforeConnect := __OnBeforeConnect;
end; // Create

destructor TmafGlobalVars.Destroy;
begin
  Save;
  Clear;
  FreeAndNil(FpIniVarController);
  FreeAndNil(FVarList);
  inherited;
end; // Destroy

procedure TmafGlobalVars.Loaded;
begin
  inherited;
  If csDesigning In ComponentState Then
    Exit;
    
  If vcfReadOnLoaded in FpIniVarController.VarControllerOptions Then begin
    FpIniVarController.Connect;
    FpIniVarController.EnumVars;
    FpIniVarController.Disconnect;
  end;  //  --  If vcfReadOnLoaded in FpIniVarController.VarControllerOptions Then
end; // Loaded

procedure TmafGlobalVars.CloseManager;
begin
  inherited;
  FpIniVarController.bClosing := True;
  Save;
  Clear;
end;

// ********************************* Comments **********************************
// Description : deletes all variables
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lehn
// Date        : 24.08.2004
// Last Update : 24.08.2004
// *****************************************************************************
procedure TmafGlobalVars.Clear;
var pData : PVarRec;
begin
  While FVarList.Count > 0 Do begin
    pData := PVarRec(FVarList[0]);
    FreeVarRec(pData);
    FVarList.Delete(0);
  end;  //  --  While FVarList.Count > 0 Do
  If not (csDestroying in ComponentState) Then
    __TriggerOnChange(nil);
end; // Clear

procedure TmafGlobalVars.Save;
var i : Integer;
    pData : PVarRec;
begin
  If csDesigning in ComponentState Then
    Exit;
  For i := FVarList.Count - 1 DownTo 0 Do begin
    pData := PVarRec(FVarList[i]);
    If vfSave in pData^.dwFlags Then
      Case pData^.LoadSave Of
        lsINIFile : FpIniVarController.WriteVar(pData);
      end;
  end;
end;

procedure TmafGlobalVars.__SaveVariable(pData: PVarRec);
var AController : TmafCustomVarController;
begin
  If ((csLoading in ComponentState) Or (csDesigning in ComponentState)) Then
    Exit;
    
  If pData = nil Then
    Exit;

  If Not (vfSave in pData^.dwFlags) Then
    Exit;

  Case pData^.LoadSave Of
    lsINIFile : AController := FpIniVarController;
    Else AController := nil;
  end;

  If Assigned(AController) Then
    AController.WriteVar(pData);
end;

// ********************************* Comments **********************************
// Description : liefert den Var-Record zu einem Namen, wenn vorhanden
// Param (in)  : Name=Name der variable
// Param (out) : Der Record zu dem Namen; nil, wenn nicht vorhanden
// Coding by   : Helge Lehn
// Date        : 24.08.2004
// Last Update : 24.08.2004
// *****************************************************************************
function TmafGlobalVars.GetVar(VarName: String): PVarRec;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FVarList.Count - 1 Do
    If String(PVarRec(FVarList[i])^.Name) = VarName Then begin
      Result := PVarRec(FVarList[i]);
      Break;
    end;  //  --  If String(PVarRec(FVarList[i])^.Name) = VarName Then
end; // GetVar

// ********************************* Comments **********************************
// Description : fügt eine Integer-Variable in die Liste ein
// Param (in)  : Name=Name der var; Inhalt=Zahl, die in der var gespeichert werden soll
// Param (out) : ErrCode
// Coding by   : Helge Lehn
// Date        : 24.08.2004
// Last Update : 01.09.2009
// *****************************************************************************
procedure TmafGlobalVars.AddInteger(VarName: String; Value: Integer; VarFlags: TVarFlags; ALoadSave: TLoadSave);
var pData : PVarRec;
begin
  pData := GetVar(VarName);

  If Assigned(pData) Then begin        // do we know this variable
    If Not (vfReadOnly in pData^.dwFlags) Then
      pData^.Var1 := Pointer(Value);     // save new content in variable
  end else begin
    pData := CreateVarRec(VarName, VarFlags, ALoadSave);
    pData^.Typ := vtInteger;
    pData^.Var1 := Pointer(Value);
    FVarList.Add(pData);
  end;  //  --  If Assigned(pData) Then
  __SetVarDefaults(pData, ALoadSave);
  __SaveVariable(pData);
  __TriggerOnChange(pData);
end; // AddInteger

procedure TmafGlobalVars.AddBoolean(VarName: String; Value : Boolean; VarFlags: TVarFlags; ALoadSave: TLoadSave);
var pData : PVarRec;
begin
  pData := GetVar(VarName);

  If Assigned(pData) Then begin        // do we know this variable
    If Not (vfReadOnly in VarFlags) Then
      pData^.Var1 := Pointer(Value);     // save new content in variable
  end else begin
    pData := CreateVarRec(VarName, VarFlags, ALoadSave);
    pData^.Typ := vtBoolean;
    pData^.Var1 := Pointer(Value);
    FVarList.Add(pData);
  end;  //  --  If Assigned(pData) Then
  __SetVarDefaults(pData, ALoadSave);
  __SaveVariable(pData);
  __TriggerOnChange(pData);
end;

// ********************************* Comments **********************************
// Description : fügt eine String-Var in die Variablen-Liste ein
// Param (in)  : Name=Name der var; Inhalt=String, der in der Variable gespeichert
//               werden soll
// Param (out) : N/A
// Coding by   : Helge Lehn
// Date        : 24.08.2004
// Last Update : 01.09.2009
// *****************************************************************************
procedure TmafGlobalVars.AddString(VarName, Value: String; VarFlags: TVarFlags; ALoadSave: TLoadSave);
var pData : PVarRec;
begin
  pData := GetVar(VarName);

  If Assigned(pData) Then begin                 // we know this variable
    If Not (vfReadOnly in VarFlags) Then begin
      FreePChar(PChar(pData^.Var1));              // free memory of old variable
      StrToPChar(Value, PChar(pData^.Var1));
    end;
  end else begin  //  --  If Assigned(pData) Then
    pData := CreateVarRec(VarName, VarFlags, ALoadSave);
    pData^.Typ := vtString;
    StrToPChar(Value, PChar(pData^.Var1));
    FVarList.Add(pData);
  end;
  __SetVarDefaults(pData, ALoadSave);
  __SaveVariable(pData);
  __TriggerOnChange(pData);
end; // AddString

procedure TmafGlobalVars.AddObject(VarName: String; Value: Pointer);
var pData : PVarRec;
begin
  pData := GetVar(VarName);

  If Assigned(pData) Then begin    // we know this variable
    If Not (vfReadOnly in pData^.dwFlags) Then
      pData^.Var1 := Value;        // we don't know, who set it, so we can't call back to free previous one
  end else begin
    pData := CreateVarRec(VarName, [], lsNone);
    pData^.Typ := vtPointer;
    pData^.Var1 := Value;
    FVarList.Add(pData);
  end;
  __TriggerOnChange(pData);
end;

procedure TmafGlobalVars.__SetVarDefaults(pData: PVarRec; ALoadSave: TLoadSave);
begin
  If pData = nil Then
    Exit;

  If ALoadSave <> lsDefault Then
    pData^.LoadSave := ALoadSave      // if save, where...
  Else
    pData^.LoadSave := __GetDefaultLoadSave;
end;

function TmafGlobalVars.GetBoolean(VarName: String; var bFound: Boolean): Boolean;
var pData : PVarRec;
begin
  Result := FpIniVarController.DefaultBoolean;
  bFound := False;
  pData := GetVar(VarName);
  If Assigned(pData) Then
    If pData^.Typ = vtBoolean Then begin
      Result := (pData^.Var1 <> nil);
      bFound := True;
    end;  //  --  If pData^.Typ = vtBoolean Then
end;

function TmafGlobalVars.GetChildOwner: TComponent;
begin
  Result := Self;
end;

procedure TmafGlobalVars.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited;
  //Proc(FpIniVarController);
end;

function TmafGlobalVars.GetInteger(VarName: String; var bFound: Boolean): Integer;
var pData : PVarRec;
begin
  Result := FpIniVarController.DefaultInteger;
  bFound := False;
  pData := GetVar(VarName);
  If Assigned(pData) Then
    If pData^.Typ = vtInteger Then begin
      Result := Integer(pData^.Var1);
      bFound := True;
    end;  //  --  If pData^.Typ = vtInteger Then
end; // GetInteger

function TmafGlobalVars.GetString(VarName: String; var bFound: Boolean): String;
var pData : PVarRec;
begin
  Result := '';
  bFound := False;
  pData := GetVar(VarName);
  If Assigned(pData) Then
    If pData^.Typ = vtString Then begin
      Result := String(PChar(pData^.Var1));
      bFound := True;
    end;  //  --  If pData^.Typ = vtString Then
end; // GetString

function TmafGlobalVars.GetObject(VarName: String; var bFound: Boolean): Pointer;
var pData : PVarRec;
begin
  Result := nil;
  bFound := False;
  pData := GetVar(VarName);
  If Assigned(pData) Then
    If pData^.Typ = vtPointer Then begin
      Result := pData^.Var1;
      bFound := True;
    end;  //  --  If pData^.Typ = vtPointer Then
end; // GetObject

// ********************************* Comments **********************************
// Description : liefert die Anzahl der augenblicklichen Variablen
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lehn
// Date        : 26.08.2004
// Last Update : 26.08.2004
// *****************************************************************************
function TmafGlobalVars.__GetCount: Integer;
begin
  Result := FVarList.Count;
end; // __GetCount

function TmafGlobalVars.__GetDefaultLoadSave: TLoadSave;
begin
  Case FDefaultController Of
    dcIniVarController : Result := lsINIFile;
    else Result := lsComponent;
  end;  //  --  Case FDefaultController Of
end;

procedure TmafGlobalVars.__OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  Case SubHookID Of
    HM_GLOBAL_VAR_QUERY : ErrCode := Query(Integer(QHS), UserParam);
    HM_GET_GLOBALVAR_MANAGER : QHS^.pChildObj := Self;
    Else inherited __OnEvent(SubHookID, QHS, UserParam, ErrCode);
  end;  //  --  Case SubHookID Of
end;

procedure TmafGlobalVars.__OnBeforeConnect(Sender: TObject);
begin
  If IsClass(Sender, TmafINIVarController) Then
    If Assigned(FBeforeConnect) Then
      FBeforeConnect(Sender, lsINIFile);
end;

procedure TmafGlobalVars.__OnEnumBoolean(Sender: TObject; VarName: String; Value: Boolean; ALoadSave: TLoadSave);
begin
  AddBoolean(VarName, Value, [vfSave], ALoadSave);
end;

procedure TmafGlobalVars.__OnEnumInteger(Sender: TObject; VarName: String; Value: Integer; ALoadSave: TLoadSave);
begin
  AddInteger(VarName, Value, [vfSave], ALoadSave);
end;

procedure TmafGlobalVars.__OnEnumString(Sender: TObject; VarName, Value: String; ALoadSave: TLoadSave);
begin
  AddString(VarName, Value, [vfSave], ALoadSave);
end;

// ********************************* Comments **********************************
// Description : löst in der Edit-Variante das OnChange aus
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lehn
// Date        : 26.08.2004
// Last Update : 30.08.2009
// *****************************************************************************
procedure TmafGlobalVars.__TriggerOnChange(pData: PVarRec);
var i : Integer;
    Msg : TMessage;
begin
  If Assigned(FOnChange) Then
    FOnChange(Self);
  If Assigned(pData) Then
    If Assigned(pData^.CallBackList) Then begin
      Msg.MSG := MSG_GLOBALVAR_CHANGE;
      Msg.WParam := Integer(pData^.Name);
      Msg.LParam := 0;
      For i := 0 To pData^.CallBackList.Count - 1 Do
        TComponent(pData^.CallBackList.Items[i]).Dispatch(Msg);
    end;
end; // __TriggerOnChange

procedure TmafGlobalVars.__RegisterAPI;
var i : Integer;
begin
  inherited;
  If Assigned(ModuleController) Then
    For i := 1 To MAX_EVENTS Do
      ModuleController.RegisterSubHookEvent(ManagerEvents[i], __OnEvent);
end; // __RegisterAPI

// ********************************* Comments **********************************
// Description : löscht eine Variable aus der Liste
// Param (in)  : Name=Name der Variable
// Param (out) : N/A
// Coding by   : Helge Lehn
// Date        : 30.08.2004
// Last Update : 30.08.2009
// *****************************************************************************
function TmafGlobalVars.DeleteVar(VarName: String): Integer;
var pData : PVarRec;
    Msg : TMessage;
begin
  Result := ERR_GLOBAL_VAR_NOT_EXIST;
  pData := GetVar(VarName);
  If pData <> nil Then begin
    If pData^.CallBackList <> nil Then begin
      Msg.Msg := MSG_GLOBALVAR_CHANGE;
      Msg.WParam := Integer(Pointer(pData^.Name));
      Msg.LParam := 1; // deleted
      While pData^.CallBackList.Count > 0 Do begin
        TComponent(pData^.CallBackList.Items[0]).Dispatch(Msg);
        pData^.CallBackList.Delete(0);
      end;  //  --  While pData^.CallBackList.Count > 0 Do
    end;  //  --  If pData^.CallBackList <> nil Then
    FVarList.Delete(FVarList.IndexOf(pData));
    FreeVarRec(pData);
    Result := ERR_NO_ERROR;
  end;  //  --  If pData <> nil Then 
end; // DeleteVar

// ********************************* Comments **********************************
// Description : SFC-Anbindung
// Param (in)  : pData=PVarRec
// Param (out) : ErrCode
// Coding by   : Helge Lehn
// Date        : 02.09.2004
// Last Update : 02.03.2007
// *****************************************************************************
function TmafGlobalVars.Query(nAction: Integer; pData: Pointer): Integer;
var bFound : Boolean;
begin
  Result := ERR_NO_ERROR;
  bFound := False;
  Case nAction Of
    GV_SET_INTEGER : AddInteger(PVarRec(pData)^.Name, Integer(PVarRec(pData)^.Var1), PVarRec(pData)^.dwFlags, PVarRec(pData)^.LoadSave);
    GV_GET_INTEGER : begin
                       PVarRec(pData)^.Var1 := Pointer(GetInteger(PVarRec(pData)^.Name, bFound));
                       If Not bFound Then
                         Result := ERR_GLOBAL_VAR_NOT_EXIST;
                     end; // GV_GET_INTEGER
    GV_SET_BOOLEAN : AddBoolean(PVarRec(pData)^.Name, (PVarRec(pData)^.Var1 <> nil), PVarRec(pData)^.dwFlags, PVarRec(pData)^.LoadSave);
    GV_GET_BOOLEAN : begin
                       If GetBoolean(PVarRec(pData)^.Name, bFound) Then
                         PVarRec(pData)^.Var1 := Pointer(1)
                       Else
                         PVarRec(pData)^.Var1 := nil;
                       If Not bFound Then
                         Result := ERR_GLOBAL_VAR_NOT_EXIST;
                     end; // GV_GET_INTEGER
    GV_DELETE_VAR  : Result := DeleteVar(PVarRec(pData)^.Name);
    GV_SET_STRING  : AddString(PVarRec(pData)^.Name, PChar(PVarRec(pData)^.Var1), PVarRec(pData)^.dwFlags, PVarRec(pData)^.LoadSave);
    GV_GET_STRING  : begin // since we don't want to allocate memory here we save just the life pointer
                       PVarRec(pData)^.Var1 := GetVar(String(PVarRec(pData)^.Name));
                       If PVarRec(pData)^.Var1 = nil Then
                         Result := ERR_GLOBAL_VAR_NOT_EXIST;
                     end; // GV_GET_STRING
    GV_SET_OBJECT  : AddObject(PVarRec(pData)^.Name, PVarRec(pData)^.Var1);
    GV_GET_OBJECT  : begin // since we don't want to allocate memory here we save just the life pointer
                       PVarRec(pData)^.Var1 := GetVar(String(PVarRec(pData)^.Name))^.Var1;
                       If PVarRec(pData)^.Var1 = nil Then
                         Result := ERR_GLOBAL_VAR_NOT_EXIST;
                     end; // GV_GET_OBJECT
    GV_REGISTER_CALLBACK   : Result := RegisterCallBack(PCallBackRegister(pData)^.VarName, PCallBackRegister(pData)^.AHookClient);
    GV_UNREGISTER_CALLBACK : Result := UnRegisterCallBack(PCallBackRegister(pData)^.VarName, PCallBackRegister(pData)^.AHookClient);
  end; // Case nAction Of
end; // Query

function TmafGlobalVars.RegisterCallBack(VarName: String; AHookClient: TObject): Integer;
var pData : PVarRec;
begin
  pData := GetVar(VarName);
  If ((Assigned(pData)) And (Assigned(AHookClient))) Then begin
    If pData^.CallBackList = nil Then
      pData^.CallBackList := TList.Create;

    pData^.CallBackList.Add(AHookClient);
    Result := ERR_NO_ERROR;
  end else
    Result := ERR_GLOBAL_VAR_NOT_EXIST;
end;

function TmafGlobalVars.UnRegisterCallBack(VarName: String; AHookClient: TObject): Integer;
var pData : PVarRec;
    idx : Integer;
begin
  pData := GetVar(VarName);
  If ((Assigned(pData)) And (Assigned(AHookClient))) Then begin
    Result := ERR_NOTHING_TO_DO;
    If pData^.CallBackList = nil Then
      Exit;
    idx := pData^.CallBackList.IndexOf(AHookClient);
    If idx > -1 Then
      pData^.CallBackList.Delete(idx);
    If pData^.CallBackList.Count = 0 Then
      FreeAndNil(pData^.CallBackList);  
    Result := ERR_NO_ERROR;
  end else
    Result := ERR_GLOBAL_VAR_NOT_EXIST;
end;

end.
