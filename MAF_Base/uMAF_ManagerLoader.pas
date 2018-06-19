{*******************************************************************************
Name         : uMAF_ManagerLoader.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007-2014 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 28.02.2007
Last Update  : 23.10.2014
Version      : 1.0.009
Purpose      : Loads/Unloads our Managers, holds their entry points and provides
               their data to their client components

               Define conditional "RELEASE" to remove all code wich prevents
               the component in design time from doing runtime stuff
Last Changes :

1.0.009 (23.10.2014) -----------------------------------------------------------
- [FIX] When a manager was loaded, LoadLibrary always was called, but when
        unloading the manager, FreeLibrary was only called for the last manager
        in that library. That caused that the windows reference counter did
        not allow to unload the library and it was still in use while the program
        was running. Now a LoadLibrary call is made only once per library
1.0.008 (11.10.2014) -----------------------------------------------------------
- [FIX] fixed a bug where after disconnecting from the manager and connecting
        again the MAF components on the loader form don't receive a WX_INIT_DONE
        message
1.0.007 (10.11.2011) -----------------------------------------------------------
- [ADD] in the constructor the TmafManagerLoader now writes a copy of Self into
        the global variable gManagerLoader (defined in uMAF_Core.pas). It is
        used to find the ManagerLoader faster and if it will prove itself
        reliable it will replace the current mechanism how client components
        search the connection to their manager through the ManagerLoader
1.0.006 (31.05.2010) -----------------------------------------------------------
- [ADD] new ManagerLoaderOption mloAttachOnIdle, which captures the
        Application.OnIdle event and channels it through to the TmafHookManager
        If the host application wants to use it themself, please make sure to
        call TmafManagerLoader.ProcessApplication_OnIdle from there
1.0.005 (07.11.2009) -----------------------------------------------------------
- [ADD] add event BeforeOpenINI that the ini file name be changed for different
        versions or command line parameters
1.0.004 (28.09.2009) -----------------------------------------------------------
- [CHG] changed the unload order, now the first manager loaded is the last
        unloaded
- [CHG] changed component class name to TmafManagerLoader to make it more clear
        what this component does :) 
1.0.003 (29.10.2008) -----------------------------------------------------------
- [ADD] New event OnLoadingFinished wich is fired after all Manager are loaded
1.0.002 (22.10.2008) -----------------------------------------------------------
- [CHANGE] LoadManager now gives in MAN_CREATE_MANAGER instead of Application
           Pointer (wich is obsolet) the FpParamList, so Manager have a
           parameter list ready and can read initial parameters given by the
           user when starting the program
- [ADD] TmafManagerContainer now a list with all parameters given by the user
        when starting the application. Format is <-x><Parameter>, x can be
        replaced by a parameter of your choice, the param follows without space
        i.e.: -lHelge -pMyPassword
        The Parameters are stored in a TList called FpProgramParams and each item
        contains a PProgramParam record, where Tag is the param tag and parameter
        is the parameter value
1.0.001 (28.02.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_ManagerLoader;

interface

uses Windows, Classes, SysUtils, Messages, Registry, IniFiles, Forms,
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_Core;

Type TManagerNotify = procedure(Sender: TObject; Manager: String) Of Object;
     TManagerError  = procedure(Sender: TObject; Manager: String; ErrCode: Integer) Of Object;
     TLoaderNotify  = procedure(Sender: TObject; Manager: String; var ManagerFileName: String) of Object;
     TBeforeConnect = procedure(Sender: TObject; Manager: String; var pUserParam: Pointer) Of Object;
     TAfterConnect  = procedure(Sender: TObject; Manager: String; var pUserParam: Pointer; ErrCode : Integer) Of Object;

     TRegistryRoot = (rrCurrentUser, rrLocalMachine);
     TDataLoadMethod = (dlmRegistry, dlmINIFile, dlmUserdefined);
     TManagerLoaderOption = (mloUseApplicationTag, mloSaveOnClose, mloAttachOnIdle);
     TManagerLoaderOptions = Set of TManagerLoaderOption;

     TmafManagerLoader = class;

     TBaseSettings = class(TPersistent)
     private
       FbUseEnum      : Boolean;
       FOnLoadManager : TLoaderNotify;
     protected
       FOwner : TmafManagerLoader;
       function __GetManagerFileName(Manager: String): String; virtual; abstract;
       procedure __SaveManager(Manager, ManagerFileName: String); virtual; abstract;
       procedure FireOnLoadManager(var Manager, ManagerFileName : String);
     public
       constructor Create(AOwner: TmafManagerLoader); reintroduce; virtual;
       destructor Destroy; override;
       procedure InitAccess; virtual;
       procedure CloseAccess; virtual; abstract;
       procedure LoadManager(Manager: String); virtual; abstract;
       procedure EnumManager; virtual; abstract;
       // events
       property OnLoadManager : TLoaderNotify read FOnLoadManager write FOnLoadManager;
     published
       property UseEnum : Boolean read FbUseEnum write FbUseEnum default False;
     end;

     TRegistrySettings = class(TBaseSettings)
     private
       FRegistryRoot : TRegistryRoot;
       FsRegistryPath : String;
       FpRegistry : TRegistry;
     protected
       function __GetManagerFileName(Manager: String): String; override;
       procedure __SaveManager(Manager, ManagerFileName: String); override;
     public
       constructor Create(AOwner: TmafManagerLoader); override;
       procedure InitAccess; override;
       procedure CloseAccess; override;
       procedure LoadManager(Manager: String); override;
       procedure EnumManager; override;
     published
       property RegistryRoot : TRegistryRoot read FRegistryRoot write FRegistryRoot default rrCurrentUser;
       property RegistryPath : String read FsRegistryPath write FsRegistryPath;
     end; // TRegistrySettings

{ USAGE description for INISettings :

  within the INIfile there should be a section defined for all Managers included
  in this project and their filename, i.e. :

  [MAF Manager]
  Router=C:\Program Files\MAF System\Modules\Router_Firebird.dll
  LinkManager=C:\Program Files\MAF System\Modules\LinkManager.dll


  I splitted Filename and Filepath into 2 properties to give the chance, to let
  the component at starting time find the path instead of giving it
}
     TINISettings = class(TBaseSettings)
     private
       FsINIPath     : String;   // path to the INIfile, if empty the exepath is used
       FsINIFileName : String;   // filename of the ini file
       FsSection     : String;   // section with the Manager names
       FpIniFile     : TIniFile; // Our INI object
     protected
       function __GetManagerFileName(Manager: String): String; override;
       procedure __SaveManager(Manager, ManagerFileName: String); override;
     public
       constructor Create(AOwner: TmafManagerLoader); override;
       procedure InitAccess; override;
       procedure CloseAccess; override;
       procedure LoadManager(Manager: String); override;
       procedure EnumManager; override;
     published
       property IniPath : String read FsINIPath write FsINIPath;
       property INIFileName : String read FsINIFileName write FsINIFileName;
       property Section : String read FsSection write FsSection;
     end; // TINISettings

     TmafManagerLoader = class(TComponent)
     private
       FpManagerList : TStrings;
       FpProgramParams : TList;
       FDataLoadMethod : TDataLoadMethod;
       FpINISettings : TINISettings;
       FpRegistrySettings : TRegistrySettings;
       FbAutoLoad : Boolean;
       FpRegisterNotifyList : TList;
       FManagerLoaderOptions : TManagerLoaderOptions;
       FsManagerSubDirectory : String; // if set, there will be no need to save Manager filename with directory
       FOnConnect : TManagerNotify;
       FOnDisconnect : TManagerNotify;
       FOnLoadManagerError : TManagerError;
       FOnDataLoad : TLoaderNotify;
       FBeforeLoadingManagers : TNotifyEvent;
       FBeforeConnect : TBeforeConnect;
       FAfterConnect : TAfterConnect;
       FOnLoadingFinished : TNotifyEvent;
       FBeforeOpenINI : TNotifyEvent;
       bManagerLoading : Boolean;
       function __GetManagerLibraryName(Manager: String) : String;
       procedure __SetManagerList(const Value: TStrings);
       function __CreateManagerItem : PManagerItem;
       procedure WM_GetManagerData(var Msg: TMessage); Message MSG_GET_MANAGER_DATA;
       procedure __OnLoadManager(Sender: TObject; Manager: String; var ManagerFileName: String);
       function __ReadModulePath: String;
       procedure __OnApplicationIdle(Sender: TObject; var Done: Boolean);
     protected
       FsModulePath : String;
       FbInitError : Boolean;
       function LoadManager(Manager: String; bConnect: Boolean = False): Boolean;
       function UnLoadManager(Manager: String): Boolean;
       procedure SaveSettings;
       procedure SendInitDone;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;

       procedure AddManager(Manager, FileName: String);
       procedure DeleteManager(Manager: String);
       procedure ConnectManager;
       procedure DisconnectManager;
       function ProcessApplication_OnIdle: Boolean;

       property ModulePath : String read __ReadModulePath;
       property InitError: Boolean read FbInitError;
     published
       property Manager : TStrings read FpManagerList write __SetManagerList;
       property DataLoadMethod : TDataLoadMethod read FDataLoadMethod write FDataLoadMethod default dlmUserdefined;
       property INISettings : TINISettings read FpINISettings write FpINISettings;
       property RegistrySettings : TRegistrySettings read FpRegistrySettings write FpRegistrySettings;
       property AutoLoad : Boolean read FbAutoLoad write FbAutoLoad;
       property ManagerSubDirectory : String read FsManagerSubDirectory write FsManagerSubDirectory;
       property ManagerLoaderOptions : TManagerLoaderOptions read FManagerLoaderOptions write FManagerLoaderOptions default [mloUseApplicationTag];
       // events
       property OnConnect : TManagerNotify read FOnConnect write FOnConnect;
       property OnDisconnect : TManagerNotify read FOnDisconnect write FOnDisconnect;
       property OnLoadManagerError : TManagerError read FOnLoadManagerError write FOnLoadManagerError;
       property OnDataLoad : TLoaderNotify read FOnDataLoad write FOnDataLoad;
       property BeforeLoadingManagers : TNotifyEvent read FBeforeLoadingManagers write FBeforeLoadingManagers;
       property BeforeConnect : TBeforeConnect read FBeforeConnect write FBeforeConnect;
       property AfterConnect : TAfterConnect read FAfterConnect write FAfterConnect;
       property OnLoadingFinished : TNotifyEvent read FOnLoadingFinished write FOnLoadingFinished;
       property BeforeOpenINI : TNotifyEvent read FBeforeOpenINI write FBeforeOpenINI;
     end; // TmafManagerContainer

// local error constants
const ERR_FAILED_LOAD_MANAGER     = 1; // occurs when LoadLibrary fails
      ERR_FAILED_LOAD_ENTRY_POINT = 2; // occurs when we were unable to retrieve entry point "UnitServiceProc" for Manager
      ERR_MANAGER_NAME_NOT_FOUND  = 3; // occurs when we were told to load a Manager wich isn't in the FpManagerList
      ERR_MANAGER_NOT_LOADED      = 4; // occurs, if we try to unload a Manager wich isn't loaded
      ERR_OPEN_REGISTRY_KEY       = 5; // occurs when we try to open the key specified in TRegistrySettings.RegistryPath

implementation

uses uMAF_Tools, Dialogs;

function __GetApplicationPath: String;
var pPath : PChar;
begin
  GetMem(pPath, 255);
  GetModuleFileName(GetModuleHandle(nil), pPath, 255);
  Result := ExtractFilePath(String(pPath));
  FreeMem(pPath, 255);
end; // __GetApplicationPath

{ TmafManagerLoader }

constructor TmafManagerLoader.Create(AOwner: TComponent);
begin
  inherited;
  FbInitError := False;
  bManagerLoading := False;
  gManagerLoader := Self;  // all clients can access directly this variable to find the ManagerLoader
  If Not (csDesigning in ComponentState) Then
    bRunMode := True;
  FsManagerSubDirectory := '';
  FpManagerList := TStringList.Create;
  FpINISettings := TINISettings.Create(Self);
  FpINISettings.OnLoadManager := __OnLoadManager;
  FpRegistrySettings := TRegistrySettings.Create(Self);
  FpRegistrySettings.OnLoadManager := __OnLoadManager;
  FDataLoadMethod := dlmUserdefined;
  FpProgramParams := TList.Create;
  FManagerLoaderOptions := [mloUseApplicationTag];
  FpRegisterNotifyList := TList.Create;
end; // Create

destructor TmafManagerLoader.Destroy;
var i: Integer;
begin
  If mloSaveOnClose in FManagerLoaderOptions Then
    SaveSettings;
  FpINISettings.Free;
  FpRegistrySettings.Free;
  For i := FpManagerList.Count - 1 DownTo 0 Do begin
    If not (csDesigning in ComponentState) Then begin
      UnLoadManager(FpManagerList.Strings[i]);         // unloading the Manager
      Dispose(PManagerItem(FpManagerList.Objects[i])); // Free the Manager info struct
    end;  //  --  If not (csDesigning in ComponentState) Then
  end;  //  --  While FpManagerList.Count > 0 Do
  FpManagerList.Clear;
  FpManagerList.Free;
  FpRegisterNotifyList.Free;
  While FpProgramParams.Count > 0 Do begin
    Dispose(PProgramParam(FpProgramParams.Items[0]));
    FpProgramParams.Delete(0);
  end;  //  --  While FpProgramParams.Count > 0 Do
  FpProgramParams.Free;
  inherited;
end; // Destroy

procedure TmafManagerLoader.Loaded;
var i : Integer;
    pParam : PProgramParam;
begin
  inherited;
  If csDesigning in ComponentState Then
    Exit;                                // do nothing in designtime

  If ParamCount > 0 Then begin
    For i := 1 To ParamCount Do begin
      pParam := NewProgramParam;
      pParam^.Tag[1] := ParamStr(i)[1];
      pParam^.Tag[2] := ParamStr(i)[2];
//      pParam^.Tag := Copy(ParamStr(i), 1, 2);
      pParam^.Parameter := Copy(ParamStr(i), 3, Length(ParamStr(i)) - 2);
      FpProgramParams.Add(pParam);
    end;
  end;

{  For i := 0 To FpProgramParams.Count - 1 Do begin
    pParam := PProgramParam(FpProgramParams.Items[i]);
    ShowMessage(pParam^.Tag + ' : ' + pParam^.Parameter);
  end; }

  // clients can find the ManagerContainer through the Application.Tag before
  // the mainform is even created and stored in Application.MainForm
  If mloUseApplicationTag in FManagerLoaderOptions Then
    Application.Tag := Integer(Self);

  // the ManagerLoader can send every OnApplicationIdle event to the TmafHookManager
  // which has a list of all clients, that want to attach themselfs on the event
  If mloAttachOnIdle in FManagerLoaderOptions Then
    Application.OnIdle := __OnApplicationIdle;

  // creating module path if possible
  If FsManagerSubDirectory <> '' Then
    FsModulePath := __GetApplicationPath + FsManagerSubDirectory + '\';

  Case FDataLoadMethod Of
    dlmINIFile  : begin
                    If Assigned(FBeforeOpenINI) Then
                      FBeforeOpenINI(Self);
                    FpINISettings.InitAccess;
                  end;
    dlmRegistry : FpRegistrySettings.InitAccess;
  end;  //  --  Case FDataLoadMethod Of

  // Loading the Modules from the Manager-Stringlist
  For i := 0 To FpManagerList.Count - 1 Do begin
    FpManagerList.Objects[i] := TObject(__CreateManagerItem);
    LoadManager(FpManagerList.Strings[i]);
  end;  //  --  For i := 0 To FpManagerList.Count - 1 Do

  // now loading the Manager defined in Registry or IniFile
  Case FDataLoadMethod Of
    dlmINIFile  : If FpINISettings.UseEnum Then
                    FpINISettings.EnumManager;
    dlmRegistry : If FpRegistrySettings.UseEnum Then
                    FpRegistrySettings.EnumManager;
  end;  //  --  Case FDataLoadMethod Of

  // finally we connect, if wanted
  If FbAutoLoad Then
    ConnectManager;
end; // Loaded

procedure TmafManagerLoader.__SetManagerList(const Value: TStrings);
begin
  FpManagerList.Assign(Value);
end; // __SetManagerList

procedure TmafManagerLoader.WM_GetManagerData(var Msg: TMessage);
var i : integer;
    pItem : PManagerItem;
    bFound : Boolean;
begin
  If Msg.WParam > 0 Then begin
    bFound := False;
    For i := 0 To FpManagerList.Count - 1 Do begin
      pItem := PManagerItem(FpManagerList.Objects[i]);
      If pItem^.nManagerType = Msg.WParam Then begin
        bFound := True;
        Msg.Result := Integer(pItem);
        FreeNotification(TComponent(Msg.LParam));
        Break;
      end;  //  --  If pItem^.nManagerType = Msg.WParam Then
    end;  //  --  For i := 0 To FpManagerList.Count - 1 Do
    // in case a manager isnt loaded yet, we add the component to a notify list
    // and when the manager loading is done, they will be notified
    If ((Not bFound) And (bManagerLoading)) Then
      FpRegisterNotifyList.Add(Pointer(Msg.LParam));
  end;  //  --  If Msg.WParam > 0 Then
end; // WM_GetManagerData

function TmafManagerLoader.__CreateManagerItem: PManagerItem;
begin
  New(Result);
  FillChar(Result^, SizeOf(RManagerItem), 0);
  Result^.FbLoaded := False;
end; // __CreateManagerItem

function TmafManagerLoader.__GetManagerLibraryName(Manager: String): String;
var S : String;
begin
  Case FDataLoadMethod Of
    dlmRegistry    : S := FpRegistrySettings.__GetManagerFileName(Manager);
    dlmINIFile     : S := FpINISettings.__GetManagerFileName(Manager);
    dlmUserdefined : begin
                       If Assigned(FOnDataLoad) Then
                         FOnDataLoad(Self, Manager, S);
                     end; // dlmUserdefined
  End;  //  --  Case FDataLoadMethod Of
  If ExtractFilePath(S) = '' Then
    S := IncludeTrailingPathDelimiter(FsModulePath) + S;
  Result := S;
end; // __GetManagerLibraryName

procedure TmafManagerLoader.__OnLoadManager(Sender: TObject; Manager: String; var ManagerFileName: String);
begin
  AddManager(Manager, ManagerFileName);  // adding to list
  If FbAutoLoad Then
    LoadManager(Manager);                // loading the library
end; // __OnLoadManager

function TmafManagerLoader.__ReadModulePath: String;
begin
  Result := FsModulePath;
end; // __ReadModulePath

procedure TmafManagerLoader.SaveSettings;
var i : Integer;
    pItem : PManagerItem;
begin
  If csDesigning in ComponentState Then // just making sure...
    Exit;

  For i := 0 To FpManagerList.Count - 1 Do begin
    pItem := PManagerItem(FpManagerList.Objects[i]);
    Case FDataLoadMethod Of
      dlmRegistry : FpRegistrySettings.__SaveManager(FpManagerList.Strings[i], pItem^.FsLibraryName);
      dlmINIFile  : FpINISettings.__SaveManager(FpManagerList.Strings[i], pItem^.FsLibraryName);
    end;  //  --  Case FDataLoadMethod Of
  end;  //  --  For i := 0 To FpManagerList.Count - 1 Do
end; // SaveSettings

procedure TmafManagerLoader.SendInitDone;
var i: Integer;
begin
  For i := 0 To Owner.ComponentCount - 1 Do
    If IsClass(Owner.Components[i], TmafBaseComponent) Then
      SendComponentMessage(Owner.Components[i], WM_INIT_DONE, nil, nil);
end; // SendInitDone

procedure TmafManagerLoader.ConnectManager;
var i : Integer;
    pData : PManagerItem;
    QHS : pQHS;
begin
  // firing event before we are going to connect to the Manager libraries
  If Assigned(FBeforeLoadingManagers) Then
    FBeforeLoadingManagers(Self);

  bManagerLoading := True;
  For i := 0 To FpManagerList.Count - 1 Do
    LoadManager(FpManagerList.Strings[i], True);
  bManagerLoading := False;

  // during the manager loading the modules were loaded and some use client
  // components on their datamodules. Those couldn't register themselfs with
  // their manager yet, as the manager wasn't loaded yet. Now we notify them
  // that all manager are loaded and they can register now
  If FpRegisterNotifyList.Count > 0 Then
    For i := 0 To FpRegisterNotifyList.Count - 1 Do
      SendComponentMessage(TComponent(FpRegisterNotifyList.Items[i]), WM_REGISTER_NOTIFY, nil, nil);

  QHS := __Create_QueryHandlerStruct;
  For i := 0 To FpManagerList.Count - 1 Do begin
    pData := PManagerItem(FpManagerList.Objects[i]);
    QHS^.Reserved1 := pData^.nManagerType;
    Try
      pData^.FpEntryPoint(MAN_INIT_DONE, QHS, nil);
    Except
      FbInitError := True;
    End;
  end;  //  --  For i := 0 To FpManagerList.Count - 1 Do
  __Free_QueryHandlerStruct(QHS);

  // sending to the MAF components on our own form the message. They won't receive it from
  // the manager itself, because they're already loaded when we load the manager
  SendInitDone;
  // now we can tell the user, that all manager are loaded
  If Assigned(FOnLoadingFinished) Then
    FOnLoadingFinished(Self);
end; // ConnectManager

procedure TmafManagerLoader.DisconnectManager;
var i : Integer;
begin
  If mloAttachOnIdle in FManagerLoaderOptions Then
    Application.OnIdle := nil;

  For i := FpManagerList.Count - 1 DownTo 0 Do
    UnLoadManager(FpManagerList.Strings[i]);
end; // DisconnectManager

function TmafManagerLoader.LoadManager(Manager: String; bConnect: Boolean = False): Boolean;
var i, j, ErrCode, ModuleErrCode : Integer;
    pData : PManagerItem;
    pUserParam : Pointer;
    QHS : pQHS;
begin
  Result := False;

  If csDesigning in ComponentState Then // just making sure...
    Exit;

  ErrCode := ERR_MANAGER_NAME_NOT_FOUND;
  i := FpManagerList.IndexOf(Manager);
  If i > -1 Then begin
    pData := PManagerItem(FpManagerList.Objects[i]);
    If pData^.FsLibraryName = '' Then   // After AddManager we already have a filename
      pData^.FsLibraryName := __GetManagerLibraryName(Manager);

    // in case, we are not supposed to connect just now or the Manager is already connected !
    {$B-}  // Boolean short-circuit evaluation off
    If ((Not bConnect) Or (pData^.FbLoaded)) Then begin
    {$B+}  // Boolean short-circuit evaluation on
    Result := (pData^.FsLibraryName <> '');
      If bConnect Then
        Result := pData^.FbLoaded;
      Exit;
    end;  //  --  If Not bConnect Then

    For j := 0 To FpManagerList.Count - 1 Do
      If ((PManagerItem(FpManagerList.Objects[j])^.FsLibraryName = pData^.FsLibraryName) And (PManagerItem(FpManagerList.Objects[j])^.FbLoaded)) Then begin
        pData^.dwHandle := PManagerItem(FpManagerList.Objects[j])^.dwHandle;
        Break;
      end;
    If pData^.dwHandle = 0 Then
      pData^.dwHandle := LoadLibrary(PChar(pData^.FsLibraryName));

    If pData^.dwHandle > 0 Then begin
      // library was loaded, so let's get the entry point
      @pData^.FpEntryPoint := GetProcAddress(pData^.dwHandle, PChar(MANAGER_SERVICE_PROC));
      If Assigned(pData^.FpEntryPoint) Then begin
        // it's loaded, maybe a test, if it's a manager would be nice ?
        pUserParam := nil;                        // default we don't need
        pData^.FbLoaded := True;
        If Assigned(FBeforeConnect) Then          // firing event BeforeConnect
          FBeforeConnect(Self, Manager, pUserParam);
        QHS := __Create_QueryHandlerStruct;
        QHS^.pParent := Owner;
        QHS^.pChildObj := Pointer(FpProgramParams);
        ModuleErrCode := pData^.FpEntryPoint(MAN_CREATE_MANAGER, QHS, pUserParam);
        QHS^.pChildObj := nil;
        StrToPChar(Manager, PChar(QHS^.pChildObj));
        If pData^.FpEntryPoint(MAN_GET_MANAGER_TYPE, QHS, pUserParam) = ERR_NO_ERROR Then
          pData^.nManagerType := QHS^.ResVal
        Else
          ShowMessage('Could not load manager "'+pData^.FsLibraryName+'"');
        FreePChar(PChar(QHS^.pChildObj));
        __Free_QueryHandlerStruct(QHS);
        If Assigned(FAfterConnect) Then
          FAfterConnect(Self, Manager, pUserParam, ModuleErrCode);
        If Assigned(FOnConnect) Then  // firing event, that we loaded a manager
          FOnConnect(Self, Manager);
        ErrCode := 0;
      end else  //  --  If Assigned(pData^.FpEntryPoint) Then
        ErrCode := ERR_FAILED_LOAD_ENTRY_POINT;
    end else  //  --  If pData^.dwHandle > 0 Then
      ErrCode := ERR_FAILED_LOAD_MANAGER;
    Result := pData^.FbLoaded;
  end;  //  --  If i > -1 Then

  {$B-}  // Boolean short-circuit evaluation off
  If ((ErrCode > 0) And (Assigned(FOnLoadManagerError))) Then
  {$B+}  // Boolean short-circuit evaluation om
    FOnLoadManagerError(Self, Manager, ErrCode);
end; // LoadManager

function TmafManagerLoader.UnLoadManager(Manager: String): Boolean;
var i, ErrCode : Integer;
    pData : PManagerItem;
    pUserParam : Pointer;
    QHS : pQHS;
    bUnloaded : Boolean;
begin
  Result := False;

  If csDesigning in ComponentState Then // just making sure...
    Exit;

  pUserParam := nil;
  ErrCode := ERR_MANAGER_NAME_NOT_FOUND;
  i := FpManagerList.IndexOf(Manager);
  If i > -1 Then begin
    pData := PManagerItem(FpManagerList.Objects[i]);
    If pData^.FbLoaded Then begin
      If Assigned(FOnDisconnect) Then
        FOnDisconnect(Self, Manager);
      // get proc adress again
      @pData^.FpEntryPoint := GetProcAddress(pData^.dwHandle, PChar(MANAGER_SERVICE_PROC));
      If Assigned(pData^.FpEntryPoint) Then begin
        StrToPChar(Manager, PChar(pUserParam));
        QHS := __Create_QueryHandlerStruct;
        pData^.FpEntryPoint(MAN_CLOSE_COMPONENT, QHS, pUserParam);
        FreePChar(PChar(pUserParam));
        If QHS^.Reserved1 = 0 Then begin
          pData^.FpEntryPoint(MAN_CLOSE_MANAGER, nil, pUserParam);
          bUnloaded := FreeLibrary(pData^.dwHandle);
{          If not bUnloaded Then
            Showmessage(SysErrorMessage(GetLastError)); }
        end;
        __Free_QueryHandlerStruct(QHS);
      end;

      ErrCode := 0;
      FillChar(pData^, SizeOf(RManagerItem), 0);
      pData^.FbLoaded := False;
      Result := True;
    end else  //  --  If pData^.FbLoaded Then
      ErrCode := ERR_MANAGER_NOT_LOADED;
  end;  //  --  If i > -1 Then

  {$B-}  // Boolean short-circuit evaluation off
  If ((ErrCode > 0) And (Assigned(FOnLoadManagerError))) Then
  {$B+}  // Boolean short-circuit evaluation on
    FOnLoadManagerError(Self, Manager, ErrCode);
end; // UnLoadManager

procedure TmafManagerLoader.AddManager(Manager, FileName: String);
var i : Integer;
    pItem : PManagerItem;
begin
  i := FpManagerList.IndexOf(Manager);
  If i > -1 Then begin
    // we don't have this Manager yet, so let's try to load it
    If csDesigning in ComponentState Then begin
      FpManagerList.Add(Manager);                  // in designtime we just add
      Exit;
    end;  //  --  If csDesigning in ComponentState Then
  end else begin
    pItem := __CreateManagerItem;                  // creating the data struct
    pItem^.FsLibraryName := FileName;              // saving the filename
    FpManagerList.AddObject(Manager, TObject(pItem)); // adding to the list
  end;  //  --  If i > -1 Then
end; // AddManager

procedure TmafManagerLoader.DeleteManager(Manager: String);
var i : Integer;
    pItem : PManagerItem;
begin
  i := FpManagerList.IndexOf(Manager);
  If i > -1 Then begin
    pItem := PManagerItem(FpManagerList.Objects[i]);
    UnLoadManager(Manager);
    Dispose(pItem);
    FpManagerList.Delete(i);
  end;  //  --  If i > -1 Then 
end; // DeleteManager

{ TBaseSettings }

constructor TBaseSettings.Create(AOwner: TmafManagerLoader);
begin
  FOwner := AOwner;
  FbUseEnum := False;
end; // Create

destructor TBaseSettings.Destroy;
begin
  CloseAccess;
  inherited;
end;

procedure TBaseSettings.InitAccess;
begin
  // nothing
end; // InitAccess

procedure TBaseSettings.FireOnLoadManager(var Manager, ManagerFileName: String);
begin
  // if we have a ModulePath, the library names are stored without path
  If FOwner.ModulePath <> '' Then
    ManagerFileName := FOwner.ModulePath + ManagerFileName;
  {$B-}  // Boolean short-circuit evaluation off
  If ((ManagerFileName <> '') And (Assigned(FOnLoadManager))) Then
  {$B+}  // Boolean short-circuit evaluation on
    FOnLoadManager(Self, Manager, ManagerFileName);
end; // FireOnLoadManager

procedure TmafManagerLoader.__OnApplicationIdle(Sender: TObject; var Done: Boolean);
var i : Integer;
    QHS : pQHS;
begin
  For i := 0 To FpManagerList.Count - 1 Do
    If PManagerItem(FpManagerList.Objects[i])^.nManagerType = MT_ROUTER Then
      If PManagerItem(FpManagerList.Objects[i])^.FbLoaded Then begin
        QHS := __Create_QueryHandlerStruct;
        QHS^.HookID := MT_ROUTER;
        QHS^.SubHookID := HM_APPLICATION_IDLE;
        PManagerItem(FpManagerList.Objects[i])^.FpEntryPoint(MT_ROUTER, QHS, nil);
        Done := (QHS^.ResVal > 0);
        __Free_QueryHandlerStruct(QHS);
      end;
end;

function TmafManagerLoader.ProcessApplication_OnIdle: Boolean;
var bDone: Boolean;
begin
  bDone := False;
  __OnApplicationIdle(Application, bDone);
  Result := bDone;   
end;

{ TINISettings }

constructor TINISettings.Create(AOwner: TmafManagerLoader);
begin
  inherited;
  FpIniFile := nil;
end; // Create

procedure TINISettings.CloseAccess;
begin
  If Assigned(FpIniFile) Then
    FreeAndNil(FpIniFile);
  inherited;
end; // CloseAccess

procedure TINISettings.InitAccess;
begin
  inherited;
  FpIniFile := TIniFile.Create(__GetApplicationPath + FsINIFileName);
end; // InitAccess

function TINISettings.__GetManagerFileName(Manager: String): String;
begin
  Result := '';
  {$B-}  // Boolean short-circuit evaluation off
  If ((FpIniFile = nil) Or (FsSection = '')) Then
  {$B+}  // Boolean short-circuit evaluation on
    Exit;

  Result := FpIniFile.ReadString(FsSection, Manager, '');
end; //  __GetManagerFileName

procedure TINISettings.LoadManager(Manager: String);
var S : String;
begin
  {$B-}  // Boolean short-circuit evaluation off
  If ((FpIniFile = nil) Or (FsSection = '')) Then
  {$B+}  // Boolean short-circuit evaluation on
    Exit;

  FpIniFile.ReadString(FsSection, Manager, S);
  {$B-}  // Boolean short-circuit evaluation off
  If ((S <> '') And (Assigned(FOnLoadManager))) Then
  {$B+}  // Boolean short-circuit evaluation on
    FOnLoadManager(Self, Manager, S);
end; // LoadManager

procedure TINISettings.EnumManager;
var List : TStringList;
    i : Integer;
    Manager, S : String;
begin
  {$B-}  // Boolean short-circuit evaluation off
  If ((FpIniFile = nil) Or (FsSection = '')) Then
  {$B+}  // Boolean short-circuit evaluation on
    Exit;

  List := TStringList.Create;
  FpIniFile.ReadSectionValues(FsSection, List);
  For i := 0 To List.Count - 1 Do begin
    S := List.Strings[i];
    Manager := Copy(S, 1, Pos('=', S) - 1);
    If Pos('=', S) > 0 Then
      Delete(S, 1, Pos('=', S));
    FireOnLoadManager(Manager, S);
  end;  //  --  For i := 0 To List.Count - 1 Do
  List.Free;
end; // EnumManager

procedure TINISettings.__SaveManager(Manager, ManagerFileName: String);
var S : String;
begin
  {$B-}  // Boolean short-circuit evaluation off
  If ((FpIniFile = nil) Or (FsSection = '')) Then
  {$B+}  // Boolean short-circuit evaluation on
    Exit;

  // if we have a ModulePath, the library names are stored without path
  If FOwner.ModulePath <> '' Then
    S := ExtractFileName(ManagerFileName)
  Else
    S := ManagerFileName;
  FpIniFile.WriteString(FsSection, Manager, S);  // writing into INI file
end; // __SaveManager

{ TRegistrySettings }

constructor TRegistrySettings.Create(AOwner: TmafManagerLoader);
begin
  inherited;
  FpRegistry := nil;
end; // Create

procedure TRegistrySettings.InitAccess;
begin
  inherited;
  FpRegistry := TRegistry.Create;
  Case FRegistryRoot Of
    rrCurrentUser  : FpRegistry.RootKey := HKEY_CURRENT_USER;
    rrLocalMachine : FpRegistry.RootKey := HKEY_LOCAL_MACHINE;
  end;  //  --  Case FRegistryRoot Of
  // FsRegistryPath should be like "\Software\$MyCompany$\$MyProduct$\Modules"
  If Not FpRegistry.OpenKey(FsRegistryPath, True) Then
    If Assigned(FOwner.OnLoadManagerError) Then
      FOwner.OnLoadManagerError(FOwner, '', ERR_OPEN_REGISTRY_KEY);
end; // InitAccess

procedure TRegistrySettings.CloseAccess;
begin
  If Assigned(FpRegistry) Then
    FreeAndNil(FpRegistry);
  inherited;
end; // CloseAccess

procedure TRegistrySettings.EnumManager;
var List : TStringList;
    i : Integer;
    Manager, S : String;
begin
  If FpRegistry = nil Then
    Exit;

  List := TStringList.Create;
  FpRegistry.GetValueNames(List);
  For i := 0 To List.Count - 1 Do begin
    Manager := List.Strings[i];
    S := FpRegistry.ReadString(Manager);
    FireOnLoadManager(Manager, S);
  end;  //  --  For i := 0 To List.Count - 1 Do
  List.Free;
end; // EnumManager

procedure TRegistrySettings.LoadManager(Manager: String);
var S : String;
begin
  If FpRegistry = nil Then
    Exit;

  S := FpRegistry.ReadString(Manager);
  {$B-}  // Boolean short-circuit evaluation off
  If ((S <> '') And (Assigned(FOnLoadManager))) Then
  {$B+}  // Boolean short-circuit evaluation on
    FOnLoadManager(Self, Manager, S);
end; // LoadManager

function TRegistrySettings.__GetManagerFileName(Manager: String): String;
begin
  Result := '';
  If FpRegistry = nil Then
    Exit;

  Result := FpRegistry.ReadString(Manager);
  // if we have a ModulePath, the library names are stored without path
  If FOwner.ModulePath <> '' Then
    Result := FOwner.ModulePath + Result;
end; // __GetManagerFileName

procedure TRegistrySettings.__SaveManager(Manager, ManagerFileName: String);
var S : String;
begin
  If FpRegistry = nil Then
    Exit;

  // if we have a ModulePath, the library names are stored without path
  If FOwner.ModulePath <> '' Then
    S := ExtractFileName(ManagerFileName)
  Else
    S := ManagerFileName;
  FpRegistry.WriteString(Manager, S);
end; // __SaveManager

initialization
  RegisterClass(TmafManagerLoader);

finalization
  UnRegisterClass(TmafManagerLoader);

end.
