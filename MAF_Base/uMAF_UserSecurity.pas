{*******************************************************************************
Name         : uMAF_UserSecurity.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007-2010 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 25.05.2007
Last Update  : 19.08.2010
Version      : 1.1.006
Purpose      : provides double layered user security
               a standard user, wich can only access the user table and (if used)
               the group table
               from there logins and encrypted passwords can be read
               the user knows his password for his data entry only. If the
               password is provided correctly, the password2 will be decrypted
               and used to login the user into the database with the real database
               password the user doesn't even know
Last Changes :

1.1.006 (18.08.2010) -----------------------------------------------------------
- [ADD] new option soActivateGetUserData, that allows to get the decrypted
        database password, when QHS^.ResVal is set to 1
1.1.005 (30.09.2009) -----------------------------------------------------------
- [CHG] changed the encryption... again... but no there will be no need for
        external units from 3rd party anymore and provides a nice encryption
        anyway
1.1.004 (01.09.2009) -----------------------------------------------------------
- [ADD] added a new private variable to hold the string used to encrypt/decrypt
        also an event to ask for the string in case it's empty as this string
        REALLY is needed
        To set the variable one has to use the public method SetAppPW, there is
        no other access to the private string variable due to security reason
- [CHG] changed encryption to Rijndael algorithm, the routines come from a free
        encryption library called DEC 5.2 to be found under
        http://www.torry.net/vcl/security/strong/DECv5.2.zip
        The change was needed as my old routines didn't work well with the
        string changes Delphi 2009 introduced and I'm really no expert in
        encryption methods and didn't want to invent the wheel for a 2nd time ;)
1.1.003 (24.07.2009) -----------------------------------------------------------
- [CHG] changed TmafUserSecurity to inherit from TmafCustomManagerComponent
        therefore the obsolet HookAccessContainer stuff was deleted and we now
        use the ModuleController defined in the new CustomClass
1.1.002 (03.11.2008) -----------------------------------------------------------
- [ADD] in ModifyUser the old user data are requested now and if the old password
        wasn't changed, it will be taken from the current record. Therefore there
        is no need to give any password from the user dialog, just leave the
        RUserData.Password empty
1.1.001 (29.10.2008) -----------------------------------------------------------
- [ADD] added SilentConnect (with a SecurityOption soAllowSilentConnect) to
        connect through CommandLine parameters for example
        To use it the flag soAllowSilentConnect MUST be set
- [DEL] soUserList and soGroups removed from SecurityOption, they became obsolete
1.1.000 (23.10.2008) -----------------------------------------------------------
- [ADD] User functions to create, modify, delete and list users as well as groups
- [CHG] update to Manager-Style component, that registers itself with the HAC
- [CHG] improved Connect method with a new event for a login dialog, processing
        of password passed along as parameter of the program, usage of the
        standard login to read the user table
1.0.001 (25.05.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_UserSecurity;

interface

uses Windows, Classes, SysUtils,
     // 3rd party units

     // Modular Application Framework Components units
     uMAF_Globals, uMAF_Tools, uMAF_CustomBaseDB, uMAF_Core;

     // SecurityOptions are the following :
     //   soOwnEncryption       : in case a password needs to be encrypted/decrypted
     //                           an event will be fired and allows the programmer
     //                           to use an own encryption method rather than the built-in one
     //   soAllowSilentConnect  : Username and password can be set when login, no
     //                           OnLoginDialog will be fired, the password has to be
     //                           encrypted
     //   soActivateGetUserData : The program is allowed to ask for userdata with decrypted
     //                           passwords, usefull for remote-applications, where
     //                           the server administrates the user but the client has to
     //                           do the login through the DAC
Type TSecurityOption = (soOwnEncryption, soAllowSilentConnect, soActivateGetUserData);
     TSecurityOptions = Set of TSecurityOption;

     TOnEncryption = procedure(Sender: TObject; var Password: String; var PW_Encrypted: String) Of Object;
     TOnUserList = procedure(Sender: TObject; UserList: TStringList; var Password: String) Of Object;
     TOnUserAction = procedure(Sender: TObject; pData: PUserDataRec; var CanContinue: Boolean) of Object;

     TmafCustomUserSecurity = class(TmafCustomManagerComponent)

     end;
     
     TmafUserSecurity = class(TmafCustomUserSecurity)
     private
       FbAutoLoad : Boolean;  // Set to True, if automatically load on startup
       FsUserName : String;   // public account who can read the user table
       FsPassword : String;   // the accounts password
       FsUserTable : String;  // table name where all users are stored
       FsGroupTable : String; // table name where groups are stored
       FnGroupID   : Integer; // GroupID for the users to list
       FpBaseDB : TmafCustomBaseDB; // BaseDB component
       FSecurityOptions : TSecurityOptions;
       FnPasswordLength : Byte; // length of generated passwords
       // events
       FOnEncrypt : TOnEncryption; // fired when a PW needs to be encrypted and SecurityOption "soOwnEncryption" is set
       FOnDecrypt : TOnEncryption; // fired when a PW needs to be decrypted and SecurityOption "soOwnEncryption" is set
       FOnUserCreate : TOnUserAction; // fired when user will be created, deleted or modified
       FOnUserDelete : TOnUserAction;
       FOnLoginDialog : TOnUserAction; // fired, when the user might wanna pop up a login dialog
       FOnUserLoginError : TNotifyEvent; // user login error event
       FAfterConnect : TNotifyEvent;     // event, when login was successful
       FOnDecryptPassword : TOnEncryption;
       PWChars : String;
       procedure __SetAutoLoad(const Value: Boolean);
       procedure __SetBaseDB(const Value: TmafCustomBaseDB);
       procedure __SetUserTable(const Value: String);
       procedure __SetPasswordLength(const Value: Byte);
       procedure __SetGroupTable(const Value: String);
       procedure __Free_GroupList(pData: Pointer);
       function __Encrypt(APassword: String) : String;
       function __Decrypt(APassword: String) : String;
     protected
       procedure __RegisterAPI; override;
       procedure __OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer); override;

       function CreateUser(pData: PUserDataRec; bGenPW: Boolean=True): Integer;
       function DeleteUser(pData: PUserDataRec): Integer;
       function UpdateUser(pData: PUserDataRec): Integer;
       function GetUserData(pData: PUserDataRec): Integer;
       function AdministrateUsers(SubHookID: Integer; QHS: pQHS): Integer;

       function AdministrateGroup(SubHookID: Integer; QHS: pQHS): Integer;
       function GetGroupList(QHS: pQHS): Integer;
     public
       constructor Create(AOwner: TComponent); override;
       procedure Loaded; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       procedure Connect(bSilent : Boolean = False);
       procedure SilentConnect(UserName, Password: String);
       function GeneratePassword: String;  // will be returned uncrypted
       function GetUserList(List: TStringList; GroupID: Integer): Integer;
     published
       property AutoLoad : Boolean read FbAutoLoad write __SetAutoLoad;
       property SecurityOptions : TSecurityOptions read FSecurityOptions write FSecurityOptions default [];
       property UserName : String read FsUserName write FsUserName;
       property Password : String read FsPassword write FsPassword;
       property UserTable : String read FsUserTable write __SetUserTable;
       property GroupTable : String read FsGroupTable write __SetGroupTable;
       property GroupID : Integer read FnGroupID write FnGroupID;
       property PasswordLength : Byte read FnPasswordLength write __SetPasswordLength default 10;
       property BaseDB : TmafCustomBaseDB read FpBaseDB write __SetBaseDB;
       // events
       property OnEncrypt : TOnEncryption read FOnEncrypt write FOnEncrypt;
       property OnDecrypt : TOnEncryption read FOnDecrypt write FOnDecrypt;
       property OnUserCreate : TOnUserAction read FOnUserCreate write FOnUserCreate;
       property OnUserDelete : TOnUserAction read FOnUserDelete write FOnUserDelete;
       property OnLoginDialog : TOnUserAction read FOnLoginDialog write FOnLoginDialog;
       property OnUserLoginError : TNotifyEvent read FOnUserLoginError write FOnUserLoginError;
       property OnAfterConnect : TNotifyEvent read FAfterConnect write FAfterConnect;
       property OnDecryptPassword : TOnEncryption read FOnDecryptPassword write FOnDecryptPassword;
     end;

implementation

{$IFDEF Tracer} uses uMAF_Tracer; {$ENDIF}

//const PWChars : String = '';

const MAX_EVENTS = 6;
      _Var1 = 73388;
      _Var2 = -68896;
      _Var3 = 3589963;

var ManagerEvents : array[1..MAX_EVENTS] of Integer = (
      HM_USER_SECURITY,
      US_GET_USERLIST,
      US_CREATE_USER,
      US_DELETE_USER,
      US_MODIFY_USER,
      US_GET_USER_DATA);

{ TmafUserSecurity }

constructor TmafUserSecurity.Create(AOwner: TComponent);
begin
  inherited;
  ManagerType := MT_USER_SECURITY;
  Randomize;   // we'll need a randomizer to generate passwords
  PWChars := String('asd0fGghNBE9jkl1p8o#iCDX5uyIUHt3ewrq7_b6PAmLnW4vzQ2cSx'); // 54 chars will be used for create passwords
  FSecurityOptions := [];
  FsUserName := '';
  FsPassword := '';
  FsUserTable := '';
  FnGroupID := 0;  // no group
  FnPasswordLength := 10;
  ManagerOptions := [];
end;

procedure TmafUserSecurity.Loaded;
begin
  {$IFDEF Tracer}
    MAFTracer.Enter('TmafUserSecurity.Loaded');
  {$ENDIF}
  inherited;
  AutoLoad := FbAutoLoad; // now we set the value to trigger __SetAutoLoad
  {$IFDEF Tracer}
    MAFTracer.Leave;
  {$ENDIF}
end;

procedure TmafUserSecurity.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  If Operation = opRemove Then begin
    If AComponent = FpBaseDB Then
      FpBaseDB := nil;
  end;  //  --  If Operation = opRemove Then
end;

procedure TmafUserSecurity.__SetUserTable(const Value: String);
begin
  FsUserTable := Value;
  If Assigned(FpBaseDB) Then
    FpBaseDB.UserTable := FsUserTable;
end;

procedure TmafUserSecurity.__SetBaseDB(const Value: TmafCustomBaseDB);
begin
  FpBaseDB := Value;
  UserTable := FsUserTable;   // triggering __SetUserTable
  GroupTable := FsGroupTable; // triggering __SetGroupTable
end;

procedure TmafUserSecurity.__SetGroupTable(const Value: String);
begin
  FsGroupTable := Value;
  If Assigned(FpBaseDB) Then
    FpBaseDB.GroupTable := FsGroupTable;
end;

// the input password MUST be Ansistring, as it is assumed, that they are
// UTF8 encoded
function TmafUserSecurity.__Decrypt(APassword: String): String;
begin
  If soOwnEncryption in FSecurityOptions Then begin
    If Assigned(FOnDecrypt) Then
      FOnDecrypt(Self, Result, APassword);
  end else
    Result := Decrypt(APassword, _Var1, _Var2, _Var3);
end;

// Passwords can come as Ansi or unicode strings, but they will always
// be safely encrypted as AnsiString UTF8 encoded
function TmafUserSecurity.__Encrypt(APassword: String): String;
begin
  If soOwnEncryption in FSecurityOptions Then begin
    If Assigned(FOnEncrypt) Then
      FOnEncrypt(Self, APassword, Result);
  end else begin
    Result := Encrypt(APassword, _Var1, _Var2, _Var3);
  end;
end;

procedure TmafUserSecurity.__Free_GroupList(pData: Pointer);
var i : Integer;
begin
  If Assigned(pData) Then begin
    For i := 0 To TList(pData).Count - 1 Do
      Dispose(PGroupDataRec(TList(pData).Items[i]));
    TList(pData).Free;
  end;
end;

procedure TmafUserSecurity.__OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  Case SubHookID Of
    HM_USER_SECURITY :
      begin
        Case QHS^.SubHookID Of
          US_GET_GROUPLIST : ErrCode := GetGroupList(QHS);
          US_UPDATE_GROUP,
          US_DELETE_GROUP,
          US_CREATE_GROUP  : ErrCode := AdministrateGroup(QHS^.SubHookID, QHS);
          US_GET_USERLIST  : ErrCode := GetUserList(QHS^.pChildObj, QHS^.Reserved1);
          US_CREATE_USER   : ErrCode := CreateUser(QHS^.pChildObj, (QHS^.Reserved1 = 0));
          US_DELETE_USER   : ErrCode := DeleteUser(QHS^.pChildObj);
          US_MODIFY_USER   : ErrCode := UpdateUser(QHS^.pChildObj);
          US_GET_USER_DATA : begin
                               ErrCode := GetUserData(QHS^.pChildObj);
                               If ((QHS^.ResVal = 1) And (soActivateGetUserData in FSecurityOptions)) Then
                                 PUserDataRec(QHS^.pChildObj)^.Password2 := __Decrypt(PUserDataRec(QHS^.pChildObj)^.Password2);
                             end;
        end;
      end;
    US_GET_USERLIST  : ErrCode := GetUserList(QHS^.pChildObj, QHS^.Reserved1);
    US_CREATE_USER   : ErrCode := CreateUser(QHS^.pChildObj, (QHS^.Reserved1 = 0));
    US_DELETE_USER   : ErrCode := DeleteUser(QHS^.pChildObj);
    US_MODIFY_USER   : ErrCode := UpdateUser(QHS^.pChildObj);
    US_GET_USER_DATA : ErrCode := GetUserData(QHS^.pChildObj);
    Else inherited __OnEvent(SubHookID, QHS, UserParam, ErrCode);
  end;  //  --  Case SubHookID Of
end;

procedure TmafUserSecurity.__RegisterAPI;
var i : Integer;
begin
  inherited;
  If Assigned(ModuleController) Then
    For i := 1 To MAX_EVENTS Do
      ModuleController.RegisterSubHookEvent(ManagerEvents[i], __OnEvent);
end;

procedure TmafUserSecurity.__SetAutoLoad(const Value: Boolean);
begin
  If csLoading in ComponentState Then  // only when done loading the component
    Exit;

  FbAutoLoad := Value;
  If FbAutoLoad Then
    Connect;
end;

procedure TmafUserSecurity.__SetPasswordLength(const Value: Byte);
begin
  If ((Value > 4) And (Value < 32)) Then  // we accept only a password length between 4 and 32 chars
    FnPasswordLength := Value;
end;

procedure TmafUserSecurity.Connect(bSilent : Boolean = False);
var bCanContinue : Boolean;
    pData: PUserDataRec;
    sPW : String;
begin
  sPW := '';
  If Assigned(FpBaseDB) Then begin
    // let's connect with standard user to be able to read the user table
    FpBaseDB.User := FsUserName;
    FpBaseDB.Password := FsPassword;
    FpBaseDB.Connected := True;
    If FpBaseDB.Connected Then begin
      New(pData);
      FillChar(pData^, SizeOf(RUserDataRec), 0);
      bCanContinue := True;
      If Assigned(FOnLoginDialog) Then
        FOnLoginDialog(Self, pData, bCanContinue);
      If bCanContinue Then begin
        sPW := pData^.Password;   // the password the user entered in the login dialog
        If FpBaseDB.QueryUserTable(US_GET_USER_DATA, pData) Then begin
          FpBaseDB.Connected := False;
//          pData^.Password := __Encrypt('3zehnter');
//          pData^.Flags := 1;
          If __Decrypt(pData^.Password) = sPW Then begin
            FpBaseDB.User := pData^.Login;
            FpBaseDB.Password := {'3zehnter'; //}__Decrypt(pData^.Password2);
            FpBaseDB.Connected := True;
            If FpBaseDB.Connected Then begin
              If Assigned(FAfterConnect) Then
                FAfterConnect(Self);
              FnGroupID := pData^.GroupID;
            end;
          end;  //  --  If B64Decode(pData^.Password) = sPW Then
        end;  //  --  If FpBaseDB.GetUserData(pData) Then
      end else
        FpBaseDB.Connected := False;         // log standard user out

      If Not FpBaseDB.Connected Then begin
        If Assigned(FOnUserLoginError) Then
          FOnUserLoginError(Self);
      end;  //  --  If FpBaseDB.GetUserData(pData) Then
      Dispose(pData);
    end;  //  --  If FpBaseDB.Connected Then
  end;  //  --  If Assigned(FpBaseDB) Then
end;

procedure TmafUserSecurity.SilentConnect(UserName, Password: String);
var pData: PUserDataRec;
begin
  If ((Not Assigned(FpBaseDB)) or (Not (soAllowSilentConnect in FSecurityOptions))) Then
    Exit;

  FpBaseDB.User := FsUserName;
  FpBaseDB.Password := FsPassword;
  FpBaseDB.Connected := True;
  If FpBaseDB.Connected Then begin
    New(pData);
    pData^.ID := -1;
    pData^.Login := UserName;
    If FpBaseDB.QueryUserTable(US_GET_USER_DATA, pData) Then begin
      FpBaseDB.Connected := False;
      If __Decrypt(pData^.Password) = Password Then begin
        FpBaseDB.User := pData^.Login;
        FpBaseDB.Password := __Decrypt(pData^.Password2);
          FpBaseDB.Connected := True;
          If FpBaseDB.Connected Then begin
            If Assigned(FAfterConnect) Then
              FAfterConnect(Self);
            FnGroupID := pData^.GroupID;
          end;
      end;
    end else
      FpBaseDB.Connected := False;
    Dispose(pData);
  end;
  If Not FpBaseDB.Connected Then
    If Assigned(FOnUserLoginError) Then
      FOnUserLoginError(Self);
end;

function TmafUserSecurity.GeneratePassword: String;
var i : Integer;
begin
  Result := '';
  For i := 1 To FnPasswordLength Do
    Result := Result + PWChars[Random(53) + 1];
end;

function TmafUserSecurity.AdministrateUsers(SubHookID: Integer; QHS: pQHS): Integer;
begin
  Case SubHookID Of
    US_CREATE_USER   : Result := CreateUser(QHS^.pChildObj, (QHS^.Reserved1 = 0));
    US_DELETE_USER   : Result := DeleteUser(QHS^.pChildObj);
    US_MODIFY_USER   : Result := UpdateUser(QHS^.pChildObj);
    US_GET_USER_DATA : Result := GetUserData(QHS^.pChildObj);
    else Result := ERR_NO_ERROR;
  end;
end;

function TmafUserSecurity.CreateUser(pData: PUserDataRec; bGenPW: Boolean=True): Integer;
var bCanContinue: Boolean;
begin
  // check, if we have a DB to do user stuff
  If FpBaseDB = nil Then begin
    Result := ERR_COMPONENT_SETUP_FAILURE;
    Exit;
  end;  //  --  If FpBaseDB = nil Then

  Result := ERR_PARAM_FAILURE;
  If pData = nil Then
    Exit;

  If ((pData^.Login = '') Or (pData^.Password = '')) Then
    Exit;


  pData^.Password := __Encrypt(pData^.Password);
  If ((bGenPW) Or (pData^.Password2 = '')) Then
    pData^.Password2 := GeneratePassword;  // PW unencrypted, must be used like that to create the user within the database

  bCanContinue := True;
  If Assigned(FOnUserCreate) Then
    FOnUserCreate(Self, pData, bCanContinue);
  Result := ERR_USER_ACTION;
  If bCanContinue Then begin
    pData^.Password2 := __Encrypt(pData^.Password2);
    If FpBaseDB.QueryUserTable(US_CREATE_USER, pData) Then
      Result := ERR_NO_ERROR;
  end;
end;

function TmafUserSecurity.DeleteUser(pData: PUserDataRec): Integer;
var bCanContinue: Boolean;
begin
  // check, if we have a DB to do user stuff
  If FpBaseDB = nil Then begin
    Result := ERR_COMPONENT_SETUP_FAILURE;
    Exit;
  end;  //  --  If FpBaseDB = nil Then

  Result := ERR_PARAM_FAILURE;
  If pData = nil Then
    Exit;

  bCanContinue := True;
  If Assigned(FOnUserDelete) Then
    FOnUserDelete(Self, pData, bCanContinue);

  If bCanContinue Then
    FpBaseDB.QueryUserTable(US_DELETE_USER, pData);
end;

function TmafUserSecurity.UpdateUser(pData: PUserDataRec): Integer;
var pData2 : PUserDataRec;
begin
  If FpBaseDB = nil Then begin
    Result := ERR_COMPONENT_SETUP_FAILURE;
    Exit;
  end;  //  --  If FpBaseDB = nil Then

  Result := ERR_PARAM_FAILURE;
  If pData = nil Then
    Exit;

  New(pData2);
  FillChar(pData2^, SizeOf(RUserDataRec), 0);
  pData2^.ID := pData^.ID;
  pData2^.Login := pData^.Login;
  FpBaseDB.QueryUserTable(US_GET_USER_DATA, pData2);
  // only if we got a new password
  If pData^.Password <> '' Then begin
    pData^.Password := __Encrypt(pData^.Password);
//    pData2^.Password2 := pData^.Password;
  end else begin
    // we use the old passwords
    pData^.Password := pData2^.Password;
  end;  //  --  If pData^.Password <> '' Then
  pData^.Password2 := pData2^.Password2;  // we copy the encrypted password2
  Dispose(pData2);

  If FpBaseDB.QueryUserTable(US_MODIFY_USER, pData) Then
    Result := ERR_NO_ERROR;
end;

function TmafUserSecurity.AdministrateGroup(SubHookID: Integer; QHS: pQHS): Integer;
begin
  If FpBaseDB = nil Then begin
    Result := ERR_COMPONENT_SETUP_FAILURE;
    Exit;
  end;  //  --  If FpBaseDB = nil Then

  Result := ERR_PARAM_FAILURE;
  If QHS = nil Then
    Exit;

  Result := FpBaseDB.QueryGroupTable(SubHookID, PGroupDataRec(QHS^.pChildObj));
end;

function TmafUserSecurity.GetGroupList(QHS: pQHS): Integer;
begin
  If FpBaseDB = nil Then begin
    Result := ERR_COMPONENT_SETUP_FAILURE;
    Exit;
  end;  //  --  If FpBaseDB = nil Then

  Result := ERR_PARAM_FAILURE;
  If QHS = nil Then
    Exit;

  Result := ERR_NO_ERROR;
  QHS^.pChildObj := TList.Create;
  QHS^.pFreeMemFunc := __Free_GroupList;
  FpBaseDB.ListGroups(TList(QHS^.pChildObj));
end;

function TmafUserSecurity.GetUserList(List: TStringList; GroupID: Integer): Integer;
begin
  If FpBaseDB = nil Then begin
    Result := ERR_COMPONENT_SETUP_FAILURE;
    Exit;
  end;  //  --  If FpBaseDB = nil Then

  Result := ERR_PARAM_FAILURE;
  If List = nil Then
    Exit;

  Result := ERR_NO_ERROR;
  FpBaseDB.ListUsers(List, GroupID);
end;

function TmafUserSecurity.GetUserData(pData: PUserDataRec): Integer;
begin
  If FpBaseDB = nil Then begin
    Result := ERR_COMPONENT_SETUP_FAILURE;
    Exit;
  end;  //  --  If FpBaseDB = nil Then

  Result := ERR_PARAM_FAILURE;
  If pData = nil Then
    Exit;

  If FpBaseDB.QueryUserTable(US_GET_USER_DATA, pData) Then begin
    pData^.Password := __Decrypt(pData^.Password);
    Result := ERR_NO_ERROR;
  end;  //  --  If FpBaseDB.GetUserData(pData) Then
end;

end.

