{*******************************************************************************
Name         : uMAF_FileDB.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2009 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 15.09.2009
Last Update  : 08.10.2009
Version      : 1.0.001
Purpose      :
Last Changes :

1.0.001 (30.09.2009) -----------------------------------------------------------
- [ADD] when creating a new database, a super admin account and group is being
        created automatically, login is "SYSDBA", password "masterkey" belonging
        to a SuperAdmin group, which is created, too
1.0.000 (15.09.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_FileDB;

interface

uses Windows, SysUtils, Classes,
     // Modular Application Framework Components units
     uMAF_Core, uMAF_Globals, uMAF_Tools, uMAF_CustomBaseDB,
     uMAF_ResourceManager_Helper, uMAF_CustomResource, uMAF_TemplateStreamer;

Type TmafFileDBResource = class(TmafCustomResourceFile)
     private
     protected
       procedure __Internal_CreateResource; override;  // we have to set the right ID
     public
       constructor Create; override;
     end;

     TFileDBOption = (dboAutoCommit, dboAutoCreate, dboAutoConnect, dboLogin);
     TFileDBOptions = Set Of TFileDBOption;

     TmafFileDB = class(TmafCustomBaseDB)
     private
       FpFileResource : TmafFileDBResource;
       FOptions : TFileDBOptions;
       FpTemplateNode : PFolderNode;
       FpUserList : TList;  // list of PUserDataRec
       FpGroupList : TList; // list of PGroupData
       FnUser_HighID : Integer;
       FnGroup_HighID : Integer;
       procedure __OnLoaded(Sender: TObject);
       procedure __OnClose(Sender: TObject);
       procedure __OnModified(Sender: TObject);
       function __GetStreamByName(pNode: PFolderNode; AName: String): PFileDescriptor;
//       function __GetStreamByID(pNode: PFolderNode; nID: Cardinal): PFileDescriptor;
       function __LocateStream(StreamName, Path: String; var PathNode: PFolderNode; bCreatePath: Boolean = False): PFileDescriptor;
       function __Create_EmptyDescriptor(AName, Path: String): PFileDescriptor;
       function __LoadSystemTable(ATable: String): PFileDescriptor;
       function __SaveSystemTable(ATable: String; nStreamID: Integer; pDesc: PFileDescriptor = nil): PFileDescriptor;
       // Users
       function __FindUser(LoginName: String): PUserDataRec; overload;
       function __FindUser(nID: Integer): PUserDataRec; overload;
       procedure __FreeUserData;
       procedure __CreateAdminAccount;
       // Groups
       function __FindGroup(GroupName: String): PGroupDataRec; overload;
       function __FindGroup(nID: Integer): PGroupDataRec; overload;
       procedure __FreeGroupData;
     protected
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;
       procedure __ReadStreamData(Sender: TObject; ID: Integer); override;
       procedure __WriteStreamData(Sender: TObject; ID: Integer); override;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;

       function CreateDatabase(AFileName: String): Integer;
       procedure Commit;
       // Users
       function QueryUserTable(nCommand: Integer; pData: PUserDataRec): Boolean; override;
       procedure ListUsers(UserList: TStringList; GroupID : Integer); override;
       // groups
       function QueryGroupTable(nCommand : Integer; pGroup: PGroupDataRec): Integer; override;
       function ListGroups(GroupList: TList): Integer; override;

       // Templates
//       function FindTemplate(nID: Integer; TableName, Category, TemplateName: String): Boolean;
       function ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String; bReadData: Boolean = True): Integer; override;
       function WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer; override;
       property FileResource : TmafFileDBResource read FpFileResource;
     published
       property Connected;
       property DatabaseName;
       property ComputerName;
       property Options : TFileDBOptions read FOptions write FOptions default [dboAutoCommit];
       property User;
       property Password;

       property OnConnect;
       property OnDisconnect;
     end;

implementation

{$IFDEF Tracer}
uses uMAF_Tracer;
{$ENDIF}


const sMAF_UserData   = 'MAF_UserData';     // archive name for user data
      sMAF_GroupData  = 'MAF_GroupData';    // archive name for group data
      sMAF_SystemData = '$SystemData';      // folder name

{ TmafFileDBResource }

constructor TmafFileDBResource.Create;
begin
  inherited;
  ResourceType := rftFileDB;
  StreamID := 923;
end;

procedure TmafFileDBResource.__Internal_CreateResource;
begin
  pHeader^.HeaderID := HEADER_ID_FILEDB_RESOURCE;
  inherited;
end;

{ TmafFileDB }

constructor TmafFileDB.Create(AOwner: TComponent);
begin
  inherited;
  FnUser_HighID := 0;
  FOptions := [dboAutoCommit];
  FpFileResource := TmafFileDBResource.Create;
  Database := TComponent(FpFileResource);  // just to "fool" the GetConnected
  FpFileResource.Owner := Self;
  FpFileResource.OnLoaded := __OnLoaded;
  FpFileResource.OnClosed := __OnClose;
  FpFileResource.OnModified := __OnModified;
  FpUserList := TList.Create;
  FpGroupList := TList.Create;
end;

destructor TmafFileDB.Destroy;
begin
  If Connected Then
    Connected := False;
  FpFileResource.Free;
  __FreeUserData;          // just in case
  FreeAndNil(FpUserList);
  __FreeGroupData;         // also just in case
  FreeAndNil(FpGroupList);
  inherited;
end;

procedure TmafFileDB.Loaded;
begin
  {$IFDEF Tracer}
    MAFTracer.Enter('TmafFileDB.Loaded');
  {$ENDIF}
  inherited;
  If dboAutoConnect in FOptions Then
    __InternalConnect;
  {$IFDEF Tracer}
    MAFTracer.Leave;
  {$ENDIF}
end;

const _Var1 = 73388;
      _Var2 = -68896;
      _Var3 = 3589963;

procedure TmafFileDB.__InternalConnect;
var pUser : PUserDataRec;
begin
  If dboLogin in FOptions Then
    If ((User = '') or (Password = '')) Then begin
      Connected := False;
      Exit;
    end;

  If FileExists(DatabaseName) Then
    FpFileResource.ResourceFile := DatabaseName
  Else
    If dboAutoCreate in FOptions Then
      CreateDatabase(DatabaseName);

  If Not FpFileResource.Loaded Then begin
    Connected := False;
    Exit;
  end;

  If dboAutoConnect in FOptions Then begin
    If dboLogin in FOptions Then begin
      New(pUser);
      FilLChar(pUser^, SizeOf(RUserDataRec), 0);
      pUser^.Login := User;
      If QueryUserTable(US_GET_USER_DATA, pUser) Then begin
        If Decrypt(pUser^.Password2, _Var1, _Var2, _Var3) <> Password Then begin
          Connected := False;
          Exit;
        end else
          FbConnected := True;
      end;
    end Else begin
      FbConnected := FpFileResource.Loaded;
    end;
  end else
    FbConnected := FpFileResource.Loaded;


  If FpFileResource.Loaded Then begin

  end;
  inherited;
end;

procedure TmafFileDB.__InternalDisconnect;
begin
  inherited;
  If FpFileResource.Modified Then
    FpFileResource.Save;
  FpFileResource.ResourceFile := '';
end;

procedure TmafFileDB.Commit;
begin
  FpFileResource.Save;
end;

procedure TmafFileDB.__OnClose(Sender: TObject);
begin
  If Assigned(OnDisconnect) Then
    OnDisconnect(Self);
  FpTemplateNode := nil;
end;

procedure TmafFileDB.__OnLoaded(Sender: TObject);
begin
  If DataStorageTable <> '' Then
    FpTemplateNode := FpFileResource.__GetFolder(DataStorageTable, nil, True)
  Else
    FpTemplateNode := FpFileResource.__GetFolder(sDefaultTemplateTable, nil, True);
  If Assigned(OnConnect) Then
    OnConnect(Self);
end;

procedure TmafFileDB.__OnModified(Sender: TObject);
begin
//  If dboAutoCommit in FOptions Then
//    FpFileResource.Save;
end;

procedure TmafFileDB.__ReadStreamData(Sender: TObject; ID: Integer);
var i, nCount : Integer;
    pData : PUserDataRec;
    pGroup : PGroupDataRec;
    dwFlags : Byte;
begin
  Case FpStreamer.StreamID Of
    889 : begin
            __FreeUserData;
            FpStreamer.ReadInteger(nCount);
            For i := 1 To nCount Do begin
              New(pData);
              FpStreamer.ReadInteger(pData^.ID);
              FpStreamer.ReadInteger(pData^.GroupID);
              FpStreamer.ReadString(pData^.Login);
              FpStreamer.ReadString(pData^.FirstName);
              FpStreamer.ReadString(pData^.Lastname);
              FpStreamer.ReadString(pData^.Password);
              FpStreamer.ReadString(pData^.Password2);
              FpStreamer.ReadInteger(pData^.Flags);
              FpUserList.Add(pData);
              If FnUser_HighID < pData^.ID Then
                FnUser_HighID := pData^.ID;
            end;
          end;
    890 : begin
            __FreeGroupData;
            FpStreamer.ReadInteger(nCount);
            For i := 1 To nCount Do begin
              New(pGroup);
              FpStreamer.ReadInteger(pGroup^.GroupID);
              FpStreamer.ReadString(pGroup^.GroupName);
              FpStreamer.ReadByte(pGroup^.SL);
              FpStreamer.ReadByte(dwFlags);
              pGroup^.Flags := [];
              If dwFlags And 1=1 Then
                Include(pGroup^.Flags, gfVisibleToLowerSL);
              FpGroupList.Add(pGroup);
              If FnGroup_HighID < pGroup^.GroupID Then
                FnGroup_HighID := pGroup^.GroupID;
            end;
          end;
  end;
end;

procedure TmafFileDB.__WriteStreamData(Sender: TObject; ID: Integer);
var i : Integer;
    pData : PUserDataRec;
    pGroup : PGroupDataRec;
    dwFlags : Byte;
begin
  Case FpStreamer.StreamID Of
    889 : begin
            FpStreamer.WriteInteger(FpUserList.Count);
            For i := 0 To FpUserList.Count - 1 Do begin
              pData := PUserDataRec(FpUserList.Items[i]);
              FpStreamer.WriteInteger(pData^.ID);
              FpStreamer.WriteInteger(pData^.GroupID);
              FpStreamer.WriteString(pData^.Login);
              FpStreamer.WriteString(pData^.FirstName);
              FpStreamer.WriteString(pData^.Lastname);
              FpStreamer.WriteString(pData^.Password);
              FpStreamer.WriteString(pData^.Password2);
              FpStreamer.WriteInteger(pData^.Flags);
            end;
          end;
    890 : begin
            FpStreamer.WriteInteger(FpGroupList.Count);
            For i := 0 To FpGroupList.Count - 1 Do begin
              pGroup := PGroupDataRec(FpGroupList.Items[i]);
              FpStreamer.WriteInteger(pGroup^.GroupID);
              FpStreamer.WriteString(pGroup^.GroupName);
              FpStreamer.WriteByte(pGroup^.SL);
              dwFlags := 0;
              If gfVisibleToLowerSL in pGroup^.Flags Then
                dwFlags := dwFlags + 1;
              FpStreamer.WriteByte(dwFlags);
            end;
          end;
  end;
end;

function TmafFileDB.CreateDatabase(AFileName: String): Integer;
begin
  Result := ERR_UNKNOWN_ERROR;
  FpFileResource.ResourceType := rftFileDB;
  FpFileResource.CreateResource(AFileName);
  __CreateAdminAccount;
  If FpFileResource.Loaded Then
    Result := ERR_NO_ERROR;
end;

{function TmafFileDB.__GetStreamByID(pNode: PFolderNode; nID: Cardinal): PFileDescriptor;
var AList : TList;
    i : Integer;
begin
  Result := nil;
  If pNode = nil Then AList := FpFileResource.Root
                 Else AList := pNode^.FpDataList;
  For i := 0 To AList.Count - 1 Do
    If PFileDescriptor(AList.Items[i])^.ID = nID Then begin
      Result := PFileDescriptor(AList.Items[i]);
      Break;
    end;
end;    }

function TmafFileDB.__GetStreamByName(pNode: PFolderNode; AName: String): PFileDescriptor;
var AList : TList;
    i : Integer;
begin
  Result := nil;
  If pNode = nil Then AList := FpFileResource.Root
                 Else AList := pNode^.FpDataList;
  For i := 0 To AList.Count - 1 Do
    If PFileDescriptor(AList.Items[i])^.ResName = AName Then begin
      Result := PFileDescriptor(AList.Items[i]);
      Break;
    end;
end;

function TmafFileDB.ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String; bReadData: Boolean): Integer;
var aNode : PFolderNode;
    aDesc : PFileDescriptor;
begin
  {$IFDEF Tracer}
    MAFTracer.Enter('TmafFileDB.ReadTemplate');
    MAFTracer.CheckPoint('TableName = ' + TableName);
    MAFTracer.CheckPoint('Category = ' + Category);
    MAFTracer.CheckPoint('TemplateName = ' + TemplateName);
  {$ENDIF}
  If aStream = nil Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;
  {$IFDEF Tracer}
    MAFTracer.CheckPoint('aStream = assigned');
  {$ENDIF}

  aNode := nil;
  Result := ERR_TEMPLATE_READ_ERROR;
  If nID > 0 Then begin   // if we have an ID, we go for it
  {$IFDEF Tracer}
    MAFTracer.CheckPoint('locating stream by ID');
  {$ENDIF}
    aDesc := PFileDescriptor(FpFileResource.GetDescriptor(Cardinal(nID)));
    aNode := FpFileResource.__GetFolder(DataStorageTable + '\' + Category, aNode, True);
  end else begin
  {$IFDEF Tracer}
    MAFTracer.CheckPoint('locating stream by name');
  {$ENDIF}
    aDesc := __LocateStream(TemplateName, DataStorageTable + '\' + Category, aNode, True);
  {$IFDEF Tracer}
    If Assigned(aDesc) Then
      MAFTracer.CheckPoint('Stream descriptor found')
    Else
      MAFTracer.CheckPoint('Stream descriptor not found !!!');
  {$ENDIF}
  end;
  {$IFDEF Tracer}
    If aNode <> nil Then
      MAFTracer.CheckPoint('FolderNode found : ' + aNode^.NodeName)
    Else
      MAFTracer.CheckPoint('FolderNode not found');
  {$ENDIF}

  If aNode = nil Then {$IFDEF Tracer} begin
    MAFTracer.Leave;
  {$ENDIF}
    Exit;
  {$IFDEF Tracer} end; {$ENDIF}

  Result := ERR_NO_ERROR;   // it is NO ERROR if a template doesn't exist
  If aDesc = nil Then {$IFDEF Tracer} begin
    MAFTracer.Leave;
  {$ENDIF}
    Exit;
  {$IFDEF Tracer} end; {$ENDIF}

  If bReadData Then
    If FpFileResource.LoadResource(aDesc) = ERR_NO_ERROR Then begin
      aStream.LoadFromStream(aDesc^.aStream);
      FpFileResource.Release(aDesc^.ID);      // we always release the stream
    end;
  aNode := FpFileResource.__GetFolder(aDesc^.FolderNode, nil);
  Category := aNode^.NodeName;
  TemplateName := aDesc^.ResName;
  nID := aDesc^.ID;
  {$IFDEF Tracer}
    MAFTracer.Log_Integer('data stream size', aStream.Size);
    MAFTracer.Leave;
  {$ENDIF}
end;

function TmafFileDB.WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer;
var aNode : PFolderNode;
    aDesc : PFileDescriptor;
    bChanged : Boolean;
begin
  If aStream = nil Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;

  aNode := nil;

  Result := ERR_TEMPLATE_WRITE_ERROR;
  If nID > 0 Then begin   // if we have an ID, we go for it
    aDesc := PFileDescriptor(FpFileResource.GetDescriptor(Cardinal(nID)));
    aNode := FpFileResource.__GetFolder(DataStorageTable + '\' + Category, aNode, True);
  end else begin
    aDesc := __LocateStream(TemplateName, DataStorageTable + '\' + Category, aNode, True);
  end;

  If aDesc = nil Then begin    // we create a new template
    If aStream.Size = 0 Then
      Exit;

    aDesc := __Create_FileDescriptor;
    aDesc^.aStream := TMemoryStream.Create;
    aStream.Position := 0;
    aDesc^.aStream.LoadFromStream(aStream);
    aDesc^.aStream.Position := 0;
    aDesc^.nLength := aDesc^.aStream.Size;
    aDesc^.ResName := TemplateName;
    aDesc^.FolderNode := aNode^.nID;
    aDesc := PFileDescriptor(FpFileResource.Add(PBaseDescriptor(aDesc)));
    bChanged := True;
    Result := ERR_NO_ERROR;
  end else begin               // we update a template
    If aStream.Size = 0 Then begin
      FpFileResource.Delete(aDesc^.ID);
      Exit;
    end;
    aDesc := FpFileResource.Update(aDesc, aStream);
    bChanged := True;
    Result := ERR_NO_ERROR;
  end;
  If bChanged Then begin
    If dboAutoCommit in FOptions Then begin
      FpFileResource.Save;
      FpFileResource.Release(aDesc^.ID);  // only release the new entry, when committed
    end;
  end;
  aNode := FpFileResource.__GetFolder(aDesc^.FolderNode, nil);
  Category := aNode^.NodeName;
  TemplateName := aDesc^.ResName;
  nID := aDesc^.ID;
end;

function TmafFileDB.__LocateStream(StreamName, Path: String; var PathNode: PFolderNode; bCreatePath: Boolean): PFileDescriptor;
begin
  Result := nil;
  PathNode := FpFileResource.__GetFolder(Path, nil, bCreatePath);
  If PathNode = nil Then
    Exit;
  Result := __GetStreamByName(PathNode, StreamName);
end;

procedure TmafFileDB.__CreateAdminAccount;
var pData : PUserDataRec;
    pGroup : PGroupDataRec;
begin
  New(pData);
  pData^.ID := 1;
  pData^.GroupID := 1;
  pData^.Login := 'SYSDBA';
  pData^.FirstName := 'Super';
  pData^.LastName := 'Admin';
  pData^.Flags := nUserBaseFlag;
  pData^.Password := Encrypt('masterkey', _Var1, _Var2, _Var3);
  pData^.Password2 := Encrypt('masterkey', _Var1, _Var2, _Var3);
  pData^.SL := 10;
  QueryUserTable(US_CREATE_USER, pData);
  Dispose(pData);
  New(pGroup);
  pGroup^.GroupID := 1;
  pGroup^.GroupName := 'SuperAdmin';
  pGroup^.Flags := [];
  pGroup^.SL := 10;
  QueryGroupTable(US_CREATE_GROUP, pGroup);
  Dispose(pGroup);
end;

function TmafFileDB.__Create_EmptyDescriptor(AName, Path: String): PFileDescriptor;
var aNode : PFolderNode;
begin
  Result := nil;
  aNode := FpFileResource.__GetFolder(Path, nil, True);
  If aNode = nil Then
    Exit;
  Result := __Create_FileDescriptor;
  Result^.ResName := AName;
  Result^.aStream := TMemoryStream.Create;
  Result^.FolderNode := aNode^.nID;
end;

function TmafFileDB.__FindUser(LoginName: String): PUserDataRec;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpUserList.Count - 1 Do
    If PUserDataRec(FpUserList.Items[i])^.Login = LoginName Then begin
      Result := PUserDataRec(FpUserList.Items[i]);
      Break;
    end;
end;

function TmafFileDB.__FindUser(nID: Integer): PUserDataRec;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpUserList.Count - 1 Do
    If PUserDataRec(FpUserList.Items[i])^.ID = nID Then begin
      Result := PUserDataRec(FpUserList.Items[i]);
      Break;
    end;
end;

procedure TmafFileDB.__FreeUserData;
var i : Integer;
begin
  For i := 0 To FpUserList.Count - 1 Do
    Dispose(PUserDataRec(FpUserList.Items[i]));
  FpUserList.Clear;
end;

function TmafFileDB.__LoadSystemTable(ATable: String): PFileDescriptor;
var pNode : PFolderNode;
begin
  Result := __LocateStream(ATable, sMAF_SystemData, pNode);
  If Result <> nil Then begin
    FpFileResource.LoadResource(Result);
    FpStreamer.ReadStream(Result^.aStream);
    FpFileResource.Release(Result^.ID);    // we don't keep the stream in memory, we delete it eventually
  end;
end;

function TmafFileDB.__SaveSystemTable(ATable: String; nStreamID: Integer; pDesc: PFileDescriptor = nil): PFileDescriptor;
var aDesc : PFileDescriptor;
begin
  Result := nil;
  aDesc := __Create_EmptyDescriptor(ATable, sMAF_SystemData);  // create a temporary descriptor
  FpStreamer.WriteStream(nStreamID, aDesc^.aStream);                       // write all users in a new stream
  aDesc^.nLength := aDesc^.aStream.Size;

  If pDesc = nil Then begin
//    pDesc := __LocateStream('ERP_UserData', sERP_SystemData, pNode);
    pDesc := PFileDescriptor(FpFileResource.Add(PBaseDescriptor(aDesc))); // add will copy the contents and return the new descriptor
  end else
    pDesc := FpFileResource.Update(pDesc, aDesc^.aStream);
  __Free_FileDescriptor(aDesc);                                      // and we throw it away again
  If dboAutoCommit in FOptions Then begin
    FpFileResource.Save;
    FpFileResource.Release(pDesc^.ID);
  end;
  __FreeUserData;
  __FreeGroupData;
end;

procedure TmafFileDB.ListUsers(UserList: TStringList; GroupID: Integer);
var i : Integer;
    bCanAdd : Boolean;
begin
  __LoadSystemTable(sMAF_UserData);
  For i := 0 To FpUserList.Count - 1 Do begin
    bCanAdd := True;
    If GroupID > 0 Then
      bCanAdd := (PUserDataRec(FpUserList.Items[i])^.GroupID = GroupID);
    If bCanAdd Then
      UserList.AddObject(PUserDataRec(FpUserList.Items[i])^.Login, Pointer(PUserDataRec(FpUserList.Items[i])^.ID));
  end;
  __FreeUserData;
end;

function TmafFileDB.QueryUserTable(nCommand: Integer; pData: PUserDataRec): Boolean;
var pUser : PUserDataRec;
    pDesc : PFileDescriptor;
begin
  Result := False;
  pDesc := __LoadSystemTable(sMAF_UserData);
  If pData^.ID < 1 Then pUser := __FindUser(pData^.Login)
                   Else pUser := __FindUser(pData^.ID);
  Case nCommand Of
    US_CREATE_USER   : begin
                         If pUser = nil Then begin
                           New(pUser);
                           pUser^.ID := FnUser_HighID + 1;
                           Inc(FnUser_HighID);
                           pUser^.GroupID := pData^.GroupID;
                           pUser^.Login := pData^.Login;
                           pUser^.FirstName := pData^.FirstName;
                           pUser^.LastName := pData^.LastName;
                           pUser^.Password := pData^.Password;
                           pUser^.Password2 := pData^.Password2;
                           pUser^.Flags := nUserBaseFlag;
                           FpUserList.Add(pUser);
                           Result := True;
                         end;
                       end;
    US_DELETE_USER   : begin
                         If pUser <> nil Then begin
                           FpUserList.Delete(FpUserList.IndexOf(pUser));
                           Dispose(pUser);
                           Result := True;
                         end;
                       end;
    US_MODIFY_USER   : begin
                         If pUser <> nil Then begin
                           pUser^.GroupID := pData^.GroupID;
                           pUser^.FirstName := pData^.FirstName;
                           pUser^.LastName := pData^.LastName;
                           pUser^.Password := pData^.Password;
                           pUser^.Flags := nUserBaseFlag;
                           Result := True;
                         end;
                       end;
    US_GET_USER_DATA : begin
                         If pUser <> nil Then begin
                           pData^.ID := pUser^.ID;
                           pData^.GroupID := pUser^.GroupID;
                           pData^.Login := pUser^.Login;
                           pData^.FirstName := pUser^.FirstName;
                           pData^.LastName := pUser^.LastName;
                           pData^.Password := pUser^.Password;
                           pData^.Password2 := pUser^.Password2;
                           pData^.Flags := pUser^.Flags;
                           Result := True;
                         end;
                       end;
  end;
  If ((Result) And (nCommand <> US_GET_USER_DATA)) Then begin
    pDesc := __SaveSystemTable(sMAF_UserData, 889, pDesc);
    Result := (pDesc <> nil);
  end;
end;

function TmafFileDB.__FindGroup(GroupName: String): PGroupDataRec;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpGroupList.Count - 1 Do
    If PGroupDataRec(FpGroupList.Items[i])^.GroupName = GroupName Then begin
      Result := PGroupDataRec(FpGroupList.Items[i]);
      Break;
    end;
end;

function TmafFileDB.__FindGroup(nID: Integer): PGroupDataRec;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpGroupList.Count - 1 Do
    If PGroupDataRec(FpGroupList.Items[i])^.GroupID = nID Then begin
      Result := PGroupDataRec(FpGroupList.Items[i]);
      Break;
    end;
end;

procedure TmafFileDB.__FreeGroupData;
var i : Integer;
begin
  For i := 0 To FpGroupList.Count - 1 Do
    Dispose(PGroupDataRec(FpGroupList.Items[i]));
  FpGroupList.Clear;
end;

function TmafFileDB.ListGroups(GroupList: TList): Integer;
var pDesc : PFileDescriptor;
    i : Integer;
    pGroup : PGroupDataRec;
begin
  Result := ERR_DS_NOT_FOUND;
  pDesc := __LoadSystemTable(sMAF_GroupData);
  If Assigned(pDesc) Then
    Result := ERR_NO_ERROR;
  For i := 0 To FpGroupList.Count - 1 Do begin
    New(pGroup);
    pGroup^.GroupID := PGroupDataRec(FpGroupList.Items[i])^.GroupID;
    pGroup^.GroupName := PGroupDataRec(FpGroupList.Items[i])^.GroupName;
    pGroup^.SL := PGroupDataRec(FpGroupList.Items[i])^.SL;
    pGroup^.Flags := PGroupDataRec(FpGroupList.Items[i])^.Flags;
    GroupList.Add(pGroup);
  end;
  __FreeGroupData;
end;

function TmafFileDB.QueryGroupTable(nCommand : Integer; pGroup: PGroupDataRec): Integer;
var pDesc : PFileDescriptor;
    pGroup2 : PGroupDataRec;
    bChanged : Boolean;
begin
  Result := ERR_PARAM_FAILURE;
  If pGroup = nil Then
    Exit;

  Result := ERR_NO_ERROR;
  pDesc := __LoadSystemTable(sMAF_GroupData);
  If pGroup^.GroupID < 1 Then pGroup2 := __FindGroup(pGroup^.GroupName)
                         Else pGroup2 := __FindGroup(pGroup^.GroupID);
  bChanged := False;
  Case nCommand Of
    US_CREATE_GROUP : begin
                        If pGroup2 = nil Then begin
                          New(pGroup2);
                          pGroup2^.GroupID := FnGroup_HighID + 1;
                          Inc(FnGroup_HighID);
                          pGroup2^.GroupName := pGroup^.GroupName;
                          pGroup2^.Flags := pGroup^.Flags;
                          pGroup2^.SL := pGroup^.SL;
                          FpGroupList.Add(pGroup2);
                          bChanged := True;
                        end;
                      end;
    US_UPDATE_GROUP : begin
                        If pGroup2 <> nil Then begin
                          pGroup2^.GroupName := pGroup^.GroupName;
                          pGroup2^.Flags := pGroup^.Flags;
                          pGroup2^.SL := pGroup^.SL;
                          bChanged := True;
                        end;
                      end;
    US_DELETE_GROUP : begin
                        If pGroup2 <> nil Then begin
                          FpGroupList.Delete(FpGroupList.IndexOf(pGroup2));
                          Dispose(pGroup2);
                          bChanged := True;
                        end;
                      end;
    US_GET_GROUP_DATA: begin
                         If pGroup <> nil Then begin
                           pGroup^.GroupID := pGroup2^.GroupID;
                           pGroup^.GroupName := pGroup2^.GroupName;
                           pGroup^.Flags := pGroup2^.Flags;
                           pGroup^.SL := pGroup2^.SL;
                         end;
                       end;
  end;

  If bChanged Then begin
    pDesc := __SaveSystemTable(sMAF_GroupData, 890, pDesc);
    If Not Assigned(pDesc) Then
      Result := ERR_UNKNOWN_ERROR;
  end;
end;

end.
