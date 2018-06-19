{*******************************************************************************
Name         : uSDACBaseDB.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007-2009 by Helge Lange
Info         : HelgeLange@gmail.com
Website      : http://www.maf-components.com
Date         : 11.06.2007
Last Update  : 14.11.2009
Version      : 1.0.004
Purpose      : DevArt SDAC support package
               SDAC can be found under http://www.devart.com/sdac/
Last Changes :

1.0.004 (14.11.2009) -----------------------------------------------------------
- [CHG] changed to work with the new table definitions and tested with SDAC 4.80
- [CHG] User and group function (except Listing) are now protected as they are
        in other BaseDB components
- [CHG] TSDACBaseDB inherits now from TmafBaseDB
1.0.003 (01.10.2009) -----------------------------------------------------------
- [CHG] changed code to work with the new and smaller interface from the
        CustomeBaseDB
- [ADD] added group support
1.0.002 (29.10.2008) -----------------------------------------------------------
- [FIX] EnumDynamicFunction now reads, if a Hook is activated or not.
- [CHG] EnumDynamicFunction now reads in DesignTime ALL Hooks (activated or not)
        and applies the b_Active flag only in runtime. This additional code can
        be removed for release modules with the compiler Switch "RELEASE"
- [ADD] EnableDynamicFuntion to enable/disable a dynamic function from the outside
1.0.001 (11.06.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uSDACBaseDB;

interface

uses Windows, SysUtils, Classes, DB,
     // CrLab SDAC units
     DBAccess, MSAccess, MemDS,
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_Tools, uMAF_CustomBaseDB, uMAF_HookManager_Helper,
     uMAF_BaseDB;

Type TSDACBaseDB = class(TmafBaseDB)
     private
       FsServer : String;
     protected
       function __GetConnected: Boolean; override;
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;
       function RequestQuery(bWrite: Boolean = False): Integer; override;
       function RequestSQL(nID: Integer): String; override;

       procedure PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName : String);
       // Users
       function CreateUser(pData: PUserDataRec) : Boolean; override;
       function DeleteUser(pData: PUserDataRec): Boolean; override;
       function UpdateUser(pData: PUserDataRec) : Boolean; override;
       function GetUserData(pData: PUserDataRec): Boolean; override;
       // Groups
       function CreateGroup(pGroup: PGroupDataRec): Integer; override;
       function UpdateGroup(pGroup: PGroupDataRec): Integer; override;
       function DeleteGroup(pGroup: PGroupDataRec): Integer; override;
       function GetGroupData(pGroup: PGroupDataRec): Integer; override;
     public
       function CheckRequirements: Boolean; override;
       function CreateDataSet: TComponent; override;
       function GetDatabaseList(aList: TStrings): Integer; override;

       // Templates
       function ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String; bReadData: Boolean = True): Integer; override;
       function WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer; override;
       // Users
       procedure ListUsers(UserList: TStringList; GroupID : Integer); override;
       // Groups
       function ListGroups(GroupList: TList): Integer; override;
     published
       property Server : String read FsServer write FsServer;
       property Connected;
       property ComputerName;
       property DataBaseName;
       property User;
       property Password;
       property DataBase;
       // events
       property OnConnect;
       property OnDisconnect;
       property OnCreateQuery;
      // property OnSpecialSQL;
     end;

procedure Register;

implementation

uses uMAF_HookManager, uBaseSQL_Consts_MS_SQLServer,
     MemData, Dialogs, OLEDBAccess;

procedure Register;
begin
  RegisterComponents('MAF Database', [TSDACBaseDB]);
end;

{ TSDACBaseDB }

function TSDACBaseDB.__GetConnected: Boolean;
begin
  Result := False;
  If Assigned(Database) Then
    Result := TMSConnection(Database).Connected;
end;

procedure TSDACBaseDB.__InternalConnect;
begin
  If Assigned(Database) Then begin
    TMSConnection(Database).Connected := False;
    if TMSConnection(Database).Authentication <> auWindows Then begin
      TMSConnection(Database).Username := User;
      TMSConnection(Database).Password := Password;
    end;
    TMSConnection(Database).Server := FsServer;
    TMSConnection(Database).Database := DataBaseName;

    TMSConnection(Database).Connected := True;
    If Not TMSConnection(Database).Connected Then
      Raise TmafBaseDBError.Create('Failed to connect to database !');
  end;  //  --  If Assigned(Database) Then
  inherited;
end;

procedure TSDACBaseDB.__InternalDisconnect;
begin
  inherited;
  If TMSConnection(Database).Connected Then begin
    If TMSConnection(Database).InTransaction Then
      TMSConnection(Database).Commit;
    TMSConnection(Database).Connected := False;
  end;  //  --  If TSDDatabase(Database).Connected Then
end;

function TSDACBaseDB.CheckRequirements: Boolean;
begin
  Result := Assigned(Database);  // no inherited here, because we don't need an transaction
end;

function TSDACBaseDB.RequestQuery(bWrite: Boolean): Integer;
var FQ: TMSQuery;
begin
  Result := -1;
  If Not CheckRequirements Then
    Exit;
  FQ := TMSQuery.Create(nil);
  FQ.Tag := __GetQueryID(QueryList, COMPONENT_BASENAME_QUERY);
  FQ.Name := COMPONENT_BASENAME_QUERY + IntToStr(FQ.Tag);
  FQ.Connection := TMSConnection(DataBase);
  FQ.Options.AutoPrepare := True;
  FQ.ParamCheck := True;
  Result := QueryList.Add(FQ);
  inherited RequestQuery;
end; // RequestQuery

function TSDACBaseDB.CreateDataSet: TComponent;
var SDTable : TMSTable;
begin
  SDTable := TMSTable.Create(nil);
  SDTable.Tag := DataSetID;
  SDTable.Name := COMPONENT_BASENAME_DATASET + IntToStr(SDTable.Tag);
  SDTable.Connection := TMSConnection(DataBase);
  Result := SDTable;
end;

function TSDACBaseDB.GetDatabaseList(aList: TStrings): Integer;
begin
  If Database <> nil Then begin
    TMSConnection(Database).GetDatabaseNames(aList);
    Result := ERR_NO_ERROR;
  end else
    Result := ERR_NO_DB;
end;

procedure TSDACBaseDB.PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName: String);
var Query : TMSQuery;
begin
  Query := TMSQuery(QueryList.Items[nQueryID]);
  If nID > 0 Then begin
    Query.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_SELECT_ID); //InternalSQL.TemplateSQL.SQL_SELECT;
    Query.ParamByName('DataID').ParamType := ptInput;
    Query.ParamByName('DataID').AsInteger := nID;
    Query.Prepare;
  end else
    If ((sCat <> '') AND (sName <> '')) Then begin
      Query.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_SELECT_NAME);
      If Query.SQL.Text <> '' Then begin
        Query.ParamByName('Data_Category').ParamType := ptInput;
        Query.ParamByName('Data_Category').AsString := sCat;
        Query.ParamByName('Data_Name').ParamType := ptInput;
        Query.ParamByName('Data_Name').AsString := sName;
        Query.Prepare;
      end;  //  --  If Query.SQL.Text <> '' Then
    end;  //  --  If ((Category <> '') AND (TemplateName <> '')) Then

  // check again, if we have a SQL query as we should by now
  If Query.SQL.Text = '' Then begin
    FreeQuery(nQueryID);          // releasing the query
    Exit;                         // and we're gone :P
  end;  //  --  If Query.SQL.Text = '' Then
end;

function TSDACBaseDB.ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String; bReadData: Boolean): Integer;
var nQueryID : Integer;
    Query : TMSQuery;
begin
  Result := inherited ReadTemplate(aStream, nID, TableName, Category, TemplateName);
  If Result <> ERR_NO_ERROR Then
    Exit;
  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TMSQuery(QueryList.Items[nQueryID]);
    PrepareTemplateQuery(nQueryID, nID, TableName, Category, TemplateName);
    If nQueryID = -1 Then begin
      Result := ERR_PARAM_FAILURE;
      Exit;
    end;  //  --  If nQueryID = -1 Then

    Query.Active := True;
    Query.First;
    If Not Query.EoF Then begin
      // filling all 3
      nID := Query.Fields[0].AsInteger;
      Category := Query.Fields[1].AsString;
      TemplateName := Query.Fields[2].AsString;
      // reading the blob
      aStream.Position := 0;
      TBlobField(Query.Fields[3]).SaveToStream(aStream);
      aStream.Position := 0;
      Result := ERR_NO_ERROR; // everything ok here
    end;  //  --  If Not Query.EoF Then
    FreeQuery(nQueryID); // free the query
  end;  //  --  If nQueryID > -1 Then
end;

function TSDACBaseDB.WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer;
var nQueryID : Integer;
    Query : TMSQuery;
begin
  Result := inherited WriteTemplate(aStream, nID, TableName, Category, TemplateName);
  If Result <> ERR_NO_ERROR Then
    Exit;

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TMSQuery(QueryList.Items[nQueryID]);
    PrepareTemplateQuery(nQueryID, nID, TableName, Category, TemplateName);
    If nQueryID = -1 Then begin
      Result := ERR_PARAM_FAILURE;
      Exit;
    end;  //  --  If nQueryID = -1 Then

    Query.Active := True;
    Query.First;
    If Not Query.EoF Then begin
      // update mode
      nID := Query.Fields[0].AsInteger; // we need the ID in any case
      Category := Query.Fields[1].AsString; // return for users convenience
      TemplateName := Query.Fields[2].AsString;
      Query.Close;
      Query.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_UPDATE); //InternalSQL.TemplateSQL.SQL_UPDATE;
      Query.ParamByName('DataID').ParamType := ptInput;
      Query.ParamByName('DataID').AsInteger := nID;
    end else begin
      // create new template
      Query.Close;
      nID := -1; // we don't know the ID yet
      Query.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_INSERT); //InternalSQL.TemplateSQL.SQL_INSERT;
      Query.ParamByName('Data_Category').ParamType := ptInput;
      Query.ParamByName('Data_Category').AsString := Category;
      Query.ParamByName('Data_Name').ParamType := ptInput;
      Query.ParamByName('Data_Name').AsString := TemplateName;
    end;
    aStream.Position := 0;
    Query.ParamByName('Data_Storage').ParamType := ptInput;
    Query.ParamByName('Data_Storage').DataType := ftMemo;
    Query.ParamByName('Data_Storage').LoadFromStream(aStream, ftMemo);
    Query.Prepare;
    Query.Execute;
    If nID = -1 Then begin
      // we inserted a new template, we return the ID
      PrepareTemplateQuery(nQueryID, nID, TableName, Category, TemplateName); // will cause to prepare a query through Category/TemplateName
      Query.Active := True;
      Query.First;
      If Not Query.EoF Then
        nID := Query.Fields[0].AsInteger;
    end;
    FreeQuery(nQueryID);
  end;  //  --  If nQueryID > -1 Then
end;

function TSDACBaseDB.CreateUser(pData: PUserDataRec) : Boolean;
var nQueryID : Integer;
    Query : TMSQuery;
    aBlob : TBlob;
begin
  Result := False;
  If pData = nil Then
    Exit;

  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TMSQuery(QueryList.Items[nQueryID]);
//    CREATE LOGIN WanidaBenshoof
//    WITH PASSWORD = '8fdKJl3$nlNv3049jsKK';
//USE AdventureWorks;
//CREATE USER Wanida FOR LOGIN WanidaBenshoof
//    WITH DEFAULT_SCHEMA = Marketing;
{    Query.SQL.Clear;
    Query.SQL.Add('CREATE LOGIN '''+pData^.Login+''' WITH PASSWORD = '''+pData^.Password2+'''');
    Query.SQL.Add('USE '+ TMSConnection(Database).Database + ';');
    Query.SQL.Add('CREATE USER '+pData^.Login+' FOR LOGIN '+pData^.Login+';');
    Query.Execute;
    Query.SQL.Text := 'exec sp_grantdbaccess '''+pData^.Login+'''';
    Query.Execute; }

    Query.SQL.Text := RequestSQL(SQL_ID_USERS_INSERT);
    Query.Params.Clear;
    Query.Params.CreateParam(ftString, 'Login', ptInput);
    Query.Params.CreateParam(ftInteger, 'GroupID', ptInput);
    Query.Params.CreateParam(ftBlob, 'Account_Data', ptInput);
    Query.Prepare;

    Query.ParamByName('Login').AsString := pData^.Login;
    Query.ParamByName('GroupID').AsInteger := pData^.GroupID;

    FpStreamer.Data := pData;
    FpStreamer.WriteStream(6558);

    aBlob := Query.ParamByName('Account_Data').AsBlobRef;
    aBlob.Clear;
    aBlob.LoadFromStream(FpStreamer.Stream);
    aBlob.Commit;

    Query.Execute;

    FreeQuery(nQueryID);
    Result := True;
  end;  //  --  If nQueryID > -1 Then
end;

function TSDACBaseDB.UpdateUser(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    Query : TMSQuery;
    aBlob : TBlob;
begin
  Result := False;
  If pData = nil Then
    Exit;

  If pData^.ID = 0 Then
    Exit;

  nQueryID := RequestQuery(True);
  Query := TMSQuery(QueryList.Items[nQueryID]);
  Query.SQL.Text := RequestSQL(SQL_ID_USERS_UPDATE);
  Query.Prepare;
  Query.ParamByName('ID').ParamType := ptInput;
  Query.ParamByName('ID').AsInteger := pData^.ID;
  Query.ParamByName('GroupID').ParamType := ptInput;
  Query.ParamByName('GroupID').AsInteger := pData^.GroupID;

  FpStreamer.Data := pData;
  FpStreamer.WriteStream(6558);

  aBlob := Query.ParamByName('Account_Data').AsBlobRef;
  aBlob.Clear;
  aBlob.LoadFromStream(FpStreamer.Stream);
  aBlob.Commit;

{  Query.ParamByName('FirstName').ParamType := ptInput;
  Query.ParamByName('FirstName').AsString := pData^.FirstName;
  Query.ParamByName('LastName').ParamType := ptInput;
  Query.ParamByName('LastName').AsString := pData^.LastName;
  Query.ParamByName('Password').ParamType := ptInput;
  Query.ParamByName('Password').AsString := pData^.Password;
    Query.ParamByName('Flags').AsInteger := nUserBaseFlag; }

  Query.Execute;

  FreeQuery(nQueryID);
  Result := True;
end;

function TSDACBaseDB.DeleteUser(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    Query : TMSQuery;
begin
  Result := False;
  If pData = nil Then
    Exit;

  If Not GetUserData(pData) Then
    Exit;

  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TMSQuery(QueryList.Items[nQueryID]);

    Query.SQL.Text := RequestSQL(SQL_ID_USERS_DELETE);
    Query.Prepare;
    Query.ParamByName('ID').ParamType := ptInput;
    Query.ParamByName('ID').AsInteger := pData^.ID;
    Query.Execute;

{    Query.SQL.Add('USE '+ TMSConnection(Database).Database + ';');
    Query.SQL.Add('exec sp_revokedbaccess '''+pData^.Login+'''');
    Query.Execute;
    Query.SQL.Text := 'DROP LOGIN '+pData^.Login;
    Query.Execute; }

    FreeQuery(nQueryID);
    Result := True;
  end;  //  --  If nQueryID > -1 Then
end;

procedure TSDACBaseDB.ListUsers(UserList: TStringList; GroupID: Integer);
var nQueryID : Integer;
    Query : TMSQuery;
begin
  If Assigned(UserList) Then begin
    UserList.Clear;
    nQueryID := RequestQuery;
    If nQueryID > -1 Then begin
      Query := TMSQuery(QueryList.Items[nQueryID]);

      If GroupID > 0 Then begin// if a GroupID is given, we should use it
        Query.SQL.Text := RequestSQL(SQL_ID_USERS_LIST_GROUP); //InternalSQL.UserSQL.SQL_SELECT;
        Query.ParamByName('GroupID').ParamType := ptInput;
        Query.ParamByName('GroupID').AsInteger := GroupID;
      end else
        Query.SQL.Text := RequestSQL(SQL_ID_USERS_LIST_ALL); //InternalSQL.UserSQL.SQL_SELECT;
      Query.Prepare;  
      Query.Active := True;
      Query.First;
      While not Query.EoF Do begin
        // ID of the user will be saved in the object pointer for faster access later on
        UserList.AddObject(Query.Fields[1].AsString, TObject(Query.Fields[0].AsInteger));
        Query.Next;
      end;
      FreeQuery(nQueryID);
    end;
  end;  //  --  If Assigned(UserList) Then
end;

function TSDACBaseDB.GetUserData(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    Query : TMSQuery;
    BlobStream : TBlobStream;
begin
  Result := Inherited GetUserData(pData);

  If pData = nil Then
    Exit;

  If pData^.ID = 0 Then
    Exit;

  nQueryID := RequestQuery(False);
  Query := TMSQuery(QueryList.Items[nQueryID]);
  If pData^.ID = -1 Then begin
    Query.SQL.Text := RequestSQL(SQL_ID_USERS_SELECT_NAME);
    Query.Prepare;
    Query.ParamByName('Login').ParamType := ptInput;
    Query.ParamByName('Login').AsString := pData^.Login;
  end Else begin
    Query.SQL.Text := RequestSQL(SQL_ID_USERS_SELECT);
    Query.Prepare;
    Query.ParamByName('ID').ParamType := ptInput;
    Query.ParamByName('ID').AsInteger := pData^.ID;
  end;
  Query.Execute;
  If Not Query.EoF Then begin
    pData^.ID := Query.Fields[0].AsInteger;
    pData^.Login := Query.Fields[1].AsString;
    pData^.GroupID := Query.Fields[2].AsInteger;

        FpStreamer.Stream.Size := 0;
    FpStreamer.Data := pData;
    If Query.Fields[3].IsBlob Then begin
      BlobStream := TBlobStream.Create(TBlobField(Query.Fields[3]), bmRead);
      FpStreamer.Stream.LoadFromStream(BlobStream);
      BlobStream.Free;
    end;

    FpStreamer.ReadStream;
    FpStreamer.Stream.Size := 0;

    Result := True;
  end;  //  --  If Not pQuery.EoF Then
  FreeQuery(nQueryID);
end;

function TSDACBaseDB.ListGroups(GroupList: TList): Integer;
var nQueryID : Integer;
    Query : TMSQuery;
    pGroup : PGroupDataRec;
begin
  Result := ERR_NO_ERROR;
  nQueryID := RequestQuery(False);
  Query := TMSQuery(QueryList.Items[nQueryID]);
  Query.SQL.Text := RequestSQL(SQL_ID_GROUP_SELECT);
  Query.Prepare;
  Query.Execute;
  While Not Query.Eof Do begin
    New(pGroup);
    pGroup^.GroupID := Query.Fields[0].AsInteger;
    pGroup^.GroupName := Query.Fields[1].AsString;
    pGroup^.Flags := []; // TODO
    pGroup^.SL := Query.Fields[2].AsInteger;
    GroupList.Add(pGroup);
    Query.Next;
  end;
  Query.Close;
  FreeQuery(nQueryID);
end;

function TSDACBaseDB.CreateGroup(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    Query : TMSQuery;
begin
  Result := ERR_NO_ERROR;
  nQueryID := RequestQuery(True);
  Query := TMSQuery(QueryList.Items[nQueryID]);
  Query.SQL.Text := RequestSQL(SQL_ID_GROUP_INSERT);
  Query.Prepare;
  Query.ParamByName('Name').ParamType := ptInput;
  Query.ParamByName('Name').AsString := pGroup^.GroupName;
  Query.ParamByName('SL').ParamType := ptInput;
  If pGroup^.SL > ConnectionData.SecurityLevel Then
    pGroup^.SL := 1;
  Query.ParamByName('SL').AsSmallInt := pGroup^.SL;
  Try
    Query.Execute;
  Except
    Result := ERR_UNKNOWN_ERROR;
  End;
  FreeQuery(nQueryID);
end;

function TSDACBaseDB.UpdateGroup(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    Query : TMSQuery;
begin
  Result := ERR_NO_ERROR;
  nQueryID := RequestQuery(True);
  Query := TMSQuery(QueryList.Items[nQueryID]);
  Query.SQL.Text := RequestSQL(SQL_ID_GROUP_UPDATE);
  Query.Prepare;
  Query.ParamByName('ID').ParamType := ptInput;
  Query.ParamByName('ID').AsInteger := pGroup^.GroupID;
  Query.ParamByName('Name').ParamType := ptInput;
  Query.ParamByName('Name').AsString := pGroup^.GroupName;
  Query.ParamByName('SL').ParamType := ptInput;
  Query.ParamByName('Name').AsSmallInt := pGroup^.SL;
  Try
    Query.Execute;
  Except
    Result := ERR_UNKNOWN_ERROR;
  End;
  FreeQuery(nQueryID);
end;

function TSDACBaseDB.DeleteGroup(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    Query : TMSQuery;
begin
  Result := ERR_NO_ERROR;
  nQueryID := RequestQuery(True);
  Query := TMSQuery(QueryList.Items[nQueryID]);
  Query.SQL.Text := RequestSQL(SQL_ID_GROUP_DELETE);
  Query.Prepare;
  Query.ParamByName('ID').ParamType := ptInput;
  Query.ParamByName('ID').AsInteger := pGroup^.GroupID;
  Try
    Query.Execute;
  Except
    Result := ERR_UNKNOWN_ERROR;
  End;
  FreeQuery(nQueryID);
end;

function TSDACBaseDB.GetGroupData(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    Query : TMSQuery;
begin
  Result := inherited DeleteGroup(pGroup);
  nQueryID := RequestQuery(False);
  Query := TMSQuery(QueryList.Items[nQueryID]);
  Query.SQL.Text := RequestSQL(SQL_ID_GROUP_SELECT_ID);
  Query.Prepare;
  Query.ParamByName('ID').AsInteger := pGroup^.GroupID;
  Query.Execute;
  If Query.Eof = False Then begin
    pGroup^.GroupID := Query.Fields[0].AsInteger;
    pGroup^.GroupName := Query.Fields[1].AsString;
    pGroup^.SL := Byte(Query.Fields[2].AsInteger);
  end;
  FreeQuery(nQueryID);
end;

function TSDACBaseDB.RequestSQL(nID: Integer): String;
begin
  Result := '';
  Case nID Of
    SQL_ID_TEMPLATE_CREATE : Result := ReplaceTableIdent(sCREATE_TEMPLATE_TABLE, TableIdent);
    SQL_ID_USERS_CREATE    : Result := ReplaceTableIdent(sCREATE_USER_TABLE, TableIdent);
    Else Result := inherited RequestSQL(nID);
  end;
end;

end.
