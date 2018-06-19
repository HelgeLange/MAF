{*******************************************************************************
Name         : uSQLDirectBaseDB.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007 by Helge Lange
Info         : HelgeLange@gmail.com
Date         : 23.05.2007
Last Update  : 23.05.2007
Version      : 1.0.001
Purpose      : SQLDirect support base implementation

               SQLDirect Components can be found under
               http://www.sqldirect-soft.com/
Last Changes :

1.0.001 (23.05.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uSQLDirectBaseDB;

interface

uses Windows, SysUtils, Classes,
     // SQL Direct units
     DB, SDEngine,
     // ERP Framework units
     ERP_Globals, uERP_Tools, uERPCustomBaseDB;

Type TSQLDirectBaseDB = class(TERPCustomBaseDB)
     private
     protected
       function __GetConnected: Boolean; override;
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;
       function __GetHookTable: String; override;
       function __GetPlugInTable: String; override;
       function RequestQuery(bWrite: Boolean = False): Integer; override;
       function RequestSQL(nID: Integer): String; override;

       procedure PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName : String);
     public
       function CheckRequirements: Boolean; override;
       function CreateDataSet: TComponent; override;

       // modules / plugin - support
       procedure EnumModules; override;
       function AddModule(nID: Integer; LibName: String; bHasRes: Boolean): Integer; override;
       function RemoveModule(FileName: String): Integer; override;
       // dynamic functions
       procedure EnumDynamicFunctions; override;
       function AddDynamicFunction(pToken: PHookInstallToken): Integer; override;
       function RemoveDynamicFunction(pToken: PHookInstallToken): Integer; override;
       procedure ReOrderSubHooks(HookID: Integer; List: TList); override;
       // security layer
       procedure ReadProtectedFunctions; override;
       procedure WriteProtectedFunction(pToken: PRightToken); override;
       procedure DeleteProtectedFunction(nHookID, nSubHookID: Integer); override;
       // Templates
       function ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer; override;
       function WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer; override;
       // Users
       function CreateUser(sLoginName, sPassword : String) : Boolean; override;
       function GetUserPW(sLoginName: String; var sPassword : String): String; override;
//       function ListGroups
       procedure ListUsers(UserList: TStringList; GroupID : Integer); override;
     published
       property Connected;
       property ComputerName;
       property DataBaseName;
       property HookTable;
       property PlugInTable;
       property User;
       property Password;
       property DataBase;
       property HookLoadOptions;
       // events
       property OnConnect;
       property OnDisconnect;
       property OnCreateQuery;
       property OnSpecialSQL;
     end;

procedure Register;

implementation

uses uBaseSQL_Consts_MS_SQLServer, uUserRights_Shared, Dialogs;

procedure Register;
begin
  RegisterComponents('ERP Base', [TSQLDirectBaseDB]);
end;

{ TSQLDirectBaseDB }

function TSQLDirectBaseDB.__GetConnected: Boolean;
begin
  Result := False;
  If Assigned(Database) Then
    Result := TSDDatabase(Database).Connected;
end;

procedure TSQLDirectBaseDB.__InternalConnect;
begin
  If Assigned(Database) Then begin
    TSDDatabase(Database).Connected := False;
    TSDDatabase(Database).Params.Clear;
    TSDDatabase(Database).Params.Add('user name=' + User);
    TSDDatabase(Database).Params.Add('password=' + Password);
    TSDDatabase(Database).DatabaseName := DataBaseName;

    TSDDatabase(Database).Connected := True;
    If Not TSDDatabase(Database).Connected Then
      Raise TERPBaseDBError.Create('Failed to connect to database !');
  end;  //  --  If Assigned(Database) Then
  inherited;
end;

procedure TSQLDirectBaseDB.__InternalDisconnect;
begin
  inherited;
  If TSDDatabase(Database).Connected Then begin
    If TSDDatabase(Database).InTransaction Then
      TSDDatabase(Database).Commit;
    TSDDatabase(Database).Connected := False;
  end;  //  --  If TSDDatabase(Database).Connected Then
end;

function TSQLDirectBaseDB.CheckRequirements: Boolean;
begin
  Result := Assigned(Database);  // no inherited here, because we don't need an transaction
end;

function TSQLDirectBaseDB.RequestQuery(bWrite: Boolean): Integer;
var FQ: TSDQuery;
begin
  Result := -1;
  If Not CheckRequirements Then
    Exit;
  FQ := TSDQuery.Create(nil);
  FQ.Tag := __GetQueryID(QueryList, COMPONENT_BASENAME_QUERY);
  FQ.Name := COMPONENT_BASENAME_QUERY + IntToStr(FQ.Tag);
  FQ.DatabaseName := TSDDataBase(DataBase).DatabaseName;
  FQ.ParamCheck := True;
  Result := QueryList.Add(FQ);
  inherited RequestQuery;
end; // RequestQuery

function TSQLDirectBaseDB.CreateDataSet: TComponent;
var SDTable : TSDTable;
begin
  SDTable := TSDTable.Create(nil);
  SDTable.Tag := DataSetID;
  SDTable.Name := COMPONENT_BASENAME_DATASET + IntToStr(SDTable.Tag);
  SDTable.DatabaseName := TSDDataBase(DataBase).DatabaseName;
  Result := SDTable;
end;

procedure TSQLDirectBaseDB.EnumModules;
var nQueryID: Integer;
    Query : TSDQuery;
begin
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_MODULE_SELECT);
    Query.Active := True;
    Query.First;
    While Not Query.EoF Do begin
      If Assigned(OnEnumModules) Then
        OnEnumModules(Self, Query.Fields[0].AsInteger,
                            Query.Fields[1].AsString,
                            (Query.Fields[2].AsInteger = 1));
      Query.Next;
    end;  //  --  While Not Query.EoF Do
    FreeQuery(nQueryID);
  end;  //  --  If nQueryID > -1 Then
end;

function TSQLDirectBaseDB.AddModule(nID: Integer; LibName: String; bHasRes: Boolean): Integer;
var nQueryID : Integer;
    Query : TSDQuery;
begin
  If ((nID < 0) Or (LibName = EmptyStr)) Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;  //  --  If ((nID < 0) Or (LibName = EmptyStr)) Then

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery(True); // request write query
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_MODULE_INSERT);
    Query.ParamByName('ModuleID').AsInteger := nID;
    Query.ParamByName('Name').AsString := LibName;
    If bHasRes Then
      Query.ParamByName('HasRes').AsInteger := 1
    Else
      Query.ParamByName('HasRes').AsInteger := 0;
    Query.ExecSQL;
    FreeQuery(nQueryID);
    Result := ERR_NO_ERROR;
  end;  //  --  If nQueryID > -1 Then
end;

function TSQLDirectBaseDB.RemoveModule(FileName: String): Integer;
var nQueryID : Integer;
    Query : TSDQuery;
begin
  If FileName = EmptyStr Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;  //  --  If FileName = EmptyStr Then

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery(True); // request write query
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_MODULE_DELETE);
    Query.ParamByName('Name').AsString := FileName;
    Query.ExecSQL;
//    TSDDatabase(Database).Commit;
    FreeQuery(nQueryID);
    Result := ERR_NO_ERROR;
  end;  //  --  If nQueryID > -1 Then
end;

procedure TSQLDirectBaseDB.EnumDynamicFunctions;
var nQueryID: Integer;
    pToken : PHookInstallToken;
    Query : TSDQuery;
begin
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_HOOKS_SELECT);
    Query.Active := True;
    Query.First;
    New(pToken);
    pToken^.nAction := iaInsert;  // here it's always INSERT
    pToken^.InsertDir := hidLast; // always at the end since they're coming in there order from the database
    While Not Query.EoF Do begin
      If Assigned(OnEnumDynamicFunctions) Then begin
        pToken^.nHookID := Query.Fields[0].AsInteger;
        pToken^.nSubHookID := Query.Fields[1].AsInteger;
        pToken^.nModuleID := Query.Fields[2].AsInteger;
        OnEnumDynamicFunctions(Self, pToken);
      end;
      Query.Next; // Next element
    end;  //  --  While Not Query.EoF Do
    FreeQuery(nQueryID);
    Dispose(pToken);
  end;  //  --  If nQueryID > -1 Then
end;

function TSQLDirectBaseDB.AddDynamicFunction(pToken: PHookInstallToken): Integer;
var nQueryID : Integer;
    Query : TSDQuery;
begin
  If pToken = nil Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;  //  --  If pToken = nil Then

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery(True); // request write query
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_HOOKS_INSERT);
    Query.Prepare;
    Query.ParamByName('HookID').AsInteger := pToken^.nHookID;
    Query.ParamByName('SubHookID').AsInteger := pToken^.nSubHookID;
    Query.ParamByName('ModuleID').AsInteger := pToken^.nModuleID;
    Query.ParamByName('R_Order').AsInteger := pToken^.R_Order;
    Query.ParamByName('B_Active').AsInteger := pToken^.bActive;
    Query.ExecSQL;
    FreeQuery(nQueryID);
    Result := ERR_NO_ERROR;
  end;  //  --  If nQueryID > -1 Then
end;

function TSQLDirectBaseDB.RemoveDynamicFunction(pToken: PHookInstallToken): Integer;
var nQueryID : Integer;
    Query : TSDQuery;
begin
  Result := ERR_NO_ERROR;
  nQueryID := RequestQuery(True);
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_HOOKS_DELETE);
    Query.Prepare;
    Query.ParamByName('HookID').AsInteger := pToken^.nHookID;
    Query.ParamByName('SubHookID').AsInteger := pToken^.nSubHookID;
    Query.ParamByName('ModuleID').AsInteger := pToken^.nModuleID;
    Query.ExecSQL;
    FreeQuery(nQueryID);
  end;
end;

procedure TSQLDirectBaseDB.ReadProtectedFunctions;
var nQueryID : Integer;
    pItem : PAccessRightDescriptor;
    Query : TSDQuery;
begin
  If AccessRightTable = EmptyStr Then
    Exit;

  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_RIGHTS_SELECT);
    Query.Active := True;
    Query.First;
    New(pItem);
    While Not Query.EoF Do begin
      pItem^.ID := Query.Fields[0].AsInteger;
      pItem^.HookID := Query.Fields[1].AsInteger;
      pItem^.SubHookID := Query.Fields[2].AsInteger;
      If Assigned(OnEnumRights) Then
        OnEnumRights(Self, pItem);
      Query.Next;
    end;  //  --  While Not Query.EoF Do
    Dispose(pItem);
    FreeQuery(nQueryID);
  end;  //  --  If nQueryID > -1 Then
end;

procedure TSQLDirectBaseDB.WriteProtectedFunction(pToken: PRightToken);
var nQueryID : Integer;
    Query : TSDQuery;
begin
  If AccessRightTable = EmptyStr Then
    Exit;

  nQueryID := RequestQuery(True);
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_RIGHTS_INSERT);
    Query.Prepare;
    Query.ParamByName('HookID').AsInteger := pToken^.HookID;
    Query.ParamByName('SubHookID').AsInteger := pToken^.SubHookID;
    Query.ExecSQL;
    FreeQuery(nQueryID);
  end;  //  --  If nQueryID > -1 Then
end;

procedure TSQLDirectBaseDB.DeleteProtectedFunction(nHookID, nSubHookID: Integer);
var nQueryID : Integer;
    Query : TSDQuery;
begin
  If AccessRightTable = EmptyStr Then
    Exit;

  nQueryID := RequestQuery(True);
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_RIGHTS_DELETE);
    Query.Prepare;
    Query.ParamByName('HookID').AsInteger := nHookID;
    Query.ParamByName('SubHookID').AsInteger := nSubHookID;
    Query.ExecSQL;
    FreeQuery(nQueryID);
  end;  //  --  If nQueryID > -1 Then
end;

procedure TSQLDirectBaseDB.ReOrderSubHooks(HookID: Integer; List: TList);
var DS: TSDTable;
    UO : TSDUpdateSQL;
begin
  DS := TSDTable(CreateDataSet);
  UO := TSDUpdateSQL.Create(nil);
  DS.UpdateObject := UO;
  UO.RefreshSQL.Text := RequestSQL(SQL_ID_HOOKS_REORDER);

end;

procedure TSQLDirectBaseDB.PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName: String);
var Query : TSDQuery;
begin
  Query := TSDQuery(QueryList.Items[nQueryID]);
  If nID > 0 Then begin
    Query.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_SELECT_ID);
    Query.Prepare;
    Query.ParamByName('TemplateID').AsInteger := nID;
  end else
    If ((sCat <> '') AND (sName <> '')) Then begin
      Query.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_SELECT_NAME);
      If Query.SQL.Text <> '' Then begin
        Query.Prepare;
        Query.ParamByName('Category').AsString := sCat;
        Query.ParamByName('Template_Name').AsString := sName;
      end;  //  --  If Query.SQL.Text <> '' Then 
    end;  //  --  If ((Category <> '') AND (TemplateName <> '')) Then

  // check again, if we have a SQL query as we should by now
  If Query.SQL.Text = '' Then begin
    FreeQuery(nQueryID);          // releasing the query
    Exit;                         // and we're gone :P
  end;  //  --  If Query.SQL.Text = '' Then
end;

function TSQLDirectBaseDB.ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer;
var nQueryID : Integer;
    Query : TSDQuery;
    BlobStream : TStream;
begin
  Result := inherited ReadTemplate(aStream, nID, TableName, Category, TemplateName);
  If Result <> ERR_NO_ERROR Then
    Exit;
  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
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
      BlobStream := Query.CreateBlobStream(Query.Fields[3], bmRead);
      If Assigned(BlobStream) Then begin
        aStream.LoadFromStream(BlobStream);
        FreeAndNil(BlobStream);
      end;
      Result := ERR_NO_ERROR; // everything ok here
    end;  //  --  If Not Query.EoF Then
    FreeQuery(nQueryID); // free the query
  end;  //  --  If nQueryID > -1 Then
end;

function TSQLDirectBaseDB.WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer;
var nQueryID : Integer;
    Query : TSDQuery;
begin
  Result := inherited WriteTemplate(aStream, nID, TableName, Category, TemplateName);
  If Result <> ERR_NO_ERROR Then
    Exit;

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
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
      Query.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_UPDATE);
      Query.Prepare;
      Query.ParamByName('TemplateID').AsInteger := nID;
    end else begin
      // create new template
      Query.Close;
      nID := -1; // we don't know the ID yet
      Query.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_INSERT);
      Query.Prepare;
      Query.ParamByName('Category').AsString := Category;
      Query.ParamByName('Template_Name').AsString := TemplateName;
    end;
    aStream.Position := 0;
    Query.ParamByName('Template_Data').SetBlobData(aStream.Memory, aStream.Size);
    Query.ExecSQL;
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

function TSQLDirectBaseDB.__GetHookTable: String;
begin
  Result := inherited __GetHookTable;
  If Assigned(Database) Then begin
    Case TSDDatabase(Database).ServerType Of
      // for SQL-Server we have to add Databasename and '..' in front of the table name
      stSQLServer   : If Pos(TSDDatabase(Database).DatabaseName + '..', Result) = 0 Then
                        Result := TSDDatabase(Database).DatabaseName + '..' + Result;
    end;
  end;
end;

function TSQLDirectBaseDB.__GetPlugInTable: String;
begin
  Result := inherited __GetPlugInTable;
  If Assigned(Database) Then begin
    Case TSDDatabase(Database).ServerType Of
      // for SQL-Server we have to add Databasename and '..' in front of the table name
      stSQLServer   : If Pos(TSDDatabase(Database).DatabaseName + '..', Result) = 0 Then
                        Result := TSDDatabase(Database).DatabaseName + '..' + Result;
    end;
  end;
end;

function TSQLDirectBaseDB.CreateUser(sLoginName, sPassword: String): Boolean;
var nQueryID : Integer;
    Query : TSDQuery;
begin
  Result := False;
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := 'EXEC sp_addlogin '''+sLoginName+''',  '''+sPassword+'''';
    Query.ExecSQL;
    Query.SQL.Text := 'Use '+ TSDDatabase(Database).DatabaseName;
    Query.ExecSQL;
    Query.SQL.Text := 'exec sp_grantdbaccess '''+sLoginName+'''';
    Query.ExecSQL;

    FreeQuery(nQueryID);
    Result := True;
  end;  //  --  If nQueryID > -1 Then
end;

procedure TSQLDirectBaseDB.ListUsers(UserList: TStringList; GroupID: Integer);
var nQueryID : Integer;
    Query : TSDQuery;
begin
  If Assigned(UserList) Then begin
    UserList.Clear;
    nQueryID := RequestQuery;
    If nQueryID > -1 Then begin
      Query := TSDQuery(QueryList.Items[nQueryID]);
      If GroupID > 0 Then begin// if a GroupID is given, we should use it
        Query.SQL.Text := RequestSQL(SQL_ID_USERS_LIST_GROUP); //InternalSQL.UserSQL.SQL_SELECT;
        Query.ParamByName('GroupID').AsInteger := GroupID;
      end else
        Query.SQL.Text := RequestSQL(SQL_ID_USERS_LIST_ALL); //InternalSQL.UserSQL.SQL_SELECT;
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

function TSQLDirectBaseDB.GetUserPW(sLoginName: String; var sPassword: String): String;
var nQueryID : Integer;
    Query : TSDQuery;
begin
  Result := '';
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TSDQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := 'SELECT * FROM UserSecurity WHERE Login LIKE "'+sLoginName+'"';
    Query.Active := True;
    Query.First;
    If Not Query.EoF Then begin
      If Query.Fields[5].AsString = sPassword Then
        Result := Query.Fields[6].AsString // return crypted DB password for this user
      else
        sPassword := ''; // empty password
    end;  //  --  If Not Query.EoF Then
  end;  //  --  If nQueryID > -1 Then
end;

function TSQLDirectBaseDB.RequestSQL(nID: Integer): String;
begin
  Result := '';
  Case nID Of
    SQL_ID_TEMPLATE_CREATE : Result := ReplaceTableIdent(sCREATE_TEMPLATE_TABLE, TableIdent);
    SQL_ID_USERS_CREATE    : Result := ReplaceTableIdent(sCREATE_USER_TABLE, TableIdent);
    Else Result := inherited RequestSQL(nID);
  end;
end;

end.
