{*******************************************************************************
Name         : uAnyDACBaseDB.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2009 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 14.11.2009
Last Update  : 14.11.2009
Version      : 1.0.000
Purpose      : RemObjects AnyDAC support package
               AnyDAC can be found under http://www.AnyDAC.com

Last Changes :

1.0.001 (14.11.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAFAnyDACBaseDB;

interface

uses Windows, SysUtils, Classes, DB,
     // RemObjects AnyDAC units
     uADStanIntf, uADStanOption, uADStanError, uADGUIxIntf, uADPhysIntf,
     uADStanDef, uADStanPool, uADPhysManager, uADCompClient, uADStanParam,
     uADDatSManager, uADDAptIntf, uADStanAsync, uADDAptManager,
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_Tools, uMAF_CustomBaseDB, uMAF_HookManager_Helper,
     uMAF_BaseDB;

Type TAnyDACBaseDB = class(TmafBaseDB)
     private
       procedure PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName: String);
       procedure __SwitchReadWriteQuery(nQueryID: Integer; bWrite: Boolean);
     protected
       function __GetConnected: Boolean; override;
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;
       function RequestQuery(bWrite: Boolean = false): Integer; override;
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
       // Users
       procedure ListUsers(UserList: TStringList; GroupID : Integer); override;
       // Groups
       function ListGroups(GroupList: TList): Integer; override;
       // Templates
       function ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String; bReadData: Boolean = True): Integer; override;
       function WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer; override;
     published
       property Connected;
       property ComputerName;
       property DataBaseName;
       property User;
       property Password;
       property DataBase;
       // events
       property OnConnect;
       property OnDisconnect;
     end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MAF Database', [TAnyDACBaseDB]);
end;

{ TAnyDACBaseDB }

function TAnyDACBaseDB.RequestQuery(bWrite: Boolean): Integer;
var aQuery : TADQuery;
begin
  aQuery := TADQuery.Create(nil);
  aQuery.Connection := TADConnection(Database);
  aQuery.Transaction := nil;
  If Assigned(TADConnection(Database).UpdateTransaction) Then
    If bWrite Then
      aQuery.Transaction := TADConnection(Database).UpdateTransaction;
  If aQuery.Transaction = nil Then
    aQuery.Transaction := TADConnection(Database).Transaction;

  Result := QueryList.Add(aQuery);
  // triggers OnRequestQuery and allows to change for the query setting there
  inherited RequestQuery(bWrite);
end;

function TAnyDACBaseDB.__GetConnected: Boolean;
begin
  Result := False;
  If Assigned(Database) Then
    Result := TADConnection(Database).Connected;
end;

procedure TAnyDACBaseDB.__InternalConnect;
var i: Integer;
    bUserAdded, bPasswordAdded : Boolean;
begin
  If Not Assigned(Database) Then
    Exit;

  bUserAdded := False;
  bPasswordAdded := False;
  TADConnection(Database).Connected := False;
  For i := 0 To TADConnection(Database).Params.Count - 1 Do begin
    If Not bUserAdded Then
      If Pos('User_Name=', TADConnection(Database).Params.Strings[i]) > 0 Then begin
        TADConnection(Database).Params.Strings[i] := 'User_Name=' + User;
        bUserAdded := True;
      end;
    If Not bPasswordAdded Then
      If Pos('Password=', TADConnection(Database).Params.Strings[i]) > 0 Then begin
        TADConnection(Database).Params.Strings[i] := 'Password=' + Password;
        bPasswordAdded := True;
      end;
  end;
  If Not bUserAdded Then
    TADConnection(Database).Params.Add('User_Name=' + User);
  If Not bPasswordAdded Then
    TADConnection(Database).Params.Add('Password=' + Password);

  TADConnection(Database).Connected := True;
  inherited;
end;

procedure TAnyDACBaseDB.__InternalDisconnect;
begin
  If TADConnection(Database).Connected Then begin
    If TADConnection(Database).InTransaction Then
      TADConnection(Database).Commit;
    TADConnection(Database).Connected := False;
  end;  //  --  If TSDDatabase(Database).Connected Then
  inherited;
end;

procedure TAnyDACBaseDB.__SwitchReadWriteQuery(nQueryID: Integer; bWrite: Boolean);
var aQuery : TADQuery;
begin
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  aQuery.Transaction := nil;
  If Assigned(TADConnection(Database).UpdateTransaction) Then
    If bWrite Then
      aQuery.Transaction := TADConnection(Database).UpdateTransaction;
  If aQuery.Transaction = nil Then
    aQuery.Transaction := TADConnection(Database).Transaction;
end;

procedure TAnyDACBaseDB.PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName: String);
var aQuery : TADQuery;
begin
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  If nID > 0 Then begin
    aQuery.Params.CreateParam(ftInteger, 'DataID', ptInput);
    aQuery.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_SELECT_ID);
    aQuery.Prepare;
    aQuery.ParamByName('DataID').AsInteger := nID;
  end else
    If ((sCat <> '') AND (sName <> '')) Then begin
      aQuery.Params.CreateParam(ftString, 'Data_Category', ptInput);
      aQuery.Params.CreateParam(ftString, 'Data_Name', ptInput);
      aQuery.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_SELECT_NAME);
      aQuery.Prepare;
      aQuery.ParamByName('Data_Category').AsString := sCat;
      aQuery.ParamByName('Data_Name').AsString := sName;
    end;  //  --  If ((sCat <> '') AND (sName <> '')) Then

  // check again, if we have a SQL query as we should by now
  If aQuery.SQL.Text = '' Then begin
    FreeQuery(nQueryID);          // releasing the query
    Exit;                         // and we're gone :P
  end;  //  --  If aQuery.SQL.Text = '' Then
end;

function TAnyDACBaseDB.ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String; bReadData: Boolean): Integer;
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  Result := inherited ReadTemplate(aStream, nID, TableName, Category, TemplateName);
  If Result <> ERR_NO_ERROR Then
    Exit;
  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery;
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  If nQueryID > -1 Then begin
    PrepareTemplateQuery(nQueryID, nID, TableName, Category, TemplateName);
    If nQueryID = -1 Then begin
      Result := ERR_PARAM_FAILURE;
      Exit;
    end;  //  --  If nQueryID = -1 Then
    aQuery.Open;
    If Not aQuery.EoF Then begin
      // filling all 3
      nID := aQuery.Fields[0].AsInteger;
      Category := aQuery.Fields[1].AsString;
      TemplateName := aQuery.Fields[2].AsString;
      If bReadData Then
        If aQuery.Fields[3].IsBlob Then
          TBlobField(aQuery.Fields[3]).SaveToStream(aStream);
      Result := ERR_NO_ERROR; // everything ok here
    end;  //  --  If Not aQuery.EoF Then
    FreeQuery(nQueryID); // free the query
  end;
end;

function TAnyDACBaseDB.WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer;
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  Result := inherited WriteTemplate(aStream, nID, TableName, Category, TemplateName);
  If Result <> ERR_NO_ERROR Then
    Exit;

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery(False);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  If nQueryID > -1 Then begin
    PrepareTemplateQuery(nQueryID, nID, TableName, Category, TemplateName);
    If nQueryID = -1 Then begin
      Result := ERR_PARAM_FAILURE;
      Exit;
    end;  //  --  If nQueryID = -1 Then
    aQuery.Open;
    If Not aQuery.EoF Then begin
      // update mode
      nID := aQuery.Fields[0].AsInteger; // we need the ID in any case
      Category := aQuery.Fields[1].AsString; // return for users convenience
      TemplateName := aQuery.Fields[2].AsString;
      aQuery.Close;
      __SwitchReadWriteQuery(nQueryID, True);  // now we want to write
      aQuery.Params.CreateParam(ftInteger, 'DataID', ptInput);
      aQuery.Params.CreateParam(ftBlob, 'Data_Storage', ptInput);
      aQuery.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_UPDATE);
      aQuery.Prepare;
      aQuery.ParamByName('DataID').AsInteger := nID;
    end else begin
      // create new template
      aQuery.Close;
      __SwitchReadWriteQuery(nQueryID, True);  // now we want to write
      nID := -1; // we don't know the ID yet
      aQuery.Params.CreateParam(ftString, 'Data_Category', ptInput);
      aQuery.Params.CreateParam(ftString, 'Data_Name', ptInput);
      aQuery.Params.CreateParam(ftBlob, 'Data_Storage', ptInput);
      aQuery.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_INSERT);
      aQuery.Prepare;
      aQuery.ParamByName('Data_Category').DataType := ftString;
      aQuery.ParamByName('Data_Category').AsString := Category;
      aQuery.ParamByName('Data_Name').DataType := ftString;
      aQuery.ParamByName('Data_Name').AsString := TemplateName;
    end;
    aQuery.ParamByName('Data_Storage').LoadFromStream(aStream, ftBlob);
    aQuery.ExecSQL;
    If nID = -1 Then begin
      // we inserted a new template, we return the ID
      PrepareTemplateQuery(nQueryID, nID, TableName, Category, TemplateName); // will cause to prepare a query through Category/TemplateName
      __SwitchReadWriteQuery(nQueryID, False); // just reading... promise!
      aQuery.Open;
      If Not aQuery.EoF Then
        nID := aQuery.Fields[0].AsInteger;
    end;
    FreeQuery(nQueryID);
  end;  //  --  If nQueryID > -1 Then
end;

function TAnyDACBaseDB.CreateUser(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  Result := False;
  If pData = nil Then
    Exit;

  nQueryID := RequestQuery(True);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  aQuery.Params.CreateParam(ftString, 'Login', ptInput);
  aQuery.Params.CreateParam(ftInteger, 'GroupID', ptInput);
  aQuery.Params.CreateParam(ftBlob, 'Account_Data', ptInput);
  aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_INSERT);
  aQuery.Prepare;
  aQuery.ParamByName('Login').AsString := pData^.Login;
  aQuery.ParamByName('GroupID').AsInteger := pData^.GroupID;

  FpStreamer.Data := pData;
  FpStreamer.WriteStream(6558);

  aQuery.ParamByName('Account_Data').LoadFromStream(FpStreamer.Stream, ftBlob);

  aQuery.ExecSQL;

  FreeQuery(nQueryID);
  Result := True;
end;

function TAnyDACBaseDB.DeleteUser(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  Result := False;
  If pData = nil Then
    Exit;

  If pData^.ID = 0 Then
    Exit;

  nQueryID := RequestQuery(True);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  aQuery.Params.CreateParam(ftInteger, 'ID', ptInput);
  aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_DELETE);
  aQuery.Prepare;
  aQuery.ParamByName('ID').AsInteger := pData^.ID;
  aQuery.ExecSQL;

  FreeQuery(nQueryID);
  Result := True;
end;

function TAnyDACBaseDB.UpdateUser(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  Result := False;
  If pData = nil Then
    Exit;

  If pData^.ID = 0 Then
    Exit;

  nQueryID := RequestQuery(True);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  aQuery.Params.CreateParam(ftInteger, 'ID', ptInput);
  aQuery.Params.CreateParam(ftInteger, 'GroupID', ptInput);
  aQuery.Params.CreateParam(ftBlob, 'Account_Data', ptInput);

  aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_UPDATE);
  aQuery.Prepare;
  aQuery.ParamByName('ID').AsInteger := pData^.ID;
  aQuery.ParamByName('GroupID').AsInteger := pData^.GroupID;

  FpStreamer.Data := pData;
  FpStreamer.WriteStream(6558);

  aQuery.ParamByName('Account_Data').LoadFromStream(FpStreamer.Stream, ftBlob);
  aQuery.ExecSQL;

  FreeQuery(nQueryID);
  Result := True;
end;

function TAnyDACBaseDB.GetUserData(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  Result := Inherited GetUserData(pData);

  If pData = nil Then
    Exit;

  If pData^.ID = 0 Then
    Exit;

  nQueryID := RequestQuery(False);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  If pData^.ID < 1 Then begin
    aQuery.Params.CreateParam(ftString, 'Login', ptInput);
    aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_SELECT_NAME);
    aQuery.Prepare;
    aQuery.ParamByName('Login').AsString := pData^.Login;
  end Else begin
    aQuery.Params.CreateParam(ftInteger, 'ID', ptInput);
    aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_SELECT);
    aQuery.Prepare;
    aQuery.ParamByName('ID').AsInteger := pData^.ID;
  end;
  aQuery.Open;
  If Not aQuery.EoF Then begin
    pData^.ID := aQuery.Fields[0].AsInteger;
    pData^.Login := aQuery.Fields[1].AsString;
    pData^.GroupID := aQuery.Fields[2].AsInteger;

    FpStreamer.Stream.Size := 0;
    FpStreamer.Data := pData;
    If aQuery.Fields[3].IsBlob Then
      TBlobField(aQuery.Fields[3]).SaveToStream(FpStreamer.Stream);

    FpStreamer.ReadStream;
    FpStreamer.Stream.Size := 0;

    Result := True;
  end;  //  --  If Not pQuery.EoF Then
  FreeQuery(nQueryID);
end;

procedure TAnyDACBaseDB.ListUsers(UserList: TStringList; GroupID: Integer);
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  nQueryID := RequestQuery(False);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  If GroupID < 1 Then begin
    aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_LIST_ALL);
    aQuery.Prepare;
  end Else begin
    aQuery.Params.CreateParam(ftInteger, 'GroupID', ptInput);
    aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_LIST_GROUP);
    aQuery.Prepare;
    aQuery.ParamByName('GroupID').AsInteger := GroupID;
  end;  //  --  If GroupID = -1 Then

  aQuery.Open;
  While Not aQuery.EoF Do begin
    UserList.AddObject(aQuery.Fields[1].AsString, Pointer(aQuery.Fields[0].AsInteger));
    aQuery.Next;
  end;  //  --  While Not pQuery.EoF Do
  aQuery.Close;
  FreeQuery(nQueryID);
end;

function TAnyDACBaseDB.ListGroups(GroupList: TList): Integer;
var nQueryID : Integer;
    aQuery : TADQuery;
    pGroup : PGroupDataRec;
begin
  Result := inherited ListGroups(GroupList);
  nQueryID := RequestQuery(False);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  aQuery.SQL.Text := RequestSQL(SQL_ID_GROUP_SELECT);
  aQuery.Prepare;
  aQuery.Open;
  While Not aQuery.EoF Do begin
    New(pGroup);
    pGroup^.GroupID := aQuery.Fields[0].AsInteger;
    pGroup^.GroupName := aQuery.Fields[1].AsString;
    pGroup^.Flags := []; // TODO
    pGroup^.SL := aQuery.Fields[2].AsInteger;
    GroupList.Add(pGroup);
    aQuery.Next;
  end;  //  --  While Not pQuery.EoF Do
  aQuery.Close;
  FreeQuery(nQueryID);
end;

function TAnyDACBaseDB.CreateGroup(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  Result := inherited CreateGroup(pGroup);
  nQueryID := RequestQuery(True);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  aQuery.Params.CreateParam(ftString, 'Name', ptInput);
  aQuery.Params.CreateParam(ftSmallint, 'SL', ptInput);
  aQuery.SQL.Text := RequestSQL(SQL_ID_GROUP_INSERT);
  aQuery.Prepare;
  aQuery.ParamByName('Name').AsString := pGroup^.GroupName; // the trigger will set the ID
  If pGroup^.SL > ConnectionData.SecurityLevel Then
    pGroup^.SL := 1;
  aQuery.ParamByName('SL').AsSmallInt := pGroup^.SL;
  aQuery.ExecSQL;
  If aQuery.Transaction.IsActive Then
    aQuery.Transaction.Commit;
  FreeQuery(nQueryID);
end;

function TAnyDACBaseDB.UpdateGroup(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  Result := inherited UpdateGroup(pGroup);
  nQueryID := RequestQuery(True);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  aQuery.Params.CreateParam(ftInteger, 'ID', ptInput);
  aQuery.Params.CreateParam(ftString, 'Name', ptInput);
  aQuery.Params.CreateParam(ftSmallint, 'SL', ptInput);
  aQuery.SQL.Text := RequestSQL(SQL_ID_GROUP_UPDATE);
  aQuery.Prepare;
  aQuery.ParamByName('ID').AsInteger := pGroup^.GroupID;
  aQuery.ParamByName('Name').AsString := pGroup^.GroupName;
  aQuery.ParamByName('SL').AsSmallInt := pGroup^.SL;
  aQuery.ExecSQL;
  If aQuery.Transaction.IsActive Then
    aQuery.Transaction.Commit;
  FreeQuery(nQueryID);
end;

function TAnyDACBaseDB.DeleteGroup(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  Result := inherited DeleteGroup(pGroup);
  nQueryID := RequestQuery(True);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  aQuery.Params.CreateParam(ftInteger, 'ID', ptInput);
  aQuery.SQL.Text := RequestSQL(SQL_ID_GROUP_DELETE);
  aQuery.Prepare;
  aQuery.ParamByName('ID').AsInteger := pGroup^.GroupID;
  aQuery.ExecSQL;
  FreeQuery(nQueryID);
end;

function TAnyDACBaseDB.GetGroupData(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    aQuery : TADQuery;
begin
  Result := inherited DeleteGroup(pGroup);
  nQueryID := RequestQuery(False);
  aQuery := TADQuery(QueryList.Items[nQueryID]);
  aQuery.Params.CreateParam(ftInteger, 'ID', ptInput);
  aQuery.SQL.Text := RequestSQL(SQL_ID_GROUP_SELECT_ID);
  aQuery.Prepare;
  aQuery.ParamByName('ID').AsInteger := pGroup^.GroupID;
  aQuery.Open;
  If aQuery.Eof = False Then begin
    pGroup^.GroupID := aQuery.Fields[0].AsInteger;
    pGroup^.GroupName := aQuery.Fields[1].AsString;
    pGroup^.SL := Byte(aQuery.Fields[2].AsInteger);
  end;
  FreeQuery(nQueryID);
end;

end.
