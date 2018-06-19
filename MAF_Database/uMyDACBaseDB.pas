{*******************************************************************************
Name         : uMyDACBaseDB.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2009 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 05.11.2009
Last Update  : 05.11.2009
Version      : 1.0.000
Purpose      : DevArt MyDAC implementation

               MyDAC components can be found under
               http://www.devart.com/mydac/

Last Changes :

1.0.001 (19.03.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMyDACBaseDB;

interface

uses SysUtils, Classes, Windows, DB,
     // MyDAC units
     MemDS, DBAccess, MyAccess, MemData,

     // Modular Application Framework Components units
     uMAF_Globals, uMAF_CustomBaseDB, uMAF_HookManager_Helper, uMAF_BaseDB,
     uMAF_TemplateStreamer, uBaseSQL_Consts_MySQL;

Type TMyDACBaseDB = class(TmafBaseDB)
     private
       procedure PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName: String);
       procedure __SwitchReadWriteQuery(nQueryID: Integer; bWrite: Boolean);
     protected
       function __GetConnected: Boolean; override;
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;
       function RequestSQL(nID: Integer): String; override;
       function RequestQuery(bWrite: Boolean = false): Integer; override;
       procedure __ExecuteQuery(nQueryID: Integer);
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

     TMySQLDB = class(TMyConnection);
     TMySQLQuery = class(TMyQuery);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MAF Database', [TMyDACBaseDB]);
end;

{ TMyDACBaseDB }

function TMyDACBaseDB.RequestSQL(nID: Integer): String;
begin
  Result := inherited RequestSQL(nID);
  Case nID Of
    SQL_ID_TEMPLATE_SELECT_ID : Result := ReplaceTableIdent(sQUERY_TEMPLATE_ID, DataStorageTable);
    SQL_ID_TEMPLATE_SELECT_NAME : Result := ReplaceTableIdent(sQUERY_TEMPLATE_NAME, DataStorageTable);
    SQL_ID_TEMPLATE_UPDATE  : Result := ReplaceTableIdent(sUPDATE_TEMPLATE, DataStorageTable);
    SQL_ID_TEMPLATE_INSERT  : Result := ReplaceTableIdent(sINSERT_TEMPLATE, DataStorageTable);
    SQL_ID_USERS_SELECT     : Result := ReplaceTableIdent(sQUERY_USERS_ID, UserTable);
    SQL_ID_USERS_SELECT_NAME: Result := ReplaceTableIdent(sQUERY_USERS_NAME, UserTable);
    SQL_ID_USERS_UPDATE     : Result := ReplaceTableIdent(sUPDATE_USERS, UserTable);
    SQL_ID_USERS_INSERT     : Result := ReplaceTableIdent(sINSERT_USERS, UserTable);
  end;
end;

function TMyDACBaseDB.RequestQuery(bWrite: Boolean): Integer;
var aQuery : TMySQLQuery;
begin
  aQuery := TMySQLQuery.Create(nil);
  aQuery.Connection := TMySQLDB(Database);
  aQuery.ReadOnly := Not bWrite;

  Result := QueryList.Add(aQuery);
  // triggers OnRequestQuery and allows to change for the query setting there
  inherited RequestQuery(bWrite);
end;

procedure TMyDACBaseDB.__ExecuteQuery(nQueryID: Integer);
begin
  TMySQLQuery(QueryList.Items[nQueryID]).Execute;
end;

procedure TMyDACBaseDB.__InternalConnect;
begin
  If Not Assigned(Database) Then
    Exit;

  TMySQLDB(Database).Connected := False;
  TMySQLDB(Database).Username := User;
  TMySQLDB(Database).Password := Password;
  TMySQLDB(Database).Connected := True;
  inherited;
end;

procedure TMyDACBaseDB.__InternalDisconnect;
begin
  inherited;
  If Assigned(Database) Then
    TMySQLDB(Database).Connected := False;
end;

function TMyDACBaseDB.__GetConnected: Boolean;
begin
  Result := False;
  If Assigned(Database) Then
    Result := TMySQLDB(Database).Connected;
end;

procedure TMyDACBaseDB.__SwitchReadWriteQuery(nQueryID: Integer; bWrite: Boolean);
begin
  TMySQLQuery(QueryList.Items[nQueryID]).ReadOnly := (Not bWrite);
end;

procedure TMyDACBaseDB.PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName: String);
var aQuery : TMySQLQuery;
begin
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  If nID > 0 Then begin
    aQuery.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_SELECT_ID);
    aQuery.Prepare;
    aQuery.ParamByName('DataID').AsInteger := nID;
  end else
    If ((sCat <> '') AND (sName <> '')) Then begin
      aQuery.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_SELECT_NAME);
      aQuery.Prepare;
      aQuery.ParamByName('Data_Category').AsString := sCat;
      aQuery.ParamByName('Data_Name').AsString := sName;
    end;  //  --  If ((Category <> '') AND (TemplateName <> '')) Then

  // check again, if we have a SQL query as we should by now
  If aQuery.SQL.Text = '' Then begin
    FreeQuery(nQueryID);          // releasing the query
    Exit;                         // and we're gone :P
  end;  //  --  If TpFIBQuery(QueryList.Items[nQueryID]).SQL.Text = '' Then
end;

function TMyDACBaseDB.ReadTemplate(aStream: TMemoryStream; var nID: Integer;
                      TableName: String; var Category, TemplateName: String;
                      bReadData: Boolean): Integer;
var nQueryID : Integer;
    BlobStream : TBlobStream;
begin
  Result := inherited ReadTemplate(aStream, nID, TableName, Category, TemplateName);
  If Result <> ERR_NO_ERROR Then
    Exit;
  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    PrepareTemplateQuery(nQueryID, nID, TableName, Category, TemplateName);
    If nQueryID = -1 Then begin
      Result := ERR_PARAM_FAILURE;
      Exit;
    end;  //  --  If nQueryID = -1 Then
    __ExecuteQuery(nQueryID);
    If Not TMySQLQuery(QueryList.Items[nQueryID]).EoF Then begin
      // filling all 3
      nID := TMySQLQuery(QueryList.Items[nQueryID]).Fields[0].AsInteger;
      Category := TMySQLQuery(QueryList.Items[nQueryID]).Fields[1].AsString;
      TemplateName := TMySQLQuery(QueryList.Items[nQueryID]).Fields[2].AsString;
      If bReadData Then
        If TMySQLQuery(QueryList.Items[nQueryID]).Fields[3].IsBlob Then begin
          BlobStream := TBlobStream.Create(TBlobField(TMySQLQuery(QueryList.Items[nQueryID]).Fields[3]), bmRead);
          aStream.LoadFromStream(BlobStream);
          BlobStream.Free;
        end;
      Result := ERR_NO_ERROR; // everything ok here
    end;  //  --  If Not TpFIBQuery(QueryList.Items[nQueryID]).EoF Then
    FreeQuery(nQueryID); // free the query
  end;
end;

function TMyDACBaseDB.WriteTemplate(aStream: TMemoryStream; var nID: Integer;
                      TableName: String; var Category, TemplateName: String): Integer;
var nQueryID : Integer;
    aBlob : TBlob;
begin
  Result := inherited WriteTemplate(aStream, nID, TableName, Category, TemplateName);
  If Result <> ERR_NO_ERROR Then
    Exit;

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery(False);
  If nQueryID > -1 Then begin
    PrepareTemplateQuery(nQueryID, nID, TableName, Category, TemplateName);
    If nQueryID = -1 Then begin
      Result := ERR_PARAM_FAILURE;
      Exit;
    end;  //  --  If nQueryID = -1 Then
    __ExecuteQuery(nQueryID);
    If Not TMySQLQuery(QueryList.Items[nQueryID]).EoF Then begin
      // update mode
      nID := TMySQLQuery(QueryList.Items[nQueryID]).Fields[0].AsInteger; // we need the ID in any case
      Category := TMySQLQuery(QueryList.Items[nQueryID]).Fields[1].AsString; // return for users convenience
      TemplateName := TMySQLQuery(QueryList.Items[nQueryID]).Fields[2].AsString;
      TMySQLQuery(QueryList.Items[nQueryID]).Close;
      __SwitchReadWriteQuery(nQueryID, True);  // now we want to write
      TMySQLQuery(QueryList.Items[nQueryID]).SQL.Text := RequestSQL(SQL_ID_TEMPLATE_UPDATE);
      TMySQLQuery(QueryList.Items[nQueryID]).Prepare;
      TMySQLQuery(QueryList.Items[nQueryID]).ParamByName('DataID').AsInteger := nID;
    end else begin
      // create new template
      TMySQLQuery(QueryList.Items[nQueryID]).Close;
      __SwitchReadWriteQuery(nQueryID, True);  // now we want to write
      nID := -1; // we don't know the ID yet
      TMySQLQuery(QueryList.Items[nQueryID]).SQL.Text := RequestSQL(SQL_ID_TEMPLATE_INSERT);
      TMySQLQuery(QueryList.Items[nQueryID]).Prepare;
      TMySQLQuery(QueryList.Items[nQueryID]).ParamByName('Data_Category').AsString := Category;
      TMySQLQuery(QueryList.Items[nQueryID]).ParamByName('Data_Name').AsString := TemplateName;
    end;
     aBlob := TMySQLQuery(QueryList.Items[nQueryID]).ParamByName('Data_Storage').AsBlobRef;
     aBlob.Clear;
     aBlob.LoadFromStream(aStream);
     aBlob.Commit;
    __ExecuteQuery(nQueryID);
    If nID = -1 Then begin
      // we inserted a new template, we return the ID
      PrepareTemplateQuery(nQueryID, nID, TableName, Category, TemplateName); // will cause to prepare a query through Category/TemplateName
      __SwitchReadWriteQuery(nQueryID, False); // just reading... promise!
      __ExecuteQuery(nQueryID);
      If Not TMySQLQuery(QueryList.Items[nQueryID]).EoF Then
        nID := TMySQLQuery(QueryList.Items[nQueryID]).Fields[0].AsInteger;
    end;
    FreeQuery(nQueryID);
  end;  //  --  If nQueryID > -1 Then
end;

function TMyDACBaseDB.GetUserData(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    aQuery : TMySQLQuery;
    BlobStream : TBlobStream;
begin
  Result := Inherited GetUserData(pData);

  If pData = nil Then
    Exit;

  If pData^.ID = 0 Then
    Exit;

  nQueryID := RequestQuery(False);
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  If pData^.ID < 1 Then begin
    aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_SELECT_NAME);
    aQuery.Prepare;
    aQuery.ParamByName('Login').AsString := pData^.Login;
  end Else begin
    aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_SELECT);
    aQuery.Prepare;
    aQuery.ParamByName('ID').AsInteger := pData^.ID;
  end;
  __ExecuteQuery(nQueryID);
  If Not aQuery.EoF Then begin
    pData^.ID := aQuery.Fields[0].AsInteger;
    pData^.Login := aQuery.Fields[1].AsString;
    pData^.GroupID := aQuery.Fields[2].AsInteger;

    FpStreamer.Stream.Size := 0;
    FpStreamer.Data := pData;
    If aQuery.Fields[3].IsBlob Then begin
      BlobStream := TBlobStream.Create(TBlobField(aQuery.Fields[3]), bmRead);
      FpStreamer.Stream.LoadFromStream(BlobStream);
      BlobStream.Free;
    end;

    FpStreamer.ReadStream;
    FpStreamer.Stream.Size := 0;

    Result := True;
  end;  //  --  If Not pQuery.EoF Then
  FreeQuery(nQueryID);
end;

function TMyDACBaseDB.CreateUser(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    aQuery : TMySQLQuery;
    aBlob : TBlob;
begin
  Result := False;
  If pData = nil Then
    Exit;

  nQueryID := RequestQuery(True);
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_INSERT);
  aQuery.Prepare;
  aQuery.ParamByName('Login').AsString := pData^.Login;
  aQuery.ParamByName('GroupID').AsInteger := pData^.GroupID;

  FpStreamer.Data := pData;
  FpStreamer.WriteStream(6558);

  aBlob := aQuery.ParamByName('Account_Data').AsBlobRef;
  aBlob.Clear;
  aBlob.LoadFromStream(FpStreamer.Stream);
  aBlob.Commit;

  __ExecuteQuery(nQueryID);

  FreeQuery(nQueryID);
  Result := True;
end;

function TMyDACBaseDB.UpdateUser(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    aQuery : TMySQLQuery;
    aBlob : TBlob;
begin
  Result := False;
  If pData = nil Then
    Exit;

  If pData^.ID = 0 Then
    Exit;

  nQueryID := RequestQuery(True);
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_UPDATE);
  aQuery.Prepare;
  aQuery.ParamByName('ID').AsInteger := pData^.ID;
  aQuery.ParamByName('GroupID').AsInteger := pData^.GroupID;

  FpStreamer.Data := pData;
  FpStreamer.WriteStream(6558);

  aBlob := aQuery.ParamByName('Account_Data').AsBlobRef;
  aBlob.Clear;
  aBlob.LoadFromStream(FpStreamer.Stream);
  aBlob.Commit;

  __ExecuteQuery(nQueryID);

  FreeQuery(nQueryID);
  Result := True;
end;

function TMyDACBaseDB.DeleteUser(pData: PUserDataRec): Boolean;
var nQueryID : Integer;
    aQuery : TMySQLQuery;
begin
  Result := False;
  If pData = nil Then
    Exit;

  If pData^.ID = 0 Then
    Exit;

  nQueryID := RequestQuery(True);
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_DELETE);
  aQuery.Prepare;
  aQuery.ParamByName('ID').AsInteger := pData^.ID;
  __ExecuteQuery(nQueryID);

  FreeQuery(nQueryID);
  Result := True;
end;

procedure TMyDACBaseDB.ListUsers(UserList: TStringList; GroupID: Integer);
var nQueryID : Integer;
    aQuery : TMySQLQuery;
begin
  nQueryID := RequestQuery(False);
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  If GroupID < 1 Then begin
    aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_LIST_ALL);
    aQuery.Prepare;
  end Else begin
    aQuery.SQL.Text := RequestSQL(SQL_ID_USERS_LIST_GROUP);
    aQuery.Prepare;
    aQuery.ParamByName('GroupID').AsInteger := GroupID;
  end;  //  --  If GroupID = -1 Then

  __ExecuteQuery(nQueryID);
  While Not aQuery.EoF Do begin
    UserList.AddObject(aQuery.Fields[1].AsString, Pointer(aQuery.Fields[0].AsInteger));
    aQuery.Next;
  end;  //  --  While Not pQuery.EoF Do
  aQuery.Close;
  FreeQuery(nQueryID);
end;

function TMyDACBaseDB.GetGroupData(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    aQuery : TMySQLQuery;
begin
  Result := inherited DeleteGroup(pGroup);
  nQueryID := RequestQuery(False);
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  aQuery.SQL.Text := RequestSQL(SQL_ID_GROUP_SELECT_ID);
  aQuery.Prepare;
  aQuery.ParamByName('ID').AsInteger := pGroup^.GroupID;
  __ExecuteQuery(nQueryID);
  If aQuery.Eof = False Then begin
    pGroup^.GroupID := aQuery.Fields[0].AsInteger;
    pGroup^.GroupName := aQuery.Fields[1].AsString;
    pGroup^.SL := Byte(aQuery.Fields[2].AsInteger);
  end;
  FreeQuery(nQueryID);
end;

function TMyDACBaseDB.CreateGroup(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    aQuery : TMySQLQuery;
begin
  Result := inherited CreateGroup(pGroup);
  nQueryID := RequestQuery(True);
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  aQuery.SQL.Text := RequestSQL(SQL_ID_GROUP_INSERT);
  aQuery.Prepare;
  aQuery.ParamByName('Name').AsString := pGroup^.GroupName; // the trigger will set the ID
  If pGroup^.SL > ConnectionData.SecurityLevel Then
    pGroup^.SL := 1;
  aQuery.ParamByName('SL').AsSmallInt := pGroup^.SL;
  __ExecuteQuery(nQueryID);
  If aQuery.Transaction.Active Then
    aQuery.Transaction.Commit;
  FreeQuery(nQueryID);
end;

function TMyDACBaseDB.UpdateGroup(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    aQuery : TMySQLQuery;
begin
  Result := inherited UpdateGroup(pGroup);
  nQueryID := RequestQuery(True);
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  aQuery.SQL.Text := RequestSQL(SQL_ID_GROUP_UPDATE);
  aQuery.Prepare;
  aQuery.ParamByName('ID').AsInteger := pGroup^.GroupID;
  aQuery.ParamByName('Name').AsString := pGroup^.GroupName;
  aQuery.ParamByName('SL').AsSmallInt := pGroup^.SL;
  __ExecuteQuery(nQueryID);
  If aQuery.Transaction.Active Then
    aQuery.Transaction.Commit;
  FreeQuery(nQueryID);
end;

function TMyDACBaseDB.DeleteGroup(pGroup: PGroupDataRec): Integer;
var nQueryID : Integer;
    aQuery : TMySQLQuery;
begin
  Result := inherited DeleteGroup(pGroup);
  nQueryID := RequestQuery(True);
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  aQuery.SQL.Text := RequestSQL(SQL_ID_GROUP_DELETE);
  aQuery.Prepare;
  aQuery.ParamByName('ID').AsInteger := pGroup^.GroupID;
  __ExecuteQuery(nQueryID);
  FreeQuery(nQueryID);
end;

function TMyDACBaseDB.ListGroups(GroupList: TList): Integer;
var nQueryID : Integer;
    aQuery : TMySQLQuery;
    pGroup : PGroupDataRec;
begin
  Result := inherited ListGroups(GroupList);
  nQueryID := RequestQuery(False);
  aQuery := TMySQLQuery(QueryList.Items[nQueryID]);
  aQuery.SQL.Text := RequestSQL(SQL_ID_GROUP_SELECT);
  aQuery.Prepare;
  __ExecuteQuery(nQueryID);
  While Not aQuery.EoF Do begin
    New(pGroup);
    pGroup^.GroupID := aQuery.Fields[0].AsInteger;
    pGroup^.GroupName := aQuery.Fields[1].AsString;
    pGroup^.Flags := []; // TODO
    pGroup^.SL := aQuery.Fields[2].AsInteger;
    GroupList.Add(pGroup);
//    GroupList.AddObject(pQuery.Fields[1].AsString, Pointer(pQuery.Fields[0].AsInteger));
    aQuery.Next;
  end;  //  --  While Not pQuery.EoF Do
  aQuery.Close;
  FreeQuery(nQueryID);
end;

end.
