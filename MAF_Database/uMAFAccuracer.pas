unit uMAFAccuracer;

interface

uses SysUtils, Classes, Windows, DB,
     ACRMain,
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_CustomBaseDB, uMAF_HookManager_Helper, uMAF_BaseDB,
     uMAF_TemplateStreamer, uBaseSQL_Consts_Accuracer;

Type TAccuracerBaseDB = class(TmafBaseDB)
     private
       aTable : TACRTable;
       procedure __Create_UserTable;
       procedure __Create_GroupTable;
       procedure __Create_DataStorageTable;
       procedure __SwitchReadWriteQuery(nQueryID: Integer; bWrite: Boolean);
       procedure PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName: String);
     protected
       function RequestSQL(nID: Integer): String; override;
       function RequestQuery(bWrite: Boolean = false): Integer; override;
       procedure __ExecuteQuery(nQueryID: Integer);
       function __GetConnected: Boolean; override;
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;
     public
       // Templates
       function ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String; bReadData: Boolean = True): Integer; override;
       function WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer; override;
     published
       property Connected;
       property ComputerName;
       property User;
       property Password;
       property DataBase;
       // events
       property OnConnect;
       property OnDisconnect;
     end;

     TmafDB = class(TACRDatabase);
     TmafQuery = class(TACRQuery);
     TmafTable = class(TACRTable);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MAF Database', [TAccuracerBaseDB]);
end;

{ TAccuracerBaseDB }

procedure TAccuracerBaseDB.__Create_DataStorageTable;
begin
  aTable.TableName := DataStorageTable;
  If Not TmafTable(aTable).Exists Then begin
    aTable.FieldDefs.Clear;
    aTable.FieldDefs.Add('Data_ID', ftAutoInc, 0, False);
    aTable.FieldDefs.Add('Data_Category', ftString, 30, True);
    aTable.FieldDefs.Add('Data_Name', ftString, 30, True);
    aTable.FieldDefs.Add('Data_Storage', ftBlob);
    aTable.IndexDefs.Add('PrimaryKey', 'Data_ID', [ixPrimary, ixUnique]);
    aTable.CreateTable;
  end;
end;

procedure TAccuracerBaseDB.__Create_GroupTable;
begin
  aTable.TableName := GroupTable;
  If Not TmafTable(aTable).Exists Then begin


  end;
end;

procedure TAccuracerBaseDB.__Create_UserTable;
begin
  aTable.TableName := UserTable;
  If Not TmafTable(aTable).Exists Then begin


  end;
end;

function TAccuracerBaseDB.RequestQuery(bWrite: Boolean): Integer;
var aQuery : TMafQuery;
begin
  aQuery := TMafQuery.Create(nil);
  aQuery.DatabaseName := TmafDB(Database).DatabaseName;
  aQuery.ReadOnly := Not bWrite;

  Result := QueryList.Add(aQuery);
  // triggers OnRequestQuery and allows to change for the query setting there
  inherited RequestQuery(bWrite);
end;

function TAccuracerBaseDB.RequestSQL(nID: Integer): String;
begin
  Result := inherited RequestSQL(nID);
  Case nID Of
    SQL_ID_TEMPLATE_SELECT_ID : Result := ReplaceTableIdent(sQUERY_TEMPLATE_ID, DataStorageTable);
    SQL_ID_TEMPLATE_SELECT_NAME : Result := ReplaceTableIdent(sQUERY_TEMPLATE_NAME, DataStorageTable);
    SQL_ID_TEMPLATE_UPDATE  : Result := ReplaceTableIdent(sUPDATE_TEMPLATE, DataStorageTable);
    SQL_ID_TEMPLATE_INSERT  : Result := ReplaceTableIdent(sINSERT_TEMPLATE, DataStorageTable);
  end;
end;

procedure TAccuracerBaseDB.__ExecuteQuery(nQueryID: Integer);
begin
  TMafQuery(QueryList.Items[nQueryID]).ExecSQL;
end;

function TAccuracerBaseDB.__GetConnected: Boolean;
begin
  Result := False;
  If Assigned(Database) Then
    Result := TmafDB(Database).Connected;
end;

procedure TAccuracerBaseDB.__InternalConnect;
begin
  If Not Assigned(Database) Then
    Exit;

  // accuracer does not seem to have user login support
//  TmafDB(Database).Connected := False;
//  TmafDB(Database).Username := User;
//  TmafDB(Database).Password := Password;
  TmafDB(Database).Connected := True;

  aTable := TACRTable.Create(nil);
  aTable.DatabaseName := TmafDB(Database).DatabaseName;
//  __Create_UserTable;
//  __Create_GroupTable;
  __Create_DataStorageTable;
  aTable.Free;
  inherited;
end;

procedure TAccuracerBaseDB.__InternalDisconnect;
begin
  inherited;
  If Assigned(Database) Then
    TMafDB(Database).Connected := False;
end;

procedure TAccuracerBaseDB.__SwitchReadWriteQuery(nQueryID: Integer; bWrite: Boolean);
begin
  TMafQuery(QueryList.Items[nQueryID]).ReadOnly := (Not bWrite);
end;

procedure TAccuracerBaseDB.PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName: String);
var aQuery : TMafQuery;
begin
  aQuery := TMafQuery(QueryList.Items[nQueryID]);
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

function TAccuracerBaseDB.ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String;  bReadData: Boolean): Integer;
var nQueryID : Integer;
    BlobStream : TACRBlobStream;
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
    If Not TmafQuery(QueryList.Items[nQueryID]).EoF Then begin
      // filling all 3
      nID := TmafQuery(QueryList.Items[nQueryID]).Fields[0].AsInteger;
      Category := TmafQuery(QueryList.Items[nQueryID]).Fields[1].AsString;
      TemplateName := TmafQuery(QueryList.Items[nQueryID]).Fields[2].AsString;
      If bReadData Then
        If TmafQuery(QueryList.Items[nQueryID]).Fields[3].IsBlob Then begin
          BlobStream := TACRBlobStream.Create(TBlobField(TmafQuery(QueryList.Items[nQueryID]).Fields[3]), bmRead);
          aStream.LoadFromStream(BlobStream);
          BlobStream.Free;
        end;
      Result := ERR_NO_ERROR; // everything ok here
    end;  //  --  If Not TpFIBQuery(QueryList.Items[nQueryID]).EoF Then
    FreeQuery(nQueryID); // free the query
  end;
end;

function TAccuracerBaseDB.WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer;
var nQueryID : Integer;
//    aBlob : TBlob;
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
    If Not TmafQuery(QueryList.Items[nQueryID]).EoF Then begin
      // update mode
      nID := TmafQuery(QueryList.Items[nQueryID]).Fields[0].AsInteger; // we need the ID in any case
      Category := TmafQuery(QueryList.Items[nQueryID]).Fields[1].AsString; // return for users convenience
      TemplateName := TmafQuery(QueryList.Items[nQueryID]).Fields[2].AsString;
      TmafQuery(QueryList.Items[nQueryID]).Close;
      __SwitchReadWriteQuery(nQueryID, True);  // now we want to write
      TmafQuery(QueryList.Items[nQueryID]).SQL.Text := RequestSQL(SQL_ID_TEMPLATE_UPDATE);
      TmafQuery(QueryList.Items[nQueryID]).Prepare;
      TmafQuery(QueryList.Items[nQueryID]).ParamByName('DataID').AsInteger := nID;
    end else begin
      // create new template
      TmafQuery(QueryList.Items[nQueryID]).Close;
      __SwitchReadWriteQuery(nQueryID, True);  // now we want to write
      nID := -1; // we don't know the ID yet
      TmafQuery(QueryList.Items[nQueryID]).SQL.Text := RequestSQL(SQL_ID_TEMPLATE_INSERT);
      TmafQuery(QueryList.Items[nQueryID]).Prepare;
      TmafQuery(QueryList.Items[nQueryID]).ParamByName('Data_Category').AsString := Category;
      TmafQuery(QueryList.Items[nQueryID]).ParamByName('Data_Name').AsString := TemplateName;
    end;
    TmafQuery(QueryList.Items[nQueryID]).ParamByName('Data_Storage').LoadFromStream(aStream, ftBlob);
{     aBlob := TmafQuery(QueryList.Items[nQueryID]).ParamByName('Data_Storage').AsBlobRef;
     aBlob.Clear;
     aBlob.LoadFromStream(aStream);
     aBlob.Commit; }
    __ExecuteQuery(nQueryID);
    If nID = -1 Then begin
      // we inserted a new template, we return the ID
      PrepareTemplateQuery(nQueryID, nID, TableName, Category, TemplateName); // will cause to prepare a query through Category/TemplateName
      __SwitchReadWriteQuery(nQueryID, False); // just reading... promise!
      __ExecuteQuery(nQueryID);
      If Not TmafQuery(QueryList.Items[nQueryID]).EoF Then
        nID := TmafQuery(QueryList.Items[nQueryID]).Fields[0].AsInteger;
    end;
    FreeQuery(nQueryID);
  end;  //  --  If nQueryID > -1 Then
end;

end.
