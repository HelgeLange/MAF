unit uMAFAbsoluteBaseDB;

interface

uses SysUtils, Classes, Windows, DB,
     // Absolute Database units
     ABSMain,
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_CustomBaseDB, uMAF_HookManager_Helper, uMAF_BaseDB,
     uMAF_TemplateStreamer;

Type TAbsoluteBaseDB = class(TmafBaseDB)
     private
       procedure PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName: String);
       procedure __SwitchReadWriteQuery(nQueryID: Integer; bWrite: Boolean);
     protected
       function __GetConnected: Boolean; override;
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;
       function RequestQuery(bWrite: Boolean = false): Integer; override;
       function RequestSQL(nID: Integer): String; override;
     public
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
       property Transaction;
       // events
       property OnConnect;
       property OnDisconnect;
     end;

procedure Register;

implementation

const sINSERT_TEMPLATE     : String = 'INSERT INTO %Ident_DataStorageTable% (DataID, Data_Category, Data_Name, Data_Storage) VALUES (:DataID, :Data_Category, :Data_Name, :Data_Storage)';

procedure Register;
begin
  RegisterComponents('MAF Database', [TAbsoluteBaseDB]);
end;

{ TAbsoluteBaseDB }

function TAbsoluteBaseDB.RequestQuery(bWrite: Boolean): Integer;
var aQuery : TABSQuery;
begin
  aQuery := TABSQuery.Create(nil);
  aQuery.DatabaseName := TABSDatabase(Database).DatabaseName;
  aQuery.SessionName := TABSSession(Transaction).SessionName;
  aQuery.ReadOnly := Not bWrite;

  Result := QueryList.Add(aQuery);
  // triggers OnRequestQuery and allows to change for the query setting there
  inherited RequestQuery(bWrite);
end;

function TAbsoluteBaseDB.RequestSQL(nID: Integer): String;
begin
  Result := inherited RequestSQL(nID);
  Case nID Of
    SQL_ID_TEMPLATE_INSERT  : Result := ReplaceTableIdent(sINSERT_TEMPLATE, DataStorageTable);
  end;
end;

function TAbsoluteBaseDB.__GetConnected: Boolean;
begin
  Result := False;
  If Assigned(Database) Then
    Result := TABSDatabase(Database).Connected;
end;

procedure TAbsoluteBaseDB.__InternalConnect;
begin
  If Not Assigned(Database) Then
    Exit;

  TABSDatabase(Database).Connected := False;
  TABSDatabase(Database).Password := Password;
  TABSDatabase(Database).Connected := True;
  inherited;
end;

procedure TAbsoluteBaseDB.__InternalDisconnect;
begin
  inherited;
  If Assigned(Database) Then
    TABSDatabase(Database).Connected := False;
end;

procedure TAbsoluteBaseDB.__SwitchReadWriteQuery(nQueryID: Integer; bWrite: Boolean);
begin
  TABSQuery(QueryList.Items[nQueryID]).ReadOnly := (Not bWrite);
end;

procedure TAbsoluteBaseDB.PrepareTemplateQuery(var nQueryID: Integer; nID: Integer; TabName, sCat, sName: String);
var aQuery : TABSQuery;
begin
  aQuery := TABSQuery(QueryList.Items[nQueryID]);
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

function TAbsoluteBaseDB.ReadTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String; bReadData: Boolean): Integer;
var nQueryID : Integer;
    BlobStream : TStream;
    aQuery : TABSQuery;
begin
  Result := inherited ReadTemplate(aStream, nID, TableName, Category, TemplateName);
  If Result <> ERR_NO_ERROR Then
    Exit;
  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery;
  aQuery := TABSQuery(QueryList.Items[nQueryID]);
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
        If aQuery.Fields[3].IsBlob Then begin
          BlobStream := aQuery.CreateBlobStream(aQuery.Fields[3], bmRead);
          aStream.LoadFromStream(BlobStream);
          BlobStream.Free;
        end;
      Result := ERR_NO_ERROR; // everything ok here
    end;  //  --  If Not TpFIBQuery(QueryList.Items[nQueryID]).EoF Then
    FreeQuery(nQueryID); // free the query
  end;
end;

function TAbsoluteBaseDB.WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer;
var nQueryID, nHighID : Integer;
    aQuery : TABSQuery;
begin
  Result := inherited WriteTemplate(aStream, nID, TableName, Category, TemplateName);
  If Result <> ERR_NO_ERROR Then
    Exit;

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery(False);
  aQuery := TABSQuery(QueryList.Items[nQueryID]);
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
      aQuery.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_UPDATE);
      aQuery.Prepare;
      aQuery.ParamByName('DataID').AsInteger := nID;
    end else begin
      // create new template
      aQuery.Close;
      aQuery.SQL.Text := 'SELECT Max(DataID) FROM ' + DataStorageTable;
      aQuery.Open;
      If not aQuery.Eof Then
        nHighID := aQuery.Fields[0].AsInteger + 1
      Else
        nHighID := 1;
      aQuery.Close;
      __SwitchReadWriteQuery(nQueryID, True);  // now we want to write
      nID := -1; // we don't know the ID yet
      aQuery.SQL.Text := RequestSQL(SQL_ID_TEMPLATE_INSERT);
      aQuery.Prepare;
      aQuery.ParamByName('DataID').AsInteger := nHighID;
      aQuery.ParamByName('Data_Category').AsString := Category;
      aQuery.ParamByName('Data_Name').AsString := TemplateName;
    end;
    aQuery.Params.CreateParam(ftBlob, 'Data_Storage', ptInput);
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

end.
