{*******************************************************************************
Name         : uIBBaseDB.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007-2009 by Helge Lange
Info         : HelgeLange@gmail.com
Website      : http://www.maf-components.com
Date         : 19.03.2007
Last Update  : 2.09.2013
Version      : 1.1.012
Purpose      : FIBPlus, IBX, IBDAC and UniDAC support BaseDB implementation

               FIBPlus components can be found under
               http://www.devrace.com/en/fibplus/

               IBX comes with Delphi

               IBDAC and UniDAC can be found at
               http://www.devart.com

Last Changes :

1.1.012 (02.09.2013) -----------------------------------------------------------
- [FIX] when disconnecting (Property Connected = False) there was no check if a
        transaction was assigned. This is fixed now 
1.1.011 (13.10.2009) -----------------------------------------------------------
- [CHG] changed the table structure for users to place all but ID, Login and
        GroupID into a stream, which makes it easier to handle AnsiString and
        UniCode passwords and also is easier to extend if needed
1.1.010 (06.09.2009) -----------------------------------------------------------
- [ADD] Devart UniDAC support
- [ADD] DevArt IBDAC support
- [CHG] small changes in object hirarchy for the new DACs to support. Now the
        base class for firebird databases is called TCustomIBBaseDB. TIBBaseDB
        is now the base class for the IBX and FIBlus version, because both
        have the same code base FIBComponents
1.1.009 (30.07.2009) -----------------------------------------------------------
- [DEL] removed obsolete code for DynamicFunction- and Module-Administration
        as they're now handled by templates
1.1.008 (03.11.2008) -----------------------------------------------------------
- [CHG] uFIBBaseDB.pas and uIBXBaseDB.pas now together in one file as the
        differences are very small and it made no sense to have 2 code bases
        For IBX package it uses the compiler conditional "IBX", for FIBPlus it
        is "FIBPlus"
1.0.007 (29.10.2008) -----------------------------------------------------------
- [FIX] EnumDynamicFunction now reads, if a Hook is activated or not.
- [CHG] EnumDynamicFunction now reads in DesignTime ALL Hooks (activated or not)
        and applies the b_Active flag only in runtime. This additional code can
        be removed for release modules with the compiler Switch "RELEASE"
- [ADD] EnableDynamicFuntion to enable/disable a dynamic function from the outside
1.0.006 (24.05.2007) -----------------------------------------------------------
- [FIX] fixed a bug in WriteTemplate, where inherited called ReadTemplate, didn't
        do any harm, because both do the same, but ... anyway.. fixed now
1.0.005 (23.05.2007) -----------------------------------------------------------
- [ADD] RequestQuery now calls inherited to execute the event OnCreateQuery
1.0.004 (22.04.2007) -----------------------------------------------------------
- [FIX] fixed a bug in __InternalDisconnect wich occured when no database or
        transactions was assigned and the component was loaded from the dfm
- [FIX] fixed a bug __InternalConnect, when no database or transaction was
        assinged, but the user set Connected to TRUE
1.0.003 (03.04.2007) -----------------------------------------------------------
- [ADD] Write/Delete entries from SecurityLayerTable
1.0.002 (26.03.2007) -----------------------------------------------------------
- [ADD] added generic template support
- [ADD] Blob read/write-method
- [ADD] added the property UpdateTransaction to support FIBPlus' strategy of
        seperating read-only- and write-transactions
1.0.001 (19.03.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uIBBaseDB;

{$I ..\MAF_Base\MAFramework.inc}

interface

{$IFDEF UniDAC}
  {$DEFINE DevArtDAC}
{$ENDIF}
{$IFDEF IBDAC}
  {$DEFINE DevArtDAC}
{$ENDIF}

{$IFDEF FIBPlus}
  {$DEFINE FIBComponents}
{$ENDIF}
{$IFDEF IBX}
  {$DEFINE FIBComponents}
{$ENDIF}

uses SysUtils, Classes, Windows, DB,
     {$IFDEF FIBPlus}
     // FIB units
     FIBDatabase, pFIBDatabase, FIBQuery, pFIBQuery, ibase, FIBDataSet,
     pFIBDataSet, FIBMiscellaneous,
     {$ENDIF}
     {$IFDEF IBX}
     IBCustomDataSet, IBDatabase, IB, IBHeader, IBQuery, IBBlob, IBSQL,
     {$ENDIF}
     {$IFDEF IBDAC}
     MemDS, DBAccess, IBC, IBCCall, MemData,
     {$ENDIF}
     {$IFDEF UniDAC}
     MemDS, DBAccess, Uni, MemData, UniProvider, CRConnectionPool,
     {$ENDIF}
     // Modular Application Framework Component units
     uMAF_Globals, uMAF_CustomBaseDB, uMAF_HookManager_Helper,
     uMAF_TemplateStreamer, uMAF_BaseDB;

Type {$IFDEF FIBPlus}
       TIB_DB = class(TpFIBDatabase);
       TIB_TR = class(TpFIBTransaction);
       TIB_DS = class(TpFIBDataSet);
       TIB_Q  = class(TpFIBQuery);
       TIB_BS = class(TFIBBlobStream);
     {$ENDIF}

     {$IFDEF IBX}
       TIB_DB = class(TIBDataBase);
       TIB_TR = class(TIBTransaction);
       TIB_DS = class(TIBDataSet);
       TIB_Q  = class(TIBSQL);
       TIB_BS = class(TIBBlobStream);
     {$ENDIF}

     {$IFDEF IBDAC}
       TIB_DB = class(TIBCConnection);
       TIB_TR = class(TIBCTransaction);
//       TIB_DS = class(TIBDataSet);
       TIB_Q  = class(TIBCQuery);
       TIB_BS = class(TBlobStream);
     {$ENDIF}

     {$IFDEF UniDAC}
       TIB_DB = class(TUniConnection);
       TIB_TR = class(TUniTransaction);
//       TIB_DS = class(TIBDataSet);
       TIB_Q  = class(TUniQuery);
       TIB_BS = class(TBlobStream);
     {$ENDIF}

     TCustomIBBaseDB = class(TmafBaseDB)
     private
       FsRole : String;
     protected
       procedure __SetRole(const Value: String); virtual;
       function __GetRole: String; virtual;
       function __GetConnected: Boolean; override;
       procedure __SwitchReadWriteQuery(AQuery: TComponent; bWrite: Boolean); override;
       function RequestSQL(nID: Integer): String; override;
       procedure ApplyChanges; override;

       function __RequestQuery(bWrite: Boolean = False; SQL_ID: Integer = -1): Integer; override;
       function __QueryHasSQL(AQuery: TComponent): Boolean; override;
       procedure __SetQueryParameter(AQuery: TComponent; FieldName: String; Value: Variant); override;
     public
       function GetDatabaseList(aList: TStrings): Integer; override;
     published
       property Connected;
       property ComputerName;
       property DataBaseName;
       property User;
       property Password;
       property Role : String read __GetRole write __SetRole;
       property DataBase;
       property Transaction;
       property UpdateTransaction;
       // events
       property OnConnect;
       property OnDisconnect;
       property OnCreateQuery;
     end;

Type {$IFDEF FIBComponents}

     TIBBaseDB = class(TCustomIBBaseDB)
     protected
       BlobID: TISC_QUAD;
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;
       procedure __ExecuteQuery(AQuery: TComponent); override;
       function EmptyBlobID: TISC_QUAD;
       procedure __ReadBlob(AQuery: TComponent; FieldName: String; aStream: TMemoryStream); override;
       procedure __WriteBlob(AQuery: TComponent; FieldName: String; aStream: TStream); override;
       // Users
       function CreateUser(pData: PUserDataRec) : Boolean; override;
     public
       // DataStrorage
       function WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer; override;
     end;

     {$IFDEF FIBPlus}
     TFIBBaseDB = class(TIBBaseDB);
     {$ENDIF}

     {$IFDEF IBX}
     TIBXBaseDB = class(TIBBaseDB);
     {$ENDIF}

     {$ENDIF}

     {$IFDEF DevArtDAC}
     TCustomDevArtBaseDB = class(TCustomIBBaseDB)
     protected
       procedure __ExecuteQuery(AQuery: TComponent); override;
       procedure __ReadBlob(AQuery: TComponent; FieldName: String; aStream: TMemoryStream); override;
       procedure __WriteBlob(AQuery: TComponent; FieldName: String; aStream: TStream); override;
     end;

     {$IFDEF IBDAC}
     TIBDACBaseDB = class(TCustomDevArtBaseDB)
     protected
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;
     end;
     {$ENDIF}   // IFDEF IBDAC

     {$IFDEF UniDAC}
     TUniDACBaseDB = class(TCustomDevArtBaseDB)
     protected
       procedure __SetRole(const Value: String); override;
       function __GetRole: String; override;
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;
     end;
     {$ENDIF}  // IFDEF UniDAC

     {$ENDIF}  // IFDEF DevArtDAC

procedure Register;

implementation

uses uBaseSQL_Consts,
     {$IFDEF FIBPlus}
     // FIB units
     pFIBProps,
     {$ENDIF}
     uBaseSQL_Consts_Firebird;

procedure Register;
begin
  {$IFDEF FIBPlus}
  RegisterComponents('MAF Database', [TFIBBaseDB]);
  {$ENDIF}

  {$IFDEF IBX}
  RegisterComponents('MAF Database', [TIBXBaseDB]);
  {$ENDIF}

  {$IFDEF IBDAC}
  RegisterComponents('MAF Database', [TIBDACBaseDB]);
  {$ENDIF}

  {$IFDEF UniDAC}
  RegisterComponents('MAF Database', [TUniDACBaseDB]);
  {$ENDIF}
end;


{-------------------------------------------------------------------------------
     Base class for all BaseDBs using DACs connecting to Firebird/Interbase

1 : IBDAC   -> TIBDACBaseDB
2 : UniDAC  -> TUniDACBaseDB
3 : FibPlus -> TFIBBaseDB
4 : IBX     -> TIBXBaseDB
-------------------------------------------------------------------------------}

{ TIBBaseDB }

procedure TCustomIBBaseDB.ApplyChanges;
begin
  If Assigned(UpdateTransaction) Then begin
    If TIB_TR(UpdateTransaction).Active Then
      TIB_TR(UpdateTransaction).Commit;
  end else
    If Assigned(Transaction) Then
      If TIB_TR(Transaction).Active Then
        TIB_TR(Transaction).CommitRetaining;
end;

function TCustomIBBaseDB.__GetConnected: Boolean;
begin
  Result := False;
  If Assigned(Database) Then
    Result := TIB_DB(Database).Connected;
end;








{-------------------------------------------------------------------------------
                    DevArt DACs supporting Firebird / Interbase

1 : IBDAC  -> TIBDACBaseDB
2 : UniDAC -> TUniDACBaseDB
-------------------------------------------------------------------------------}
{$IFDEF DevArtDAC}

{ TCustomDevArtBaseDB }  // base class for both DevArt DAC BaseDBs

procedure TCustomDevArtBaseDB.__ExecuteQuery(AQuery: TComponent);
begin
  TIB_Q(AQuery).Execute;
end;

procedure TCustomDevArtBaseDB.__ReadBlob(AQuery: TComponent; FieldName: String; aStream: TMemoryStream);
var BlobStream : TBlobStream;
begin
  inherited;
  If Not Assigned(aStream) Then
    Exit;
    
  If TIB_Q(AQuery).Fields.FieldByName(FieldName).IsBlob Then begin
    BlobStream := TBlobStream.Create(TBlobField(TIB_Q(AQuery).Fields.FieldByName(FieldName)), bmRead);
    aStream.LoadFromStream(BlobStream);
    BlobStream.Free;
  end;
end;

procedure TCustomDevArtBaseDB.__WriteBlob(AQuery: TComponent; FieldName: String; aStream: TStream);
var aBlob : TBlob;
begin
  aBlob := TIB_Q(AQuery).ParamByName('Data_Storage').AsBlobRef;
  aBlob.Clear;
  aBlob.LoadFromStream(aStream);
  aBlob.Commit;
end;

{$ENDIF}  // DevArtDAC






{-------------------------------------------------------------------------------
                                 DevArt IBDAC
-------------------------------------------------------------------------------}

{ TIBDACBaseDB }

{$IFDEF IBDAC}
procedure TIBDACBaseDB.__InternalConnect;
begin
  If Assigned(DataBase) Then begin
    TIB_DB(Database).Connected := False;
    TIB_DB(Database).Params.Clear;
    TIB_DB(Database).Database := DataBaseName;
    TIB_DB(Database).UserName := User;
    TIB_DB(Database).Password := Password;
    TIB_DB(Database).Database := DataBaseName;
    TIB_DB(Database).Connected := True;
    If TIB_DB(Database).Connected Then begin
      If Assigned(Transaction) Then
        TIB_TR(Transaction).Active := True;
    end else begin
      {$IFDEF Debug}
      //OutputDebugString(PChar(ModuleName + ': No connection to selected database, handle is 0'));
      {$ENDIF}
    end;
  end;
  inherited;
end;

procedure TIBDACBaseDB.__InternalDisconnect;
begin
  inherited;
  If Assigned(Transaction) Then
    If TIB_TR(Transaction).Active Then
      TIB_TR(Transaction).Active := False;
  If Assigned(Database) Then
    TIB_DB(Database).Connected := False;
end;
{$ENDIF}  // IBDAC






{-------------------------------------------------------------------------------
                                 DevArt UniDAC
-------------------------------------------------------------------------------}

{$IFDEF UniDAC}

procedure TUniDACBaseDB.__InternalConnect;
begin
  If Assigned(DataBase) Then begin
    TIB_DB(Database).Connected := False;
    If TIB_DB(Database).ProviderName = 'Interbase' Then
      TIB_DB(Database).SpecificOptions.Add('InterBase.Role='+FsRole);
    TIB_DB(Database).UserName := User;
    TIB_DB(Database).Password := Password;
    TIB_DB(Database).Database := DataBaseName;
    TIB_DB(Database).Connected := True;
    If TIB_DB(Database).Connected Then begin
      If Assigned(Transaction) Then begin
        If TIB_TR(Transaction).Active Then
          TIB_TR(Transaction).Commit;
        TIB_TR(Transaction).StartTransaction;
      end;  //  --  If Assigned(Transaction) Then 
    end else begin
      {$IFDEF Debug}
      //OutputDebugString(PChar(ModuleName + ': No connection to selected database, handle is 0'));
      {$ENDIF}
    end;
  end;
  inherited;
end;

procedure TUniDACBaseDB.__InternalDisconnect;
begin
  inherited;
  If ((Assigned(Transaction)) And (TIB_TR(Transaction).Active)) Then
    TIB_TR(Transaction).Commit;
  If Assigned(Database) Then
    TIB_DB(Database).Connected := False;
end;

procedure TUniDACBaseDB.__SetRole(const Value: String);
var S : String;
    i : Integer;
    bFound : Boolean;
begin
  If ((Not Assigned(Database)) Or (csLoading in ComponentState)) Then
    Exit;
  S := TIB_DB(Database).ProviderName + '.Role=';
  bFound := False;
  For i := 0 To TIB_DB(Database).SpecificOptions.Count - 1 Do
    If Pos(S, TIB_DB(Database).SpecificOptions.Strings[i]) > 0 Then begin
      If FsRole <> '' Then
        TIB_DB(Database).SpecificOptions.Strings[i] := S + FsRole
      Else
        TIB_DB(Database).SpecificOptions.Delete(i);
      bFound := True;
      Break;
    end;
  If Not bFound Then
    If FsRole <> '' Then
      TIB_DB(Database).SpecificOptions.Add(S + FsRole);
end;

function TUniDACBaseDB.__GetRole: String;
var S : String;
    i : Integer;
begin
  FsRole := '';
  If Database <> nil Then begin
    S := TIB_DB(Database).ProviderName + '.Role=';
    For i := 0 To TIB_DB(Database).SpecificOptions.Count - 1 Do
      If Pos(S, TIB_DB(Database).SpecificOptions.Strings[i]) > 0 Then begin
        FsRole := TIB_DB(Database).SpecificOptions.Strings[i];
        Delete(FsRole, 1, Length(S));
        Break;
      end;  //  --  If Pos(S, TIB_DB(Database).SpecificOptions.Strings[i]) > 0 Then
  end;  //  --  If Database <> nil Then
end;

{$ENDIF} // UniDAC


{-------------------------------------------------------------------------------
                        DACs based on FIBComponents

1: FibPlus
2: IBX
-------------------------------------------------------------------------------}

{$IFDEF FIBComponents}

procedure TIBBaseDB.__InternalConnect;
var Params : TStrings;
begin
  If Assigned(DataBase) Then begin
    TIB_DB(Database).Connected := False;
    {$IFDEF FIBPlus}
    Params := TIB_DB(Database).DBParams;
    TIB_DB(Database).DBName := DataBaseName;
    {$ENDIF}
    {$IFDEF IBX}
    Params := TIB_DB(Database).Params;
    TIB_DB(Database).DatabaseName := DataBaseName;
    {$ENDIF}
    Params.Clear;
    Params.Add('user_name=' + User);
    Params.Add('password=' + Password);
    If FsRole <> '' Then
      Params.Add('sql_role_name=' + FsRole);

    TIB_DB(Database).Connected := True;
    If TIB_DB(Database).Connected Then begin
      {$IFDEF Debug}
      //OutputDebugString(PChar(ModuleName + ': Database connected !'));
      {$ENDIF}
      If Assigned(Transaction) Then
        TIB_TR(Transaction).Active := True;
    end else begin
      {$IFDEF Debug}
      //OutputDebugString(PChar(ModuleName + ': No connection to selected database, handle is 0'));
      {$ENDIF}
    end;
  end;
  inherited;
end;

procedure TIBBaseDB.__InternalDisconnect;
begin
  inherited;
  If ((Assigned(Transaction)) And (TIB_TR(Transaction).Active)) Then
    TIB_TR(Transaction).Active := False;
  If Assigned(Database) Then
    TIB_DB(Database).Connected := False;
end;

// call __ReadBlob with aStream=nil to just get the BlobID stored
procedure TIBBaseDB.__ReadBlob(AQuery: TComponent; FieldName: String; aStream: TMemoryStream);
var BlobStream : TIB_BS;
    StreamPos  : Integer;
begin
  BlobID := TIB_Q(AQuery).FieldByName(FieldName).AsQuad;          // storing the BlobID
  If Not Assigned(aStream) Then
    Exit;

  StreamPos := aStream.Position;
  BlobStream := TIB_BS.Create; // Create the blob stream
  BlobStream.Database := TIB_DB(DataBase); // assign the database object
  BlobStream.Transaction := TIB_TR(Transaction); // and transaction object, too
  BlobStream.Mode := bmRead; // read-only mode
  BlobStream.BlobID := TIB_Q(AQuery).FieldByName(FieldName).AsQuad; // assign the ID for the blob
  BlobStream.SaveToStream(aStream); // and read the blob into our stream
  aStream.Position := StreamPos; // go back to previous position
  BlobStream.Finalize; // and close the blob
  BlobStream.Free;
end;

procedure TIBBaseDB.__WriteBlob(AQuery: TComponent; FieldName: String; aStream: TStream);
var BlobStream  : TIB_BS;
    StreamPos  : Integer;
begin
  If not Assigned(aStream) Then
    Exit;

  BlobStream := TIB_BS.Create; // Create the blob stream
  BlobStream.Database := TIB_DB(DataBase); // assign the database object
  If Assigned(UpdateTransaction) Then begin
    BlobStream.Transaction := TIB_TR(UpdateTransaction); // we use an update transaction if possible
    If Not TIB_TR(UpdateTransaction).Active Then
      TIB_TR(UpdateTransaction).StartTransaction;
  end Else
    BlobStream.Transaction := TIB_TR(Transaction); // and transaction object, too
  BlobStream.Mode := bmReadWrite; // read/write mode
  BlobStream.BlobID := BlobID; // assigning BlobID, will cause a Load, if exists
  StreamPos := aStream.Position; // remember the position
  BlobStream.LoadFromStream(aStream); // reading the data into the BlobStream
  aStream.Position := StreamPos;      // restoring the previous position
  BlobStream.Finalize; // saving the new data into the database
  TIB_Q(AQuery).ParamByName(FieldName).AsQuad := BlobStream.BlobID;
  BlobStream.Free;
end;

function TIBBaseDB.EmptyBlobID: TISC_QUAD;
begin
  Result.gds_quad_high := 0;
  Result.gds_quad_low := 0;
end;

procedure TIBBaseDB.__ExecuteQuery(AQuery: TComponent);
begin
  TIB_Q(AQuery).ExecQuery;
end;

function TIBBaseDB.CreateUser(pData: PUserDataRec): Boolean;
begin
  BlobID := EmptyBlobID;  // as it is a new user, no need to read the blob
  Result := inherited CreateUSer(pData);
end;

function TIBBaseDB.WriteTemplate(aStream: TMemoryStream; var nID: Integer; TableName: String; var Category, TemplateName: String): Integer;
begin
  BlobID := EmptyBlobID;
  Result := inherited WriteTemplate(aStream, nID, TableName, Category, TemplateName);
end;

{$ENDIF}

function TCustomIBBaseDB.GetDatabaseList(aList: TStrings): Integer;
begin
  Result := ERR_PARAM_FAILURE;
  If aList = nil Then
    Exit;
  aList.Clear;
  Result := ERR_NO_ERROR;
  If Assigned(Database) Then
    {$IFDEF DevArtDAC}
    If TIB_DB(Database).Database <> '' Then
      aList.Add(TIB_DB(Database).Database);
    {$ELSE}
    If TIB_DB(Database).DatabaseName <> '' Then
      aList.Add(TIB_DB(Database).DatabaseName);
    {$ENDIF}
end;

function TCustomIBBaseDB.RequestSQL(nID: Integer): String;
begin
  Result := inherited RequestSQL(nID);
  Case nID Of
    SQL_ID_USERS_CREATE    : Result := ReplaceTableIdent(sCREATE_USER_TABLE, UserTable);
    SQL_ID_TEMPLATE_CREATE : Result := ReplaceTableIdent(sCREATE_TEMPLATE_TABLE, DataStorageTable);
  end;  //  --  Case nID Of
end;

procedure TCustomIBBaseDB.__SetQueryParameter(AQuery: TComponent; FieldName: String; Value: Variant);
begin
  TIB_Q(AQuery).ParamByName(FieldName).Value := Value;
end;

function TCustomIBBaseDB.__QueryHasSQL(AQuery: TComponent): Boolean;
begin
  Result := False;
  If Assigned(AQuery) Then
    Result := (TIB_Q(AQuery).SQL.Text <> '');
end;

procedure TCustomIBBaseDB.__SetRole(const Value: String);
begin
  FsRole := Value;
end;

function TCustomIBBaseDB.__GetRole: String;
begin
  Result := FsRole;
end;

procedure TCustomIBBaseDB.__SwitchReadWriteQuery(AQuery: TComponent; bWrite: Boolean);
begin
  If UpdateTransaction = nil Then
    Exit;

  If bWrite Then begin
    TIB_Q(AQuery).Transaction := TIB_TR(UpdateTransaction);
    {$IFDEF FIBPlus}
    TIB_Q(AQuery).Options := [qoStartTransaction, qoAutoCommit];
    {$ENDIF}
  end else begin
    TIB_Q(AQuery).Transaction := TIB_TR(Transaction);
    {$IFDEF FIBPlus}
    TIB_Q(AQuery).Options := [];
    {$ENDIF}
  end;
end;

function TCustomIBBaseDB.__RequestQuery(bWrite: Boolean = False; SQL_ID: Integer = -1): Integer;
var FQ: TIB_Q;
begin
  Result := -1;
  If Not CheckRequirements Then
    Exit;
  FQ := TIB_Q.Create(nil);
  FQ.Name := COMPONENT_BASENAME_QUERY + IntToStr(__GetQueryID(QueryList, COMPONENT_BASENAME_QUERY));
  {$IFDEF DevArtDAC}
    FQ.Connection := TIB_DB(Database);
  {$ELSE}
    FQ.Database := TIB_DB(DataBase);
  {$ENDIF}
  FQ.ParamCheck := True;
  {$IFDEF DevArtDAC}
  {$ELSE}
    FQ.GoToFirstRecordOnExecute := True;
  {$ENDIF}
  If ((bWrite) And(UpdateTransaction <> nil)) Then begin
    FQ.Transaction := TIB_TR(UpdateTransaction);
    {$IFDEF FIBPlus}
    FQ.Options := [qoStartTransaction, qoAutoCommit];
    {$ENDIF}
    {$IFDEF DevArtDAC}
    FQ.AutoCommit := True;
    {$ENDIF}
  end Else
    FQ.Transaction := TIB_TR(Transaction);
  If SQL_ID > -1 Then begin
    FQ.SQL.Text := RequestSQL(SQL_ID);
    FQ.Prepare;
  end;  //  --  If SQL_ID > -1 Then
  If Not FQ.Transaction.Active Then
    FQ.Transaction.StartTransaction;
  Result := QueryList.Add(FQ);
  inherited __RequestQuery;
end;

end.
