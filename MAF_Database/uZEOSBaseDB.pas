{*******************************************************************************
Name         : uZEOSBaseDB.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007 by Helge Lange
Info         : HelgeLange@gmail.com
Date         : 13.05.2007
Last Update  : 13.05.2007
Version      : 1.0.001
Purpose      : ZEOS Database connection support component for ERP Framework
               Components
Last Changes :

1.0.001 (13.05.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uZEOSBaseDB;

interface

uses SysUtils, Classes, Windows, DB,
     // Zeos units
     ZDataset, ZAbstractRODataset, ZAbstractDataset, ZConnection,
     // ERP Framework units
     ERP_Globals, uERP_Tools, uUserRights_Shared, uERPCustomBaseDB;

Type TZEOSBaseDB = class(TERPCustomBaseDB)
     private
     protected
       function __GetConnected: Boolean; override;
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;

       function RequestQuery(bWrite: Boolean = False): Integer; override;
     public
       // modules / plugin - support
       procedure EnumModules; override;
       function AddModule(nID: Integer; LibName: String; bHasRes: Boolean): Integer; override;
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
     end;

implementation

{ TZEOSBaseDB }

function TZEOSBaseDB.__GetConnected: Boolean;
begin
  Result := False;
  If Assigned(Database) Then
    Result := TZConnection(Database).Connected;
end;

procedure TZEOSBaseDB.__InternalConnect;
begin
  If Assigned(Database) Then begin
    TZConnection(DataBase).Database := DataBaseName;
    TZConnection(DataBase).Password := Password;
    TZConnection(DataBase).User := User;

    TZConnection(DataBase).Connected := True;
  end;  //  --  If Assigned(Database) Then 
  inherited;
end;

procedure TZEOSBaseDB.__InternalDisconnect;
begin
  inherited;
  If Assigned(Database) Then
    TZConnection(Database).Connected := False;
end;

function TZEOSBaseDB.RequestQuery(bWrite: Boolean): Integer;
var Query : TZQuery;
begin
  Result := -1;
  If ((Assigned(Database)) And (TZConnection(Database).Connected)) Then begin
    Query := TZQuery.Create(nil);
    Query.Name := COMPONENT_BASENAME_QUERY + IntToStr(__GetQueryID(QueryList, COMPONENT_BASENAME_QUERY));
    Query.ReadOnly := Not bWrite;
    Query.Connection := TZConnection(Database);
    Result := QueryList.Add(Query);
  end;  //  --  If ((Assigned(Database)) And (TZConnection(Database).Connected)) Then
  inherited RequestQuery;
end;

procedure TZEOSBaseDB.EnumModules;
var nQueryID: Integer;
    Query : TZQuery;
begin
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TZQuery(QueryList.Items[nQueryID]);

    Query.SQL.Text := RequestSQL(SQL_ID_MODULE_SELECT);
    Query.ExecSQL;
    While Not Query.EoF Do begin
      If Assigned(OnEnumModules) Then
        OnEnumModules(Self, Query.Fields[0].AsInteger, Query.Fields[1].AsString, (Query.Fields[2].AsInteger = 1));
      Query.Next;
    end;  //  --  While Not Query.EoF Do
    FreeQuery(nQueryID);
  end;  //  --  If nQueryID > -1 Then
end;

function TZEOSBaseDB.AddModule(nID: Integer; LibName: String; bHasRes: Boolean): Integer;
var nQueryID : Integer;
    Query : TZQuery;
begin
  If ((nID < 0) Or (LibName = EmptyStr)) Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;  //  --  If ((nID < 0) Or (LibName = EmptyStr)) Then

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery(True); // request write query
  If nQueryID > -1 Then begin
    Query := TZQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_MODULE_INSERT);
    Query.Prepare;
    TpFIBQuery(QueryList.Items[nQueryID]).ParamByName('ModuleID').AsInteger := nID;
    TpFIBQuery(QueryList.Items[nQueryID]).ParamByName('Name').AsString := LibName;
    If bHasRes Then
      TpFIBQuery(QueryList.Items[nQueryID]).ParamByName('HasRes').AsInteger := 1
    Else
      TpFIBQuery(QueryList.Items[nQueryID]).ParamByName('HasRes').AsInteger := 0;
    TpFIBQuery(QueryList.Items[nQueryID]).ExecQuery;
    TpFIBTransaction(Transaction).CommitRetaining;
    FreeQuery(nQueryID);
    Result := ERR_NO_ERROR;
  end;  //  --  If nQueryID > -1 Then
end;

end.
