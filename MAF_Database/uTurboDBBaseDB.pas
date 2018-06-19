{*******************************************************************************
Name         : uTurboDBBaseDB.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2008 by Helge Lange
Info         : HelgeLange@gmail.com
Date         : 30.11.2008
Last Update  : 30.11.2008
Version      : 1.0.001
Purpose      : TurboDB support BaseDB implementation

               TurboDB components can be found under
               http://www.dataweb.de/
Last Changes :

1.0.001 (30.11.2008) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uTurboDBBaseDB;

interface

uses SysUtils, Classes, Windows, DB,
     // TurboDB units
     TdbDataSet, TdbQuery,
     // ERP Framework Component units
     ERP_Globals, uUserRights_Shared, uERPCustomBaseDB;

Type TTurboDBBaseDB = class(TERPCustomBaseDB)
     protected
       function __GetConnected: Boolean; override;
       procedure __InternalConnect; override;
       procedure __InternalDisconnect; override;

       function RequestQuery(bWrite: Boolean = false): Integer; override;
     public
       function GetDatabaseList(aList: TStrings): Integer; override;
       // modules / plugin - support
       procedure EnumModules; override;
       function AddModule(nID: Integer; LibName: String; bHasRes: Boolean): Integer; override;
       function RemoveModule(FileName: String): Integer; override;
       // dynamic functions
       procedure EnumDynamicFunctions; override;
       function AddDynamicFunction(pToken: PHookInstallToken): Integer; override;
       function RemoveDynamicFunction(pToken: PHookInstallToken): Integer; override;
       function EnableDynamicFuntion(pToken: PHookInstallToken): Integer; override;
     published
       property Connected;
       property ComputerName;
       property DataBaseName;
       property HookTable;
       property PlugInTable;
       property DataBase;
       property Transaction;
       property HookLoadOptions;
       // events
       property OnConnect;
       property OnDisconnect;
       property OnCreateQuery;
     end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ERP Base', [TTurboDBBaseDB]);
end;

{ TTurboDBBaseDB }

function TTurboDBBaseDB.__GetConnected: Boolean;
begin
  Result := False;
  If Assigned(Database) Then
    Result := TTdbDatabase(Database).Connected;
end;

procedure TTurboDBBaseDB.__InternalConnect;
begin
  TTdbDatabase(Database).Location := DataBaseName;
  TTdbDatabase(Database).Connected := True;
  inherited;
end;

procedure TTurboDBBaseDB.__InternalDisconnect;
begin
  If TTdbDatabase(Database).Connected Then
    TTdbDatabase(Database).Connected := False;
end;

function TTurboDBBaseDB.RequestQuery(bWrite: Boolean): Integer;
var Query : TTdbQuery;
begin
  Result := -1;
  If Not CheckRequirements Then
    Exit;

  Query := TTdbQuery.Create(nil);
  Query.Name := COMPONENT_BASENAME_QUERY + IntToStr(__GetQueryID(QueryList, COMPONENT_BASENAME_QUERY));
  Query.DatabaseName := TTdbDatabase(Database).Name;
  Result := QueryList.Add(Query);
  inherited RequestQuery;
end;

function TTurboDBBaseDB.GetDatabaseList(aList: TStrings): Integer;
begin
  Result := ERR_PARAM_FAILURE;
  If aList = nil Then
    Exit;
  aList.Clear;
  Result := ERR_NO_ERROR;
  If Assigned(Database) Then
    If TTdbDatabase(Database).Location <> '' Then
      aList.Add(TTdbDatabase(Database).Location);
end;


//             Module support

function TTurboDBBaseDB.AddModule(nID: Integer; LibName: String;  bHasRes: Boolean): Integer;
var nQueryID : Integer;
    Query : TTdbQuery;
begin
  If ((nID < 0) Or (LibName = EmptyStr)) Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;  //  --  If ((nID < 0) Or (LibName = EmptyStr)) Then

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery(True); // request write query
  If nQueryID > -1 Then begin
    Query := TTdbQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_MODULE_INSERT);
    Query.Prepare;
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

procedure TTurboDBBaseDB.EnumModules;
var nQueryID: Integer;
    Query : TTdbQuery;
begin
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TTdbQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_MODULE_SELECT);
    Query.ExecSQL;
    While Not Query.EoF Do begin
      If Assigned(OnEnumModules) Then
        OnEnumModules(Self, Query.Fields[0].AsInteger,
                            Query.Fields[1].AsString,
                            (Query.Fields[2].AsInteger = 1));
      Query.Next;
    end;  //  --  While Not TpFIBQuery(QueryList.Items[nQueryID]).EoF Do
    FreeQuery(nQueryID);
  end;  //  --  If nQueryID > -1 Then

end;

function TTurboDBBaseDB.RemoveModule(FileName: String): Integer;
var nQueryID : Integer;
    Query : TTdbQuery;
begin
  If FileName = EmptyStr Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;  //  --  If FileName = EmptyStr Then

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery(True); // request write query
  If nQueryID > -1 Then begin
    Query := TTdbQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_MODULE_DELETE);
    Query.Prepare;
    Query.ParamByName('Name').AsString := FileName;
    Query.ExecSQL;
    FreeQuery(nQueryID);
    Result := ERR_NO_ERROR;
  end;  //  --  If nQueryID > -1 Then
end;

//         Dynamic functions support

function TTurboDBBaseDB.AddDynamicFunction(pToken: PHookInstallToken): Integer;
var nQueryID : Integer;
    Query : TTdbQuery;
begin
  If pToken = nil Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;  //  --  If pToken = nil Then

  Result := ERR_UNKNOWN_ERROR;
  nQueryID := RequestQuery(True); // request write query
  If nQueryID > -1 Then begin
    Query := TTdbQuery(QueryList.Items[nQueryID]);
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

function TTurboDBBaseDB.RemoveDynamicFunction(pToken: PHookInstallToken): Integer;
var nQueryID : Integer;
    Query : TTdbQuery;
begin
  Result := ERR_NO_ERROR;
  nQueryID := RequestQuery(True);
  If nQueryID > -1 Then begin
    Query := TTdbQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_HOOKS_DELETE);
    Query.Prepare;
    Query.ParamByName('HookID').AsInteger := pToken^.nHookID;
    Query.ParamByName('SubHookID').AsInteger := pToken^.nSubHookID;
    Query.ParamByName('ModuleID').AsInteger := pToken^.nModuleID;
    Query.ExecSQL;
    FreeQuery(nQueryID);
  end;
end;

function TTurboDBBaseDB.EnableDynamicFuntion(pToken: PHookInstallToken): Integer;
var nQueryID : Integer;
    Query : TTdbQuery;
begin
  Result := ERR_NO_ERROR;
  nQueryID := RequestQuery(True);
  If nQueryID > -1 Then begin
    Query := TTdbQuery(QueryList.Items[nQueryID]);
    Query.SQL.Text := RequestSQL(SQL_ID_HOOKS_UPDATE_ACTIVE);
    Query.Prepare;
    Query.ParamByName('HookID').AsInteger := pToken^.nHookID;
    Query.ParamByName('SubHookID').AsInteger := pToken^.nSubHookID;
    Query.ParamByName('ModuleID').AsInteger := pToken^.nModuleID;
    Query.ParamByName('b_Active').AsInteger := pToken^.bActive;
    Query.ExecSQL;
    FreeQuery(nQueryID);
  end;
end;

procedure TTurboDBBaseDB.EnumDynamicFunctions;
var nQueryID: Integer;
    pToken : PHookInstallToken;
    Query : TTdbQuery;
begin
  nQueryID := RequestQuery;
  If nQueryID > -1 Then begin
    Query := TTdbQuery(QueryList.Items[nQueryID]);
    {$IFNDEF RELEASE}
    If csDesigning in ComponentState Then begin // in designmode we want to see all hooks
      Query.SQL.Text := RequestSQL(SQL_ID_HOOKS_SELECT_ALL);
    end Else
    {$ENDIF}
      Query.SQL.Text := RequestSQL(SQL_ID_HOOKS_SELECT);
    Query.ExecSQL;
    New(pToken);
    pToken^.nAction := iaInsert;  // here it's always INSERT
    pToken^.InsertDir := hidLast; // always at the end since they're coming in there order from the database
    While Not Query.EoF Do begin
      If Assigned(OnEnumDynamicFunctions) Then begin
        pToken^.nHookID := Query.Fields[0].AsInteger;
        pToken^.nSubHookID := Query.Fields[1].AsInteger;
        pToken^.nModuleID := Query.Fields[2].AsInteger;
        pToken^.bActive := Query.Fields[4].AsInteger;
        OnEnumDynamicFunctions(Self, pToken);
      end;
      Query.Next; // Next element
    end;  //  --  While Not TpFIBQuery(QueryList.Items[nQueryID]).EoF Do
    FreeQuery(nQueryID);
    Dispose(pToken);
  end;  //  --  If nQueryID > -1 Then
end;



end.
