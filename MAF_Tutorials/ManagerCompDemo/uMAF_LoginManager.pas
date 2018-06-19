{*******************************************************************************
Name         : uMAF_LoginManager.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2010 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 30.08.2010
Last Update  : 31.08.2010
Version      : 1.0.000
Purpose      : A tutorial component that show, how to implement an own MAF
               manager component, that can be loaded through the TmafManagerLoader
               It also shows how to implement the client side, normally a
               component, but it could be just the API calls to the manager
Last Changes :

1.0.000 (30.08.2010)------------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_LoginManager;

interface

uses SysUtils, Classes, Windows, ExtCtrls,
     // Modular Application Framework Components
     uMAF_Core, uMAF_Globals, uMAF_Parameters;

const MT_LOGIN_MANAGER = MT_USER_SECURITY + 1250;  // keep some space to avoid clashs with future MAF Manager

      LM_SEND_LOGIN     = 5100;
      LM_SEND_LOGOFF    = 5101;

      ERR_TIMEOUT       = 500;
      ERR_UNKNOWN_ERROR = 501;

Type // callback from manager to client for asynchron login/logout
     // ALoginReq is a pointer to a RAsyncLoginRequest structur
     TLoginManagerCallBack = procedure(Sender: TObject; ALoginReq: Pointer) Of Object;

     RAsyncLoginRequest = packed record
       nToken : Integer;   // unique identifier
       nErrCode : Integer; // error code
       nCmd : Integer;
       Params : TmafParameters;
       FCallback : TLoginManagerCallBack;
     end;
     PAsyncLoginRequest = ^RAsyncLoginRequest;

     TLoginEvent = procedure(Sender: TObject; pData: PAsyncLoginRequest) Of Object;
     TLogoffEvent = procedure(Sender: TObject; pData: PAsyncLoginRequest) Of Object;

     TmafLoginManager = class(TmafCustomManagerComponent) // implements all basic interfaces for a MAF manager component
     private
       FOnLogin : TLoginEvent;
       FOnLogoff : TLogoffEvent;
       FnToken : Integer;
       FpCommList : TList;
       FpTimerList : TList;
       FnTimeout : Integer;          // timeout for any request
       FpTimer : TTimer;
       function __GetByToken(TokenID: Integer): PAsyncLoginRequest;
     protected
       procedure __OnTimer(Sender: TObject);
       procedure __OnInternalTimer(Sender: TObject);
       procedure __DoManagerEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer); override;
       function __DoLogin(pData: PAsyncLoginRequest): Integer;
       function __DoLogoff(pData: PAsyncLoginRequest): Integer;
       function __AssignToken: Integer;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;

       property ByToken[TokenID: Integer]: PAsyncLoginRequest read __GetByToken;
     published
       property Timeout : Integer read FnTimeout write FnTimeout;
       property OnLoginRequest: TLoginEvent read FOnLogin write FOnLogin;
       property OnLogoffRequest : TLogoffEvent read FOnLogoff write FOnLogoff;
     end;

     TmafLoginClient = class(TmafBaseComponent) // implements all basic interfaces for a MAF client component
     private
       FOnLogin : TLoginEvent;
       FOnLogoff : TLogoffEvent;
       FpParams : TmafParameters;
       procedure __ManagerCallback(Sender: TObject; ALoginReq: Pointer);
     protected
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;

       procedure DoLogin(sLogin, sPassword, sDBName: String);
       procedure DoLogoff(sLogin: String);
     published
       property OnLoginRequest: TLoginEvent read FOnLogin write FOnLogin;
       property OnLogoffRequest : TLogoffEvent read FOnLogoff write FOnLogoff;
     end;

procedure Register;

implementation

uses uMAF_Tools;

const MAX_EVENTS = 1;

var ManagerEvents : array[1..MAX_EVENTS] of Integer = (MT_LOGIN_MANAGER);

procedure Register;
begin
  RegisterComponents('MAF Manager', [TmafLoginManager]);
  RegisterComponents('MAF Clients', [TmafLoginClient]);
end;

function __Create_AsyncLoginRequest: PAsyncLoginRequest;
begin
  New(Result);
  FillChar(Result^, SizeOf(RAsyncLoginRequest), 0);
end;

procedure __Free_AsyncLoginRequest(var pData: PAsyncLoginRequest);
begin
  If pData = nil Then
    Exit;
  Dispose(pData);
  pData := nil;
end;

{ TmafLoginManager }

constructor TmafLoginManager.Create(AOwner: TComponent);
begin
  inherited;
  ManagerType := MT_LOGIN_MANAGER; // to accept aumatically messages from TmafLoginClient
  FnToken := 0;
  FpCommList := TList.Create;
  FpTimerList := TList.Create;
  FpTimer := TTimer.Create(Self);
  FpTimer.Enabled := False;
  FpTimer.OnTimer := __OnInternalTimer;
  FpTimer.Interval := 100;
end;

destructor TmafLoginManager.Destroy;
begin
  While FpTimerList.Count > 0 Do begin
    TTimer(FpTimerList.Items[0]).Free;
    FpTimerList.Delete(0);
  end;  //  --  While FpTimerList.Count > 0 Do
  FpTimerList.Free;
  FpCommList.Free;
  inherited;
end;

function TmafLoginManager.__AssignToken: Integer;
begin
  Inc(FnToken);
  Result := FnToken;
end;

// override this function to accept messages from your client
// to come into this function, just call from inside the client :
//   ErrCode := __AdvQuery_Manager(QueryID, QHS, UserParam);
// the parameters are :
//   QueryID   = the ID for a query accepted here in __DoManagerEvent
//   QHS       = a valid QueryHandlerStruct
//   UserParam = a user param pointer, normally not used, just leave it nil
function TmafLoginManager.__DoLogin(pData: PAsyncLoginRequest): Integer;
var ATimer : TTimer;
begin
  Result := ERR_UNKNOWN_ERROR;
  pData^.nToken := __AssignToken;
  FpCommList.Add(pData);
  If FnTimeout > 0 Then begin
    ATimer := TTimer.Create(Self);
    ATimer.Tag := pData^.nToken;    // to identify the timer
    ATimer.Interval := FnTimeout;
    ATimer.Enabled := True;
    FpTimerList.Add(ATimer);
  end;
  If Assigned(FOnLogin) Then
    FOnLogin(Self, pData);
end;

function TmafLoginManager.__DoLogoff(pData: PAsyncLoginRequest): Integer;
begin
  If Assigned(FOnLogoff) Then
    FOnLogoff(Self, pData);
end;

procedure TmafLoginManager.__DoManagerEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  Case QHS^.SubHookID Of
    LM_SEND_LOGIN  : ErrCode := __DoLogin(PAsyncLoginRequest(QHS^.pChildObj));
    LM_SEND_LOGOFF : ErrCode := __DoLogoff(PAsyncLoginRequest(QHS^.pChildObj));
  end;  //  --  Case QHS^.SubHookID Of
  inherited;
end;

function TmafLoginManager.__GetByToken(TokenID: Integer): PAsyncLoginRequest;
var i: Integer;
begin
  Result := nil;
  For i := 0 To FpCommList.Count - 1 Do
    If PAsyncLoginRequest(FpCommList.Items[i])^.nToken = TokenID Then begin
      Result := PAsyncLoginRequest(FpCommList.Items[i]);
      Break;
    end;  //  --  If PAsyncLoginRequest(FpCommList.Items[i])^.nToken = TokenID Then 
end;

procedure TmafLoginManager.__OnInternalTimer(Sender: TObject);
var i: Integer;
begin
  For i := FpTimerList.Count - 1 DownTo 0 Do
    If TTimer(FpTimerList.Items[i]).Tag = -1 Then begin
      TTimer(FpTimerList.Items[i]).Free;
      FpTimerList.Delete(i);
    end;
  FpTimer.Enabled := False;   // will be enabled again, when the next timer expires
end;

procedure TmafLoginManager.__OnTimer(Sender: TObject);
var i: Integer;
    pData : PAsyncLoginRequest;
begin
  pData := nil;
  For i := 0 To FpCommList.Count - 1 Do
    If PAsyncLoginRequest(FpCommList.Items[i])^.nToken = TTimer(Sender).Tag Then begin
      pData := PAsyncLoginRequest(FpCommList.Items[i]);
      Break;
    end;
  If Assigned(pData) Then begin
    // we cannot delete the timer just yet, as we're inside the event of that timer
    // so we just set the Tag to -1 and delete it later
    TTimer(Sender).Tag := -1;
    TTimer(Sender).Enabled := False; // we turn it off, we don't need this timer anymore
    If Assigned(pData^.FCallback) Then
      pData^.FCallback(Self, pData);
    FpCommList.Delete(FpCommList.IndexOf(pData));
    FpTimer.Enabled := True;         // enable the timer to delete this timer
  end;
end;

{ TmafLoginClient }

constructor TmafLoginClient.Create(AOwner: TComponent);
begin
  inherited;
  FManagerType := MT_LOGIN_MANAGER; // that way we call automatically "our" manager
  AManagerQueryID := MT_LOGIN_MANAGER;
  FpParams := TmafParameters.Create(Self);
  FpParams.Add('UserName', '');
  FpParams.Add('Password', '');
  FpParams.Add('DBName', '');
end;

destructor TmafLoginClient.Destroy;
begin
  FpParams.Free;
  inherited;
end;

procedure TmafLoginClient.DoLogin(sLogin, sPassword, sDBName: String);
var QHS: pQHS;
    LR : PAsyncLoginRequest;
begin
  FpParams.ParamByName['UserName'].Value := sLogin;
  FpParams.ParamByName['Password'].Value := sPassword;
  FpParams.ParamByName['DBName'].Value := sDBName;

  QHS := __Create_QueryHandlerStruct;       // all calls through the DFT need the QHS
  LR := __Create_AsyncLoginRequest;
  LR^.nCmd := LM_SEND_LOGIN;
  LR^.Params := FpParams;
  LR^.FCallback := __ManagerCallback;       // set the call back as the answer will come asynchron
  QHS^.pChildObj := LR;
  __AdvQuery_Manager(LM_SEND_LOGIN, QHS, nil); // sending the request to the manager component
  // we don't free the PAsyncLoginRequest record here, as the manager will save it and return it to us
  // with the callback and we can free it there
  //__Free_AsyncLoginRequest(LR);
  __Free_QueryHandlerStruct(QHS);           // free the QueryHandlerStruct again
  FpParams.ParamByName['Password'].Value := '';
  FpParams.ParamByName['DBName'].Value := '';
end;

procedure TmafLoginClient.DoLogoff(sLogin: String);
var QHS: pQHS;
    LR : PAsyncLoginRequest;
begin
  FpParams.ParamByName['UserName'].Value := sLogin;
  QHS := __Create_QueryHandlerStruct;       // all calls through the DFT need the QHS
  LR := __Create_AsyncLoginRequest;
  LR^.nCmd := LM_SEND_LOGOFF;
  LR^.Params := FpParams;
  LR^.FCallback := __ManagerCallback;       // set the call back as the answer will come asynchron
  QHS^.pChildObj := LR;
  __AdvQuery_Manager(LM_SEND_LOGOFF, QHS, nil);  // sending the request to the manager component
  __Free_QueryHandlerStruct(QHS);           // free the QueryHandlerStruct again
end;

// here all callbacks from the manager come in, as call to the manager won't have
// an answer instantly, but they come asynchron
procedure TmafLoginClient.__ManagerCallback(Sender: TObject; ALoginReq: Pointer);
var LR : PAsyncLoginRequest;
begin
  LR := PAsyncLoginRequest(ALoginReq);
  Case LR^.nCmd Of
    LM_SEND_LOGIN  : begin
                       If Assigned(FOnLogin) Then
                         FOnLogin(Self, LR);
                     end;
    LM_SEND_LOGOFF : begin
                       If Assigned(FOnLogoff) Then
                         FOnLogoff(Self, LR);
                     end;
  end;
  __Free_AsyncLoginRequest(LR);  // now we can free the request
end;

end.
