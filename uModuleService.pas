{*******************************************************************************
Name         : uModuleService.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2009-2015 by Helge Lange
Info         : HelgeLange@gmail.com
Website      : http://www.maf-components.com
Date         : 11.09.2009
Last Update  : 26.06.2015
Version      : 1.0.001
Purpose      : used in the dpr file in every module it provides the 2 exported
               functions for the module service
Last Changes :

1.0.001 (19.10.2009) -----------------------------------------------------------
- [ADD] added code to send a message to the ModuleController in manager modules
        when there are start parameters delivered from the TmafManagerLoader
1.0.000 (11.09.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uModuleService;

{$J+} // allow assignable typed constants

interface

{$I MAF_Base\MAFramework.inc}

uses Windows, Classes, SysUtils, {$IFDEF D18+}vcl.Dialogs,{$ELSE}Dialogs,{$ENDIF}
     {$IFDEF USE_CODESITE} CodeSiteLogging, {$ENDIF}
     uMAF_Core, uMAF_Globals;

procedure __RegisterDMClass(AClass: TComponentClass);

implementation

uses uMAF_Tools, uMAF_ModuleController;

var AUserClass : TComponentClass;
    AModuleController : TmafModuleController;

const bRegistered : Boolean = False;
      aComp : TComponent = nil;
      sRegisterDMClassFail : String = '__RegisterDMClass was not called and no class registered'+#13#10+
                                      'that contains a ModuleController. See Help for more information.';

function __FindModuleController(AComponent: TComponent): TmafModuleController;
var i : Integer;
begin
  Result := nil;
  If AComponent = nil Then
    Exit;
  For i := 0 To AComponent.ComponentCount - 1 Do
    If IsClass(AComponent.Components[i], TmafModuleController) Then begin
      Result := TmafModuleController(AComponent.Components[i]);
      Break;
    end;
end;

function __ManagerModuleServiceProc(nSubHookID: Integer; QHS: pQHS; pUserParam: Pointer): Integer; stdcall;
begin
  Result := ERR_NO_ERROR;
  If ((nSubHookID = MAN_CREATE_MANAGER) And (aComp = nil)) Then begin
    If Not bRegistered Then
      Raise EComponentError.Create(sRegisterDMClassFail);
    AModuleController := nil;
    __CreateComponent(AUserClass, aComp, nil);
    If aComp <> nil Then
      AModuleController := __FindModuleController(aComp);

    If Assigned(AModuleController) Then begin
      If Assigned(QHS^.pChildObj) Then
        SendComponentMessage(AModuleController, WM_START_PARAMS, QHS^.pChildObj, nil);
      Result := AModuleController.__DoInitialize;
    end else
      Result := ERR_NO_MODULE_CONTROLLER;
  end;  //  --  If ((nSubHookID = MAN_CREATE_MANAGER) And (aComp = nil)) Then

  If Not (Assigned(aComp)) Then
    Result := ERR_MODULE_NOT_INITIALIZED;

  If Result <> ERR_NO_ERROR Then begin // in case we're in bad shape already...
    ShowMessage('ErrorCode : ' + IntToStr(Result));
    Exit;
  end;

  Case nSubHookID Of
    MAN_CLOSE_MANAGER  : FreeAndNil(aComp);
    Else Result := AModuleController.Execute(nSubHookID, QHS, pUserParam)
  end;  //  --  Case nCommand Of
end;

function __ModuleServiceProc(nSubHookID: Integer; QHS: pQHS; pUserParam: Pointer): Integer; stdcall;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( '__ModuleServiceProc' );{$ENDIF}
  Result := ERR_NO_ERROR;
  If ((nSubHookID = HM_CREATE) And (aComp = nil)) Then begin
    If Not bRegistered Then
      Raise EComponentError.Create(sRegisterDMClassFail);
    AModuleController := nil;
    Try
      __CreateComponent(AUserClass, aComp, nil);
    Except
      On E: Exception Do begin
        {$IFDEF USE_CodeSite} CodeSite.Send(csmLevel1, 'Could not create datamodule class object', E); {$ENDIF}
        Raise Exception.Create('Could not create datamodule class object' + #13#10 + E.Message);
      end;
    End;
    If aComp <> nil Then
      AModuleController := __FindModuleController(aComp);

    If Assigned(AModuleController) Then
      Result := AModuleController.__DoInitialize
    Else
      Result := ERR_NO_MODULE_CONTROLLER;
  end;  //  --  If ((nSubHookID = HM_CREATE) And (DMR = nil)) Then

  If Not (Assigned(aComp)) Then begin
    Result := ERR_MODULE_NOT_INITIALIZED;
    Exit;
  end;

  If Result <> ERR_NO_ERROR Then  // in case we're in bad shape already...
    Exit;

  Case nSubHookID Of
    HM_CLOSE  : begin
                  // if it's not a manager module
                  If ((not (mfManagerModule in AModuleController.ModuleInfo.ModuleFlags)) Or (Not bRunMode)) Then begin
                    FreeAndNil(aComp);
                    AModuleController := nil;
                  end;
                end;
    Else Result := AModuleController.Execute(nSubHookID, QHS, pUserParam)
  end;  //  --  Case nCommand Of
end;

function __ModuleInitServiceProc(nSubHookID: Integer; QHS: pQHS; pUserParam: Pointer): Integer; stdcall;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( '__ModuleInitServiceProc' );{$ENDIF}
  Result := ERR_NO_ERROR;
  If ((nSubHookID = HM_INIT_MODULE) And (aComp = nil)) Then begin
    If Not bRegistered Then
      Raise EComponentError.Create(sRegisterDMClassFail);
    AModuleController := nil;
    __CreateComponent(AUserClass, aComp, nil);

    If Not (Assigned(aComp)) Then begin
      Result := ERR_MODULE_NOT_INITIALIZED;
      Exit;
    end;

    If aComp <> nil Then
      AModuleController := __FindModuleController(aComp);

    If AModuleController = nil Then begin
      Result := ERR_MODULE_NOT_INITIALIZED;
    end Else begin
      QHS^.pChildObj := AModuleController;
      Result := ERR_NO_ERROR;
    end;
  end;  //  --  If ((nSubHookID = HM_INIT_MODULE) And (aComp = nil)) Then
end;

procedure __RegisterDMClass(AClass: TComponentClass);
begin
  AUserClass := AClass;
  bRegistered := True;
end;

exports
  __ModuleServiceProc,           // service proc for simple modules
  __ModuleInitServiceProc,       // service proc to initialize a module in design time
  __ManagerModuleServiceProc;    // service proc to initialize manager modules

initialization

end.


