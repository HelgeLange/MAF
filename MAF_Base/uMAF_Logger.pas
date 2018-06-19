{*******************************************************************************
Name         : uMAF_Logger.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2008-2009 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 27.10.2008
Last Update  : 27.10.2008
Version      : 1.0.001
Purpose      :
Last Changes :

1.0.001 (27.10.2008) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_Logger;

interface

uses Classes, SysUtils,
     // Modular Application Framework Components units
     uMAF_Globals;

Type TOnLogMessage = procedure(Sender: TObject; aLogType : TLogType; aMessage: String) Of Object;

     TmafCustomLogger = class(TComponent)
     private
       FpHookManager : Pointer;
       FOnLogMessage : TOnLogMessage; // event, when a message gets logged
       FOnLogError   : TOnLogMessage; // event, when writing a log message fails
       FsLastMessage : String;
       FLastType     : TLogType;
       procedure __SetHookManager(const Value: Pointer);
     protected
       property OnLogMessage : TOnLogMessage read FOnLogMessage write FOnLogMessage;
       property OnLogError : TOnLogMessage read FOnLogError write FOnLogError;
     public
       constructor Create(AOwner: TComponent); override; 
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       function WriteLog(aLogType: TLogType; aLogMessage: String): Integer; virtual;
       property HookManager : Pointer read FpHookManager write __SetHookManager;
       property LastMessage : String read FsLastMessage;
       property LastType : TLogType read FLastType;
     end;

     // logfile options are
     //
     //   loLogPerDay     : 1 logfile per day, except maxfile size is reached
     //   loLogPerSession : 1 logfile for every session of the program
     //   loFilePerType   : each log type (ltError etc) has its own logfile
     //
     // naturally loLogPerDay and loLogPerSession excluding each other
     TLogfileOption  = (loLogPerDay, loLogPerSession, loFilePerType);
     TLogfileOptions = Set Of TLogfileOption;

     TmafFileLogger = class(TmafCustomLogger)
     private
       FsFileName : String;
       FsMacroFileName : String;
       FsLogDirectory : String;
       FnMaxFileSizeKB : Integer;
       FLogfileOptions : TLogfileOptions;
       procedure ReadData(Reader: TReader);
       procedure WriteData(Writer: TWriter);
       procedure __SetLogfileOptions(const Value: TLogfileOptions);
       procedure __SetMacroFileName(const Value: String);
     protected
       procedure DefineProperties(Filer: TFiler); override;
       function _InternalWriteLog(sFileName: String; aLogType: TLogType; aLogMessage: String) : Integer;
     public
       constructor Create(AOwner: TComponent); override;
       procedure Loaded; override;
       function WriteLog(aLogType: TLogType; aLogMessage: String): Integer; override;
       property MacroFileName : String read FsMacroFileName write __SetMacroFileName;
     published
       property FileName : String read FsFileName write FsFileName;
       property LogDirectory : String read FsLogDirectory write FsLogDirectory;
       property MaxFileSizeKB : Integer read FnMaxFileSizeKB write FnMaxFileSizeKB default 100;
       property LogfileOptions : TLogfileOptions read FLogfileOptions write __SetLogfileOptions default [loLogPerDay];

       property OnLogMessage;
       property OnLogError;
     end;

implementation

uses uMAF_Tools, uMAF_HookManager, uMAF_CustomBaseDB;

{ TmafCustomLogger }

constructor TmafCustomLogger.Create(AOwner: TComponent);
begin
  inherited;
  FpHookManager := nil;
  FsLastMessage := '';
  FLastType     := ltInfo;
end;

procedure TmafCustomLogger.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  If ((Operation = opRemove) And (AComponent = FpHookManager)) Then
    FpHookManager := nil;
end;

function TmafCustomLogger.WriteLog(aLogType: TLogType; aLogMessage: String): Integer;
begin
  Result := ERR_NO_ERROR;
  FLastType := aLogType;
  FsLastMessage := aLogMessage;
  If Assigned(FOnLogMessage) Then
    FOnLogMessage(Self, aLogType, aLogMessage);
end;

procedure TmafCustomLogger.__SetHookManager(const Value: Pointer);
begin
  FpHookManager := Value;
  If FpHookManager <> nil Then
    TmafHookManager(FpHookManager).FreeNotification(Self);
end;

{ TmafFileLogger }

constructor TmafFileLogger.Create(AOwner: TComponent);
begin
  inherited;
  FLogfileOptions := [loLogPerDay];   // default every day a new logfile
  FnMaxFileSizeKB := 100;             // 100kb Max logfile size
  FsFileName := '';
  FsMacroFileName := '';
end;

procedure TmafFileLogger.__SetLogfileOptions(const Value: TLogfileOptions);
var tmpValue : TLogfileOptions;
begin
  tmpValue := Value;
  // if loLogPerDay is going to be set and we still have loLogPerSession set, then
  // we remove loLogPerSession first, because only one of those options can be true
  If ((loLogPerDay in tmpValue) And (loLogPerSession in FLogfileOptions)) Then begin
    Exclude(tmpValue, loLogPerSession);
    FLogfileOptions := tmpValue;
    Exit;
  end;

  If ((loLogPerSession in tmpValue) And (loLogPerDay in FLogfileOptions)) Then begin
    Exclude(tmpValue, loLogPerDay);
    FLogfileOptions := tmpValue;
    Exit;
  end;
  FLogfileOptions := Value;
end;

procedure TmafFileLogger.__SetMacroFileName(const Value: String);
  function MacroReplace(sMacro: String; var S1, S2 : String): Boolean;
  begin
    Result := (Pos(sMacro, S1) > 0);
    If Result Then begin
      S2 := Copy(S1, 1, Pos(sMacro, S1)-1);
      Delete(S1, 1, Pos(sMacro, S1) + Length(sMacro) - 1);
    end;

  end;

var S, tmpName : String;
begin
  FsMacroFileName := Value;
  S := FsMacroFileName;
  If MacroReplace('%DATE%', S, tmpName) Then begin
    tmpName := tmpName + FormatDateTime('dd-mm-yyyy', Now) + S;
    S := tmpName;
  end;
  If MacroReplace('%WORKSTATION%', S, tmpName) Then begin
    tmpName := tmpName + GetEnvironmentVariable('ComputerName') {GetLocalIPAddressOrHostName(_HOSTNAME)} + S;
    S := tmpName;
  end;
  If MacroReplace('%USER%', S, tmpName) Then begin
    If TmafHookManager(FpHookManager) <> nil Then
      If TmafHookManager(FpHookManager).BaseDB <> nil Then
        tmpName := tmpName + TmafHookManager(FpHookManager).BaseDB.User + S;
    S := tmpName;
  end;
  FsFileName := S;
end;

procedure TmafFileLogger.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Macro', ReadData, WriteData, True);
end;

procedure TmafFileLogger.Loaded;
begin
  If FsMacroFileName <> '' Then
    MacroFileName := FsMacroFileName;  // force set method now
end;

procedure TmafFileLogger.ReadData(Reader: TReader);
begin
  FsMacroFileName := Reader.ReadString;
end;

procedure TmafFileLogger.WriteData(Writer: TWriter);
begin
  Writer.WriteString(FsMacroFileName);
end;

function TmafFileLogger._InternalWriteLog(sFileName: String; aLogType: TLogType; aLogMessage: String): Integer;
var F: TextFile;
    s : String;
begin
  AssignFile(F, sFileName);
  // TODO : insert check for filesize
  Try
    {$I-} Append(F); {$I+}
    If IOResult <> 0 Then
      Rewrite(F);
    S := FormatDateTime('hh:nn:ss', Now);
    If Not (loFilePerType in FLogfileOptions) Then begin
      Case aLogType Of
        ltError   : S := S + ' [Error]';
        ltInfo    : S := S + ' [Info]';
        ltWarning : S := S + ' [Warning]';
      end;  //  --  Case aLogType Of
    end;  //  --  If Not (loFilePerType in FLogfileOptions) Then
    S := S + ' : ' + aLogMessage;
    WriteLn(F, S);
    CloseFile(F);
    Result := ERR_NO_ERROR;
  Except
    Result := ERR_LOG_FAILURE;
    If Assigned(FOnLogError) Then
      FOnLogError(Self, aLogType, aLogMessage);
  End;
end;

function TmafFileLogger.WriteLog(aLogType: TLogType; aLogMessage: String): Integer;
var sFileName : String;
begin
  Result := inherited WriteLog(aLogType, aLogMessage);
  If aLogMessage = '' Then
    Exit;
    
  If loFilePerType in FLogfileOptions Then begin
    Case aLogType Of
      ltError   : sFileName := '[Error] ' + FsFileName;
      ltInfo    : sFileName := '[Info] ' + FsFileName;
      ltWarning : sFileName := '[Warning] ' + FsFileName;
    end;
  end else
    sFileName := FsFileName;

  Result := _InternalWriteLog(FsLogDirectory + '\' + sFileName, aLogType, aLogMessage);
end;

end.
