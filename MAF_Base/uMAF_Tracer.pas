{*******************************************************************************
Name         : uMAF_Tracer.pas
Coding by    : Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 14.08.2009
Last Update  : 14.08.2009
Version      : 1.0.000
Last Changes : provides tracing functions for the ERP Framework Components as
               well as for the program using them

1.0.000 (14.08.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_Tracer;

interface

uses Windows, Classes, SysUtils, Variants;

Type RLogData = packed record
       AType : Word;
       Name  : String;
       Value : Variant;
       TimeStamp : TDateTime;
     end;
     PLogData = ^RLogData;


     TmafTracer = class(TObject)
     private
       FpLogData : TList;
       FsLogFileName : String;
       FbEnabled : Boolean;
       FOnChange : TNotifyEvent;
       procedure __Trigger_OnChange;
       procedure __WriteToLog;
       procedure __SetEnabled(const Value: Boolean);
     public
       constructor Create;
       destructor Destroy; override;

       procedure Enter(AProc: String);
       procedure Leave;
       procedure CheckPoint(AName: String);
       procedure Log_Integer(AName: String; Value: Integer);
       procedure Log_String(AName, Value : String);

       property LogData : TList read FpLogData write FpLogData;
       property LogFileName : String read FsLogFileName write FsLogFileName;
       property Enabled : Boolean read FbEnabled write __SetEnabled;
       property OnChange : TNotifyEvent read FOnChange write FOnChange;
     end;

var MAFTracer : TmafTracer;

const TRACER_ENTER      = 1;
      TRACER_LEAVE      = 2;
      TRACER_CHECKPOINT = 3;
      TRACER_LOG_VALUE  = 4;

implementation

uses Dialogs;

{ TmafTracer }

constructor TmafTracer.Create;
begin
  FpLogData := TList.Create;
end;

destructor TmafTracer.Destroy;
begin
  __WriteToLog;
  While FpLogData.Count > 0 Do begin
    Dispose(PLogData(FpLogData.Items[0]));
    FpLogData.Delete(0);
  end;  //  --  While FpLogData.Count > 0 Do
  FreeAndNil(FpLogData);
  inherited;
end;

procedure TmafTracer.__SetEnabled(const Value: Boolean);
begin
  If ((FbEnabled = True) And (Value = False)) Then
    If FpLogData.Count > 0 Then begin
      __WriteToLog;
      While FpLogData.Count > 0 Do begin
        Dispose(PLogData(FpLogData.Items[0]));
        FpLogData.Delete(0);
      end;  //  --  While FpLogData.Count > 0 Do
    end;
  FbEnabled := Value;
end;

procedure TmafTracer.__Trigger_OnChange;
begin
  If Assigned(FOnChange) Then
    FOnChange(Self);
end;

function __GetSpaces(ADepth: Integer): String;
var i : Integer;
begin
  Result := '';
  For i := 1 To ADepth Do
    Result := Result + '  ';
end;

procedure TmafTracer.__WriteToLog;
var F: Text;
    AText : String;
    i, ADepth : Integer;
    pData : PLogData;
begin
  If Not FbEnabled Then
    Exit;

  AssignFile(F, FsLogFileName);
  Rewrite(F);
  ADepth := 0;
  For i := 0 To FpLogData.Count - 1 Do begin
    pData := PLogData(FpLogData.Items[i]);
    AText := '[' + FormatDateTime('dd.mm.yyyy hh:mm:ss:zzz', pData^.TimeStamp) + '] ';
    Case pData^.AType Of
      TRACER_ENTER      : begin
                            AText := AText + __GetSpaces(ADepth) + 'ENTER ' + pData^.Name;
                            Inc(ADepth);
                          end;
      TRACER_LEAVE      : begin
                            Dec(ADepth);
                            AText := AText + __GetSpaces(ADepth) + 'LEAVING...'
                          end;
      TRACER_CHECKPOINT : AText := AText + __GetSpaces(ADepth) + 'CHECKPOINT ' + pData^.Name;
      TRACER_LOG_VALUE  : AText := AText + __GetSpaces(ADepth) + 'VALUE ' + pData^.Name + ' = ' + VarToStr(pData^.Value);
      Else AText := AText + __GetSpaces(ADepth) + 'Strange entry';
    end;
    Writeln(F, AText);
  end;
  CloseFile(F);
end;

procedure TmafTracer.Enter(AProc: String);
var pData : PLogData;
begin
  If MAFTracer = nil Then
    Exit;

  If Not FbEnabled Then
    Exit;

  New(pData);
  pData^.AType := TRACER_ENTER;
  pData^.Name := AProc;
  pData^.TimeStamp := Now;
  FpLogData.Add(pData);
  __Trigger_OnChange;
end;

procedure TmafTracer.Leave;
var pData : PLogData;
begin
  If MAFTracer = nil Then
    Exit;

  If Not FbEnabled Then
    Exit;

  New(pData);
  pData^.AType := TRACER_LEAVE;
  pData^.TimeStamp := Now;
  FpLogData.Add(pData);
  __Trigger_OnChange;
end;

procedure TmafTracer.Log_Integer(AName: String; Value: Integer);
var pData : PLogData;
begin
  If MAFTracer = nil Then
    Exit;

  If Not FbEnabled Then
    Exit;

  New(pData);
  pData^.AType := TRACER_LOG_VALUE;
  pData^.Name := AName;
  pData^.Value := Value;
  pData^.TimeStamp := Now;
  FpLogData.Add(pData);
  __Trigger_OnChange;
end;

procedure TmafTracer.Log_String(AName, Value: String);
var pData : PLogData;
begin
  If MAFTracer = nil Then
    Exit;

  If Not FbEnabled Then
    Exit;

  New(pData);
  pData^.AType := TRACER_LOG_VALUE;
  pData^.Name := AName;
  pData^.Value := Value;
  pData^.TimeStamp := Now;
  FpLogData.Add(pData);
  __Trigger_OnChange;
end;

procedure TmafTracer.CheckPoint(AName: String);
var pData : PLogData;
begin
  If MAFTracer = nil Then
    Exit;

  If Not FbEnabled Then
    Exit;

  New(pData);
  pData^.AType := TRACER_CHECKPOINT;
  pData^.Name := AName;
  pData^.TimeStamp := Now;
  FpLogData.Add(pData);
  __Trigger_OnChange;
end;

initialization
  MAFTracer := nil;
  {$IFDEF Tracer}
  MAFTracer := TmafTracer.Create;
  MAFTracer.Enabled := True;

  MAFTracer.LogFileName := 'D:\Delphi\Tests\ServiceXE2\Win64\MAF_Tracer.txt'; //ExtractFilePath(ParamStr(0)) + 'MAF_Tracer.txt';
  {$ENDIF}

finalization
  {$IFDEF Tracer}
  FreeAndNil(MAFTracer);
  {$ENDIF}

end.
