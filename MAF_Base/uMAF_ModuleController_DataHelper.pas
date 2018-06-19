unit uMAF_ModuleController_DataHelper;

interface

uses Windows, Classes, SysUtils, dialogs,
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_Core, uMAF_TemplateStreamer;

Type TDataAction = (daRouteThrough, daCreateWindow, daReturnData, daCreateEnumWindow, daCreateWindowShow, daCreateWindowShowModal, daCreateEnumWindowModal);

     RDataRecord = packed record
       ID     : Integer;      // unique ID for every data record
       DataID : Integer;      // ID of the TERPModuleControllerToken
       HookID : Integer;      // every DataRecord is assigned to a single SubHook
       SubHookID : Integer;
       uID       : Cardinal;  // unique ID for function within all modules, calculated when creating a new SubHook
       Action : TDataAction;  // action used for this data record
       Description : String;  // a short description for the function
       bUseDatablock : Boolean; // if true, a datablock defined in the ModuleController editor will be given to the SubHook the datblock is attached to
       FpDataTokens : TList;  // a list of PDataToken
     end;
     PDataRecord = ^RDataRecord;

     RDataToken = packed record
       aType : Byte;         // data type
       nLength : Integer;    // length of the data (depends on the type, if it is used)
       pData : Pointer;      // data or pointer to the data, depends on the type
     end;
     PDataToken = ^RDataToken;

     RRecordDef = packed record
       sName : String;       // name of the record field
       aType : Byte;         // data type
     end;
     PRecordDef = ^RRecordDef;

     RFormRegisterData = packed record
       nSubHookID : Integer;
       FormType   : TComponentClass;
       FormData   : Pointer;
     end; // RFormRegisterData
     PFormRegisterData = ^RFormRegisterData;

     RMemoryToken = packed record
       pDR : PDataRecord;
       pData : Pointer;
     end;
     PMemoryToken = ^RMemoryToken;

     TmafModuleControllerDataTypes = class(TPersistent)
     private
       FpToken : TList;           // List of PRecordDef
       FnTokenID : Integer;
       FsTokenName : String;
       procedure __ReadStreamData(Sender: TObject; ID: Integer);
       procedure __WriteStreamData(Sender: TObject; ID: Integer);
       procedure Setup_Streamer(var aStreamer: TTemplateStreamer);
     protected
     public
       constructor Create;
       destructor Destroy; override;
       procedure ReadDataDefs(aStream: TStream);
       procedure WriteDataDefs(aStream: TStream);
       property Token : TList read FpToken;
       property TokenID : Integer read FnTokenID write FnTokenID;
       property TokenName : String read FsTokenName write FsTokenName;
     published
     end;

     TModuleInfo = class(TPersistent)
     private
       FsModuleName : String;
       FsVersionString : String;
       FsBuildDate     : String;
       FsCopyRight     : String;
       FsAuthor        : String;
       FnModuleID      : Integer;
       FpDescription   : TStrings;
       FModuleFlags    : TModuleFlags;
       FOnModuleIDChange : TNotifyEvent;
       procedure __SetDescription(const Value: TStrings);
       procedure __SetModuleID(const Value: Integer);
     public
       constructor Create;
       destructor Destroy; override;
       property OnModuleIDChange : TNotifyEvent read FOnModuleIDChange write FOnModuleIDChange;
     published
       property ModuleName : String read FsModuleName write FsModuleName;
       property VersionString : String read FsVersionString write FsVersionString;
       property BuildDate     : String read FsBuildDate write FsBuildDate;
       property CopyRight     : String read FsCopyRight write FsCopyRight;
       property Author        : String read FsAuthor write FsAuthor;
       property ModuleID      : Integer read FnModuleID write __SetModuleID;
       property FsDescription : TStrings read FpDescription write __SetDescription;
       property ModuleFlags   : TModuleFlags read FModuleFlags write FModuleFlags;
     end;

function __Create_RecordDef: PRecordDef;
procedure __Free_RecordDef(var pData: PRecordDef);

function __Create_DataRecord: PDataRecord;
procedure __Free_DataRecord(var pData: PDataRecord);
procedure __Clean_Data(pData: PDataRecord);
procedure __Set_Data(pData: PDataRecord; aToken: TmafModuleControllerDataTypes);

function __Create_DataToken(aType: Byte = 0): PDataToken;
procedure __Free_DataToken(var pData: PDataToken);

function __Create_EventToken: PEventToken;
procedure __Free_EventToken(var pToken: PEventToken);

implementation

function __Create_DataToken(aType: Byte = 0): PDataToken;
begin
  New(Result);
  Result^.aType := aType;  // 0 = not defined
  Case aType Of
    sdtInteger     : Result^.nLength := SizeOf(Integer);
    sdtDateTime    : Result^.nLength := SizeOf(TDateTime);
    sdtReal        : Result^.nLength := SizeOf(Real);
    sdtByte        : Result^.nLength := SizeOf(Byte);
    sdtWord        : Result^.nLength := SizeOf(Word);
    sdtInt64       : Result^.nLength := SizeOf(Int64);
    sdtCardinal    : Result^.nLength := SizeOf(Cardinal);
    sdtString      : Result^.nLength := 0;                // we don't know the string yet
    sdtMediaItem   : Result^.nLength := SizeOf(Cardinal) + SizeOf(Byte);
    Else Result^.nLength := 0;
  end;
  If Result^.nLength > 0 Then begin
    GetMem(Result^.pData, Result^.nLength);
    FillChar(Result^.pData^, Result^.nLength, 0);
  end else
    Result^.pData := nil;
end;

procedure __Free_DataToken(var pData: PDataToken);
begin
  If pData = nil Then
    Exit;

  FreeMem(pData^.pData, pData^.nLength);
  Dispose(pData);
  pData := nil;
end;

procedure __Clean_Data(pData: PDataRecord);
var pToken : PDataToken;
begin
  If pData = nil Then
    Exit;

  If pData^.DataID > 0 Then
    While pData^.FpDataTokens.Count > 0 Do begin
      pToken := PDataToken(pData^.FpDataTokens.Items[0]);
      __Free_DataToken(pToken);
      pData^.FpDataTokens.Delete(0);
    end;  //  --  While pData^.FpDataTokens.Count > 0 Do
  pData^.DataID := 0;
end;

procedure __Set_Data(pData: PDataRecord; aToken: TmafModuleControllerDataTypes);
var i : Integer;
    NewData : PDataToken;
begin
  If pData = nil Then
    Exit;

  If pData^.DataID > 0 Then
    __Clean_Data(pData);

  If aToken <> nil Then
    For i := 0 To aToken.Token.Count - 1 Do begin
      NewData := __Create_DataToken(PRecordDef(aToken.Token.Items[i])^.aType);
      pData^.FpDataTokens.Add(NewData);
    end;  //  --  For i := 0 To aToken.Token.Count - 1 Do
end;

function __Create_DataRecord: PDataRecord;
begin
  New(Result);
  FillChar(Result^, SizeOf(RDataRecord), 0);
  Result^.Action := daRouteThrough;
  Result^.FpDataTokens := TList.Create;
end;

procedure __Free_DataRecord(var pData: PDataRecord);
var pDT : PDataToken;
    i : Integer;
begin
  For i := pData^.FpDataTokens.Count - 1 DownTo 0 Do begin
    pDT := PDataToken(pData^.FpDataTokens.Items[i]);
    __Free_DataToken(pDT);
  end;  //  --  While pData^.FpDataTokens.Count > 0 Do
  pData^.FpDataTokens.Free;
  Dispose(pData);
  pData := nil;
end;

function __Create_RecordDef: PRecordDef;
begin
  New(Result);
  Result^.sName := '';
  Result^.aType := 0;
end;

procedure __Free_RecordDef(var pData: PRecordDef);
begin
  If pData = nil Then
    Exit;
  Dispose(pData);
  pData := nil;  
end;

function __Create_EventToken: PEventToken;
begin
  New(Result);
  FillChar(Result^, SizeOf(REventToken), 0);
end;

procedure __Free_EventToken(var pToken: PEventToken);
begin
  If pToken <> nil Then begin
    Dispose(pToken);
    pToken := nil;
  end;  //  --  If pToken <> nil Then
end;

{ TERPModuleControllerDataTypes }

constructor TmafModuleControllerDataTypes.Create;
begin
  FpToken := TList.Create;
  FnTokenID := 0;
  FsTokenName := '';
end;

destructor TmafModuleControllerDataTypes.Destroy;
begin
  While FpToken.Count > 0 Do begin
    Dispose(PRecordDef(FpToken.Items[0]));
    FpToken.Delete(0);
  end;  //  --  While FpToken.Count > 0 Do
  FreeAndNil(FpToken);
  inherited;
end;

procedure TmafModuleControllerDataTypes.__ReadStreamData(Sender: TObject; ID: Integer);
var i, nCount : Integer;
    pData : PRecordDef;
begin
  TTemplateStreamer(Sender).ReadInteger(FnTokenID);
  TTemplateStreamer(Sender).ReadString(FsTokenName);
  TTemplateStreamer(Sender).ReadInteger(nCount);
  For i := 1 To nCount Do begin
    pData := __Create_RecordDef;
    TTemplateStreamer(Sender).ReadString(pData^.sName);
    TTemplateStreamer(Sender).ReadByte(pData^.aType);
    FpToken.Add(pData);
  end;  //  --  For i := 1 To nCount Do 
end;

procedure TmafModuleControllerDataTypes.__WriteStreamData(Sender: TObject; ID: Integer);
var i : Integer;
begin
  TTemplateStreamer(Sender).WriteInteger(FnTokenID);
  TTemplateStreamer(Sender).WriteString(FsTokenName);
  TTemplateStreamer(Sender).WriteInteger(FpToken.Count);  // how many attributes do we have ?
  For i := 0 To FpToken.Count - 1 Do begin
    TTemplateStreamer(Sender).WriteString(PRecordDef(FpToken.Items[i])^.sName);
    TTemplateStreamer(Sender).WriteByte(PRecordDef(FpToken.Items[i])^.aType);
  end;  //  --  For i := 0 To FpToken.Count - 1 Do 
end;

procedure TmafModuleControllerDataTypes.Setup_Streamer(var aStreamer: TTemplateStreamer);
begin
  aStreamer := TTemplateStreamer.Create;
  aStreamer.Attributes := 1;
  aStreamer.StreamVersion := 589632;
  aStreamer.OnStreamReadAttribute := __ReadStreamData;
  aStreamer.OnStreamWriteAttribute := __WriteStreamData;
end;

procedure TmafModuleControllerDataTypes.ReadDataDefs(aStream: TStream);
var Streamer : TTemplateStreamer;
begin
  Setup_Streamer(Streamer);
  Streamer.ReadStream(TMemoryStream(aStream));
  FreeAndNil(Streamer);
end;

procedure TmafModuleControllerDataTypes.WriteDataDefs(aStream: TStream);
var Streamer : TTemplateStreamer;
begin
  Setup_Streamer(Streamer);
  Streamer.WriteStream(766, TMemoryStream(aStream));

  FreeAndNil(Streamer);
end;

{ TModuleInfo }

constructor TModuleInfo.Create;
begin
  FsModuleName := '';
  FsVersionString := '1.0.000';
  FsBuildDate := '';
  FsCopyRight := '';
  FsAuthor := '';
  FnModuleID := -1;
  FpDescription := TStringList.Create;
end;

destructor TModuleInfo.Destroy;
begin
  FreeAndNil(FpDescription);
  inherited;
end;

procedure TModuleInfo.__SetDescription(const Value: TStrings);
begin
  FpDescription.Assign(Value);
end;

procedure TModuleInfo.__SetModuleID(const Value: Integer);
begin
  FnModuleID := Value;
  If Assigned(FOnModuleIDChange) Then
    FOnModuleIDChange(Self);
end;

end.
