// similar to DFM streaming this class helps to manage data stream
// usage :
//
// Write : Set a stream Version and the number of attributes, then call
//         WriteStream, wich expects as paramater a unique identifier.
//         This ID makes it possible to read / write several types of
//         streams within the same stream-Read/write-event
//         Example : begin
//                     HookClient.Streamer.Attributes := 15;      // 15 writes
//                     HookClient.Streamer.StreamVersion := 1234; // newest Stream version
//                     HookClient.Streamer.WriteStream;           // triggers 15 times OnStreamWriteAtrribute
//                   end;
//         Example for event method OnStreamWriteAttribute
//         const STREAM_TEST1 = 244;
//               STREAM_TEST2 = 245;
//         begin
//           Case HookClient.Streamer.StreamID Of
//             STREAM_TEST1 : Case ID Of
//                               1 : HookClient.Streamer.WriteString(FsMyString1);
//                               2 : HookClient.Streamer.WriteInteger(FiMyInteger1);
//                              ..
//                              15 : HookClient.Streamer.WritePAnsiChar(FiMyPAnsiChar4);
//                            end;
//             STREAM_TEST2 : Case ID Of
//                               1 : bla bla
//                            end;
//           end;
//         end;
// Read  : Just set your stream version (or set to -1 to surpress
//         stream version errors) and call ReadStream. ReadStream automatically
//         sets StreamVersion and number of attributes and StreamID
//         Example : not needed, i guess ;)
//         Example for event method OnStreamReadAttribute
//         begin
//           Case ID Of
//              1 : HookClient.Streamer.ReadString(FsMyString1);
//              2 : HookClient.Streamer.ReadInteger(FiMyInteger1);
//              ..
//             15 : HookClient.Streamer.ReadPAnsiChar(FiMyPAnsiChar4);
//           end;
//         end;
//
// ReadStream and WriteStream methods use internal stream, if no parameter
// is given or nil (default), otherwise one can use self-managed streams
// by giving the stream object as parameter
unit uMAF_TemplateStreamer;

interface

uses Windows, Classes, SysUtils, Messages,
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_Core;

Type TOnStreamVersionError = procedure(Sender: TObject; ReadVersion: Integer; var CanContinue: Boolean) Of Object;
     TOnAttribute = procedure(Sender: TObject; ID: Integer) Of Object;

     TTemplateStreamer = class(TPersistent)
     private
       FpHookClient : Pointer;
       FnStreamVersion : Integer;
       FnAttributes : Integer;  // number of attributes in a stream
       FStream : TMemoryStream; // will be set in loading to campare and automatically written
       FnStreamID : Integer;    // unique identifier for current stream
       FpDataStorageQueryToken : PDataStorageQuery;
       FpData : Pointer;
       FOnStreamVersionError : TOnStreamVersionError;
       FOnStreamVersion : TNotifyEvent;
       FOnStreamReadAttribute : TOnAttribute;
       FOnStreamWriteAttribute : TOnAttribute;
       procedure __SetHookClient(const Value: Pointer);
       function __GetTemplateCategory: String;
       procedure __SetTemplateCategory(const Value: String);
       function __GetTemplateName: String;
       procedure __SetTemplateName(const Value: String);
       function __GetTemplateID: Integer;
       procedure __SetTemplateID(const Value: Integer);
       procedure __FreeQueryNames(pData: Pointer);
     public
       constructor Create;
       destructor Destroy; override;
       function CreateStream: TMemoryStream;
       procedure FreeStream;
       procedure ReadStream(aStream: TMemoryStream = nil);
       procedure WriteStream(aStreamID: Integer; aStream: TMemoryStream = nil);
       procedure ResetQueryToken;

       procedure WriteString(S: String);
       procedure ReadString(var S: String); overload;
       function ReadString: String; overload;
       procedure ReadStringA(var S: AnsiString); overload;
       function ReadStringA: AnsiString; overload;
       procedure WriteStringA(S: AnsiString);
       procedure WriteInteger(i: Integer);
       procedure ReadInteger(var i: Integer); overload;
       function ReadInteger: Integer; overload;
       procedure WritePChar(PC: PChar);
       procedure ReadPChar(var PC: PChar);
       procedure WriteDateTime(dtVar: TDateTime);
       procedure ReadDateTime(var dtVar: TDateTime); overload;
       function ReadDateTime: TDateTime; overload;
       procedure WriteByte(B: Byte);
       procedure ReadByte(var B: Byte); overload;
       function ReadByte: Byte; overload;
       procedure WriteInt64(Value: Int64);
       procedure ReadInt64(var Value: Int64); overload;
       function ReadInt64: Int64; overload;
       procedure WriteWord(Value: Word);
       procedure ReadWord(var Value: Word); overload;
       function ReadWord: Word; overload;
       procedure WriteCardinal(Value: Cardinal);
       procedure ReadCardinal(var Value: Cardinal); overload;
       function ReadCardinal: Cardinal; overload;
       procedure WriteBoolean(Value: Boolean);
       procedure ReadBoolean(var Value: Boolean); overload;
       function ReadBoolean: Boolean; overload;

       property HookClient : Pointer read FpHookClient write __SetHookClient;
       property Stream: TMemoryStream read FStream;
       property DataStorageQueryToken : PDataStorageQuery read FpDataStorageQueryToken write FpDataStorageQueryToken;
       property Data : Pointer read FpData write FpData;

       property OnStreamVersionError : TOnStreamVersionError read FOnStreamVersionError write FOnStreamVersionError;
       property OnStreamReadAttribute : TOnAttribute read FOnStreamReadAttribute write FOnStreamReadAttribute;
       property OnStreamWriteAttribute : TOnAttribute read FOnStreamWriteAttribute write FOnStreamWriteAttribute;
       property OnStreamVersion : TNotifyEvent read FOnStreamVersion write FOnStreamVersion;
     published
       property StreamVersion : Integer read FnStreamVersion write FnStreamVersion default -1;
       property Attributes : Integer read FnAttributes write FnAttributes;
       property StreamID : Integer read FnStreamID;
       property TemplateID : Integer read __GetTemplateID write __SetTemplateID;
       property TemplateCategory : String read __GetTemplateCategory write __SetTemplateCategory;
       property TemplateName : String read __GetTemplateName write __SetTemplateName;
     end;


implementation

uses uMAF_HookClient, uMAF_Tools;

{ TTemplateStreamer }

constructor TTemplateStreamer.Create;
begin
  inherited;
  FnStreamVersion := -1;
  HookClient := nil;    // make sure it's not set... triggers __SetHookClient
  New(FpDataStorageQueryToken);
  FillChar(FpDataStorageQueryToken^, SizeOf(RDataStorageQuery), 0);
  FpDataStorageQueryToken^.aObj := CreateStream; // FStream is already set inside CreateStream
end;

destructor TTemplateStreamer.Destroy;
begin
  If FStream <> nil Then
    FreeAndNil(FStream);
  __Free_DataStorageQuery(FpDataStorageQueryToken);
  inherited;
end;

procedure TTemplateStreamer.ResetQueryToken;
begin
  FpDataStorageQueryToken^.nID := 0;
  FreePChar(FpDataStorageQueryToken^.sCategory);
  FreePChar(FpDataStorageQueryToken^.sName);
  FpDataStorageQueryToken^.aObj := FStream;
end;

procedure TTemplateStreamer.__SetHookClient(const Value: Pointer);
begin
  FpHookClient := Value;
  If Assigned(FpHookClient) Then begin
    FOnStreamVersionError := TmafHookClient(FpHookClient).OnStreamVersionError;
    FOnStreamVersion := TmafHookClient(FpHookClient).OnStreamVersion;
    FOnStreamReadAttribute := TmafHookClient(FpHookClient).OnStreamReadAttribute;
    FOnStreamWriteAttribute := TmafHookClient(FpHookClient).OnStreamWriteAttribute;
  end else begin
    FOnStreamVersionError := nil;
    FOnStreamVersion := nil;
    FOnStreamReadAttribute := nil;
    FOnStreamWriteAttribute := nil;
  end;
end;

function TTemplateStreamer.__GetTemplateID: Integer;
begin
  Result := FpDataStorageQueryToken^.nID;
end;

procedure TTemplateStreamer.__SetTemplateID(const Value: Integer);
begin
  FpDataStorageQueryToken^.nID := Value;
end;

function TTemplateStreamer.__GetTemplateCategory: String;
begin
  Result := String(FpDataStorageQueryToken^.sCategory);
end;

procedure TTemplateStreamer.__SetTemplateCategory(const Value: String);
begin
  If FpDataStorageQueryToken^.sCategory <> nil Then
    FreePChar(FpDataStorageQueryToken^.sCategory);
  StrToPChar(Value, FpDataStorageQueryToken^.sCategory);
  FpDataStorageQueryToken^.pFreeMemFunc := __FreeQueryNames;
end;

function TTemplateStreamer.__GetTemplateName: String;
begin
  Result := String(FpDataStorageQueryToken^.sName);
end;

procedure TTemplateStreamer.__SetTemplateName(const Value: String);
begin
  If FpDataStorageQueryToken^.sName <> nil Then
    FreePChar(FpDataStorageQueryToken^.sName);
  StrToPChar(Value, FpDataStorageQueryToken^.sName);
  FpDataStorageQueryToken^.pFreeMemFunc := __FreeQueryNames;
end;

procedure TTemplateStreamer.__FreeQueryNames(pData: Pointer);
begin
  If Not Assigned(pData) Then
    Exit;
  If PDataStorageQuery(pData)^.sCategory <> nil Then
    FreePChar(PDataStorageQuery(pData)^.sCategory);
  If PDataStorageQuery(pData)^.sName <> nil Then
    FreePChar(PDataStorageQuery(pData)^.sName);
  PDataStorageQuery(pData)^.pFreeMemFunc := nil;
end;

function TTemplateStreamer.CreateStream: TMemoryStream;
begin
  If FStream = nil Then
    FStream := TMemoryStream.Create
  Else
    FStream.Size := 0;
  Result := FStream;
end;

procedure TTemplateStreamer.FreeStream;
begin
  If FStream <> nil Then
    FreeAndNil(FStream);
end;

procedure TTemplateStreamer.ReadStream(aStream: TMemoryStream);
var i : Integer;
    iVer : Integer;
    OldStream : TMemoryStream;
    bCanContinue : Boolean;
begin
  OldStream := nil;
  If aStream <> nil Then begin
    // we got another stream as our internal one, so we patch that up and restore
    // our stream pointer later
    If aStream.Size = 0 Then
      Exit;
    OldStream := FStream;
    FStream := aStream;
    FpDataStorageQueryToken^.aObj := FStream;
  end;  //  --  If aStream <> nil Then
  If FStream.Size = 0 Then
    Exit;
  bCanContinue := True;
  FStream.Read(iVer, SizeOf(Integer)); // first we read the version number
  If iVer <> FnStreamVersion Then
    If Assigned(OnStreamVersionError) Then
      OnStreamVersionError(Self, iVer, bCanContinue);
  If Not bCanContinue Then begin
    If aStream <> nil Then begin
      FStream := OldStream; // restore our stream pointer
      FpDataStorageQueryToken^.aObj := FStream;
    end;  //  --  If aStream <> nil Then
    Exit;
  end;
  FnStreamVersion := iVer;
  If Assigned(OnStreamVersion) Then
    OnStreamVersion(Self);

  FStream.Read(FnStreamID, SizeOf(Integer));   // unique StreamID
  FStream.Read(FnAttributes, SizeOf(Integer)); // now we read number of attributes
  For i := 1 To FnAttributes Do
    If Assigned(OnStreamReadAttribute) Then
      OnStreamReadAttribute(Self, i);

  If aStream <> nil Then begin
    FStream := OldStream; // restore our stream pointer
    FpDataStorageQueryToken^.aObj := FStream;
  end;  //  --  If aStream <> nil Then
end;

procedure TTemplateStreamer.WriteStream(aStreamID: Integer; aStream: TMemoryStream);
var i : Integer;
    OldStream : TMemoryStream;
begin
  OldStream := nil;
  If aStream <> nil Then begin
    // we got another stream as our internal one, so we patch that up and restore
    // our stream pointer later
    OldStream := FStream;
    FStream := aStream;
    FpDataStorageQueryToken^.aObj := FStream;
  end;  //  --  If aStream <> nil Then
  FnStreamID := aStreamID;
  FStream.Write(FnStreamVersion, SizeOf(Integer)); // first we write the version number
  FStream.Write(aStreamID, SizeOf(Integer));       // write unique stream identifier
  FStream.Write(FnAttributes, SizeOf(Integer));    // now we write the number of attributes
  For i := 1 To FnAttributes Do
    If Assigned(OnStreamWriteAttribute) Then
      OnStreamWriteAttribute(Self, i);

  If aStream <> nil Then begin
    FStream := OldStream; // restore our stream pointer
    FpDataStorageQueryToken^.aObj := FStream;
  end;  //  --  If aStream <> nil Then
end;

procedure TTemplateStreamer.ReadString(var S: String);
var i : Integer;
    b : Byte;
    aString : AnsiString;
    {$IFNDEF Unicode}
    wString : WideString;
    {$ENDIF}
begin
  FStream.Read(b, SizeOf(Byte));
  If ((b <> sdtString) And (b <> sdtWideString) And (b <> sdtPAnsiChar)) Then
    Raise EComponentError.Create('TTemplateStreamer.ReadString : Next stream data isn''t a string');
  FStream.Read(i, SizeOf(Integer));
  SetLength(S, i);
  Case b Of
    sdtPAnsiChar,
    sdtString     : begin
                      SetLength(aString, i);
                      FStream.Read(Pointer(aString)^, i);
                      S := String(aString);
                    end;
    {$IFDEF Unicode}
    sdtWideString : FStream.Read(Pointer(S)^, i * SizeOf(Char));
    {$ELSE}
    sdtWideString : begin
                      SetLength(wString, i);
                      FStream.Read(Pointer(wString)^, i * 2);
                      S := String(wString);
                    end;
    {$ENDIF}
  end;
end;

function TTemplateStreamer.ReadString: String;
var S: String;
begin
  ReadString(S);
  Result := S;
end;

procedure TTemplateStreamer.ReadStringA(var S: AnsiString);
var i : Integer;
    b : Byte;
begin
  FStream.Read(b, SizeOf(Byte));
  If b <> sdtString Then
    Raise EComponentError.Create('TTemplateStreamer.ReadStringA : Next stream data isn''t a AnsiString');
  FStream.Read(i, SizeOf(Integer));
  SetLength(S, i);
  FStream.Read(Pointer(S)^, i);
end;

function TTemplateStreamer.ReadStringA: AnsiString;
var S: AnsiString;
begin
  ReadStringA(S);
  Result := S;
end;

procedure TTemplateStreamer.WriteStringA(S: AnsiString);
var i : Integer;
    b : Byte;
begin
  b := sdtString;
  FStream.Write(b, SizeOf(Byte));
  i := Length(S);  // pure printable chars
  FStream.Write(i, SizeOf(Integer));
  FStream.Write(Pointer(S)^, Length(S));
end;

procedure TTemplateStreamer.WriteString(S: String);
var i : Integer;
    b : Byte;
begin
  If SizeOf(Char) = 1 Then b := sdtString
                      Else b := sdtWideString;
  FStream.Write(b, SizeOf(Byte));
  i := Length(S);  // pure printable chars
  FStream.Write(i, SizeOf(Integer));
  FStream.Write(Pointer(S)^, Length(S) * SizeOf(Char));
end;

procedure TTemplateStreamer.ReadInteger(var i: Integer);
var b : Byte;
begin
  FStream.Read(b, SizeOf(Byte));
  If b <> sdtInteger Then
    Raise EComponentError.Create('TTemplateStreamer.ReadInteger : Next stream data isn''t an Integer');
  FStream.Read(i, SizeOf(Integer));
end;

function TTemplateStreamer.ReadInteger: Integer;
var i: Integer;
begin
  ReadInteger(i);
  Result := i;
end;

procedure TTemplateStreamer.WriteInteger(i: Integer);
var b : Byte;
begin
  b := sdtInteger;
  FStream.Write(b, SizeOf(Byte));
  FStream.Write(i, SizeOf(Integer));
end;

procedure TTemplateStreamer.ReadPChar(var PC: PChar);
var i : Integer;
{$IFDEF Unicode}
    S : String;
{$ELSE}
    S : AnsiString;
{$ENDIF}
begin
  ReadString(S);
  i := Length(S) * SizeOf(Char); // with AnsiString it returns number of bytes, with widestring Number of bytes divided by 2 (SizeOf(Char))
{  FStream.Read(b, SizeOf(Byte));
  If b <> sdtPAnsiChar Then
    Raise EComponentError.Create('TTemplateStreamer.ReadPAnsiChar : Next stream data isn''t an PAnsiChar');
  FStream.Read(i, SizeOf(Integer));}
  GetMem(PC, i + SizeOf(Char));
  FillChar(PC^, i + SizeOf(Char), 0);
  StrLCopy(PC, PChar(S), i);
//  FStream.Read(PC^, i * SizeOf(Char));
end;

procedure TTemplateStreamer.WritePChar(PC: PChar);
var
{$IFDEF Unicode}
    S : WideString;
{$ELSE}
    S : AnsiString;
{$ENDIF}
begin
  S := String(PC);
  WriteString(S);
{  b := sdtPAnsiChar;
  FStream.Write(b, SizeOf(Byte));
  i := Length(String(PC));  // pure printable chars
  FStream.Write(i, SizeOf(Integer));
  FStream.Write(PC^, i * SizeOf(Char)); }
end;

procedure TTemplateStreamer.ReadDateTime(var dtVar: TDateTime);
var b : Byte;
begin
  FStream.Read(b, SizeOf(Byte));
  If b <> sdtDateTime Then
    Raise EComponentError.Create('TTemplateStreamer.ReadDateTime : Next stream data isn''t an TDateTime');
  FStream.Read(dtvar, SizeOf(TdateTime));
end;

function TTemplateStreamer.ReadDateTime: TDateTime;
var dt: TDateTime;
begin
  ReadDateTime(dt);
  Result := dt;
end;

procedure TTemplateStreamer.WriteDateTime(dtVar: TDateTime);
var b : Byte;
begin
  b := sdtDateTime;
  FStream.Write(b, SizeOf(Byte));
  FStream.Write(dtVar, SizeOf(TDateTime));
end;

procedure TTemplateStreamer.ReadBoolean(var Value: Boolean);
var b, bTag : Byte;
begin
  FStream.Read(bTag, SizeOf(Byte));
  If bTag <> sdtBoolean Then
    Raise EComponentError.Create('TTemplateStreamer.ReadBoolean : Next stream data isn''t a Boolean');
  FStream.Read(B, SizeOf(Byte));
  Value := (B > 0);
end;

function TTemplateStreamer.ReadBoolean: Boolean;
var b: Boolean;
begin
  ReadBoolean(b);
  Result := b;
end;

procedure TTemplateStreamer.WriteBoolean(Value: Boolean);
var b, bTag : Byte;
begin
  bTag := sdtBoolean;
  FStream.Write(bTag, SizeOf(Byte));
  If Value Then B := 1
           Else B := 0;
  FStream.Write(B, SizeOf(Byte));
end;

procedure TTemplateStreamer.ReadByte(var B: Byte);
var bTag : Byte;
begin
  FStream.Read(bTag, SizeOf(Byte));
  If bTag <> sdtByte Then
    Raise EComponentError.Create('TTemplateStreamer.ReadByte : Next stream data isn''t a Byte');
  FStream.Read(B, SizeOf(Byte));
end;

function TTemplateStreamer.ReadByte: Byte;
var b: Byte;
begin
  ReadByte(b);
  Result := b;
end;

procedure TTemplateStreamer.WriteByte(B: Byte);
var bTag : Byte;
begin
  bTag := sdtByte;
  FStream.Write(bTag, SizeOf(Byte));
  FStream.Write(B, SizeOf(Byte));
end;

procedure TTemplateStreamer.WriteInt64(Value: Int64);
var bTag : Byte;
begin
  bTag := sdtInt64;
  FStream.Write(bTag, SizeOf(Byte));
  FStream.Write(Value, SizeOf(Int64));
end;

procedure TTemplateStreamer.ReadInt64(var Value: Int64);
var bTag : Byte;
begin
  FStream.Read(bTag, SizeOf(Byte));
  If bTag <> sdtInt64 Then
    Raise EComponentError.Create('TTemplateStreamer.ReadInt64 : Next stream data isn''t an Int64');
  FStream.Read(Value, SizeOf(Int64));
end;

function TTemplateStreamer.ReadInt64: Int64;
var i: Int64;
begin
  ReadInt64(i);
  Result := i;
end;

procedure TTemplateStreamer.WriteWord(Value: Word);
var bTag : Byte;
begin
  bTag := sdtWord;
  FStream.Write(bTag, SizeOf(Byte));
  FStream.Write(Value, SizeOf(Word));
end;

procedure TTemplateStreamer.ReadWord(var Value: Word);
var bTag : Byte;
begin
  FStream.Read(bTag, SizeOf(Byte));
  If bTag <> sdtWord Then
    Raise EComponentError.Create('TTemplateStreamer.ReadWord : Next stream data isn''t a Word');
  FStream.Read(Value, SizeOf(Word));
end;

function TTemplateStreamer.ReadWord: Word;
var w: Word;
begin
  ReadWord(w);
  Result := w;
end;

procedure TTemplateStreamer.WriteCardinal(Value: Cardinal);
var bTag : Byte;
begin
  bTag := sdtCardinal;
  FStream.Write(bTag, SizeOf(Byte));
  FStream.Write(Value, SizeOf(Cardinal));
end;

procedure TTemplateStreamer.ReadCardinal(var Value: Cardinal);
var bTag : Byte;
begin
  FStream.Read(bTag, SizeOf(Byte));
  If bTag <> sdtCardinal Then
    Raise EComponentError.Create('TTemplateStreamer.ReadCardinal : Next stream data isn''t a Cardinal');
  FStream.Read(Value, SizeOf(Cardinal));
end;

function TTemplateStreamer.ReadCardinal: Cardinal;
var c: Cardinal;
begin
  ReadCardinal(c);
  Result := c;
end;

end.
