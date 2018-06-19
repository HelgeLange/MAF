{*******************************************************************************
Name         : uMAF_Tools.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007-2011 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 20.03.2007
Last Update  : 15.11.2011
Version      : 1.0.004
Purpose      :
Last Changes :

1.0.004 (15.11.2011) -----------------------------------------------------------
- [FIX] fixed Encrypt and Decrypt (AnsiString versions), which produced under
        Delphi 2009+ wrong results due to the size change of Char
1.0.003 (17.06.2009) -----------------------------------------------------------
- [CHG] StrToPChar changed to a function and returns now the amount of allocated
        memory
1.0.002 (29.10.2008) -----------------------------------------------------------
- [ADD] FreePChar to release PChars previously allocated by StrToPChar
- [CHG] StrToPChar updated to reflect the changes with Delphi 2009
1.0.001 (20.03.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_Tools;

interface

uses Windows, SysUtils, Classes;

type IP_HostName = (_IP, _HOSTNAME);

function IsClass(pObject: TObject; pClass: TClass): Boolean;
function GetLocalIPAddressOrHostName(IP_Or_HostName: IP_HostName): AnsiString;
function SendComponentMessage(FpTarget: TComponent; MsgID: Integer; WParam, LParam: Pointer; bSubComps: Boolean = False): Integer;
function StrToPChar(S: String; var PC: PChar): Integer;
function StrToPCharA(S: AnsiString; var PC: PAnsiChar): Integer;
procedure FreePChar(var PC: PChar);
procedure FreePCharA(var PC: PAnsiChar);
function GetIDFromString(var s: String): Integer;
procedure __CreateComponent(AClass: TComponentClass; var Reference; AOwner: TComponent);

function Decrypt(const InString: AnsiString; StartKey, MultKey, AddKey: Integer): AnsiString; overload;
function Encrypt(const InString: AnsiString; StartKey, MultKey, AddKey: Integer): AnsiString; overload;
function Decrypt(const InString: WideString; StartKey, MultKey, AddKey: Integer): WideString; overload;
function Encrypt(const InString: Widestring; StartKey, MultKey, AddKey: Integer): WideString; overload;

function ComputeCRC32(aStream: TMemoryStream) : DWORD; overload;
function ComputeCRC32(S: String) : DWORD; overload;

implementation

uses WinSock, Messages, Dialogs{, Forms};

function IsClass(pObject: TObject; pClass: TClass): boolean;
var  ClassRef: TClass;
     nSize1, nSize2: LongInt;
begin
  Result := False; // pessimistic start
  If (pObject <> nil) Then begin
    ClassRef := pObject.ClassType; // get its classetyp
    // as long as Classtype isn't empty and also the ClassName not the one we
    // look for, we go up until we at the top or found the classtype
    While (ClassRef <> nil) and (ClassRef.ClassName <> pClass.ClassName) do
      ClassRef := ClassRef.ClassParent;
    // if we found the type we were looking for
    If (ClassRef <> nil) and (ClassRef.ClassNameIs(pClass.ClassName)) Then begin
      nSize1 := ClassRef.InstanceSize;
      nSize2 := pClass.InstanceSize;
      Result := TRUE and (nSize1 = nSize2); // sind alle glücklich
    end;  //  --  If (ClassRef <> nil) and (ClassRef.ClassNameIs(pClass.ClassName)) Then
  end;  //  --  If (pObject <> nil) Then
end; // IsClass

function GetLocalIPAddressOrHostName(IP_Or_HostName: IP_HostName): AnsiString;
var
  p : PHostEnt;
  s : array[0..128] of char;
  p2 : PAnsiChar;
  wVersionRequested : WORD;
  wsaData : TWSAData;
begin
  {Start up WinSock}
  wVersionRequested := MAKEWORD(1, 1);
  WSAStartup(wVersionRequested, wsaData);
  {Get the computer name} 
  GetHostName(@s, 128);
  p := GetHostByName(@s);
  if IP_Or_HostName = _HOSTNAME then
    Result := p^.h_Name
  else
    begin
      {Get the IpAddress}
      p2 := iNet_ntoa(PInAddr(p^.h_addr_list^)^);
      Result := p2;
    end;
  {Shut down WinSock}
  WSACleanup;
end;

// ********************************* Comments **********************************
// Description : sends a message to a component
// Param (in)  : FpTarget=Receiver Component
//               MsgID=ID of the message
//               WParam, LParam=user defined pointers
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 13.01.2007
// Last Update : 13.01.2007
// *****************************************************************************
function SendComponentMessage(FpTarget: TComponent; MsgID: Integer; WParam, LParam: Pointer; bSubComps: Boolean = False): Integer;
var Msg: TMessage;
    i : Integer;
begin
  Result := 0;
  If Not Assigned(FpTarget) Then
    Exit;
  FillChar(Msg, SizeOf(TMessage), 0); // clear record
  Msg.Msg := MsgID;                   // Message ID
  Msg.WParam := Integer(WParam);      // setting user defined pointers
  Msg.LParam := Integer(LParam);
  Try
    TComponent(FpTarget).Dispatch(Msg); // sending the message
    If bSubComps Then begin
      For i := 0 To TComponent(FpTarget).ComponentCount - 1 Do
        TComponent(FpTarget).Components[i].Dispatch(Msg);
    end;
  Except
    // TODO: we can add CodeSite message output here
  End;
  Result := Msg.Result;
end; // SendComponentMessage

// ********************************* Comments **********************************
// Description : copies a String into a PChar and allocates the memory for it
// Param (in)  : S=Source-String;
//               PC=Destination PChar
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 11.01.2004
// Last Update : 17.06.2009
// *****************************************************************************
function StrToPChar(S: String; var PC: PChar): Integer;
var i : Integer;
begin
  i := Length(S) * SizeOf(Char);     // faster, if we do it only once
  ReAllocMem(PC, i+SizeOf(Char));    // +SizeOf(Char) because pchar is zero terminated
  FillChar(PC^, i+SizeOf(Char), 0);  // fill with 0
  StrLCopy(PC, PChar(S), i);         // and copy from the source
  Result := i + SizeOf(Char);        // and return the amount of allocated memory
end; // StrToPChar

function StrToPCharA(S: AnsiString; var PC: PAnsiChar): Integer;
var i : Integer;
begin
  i := Length(S) * SizeOf(AnsiChar);     // faster, if we do it only once
  ReAllocMem(PC, i+SizeOf(AnsiChar));    // +SizeOf(Char) because pchar is zero terminated
  FillChar(PC^, i+SizeOf(AnsiChar), 0);  // fill with 0
  StrLCopy(PC, PAnsiChar(S), i);         // and copy from the source
  Result := i + SizeOf(AnsiChar);        // and return the amount of allocated memory
end;

// ********************************* Comments **********************************
// Description : destroys a PChar previously allocated by StrToPChar
// Param (in)  : PC=PChar
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 29.10.2008
// Last Update : 29.10.2008
// *****************************************************************************
procedure FreePChar(var PC: PChar);
begin
  If PC <> nil Then begin
    FreeMem(PC, Length(PC) * SizeOf(Char) + SizeOf(Char)); // length + closing 0
    PC := nil;
  end;  //  --  If PC <> nil Then
end; // FreePChar

procedure FreePCharA(var PC: PAnsiChar);
begin
  If PC <> nil Then begin
    FreeMem(PC, Length(PC) + 1); // length + closing 0
    PC := nil;
  end;  //  --  If PC <> nil Then
end;

// ********************************* Comments **********************************
// Description : extracts a number from an string and deletes it from there
// Param (in)  : S=String w/ IDs
// Param (out) : extracted ID
// Coding by   : Helge Lehn
// Date        : 04.02.2002
// Last Update : 11.01.2004
// *****************************************************************************
function GetIDFromString(var s: String): Integer;
var temp : String;
begin
  If Length(s) < 3 Then begin
    Result := -1;
    Exit;
  end;  //  --  If Length(s) < 3 Then
  temp := '';
  Delete(s, 1, 1);  // delete space in front of
  While s[1] <> ' ' Do begin
    temp := temp + s[1];
    Delete(s, 1, 1); // and delete this char
  end;  //  --  While s[1] <> ' ' Do
  Delete(s, 1, 1); // delete terminating space
  Result := StrToIntDef(temp, 0);
end; // GetIDFromString

procedure __CreateComponent(AClass: TComponentClass; var Reference; AOwner: TComponent);
var Instance: TComponent;
begin
//  Reference := TComponent(AClass.Create(AOwner));

  Instance := TComponent(AClass.NewInstance);
  TComponent(Reference) := Instance;
  try
    Instance.Create(AOwner);
  except
    On E: Exception Do begin
      ShowMessage('MAF Exception in uMAF_Tools.__CreateComponent: ' + #13#10 + E.Message);
      TComponent(Reference) := nil;
    end;
  end;  //  --  try... except
end;

{*******************************************************
 * Standard Encryption algorithm - Copied from Borland *
 *******************************************************}
function Encrypt(const InString: Widestring; StartKey, MultKey, AddKey: Integer): WideString;
var I : Integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    Result := Result + WideChar(WORD(InString[I]) xor (StartKey shr 8));
    StartKey := (WORD(Result[I]) + StartKey) * MultKey + AddKey;
  end;
end;


{*******************************************************
 * Standard Decryption algorithm - Copied from Borland *
 *******************************************************}
function Decrypt(const InString: WideString; StartKey, MultKey, AddKey: Integer): WideString;
var I : Integer;
    S : String;
begin
  S := '';
  for I := 1 to Length(InString) do
  begin
    S := S + WideChar(WORD(InString[I]) xor (StartKey shr 8));
    StartKey := (WORD(InString[I]) + StartKey) * MultKey + AddKey;
  end;
  Result := S;
end;

{*******************************************************
 * Standard Encryption algorithm - Copied from Borland *
 *******************************************************}
function Encrypt(const InString: AnsiString; StartKey,MultKey,AddKey:Integer): AnsiString;
var I, j : Byte;
begin
  Result := '';
  {$Q-}
  for I := 1 to Length(InString) do
  begin
    Result := Result + AnsiChar(Byte(InString[I]) xor (StartKey shr 8));
    StartKey := (Byte(Result[I]) + StartKey) * MultKey + AddKey;

{    Result := Result + AnsiChar(Byte(InString[I]) xor (StartKey shr 8));
    j := Byte(Result[I]);
    asm
      xor eax, eax
      mov al, [j]
      add eax, [StartKey]
      mov ebx, [MultKey]
      mul eax, ebx
      add eax, [AddKey]
      mov [StartKey], eax
    end;
//    StartKey := (Byte(Result[I]) + StartKey) * MultKey + AddKey; }
  end;
  {$Q+}
end;


{*******************************************************
 * Standard Decryption algorithm - Copied from Borland *
 *******************************************************}
function Decrypt(const InString: AnsiString; StartKey,MultKey,AddKey:Integer): AnsiString;
var I, j : Byte;
    S : AnsiString;
    S1 : ShortString;
begin
  S := '';
{  S1 := ShortString(InString);
  for I := 1 to Length(S1) do
  begin
    S := S + AnsiChar(Byte(S1[I]) xor (StartKey shr 8));
    j := Byte(S1[I]);
    Asm
      xor eax, eax
      mov al, [j]
      add eax, [StartKey]
      mov ebx, [MultKey]
      mul eax, ebx
      add eax, [AddKey]
      mov [StartKey], eax
    End;
  end; }

  {$Q-}
  for I := 1 to Length(InString) do
  begin
    S := S + AnsiChar(Byte(InString[I]) xor (StartKey shr 8));
    StartKey := (Byte(InString[I]) + StartKey) * MultKey + AddKey;
  end;
  {$Q+}
  Result := S;
  S := '';
end;

type
  CRCTable = array [0..255] of DWORD;

const
  BufLen = 32768;

  CRC32Table : CRCTable =
    ($00000000, $77073096, $ee0e612c, $990951ba,
     $076dc419, $706af48f, $e963a535, $9e6495a3,
     $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988,
     $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,

     $1db71064, $6ab020f2, $f3b97148, $84be41de,
     $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
     $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
     $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,

     $3b6e20c8, $4c69105e, $d56041e4, $a2677172,
     $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
     $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940,
     $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,

     $26d930ac, $51de003a, $c8d75180, $bfd06116,
     $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
     $2802b89e, $5f058808, $c60cd9b2, $b10be924,
     $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,

     $76dc4190, $01db7106, $98d220bc, $efd5102a,
     $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
     $7807c9a2, $0f00f934, $9609a88e, $e10e9818,
     $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,

     $6b6b51f4, $1c6c6162, $856530d8, $f262004e,
     $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
     $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c,
     $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,

     $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2,
     $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
     $4369e96a, $346ed9fc, $ad678846, $da60b8d0,
     $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,

     $5005713c, $270241aa, $be0b1010, $c90c2086,
     $5768b525, $206f85b3, $b966d409, $ce61e49f,
     $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4,
     $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,

     $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a,
     $ead54739, $9dd277af, $04db2615, $73dc1683,
     $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
     $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,

     $f00f9344, $8708a3d2, $1e01f268, $6906c2fe,
     $f762575d, $806567cb, $196c3671, $6e6b06e7,
     $fed41b76, $89d32be0, $10da7a5a, $67dd4acc,
     $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,

     $d6d6a3e8, $a1d1937e, $38d8c2c4, $04fdff252,
     $d1bb67f1, $a6bc5767, $3fb506dd, $048b2364b,
     $d80d2bda, $af0a1b4c, $36034af6, $041047a60,
     $df60efc3, $a867df55, $316e8eef, $04669be79,

     $cb61b38c, $bc66831a, $256fd2a0, $5268e236,
     $cc0c7795, $bb0b4703, $220216b9, $5505262f,
     $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04,
     $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,

     $9b64c2b0, $ec63f226, $756aa39c, $026d930a,
     $9c0906a9, $eb0e363f, $72076785, $05005713,
     $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38,
     $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,

     $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e,
     $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
     $88085ae6, $ff0f6a70, $66063bca, $11010b5c,
     $8f659eff, $f862ae69, $616bffd3, $166ccf45,

     $a00ae278, $d70dd2ee, $4e048354, $3903b3c2,
     $a7672661, $d06016f7, $4969474d, $3e6e77db,
     $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0,
     $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,

     $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6,
     $bad03605, $cdd70693, $54de5729, $23d967bf,
     $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94,
     $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d);

function UpdateCRC32(InitCRC : DWORD; BufPtr : Pointer; Len : Word) : LongInt;
var i, crc   : DWORD;
    index : Integer;
begin
  crc := InitCRC;
  For i := 0 to Len - 1 Do begin
    index := (crc  xor DWORD(Pointer(DWORD(BufPtr)+i)^)) and $000000FF;
    crc := ((crc shr 8) and $00FFFFFF) xor CRC32Table[index];
  end;
  Result := crc;
end;

function ComputeCRC32(aStream: TMemoryStream) : DWORD;
var crc32  : DWORD;
    i : Integer;
    iLen : Int64;
    Buf : Pointer;
begin
   aStream.Position := 0;
   crc32 := $FFFFFFFF;
   iLen := aStream.Size;
   GetMem(Buf, BufLen);
   While iLen > 0 Do begin
     If iLen > BufLen Then begin
       i := BufLen;
       Dec(iLen, BufLen);
     end else begin
       i := iLen;
       iLen := 0;
     end;

     aStream.Read(Buf^, i);
     crc32 := UpdateCRC32(CRC32, Buf, i);
   end;
   FreeMem(Buf, BufLen);
   aStream.Position := 0;
   crc32 := Not crc32;
   Result := crc32;
end;

function ComputeCRC32(S: String) : DWORD; overload;
var crc32  : DWORD;
    i : Integer;
    iLen : Int64;
    Buf : Pointer;
begin
   crc32 := $FFFFFFFF;
   iLen := Length(S);
   GetMem(Buf, BufLen);
   FillChar(Buf^, BufLen, 0);
   While iLen > 0 Do begin
     If iLen > BufLen Then begin
       i := BufLen;
       Dec(iLen, BufLen);
     end else begin
       i := iLen;
       iLen := 0;
     end;
     StrLCopy(PChar(Buf), PChar(S), i);
//     aStream.Read(Buf^, i);
     crc32 := UpdateCRC32(CRC32, Buf, i);
   end;
   FreeMem(Buf, BufLen);
   crc32 := Not crc32;
   Result := crc32;
end;

end.
