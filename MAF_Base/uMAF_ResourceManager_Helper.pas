unit uMAF_ResourceManager_Helper;

interface

uses Windows, Classes, SysUtils, uMAF_Core;

const HEADER_ID_FILE_RESOURCE   = 27765;
      HEADER_ID_STRING_RESOURCE = 86995;
      HEADER_ID_SQL_RESOURCE    = 64633;
      HEADER_ID_FILEDB_RESOURCE = 58933;

      MAX_BYTES                 = 64000; // max bytes for BlockRead and BlockWrite
      HEADER_IMAGE_RES_STRING   : Array [0..6] Of AnsiChar = ('M','A','F','_','R','E','S');
      HEADER_TEXT_RES_STRING    : Array [0..6] Of AnsiChar = ('M','A','F','_','T','X','T');
      HEADER_SQL_RES_STRING     : Array [0..6] Of AnsiChar = ('M','A','F','_','S','Q','L');
      HEADER_FDB_RES_STRING     : Array [0..6] Of AnsiChar = ('M','A','F','_','F','D','B');

      sFileResource   = 'FileResource';
      sStringResource = 'StringResource';
      sSQLResource    = 'SQLResource';
      SFileDBResource = 'FileDBResource';

     Encoding_UTF_8  : Byte = 1;
     Encoding_UTF_16 : Byte = 2;

Type RFileResourceHeader = packed record
       HeaderString      : Array [0..6] Of AnsiChar;
       HeaderID          : Integer;
       FileMapping_Start : Int64;
       FileMapping_Size  : Int64;
       NumItems          : Cardinal;
       Encoding          : Byte;
       Author            : Array[1..99] Of AnsiChar;   // who made the file
       Date              : Real;                    // when was it made
     end; //RFileResourceHeader
     PFileResourceHeader = ^RFileResourceHeader;

     RDeletedData = packed record
       nStart : Int64;
       nLength : Int64;
     end;
     PDeletedData = ^RDeletedData;

     RFolderNode = packed record
       nID : Integer;
       ParentNode : Integer;
       NodeName : String;
       FpChildren : TList;     // list of PFolderNode
       FpDataList : TList;     // list of PFileResourceDescriptor - do  NOT  free !
     end; // RDirectoryNode
     PFolderNode = ^RFolderNode;

     // both PFileResourceDescriptor and PStringResourceDescriptor can be casted
     // to PBaseDescriptor to get the base data from both types without knowing,
     // wich type exactly we're dealing with
     RBaseDescriptor = packed record
       ID : Cardinal;
       nStart : Int64;
       nLength : Int64;
       CategoryID : Integer;
       ModuleID : Integer;
     end;
     PBaseDescriptor = ^RBaseDescriptor;

     // the FileDescriptor is used for both the FileDatabase and the ResourceFile
     // The RFileResourceDescriptor can be casted on RFileDescriptor as long as
     // the members ResX and ResY aren't needed
     RFileDescriptor = packed record
       ID : Cardinal;
       nStart : Int64;
       nLength : Int64;
       CategoryID : Integer;
       ModuleID : Integer;
       ResName : String;
       FolderNode : Integer;
       Flags : Byte;
       RefCount : Integer;
       aStream : TMemoryStream;
     end;
     PFileDescriptor = ^RFileDescriptor;

     RFileResourceDescriptor = packed record
       ResID : Cardinal;
       nStart : Int64;
       nLength : Int64;
       CategoryID : Integer;
       ModuleID : Integer;
       ResName : String;
       FolderNode : Integer;
       Flags : Byte;
       RefCount : Integer;
       aStream : TMemoryStream;
       ResX : Word;
       ResY : Word;
     end; // RFileResourceDescriptor
     PFileResourceDescriptor = ^RFileResourceDescriptor;

     RStringResourceDescriptor = packed record
       StringID : Cardinal;
       nStart : Int64;
       nLength : Int64;
       CategoryID : Integer;
       ModuleID : Integer;
       pData : PChar;
     end;
     PStringResourceDescriptor = ^RStringResourceDescriptor;

function __Create_FileResourceDescriptor: PFileResourceDescriptor; overload;
function __Create_FileResourceDescriptor(FileName: String): PFileResourceDescriptor; overload;
procedure __Free_FileResourceDescriptor(var pData: PFileResourceDescriptor);

function __Create_FileDescriptor: PFileDescriptor; overload;
function __Create_FileDescriptor(FileName: String): PFileDescriptor; overload;
procedure __Free_FileDescriptor(var pData: PFileDescriptor);

function __Create_ResourceHeader(AType: TResourceFileType): PFileResourceHeader;

function __Create_StringResourceDescriptor: PStringResourceDescriptor;
procedure __Free_StringResourceDescriptor(var pData: PStringResourceDescriptor);
function __Create_FolderNode(AParentID: Integer): PFolderNode;
procedure __Free_FolderNode(var aNode : PFolderNode);

function __GetResourceIDString(ID: Cardinal): String;

implementation

uses Graphics, ExtCtrls, Dialogs;

function __GetResourceIDString(ID: Cardinal): String;
begin
  Case ID Of
    HEADER_ID_FILE_RESOURCE   : Result := sFileResource;
    HEADER_ID_STRING_RESOURCE : Result := sStringResource;
    HEADER_ID_SQL_RESOURCE    : Result := sSQLResource;
    HEADER_ID_FILEDB_RESOURCE : Result := SFileDBResource;
  end;
end;

function __Create_FileDescriptor: PFileDescriptor; overload;
begin
  New(Result);
  FillChar(Result^, SizeOf(RFileDescriptor), 0);
  Result^.FolderNode := -1;
end;

function __Create_FileDescriptor(FileName: String): PFileDescriptor; overload;
var aStream : TFileStream;
begin
  aStream := TFileStream.Create(FileName, fmOpenRead);
  Result := __Create_FileDescriptor;
  Result^.ResName := ExtractFileName(FileName);
  Result^.nLength := aStream.Size;
  Result^.aStream := TMemoryStream.Create;
  Result^.aStream.LoadFromStream(aStream);
  Result^.aStream.Position := 0;
  aStream.Free;
end;

procedure __Free_FileDescriptor(var pData: PFileDescriptor);
begin
  If Assigned(pData) Then begin
    If Assigned(pData^.aStream) Then
      FreeAndNil(pData^.aStream);
    Dispose(pData);
    pData := nil;
  end;  //  --  If Assigned(pData) Then
end;

function __Create_FileResourceDescriptor: PFileResourceDescriptor;
begin
  New(Result);
  FillChar(Result^, SizeOf(RFileResourceDescriptor), 0);
  Result^.FolderNode := -1;
end;

procedure __Free_FileResourceDescriptor(var pData: PFileResourceDescriptor);
begin
  If Assigned(pData) Then begin
    If Assigned(pData^.aStream) Then begin
      pData^.aStream.Size := 0;
      pData^.aStream.Free;
      pData^.aStream := nil;
//      FreeAndNil(pData^.aStream);
    end;
    Dispose(pData);
    pData := nil;
  end;  //  --  If Assigned(pData) Then
end;

function __Create_FileResourceDescriptor(FileName: String): PFileResourceDescriptor;
var Image : TImage;
    aStream : TFileStream;
    S : String;
begin
  aStream := TFileStream.Create(FileName, fmOpenRead);
  New(Result);
  FillChar(Result^, SizeOf(RFileResourceDescriptor), 0);
  Result^.ResName := ExtractFileName(FileName);
  Result^.nLength := aStream.Size;
  Result^.aStream := TMemoryStream.Create;
  Result^.aStream.LoadFromStream(aStream);
  Result^.aStream.Position := 0;
  aStream.Free;

  S := ExtractFileExt(FileName);
  If Pos(S, GraphicFilter(TGraphic)) > 0 Then begin
    Image := TImage.Create(nil);
    Image.Picture.LoadFromFile(FileName);
    Result^.ResX := Image.Picture.Graphic.Height;
    Result^.ResY := Image.Picture.Graphic.Width;
    Result^.FolderNode := -1;
    Image.Free;
  end else begin
    Result^.ResX := 0;
    Result^.ResY := 0;
  end;
end;

function __Create_ResourceHeader(AType: TResourceFileType): PFileResourceHeader;
begin
  New(Result);
  FillChar(Result^, SizeOf(RFileResourceHeader), 0);
  Case AType Of
    rftMedia  : CopyMemory(@Result^.HeaderString[0], @HEADER_IMAGE_RES_STRING[0], 7);
    rftString : CopyMemory(@Result^.HeaderString[0], @HEADER_TEXT_RES_STRING[0], 7);
    rftSQL    : CopyMemory(@Result^.HeaderString[0], @HEADER_SQL_RES_STRING[0], 7);
    rftFileDB : CopyMemory(@Result^.HeaderString[0], @HEADER_FDB_RES_STRING[0], 7);
  end;
end;

function __Create_StringResourceDescriptor: PStringResourceDescriptor;
begin
  New(Result);
  FilLChar(Result^, SizeOf(RStringResourceDescriptor), 0);
end;

procedure __Free_StringResourceDescriptor(var pData: PStringResourceDescriptor);
begin
  If Assigned(pData) Then begin
    If ((pData^.nLength > 0) And (pData^.pData <> nil)) Then
      FreeMem(pData^.pData, pData^.nLength);
    Dispose(pData);
    pData := nil;
  end;  //  --  If Assigned(pData) Then
end;

function __Create_FolderNode(AParentID: Integer): PFolderNode;
begin
  New(Result);
  FillChar(Result^, SizeOf(RFolderNode), 0);
  Result^.ParentNode := AParentID;
  Result^.FpChildren := TList.Create;
  Result^.FpDataList := TList.Create;
end;

procedure __Free_SubFolder(var pNode: PFolderNode);
var aNode : PFolderNode;
    i : Integer;
begin
  If Not Assigned(pNode) Then
    Exit;
  For i := 0 To pNode.FpChildren.Count - 1 Do begin
    aNode := PFolderNode(pNode^.FpChildren.Items[i]);
    If aNode^.FpChildren.Count > 0 Then
      __Free_SubFolder(aNode);
  end;  //  --  For i := 0 To pNode.FpChildren.Count - 1 Do
  pNode^.FpChildren.Free;
  pNode^.FpDataList.Free;
  Dispose(pNode);
  aNode := nil;
end;

procedure __Free_FolderNode(var aNode : PFolderNode);
var i : Integer;
    pNode : PFolderNode;
begin
  If aNode = nil Then
    Exit;
  For i := aNode.FpChildren.Count -1 DownTo 0 Do begin
    pNode := PFolderNode(aNode^.FpChildren.Items[i]);
    __Free_SubFolder(pNode);
  end;  //  --  For i := aNode.FpChildren.Count -1 DownTo 0 Do
  aNode^.FpChildren.Free;
  aNode^.FpDataList.Free;
  Dispose(aNode);
  aNode := nil;
end;

end.
