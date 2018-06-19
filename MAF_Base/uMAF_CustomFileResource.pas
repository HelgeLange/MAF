{*******************************************************************************
Name         : uMAF_CustomFileResource.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2010 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 11.06.2010
Last Update  : 15.06.2010
Version      : 1.0.000
Purpose      : The filesystem is cluster based. On creation of the resource file
               a cluster size can be defined. Each file can use 1..n cluster,
               OnDeletion the cluster used by the deleted file will be marked as
               cfEmpty and will be re-used next time a file is written. The
               defragmentation caused by this system can be fixed by calling
               Defrag on an open resource file.

               The resource file is build as follow :

               -----------------------------------------------------------------
               |                            Header                             |
               -----------------------------------------------------------------
               |   Cluster 0-n: System Cluster with file system information    |
               -----------------------------------------------------------------
               |   Cluster n- file end : files                                 |
               -----------------------------------------------------------------
Last Changes :


1.0.000 (11.06.2010) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_CustomFileResource;

interface

uses Windows, Classes, SysUtils, Messages,

     uMAF_Core, uMAF_Globals, uMAF_Tools, uMAF_TemplateStreamer;

Type TClusterFlag = (cfUsed, cfEmpty, cfSystem);

     RClusterDesc = packed record
       nID : Integer;         // ID of the cluster
       nStart : Int64;        // start position in the file
       nUsed : Word;          // number of bytes used in the cluster
       Flag : TClusterFlag;   // the flags
       // the following members are will be written but calculated when the resource file is loaded
       File_uID : Integer;    // uID of the file using the cluster
     end;
     PClusterDesc = ^RClusterDesc;

     // ffEncrypted   : file is encrypted
     // ffCompressed  : file is compressed
     // ffSystem      : file is a system file (invisible for user)
     // ffModified    : file is modified and needs to be saved as well as to clear old used cluster
     // ffUnsaved     : file is new and needs to be saved
     // ffCache       : file can be cached (stream will not be cleared after loading)
     TFileFlag = (ffEncrypted, ffCompressed, ffSystem, ffModified, ffUnsaved, ffCache);
     TFileFlags = Set Of TFileFlag;

     RFileDesc = packed record
       uID : Integer;              // unique ID for the file within the resource file
       nPublicID : Integer;        // public ID that is only unique within its own folder
       nFolderID : Integer;        // folder node ID, where the file belongs to
       sName : String;             // name of the file
       nSize : Integer;            // size of the file
       nClusterUsed : Integer;     // number of Cluster used
       pUsedCluster : Array Of PClusterDesc; // the cluster used to store the file, array of PClusterDesc pointer to have direct access
       Flags : TFileFlags;         // file flags
       CRC32 : DWORD;              // the checksum
       // the following members are will be written but calculated when the resource file is loaded
       aStream : TMemoryStream; // stream to hold the file when loaded
       RefCount : Word;         // cached files hold a RefCount and the file will be unloaded when it reaches 0
     end;
     PFileDesc = ^RFileDesc;

Type RFileResourceHeader = packed record
       HeaderString      : Array [0..5] Of AnsiChar;  // header string
       HeaderID          : Integer;                   // header ID
       ClusterSize       : Word;                      // size of each cluster
       ClusterCount      : Int64;                     // number of cluster in the file (System, used and unused)
       SystemClusterCount: Word;                      // number of cluster used for the system, always starts with Cluster 1
       SystemSize        : Integer;                   // size of the system stream  
       NumItems          : Cardinal;                  // number of files stored
       Encoding          : Byte;                      // Encoding for the file and folder names : 1=ANSI String, 2=WideString
       Creation_Date     : Real;                      // when was it made
       Last_Update       : Real;                      // when it was last updated
     end; //RFileResourceHeader
     PFileResourceHeader = ^RFileResourceHeader;

     TmafFileResourceOption = (frAutoLoad);
     TmafFileResourceOptions = Set Of TmafFileResourceOption;

     TmafCustomFileResource = class;

     TmafRFStatistics = class(TPersistent)
     private
       FnUnusedCluster : Integer;
       FnUsedCluster   : Integer;
       FnStoredFiles   : Integer;
       FnDirectories   : Integer;
    function __GetClusterSize: Word;
     public
       FpParent: TmafCustomFileResource;
     published
       property ClusterSize : Word read __GetClusterSize;
       property UnusedCluster : Integer read FnUnusedCluster write FnUnusedCluster;
       property UsedCluster : Integer read FnUsedCluster write FnUsedCluster;
       property StoredFiles : Integer read FnStoredFiles write FnStoredFiles;
       property Directories : Integer read FnDirectories write FnDirectories;
     end;

     TmafCustomFileResource = class(TComponent)
     private
       FsFileName : String;
       nFileHandle : File;
       pHeader : PFileResourceHeader;
       FbLoaded : Boolean;
       FFileResourceOptions : TmafFileResourceOptions;
       FEncoding : TEncodingType;
       nMax_uID : Integer;
       nMax_PublicID : Integer;
       FpFileList : TList;
       FpUnusedCluster : TList;
       FbModified : Boolean;
       FpStreamer     : TTemplateStreamer;
       FpSystemStream : TMemoryStream;
       FpStatistics : TmafRFStatistics;
       FOnModified : TNotifyEvent;
       arrCluster : Array Of PClusterDesc;   // direct access to the cluster
       pSystemBlock : PFileDesc;             // the file system, handled like a file
       procedure __SetFileName(const Value: String);
       procedure __SetLoaded(const Value: Boolean);
       function __SetFileAccessMode(AMode: Byte): Boolean;
       function __ReadHeader: Boolean;
       procedure __WriteHeader;
       function __ClusterNeeded(FileSize: Int64): Integer;
       function __GetFolderNodeID(APath: String): Integer;
       function __GetMax_uID: Integer;
       function __GetMax_PublicID: Integer;
       // clear procedures
       procedure __Clear_FileList;
       procedure __Clear_ClusterList;
       procedure __Clear_UnusedClusterList;

       procedure __SetModified(const Value: Boolean);
       function __GetMax_ClusterID: Integer;
       function __GetUnusedCluster(pDesc: PFileDesc): Boolean;
       procedure __AllocateCluster;
       procedure __WriteCluster(pMem: Pointer; nSize: Integer; FilePos: Int64);
       procedure __ReadCluster(pMem: Pointer; nSize: Integer; FilePos: Int64);
       function __GetClusterDesc(nID: Integer): PClusterDesc;
       procedure __FreeSystemClusterSpace(nUsed, nNeeded: Integer);
       procedure __MoveCluster(pSource, pDest: PClusterDesc);
       procedure __FreeCluster(pCluster: PClusterDesc);

       procedure __ReadSystemCluster;

     protected
       procedure __WriteFile(pDesc : PFileDesc);
       procedure __ReadFile(pDesc : PFileDesc);
       procedure __ReadStreamData(Sender: TObject; ID: Integer); virtual;
       procedure __WriteStreamData(Sender: TObject; ID: Integer); virtual;
       procedure __Close_ResourceFile; virtual;
       procedure __Open_ResourceFile; virtual;

       function __GetResource_uID(uID: Integer): PFileDesc;
       procedure __Internal_LoadResource(pDesc: PFileDesc);

       property Max_uID : Integer read __GetMax_uID;
       property Max_PublicID : Integer read __GetMax_PublicID;
       property Max_ClusterID : Integer read __GetMax_ClusterID;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       function CreateResourceFile(FileName: String; ClusterSize: Word = 8192): Boolean;

       function Add(AFileName, APath: String): PFileDesc;                       // adds a file in APath (in root, if APath = '')
       procedure Delete(uID: Integer);

       function LoadResource(uID: Integer): TMemoryStream;
       procedure FreeResource(uID: Integer);

       procedure Save;

       property Modified : Boolean read FbModified write __SetModified;
     published
       property FileName : String read FsFileName write __SetFileName;
       property Connected : Boolean read FbLoaded write __SetLoaded;
       property FileResourceOptions : TmafFileResourceOptions read FFileResourceOptions write FFileResourceOptions;
       property Statistics : TmafRFStatistics read FpStatistics;
       property OnModified : TNotifyEvent read FOnModified write FOnModified;
     end;

     TmafFileResource = class(TmafCustomFileResource);

implementation

uses Dialogs;

const AllowedClusterSize : Array [1..6] Of Word = (1024, 2048, 4096, 8192, 16384, 32768);
      HEADER_RF_STRING   : Array [0..5] Of AnsiChar = ('M','A','F','_','R','F');
      HEADER_ID_RF       = 27766;

function __Create_FileDesc(AFileName: String): PFileDesc;
begin
  New(Result);
  FillChar(Result^, SizeOf(RFileDesc), 0);
  Result^.sName := AFileName;
end;

function __Create_ClusterDesc(nID: Integer; nSize: Word): PClusterDesc;
begin
  New(Result);
  FillChar(Result^, SizeOf(RClusterDesc), 0);
  Result^.nID := nID;
  Result^.nStart := SizeOf(RFileResourceHeader) + (nID * nSize);
  Result^.File_uID := -1;
end;

{ TmafRFStatistics }

function TmafRFStatistics.__GetClusterSize: Word;
begin
  Result := 0;
  If Assigned(FpParent) Then
    If Assigned(FpParent.pHeader) Then
      Result := FpParent.pHeader^.ClusterSize;
end;

{ TmafCustomFileResource }

constructor TmafCustomFileResource.Create(AOwner: TComponent);
begin
  inherited;
  pHeader := nil;
  nMax_uID := 0;
  nMax_PublicID := 0;
  FpFileList := TList.Create;
  FpUnusedCluster := TList.Create;
  FpStreamer := TTemplateStreamer.Create;
  // attribute 1 : Cluster
  // attribute 2 : files
  // attribute 3 : folders
  FpStreamer.Attributes := 2;
  FpStreamer.StreamVersion := 1;
  FpStreamer.OnStreamReadAttribute := __ReadStreamData;
  FpStreamer.OnStreamWriteAttribute := __WriteStreamData;
  FpStatistics := TmafRFStatistics.Create;
  FpStatistics.FpParent := Self;
end;

destructor TmafCustomFileResource.Destroy;
begin
  If FbLoaded Then
    __Close_ResourceFile;
  FpStatistics.Free;
  FpStreamer.Free;
  FpUnusedCluster.Free;
  FpFileList.Free;
  inherited;
end;

function TmafCustomFileResource.CreateResourceFile(FileName: String; ClusterSize: Word = 8192): Boolean;
var i, j : Integer;
begin
  Result := True;
  If FbLoaded Then
    __Close_ResourceFile;

  New(pHeader);
  FillChar(pHeader^, SizeOf(RFileResourceHeader), 0);
  pHeader^.HeaderID := HEADER_ID_RF;
  CopyMemory(@pHeader^.HeaderString[0], @HEADER_RF_STRING[0], 6);
  AssignFile(nFileHandle, FileName);
  __SetFileAccessMode(fmOpenReadWrite);
  Rewrite(nFileHandle, 1);
  j := ClusterSize;
  For i := 1 To 6 Do
    If ClusterSize >= AllowedClusterSize[i] Then
      j := AllowedClusterSize[i];
  pHeader^.ClusterSize := j;
  pHeader^.Creation_Date := Now;
  pHeader^.Last_Update := Now;
  pHeader^.SystemClusterCount := 1;
  {$IFDEF UniCode}
    pHeader^.Encoding := 2;  // Unicode
  {$ELSE}
    pHeader^.Encoding := 1;  // ANSI
  {$ENDIF}
  __WriteHeader;
  __AllocateCluster; // allocate an empty cluster
  pSystemBlock := __Create_FileDesc('');
  pSystemBlock^.Flags := [ffSystem, ffUnsaved];
  SetLength(pSystemBlock^.pUsedCluster, 1);
  pSystemBlock^.nClusterUsed := 1;
  pSystemBlock^.pUsedCluster[0] := arrCluster[0];
  arrCluster[0]^.Flag := cfSystem;
  FpUnusedCluster.Delete(0);      // remove the previously unused cluster from the list as it is used now 
  FbModified := True;
  Save;
end;

function TmafCustomFileResource.__SetFileAccessMode(AMode: Byte): Boolean;
var OldMode : Byte;
begin
  Result := False;
  If FbLoaded Then
    CloseFile(nFileHandle);  // we close the file to switch the mode
  OldMode := FileMode;
  FileMode := AMode;
  If FbLoaded Then begin     // we open the file ONLY when if was open
    {$I-}
    Reset(nFileHandle, 1);
    {$I+}
    If IOResult <> 0 Then begin
      FileMode := OldMode;
      Reset(nFileHandle, 1);
    end else
      Result := True;
  end else
    Result := True;
end;

procedure TmafCustomFileResource.__SetFileName(const Value: String);
begin
  If ((FsFileName = Value) And (FbLoaded)) Then
    Exit;

  If csLoading in ComponentState Then begin
    FsFileName := Value;
    Exit;
  end;

  FsFileName := Value;
  If frAutoLoad in FFileResourceOptions Then
    __Open_ResourceFile;
end;

procedure TmafCustomFileResource.__SetLoaded(const Value: Boolean);
begin
  FbLoaded := Value;
end;

procedure TmafCustomFileResource.__SetModified(const Value: Boolean);
begin
  FbModified := Value;
  If Assigned(FOnModified) Then
    FOnModified(Self);
end;

procedure TmafCustomFileResource.__ReadSystemCluster;
var i, nCount, nRead : Integer;
    pMem : Pointer;
begin
  // setting up the stream
  pSystemBlock^.aStream := FpStreamer.Stream;
  pSystemBlock^.aStream.Size := pHeader^.SystemSize;
  pSystemBlock^.aStream.Position := 0;

  GetMem(pMem, pHeader^.ClusterSize);
  FillChar(pMem^, pHeader^.ClusterSize, 0);
  nCount := pHeader^.SystemSize;
  For i := 1 To pHeader^.SystemClusterCount Do begin
    If nCount >= pHeader^.ClusterSize Then
      nRead := pHeader^.ClusterSize
    Else
      nRead := nCount;

    __ReadCluster(pMem, nRead, pSystemBlock^.pUsedCluster[i-1]^.nStart);
    pSystemBlock^.aStream.Write(pMem^, nRead);
  end;
  FreeMem(pMem, pHeader^.ClusterSize);
  pSystemBlock^.aStream.Position := 0;
end;

procedure TmafCustomFileResource.__Open_ResourceFile;
var i : Integer;
begin
  If FbLoaded Then
    __Close_ResourceFile;

  FbModified := False;
  If Not FileExists(FsFileName) Then begin
    MessageDlg('Error: File "' + FsFileName + '" not exists!', mtError, [mbOK], 0);
    Exit;
  end;

  AssignFile(nFileHandle, FsFileName);
  __SetFileAccessMode(fmOpenRead); // we open only for reading atm
  {$I-}
    Reset(nFileHandle, 1);
  {$I+}
  If IOResult <> 0 Then
    Raise EComponentError.Create('Cannot open resource file, error code : ' + IntToStr(IOResult));

  New(pHeader);
  If __ReadHeader Then begin
    // allocating memory for the cluster
    SetLength(arrCluster, pHeader^.ClusterCount);
    For i := 0 To pHeader^.ClusterCount - 1 Do
      arrCluster[i] := __Create_ClusterDesc(i, pHeader^.ClusterSize);

    pSystemBlock := __Create_FileDesc('');
    SetLength(pSystemBlock^.pUsedCluster, pHeader^.SystemClusterCount);
    For i := 0 To pHeader^.SystemClusterCount - 1 Do
      pSystemBlock^.pUsedCluster[i] := arrCluster[i];
    pSystemBlock^.nSize := pHeader^.SystemSize;
    pSystemBlock^.nClusterUsed := pHeader^.SystemClusterCount;
    pSystemBlock^.Flags := [ffSystem];

    __ReadSystemCluster;
    FpStreamer.ReadStream;
  end else
    CloseFile(nFileHandle);
end;

procedure TmafCustomFileResource.__Close_ResourceFile;
begin
  If FbModified Then
    Save;
  __Clear_FileList;
  __Clear_ClusterList;
  __Clear_UnusedClusterList;
  CloseFile(nFileHandle);
  SetLength(arrCluster, 0);
  SetLength(pSystemBlock^.pUsedCluster, 0);
  Dispose(pSystemBlock);
  Dispose(pHeader);
  pSystemBlock := nil;
  pHeader := nil;
  FbLoaded := False;
end;

procedure TmafCustomFileResource.__Clear_ClusterList;
var i : Integer;
begin
  For i := 0 to pHeader^.ClusterCount - 1 Do
    Dispose(arrCluster[i]);
  SetLength(arrCluster, 0);
end;

procedure TmafCustomFileResource.__Clear_FileList;
var i : Integer;
    pDesc : PFileDesc;
begin
  For i := 0 To FpFileList.Count - 1 Do begin
    pDesc := PFileDesc(FpFileList.Items[i]);
    If pDesc^.aStream <> nil Then begin
      pDesc^.aStream.Size := 0;
      pDesc^.aStream.Free;
      pDesc^.aStream := nil;
    end;
    SetLength(pDesc^.pUsedCluster, 0); // set the dynamic array to 0 again
    Dispose(pDesc);
  end;
  FpFileList.Count := 0;
end;

procedure TmafCustomFileResource.__Clear_UnusedClusterList;
var i : Integer;
begin
  For i := 0 to FpUnusedCluster.Count - 1 Do
    Dispose(PClusterDesc(FpUnusedCluster.Items[i]));
  FpUnusedCluster.Count := 0;
end;

function TmafCustomFileResource.__ClusterNeeded(FileSize: Int64): Integer;
var aMod: Integer;
begin
  Result := FileSize div FpStatistics.ClusterSize;
  aMod := FileSize mod FpStatistics.ClusterSize;
  If aMod > 0 Then
    Inc(Result);
end;

procedure TmafCustomFileResource.__FreeCluster(pCluster: PClusterDesc);
begin
  pCluster^.nUsed := 0;
  pCluster^.Flag := cfEmpty;
  pCluster^.File_uID := -1;
  FpUnusedCluster.Add(pCluster);
end;

// If nUsed = nNeeded, __FreeSystemClusterSpace shouldn't be called
procedure TmafCustomFileResource.__FreeSystemClusterSpace(nUsed, nNeeded: Integer);
var i : Integer;
begin
  If nNeeded > nUsed Then begin
    SetLength(pSystemBlock^.pUsedCluster, nNeeded); // allocate space for the new cluster
    For i := nUsed To nNeeded - 1 Do begin
      If FpUnusedCluster.Count = 0 Then  // if we don't have a free cluster
        __AllocateCluster;               // we allocate one
      __MoveCluster(arrCluster[i], PClusterDesc(FpUnusedCluster.Items[0])); // and move the blocking cluster the first free cluster
      FpUnusedCluster.Delete(0);
      arrCluster[i]^.Flag := cfSystem;
      arrCluster[i]^.File_uID := -1;
      pSystemBlock^.pUsedCluster[i] := arrCluster[i];
    end;  //  --  For i := nUsed To nNeeded - 1 Do
  end else
    If nNeeded < nUsed Then begin
      For i := nNeeded - 1 DownTo nUsed Do begin
        pSystemBlock^.pUsedCluster[i]^.Flag := cfEmpty;      // we flag as empty
        pSystemBlock^.pUsedCluster[i]^.nUsed := 0;           // set the used member to 0
        FpUnusedCluster.Add(pSystemBlock^.pUsedCluster[i]);  // and move it to the empty list
      end;  //  --  For i := nNeeded - 1 DownTo nUsed Do
      SetLength(pSystemBlock^.pUsedCluster, nNeeded);        // and set our array to the new (shorter) length
    end;
end;

function TmafCustomFileResource.__ReadHeader: Boolean;
var nRead : Integer;
begin
  Seek(nFileHandle, 0);
  Blockread(nFileHandle, pHeader^, SizeOf(RFileResourceHeader), nRead);
  If nRead <> SizeOf(RFileResourceHeader) Then
    Raise EComponentError.Create('Could not read header from file resource');
  Case pHeader^.Encoding Of
    1 : FEncoding := etANSI;
    2 : FEncoding := etWideString;
  end;  //  --  Case pHeader^.Encoding Of
  Result := (pHeader^.HeaderID = HEADER_ID_RF); 
end;

procedure TmafCustomFileResource.__WriteHeader;
var nWritten : Integer;
begin
  Seek(nFileHandle, 0);
  pHeader^.Last_Update := Now;
  pHeader^.NumItems := FpFileList.Count;
  BlockWrite(nFileHandle, pHeader^, SizeOf(RFileResourceHeader), nWritten);
  If nWritten <> SizeOf(RFileResourceHeader) Then
    Raise EComponentError.Create('Could not write header to file resource');
end;

procedure TmafCustomFileResource.__ReadStreamData(Sender: TObject; ID: Integer);
var nCountFiles, nCountCluster, i, j, k : Integer;
    nVer : Word;
    pDesc : PFileDesc;
    pCluster : PClusterDesc;
begin
  Case ID Of
    1 : begin
          FpStreamer.ReadWord(nVer);
          FpStreamer.ReadInteger(nCountCluster);
          For i := 0 to nCountCluster - 1 Do begin
            pCluster := arrCluster[i];
            FpStreamer.ReadInteger(pCluster^.nID);
            FpStreamer.ReadInt64(pCluster^.nStart);
            FpStreamer.ReadWord(pCluster^.nUsed);
            FpStreamer.ReadInteger(k); // the flag
            case k of
              1: pCluster^.Flag := cfUsed;
              2: pCluster^.Flag := cfSystem;
              3: begin
                   pCluster^.Flag := cfEmpty;
                   FpUnusedCluster.Add(pCluster);
                 end;
            end;
            FpStreamer.ReadInteger(pCluster^.File_uID);
          end;
        end;
    2 : begin  // reading file infos
          nMax_uID := 0;
          FpStreamer.ReadWord(nVer);
          FpStreamer.ReadInteger(nCountFiles);
          For i := 1 To nCountFiles Do begin
            pDesc := __Create_FileDesc('');

            // we try to get the highest used uID and start counting from there with new files
            FpStreamer.ReadInteger(pDesc^.uID);
            If pDesc^.uID > nMax_uID Then
              nMax_uID := pDesc^.uID;

            // we try to get the highest PublicID and start counting from there with new files
            FpStreamer.ReadInteger(pDesc^.nPublicID);
            If pDesc^.nPublicID > nMax_PublicID Then
              nMax_PublicID := pDesc^.nPublicID;

            FpStreamer.ReadInteger(pDesc^.nFolderID);
            FpStreamer.ReadString(pDesc^.sName);
            FpStreamer.ReadInteger(pDesc^.nSize);
            FpStreamer.ReadInteger(pDesc^.nClusterUsed);
            SetLength(pDesc^.pUsedCluster, pDesc^.nClusterUsed);
            For j := 0 To pDesc^.nClusterUsed - 1 Do begin
              FpStreamer.ReadInteger(k);
              pDesc^.pUsedCluster[j] := __GetClusterDesc(k);
            end;
            FpStreamer.ReadInteger(j);  // flags
            pDesc^.Flags := [];
            If (j And 1) = 1 Then
              Include(pDesc^.Flags, ffEncrypted);
            If (j And 2) = 2 Then
              Include(pDesc^.Flags, ffCompressed);
            If (j And 4) = 4 Then
              Include(pDesc^.Flags, ffSystem);
            If (j And 8) = 8 Then
              Include(pDesc^.Flags, ffCache);
            FpStreamer.ReadCardinal(pDesc^.CRC32);
            FpFileList.Add(pDesc);
          end;
        end;
    3 : begin  // reading folder infos
        end;
  end;
end;

const VER_FileStream : Word = 1;
      VER_FolderStream : Word = 1;
      VER_ClusterStream : Word = 1;

procedure TmafCustomFileResource.__WriteStreamData(Sender: TObject; ID: Integer);
var i, j : Integer;
    pDesc : PFileDesc;
    pCluster : PClusterDesc;
begin
  Case ID Of
    1 : begin  // writing cluster infos
          // writing used cluster first
          FpStreamer.WriteWord(VER_ClusterStream);
          FpStreamer.WriteInteger(pHeader^.ClusterCount);
          For i := 0 To pHeader^.ClusterCount - 1 Do begin
            pCluster := arrCluster[i];
            FpStreamer.WriteInteger(pCluster^.nID);
            FpStreamer.WriteInt64(pCluster^.nStart);
            FpStreamer.WriteWord(pCluster^.nUsed);
            Case pCluster^.Flag Of
              cfUsed   : j := 1;
              cfSystem : j := 2;
              cfEmpty  : j := 3;
              Else j := 0;
            end;
            FpStreamer.WriteInteger(j);
            // we could restore the FileID later, when we read the files, but
            // this way we could also have a reference back in case something gets
            // fucked up
            FpStreamer.WriteInteger(pCluster^.File_uID);
          end;
        end;
    2 : begin  // writing file infos
          FpStreamer.WriteWord(VER_FileStream);
          FpStreamer.WriteInteger(FpFileList.Count);
          For i := 0 To FpFileList.Count - 1 Do begin
            pDesc := PFileDesc(FpFileList.Items[i]);
            FpStreamer.WriteInteger(pDesc^.uID);
            FpStreamer.WriteInteger(pDesc^.nPublicID);
            FpStreamer.WriteInteger(pDesc^.nFolderID);
            FpStreamer.WriteString(pDesc^.sName);
            FpStreamer.WriteInteger(pDesc^.nSize);
            FpStreamer.WriteInteger(pDesc^.nClusterUsed);
            For j := 0 To pDesc^.nClusterUsed - 1 Do
              FpStreamer.WriteInteger(PClusterDesc(pDesc^.pUsedCluster[j])^.nID);
            j := 0;
            If ffEncrypted in pDesc^.Flags Then
              Inc(j, 1);
            If ffCompressed in pDesc^.Flags Then
              Inc(j, 2);
            If ffSystem in pDesc^.Flags Then
              Inc(j, 4);
            If ffCache in pDesc^.Flags Then
              Inc(j, 8);
            FpStreamer.WriteInteger(j);
            FpStreamer.WriteCardinal(pDesc^.CRC32);
          end;

        end;
    3 : begin  // writing folder infos
        end;
  end;
end;

function TmafCustomFileResource.__GetMax_ClusterID: Integer;
begin
  Result := pHeader^.ClusterCount;
end;

function TmafCustomFileResource.__GetMax_PublicID: Integer;
begin
  Inc(nMax_PublicID);
  Result := nMax_PublicID;
end;

function TmafCustomFileResource.__GetMax_uID: Integer;
begin
  Inc(nMax_uID);
  Result := nMax_uID;
end;



procedure TmafCustomFileResource.__WriteCluster(pMem: Pointer; nSize: Integer; FilePos: Int64);
begin
  Seek(nFileHandle, FilePos);
  BlockWrite(nFileHandle, pMem^, nSize);
end;

procedure TmafCustomFileResource.__ReadCluster(pMem: Pointer; nSize: Integer; FilePos: Int64);
begin
  If FilePos > -1 Then
    Seek(nFileHandle, FilePos);
  BlockRead(nFileHandle, pMem^, nSize);
end;

procedure TmafCustomFileResource.__AllocateCluster;
var aCluster : PClusterDesc;
    pMem : Pointer;
begin
  // when empty, ClusterCount is 0, so we create 1 cluster
  SetLength(arrCluster, pHeader^.ClusterCount + 1);
  // we place it at array[0] with nID 0
  arrCluster[pHeader^.ClusterCount] := __Create_ClusterDesc(pHeader^.ClusterCount, FpStatistics.ClusterSize);
  // we save it in a variable for easier access
  aCluster := arrCluster[pHeader^.ClusterCount];
  // and increase the clustercount to 1
  Inc(pHeader^.ClusterCount);
  aCluster^.nUsed := 0;
  aCluster^.Flag := cfEmpty;
  aCluster^.File_uID := -1;      // unassigned
  GetMem(pMem, FpStatistics.ClusterSize);
  FillChar(pMem^, FpStatistics.ClusterSize, 0);
  __WriteCluster(pMem, FpStatistics.ClusterSize, aCluster^.nStart);   // writes an empty Cluster to reserve space within the file
  FpUnusedCluster.Add(aCluster);
end;

function TmafCustomFileResource.__GetUnusedCluster(pDesc: PFileDesc): Boolean;
var i, ClusterNeeded : Integer;
begin
  Result := False;
  ClusterNeeded := __ClusterNeeded(pDesc^.nSize) - pDesc^.nClusterUsed;  // calculates, how many cluster we will need
  While FpUnusedCluster.Count < ClusterNeeded Do   // allocating the cluster needed
    __AllocateCluster;
  If ffUnsaved in pDesc^.Flags Then begin          // handling a completly new file
    SetLength(pDesc^.pUsedCluster, ClusterNeeded); // allocating the memory to hold the links to cluster
    pDesc^.nClusterUsed := ClusterNeeded;          // saving the amount of cluster used
    For i := 0 to ClusterNeeded - 1 Do begin       // tranferring the cluster from the unused-pool to the used-pool
      pDesc^.pUsedCluster[i] := PClusterDesc(FpUnusedCluster.Items[0]); // saving the cluster in order we read/write
      FpUnusedCluster.Delete(0);                   // delete it from the unused-pool
      pDesc^.pUsedCluster[i]^.Flag := cfUsed;      // flagging the cluster as used
      pDesc^.pUsedCluster[i]^.File_uID := pDesc^.uID; // saving the FileID
    end;
    Result := True;
  end;

end;

procedure TmafCustomFileResource.__MoveCluster(pSource, pDest: PClusterDesc);
var pMem: Pointer;
begin
  pDest^.File_uID := pSource^.File_uID;
  GetMem(pMem, pSource^.nUsed);
  __ReadCluster(pMem, pSource^.nUsed, pSource^.nStart);
  __WriteCluster(pMem, pSource^.nUsed, pDest^.nStart);
  pDest^.nUsed := pSource^.nUsed;
  pDest^.Flag := pSource^.Flag;
end;

// TODO: faster search algo
function TmafCustomFileResource.__GetClusterDesc(nID: Integer): PClusterDesc;
//var i : Integer;
begin
  Result := arrCluster[nID];
{  Result := nil;
  For i := 0 to FpCluster.Count - 1 Do
    if PClusterDesc(FpCluster.Items[i])^.nID = nID then begin
      Result := PClusterDesc(FpCluster.Items[i]);
      Break;
    end; }
end;

function TmafCustomFileResource.__GetFolderNodeID(APath: String): Integer;
begin
  Result := -1;  // TODO
end;


procedure TmafCustomFileResource.__WriteFile(pDesc: PFileDesc);
var i, nCount : Integer;
    PC : ^Char;
    pCluster : PClusterDesc;
begin
  nCount := __ClusterNeeded(pDesc^.nSize);      // that's the cluster amount needed for the file
  If pDesc^.nClusterUsed <> nCount Then begin   // let's see, if we need more or less cluster as we already use
    // we have more cluster as we need
    If pDesc^.nClusterUsed > nCount Then begin
      While pDesc^.nClusterUsed > nCount Do begin
        __FreeCluster(pDesc^.pUsedCluster[pDesc^.nClusterUsed - 1]);  // we free the last cluster
        Dec(pDesc^.nClusterUsed);                                     // and decrement cluster used
      end;
      SetLength(pDesc^.pUsedCluster, nCount);                         // we reduce the array size
    end;
    If pDesc^.nClusterUsed < nCount Then begin
      SetLength(pDesc^.pUsedCluster, nCount);                         // we allocate additional space for the new cluster
      While pDesc^.nClusterUsed < nCount Do begin
        If FpUnusedCluster.Count = 0 Then
          __AllocateCluster;
        pCluster := PClusterDesc(FpUnusedCluster.Items[0]);           // the first free cluster
        pDesc^.pUsedCluster[pDesc^.nClusterUsed] := pCluster;         // we assign it
        FpUnusedCluster.Delete(0);                                    // and remove the unused cluster
        pCluster^.Flag := cfUsed;                                     // we mark it as used
        pCluster^.File_uID := pDesc^.uID;                             // and assign the file ID
        Inc(pDesc^.nClusterUsed);                                     // and increment the used cluster
      end;
    end;
  end;

  PC := pDesc^.aStream.Memory;
  nCount := pDesc^.nSize;
  i := 0;
  While nCount > 0 Do begin
    If nCount >= FpStatistics.ClusterSize Then begin
      __WriteCluster(PC, FpStatistics.ClusterSize, pDesc^.pUsedCluster[i]^.nStart);
      pDesc^.pUsedCluster[i]^.nUsed := FpStatistics.ClusterSize;
    end Else begin
      __WriteCluster(PC, nCount, pDesc^.pUsedCluster[i]^.nStart);
      pDesc^.pUsedCluster[i]^.nUsed := nCount;
    end;
    Dec(nCount, pDesc^.pUsedCluster[i]^.nUsed);
    Inc(PC, pDesc^.pUsedCluster[i]^.nUsed);
    Inc(i);
  end;
end;

procedure TmafCustomFileResource.__ReadFile(pDesc: PFileDesc);
begin
  If Assigned(pDesc) Then begin

  end;  //  --  If Assigned(pDesc) Then
end;

procedure TmafCustomFileResource.Save;
var i, nCount : Integer;
begin
  If FbModified Then begin
    __SetFileAccessMode(fmOpenReadWrite);
    For i := 0 To FpFileList.Count - 1 Do begin
      If ffUnsaved in PFileDesc(FpFileList.Items[i])^.Flags Then begin
        // writing a new file into the resource file
        __WriteFile(PFileDesc(FpFileList.Items[i]));
      end;
      If ffModified in PFileDesc(FpFileList.Items[i])^.Flags Then begin
        // updating an existing file in the resource file by deleting the current and creating a completly new with same ID etc
//        nID := PFileDesc(FpFileList.Items[i])^.uID;
      end;
    end;
    // now saving the file table
    FpStreamer.Stream.Size := 0;
    FpStreamer.WriteStream(5589);
    FpStreamer.Stream.Position := 0;
    nCount := __ClusterNeeded(FpStreamer.Stream.Size);
    pSystemBlock^.nSize := FpStreamer.Stream.Size;
    pHeader^.SystemSize := FpStreamer.Stream.Size;
    If nCount <> pHeader^.SystemClusterCount Then
      __FreeSystemClusterSpace(pHeader^.SystemClusterCount, nCount);
    Include(pSystemBlock^.Flags, ffUnsaved);
    pSystemBlock^.aStream := FpStreamer.Stream;
    __WriteFile(pSystemBlock);
    __WriteHeader;
    __SetFileAccessMode(fmOpenRead);
  end;

  FbModified := False;
end;

// TODO: faster search algo
function TmafCustomFileResource.__GetResource_uID(uID: Integer): PFileDesc;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpFileList.Count - 1 Do
    If PFileDesc(FpFileList.Items[i])^.uID = uID Then begin
      Result := PFileDesc(FpFileList.Items[i]);
      Break;
    end;
end;

// AFileName :
function TmafCustomFileResource.Add(AFileName, APath: String): PFileDesc;
begin
  If Not FileExists(AFileName) Then
    Raise EComponentError.Create('File "' + AFileName + '" does not exist!');

  Result := __Create_FileDesc(ExtractFileName(AFileName)); // we save only the file name into the file descriptor
  Result^.uID := Max_uID;
  Result^.nPublicID := Max_PublicID;
  Result^.nFolderID := __GetFolderNodeID(APath);
  Result^.aStream := TMemoryStream.Create;
  Result^.aStream.LoadFromFile(AFileName);
  Result^.aStream.Position := 0;
  Result^.CRC32 := ComputeCRC32(Result^.aStream);
  Result^.nSize := Result^.aStream.Size;
  Result^.nClusterUsed := 0;
  Result^.Flags := [ffUnsaved];

  FpFileList.Add(Result);
  Modified := True;
end;

procedure TmafCustomFileResource.Delete(uID: Integer);
var pDesc: PFileDesc;
    i : Integer;
begin
  pDesc := __GetResource_uID(uID);
  For i := 0 To pDesc^.nClusterUsed - 1 Do
    __FreeCluster(pDesc^.pUsedCluster[i]);
  SetLength(pDesc^.pUsedCluster, 0);
  FpFileList.Delete(FpFileList.IndexOf(pDesc));
  Dispose(pDesc);
end;

procedure TmafCustomFileResource.__Internal_LoadResource(pDesc: PFileDesc);
var pMem : Pointer;
    i : Integer;
begin
  If ((ffCache in pDesc^.Flags) And (pDesc^.aStream <> nil)) Then begin
    pDesc^.aStream.Position := 0;
    Inc(pDesc^.RefCount);
    Exit;
  end;
  pDesc^.aStream := TMemoryStream.Create;
  GetMem(pMem, pHeader^.ClusterSize);
  For i := 0 To pDesc^.nClusterUsed - 1 Do begin
    __ReadCluster(pMem, pDesc^.pUsedCluster[i]^.nUsed, pDesc^.pUsedCluster[i]^.nStart);
    pDesc^.aStream.Write(pMem^, pDesc^.pUsedCluster[i]^.nUsed);
  end;
  FreeMem(pMem, pHeader^.ClusterSize);
  Inc(pDesc^.RefCount);
  pDesc^.aStream.Position := 0;
end;

function TmafCustomFileResource.LoadResource(uID: Integer): TMemoryStream;
var pDesc: PFileDesc;
begin
  Result := nil;
  pDesc := __GetResource_uID(uID);
  If Assigned(pDesc) Then begin
    __Internal_LoadResource(pDesc);
    Result := pDesc^.aStream;
  end;
end;

procedure TmafCustomFileResource.FreeResource(uID: Integer);
var pDesc: PFileDesc;
begin
  pDesc := __GetResource_uID(uID);
  If Assigned(pDesc) Then begin
    Dec(pDesc^.RefCount);
    If pDesc^.RefCount < 1 Then begin
      pDesc^.aStream.Size := 0;
      pDesc^.aStream.Free;
      pDesc^.aStream := nil;
      pDesc^.RefCount := 0;
    end;
  end;
end;



end.
