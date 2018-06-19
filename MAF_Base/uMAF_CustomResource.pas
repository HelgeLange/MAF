{*******************************************************************************
Name         : uMAF_CustomResource.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2009-2014 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 15.09.2009
Last Update  : 17.09.2014
Version      : 1.0.002
Purpose      :
Last Changes :

1.0.002 (17.09.2014) -----------------------------------------------------------
- [FIX] an error was fixed that caused an IO Error 103 when using
        TmafResourceManager.FileResource.Save(True)
1.0.001 (01.07.2014) -----------------------------------------------------------
- in TmafBaseResourceFile.WriteData the type of the local var PC was changed from
  ^Char to ^AnsiChar because it caused problems in Delphi 2009+ with larger files
- in TmafBaseResourceFile.ReadData the code was changed from reading in blocks of
  64k to a single blockread of the whole length
1.0.000 (15.09.2009) -----------------------------------------------------------
- initial version with mostly code from uMAF_ResourceManager as I wanted to reuse
  most code for the new TmafFileDB
*******************************************************************************}
unit uMAF_CustomResource;

interface


uses Windows, Classes, SysUtils, 
     // Modular Application Framework Components units
     uMAF_Core, uMAF_ResourceManager_Helper, uMAF_Globals, uMAF_TemplateStreamer;

Type TEncodingType = (etANSI, etWideString);

     TStringReader = procedure(nStart, nLength: Int64; var S : String) Of Object;
     TStringWriter = procedure(nStart : Int64; var nLength: Int64; var S : String) Of Object;

     TmafBaseResourceFile = class(TPersistent)
     private
       FsResourceFile : String;
       FbLoaded       : Boolean;
       FpData         : TList;
       FpCategories   : TStringList;  // category list
       FpDeleted      : TList;        // list of deleted entries
       FnHeaderID     : Integer;
       FnStreamID     : Integer;      // every Child object needs its own StreamID
       FpModules      : TStringList;  // List with all modules
       FpStreamer     : TTemplateStreamer;
       FEncoding      : TEncodingType;
       FOnModified    : TNotifyEvent; // event that occurs, when the resourcefile was modified
       FOnLoaded      : TNotifyEvent; // event that occurs, when a resource file was loaded
       FOnClosed      : TNotifyEvent; // event that occurs, when a resource file was closed
       FResourceType  : TResourceFileType;
       function __GetHighID: Cardinal; virtual;
     protected
       FHandle : File;
       pHeader : PFileResourceHeader;
       FbModified : Boolean;
       function __GetResourceFile: String; virtual;
       procedure __SetModified(const Value: Boolean); virtual;
       procedure __Clear_Data; virtual;
       procedure __SetResourceFile(const Value: String); virtual;
       procedure OpenResource; virtual;
       procedure CloseResource; virtual;
       procedure ReadHeader; virtual;
       procedure WriteHeader; virtual;
       procedure ReadFileMapping; virtual;
       procedure WriteFileMapping; virtual;
       procedure __Internal_CreateResource; virtual;
       procedure WriteData(pData: Pointer; nLength: Int64); virtual;
       procedure ReadData(var pData: Pointer; nLength: Int64); virtual;
       procedure Free_ResourceDescriptor(var pData: Pointer); virtual; 
       procedure __ReadStreamData(Sender: TObject; ID: Integer); virtual;
       procedure __WriteStreamData(Sender: TObject; ID: Integer); virtual;
       function __SeekElement(ID: Cardinal; var Idx: Integer; ADataList : TList = nil): PBaseDescriptor; virtual;
       procedure SortItems; virtual;
       function __GetCategoryID(Name: String) : Integer;
       procedure __PrepareData; virtual;
       function __SetFileAccessMode(AMode: Byte): Boolean;
       function __CreateDescriptor: PBaseDescriptor; virtual; abstract;
       property HeaderID : Integer read FnHeaderID write FnHeaderID;  // not for public use!
       property StreamID : Integer read FnStreamID write FnStreamID;  // not for public use!
       property TrashCan : TList read FpDeleted;
     public
       Owner : TComponent;
       constructor Create; virtual;
       destructor Destroy; override;
       procedure CreateResource(FileName: String);
       function RegisterModule(AName: String; nID : Integer): Integer;
       function UnRegisterModule(ID: Integer): Integer;
       procedure Save(bCompleteRewrite: Boolean = False); virtual;
       function Add(pDesc: PBaseDescriptor): PBaseDescriptor; virtual;
       function Delete(nID: Integer): Integer; virtual;
       function GetDescriptor(ID: Cardinal): PBaseDescriptor; virtual;
       function GetDescriptorByPath(APath : String): PBaseDescriptor; virtual;
       function IsResourceType(AFileName: String): Boolean;

       property Encoding : TEncodingType read FEncoding write FEncoding;
       property ResourceType : TResourceFileType read FResourceType write FResourceType;
       property Data : TList read FpData;
       property Streamer : TTemplateStreamer read FpStreamer;
       property Modified : Boolean read FbModified write __SetModified;
       property Categories : TStringList read FpCategories;
       property Modules : TStringList read FpModules write FpModules;
       property HighID : Cardinal read __GetHighID;
       property OnModified : TNotifyEvent read FOnModified write FOnModified;
       property OnLoaded : TNotifyEvent read FOnLoaded write FOnLoaded;
       property OnClosed : TNotifyEvent read FOnClosed write FOnClosed;
     published
       property ResourceFile : String read __GetResourceFile write __SetResourceFile;
       property Loaded : Boolean read FbLoaded;
     end;

     TmafCustomResourceFile = class(TmafBaseResourceFile)
     private
       FpRoot : TList;
       FpFolderNodes : TList;
       FnFolder_HighID : Integer;
     protected
       procedure __Clear_Data; override;
       procedure __ReadStreamData(Sender: TObject; ID: Integer); override;
       procedure __WriteStreamData(Sender: TObject; ID: Integer); override;
       procedure __ReadSubFolder(pNode: PFolderNode);
       procedure __WriteSubFolder(aNode: PFolderNode);
       function __InternalLoadFileResource(pDesc: PFileDescriptor): Boolean; virtual;
       function __GetFolder_HighID: Integer;
       function __CreateDescriptor: PBaseDescriptor; override;
       function __FindFreeSpace(nLength: Int64): PDeletedData;
       procedure Free_ResourceDescriptor(var pData: Pointer); override;
     public
       constructor Create; override;
       destructor Destroy; override;

       procedure Save(bCompleteRewrite: Boolean = False); override;
       function Add(pDesc: PBaseDescriptor): PBaseDescriptor; override;
       function Update(pDesc: PFileDescriptor; var NewData): PFileDescriptor; virtual;
       function LoadResource(pDesc: PFileDescriptor): Integer;
       function Release(ID: Cardinal): Integer;
       function CreateFolder(AParentID : Integer): PFolderNode;
       procedure DeleteFolder(nID: Integer);
       procedure MoveToFolder(pDesc: PFileResourceDescriptor; NewFolderNode: PFolderNode);
       function __GetFolder(nID: Integer; ANode: PFolderNode): PFolderNode; overload;
       function __GetFolder(APath: String; ANode: PFolderNode; bCreate: Boolean = False): PFolderNode; overload;
       function __CreatePath(aNode: PFolderNode): String;
       function GetFileStream(nID: Cardinal): TMemoryStream; virtual;
       property Root : TList read FpRoot;
       property Folders : TList read FpFolderNodes;
       property TrashCan;
     published
       property Encoding;
     end;

const sDefault_Resourcefile_Extension : String = 'maf';

implementation

uses Dialogs;

{ TmafCustomResource }

constructor TmafBaseResourceFile.Create;
begin
  pHeader := __Create_ResourceHeader(FResourceType);
  FbModified := False;
  FbLoaded := False;
  FsResourceFile := '';
  FpData := TList.Create;
  FpDeleted := TList.Create;
  FpCategories := TStringList.Create;
  FpModules := TStringList.Create;
  FpStreamer := TTemplateStreamer.Create;
  FpStreamer.Attributes := 4;
  FpStreamer.StreamVersion := 12;
  FpStreamer.OnStreamReadAttribute := __ReadStreamData;
  FpStreamer.OnStreamWriteAttribute := __WriteStreamData;
end;

destructor TmafBaseResourceFile.Destroy;
begin
  If FbLoaded Then
    CloseResource;  // clears all data also

  FreeAndNil(FpModules);
  FreeAndNil(FpCategories);
  FreeAndNil(FpDeleted);
  FreeAndNil(FpData);
  FreeAndNil(FpStreamer);
  Dispose(pHeader);
  inherited;
end;

procedure TmafBaseResourceFile.Free_ResourceDescriptor(var pData: Pointer);
begin
  // do nothing, just avoiding abstract error
end;

function TmafBaseResourceFile.GetDescriptor(ID: Cardinal): PBaseDescriptor;
var idx: Integer;
begin
  Result := __SeekElement(ID, idx);
end;

function TmafBaseResourceFile.GetDescriptorByPath(APath: String): PBaseDescriptor;
begin
  Result := nil;
end;

function TmafBaseResourceFile.IsResourceType(AFileName: String): Boolean;
var F : File;
    Header : PFileResourceHeader;
begin
  Result := False;
  AssignFile(F, AFileName);
  Reset(F, 1);
  New(Header);
  Seek(F, 0);
  If FileSize(F) > SizeOf(RFileResourceHeader) Then begin
    BlockRead(F, Header^, SizeOf(RFileResourceHeader));
    If Header^.HeaderID = HeaderID Then
      Result := True;
  end;  //  --  If FileSize(F) > SizeOf(RFileResourceHeader) Then
  Dispose(Header); 
  CloseFile(F);
end;

procedure TmafBaseResourceFile.ReadFileMapping;
var p : pointer;
begin
  If pHeader^.FileMapping_Start > 0 Then begin
    Seek(FHandle, pHeader^.FileMapping_Start);
    P := nil;
    ReadData(P, pHeader^.FileMapping_Size);
    Streamer.Stream.Size := 0;
    Streamer.Stream.Write(P^, pHeader^.FileMapping_Size);
    Streamer.Stream.Position := 0;
    Streamer.ReadStream;
    Streamer.Stream.Size := 0;
    FreeMem(P, pHeader^.FileMapping_Size);
    __PrepareData;
  end else
    pHeader^.FileMapping_Start := SizeOf(RFileResourceHeader);
end;

procedure TmafBaseResourceFile.WriteFileMapping;
begin
  SortItems;                                               // we write them sorted
  Streamer.StreamVersion := 12;
  Streamer.Stream.Size := 0;                               // empty the stream, just to be sure...
//  Streamer.Attributes := 5;
  Streamer.WriteStream(FnStreamID);                        // and write the file mapping stream
  Streamer.Stream.Position := 0;                           // set stream to start
  pHeader^.FileMapping_Size := Streamer.Stream.Size;       // update the size for the file mapping in the header
  Seek(FHandle, pHeader^.FileMapping_Start);               // goto new file mapping pos
  WriteData(Streamer.Stream.Memory, Streamer.Stream.Size); // write the new file mapping
  Streamer.Stream.Size := 0;                               // kill the reserved stream memory
  pHeader^.NumItems := FpData.Count;
  WriteHeader;                                             // write the updated header
  Modified := False;
end;

procedure TmafBaseResourceFile.__Internal_CreateResource;
begin
  Dispose(pHeader);
  pHeader := __Create_ResourceHeader(FResourceType);
  pHeader^.HeaderID := FnHeaderID;
  pHeader^.FileMapping_Start := SizeOf(RFileResourceHeader);
  pHeader^.NumItems := 0;
  {$IFDEF Unicode}         // Delphi 2009 +
    FEncoding := etWideString;
  {$ELSE}                 // all others
    FEncoding := etANSI;
  {$ENDIF}
  Case FEncoding Of
    etANSI       : pHeader^.Encoding := Encoding_UTF_8;
    etWideString : pHeader^.Encoding := Encoding_UTF_16;
  end;

  FpCategories.Clear;
  FpModules.Clear;
  FpModules.Add('Shared Item');
  WriteHeader;      // to make sure, we have a file with the header to write the file mapping after it
  WriteFileMapping; // write an empty file mapping and write the header again to update it
end;

procedure TmafBaseResourceFile.__PrepareData;
begin
  // do nothing
end;

procedure TmafBaseResourceFile.CreateResource(FileName: String);
begin
  If FbLoaded Then
    CloseResource;
  try
    AssignFile(FHandle, FileName);
    FileMode := fmOpenReadWrite; // can't use __SetFileAccessMode here, because it works only with existing files
    {$I-}
    Rewrite(FHandle, 1);
    {$I+}
    __Internal_CreateResource;       // call the virtual function
    __SetFileAccessMode(fmOpenRead);
    FsResourceFile := FileName;
    FbLoaded := True;
    If Assigned(FOnLoaded) Then
      FOnLoaded(Self);
  except
    Raise EComponentError.Create('Could not create resource file "'+FileName+'" !');
  end;
end;

procedure TmafBaseResourceFile.ReadHeader;
var nRead : Integer;
begin
  Seek(FHandle, 0);
  Blockread(FHandle, pHeader^, SizeOf(RFileResourceHeader), nRead);
  If nRead <> SizeOf(RFileResourceHeader) Then
    Raise EComponentError.Create('Could not read header from file resource');
  Case pHeader^.Encoding Of
    1 : FEncoding := etANSI;
    2 : FEncoding := etWideString;
  end;  //  --  Case pHeader^.Encoding Of
end;

function TmafBaseResourceFile.RegisterModule(AName: String; nID: Integer): Integer;
begin
  Result := ERR_NOTHING_TO_DO;
  If FpModules.IndexOf(AName) = -1 Then begin
    FpModules.AddObject(AName, TObject(nID));
    FbModified := True;
    Result := ERR_NO_ERROR;
  end;  //  --  If FpModules.IndexOf(AName) = -1 Then
end;

function TmafBaseResourceFile.UnRegisterModule(ID: Integer): Integer;
var i : Integer;
begin
  Result := ERR_NOTHING_TO_DO;
  For i := 0 To FpModules.Count - 1 Do
    If Integer(FpModules.Objects[i]) = ID Then begin
      // TODO : Delete all strings used by that module
      FpModules.Delete(i);
      Result := ERR_NO_ERROR;
      Break;
    end;
end;

procedure TmafBaseResourceFile.Save(bCompleteRewrite: Boolean = False);
begin
  WriteFileMapping;
end;

function TmafBaseResourceFile.Delete(nID: Integer): Integer;
var pDesc : PBaseDescriptor;
    pData : PDeletedData;
    idx : Integer;
begin
  Result := ERR_RESOURCE_NOT_FOUND;
  pDesc := __SeekElement(nID, idx);
  If ((idx > -1) And (pDesc <> nil)) Then begin
    New(pData);
    pData^.nStart := pDesc^.nStart;
    pData^.nLength := pDesc^.nLength;

    Free_ResourceDescriptor(Pointer(pDesc));
    If pData^.nLength > 0 Then
      FpDeleted.Add(pData)
    Else
      Dispose(pData);
    FpData.Delete(idx);
    Modified := True;
    Result := ERR_NO_ERROR;
  end;  //  --  If ((idx > -1) And (pDesc <> nil)) Then
end;

function __DoSort_Items(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  // return -1 if Item1 < Item2
  //         0 if equal (shouldn't happen anyway)
  //         1 if Item1 > Item2
  If PBaseDescriptor(Item1)^.ID > PBaseDescriptor(Item2)^.ID Then begin
    Result := 1;
    Exit;
  end;

  If PBaseDescriptor(Item1)^.ID < PBaseDescriptor(Item2)^.ID Then begin
    Result := -1;
    Exit;
  end;
end;

procedure TmafBaseResourceFile.SortItems;
begin
  Data.Sort(__DoSort_Items);
end;

procedure TmafBaseResourceFile.WriteHeader;
var nWritten : Integer;
begin
  Seek(FHandle, 0);
  pHeader^.Date := Now;
  BlockWrite(FHandle, pHeader^, SizeOf(RFileResourceHeader), nWritten);
  If nWritten <> SizeOf(RFileResourceHeader) Then
    Raise EComponentError.Create('Could not write header to file resource');
end;

procedure TmafBaseResourceFile.WriteData(pData: Pointer; nLength: Int64);
var PC : ^AnsiChar;
    numWritten, nCount : Integer;
begin
  PC := pData;
  nCount := nLength;
  While nCount > 0 Do begin
    If nCount > MAX_BYTES Then
      BlockWrite(FHandle, PC^, MAX_BYTES, numWritten)
    Else
      BlockWrite(FHandle, PC^, nCount, numWritten);
    Dec(nCount, numWritten);
    Inc(PC, numWritten);
  end;
end;

procedure TmafBaseResourceFile.ReadData(var pData: Pointer; nLength: Int64);
var numRead : Integer;
begin
  GetMem(pData, nLength);
  BlockRead(FHandle, pData^, nLength, numRead);
  If numRead < nLength Then
    Exception.Create('TmafBaseResourceFile.ReadData was ordered to read over the file size');
end;

procedure TmafBaseResourceFile.OpenResource;
var FullFileName : String;
begin
  If FbLoaded Then
    CloseResource;
  If ExtractFileName(ResourceFile) = '' Then begin
    FsResourceFile := '';
    Exit;
  end;

  If Not FileExists(FsResourceFile) Then begin
    MessageDlg('The file "'+FsResourceFile+'" does not exist', mtInformation, [mbOk], 0);
    FsResourceFile := '';
    Exit;
  end;

  AssignFile(FHandle, ResourceFile);
  __SetFileAccessMode(fmOpenRead); // we open only for reading atm
  {$I-}
  Reset(FHandle, 1);
  {$I+}
  If IOResult <> 0 Then
    Raise EComponentError.Create('Could not open file resource '+ FullFileName + ' !');

  ReadHeader;
  If FnHeaderID <> pHeader^.HeaderID Then begin
    CloseResource;
    Raise EComponentError.Create('Found : ' + __GetResourceIDString(pHeader^.HeaderID) + #13#10 + 'Expected : ' + __GetResourceIDString(FnHeaderID));
  end;
  FbLoaded := True;
  FbModified := False;
  ReadFileMapping;
  If Assigned(FOnLoaded) Then
    FOnLoaded(Self);
end;

function TmafBaseResourceFile.Add(pDesc: PBaseDescriptor): PBaseDescriptor;
begin
  Result := nil;
  If Not Loaded Then
    Exit;

  If pDesc = nil Then   // no item ?
    Exit;

  Result := __CreateDescriptor; // creates the right Descriptor for us
  If pDesc^.ID > 0 Then Result^.ID := pDesc^.ID
                   Else Result^.ID := HighID + 1;

  Result^.CategoryID := pDesc^.CategoryID;
  Result^.ModuleID := pDesc^.ModuleID;
  Result^.nStart := 0; // indicates, that it wasn't saved yet
  Data.Add(Result);
  Inc(pHeader^.NumItems);
  Modified := True;
end;

procedure TmafBaseResourceFile.CloseResource;
begin
  If FbLoaded Then begin
    If Assigned(FOnClosed) Then
      FOnClosed(Self);
    If FbModified Then begin
      __SetFileAccessMode(fmOpenReadWrite);
      WriteFileMapping;
      __SetFileAccessMode(fmOpenRead);
    end;
    __Clear_Data;
    CloseFile(FHandle);
    FillChar(pHeader^, SizeOf(RFileResourceHeader), 0);
    FbLoaded := False;
  end;
end;

procedure TmafBaseResourceFile.__Clear_Data;
var p : Pointer;
    i : Integer;
begin
  For i := FpData.Count - 1 DownTo 0 Do begin
    p := FpData.Items[i];
    Free_ResourceDescriptor(p);
  end;  //  --  While FpData.Count > Do
  FpData.Clear;
  For i := FpDeleted.Count - 1 DownTo 0 Do
    Dispose(PDeletedData(FpDeleted.Items[i]));
  FpDeleted.Clear;
  FpModules.Clear;
  FpCategories.Clear;
end;

function TmafBaseResourceFile.__SeekElement(ID: Cardinal; var Idx: Integer; ADataList : TList = nil): PBaseDescriptor;
var nLower, nUpper, nMid : Integer;
    DataList : TList;
begin
  If ADataList <> nil Then DataList := ADataList
                      Else DataList := FpData;

  Result := nil;
  idx := -1;
  If DataList.Count < 2 Then begin
    If DataList.Count = 1 Then begin
      If PBaseDescriptor(DataList.Items[0])^.ID = ID Then begin
        Result := PBaseDescriptor(DataList.Items[0]);
        idx := 0;
      end;
    end;  //  --  If Data.Count = 1 Then
    Exit;
  end;  //  --  If Data.Count < 2 Then 

  nLower := 0;
  nUpper := DataList.Count - 1;
  While True Do begin
    nMid := (nLower + nUpper) div 2;
    If ID < PBaseDescriptor(DataList.Items[nMid])^.ID Then
      nUpper := nMid - 1
    Else
      If ID > PBaseDescriptor(DataList.Items[nMid])^.ID Then
        nLower := nMid + 1
      Else begin
        Result := PBaseDescriptor(DataList.Items[nMid]);
        idx := nMid;
        Exit;
      end;
    If (nLower > nUpper) Then
      Exit;
  end;  //  --  While True Do
end;

function TmafBaseResourceFile.__SetFileAccessMode(AMode: Byte): Boolean;
var OldMode : Byte;
begin
  Result := False;
  If FbLoaded Then
    CloseFile(FHandle);  // we close the file to switch the mode
  OldMode := FileMode;
  FileMode := AMode;
  {$I-}
  Reset(FHandle, 1);
  {$I+}
  If IOResult <> 0 Then begin
    FileMode := OldMode;
    Reset(FHandle, 1);
  end else
    Result := True;
end;

procedure TmafBaseResourceFile.__SetModified(const Value: Boolean);
begin
  If Value <> FbModified Then begin
    FbModified := Value;
    If Assigned(FOnModified) Then
      FOnModified(Self);
  end;  //  --  If Value <> FbModified Then
end;

procedure TmafBaseResourceFile.__SetResourceFile(const Value: String);
begin
  If ((FsResourceFile = Value) And (FbLoaded)) Then
    Exit;

  If csLoading in Owner.ComponentState Then begin
    FsResourceFile := Value;
    Exit;
  end;
  
  If FbLoaded Then
    CloseResource;
  FsResourceFile := Value;
  If FsResourceFile <> '' Then
    OpenResource;
end;

procedure TmafBaseResourceFile.__ReadStreamData(Sender: TObject; ID: Integer);
var i, j, nCount: Integer;
    S : String;
    pData : PDeletedData;
//    W : Word;
//    pTempList : TStringList;
begin
  Case ID Of
    2 : begin
          Categories.Clear;
//          pTempList := TStringList.Create;
          Streamer.ReadInteger(nCount);
          Categories.Capacity := nCount;
          For i := 1 To nCount Do begin
            Streamer.ReadInteger(j);
            Streamer.ReadString(S);
//            pTempList.AddObject(S, TObject(j));
            Categories.AddObject(S, TObject(j));
          end;  //  --  For i := 1 To nCount Do
        end;
    3 : begin
          Modules.Clear;
//          Modules.AddObject('Shared Item', nil);
          Streamer.ReadInteger(nCount);
          Modules.Capacity := nCount;
          For i := 1 To nCount Do begin
            Streamer.ReadInteger(j);
            Streamer.ReadString(S);
//            pTempList.AddObject(S, TObject(j));
            Modules.AddObject(S, TObject(j));
          end;  //  --  For i := 1 To nCount Do
//          Modules.Clear;
//          Modules.AddObject('Shared Item', nil);
        end;
    4 : begin
          Streamer.ReadInteger(nCount);
          FpDeleted.Capacity := nCount;
          For i := 1 To nCount Do begin
            New(pData);
            Streamer.ReadInt64(pData^.nStart);
            Streamer.ReadInt64(pData^.nLength);
            FpDeleted.Add(pData);
          end;
        end;
  end;
end;

procedure TmafBaseResourceFile.__WriteStreamData(Sender: TObject; ID: Integer);
var i : Integer;
begin
   Case ID Of
    2 : begin
          Streamer.WriteInteger(Categories.Count);
          For i := 0 To Categories.Count - 1 Do begin
            Streamer.WriteInteger(Integer(Categories.Objects[i]));
            Streamer.WriteString(Categories.Strings[i]);
          end;
        end;
    3 : begin
          Streamer.WriteInteger(Modules.Count);
          For i := 0 To Modules.Count - 1 Do begin
            Streamer.WriteInteger(Integer(Modules.Objects[i]));
            Streamer.WriteString(Modules.Strings[i]);
          end;
        end;
    4 : begin
          Streamer.WriteInteger(FpDeleted.Count);
          For i := 0 To FpDeleted.Count - 1 Do begin
            Streamer.WriteInt64(PDeletedData(FpDeleted.Items[i])^.nStart);
            Streamer.Writeint64(PDeletedData(FpDeleted.Items[i])^.nLength);
          end;
        end;
   end;
end;

function TmafBaseResourceFile.__GetCategoryID(Name: String): Integer;
var i : Integer;
begin
  Result := 0;
  For i := 0 To Categories.Count - 1 Do
    If Categories.Strings[i] = Name Then begin
      Result := Integer(Categories.Objects[i]);
      Break;
    end;  //  --  If Categories.Strings[i] = Name Then
end;


function TmafBaseResourceFile.__GetHighID: Cardinal;
begin
  Result := 0;
  If Data.Count > 0 Then
    Result := PBaseDescriptor(Data.Items[Data.Count - 1])^.ID;
end;

function TmafBaseResourceFile.__GetResourceFile: String;
begin
  Result := FsResourceFile;
end;

{ TmafCustomFileResource }

constructor TmafCustomResourceFile.Create;
begin
  inherited;
  FnFolder_HighID := 0;
  Streamer.Attributes := 5;
  FpRoot := TList.Create;
  FpFolderNodes := TList.Create;
end;

destructor TmafCustomResourceFile.Destroy;
begin
  inherited;
  FreeAndNil(FpFolderNodes);
  FreeAndNil(FpRoot);
end;

procedure TmafCustomResourceFile.Free_ResourceDescriptor(var pData: Pointer);
begin
  __Free_FileDescriptor(PFileDescriptor(pData));
end;

function TmafCustomResourceFile.GetFileStream(nID: Cardinal): TMemoryStream;
begin
  Result := nil;
end;

function TmafCustomResourceFile.LoadResource(pDesc: PFileDescriptor): Integer;
begin
  Result := ERR_UNKNOWN_ERROR;
  If __InternalLoadFileResource(pDesc) Then
    Result := ERR_NO_ERROR;
end;

procedure TmafCustomResourceFile.__Clear_Data;
var aNode : PFolderNode;
    i : Integer;
begin
  inherited;
  For i := FpFolderNodes.Count - 1 DownTo 0 Do begin  // all root items
    aNode := PFolderNode(FpFolderNodes.Items[i]);
    __Free_FolderNode(aNode);                         // gives all sub folder automatically free
  end;  //  --  For i := FpFolderNodes.Count - 1 DownTo 0 Do
  FpFolderNodes.Clear;
end;

function TmafCustomResourceFile.__CreateDescriptor: PBaseDescriptor;
begin
  Result := PBaseDescriptor(__Create_FileDescriptor);
end;

function TmafCustomResourceFile.__CreatePath(aNode: PFolderNode): String;
begin
  Result := '';
  While aNode.ParentNode > -1 Do begin
    aNode := __GetFolder(aNode^.ParentNode, nil);
    Result := aNode^.NodeName + '\' + Result;
  end;
end;

function TmafCustomResourceFile.__FindFreeSpace(nLength: Int64): PDeletedData;
var i: Integer;
begin
  Result := nil;
  For i := 0 To FpDeleted.Count - 1 Do
    If PDeletedData(FpDeleted.Items[i])^.nLength >= nLength Then begin
      Result := PDeletedData(FpDeleted.Items[i]);
      Break;
    end;
end;

function TmafCustomResourceFile.__GetFolder(APath: String; ANode: PFolderNode; bCreate: Boolean = False): PFolderNode;
var AList : TList;
    S, S1 : String;
    i, pNodeID : Integer;
    pNode : PFolderNode;
    bModified : Boolean;
begin
//  Result := nil;
  If ANode = nil Then begin
    AList := FpFolderNodes;
    pNodeID := -1;
  end else begin
    AList := ANode^.FpChildren;
    pNodeID := ANode^.nID;
  end;

  bModified := False;
  pNode := nil;
  S := APath;
  S1 := '';
  If Pos('\', APath) > 0 Then begin
    System.Delete(S, Pos('\', S), Length(S) - Pos('\', S) + 1);
    S1 := APath;
    System.Delete(S1, 1, Length(S) + 1);
  end;  //  --  If Pos('\', APath) > 0 Then

  For i := 0 To AList.Count - 1 Do
    If S = PFolderNode(AList.Items[i])^.NodeName Then begin
      pNode := PFolderNode(AList.Items[i]);
      Break;
    end;

  // the current folder we didn't find, if bCreate is true, we create it
  If ((pNode = nil) And (bCreate)) Then begin
    pNode := __Create_FolderNode(pNodeID);
    pNode^.nID := __GetFolder_HighID;
    pNode^.NodeName := S;
    If ANode = nil Then FpFolderNodes.Add(pNode)
                   Else ANode^.FpChildren.Add(pNode);
    bModified := True;
  end;

  If ((S1 <> '') And (pNode <> nil)) Then
    pNode := __GetFolder(S1, pNode, bCreate);

  If ((bModified <> Modified) And (Modified = False)) Then
    Modified := True;
  Result := pNode;
end;

function TmafCustomResourceFile.__GetFolder(nID: Integer; ANode: PFolderNode): PFolderNode;
var i : Integer;
    AList : TList;
begin
  Result := nil;
  If ANode = nil Then AList := FpFolderNodes
                 Else AList := ANode^.FpChildren;
  For i := 0 To AList.Count - 1 Do begin
    If PFolderNode(AList.Items[i])^.nID = nID Then begin
      Result := PFolderNode(AList.Items[i]);
      Break;
    end else
      If PFolderNode(AList.Items[i])^.FpChildren.Count > 0 Then begin
        Result := __GetFolder(nID, PFolderNode(AList.Items[i]));
        If Result <> nil Then
          Break;
      end;
  end;  //  --  For i := 0 To AList.Count - 1 Do
end;

function TmafCustomResourceFile.__GetFolder_HighID: Integer;
begin
  Inc(FnFolder_HighID);
  Result := FnFolder_HighID;
end;

function TmafCustomResourceFile.__InternalLoadFileResource(pDesc: PFileDescriptor): Boolean;
var pData : Pointer;
begin
  Result := False;
  If Assigned(pDesc) Then begin
    If ((pDesc^.RefCount = 0) And (pDesc^.nLength > 0)) Then begin
      pData := nil;
      Seek(FHandle, pDesc^.nStart);
      ReadData(pData, pDesc^.nLength);
      pDesc^.aStream := TMemoryStream.Create;
      pDesc^.aStream.Write(pData^, pDesc^.nLength);
      FreeMem(pData, pDesc^.nLength);
    end;  //  --  If PFileResourceDescriptor(Data.Items[i])^.RefCount = 0 Then
    If Assigned(pDesc^.aStream) Then begin
      pDesc^.aStream.Position := 0;
      Inc(pDesc^.RefCount);
      Result := (pDesc^.aStream.Size > 0);
    end;
  end;
end;

function TmafCustomResourceFile.Release(ID: Cardinal): Integer;
var idx : Integer;
    pDesc : PFileResourceDescriptor;
begin
  Result := ERR_NO_ERROR;
  pDesc := PFileResourceDescriptor(__SeekElement(ID, idx));
  If Assigned(pDesc) Then begin
    If pDesc^.RefCount > 0 Then
      Dec(pDesc^.RefCount);
    If ((pDesc^.RefCount = 0) And (Assigned(pDesc^.aStream))) Then
      FreeAndNil(pDesc^.aStream);
  end;
end;

procedure TmafCustomResourceFile.Save(bCompleteRewrite: Boolean = False);
var i : Integer;
    pDesc : PFileDescriptor;
    pDeleted : PDeletedData;
begin
  // when complete rewrite is requested, we can destroy any data about deleted file space
  // and have to load any file from the current archive and mark it for allocate space again
  If bCompleteRewrite Then begin
    For i := FpDeleted.Count - 1 DownTo 0 Do
      Dispose(PDeletedData(FpDeleted.Items[i]));
    For i := 0 To Data.Count - 1 Do begin
      GetFileStream(PFileDescriptor(Data.Items[i])^.ID);  // causes a load from the file into a MemoryStream to save it again later on
      PFileDescriptor(Data.Items[i])^.nStart := 0;
    end;
    CloseFile(FHandle);
    DeleteFile(FsResourceFile);
    FbLoaded := False;
    CreateResource(FsResourceFile);
  end;

  If FileMode = fmOpenRead Then
    __SetFileAccessMode(fmOpenReadWrite);

  For i := 0 To Data.Count - 1 Do begin
    pDesc := PFileDescriptor(Data.Items[i]);
    If pDesc^.nStart = 0 Then begin                 // we write only the ones that aren't in the file yet
      pDeleted := __FindFreeSpace(pDesc^.nLength);
      If Assigned(pDeleted) Then begin
        // we found some space in the deleted part to write the current file
        pDesc^.nStart := pDeleted^.nStart;
      end;

      pDesc^.nStart := pHeader^.FileMapping_Start;  // we start where now is the file mapping
      Seek(FHandle, pHeader^.FileMapping_Start);
      pDesc^.aStream.Position := 0;
      WriteData(pDesc^.aStream.Memory, pDesc^.nLength);             // write the data at the end, where the filemapping starts
      pHeader^.FileMapping_Start := pDesc^.nStart + pDesc^.nLength; // We increase the current filemapping_start
    end;
  end;
  inherited;
  __SetFileAccessMode(fmOpenRead);
end;

// pDesc is the current Descriptor, Newdata contains a MemoryStream
function TmafCustomResourceFile.Update(pDesc: PFileDescriptor; var NewData): PFileDescriptor;
var pData : PDeletedData;
begin
  Result := nil;
  If pDesc = nil Then
    Exit;

  New(pData);
  pData^.nStart := pDesc^.nStart;
  pData^.nLength := pDesc^.nLength;
  FpDeleted.Add(pData);              // move it to TrashCan

  If pDesc^.aStream <> nil Then
    pDesc^.aStream.Size := 0
  Else
    pDesc^.aStream := TMemoryStream.Create;

  pDesc^.aStream.LoadFromStream(TMemoryStream(NewData));
  pDesc^.nStart := 0; // indicates save
  pDesc^.nLength := pDesc^.aStream.Size;
  pDesc^.aStream.Position := 0;
  Result := pDesc;
  Modified := True;
end;

function TmafCustomResourceFile.Add(pDesc: PBaseDescriptor): PBaseDescriptor;
var aNode : PFolderNode;
begin
  Result := inherited Add(pDesc);
  If Result = nil Then
    Exit;

  If ((PFileDescriptor(PDesc)^.aStream = nil) Or (PFileDescriptor(pDesc)^.nLength = 0)) Then  // no data ?
    Exit;

  PFileDescriptor(pDesc)^.aStream.Position := 0;  
  PFileDescriptor(Result)^.ResName := PFileDescriptor(pDesc)^.ResName;
  PFileDescriptor(Result)^.nLength := PFileDescriptor(pDesc)^.nLength;
  PFileDescriptor(Result)^.aStream := TMemoryStream.Create;
  PFileDescriptor(Result)^.aStream.LoadFromStream(PFileDescriptor(pDesc)^.aStream);
  PFileDescriptor(Result)^.aStream.Position := 0;
  PFileDescriptor(Result)^.RefCount := 1;
  PFileDescriptor(Result)^.FolderNode := PFileDescriptor(pDesc)^.FolderNode;
  aNode := __GetFolder(PFileDescriptor(pDesc)^.FolderNode, nil);
  If Assigned(aNode) Then
    aNode^.FpDataList.Add(Result);
end;

// pNode is the parent for the next nodes
procedure TmafCustomResourceFile.__ReadSubFolder(pNode: PFolderNode);
var i, nCount : Integer;
    aNode : PFolderNode;
begin
  If Not Assigned(pNode) Then
    Exit;

  aNode := __Create_FolderNode(pNode^.nID);
  Streamer.ReadInteger(aNode^.nID);
  Streamer.ReadInteger(nCount);   // we use the parent node ID from the one that reads us
  Streamer.ReadString(aNode^.NodeName);
  Streamer.ReadInteger(nCount);
  If aNode^.ParentNode < 1 Then begin
    aNode^.ParentNode := -1;
    FpFolderNodes.Add(aNode);
  end else
    pNode^.FpChildren.Add(aNode);
  If aNode^.nID > FnFolder_HighID Then
    FnFolder_HighID := aNode^.nID;
  For i := 1 To nCount Do
    __ReadSubFolder(aNode);
end;

procedure TmafCustomResourceFile.__ReadStreamData(Sender: TObject; ID: Integer);
var i, j, nCount, nChildCount: Integer;
    pData : PFileResourceDescriptor;
    aNode : PFolderNode;
begin
  Case ID Of
    5 : begin
          Streamer.ReadInteger(nCount);
          Data.Capacity := nCount;
          For i := 1 To nCount Do begin
            New(pData);
            Streamer.ReadCardinal(pData^.ResID);
            Streamer.ReadInt64(pData^.nStart);
            Streamer.ReadInt64(pData^.nLength);
            Streamer.ReadString(pData^.ResName);
            Streamer.ReadInteger(pData^.CategoryID);
            Streamer.ReadInteger(pData^.ModuleID);
            Streamer.ReadInteger(pData^.FolderNode);
            aNode := __GetFolder(pData^.FolderNode, nil);
            If aNode <> nil Then
              aNode^.FpDataList.Add(pData)
            else
              pData^.FolderNode := -1;
            Streamer.ReadByte(pData^.Flags);
            pData^.RefCount := 0;
            pData^.aStream := nil;
            Data.Add(pData);
          end;  //  --  For i := 1 To nCount Do
        end;
    1 : begin
          Streamer.ReadInteger(nCount);
          For i := 1 To nCount Do begin
            aNode := __Create_FolderNode(-1);
            Streamer.ReadInteger(aNode^.nID);
            Streamer.ReadInteger(aNode^.ParentNode);
            Streamer.ReadString(aNode^.NodeName);
            Streamer.ReadInteger(nChildCount);
            FpFolderNodes.Add(aNode);
            If aNode^.ParentNode < 1 Then
              aNode^.ParentNode := -1;
            If aNode^.nID > FnFolder_HighID Then
              FnFolder_HighID := aNode^.nID;
            For j := 1 To nChildCount Do
              __ReadSubFolder(aNode);
          end;
{            For i := FpFolderNodes.Count - 1 DownTo 0 Do begin  // all root items
    aNode := PFolderNode(FpFolderNodes.Items[i]);
    __Free_FolderNode(aNode);                         // gives all sub folder automatically free
  end;  //  --  For i := FpFolderNodes.Count - 1 DownTo 0 Do
  FpFolderNodes.Clear;
}
        end;
    Else inherited __ReadStreamData(Sender, ID);
  end;
end;

procedure TmafCustomResourceFile.__WriteSubFolder(aNode: PFolderNode);
var i : Integer;
begin
  If Not Assigned(aNode) Then
    Exit;
  Streamer.WriteInteger(aNode^.nID);
  Streamer.WriteInteger(aNode^.ParentNode);
  Streamer.WriteString(aNode^.NodeName);
  Streamer.WriteInteger(aNode^.FpChildren.Count);
  For i := 0 To aNode^.FpChildren.Count - 1 Do
    __WriteSubFolder(PFolderNode(aNode^.FpChildren.Items[i]));
end;

procedure TmafCustomResourceFile.__WriteStreamData(Sender: TObject; ID: Integer);
var i, j : Integer;
    pData : PFileResourceDescriptor;
begin
  Case ID Of
    5 : begin  // file data
          Streamer.WriteInteger(Data.Count);
          For i := 0 To Data.Count - 1 Do begin
            pData := PFileResourceDescriptor(Data.Items[i]);
            Streamer.WriteCardinal(pData^.ResID);
            Streamer.WriteInt64(pData^.nStart);
            Streamer.WriteInt64(pData^.nLength);
            Streamer.WriteString(pData^.ResName);
            Streamer.WriteInteger(pData^.CategoryID);
            Streamer.WriteInteger(pData^.ModuleID);
            If pData^.FolderNode < 1 Then
              pData^.FolderNode := -1;
            Streamer.WriteInteger(pData^.FolderNode);
            Streamer.WriteByte(pData^.Flags);
          end;  //  --  For i := 0 To Data.Count - 1 Do
        end;
    1 : begin  // directory nodes
          Streamer.WriteInteger(FpFolderNodes.Count);
          For i := 0 To FpFolderNodes.Count - 1 Do begin
            Streamer.WriteInteger(PFolderNode(FpFolderNodes.Items[i])^.nID);
            Streamer.WriteInteger(PFolderNode(FpFolderNodes.Items[i])^.ParentNode);
            Streamer.WriteString(PFolderNode(FpFolderNodes.Items[i])^.NodeName);
            Streamer.WriteInteger(PFolderNode(FpFolderNodes.Items[i])^.FpChildren.Count);
            For j := 0 To PFolderNode(FpFolderNodes.Items[i])^.FpChildren.Count - 1 Do
              __WriteSubFolder(PFolderNode(PFolderNode(FpFolderNodes.Items[i])^.FpChildren.Items[j]));
          end;
        end;
    Else inherited __WriteStreamData(Sender, ID);
  end;
end;

function TmafCustomResourceFile.CreateFolder(AParentID: Integer): PFolderNode;
var aNode : PFolderNode;
begin
  Result := __Create_FolderNode(AParentID);
  Result^.NodeName := 'New Folder';
  Result^.nID := __GetFolder_HighID;
  aNode := __GetFolder(AParentID, nil);
  If Assigned(aNode) Then
    aNode^.FpChildren.Add(Result)  // if we got a folder, we add it in its Child list
  Else
    FpFolderNodes.Add(Result);     // if not, we put it into the root
  Modified := True;
end;

procedure TmafCustomResourceFile.DeleteFolder(nID: Integer);
var aNode, pNode : PFolderNode;
begin
  aNode := __GetFolder(nID, nil);
  If aNode <> nil Then begin
    If aNode^.ParentNode > 0 Then begin
      pNode := __GetFolder(aNode^.ParentNode, nil);
      If Assigned(pNode) Then
        pNode^.FpChildren.Delete(pNode^.FpChildren.IndexOf(aNode));
    end else
      FpFolderNodes.Delete(FpFolderNodes.IndexOf(aNode));
    __Free_FolderNode(aNode);
    Modified := True;
  end;
end;

procedure TmafCustomResourceFile.MoveToFolder(pDesc: PFileResourceDescriptor; NewFolderNode: PFolderNode);
var aNode : PFolderNode;
begin
  If ((pDesc = nil) or (NewFolderNode = nil)) Then
    Exit;

  aNode := __GetFolder(pDesc^.FolderNode, nil);
  If aNode <> nil Then
    aNode^.FpDataList.Delete(aNode^.FpDataList.IndexOf(pDesc));
  pDesc^.FolderNode := NewFolderNode^.nID;
  NewFolderNode^.FpDataList.Add(pDesc);
  Modified := True;
end;


end.
