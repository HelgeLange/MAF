{*******************************************************************************
Name         : uMAF_ResourceManager.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2009-2014 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 11.10.2009
Last Update  : 02.07.2014
Version      : 1.0.005
Purpose      :
Last Changes :


1.0.005 (06.03.2010) -----------------------------------------------------------
- [FIX] bug fixed, where the CurrentSkin property in the
        TmafResourceManager.FileResource wasn't properly loaded
1.0.004 (22.11.2009) -----------------------------------------------------------
- [FIX] sending now 2 messages to indicate Before- and AfterLanguageChange
- [CHG] moved all API function into the HM_RESOURCEMANAGER_QUERY in the continuing
        effort to reduce the registered API message in the TmafModuleController
1.0.003 (19.11.2009) -----------------------------------------------------------
- [ADD] TmafResourceManager.GetFileStream to get the file stream from the resource
        file instead of a TGraphic. which allows to get any type of file as
        TMemoryStream
1.0.002 (07.11.2009) -----------------------------------------------------------
- [ADD] added an additional check for file existance in TmafResourceManager.Loaded
        to avoid problems if the file doesn't exist and the resource loader
        tries to open the file, causing problems loading the dfm file
1.0.001 (30.09.2009) -----------------------------------------------------------
- [FIX] fixed a bug, where the loading of a module or dfm with the ResourceManager
        on it crashed when no GlobalVars were assigned.
1.0.000 (15.09.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_ResourceManager;

interface

{$I MAFramework.inc}

uses Windows, Classes, SysUtils, Graphics, jpeg, Messages,
     {$IFDEF PNGimage}
     PNGimage,
     {$ENDIF}
     // Modular Application Framework Components units
     uMAF_Core, uMAF_ResourceManager_Helper, uMAF_Globals,
     uMAF_TemplateStreamer, uMAF_GlobalVars, uMAF_CustomResource;

Type TOnUnknownImageType = procedure(Sender: TObject; Extension: String; aStream: TMemoryStream; var Image: TGraphic) of object;

     TFileResourceOption = (froSkinSupport, froAutoCreateSkinFolder, froCache, froCompleteRewrite);
     TFileResourceOptions = Set Of TFileResourceOption;

     TmafResourceManager = class;

     TFileResource = class(TMAFCustomResourceFile)
     private
       FpResourceManager : TmafResourceManager;
       FsSkinFolderRoot : String;
       FpCurrentSkin : PFolderNode;
       FOptions: TFileResourceOptions;
       FpSkinFolderNode : PFolderNode;
       FsCurrentSkinName : String;
       FsOldSkinName : String;  // used only during SkinChange events
       FbSkinChanging : Boolean;
       function __GetCurrentSkin: String;
       procedure __SetCurrentSkin(const Value: String);
       procedure __SetOptions(const Value: TFileResourceOptions);
       procedure __SetResourceManager(const Value: TmafResourceManager);
     protected
       procedure __ReadStreamData(Sender: TObject; ID: Integer); override;
       procedure __WriteStreamData(Sender: TObject; ID: Integer); override;
       procedure __Internal_CreateResource; override;
       procedure __SetResourceFile(const Value: String); override;
       procedure __Clear_Data; override;
       function __SeekElement(ID: Cardinal; var Idx: Integer; ADataList : TList = nil): PBaseDescriptor; override;
       procedure Free_ResourceDescriptor(var pData: Pointer); override;
       function __CreateGraphicObject(pDesc: PFileResourceDescriptor): TGraphic; virtual;
       function __CreateDescriptor: PBaseDescriptor; override;
     public
       constructor Create; override;
       function Add(pDesc: PBaseDescriptor): PBaseDescriptor; override;
       function Get(ID: Cardinal; ACategory: String; var pDesc: PFileResourceDescriptor): TGraphic; overload;
       function GetFileStream(nID: Cardinal): TMemoryStream; override;
       function Get(pDesc: PFileResourceDescriptor): TGraphic; overload;
       function GetDescriptorByPath(APath : String): PBaseDescriptor; override;
       property ResourceManager : TmafResourceManager read FpResourceManager write __SetResourceManager;
     published
       property CurrentSkin : String read __GetCurrentSkin write __SetCurrentSkin;
       property SkinFolderRoot : String read FsSkinFolderRoot write FsSkinFolderRoot;
       property Options : TFileResourceOptions read FOptions write __SetOptions default [];
     end;

     TStringOption = (soKeepEncoding);
     TStringOptions = Set Of TStringOption;

     TStringResource = class(TmafBaseResourceFile)
     private
       FpResourceManager : TmafResourceManager;
       __ReaderProc : TStringReader;
       __WriterProc : TStringWriter;
       FStringOptions : TStringOptions;
       procedure __SetResourceManager(const Value: TmafResourceManager);
       procedure ReaderProcA(nStart, nLength: Int64; var S: String);
       procedure WriterProcA(nStart : Int64; var nLength: Int64; var S : String);
       procedure ReaderProcW(nStart, nLength: Int64; var S: String);
       procedure WriterProcW(nStart : Int64; var nLength: Int64; var S : String);
     protected
       function __GetResourceFile: String; override;
       procedure __SetModified(const Value: Boolean); override;
       procedure Free_ResourceDescriptor(var pData: Pointer); override;
       procedure __ReadStreamData(Sender: TObject; ID: Integer); override;
       procedure __WriteStreamData(Sender: TObject; ID: Integer); override;
       procedure __Internal_CreateResource; override;
       procedure __PrepareData; override;
       function __CreateDescriptor: PBaseDescriptor; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       procedure Save(bCompleteRewrite: Boolean = False); override;
       function Delete(nID: Integer): Integer; override;

       function Add(pDesc: PBaseDescriptor): PBaseDescriptor; override;
       function Update(pDesc: PStringResourceDescriptor; NewString: String): PStringResourceDescriptor;
       function CreateEmpty(ForceID: Cardinal = 0): PStringResourceDescriptor;
       function Get(ID: Cardinal; pData: PStringResourceDescriptor = nil): PChar;
       function Release(ID: Cardinal): Integer;
       property ResourceManager : TmafResourceManager read FpResourceManager write __SetResourceManager;
     published
       property Encoding;
       property StringOptions : TStringOptions read FStringOptions write FStringOptions default [soKeepEncoding];
     end;

     TSQLResource = class(TStringResource)
     public
       constructor Create; override;
     end;

     TOnResourceFileLoaded = procedure(Sender: TObject; ResType: TResourceFileType) Of Object;

     TmafResourceManager = class(TmafCustomManagerComponent)
     private
       FpFileResource : TFileResource;
       FpStringResource : TStringResource;
       FpSQLResource : TSQLResource;
       FsLanguageSubDir : String;
       FOnResourceFileLoaded : TOnResourceFileLoaded;
       FOnResourceFileClosed : TOnResourceFileLoaded;
       FOnResourceFileModified : TOnResourceFileLoaded;
       FpGlobalVars : TmafGlobalVars;
       FOnUnknownImageType : TOnUnknownImageType;
       procedure OnLoaded(Sender: TObject);
       procedure OnClose(Sender: TObject);
       procedure OnModified(Sender: TObject);
     protected
       procedure MSG_GlobalVar_Change(var Msg: TMessage); message MSG_GLOBALVAR_CHANGE;
       function __GetAvailableSkins(AList: TStrings): Integer;
       function __Get_ResourceType(AResource: TObject): TResourceFileType;
       procedure __RegisterAPI; override;
       procedure __OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer); override;
       procedure CloseManager; override;
       function __GetFileResource(QHS: pQHS): Integer;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       procedure __InformClients(MsgId: Integer);
       function GetAvailableLanguages(CurrentLanguage: PChar; Items: TStrings): Integer;
       function SetLanguage(AFileName: String): Integer;
     published
       property FileResource : TFileResource read FpFileResource write FpFileResource;
       property StringResource : TStringResource read FpStringResource write FpStringResource;
       property SQLResource : TSQLResource read FpSQLResource write FpSQLResource;
       property LanguageSubDir : String read FsLanguageSubDir write FsLanguageSubDir;
       property GlobalVars : TmafGlobalVars read FpGlobalVars write FpGlobalVars;

       // Events
       property OnUnknownImageType : TOnUnknownImageType read FOnUnknownImageType write FOnUnknownImageType;
       property OnResourceFileLoaded : TOnResourceFileLoaded read FOnResourceFileLoaded write FOnResourceFileLoaded;
       property OnResourceFileClosed : TOnResourceFileLoaded read FOnResourceFileClosed write FOnResourceFileClosed;
       property OnResourceFileModified : TOnResourceFileLoaded read FOnResourceFileModified write FOnResourceFileModified;
     end;

implementation

uses {$IFDEF Tracer} uMAF_Tracer, {$ENDIF} uMAF_ResourceClient, uMAF_Tools, Dialogs;

const MAX_EVENTS = 1;

var ManagerEvents : array[1..MAX_EVENTS] of Integer = (
      HM_RESOURCEMANAGER_QUERY
      );
    EventPriorities : array[1..MAX_EVENTS] of TEventPriority = (
      epHigh
      );

function TFileResource.__SeekElement(ID: Cardinal; var Idx: Integer; ADataList : TList = nil): PBaseDescriptor;
var pDesc : PBaseDescriptor;
begin
  pDesc := nil;
  If ((froSkinSupport in FOptions) And (FpCurrentSkin <> nil)) Then
    pDesc := inherited __SeekElement(ID, idx, FpCurrentSkin^.FpDataList);
  If pDesc = nil Then
    pDesc := inherited __SeekElement(ID, idx);
  Result := pDesc;
end;

procedure TFileResource.__SetOptions(const Value: TFileResourceOptions);
begin
  FOptions := Value;
  If Not (froSkinSupport in FOptions) Then      // If we cannot have one
    Exclude(FOptions, froAutoCreateSkinFolder); // .. then we don't want the other either
end;

procedure TFileResource.__SetResourceFile(const Value: String);
begin
  inherited;
  If Loaded Then begin
    If ((froSkinSupport in FOptions) And (FsSkinFolderRoot <> '')) Then
      FpSkinFolderNode := __GetFolder(FsSkinFolderRoot, nil, (froAutoCreateSkinFolder in FOptions));
    CurrentSkin := FsCurrentSkinName;  // and we set the skin new as either the resource file changed or we couldn't set before because it wasn't loaded yet
  end;
end;

procedure TFileResource.__SetResourceManager(const Value: TmafResourceManager);
begin
  FpResourceManager := Value;
  Owner := TComponent(FpResourceManager);
end;

procedure TFileResource.__WriteStreamData(Sender: TObject; ID: Integer);
var i : Integer;
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
            Streamer.WriteWord(pData^.ResX);
            Streamer.WriteWord(pData^.ResY);
            If pData^.FolderNode < 1 Then
              pData^.FolderNode := -1;
            Streamer.WriteInteger(pData^.FolderNode);
            Streamer.WriteByte(pData^.Flags);
          end;  //  --  For i := 0 To Data.Count - 1 Do
        end;
    Else inherited __WriteStreamData(Sender, ID);
  end;
end;

function TFileResource.Add(pDesc: PBaseDescriptor): PBaseDescriptor;
begin
  Result := inherited Add(pDesc);
  If Result <> nil Then begin
    PFileResourceDescriptor(Result)^.ResX := PFileResourceDescriptor(pDesc)^.ResX;
    PFileResourceDescriptor(Result)^.ResY := PFileResourceDescriptor(pDesc)^.ResY;
  end;
end;

constructor TFileResource.Create;
begin
  FbSkinChanging := False;
  ResourceType := rftMedia;
  HeaderID := HEADER_ID_FILE_RESOURCE;
  inherited;
  FOptions := [];
  FpSkinFolderNode := nil;
  StreamID := 863;
  Streamer.Attributes := 5;
  FsSkinFolderRoot := 'IconSkin';
end;

procedure TFileResource.__Clear_Data;
begin
  inherited;
  FpCurrentSkin := nil;
end;

{$IFDEF PNGimage}
  {$IFDEF D12+}
    Type TmpPNG = class(TPNGImage);
  {$ELSE}
    Type TmpPNG = class(TPNGObject);
  {$ENDIF}
{$ENDIF}

function TFileResource.__CreateDescriptor: PBaseDescriptor;
begin
  Result := PBaseDescriptor(__Create_FileResourceDescriptor);
end;

function TFileResource.__CreateGraphicObject(pDesc: PFileResourceDescriptor): TGraphic;
var FileExt : String;
begin
  FileExt := LowerCase(ExtractFileExt(pDesc^.ResName));
  Result := nil;
  {$IFDEF PNGimage}
  If FileExt = '.png' Then
    Result := TmpPNG.Create;
  {$ENDIF}
  If FileExt = '.jpg' Then
    Result := TJPEGImage.Create;
  If FileExt = '.bmp' Then
    Result := TBitmap.Create;
  If FileExt = '.ico' Then
    Result := TIcon.Create;

  pDesc^.aStream.Position := 0;
  If Not Assigned(Result) Then begin
    If Assigned(ResourceManager.OnUnknownImageType) Then begin
      ResourceManager.OnUnknownImageType(Self, FileExt, pDesc^.aStream, TGraphic(Result));
    end;
    If Result = nil Then begin
      pDesc^.aStream.Free;
      pDesc^.RefCount := 0;
      Exit;
    end;  //  --  If Result = nil Then
  end;  //  --  If not Assigned(Result) Then
  If pDesc^.aStream.Position = 0 then
    Result.LoadFromStream(pDesc^.aStream);
  If Not (froCache in FOptions) Then begin
    pDesc^.aStream.Free;
    pDesc^.aStream := nil;
    pDesc^.RefCount := 0;
  end;
end;

function TFileResource.__GetCurrentSkin: String;
begin
  Result := '';
  If Not (froSkinSupport in FOptions) Then
    Exit;
  If FpCurrentSkin = nil Then
    FpCurrentSkin := __GetFolder(FsSkinFolderRoot + '\' + FsCurrentSkinName, nil);
  If FpCurrentSkin <> nil Then
    Result := FpCurrentSkin^.NodeName;
end;

procedure TFileResource.__Internal_CreateResource;
begin
  pHeader^.HeaderID := HEADER_ID_FILE_RESOURCE;
  inherited;
end;

procedure TFileResource.__ReadStreamData(Sender: TObject; ID: Integer);
var i, nCount: Integer;
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
            Streamer.ReadWord(pData^.ResX);
            Streamer.ReadWord(pData^.ResY);
            Streamer.ReadInteger(pData^.FolderNode);
            aNode := __GetFolder(pData^.FolderNode, nil);
            If aNode = nil Then begin
              pData^.FolderNode := -1;
              Root.Add(pData);
            end else
              aNode^.FpDataList.Add(pData);
            Streamer.ReadByte(pData^.Flags);
            pData^.RefCount := 0;
            pData^.aStream := nil;
            Data.Add(pData);
          end;  //  --  For i := 1 To nCount Do
        end;
    Else inherited __ReadStreamData(Sender, ID);
  end;
end;

procedure TFileResource.__SetCurrentSkin(const Value: String);
var S : String;
    bFound : Boolean;
begin
  FsOldSkinName := FsCurrentSkinName;  // save the name
  FsCurrentSkinName := Value;

  If ((csLoading in ResourceManager.ComponentState) Or (FsCurrentSkinName = '')) Then begin
    FsOldSkinName := '';
    Exit;
  end;

  FpCurrentSkin := __GetFolder(FsSkinFolderRoot + '\' + FsCurrentSkinName, nil);
  If FpCurrentSkin = nil Then begin
    If Self.Loaded Then
      FsCurrentSkinName := '';
{    If Assigned(ResourceManager.GlobalVars) Then begin
      S := ResourceManager.GlobalVars.GetString('CurrentSkin', bFound);
      If ((bFound) And (S <> '')) Then
        CurrentSkin := S;  // calls __SetCurrentSkin again
    end;         }
    FsOldSkinName := '';
    Exit;
  end;

  FbSkinChanging := True;
  ResourceManager.__InformClients(WM_ICON_SKIN_CHANGE);
  FbSkinChanging := False;
  FsOldSkinName := '';
  If Assigned(ResourceManager.GlobalVars) Then begin
    ResourceManager.Globalvars.UnregisterCallBack('CurrentSkin', ResourceManager);  // we need to unregister the callback or we get a stack overflow :P
    ResourceManager.GlobalVars.AddString('CurrentSkin', CurrentSkin, [vfSave], lsDefault);
    ResourceManager.Globalvars.RegisterCallBack('CurrentSkin', ResourceManager);
  end;
end;

procedure TFileResource.Free_ResourceDescriptor(var pData: Pointer);
begin
  __Free_FileResourceDescriptor(PFileResourceDescriptor(pData));
end;

function TFileResource.Get(pDesc: PFileResourceDescriptor): TGraphic;
begin
  Result := nil;
  If __InternalLoadFileResource(PFileDescriptor(pDesc)) Then
    Result := __CreateGraphicObject(pDesc);
end;

function TFileResource.GetDescriptorByPath(APath: String): PBaseDescriptor;
var S, FilePath, ResourceName : String;
    AFolder : PFolderNode;
    i : Integer;
    DataList : TList;
begin
  Result := nil;
  FilePath := IncludeTrailingPathDelimiter(ExtractFilePath(ResourceFile));
  ResourceName := ExtractFileName(APath);
  S := APath;
  System.Delete(S, 1, Length(FilePath));
  System.Delete(S, Pos(ResourceName, S), Length(ResourceName));
  S := ExcludeTrailingPathDelimiter(S);
  AFolder := __GetFolder(S, nil);
  If AFolder = nil Then
    DataList := Root
  Else
  DataList := AFolder^.FpDataList;
  For i := 0 To DataList.Count - 1 Do
    If PFileResourceDescriptor(DataList.Items[i])^.ResName = ResourceName Then begin
      Result := PBaseDescriptor(DataList.Items[i]);
      Break;
    end;
end;

function TFileResource.Get(ID: Cardinal; ACategory: String; var pDesc: PFileResourceDescriptor): TGraphic;
var idx : Integer;
begin
  Result := nil;
  pDesc := PFileResourceDescriptor(__SeekElement(ID, idx));
  If __InternalLoadFileResource(PFileDescriptor(pDesc)) then
    Result := __CreateGraphicObject(pDesc);
end;

function TFileResource.GetFileStream(nID: Cardinal): TMemoryStream;
var idx : Integer;
    pDesc : PFileResourceDescriptor;
begin
  Result := nil;
  pDesc := PFileResourceDescriptor(__SeekElement(nID, idx));
  If __InternalLoadFileResource(PFileDescriptor(pDesc)) Then
    Result := pDesc^.aStream;
end;

{ TStringResource }

constructor TStringResource.Create;
begin
  FStringOptions := [soKeepEncoding];
  ResourceType := rftString;
  HeaderID := HEADER_ID_STRING_RESOURCE;
  inherited;
  StreamID := 864;
end;

destructor TStringResource.Destroy;
begin
  inherited;
end;

// read alsways as AnsiString
procedure TStringResource.ReaderProcA(nStart, nLength: Int64; var S: String);
var aString : AnsiString;
    PC : PAnsiChar;
begin
  aString := AnsiString(S);
  GetMem(PC, nLength);
  FillChar(PC^, nLength, 0);
  Seek(FHandle, nStart);
  BlockRead(FHandle, PC^, nLength);
  S := String(PC);
  FreeMem(PC, nLength);
end;

// reads always as WideString
procedure TStringResource.ReaderProcW(nStart, nLength: Int64; var S: String);
var aString : WideString;
    PC : PWideChar;
begin
  aString := WideString(S);
  GetMem(PC, nLength);
  FillChar(PC^, nLength, 0);
  Seek(FHandle, nStart);
  BlockRead(FHandle, PC^, nLength);
  S := String(PC);
  FreeMem(PC, nLength);
end;

// writes always as AnsiString
procedure TStringResource.WriterProcA(nStart : Int64; var nLength: Int64; var S : String);
var aString : AnsiString;
    PC : PAnsiChar;
begin
  aString := AnsiString(S);
  nLength := Length(aString) + 1;
  GetMem(PC, nLength);
  FillChar(PC^, nLength, 0);
  StrLCopy(PC, PAnsiChar(aString), nLength - 1);
  Seek(FHandle, nStart);
  BlockWrite(FHandle, PC^, nLength);
  FreeMem(PC, nLength);
end;

// writes always as WideString
procedure TStringResource.WriterProcW(nStart : Int64; var nLength: Int64; var S : String);
var aString : WideString;
    PC : PWideChar;
begin
  aString := WideString(S);
  nLength := (Length(aString) * 2) + 2;
  GetMem(PC, nLength);
  FillChar(PC^, nLength, 0);
  {$IFDEF Unicode}
  StrLCopy(PC, PWideChar(aString), nLength - 2);
  {$ELSE}
  CopyMemory(PC, PWideChar(aString), nLength - 2);
  {$ENDIF}
  Seek(FHandle, nStart);
  BlockWrite(FHandle, PC^, nLength);
  FreeMem(PC, nLength);
end;

function TStringResource.__CreateDescriptor: PBaseDescriptor;
begin
  Result := PBaseDescriptor(__Create_StringResourceDescriptor);
end;

function TStringResource.__GetResourceFile: String;
begin
  Result := inherited __GetResourceFile;
{  S := inherited __GetResourceFile;
  If ExtractFilePath(S) <> '' Then
    FullFileName := S
  Else
    FullFileName := FpResourceManager.FsLanguageSubDir + ExtractFileName(S);
  Result := FullFileName; }
end;

procedure TStringResource.__Internal_CreateResource;
begin
  pHeader^.HeaderID := HEADER_ID_STRING_RESOURCE;
  inherited;
end;

procedure TStringResource.__PrepareData;
var i : Integer;
    pDesc : PStringResourceDescriptor;
begin
  inherited;
  Case Encoding Of
    etANSI       : begin
                     __ReaderProc := ReaderProcA;
                     __WriterProc := WriterProcA;
                   end;
    etWideString : begin
                     __ReaderProc := ReaderProcW;
                     __WriterProc := WriterProcW;
                   end;
  end;
  // we read all Strings into the memory
  For i := 0 To Data.Count - 1 Do begin
    pDesc := PStringResourceDescriptor(Data.Items[i]);
    Get(pDesc^.StringID, pDesc);
  end;
end;

function TStringResource.Delete(nID: Integer): Integer;
var pDesc : PStringResourceDescriptor;
    idx : Integer;
begin
  Result := ERR_RESOURCE_NOT_FOUND;
  pDesc := PStringResourceDescriptor(__SeekElement(nID, idx));
  If ((idx > -1) And (pDesc <> nil)) Then begin
    Free_ResourceDescriptor(Pointer(pDesc));
    Data.Delete(idx);
    Dec(pHeader^.NumItems);
    Modified := True;
    Result := ERR_NO_ERROR;
  end;  //  --  If ((idx > -1) And (pDesc <> nil)) Then
end;

procedure TStringResource.Free_ResourceDescriptor(var pData: Pointer);
begin
  __Free_StringResourceDescriptor(PStringResourceDescriptor(pData));
end;

function TStringResource.Get(ID: Cardinal; pData: PStringResourceDescriptor = nil): PChar;
var idx : Integer;
    pDesc : PStringResourceDescriptor;
    S : String;
begin
  Result := nil;
  If pData <> nil Then pDesc := pData
                  Else pDesc := PStringResourceDescriptor(__SeekElement(ID, idx));
  If Assigned(pDesc) Then begin
    If pDesc^.nLength = 0 Then
      Exit;
    If pDesc^.pData = nil Then begin
      Seek(FHandle, pDesc^.nStart);
      __ReaderProc(pDesc^.nStart, pDesc^.nLength, S);
      StrToPChar(S, pDesc^.pData);
    end;  //  --  If pDesc^.pData = nil Then
    Result := pDesc^.pData;
  end;
end;

function TStringResource.Release(ID: Cardinal): Integer;
begin
  Result := ERR_NO_ERROR;
end;

procedure TStringResource.Save(bCompleteRewrite: Boolean = False);
var i : Integer;
    pDesc : PStringResourceDescriptor;
    nStart : Int64;
    S : String;
begin
  nStart := SizeOf(RFileResourceHeader);
  If FileMode = fmOpenRead Then
    __SetFileAccessMode(fmOpenReadWrite);
  Seek(FHandle, nStart);
  FStringOptions := [];
  If Not (soKeepEncoding in FStringOptions) Then begin
    {$IFDEF Unicode}
    pHeader^.Encoding := Encoding_UTF_16;
    __WriterProc := WriterProcW;
    __ReaderProc := ReaderProcW;
    {$ELSE}
    pHeader^.Encoding := Encoding_UTF_8;
    __WriterProc := WriterProcA;
    __ReaderProc := ReaderProcA;
    {$ENDIF}
  end;
  For i := 0 To Data.Count - 1 Do begin
    pDesc := PStringResourceDescriptor(Data.Items[i]);
    pDesc^.nStart := nStart;
    S := String(pDesc^.pData);
    __WriterProc(pDesc^.nStart, pDesc^.nLength, S);
//    WriteData(pDesc^.pData, pDesc^.nLength);
    nStart := nStart + pDesc^.nLength;
  end;
  pHeader^.FileMapping_Start := nStart;
  pHeader^.NumItems := Data.Count;
  inherited;
  __SetFileAccessMode(fmOpenRead);
end;

function TStringResource.Update(pDesc: PStringResourceDescriptor; NewString: String): PStringResourceDescriptor;
var idx : Integer;
begin
  Result := PStringResourceDescriptor(__SeekElement(pDesc^.StringID, idx));
  If Assigned(Result) Then begin
    FreePChar(Result^.pData);
    Result^.nLength := StrToPChar(NewString, Result^.pData);
    Modified := True;
  end;  //  --  f Assigned(Result) Then
end;

procedure TStringResource.__ReadStreamData(Sender: TObject; ID: Integer);
var i, nCount : Integer;
    pData : PStringResourceDescriptor;
begin
  Case ID Of
    1 : begin
          Streamer.ReadInteger(nCount);
          For i := 1 To nCount Do begin
            New(pData);
            FillChar(pData^, SizeOf(RStringResourceDescriptor), 0);
            Streamer.ReadCardinal(pData^.StringID);
            Streamer.ReadInt64(pData^.nStart);
            Streamer.ReadInt64(pData^.nLength);
            Streamer.ReadInteger(pData^.CategoryID);
            Streamer.ReadInteger(pData^.ModuleID);
            If pData^.nLength > 0 Then
              Data.Add(pData)
            Else
              Dispose(pData);
          end;  //  --  For i := 1 To nCount Do
        end
    else inherited __ReadStreamData(Sender, ID);
  end;
end;

procedure TStringResource.__SetModified(const Value: Boolean);
begin
  inherited;
  SortItems;  // in case the ID was changed, we need to sort the items again 
end;

procedure TStringResource.__SetResourceManager(const Value: TmafResourceManager);
begin
  FpResourceManager := Value;
  Owner := FpResourceManager;
end;

procedure TStringResource.__WriteStreamData(Sender: TObject; ID: Integer);
var i : Integer;
    pData : PStringResourceDescriptor;
begin
  Case ID Of
    1 : begin
          Streamer.WriteInteger(Data.Count);
          For i := 0 To Data.Count - 1 Do begin
            pData := PStringResourceDescriptor(Data.Items[i]);
            Streamer.WriteCardinal(pData^.StringID);
            Streamer.WriteInt64(pData^.nStart);
            Streamer.Writeint64(pData^.nLength);
            Streamer.WriteInteger(pData^.CategoryID);
            Streamer.WriteInteger(pData^.ModuleID);
          end;  //  --  For i := 0 To Data.Count - 1 Do 
        end
    else inherited __WriteStreamData(Sender, ID);
  end;
end;

function TStringResource.Add(pDesc: PBaseDescriptor): PBaseDescriptor;
var S : String;
begin
  Result := inherited Add(pDesc);

  If pDesc^.ID < 1 Then
    Result^.ID := HighID + 1
  Else
    Result^.ID := pDesc^.ID;
  S := String(PStringResourceDescriptor(pDesc)^.pData);
  Result^.nLength := StrToPChar(S, PStringResourceDescriptor(Result)^.pData);
  SortItems;
end;

function TStringResource.CreateEmpty(ForceID: Cardinal = 0): PStringResourceDescriptor;
var idx : Integer;
begin
  New(Result);
  FillChar(Result^, SizeOf(RStringResourceDescriptor), 0);

  Result^.StringID := HighID + 1;
  If ForceID > 0 Then
    If __SeekElement(ForceID, idx) = nil Then
      Result^.StringID := ForceID;
end;

{ TSQLResource }

constructor TSQLResource.Create;
begin
  ResourceType := rftSQL;
  HeaderID := HEADER_ID_SQL_RESOURCE;
  inherited;
  StreamID := 865;
end;

{ TERPResourceManager }

constructor TmafResourceManager.Create(AOwner: TComponent);
begin
  inherited;
  FpGlobalVars := nil;
  ManagerType := MT_RESOURCE_MANAGER;
  FpFileResource := TFileResource.Create;
  FpFileResource.ResourceManager := Self;
  FpFileResource.OnLoaded := OnLoaded;
  FpFileResource.OnClosed := OnClose;
  FpFileResource.OnModified := OnModified;

  FpStringResource := TStringResource.Create;
  FpStringResource.ResourceManager := Self;
  FpStringResource.OnLoaded := OnLoaded;
  FpStringResource.OnClosed := OnClose;
  FpStringResource.OnModified := OnModified;

  FpSQLResource := TSQLResource.Create;
  FpSQLResource.ResourceManager := Self;
  FpSQLResource.OnLoaded := OnLoaded;
  FpSQLResource.OnClosed := OnClose;
  FpSQLResource.OnModified := OnModified;
end;

destructor TmafResourceManager.Destroy;
begin
  FpFileResource.Free;
  FpStringResource.Free;
  FpSQLResource.Free;
  inherited;
end;

procedure TmafResourceManager.Loaded;
var S : String;
    bFound : Boolean;
begin
  {$IFDEF Tracer}
    MAFTracer.Enter('TmafResourceManager.Loaded');
  {$ENDIF}
  inherited;

  FileResource.ResourceFile := FileResource.ResourceFile;
  bFound := False;
  If Assigned(FpGlobalVars) Then begin
    S := FpGlobalVars.GetString('CurrentSkin', bFound);
    If Not bFound Then begin
      // it could be, that the TmafGlobalVars still haven't loaded yet, so we put an observer on that variable
      FpGlobalVars.AddString('CurrentSkin', '', [vfSave], lsDefault);
      FpGlobalvars.RegisterCallBack('CurrentSkin', Self);
    end;
  end;
  If ((bFound) And (S <> '')) Then
    FileResource.CurrentSkin := S
  Else
    If FileResource.CurrentSkin <> '' Then
      FileResource.CurrentSkin := FileResource.CurrentSkin;

  If StringResource.ResourceFile <> '' Then
    If bRunMode Then
      StringResource.ResourceFile := ExtractFileName(StringResource.ResourceFile)
    Else
      StringResource.ResourceFile := StringResource.ResourceFile;
  If SQLResource.ResourceFile <> '' Then
    SQLResource.ResourceFile := ExtractFileName(SQLResource.ResourceFile);
  {$IFDEF Tracer}
    MAFTracer.Leave;
  {$ENDIF}
end;

procedure TmafResourceManager.MSG_GlobalVar_Change(var Msg: TMessage);
var S, VarName : String;
    bFound : Boolean;
begin
  VarName := String(PChar(Msg.WParam));
  If VarName = 'CurrentSkin' Then begin
    S := FpGlobalVars.GetString('CurrentSkin', bFound);
    If bFound Then
      FileResource.CurrentSkin := S;
  end;
end;

procedure TmafResourceManager.CloseManager;
begin
  inherited;
  FpFileResource.ResourceFile := '';
  FpStringResource.ResourceFile := '';
  FpSQLResource.ResourceFile := '';
end;

procedure TmafResourceManager.OnClose(Sender: TObject);
begin
  Case __Get_ResourceType(Sender) Of
    rftString : __InformClients(WM_LANGUAGE_CHANGE);
    rftMedia  : __InformClients(WM_MEDIA_RESOURCE_CHANGE);
  end;

  If Assigned(FOnResourceFileClosed) Then
    FOnResourceFileClosed(Self, __Get_ResourceType(Sender));
end;

procedure TmafResourceManager.OnLoaded(Sender: TObject);
begin
  Case __Get_ResourceType(Sender) Of
    rftString : __InformClients(WM_LANGUAGE_CHANGED);
    rftMedia  : __InformClients(WM_MEDIA_RESOURCE_CHANGED);
  end;

  If Assigned(FOnResourceFileLoaded) Then
    FOnResourceFileLoaded(Self, __Get_ResourceType(Sender));
end;

procedure TmafResourceManager.OnModified(Sender: TObject);
begin
  If Assigned(FOnResourceFileModified) Then
    FOnResourceFileModified(Self, __Get_ResourceType(Sender));
end;

function TmafResourceManager.__Get_ResourceType(AResource: TObject): TResourceFileType;
begin
  Result := rftUnknown;
  If AResource = FpFileResource Then
    Result := rftMedia
  Else
    If AResource = FpStringResource Then
      Result := rftString
    Else
      If AResource = FpSQLResource Then
        Result := rftSQL;
end;

function TmafResourceManager.__GetAvailableSkins(AList: TStrings): Integer;
var i : Integer;
begin
  Result := ERR_NO_ERROR;
  If FpFileResource.Loaded = False Then
    Result := ERR_RESOURCEFILE_CLOSED;
  If Not (froSkinSupport in FpFileResource.Options) Then
    Result := ERR_SKINS_DISABLED;

  If Result = ERR_NO_ERROR Then begin
    If FpFileResource.FpSkinFolderNode <> nil Then begin
      AList.Clear;
      For i := 0 To FpFileResource.FpSkinFolderNode.FpChildren.Count - 1 Do
        AList.Add(PFolderNode(FpFileResource.FpSkinFolderNode.FpChildren.Items[i])^.NodeName);
    end else
      Result := ERR_COMPONENT_SETUP_FAILURE; 
  end;
end;

procedure TmafResourceManager.__InformClients(MsgID: Integer);
var i : Integer;
    Msg : TMessage;
begin
  Msg.Msg := MsgId;
  For i := 0 To Clients.Count - 1 Do        // inform all clients
    If IsClass(Clients[i], TmafResourceClient) Then // make sure, it´s a client
      TmafResourceClient(Clients[i]).Dispatch(Msg); // and send the message
end;

// QHS^.Reserved2 defines, how and what the ResourceManager returns as data
// 1 : the filestream will be returned as live pointer
// 2 : TGraphic object will be returned, must be destroyed on client side, the ResID is used
// 3 : same as 2, but the path and filename will be used
function TmafResourceManager.__GetFileResource(QHS: pQHS): Integer;
var pDesc: PFileResourceDescriptor;
begin
  Result := ERR_NO_ERROR;
  Case QHS^.Reserved2 Of
    1 : begin
          QHS^.pChildObj := Pointer(FileResource.GetFileStream(QHS^.Reserved1));
          If QHS^.pChildObj = nil Then
            Result := ERR_RESOURCE_NOT_FOUND;
        end;
    2 : begin
          PResQueryStruct(QHS^.pChildObj)^.pResult := FileResource.Get(PResQueryStruct(QHS^.pChildObj)^.ResID, '', pDesc);
          if PResQueryStruct(QHS^.pChildObj)^.pResult = nil then
            Result := ERR_RESOURCE_NOT_FOUND;
        end;
    3 : begin
          pDesc := PFileResourceDescriptor(FpFileResource.GetDescriptorByPath(String(PResQueryStruct(QHS^.pChildObj)^.ResName)));
          If Assigned(pDesc) Then
            QHS^.pChildObj := FpFileResource.__CreateGraphicObject(pDesc);
          If QHS^.pChildObj = nil Then
            Result := ERR_RESOURCE_NOT_FOUND;
        end;
  end;
end;

procedure TmafResourceManager.__OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
//var pDesc: PFileResourceDescriptor;
begin
  ErrCode := ERR_NO_ERROR;
  Case SubHookID Of
      HM_RESOURCEMANAGER_QUERY : begin
        Case QHS^.SubHookID Of
          RM_GET_FILERES_NAME   : begin
                                    If FpFileResource.ResourceFile <> '' Then
                                      StrLCopy(PChar(QHS^.pChildObj), PChar(FpFileResource.ResourceFile), MAX_PATH);
                                  end;
          RM_GET_STRINGRES_NAME : begin
                                    If FpStringResource.ResourceFile <> '' Then
                                      StrLCopy(PChar(QHS^.pChildObj), PChar(FpStringResource.ResourceFile), MAX_PATH);
                                  end;
          RM_GET_SQLRES_NAME    : begin
                                    If FpSQLResource.ResourceFile <> '' Then
                                      StrLCopy(PChar(QHS^.pChildObj), PChar(FpSQLResource.ResourceFile), MAX_PATH);
                                  end;
          RM_GET_ICON_SKIN      : begin
                                    If FpFileResource.CurrentSkin <> '' Then
                                     StrLCopy(PChar(QHS^.pChildObj), PChar(FileResource.CurrentSkin), Length(FileResource.CurrentSkin));
                                  end;
          RM_SET_ICON_SKIN      : begin
                                    If QHS^.pChildObj <> nil Then
                                      FileResource.CurrentSkin := String(PChar(QHS^.pChildObj));
                                  end;
          RM_GET_SKIN_LIST      : ErrCode := __GetAvailableSkins(TStrings(QHS^.pChildObj));
          RM_GET_FILE_STREAM    : begin
                                    QHS^.pChildObj := Pointer(FileResource.GetFileStream(QHS^.Reserved1));
                                    If QHS^.pChildObj = nil Then
                                      ErrCode := ERR_RESOURCE_NOT_FOUND;
                                  end;
          RM_GET_GRAPHIC        : ErrCode := __GetFileResource(QHS);//PResQueryStruct(QHS^.pChildObj)^.pResult := FileResource.Get(PResQueryStruct(QHS^.pChildObj)^.ResID, '', pDesc);
          RM_RELEASE_FILE_STREAM,
          RM_RELEASE_GRAPHIC    : FileResource.Release(QHS^.Reserved1);
          RM_GET_STRING         : PResQueryStruct(QHS^.pChildObj)^.pResult := StringResource.Get(PResQueryStruct(QHS^.pChildObj)^.ResID);
          RM_GET_SQL            : PResQueryStruct(QHS^.pChildObj)^.pResult := SQLResource.Get(PResQueryStruct(QHS^.pChildObj)^.ResID);
          RM_SET_ACTIVE_LANGUAGE: SetLanguage(String(PChar(QHS^.pChildObj)));
          RM_GET_LANGUAGE_LIST  : ErrCode := GetAvailableLanguages(PChar(QHS^.pChildObj), TStrings(UserParam));
        end;
      end;
    Else inherited __OnEvent(SubHookID, QHS, UserParam, ErrCode);
  end;  //  --  Case SubHookID Of
end; // __OnEvent

procedure TmafResourceManager.__RegisterAPI;
var i : Integer;
begin
  inherited;
  // register our events
  If Assigned(ModuleController) Then
    For i := 1 To MAX_EVENTS Do
      ModuleController.RegisterSubHookEvent(ManagerEvents[i], __OnEvent, EventPriorities[i]);
end;

function TmafResourceManager.SetLanguage(AFileName: String): Integer;
var S : String;
begin
  Result := ERR_PARAM_FAILURE;
  S := FsLanguageSubDir + AFileName;
  If Pos('.'+sDefault_Resourcefile_Extension, AFileName) < 1 Then
    S := S + '.'+sDefault_Resourcefile_Extension;
  If FileExists(S) Then
    If FpStringResource.IsResourceType(S) Then begin
      FpStringResource.ResourceFile := S;
      Result := ERR_NO_ERROR;
    end;
end;

function TmafResourceManager.GetAvailableLanguages(CurrentLanguage: PChar; Items: TStrings): Integer;
var SearchRec : TSearchRec;
    S : String;
begin
  If Items = nil Then begin
    Result := ERR_PARAM_FAILURE;
    Exit;
  end;

  Items.Clear;
  Result := ERR_COMPONENT_SETUP_FAILURE;
  If FsLanguageSubDir = '' Then
    Exit;

  If FindFirst(FsLanguageSubDir + '*.'+sDefault_Resourcefile_Extension, faAnyFile, SearchRec) = 0 Then begin
    If FpStringResource.IsResourceType(FsLanguageSubDir + SearchRec.Name) Then begin
      S := SearchRec.Name;
      Delete(S, Pos('.'+sDefault_Resourcefile_Extension, S), 4);
      Items.Add(S);
      Result := ERR_NO_ERROR;
    end;
    While FindNext(SearchRec) = 0 Do begin
      If FpStringResource.IsResourceType(FsLanguageSubDir + SearchRec.Name) Then begin
        S := SearchRec.Name;
        Delete(S, Pos('.'+sDefault_Resourcefile_Extension, S), 4);
        Items.Add(S);
      end;
    end;  //  --  While FindNext(SearchRec) = 0 Do
    FindClose(SearchRec);
  end;  //  --  If FindFirst(FsLanguageSubDir + '*.maf', faAnyFile, SearchRec) = 0 Then
  S := ExtractFileName(FpStringResource.ResourceFile);
  Delete(S, Pos('.'+sDefault_Resourcefile_Extension, S), 4);
  StrLCopy(CurrentLanguage, PChar(S), Length(S));
end;

end.
