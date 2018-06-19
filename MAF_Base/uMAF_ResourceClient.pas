{*******************************************************************************
Name         : uMAF_ResourceClient.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2004-2009 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 28.01.2004
Last Update  : 22.11.2009
Version      : 1.2.014
Last Changes :

1.2.014 (22.11.2009) -----------------------------------------------------------
- [CHG] now using only the TmafBaseComponent.__AdvQuery_Manager method to query
        the TmafResourceManager interface
- [ADD] new message handler WM_LANGUAGE_CHANGED which is executed after the
        language file change in the TmafResourceManager
- [FIX] ApplyLanguage isn't executed anymore when the language is about to change
        but only after the language change (indicated by WM_LANGUAGE_CHANGED)
1.2.013 (09.09.2009) -----------------------------------------------------------
- [ADD] List of components to update for the new component editor, wich makes the
        component itself independent of the TAG property of components. The data
        is as always written in a binary stream in the component editor
- [ADD] If a TImage is among the update components then the IconSkin change
        event will now automatically try to update that image
1.2.012 (29.08.2009) -----------------------------------------------------------
- [CHG] change to new TERPResourceManager started with removing obsolete functions
        and implementing the new interfaces 
1.1.011 (12.06.2009) -----------------------------------------------------------
- [CHG] moved the answer string for currently used language DLL from QHS^.Param
        to QHS^.pChildObj for the Delphi 2009 support changes
1.1.010 (29.10.2008) -----------------------------------------------------------
- [ADD] Icon support
1.1.009 (22.04.2007) -----------------------------------------------------------
- [ADD] Notification list for components, which wish to know about language
        change
1.1.008 (06.04.2007) -----------------------------------------------------------
- [ADD] added GetAvailableLanguages to retrieve all language modules available to
        the system
- [ADD] added GetCurrentLanguageLib to get the module file name for the currently
        used language 
1.1.007 (11.03.2007) -----------------------------------------------------------
- [ADD] added a method to load a bitmap library, wich i just forgot to add here
        before, the code to do it was already in the Manager ;)
- [FIX] fixed an "out of memory"-bug in adding libraries into the
        ResManager, because the PChar variable wasn't set to NIL before allocating
        memory vor the name of the library
1.1.006 (01.03.2007) -----------------------------------------------------------
- [CHANGE] changed to use TManagerBaseContainer
1.0.005 (08.02.2007) -----------------------------------------------------------
- added support for resource SQL strings
1.0.004 (02.02.2007) -----------------------------------------------------------
- added column-caption-support for TListView in ApplyLanguage
1.0.003 (04.03.2004) -----------------------------------------------------------
- Multi language support implemented
1.0.002 (22.02.2004) -----------------------------------------------------------
- added a list of used bitmap resources, so that WE can tell the resource manager
  to free them up and not the Owner form anymore
1.0.001 (28.01.2004) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_ResourceClient;

{$I MAFramework.inc}

interface

uses Messages, Classes, Windows, SysUtils, Graphics, Controls, Forms, StdCtrls,
     Menus, ComCtrls, ExtCtrls, TypInfo, Buttons, ImgList,
     {$IFDEF PNGComponents} PngImageList, PngBitBtn, PngSpeedButton, {$ENDIF}
     {$IFDEF PNGimage} PNGimage, {$ENDIF}
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_Core, uMAF_TemplateStreamer, uMAF_Tools;

Type RComponentInfo = packed record
       ResID : Cardinal;        // ResourceID to use
       ComponentName : String;  // Name of the Component
       aComp : TObject;         // the component pointer to have fast access
       ClassID : Word;          // ID of the class in the AClasses array
     end;
     PComponentInfo = ^RComponentInfo;

     RImageListInfo = packed record
       ImageList  : TCustomImageList;
       FpData : TList;          // list of PImageInfo
     end;
     PImageListInfo = ^RImageListInfo;

     RImageInfo = packed record
       Index : Integer;
       ResID : Cardinal;
     end;
     PImageInfo = ^RImageInfo;

     TManageOption = (moCaption, moHint, moPicture);
     TManageOptions = Set Of TManageOption;

     TmafResourceClient = class(TmafBaseComponent)
     private
       FBeforeLanguageChange,
       FAfterLanguageChange   : TNotifyEvent;
       FOnIconSkinChange      : TNotifyEvent;
       FBeforeApplyInitialLanguage : TNotifyEvent;
       FAfterApplyInitialLanguage : TNotifyEvent;
       FpUsedBmpList          : TList;
       FpNotifyList           : TList;
       FpComponents           : TList;  // list of PComponentInfo to update language and images
       FpImageLists           : TList;  // List of TImageLists the user gives us to manage
       FpStreamer             : TTemplateStreamer; // streamer to write the binary component stream
       FManageOptions         : TManageOptions;
       procedure WMForm_Initialized(var Message: TMessage); message MSG_FORM_INITIALIZED;
       procedure WMLanguageChange(var Message: TMessage); message WM_LANGUAGE_CHANGE;
       procedure WMLanguageChanged(var Message: TMessage); message WM_LANGUAGE_CHANGED;
       procedure WMIconSkinChange(var Message: TMessage); message WM_ICON_SKIN_CHANGE;
       procedure ReadData(aStream: TStream);
       procedure WriteData(aStream: TStream);
       function __GetUpdateComponentCount: Integer;
       function __FindComponent(AComponent: TComponent; AName: String): TObject;
       function __FindSpecialComponent(pData : PComponentInfo): TObject;
       function __Find_ImageListInfo(IL: TCustomImageList): PImageListInfo;
       function __Find_ImageInfo(AList: TList; ResID: Cardinal): PImageInfo;
       function __AddGraphic_ImageList(AGraphic: TGraphic; AList: TCustomImageList; Idx: Integer = -1): Integer;
     protected
       procedure DefineProperties(Filer: TFiler); override;
       function __InternalJump(nCommand: Integer; Param: Cardinal): Pointer;
       procedure __DisconnectFromManager; override;
       procedure __ReadStreamData(Sender: TObject; ID: Integer);
       procedure __WriteStreamData(Sender: TObject; ID: Integer);
       function __GetResourceFileName(AQueryID: Integer): String;
       function __GetFileObject(AQueryID, Res2: Integer; nID: Cardinal): Pointer;
     public
       bLocalMode : Boolean;   // true in design time
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       function __IsSupported(AComponent: TComponent): Boolean;
       procedure __ApplyLanguage(MyOwner: TComponent);
       procedure ApplyLanguage;
       function GetSQL(ID : Integer): String;
       function GetCurrentIconSkin: String;
       procedure SetCurrentIconSkin(AIconSkinName: String);
       procedure GetAvailableSkins(AList: TStrings);
       procedure SetActiveLanguage(LibName: String);
       function GetAvailableLanguages(List: TStrings): String;
       function GetCurrentLanguageLib: String;
       function GetCurrentFileResource: String;
       function GetCurrentStringResource: String;
       function GetCurrentSQLResource: String;

       procedure AddNotifyComponent(AComponent: TComponent);
       procedure DeleteNotifyComponent(AComponent: TComponent);

       function GetGraphic(ID: Cardinal; AImage: TImage = nil): TGraphic; overload;
       function GetGraphic(ID: Cardinal; IL : TCustomImageList): Integer; overload;
       function GetGraphic(AName: String) : TGraphic; overload;
       procedure ReleaseGraphic(ID: Cardinal);
       function GetString(ID: Cardinal): String;
       function GetFileAsStream(nID: Cardinal): TMemoryStream;
       property UpdateComponents : TList read FpComponents;
     published
       property ManageOptions : TManageOptions read FManageOptions write FManageOptions default [moCaption,moPicture];
       property UpdateComponentCount : Integer read __GetUpdateComponentCount;
       property BeforeLanguageChange : TNotifyEvent read FBeforeLanguageChange write FBeforeLanguageChange;
       property AfterLanguageChange : TNotifyEvent read FAfterLanguageChange write FAfterLanguageChange;
       property BeforeApplyInitialLanguage : TNotifyEvent read FBeforeApplyInitialLanguage write FBeforeApplyInitialLanguage;
       property AfterApplyInitialLanguage : TNotifyEvent read FAfterApplyInitialLanguage write FAfterApplyInitialLanguage;
       property OnIconSkinChange : TNotifyEvent read FOnIconSkinChange write FOnIconSkinChange;
     end; // TResClient

const MAX_CLASSES = 14;
      AClasses : Array [1..MAX_CLASSES] of TComponentClass = (
                   TLabel, TRadioButton, TButton, TCheckBox, TMenuItem,
                   TGroupBox, TForm, TTabSheet, TPanel, TImage,
                   TListView, TToolButton, TBitBtn, TSpeedButton
                   );

      MAX_PROPERTIES = 4;
      AProperties : Array [1..MAX_PROPERTIES] Of String = (
                      'Caption', 'Hint', 'Picture', 'Glyph'
                    );
      APropKinds  : Array [1..MAX_PROPERTIES] Of TTypeKind = (
                       tkString, tkString, tkClass, tkClass
                    );

var ResManPtr : Pointer;

implementation

uses uMAF_ResourceManager, uMAF_ResourceManager_Helper, dialogs;

{$IFDEF PNGimage}
  {$IFDEF D12+}
    Type TmpPNG = class(TPNGImage);
  {$ELSE}
    Type TmpPNG = class(TPNGObject);
  {$ENDIF}
{$ENDIF}

function __Create_ImageListInfo : PImageListInfo;
begin
  New(Result);
  Result^.ImageList := nil;
  Result^.FpData := TList.Create;
end;

procedure __Free_ImageListInfo(pData : PImageListInfo);
var i : Integer;
begin
  For i := 0 To pData^.FpData.Count - 1 Do
    Dispose(PImageInfo(pData^.FpData.Items[i]));
  pData^.FpData.Free;
  pData^.ImageList := nil;
  pData^.FpData := nil;
  Dispose(pData);
end;

{ TmafResourceClient }

// ********************************* Comments **********************************
// Description : create
// Param (in)  : AOwner=Owner-component (Form?)
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.01.2004
// Last Update : 09.09.2009
// *****************************************************************************
constructor TmafResourceClient.Create(AOwner: TComponent);
begin
  inherited;
  AManagerQueryID := HM_RESOURCEMANAGER_QUERY;
  FManagerType := MT_RESOURCE_MANAGER;
  // since it's easier to remember inside the component, wich resources we
  // requested, we will put the life pointers we got from the ResourceManager
  // directly into our lists and can free them through the Resourcemanager, when
  // the component is to be destroyed
  FpUsedBmpList := TList.Create; // list of our requested bitmaps
  FpNotifyList  := TList.Create; // List of components wich need the language change events, too
  FpComponents  := TList.Create; // list with components to update
  FpImageLists  := TList.Create; // list with TImageList (or/and TPNGImageLists)
  ClientOptions  := [coRegisterSelf];
  FManageOptions := [moCaption, moPicture];
  FpStreamer := TTemplateStreamer.Create;
  FpStreamer.Attributes := 1;
  FpStreamer.StreamVersion := 25;
  FpStreamer.OnStreamReadAttribute := __ReadStreamData;
  FpStreamer.OnStreamWriteAttribute := __WriteStreamData;
  bLocalMode := (csDesigning in ComponentState);   // true in design time
  ResManPtr := nil;
end; // Create

// ********************************* Comments **********************************
// Description : destroys the ResClient
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.01.2004
// Last Update : 10.09.2009
// *****************************************************************************
destructor TmafResourceClient.Destroy;
var i : Integer;
begin
  For i := 0 To FpComponents.Count - 1 do
    Dispose(PComponentInfo(FpComponents.Items[i]));
  FpComponents.Free;
  For i := 0 To FpImageLists.Count - 1 do
    __Free_ImageListInfo(PImageListInfo(FpImageLists.Items[i]));
  FpImageLists.Free;
  __DisconnectFromManager; // remove ourself from our Manager and free up the resources allocated by us
  FpUsedBmpList.Free;
  FpNotifyList.Free;
  FpNotifyList := nil;
  FreeAndNil(FpStreamer);
  inherited;
end; // Destroy

procedure TmafResourceClient.Loaded;
begin
  inherited;
end;

procedure TmafResourceClient.AddNotifyComponent(AComponent: TComponent);
begin
  If FpNotifyList.IndexOf(AComponent) = -1 Then
    FpNotifyList.Add(AComponent);
end; // AddNotifyComponent

function TmafResourceClient.__GetUpdateComponentCount: Integer;
begin
  Result := FpComponents.Count;
end;

procedure TmafResourceClient.ReadData(aStream: TStream);
begin
  FpStreamer.ReadStream(TMemoryStream(aStream));
end;

procedure TmafResourceClient.WriteData(aStream: TStream);
begin
  FpStreamer.WriteStream(885, TMemoryStream(aStream));
end;

procedure TmafResourceClient.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('RC_Data', ReadData, WriteData, True);
end;

procedure TmafResourceClient.DeleteNotifyComponent(AComponent: TComponent);
var i : Integer;
begin
  i := FpNotifyList.IndexOf(AComponent);
  If i > -1 Then
    FpNotifyList.Delete(i);
end; // DeleteNotifyComponent

// ********************************* Comments **********************************
// Description : wir werden hier vom ResManager informiert, wenn Sprache gewechselt wird
// Param (in)  : Message=Nachricht, die übertragen wird
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 04.03.2004
// Last Update : 22.11.2009
// *****************************************************************************
procedure TmafResourceClient.WMLanguageChange(var Message: TMessage);
begin
  If Assigned(FBeforeLanguageChange) Then
    FBeforeLanguageChange(Self);
end; // WMLanguageChange

procedure TmafResourceClient.WMLanguageChanged(var Message: TMessage);
begin
  ApplyLanguage; // iterate through the form and update the language
  If Assigned(FAfterLanguageChange) Then
    FAfterLanguageChange(Self);
end;

// ********************************* Comments **********************************
// Description : Disconnect from the ResourceManager
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.02.2007
// Last Update : 28.02.2007
// *****************************************************************************
procedure TmafResourceClient.__DisconnectFromManager;
begin
  If IsLoaded Then begin
    While FpUsedBmpList.Count > 0 Do
      ReleaseGraphic(Cardinal(FpUsedBmpList[0]));
  end;
  inherited;
end; // __DisconnectFromManager

function TmafResourceClient.__IsSupported(AComponent: TComponent): Boolean;
var i : Integer;
begin
  Result := False;
  For i := 1 To MAX_CLASSES Do
    If IsClass(AComponent, AClasses[i]) Then begin
      Result := True;
      Break;
    end;
end;

function TmafResourceClient.__Find_ImageInfo(AList: TList; ResID: Cardinal): PImageInfo;
var i : Integer;
begin
  Result := nil;
  For i := 0 To AList.Count - 1 Do
    If PImageInfo(AList.Items[i])^.ResID = ResID Then begin
      Result := PImageInfo(AList.Items[i]);
      Break;
    end;  //  --  If PImageInfo(AList.Items[i])^.ResID Then
end;

function TmafResourceClient.__Find_ImageListInfo(IL: TCustomImageList): PImageListInfo;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpImageLists.Count - 1 Do
    If PImageListInfo(FpImageLists.Items[i])^.ImageList = IL Then begin
      Result := PImageListInfo(FpImageLists.Items[i]);
      Break;
    end;  //  --  If PImageListInfo(FpImageLists.Items[i])^.ImageList = IL Then 
end;

function TmafResourceClient.__FindComponent(AComponent: TComponent; AName: String): TObject;
var i : Integer;
begin
  Result := AComponent.FindComponent(AName);
  If Result = nil Then
    For i := 0 To AComponent.ComponentCount - 1 Do begin
      Result := __FindComponent(AComponent.Components[i], AName);
      If Result <> nil Then
        Break;
    end;
end;

function TmafResourceClient.__FindSpecialComponent(pData : PComponentInfo): TObject;
var S : String;
    aComp : TObject;
    i : Integer;
begin
  Result := nil;
  Case pData^.ClassID Of
    11 : begin // TListView, so we're looking for TListColumns here
           S := pData^.ComponentName;  // we saved it like "ListView1.Columns[x]"
           Delete(S, Pos('.', S), Length(S) - Pos('.', S) + 1);
           aComp := __FindComponent(Owner, S);
           S := pData^.ComponentName;  // we saved it like "ListView1.Columns[x]"
           Delete(S, 1, Pos('.', S)); // left is "Columns[x]"
           Delete(S, 1, Pos('[', S)); // left is "x]"
           Delete(S, Pos(']', S), 1);     // Left is "x"
           i := StrToIntDef(S, 0);
           If aComp <> nil Then
             pData^.aComp := TListView(aComp).Column[i];
         end;
    12 : begin
           S := pData^.ComponentName;  // we saved it like "ToolButton1.Image"
           Delete(S, Pos('.', S), Length(S) - Pos('.', S) + 1);
           aComp := __FindComponent(Owner, S);
           If aComp <> nil Then
             pData^.aComp := TToolButton(aComp);
         end;
    13 : begin
           S := pData^.ComponentName;  // we saved it like "ToolButton1.Image"
           Delete(S, Pos('.', S), Length(S) - Pos('.', S) + 1);
           aComp := __FindComponent(Owner, S);
           If aComp <> nil Then
             pData^.aComp := TBitBtn(aComp);
         end;
    14 : begin
           S := pData^.ComponentName;  // we saved it like "SpeedButton1.Image"
           Delete(S, Pos('.', S), Length(S) - Pos('.', S) + 1);
           aComp := __FindComponent(Owner, S);
           If aComp <> nil Then
             pData^.aComp := TSpeedButton(aComp);
         end;
  end;
end;

procedure TmafResourceClient.__ReadStreamData(Sender: TObject; ID: Integer);
var i, nCount : Integer;
    pData : PComponentInfo;
begin
  Case ID Of
    1 : begin
          FpStreamer.ReadInteger(nCount);
          For i := 1 To nCount Do begin
            New(pData);
            FpStreamer.ReadCardinal(pData^.ResID);
            FpStreamer.ReadString(pData^.ComponentName);
            FpStreamer.ReadWord(pData^.ClassID);
            pData^.aComp := __FindComponent(Owner, pData^.ComponentName);
            If pData^.aComp = nil Then
              __FindSpecialComponent(pData);
            FpComponents.Add(pData);
          end;
        end;
  end;
end;

procedure TmafResourceClient.__WriteStreamData(Sender: TObject; ID: Integer);
var pData : PComponentInfo;
    i : Integer;
begin
  Case ID Of
    1 : begin
          FpStreamer.WriteInteger(FpComponents.Count);
          For i := 0 To FpComponents.Count - 1 Do begin
            pData := PComponentInfo(FpComponents.Items[i]);
            FpStreamer.WriteCardinal(pData^.ResID);
            FpStreamer.WriteString(pData^.ComponentName);
            FpStreamer.WriteWord(pData^.ClassID);
          end;
        end;
  end;
end;

// ********************************* Comments **********************************
// Description : interne Routine, um die Service-DLL anzusprechen
// Param (in)  : nCommand=Kommando an die DLL; Param=ID der Resource
// Param (out) : Pointer auf Result-Object
// Coding by   : Helge Lange
// Date        : 28.01.2004
// Last Update : 01.10.2009
// *****************************************************************************
function TmafResourceClient.__InternalJump(nCommand: Integer; Param: Cardinal): Pointer;
var RQS: PResQueryStruct;
    QHS : pQHS;
    pDesc: PFileResourceDescriptor;
begin
  Result := nil;
  If ((bLocalMode) And (ResManPtr <> nil)) Then begin
    Case nCommand Of
      RM_GET_GRAPHIC : TGraphic(Result) := TmafResourceManager(ResManPtr).FileResource.Get(Param, '', pDesc);
      RM_GET_STRING  : PChar(Result) := TmafResourceManager(ResManPtr).StringResource.Get(Param);
      RM_GET_SQL     : PChar(Result) := TmafResourceManager(ResManPtr).SQLResource.Get(Param);
    end;
    Exit;
  end;
  RQS := NewResQueryStruct;
  RQS^.nCommand := nCommand;
  RQS^.ResID := Param;
//  GetMem(RQS^.ResName, MAX)
  QHS := __Create_QueryHandlerStruct;
  QHS^.pChildObj := RQS;
  QHS^.Reserved2 := 2;  // indicates that the resource is loaded by its ID
  __AdvQuery_Manager(nCommand, QHS, nil);
//  CallManager(RQS^.nCommand, RQS, nil);
  Result := RQS^.pResult;
  Dispose(RQS);
  __Free_QueryHandlerStruct(QHS);
end; // __InternalJump

// ********************************* Comments **********************************
// Description : retrieves the list of all language libraries from the manager
// Param (in)  : List=valid TStrings
// Param (out) : filename of the currently used language module  
// Coding by   : Helge Lange
// Date        : 06.04.2007
// Last Update : 30.10.2008
// *****************************************************************************
function TmafResourceClient.GetAvailableLanguages(List: TStrings): String;
var QHS : pQHS;
begin
  If List <> nil Then begin
    QHS := __Create_QueryHandlerStruct;
    GetMem(QHS^.pChildObj, MAX_PATH * SizeOf(Char));
    FillChar(QHS^.pChildObj^, MAX_PATH * SizeOf(Char), 0);
    __AdvQuery_Manager(RM_GET_LANGUAGE_LIST, QHS, List);
    Result := String(PChar(QHS^.pChildObj));
    FreeMem(QHS^.pChildObj, MAX_PATH * SizeOf(Char));
    __Free_QueryHandlerStruct(QHS);
  end;  //  --  If List <> nil Then
end; // GetAvailableLanguages

// ********************************* Comments **********************************
// Description : returns the currently used language library
// Param (in)  : N/A
// Param (out) : library name
// Coding by   : Helge Lange
// Date        : 06.04.2007
// Last Update : 02.11.2009
// *****************************************************************************
function TmafResourceClient.GetCurrentLanguageLib: String;
begin
  Result := ExtractFileName(__GetResourceFileName(RM_GET_STRINGRES_NAME));
  Delete(Result, Pos('.', Result), Length(ExtractFileExt(Result)));
end; // GetCurrentLanguageLib

function TmafResourceClient.__GetFileObject(AQueryID, Res2: Integer; nID: Cardinal): Pointer;
var QHS : pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.Reserved1 := nID;
  QHS^.Reserved2 := Res2;
  __AdvQuery_Manager(AQueryID, QHS, nil);
  Result := QHS^.pChildObj;
  __Free_QueryHandlerStruct(QHS);
end;

function TmafResourceClient.__GetResourceFileName(AQueryID: Integer): String;
var QHS : pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  GetMem(QHS^.pChildObj, MAX_PATH * SizeOf(Char));
  FillChar(QHS^.pChildObj^, MAX_PATH * SizeOf(Char), 0);
  __AdvQuery_Manager(AQueryID, QHS, nil);
  Result := String(PChar(QHS^.pChildObj));
  FreeMem(QHS^.pChildObj, MAX_PATH * SizeOf(Char));
  __Free_QueryHandlerStruct(QHS);
end;

function TmafResourceClient.GetCurrentFileResource: String;
begin
  Result := __GetResourceFileName(RM_GET_FILERES_NAME);
end;

function TmafResourceClient.GetCurrentStringResource: String;
begin
  Result := __GetResourceFileName(RM_GET_STRINGRES_NAME);
end;

function TmafResourceClient.GetFileAsStream(nID: Cardinal): TMemoryStream;
begin
  Result := TMemoryStream(__GetFileObject(RM_GET_FILE_STREAM, 1, nID));
end;

function TmafResourceClient.GetCurrentSQLResource: String;
begin
  Result := __GetResourceFileName(RM_GET_SQLRES_NAME);
end;

function TmafResourceClient.GetCurrentIconSkin: String;
begin
  Result := __GetResourceFileName(RM_GET_ICON_SKIN);
end;

procedure TmafResourceClient.SetCurrentIconSkin(AIconSkinName: String);
var QHS : pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  StrToPChar(AIconSkinName, PChar(QHS^.pChildObj));
  __AdvQuery_Manager(RM_SET_ICON_SKIN, QHS, nil);
  FreePChar(PChar(QHS^.pChildObj));
  __Free_QueryHandlerStruct(QHS);
end;

procedure TmafResourceClient.GetAvailableSkins(AList: TStrings);
var QHS : pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  QHS^.pChildObj := AList;
  __AdvQuery_Manager(RM_GET_SKIN_LIST, QHS, nil);
  __Free_QueryHandlerStruct(QHS);
end;

procedure TmafResourceClient.ApplyLanguage;
var i : Integer;
begin
  __ApplyLanguage(Owner);
  For i := 0 To FpNotifyList.Count - 1 Do
    SendComponentMessage(TComponent(FpNotifyList.Items[i]), WM_LANGUAGE_CHANGE, nil, nil, False);
end; // ApplyLanguage

// ********************************* Comments **********************************
// Description : update Strings and images in the form 
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 04.03.2004
// Last Update : 09.09.2009
// *****************************************************************************
procedure TmafResourceClient.__ApplyLanguage(MyOwner: TComponent);
var i, j : Integer;
    AGraphic : TGraphic;
    CI : PComponentInfo;
begin
  For i := 0 To FpComponents.Count - 1 Do begin
    CI := PComponentInfo(FpComponents.Items[i]);
    If CI^.aComp <> nil Then begin
      Case CI^.ClassID Of
         1 : TLabel(CI^.aComp).Caption := GetString(CI^.ResID);
         2 : TRadioButton(CI^.aComp).Caption := GetString(CI^.ResID);
         3 : TButton(CI^.aComp).Caption := GetString(CI^.ResID);
         4 : TCheckBox(CI^.aComp).Caption := GetString(CI^.ResID);
         5 : TMenuItem(CI^.aComp).Caption := GetString(CI^.ResID);
         6 : TGroupBox(CI^.aComp).Caption := GetString(CI^.ResID);
         7 : TForm(CI^.aComp).Caption := GetString(CI^.ResID);
         8 : TTabSheet(CI^.aComp).Caption := GetString(CI^.ResID);
         9 : TPanel(CI^.aComp).Caption := GetString(CI^.ResID);
        10 : begin
               AGraphic := GetGraphic(CI^.ResID);
               If AGraphic <> nil Then begin
                 TImage(CI^.aComp).Picture.Assign(AGraphic);
                 AGraphic.Free;
                 ReleaseGraphic(CI^.ResID);
               end;
             end;
        11 : TListColumn(CI^.aComp).Caption := GetString(CI^.ResID);
        12 : begin
               j := Pos('Image', CI^.ComponentName);
               If Pos('Image', CI^.ComponentName) > 0 Then begin
                 If TToolBar(TToolButton(CI^.aComp).Parent).Images <> nil Then begin
                   TToolButton(CI^.aComp).ImageIndex := GetGraphic(CI^.ResID, TCustomImageList(TToolBar(TToolButton(CI^.aComp).Parent).Images));
                 end;
               end else
                 If Pos('Caption', CI^.ComponentName) > 0 Then begin
                   TToolButton(CI^.aComp).Caption := GetString(CI^.ResID);
                 end else
                   If Pos('Hint', CI^.ComponentName) > 0 Then begin
                     TToolButton(CI^.aComp).Hint := GetString(CI^.ResID);
                   end;
             end;
        13 : begin
               j := Pos('Glyph', CI^.ComponentName);
               If j > 0 Then begin
                 AGraphic := GetGraphic(CI^.ResID);
                 If Assigned(AGraphic) Then begin
                   {$IFDEF PNGComponents}
                   If IsClass(CI^.aComp, TPngBitBtn) Then
                     TPngBitBtn(CI^.aComp).PngImage.Assign(AGraphic)
                   Else
                     TBitBtn(CI^.aComp).Glyph.Assign(AGraphic);
                   {$ELSE}
                   TBitBtn(CI^.aComp).Glyph.Assign(AGraphic);
                   {$ENDIF}
                   AGraphic.Free;
                   ReleaseGraphic(CI^.ResID);
                 end;
               end else
                 If Pos('Caption', CI^.ComponentName) > 0 Then begin
                   TBitBtn(CI^.aComp).Caption := GetString(CI^.ResID);
                 end else
                   If Pos('Hint', CI^.ComponentName) > 0 Then begin
                     TBitBtn(CI^.aComp).Hint := GetString(CI^.ResID);
                   end;
             end;
        14 : begin
               j := Pos('Glyph', CI^.ComponentName);
               If j > 0 Then begin
                 AGraphic := GetGraphic(CI^.ResID);
                 If Assigned(AGraphic) Then begin
                   {$IFDEF PNGComponents}
                   If IsClass(CI^.aComp, TPngSpeedButton) Then
                     TPngSpeedButton(CI^.aComp).PngImage.Assign(AGraphic)
                   Else
                     TSpeedButton(CI^.aComp).Glyph.Assign(AGraphic);
                   {$ELSE}
                   TSpeedButton(CI^.aComp).Glyph.Assign(AGraphic);
                   {$ENDIF}
                   AGraphic.Free;
                   ReleaseGraphic(CI^.ResID);
                 end;
               end else
                 If Pos('Caption', CI^.ComponentName) > 0 Then begin
                   TSpeedButton(CI^.aComp).Caption := GetString(CI^.ResID);
                 end else
                   If Pos('Hint', CI^.ComponentName) > 0 Then begin
                     TSpeedButton(CI^.aComp).Hint := GetString(CI^.ResID);
                   end;
             end;
      end;
    end;
  end;
end; // ApplyLanguage

procedure TmafResourceClient.WMForm_Initialized(var Message: TMessage);
begin
  If Assigned(FBeforeApplyInitialLanguage) Then
    FBeforeApplyInitialLanguage(Self);
  ApplyLanguage;
  If Assigned(FAfterApplyInitialLanguage) Then
    FAfterApplyInitialLanguage(Self);
end; // WMForm_Initialized

procedure TmafResourceClient.WMIconSkinChange(var Message: TMessage);
var i, j : Integer;
    AGraphic : TGraphic;
    pILI : PImageListInfo;
begin
  // updating TImage objects we manage for the user
  For i := 0 To FpComponents.Count - 1 Do
    If PComponentInfo(FpComponents.Items[i])^.ClassID = 10 Then begin // ClassID 10 = TImage
      ReleaseGraphic(PComponentInfo(FpComponents.Items[i])^.ResID);
      AGraphic := GetGraphic(PComponentInfo(FpComponents.Items[i])^.ResID);
      If AGraphic <> nil Then begin
        TImage(PComponentInfo(FpComponents.Items[i])^.aComp).Picture.Assign(AGraphic);
        AGraphic.Free;
      end;  //  --  If AGraphic <> nil Then
    end;  //  --  If PComponentInfo(FpComponents.Items[i])^.ClassID = 10 Then

  // updating TImageList objects we manage for the user
  For i := 0 To FpImageLists.Count - 1 Do begin
    pILI := PImageListInfo(FpImageLists.Items[i]);
    For j := 0 To pILI^.FpData.Count - 1 Do begin
      ReleaseGraphic(PImageInfo(pILI^.FpData.Items[j])^.ResID); // release the currently used graphic
      AGraphic := GetGraphic(PImageInfo(pILI^.FpData.Items[j])^.ResID);
      PImageInfo(pILI^.FpData.Items[j])^.Index := __AddGraphic_ImageList(AGraphic, pILI^.ImageList, PImageInfo(pILI^.FpData.Items[j])^.Index);
      AGraphic.Free;
    end;
  end;

  If Assigned(FOnIconSkinChange) Then
    FOnIconSkinChange(Self);
end;

// ********************************* Comments **********************************
// Description : requests a SQL string resource from the resource manager
// Param (in)  : ID=ID of the SQL resource string
// Param (out) : SQL resource string
// Coding by   : Helge Lange
// Date        : 08.02.2007
// Last Update : 01.10.2009
// *****************************************************************************
function TmafResourceClient.GetSQL(ID: Integer): String;
begin
  Result := String(PChar(__InternalJump(RM_GET_SQL, ID)));
end;

// ********************************* Comments **********************************
// Description : Sets the active language
// Param (in)  : LibName=filename without extension, must be in LanguagePath
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 08.02.2007
// Last Update : 22.11.2009
// *****************************************************************************
procedure TmafResourceClient.SetActiveLanguage(LibName: String);
var QHS : pQHS;
begin
  QHS := __Create_QueryHandlerStruct;
  StrToPChar(LibName, PChar(QHS^.pChildObj));
  __AdvQuery_Manager(RM_SET_ACTIVE_LANGUAGE, QHS, nil);
  FreePChar(PChar(QHS^.pChildObj));
  __Free_QueryHandlerStruct(QHS);
end; // SetLanguageLibrary

// ********************************* Comments **********************************
// Description : gets a TGraphic from ResourceManager
// Param (in)  : ID=ID of the graphic
// Param (out) : TGraphic-Object
// Coding by   : Helge Lange
// Date        : 28.01.2004
// Last Update : 28.10.2009
// *****************************************************************************
function TmafResourceClient.GetGraphic(ID: Cardinal; AImage: TImage = nil): TGraphic;
begin
  Result := __InternalJump(RM_GET_GRAPHIC, ID);
  If Result <> nil Then begin
    FpUsedBmpList.Add(Pointer(ID));
    If AImage <> nil Then begin
      AImage.Picture.Assign(Result);
      Result.Free;
      Result := AImage.Picture.Graphic;
    end;
  end;
end;

function TmafResourceClient.GetGraphic(AName: String): TGraphic;
var RQS: PResQueryStruct;
    QHS : pQHS;
begin
  RQS := NewResQueryStruct;
  RQS^.nCommand := RM_GET_GRAPHIC;
  StrToPChar(AName, RQS^.ResName);
  QHS := __Create_QueryHandlerStruct;
  QHS^.pChildObj := RQS;
  QHS^.Reserved2 := 3;  // indicates that the resource is loaded by its Name and path
  __AdvQuery_Manager(RM_GET_GRAPHIC, QHS, nil);
  Result := RQS^.pResult;
  FreePChar(RQS^.ResName);
  Dispose(RQS);
  __Free_QueryHandlerStruct(QHS);
end;

function TmafResourceClient.GetGraphic(ID: Cardinal; IL: TCustomImageList): Integer;
var AGraphic : TGraphic;
    pListInfo : PImageListInfo;
    pImageI : PImageInfo;
begin
  Result := -1;
  pImageI := nil;
  pListInfo := __Find_ImageListInfo(IL);
  If pListInfo = nil Then begin
    pListInfo := __Create_ImageListInfo;
    pListInfo^.ImageList := IL;
    FpImageLists.Add(pListInfo);
  end Else
    pImageI := __Find_ImageInfo(pListInfo^.FpData, ID);

  If pImageI <> nil Then begin
    Result := pImageI^.Index;
    Exit;
  end;

  AGraphic := GetGraphic(ID);
  If AGraphic = nil Then
    Exit;

  New(pImageI);
  pImageI^.ResID := ID;
  pListInfo^.FpData.Add(pImageI);
  Result := __AddGraphic_ImageList(AGraphic, IL);
  pImageI^.Index := Result;
  AGraphic.Free;
end;

function TmafResourceClient.__AddGraphic_ImageList(AGraphic: TGraphic; AList: TCustomImageList; Idx: Integer = -1): Integer;
var ABitmap : TBitmap;
    {$IFDEF PNGComponents}
    Png: TPngImageCollectionItem;
    {$ENDIF}
begin
  Result := -1;
  {$IFDEF PNGComponents}
  If IsClass(AList, TPNGImageList) Then begin
    TPNGImageList(AList).BeginUpdate;
    If Idx = -1 Then begin
      Png := TPNGImageList(AList).PngImages.Add;
      idx := Png.Index;
    end;
    PNG := TPNGImageList(AList).PngImages.Items[idx];
    Png.PngImage.Assign(AGraphic);
    Result := PNG.Index;
    TPNGImageList(AList).EndUpdate(True);
  end else {$ENDIF} begin
    ABitmap := TBitmap.Create;
    ABitmap.Assign(AGraphic);
    If idx = -1 Then
      Result := AList.AddMasked(ABitmap, ABitmap.TransparentColor)
    Else
      AList.ReplaceMasked(idx, ABitmap, ABitmap.TransparentColor);
    ABitmap.Free;
  end;
end;

// ********************************* Comments **********************************
// Description : releases a TGraphic in the ResourceManager
// Param (in)  : ID=ID of the graphic
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.01.2004
// Last Update : 29.08.2009
// *****************************************************************************
procedure TmafResourceClient.ReleaseGraphic(ID: Cardinal);
var idx : Integer;
begin
  __InternalJump(RM_RELEASE_GRAPHIC, ID);
  idx := FpUsedBmpList.IndexOf(Pointer(ID));
  If idx > -1 Then
    FpUsedBmpList.Delete(idx);
end;

// ********************************* Comments **********************************
// Description : gets a String from the ResourceManager
// Param (in)  : ID=String ID
// Param (out) : the String
// Coding by   : Helge Lange
// Date        : 04.03.2004
// Last Update : 29.08.2009
// *****************************************************************************
function TmafResourceClient.GetString(ID: Cardinal): String;
begin
  Result := String(PChar(__InternalJump(RM_GET_STRING, ID)));
end;

end.
