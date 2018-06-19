unit uMAF_PropertyEditors;

interface

uses Windows, Messages, SysUtils, Classes, DesignIntf, DesignEditors,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     // Modular Application Framework Components units
     frmHookManager_Editor, uMAF_HookManager, frmConfiguration, uMAF_ResourceManager,
     uMAF_ResourceClient, uMAF_FileDB, uMAF_UserSecurity, uMAF_CustomBaseDB,
     uMAF_HookCLient,

     frmLinkData_Editor, uMAF_LinkClient, frmMAFFileLogger_Editor, uMAF_Logger,
     uMAF_ModuleController, frmModuleController_Editor, frmResourceManager_Editor,
     frmResourceClient_Editor, frmFileDB_UserEditor, frmFileDB_Viewer,
     frmCreateDevAccount{, frmParameter_Edit};

Type THookDataEditor = class(TComponentEditor)
     public
       function GetVerb(Index: Integer): string; override;
       function GetVerbCount: Integer; override;
       procedure ExecuteVerb(Index: Integer); override;
     end;

{Type THookClientEditor = class(TComponentEditor)
     public
       function GetVerb(Index: Integer): string; override;
       function GetVerbCount: Integer; override;
       procedure ExecuteVerb(Index: Integer); override;
     end;}

Type TLinkDataEditor = class(TComponentEditor)
     public
       function GetVerb(Index: Integer): string; override;
       function GetVerbCount: Integer; override;
       procedure ExecuteVerb(Index: Integer); override;
     end;

Type TFileLoggerEditor = class(TComponentEditor)
     public
       function GetVerb(Index: Integer): string; override;
       function GetVerbCount: Integer; override;
       procedure ExecuteVerb(Index: Integer); override;
     end;

Type TModuleControllerEditor = class(TComponentEditor)
     public
       function GetVerb(Index: Integer): string; override;
       function GetVerbCount: Integer; override;
       procedure ExecuteVerb(Index: Integer); override;
     end;

Type TResourceManagerEditor = class(TComponentEditor)
     public
       function GetVerb(Index: Integer): string; override;
       function GetVerbCount: Integer; override;
       procedure ExecuteVerb(Index: Integer); override;
     end;

Type TResClientEditor = class(TComponentEditor)
     public
       function GetVerb(Index: Integer): string; override;
       function GetVerbCount: Integer; override;
       procedure ExecuteVerb(Index: Integer); override;
     end;

Type TFileDBEditor = class(TComponentEditor)
     public
       function GetVerb(Index: Integer): string; override;
       function GetVerbCount: Integer; override;
       procedure ExecuteVerb(Index: Integer); override;
     end;

     TUserSecurityEditor = class(TComponentEditor)
     public
       function GetVerb(Index: Integer): string; override;
       function GetVerbCount: Integer; override;
       procedure ExecuteVerb(Index: Integer); override;
     end;

     TmafProviderNameEditor = class (TStringProperty)
     public
       function GetAttributes: TPropertyAttributes; override;
       procedure GetValues(Proc: TGetStrProc); override;
     end;

procedure Register;

implementation

uses xmldom, XMLIntf, XMLDoc, msxmldom, uXMLSynchro;

procedure Register;
begin
  // causes Delphi to always load the package when it starts to register
  // menu item and the SplashScreen message
  {$IFDEF D9+}
  ForceDemandLoadState(dlDisable);
  {$ENDIF}
  RegisterComponentEditor(TmafHookManager, THookDataEditor);
//  RegisterComponentEditor(TmafHookClient, THookClientEditor);
  RegisterComponentEditor(TmafLinkClient, TLinkDataEditor);
  RegisterComponentEditor(TmafFileLogger, TFileLoggerEditor);
  RegisterComponentEditor(TmafModuleController, TModuleControllerEditor);
  RegisterComponentEditor(TmafResourceManager, TResourceManagerEditor);
  RegisterComponentEditor(TmafResourceClient, TResClientEditor);
  RegisterComponentEditor(TmafFileDB, TFileDBEditor);
  RegisterComponentEditor(TmafUserSecurity, TUserSecurityEditor);
  RegisterPropertyEditor(TypeInfo(String), TmafCustomBaseDB2, 'ProviderName', TmafProviderNameEditor);
end;

{ THookDataEditor }

function THookDataEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit DynamicFunctionTable';
end;

function THookDataEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure THookDataEditor.ExecuteVerb(Index: Integer);
var frm : TfDFTEdit;
    bOldConnected : Boolean;
    bOldUseInstallHistory : Boolean;
begin
  If TmafHookManager(Component).BaseDB = nil Then begin
    MessageDlg('No BaseDB connected. Without it, no data can be read or written', mtError, [mbok], 0);
    Exit;
  end;

  If Not TmafHookManager(Component).BaseDB.Connected Then begin
    MessageDlg('The BaseDB is not connected. Without a connection, no data can be read or written', mtError, [mbok], 0);
    Exit;
  end;

  bOldConnected := TmafHookManager(Component).Connected;
  bOldUseInstallHistory := TmafHookManager(Component).DynamicFuntionTable.UseInstallHistory;
  TmafHookManager(Component).DynamicFuntionTable.UseInstallHistory := False;
  // first we should be able to load the functions
  TmafHookManager(Component).DynamicFuntionTable.LockDown := True;
  If not TmafHookManager(Component).Connected Then
    TmafHookManager(Component).Connected := True;
  If not TmafHookManager(Component).Connected Then begin
    TmafHookManager(Component).DynamicFuntionTable.LockDown := False;
    MessageDlg('Cannot load dynamic function, check if database object is connected and active !', mtError, [mbOk], 0);
    Exit;
  end;

  frm := TfDFTEdit.Create(Application);
  frm.HM := TmafHookManager(Component);

  try
    frm.ShowModal;
    If FpProjectSettings.EnableXMLSynchro Then begin
      frm.SD.FileName := FpProjectSettings.XMLName;
      frm.DoXML_Export;
    end;
  finally
    frm.Free;
  end;
  TmafHookManager(Component).DynamicFuntionTable.LockDown := False;
  TmafHookManager(Component).DynamicFuntionTable.UseInstallHistory := bOldUseInstallHistory;
  TmafHookManager(Component).Connected := bOldConnected;
end;

{ TLinkDataEditor }

procedure TLinkDataEditor.ExecuteVerb(Index: Integer);
var frm : TfLinkDataEditor;
begin
  inherited;
  frm := TfLinkDataEditor.Create(Application);
  frm.LinkClient := TmafLinkClient(Component);
  try
    frm.ShowModal;
  finally
    FreeAndNil(Frm);
  end;
end;

function TLinkDataEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit links';
end;

function TLinkDataEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TFileLoggerEditor }

procedure TFileLoggerEditor.ExecuteVerb(Index: Integer);
var frm : TfFileLoggerEditor;
begin
  inherited;
  frm := TfFileLoggerEditor.Create(Application);
  Try
    frm.sOldFileName := TmafFileLogger(Component).MacroFileName;
    frm.ShowModal;
    If frm.bModified Then begin
      TmafFileLogger(Component).MacroFileName := frm.sOldFileName;
      Designer.Modified;
    end;
  Finally
    frm.Free;
  End;
end;

function TFileLoggerEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit filename';
end;

function TFileLoggerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TModuleControllerEditor }

procedure TModuleControllerEditor.ExecuteVerb(Index: Integer);
var frm : TfModuleController_Editor;
begin
  inherited;
  frm := TfModuleController_Editor.Create(Application);
  Try
    frm.pMC := TmafModuleController(Component);
    frm.ShowModal;
    If frm.bModified Then begin
      Designer.Modified;
    end;
  Finally
    frm.Free;
  End;
end;

function TModuleControllerEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit ModuleController Data';
end;

function TModuleControllerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TResourceManagerEditor }

procedure TResourceManagerEditor.ExecuteVerb(Index: Integer);
var frm : TfRMT_Main;
    ModuleRoot, ModuleNode : IXMLNode;
    i, j : Integer;
begin
  inherited;
  Case Index Of
    0 : begin
          frm := TfRMT_Main.Create(Application);
          frm.RM := TmafResourceManager(Component);
          try
            frm.ShowModal;
          finally
            frm.Free;
          end;
        end;
    1 : begin
          fConfiguration := TfConfiguration.Create(Application);
          fConfiguration.XML.LoadFromFile(FpProjectSettings.XMLName);
          fConfiguration.XML.Active := True;
          ModuleRoot := fConfiguration.XML.DocumentElement.ChildNodes.FindNode('Modules');
          If ModuleRoot <> nil Then begin
            For i := 0 To ModuleRoot.ChildNodes.Count - 1 Do begin
              ModuleNode := ModuleRoot.ChildNodes.Nodes[i];
              j := ModuleNode.Attributes['ID'];
              If TmafResourceManager(Component).FileResource.Loaded Then
                TmafResourceManager(Component).FileResource.RegisterModule(ModuleNode.Attributes['Name'], j);
              If TmafResourceManager(Component).StringResource.Loaded Then
                TmafResourceManager(Component).StringResource.RegisterModule(ModuleNode.Attributes['Name'], j);
              If TmafResourceManager(Component).SQLResource.Loaded Then
                TmafResourceManager(Component).SQLResource.RegisterModule(ModuleNode.Attributes['Name'], j);
            end;
          end;
          fConfiguration.Free;
          If TmafResourceManager(Component).FileResource.Loaded Then
            TmafResourceManager(Component).FileResource.Save;
          If TmafResourceManager(Component).StringResource.Loaded Then
            TmafResourceManager(Component).StringResource.Save;
          If TmafResourceManager(Component).SQLResource.Loaded Then
            TmafResourceManager(Component).SQLResource.Save;
        end;
  end;
end;

function TResourceManagerEditor.GetVerb(Index: Integer): string;
begin
  Case Index Of
    0 : Result := 'Edit Resource Data';
    1 : Result := 'Load Module Names';
  end;
end;

function TResourceManagerEditor.GetVerbCount: Integer;
begin
  If (((TmafResourceManager(Component).FileResource.Loaded) Or (TmafResourceManager(Component).StringResource.Loaded) Or (TmafResourceManager(Component).SQLResource.Loaded)) And (FpProjectSettings.EnableXMLSynchro)) Then
    Result := 2
  Else
    Result := 1;
end;

{ TResClientEditor }

procedure TResClientEditor.ExecuteVerb(Index: Integer);
var frm : TfResourceClient_Editor;
    RM : TmafResourceManager;
begin
  inherited;
  Case Index Of
    0 : begin
          frm := TfResourceClient_Editor.Create(Application);
          frm.RC := TmafResourceClient(Component);
          try
            frm.ShowModal;
            If frm.Modified Then
              Designer.Modified;
          finally
            frm.Free;
          end;
        end;
    1 : begin
          RM := TmafResourceManager.Create(nil);
          RM.FileResource.ResourceFile := FpProjectSettings.FileResName;
          RM.StringResource.ResourceFile := FpProjectSettings.StringResName;
          ResManPtr := Pointer(RM);
          TmafResourceClient(Component).ApplyLanguage;
          RM.Free;
        end;
  end;
end;

function TResClientEditor.GetVerb(Index: Integer): string;
begin
  Case Index Of
    0 : Result := 'Edit ResClient Data';
    1 : Result := 'Apply Rersources';
  end;
end;

function TResClientEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TFileDBEditor }

procedure TFileDBEditor.ExecuteVerb(Index: Integer);
var frm : TfFileDB_Viewer;
begin
  inherited;
  frm := TfFileDB_Viewer.Create(nil);
  Try
    frm.FpFileDB := TmafFileDB(Component);
    frm.ShowModal;
  Finally
    frm.Free;
  End;
end;

function TFileDBEditor.GetVerb(Index: Integer): string;
begin
  Result := 'View FileDB';
end;

function TFileDBEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TUserSecurityEditor }

procedure TUserSecurityEditor.ExecuteVerb(Index: Integer);
var frm : TfCreateDevAccount;
begin
  frm := TfCreateDevAccount.Create(nil);
  frm.aUS := TmafUserSecurity(Component);
  try
    If frm.ShowModal = mrOk Then begin

    end;
  finally
    frm.Free;
  end;
end;

function TUserSecurityEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Create Developer Account';
end;

function TUserSecurityEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TmafProviderNameEditor }

function TmafProviderNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TmafProviderNameEditor.GetValues(Proc: TGetStrProc);
var List : TStringList;
    i : integer;
begin
  List := TStringList.Create;
  try
    mafProviders.GetProviderNames(List);
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ THookClientEditor }

{procedure THookClientEditor.ExecuteVerb(Index: Integer);
var frm: TfParameter_Edit;
begin
  inherited;
  frm := TfParameter_Edit.Create(nil);
  frm.AComponent := TmafHookClient(Component).Parameter;
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

function THookClientEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit Parameter';
end;

function THookClientEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;           }

end.
