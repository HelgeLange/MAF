{*******************************************************************************
Name         : frmConfiguration.pas
Coding by    : Helge Lange
Copyright by : Helge Lange
Date         : 21.07.2009
Last Update  : 03.12.2009
Version      : 1.0.000
Purpose      : - controls a menu in the Delphi main menu for the Modular
                 Application Framework Components
               - contains the configuration dialog for global settings
Last Changes :

1.0.000 (21.07.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit frmConfiguration;

interface

{$I ..\MAF_Base\MAFramework.inc}

{$IFDEF D9+}
  {$R ..\MAF_SplashIcon.res}
{$ENDIF}

uses Windows, Messages, SysUtils, Variants, Classes, ShellApi, DesignIntf, DesignEditors, ToolsAPI,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.Menus,
     System.IniFiles, Xml.Win.msxmldom, Xml.XMLDoc, Vcl.ComCtrls,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs, Menus, IniFiles, msxmldom, XMLDoc, ComCtrls,
     {$ENDIF}
     {$IFDEF CODESITE} CodeSiteLogging, {$ENDIF}
     xmldom, XMLIntf,
  // Modular Application Framework Components units
  {$IFDEF Tracer} uMAF_Tracer, frmTracerOptions, {$ENDIF}
  uXMLSynchro, uMAF_Core, uMAF_ResourceManager, uMAF_ResourceManager_Helper;

type
  TPNGProc = procedure (aStream: TMemoryStream; var AGraphic: TGraphic);

  TProjectSettings = class
  private
    FpIniFile : TIniFile;
    function __GetXMLSynchro: Boolean;
    procedure __SetXMLSynchro(const Value: Boolean);
    function __GetProjectINI: String;
    function __GetIniValue(sSection, sIdent: String): String;
    procedure __SetIniValue(sSection, sIdent, Value: String);

    function GetActiveProjectGroup: IOTAProjectGroup;
    function __GetXMLName: String;
    function __GetFileResName: String;
    procedure __SetFileResName(const Value: String);
    function __GetSQLResName: String;
    function __GetStringResName: String;
    procedure __SetSQLResName(const Value: String);
    procedure __SetStringResName(const Value: String);
    function __GetEnableTracer: Boolean;
    procedure __SetEnableTracer(const Value: Boolean);
    function __GetTracerLogfile: String;
    procedure __SetTracerLogfile(const Value: String);
    function __GetTraceHookManager: Boolean;
    procedure __SetTraceHookManager(const Value: Boolean);
  protected
//    hPNGLib : THandle;
//    PNGProc : TPNGProc;
  public
    constructor Create;
    destructor Destroy; override;
    function __GetPNG(aStream: TMemoryStream): TGraphic;
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnMenuItemUpdate(Sender: TObject);
    function __ShowResourceEditor(nID : Cardinal; AType: TResourceFileType): Cardinal;
    property EnableXMLSynchro : Boolean read __GetXMLSynchro write __SetXMLSynchro;
    property XMLName : String read __GetXMLName;
    property FileResName : String read __GetFileResName write __SetFileResName;
    property StringResName : String read __GetStringResName write __SetStringResName;
    property SQLResName : String read __GetSQLResName write __SetSQLResName;
    property EnableTracer : Boolean read __GetEnableTracer write __SetEnableTracer;
    property TracerLogFile : String read __GetTracerLogfile write __SetTracerLogfile;
    property Trace_HookManager : Boolean read __GetTraceHookManager write __SetTraceHookManager;
  end;

  TfConfiguration = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    XML: TXMLDocument;
    OD: TOpenDialog;
    PageControl1: TPageControl;
    tsSynchro: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edFileRes: TEdit;
    btnSearchFileRes: TButton;
    edStringRes: TEdit;
    btnSearchStringRes: TButton;
    edSQLRes: TEdit;
    btnSearchSQLRes: TButton;
    gbSettings: TGroupBox;
    cbSynchro: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSearchFileResClick(Sender: TObject);
  private
    { Private declarations }
    nOldHintPause : Integer;
  public
    { Public declarations }
  end;

var FpProjectSettings : TProjectSettings;
    fConfiguration: TfConfiguration;

implementation

{$R *.dfm}

uses {$IFDEF VER230}
     VCL.ActnList,
     {$ELSE}
     ActnList,
     {$ENDIF}
     frmResourceManager_Editor, uMAF_ResourceClient, uMAF_Globals;

var s_cbSynchroHint : String = 'Both TmafHookManager and TmafModuleController use a XML file' + #13#10 +
                               'to keep themselfs up-to-date between eachother. If a new' + #13#10 +
                               'Hook/SubHook is added in the HookManager-Component-Editor,' + #13#10 +
                               'this Hook will be added in the ModuleController next time' + #13#10 +
                               'you open its Component-Editor.';

procedure ReadMAFValues;
begin
  {$IFDEF Tracer}
    MAFTracer.Enabled := FpProjectSettings.EnableTracer;
    MAFTracer.LogFileName := FpProjectSettings.TracerLogFile;
  {$ENDIF}
end;

var {$IFNDEF D9+} miMAFFrameworkRoot, {$ENDIF} aMAFMenu : TMenuItem;
    aMenuAction : TAction;

procedure RegisterMAF_Config;
var {$IFNDEF D9+} MM : TMainMenu; {$ENDIF}
    aToolsMenu : TMenuItem;
    MenuService: INTAServices;
    i : Integer;

function AddMenuItem(Root: TMenuItem; const aCaption: string; anOnClick: TNotifyEvent; aTag: Integer): TMenuItem;
  begin
    Result := NewItem(aCaption, 0, False, True, anOnClick, 0, '');
    Result.Tag := aTag;
    Root.Add(Result);
  end;

begin
  {$IFDEF D9+}
  {$IFDEF CodeSite} CodeSite.Send(csmLevel1, 'Using menu service', 'D9+'); {$ENDIF}
  MenuService := BorlandIDEServices as INTAServices;
  If Assigned(MenuService) Then begin
    {$IFDEF CodeSite} CodeSite.Send(csmLevel4, 'Found menu service', True); {$ENDIF}
    aToolsMenu := nil;
    For i := 0 To MenuService.MainMenu.Items.Count - 1 Do begin
      If MenuService.MainMenu.Items[i].Name = 'ToolsMenu' Then
        aToolsMenu := MenuService.MainMenu.Items[i];
    end;
    If aToolsMenu <> nil Then begin
      aMAFMenu := TMenuItem.Create(nil);
      aMenuAction := TAction.Create(nil);
      aMAFMenu.Name := 'miMAFConfiguration1';
      aMAFMenu.Tag := 1;
      aMAFMenu.Action := aMenuAction;
      aMenuAction.Caption := 'MAF Configuration';
      aMenuAction.Tag := 1;
      aMenuAction.OnExecute := FpProjectSettings.OnMenuItemClick;
      aMenuAction.OnUpdate := FpProjectSettings.OnMenuItemUpdate;
      aMenuAction.Enabled := True;
      aMenuAction.HelpContext := 0;
      MenuService.AddActionMenu('ToolsToolsItem', aMenuAction, aMAFMenu);
      MenuService.UpdateMenuAccelerators(MenuService.MainMenu);
    end;
  end;
  {$ELSE}
  {$IFDEF CodeSite} CodeSite.Send(csmLevel1, 'Using menu service', 'before D9+'); {$ENDIF}
  If Application.MainForm <> nil Then begin
    MM := TMainMenu(Application.MainForm.FindComponent('MainMenu1'));

    If Assigned(MM) Then begin
      miMAFFrameworkRoot := NewItem('MAF', 0, False, True, nil, 0, 'miMAFrameworkRoot');
      MM.Items.Insert(8, miMAFFrameworkRoot);
      aMAFMenu := AddMenuItem(miMAFFrameworkRoot, 'MAF Configuration', FpProjectSettings.OnMenuItemClick, 1);
    end;
  end;
  {$ENDIF}
end;

procedure UnRegisterMAF_Config;
begin
    aMAFMenu.Free;
  {$IFDEF D9+}
    aMenuAction.Free;
  {$ELSE}
    miMAFFrameworkRoot.Free;
  {$ENDIF}
end;

procedure TfConfiguration.btnSaveClick(Sender: TObject);
begin
  FpProjectSettings.EnableXMLSynchro := cbSynchro.Checked;
  FpProjectSettings.FileResName := edFileRes.Text;
  FpProjectSettings.StringResName := edStringRes.Text;
  FpProjectSettings.SQLResName := edSQLRes.Text;
  {$IFDEF Tracer}
    FpProjectSettings.EnableTracer := fTracerOptions.cbUseTracer.Checked;
    MAFTracer.Enabled := fTracerOptions.cbUseTracer.Checked;
    FpProjectSettings.TracerLogFile := fTracerOptions.edMAFLogfile.Text;
    FpProjectSettings.Trace_HookManager := fTracerOptions.cbHookManager.Checked;
  {$ENDIF}
end;

procedure TfConfiguration.btnSearchFileResClick(Sender: TObject);
var aEdit: TEdit;
begin
  Case TButton(Sender).Tag Of
    1 : aEdit := edFileRes;
    2 : aEdit := edStringRes;
    3 : aEdit := edSQLRes;
    Else aEdit := nil;
  end;
  If OD.Execute{$IFDEF D9+}(Handle){$ENDIF} Then
    If Assigned(aEdit) Then
      aEdit.Text := OD.FileName;
end;

procedure TfConfiguration.FormCreate(Sender: TObject);
begin
  nOldHintPause := Application.HintHidePause;
  Application.HintHidePause := nOldHintPause * 5;
  cbSynchro.Hint := s_cbSynchroHint;
end;

procedure TfConfiguration.FormDestroy(Sender: TObject);
begin
  Application.HintHidePause := nOldHintPause;
end;

procedure TfConfiguration.FormShow(Sender: TObject);
var ActiveProjectGroup: IOTAProjectGroup;
//    aTabSheet : TTabSheet;
begin
  {$IFDEF Tracer}
    aTabSheet := TTabSheet.Create(PageControl1);
    aTabSheet.Caption := 'Tracer Options';
    aTabSheet.PageControl := PageControl1;
    fTracerOptions := TfTracerOptions.Create(Self);
    fTracerOptions.Parent := aTabSheet;
    fTracerOptions.ParentWindow := aTabSheet.Handle;
    fTracerOptions.BorderStyle := bsNone;
    fTracerOptions.Align := alClient;
    fTracerOptions.Visible := True;
  {$ENDIF}

  ActiveProjectGroup := FpProjectSettings.GetActiveProjectGroup;
  If ActiveProjectGroup = nil Then begin
    edFileRes.Enabled := False;
    edStringRes.Enabled := False;
    edSQLRes.Enabled := False;
    btnSave.Enabled := False;
    btnSearchFileRes.Enabled := False;
    btnSearchStringRes.Enabled := False;
    btnSearchSQLRes.Enabled := False;
    cbSynchro.Enabled := False;
    {$IFDEF Tracer}
      fTracerOptions.cbUseTracer.Checked := False;
      fTracerOptions.edMAFLogfile.Text := '';
      fTracerOptions.cbHookManager.Checked := False;
    {$ENDIF}
  end else begin
    cbSynchro.Checked := FpProjectSettings.EnableXMLSynchro;
    edFileRes.Text := FpProjectSettings.FileResName;
    edStringRes.Text := FpProjectSettings.StringResName;
    edSQLRes.Text := FpProjectSettings.SQLResName;
    OD.InitialDir := ExtractFilePath(ActiveProjectGroup.FileName);
    {$IFDEF Tracer}
      fTracerOptions.cbUseTracer.Checked := FpProjectSettings.EnableTracer;
      fTracerOptions.edMAFLogfile.Text := FpProjectSettings.TracerLogFile;
      fTracerOptions.cbHookManager.Checked := FpProjectSettings.Trace_HookManager;
    {$ENDIF}
  end;
end;

{ TProjectSettings }

constructor TProjectSettings.Create;
begin

end;

destructor TProjectSettings.Destroy;
begin
{  If hPNGLib <> 0 Then
    FreeLibrary(hPNGLib);
  @PNGProc := nil; }
  inherited;
end;

function TProjectSettings.GetActiveProjectGroup: IOTAProjectGroup;
{$IFNDEF D9+}
var ModServices: IOTAModuleServices;
    Module: IOTAModule;
    ProjectGroup: IOTAProjectGroup;
    i : Integer;
{$ENDIF}
begin
  {$IFDEF D9+}
  Result := IOTAModuleServices(BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
  {$ELSE}
  Result := nil;
  ModServices := BorlandIDEServices as IOTAModuleServices;
  For i := 0 to ModServices.ModuleCount - 1 Do begin
    Module := ModServices.Modules[i];
    If Supports(Module, IOTAProjectGroup, ProjectGroup) Then
      Result := ProjectGroup;
  end;
  {$ENDIF}
end;

procedure TProjectSettings.OnMenuItemClick(Sender: TObject);
var EditFrm : TfConfiguration;
begin
  Case TComponent(Sender).Tag Of
    1 : begin
          EditFrm := TfConfiguration.Create(Application);
          Try
            EditFrm.ShowModal;
          Finally
            EditFrm.Free;
          End;
        end;
  end;
end;

procedure TProjectSettings.OnMenuItemUpdate(Sender: TObject);
begin
  aMAFMenu.Enabled := (GetActiveProjectGroup <> nil);
  {$IFDEF CODESITE} CodeSite.Send(csmLevel4, 'MAF Menu Enabled', aMAFMenu.Enabled); {$ENDIF}
end;

function TProjectSettings.__GetIniValue(sSection, sIdent: String): String;
var S : String;
begin
  Result := '';
  S := __GetProjectINI;
  If S = '' Then
    Exit;
  FpIniFile := TIniFile.Create(S);
  Result := FpIniFile.ReadString(sSection, sIdent, '');
  FreeAndNil(FpIniFile);
end;

procedure TProjectSettings.__SetIniValue(sSection, sIdent, Value: String);
var S : String;
begin
  S := __GetProjectINI;
  If S = '' Then
    Exit;
  FpIniFile := TIniFile.Create(S);
  FpIniFile.WriteString(sSection, sIdent, Value);
  FreeAndNil(FpIniFile);
end;

function TProjectSettings.__GetPNG(aStream: TMemoryStream): TGraphic;
begin
  Result := nil;
{  If hPNGLib = 0 Then begin
    hPNGLib := LoadLibrary('D:\Delphi\MAFramework\MAF_Base\PNGLoader.dll');
    If hPNGLib <> 0 Then
      @PNGProc := GetProcAddress(hPNGLib, PChar('__Create_PNG'))
    else
      @PNGProc := nil;
  end;
  If Assigned(PNGProc) Then
    PNGProc(aStream, Result); }
end;

function TProjectSettings.__GetProjectINI: String;
var ActiveProjectGroup: IOTAProjectGroup;
    S : String;
begin
  S := '';
  ActiveProjectGroup := GetActiveProjectGroup;
  If ActiveProjectGroup = nil Then
    Exit;

  S := ExtractFileName(ActiveProjectGroup.FileName);
  If S = '' Then
    Exit;

  Delete(S, Pos(ExtractFileExt(S), S), Length(ExtractFileExt(S)));
  S := ExtractFilePath(ActiveProjectGroup.FileName) + S + '_maf.ini';
  Result := S;
end;

function TProjectSettings.__GetXMLName: String;
var ActiveProjectGroup: IOTAProjectGroup;
    S : String;
begin
  S := '';
  ActiveProjectGroup := GetActiveProjectGroup;
  S := ExtractFileName(ActiveProjectGroup.FileName);
  If S = '' Then
    Exit;

  Delete(S, Pos(ExtractFileExt(S), S), Length(ExtractFileExt(S)));
  S := ExtractFilePath(ActiveProjectGroup.FileName) + S + '_DFT.XML';
  Result := S;
end;

function TProjectSettings.__GetXMLSynchro: Boolean;
begin
  Result := (__GetIniValue('GLOBAL', 'XML_Synchro') = 'On');
end;

procedure TProjectSettings.__SetXMLSynchro(const Value: Boolean);
begin
  If Value Then __SetIniValue('GLOBAL', 'XML_Synchro', 'On')
           Else __SetIniValue('GLOBAL', 'XML_Synchro', 'Off');
end;

function TProjectSettings.__GetEnableTracer: Boolean;
begin
  Result := (__GetIniValue('DEBUG', 'Tracer') = 'On');
end;

function TProjectSettings.__GetFileResName: String;
begin
  Result := __GetIniValue('GLOBAL', 'FileResName');
end;

procedure TProjectSettings.__SetEnableTracer(const Value: Boolean);
begin
  If Value Then  __SetIniValue('DEBUG', 'Tracer', 'On')
           Else  __SetIniValue('DEBUG', 'Tracer', 'Off');
end;

procedure TProjectSettings.__SetFileResName(const Value: String);
begin
  __SetIniValue('GLOBAL', 'FileResName', Value);
end;

procedure TProjectSettings.__SetSQLResName(const Value: String);
begin
  __SetIniValue('GLOBAL', 'SQLResName', Value);
end;

procedure TProjectSettings.__SetStringResName(const Value: String);
begin
  __SetIniValue('GLOBAL', 'StringResName', Value);
end;

procedure TProjectSettings.__SetTraceHookManager(const Value: Boolean);
begin
  If Value Then  __SetIniValue('DEBUG', 'Trace_HookManager', 'On')
           Else  __SetIniValue('DEBUG', 'Trace_HookManager', 'Off');
end;

procedure TProjectSettings.__SetTracerLogfile(const Value: String);
begin
  __SetIniValue('DEBUG', 'TracerLogFile', Value);
end;

function TProjectSettings.__GetSQLResName: String;
begin
  Result := __GetIniValue('GLOBAL', 'SQLResName');
end;

function TProjectSettings.__GetStringResName: String;
begin
  Result := __GetIniValue('GLOBAL', 'StringResName');
end;

function TProjectSettings.__GetTraceHookManager: Boolean;
begin
  Result := (__GetIniValue('DEBUG', 'Trace_HookManager') = 'On');
end;

function TProjectSettings.__GetTracerLogfile: String;
begin
  Result := __GetIniValue('DEBUG', 'TracerLogFile');
end;

function TProjectSettings.__ShowResourceEditor(nID : Cardinal; AType: TResourceFileType): Cardinal;
var frm: TfRMT_Main;
    FpResClient : TmafResourceClient;
begin
  frm := TfRMT_Main.Create(Application);
  frm.RM := TmafResourceManager.Create(frm);
  FpResClient := nil;
  If bRunMode Then
    FpResClient := TmafResourceClient.Create(nil);
  Case AType Of
    rftUnknown : begin
                   If bRunMode Then begin
                     frm.RM.FileResource.ResourceFile := FpResClient.GetCurrentFileResource;
                     frm.RM.StringResource.ResourceFile := FpResClient.GetCurrentStringResource;
                     frm.RM.SQLResource.ResourceFile := FpResClient.GetCurrentSQLResource;
                   end else begin
                     frm.RM.FileResource.ResourceFile := FileResName;
                     frm.RM.StringResource.ResourceFile := StringResName;
                     frm.RM.SQLResource.ResourceFile := SQLResName;
                   end;
                   frm.panFileRes.Visible := True;
                   frm.panStringRes.Visible := True;
                   frm.panSQLRes.Visible := True;
                 end;
    rftMedia   : begin
                   If bRunMode Then begin
                     frm.RM.FileResource.ResourceFile := FpResClient.GetCurrentFileResource;
                   end else begin
                     frm.RM.FileResource.ResourceFile := FileResName;
                   end;
                   frm.PageControl1.ActivePage := frm.tsFileResource;
                   frm.panFileRes.Visible := True;
                   frm.tsStrings.Visible := False;
                   frm.tsSQL.Visible := False;
                 end;
    rftString  : begin
                   If bRunMode Then begin
                     frm.RM.StringResource.ResourceFile := FpResClient.GetCurrentStringResource;
                   end else begin
                     frm.RM.StringResource.ResourceFile := StringResName;
                   end;
                   frm.panStringRes.Visible := True;
                   frm.PageControl1.ActivePage := frm.tsStrings;
                   frm.tsFileResource.Visible := False;
                   frm.tsSQL.Visible := False;
                   frm.Select_StringID(nID);
                 end;
    rftSQL     : begin
                   If bRunMode Then begin
                     frm.RM.SQLResource.ResourceFile := FpResClient.GetCurrentSQLResource;
                   end else begin
                     frm.RM.SQLResource.ResourceFile := SQLResName;
                   end;
                   frm.PageControl1.ActivePage := frm.tsSQL;
                   frm.panSQLRes.Visible := True;
                   frm.tsFileResource.Visible := False;
                   frm.tsStrings.Visible := False;
                   frm.Select_SQLID(nID);
                 end;
  end;
  try
    frm.SelectedID := nID;
    frm.ShowModal;
  finally
    Result := frm.SelectedID;
    If Not bRunMode Then begin
      If FileResName = '' Then
        FileResName := frm.RM.FileResource.ResourceFile;
      If StringResName = '' Then
        StringResName := frm.RM.StringResource.ResourceFile;
      If SQLResName = '' Then
        SQLResName := frm.RM.SQLResource.ResourceFile;
    end;  //  --  If Not bRunMode Then 
    frm.RM.Free;
    frm.RM := nil;
    frm.Free;
  end;
  If bRunMode Then
    FpResClient.Free;
end;

function BitmapFromResource(const ABitmapName: string): TBitmap;
begin
  Result := {$IFDEF VER230}VCL.{$ENDIF}Graphics.TBitmap.Create;
  Result.LoadFromResourceName(hInstance, ABitmapName);
end;

var sLicense : String;
    sUse : String;
    {$IFDEF D9+} AboutBoxServices: IOTAAboutBoxServices; {$ENDIF}
    idx : Integer;

const AboutInfo = 'Modular Application Framework Components'+#13#10+
                  'Copyright 2007-2016 by Helge Lange'+#13#10+
                  'http://www.maf-components.com';
initialization
  sLicense := 'Standard Edition';
  sUse := '';
  idx := -1;
{$IFDEF Trial} sLicense := 'Trial Edition'; sUse := 'Only for 30-day evaluation purposes'; {$ENDIF}
{$IFDEF PE}    sLicense := 'Personal Edition'; sUse := 'Only for non-comercial use'; {$ENDIF}
{$IFDEF SourceEdition} sLicense := 'Professional Edition';{$ENDIF}

  {$IFDEF D9+}
  If Assigned(SplashScreenServices) Then
    SplashScreenServices.AddPluginBitmap('MAF Components', BitmapFromResource('MAFSPLASHICON').Handle, {$IFDEF trial}True{$ELSE}False{$ENDIF}, sLicense, sUse);
  If Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then begin
    idx := AboutBoxServices.AddPluginInfo('Modular Application Framework Components', AboutInfo, BitmapFromResource('MAFSPLASHICON').Handle,
    False, sLicense, sUse);
  end;
  {$ENDIF}
  FpProjectSettings := TProjectSettings.Create;
  RegisterMAF_Config;
  ReadMAFValues;

finalization
  {$IFDEF D9+}
  If Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) Then
    If idx > 1 Then
      AboutBoxServices.RemovePluginInfo(idx);
  {$ENDIF}

  FreeAndNil(FpProjectSettings);
  UnRegisterMAF_Config;

end.
