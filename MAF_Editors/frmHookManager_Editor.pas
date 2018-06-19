{*******************************************************************************
Name         : frmHookManager_Editor.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007-2010 by Helge Lange
Info         : HelgeLange@gmail.com
Website      : http://www.maf-components.com
Date         : 23.03.2007
Last Update  : 23.06.2010
Version      : 1.0.013
Purpose      : Property-Editor for TmafHookManager component
Last Changes :

1.0.013 (23.06.2010) -----------------------------------------------------------
- [FIX] fixed a bug where the modules weren't written into the XML when the editor
        was closed
1.0.012 (26.03.2010) -----------------------------------------------------------
- [FIX] the entry "All Modules" was gone from the module filter in the Editor-Tab
        causing also the SubHookEditor window to remove the first module in list
        to disappear there
1.0.011 (21.10.2009) -----------------------------------------------------------
- [FIX] small bug fixed when adding a new module and it exported no functions
        yet, it asked anyway, if the 0 functions should be imported
1.0.010 (19.09.2009) -----------------------------------------------------------
- [FIX] fixed broken SubHook order (save the DFT was disabled while the
        HookManager component was rebuild)
1.0.009 (13.09.2009) -----------------------------------------------------------
- [ADD] added support for public CodeGroup Names and Descriptions through the
        IDs for both Name and Description. Now these values can be used in an
        admin panel without giving access to the internal Codegroup handling
        to another module 
1.0.008 (31.08.2009) -----------------------------------------------------------
- [FIX] another bug was fixed when updating and reading the description of a
        SubHook
- [CHG] code change to the new HookManager InstallAPI function, also when starting
        the DFT Editor the Hookmanager will be set now automatically into the
        LockDown mode that allows to manipulate the DFT 
1.0.007 (29.08.2009) -----------------------------------------------------------
- [FIX] a bug when assigning a description to a dynamic function was fixed
- [ADD] according to the settings a XML file in the project directory will now
        automatically created when closing the editor window (as component editor
        only)
1.0.006 (22.07.2009) -----------------------------------------------------------
- [ADD] added Hook/SubHook/CodeGroup-Descriptions into the XML export file
- [ADD] when reading the descriptions for Hooks/SubHooks and Codegroups from
        the template data streams, empty items (where the Strings = '') will not
        be added to out internal list and the next time, the list is saved,
        they're gone for good...
1.0.005 (08.07.2009) -----------------------------------------------------------
- [FIX] fixed a bug in __FillSubHookView where the SubHookList was filled with
        all SubHooks, although the Module filter was enabled
- [CHG] Stream read and write functions changed to use TTemplateStreamer, deleted
        the old functions
1.0.004 (13.06.2009) -----------------------------------------------------------
- [ADD] CodeGroup support
- [ADD] XML export
1.0.003 (29.10.2008) -----------------------------------------------------------
- [FIX] fixed the half-implemeted function to activate/deactivate Hooks :)
1.0.002 (10.10.2008) -----------------------------------------------------------
- [FIX] removed a debug stream write for Hook description stream, that is
        normally written only to database
1.0.001 (23.03.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit frmHookManager_Editor;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     VCL.ComCtrls, VCL.Buttons,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs, ComCtrls, Buttons,
     {$ENDIF}
     xmldom, XMLIntf,
     XMLDoc, msxmldom,
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_ModuleManager, uMAF_HookManager, uMAF_DynamicFunctionTable,
     uMAF_HookManager_Helper, uMAF_TemplateStreamer, uMAF_Core, Menus;

type
  TfDFTEdit = class(TForm)
    PageControl1: TPageControl;
    tsEdit: TTabSheet;
    tsLibraries: TTabSheet;
    lvModules: TListView;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    lblID: TLabel;
    lblVersion: TLabel;
    Label3: TLabel;
    lblCopyright: TLabel;
    Label5: TLabel;
    lblBuildDate: TLabel;
    Label7: TLabel;
    lblAuthor: TLabel;
    Label9: TLabel;
    btnAddModule: TButton;
    btnDelete: TButton;
    mDescription: TMemo;
    Label2: TLabel;
    OpenDialog: TOpenDialog;
    Label4: TLabel;
    Label6: TLabel;
    tsCodeGroups: TTabSheet;
    pCodeGroups: TPanel;
    lvCodeGroups: TListView;
    btnCG_Delete: TButton;
    btnCG_Edit: TButton;
    btnCG_Add: TButton;
    Label12: TLabel;
    pHooks: TPanel;
    lvUnassignedSubHooks: TListView;
    lvAssignedSubHooks: TListView;
    btnCG_HookAdd: TButton;
    btnCG_HookRemove: TButton;
    Label13: TLabel;
    Label14: TLabel;
    XML: TXMLDocument;
    SD: TSaveDialog;
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    btnXML_Import: TButton;
    cbClearCurrentData: TCheckBox;
    cbImportDescriptions: TCheckBox;
    cbUpdateExistingData: TCheckBox;
    Panel2: TPanel;
    btnXML_Export: TButton;
    cbExportDescriptions: TCheckBox;
    Button2: TButton;
    PopupMenu1: TPopupMenu;
    mForce_uID_Change: TMenuItem;
    Panel3: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Panel6: TPanel;
    lvHooks: TListView;
    Label10: TLabel;
    Panel7: TPanel;
    cbModules: TComboBox;
    Label8: TLabel;
    Panel8: TPanel;
    Panel9: TPanel;
    btnUp: TBitBtn;
    btnDown: TBitBtn;
    Panel4: TPanel;
    Panel10: TPanel;
    btnEditHook: TButton;
    lvSubHooks: TListView;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Label11: TLabel;
    bFilterSubHooks: TCheckBox;
    Panel14: TPanel;
    btnNewSubHook: TButton;
    btnEditSubHook: TButton;
    btnDeleteSubHook: TButton;
    procedure FormShow(Sender: TObject);
    procedure lvModulesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnAddModuleClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tsEditShow(Sender: TObject);
    procedure cbModulesChange(Sender: TObject);
    procedure lvHooksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvHooksClick(Sender: TObject);
    procedure btnEditHookClick(Sender: TObject);
    procedure lvSubHooksClick(Sender: TObject);
    procedure btnEditSubHookClick(Sender: TObject);
    procedure btnDeleteSubHookClick(Sender: TObject);
    procedure btnUpDownClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnXML_ExportClick(Sender: TObject);

    // CodeGroup support
    procedure btnCG_AddClick(Sender: TObject);
    procedure lvCodeGroupsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvUnassignedSubHooksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvAssignedSubHooksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnCG_HookAddClick(Sender: TObject);
    procedure btnCG_HookRemoveClick(Sender: TObject);
    procedure btnCG_EditClick(Sender: TObject);
    procedure btnCG_DeleteClick(Sender: TObject);
    procedure lvUnassignedSubHooksInfoTip(Sender: TObject; Item: TListItem; var InfoTip: string);
    procedure Button2Click(Sender: TObject);
    procedure btnXML_ImportClick(Sender: TObject);
    procedure cbClearCurrentDataClick(Sender: TObject);
    procedure mForce_uID_ChangeClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    FpHookDescList : TStringList;
    FpSubHookDescList : TStringList;
    FpCodeGroupDataList : TList;       // list of PCodeGroupData
    FpTemplateStreamer : TTemplateStreamer;
    FOnCloseEditor : TNotifyEvent;
    // stream loader events
    procedure __WriteStreamAttribute(Sender: TObject; ID: Integer);
    procedure __ReadStreamAttribute(Sender: TObject; ID: Integer);

    procedure EnableDisableEditControls;
    procedure __FillLibraryListView;
    function __FillHookView(nSelectedHookID: Integer = -1): TListItem;
    procedure __FillSubHookView(pToken: PDynamicFunction);
    function __GetHookListIndex(List: TStringList; HookID: Integer): Integer;
    procedure RegisterHook(pSH: PSubHook; bDelete: Boolean);
    function GetModuleName(nModuleID: Integer): String;
    function UsesModule(SubHookList: TList; ModuleID: Integer): Boolean;

    // CodeGroup support
    procedure __FillCodeGroupListView;
    procedure __FillUnassignedHooksListView;
    procedure __FillAssignedHookListView(nCodeGroupID: Integer);
    procedure __ReadWrite_Template(StreamID: Integer; Action: TDataStorageAction);
    procedure __AddCodeGroupInListView(pToken: PCodeGroupData);
    procedure __AddSubHookInListView(AListView: TListView; pToken: PSubHook);
    procedure __RegisterCodeGroupChange(pData: PSubHook; nNewGroupID: Integer);
    function __GetCodeGroupData(nCodeGroupID: Integer): PCodeGroupData;

  public
    HM : TmafHookManager;
    procedure DoXML_Export;
    property OnCloseEditor : TNotifyEvent read FOnCloseEditor write FOnCloseEditor;
  end;

implementation

uses {$IFDEF Tracer} uMAF_Tracer, {$ENDIF}
     frmEditName_Generic, frmEditSubHook, frmCodeGroupEdit, uMAF_Tools;

{$R *.dfm}

const sHookDescIdent      : String = 'Hook_Descriptions';
      sSubHookDescIdent   : String = 'SubHook_Descriptions';
      HookStreamVer     = 1000;
      SubHookStreamVer  = 2000;
      ID_HookDesc       = 543;
      ID_SubHookDesc    = 544;

function TfDFTEdit.__GetHookListIndex(List: TStringList; HookID: Integer): Integer;
var i : integer;
begin
  Result := -1;
  If List = FpHookDescList Then begin
    For i := 0 To List.Count - 1 Do
      If List.Objects[i] <> nil Then
        If HookID = PDynamicFunction(List.Objects[i])^.HookID Then begin
          Result := i;
          Exit;
        end;
  end;
  If List = FpSubHookDescList Then begin
    For i := 0 To List.Count - 1 Do
      If List.Objects[i] <> nil Then
        If HookID = PSubHook(List.Objects[i])^.uID Then begin
          Result := i;
          Exit;
        end;
  end;
end;

procedure TfDFTEdit.btnDeleteClick(Sender: TObject);
var pToken : PMAFInstallToken;
begin
  If lvModules.Selected <> nil Then begin
    pToken := __Create_InstallToken(iaUnregisterModule);
    pToken^.nModuleID := TmafModuleManager(lvModules.Selected.Data).ModuleController.ModuleInfo.ModuleID;
    HM.DynamicFuntionTable.Query_InstallAPI(pToken);
    __Free_InstallToken(pToken);
    __FillLibraryListView;
    lvSubHooks.Items.Clear;
    __FillHookView;
  end;  //  --  If lvModules.Selected <> nil Then 
end;

procedure TfDFTEdit.btnDeleteSubHookClick(Sender: TObject);
var pInstallToken: PMAFInstallToken;
begin
  If ((lvSubHooks.Selected <> nil) And (lvHooks.Selected <> nil)) Then  // just to be sure
    If MessageDlg('Delete SubbHook ?', mtConfirmation, [mbYes,mbNo], 0) = mrYes Then begin
      pInstallToken := __Create_InstallToken(iaDelete);
      pInstallToken^.uID := PSubHook(lvSubHooks.Selected.Data)^.uID;
      pInstallToken^.nHookID := PDynamicFunction(lvHooks.Selected.Data)^.HookID;
      pInstallToken^.nSubHookID := PSubHook(lvSubHooks.Selected.Data)^.SubHookID;
      pInstallToken^.nModuleID := PSubHook(lvSubHooks.Selected.Data)^.nModuleID;
      HM.DynamicFuntionTable.Query_InstallAPI(pInstallToken);
      If HM.DynamicFuntionTable.GetDynamicFunction(pInstallToken^.nHookID) = nil Then begin
        __FillHookView;
        lvSubHooks.Clear;
      end else
        __FillSubHookView(PDynamicFunction(lvHooks.Selected.Data));

      // For we don't use the install token anymore, we can "rape" it a bit
      pInstallToken^.nRelativeHook := __GetHookListIndex(FpSubHookDescList, pInstallToken^.nSubHookID);
      If pInstallToken^.nRelativeHook > -1 Then
        FpSubHookDescList.Delete(pInstallToken^.nRelativeHook);
      pInstallToken^.nRelativeHook := __GetHookListIndex(FpHookDescList, pInstallToken^.nHookID);
      If pInstallToken^.nRelativeHook > -1 Then
        FpHookDescList.Delete(pInstallToken^.nRelativeHook);
        
      __Free_InstallToken(pInstallToken);
    end;  //  --  If MessageDlg('Delete SubbHook ?', mtConfirmation, [mbYes,mbNo], 0) = mrYes Then
  EnableDisableEditControls;  
end;

procedure TfDFTEdit.btnEditHookClick(Sender: TObject);
var EditFrm : TfEditName_Generic;
    DescIdx : Integer;
begin
  DescIdx := __GetHookListIndex(FpHookDescList, PDynamicFunction(lvHooks.Selected.Data)^.HookID);
  EditFrm := TfEditName_Generic.Create(Self);
  EditFrm.Caption := 'Enter a new Hook description...';
  EditFrm.Label1.Caption := 'New description (max. 250 chars)';
  If DescIdx > -1 Then
    EditFrm.eHookDesc.Text := FpHookDescList.Strings[DescIdx];

  If EditFrm.ShowModal = mrOk Then begin
    If DescIdx > -1 Then FpHookDescList.Strings[DescIdx] := EditFrm.eHookDesc.Text
                    Else FpHookDescList.AddObject(EditFrm.eHookDesc.Text, TObject(PDynamicFunction(lvHooks.Selected.Data)));
    lvHooks.Selected.SubItems.Strings[1] := EditFrm.eHookDesc.Text;
    __ReadWrite_Template(ID_HookDesc, taWrite);
  end;
  FreeAndNil(EditFrm);
end;

procedure TfDFTEdit.btnEditSubHookClick(Sender: TObject);
var EditFrm : TfEditSubHook;
    i, DescIdx : Integer;
    pSubHookToken, pData : PSubHook;
    pDF : PDynamicFunction;
begin
  DescIdx := -1;
  EditFrm := TfEditSubHook.Create(Self);
  pData := nil;

  // adding the module list
  For i := 1 To cbModules.Items.Count - 1 Do // item 0 is "all modules" and we skip this ;)
    EditFrm.cbModules.Items.AddObject(cbModules.Items.Strings[i], TObject(TmafModuleManager(cbModules.Items.Objects[i]).ModuleController.ModuleInfo.ModuleID));
  If EditFrm.cbModules.Items.Count > 0 Then
    EditFrm.cbModules.ItemIndex := 0;

  // in Edit mode only
  If Sender = btnEditSubHook Then begin
    pDF := PDynamicFunction(lvHooks.Selected.Data);
    pSubHookToken := PSubHook(lvSubHooks.Selected.Data);
    DescIdx := __GetHookListIndex(FpSubHookDescList, pSubHookToken^.uID);
    If DescIdx > -1 Then // setting description, if we have one
      EditFrm.eSubHookDesc.Text := FpSubHookDescList.Strings[DescIdx];
    EditFrm.eHookID.Text := IntToStr(pDF^.HookID);
    EditFrm.eSubHookID.Text := lvSubHooks.Selected.SubItems[0]; // setting the SubHookID
    EditFrm.ModuleID := pSubHookToken^.nModuleID;
  end else begin
    pDF := nil;
    pSubHookToken := nil;
    If Assigned(lvHooks.Selected) Then
      EditFrm.eHookID.Text := IntToStr(PDynamicFunction(lvHooks.Selected.Data)^.HookID);
  end;  //  --  If Sender = btnEditSubHook Then

  If EditFrm.ShowModal = mrOk Then begin
    pData := __Create_SubHookToken;
    pData^.HookID := EditFrm.HookID;
    pData^.SubHookID := EditFrm.SubHookID;
    pData^.nModuleID := EditFrm.ModuleID;
    // we have to check, if there are changes made in HookID or SubHookID
    // If one changed, the current SubHook HAS TO BE removed and a new installed
    If Sender = btnEditSubHook Then begin
      // we still have pDF and pSubHookToken to compare with the new values
      // so if HookID, SubHookID or ModuleID has changed, we unregister the SubHook
      If ((pDF^.HookID <> pData^.HookID) Or (pSubHookToken^.SubHookID <> pData^.SubHookID) Or (pSubHookToken^.nModuleID <> pData^.nModuleID)) Then begin
        If pSubHookToken^.nModuleID <> pData^.nModuleID Then
          If MessageDlg('The module changed, therefore a new uID should be assinged. Would you like to do that ?', mtConfirmation, [mbYes,mbNo], 0) = mrNo Then
            pData^.uID := pSubHookToken^.uID;  // in edit mode we use the old SubHookID
        // we don't need the old description for the SubHook
        If DescIdx > -1 Then
          FpSubHookDescList.Delete(DescIdx);
        // if we remove the Hook itself, because it's the only SubHook in it, we can
        // delete its description, too
        If ((pDF^.FpSubHooks.Count = 1) And (pDF^.HookID <> pData^.HookID)) Then begin
          DescIdx := __GetHookListIndex(FpHookDescList, pDF^.HookID);
          i := __GetHookListIndex(FpHookDescList, EditFrm.HookID);
          If i = -1 Then begin // is no description saved for the new HookID ?
            If DescIdx > -1 Then // and there is a description already for the old Hook
              FpHookDescList.Objects[DescIdx] := TObject(EditFrm.HookID); // we move the description to the new Hook
          end else
            If DescIdx > -1 Then
              FpHookDescList.Delete(DescIdx);
        end;  //  --  If ((pDF^.FpSubHooks.Count = 1) And (pDF^.HookID <> EditFrm.HookID)) Then
        RegisterHook(pSubHookToken, True);
        DescIdx := -1;
      end;  //  --  If ((pDF^.HookID <> EditFrm.HookID) Or (pSubHookToken^.SubHookID <> EditFrm.SubHookID) Or (pSubHookToken^.nModuleID <> EditFrm.ModuleID)) Then
    end;  //  --  If Sender = btnEditSubHook Then

    // In edit mode could be changes made wich lead to a completly new SubHook,
    // so in a way we have to treat an edited token like a new token
    pDF  := HM.DynamicFuntionTable.GetDynamicFunction(pData^.HookID); // do we have this Hook already ?
    If pDF <> nil Then begin
      // for this Hook already exists, we have to check, if the SubHook exist, too
      pSubHookToken := HM.DynamicFuntionTable.GetSubHookToken(pData^.HookID, pData^.SubHookID, pData^.nModuleID);
      If pSubHookToken = nil Then begin
        RegisterHook(pData, False);
      end;
    end else begin
      RegisterHook(pData, False);
    end;

    pDF  := HM.DynamicFuntionTable.GetDynamicFunction(EditFrm.HookID); // now we should get the Hook & SubHook
    pSubHookToken := HM.DynamicFuntionTable.GetSubHookToken(pDF^.HookID, EditFrm.SubHookID, EditFrm.ModuleID);
    // We're just going to add the description for the SubHook
    If ((DescIdx > -1) And (DescIdx < FpSubHookDescList.Count)) Then
      FpSubHookDescList.Strings[DescIdx] := EditFrm.eSubHookDesc.Text
    Else
      FpSubHookDescList.AddObject(EditFrm.eSubHookDesc.Text, TObject(pSubHookToken));
    __ReadWrite_Template(ID_HookDesc, taWrite);
    __ReadWrite_Template(ID_SubHookDesc, taWrite);
    __FillHookView(pDF^.HookID);
    lvHooksSelectItem(lvHooks, lvHooks.Selected, True); // fire fake event
  end;  //  --  If EditFrm.ShowModal = mrOk Then
  FreeAndNil(EditFrm);
  __Free_SubHookToken(pData);
  EnableDisableEditControls;
end;

procedure TfDFTEdit.btnUpDownClick(Sender: TObject);
var pToken, pTemp : PSubHook;
    pDF : PDynamicFunction;
    idx : Integer;
    pData : PMAFInstallToken;
begin
  If ((lvSubHooks.Selected = nil) Or (lvHooks.Selected = nil)) Then   // just to be sure :)
    Exit;

  pDF := PDynamicFunction(lvHooks.Selected.Data);
  pToken := PSubHook(lvSubHooks.Selected.Data);
  If ((pDF = nil) Or (pToken = nil)) Then
    Exit;

  idx := pDF^.FpSubHooks.IndexOf(pToken);
  If idx = -1 Then
    Exit;

  // now we have Hook and SubHook
  If Sender = btnUp Then begin
    If idx > 0 Then begin
      // switching previous and current SubHook
      pTemp := pDF^.FpSubHooks.Items[idx-1];
      pDF^.FpSubHooks.Items[idx-1] := pToken;
      pDF^.FpSubHooks.Items[idx] := pTemp;
    end;
  end;

  If Sender = btnDown Then begin
    If idx < pDF^.FpSubHooks.Count Then begin
      // switching next and current SubHook
      pTemp := pDF^.FpSubHooks.Items[idx+1];
      pDF^.FpSubHooks.Items[idx+1] := pToken;
      pDF^.FpSubHooks.Items[idx] := pTemp;
    end;
  end;
  // and finally update the order in the database
  New(pData);
  pData^.nAction := iaSaveChanges;
  HM.DynamicFuntionTable.Query_InstallAPI(pData);
  Dispose(pData);
  // and fill the SubHookView fresh 
  __FillSubHookView(pDF);
  For idx := 0 To lvSubHooks.Items.Count - 1 do
    If lvSubHooks.Items.Item[idx].Data = pToken Then
      lvSubHooks.Selected := lvSubHooks.Items.Item[idx];
  EnableDisableEditControls;
end;

procedure TfDFTEdit.cbClearCurrentDataClick(Sender: TObject);
begin
  cbUpdateExistingData.Enabled := Not cbClearCurrentData.Checked;
  If cbClearCurrentData.Checked Then
    cbUpdateExistingData.Checked := False;
end;

procedure TfDFTEdit.cbModulesChange(Sender: TObject);
begin
  __FillHookView;
end;

procedure TfDFTEdit.EnableDisableEditControls;
begin
  If lvHooks.Selected <> nil Then begin
    btnEditHook.Enabled := True;
  end else begin
    btnEditHook.Enabled := False;
  end;

  If lvSubHooks.Selected <> nil Then begin
    btnEditSubHook.Enabled := True;
    btnUp.Enabled := (lvSubHooks.Selected <> lvSubHooks.Items.Item[0]);
    btnDown.Enabled := (lvSubHooks.Selected <> lvSubHooks.Items.Item[lvSubHooks.Items.Count-1]);
  end else begin
    btnEditSubHook.Enabled := False;
    btnUp.Enabled := False;
    btnDown.Enabled := False;
  end;
end;

procedure TfDFTEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  While lvModules.Items.Count > 0 Do begin
    lvModules.Items.Item[0].Data := nil;
    lvModules.Items.Delete(0);
  end;
  Action := caFree;
  If Assigned(FOnCloseEditor) Then
    FOnCloseEditor(Self);
end;

procedure TfDFTEdit.FormCreate(Sender: TObject);
begin
  FpHookDescList := TStringList.Create;
  FpSubHookDescList := TStringList.Create;
  FpCodeGroupDataList := TList.Create;
  FpTemplateStreamer := TTemplateStreamer.Create;
  FpTemplateStreamer.OnStreamReadAttribute := __ReadStreamAttribute;
  FpTemplateStreamer.OnStreamWriteAttribute := __WriteStreamAttribute;
end;

procedure TfDFTEdit.FormDestroy(Sender: TObject);
var pData : PCodeGroupData;
begin
  FreeAndNil(FpHookDescList);
  FreeAndNil(FpSubHookDescList);
  While FpCodeGroupDataList.Count > 0 Do begin
    pData := PCodeGroupData(FpCodeGroupDataList.Items[0]);
    Dispose(pData);
    FpCodeGroupDataList.Delete(0);
  end;  //  --  While FpCodeGroupDataList.Count > 0 Do
  FreeAndNil(FpCodeGroupDataList);
  FreeAndNil(FpTemplateStreamer);
end;

procedure TfDFTEdit.FormShow(Sender: TObject);
begin
  OpenDialog.InitialDir := HM.DynamicFuntionTable.ModulePath;
  Label6.Caption := OpenDialog.InitialDir;
  cbModules.Items.Add('from all modules');

  cbModules.ItemIndex := 0;
  cbModules.OnChange := cbModulesChange;
  {$IFDEF Tracer}
  MAFTracer.Enabled := True;
  {$ENDIF}
  OutputDebugString(PChar('__ReadWrite_Template(ID_HookDesc, taRead)'));
  __ReadWrite_Template(ID_HookDesc, taRead);

  OutputDebugString(PChar('__ReadWrite_Template(ID_SubHookDesc, taRead)'));
  __ReadWrite_Template(ID_SubHookDesc, taRead);
  OutputDebugString(PChar('__FillLibraryListView'));
  __FillLibraryListView;      // loading the library list
  __ReadWrite_Template(ID_CodeGroupDesc, taRead);
  {$IFDEF Tracer}
  MAFTracer.Enabled := False;
  {$ENDIF}
  __FillHookView;             // loading Hooks
  __FillCodeGroupListView;    // loading Code Groups
  EnableDisableEditControls;
  If lvModules.Items.Count > 0 Then
    lvModules.Selected := lvModules.Items.Item[0];
end;

procedure TfDFTEdit.lvModulesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  If ((Selected) And (Item.Data <> nil) And (TmafModuleManager(Item.Data).ModuleController <> nil)) Then begin
    lblID.Caption := IntToStr(TmafModuleManager(Item.Data).ModuleController.ModuleInfo.ModuleID);
    lblVersion.Caption := TmafModuleManager(Item.Data).ModuleController.ModuleInfo.VersionString;
    lblCopyright.Caption := TmafModuleManager(Item.Data).ModuleController.ModuleInfo.CopyRight;
    lblBuildDate.Caption := TmafModuleManager(Item.Data).ModuleController.ModuleInfo.BuildDate;
    lblAuthor.Caption := TmafModuleManager(Item.Data).ModuleController.ModuleInfo.Author;
    mDescription.Lines.Text := TmafModuleManager(Item.Data).ModuleController.ModuleInfo.FsDescription.Text;
  end;  //  --  If Selected Then
end;

procedure TfDFTEdit.lvSubHooksClick(Sender: TObject);
var i : Integer;
    pData : PSubHook;
    pToken : PMAFInstallToken;
begin
  EnableDisableEditControls;
  pToken := __Create_InstallToken(iaActivate);
  For i := 0 To lvSubHooks.Items.Count - 1 Do begin
    pData := lvSubHooks.Items.Item[i].Data;
    If pData^.bActive <> lvSubHooks.Items.Item[i].Checked Then begin
      pToken^.uID := pData^.uID;
      If lvSubHooks.Items.Item[i].Checked Then pToken^.bActive := 1
                                          Else pToken^.bActive := 0;
      HM.DynamicFuntionTable.Query_InstallAPI(pToken);
    end;
  end;  //  --  For i := 0 To lvSubHooks.Items.Count - 1 Do
  __Free_InstallToken(pToken);
end;

procedure TfDFTEdit.RegisterHook(pSH: PSubHook; bDelete: Boolean);
var pToken : PMAFInstallToken;
begin
  pToken := HookToInstallToken(pSH);
  If bDelete Then
    pToken^.nAction := iaDelete;
  pToken^.InsertDir := hidLast;
  pToken^.bActive := 1;
  HM.DynamicFuntionTable.Query_InstallAPI(pToken);
  __Free_InstallToken(pToken);
end;

procedure TfDFTEdit.tsEditShow(Sender: TObject);
begin
  If cbModules.ItemIndex = -1 Then
    cbModules.ItemIndex := 0; // all modules
end;

function TfDFTEdit.UsesModule(SubHookList: TList; ModuleID: Integer): Boolean;
var i : Integer;
begin
  Result := False;
  For i := 0 To SubHookList.Count - 1 Do
    If PSubHook(SubHookList.Items[i])^.nModuleID = ModuleID Then begin
      Result := True;
      Exit;
    end;
end;

function TfDFTEdit.GetModuleName(nModuleID: Integer): String;
var i: Integer;
begin
  Result := 'Unknown module';
  For i := 0 To lvModules.Items.Count - 1 Do begin
    If TmafModuleManager(lvModules.Items.Item[i].Data).ModuleController.ModuleInfo.ModuleID = nModuleID Then begin
      Result := lvModules.Items.Item[i].Caption;
      Exit;
    end;
  end;
end;

procedure TfDFTEdit.lvHooksClick(Sender: TObject);
begin
  btnEditHook.Enabled := (lvHooks.Selected <> nil);
end;

procedure TfDFTEdit.__FillSubHookView(pToken: PDynamicFunction);
var i, j, CurrModuleID : Integer;
    LI : TListItem;
    pData : PSubHook;
begin
  j := -1;
  If ((bFilterSubHooks.Checked) And (cbModules.ItemIndex > 0)) Then
    j := TmafModuleManager(cbModules.Items.Objects[cbModules.ItemIndex]).ModuleController.ModuleInfo.ModuleID;

  lvSubHooks.Items.BeginUpdate;
  lvSubHooks.Clear;
  btnEditSubHook.Enabled := False;
  For i := 0 To pToken^.FpSubHooks.Count - 1 Do begin
    CurrModuleID := PSubHook(pToken^.FpSubHooks.Items[i])^.nModuleID;
    If ((j = -1) or (j = CurrModuleID)) Then begin
      LI := lvSubHooks.Items.Add;
      LI.Data := PSubHook(pToken^.FpSubHooks.Items[i]);
      pData := PSubHook(LI.Data);
      LI.Caption := IntToStr(pData^.uID);
      LI.SubItems.Add(IntToStr(pData^.SubHookID));
      LI.SubItems.Add(GetModuleName(CurrModuleID));
      CurrModuleID := __GetHookListIndex(FpSubHookDescList, pData^.uID);
      If CurrModuleID > -1 Then
        LI.SubItems.Add(FpSubHookDescList.Strings[CurrModuleID])
      Else
        LI.SubItems.Add('');
      If pData^.nCodeGroupID > -1 Then
        LI.SubItems.Add(__GetCodeGroupData(pData^.nCodeGroupID)^.Name)
      Else
        LI.SubItems.Add('');
      LI.Checked := pData^.bActive;
{      If HM.SecurityLayer <> nil Then
        If HM.SecurityLayer.IsProtected(pToken^.HookID, pData^.SubHookID) Then
          LI.Checked := True; }
    end;
  end;
  lvSubHooks.Items.EndUpdate;
  EnableDisableEditControls;
end;

procedure TfDFTEdit.lvHooksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  If ((Selected) And (Item <> nil)) Then
    __FillSubHookView(PDynamicFunction(lvHooks.Selected.Data));
end;

function TfDFTEdit.__FillHookView(nSelectedHookID: Integer = -1): TListItem;
var i, j, CurrModuleID : Integer;
    LI, LIStored : TListItem;
    bAdd : Boolean;
begin
  LIStored := nil;
  lvHooks.Items.BeginUpdate;
  lvSubHooks.Items.BeginUpdate;
  lvHooks.Clear;
  lvSubHooks.Clear;
  If cbModules.ItemIndex < 1 Then
    CurrModuleID := -1
  Else
    CurrModuleID := TmafModuleManager(cbModules.Items.Objects[cbModules.ItemIndex]).ModuleController.ModuleInfo.ModuleID;
  For i := 0 to HM.DynamicFuntionTable.DFT.Count - 1 Do begin
    bAdd := True;
    If CurrModuleID > -1 Then
      bAdd := UsesModule(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks, CurrModuleID);
    If bAdd Then begin
      LI := lvHooks.Items.Add;
      LI.Caption := IntToStr(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.HookID);
      If PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.HookID = nSelectedHookID Then
        LIStored := LI;
      LI.SubItems.Add(IntToStr(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Count));
      // fill description
      j := __GetHookListIndex(FpHookDescList, PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.HookID);
      If j > -1 Then LI.SubItems.Add(FpHookDescList.Strings[j])
                Else LI.SubItems.Add('');
      LI.Data := HM.DynamicFuntionTable.DFT.Items[i];
    end;  //  --  If bAdd Then
  end;  //  --  For i := 0 to HM.DFT.Count - 1 Do
  lvHooks.Items.EndUpdate;
  lvSubHooks.Items.EndUpdate;
  lvHooks.Selected := LIStored;
  Result := LIStored;
  EnableDisableEditControls;
end;

procedure TfDFTEdit.__FillLibraryListView;
var i : Integer;
    LI : TListItem;
begin
  lvModules.Items.BeginUpdate;
  lvModules.Clear;
  cbModules.Items.Clear;
  cbModules.Items.Add('All Modules');
  cbModules.ItemIndex := 0;
  For i := 0 To HM.DynamicFuntionTable.Modules.Count - 1 Do begin
    LI := lvModules.Items.Add;
    LI.Data := TmafModuleManager(HM.DynamicFuntionTable.Modules.Items[i]);
    If TmafModuleManager(HM.DynamicFuntionTable.Modules.Items[i]).ModuleController <> nil Then
      LI.Caption := TmafModuleManager(HM.DynamicFuntionTable.Modules.Items[i]).ModuleController.ModuleInfo.ModuleName
    Else
      LI.Caption := TmafModuleManager(HM.DynamicFuntionTable.Modules.Items[i]).ModuleName;
    // and adding the same item in the ComboBox
    cbModules.Items.AddObject(LI.Caption, TmafModuleManager(HM.DynamicFuntionTable.Modules.Items[i]));
  end;  //  --  For i := 0 To HM.DynamicFuntionTable.Modules.Count - 1 Do 
  lvModules.Items.EndUpdate;
end;

procedure TfDFTEdit.btnAddModuleClick(Sender: TObject);
var ModuleManager : TmafModuleManager;
    LI : TListItem;
    S : String;
    i, nModuleID, nFunctionCount, ErrCode : Integer;
    pToken : PMAFInstallToken;
begin
  If OpenDialog.Execute Then begin
    pToken := __Create_InstallToken(iaRegisterModule);
    S := ExtractFileName(OpenDialog.FileName);
    For i := 0 To lvModules.Items.Count - 1 Do
      If UpperCase(S) = Uppercase(lvModules.Items[i].Caption) Then begin
        MessageDlg('Module already in loaded !', mtError, [mbOK], 0);
        Exit;
      end;  //  --  If UpperCase(S) = Uppercase(lvModules.Items[i].Caption) Then
    ModuleManager := TmafModuleManager.Create(OpenDialog.FileName);
    ErrCode := ModuleManager.Initialize(nil, nil);
    If ErrCode <> ERR_NO_ERROR Then begin
      MessageDlg('Error initializing the module !'+#13#10+'Error Code : ' + IntToStr(ErrCode), mtError, [mbOk], 0);
      Exit;
    end;
    If ModuleManager.ModuleController.ModuleInfo.ModuleID = -1 Then begin
      ModuleManager.Free;
      MessageDlg('Error loading module ! Module information missing...', mtError, [mbOK], 0);
    end Else begin
      nModuleID := ModuleManager.ModuleController.ModuleInfo.ModuleID;
      If HM.DynamicFuntionTable.GetModuleManager(nModuleID) <> nil Then begin
        MessageDlg('A module with the same ID is already loaded. Aborting...', mtError, [mbOK], 0);
        ModuleManager.Free;
        Exit;
      end;
      LI := lvModules.Items.Add;
      S := ModuleManager.ModuleController.ModuleInfo.ModuleName;
      StrToPChar(S, pToken^.sDescription);
      LI.Caption := ModuleManager.ModuleController.ModuleInfo.ModuleName;
      nFunctionCount := ModuleManager.ModuleController.Data.Count;
      ModuleManager.Free;

      If nFunctionCount > 0 Then
        If MessageDlg('The ModuleController reports '+IntToStr(nFunctionCount)+' SubHooks, import them automatically ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
          pToken^.bActive := 1;
      HM.DynamicFuntionTable.Query_InstallAPI(pToken);
      ModuleManager := HM.DynamicFuntionTable.GetModuleManager(nModuleID);
      LI.Data := Pointer(ModuleManager);
      // and adding the same item in the ComboBox
      cbModules.Items.AddObject(LI.Caption, ModuleManager);
    end;
    __Free_InstallToken(pToken);
  end;
end;


{ ******************  CodeGroup Support ************************************** }


procedure TfDFTEdit.__ReadWrite_Template(StreamID: Integer; Action: TDataStorageAction);
//var ErrCode : Integer;
begin
  If Assigned(HM.DataStorage) Then begin // works only through templates
    FpTemplateStreamer.Attributes := 1;
    FpTemplateStreamer.Stream.Size := 0;
    FpTemplateStreamer.DataStorageQueryToken^.Action := Action;
    FpTemplateStreamer.DataStorageQueryToken^.nID := 0;
    FpTemplateStreamer.TemplateCategory := 'HookDesc';


    Case StreamID Of
      ID_HookDesc       : begin
                            {$IFDEF Tracer}
                            MAFTracer.CheckPoint('ID_HookDesc');
                            {$ENDIF}
                            FpTemplateStreamer.StreamVersion := HookStreamVer;     // Stream version for Hook Descriptions
                            FpTemplateStreamer.TemplateName := sHookDescIdent;
                          end; // ID_HookDesc
      ID_SubHookDesc    : begin
                            {$IFDEF Tracer}
                            MAFTracer.CheckPoint('ID_SubHookDesc');
                            {$ENDIF}
                            FpTemplateStreamer.StreamVersion := SubHookStreamVer;  // Stream version for SubHook Descriptions
                            FpTemplateStreamer.TemplateName := sSubHookDescIdent;
                          end; // ID_SubHookDesc
      ID_CodeGroupDesc  : begin
                            {$IFDEF Tracer}
                            MAFTracer.CheckPoint('ID_CodeGroupDesc');
                            {$ENDIF}
                            FpTemplateStreamer.StreamVersion := CodeGroupDataVer;  // Stream version for CodeGroup Descriptions
                            FpTemplateStreamer.TemplateName := HOOKMANAGER_TEMPLATE_CODEGROUP;
                          end; // ID_CodeGroupDesc
    end;  //  --  Case StreamID Of

    If Action = taWrite Then
      FpTemplateStreamer.WriteStream(StreamID);

    // do the query to the template handler
{    If __CheckQueryToken(FpTemplateStreamer.DataStorageQueryToken) = False Then
      ShowMessage('Token problem'); }
    {ErrCode := }HM.DataStorage.Query(FpTemplateStreamer.DataStorageQueryToken);
//    If ErrCode <> ERR_NO_ERROR Then
//      ShowMessage(IntToStr(ErrCode));

    If ((Action = taRead) And (FpTemplateStreamer.Stream.Size > 0)) Then
      FpTemplateStreamer.ReadStream;
  end;  //  --  If Assigned(HM.TemplateHandler) Then
end;

procedure TfDFTEdit.lvCodeGroupsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  If Selected Then begin
    __FillUnassignedHooksListView;
    __FillAssignedHookListView(PCodeGroupData(Item.Data)^.nID);
  end;
  btnCG_Delete.Enabled := Selected;
  btnCG_Edit.Enabled := Selected;
end;

procedure TfDFTEdit.lvUnassignedSubHooksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnCG_HookAdd.Enabled := Selected;
end;

procedure TfDFTEdit.mForce_uID_ChangeClick(Sender: TObject);
var pSH : PSubHook;
    S : String;
    i : Integer;
begin
  If lvSubHooks.Selected <> nil Then begin
    pSH := PSubHook(lvSubHooks.Selected.Data);
    If Assigned(pSH) Then begin
      S := IntToStr(pSH^.uID);
      If InputQuery('Force uID', 'New uID :', S) Then begin
        i := StrToIntDef(S, -1);
        If i > 0 Then
          pSH^.uID := i;
      end;
    end;
    lvSubHooks.Refresh;
  end;
end;

procedure TfDFTEdit.PopupMenu1Popup(Sender: TObject);
begin
  mForce_uID_Change.Enabled := (lvSubHooks.Selected <> nil);
end;

procedure TfDFTEdit.lvAssignedSubHooksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnCG_HookRemove.Enabled := Selected;
end;

function TfDFTEdit.__GetCodeGroupData(nCodeGroupID: Integer): PCodeGroupData;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpCodeGroupDataList.Count - 1 do
    If PCodeGroupData(FpCodeGroupDataList.Items[i])^.nID = nCodeGroupID Then begin
      Result := PCodeGroupData(FpCodeGroupDataList.Items[i]);
      Break;
    end;  //  --  If PCodeGroupData(FpCodeGroupDataList.Items[i])^.nID = nCodeGroupID Then
end;

procedure TfDFTEdit.__FillCodeGroupListView;
var i : Integer;
    LI : TListItem;
    pData : PCodeGroupData;
begin
  lvCodeGroups.Items.BeginUpdate;
  lvCodeGroups.Items.Clear;
  For i := 0 to HM.DynamicFuntionTable.CodeGroups.Count - 1 Do begin
    LI := lvCodeGroups.Items.Add;
    pData := __GetCodeGroupData(PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.nCodeGroupID);
    LI.Caption := IntToStr(PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.nCodeGroupID);
    If pData = nil Then begin  // we make sure, that we have that data in our list
      New(pData);
      pData^.nID := PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.nCodeGroupID;
      FpCodeGroupDataList.Add(pData);
      pData^.Name := '';
      pData^.Desc := '';
    end;  //  --  If pData = nil Then
    LI.SubItems.Add(pData^.Name);  // name
    LI.SubItems.Add(pData^.Desc);  // description
    LI.SubItems.Add(IntToStr(pData^.MinSL));
    LI.SubItems.Add(IntToStr(PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.FpSubHooks.Count));  // member count
    LI.Data := pData;
  end;  //  --  or i := 0 to HM.CodeGroups.Count - 1 Do
  lvCodeGroups.Items.EndUpdate;
end;


//erstellt beim start eine codegruppe, die noch keinen codegroupdata eintrag in der liste hat


procedure TfDFTEdit.__FillUnassignedHooksListView;
var i, j: Integer;
//    LI : TListItem;
    pToken : PSubHook;
begin
  lvUnassignedSubHooks.Items.BeginUpdate;
  lvUnassignedSubHooks.Items.Clear;
  For i := 0 To HM.DynamicFuntionTable.DFT.Count - 1 do
    For j := 0 To PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Count - 1 Do begin
      If PSubHook(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Items[j])^.nCodeGroupID = -1 Then begin
        pToken := PSubHook(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Items[j]);
        __AddSubHookInListView(lvUnassignedSubHooks, pToken);
      end;  //  --  If PSubHook(PDynamicFunction(HM.DFT.Items[i])^.FpSubHooks.Items[j])^.nCodeGroupID = -1 Then
    end;  //  --  For j := 0 To PDynamicFunction(HM.DFT.Items[i])^.FpSubHooks.Count - 1 Do 
  lvUnassignedSubHooks.Items.EndUpdate;
end;

procedure TfDFTEdit.__FillAssignedHookListView(nCodeGroupID: Integer);
var i, j : Integer;
begin
  lvAssignedSubHooks.Items.BeginUpdate;
  lvAssignedSubHooks.Items.Clear;
  For i := 0 To HM.DynamicFuntionTable.CodeGroups.Count - 1 Do
    If PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.nCodeGroupID = nCodeGroupID Then begin
      For j := 0 To PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.FpSubHooks.Count - 1 do begin
        __AddSubHookInListView(lvAssignedSubHooks, PSubHook(PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.FpSubHooks.Items[j]));
      end;
    end;
  lvAssignedSubHooks.Items.EndUpdate;  
end;

procedure TfDFTEdit.__AddCodeGroupInListView(pToken: PCodeGroupData);
var LI : TListItem;
begin
  LI := lvCodeGroups.Items.Add;
  LI.Caption := IntToStr(pToken^.nID);
  LI.SubItems.Add(pToken^.Name);
  LI.SubItems.Add(pToken^.Desc);
  LI.Data := pToken;
  lvCodeGroups.Selected := LI;  // we select the new CodeGroup entry
end;

procedure TfDFTEdit.btnCG_AddClick(Sender: TObject);
var frm: TfCodeGroupEdit;
    pData : PCodeGroupData;
begin
  frm := TfCodeGroupEdit.Create(Self);
  frm.CodeGroupID := HM.DynamicFuntionTable.HighCodeGroupID + 1;
  If frm.ShowModal = mrOk Then begin
    pData := __Create_CodeGroupDataToken;
    pData^.nID := frm.CodeGroupID;
    pData^.Name := frm.CodeGroupName;
    pData^.Desc := frm.CodeGroupDesc;
    pData^.NameResID := frm.CodeGroupNameID;
    pData^.DescResID := frm.CodeGroupDescID;
    pData^.MinSL := frm.SL;
    FpCodeGroupDataList.Add(pData);
    __ReadWrite_Template(ID_CodeGroupDesc, taWrite);
    HM.DynamicFuntionTable.HighCodeGroupID := HM.DynamicFuntionTable.HighCodeGroupID + 1;  // increment the highest CodeGroupID
    __AddCodeGroupInListView(pData);
    __FillUnassignedHooksListView;
    __FillAssignedHookListView(pData^.nID);
  end;  //  --  If frm.ShowModal = mrOk Then
  frm.Free;
end;

procedure TfDFTEdit.btnCG_DeleteClick(Sender: TObject);
var i, nCodeGroup : Integer;
    pToken : PMAFInstallToken;
    pData : PSubHook;
begin
  If MessageDlg('Really delete that CodeGroup ?', mtConfirmation, [mbYes,mbNo], 0) = mrOk Then begin
    nCodeGroup := PCodeGroupData(lvCodeGroups.Selected.Data)^.nID;
    For i := 0 To HM.DynamicFuntionTable.CodeGroups.Count - 1 Do
      If PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.nCodeGroupID = nCodeGroup Then begin
        While PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.FpSubHooks.Count > 0 Do begin
          pData := PSubHook(PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.FpSubHooks.Items[0]);
          pToken := HookToInstallToken(pData);       // creates the HookInstallToken and fills with the data from the SubHook record
          pToken^.nAction := iaSetCodeGroup;
          pToken^.nCodeGroupID := -1;                // tell the token to set CodeGroupID to -1 (not a member of any CodeGroup)
          HM.DynamicFuntionTable.Query_InstallAPI(pToken);  // let the HookManager save all this
          __Free_InstallToken(pToken);               // free the HookInstallToken
        end;  //  --  While PCodeGroup(HM.CodeGroups.Items[i])^.FpSubHooks.Count > 0 Do
        lvCodeGroups.Items.Delete(lvCodeGroups.Selected.Index); // delete the CodeGroup in the ListView
        Break;                                                  // Break the loop
      end;  //  --  If PCodeGroup(HM.CodeGroups.Items[i])^.nCodeGroupID = nCodeGroup Then


    // And now we delete the description for the CodeGroup  
    For i := 0 To FpCodeGroupDataList.Count - 1 Do
      If PCodeGroupData(FpCodeGroupDataList.Items[i])^.nID = nCodeGroup Then begin
        FpCodeGroupDataList.Delete(i);
        __ReadWrite_Template(ID_CodeGroupDesc, taWrite);
        Break;
      end;  //  --  If PCodeGroupData(FpCodeGroupDataList.Items[i])^.nID = nCodeGroup Then 
  end;  //  --  If MessageDlg('Really delete that CodeGroup ?', mtConfirmation, [mbYes,mbNo], 0) = mrOk Then
end;

procedure TfDFTEdit.btnCG_EditClick(Sender: TObject);
var frm: TfCodeGroupEdit;
begin
  frm := TfCodeGroupEdit.Create(Self);
  frm.CodeGroupID := PCodeGroupData(lvCodeGroups.Selected.Data)^.nID;
  frm.CodeGroupName := PCodeGroupData(lvCodeGroups.Selected.Data)^.Name;
  frm.CodeGroupDesc := PCodeGroupData(lvCodeGroups.Selected.Data)^.Desc;
  frm.CodeGroupNameID := PCodeGroupData(lvCodeGroups.Selected.Data)^.NameResID;
  frm.CodeGroupDescID := PCodeGroupData(lvCodeGroups.Selected.Data)^.DescResID;
  frm.SL := PCodeGroupData(lvCodeGroups.Selected.Data)^.MinSL;
  If frm.ShowModal = mrOk Then begin
    PCodeGroupData(lvCodeGroups.Selected.Data)^.Name := frm.CodeGroupName;
    lvCodeGroups.Selected.SubItems.Strings[0] := frm.CodeGroupName;
    PCodeGroupData(lvCodeGroups.Selected.Data)^.Desc := frm.CodeGroupDesc;
    lvCodeGroups.Selected.SubItems.Strings[1] := frm.CodeGroupDesc;
    PCodeGroupData(lvCodeGroups.Selected.Data)^.MinSL := frm.SL;
    lvCodeGroups.Selected.SubItems.Strings[2] := IntToStr(frm.SL);
    PCodeGroupData(lvCodeGroups.Selected.Data)^.NameResID := frm.CodeGroupNameID;
    PCodeGroupData(lvCodeGroups.Selected.Data)^.DescResID := frm.CodeGroupDescID;
    __ReadWrite_Template(ID_CodeGroupDesc, taWrite);  // save changes
  end;  //  --  If frm.ShowModal = mrOk Then 
  frm.Free;
end;

procedure TfDFTEdit.__AddSubHookInListView(AListView: TListView; pToken: PSubHook);
var LI: TListItem;
begin
  If Assigned(AListView) Then begin
    LI := AListView.Items.Add;
    LI.Caption := IntToStr(pToken^.HookID);
    LI.SubItems.Add(IntToStr(pToken^.SubHookID));
    LI.SubItems.Add(GetModuleName(pToken^.nModuleID));
    LI.Data := pToken;
  end;  //  --  If Assigned(AListView) Then
end;

procedure TfDFTEdit.__RegisterCodeGroupChange(pData: PSubHook; nNewGroupID: Integer);
var pToken: PMAFInstallToken;
begin
  pToken := HookToInstallToken(pData);
  pToken^.nAction := iaSetCodeGroup;
  pToken^.nCodeGroupID := nNewGroupID;
  HM.DynamicFuntionTable.Query_InstallAPI(pToken);
  __Free_InstallToken(pToken);
end;

procedure TfDFTEdit.btnCG_HookAddClick(Sender: TObject);
var pData: PSubHook;
begin
  pData := PSubHook(lvUnassignedSubHooks.Selected.Data);
  __AddSubHookInListView(lvAssignedSubHooks, pData); // create in assigned
  lvUnassignedSubHooks.Items.Delete(lvUnassignedSubHooks.Selected.Index); // delete from unassigned
  __RegisterCodeGroupChange(pData, PCodeGroupData(lvCodeGroups.Selected.Data)^.nID);
end;

procedure TfDFTEdit.btnCG_HookRemoveClick(Sender: TObject);
var pData: PSubHook;
begin
  pData := PSubHook(lvAssignedSubHooks.Selected.Data);
  __AddSubHookInListView(lvUnassignedSubHooks, pData); // create in assigned
  lvAssignedSubHooks.Items.Delete(lvAssignedSubHooks.Selected.Index); // delete from unassigned
  __RegisterCodeGroupChange(pData, -1);
end;

procedure TfDFTEdit.lvUnassignedSubHooksInfoTip(Sender: TObject; Item: TListItem; var InfoTip: string);
var i : Integer;
begin
  If Item <> nil Then
    For i := 0 To FpSubHookDescList.Count - 1 Do
      If FpSubHookDescList.Objects[i] = TObject(Item.Data) Then begin
        InfoTip := FpSubHookDescList.Strings[i];
        Break;
      end;
end;

procedure TfDFTEdit.btnXML_ExportClick(Sender: TObject);
var iNode, aNode, cNode, dNode : IXMLNode;
    i, j, idx : Integer;
    ModuleManager : TmafModuleManager;
    pCG_Data : PCodeGroupData;
begin
  If XML.Active Then
    XML.Active := False;
  If Sender = nil Then
    cbExportDescriptions.Checked := True;
  XML.FileName := '';
  XML.Active := True;
  aNode := XML.AddChild('HookManagerData');
  iNode := aNode.AddChild('Modules');
  For i := 0 to HM.DynamicFuntionTable.Modules.Count - 1 Do begin
    ModuleManager := TmafModuleManager(HM.DynamicFuntionTable.Modules.Items[i]);
    cNode := iNode.AddChild('Module');
    cNode.Attributes['ID'] := ModuleManager.ModuleController.ModuleInfo.ModuleID;
    cNode.Attributes['Name'] := ModuleManager.ModuleController.ModuleInfo.ModuleName;
  end;  //  --  For i := 0 To lvModules.Items.Count - 1 Do

  iNode := aNode.AddChild('Hooks');
  For i := 0 To HM.DynamicFuntionTable.DFT.Count - 1 Do begin
    cNode := iNode.AddChild('HookID');
    cNode.Attributes['HookID'] := PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.HookID;
    If cbExportDescriptions.Checked Then begin
      idx := __GetHookListIndex(FpHookDescList, PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.HookID);
      If idx > -1 Then
        cNode.Attributes['HookDesc'] := FpHookDescList.Strings[idx];
    end;  //  --  If cbExportDescriptions.Checked Then
    For j := 0 To PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Count - 1 Do begin
      dNode := cNode.AddChild('SubHook');
      dNode.Attributes['uID'] := PSubHook(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Items[j])^.uID;
      dNode.Attributes['SubHookID'] := PSubHook(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Items[j])^.SubHookID;
      dNode.Attributes['ModuleID'] := PSubHook(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Items[j])^.nModuleID;
      dNode.Attributes['CodeGroupID'] := PSubHook(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Items[j])^.nCodeGroupID;
      If PSubHook(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Items[j])^.bActive Then
        dNode.Attributes['bActive'] := 1
      Else
        dNode.Attributes['bActive'] := 0;
      If cbExportDescriptions.Checked Then begin
        idx := __GetHookListIndex(FpSubHookDescList, PSubHook(PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Items[j])^.uID);
        If idx > -1 Then
          dNode.Attributes['SubHookDesc'] := FpSubHookDescList.Strings[idx];
      end;  //  --  If cbExportDescriptions.Checked Then
    end;  //  --  For j := 0 To PDynamicFunction(HM.DynamicFuntionTable.DFT.Items[i])^.FpSubHooks.Count - 1 Do
  end;  //  --  For i := 0 To HM.DynamicFuntionTable.DFT.Count - 1 Do

  iNode := aNode.AddChild('CodeGroups');
  For i := 0 To HM.DynamicFuntionTable.CodeGroups.Count - 1 Do begin
    pCG_Data := __GetCodeGroupData(PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.nCodeGroupID);
    If Assigned(pCG_Data) Then begin
      cNode := iNode.AddChild('CodeGroup');
      cNode.Attributes['CodeGroupID'] := PCodeGroup(HM.DynamicFuntionTable.CodeGroups.Items[i])^.nCodeGroupID;
      cNode.Attributes['CodeGroupName'] := pCG_Data^.Name;
      cNode.Attributes['CodegroupDesc'] := pCG_Data^.Desc;
      cNode.Attributes['NameResID'] := pCG_Data^.NameResID;
      cNode.Attributes['DescResID'] := pCG_Data^.DescResID;
      cNode.Attributes['MinSL'] := pCG_Data^.MinSL;
    end;  //  --  If Assigned(pCG_Data) Then
  end;  //  --  For i := 0 To HM.DynamicFuntionTable.CodeGroups.Count - 1 Do

  If Sender = btnXML_Export Then begin
    If SD.Execute{$IFDEF D9+}(Self.WindowHandle){$ENDIF} Then
      XML.SaveToFile(SD.FileName);
  end else
    XML.SaveToFile(SD.FileName);
end;

procedure TfDFTEdit.btnXML_ImportClick(Sender: TObject);
var i, j, idx, Last_uID, HookID : Integer;
    aRoot, bNode, HookIDNode : IXMLNode;
    aModule : TmafModuleManager;
    pToken : PMAFInstallToken;
    pDF : PDynamicFunction;
    pSH : PSubHook;
    S : String;
    pCG : PCodeGroupData;
begin
  If OpenDialog.Execute{$IFDEF D9+}(ParentWindow){$ENDIF} Then begin
    If cbClearCurrentData.Checked Then begin
      HM.DynamicFuntionTable.Close;
      FpHookDescList.Clear;     // the data in the object field were the ones from the DFT and were disposed there
      FpSubHookDescList.Clear;
      While FpCodeGroupDataList.Count > 0 Do begin
        pCG := PCodeGroupData(FpCodeGroupDataList.Items[0]);
        Dispose(pCG);
        FpCodeGroupDataList.Delete(0);
      end;  //  --  While FpCodeGroupDataList.Count > 0 Do
    end;  //  --  If cbClearCurrentData.Checked Then

    XML.Active := False;
    XML.LoadFromFile(OpenDialog.FileName);
    XML.Active := True;
    ARoot := XML.DocumentElement.ChildNodes.FindNode('Modules');
    If ARoot <> nil Then begin
      For i := 0 To ARoot.ChildNodes.Count - 1 Do begin
        bNode := aRoot.ChildNodes.Nodes[i];
        j := bNode.Attributes['ID'];
        S := bNode.Attributes['Name'];
        aModule := HM.DynamicFuntionTable.GetModuleManager(j);
        If Not Assigned(aModule) Then begin
          pToken := __Create_InstallToken(iaRegisterModule);
          StrToPChar(S, pToken^.sDescription);
          pToken^.bActive := 0;  // we don't want to install the SubHooks from the modules ModuleController
          HM.DynamicFuntionTable.Query_InstallAPI(pToken);
          __Free_InstallToken(pToken);
        end;  //  --  If Not Assigned(aModule) Then 
      end;  //  --  If Not Assigned(aModule) Then
    end;  //  --  If ARoot <> nil Then
    __FillLibraryListView;      // loading the library list

    ARoot := XML.DocumentElement.ChildNodes.FindNode('Hooks');
    If ARoot <> nil Then begin
      For i := 0 To aRoot.ChildNodes.Count - 1 Do begin
        Last_uID := -1;   // new dynamic function, so no previous SubHook
        HookIDNode := aRoot.ChildNodes.Nodes[i];
        HookID := HookIDNode.Attributes['HookID'];   // we need it later for the description
        For j := 0 To HookIDNode.ChildNodes.Count - 1 Do begin
          pToken := __Create_InstallToken(iaInsert);
          pToken^.InsertDir := hidLast;
          pToken^.nHookID := HookID;
          bNode := HookIDNode.ChildNodes.Nodes[j];
          pToken^.nSubHookID := bNode.Attributes['SubHookID'];
          pToken^.nModuleID := bNode.Attributes['ModuleID'];
          pToken^.uID := bNode.Attributes['uID'];
          pToken^.nCodeGroupID := bNode.Attributes['CodeGroupID'];
          pToken^.bActive := bNode.Attributes['bActive'];
          pDF := HM.DynamicFuntionTable.GetDynamicFunction(pToken^.nHookID);
          If Assigned(pDF) Then begin
            pSH := HM.DynamicFuntionTable.__GetSubHook_uID(pToken^.uID);
            If Assigned(pSH) Then begin
              // ok, so we got a SubHook under this uID, let's update it
              pSH^.SubHookID := pToken^.nSubHookID;
              pSH^.nModuleID := pToken^.nModuleID;
              pSH^.nCodeGroupID := pToken^.nCodeGroupID;
              pSH^.pModuleManager := HM.DynamicFuntionTable.GetModuleManager(pSH^.nModuleID);
              pSH^.bActive := (pToken^.bActive = 1);
            end else begin
              // we don't have the SubHook itself (according to the uID)
              If Last_uID > -1 Then begin
                pToken^.InsertDir := hidAfter;
                pToken^.nRelative_uID := Last_uID;
              end else
                pToken^.InsertDir := hidLast;
              HM.DynamicFuntionTable.Query_InstallAPI(pToken);
            end;  //  --  If Assigned(pSH) Then
          end else
            HM.DynamicFuntionTable.Query_InstallAPI(pToken);  // we install the dynamic function and the first SubHook

          If cbImportDescriptions.Checked Then begin
            pSH := HM.DynamicFuntionTable.__GetSubHook_uID(pToken^.uID);
            If ((pSH <> nil) And (bNode.HasAttribute('SubHookDesc'))) Then begin
              S := bNode.Attributes['SubHookDesc'];
              idx := __GetHookListIndex(FpSubHookDescList, pSH^.uID);
              If idx > -1 Then begin
                FpSubHookDescList.Strings[idx] := S;
                FpSubHookDescList.Objects[idx] := TObject(pSH);
              end else
                FpSubHookDescList.AddObject(S, TObject(pSH));
            end;  //  --  If ((pSH <> nil) And (bNode.HasAttribute('SubHookDesc'))) Then
          end;  //  --  If cbImportDescriptions.Checked Then
          Last_uID := pToken^.uID;  // save the last uID we had in this dynamic function
          __Free_InstallToken(pToken);
        end;
        If cbImportDescriptions.Checked Then begin
          pDF := HM.DynamicFuntionTable.GetDynamicFunction(HookID); // now we get the actual pointer to the new dynamic function
          If ((pDF = nil) Or (HookIDNode.HasAttribute('HookDesc') = False)) Then
            Continue;
          S := HookIDNode.Attributes['HookDesc'];
          idx := __GetHookListIndex(FpHookDescList, HookID);
          If idx > -1 Then begin                       // the description exists somehow
            FpHookDescList.Strings[idx] := S;          // we just update it
            FpHookDescList.Objects[idx] := TObject(pDF);
          end else
            FpHookDescList.AddObject(S, TObject(pDF))  // and register the description as it doesn't existed yet
        end;  //  --  If cbImportDescriptions.Checked Then
      end;  //  --  For i := 0 To aRoot.ChildNodes.Count - 1 Do
      __ReadWrite_Template(ID_HookDesc, taWrite);    // write all Hook descriptions
      __ReadWrite_Template(ID_SubHookDesc, taWrite); // write all SubHook descriptions
    end;  //  --  If ARoot <> nil Then

    // the section in the XML file called CodeGroups saves only the descriptions
    // so if the import of those is turned off, we can skip the whole section
    If cbImportDescriptions.Checked Then begin
      ARoot := XML.DocumentElement.ChildNodes.FindNode('CodeGroups');
      If ARoot <> nil Then begin
        For i := 0 To ARoot.ChildNodes.Count - 1 Do begin
          bNode := aRoot.ChildNodes.Nodes[i];
          j := bNode.Attributes['CodeGroupID'];
          pCG := __GetCodeGroupData(j);
          If pCG = nil Then begin
            pCG := __Create_CodeGroupDataToken;
            FpCodeGroupDataList.Add(pCG);
          end;
          pCG^.nID := bNode.Attributes['CodeGroupID'];
          If bNode.HasAttribute('CodeGroupName') Then
            pCG^.Name := bNode.Attributes['CodeGroupName'];
          If bNode.HasAttribute('CodegroupDesc') Then
            pCG^.Desc := bNode.Attributes['CodegroupDesc'];
          If bNode.HasAttribute('NameResID') Then
            pCG^.NameResID := bNode.Attributes['NameResID'];
          If bNode.HasAttribute('DescResID') Then
            pCG^.DescResID := bNode.Attributes['DescResID'];
          If bNode.HasAttribute('MinSL') Then
            pCG^.MinSL := bNode.Attributes['MinSL'];
        end;  //  --  For i := 0 To ARoot.ChildNodes.Count - 1 Do
        __ReadWrite_Template(ID_CodeGroupDesc, taWrite);
      end;  //  --  If ARoot <> nil Then
    end;  //  --  If cbImportDescriptions.Checked Then
  end;  //  --  If OpenDialog.Execute(Self.ParentWindow) Then
  __FillHookView;
  __FillCodeGroupListView;
end;

procedure TfDFTEdit.DoXML_Export;
begin
  btnXML_ExportClick(nil);
end;

procedure TfDFTEdit.Button2Click(Sender: TObject);
var i : Integer;
begin
  For i := FpHookDescList.Count - 1 DownTo 0 do
    If FpHookDescList.Strings[i] = '' Then
      FpHookDescList.Delete(i);
  __ReadWrite_Template(ID_HookDesc, taWrite);

  For i := FpCodeGroupDataList.Count - 1 downto 0 do
    If PCodeGroupData(FpCodeGroupDataList.Items[i])^.Name = '' Then begin
//      ShowMessage(IntToStr(FpCodeGroupDataList.Count));
      FpCodeGroupDataList.Delete(i);
    end;  //  --  If PCodeGroupData(FpCodeGroupDataList.Items[i])^.nID = nCodeGroupID Then
  __ReadWrite_Template(ID_CodeGroupDesc, taWrite);
end;

procedure TfDFTEdit.__WriteStreamAttribute(Sender: TObject; ID: Integer);
var pCG_Data : PCodeGroupData;
    i : Integer;
begin
  Case FpTemplateStreamer.StreamID Of
    ID_HookDesc      : begin
                         FpTemplateStreamer.WriteInteger(FpHookDescList.Count);
                         For i := 0 To FpHookDescList.Count - 1 Do begin
                           FpTemplateStreamer.WriteInteger(PDynamicFunction(FpHookDescList.Objects[i])^.HookID); 
                           FpTemplateStreamer.WriteString(FpHookDescList.Strings[i]);
                         end;
                       end; // ID_HookDesc
    ID_SubHookDesc   : begin
                         FpTemplateStreamer.WriteInteger( FpSubHookDescList.Count);
                         For i := 0 To FpSubHookDescList.Count - 1 Do begin
                           FpTemplateStreamer.WriteInteger(PSubHook(FpSubHookDescList.Objects[i])^.uID);
                           FpTemplateStreamer.WriteString(FpSubHookDescList.Strings[i]);
                         end;
                       end; // ID_SubHookDesc
    ID_CodeGroupDesc : begin
                         FpTemplateStreamer.WriteInteger(FpCodeGroupDataList.Count);
                         For i := 0 To FpCodeGroupDataList.Count - 1 Do begin
                           pCG_Data := PCodeGroupData(FpCodeGroupDataList.Items[i]);
                           FpTemplateStreamer.WriteInteger(pCG_Data^.nID);
                           FpTemplateStreamer.WriteString(pCG_Data^.Name);
                           FpTemplateStreamer.WriteString(pCG_Data^.Desc);
                           FpTemplateStreamer.WriteCardinal(pCG_Data^.NameResID);
                           FpTemplateStreamer.WriteCardinal(pCG_Data^.DescResID);
                           FpTemplateStreamer.WriteInteger(pCG_Data^.MinSL);
                         end;  //  --  For i := 0 To FpCodeGroupDataList.Count - 1 Do
                       end; // ID_CodeGroupDesc
  end;  //  --  Case FpTemplateStreamer.StreamID Of
  FpTemplateStreamer.Stream.Position := 0;
end;

procedure TfDFTEdit.__ReadStreamAttribute(Sender: TObject; ID: Integer);
var i, nCount, nHookID : Integer;
    pCG_Data : PCodeGroupData;
    pDF_Data : PDynamicFunction;
    pSH_Data : PSubHook;
    S : String;
begin
  {$IFDEF Tracer}
    MAFTracer.Enter('TfDFTEdit.__ReadStreamAttribute');
  {$ENDIF}

  Case FpTemplateStreamer.StreamID Of
    ID_HookDesc      : begin
                         FpTemplateStreamer.ReadInteger(nCount);
                         For i := 1 To nCount Do begin
                           FpTemplateStreamer.ReadInteger(nHookID);
                           FpTemplateStreamer.ReadString(S);
                           If S <> '' Then begin
                             pDF_Data := HM.DynamicFuntionTable.GetDynamicFunction(nHookID);
                             If pDF_Data <> nil Then
                               FpHookDescList.AddObject(S, TObject(pDF_Data));
                           end;  //  --  If S <> '' Then
                         end;  //  --  For i := 1 To nCount Do
                       end; // ID_HookDesc
    ID_SubHookDesc   : begin
                         FpTemplateStreamer.ReadInteger(nCount);
                         For i := 1 To nCount Do begin
                           FpTemplateStreamer.ReadInteger(nHookID); // uID of the SubHook
                           FpTemplateStreamer.ReadString(S);
                           If S <> '' Then begin
                             pSH_Data := HM.DynamicFuntionTable.__GetSubHook_uID(nHookID);
                             If pSH_Data <> nil Then
                               FpSubHookDescList.AddObject(S, TObject(pSH_Data));
                           end;
                         end;
                       end; // ID_SubHookDesc
    ID_CodeGroupDesc : begin
                         FpTemplateStreamer.ReadInteger(nCount);
                         {$IFDEF Tracer}
                           MAFTracer.Log_Integer('Code group count', nCount);
                         {$ENDIF}
                         For i := 1 To nCount Do begin
                           pCG_Data := __Create_CodeGroupDataToken;
                           FpTemplateStreamer.ReadInteger(pCG_Data^.nID);
                           FpTemplateStreamer.ReadString(pCG_Data^.Name);
                           {$IFDEF Tracer}
                             MAFTracer.CheckPoint('Codegroup name = ' + pCG_Data^.Name);
                           {$ENDIF}
                           FpTemplateStreamer.ReadString(pCG_Data^.Desc);
                           FpTemplateStreamer.ReadCardinal(pCG_Data^.NameResID);
                           FpTemplateStreamer.ReadCardinal(pCG_Data^.DescResID);
                           FpTemplateStreamer.ReadInteger(pCG_Data^.MinSL);
                           If ((pCG_Data^.Name = '') And (pCG_Data^.Desc = '')) Then  // filter empty ones out
                             Dispose(pCG_Data)
                           Else
                             FpCodeGroupDataList.Add(pCG_Data);
                         end;
                       end; // ID_CodeGroupDesc
  end;  //  --  Case FpTemplateStreamer.StreamID Of
  {$IFDEF Tracer}
    MAFTracer.Leave;
  {$ENDIF}
end;

end.
