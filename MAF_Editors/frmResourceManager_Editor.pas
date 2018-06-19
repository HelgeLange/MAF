unit frmResourceManager_Editor;

interface

{$I ..\MAF_Base\MAFramework.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes,
  {$IFDEF VER230}
  VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.Menus, VCL.ComCtrls, VCL.CheckLst, VCL.Buttons, VCL.ImgList, System.IniFiles,
  {$ELSE}
  StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs, Menus, ComCtrls, CheckLst,
  Buttons, ImgList, IniFiles,
  {$ENDIF}
  {$IFDEF PNGimage} PNGimage, {$ENDIF}
  // Modular Application Framework Components units
  frmImageData_Edit, uMAF_ResourceManager_Helper, uMAF_ResourceManager,
  frmListView_ColumnFilter, frmStringData_Edit, frmResourceBaseEditorForm,
  uMAF_CustomResource, uMAF_Core, ToolWin;

Type TFillProc = procedure(AType: Integer) Of Object;

  TfRMT_Main = class(TForm)
    SD: TSaveDialog;
    OD: TOpenDialog;
    PageControl1: TPageControl;
    tsFileResource: TTabSheet;
    tsStrings: TTabSheet;
    tsSQL: TTabSheet;
    lvStringRes: TListView;
    lvSQL: TListView;
    ImageList2: TImageList;
    ToolBar1: TToolBar;
    btnNew: TToolButton;
    btnOpen: TToolButton;
    btnSave: TToolButton;
    ToolButton3: TToolButton;
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    ToolButton7: TToolButton;
    btnShowEditor: TToolButton;
    ToolButton9: TToolButton;
    btnCategories: TToolButton;
    btnFileInfo: TToolButton;
    Panel1: TPanel;
    tvDirectory: TTreeView;
    Panel2: TPanel;
    btnFolderDelete: TBitBtn;
    btnFolderAdd: TBitBtn;
    Panel3: TPanel;
    lvFileRes: TListView;
    panFileRes: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Label1: TLabel;
    Panel7: TPanel;
    lFileName: TLabel;
    panStringRes: TPanel;
    panSQLRes: TPanel;
    Panel10: TPanel;
    btnSelectSQLResID: TButton;
    Panel11: TPanel;
    btnSelectStringResID: TButton;
    Panel12: TPanel;
    btnSelectFileResID: TButton;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Label2: TLabel;
    lStringResFileName: TLabel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Label3: TLabel;
    lSQLResFileName: TLabel;
    Panel4: TPanel;
    Image1: TImage;
    btnExport: TButton;
    SDExport: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lvFileResColumnClick(Sender: TObject; Column: TListColumn);

//    procedure FileRes_ShowEditorClick(Sender: TObject);
    procedure FileRes_AddWizardClick(Sender: TObject);

    procedure OpenClick(Sender: TObject);
    procedure NewClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure ShowEditorClick(Sender: TObject);
    procedure CategoriesClick(Sender: TObject);

    procedure StringRes_AddClick(Sender: TObject);

    procedure SQLRes_AddClick(Sender: TObject);

    procedure lvClick(Sender: TObject);
    procedure tsFileResourceShow(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure PageControl1Change(Sender: TObject);
    procedure ODTypeChange(Sender: TObject);
    procedure btnSelectFileResIDClick(Sender: TObject);
    procedure btnFolderAddClick(Sender: TObject);
    procedure tvDirectoryEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvDirectoryDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvDirectoryDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvDirectoryChange(Sender: TObject; Node: TTreeNode);
    procedure btnFolderDeleteClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFileInfoClick(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
  private
    Old_OnLoaded,
    Old_OnClosed,
    Old_OnModified : TOnResourceFileLoaded;
    CurrResource : TmafBaseResourceFile;
    CurrEditor : TfResourceBaseEditorForm;
    CurrListView : TListView;
    CurrLV_FillProc : TFillProc;
    LastSelectedID: Cardinal;
    frmImageEditor : TfImageData_Edit;
    frmSQLEditor,
    frmStringEditor : TfStringData_Editor;
    frmTextFilter,
    frmSizeFilter,
    frmCategoryFilter,
    frmModuleFilter : TfListView_ColumnFilter;
    ATrashNode,
    ARootNode : PFolderNode;
    procedure EnableDisable_Buttons(bEnable: Boolean);

    procedure BaseEditor_CategoryChange(Sender: TObject);
    procedure BaseEditor_FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImageDataEdit_OnActivate(Sender: TObject);

    procedure Resource_OnLoaded(Sender: TObject; ResType: TResourceFileType);
    procedure Resource_OnModified(Sender: TObject; ResType: TResourceFileType);
    procedure __Fill_MediaListView(AType: Integer = 1);
    procedure __Fill_FolderView;
    procedure __Add_SubFolders(pNode: PFolderNode; tvNode: TTreeNode);
    procedure __Fill_StringListView(AType: Integer);
    function LV__AddItem(PageID: Integer; pDesc: PBaseDescriptor): TListItem;
    function __Get_Index(ID: Integer; AList: TStringList): Integer;
    procedure __Create_TreeViewRootNode;
    function __Find_TreeViewNode(nID: Integer; pNode: TTreeNode): TTreeNode;
    procedure ResourceManagerUnknownImageType(Sender: TObject;  Extension: string; aStream: TMemoryStream; var Image: TGraphic);
  public
    bStandAlone : Boolean;
    RM : TmafResourceManager;
    SelectedID : Cardinal;
    procedure Select_StringID(nID: Cardinal);
    procedure Select_SQLID(nID: Cardinal);
  end;

var
  fRMT_Main: TfRMT_Main;

implementation


uses {$IFDEF VER230}
     Vcl.Imaging.jpeg,
     {$ELSE}
     jpeg,
     {$ENDIF}
     frmFilesAdd_Wizard, frmCategoryEdit, frmResourceFileInfo;

{$R *.dfm}

procedure TfRMT_Main.FileRes_AddWizardClick(Sender: TObject);
var frm: TfFilesAdd_Wizard;
begin
  frm := TfFilesAdd_Wizard.Create(Application);
  frm.RM := RM;
  frm.CurrFolderNodeID := PFolderNode(tvDirectory.Selected.Data)^.nID;
  frm.cbCategories.Items.AddStrings(RM.FileResource.Categories);
  frm.cbModules.Items.AddStrings(RM.FileResource.Modules);
  Frm.ShowModal;
  frm.Free;
  __Fill_MediaListView;
end;

procedure TfRMT_Main.SaveClick(Sender: TObject);
begin
  If Assigned(CurrResource) Then
    CurrResource.Save;
end;

procedure TfRMT_Main.CategoriesClick(Sender: TObject);
var frm: TfCategoryEdit;
    i : Integer;
begin
  frm := TfCategoryEdit.Create(Self);
  frm.ListBox1.Items.Assign(CurrResource.Categories);
  frm.ShowModal;
  CurrResource.Categories.Clear;
  For i := 0 To frm.ListBox1.Items.Count - 1 do
    CurrResource.Categories.AddObject(frm.ListBox1.Items.Strings[i], frm.ListBox1.Items.Objects[i]);
  CurrLV_FillProc(PageControl1.TabIndex + 1);
  CurrResource.Modified := True;
  frm.Free;
end;

procedure TfRMT_Main.DeleteClick(Sender: TObject);
var i : Integer;
begin
  If CurrListView.SelCount > 0 Then begin
    For i := 0 To CurrListView.Items.Count - 1 Do
      If CurrListView.Items.Item[i].Selected Then
        CurrResource.Delete(PBaseDescriptor(CurrListView.Items.Item[i].Data)^.ID);
    CurrLV_FillProc(PageControl1.TabIndex + 1);
  end;
end;

procedure TfRMT_Main.EnableDisable_Buttons(bEnable: Boolean);
begin
  btnAdd.Enabled := bEnable;
  If Not bEnable Then begin
    btnDelete.Enabled := bEnable;
    btnFolderDelete.Enabled := bEnable;
  end;
  btnShowEditor.Enabled := bEnable;
  btnCategories.Enabled := bEnable;
  btnFileInfo.Enabled := bEnable;
  btnFolderAdd.Enabled := bEnable;
end;

procedure TfRMT_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  lvFileRes.OnSelectItem := nil;
  RM.OnResourceFileLoaded := Old_OnLoaded;
  RM.OnResourceFileClosed := Old_OnClosed;
  RM.OnResourceFileModified := Old_OnModified;
//  {$IFDEF Standalone}
  If bStandAlone Then
    RM.Free;
//  {$ENDIF}
  FreeAndNil(frmSizeFilter);
  FreeAndNil(frmCategoryFilter);
  FreeAndNil(frmModuleFilter);
  FreeAndNil(frmTextFilter);
end;

procedure TfRMT_Main.FormCreate(Sender: TObject);
begin
  bStandAlone := False;
end;

procedure TfRMT_Main.FormShow(Sender: TObject);
var INIfile : TIniFile;
    S : String;
begin
  ARootNode := nil;
  CurrEditor := nil;

  frmTextFilter := TfListView_ColumnFilter.Create(Self);
  frmTextFilter.FilterID := 1;
  frmTextFilter.FilterMode := fmText;
  frmTextFilter.OnActivate := ImageDataEdit_OnActivate;

  frmSizeFilter := TfListView_ColumnFilter.Create(Self);
  frmSizeFilter.FilterID := 2;
  frmSizeFilter.OnActivate := ImageDataEdit_OnActivate;

  frmCategoryFilter := TfListView_ColumnFilter.Create(Self);
  frmCategoryFilter.FilterID := 3;
  frmCategoryFilter.OnActivate := ImageDataEdit_OnActivate;

  frmModuleFilter := TfListView_ColumnFilter.Create(Self);
  frmModuleFilter.FilterID := 4;
  frmModuleFilter.OnActivate := ImageDataEdit_OnActivate;

  frmImageEditor := nil;
  frmStringEditor := nil;
  frmSQLEditor := nil;
  LastSelectedID := 0;
  EnableDisable_Buttons(False);
//  {$IFDEF Standalone}
  If bStandAlone Then begin
    S := ParamStr(0);
    S := IncludeTrailingPathDelimiter(ExtractFileDir(S)) + 'RM.ini';
    INIfile := TIniFile.Create(S);

    RM := TmafResourceManager.Create(Self);
    RM.FileResource.Options := [froSkinSupport];
    RM.FileResource.SkinFolderRoot := 'IconSkin';
    RM.OnUnknownImageType := ResourceManagerUnknownImageType;
  end;
//  {$ENDIF}

  // save the current event handler
  Old_OnLoaded := RM.OnResourceFileLoaded;
  Old_OnClosed := RM.OnResourceFileClosed;
  Old_OnModified := RM.OnResourceFileModified;

  // set our own event handler
  RM.OnResourceFileModified := Resource_OnModified;
  RM.OnResourceFileLoaded := Resource_OnLoaded;
  RM.OnResourceFileClosed := Resource_OnLoaded;

  PageControl1Change(Self);

  If bStandAlone Then
    RM.FileResource.ResourceFile := INIFile.ReadString('Files', 'FileResource', '');

  If RM.FileResource.Loaded Then begin
    __Fill_FolderView;
    __Fill_MediaListView;
    Resource_OnModified(RM, rftMedia);
  end;

  If bStandAlone Then
    RM.StringResource.ResourceFile := INIFile.ReadString('Files', 'StringResource', '');
  If RM.StringResource.Loaded Then
    __Fill_StringListView(2);
  If bStandAlone Then begin
    RM.SQLResource.ResourceFile := INIFile.ReadString('Files', 'SQLResource', '');
    INIfile.Free;
  end;
  If RM.SQLResource.Loaded Then
    __Fill_StringListView(3);
end;

procedure TfRMT_Main.ImageDataEdit_OnActivate(Sender: TObject);
begin
  If frmSizeFilter <> Sender Then
    frmSizeFilter.Active := False;
  If frmCategoryFilter <> Sender Then
    frmCategoryFilter.Active := False;
  If frmModuleFilter <> Sender Then
    frmModuleFilter.Active := False;
  If frmTextFilter <> Sender Then
   frmTextFilter.Active := False;
  CurrLV_FillProc(PageControl1.TabIndex + 1);
end;

procedure TfRMT_Main.BaseEditor_CategoryChange(Sender: TObject);
var i, idx : Integer;
    LI : TListItem;
begin
  Case PageControl1.ActivePageIndex Of
    0 : begin
          If Sender = CurrEditor.cbCategory Then idx := 2
                                            Else idx := 3;
        end;
    1 : begin
          If Sender = CurrEditor.cbCategory Then idx := 1
                                            Else idx := 2;
        end;
    Else Exit;
  end;
  If ((CurrEditor = nil) or (CurrListView = nil) Or (CurrResource = nil))  Then
    Exit;

  If CurrEditor.MultiSelected Then begin
    For i := 0 To CurrListView.Items.Count - 1 Do
      If CurrListView.Items.Item[i].Checked Then begin
        LI := CurrListView.Items.Item[i];
        Case TComboBox(Sender).Tag Of
          123 : PBaseDescriptor(LI.Data)^.CategoryID := Integer(TComboBox(Sender).Items.Objects[TComboBox(Sender).ItemIndex]);
          321 : PBaseDescriptor(LI.Data)^.ModuleID := Integer(TComboBox(Sender).Items.Objects[TComboBox(Sender).ItemIndex]);
        end;
        LI.SubItems.Strings[idx] := TComboBox(Sender).Text;
        CurrResource.Modified := True;
      end;
    Exit;
  end;

  If CurrListView.Selected <> nil Then begin
    Case TComboBox(Sender).Tag Of
      123 : PBaseDescriptor(CurrListView.Selected.Data)^.CategoryID := Integer(TComboBox(Sender).Items.Objects[TComboBox(Sender).ItemIndex]);
      321 : PBaseDescriptor(CurrListView.Selected.Data)^.ModuleID := Integer(TComboBox(Sender).Items.Objects[TComboBox(Sender).ItemIndex]);
    end;
    CurrListView.Selected.SubItems.Strings[idx] := TComboBox(Sender).Text;
    CurrResource.Modified := True;
  end;
end;

procedure TfRMT_Main.BaseEditor_FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  Case PageControl1.ActivePageIndex Of
    0 : frmImageEditor := nil;
    1 : frmStringEditor := nil;
    2 : frmSQLEditor := nil;
  end;  //  --  Case PageControl1.ActivePageIndex Of
  CurrEditor := nil;
end;

procedure TfRMT_Main.btnFileInfoClick(Sender: TObject);
var frm : TfResourceFileInfo;
begin
  frm := TfResourceFileInfo.Create(Self);
  try
    frm.Resource := CurrResource;
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfRMT_Main.btnFolderAddClick(Sender: TObject);
var nParentID : Integer;
    aNode : PFolderNode;
begin
  If tvDirectory.Selected = nil Then
    tvDirectory.Selected := tvDirectory.Items.GetFirstNode;
  nParentID := PFolderNode(tvDirectory.Selected.Data)^.nID;
  aNode := RM.FileResource.CreateFolder(nParentID);
  tvDirectory.Items.AddChildObject(tvDirectory.Selected, aNode^.NodeName, aNode);
  tvDirectory.Selected.Expand(True);
end;

procedure MoveChildren(aNode, pNode: PFolderNode);
var i : Integer;
begin
  If pNode = nil Then
    Exit;

  For i := 0 To pNode^.FpDataList.Count - 1 do begin
    aNode^.FpDataList.Add(pNode^.FpDataList.Items[i]);
    PFileResourceDescriptor(pNode^.FpDataList.Items[i])^.FolderNode := -1;
  end;
  For i := 0 To pNode^.FpChildren.Count - 1 Do
    MoveChildren(aNode, PFolderNode(pNode^.FpChildren.Items[i]));
end;

procedure TfRMT_Main.btnFolderDeleteClick(Sender: TObject);
var aNode : TTreeNode;
    pNode : PFolderNode;
    i : Integer;
begin
  If tvDirectory.Selected <> nil Then
    If tvDirectory.Selected <> tvDirectory.Items.GetFirstNode Then begin
      pNode := PFolderNode(tvDirectory.Selected.Data);
      If pNode^.FpChildren.Count > 0 Then begin
        For i := 0 To pNode^.FpChildren.Count - 1 do
          MoveChildren(ARootNode, PFolderNode(pNode^.FpChildren.Items[i]));   // move it internally
        For i := 0 To pNode^.FpDataList.Count - 1 do begin
          ARootNode^.FpDataList.Add(pNode^.FpDataList.Items[i]); // move to root node entry
          PFileResourceDescriptor(pNode^.FpDataList.Items[i])^.FolderNode := -1;
        end;
      end;
      RM.FileResource.DeleteFolder(pNode^.nID);

      aNode := tvDirectory.Selected;
      tvDirectory.Items.Delete(aNode);
      tvDirectory.Selected := aNode.Parent;
    end;
end;

procedure TfRMT_Main.btnSelectFileResIDClick(Sender: TObject);
begin
  If CurrListView.Selected <> nil Then
    SelectedID := PBaseDescriptor(CurrListView.Selected.Data)^.ID
  Else
    SelectedID := 0;
  If CurrResource.Modified Then
    If MessageDlg('Save the changes ?', mtConfirmation, [mbYes,mbNo], 0) = mrYes Then
      CurrResource.Save;
  Close;
end;

procedure TfRMT_Main.btnExportClick(Sender: TObject);
var aStream : TMemoryStream;
begin
  aStream := RM.FileResource.GetFileStream(PFileResourceDescriptor(lvFileRes.Selected.Data)^.ResID);
  if SDExport.Execute then
    aStream.SaveToFile(SDExport.FileName);
end;

procedure TfRMT_Main.lvChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  If CurrEditor <> nil Then
    CurrEditor.MultiSelected := (CurrListView.SelCount > 1);
end;

procedure TfRMT_Main.lvClick(Sender: TObject);
begin
  frmSizeFilter.Visible := False;
  frmCategoryFilter.Visible := False;
  frmModuleFilter.Visible := False;
  frmTextFilter.Visible := False;
end;

procedure TfRMT_Main.lvFileResColumnClick(Sender: TObject; Column: TListColumn);
var i : Integer;
    pData : PFileResourceDescriptor;
    S : String;
begin
  If Column.Tag = 0 Then
    Exit;

  Case Column.Tag Of
    1 : begin
          frmSizeFilter.Visible := False;
          frmCategoryFilter.Visible := False;
          frmModuleFilter.Visible := False;
          If frmTextFilter.Visible Then begin
            frmTextFilter.Hide;
            Exit;
          end;

          frmTextFilter.Show;
        end;
    2 : begin
          frmTextFilter.Visible := False;
          frmCategoryFilter.Visible := False;
          frmModuleFilter.Visible := False;
          If frmSizeFilter.Visible Then begin
            frmSizeFilter.Hide;
            Exit;
          end;

          frmSizeFilter.clbFilterItems.Items.Clear;
          For i := 0 To CurrResource.Data.Count - 1 Do begin
            pData := PFileResourceDescriptor(RM.FileResource.Data.Items[i]);
            S := IntToStr(pData^.ResX) + ' x ' + IntToStr(pData^.ResY);
            If frmSizeFilter.clbFilterItems.Items.IndexOf(S) = -1 Then
              frmSizeFilter.clbFilterItems.Items.Add(S);
          end;
          frmSizeFilter.Show;
        end;
    3 : begin
          frmTextFilter.Visible := False;
          frmSizeFilter.Visible := False;
          frmModuleFilter.Visible := False;
          If frmCategoryFilter.Visible Then begin
            frmCategoryFilter.Hide;
            Exit;
          end;

          frmCategoryFilter.clbFilterItems.Items.Clear;
          frmCategoryFilter.clbFilterItems.Items.AddStrings(CurrResource.Categories);
          frmCategoryFilter.Show;
        end;
    4 : begin
          frmTextFilter.Visible := False;
          frmSizeFilter.Visible := False;
          frmCategoryFilter.Visible := False;
          If frmModuleFilter.Visible Then begin
            frmModuleFilter.Hide;
            Exit;
          end;

          frmModuleFilter.clbFilterItems.Items.Clear;
          frmModuleFilter.clbFilterItems.Items.AddStrings(CurrResource.Modules);
          frmModuleFilter.Show;
        end;
  end;
end;

procedure TfRMT_Main.lvSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var EditorFrm : TfResourceBaseEditorForm;
    Image : TGraphic;
begin
  Case PageControl1.ActivePageIndex Of
    0 : begin
          If LastSelectedID > 0 Then
            RM.FileResource.Release(LastSelectedID);
          EditorFrm := frmImageEditor;
          btnShowEditor.Enabled := (lvFileRes.Selected <> nil);
          If ((Selected) And (Item <> nil) And (Item.Data <> nil)) Then begin
            Image := RM.FileResource.Get(PFileResourceDescriptor(Item.Data));
            If Assigned(Image) Then begin
              Image1.Picture.Assign(Image);
              Image.Free;
            end else
              If Image1.Picture.Graphic <> nil Then begin
                Image1.Picture.Graphic.Free;
                Image1.Picture.Graphic := nil;
              end;
          end;
        end;
    1 : begin
          EditorFrm := frmStringEditor;
          btnShowEditor.Enabled := (lvStringRes.Selected <> nil);
        end;
    2 : begin
          EditorFrm := frmSQLEditor;
          btnShowEditor.Enabled := (lvSQL.Selected <> nil);
        end;
    Else Exit;
  end;
  If ((Selected) And (Item <> nil) And (Item.Data <> nil)) Then
    If EditorFrm <> nil Then
      EditorFrm.LI := Item;
  btnDelete.Enabled := CurrListView.SelCount > 0;
end;

procedure TfRMT_Main.Resource_OnModified(Sender: TObject; ResType: TResourceFileType);
begin
  Case ResType Of
    rftMedia  : btnSave.Enabled := RM.FileResource.Modified;
    rftString : btnSave.Enabled := RM.StringResource.Modified;
    rftSQL    : btnSave.Enabled := RM.SQLResource.Modified;
  end;

end;

procedure TfRMT_Main.PageControl1Change(Sender: TObject);
begin
  If CurrEditor <> nil Then
    ShowEditorClick(Self);  // to close the Editor
  frmSizeFilter.Hide;
  frmCategoryFilter.Hide;
  frmModuleFilter.Hide;
  EnableDisable_Buttons(False);  // disable all buttons that won't work without an open resource file
  Case PageControl1.ActivePage.Tag Of
    1 : begin
          CurrResource := RM.FileResource;
          CurrEditor := frmImageEditor;
          CurrListView := lvFileRes;
          CurrLV_FillProc := __Fill_MediaListView;
          btnAdd.OnClick := FileRes_AddWizardClick;
          If RM.FileResource.Loaded Then
            EnableDisable_Buttons(True);
          btnShowEditor.Enabled := (lvFileRes.Selected <> nil);
        end;
    2 : begin
          CurrResource := RM.StringResource;
          CurrEditor := frmStringEditor;
          CurrListView := lvStringRes;
          CurrLV_FillProc := __Fill_StringListView;
          btnAdd.OnClick := StringRes_AddClick;
          If RM.StringResource.Loaded Then
            EnableDisable_Buttons(True);
          btnShowEditor.Enabled := (lvStringRes.Selected <> nil);
        end;
    3 : begin
          CurrResource := RM.SQLResource;
          CurrEditor := frmSQLEditor;
          CurrListView := lvSQL;
          btnAdd.OnClick := SQLRes_AddClick;
          If RM.SQLResource.Loaded Then
            EnableDisable_Buttons(True);
          btnShowEditor.Enabled := (lvSQL.Selected <> nil);
        end;
  end;
  frmSizeFilter.LV := CurrListView;
  frmCategoryFilter.LV := CurrListView;
  frmModuleFilter.LV := CurrListView;
  frmTextFilter.LV := CurrListView;
end;

procedure TfRMT_Main.PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  Case PageControl1.ActivePage.Tag Of
    1 : begin
          If RM.FileResource.Modified Then
            If MessageDlg('Changes were made to the resource file, save them now ?', mtConfirmation, [mbYes,mbNo], 0) = mrYes Then
              RM.FileResource.Save;
        end;
    2 : begin
          If RM.StringResource.Modified Then
            If MessageDlg('Changes were made to the string resource file, save them now ?', mtConfirmation, [mbYes,mbNo], 0) = mrYes Then
              RM.StringResource.Save;
        end;
    3 : begin
          If RM.SQLResource.Modified Then
            If MessageDlg('Changes were made to the resource file, save them now ?', mtConfirmation, [mbYes,mbNo], 0) = mrYes Then
              RM.SQLResource.Save;
        end;
  end;
end;

procedure TfRMT_Main.ResourceManagerUnknownImageType(Sender: TObject; Extension: string; aStream: TMemoryStream; var Image: TGraphic);
begin
  {$IFDEF PNGimage}
  If Extension = '.png' Then
    {$IFDEF Unicode}
    Image := TPNGimage.Create;
    {$ELSE}
    Image := TPNGObject.Create;
    {$ENDIF}
  {$ENDIF}
end;

procedure TfRMT_Main.Resource_OnLoaded(Sender: TObject; ResType: TResourceFileType);
begin
  Case ResType Of
    rftMedia  : begin
                  If RM.FileResource.Loaded = False Then begin
                    If ARootNode <> nil Then
                      Dispose(ARootNode);
                    ARootNode := nil;
                  end;
                  lFileName.Caption := RM.FileResource.ResourceFile;
                  __Fill_FolderView;
                  __Fill_MediaListView;
                  EnableDisable_Buttons(True);
                  btnShowEditor.Enabled := False;
                end;
    rftString : begin
                  lStringResFileName.Caption := RM.StringResource.ResourceFile;
                  __Fill_StringListView(2);
                  EnableDisable_Buttons(True);
                  btnShowEditor.Enabled := False;
                end;
    rftSQL    : begin
                  lSQLResFileName.Caption := RM.SQLResource.ResourceFile;
                  __Fill_StringListView(3);
                  EnableDisable_Buttons(True);
                  btnShowEditor.Enabled := False;
                end;
  end;
end;

procedure TfRMT_Main.OpenClick(Sender: TObject);
begin
  CurrResource.ResourceFile := ''; // closes a previously opened file
  CurrListView.Items.Clear;
  OD.FileName := '*.maf';
  If OD.Execute{$IFDEF D9+}(WindowHandle){$ENDIF} Then
    CurrResource.ResourceFile := OD.FileName;
end;

function TfRMT_Main.LV__AddItem(PageID: Integer; pDesc: PBaseDescriptor): TListItem;
var i : Integer;
    LV : TListView;
    Resource : TmafBaseResourceFile;
begin
  Result := nil;
  Case PageID Of
    1 : begin
          lv := lvFileRes;
          Resource := RM.FileResource;
        end;
    2 : begin
          lv := lvStringRes;
          Resource := RM.StringResource;
        end;
    3 : begin
          lv := lvSQL;
          Resource := RM.SQLResource;
        end;
    Else Exit;
  end;
  Result := LV.Items.Add;
  Result.Caption := IntToStr(pDesc^.ID);
  If pDesc^.ID = SelectedID Then
    LV.Selected := Result;
  Case PageID Of
    1 : begin
          Result.SubItems.Add(PFileResourceDescriptor(pDesc)^.ResName);
          Result.SubItems.Add(IntToStr(PFileResourceDescriptor(pDesc)^.ResX) + ' x ' + IntToStr(PFileResourceDescriptor(pDesc)^.ResY));
        end;
    2 : Result.SubItems.Add(String(PChar(PStringResourceDescriptor(pDesc)^.pData)));
    3 : Result.SubItems.Add(String(PChar(PStringResourceDescriptor(pDesc)^.pData)));
  end;
  i := __Get_Index(pDesc^.CategoryID, Resource.Categories);
  If i > -1 Then Result.SubItems.Add(Resource.Categories.Strings[i])
            Else Result.SubItems.Add('None');
  i := __Get_Index(pDesc^.ModuleID, Resource.Modules);
  If i > -1 Then Result.SubItems.Add(Resource.Modules.Strings[i])
            Else Result.SubItems.Add('Shared Item');
  Result.Data := pDesc;
end;

procedure TfRMT_Main.ODTypeChange(Sender: TObject);
begin
  Case OD.FilterIndex Of
    1 : OD.FileName := '*.maf';
    2 : OD.FileName := '*.*';
  end;
end;

procedure TfRMT_Main.__Create_TreeViewRootNode;
var i : Integer;
begin
  If ARootNode <> nil Then begin
    Dispose(ARootNode);
    ARootNode := nil;
  end;

  ARootNode := __Create_FolderNode(-1);
  ARootNode^.nID := -1;
  ARootNode^.NodeName := '$Root';
  // now adding all items without a parent node into the root
  For i := 0 To RM.FileResource.Data.Count - 1 Do
    If PFileResourceDescriptor(RM.FileResource.Data.Items[i])^.FolderNode = -1 Then
      ARootNode^.FpDataList.Add(RM.FileResource.Data.Items[i]);

  ATrashNode := __Create_FolderNode(-1);
  ATrashNode^.nID := -1;
  ATrashNode^.NodeName := '$Trash';
  For i := 0 To RM.FileResource.TrashCan.Count - 1 Do

  tvDirectory.OnChange := nil;
  tvDirectory.Selected := tvDirectory.Items.AddChildObject(nil, '$Root', ARootNode);
  tvDirectory.Items.AddChildObject(nil, '$Trash', ATrashNode);
  tvDirectory.OnChange := tvDirectoryChange;
end;

procedure TfRMT_Main.__Add_SubFolders(pNode: PFolderNode; tvNode: TTreeNode);
var i : Integer;
    aChildNode : TTreeNode;
    aNode : PFolderNode;
begin
  If pNode = nil Then
    Exit;
  For i := 0 To pNode^.FpChildren.Count - 1 Do begin
    aNode := PFolderNode(pNode^.FpChildren.Items[i]);
    aChildNode := tvDirectory.Items.AddChildObject(tvNode, aNode^.NodeName, aNode);
    aChildNode.Data := aNode;
    If aNode^.FpChildren.Count > 0 Then
      __Add_SubFolders(aNode, aChildNode);
  end;
end;

procedure TfRMT_Main.__Fill_FolderView;
var i : Integer;
    tvNode : TTreeNode;
    aNode : PFolderNode;
begin
  If RM = nil Then
    Exit;
  If RM.FileResource.Loaded = False Then
    Exit;
  tvDirectory.Items.Clear;
  __Create_TreeViewRootNode;
  For i := 0 To RM.FileResource.Folders.Count - 1 Do begin
    aNode := PFolderNode(RM.FileResource.Folders.Items[i]);
    tvNode := tvDirectory.Items.AddChildObject(tvDirectory.Selected, aNode^.NodeName, aNode);
    tvNode.Data := aNode;
    If aNode^.FpChildren.Count > 0 Then
      __Add_SubFolders(aNode, tvNode);
  end;
end;

procedure TfRMT_Main.__Fill_MediaListView;
var i : Integer;
    pDesc : PFileResourceDescriptor;
    bInsert : Boolean;
    ActiveFilter : TfListView_ColumnFilter;
    S : String;
    aNode : PFolderNode;
    tvNode : TTreeNode;
begin
  If tvDirectory.Selected = nil Then
    Exit;

  If frmSizeFilter.Active Then
    ActiveFilter := frmSizeFilter
  Else
    If frmCategoryFilter.Active Then
      ActiveFilter := frmCategoryFilter
    Else
      If frmModuleFilter.Active Then
        ActiveFilter := frmModuleFilter
      Else
        If frmTextFilter.Active Then
          ActiveFilter := frmTextFilter
        Else
          ActiveFilter := nil;

  lvFileRes.Selected := nil;
  lvFileRes.Items.BeginUpdate;
  lvFileRes.Items.Clear;


  aNode := PFolderNode(tvDirectory.Selected.Data);
  For i := 0 To aNode^.FpDataList.Count - 1 Do begin
    pDesc := PFileResourceDescriptor(aNode^.FpDataList.Items[i]);
      bInsert := False;
      If ActiveFilter <> nil Then begin
        Case ActiveFilter.Column.Tag Of
          1 : begin
                If ActiveFilter.FilterString <> '' Then
                  bInsert := (Pos(ActiveFilter.FilterString, pDesc^.ResName) > 0)
                Else
                  bInsert := True;
              end;
          2 : begin  // image size filter
                S := IntToStr(pDesc^.ResX) + ' x ' + IntToStr(pDesc^.ResY);
                bInsert := (ActiveFilter.CurrentFilterList.IndexOf(S) > -1);
              end;
          3 : bInsert := ActiveFilter.IsActiveFilterItem(pDesc^.CategoryID);
          4 : bInsert := ActiveFilter.IsActiveFilterItem(pDesc^.ModuleID);
        end;  //  --  Case ActiveFilter.Column.Tag Of
      end else  //  --  If ActiveFilter <> nil Then
        bInsert := True;

    If bInsert Then
      LV__AddItem(1, PBaseDescriptor(pDesc));
  end;  //  --  For i := 0 To RM.FileResource.Data.Count - 1 Do
  lvFileRes.Items.EndUpdate;
  pDesc := PFileResourceDescriptor(RM.FileResource.GetDescriptor(SelectedID));
  If Assigned(pDesc) Then begin
    tvNode := __Find_TreeViewNode(pDesc^.FolderNode, nil);
    If Assigned(tvNode) Then begin
      tvDirectory.Selected := tvNode;
      tvNode.Expanded := True;
      While tvNode.Parent <> nil Do begin
        tvNode.Parent.Expand(True);
        tvNode := tvNode.Parent;
      end;
      lvFileRes.Selected.MakeVisible(False);
    end;
  end;
end;

function TfRMT_Main.__Find_TreeViewNode(nID: Integer; pNode: TTreeNode): TTreeNode;
var aNode: TTreeNode;
begin
  Result := nil;
  If pNode = nil Then aNode := tvDirectory.Items.GetFirstNode
                 Else aNode := pNode.GetFirstChild;

  While aNode <> nil Do begin
    If PFolderNode(aNode.Data)^.nID = nID Then begin
      Result := aNode;
      Break;
    end;
    aNode := aNode.GetNext;
  end;
end;

procedure TfRMT_Main.__Fill_StringListView(AType: Integer);
var pDesc: PStringResourceDescriptor;
    i : Integer;
    bInsert : Boolean;
    ActiveFilter : TfListView_ColumnFilter;
    LV : TListView;
    Res : TmafBaseResourceFile;
begin
  Case AType Of
    2 : begin
          LV := lvStringRes;
          Res := RM.StringResource;
        end;
    3 : begin
          lv := lvSQL;
          Res := RM.SQLResource;
        end;
    Else Exit;
  end;
  lv.Selected := nil;
  lv.Items.BeginUpdate;
  lv.Items.Clear;
  If frmCategoryFilter.Active Then
    ActiveFilter := frmCategoryFilter
  Else
    If frmModuleFilter.Active Then
      ActiveFilter := frmModuleFilter
    Else
      If frmTextFilter.Active Then
        ActiveFilter := frmTextFilter
      Else
        ActiveFilter := nil;

  For i := 0 To Res.Data.Count - 1 Do begin
    pDesc := Res.Data.Items[i];
    bInsert := False;
    If ActiveFilter <> nil Then begin
      Case ActiveFilter.Column.Tag Of
        1 : begin
              If ActiveFilter.FilterString <> '' Then
                bInsert := (Pos(ActiveFilter.FilterString, String(PChar(pDesc^.pData))) > 0)
              Else
                bInsert := True;
            end;
        3 : bInsert := ActiveFilter.IsActiveFilterItem(pDesc^.CategoryID);
        4 : bInsert := ActiveFilter.IsActiveFilterItem(pDesc^.ModuleID);
      end;
    end else
      bInsert := True;

    If bInsert Then
      LV__AddItem(AType, PBaseDescriptor(pDesc));
  end;
  lv.Items.EndUpdate;
  If lv.Selected <> nil Then
    lv.Selected.MakeVisible(False);
end;

function TfRMT_Main.__Get_Index(ID: Integer; AList: TStringList): Integer;
var i : Integer;
begin
  Result := -1;
  For i := 0 To AList.Count - 1 Do
    If Integer(AList.Objects[i]) = ID Then begin
      Result := i;
      Break;
    end;
end;

procedure TfRMT_Main.tsFileResourceShow(Sender: TObject);
begin
  If Assigned(RM) Then
    lFileName.Caption := RM.FileResource.ResourceFile
  else
    lFileName.Caption := '';
end;

procedure TfRMT_Main.tvDirectoryChange(Sender: TObject; Node: TTreeNode);
begin
  If csDestroying in tvDirectory.ComponentState Then
    Exit;

  __Fill_MediaListView;
  btnFolderDelete.Enabled := ((tvDirectory.Selected <> tvDirectory.Items.GetFirstNode) {And (PFolderNode(tvDirectory.Selected.Data)^.)});
end;

procedure TfRMT_Main.tvDirectoryDragDrop(Sender, Source: TObject; X, Y: Integer);
var AnItem: TTreeNode;
    HT: THitTests;
    i : Integer;
begin
  If lvFileRes.Selected = nil Then
    Exit;

  HT := tvDirectory.GetHitTestInfoAt(X, Y);
  AnItem := tvDirectory.GetNodeAt(X, Y);
  If (HT - [htOnItem, htOnIcon, htNowhere, htOnIndent] <> HT) Then begin
    If (htOnItem in HT) or (htOnIcon in HT) Then begin
      If lvFileRes.SelCount > 0 Then begin
        For i := 0 To lvFileRes.Items.Count - 1 Do
          If lvFileRes.Items.Item[i].Selected Then
            RM.FileResource.MoveToFolder(PFileResourceDescriptor(lvFileRes.Items.Item[i].Data), PFolderNode(AnItem.Data));
//            PFileResourceDescriptor(lvFileRes.Items.Item[i].Data)^.FolderNode := PFolderNode(AnItem.Data)^.nID;
        For i := lvFileRes.Items.Count - 1 DownTo 0 Do
          If lvFileRes.Items.Item[i].Selected Then
            lvFileRes.Items.Delete(lvFileRes.Items.Item[i].Index);
      end;
      lvFileRes.Selected := nil;
    end;
  end;

end;

procedure TfRMT_Main.tvDirectoryDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lvFileRes);
end;

procedure TfRMT_Main.tvDirectoryEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  If Node <> nil Then begin
    PFolderNode(Node.Data)^.NodeName := S;
    RM.FileResource.Modified := True;
  end;  //  --  If Node <> nil Then 
end;

procedure TfRMT_Main.SQLRes_AddClick(Sender: TObject);
var pDesc, pDesc2 : PStringResourceDescriptor;
begin
  pDesc := RM.SQLResource.CreateEmpty;
  RM.SQLResource.Add(PBaseDescriptor(pDesc));
  pDesc2 := PStringResourceDescriptor(RM.SQLResource.GetDescriptor(pDesc^.StringID));
  Dispose(pDesc);
  lvSQL.Selected := nil;
  lvSQL.Selected := LV__AddItem(3, PBaseDescriptor(pDesc2));
  lvSQL.Selected.MakeVisible(False);
  If CurrEditor = nil Then
    ShowEditorClick(nil);
end;

procedure TfRMT_Main.StringRes_AddClick(Sender: TObject);
var pDesc, pDesc2 : PStringResourceDescriptor;
begin
  pDesc := RM.StringResource.CreateEmpty;
  RM.StringResource.Add(PBaseDescriptor(pDesc));
  pDesc2 := PStringResourceDescriptor(RM.StringResource.GetDescriptor(pDesc^.StringID));
  Dispose(pDesc);
  lvStringRes.Selected := nil;
  lvStringRes.Selected := LV__AddItem(2, PBaseDescriptor(pDesc2));
  lvStringRes.Selected.MakeVisible(False);
  If CurrEditor = nil Then
    ShowEditorClick(nil);
end;

procedure TfRMT_Main.ToolButton13Click(Sender: TObject);
begin
  Close;
end;

procedure TfRMT_Main.NewClick(Sender: TObject);
begin
  If SD.Execute{$IFDEF D9+}(WindowHandle){$ENDIF} Then
    CurrResource.CreateResource(SD.FileName);
end;

procedure TfRMT_Main.ShowEditorClick(Sender: TObject);
begin
  If CurrEditor = nil Then begin
//    ImageList1.GetBitmap(1, btnShowEditor.Glyph);
    Case PageControl1.ActivePageIndex Of
      0 : begin
            frmImageEditor := TfImageData_Edit.Create(Self);
            CurrEditor := frmImageEditor;
          end;
      1 : begin
            frmStringEditor := TfStringData_Editor.Create(Self);
            frmStringEditor.CurrResource := RM.StringResource;
            CurrEditor := frmStringEditor;
          end;
      2 : begin
            frmSQLEditor := TfStringData_Editor.Create(Self);
            frmSQLEditor.CurrResource := RM.SQLResource;
            CurrEditor := frmSQLEditor;
          end;
    end;

    CurrEditor.RM := RM;
    CurrEditor.cbCategory.Items.AddStrings(CurrResource.Categories);
    CurrEditor.cbModule.Items.AddStrings(CurrResource.Modules);
    CurrEditor.btnClose.OnClick := ShowEditorClick;
    CurrEditor.cbCategory.OnChange := BaseEditor_CategoryChange;
    CurrEditor.cbModule.OnChange := BaseEditor_CategoryChange;
    CurrEditor.OnClose := BaseEditor_FormClose;
    CurrEditor.CurrListView := CurrListView;
    CurrEditor.Show;
    If CurrListView.Selected <> nil Then begin
      CurrEditor.LI := CurrListView.Selected;
    end Else begin
      CurrEditor.LI := nil;
    end;

  end else begin
    If Sender <> nil Then begin
//      ImageList1.GetBitmap(0, btnShowEditor.Glyph);
      CurrEditor.Free;
      CurrEditor := nil;
      frmImageEditor := nil;
      frmStringEditor := nil;
      frmSQLEditor := nil;
    end;  //  --  If Sender <> nil Then
  end;
end;

procedure TfRMT_Main.Select_SQLID(nID: Cardinal);
var i: Integer;
begin
  For i := 0 To lvSQL.Items.Count - 1 Do
    If lvSQL.Items[i].Caption = IntToStr(nID) Then begin
      lvSQL.Items[i].Selected := true;
      lvSQL.Scroll(0, i * Abs(lvSQL.Font.Height));
      lvSQL.SetFocus;
      Break;
    end;
end;

procedure TfRMT_Main.Select_StringID(nID: Cardinal);
var i: Integer;
begin
  For i := 0 To lvStringRes.Items.Count - 1 Do
    If lvStringRes.Items[i].Caption = IntToStr(nID) Then begin
      lvStringRes.Items[i].Selected := true;
      lvStringRes.Scroll(0, i * Abs(lvStringRes.Font.Height));
      lvStringRes.SetFocus;
      Break;
    end;
end;

end.
