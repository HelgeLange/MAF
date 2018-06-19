unit frmFileDB_Viewer;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ComCtrls,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs, ComCtrls,
     {$ENDIF}
     uMAF_Globals, uMAF_ResourceManager_Helper, uMAF_CustomResource, uMAF_FileDB;

type
  TfFileDB_Viewer = class(TForm)
    tvDirectory: TTreeView;
    lvFileRes: TListView;
    btnDelete: TButton;
    procedure tvDirectoryChange(Sender: TObject; Node: TTreeNode);
    procedure FormShow(Sender: TObject);
    procedure lvFileResSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnDeleteClick(Sender: TObject);
  private
    ATrashNode,
    ARootNode : PFolderNode;
    procedure __Create_TreeViewRootNode;
    procedure __Add_SubFolders(pNode: PFolderNode; tvNode: TTreeNode);
    procedure __Fill_FolderView;
    procedure __Fill_FileView;
  public
    FpFileDB : TmafFileDB;
  end;

var
  fFileDB_Viewer: TfFileDB_Viewer;

implementation

{$R *.dfm}

{ TForm1 }

procedure TfFileDB_Viewer.btnDeleteClick(Sender: TObject);
var pDesc : PFileResourceDescriptor;
begin
  If lvFileRes.Selected <> nil Then begin
    pDesc := PFileResourceDescriptor(lvFileRes.Selected.Data);
    FpFileDB.FileResource.Delete(pDesc^.ResID);
    FpFileDB.Commit;
    __Fill_FileView;
  end;
end;

procedure TfFileDB_Viewer.FormShow(Sender: TObject);
begin
  __Fill_FolderView;
end;

procedure TfFileDB_Viewer.lvFileResSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnDelete.Enabled := Selected;
end;

procedure TfFileDB_Viewer.tvDirectoryChange(Sender: TObject; Node: TTreeNode);
begin
  If csDestroying in tvDirectory.ComponentState Then
    Exit;

  __Fill_FileView;
end;

procedure TfFileDB_Viewer.__Add_SubFolders(pNode: PFolderNode; tvNode: TTreeNode);
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

procedure TfFileDB_Viewer.__Create_TreeViewRootNode;
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
  For i := 0 To FpFileDB.FileResource.Data.Count - 1 Do
    If PFileResourceDescriptor(FpFileDB.FileResource.Data.Items[i])^.FolderNode = -1 Then
      ARootNode^.FpDataList.Add(FpFileDB.FileResource.Data.Items[i]);

  ATrashNode := __Create_FolderNode(-1);
  ATrashNode^.nID := -1;
  ATrashNode^.NodeName := '$Trash';
  For i := 0 To FpFileDB.FileResource.TrashCan.Count - 1 Do

  tvDirectory.OnChange := nil;
  tvDirectory.Selected := tvDirectory.Items.AddChildObject(nil, '$Root', ARootNode);
  tvDirectory.Items.AddChildObject(nil, '$Trash', ATrashNode);
  tvDirectory.OnChange := tvDirectoryChange;
end;

procedure TfFileDB_Viewer.__Fill_FileView;
var i : Integer;
    pDesc : PFileResourceDescriptor;
//    bInsert : Boolean;
//    S : String;
    aNode : PFolderNode;
//    tvNode : TTreeNode;
    LI : TListItem;
begin
  lvFileRes.Selected := nil;
  lvFileRes.Items.BeginUpdate;
  lvFileres.Items.Clear;
  aNode := PFolderNode(tvDirectory.Selected.Data);
  For i := 0 To aNode^.FpDataList.Count - 1 Do begin
    pDesc := PFileResourceDescriptor(aNode^.FpDataList.Items[i]);
    LI := lvFileRes.Items.Add;
    LI.Caption := IntToStr(pDesc^.ResID);
    LI.SubItems.Add(pDesc^.ResName);
    LI.SubItems.Add(IntToStr(pDesc^.nLength));
    LI.Data := pDesc;
  end;
  lvFileRes.Items.EndUpdate;
end;

procedure TfFileDB_Viewer.__Fill_FolderView;
var i : Integer;
    tvNode : TTreeNode;
    aNode : PFolderNode;
begin
  If FpFileDB = nil Then
    Exit;
  If FpFileDB.FileResource.Loaded = False Then
    Exit;
  tvDirectory.Items.Clear;
  __Create_TreeViewRootNode;
  For i := 0 To FpFileDB.FileResource.Folders.Count - 1 Do begin
    aNode := PFolderNode(FpFileDB.FileResource.Folders.Items[i]);
    tvNode := tvDirectory.Items.AddChildObject(tvDirectory.Selected, aNode^.NodeName, aNode);
    tvNode.Data := aNode;
    If aNode^.FpChildren.Count > 0 Then
      __Add_SubFolders(aNode, tvNode);
  end;
end;

end.
