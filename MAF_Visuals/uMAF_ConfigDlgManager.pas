{*******************************************************************************
Name         : uMAF_ConfigDlgManager.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007-2010 by Helge Lange
Info         : HelgeLange@maf-components.com
Date         : 19.04.2007
Last Update  : 08.09.2011
Version      : 1.0.005
Purpose      : put on a form it can create with all items set a complete dynamic
               configuration dialog
Last Changes :

1.0.005 (08.09.2011) -----------------------------------------------------------
- [FIX] now the TreeViewConfigManager uses the ConfigOption coJustNotify
1.0.004 (08.04.2010) -----------------------------------------------------------
- [ADD] new component TmafTreeViewManager added, that can handle multiple levels
        of entries and open dialogs for each level, not only the first child level
        It will also replace the "old" TmafTreeViewConfigManager, which will not
        receive further development and should be replaced by the new and more
        powerfull version TmafTreeViewManager
- [ADD] New event OnItemAdded added which is called when an Item is added to the
        Display Control
- [ADD] ConfigOption coOnlyNotify added that prevents the Controller component
        to open the dialog associated with the entry and just fires the even
        "OnItemClick"
1.0.003 (23.09.2008) -----------------------------------------------------------
- [FIX] fixed a bug in Language change for TmafTreeViewConfigManager, where
        always the Item name was used, also for parent nodes, that should use
        ParentName
1.0.002 (22.04.2007) -----------------------------------------------------------
- [ADD] added events BeforeClear and OnAddItem in TmafListViewManager
- [ADD] added event AfterLoad in TmafCustomConfigDlgManager
- [ADD] automatic language change for TTreeView items
1.0.001 (19.04.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_ConfigDlgManager;

interface

uses Windows, Classes, Forms, Controls, ComCtrls, ExtCtrls, ImgList, Messages,
     SysUtils,
     {$IFDEF PNGimage} PngImageList, {$ENDIF}

     // Modular Application Framework Components units
     uMAF_Core, uMAF_Globals, uMAF_HookClient, uMAF_ResourceClient;

     // ConfigOptions will set up certain behaviour for the components
     // coLoadImages : images for every item will be loaded
     // coResortList : Items will be resorted after loading
     // coJustNotify : the dialogs will not be loaded and displayed in the associated Panel, but just an event will be fired
     // coUseHookToCall : If enabled, the component will not try to call the function on the same Hook, but expects the additional fields HookToCall and SubHookToCall to provide a call vector 
Type TConfigOption = (coLoadImages, coResortList, coJustNotify, coUseHookToCall);
     TConfigOptions = Set Of TConfigOption;

     TItemAdded = procedure(Sender: TObject; Node: TTreeNode) Of Object;
     TItemClick = procedure(Sender: TObject; nHookID, nSubHookID: Integer; aItem: TObject) Of Object;

     TmafCustomConfigDlgManager = class(TComponent)
     private
       FpHookClient : TmafHookClient;
       FpResClient  : TmafResourceClient;
       FpDataList : TList;
       FnHookID : Integer;
       FbAutoLoad : Boolean;
       FConfigOptions : TConfigOptions;
       FpDisplayPanel : TPanel;
       FBeforeLoadDialog : TNotifyEvent;
       FAfterLoadDialog : TNotifyEvent;
       FBeforeFreeDialog : TNotifyEvent;
       FAfterLoad : TNotifyEvent;
       FItemAdded : TItemAdded;
       FItemClick : TItemClick;
       procedure __SetAutoLoad(const Value: Boolean);
       procedure __SetResClient(const Value: TmafResourceClient);
     protected
       FpClientDlg : Pointer;  // the currently loaded dialog
       FbLoading : Boolean;    // startup mode ?
       FnSubHookID : Integer;  // SubHookID for currently loaded dialog
       procedure BCR(nCommand: Integer; QHS: pQHS; var UserParam: Pointer); virtual;
       procedure AEH(nCommand: Integer; QHS: pQHS; var UserParam: Pointer; ErrCode: Integer); virtual;
       procedure ACR(nCommand: Integer; QHS: pQHS; var UserParam: Pointer); virtual;
       function AddItem(pToken: Pointer): TObject; virtual; abstract;
       procedure __Free_DataListToken(var pToken: Pointer); virtual;
       procedure MSG_BeforeFormClose(var Msg: TMessage); message MSG_BEFORE_FORM_CLOSE;
       procedure MSG_FunctionObserver_Add(var Msg: TMessage); message MSG_FUNCTION_OBSERVER_ADD;
       procedure MSG_FunctionObserver_Delete(var Msg: TMessage); message MSG_FUNCTION_OBSERVER_DEL;
       procedure LoadFreeDialog; virtual;
       procedure __OnFunctionObserver(nHookID, nSubHookID: Integer; Action: TFunctionObserverAction); virtual;
       function __GetString(nID: Cardinal): String;
       function __GetImage(nID: Cardinal; IL: TCustomImageList): Integer;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       procedure Load(nSubHookID: Integer = 0); virtual;
       procedure Clear; virtual;
       procedure Select(nSubHookID: Integer); virtual; abstract;
       property DataList : TList read FpDataList;
     published
       property HookClient : TmafHookClient read FpHookClient write FpHookClient;
       property ResClient  : TmafResourceClient read FpResClient write __SetResClient;
       property DisplayPanel : TPanel read FpDisplayPanel write FpDisplayPanel;
       property HookID : Integer read FnHookID write FnHookID default 0;
       property AutoLoad : Boolean read FbAutoLoad write __SetAutoLoad default False;
       property ConfigOptions : TConfigOptions read FConfigOptions write FConfigOptions default [coLoadImages];
       property SubHookID : Integer read FnSubHookID;
       property BeforeLoadDialog : TNotifyEvent read FBeforeLoadDialog write FBeforeLoadDialog;
       property AfterLoadDialog : TNotifyEvent read FAfterLoadDialog write FAfterLoadDialog;
       property BeforeFreeDialog : TNotifyEvent read FBeforeFreeDialog write FBeforeFreeDialog;
       property AfterLoad : TNotifyEvent read FAfterLoad write FAfterLoad;
       property OnItemAdded : TItemAdded read FItemAdded write FItemAdded;
       property OnItemClick : TItemClick read FItemClick write FItemClick;
     end;

     TmafTreeViewConfigManager = class(TmafCustomConfigDlgManager)
     private
       FpTreeView : TTreeView;
       FbAutoExpand : Boolean;
       function TreeFindItem(NodeItem: TTreeNode; Name: String): TTreeNode;
       procedure tvItemChange(Sender: TObject; Node: TTreeNode);
       procedure __SetTreeView(const Value: TTreeView);
     protected
       function AddItem(pToken: Pointer): TObject; Override;
       procedure WMLanguageChange(var Message: TMessage); message WM_LANGUAGE_CHANGE;
       procedure __OnFunctionObserver(nHookID, nSubHookID: Integer; Action: TFunctionObserverAction); override;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       procedure Clear; override;
       procedure Load(nSubHookID: Integer = 0); override;
       procedure Select(nSubHookID: Integer); override;
     published
       property TreeView : TTreeView read FpTreeView write __SetTreeView;
       property AutoExpand : Boolean read FbAutoExpand write FbAutoExpand default True;
     end;

     RTreeItem = packed record
       SubHookID : Integer;
       pItem : TTreeNode;
     end;
     PTreeItem = ^RTreeItem;

     TmafTreeViewManager = class(TmafCustomConfigDlgManager)
     private
       FpTreeView : TTreeView;
       FpTreeList : TList;
       FnHookToCall : Integer;
       FnSubHookToCall : Integer;
       FnOldIndex : Integer;
       procedure __SetTreeView(const Value: TTreeView);
       procedure tvItemChange(Sender: TObject; Node: TTreeNode);
       procedure tvOnClick(Sender: TObject);
       function __Find_DataItem(nSubHookID: Integer): PTreeItem;
     protected
       procedure WMLanguageChange(var Message: TMessage); message WM_LANGUAGE_CHANGE;
       procedure AEH(nCommand: Integer; QHS: pQHS; var UserParam: Pointer; ErrCode: Integer); override;
       procedure __OnFunctionObserver(nHookID, nSubHookID: Integer; Action: TFunctionObserverAction); override;
       procedure __Free_DataListToken(var pToken: Pointer); override;
       function AddItem(pToken: Pointer): TObject; override;
       procedure LoadFreeDialog; override;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       procedure Select(nSubHookID: Integer); override;
       procedure Clear; override;
     published
       property TreeView : TTreeView read FpTreeView write __SetTreeView;
     end;

     TOnAddListItem = procedure(Sender: TObject; Item: TListItem) Of Object;

     TmafListViewManager = class(TmafCustomConfigDlgManager)
     private
       FpListView : TListView;
       FOnAddItem : TOnAddListItem;
       FBeforeClear : TNotifyEvent;
       procedure lvSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
       procedure __SetListView(const Value: TListView);
     protected
       function AddItem(pToken: Pointer): TObject; Override;
     public
       constructor Create(AOwner: TComponent); override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       procedure Clear; override;
       procedure Load(nSubHookID: Integer = 0); override;
       procedure Select(nSubHookID: Integer); override;
     published
       property ListView : TListView read FpListView write __SetListView;
       property OnAddItem : TOnAddListItem read FOnAddItem write FOnAddItem;
       property BeforeClear : TNotifyEvent read FBeforeClear write FBeforeClear;
     end;

     RNavBarToken = packed record
       HookID : Integer;
       SubHookID : Integer;
       NameID : RmafMediaItem;
       ImageID : RmafMediaItem;
       ParentNameID : RmafMediaItem;
       ParentImageID : RmafMediaItem;
     end; // RNavBarToken
     PNavBarToken = ^RNavBarToken;

     // the data for the window come like that from the ModuleController
     RConfigTreeData = packed record
       NameID : RmafMediaItem;
       ImageID : RmafMediaItem;
       ParentNameID : RmafMediaItem;
       ParentImageID : RmafMediaItem;
     end;
     PConfigTreeData = ^RConfigTreeData;

     RTreeViewData = packed record
       SubHookID : Integer;
       NameID : RmafMediaItem;
       ImageID : RmafMediaItem;
       // the parent will not be determined by name and created if necessary,
       // but by the SubHookID. If this is 0, the item will be added in the root,
       // otherwise as child item of an already created Item with the fitting SubHookID.
       // That means, the order for every entry within the DynamicFunction is
       // very important, root items have always be created first  
       ParentSubHookID : Integer;
       HookToCall : Integer;
       SubHookToCall : Integer;
     end;
     PTreeViewData = ^RTreeViewData;

implementation

uses Graphics, uMAF_Tools, dialogs;

{ TmafConfigDlgManager }

procedure TmafCustomConfigDlgManager.Clear;
begin
  While FpDataList.Count > 0 Do begin
    Dispose(PNavBarToken(FpDataList.Items[0]));
    FpDataList.Delete(0);
  end;  //  --  While FpDataList.Count > 0 Do
  FpHookClient.__Delete_FunctionObserver(FnHookID, 0, Self);
end;

constructor TmafCustomConfigDlgManager.Create(AOwner: TComponent);
begin
  inherited;
  FnHookID := 0;
  FnSubHookID := 0;
  FpHookClient := nil;;
  FpResClient  := nil;
  FpDisplayPanel := nil;
  FpClientDlg := nil;
  FbLoading := False;
  FConfigOptions := [coLoadImages];
  FpDataList := TList.Create;
end;

destructor TmafCustomConfigDlgManager.Destroy;
begin
  ResClient := nil;
  FreeAndNil(FpDataList);
  inherited;
end;

procedure TmafCustomConfigDlgManager.Loaded;
begin
  inherited;
  AutoLoad := FbAutoLoad;
end;

procedure TmafCustomConfigDlgManager.LoadFreeDialog;
begin
  If Not Assigned(FpHookClient) Then
    Exit;
  FpHookClient.PushEvents;
  FpHookClient.BeforeCallRouter := BCR;
  FpHookClient.AfterCallRouter := ACR;
  // the ModuleController expects as pChildObj = nil and pParent <> nil to create a window
  // to destroy a Window, pParent and pChildObj have to be <> nil
  FpHookClient.PushVar.AsObject := FpClientDlg;
  FpHookClient.ExecuteHook(FnHookID, FnSubHookID, False, 3);
  FpHookClient.PopEvents;
end;

procedure TmafCustomConfigDlgManager.MSG_BeforeFormClose(var Msg: TMessage);
begin
  If FpClientDlg <> nil Then
    LoadFreeDialog;
  Clear;
end;

procedure TmafCustomConfigDlgManager.MSG_FunctionObserver_Add(var Msg: TMessage);
begin
    __OnFunctionObserver(Msg.WParam, Msg.LParam, foaAdd);
end;

procedure TmafCustomConfigDlgManager.MSG_FunctionObserver_Delete(var Msg: TMessage);
begin
    __OnFunctionObserver(Msg.WParam, Msg.LParam, foaDelete);
end;

procedure TmafCustomConfigDlgManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  If Operation = opRemove Then begin
    If ((AComponent = FpHookClient) And (FpHookClient <> nil)) Then
      If FpClientDlg <> nil Then  // if the HookClient goes, we better free a loaded dialog
        LoadFreeDialog;
    If AComponent = Owner Then
      If FpClientDlg <> nil Then // if the owner form goes, it won't be too late, hopefully ;)
        LoadFreeDialog;
    If ((AComponent = FpResClient) And (Assigned(FpResClient))) Then
      ResClient := nil;
  end;
end;

procedure TmafCustomConfigDlgManager.__Free_DataListToken(var pToken: Pointer);
begin
  If Assigned(pToken) Then
    Dispose(PNavBarToken(pToken));
  pToken := nil;
end;

function TmafCustomConfigDlgManager.__GetImage(nID: Cardinal; IL: TCustomImageList): Integer;
var AGraphic : TGraphic;
    {$IFDEF PNGimage}
    PNG: TPngImageCollectionItem;
    {$ENDIF}
begin
  Result := -1;
  If ((coLoadImages in FConfigOptions) And (IL <> nil) And (FpResClient <> nil)) Then begin
    AGraphic := FpResClient.GetGraphic(nID);
    {$IFDEF PNGimage}
    If IsClass(IL, TPngImageList) Then begin
      PNG := TPngImageList(IL).PngImages.Add;
      Png.PngImage.Assign(AGraphic);
      Result := PNG.Index;
    end else begin
      Result := IL.Add(TBitmap(AGraphic), nil);
    end;
    {$ELSE}
    Result := IL.Add(TBitmap(AGraphic), nil);
    {$ENDIF}
    AGraphic.Free;
  end;
end;

function TmafCustomConfigDlgManager.__GetString(nID: Cardinal): String;
begin
  If Assigned(FpResClient) Then Result := FpResClient.GetString(nID)
                           Else Result := '__ResClient_NOT_SET__';
end;

procedure TmafCustomConfigDlgManager.__OnFunctionObserver(nHookID, nSubHookID: Integer; Action: TFunctionObserverAction);
begin
  If Action = foaAdd Then
    Load(nSubHookID);
end;

procedure TmafCustomConfigDlgManager.__SetAutoLoad(const Value: Boolean);
begin
  FbAutoLoad := Value;
  If ((csDesigning in ComponentState) Or (csLoading in ComponentState)) Then
    Exit;
  If FbAutoLoad Then
    Load;
end;

procedure TmafCustomConfigDlgManager.__SetResClient(const Value: TmafResourceClient);
begin
  If csDesigning in ComponentState Then begin
    FpResClient := Value;
    Exit;
  end;

  If ((Value = nil) And (FpResClient <> nil)) Then
    FpResClient.DeleteNotifyComponent(Self);
  FpResClient := Value;
  If Assigned(FpResClient) Then
    FpResClient.AddNotifyComponent(Self);
end;

procedure TmafCustomConfigDlgManager.Load(nSubHookID: Integer = 0);
begin
  If ((Assigned(FpHookClient)) And (FnHookID > 0)) Then begin
    FbLoading := True;
    FpHookClient.PushEvents;
    FpHookClient.BeforeCallRouter := BCR;
    FpHookClient.AfterExecHook := AEH;
    FpHookClient.ExecuteHook(FnHookID, nSubHookID);
    FpHookClient.PopEvents;
    FbLoading := False;
    FpHookClient.__Add_FunctionObserver(FnHookID, 0, Self);
    If Assigned(FAfterLoad) Then
      FAfterLoad(Self);
  end;  //  --  If ((Assigned(FpHookClient)) And (FnHookID > 0)) Then
end;

procedure TmafCustomConfigDlgManager.AEH(nCommand: Integer; QHS: pQHS; var UserParam: Pointer; ErrCode: Integer);
var pToken : PNavBarToken;
begin
  If QHS^.pChildObj <> nil Then begin
    New(pToken);
    CopyMemory(@pToken^.NameID, QHS^.pChildObj, SizeOf(RConfigTreeData));
    pToken^.HookID := QHS^.HookID;
    pToken^.SubHookID := QHS^.SubHookID;
    AddItem(pToken);
    FpDataList.Add(pToken);
  end;  //  --  If QHS^.pChildObj <> nil Then
end;

procedure TmafCustomConfigDlgManager.BCR(nCommand: Integer; QHS: pQHS; var UserParam: Pointer);
begin
  If nCommand = FnHookID Then begin
    If FbLoading Then begin
      // start up mode, we just want the descriptions
      QHS^.pParent := nil;  // make sure, it's always nil
      Exit;
    end;  //  --  If FbLoading Then
    QHS^.pChildObj := FpHookClient.PopVar.AsObject; // is nil to create a window and <> nil to free the window
    QHS^.FhParentHandle := FpDisplayPanel.Handle; // Client-Area
    QHS^.pParent := FpDisplayPanel;               // the owner
  end;  //  --  If nCommand = FnHookID Then
end;

procedure TmafCustomConfigDlgManager.ACR(nCommand: Integer; QHS: pQHS; var UserParam: Pointer);
begin
  If Assigned(FpClientDlg) Then begin
    // we had a dialog loaded, but it's released now, we delete our reference, too
    FpClientDlg := nil;
    FnSubHookID := 0;
  end else begin
    // no dialog was loaded, we assign the pointer delivered data to our references
    If QHS^.pChildObj = nil Then
      Raise EComponentError.Create('Requested form on SubHookID '+IntToStr(QHS^.SubHookID)+' was not created !');
    FpClientDlg := QHS^.pChildObj;
    FnSubHookID := QHS^.SubHookID;
    TForm(FpClientDlg).ParentWindow := FpDisplayPanel.Handle;
    TForm(FpClientDlg).Parent := FpDisplayPanel;
    TForm(FpClientDlg).BorderStyle := bsNone;
    TForm(FpClientDlg).Align := alClient;
    TForm(FpClientDlg).Show;
    If Assigned(FAfterLoadDialog) Then
      FAfterLoadDialog(Self);
  end;  //  --  If Assigned(FpClientDlg) Then
end;

{ TmafTreeViewConfigManager }

constructor TmafTreeViewConfigManager.Create(AOwner: TComponent);
begin
  inherited;
  FpTreeView := nil;
  FbAutoExpand := True;
end;

destructor TmafTreeViewConfigManager.Destroy;
begin

  inherited;
end;

procedure TmafTreeViewConfigManager.Load(nSubHookID: Integer = 0);
var pNode : TTreeNode;
begin
  If csLoading in ComponentState Then
    Exit;
  inherited;
  If Assigned(FpTreeView) Then begin
    pNode := FpTreeView.Items.GetFirstNode;
    If FbAutoExpand Then begin
      While pNode <> nil Do begin
        pNode.Expand(True);
        pNode := pNode.GetNextSibling;
      end;  //  --  While pNode <> nil Do
    end else begin
      While pNode <> nil Do begin
        pNode.Collapse(True);
        pNode := pNode.GetNextSibling;
      end;  //  --  While pNode <> nil Do
    end;  //  --  If FbAutoExpand Then
  end;  //  --  If Assigned(FpTreeView) Then
end;

procedure TmafTreeViewConfigManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  If Operation = opRemove Then
    If AComponent = FpTreeView Then
      If Assigned(FpClientDlg) Then
        LoadFreeDialog;
  inherited;
end;

procedure TmafTreeViewConfigManager.Select(nSubHookID: Integer);
var i : Integer;
begin
  If Assigned(FpTreeView) Then
    For i := 0 To FpTreeView.Items.Count - 1 Do
    If Assigned(FpTreeView.Items[i].Data) Then
      If PNavBarToken(FpTreeView.Items[i].Data)^.SubHookID = nSubHookID Then begin
        FpTreeView.Selected := FpTreeView.Items[i];
        Break;
      end;
end;

procedure TmafTreeViewConfigManager.Clear;
begin
  inherited;
  If Assigned(FpTreeView) Then
    FpTreeView.Items.Clear;
end;

function TmafTreeViewConfigManager.TreeFindItem(NodeItem: TTreeNode; Name: String): TTreeNode;
begin
  if NodeItem = nil Then NodeItem := FpTreeView.Items.GetFirstNode // this will be rootnode
                    Else NodeItem := NodeItem.GetFirstChild;
  // NodeItem is the first selected item in this level
  // if there are no items in this level, it will be nil
  if (NodeItem <> nil) and (NodeItem.Text <> Name) then
    repeat
      NodeItem := NodeItem.GetNextSibling;
    until (NodeItem = nil) or (NodeItem.Text = Name);
  Result := NodeItem;
end;

function TmafTreeViewConfigManager.AddItem(pToken: Pointer): TObject;
var ThisNode, Node : TTreeNode;
    S : String;
    AGraphic : TGraphic;
    idx : Integer;
    {$IFDEF PNGimage}
    PNG: TPngImageCollectionItem;
    {$ENDIF}
begin
  idx := -1;
  Node := nil;
  // nil = level 0 no item above = root
  // will be tested by TreeFindItem
  S := FpResClient.GetString(PNavBarToken(pToken)^.ParentNameID.MediaID);
  ThisNode := TreeFindItem(Node, S);
  If ThisNode <> nil Then
    Node := ThisNode // Parent schon vorhanden
  Else begin
    Node := FpTreeView.Items.AddChildObject(nil, S, nil);
    If coLoadImages in FConfigOptions Then begin
      If FpTreeView.Images <> nil Then begin
        AGraphic := FpResClient.GetGraphic(PNavBarToken(pToken)^.ParentImageID.MediaID);
        {$IFDEF PNGimage}
          If IsClass(FpTreeView.Images, TPngImageList) Then begin
            PNG := TPngImageList(FpTreeView.Images).PngImages.Add;
            Png.PngImage.Assign(AGraphic);
            idx := PNG.Index;
          end else begin
            idx := FpTreeView.Images.Add(TBitmap(AGraphic), nil);
          end;
        {$ELSE}
          idx := FpTreeView.Images.Add(TBitmap(AGraphic), nil);
        {$ENDIF}
        AGraphic.Free;
        Node.ImageIndex := idx;
      end;
    end else
      Node.ImageIndex := -1;
    Node.SelectedIndex := Node.ImageIndex;
  end;  //  --  If ThisNode <> nil Then

  // jetzt den Eintrag für den Dialog
  S := FpResClient.GetString(PNavBarToken(pToken)^.NameID.MediaID);
  ThisNode := FpTreeView.Items.AddChildObject(Node, S, pToken);
  If coLoadImages in FConfigOptions Then begin
    If FpTreeView.Images <> nil Then begin
      AGraphic := FpResClient.GetGraphic(PNavBarToken(pToken)^.ParentImageID.MediaID);
      {$IFDEF PNGimage}
        If IsClass(FpTreeView.Images, TPngImageList) Then begin
          PNG := TPngImageList(FpTreeView.Images).PngImages.Add;
          Png.PngImage.Assign(AGraphic);
          idx := PNG.Index;
        end else begin
          idx := FpTreeView.Images.Add(TBitmap(AGraphic), nil);
        end;
      {$ELSE}
        idx := FpTreeView.Images.Add(TBitmap(AGraphic), nil);
      {$ENDIF}
      AGraphic.Free;
    end;
    ThisNode.ImageIndex := idx;
  end else
    ThisNode.ImageIndex := -1;
  ThisNode.SelectedIndex := ThisNode.ImageIndex;
  ThisNode.StateIndex := ThisNode.Level + 1;
  If ((coResortList in FConfigOptions) and (Node.Parent <> nil)) Then
    Node.Parent.AlphaSort;
  Result := ThisNode;
end;

procedure TmafTreeViewConfigManager.tvItemChange(Sender: TObject; Node: TTreeNode);
var nNewSubHookID : Integer;
begin
  If csDestroying in FpTreeView.ComponentState Then
    Exit;

  nNewSubHookID := -1;
  If Assigned(Node.Data) Then
    nNewSubHookID := PNavBarToken(Node.Data)^.SubHookID;

  If Node.HasChildren Then
    If Not Node.Expanded Then Node.Expand(True);

  If ((coJustNotify in FConfigOptions) And (Assigned(Node.Data))) Then begin
    If Assigned(FItemClick) Then
      FItemClick(Self, PNavBarToken(Node.Data)^.HookID, PNavBarToken(Node.Data)^.SubHookID, Node);
    Exit;
  end;

  If Assigned(FpClientDlg) Then // we have a dialog loaded ?
    If nNewSubHookID <> FnSubHookID Then begin // does it differ
      If Assigned(FBeforeFreeDialog) Then
        FBeforeFreeDialog(Self);
      LoadFreeDialog;  // we free the current
    end Else
      Exit;  // still the same, we do nothing

  If Not Node.HasChildren Then begin
    FnSubHookID := nNewSubHookID;
    If Assigned(FBeforeLoadDialog) Then
      FBeforeLoadDialog(Self);
    LoadFreeDialog;
  end else begin
    If Assigned(FpClientDlg) Then
      LoadFreeDialog;
  end; // If Not Node.HasChildren Then
end;

procedure TmafTreeViewConfigManager.WMLanguageChange(var Message: TMessage);
var i : Integer;
begin
  If Assigned(FpTreeView) Then
    For i := 0 To FpTreeView.Items.Count - 1 Do
      If FpTreeView.Items[i].Data <> nil Then
        If FpTreeView.Items[i].HasChildren Then
          FpTreeView.Items[i].Text := FpResClient.GetString(PNavBarToken(FpTreeView.Items[i].Data)^.ParentNameID.MediaID)
        Else
          FpTreeView.Items[i].Text := FpResClient.GetString(PNavBarToken(FpTreeView.Items[i].Data)^.NameID.MediaID);
end;

procedure TmafTreeViewConfigManager.__OnFunctionObserver(nHookID, nSubHookID: Integer; Action: TFunctionObserverAction);
var pData : PNavBarToken;
    i, idx : integer;
    aParent : TTreeNode;
begin
  If Action = foaDelete Then begin
    // first we make sure, that the Dialog behind the SubHook being deleted is displayed atm
    // if it is displayed, we change to next parent element or (if there isn't any) set it
    // to nil
    If ((Action = foaDelete) And (Assigned(FpClientDlg)) And (FpTreeview.Selected <> nil)) Then begin
      pData := PNavBarToken(FpTreeView.Selected.Data);
      If pData^.SubHookID = nSubHookID Then
        If FpTreeView.Selected.Parent <> nil Then
          FpTreeView.Selected := FpTreeView.Selected.Parent
        Else
          FpTreeView.Selected := nil;
    end;
    // now we find the element for this entry
    For i := FpTreeView.Items.Count - 1 DownTo 0 Do begin
      pData := FpTreeView.Items[i].Data;
      aParent := FpTreeView.Items[i].Parent;
      If pData = nil Then  // parent entries don't have any data set, we can ignore them for now
        Continue;
      If pData^.SubHookID = nSubHookID Then begin
        idx := DataList.IndexOf(pData);
        FpTreeView.Items.Delete(FpTreeView.Items[i]);
        If Assigned(aParent) Then
          If Not aParent.HasChildren Then
            FpTreeView.Items.Delete(aParent);
        DataList.Delete(idx);
        Dispose(pData);
        Break;
      end;  //  --  If pData^.SubHookID = nSubHookID Then
    end;  //  --  For i := FpTreeView.Items.Count - 1 DownTo 0 Do
  end;  //  --  If Action = foaDelete Then
  inherited;
end;

procedure TmafTreeViewConfigManager.__SetTreeView(const Value: TTreeView);
begin
  FpTreeView := Value;
  If Assigned(FpTreeView) Then
    FpTreeView.OnChange := tvItemChange;
end;

{ TmafListViewManager }

constructor TmafListViewManager.Create(AOwner: TComponent);
begin
  inherited;
  FpListView := nil;
end;

procedure TmafListViewManager.Load(nSubHookID: Integer = 0);
begin
  If csLoading in ComponentState Then
    Exit;
  inherited;
end;

procedure TmafListViewManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  If Operation = opRemove Then
    If AComponent = FpListView Then
      Clear;
  inherited;
end;

procedure TmafListViewManager.Select(nSubHookID: Integer);
var i : Integer;
begin
  If Assigned(FpListView) Then
    For i := 0 To FpListView.Items.Count - 1 Do
      If PNavBarToken(FpListView.Items[i].Data)^.SubHookID = nSubHookID Then begin
        FpListView.Selected := FpListView.Items[i];
        Break;
      end;
end;

procedure TmafListViewManager.lvSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  // stupid TListView fires SelectItem even when its getting trashed.. wtf!
  If csDestroying in FpListView.ComponentState Then
    Exit;
  If ((FpClientDlg = nil) And (Not Selected)) Then
    Exit;

  If Item <> nil Then
    FnSubHookID := PNavBarToken(Item.Data)^.SubHookID;
  If coJustNotify in FConfigOptions Then begin
    If Assigned(FItemClick) Then
      FItemClick(Self, FnHookID, FnSubHookID, Item);
  end else
    LoadFreeDialog;
end;

procedure TmafListViewManager.__SetListView(const Value: TListView);
begin
  FpListView := Value;
  If Assigned(FpListView) Then
    FpListView.OnSelectItem := lvSelectItem;
end;

function TmafListViewManager.AddItem(pToken: Pointer): TObject;
var LI : TListItem;
    IL : TCustomImageList;
    AGraphic : TGraphic;
    idx : Integer;
    {$IFDEF PNGimage}
    PNG: TPngImageCollectionItem;
    {$ENDIF}
begin
  IL := nil;
  Result := nil;
  If coLoadImages in FConfigOptions Then begin
    If FpListView.ViewStyle = vsIcon Then IL := FpListView.LargeImages
                                     Else IL := FpListView.SmallImages;
    If IL = nil Then
      Exclude(FConfigOptions, coLoadImages);
  end;
  If pToken <> nil Then begin
    LI := FpListView.Items.Add;
    LI.Caption := FpResClient.GetString(PNavBarToken(pToken)^.NameID.MediaID);
    If ((coLoadImages in FConfigOptions) And (IL <> nil)) Then begin
      AGraphic := FpResClient.GetGraphic(PNavBarToken(pToken)^.ParentImageID.MediaID);
      {$IFDEF PNGimage}
        If IsClass(IL, TPngImageList) Then begin
          PNG := TPngImageList(IL).PngImages.Add;
          Png.PngImage.Assign(AGraphic);
          idx := PNG.Index;
        end else begin
          idx := IL.Add(TBitmap(AGraphic), nil);
        end;
      {$ELSE}
        idx := IL.Add(TBitmap(AGraphic), nil);
      {$ENDIF}
      AGraphic.Free;
      LI.ImageIndex := idx;
    end Else
      LI.ImageIndex := -1;
    LI.Data := pToken;
    Result := LI;
    If Assigned(FOnAddItem) Then
      FOnAddItem(Self, LI);
  end;
end;

procedure TmafListViewManager.Clear;
begin
  If Assigned(FBeforeClear) THen
    BeforeClear(Self);
  If Assigned(FpClientDlg) Then
    LoadFreeDialog;
  If Assigned(FpListView) Then
    FpListView.Items.Clear;
  inherited;
end;

{ TMafTreeViewManager }

procedure TMafTreeViewManager.AEH(nCommand: Integer; QHS: pQHS; var UserParam: Pointer; ErrCode: Integer);
var pToken : PTreeViewData; // will be saved in TTreeNode.Data
    pItem  : PTreeItem;     // will be saved in FpDataList
begin
  If QHS^.pChildObj <> nil Then begin
    New(pToken);
    New(pItem);
    CopyMemory(@pToken^.NameID, QHS^.pChildObj, SizeOf(RTreeViewData)-4); // -4 for the first interger in our local record
    pToken^.SubHookID := QHS^.SubHookID;
    pItem^.SubHookID := QHS^.SubHookID;
    pItem^.pItem := nil; // no TreeNode assigned yet
    FpDataList.Add(pItem);
    AddItem(pToken);
  end;  //  --  If QHS^.pChildObj <> nil Then
end;

procedure TmafTreeViewManager.Clear;
var i : Integer;
    pItem  : PTreeItem;
begin
  // we don't call inherited as it tries to clean the FpDatList with wrong record type
  For i := FpDataList.Count - 1 DownTo 0 Do begin
    pItem := PTreeItem(FpDataList.Items[i]);
    Dispose(pItem);
    FpDataList.Delete(i);
  end;  //  --  For i := FpDataList.Count - 1 DownTo 0 Do 
end;

constructor TMafTreeViewManager.Create(AOwner: TComponent);
begin
  inherited;
  FnHookToCall := 0;
  FnSubHookToCall := 0;
  FpTreeView := nil;
  FpTreeList := TList.Create;
  FnOldIndex := -1;
end;

destructor TMafTreeViewManager.Destroy;
begin
  FpTreeList.Free;   
  inherited;
end;

procedure TmafTreeViewManager.LoadFreeDialog;
begin
  If Not Assigned(FpHookClient) Then
    Exit;
  FpHookClient.PushEvents;
  FpHookClient.BeforeCallRouter := BCR;
  FpHookClient.AfterCallRouter := ACR;
  // the ModuleController expects as pChildObj = nil and pParent <> nil to create a window
  // to destroy a Window, pParent and pChildObj have to be <> nil
  FpHookClient.PushVar.AsObject := FpClientDlg;
  FpHookClient.ExecuteHook(FnHookToCall, FnSubHookToCall, False, 3);
  FpHookClient.PopEvents;
end;

procedure TMafTreeViewManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  If Operation = opRemove Then
    If ((AComponent = FpTreeView) And (FpClientDlg <> nil)) Then
      LoadFreeDialog;
end;

procedure TMafTreeViewManager.Select(nSubHookID: Integer);
var pItem : PTreeItem;
begin
  pItem := __Find_DataItem(nSubHookID);
  If Assigned(pItem) Then
    FpTreeView.Selected := pItem^.pItem
  Else
    FpTreeView.Selected := nil;
end;

procedure TMafTreeViewManager.tvItemChange(Sender: TObject; Node: TTreeNode);
var nNewSubHookID : Integer;
begin
  Exit;
  If csDestroying in FpTreeView.ComponentState Then
    Exit;

  If Not FpTreeView.Visible Then
    Exit;

  If Node.HasChildren Then
    If Not Node.Expanded Then Node.Expand(True);

  If Node.Data = nil Then
    Exit;

  If coUseHookToCall in FConfigOptions Then begin
    FnHookToCall := PTreeViewData(Node.Data)^.HookToCall;
    nNewSubHookID := PTreeViewData(Node.Data)^.SubHookToCall;
    If FnHookToCall = 0 Then
      Exit;
  end else begin
    FnHookToCall := FnHookID;
    nNewSubHookID := PTreeViewData(Node.Data)^.SubHookID;
  end;

  If coJustNotify in FConfigOptions Then begin
    If Assigned(FItemClick) Then
      FItemClick(Self, FnHookToCall, nNewSubHookID, Node);
    Exit;               // we're done...
  end;  //  --  If coJustNotify in FConfigOptions Then

{  nNewSubHookID := -1;
  If Assigned(Node.Data) Then
    nNewSubHookID := PTreeViewData(Node.Data)^.SubHookID; }

  If Assigned(FpClientDlg) Then // we have a dialog loaded ?
    If nNewSubHookID <> FnSubHookToCall Then begin // does it differ
      If Assigned(FBeforeFreeDialog) Then
        FBeforeFreeDialog(Self);
      LoadFreeDialog;  // we free the current
    end Else
      Exit;  // still the same, we do nothing

  If nNewSubHookID > 0 Then begin
    FnSubHookToCall := nNewSubHookID;
    If Assigned(FBeforeLoadDialog) Then
      FBeforeLoadDialog(Self);
    LoadFreeDialog;
  end else begin
    If Assigned(FpClientDlg) Then
      LoadFreeDialog;
  end; // If nNewSubHookID > 0 Then
end;

procedure TmafTreeViewManager.tvOnClick(Sender: TObject);
var nNewSubHookID : Integer;
    Node : TTreeNode;
begin
  If csDestroying in FpTreeView.ComponentState Then
    Exit;

  If FpTreeView.SelectionCount > 0 Then
    If ((FpTreeView.Selected.Index <> FnOldIndex) And (FpTreeView.Selected.Index > -1)) Then begin
      FnOldIndex := FpTreeView.Selected.Index;
    end else
      Exit;

  Node := FpTreeView.Selected;

  If Node.HasChildren Then
    If Not Node.Expanded Then Node.Expand(True);

  If Node.Data = nil Then
    Exit;

  If coUseHookToCall in FConfigOptions Then begin
    FnHookToCall := PTreeViewData(Node.Data)^.HookToCall;
    nNewSubHookID := PTreeViewData(Node.Data)^.SubHookToCall;
    If FnHookToCall = 0 Then
      Exit;
  end else begin
    FnHookToCall := FnHookID;
    nNewSubHookID := PTreeViewData(Node.Data)^.SubHookID;
  end;

  If coJustNotify in FConfigOptions Then begin
    If Assigned(FItemClick) Then
      FItemClick(Self, FnHookToCall, nNewSubHookID, Node);
    Exit;               // we're done...
  end;  //  --  If coJustNotify in FConfigOptions Then

{  nNewSubHookID := -1;
  If Assigned(Node.Data) Then
    nNewSubHookID := PTreeViewData(Node.Data)^.SubHookID; }

  If Assigned(FpClientDlg) Then // we have a dialog loaded ?
    If nNewSubHookID <> FnSubHookToCall Then begin // does it differ
      If Assigned(FBeforeFreeDialog) Then
        FBeforeFreeDialog(Self);
      LoadFreeDialog;  // we free the current
    end Else
      Exit;  // still the same, we do nothing

  If nNewSubHookID > 0 Then begin
    FnSubHookToCall := nNewSubHookID;
    If Assigned(FBeforeLoadDialog) Then
      FBeforeLoadDialog(Self);
    LoadFreeDialog;
  end else begin
    If Assigned(FpClientDlg) Then
      LoadFreeDialog;
  end; // If nNewSubHookID > 0 Then
end;

procedure TMafTreeViewManager.WMLanguageChange(var Message: TMessage);
var i : Integer;
begin
  For i := 0 To FpDataList.Count - 1 Do
    If Assigned(PTreeItem(FpDataList.Items[i])^.pItem) Then
      PTreeItem(FpDataList.Items[i])^.pItem.Text := __GetString(PTreeViewData(PTreeItem(FpDataList.Items[i])^.pItem.Data)^.NameID.MediaID);
end;

function TMafTreeViewManager.__Find_DataItem(nSubHookID: Integer): PTreeItem;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpDataList.Count - 1 Do
    If PTreeItem(FpDataList.Items[i])^.SubHookID = nSubHookID Then begin
      Result := PTreeItem(FpDataList.Items[i]);
      Break;
    end;
end;

procedure TMafTreeViewManager.__Free_DataListToken(var pToken: Pointer);
begin
  If Assigned(pToken) Then
    Dispose(PTreeItem(pToken));
  pToken := nil;
end;

procedure TMafTreeViewManager.__OnFunctionObserver(nHookID, nSubHookID: Integer; Action: TFunctionObserverAction);
var pItem : PTreeItem;
begin
  If Action = foaDelete Then begin
    If ((Assigned(FpClientDlg)) And (FpTreeView.Selected <> nil)) Then
      If PTreeViewData(FpTreeView.Selected.Data)^.SubHookID = nSubHookID Then begin
        LoadFreeDialog;
        FpTreeView.Selected := nil;
      end;

    // now we find the entry to delete
    pItem := __Find_DataItem(nSubHookID);
    FpDataList.Delete(FpDataList.IndexOf(pItem));
    FpTreeView.Items.Delete(pItem^.pItem);
    Dispose(pItem);
  end;
  If Action = foaAdd Then begin


  end;
end;

procedure TMafTreeViewManager.__SetTreeView(const Value: TTreeView);
begin
  FpTreeView := Value;
  If Assigned(FpTreeView) Then begin
    FpTreeView.OnChange := tvItemChange;
    FpTreeView.OnClick := tvOnClick;
  end;
end;

function TMafTreeViewManager.AddItem(pToken: Pointer): TObject;
var pItem : PTreeItem;
    pRoot, Node : TTreeNode;
begin
  If PTreeViewData(pToken)^.ParentSubHookID > 0 Then begin
    pItem := __Find_DataItem(PTreeViewData(pToken)^.ParentSubHookID);
    If Assigned(pItem) Then pRoot := pItem^.pItem
                       Else pRoot := nil;
  end Else
    pRoot := nil;

  // Now we have the root for the new item and can get the pItem to save the new
  // TTreeNode in it
  pItem := __Find_DataItem(PTreeViewData(pToken)^.SubHookID);
  // adding item
  Node := FpTreeView.Items.AddChildObject(pRoot, __GetString(PTreeViewData(pToken)^.NameID.MediaID), pToken);
  If pItem <> nil Then
    pItem^.pItem := Node;
  // adding an icon, if option is enabled and everything is set up correctly
  Node.ImageIndex := __GetImage(PTreeViewData(pToken)^.ImageID.MediaID, FpTreeView.Images);
  Node.SelectedIndex := Node.ImageIndex;
  If Assigned(FItemAdded) Then
    FItemAdded(Self, Node);
  Result := Node;
end;

end.
