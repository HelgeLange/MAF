unit frmFilesAdd_Wizard;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.Menus, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ExtCtrls, VCL.StdCtrls,
     {$ELSE}
     Menus, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
     {$ENDIF}
     uMAF_ResourceManager_Helper, uMAF_ResourceManager;

{$I ..\MAF_Base\MAFramework.inc}

type
  TmafListBox = class(TListBox)

  end;

  TfFilesAdd_Wizard = class(TForm)
    btnAddSelected: TButton;
    ListBox1: TListBox;
    btnClose: TButton;
    btnAdd: TButton;
    btnDelete: TButton;
    OD: TOpenDialog;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    lFileSize: TLabel;
    lHeight: TLabel;
    Label3: TLabel;
    lWidth: TLabel;
    Label5: TLabel;
    cbCategories: TComboBox;
    cbModules: TComboBox;
    Label2: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnAddSelectedClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure ListBox1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  private
    OldLBWindowProc: TWndMethod;
    procedure WMDROPFILES(var Msg: TMessage);  message WM_DROPFILES;
    procedure LBWindowProc(var Message: TMessage);
  public
    CurrFolderNodeID : Integer;
    RM : TmafResourceManager;
  end;

var
  fFilesAdd_Wizard: TfFilesAdd_Wizard;

implementation

uses ShellAPI;

{$R *.dfm}

procedure TfFilesAdd_Wizard.btnAddClick(Sender: TObject);
//var pDesc : PFileResourceDescriptor;
begin
  OD.Filter := GraphicFilter(TGraphic);
  If OD.Execute{$IFDEF D9+}(WindowHandle){$ENDIF} Then begin
    ListBox1.Items.AddStrings(OD.Files);
//    pDesc := __Create_FileResourceDescriptor(OD.FileName);
//    RM.FileResource.Add(PBaseDescriptor(pDesc));
//    __Free_FileResourceDescriptor(pDesc);
  end;
end;

procedure TfFilesAdd_Wizard.btnAddSelectedClick(Sender: TObject);
var i, idx, cID, mID : Integer;
    pDesc : PFileResourceDescriptor;
begin
  If cbCategories.ItemIndex > -1 Then begin
    idx := RM.FileResource.Categories.IndexOf(cbCategories.Text);
    cID := Integer(RM.FileResource.Categories.Objects[idx]);
  end else
    cID := 0;
  If cID = -1 Then
    cID := 0;

  If cbModules.ItemIndex > -1 Then begin
    idx := RM.FileResource.Modules.IndexOf(cbModules.Text);
    mID := Integer(RM.FileResource.Modules.Objects[idx]);
  end else
    mID := 0;
  If mID = -1 Then
    mID := 0;

  For i := 0 To ListBox1.Items.Count - 1 Do begin
    pDesc := __Create_FileResourceDescriptor(ListBox1.Items.Strings[i]);
    pDesc^.ResID := RM.FileResource.HighID + 1;
    pDesc^.CategoryID := cID;
    pDesc^.ModuleID := mID;
    pDesc^.FolderNode := CurrFolderNodeID;
    RM.FileResource.Add(PBaseDescriptor(pDesc));
    __Free_FileResourceDescriptor(pDesc);
  end;  //  --  For i := 0 To ListBox1.Items.Count - 1 Do
  Close;
end;

procedure TfFilesAdd_Wizard.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfFilesAdd_Wizard.btnDeleteClick(Sender: TObject);
begin
  If ListBox1.SelCount > 0 Then
    ListBox1.DeleteSelected;
end;

procedure TfFilesAdd_Wizard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfFilesAdd_Wizard.FormCreate(Sender: TObject);
begin
  OldLBWindowProc := ListBox1.WindowProc; // store defualt WindowProc
  ListBox1.WindowProc := LBWindowProc; // replace default WindowProc
  DragAcceptFiles(ListBox1.Handle, True); // now ListBox1 accept dropped files
end;

procedure TfFilesAdd_Wizard.FormDestroy(Sender: TObject);
begin
  ListBox1.WindowProc := OldLBWindowProc;
  DragAcceptFiles(ListBox1.Handle, False);
end;

procedure TfFilesAdd_Wizard.WMDROPFILES(var Msg: TMessage);
var pcFileName: PChar;
    i, iSize, iFileCount: integer;
begin
  pcFileName := ''; // to avoid compiler warning message
  iFileCount := DragQueryFile(Msg.wParam, $FFFFFFFF, pcFileName, 255);
  for i := 0 to iFileCount - 1 do
  begin
    iSize := DragQueryFile(Msg.wParam, i, nil, 0) + 1;
    pcFileName := StrAlloc(iSize);
    DragQueryFile(Msg.wParam, i, pcFileName, iSize);
    If FileExists(pcFileName) Then
      ListBox1.Items.Add(pcFileName);
    StrDispose(pcFileName);
  end;
  DragFinish(Msg.wParam);
end;

procedure TfFilesAdd_Wizard.LBWindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_DROPFILES then
    WMDROPFILES(Message); // handle WM_DROPFILES message
  OldLBWindowProc(Message);
end;

procedure TfFilesAdd_Wizard.ListBox1Click(Sender: TObject);
var F : File;
    S : String;
begin
  Image1.Visible := False;
  If ListBox1.ItemIndex > -1 Then begin
    S := ExtractFileExt(ListBox1.Items.Strings[ListBox1.ItemIndex]);
    OD.Filter := GraphicFilter(TGraphic);
    If Pos(S, OD.Filter) > 0 Then begin
      Image1.Picture.LoadFromFile(ListBox1.Items.Strings[ListBox1.ItemIndex]);
      Image1.Stretch := ((Image1.Picture.Height > 128) or (Image1.Picture.Width > 128));

      Visible := True;
      FileMode := 0;  // read only
      AssignFile(F, ListBox1.Items.Strings[ListBox1.ItemIndex]);
      Reset(F, 1);
      lFileSize.Caption := IntToStr(FileSize(F));
      CloseFile(F);
      lHeight.Caption := IntToStr(Image1.Picture.Height);
      lWidth.Caption := IntToStr(Image1.Picture.Width);
    end;
  end;
end;

procedure TfFilesAdd_Wizard.ListBox1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

end.
