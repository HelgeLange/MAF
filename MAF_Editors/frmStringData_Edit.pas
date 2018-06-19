unit frmStringData_Edit;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.CheckLst, VCL.ComCtrls,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs, CheckLst, ComCtrls,
     {$ENDIF}
     uMAF_ResourceManager, uMAF_ResourceManager_Helper, frmResourceBaseEditorForm;

type
  TfStringData_Editor = class(TfResourceBaseEditorForm)
    Memo1: TMemo;
    Label1: TLabel;
    btnAddNext: TButton;
    edStringID: TEdit;
    lStringID: TLabel;
    procedure edStringIDExit(Sender: TObject);
    procedure btnAddNextClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
  protected
    procedure __SetMultiSelected(const Value: Boolean); override;
    procedure __SetDataID(const Value: Cardinal); override;
    procedure __SetLI(const Value: TListItem); override;
  public
    CurrResource : TStringResource;
  end;

implementation

{$R *.dfm}

uses uMAF_Tools;

{ TfStringData_Editor }

procedure TfStringData_Editor.btnAddNextClick(Sender: TObject);
var pDesc : PStringResourceDescriptor;
    pBaseDesc : PbaseDescriptor;
    i : Integer;
    NewLI : TListItem;
begin
  i := CurrDesc^.ID;
  Repeat
    Inc(i);
    pBaseDesc := CurrResource.GetDescriptor(i);
  Until pBaseDesc = nil;

  pDesc := CurrResource.CreateEmpty(i); // it creates only an empty string without adding it to the StringList internally
  pDesc^.CategoryID := CurrDesc^.CategoryID;
  pDesc^.ModuleID := CurrDesc^.ModuleID;
  CurrResource.Add(PBaseDescriptor(pDesc)); // now we add it
  If CurrListView <> nil Then begin
    NewLI := CurrListView.Items.Insert(CurrListView.Selected.Index + 1);
    NewLI.Data := CurrResource.GetDescriptor(pDesc^.StringID);
    NewLI.Caption := IntToStr(pDesc^.StringID);
    NewLI.SubItems.Add('');
    NewLI.SubItems.Add(cbCategory.Text);
    NewLI.SubItems.Add(cbModule.Text);
    DataID := pDesc^.StringID;
    CurrListView.Selected := nil;
    CurrListView.Selected := NewLI;
    CurrListView.Selected.MakeVisible(False);
  end;
  Dispose(pDesc);
end;

procedure TfStringData_Editor.btnCloseClick(Sender: TObject);
begin
  edStringIDExit(edStringID);
  edStringIDExit(Memo1);
  inherited;

end;

procedure TfStringData_Editor.edStringIDExit(Sender: TObject);
var nID : Cardinal;
//    pDesc : PStringResourceDescriptor;
begin
  If CurrDesc = nil Then
    Exit;

  If Sender = edStringID Then begin
    nID := StrToIntDef(edStringID.Text, 0);
    If nID > 0 Then
      If ((CurrDesc^.ID <> nID) And (CurrResource.Get(nID) = nil)) Then begin
        CurrDesc^.ID := nID;
        LI.Caption := edStringID.Text;
        CurrResource.Modified := True;
      end;
  end;

  If Sender = Memo1 Then
    If PChar(PStringResourceDescriptor(CurrDesc)^.pData) <> Memo1.Lines.Text Then begin
      LI.Data := CurrResource.Update(PStringResourceDescriptor(CurrDesc), Memo1.Lines.Text);
      LI.SubItems.Strings[0] := Memo1.Lines.Text;
      CurrResource.Modified := True;
    end;
end;

procedure TfStringData_Editor.__SetDataID(const Value: Cardinal);
begin
  inherited;
  edStringID.Text := IntToStr(Value);
  CurrDesc := CurrResource.GetDescriptor(Value);
  If CurrDesc <> nil Then begin
    Memo1.Lines.Text := PStringResourceDescriptor(CurrDesc)^.pData;
    cbCategory.ItemIndex := __GetIndex(CurrDesc^.CategoryID, cbCategory.Items);
    cbModule.ItemIndex := __GetIndex(CurrDesc^.ModuleID, cbModule.Items);
  end;
end;

procedure TfStringData_Editor.__SetLI(const Value: TListItem);
begin
  inherited;
  If Assigned(CurrDesc) Then
    DataID := CurrDesc^.ID;
end;

procedure TfStringData_Editor.__SetMultiSelected(const Value: Boolean);
begin
  inherited __SetMultiSelected(Value);
  edStringID.Enabled := Not Value;
  Memo1.Enabled := Not Value;
end;

end.
