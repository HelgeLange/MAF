unit frmImageData_Edit;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ComCtrls,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs, ComCtrls,
     {$ENDIF}
     uMAF_ResourceManager, uMAF_ResourceManager_Helper,
     frmResourceBaseEditorForm;

type
  TfImageData_Edit = class(TfResourceBaseEditorForm) {fResourceBaseEditor}
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edFileName: TEdit;
    Label6: TLabel;
    edResID: TEdit;
    lHeight: TLabel;
    lWidth: TLabel;
    procedure edResIDExit(Sender: TObject);
  private
  protected
    procedure __SetLI(const Value: TListItem); override;
    procedure __SetDataID(const Value: Cardinal); override;
    procedure __SetMultiSelected(const Value: Boolean); override;
  public
  end;

implementation

{$R *.dfm}

{ TfImageData_Edit }

procedure TfImageData_Edit.edResIDExit(Sender: TObject);
var nID : Cardinal;
begin
  If CurrDesc = nil Then
    Exit;

  If Sender = edResID Then begin
    nID := StrToIntDef(edResID.Text, 0);
    If nID > 0 Then
      If CurrDesc^.ID <> nID Then begin
        CurrDesc^.ID := nID;
        LI.Caption := IntToStr(nID);
        RM.FileResource.Modified := True;
      end;
  end;

  If Sender = edFileName Then
    If PFileResourceDescriptor(CurrDesc)^.ResName <> edFileName.Text Then begin
      PFileResourceDescriptor(CurrDesc)^.ResName := edFileName.Text;
      RM.FileResource.Modified := True;
    end;
end;

procedure TfImageData_Edit.__SetDataID(const Value: Cardinal);
var Image : TGraphic;
    aEvt : TNotifyEvent;
begin
  inherited;
  aEvt := cbCategory.OnChange;
  cbCategory.OnChange := nil;
  If RM <> nil Then begin
    Image := RM.FileResource.Get(PFileResourceDescriptor(CurrDesc));
    edResID.Text := IntToStr(DataID);
    If Assigned(CurrDesc) Then begin
      edFileName.Text := PFileResourceDescriptor(CurrDesc)^.ResName;
      cbCategory.ItemIndex := __GetIndex(CurrDesc^.CategoryID, cbCategory.Items);
      cbModule.ItemIndex := __GetIndex(CurrDesc^.ModuleID, cbModule.Items);
    end;
    If Assigned(Image) Then begin
      Image1.Picture.Assign(Image);
      lHeight.Caption := IntToStr(Image.Height);
      lWidth.Caption := IntToStr(Image.Width);
      If PFileResourceDescriptor(CurrDesc)^.ResX <> Image.Height Then begin
        PFileResourceDescriptor(CurrDesc)^.ResX := Image.Height;
        RM.FileResource.Modified := True;
      end;
      If PFileResourceDescriptor(CurrDesc)^.ResY <> Image.Width Then begin
        PFileResourceDescriptor(CurrDesc)^.ResY := Image.Width;
        RM.FileResource.Modified := True;
      end;
      Image.Free;
      edResID.Text := IntToStr(DataID);
      If Assigned(CurrDesc) Then begin
        edFileName.Text := PFileResourceDescriptor(CurrDesc)^.ResName;
        cbCategory.ItemIndex := __GetIndex(CurrDesc^.CategoryID, cbCategory.Items);
        cbModule.ItemIndex := __GetIndex(CurrDesc^.ModuleID, cbModule.Items);
      end;
    end else begin
      If Image1.Picture.Graphic <> nil Then begin
        Image1.Picture.Graphic.Free;
        Image1.Picture.Graphic := nil;
      end;
      lHeight.Caption := '0';
      lWidth.Caption := '0';
    end;
  end;
  cbCategory.OnChange := aEvt;
end;

procedure TfImageData_Edit.__SetLI(const Value: TListItem);
begin
  inherited;
  If Assigned(CurrDesc) Then
    DataID := CurrDesc^.ID
  Else
    DataID := 0;
end;

procedure TfImageData_Edit.__SetMultiSelected(const Value: Boolean);
begin
  inherited __SetMultiSelected(Value);
  edFileName.Enabled := Not Value;
  edResID.Enabled := Not Value;
end;

end.
