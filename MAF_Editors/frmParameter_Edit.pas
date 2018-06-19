unit frmParameter_Edit;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     uMAF_Core, uMAF_HookClient, uMAF_Parameters;

type
  TfParameter_Edit = class(TForm)
    lbVariables: TListBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    btnNew: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    eName: TEdit;
    eValue: TEdit;
    cbType: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    cbParamType: TComboBox;
    Label5: TLabel;
    procedure lbVariablesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
  private
    procedure __Switch_EditMode(bEdit: Boolean);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  public
    AComponent : TmafParameter;
    procedure __Fill_ListView;
  end;

var
  fParameter_Edit: TfParameter_Edit;

implementation

{$R *.dfm}

procedure TfParameter_Edit.btnDeleteClick(Sender: TObject);
var aVar: TmafVariable;
    i : Integer;
begin
  If lbVariables.ItemIndex > -1 Then begin
    aVar := TmafVariable(lbVariables.Items.Objects[lbVariables.ItemIndex]);
    AComponent.Delete(aVar.Index);
    lbVariables.Items.Delete(lbVariables.ItemIndex);
    lbVariables.ItemIndex := -1;
    lbVariablesClick(nil);
  end;
end;

procedure TfParameter_Edit.btnEditClick(Sender: TObject);
begin
  __Switch_EditMode(True);
  cbType.Enabled := False;
end;

procedure TfParameter_Edit.btnNewClick(Sender: TObject);
begin
  __Switch_EditMode(True);
  cbType.Enabled := True;
end;

procedure TfParameter_Edit.btnCancelClick(Sender: TObject);
begin
  __Switch_EditMode(False);
  lbVariables.ItemIndex := -1;
end;

procedure TfParameter_Edit.btnSaveClick(Sender: TObject);
var aVarType : TVarType;
    i : Integer;
    aVar: TmafVariable;
begin
  If eName.Text <> '' Then begin
    For i := 0 To AComponent.Count - 1 Do
      If UpperCase(AComponent.Params[i].VarName) = UpperCase(eName.Text) Then begin
        MessageDlg('A variable with the same name already exists !', mtError, [mbOk], 0);
        Exit;
      end;

    aVar := nil;

    __Switch_EditMode(False);
    Case cbType.ItemIndex Of
      0 : aVarType := vtString;
      1 : aVarType := vtInteger;
      2 : aVarType := vtDateTime;
      3 : aVarType := vtBoolean;
      4 : aVarType := vtPointer;
      5 : aVarType := vtObject;
      6 : aVarType := vtCallback;
    end;  //  --  Case cbType.ItemIndex Of

    If Sender = btnNew Then begin
      i := AComponent.Add(eName.Text, aVarType);
      aVar := AComponent.Params[i];
      lbVariables.ItemIndex := lbVariables.Items.AddObject(eName.Text, aVar);
    end else
      aVar := TmafVariable(lbVariables.Items.Objects[lbVariables.ItemIndex]);

    Case cbParamType.ItemIndex Of
      0 : aVar.ParamType := vptIn;
      1 : aVar.ParamType := vptOut;
    end;

    If eValue.Text <> '' Then begin
      Try
        aVar.AsString := eValue.Text;
      Except
        aVar.AsString := '';
      end;
    end;
  end;
end;

procedure TfParameter_Edit.cbTypeChange(Sender: TObject);
begin
  Case cbType.ItemIndex Of
    0, 1, 2, 3 : eValue.Enabled := True;
    4, 5, 6    : begin
                   eValue.Enabled := False;
                   eValue.Text := '';
                 end;
  end;
end;

procedure TfParameter_Edit.FormShow(Sender: TObject);
begin
  __Fill_ListView;
end;

procedure TfParameter_Edit.lbVariablesClick(Sender: TObject);
var aVar: TmafVariable;
begin
  eName.Text := '';
  cbType.ItemIndex := 0;
  eValue.Text := '';
  btnEdit.Enabled := (lbVariables.ItemIndex > -1);
  btnDelete.Enabled := (lbVariables.ItemIndex > -1);
  If lbVariables.ItemIndex > -1 Then begin
    aVar := TmafVariable(lbVariables.Items.Objects[lbVariables.ItemIndex]);
    If Assigned(aVar) Then begin
      eName.Text := aVar.VarName;
      case aVar.ParamType of
        vptIn  : cbParamType.ItemIndex := 0;
        vptOut : cbParamType.ItemIndex := 1;
      end;
      Case aVar.VarType Of
        vtString   : begin
                       eValue.Enabled := True;
                       eValue.Text := aVar.Value;
                       cbType.ItemIndex := 0;
                     end;
        vtInteger  : begin
                       eValue.Enabled := True;
                       eValue.Text := aVar.Value;
                       cbType.ItemIndex := 1;
                     end;
        vtPointer  : begin
                       eValue.Enabled := False;
                       eValue.Text := '';
                       cbType.ItemIndex := 4;
                     end;
        vtObject   : begin
                       eValue.Enabled := False;
                       eValue.Text := '';
                       cbType.ItemIndex := 5;
                     end;
        vtBoolean  : begin
                       eValue.Enabled := True;
                       eValue.Text := aVar.AsString;
                       cbType.ItemIndex := 3;
                     end;
        vtDateTime : begin
                       eValue.Enabled := True;
                       eValue.Text := aVar.AsString;
                       cbType.ItemIndex := 2;
                     end;
        vtCallback : begin
                       eValue.Enabled := False;
                       eValue.Text := '';
                       cbType.ItemIndex := 6;
                     end;

      end;
    end;
  end;
end;

procedure TfParameter_Edit.__Fill_ListView;
var i : Integer;
    aVar: TmafVariable;
begin
  If Assigned(AComponent) Then begin
    For i := 0 To AComponent.Count - 1 Do begin
      aVar := TmafVariable(AComponent.Params[i]);
      lbVariables.Items.AddObject(aVar.VarName, aVar);
    end;

  end;
end;

procedure TfParameter_Edit.__Switch_EditMode(bEdit: Boolean);
begin
  GroupBox1.Enabled := bEdit;
  btnDelete.Visible := Not bEdit;
  lbVariables.Enabled := Not bEdit;
  If bEdit Then begin
    btnEdit.Caption := 'Cancel';
    btnEdit.OnClick := btnCancelClick;
    btnEdit.Enabled := True;
    btnNew.Caption := 'Save';
    btnNew.OnClick := btnSaveClick;
  end else begin
    btnEdit.Caption := 'Edit';
    btnEdit.OnClick := btnEditClick;
    btnNew.Caption := 'New';
    btnNew.OnClick := btnNewClick;
  end;
end;

end.
