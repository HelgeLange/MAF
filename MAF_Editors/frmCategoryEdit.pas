unit frmCategoryEdit;

interface

uses Windows, SysUtils, Variants, Classes, 
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     Messages;

type
  TEditMode = (emShow, emEdit, emNew);

  TfCategoryEdit = class(TForm)
    ListBox1: TListBox;
    Edit1: TEdit;
    btnDelete: TButton;
    btnEdit: TButton;
    btnAdd: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    nHighID : Integer;
    FMode : TEditMode;
    procedure SwitchMode(aMode: TEditMode);
    function __GetHighID: Integer;
  public
    property HighID : Integer read __GetHighID;
  end;

var
  fCategoryEdit: TfCategoryEdit;

implementation

{$R *.dfm}

procedure TfCategoryEdit.btnAddClick(Sender: TObject);
begin
  SwitchMode(emNew);
end;

procedure TfCategoryEdit.btnCancelClick(Sender: TObject);
begin
  SwitchMode(emShow);
end;

procedure TfCategoryEdit.btnDeleteClick(Sender: TObject);
begin
  If ListBox1.ItemIndex > -1 Then
    ListBox1.Items.Delete(ListBox1.ItemIndex);
end;

procedure TfCategoryEdit.btnEditClick(Sender: TObject);
begin
  Edit1.Tag := ListBox1.ItemIndex; // item to edit
  Edit1.Text := ListBox1.Items.Strings[ListBox1.ItemIndex];
  SwitchMode(emEdit);
end;

procedure TfCategoryEdit.btnSaveClick(Sender: TObject);
begin
  case FMode of
    emEdit: ListBox1.Items.Strings[Edit1.Tag] := Edit1.Text;
    emNew: ListBox1.Items.AddObject(Edit1.Text, TObject(HighID));
  end;

  SwitchMode(emShow);
end;

procedure TfCategoryEdit.FormShow(Sender: TObject);
var i : Integer;
begin
  nHighID := 0;
  For i := 0 To ListBox1.Items.Count - 1 Do
    If Integer(ListBox1.Items.Objects[i]) > nHighID Then
      nHighID := Integer(ListBox1.Items.Objects[i]);
end;

procedure TfCategoryEdit.SwitchMode(aMode: TEditMode);
begin
  case aMode of
    emShow: begin
              Edit1.Visible := False;
              Edit1.Text := '';
              ListBox1.Height := 228;
              btnDelete.Caption := 'Delete';
              btnDelete.OnClick := btnDeleteClick;
              btnEdit.Caption := 'Edit';
              btnEdit.OnClick := btnEditClick;
              Edit1.Tag := -1;
              btnAdd.Visible := True;
            end;
    emEdit,
    emNew: begin
             btnAdd.Visible := False;
             Edit1.Visible := True;
             ListBox1.Height := 200;
             Edit1.SetFocus;
             btnDelete.Caption := 'Cancel';
             btnDelete.OnClick := btnCancelClick;
             btnEdit.Caption := 'Save';
             btnEdit.OnClick := btnSaveClick;
           end;
  end;
  FMode := aMode;
end;

function TfCategoryEdit.__GetHighID: Integer;
begin
  Inc(nHighID);
  Result := nHighID;
end;

end.
