unit frmFileDB_UserEditor;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ComCtrls,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs, ComCtrls,
     {$ENDIF}
     uMAF_Globals, uMAF_FileDB, uMAF_UserSecurity;

type
  TfFileDB_UserEditor = class(TForm)
    PageControl1: TPageControl;
    tsUsers: TTabSheet;
    tsGroups: TTabSheet;
    ListBox1: TListBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    edLogin: TEdit;
    edPassword: TEdit;
    edPassword2: TEdit;
    edFirstName: TEdit;
    edLastName: TEdit;
    cbEditGroup: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    btnDelete: TButton;
    btnEdit: TButton;
    btnNew: TButton;
    edEncryptionKey: TEdit;
    Label8: TLabel;
    procedure btnNewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure EnableEditArea(bEnable: Boolean);
    procedure btnSaveNewClick(Sender: TObject);
  public
    FileDB : TmafFileDB;
    UserSecurity : TmafUserSecurity;
  end;

var
  fFileDB_UserEditor: TfFileDB_UserEditor;

implementation

{$R *.dfm}

const sEncryptionKey_Hint : String = 'The encryption key is used to encrypt the passwords'+#13#10+
                                     'in the database. It has to be the same you plan to'+#13#10+
                                     'use in the program itself. To use the encryption'+#13#10+
                                     'in your program, call ERPUserSecurity1.SetAppPW(''<Your Key>'')'+#13#10+
                                     'before calling ERPUserSecurity1.Connect.';

procedure TfFileDB_UserEditor.FormShow(Sender: TObject);
begin
  edEncryptionKey.Hint := sEncryptionKey_Hint;
end;

procedure TfFileDB_UserEditor.btnNewClick(Sender: TObject);
begin
  EnableEditArea(True);
  btnNew.Caption := 'Save';
  btnNew.OnClick := btnSaveNewClick;
  btnEdit.Caption := 'Cancel';
end;

procedure TfFileDB_UserEditor.btnSaveNewClick(Sender: TObject);
var pUser : PUserDataRec;
begin
  If ((edPassword.Text = edPassword2.Text) And (edPassword.Text <> '') And (edLogin.Text <> '')) Then begin
    New(pUser);
    FillChar(pUser^, SizeOf(RUserDataRec), 0);
    pUser^.Login := edLogin.Text;
    If FileDB.QueryUserTable(US_GET_USER_DATA, pUser) = False Then begin
      // User login does not exist yet
      pUser^.Password := edPassword.Text;
      pUser^.FirstName := edFirstName.Text;
      pUser^.LastName := edLastName.Text;
      pUser^.GroupID := Integer(cbEditGroup.Items.Objects[cbEditGroup.ItemIndex]);
    end else begin
      MessageDlg('The Login already exists !', mtError, [mbOk], 0);
      Exit;
    end;
    btnNew.Caption := 'New';
    btnNew.OnClick := btnNewClick;
    btnEdit.Caption := 'Edit';
  end;
end;

procedure TfFileDB_UserEditor.EnableEditArea(bEnable: Boolean);
begin
  edLogin.Enabled := bEnable;
  edPassword.Enabled := bEnable;
  edPassword2.Enabled := bEnable;
  edFirstName.Enabled := bEnable;
  edLastName.Enabled := bEnable;
  cbEditGroup.Enabled := bEnable;
end;

end.
