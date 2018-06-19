unit frmCreateDevAccount;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     uMAF_UserSecurity, uMAF_Globals, uMAF_Core;

type
  TfCreateDevAccount = class(TForm)
    GroupBox1: TGroupBox;
    edGroup: TEdit;
    Label1: TLabel;
    edLogin: TEdit;
    Label2: TLabel;
    edPassword: TEdit;
    Label3: TLabel;
    btnCreate: TButton;
    btnCancel: TButton;
    Label4: TLabel;
    edProgrammPassword: TEdit;
    Label5: TLabel;
    procedure btnCreateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    nGroupID : Integer;
    procedure CreateGroup;
    procedure CreateUser;
  public
    aUS : TmafUserSecurity;
  end;

var
  fCreateDevAccount: TfCreateDevAccount;

implementation

{$R *.dfm}

const sLabelText = 'The developer account is the start account with which a developer creates '+
                   'the necessary entries in the database to start with.'+#13#10+ 'The group '+
                   'created will automatically get a SecurityLevel of 10 (highest)and Login and '+
                   'Password for the account should match your current database login.'+#13#10+
                   'The Login Password defines the password, that you will use to login into the '+
                   'program, masking your real database password.';

procedure TfCreateDevAccount.btnCreateClick(Sender: TObject);
begin
  CreateGroup;
  CreateUser;
  Close;
end;

procedure TfCreateDevAccount.CreateGroup;
var QHS : pQHS;
    AList : TList;
    i : Integer;
    bCreate: Boolean;
    pGroup : PGroupDataRec;
begin
  If Not Assigned(aUS.ModuleController) Then
    Exit;

  bCreate := True;
  QHS := __Create_QueryHandlerStruct;
  QHS^.HookID := HM_USER_SECURITY;
  QHS^.SubHookID := US_GET_GROUPLIST;
  aUS.ModuleController.Execute(HM_USER_SECURITY, QHS, nil);
  If Assigned(QHS^.pChildObj) Then begin
    ShowMessage('CP #-1');
    aList := TList(QHS^.pChildObj);
    For i := 0 To aList.Count - 1 Do
      If PGroupDataRec(AList.Items[i])^.GroupName = edGroup.Text Then begin
        bCreate := False;
        nGroupID := PGroupDataRec(AList.Items[i])^.GroupID;
        ShowMessage('Group exists');
        Break;
      end;
    QHS^.pFreeMemFunc(QHS^.pChildObj);
    QHS^.pChildObj := nil;
  end;

  If bCreate Then begin
    New(pGroup);
    pGroup^.GroupID := 0;
    pGroup^.GroupName := edGroup.Text;
    pGroup^.Flags := [];
    pGroup^.SL := 10;
    QHS^.pChildObj := pGroup;
    QHS^.SubHookID := US_CREATE_GROUP;
    aUS.ModuleController.Execute(HM_USER_SECURITY, QHS, nil);
    Dispose(pGroup);
        SHowMessage('Group created');
    // we call ourselfs again, which does nothing as the group is created but
    // it will set the group ID
    CreateGroup;
  end;
  __Free_QueryHandlerStruct(QHS);
end;

procedure TfCreateDevAccount.CreateUser;
var QHS : pQHS;
    pUSer : PUserDataRec;
begin
  If Not Assigned(aUS.ModuleController) Then
    Exit;

ShowMessage('CP #1');
  QHS := __Create_QueryHandlerStruct;
  QHS^.HookID := HM_USER_SECURITY;
  QHS^.SubHookID := US_GET_USER_DATA;
  New(pUser);
  FillChar(pUser^, SizeOf(RUserDataRec), 0);
  pUser^.Login := edLogin.Text;
  pUser^.FirstName := edLogin.Text;
  pUser^.LastName := edLogin.Text;
  QHS^.pChildObj := pUser;
  aUS.ModuleController.Execute(HM_USER_SECURITY, QHS, nil);
ShowMessage('CP #2, UserID: '+IntToStr(pUser^.ID));
  If pUser^.ID = 0 Then begin
    QHS^.Reserved1 := 1;  // turns off the auto-generation of passwords for database access
    pUser^.Password2 := edPassword.Text;
    If edProgrammPassword.Text <> '' Then
      pUser^.Password := edProgrammPassword.Text
    Else
      pUser^.Password := edPassword.Text;
    pUser^.SL := 10;
    pUser^.GroupID := nGroupID;
    QHS^.SubHookID := US_CREATE_USER;
    aUS.ModuleController.Execute(HM_USER_SECURITY, QHS, nil);
ShowMessage('CP #3, UserID: '+IntToStr(pUser^.ID));
  end else
    MessageDlg('The user already exists...', mtInformation, [mbOk], 0);
  __Free_QueryHandlerStruct(QHS);
  Dispose(pUser);
end;

procedure TfCreateDevAccount.FormShow(Sender: TObject);
begin
  Label4.Caption := sLabelText;
  If Assigned(aUS) Then begin
    edLogin.Text := aUS.UserName;
    edPassword.Text := aUS.Password;
  end;
end;

end.
