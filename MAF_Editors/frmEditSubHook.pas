unit frmEditSubHook;

interface

uses Windows, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     Messages;

type
  TfEditSubHook = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    eSubHookDesc: TEdit;
    Label1: TLabel;
    eSubHookID: TEdit;
    Label2: TLabel;
    cbModules: TComboBox;
    Label3: TLabel;
    eHookID: TEdit;
    Label4: TLabel;
    procedure eSubHookIDKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function __GetModuleID: Integer;
    procedure __SetModuleID(const Value: Integer);
    function __GetHookID: Integer;
    procedure __SetHookID(const Value: Integer);
    function __GetSubHookID: Integer;
    procedure __SetSubHookID(const Value: Integer);

  public
    property ModuleID : Integer read __GetModuleID write __SetModuleID;
    property HookID : Integer read __GetHookID write __SetHookID;
    property SubHookID : Integer read __GetSubHookID write __SetSubHookID;
  end;

implementation

{$R *.dfm}

uses uMAF_ModuleManager;

procedure TfEditSubHook.eSubHookIDKeyPress(Sender: TObject; var Key: Char);
{$IFDEF Unicode}
var ACharSet : TSysCharSet;
  {$ENDIF}
begin
  {$IFDEF Unicode}
  ACharSet := [Char(VK_BACK), '0'..'9'];
  If Not CharInSet(Key, ACharSet) Then
  {$ELSE}
  If Not (Key in [Char(VK_BACK), '0'..'9']) Then
  {$ENDIF}
    Key := #0;
end;

procedure TfEditSubHook.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  If ModalResult = mrOk Then begin
    CanClose := (StrToIntDef(eSubHookID.Text, -1) > -1);
    If CanClose Then
      CanClose := (StrToIntDef(eHookID.Text, -1) > -1);
  end;
end;

function TfEditSubHook.__GetHookID: Integer;
begin
  Result := StrToInt(eHookID.Text); // test, if valid number was performed in FormCloseQuery
end;

procedure TfEditSubHook.__SetHookID(const Value: Integer);
begin
  eHookID.Text := IntToStr(Value);
end;

function TfEditSubHook.__GetSubHookID: Integer;
begin
  Result := StrToInt(eSubHookID.Text); // test, if valid number was performed in FormCloseQuery
end;

procedure TfEditSubHook.__SetSubHookID(const Value: Integer);
begin
  eSubHookID.Text := IntToStr(Value);
end;

function TfEditSubHook.__GetModuleID: Integer;
begin
  Result := Integer(cbModules.Items.Objects[cbModules.ItemIndex]);
end;

procedure TfEditSubHook.__SetModuleID(const Value: Integer);
var i : Integer;
begin
  For i := 0 To cbModules.Items.Count - 1 Do
    If Integer(cbModules.Items.Objects[i]) = Value Then begin
      cbModules.ItemIndex := i;
      Exit;
    end;
end;

end.
