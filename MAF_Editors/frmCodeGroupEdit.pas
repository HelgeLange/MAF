{*******************************************************************************
Name         : frmCodeGroupEdit.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2009 by Helge Lange
Info         : HelgeLange@gmail.com
Date         : 13.06.2009
Last Update  : 01.10.2009
Version      : 1.0.001
Purpose      : Part of the Property-Editor for TmafHookManager component
Last Changes :

1.0.001 (01.10.2009) -----------------------------------------------------------
- [ADD] support for security level
1.0.000 (13.06.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit frmCodeGroupEdit;

interface

uses Windows, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     Messages;

type
  TfCodeGroupEdit = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edCodeGroupID: TEdit;
    edCodeGroupName: TEdit;
    mCodeGroupDesc: TMemo;
    btnCancel: TButton;
    btnSave: TButton;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    edNameID: TEdit;
    edDescID: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    btnSearchMediaNameID: TButton;
    Button2: TButton;
    cbMinSL: TComboBox;
    Label7: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSearchMediaNameIDClick(Sender: TObject);
  private
    function __GetCodeGroupID: Integer;
    procedure __SetCodeGroupID(const Value: Integer);
    function __GetCodeGroupName: String;
    procedure __SetCodeGroupName(const Value: String);
    function __GetCodeGroupDesc: String;
    procedure __SetCodeGroupDesc(const Value: String);
    function __GetCodeGroupDescID: Integer;
    function __GetCodeGroupNameID: Integer;
    procedure __SetCodeGroupDescID(const Value: Integer);
    procedure __SetCodeGroupNameID(const Value: Integer);
    function __GetSL: Integer;
    procedure __SetSL(const Value: Integer);
    { Private declarations }
  public
    property CodeGroupID : Integer read __GetCodeGroupID write __SetCodeGroupID;
    property CodeGroupName : String read __GetCodeGroupName write __SetCodeGroupName;
    property CodeGroupDesc : String read __GetCodeGroupDesc write __SetCodeGroupDesc;
    property CodeGroupNameID : Integer read __GetCodeGroupNameID write __SetCodeGroupNameID;
    property CodeGroupDescID : Integer read __GetCodeGroupDescID write __SetCodeGroupDescID;
    property SL : Integer read __GetSL write __SetSL;
  end;

var
  fCodeGroupEdit: TfCodeGroupEdit;

implementation

{$R *.dfm}

uses frmConfiguration, uMAF_Core, uMAF_ResourceManager_Helper;

{ TfCodeGroupEdit }

procedure TfCodeGroupEdit.__SetCodeGroupID(const Value: Integer);
begin
  edCodeGroupID.Text := IntToStr(Value);
end;

function TfCodeGroupEdit.__GetCodeGroupID: Integer;
begin
  Result := StrToIntDef(edCodeGroupID.Text, -1);
end;

procedure TfCodeGroupEdit.__SetCodeGroupName(const Value: String);
begin
  edCodeGroupName.Text := Value;
end;

procedure TfCodeGroupEdit.__SetCodeGroupNameID(const Value: Integer);
begin
  edNameID.Text := IntToStr(Value);
end;

procedure TfCodeGroupEdit.__SetSL(const Value: Integer);
begin
  If ((Value > 0) and (Value < 10)) Then
    cbMinSL.ItemIndex := Value - 1
  Else
    cbMinSL.ItemIndex := 9;   // if the value comes totally wrong, we set it on maximum security :) just in case...
end;

function TfCodeGroupEdit.__GetCodeGroupName: String;
begin
  Result := edCodeGroupName.Text;
end;

function TfCodeGroupEdit.__GetCodeGroupNameID: Integer;
begin
  Result := StrToIntDef(edNameID.Text, 0);
end;

function TfCodeGroupEdit.__GetSL: Integer;
begin
  Result := cbMinSL.ItemIndex + 1;
  If Result = 0 Then
    Result := 1;
end;

procedure TfCodeGroupEdit.__SetCodeGroupDesc(const Value: String);
begin
  mCodeGroupDesc.Lines.Text := Value;
end;

procedure TfCodeGroupEdit.__SetCodeGroupDescID(const Value: Integer);
begin
  edDescID.Text := IntToStr(Value);
end;

function TfCodeGroupEdit.__GetCodeGroupDesc: String;
begin
  Result := mCodeGroupDesc.Lines.Text;
end;

function TfCodeGroupEdit.__GetCodeGroupDescID: Integer;
begin
  Result := StrToIntDef(edDescID.Text, 0);
end;

procedure TfCodeGroupEdit.btnSearchMediaNameIDClick(Sender: TObject);
var nID : Cardinal;
begin
  If Sender = btnSearchMediaNameID Then
    nID := CodeGroupNameID
  Else
    nID := CodeGroupDescID;
  nID := FpProjectSettings.__ShowResourceEditor(nID, rftString);
  If Sender = btnSearchMediaNameID Then
    CodeGroupNameID := nID
  Else
    CodeGroupDescID := nID;
end;

procedure TfCodeGroupEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  If ModalResult = mrOk Then
    If edCodeGroupName.Text = '' Then begin
      MessageDlg('You have to assign a name !', mtError, [mbOk], 0);
      CanClose := False;
    end;
end;

end.
