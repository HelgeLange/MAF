unit frmResourceBaseEditorForm;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ComCtrls,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs, ComCtrls,
     {$ENDIF}
     uMAF_ResourceManager, uMAF_ResourceManager_Helper;

type
  TfResourceBaseEditorForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    cbCategory: TComboBox;
    cbModule: TComboBox;
    Panel3: TPanel;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
  private
    FnDataID: Cardinal;
    FbMultiSelected: Boolean;
    FpLI : TListItem;
  protected
    function __GetIndex(ID: Integer; Items: TStrings): Integer;
    procedure __SetLI(const Value: TListItem); virtual;
    procedure __SetDataID(const Value: Cardinal); virtual;
    procedure __SetMultiSelected(const Value: Boolean); virtual;
  public
    RM : TmafResourceManager;
    CurrDesc : PBaseDescriptor;
    CurrListView : TListView;
    property LI : TListItem read FpLI write __SetLI;
    property DataID : Cardinal read FnDataID write __SetDataID;
    property MultiSelected : Boolean read FbMultiSelected write __SetMultiSelected;
  end;

implementation

{$R *.dfm}

{ TForm1 }

procedure TfResourceBaseEditorForm.FormCreate(Sender: TObject);
begin
  CurrDesc := nil;
  FpLI := nil;
  CurrListView := nil;
end;

function TfResourceBaseEditorForm.__GetIndex(ID: Integer; Items: TStrings): Integer;
var i : Integer;
begin
  Result := -1;
  For i := 0 To Items.Count - 1 do
    If Integer(Items.Objects[i]) = ID Then begin
      Result := i;
      Break;
    end;
end;

procedure TfResourceBaseEditorForm.__SetDataID(const Value: Cardinal);
begin
  FnDataID := Value;
end;

procedure TfResourceBaseEditorForm.__SetLI(const Value: TListItem);
begin
  FpLI := Value;
  If FpLI <> nil Then CurrDesc := LI.Data
                 Else CurrDesc := nil;
end;

procedure TfResourceBaseEditorForm.__SetMultiSelected(const Value: Boolean);
begin
  FbMultiSelected := Value;
end;

end.
