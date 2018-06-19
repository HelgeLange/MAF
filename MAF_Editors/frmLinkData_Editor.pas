unit frmLinkData_Editor;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     uMAF_LinkClient;

type
  TfLinkDataEditor = class(TForm)
    mLinkIDs: TMemo;
    btnCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    btnSave: TButton;
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FpLinkClient : TmafLinkClient;
    procedure __SetLinkClient(const Value: TmafLinkClient);
  public
    property LinkClient : TmafLinkClient read FpLinkClient write __SetLinkClient;
  end;

implementation

{$R *.dfm}

procedure TfLinkDataEditor.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfLinkDataEditor.btnSaveClick(Sender: TObject);
var i, j : Integer;
begin
  FpLinkClient.LinkIDs.Clear;
  For i := 0 To mLinkIDs.Lines.Count - 1 Do begin
    j := StrToIntDef(mLinkIDs.Lines.Strings[i], 0);
    If j > 0 Then
      FpLinkClient.LinkIDs.Add(Pointer(j));
  end;  //  --  For i := 0 To mLinkIDs.Lines.Count - 1 Do
  Close;
end;

procedure TfLinkDataEditor.__SetLinkClient(const Value: TmafLinkClient);
var i : Integer;
begin
  FpLinkClient := Value;
  If FpLinkClient <> nil Then begin
    mLinkIDs.Lines.Clear;
    For i := 0 To FpLinkClient.LinkIDs.Count - 1 Do
      mLinkIDs.Lines.Add(IntToStr(Integer(FpLinkClient.LinkIDs.Items[i])));
  end;  //  --  If FpLinkClient <> nil Then
end;

end.
