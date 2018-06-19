unit frmMAFFileLogger_Editor;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs;
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs;
     {$ENDIF}

type
  TfFileLoggerEditor = class(TForm)
    edLogfileName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    btnCancel: TButton;
    btnOk: TButton;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure edLogfileNameChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    bModified : Boolean;
    sOldFileName : String;
  end;

implementation

{$R *.dfm}

procedure TfFileLoggerEditor.btnCancelClick(Sender: TObject);
begin
  bModified := False;
  Close;
end;

procedure TfFileLoggerEditor.btnOkClick(Sender: TObject);
begin
  sOldFileName := edLogfileName.Text;
  Close;
end;

procedure TfFileLoggerEditor.edLogfileNameChange(Sender: TObject);
begin
  If sOldFileName <> edLogfileName.Text Then
    bModified := True;
end;

procedure TfFileLoggerEditor.FormCreate(Sender: TObject);
begin
  bModified := False;
end;

procedure TfFileLoggerEditor.FormShow(Sender: TObject);
begin
  edLogfileName.Text := sOldFileName;
end;

end.
