unit frmEditDataDef;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     // Modular Application Framework Components units
     uMAF_ModuleController_DataHelper, uMAF_Globals;

type
  TfEditDataDef = class(TForm)
    eName: TEdit;
    Label1: TLabel;
    cbDataType: TComboBox;
    Label2: TLabel;
    Panel1: TPanel;
    btnCancel: TButton;
    btnSave: TButton;
    procedure cbDataTypeChange(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pToken : PRecordDef;
  end;

implementation

{$R *.dfm}

procedure TfEditDataDef.btnCancelClick(Sender: TObject);
begin
  __Free_RecordDef(pToken);
end;

procedure TfEditDataDef.cbDataTypeChange(Sender: TObject);
begin
  Case cbDataType.ItemIndex of
    0 : pToken^.aType := sdtInteger;
    1 : pToken^.aType := sdtDateTime;
    2 : pToken^.aType := sdtInt64;
    3 : pToken^.aType := sdtString;
    4 : pToken^.aType := sdtMediaItem;
  end;
end;

procedure TfEditDataDef.eNameChange(Sender: TObject);
begin
  pToken^.sName := eName.Text;
end;

procedure TfEditDataDef.FormCreate(Sender: TObject);
begin
  pToken := __Create_RecordDef;
  pToken^.aType := sdtInteger;
end;

end.
