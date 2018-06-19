unit frmResourceFileInfo;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     // Modular Application Framework Components
     uMAF_ResourceManager, uMAF_CustomResource;

type
  TfResourceFileInfo = class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    cbEncoding: TComboBox;
    lItemCount: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    Resource : TmafBaseResourceFile;
  end;

var
  fResourceFileInfo: TfResourceFileInfo;

implementation

{$R *.dfm}

procedure TfResourceFileInfo.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfResourceFileInfo.FormShow(Sender: TObject);
begin
  If Assigned(Resource) Then begin
    lItemCount.Caption := IntToStr(Resource.Data.Count);
    Case Resource.Encoding Of
      etANSI       : cbEncoding.ItemIndex := 0;
      etWideString : cbEncoding.ItemIndex := 1;
    end;
  end;
end;

end.
