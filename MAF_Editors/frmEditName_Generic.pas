unit frmEditName_Generic;

interface

uses Windows, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     Messages;

type
  TfEditName_Generic = class(TForm)
    eHookDesc: TEdit;
    btnCancel: TButton;
    btnOk: TButton;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fEditName_Generic: TfEditName_Generic;

implementation

{$R *.dfm}

end.
