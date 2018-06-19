unit frmTracerOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfTracerOptions = class(TForm)
    GroupBox1: TGroupBox;
    cbUseTracer: TCheckBox;
    edMAFLogfile: TEdit;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    cbHookManager: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fTracerOptions: TfTracerOptions;

implementation

{$R *.dfm}

end.
