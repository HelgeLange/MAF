program MAF_Loader;

uses
  Forms,
  frmMAF_Main in 'frmMAF_Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
