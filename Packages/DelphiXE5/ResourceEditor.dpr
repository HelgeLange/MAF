program ResourceEditor;

uses
  Forms,
  frmResourceManager_Editor in '..\..\MAF_Editors\frmResourceManager_Editor.pas' {fRMT_Main};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfRMT_Main, fRMT_Main);
  fRMT_Main.bStandAlone := True;
  Application.Run;
end.
