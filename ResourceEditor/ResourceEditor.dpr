program ResourceEditor;

uses
  Forms,
  uMAF_FileResourceEditor in '..\MAF_Editors\uMAF_FileResourceEditor.pas' {fResourceEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  bStandAlone := True;
  Application.CreateForm(TfResourceEditor, fResourceEditor);
  Application.Run;
end.
