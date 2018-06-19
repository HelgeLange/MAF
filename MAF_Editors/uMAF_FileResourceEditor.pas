unit uMAF_FileResourceEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, ToolWin,
  // Modular Application Framework Components units
  uMAF_CustomFileResource, ExtCtrls;

type
  TfResourceEditor = class(TForm)
    ToolBar1: TToolBar;
    btnNew: TToolButton;
    ImageList1: TImageList;
    btnOpen: TToolButton;
    ToolButton3: TToolButton;
    btnAdd: TToolButton;
    btnRemove: TToolButton;
    ToolButton6: TToolButton;
    btnInfo: TToolButton;
    btnSave: TToolButton;
    ToolButton9: TToolButton;
    btnClose: TToolButton;
    SaveDialog1: TSaveDialog;
    tvDirView: TTreeView;
    Splitter1: TSplitter;
    lvFileView: TListView;
    ImageList2: TImageList;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FOldOnModified : TNotifyEvent;
    procedure OnModified(Sender: TObject);
  public
    FR : TmafFileResource;
  end;

var fResourceEditor: TfResourceEditor;

const bStandAlone : Boolean = False;   // is set to true, when running as ResourceEditor.exe

implementation

{$R *.dfm}

procedure TfResourceEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  If bStandAlone Then begin
    FR.Free;
  end else begin
    FR.OnModified := FOldOnModified;
  end;
end;

procedure TfResourceEditor.FormShow(Sender: TObject);
begin
  If bStandAlone Then begin
    FR := TmafFileResource.Create(Self);
  end else begin
    // manage the component from the IDE, so all events set there, must be preserved
    FOldOnModified := FR.OnModified;  // save the old variable from the users component
  end;
  FR.OnModified := OnModified;
end;

procedure TfResourceEditor.OnModified(Sender: TObject);
begin
  btnSave.Enabled := True;
end;

procedure TfResourceEditor.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfResourceEditor.btnNewClick(Sender: TObject);
begin
  If SaveDialog1.Execute(Self.Handle) Then begin
    FR.CreateResourceFile(SaveDialog1.FileName);
  end;
end;

end.
