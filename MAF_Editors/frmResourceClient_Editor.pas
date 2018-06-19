unit frmResourceClient_Editor;

interface

uses Windows, Messages, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ComCtrls, VCL.Menus, VCL.Buttons,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs, ComCtrls, Menus, Buttons,
     {$ENDIF}
     // Modular Application Framework Components units
     uMAF_ResourceClient;

type
  TfResourceClient_Editor = class(TForm)
    lvComponents: TListView;
    edResID: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    btnClose: TButton;
    Label2: TLabel;
    Button1: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvComponentsClick(Sender: TObject);
    procedure lvComponentsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure edResIDChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure __AddComponent(AComponent: TComponent; ForcedClassID: Integer = 0; APropName: String = '');
    procedure __EnumWinControls(AComponent: TComponent);
    procedure __UpdateEditField;
    function __FindComponentInfo(AComponentName: String): PComponentInfo;
    function __GetClassID(AComponent: TObject): Word;
  public
    RC : TmafResourceClient;
    Modified : Boolean;
  end;

var
  fResourceClient_Editor: TfResourceClient_Editor;

implementation

{$R *.dfm}

uses uMAF_Tools, frmConfiguration, uMAF_Core, uMAF_ResourceManager_Helper;

{const MAX_CLASSES = 11;
      AClasses : Array [1..MAX_CLASSES] of TComponentClass = (
                   TLabel, TRadioButton, TButton, TCheckBox, TMenuItem,
                   TGroupBox, TForm, TTabSheet, TPanel, TImage,
                   TListView
                   );
}
procedure TfResourceClient_Editor.btnCloseClick(Sender: TObject);
var i : Integer;
    pData : PComponentInfo;
begin
  For i := 0 To lvComponents.Items.Count - 1 Do begin
    pData := __FindComponentInfo(PComponentInfo(lvComponents.Items.Item[i].Data)^.ComponentName);
    If lvComponents.Items.Item[i].Checked Then begin
      If pData = nil Then
        RC.UpdateComponents.Add(lvComponents.Items.Item[i].Data);
    end else begin
      If pData <> nil Then
        RC.UpdateComponents.Delete(RC.UpdateComponents.IndexOf(pData));
      Dispose(PComponentInfo(lvComponents.Items.Item[i].Data));
    end;
  end;
  Close;
end;

procedure TfResourceClient_Editor.Button1Click(Sender: TObject);
var nID : Cardinal;
    AType: TResourceFileType;
begin
  nID := PComponentInfo(lvComponents.Selected.Data)^.ResID;
  Case PComponentInfo(lvComponents.Selected.Data)^.ClassID Of
    1..9, 11 : AType := rftString;
    10       : AType := rftMedia;
    Else AType := rftUnknown;
  End;
  nID := FpProjectSettings.__ShowResourceEditor(nID, AType);
  If nID > 0 Then
    edResID.Text := IntToStr(nID);
end;

procedure TfResourceClient_Editor.edResIDChange(Sender: TObject);
var pData : PComponentInfo;
begin
  If lvComponents.Selected = nil Then
    Exit;

  pData := PComponentInfo(lvComponents.Selected.Data);
  pData^.ResID := StrToIntDef(edResID.Text, 0);
  lvComponents.Selected.SubItems.Strings[0] := IntToStr(pData^.ResID);
  Modified := True;
end;

procedure TfResourceClient_Editor.FormShow(Sender: TObject);
begin
  Modified := False;
  If Assigned(RC) Then begin
    __EnumWinControls(RC.Owner);

  end;
end;

procedure TfResourceClient_Editor.lvComponentsClick(Sender: TObject);
begin
  __UpdateEditField;
end;

procedure TfResourceClient_Editor.lvComponentsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  If Selected Then
    __UpdateEditField;
end;

procedure TfResourceClient_Editor.__AddComponent(AComponent: TComponent; ForcedClassID: Integer = 0; APropName: String = '');
var LI : TListItem;
    pData : PComponentInfo;
begin
  LI := lvComponents.Items.Add;
  LI.Caption := AComponent.Name;
  If APropName <> '' Then
    LI.Caption := LI.Caption + '.' + APropName;
  pData := __FindComponentInfo(LI.Caption);
  If pData = nil Then begin
    New(pData);
    FillChar(pData^, SizeOf(RComponentInfo), 0);
    pData^.ComponentName := LI.Caption;
  end else  //  --  If pData = nil Then
    LI.Checked := True;
  pData^.aComp := TObject(AComponent);
  If ForcedClassID = 0 Then
    pData^.ClassID := __GetClassID(AComponent)
  Else
    pData^.ClassID := ForcedClassID;
  LI.Data := pData;
  LI.SubItems.Add(IntToStr(pData^.ResID));
  LI.SubItems.Add(AComponent.ClassName);
end;

procedure TfResourceClient_Editor.__EnumWinControls(AComponent: TComponent);
var i, j : Integer;
var LI : TListItem;
    pData : PComponentInfo;
begin
  For i := 0 To AComponent.ComponentCount - 1 Do
    If RC.__IsSupported(AComponent.Components[i]) Then begin
      If IsClass(AComponent.Components[i], TListView) Then begin
        If TListView(AComponent.Components[i]).Columns.Count > 0 Then
          For j := 0 To TListView(AComponent.Components[i]).Columns.Count - 1 do begin
            LI := lvComponents.Items.Add;
            LI.Caption := TListView(AComponent.Components[i]).Name + '.Colums[' + IntToStr(j) + ']';
            pData := __FindComponentInfo(LI.Caption);
            If pData = nil Then begin
              New(pData);
              FillChar(pData^, SizeOf(RComponentInfo), 0);
              pData^.ComponentName := LI.Caption;
            end else  //  --  If pData = nil Then
              LI.Checked := True;
            pData^.aComp := TObject(TListView(AComponent.Components[i]).Column[j]);
            pData^.ClassID := 11;  // TListView
            LI.Data := pData;
            LI.SubItems.Add(IntToStr(pData^.ResID));
            LI.SubItems.Add(TListView(AComponent.Components[i]).Column[j].ClassName);
          end;
      end else
        If IsClass(AComponent.Components[i], TToolButton) Then begin
          __AddComponent(AComponent.Components[i], 0, 'Image');
          __AddComponent(AComponent.Components[i], 0, 'Caption');
          __AddComponent(AComponent.Components[i], 0, 'Hint');
{          LI := lvComponents.Items.Add;
          LI.Caption := TToolButton(AComponent.Components[i]).Name + '.Image';
          pData := __FindComponentInfo(LI.Caption);
          If pData = nil Then begin
            New(pData);
            FillChar(pData^, SizeOf(RComponentInfo), 0);
            pData^.ComponentName := LI.Caption;
          end else
            LI.Checked := True;
          pData^.aComp := TObject(AComponent.Components[i]);
          pData^.ClassID := 12;  // TToolButton
          LI.Data := pData;
          LI.SubItems.Add(IntToStr(pData^.ResID));
          LI.SubItems.Add(TToolButton(AComponent.Components[i]).ClassName); }
        end else
          If IsClass(AComponent.Components[i], TBitBtn) Then begin
            __AddComponent(AComponent.Components[i], 13, 'Glyph');
            __AddComponent(AComponent.Components[i], 13, 'Caption');
            __AddComponent(AComponent.Components[i], 13, 'Hint');
          end else
            If IsClass(AComponent.Components[i], TSpeedButton) Then begin
              __AddComponent(AComponent.Components[i], 14, 'Glyph');
              __AddComponent(AComponent.Components[i], 14, 'Caption');
              __AddComponent(AComponent.Components[i], 14, 'Hint');
            end else
              __AddComponent(AComponent.Components[i]);
      If AComponent.Components[i].ComponentCount > 0 Then
        __EnumWinControls(AComponent.Components[i]);
    end;
end;

function TfResourceClient_Editor.__FindComponentInfo(AComponentName: String): PComponentInfo;
var i : Integer;
begin
  Result := nil;
  For i := 0 To RC.UpdateComponents.Count - 1 Do
    If PComponentInfo(RC.UpdateComponents.Items[i])^.ComponentName = AComponentName Then begin
      Result := PComponentInfo(RC.UpdateComponents.Items[i]);
      Break;
    end;
end;

function TfResourceClient_Editor.__GetClassID(AComponent: TObject): Word;
var i : Integer;
begin
  Result := 0;
  For i := 1 To MAX_CLASSES Do
    If IsClass(AComponent, AClasses[i]) Then begin
      Result := i;
      Break;
    end;
end;

procedure TfResourceClient_Editor.__UpdateEditField;
begin
  edResID.Enabled := False;
  If lvComponents.Selected <> nil Then
    If lvComponents.Selected.Checked Then begin
      edResID.Enabled := True;
      edResID.Text := IntToStr(PComponentInfo(lvComponents.Selected.Data)^.ResID);
    end;
end;

end.
