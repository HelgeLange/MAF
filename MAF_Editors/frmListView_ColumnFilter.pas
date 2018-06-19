unit frmListView_ColumnFilter;

interface

uses Windows, SysUtils, Variants, Classes,
     {$IFDEF VER230}
     VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.CheckLst, VCL.ComCtrls,
     {$ELSE}
     StdCtrls, ExtCtrls, Graphics, Controls, Forms, Dialogs, CheckLst, ComCtrls,
     {$ENDIF}
     Messages;

type
  TFilterMode = (fmList, fmText);

  TfListView_ColumnFilter = class(TForm)
    clbFilterItems: TCheckListBox;
    btnApply: TButton;
    cbShowAll: TCheckBox;
    edText: TEdit;
    procedure FormDeactivate(Sender: TObject);
    procedure cbShowAllClick(Sender: TObject);
    procedure clbFilterItemsClickCheck(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edTextChange(Sender: TObject);
  private
    FFilterMode : TFilterMode;
    FbActive : Boolean;
    FOnActivate : TNotifyEvent;
    FFilterID : Integer;   // the tag of the column in the given ListView
    FsFilterString : String;
    procedure __SetActive(const Value: Boolean);
    procedure __SetFilterMode(const Value: TFilterMode);
    procedure __SetLV(const Value: TListView);
    procedure __SetFilterID(const Value: Integer);
  public
    Column: TListColumn;
    FLV : TListView;
    CurrentFilterList : TStringList;
    bImmediateFilter : Boolean;
    function IsActiveFilterItem(nID: Integer): Boolean;
    property Active : Boolean read FbActive write __SetActive;
    property FilterString : String read FsFilterString write FsFilterString;
    property LV : TListView read FLV write __SetLV;
    property FilterID : Integer read FFilterID write __SetFilterID;
    property FilterMode : TFilterMode read FFilterMode write __SetFilterMode;
    property OnActivate : TNotifyEvent read FOnActivate write FOnActivate;
  end;

implementation

{$R *.dfm}

procedure TfListView_ColumnFilter.btnApplyClick(Sender: TObject);
var i : Integer;
begin
  If FFilterMode = fmList Then begin
    CurrentFilterList.Clear;
    If Not cbShowAll.Checked Then begin
      For i := 0 To clbFilterItems.Items.Count - 1 Do
        If clbFilterItems.Checked[i] Then
          CurrentFilterList.AddObject(clbFilterItems.Items.Strings[i], clbFilterItems.Items.Objects[i]);
      Active := True;
    end else begin
      Active := False;
      If Assigned(FOnActivate) Then
        FOnActivate(Self);
    end;
  end;
  If FFilterMode = fmText Then
    Active := True;
  If Assigned(Sender) Then
    Hide;
end;

procedure TfListView_ColumnFilter.cbShowAllClick(Sender: TObject);
var i : Integer;
begin
  If FilterMode = fmText Then begin
    bImmediateFilter := cbShowAll.Checked;
    If bImmediateFilter Then btnApply.Caption := 'Close'
                        Else btnApply.Caption := 'Apply';
    Exit;
  end;
  For i := 0 To clbFilterItems.Items.Count - 1 Do begin
    clbFilterItems.Checked[i] := cbShowAll.Checked;
    If FbActive Then begin
      If Not clbFilterItems.Checked[i] Then
        If CurrentFilterList.IndexOf(clbFilterItems.Items.Strings[i]) > -1 Then
          clbFilterItems.Checked[i] := True;
    end;
    clbFilterItems.ItemEnabled[i] := Not cbShowAll.Checked;
  end;
end;

procedure TfListView_ColumnFilter.clbFilterItemsClickCheck(Sender: TObject);
var i : Integer;
begin
  If cbShowAll.Checked Then
    For i := 0 To clbFilterItems.Items.Count - 1 Do
      If clbFilterItems.Checked[i] Then begin
        cbShowAll.Checked := False;
        Break;
      end;  //  --  If clbFilterItems.Checked Then 
end;

procedure TfListView_ColumnFilter.edTextChange(Sender: TObject);
begin
  FsFilterString := edText.Text;
  If bImmediateFilter Then
    btnApplyClick(nil);
end;

procedure TfListView_ColumnFilter.FormCreate(Sender: TObject);
begin
  CurrentFilterList := TStringList.Create;
  FbActive := False;
  FilterMode := fmList;
  FsFilterString := '';
  bImmediateFilter := True;
end;

procedure TfListView_ColumnFilter.FormDeactivate(Sender: TObject);
begin
  Hide;
end;

procedure TfListView_ColumnFilter.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CurrentFilterList);
end;

procedure TfListView_ColumnFilter.FormShow(Sender: TObject);
var i : Integer;
begin
  If Column = nil Then
    Exit;
  cbShowAllClick(cbShowAll);
  Left := 0;
  For i := 0 To Column.Index - 1 Do
    Left := Left + LV.Column[i].Width;
  Top := 28;
  Width := Column.Width;
end;

function TfListView_ColumnFilter.IsActiveFilterItem(nID: Integer): Boolean;
var i : Integer;
begin
  Result := False;
  For i := 0 To CurrentFilterList.Count - 1 Do
    If Integer(CurrentFilterList.Objects[i]) = nID Then begin
      Result := True;
      Break;
    end;
end;

procedure TfListView_ColumnFilter.__SetActive(const Value: Boolean);
begin
  FbActive := Value;
  If FbActive Then begin
    If Assigned(FOnActivate) Then
      FOnActivate(Self);
  end else begin
    cbShowAll.Checked := True;
    CurrentFilterList.Clear;
  end;
end;

procedure TfListView_ColumnFilter.__SetFilterID(const Value: Integer);
begin
  FFilterID := Value;
end;

procedure TfListView_ColumnFilter.__SetFilterMode(const Value: TFilterMode);
begin
  FFilterMode := Value;
  Case FFilterMode Of
    fmList : begin
               Height := 174;
               edText.Visible := False;
               clbFilterItems.Visible := True;
               clbFilterItems.Align := alClient;
             end;
    fmText : begin
               Height := 70;
               edText.Visible := True;
               edText.Align := alClient;
               edText.Text := FsFilterString;
               clbFilterItems.Visible := False;
               cbShowAll.Caption := 'Immediate Filter';
             end;
  end;
end;

procedure TfListView_ColumnFilter.__SetLV(const Value: TListView);
var i : Integer;
begin
  FLV := Value;
  Parent := FLV;
  Column := nil;
  If FLV = nil Then
    Exit;
  For i := 0 To FLV.Columns.Count - 1 do
    If FLV.Columns.Items[i].Tag = FFilterID Then begin
      Column := FLV.Columns.Items[i];
      Break;
    end;
end;

end.
