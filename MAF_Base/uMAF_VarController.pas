{*******************************************************************************
Name         : uMAF_VarController.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2010 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 30.07.2010
Last Update  : 31.07.2010
Version      : 1.0.000
Last Changes :


1.0.000 (30.07.2010) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_VarController;

interface

uses Windows, Classes, SysUtils, IniFiles,
     // Modular Application Framework Components units
     uMAF_Tools, uMAF_Globals, uMAF_Core;

Type TOnEnumString = procedure(Sender: TObject; VarName, Value: String; ALoadSave: TLoadSave) Of Object;
     TOnEnumInteger = procedure(Sender: TObject; VarName: String; Value: Integer; ALoadSave: TLoadSave) Of Object;
     TOnEnumBoolean = procedure(Sender: TObject; VarName: String; Value: Boolean; ALoadSave: TLoadSave) Of Object;

     TmafCustomVarController = class(TPersistent)
     private
       FLoadSaveType : TLoadSave;
       FVarControllerOptions : TVarControllerOptions;
       FiDefaultInteger : Integer;
       FsDefaultString : String;
       FbDefaultBoolean : Boolean;
       FbConnected : Boolean;
       FOnEnumString : TOnEnumString;
       FOnEnumInteger : TOnEnumInteger;
       FOnEnumBoolean : TOnEnumBoolean;
       FBeforeConnect : TNotifyEvent;
     protected
       property LoadSaveType : TLoadSave read FLoadSaveType;
     public
       bClosing : Boolean;
       constructor Create; virtual;
       procedure Connect; virtual;
       procedure Disconnect; virtual;
       procedure EnumVars; virtual;
       procedure WriteVar(pData: PVarRec); virtual; abstract;
       function ReadString(VarName: String): String; virtual;
       function ReadInteger(VarName: String): Integer; virtual;
       function ReadBoolean(VarName: String): Boolean; virtual;
       property Connected : Boolean read FbConnected write FbConnected;
     published
       property VarControllerOptions : TVarControllerOptions read FVarControllerOptions write FVarControllerOptions default [];
       property DefaultInteger : Integer read FiDefaultInteger write FiDefaultInteger default -1;
       property DefaultString : String read FsDefaultString write FsDefaultString;
       property DefaultBoolean : Boolean read FbDefaultBoolean write FbDefaultBoolean default False;
       // events
       property OnEnumString : TOnEnumString read FOnEnumString write FOnEnumString;
       property OnEnumInteger : TOnEnumInteger read FOnEnumInteger write FOnEnumInteger;
       property OnEnumBoolean : TOnEnumBoolean read FOnEnumBoolean write FOnEnumBoolean;
       property BeforeConnect : TNotifyEvent read FBeforeConnect write FBeforeConnect;
     end;

     TmafINIVarController = class(TmafCustomVarController)
     private
       FsFileName : String;
       FpIniFile : TINIFile;
     protected
     public
       constructor Create; override;
       procedure Connect; override;
       procedure Disconnect; override;
       procedure EnumVars; override;
       procedure WriteVar(pData: PVarRec); override;
       function ReadString(VarName: String): String; override;
       function ReadInteger(VarName: String): Integer; override;
       function ReadBoolean(VarName: String): Boolean; override;
     published
       property FileName : String read FsFileName write FsFileName;
     end;

implementation

const sINI_SECTION_INTEGER : String = 'INTEGER VARS';
      sINI_SECTION_STRING  : String = 'STRING VARS';
      sINI_SECTION_BOOLEAN : String = 'BOOLEAN VARS';
      
{ TmafCustomVarController }

procedure TmafCustomVarController.Connect;
begin
  // do nothing
end;

constructor TmafCustomVarController.Create;
begin
  inherited;
  bClosing := False;
  FVarControllerOptions := [];
end;

procedure TmafCustomVarController.Disconnect;
begin
  // do nothing
end;

procedure TmafCustomVarController.EnumVars;
begin
  // do nothing
end;

function TmafCustomVarController.ReadBoolean(VarName: String): Boolean;
begin
  Result := FbDefaultBoolean;
end;

function TmafCustomVarController.ReadInteger(VarName: String): Integer;
begin
  Result := FiDefaultInteger;
end;

function TmafCustomVarController.ReadString(VarName: String): String;
begin
  Result := FsDefaultString;
end;

{ TINIVarController }

procedure TmafINIVarController.Connect;
var S : String;
begin
  If Assigned(FBeforeConnect) Then
    BeforeConnect(Self);
  If FsFileName <> '' Then begin
    S := ExtractFilePath(FsFileName);
    If S = '' Then
      FsFileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + FsFileName;
    FpIniFile := TINIFile.Create(FsFileName);
    FbConnected := True;
  end;  //  --  If FsFileName <> '' Then
end;

constructor TmafINIVarController.Create;
begin
  inherited;
  FLoadSaveType := lsINIFile;
end;

procedure TmafINIVarController.Disconnect;
begin
  FreeAndNil(FpIniFile);
  FbConnected := False;
end;

procedure TmafINIVarController.EnumVars;
var SectionList : TStringList;
    i, j, iVal : Integer;
    sVal : String;
    bDoDisconnect: Boolean;
begin
  bDoDisconnect := False;
  If Not FbConnected Then begin
    Connect;
    bDoDisconnect := True;
  end;  //  --  If Not FbConnected Then

  If Not Assigned(FpIniFile) Then
    Exit;
    
  SectionList := TStringList.Create;
  FpIniFile.ReadSection(sINI_SECTION_INTEGER, SectionList);
  For i := 0 To SectionList.Count - 1 Do begin
    iVal := FpIniFile.ReadInteger(sINI_SECTION_INTEGER, SectionList.Strings[i], FiDefaultInteger);
    If Assigned(FOnEnumInteger) Then
      FOnEnumInteger(Self, SectionList.Strings[i], iVal, lsINIFile);
//    FpParent.AddInteger(SectionList.Strings[i], iVal, [vfSave], lsINIFile);
  end;
  SectionList.Clear;
  FpIniFile.ReadSection(sINI_SECTION_STRING, SectionList);
  For i := 0 To SectionList.Count - 1 Do begin
    sVal := FpIniFile.ReadString(sINI_SECTION_STRING, SectionList.Strings[i], FsDefaultString);
    If Assigned(FOnEnumString) Then
      FOnEnumString(Self, SectionList.Strings[i], sVal, lsINIFile);
//    FpParent.AddString(SectionList.Strings[i], sVal, [vfSave], lsINIFile);
  end;
  SectionList.Clear;
  FpIniFile.ReadSection(sINI_SECTION_BOOLEAN, SectionList);
  If FbDefaultBoolean Then j := 1
                      Else j := 0;
  For i := 0 To SectionList.Count - 1 Do begin
    iVal := FpIniFile.ReadInteger(sINI_SECTION_BOOLEAN, SectionList.Strings[i], j);
    If Assigned(FOnEnumBoolean) Then
      FOnEnumBoolean(Self, SectionList.Strings[i], (iVal <> 0), lsINIFile);
//    FpParent.AddBoolean(SectionList.Strings[i], (iVal <> 0), [vfSave], lsINIFile);
  end;
  SectionList.Free;
  If bDoDisconnect Then
    Disconnect;
end;

function TmafINIVarController.ReadBoolean(VarName: String): Boolean;
var bDoDisconnect: Boolean;
    iVal, j : Integer;
begin
  bDoDisconnect := Not FbConnected;
  If Not FbConnected Then
    Connect;
  If FbDefaultBoolean Then j := 1
                      Else j := 0;
  iVal := FpIniFile.ReadInteger(sINI_SECTION_INTEGER, VarName, j);
  Result := (iVal <> 0);
  If bDoDisconnect Then
    Disconnect;
end;

function TmafINIVarController.ReadInteger(VarName: String): Integer;
var bDoDisconnect: Boolean;
begin
  bDoDisconnect := Not FbConnected;
  If Not FbConnected Then
    Connect;
  Result := FpIniFile.ReadInteger(sINI_SECTION_INTEGER, VarName, FiDefaultInteger);
  If bDoDisconnect Then
    Disconnect;
end;

function TmafINIVarController.ReadString(VarName: String): String;
var bDoDisconnect: Boolean;
begin
  bDoDisconnect := Not FbConnected;
  If Not FbConnected Then
    Connect;
  Result := FpIniFile.ReadString(sINI_SECTION_STRING, VarName, FsDefaultString);
  If bDoDisconnect Then
    Disconnect;
end;

procedure TmafINIVarController.WriteVar(pData: PVarRec);
var bDoDisconnect: Boolean;
begin
  If bClosing Then
    Exit;
    
  If (Not (bClosing) And (vcfSaveOnClose in FVarControllerOptions)) Then
    Exit;

  bDoDisconnect := Not FbConnected;
  If Not FbConnected Then
    Connect;
  If Not FbConnected Then
    Exit;
  Case pData^.Typ Of
    vtString  : FpIniFile.WriteString(sINI_SECTION_STRING, String(pData^.Name), String(PChar(pData^.Var1)));
    vtInteger : FpIniFile.WriteInteger(sINI_SECTION_INTEGER, String(pData^.Name), Integer(pData^.Var1));
    vtBoolean : FpIniFile.WriteInteger(sINI_SECTION_BOOLEAN, String(pData^.Name), Integer(pData^.Var1));
  end;
  If bDoDisconnect Then
    Disconnect;
end;

end.
