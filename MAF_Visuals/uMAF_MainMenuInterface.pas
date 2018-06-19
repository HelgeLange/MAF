{*******************************************************************************
Name         : uMAF_MainMenuInterface.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2008-2009 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 22.09.2008
Last Update  : 24.07.2009
Version      : 1.0.002
Purpose      : creates a simple connection between a TMainMenu component and the
               MA Framework Components and allows to create dynamic main menus
               in the application
Last Changes :

1.0.002 (24.07.2009) -----------------------------------------------------------
- [CHG] change of the interface to enumerate the menu items to work together
        with the new TmafModuleController
- [FIX] fixed a bug in TmafMenuItem.Destroy where the ResClient was called without
        check, if it was assigned to the component
1.0.001 (13.06.2009) -----------------------------------------------------------
- [CHG] changed the procedure TmafMainMenuInterface.AddMenuItem to accept
        PMenuItemData pointer as parameter now as they come from the FAM
1.0.000 (22.09.2008) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_MainMenuInterface;

interface

uses SysUtils, Windows, Classes, Menus, Messages,
     // Modular Application Framework Components units
     uMAF_Core, uMAF_ResourceClient, uMAF_Globals, uMAF_HookClient;

Type RMainMenuData = packed record
       CallHookID : Integer;       // Hook to call, when menu item was clicked
       NameID     : Integer;       // id to a string including the parent menu IDs (format : " 123  234  567 ".. spaces are important for parser !)
       ImageID    : Integer;       // ImageID for the ResourceManager to get an image for the menu item
       Flags      : Integer;       // flags for future use of the TmafMainMenuInterface
       SubHookID  : Integer;       // in case of the need to place several different menu items into one Dynamic function, each one can use another SubHookID
       DataHookID : Integer;       // HookID to get needed data
       DataSubHookID : Integer;    // SubHookID to get needed data 
     end; // RMainMenuData
     PMainMenuData = ^RMainMenuData;

     TmafMenuItem = class(TMenuItem)
     private
       FiHookID : Integer;
       FiSubHookID : Integer;
       FiCaptionID : Integer;
       FpResClient : TmafResourceClient;
       FiDataHookID : Integer;
       FiDataSubHookID : Integer;
       procedure __SetResClient(const Value: TmafResourceClient);
     protected
       procedure WM_LanguageChange(var Message: TMessage); message WM_LANGUAGE_CHANGE;
     public
       pWindow : Pointer;
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       property HookID : Integer read FiHookID write FiHookID;
       property SubHookID : Integer read FiSubHookID write FiSubHookID;
       property CaptionID : Integer read FiCaptionID write FiCaptionID;
       property ResClient : TmafResourceClient read FpResClient write __SetResClient;
       property DataSubHookID : Integer read FiDataSubHookID write FiDataSubHookID;
       property DataHookID : Integer read FiDataHookID write FiDataHookID;
     end;

     // MenuItems will be added in a specific location using the parent items
     // Parent items can be found through their names, in order to search them
     // within the component, they must be send in parameter sParent in following
     // format : SPACE CaptionID SPACE SPACE CaptionID SPACE....SPACE CaptionID SPACE
     // CaptionID is the ID used to create the Caption and is stored in the
     // property Tag
     TmafMainMenuInterface = class(TmafBaseComponent)
     private
       FpMainMenu   : TMainMenu;
       FpResClient  : TmafResourceClient;
       FpHookClient : TmafHookClient;
       FpParent     : Pointer;
     protected
       procedure __ExecuteHook(Sender: TObject); // OnClick-Event-Handler for each MenuItem
       procedure BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
       procedure ACR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
       function AddMenuItemIntern(pData: PMainMenuData): TmafMenuItem;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure AddMenuItem(QHS: pQHS);
       property Parent : Pointer read FpParent write FpParent;
     published
       property MainMenu : TMainMenu read FpMainMenu write FpMainMenu;
       property ResClient : TmafResourceClient read FpResClient write FpResClient;
       property HookClient : TmafHookClient read FpHookClient write FpHookClient;
     end;

implementation

uses uMAF_Tools;

{ TmafMainMenuInterface }

procedure TmafMainMenuInterface.BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin
  QHS^.Reserved2 := FpHookClient.PopVar.AsInteger;  // DataSubHookID
  QHS^.Reserved1 := FpHookClient.PopVar.AsInteger;  // DataHookID
  QHS^.pChildObj := FpHookClient.PopVar.AsPointer;
  QHS^.pParent := FpHookClient.PopVar.AsPointer;
end;

procedure TmafMainMenuInterface.ACR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin
  FpHookClient.PushVar.AsPointer := QHS^.pChildObj;
end;

constructor TmafMainMenuInterface.Create(AOwner: TComponent);
begin
  inherited;
  FpResClient := nil;
  FpMainMenu := nil;
  FpHookClient := nil;
end;

destructor TmafMainMenuInterface.Destroy;
begin
  inherited;
end;

procedure TmafMainMenuInterface.__ExecuteHook(Sender: TObject);
begin
  If FpHookClient <> nil Then begin
    FpHookClient.PushEvents;
    FpHookClient.BeforeCallRouter := BCR;
    FpHookClient.AfterCallRouter := ACR;
    FpHookClient.PushVar.AsPointer := FpParent;
    FpHookClient.PushVar.AsPointer := TmafMenuItem(Sender).pWindow;
    FpHookClient.PushVar.AsInteger := TmafMenuItem(Sender).DataHookID;
    FpHookClient.PushVar.AsInteger := TmafMenuItem(Sender).DataSubHookID;
    FpHookClient.ExecuteHook(TmafMenuItem(Sender).HookID, TmafMenuItem(Sender).SubHookID);
    TmafMenuItem(Sender).pWindow := FpHookClient.PopVar.AsPointer;
    FpHookClient.PopEvents;
  end;
end;

procedure TmafMainMenuInterface.AddMenuItem(QHS: pQHS);
var pData : PMainMenuData;
begin
  If QHS^.pChildObj = nil Then
    Exit;

  pData := PMainMenuData(QHS^.pChildObj);
  AddMenuItemIntern(pData);
end; // AddMenuItem

function TmafMainMenuInterface.AddMenuItemIntern(pData: PMainMenuData): TmafMenuItem;
var i, j : Integer;
    MI, pParent : TmafMenuItem;
    s, s1 : String;
begin
  If ((FpMainMenu = nil) Or (FpResClient = nil)) Then begin
    Result := nil;
    Exit;
  end;  //  --  If Not Assigned(FpMainMenu) Then
  S := FpResClient.GetString(pData^.NameID);   // ID chain for the menu structure
  pParent := nil;
  MI := nil;
  While Length(S) > 3 Do begin
    j := GetIDFromString(S);   // get next text ID
    If j > -1 Then begin
      S1 := FpResClient.GetString(j);
      If S1 <> '' Then begin
        If pParent = nil Then begin
          For i := 0 to FpMainMenu.Items.Count - 1 Do
            If TmafMenuItem(FpMainMenu.Items[i]).CaptionID = j Then
              pParent := TmafMenuItem(FpMainMenu.Items[i]);
          If Not Assigned(pParent) Then begin
            MI := TmafMenuItem.Create(FpMainMenu);
            MI.Caption := S1;
            MI.CaptionID := j;
            MI.ResClient := FpResClient;
            MI.SubHookID := pData^.SubHookID;
            MI.DataHookID := pData^.DataHookID;
            MI.DataSubHookID := pData^.DataSubHookID;
            FpMainMenu.Items.Add(MI);
            pParent := MI;
          end;  //  --  If Not Assigned(pParent) Then
        end else begin
          MI := TmafMenuItem(pParent.Find(S1));
          If MI = nil Then begin
            MI := TmafMenuItem.Create(pParent);
            MI.ResClient := FpResClient;
            MI.Caption := S1;
            MI.CaptionID := j;
            MI.SubHookID := pData^.SubHookID;
            MI.DataHookID := pData^.DataHookID;
            MI.DataSubHookID := pData^.DataSubHookID;
            pParent.Add(MI);
          end;  //  --  If MI = nil Then
          pParent := MI;
        end;  //  --  if pParent = nil Then
      end;  //  --  If S1 <> '' Then
    end;  //  --  If j > -1 Then
  end;  //  --  While Length(S) > 3 Do
  Result := MI;
  If MI <> nil Then begin
    MI.HookID := pData^.CallHookID;
    MI.OnClick := __ExecuteHook;
  end;
end;

{ TmafMenuItem }

constructor TmafMenuItem.Create(AOwner: TComponent);
begin
  inherited;
  pWindow := nil;
end;

destructor TmafMenuItem.Destroy;
begin
  If Assigned(FpResClient) Then
    FpResClient.DeleteNotifyComponent(Self);
  inherited;
end;

procedure TmafMenuItem.WM_LanguageChange(var Message: TMessage);
begin
  Caption := FpResClient.GetString(FiCaptionID);
end;

procedure TmafMenuItem.__SetResClient(const Value: TmafResourceClient);
begin
  FpResClient := Value;
  If FpResClient <> nil Then
    FpResClient.AddNotifyComponent(Self);
end;

end.
