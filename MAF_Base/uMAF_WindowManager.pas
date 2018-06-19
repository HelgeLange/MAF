{*******************************************************************************
Name         : uMAF_WindowManager.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2002-2009 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 28.08.2004
Last Update  : 26.10.2009
Version      : 1.0.007
Last Changes :

1.0.007 (26.10.2009)------------------------------------------------------------
- [ADD] added the possibility to store the main form pointer, that it can be asked
        for from the TmafWindowController
- [CHG] moved some API commands to a new HM_WINDOWMANAGER_QUERY as Sub-commands
        to reduce the amount of functions registered with the ModuleController
1.0.006 (24.07.2009)------------------------------------------------------------
- [CHG] changed TmafWindowManager to inherit from TmafCustomManagerComponent
        therefore the obsolet HookAccessContainer stuff was deleted and we now
        use the ModuleController defined in the new CustomClass
1.0.005 (11.10.2008)------------------------------------------------------------
- [ADD] WM__FIND_WINDOW_ID added as request message to find a window by its FormID
- [ADD] FindWindowByFormID added as function behind the message WM__FIND_WINDOW_ID
1.0.004 (26.09.2008)------------------------------------------------------------
- [ADD] Transformation to full Manager wich can be used like all other manager
        in the ERP Framework Component family
- [DEL] all code that uses forms, the manager communicates now only through
        the TERPWindowController component
1.0.003 (09.02.2007)------------------------------------------------------------
- [ADD] added OnChange-Event to inform about adding/removing windows instead of
        accessing direct the TfWindowList-form through a pointer
1.0.002 (07.11.2004)------------------------------------------------------------
- [FIX] TClientWindow.FindWindow fixed, can now find also the windows if there
        are more than one from the same type
- [ADD] code to give same window types numbers has been re-written completly
- [ADD] Window flags added (WM_GET_WINDOWINFO)
1.0.001 (17.09.2004)------------------------------------------------------------
- [ADD] Dirty flag
1.0.000 (28.08.2004)------------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_WindowManager;

interface

uses Windows, Messages, SysUtils, Classes, Forms,
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_WindowController, uMAF_Core;

Type TClientWindow = class(TObject)
     private
       FpClientList : TList;
       FpWindow : Pointer;
       FpParent : TClientWindow;
       FpController : TmafWindowController;
       FbDirty  : Boolean;
       FOnDirty : TNotifyEvent;
       FiDirtyCount : Integer;
       function __GetHasChildren: Boolean;
       procedure __SetID(const Value: Integer);
       function __GetID: Integer;
       procedure __SetWindow(const Value: Pointer);
       procedure __SetDirty(const Value: Boolean);
       procedure __SetController(const Value: TmafWindowController);
       function __GetFormID: Integer;
       function __GetCaptionID: Integer;
     public
       constructor Create;
       destructor Destroy; override;
       function Close(bImmediately: Boolean): Integer;
       function CloseClients: Integer;
       function CanClose : Boolean;
       function Save : Boolean;
       function AddChild(aWnd : TClientWindow): Boolean;
       function FindWindow(aWindow: Pointer): TClientWindow;
       property HasChildren : Boolean read __GetHasChildren;
       property pWindow : Pointer read FpWindow write __SetWindow;
       property Parent : TClientWindow read FpParent write FpParent;
       property ClientList : TList read FpClientList;
       property ID : Integer read __GetID write __SetID;
       property FormID : Integer read __GetFormID;
       property CaptionID : Integer read __GetCaptionID;
       property bDirty : Boolean read FbDirty write __SetDirty;
       property Controller : TmafWindowController read FpController write __SetController;
     end;

     RFormIDReg = packed record
       nFormID : Integer;
       nHookID : Integer;
       nSubHookID : Integer;
     end;
     PFormIDReg = ^RFormIDReg;

     TmafWindowManager = class(TmafCustomManagerComponent)
     private
       FpWindowList : TList;
       FOnChange : TNotifyEvent;
       FpProgramRoot : TClientWindow;
       FpMainWindow : Pointer;
       FpFormIDList : TList;
       function __GetWindowNumber(HookID, SubHookID : Integer): Integer;
     protected
       procedure __OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer); override;
       procedure __RegisterAPI; override;
       function __Query_WindowManager(nQueryID : Integer; QHS: pQHS; UserParam: Pointer): Integer;

       procedure __AddWindow(aController: Pointer);
       procedure __RegisterFormID(QHS: pQHS);
       procedure __GetFormID_Data(QHS: pQHS);
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       function RemoveWindow(aController: TmafWindowController): Integer;
       function RemoveMyClients(aWindow: Pointer): Integer;
       function GetClientCount(aWindow: Pointer): Integer;
       function FindWindow(aWindow: Pointer): TClientWindow;
       function FindWindowByFormID(nFormID: Integer): Pointer;
       function FindWindowByHookID(HookID, SubHookID: Integer): Pointer;
       procedure SetDirtyFlag(aWindow: Pointer; bDirty: Boolean);
       procedure Dispatch_Message_To_Parent(aWindow: Pointer; Msg: TMessage);
       property WindowList : TList read FpWindowList;
       property ProgramRoot : TClientWindow read FpProgramRoot;
     published
       property OnChange : TNotifyEvent read FOnChange write FOnChange;
     end;

var WM : TmafWindowManager;

implementation

uses uMAF_Tools;

const MAX_EVENTS = 2;

var ManagerEvents : array[1..MAX_EVENTS] of Integer = (
      HM_WINDOWMANAGER_QUERY,
//      HM_GET_WINDOWMANAGER,
      WM__FIND_WINDOW_BY_HOOK);

{ TmafWindowManager }

constructor TmafWindowManager.Create(AOwner: TComponent);
begin
  inherited;
  FpWindowList := TList.Create;
  ManagerType := MT_WINDOW_MANAGER;
  FpProgramRoot := nil;
  FpMainWindow := nil;
  FpFormIDList := TList.Create;
end;

destructor TmafWindowManager.Destroy;
var i : Integer;
begin
  For i := FpWindowList.Count - 1 DownTo 0 Do
    If TClientWindow(FpWindowList[i]) <> FpProgramRoot Then begin
      TClientWindow(FpWindowList[i]).Close(True);
      TClientWindow(FpWindowList[i]).Free;
      FpWindowList.Delete(i);
    end;
  For i := 0 To FpFormIDList.Count - 1 Do
    Dispose(PFormIDReg(FpFormIDList.Items[i]));
  FpWindowList.Free;
  FpProgramRoot.Free;
  FpFormIDList.Free;
  inherited;
end; // Destroy

procedure TmafWindowManager.__GetFormID_Data(QHS: pQHS);
var i: Integer;
begin
  For i := 0 To FpFormIDList.Count - 1 Do
    If PFormIDReg(FpFormIDList.Items[i])^.nFormID = QHS^.Reserved1 Then begin
      QHS^.Reserved2 := PFormIDReg(FpFormIDList.Items[i])^.nHookID;
      QHS^.PReserved1 := Pointer(PFormIDReg(FpFormIDList.Items[i])^.nSubHookID);
      Break;
    end;
end;

function TmafWindowManager.__GetWindowNumber(HookID, SubHookID: Integer): Integer;
var i, MaxID : Integer;
begin
  MaxID := 0;
  For i := 0 To FpWindowList.Count - 1 Do begin
    If ((TClientWindow(FpWindowList[i]).Controller.HookID = HookID) And (TClientWindow(FpWindowList[i]).Controller.SubHookID = SubHookID)) Then
      If TClientWindow(FpWindowList[i]).ID > MaxID Then
        MaxID := TClientWindow(FpWindowList[i]).ID;
  end;  //  --  For i := 0 To FpWindowList.Count - 1 Do
  MaxID := MaxID + 1;
  Result := MaxID;
end;

procedure TmafWindowManager.__OnEvent(SubHookID: Integer; QHS: pQHS; UserParam: Pointer; var ErrCode: Integer);
begin
  Case SubHookID Of
    HM_WINDOWMANAGER_QUERY  : ErrCode := __Query_WindowManager(QHS^.SubHookID, QHS, UserParam);
//    HM_GET_WINDOWMANAGER    : QHS^.pChildObj := Self;
    WM__FIND_WINDOW_BY_HOOK : QHS^.pChildObj := FindWindowByHookID(QHS^.HookID, QHS^.SubHookID);
    Else inherited __OnEvent(SubHookID, QHS, UserParam, ErrCode);
  end;  //  --  Case SubHookID Of
end;

function TmafWindowManager.__Query_WindowManager(nQueryID: Integer; QHS: pQHS; UserParam: Pointer): Integer;
begin
  Result := ERR_NO_ERROR;
  Case nQueryID Of
    WM__ADD_WINDOW     : __AddWindow(UserParam);
    WM__REMOVE_WINDOW  : RemoveWindow(UserParam);
    WM__FIND_WINDOW_ID : QHS^.pChildObj := FindWindowByFormID(QHS^.ResVal);
    WM__SET_MAINFORM   : FpMainWindow := UserParam;
    WM__GET_MAINFORM   : QHS^.pChildObj := FpMainWindow;
    WM__GET_WINDOW_NO  : QHS^.ResVal := __GetWindowNumber(QHS^.Reserved1, QHS^.Reserved2);
    WM__GET_WINDOWMANAGER : QHS^.pChildObj := Self;
    WM__REGISTER_FORM_ID  : __RegisterFormID(QHS);
    WM__GET_FORM_ID_DATA  : __GetFormID_Data(QHS);
  end;
end;

procedure TmafWindowManager.__RegisterAPI;
var i : Integer;
begin
  inherited;
  If Assigned(ModuleController) Then
    For i := 1 To MAX_EVENTS Do
      ModuleController.RegisterSubHookEvent(ManagerEvents[i], __OnEvent);
end; // __RegisterAPI

procedure TmafWindowManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end; // Notification

procedure TmafWindowManager.__RegisterFormID(QHS: pQHS);
var pData : PFormIDReg;
begin
  New(pData);
  pData^.nFormID := QHS^.Reserved1;
  pData^.nHookID := QHS^.Reserved2;
  pData^.nSubHookID := Integer(QHS^.PReserved1);
  FpFormIDList.Add(pData);
end;

// ********************************* Comments **********************************
// Description : adds a window
// Param (in)  : aParent=nil, wenn Rootwindow, ansonsten Pointer des ParentWindows
//               aWindow=Zeiger des Fensters selbst
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.08.2004
// Last Update : 29.09.2008
// *****************************************************************************
procedure TmafWindowManager.__AddWindow(aController: Pointer);
var aWnd, ParentWnd : TClientWindow;
    i, MaxID, FormID : Integer;
begin
  If woRootWindow in TmafWindowController(aController).WindowOptions Then begin
    // the root window registers itself
    FpProgramRoot := TClientWindow.Create;
    FpProgramRoot.Controller := aController;
    FpProgramRoot.Parent := nil;
    FpWindowList.Add(FpProgramRoot);
  end else begin
    If TmafWindowController(aController).ParentController = FpProgramRoot.Controller Then begin
      // a window directly in the root window registers itself
      aWnd := TClientWindow.Create;
      aWnd.Controller := aController;
      FpProgramRoot.AddChild(aWnd);
      // only windows in the root need a Window number
      If woWindowNumber in aWnd.Controller.WindowOptions Then begin
        MaxID := 0;
        FormID := aWnd.FormID;
        For i := 0 To FpWindowList.Count - 1 Do begin
          If TClientWindow(FpWindowList[i]).Controller.FormID = FormID Then
            If TClientWindow(FpWindowList[i]).ID > MaxID Then
              MaxID := TClientWindow(FpWindowList[i]).ID;
        end;  //  --  For i := 0 To FpWindowList.Count - 1 Do
        MaxID := MaxID + 1;
        aWnd.ID := MaxID;
        aWnd.Controller.FnWindowNumber := MaxID;
      end;
      FpWindowList.Add(aWnd);
    end else begin
      // it's a Window in a Window in a window...
      aWnd := TClientWindow.Create;
      aWnd.Controller := aController;
      ParentWnd := nil;
      For i := 0 To FpWindowList.Count - 1 Do
        If TClientWindow(FpWindowList.Items[i]).FpController = aWnd.Controller.ParentController Then begin
          ParentWnd := TClientWindow(FpWindowList.Items[i]);
          Break;
        end;
      If ParentWnd <> nil Then
        ParentWnd.AddChild(aWnd);
      FpWindowList.Add(aWnd);
    end;
  end;
  If Assigned(FOnChange) Then
    FOnChange(Self);
end; // AddWindow

// ********************************* Comments **********************************
// Description : deletes a window including its children
// Param (in)  : aController=Window controller for that window
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.08.2004
// Last Update : 29.09.2008
// *****************************************************************************
function TmafWindowManager.RemoveWindow(aController: TmafWindowController): Integer;
var aWnd : TClientWindow;
    i : Integer;
begin
  Result := ERR_NO_ERROR;
  aWnd := nil;
  For i := 0 To FpWindowList.Count - 1 Do
    If TClientWindow(FpWindowList.Items[i]).Controller = aController Then begin
      aWnd := TClientWindow(FpWindowList.Items[i]);
      Break;
    end;
  If aWnd <> nil Then begin
    FpWindowList.Delete(FpWindowList.IndexOf(aWnd));
    If aWnd = FpProgramRoot Then
      FpProgramRoot := nil;
    aWnd.Free;
  end;
  If Assigned(FOnChange) Then
    FOnChange(Self);
end; // RemoveWindow

// ********************************* Comments **********************************
// Description : liefert die Anzahl der Clients unter einem Fenster
// Param (in)  : aWindow=Fenster, dessen Client-Anzahl wir wissen wollen
// Param (out) : Anzahl der Clients
// Coding by   : Helge Lange
// Date        : 28.08.2004
// Last Update : 28.08.2004
// *****************************************************************************
function TmafWindowManager.GetClientCount(aWindow: Pointer): Integer;
var aWnd : TClientWindow;
begin
  Result := 0;
  aWnd := FindWindow(aWindow);
  If Assigned(aWnd) Then
    Result := aWnd.FpClientList.Count;
end; // GetClientCount

// ********************************* Comments **********************************
// Description : schließt die Clients eines Fensters
// Param (in)  : aWindow=Fenster, dessen Clients geschlossen werden sollen
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.08.2004
// Last Update : 17.09.2004
// *****************************************************************************
function TmafWindowManager.RemoveMyClients(aWindow: Pointer): Integer;
var aWnd : TClientWindow;
begin
  Result := ERR_NO_ERROR;
  aWnd := FindWindow(aWindow);   // kennen wir das Fenster ?
  If Assigned(aWnd) Then
    Result := aWnd.CloseClients; // wenn ja, dann schließen wir dessen Clients
  If Assigned(FOnChange) Then
    FOnChange(Self);
end; // RemoveMyClients

// ********************************* Comments **********************************
// Description : findet ein Fenster im Baum
// Param (in)  : aWindow=Fenster welches gefunden werden soll
// Param (out) : Zeiger auf unser internes Fenster-Object
// Coding by   : Helge Lange
// Date        : 28.08.2004
// Last Update : 28.08.2004
// *****************************************************************************
function TmafWindowManager.FindWindow(aWindow: Pointer): TClientWindow;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpWindowList.Count - 1 Do
    If TClientWindow(FpWindowList[i]).HasChildren Then begin
      Result := TClientWindow(FpWindowList[i]).FindWindow(aWindow);
      If Assigned(Result) Then
        Break;
    end else begin
      If TClientWindow(FpWindowList[i]).pWindow = aWindow Then begin
        Result := TClientWindow(FpWindowList[i]);
        Break;
      end;
    end;  //  --  If TClientWindow(FpClientList[i]).HasChildren Then
end; // FindWindow

function TmafWindowManager.FindWindowByFormID(nFormID: Integer): Pointer;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpWindowList.Count - 1 Do
    If TClientWindow(FpWindowList[i]).FpController.FormID = nFormID Then
      If Not TClientWindow(FpWindowList[i]).FpController.bClosing Then begin
        Result := TClientWindow(FpWindowList[i]).FpWindow;
        Break;
      end;
end;

function TmafWindowManager.FindWindowByHookID(HookID, SubHookID: Integer): Pointer;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpWindowList.Count - 1 Do
    If ((TClientWindow(FpWindowList[i]).FpController.HookID = HookID) And (TClientWindow(FpWindowList[i]).FpController.SubHookID = SubHookID)) Then
      If Not TClientWindow(FpWindowList[i]).FpController.bClosing Then begin
        Result := TClientWindow(FpWindowList[i]).FpController;
        Break;
      end;
end;

// ********************************* Comments **********************************
// Description : setzt ein Fenster auf Dirty
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 17.09.2004
// Last Update : 17.09.2004
// *****************************************************************************
procedure TmafWindowManager.SetDirtyFlag(aWindow: Pointer; bDirty: Boolean);
var aWnd : TClientWindow;
begin
  aWnd := FindWindow(aWindow);  // kennen wir das Fenster ?
  If Assigned(aWnd) Then begin
    If Not aWnd.bDirty Then begin  // ist das Fenster noch nicht dirty ?
      aWnd.bDirty := bDirty;       // also setzen wir es jetzt dirty
      While aWnd.Parent <> nil Do begin  // solange wir einen Parent haben...
        // hat unser Parent einen Parent, ist er noch ned top most
        If Assigned(aWnd.Parent.Parent) Then begin
          aWnd := aWnd.Parent;           // wir schnappen uns also unseren Parent
          Continue;                      // und machen einen neuen Durchgang
        end;  //  --  If Assigned(aWnd.Parent.Parent) Then
        aWnd.Parent.bDirty := bDirty; // wir haben also unser top most gefunden und setzen das flag dort auch
        aWnd := aWnd.Parent;           // wir schnappen uns also unseren Parent
      end;  //  --  While aWnd.Parent <> nil Do
    end;  //  --  If Not aWnd.bDirty Then
  end;  //  --  If Assigned(aWindow) Then
end; // SetDirtyFlag

// ********************************* Comments **********************************
// Description : schickt eine Nachricht zum ParentWindow
// Param (in)  : aWindow=ClientWindow, dess Parent die Nachricht bekommen soll
//               Msg=Message, welche zugestellt werden soll
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 07.11.2004
// Last Update : 07.11.2004
// *****************************************************************************
procedure TmafWindowManager.Dispatch_Message_To_Parent(aWindow: Pointer; Msg: TMessage);
var aWnd : TClientWindow;
begin
  aWnd := FindWindow(aWindow);                               // kennen wir das Fenster ?
  If Assigned(aWnd) Then begin                               // haben wir ein gültiges ClientWindow gefunden ?
    If Assigned(aWnd.FpParent) Then                          // hat es einen Parent-ClientWindow ?
      If Assigned(aWnd.FpParent.FpWindow) Then               // ist dort ein gültiger Fenster-Zeiger drin ?
        TForm(aWnd.FpParent.FpWindow).Dispatch(Msg);         // dann stellen wir die Nachricht zu *puh!*
  end;
end; // Dispatch_Message







{ TClientWindow }

constructor TClientWindow.Create;
begin
  FpClientList := TList.Create;
  FpController := nil;
  FpParent := nil;
  FiDirtyCount := 0;
  FOnDirty := nil;
end; // Create

destructor TClientWindow.Destroy;
begin
//  Close;
  FpClientList.Free;
  inherited;
end; // Destroy

// ********************************* Comments **********************************
// Description : Abfrage, ob Children vorhanden sind
// Param (in)  : N/A
// Param (out) : True, wenn Kindfenster da sind
// Coding by   : Helge Lange
// Date        : 28.08.2004
// Last Update : 28.08.2004
// *****************************************************************************
function TClientWindow.__GetHasChildren: Boolean;
begin
  Result := (FpClientList.Count > 0);
end; // __GetHasChildren

// ********************************* Comments **********************************
// Description : fügt ein Child hinzu, wenn Object selbst nicht Parent ist, dann
//               wird probiert, ob eines der Kinder der Parent ist
// Param (in)  : aParent=übergeordnetes Fenster
//               aWindow=Zeiger des Fensters selbst
// Param (out) : True, wenn eines der Kinder oder Object selbst Parent ist
// Coding by   : Helge Lange
// Date        : 28.08.2004
// Last Update : 07.11.2004
// *****************************************************************************
function TClientWindow.AddChild(aWnd : TClientWindow): Boolean;
{var aWnd : TClientWindow;
    i, MaxID : Integer; }
begin
  Result := True;
  FpClientList.Add(aWnd);
  aWnd.Parent := Self;
{  If aParent = FpWindow Then begin
    aWnd := TClientWindow.Create;
    aWnd.pWindow := aWindow;
    aWnd.Parent := Self;
    FpClientList.Add(aWnd);
    Result := True;
    // für TfDLLForm muss eine Caption-Nummer gefunden werden
    If IsClass(aWindow, TfDLLForm) Then begin
      MaxID := 1;
      For i := 0 To WM.FpWindowList.Count - 1 Do
        WM.GetNextCaptionNumber(TClientWindow(WM.FpWindowList[i]), TfDLLForm(aWindow).WC.FormID, MaxID);
      aWnd.ID := MaxID;
    end;  //  --  If IsClass(aWindow, TfDLLForm) Then
    Exit;
  end;  //  --  If Not Assigned(aParent) Then

  For i := 0 To FpClientList.Count - 1 Do
    If TClientWindow(FpClientList[i]).AddChild(aParent, aWindow) Then begin
      TClientWindow(FpClientList[i]).Parent := Self;
      Result := True;
      Break;
    end; } //  --  If TClientWindow(FpClientList[i]).AddChild(aParent, aWindow) Then
end; // AddChild

// ********************************* Comments **********************************
// Description : allen untergeordneten Fenster wird ein Close geschickt
// Param (in)  : N/A
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 28.08.2004
// Last Update : 29.08.2004
// *****************************************************************************
function TClientWindow.Close(bImmediately: Boolean): Integer;
begin
//  If woRootWindow in Controller.WindowOptions Then
//    Exit;
  Result := CloseClients;  // wir schließen alle Client-Fenster
  If bImmediately Then
    SendMessage(TForm(pWindow).Handle, WM_CLOSE, 0, 0)  // wir schließen uns selbst sofort
  Else
    PostMessage(TForm(pWindow).Handle, WM_CLOSE, 0, 0); // wir schließen uns selbst
  pWindow := nil;
end; // Close

function TClientWindow.CanClose: Boolean;
var i : Integer;
begin
  Result := Not FbDirty;  // wenn Dirty, dann False zurückgeben

  If Result Then  // Wenn wir uns schließen könnten ...
    // dann alle Client durchgehen und fragen, ob sie geschlossen werden können
    If FpClientList.Count > 0 Then
      For i := 0 To FpClientList.Count - 1 Do
        If Not TClientWindow(FpClientList[i]).CanClose Then begin
          Result := False;  // das Fenster meldete, daß es nicht geschlossen werden kann
          Exit;             // also Abbruch!
        end;
end; // CanClose

function TClientWindow.Save: Boolean;
var Msg : TMessage;
    i : Integer;
begin
  Result := True;  // optimistischer Ansatz

  // erst sichern wir uns
  If Result Then begin
    FillChar(Msg, SizeOf(TMessage), 0);
    Msg.Msg := WM_SAVE_DATA;
    TComponent(pWindow).Dispatch(Msg);
    If Msg.Result = 13 Then  // wir haben etwas gegen das Sichern
      Result := False;
  end;
  // dann alle Client durchgehen und sie auffordern, sich zu sichern
  If FpClientList.Count > 0 Then
    For i := 0 To FpClientList.Count - 1 Do
      If Not TClientWindow(FpClientList[i]).Save Then begin
        Result := False;  // das Fenster meldete, daß es sich nicht sichern kann
        Exit;             // also Abbruch!
      end;
end; // Save

// ********************************* Comments **********************************
// Description : call all clients to close themselves
// Param (in)  : N/A
// Param (out) : Standard ErrorCodes
// Coding by   : Helge Lange
// Date        : 28.08.2004
// Last Update : 17.09.2004
// *****************************************************************************
function TClientWindow.CloseClients: Integer;
const Msg_DoSaveData : String = 'Data not saved yet, do that now ?';
      Msg_SaveError  : String = 'Could not save the data !';
begin
  Result := ERR_NO_ERROR;
{  If Not CanClose Then begin
    // TODO : Resclient to get localized messages
    Case MessageDlg(Msg_DoSaveData, mtConfirmation, [mbYes,mbNo,mbCancel], 0) Of
      mrCancel :
        begin
          Result := ERR_CANCEL_CLOSE;  // Schließen abbrechen
          Exit;
        end;
      mrYes :
        begin
          If Not Save Then begin
            MessageDlg(Msg_SaveError, mtWarning, [mbOk], 0);
            Result := ERR_DS_SAVE_ERROR;
            Exit;
          end;
        end;
    end;  //  --  Case MessageDlg('Dat...
  end;  //  --  If Not CanClose Then   }

  // sind wir hier, können wir die Fenster killen
  While FpClientList.Count > 0 Do begin
    Result := TClientWindow(FpClientList[0]).Close(True);
    If Result = ERR_NO_ERROR Then begin
      TClientWindow(FpClientList[0]).Free;
      FpClientList.Delete(0);
    end else
      Break;
  end;  //  --  While FpClientList.Count > 0 Do
end; // CloseClients

// ********************************* Comments **********************************
// Description : findet ein Fenster im Baum
// Param (in)  : aWindow=Fenster welches gefunden werden soll
// Param (out) : Zeiger auf unser internes Fenster-Object
// Coding by   : Helge Lange
// Date        : 28.08.2004
// Last Update : 28.08.2004
// *****************************************************************************
function TClientWindow.FindWindow(aWindow: Pointer): TClientWindow;
var i : Integer;
begin
  Result := nil;
  // sind wir selbst das gesuchte Fenster ?
  If aWindow = pWindow Then begin
    Result := Self;               // wir geben uns zurück...
    Exit;                         // ... und verschwinden
  end;  //  --  If aWindow = pWindow Then

  For i := 0 To FpClientList.Count - 1 Do
    If TClientWindow(FpClientList[i]).HasChildren Then begin
      Result := TClientWindow(FpClientList[i]).FindWindow(aWindow);
      If Assigned(Result) Then
        Exit;
    end Else
      If TClientWindow(FpClientList[i]).pWindow = aWindow Then begin
        Result := TClientWindow(FpClientList[i]);
        Exit;
      end;
end; // FindWindow

procedure TClientWindow.__SetID(const Value: Integer);
begin
  If FpController <> nil Then begin

    If Not (woWindowNumber in FpController.WindowOptions) Then
      Exit;

    FpController.FnWindowNumber := Value;
  end;  //  --  If FpController <> nil Then
end; // __SetID

function TClientWindow.__GetID: Integer;
begin
  Result := -1;
  If FpController <> nil Then
    Result := FpController.FnWindowNumber;
end; // __GetID

function TClientWindow.__GetFormID: Integer;
begin
  Result := 0;
  If FpController <> nil Then
    Result := FpController.FormID;
end; // __GetFormID

function TClientWindow.__GetCaptionID: Integer;
begin
  Result := 0;
  If FpController <> nil Then
    Result := FpController.CaptionID;
end;

// ********************************* Comments **********************************
// Description : kriegen wir das Fenster, fragen wir erstmal, was es für eins ist
// Param (in)  : Value=Zeiger auf das Fenster
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 29.08.2004
// Last Update : 03.10.2004
// *****************************************************************************
procedure TClientWindow.__SetWindow(const Value: Pointer);
begin
  FpWindow := Value;                   // wir merken uns den Pointer
end; // __SetWindow

// ********************************* Comments **********************************
// Description : ein Fenster meldet, daß es verändert wurde, wir geben es zum
//               obersten Fenster durch
// Param (in)  : Value=True, wenn Dirty
// Param (out) : N/A
// Coding by   : Helge Lange
// Date        : 17.09.2004
// Last Update : 17.09.2004
// *****************************************************************************
procedure TClientWindow.__SetDirty(const Value: Boolean);
begin
  FbDirty := Value;
  If FiDirtyCount = 0 Then begin

  end;
end;

procedure TClientWindow.__SetController(const Value: TmafWindowController);
begin
  FpController := Value;
  If FpController <> nil Then begin
    FpWindow := FpController.Owner;
  end;
end;

end.
