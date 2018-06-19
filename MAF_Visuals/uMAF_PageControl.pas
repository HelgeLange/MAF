{*******************************************************************************
Name         : uMAF_PageControl.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2008-2015 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 22.09.2009
Last Update  : 26.06.2015
Version      : 1.0.007
Purpose      :
Last Changes :

1.0.007 (26.06.2015) -----------------------------------------------------------
- [FIX] when returning from creating a frame in AEH the pDatablock usage was
        wrong, the record already destroyed. In Delphi before Delphi XE2 it
        worked just fine, but after that it caused errors. Now it uses an internal
        TmafParameters
1.0.006 (28.05.2012) -----------------------------------------------------------
- [FIX] TmafPageControl was casting directly any incoming form to TForm, which
        causes problems with TFrames ;)
        This is fixed now.
1.0.005 (07.11.2011) -----------------------------------------------------------
- [ADD] TmafPageControl now uses the TmafWindowController.CaptionID for the
        TabSheet Caption, if CaptionID is greater than 0
        A TmafResourceClient must be connected to the WindowController
1.0.004 (24.03.2011) -----------------------------------------------------------
- [FIX] fixed __AddClientSheet_uID which checked for FnHookID > 0 and not the uID
        given when calling the function
1.0.003 (09.04.2010) -----------------------------------------------------------
- [ADD] event handler BeforeLoadSheets added, which is fired, before the client
        sheets will be loaded, which gives a possibility to pass data along
- [ADD] event handler OnTabSheetLoaded that is fired after every TabSheet loaded
- [ADD] event handler AfterLoading to indicate, that all client sheets are loaded
        now
1.0.002 (23.03.2010) -----------------------------------------------------------
- [ADD] added option pcoHokClientEvents with necessary code
- [CHG] TmafTabSheet.Client is not from type TForm anymore, but TWinControl to
        support TFrames also 
1.0.001 (22.09.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_PageControl;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ComCtrls, ExtCtrls,
     // Modular Application Framework Components units
     uMAF_HookClient, uMAF_LinkClient, uMAF_Globals, uMAF_Core, uMAF_Parameters,
     uMAF_ResourceClient, uMAF_WindowController;

     // pcoAutoLoad                 = TabSheets will be loaded automatically with FormInitialized event
     // pcoHookClientEvents         = When set, the original HookClient events set by the Form will be executed additionally to the internal ones             
Type TPageControlOption = (pcoAutoLoad, pcoHookClientEvents);
     TPageControlOptions = Set Of TPageControlOption;

     // This can be declared in the ModuleController as DataDefinition and can
     // be send through the Hook in the QueryHandlerStruct.pDataBlock
     // if more than FrameID and FrameName is necessary, the record can be
     // aumented in the users pas files, but the starting stucture should be the
     // same as this one, the first 2 item ins the record should be FrameID and
     // FrameName
     RBaseFrameData = packed record
       FrameID : Integer;              // normally the unique id, but it can be filled with whatever comes to mind
       FrameName : PChar;              // name of the frame, should be unique within a hook
     end;
     PBaseFrameData = ^RBaseFrameData;

     TmafTabSheet = class(TTabSheet)
     private
       FnSubHookID : Integer;
       FpClient : TWinControl;
       FpController : TmafWindowController;
       FEnabled : Boolean;
       FnFrameID : Integer;
       FsFrameName : String;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       property SubHookID : Integer read FnSubHookID write FnSubHookID;
       property Client : TWinControl read FpClient write FpClient;
       property Controller : TmafWindowController read FpController write FpController;
       property Enabled : Boolean read FEnabled write FEnabled default True;
       property FrameID : Integer read FnFrameID;
       property FrameName : String read FsFrameName;
     end;

     TOnTabSheetLoaded = procedure (Sender: TObject; ATabSheet: TmafTabSheet) Of Object;

     TmafPageControl = class(TPageControl)
     private
       FnHookID : Integer;
       FpHookClient : TmafHookClient;
       FpResClient : TmafResourceClient;
       FPageControlOptions : TPageControlOptions;
       FpParameters : TmafParameters;
       FnRandomID : Integer;
       FnLocalMode : Integer;
       Old_ACR,
       Old_BCR : TCallRouter;
       Old_BEH,
       Old_AEH : TExecuteHook;
       FBeforeLoadSheets : TCallRouter;
       FAfterLoading : TNotifyEvent;
       FOnTabSheetLoaded : TOnTabSheetLoaded;
       FBeforeTabsheetUnload : TOnTabSheetLoaded;
       procedure __SetResClient(const Value: TmafResourceClient);
     protected
       procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
       function CanChange: Boolean; override;

       procedure BEH(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
       procedure AEH(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
       procedure BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
       procedure ACR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
       procedure MSG_BeforeFormClose(var Msg: TMessage); message MSG_BEFORE_FORM_CLOSE;
       procedure MSG_LanguageChange(var Msg: TMessage); message WM_LANGUAGE_CHANGE;
       procedure MSG_FunctionObserver_Add(var Msg: TMessage); message MSG_FUNCTION_OBSERVER_ADD;
       procedure MSG_FunctionObserver_Delete(var Msg: TMessage); message MSG_FUNCTION_OBSERVER_DEL;
     public
       constructor Create(AOwner : TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       procedure LoadClientSheets;
       procedure Clear;
       procedure __AddClientSheet(nSubHookID : Integer);
       procedure __AddClientSheet_uID(uID: Integer);
       procedure __DeleteClientSheet(idx: Integer);
     published
       property HookClient : TmafHookClient read FpHookClient write FpHookClient;
       property ResourceClient : TmafResourceClient read FpResClient write __SetResClient;
       property HookID : Integer read FnHookID write FnHookID;
       property PageControlOptions : TPageControlOptions read FPageControlOptions write FPageControlOptions;
       property BeforeLoadSheets : TCallRouter read FBeforeLoadSheets write FBeforeLoadSheets;
       property OnTabSheetLoaded : TOnTabSheetLoaded read FOnTabSheetLoaded write FOnTabSheetLoaded;
       property AfterLoading : TNotifyEvent read FAfterLoading write FAfterLoading;
       property BeforeTabsheetUnload : TOnTabSheetLoaded read FBeforeTabsheetUnload write FBeforeTabsheetUnload;
     end;

implementation

uses uMAF_Tools;

{ TmafPageControl }

constructor TmafPageControl.Create(AOwner: TComponent);
begin
  inherited;
  // we need to create this number just here and it's valid for this instance of
  // the TabSheetServer
  // problem is, that Delphi expects unique names throughout your whole program
  // by creating a unique number and building a name for the ClientSheets later on
  // we shouldn't have problems with this
  // maybe we could add a random number feature to the router, and the router
  // keeps a list of all given random numbers throughout the runtime of the
  // program, so that no number can be given twice in any case
  Randomize;
  FnRandomID := Random(2000000000) + 1;
  FpParameters := nil;
end; // Create

destructor TmafPageControl.Destroy;
var aController : TmafWindowController;
begin
  aController := GetController(Owner);
  If Assigned(aController) Then
    aController.RemoveFromNotifyList(Self);

  Clear;
  FpParameters.Free;
  FpParameters := nil;
  inherited;
end;

procedure TmafPageControl.Loaded;
var aController : TmafWindowController;
begin
  inherited;

  If (csDesigning in ComponentState) Then
    Exit;                        // only in RunTime

  FpParameters := TmafParameters.Create(Self);

  aController := GetController(Owner);
  If Assigned(aController) Then
    aController.AddToNotifyList(Self);

  If pcoAutoLoad in FPageControlOptions Then
    LoadClientSheets;            // if designed, then we load our ClientSheets here
end;

// overwritten to enable/disable tabs
procedure TmafPageControl.DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean);
begin
  If Not TmafTabSheet(Pages[TabIndex]).Enabled Then begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Font.Color := clBtnHighlight;
    Canvas.TextOut(Rect.Left + 6, Rect.Top + 5, Pages[TabIndex].Caption);

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color  := clBtnShadow;
    Canvas.TextOut(Rect.Left + 5, Rect.Top + 4, Pages[Tabindex].Caption);
  end else
    inherited;
end;

function TmafPageControl.CanChange: Boolean;
begin
  //Result :=
end;

procedure TmafPageControl.MSG_BeforeFormClose(var Msg: TMessage);
begin
  Clear;
end;

procedure TmafPageControl.MSG_FunctionObserver_Add(var Msg: TMessage);
begin
  __AddClientSheet(Msg.LParam);
end;

procedure TmafPageControl.MSG_FunctionObserver_Delete(var Msg: TMessage);
var i, idx : Integer;
begin
  idx := -1;
  For i := 0 To PageCount - 1 Do
    If TmafTabSheet(Pages[i]).SubHookID = Msg.LParam Then begin
      idx := i;
      Break;
    end;
  If idx > -1 Then
    __DeleteClientSheet(idx);
end;

procedure TmafPageControl.MSG_LanguageChange(var Msg: TMessage);
var i : Integer;
begin
  For i := 0 To PageCount - 1 Do
    If Assigned(TmafTabSheet(Pages[i]).Controller) Then
      Pages[i].Caption := FpResClient.GetString(TmafTabSheet(Pages[i]).Controller.CaptionID);
end;

procedure TmafPageControl.Notification(AComponent: TComponent; Operation: TOperation);
var i, j : Integer;
begin
  inherited;
  If Operation = opRemove Then
    If IsClass(AComponent, TmafWindowController) Then
      For i := PageCount - 1 DownTo 0 Do
        If TmafTabSheet(Pages[i]).Controller = AComponent Then begin
          TmafTabSheet(Pages[i]).Client := nil;
          TmafTabSheet(Pages[i]).Controller := nil;
          TmafTabSheet(Pages[i]).SubHookID := 0;
          j := ActivePageIndex;
          TmafTabSheet(Pages[i]).Free;
          If j > 0 Then                      // if there isn't the first
            ActivePageIndex := j - 1;        // we go to the one before the current we're about to destroy
          Break;
        end;
end;

procedure TmafPageControl.__SetResClient(const Value: TmafResourceClient);
begin
  If Assigned(FpResClient) Then
    FpResClient.DeleteNotifyComponent(Self);
  FpResClient := Value;
  If Assigned(FpResClient) Then
    FpResClient.AddNotifyComponent(Self);
end;

procedure TmafPageControl.BEH(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
begin
  Case FnLocalMode Of
    RES_GetWindow  : QHS^.pParent := TmafTabSheet.Create(Self);
    RES_FreeWindow : QHS^.pChildObj := FpHookClient.PopVar.AsObject;
  End;
  If pcoHookClientEvents in FPageControlOptions Then
    If Assigned(Old_BEH) Then
      Old_BEH(nCommand, QHS, pUserParam, ErrCode);
end;

procedure TmafPageControl.AEH(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
var TS : TmafTabSheet;
begin
  Case FnLocalMode Of
    RES_GetWindow  : begin
                       If Assigned(QHS^.pChildObj) Then begin
                         TS := TmafTabSheet(QHS^.pParent);
                         Inc(FnRandomID);
                         TS.Name := 'TabSheet_' + IntToStr(FnRandomID) + '_' + IntToStr(QHS^.HookID) + '_' + IntToStr(QHS^.SubHookID);
                         If IsClass(QHS^.pChildObj, TForm) Then begin
                           TS.Caption := TForm(QHS^.pChildObj).Caption;
                           TForm(QHS^.pChildObj).BorderStyle := bsNone;
                         end;

                         TS.PageControl := Self;
                         TS.SubHookID := QHS^.SubHookID;
                         TS.Client := TScrollingWinControl(QHS^.pChildObj);
                         TS.Controller := GetController(TS.Client);
                         If Assigned(TS.Controller) Then begin
                           TS.Controller.FreeNotification(Self);
                           If TS.Controller.CaptionID > 0 Then
                             If Assigned(TS.Controller.ResClient) Then
                               TS.Caption := TS.Controller.ResClient.GetString(TS.Controller.CaptionID);
                         end;
                         TS.FnFrameID := QHS^.pParams.ParamByName['FrameId'].Value;
                         TS.FsFrameName := QHS^.pParams.ParamByName['FrameName'].Value;
{                         If QHS^.pDataBlock <> nil Then begin
                           TS.FnFrameID := PBaseFrameData(QHS^.pDataBlock)^.FrameID;
                           TS.FsFrameName := String(PBaseFrameData(QHS^.pDataBlock)^.FrameName);  // it comes as PChar
                         end;  //  --  If QHS^.pDataBlock <> nil Then }
                         TScrollingWinControl(QHS^.pChildObj).Align := alClient;
                         TScrollingWinControl(QHS^.pChildObj).ParentWindow := TS.Handle;
                         TScrollingWinControl(QHS^.pChildObj).Visible := False;
                         TScrollingWinControl(QHS^.pChildObj).Parent := TS;
                         TScrollingWinControl(QHS^.pChildObj).Visible := True;
                         If Assigned(FOnTabSheetLoaded) Then
                           FOnTabSheetLoaded(Self, TS);
                       end;
                       QHS^.pChildObj := nil;
                     end;
    RES_FreeWindow : begin

                     end;
  end;
  If pcoHookClientEvents in FPageControlOptions Then
    If Assigned(Old_AEH) Then
      Old_AEH(nCommand, QHS, pUserParam, ErrCode);
end;

procedure TmafPageControl.BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin
  If pcoHookClientEvents in FPageControlOptions Then
    If Assigned(Old_BCR) Then
      Old_BCR(nCommand, QHS, pUserParam);
  If Assigned(FBeforeLoadSheets) Then
    FBeforeLoadSheets(nCommand, QHS, pUserParam);
end;

procedure TmafPageControl.ACR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin
  If pcoHookClientEvents in FPageControlOptions Then
    If Assigned(Old_ACR) Then
      Old_ACR(nCommand, QHS, pUserParam);
end;

procedure TmafPageControl.__AddClientSheet(nSubHookID: Integer);
begin
  If (csDesigning in ComponentState) Then // only RunMode ! (although it should work in DesignMode too ;) )
    Exit;

  If ((Assigned(FpHookClient)) And (FnHookID > 0)) Then begin
    Old_BCR := FpHookClient.BeforeCallRouter;
    Old_ACR := FpHookClient.AfterCallRouter;
    Old_BEH := FpHookClient.BeforeExecHook;
    Old_AEH := FpHookClient.AfterExecHook;
    FpHookClient.PushEvents;               // saving the programs event handler
    FpHookClient.BeforeExecHook := BEH;    // setting our event handler
    FpHookClient.AfterExecHook := AEH;
    FpHookClient.BeforeCallRouter := BCR;
    FpHookClient.AfterCallRouter := ACR;
    FnLocalMode := RES_GetWindow;
    FpHookClient.PushVar.AsObject := FpHookClient.Parameter;
    FpHookClient.Parameter := FpParameters;
    FpHookClient.ExecuteHook(FnHookID, nSubHookID);
    FpHookClient.Parameter := TmafParameters(FpHookClient.PopVar.AsObject);
    FpHookClient.PopEvents;  // restoring previous event handler
  end;
end;

procedure TmafPageControl.__AddClientSheet_uID(uID: Integer);
begin
  If (csDesigning in ComponentState) Then // only RunMode ! (although it should work in DesignMode too ;) )
    Exit;

  If ((Assigned(FpHookClient)) And (uID > 0)) Then begin
    Old_BCR := FpHookClient.BeforeCallRouter;
    Old_ACR := FpHookClient.AfterCallRouter;
    Old_BEH := FpHookClient.BeforeExecHook;
    Old_AEH := FpHookClient.AfterExecHook;
    FpHookClient.PushEvents;               // saving the programs event handler
    FpHookClient.BeforeExecHook := BEH;    // setting our event handler
    FpHookClient.AfterExecHook := AEH;
    FpHookClient.BeforeCallRouter := BCR;
    FpHookClient.AfterCallRouter := ACR;
    FnLocalMode := RES_GetWindow;
    FpHookClient.ExecuteUniqueID(uID);
    FpHookClient.PopEvents;  // restoring previous event handler
  end;
end;

procedure TmafPageControl.__DeleteClientSheet(idx: Integer);
var TS : TmafTabSheet;
begin
  If (csDesigning in ComponentState) Then // only RunMode ! (although it should work in DesignMode too ;) )
    Exit;

  If ((Assigned(FpHookClient)) And (FnHookID > 0)) Then begin
    If Assigned(FBeforeTabsheetUnload) Then
      FBeforeTabsheetUnload(Self, TmafTabSheet(Pages[idx]));
    Old_BCR := FpHookClient.BeforeCallRouter;
    Old_ACR := FpHookClient.AfterCallRouter;
    Old_BEH := FpHookClient.BeforeExecHook;
    Old_AEH := FpHookClient.AfterExecHook;
    FpHookClient.PushEvents;               // saving the programs event handler
    FpHookClient.BeforeCallRouter := BCR;  // setting our event handler
    FpHookClient.AfterCallRouter := ACR;
    FpHookClient.BeforeExecHook := BEH;
    FpHookClient.AfterExecHook := AEH;
    FnLocalMode := RES_FreeWindow;
    // we remove it before, because now we're destroying it and don't need a notify
    // about the destruction anymore
    TS := TmafTabSheet(Pages[idx]);
    If TS.Controller <> nil Then
      TS.Controller.RemoveFreeNotification(Self);
    FpHookClient.PushVar.AsObject := TS.Client;
    TS.Client.Parent := nil;
    FpHookClient.ExecuteHook(FnHookID, TS.FnSubHookID);
    FpHookClient.PopEvents;  // restoring previous event handler
    TS.FnSubHookID := 0;
    TS.Client := nil;
    TS.Free;
    TS := nil;
  end;
end;

procedure TmafPageControl.LoadClientSheets;
begin
  __AddClientSheet(0);  // load all sheets
  If Assigned(FpHookClient) Then
    FpHookClient.__Add_FunctionObserver(FnHookID, 0, Self);
  If Assigned(FAfterLoading) Then
    FAfterLoading(Self);
end;

procedure TmafPageControl.Clear;
var i : Integer;
begin
  For i := PageCount - 1 DownTo 0 Do
    If TmafTabSheet(Pages[i]).Client <> nil Then
      __DeleteClientSheet(i);
  If Assigned(FpHookClient) Then
    FpHookClient.__Delete_FunctionObserver(FnHookID, 0, Self);
end;

{ TmafTabSheet }

constructor TmafTabSheet.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := True;
  FnSubHookID := 0;
  FpClient := nil;
  FpController := nil;
end;

destructor TmafTabSheet.Destroy;
begin
  FEnabled := False;
  inherited;
end;

end.
