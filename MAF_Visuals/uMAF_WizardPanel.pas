{*******************************************************************************
Name         : uMAF_WizardPanel.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2008-2009 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 22.09.2009
Last Update  : 22.09.2009
Version      : 1.0.000
Purpose      : 
Last Changes :

1.0.000 (22.09.2009) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_WizardPanel;

interface

uses SysUtils, Classes, Controls, ExtCtrls, Messages, Forms,
     // Modular Application Framework Components units
     uMAF_Core, uMAF_HookClient, uMAF_ResourceClient, uMAF_Globals, uMAF_Tools,
     uMAF_WindowController, uMAF_Parameters;

Type TWizardPanelOption = (wpoAutoLoad, wpoShowFirstPage);
     TWizardPanelOptions = Set Of TWizardPanelOption;

     TmafControlOption = (coAutoLoad, coPreloadFrames, coAfterExecHook);
     TmafControlOptions = Set Of TmafControlOption;

     TmafPanelAnimation = (paNone, paScrollRightToLeft);

     TOnStep = procedure(Sender: TObject; Step: Integer) of object;
     TOnBeforeStep = procedure(Sender: TObject; Step: Integer; var CanChange: Boolean) of object;
     
     RFrameData = packed record
       FrameID : Integer;
       FrameName : PChar;
     end;
     PFrameData = ^RFrameData;

     RFrame = packed record
       ID : Integer;
       FrameName : String;
       aObj : TObject;
       uID : Cardinal;
     end;
     PFrame = ^RFrame; 

     TFrameLoad = procedure(Sender: TObject; AFrame: TObject) Of Object;
     TOnScroll = procedure(Sender: TObject; AFrame, BFrame: TObject) Of Object;
     TFrameDataLoad = procedure(Sender: TObject; pData: PFrameData; var DoDiscard: Boolean) Of Object;

     TmafBasePanel = class(TPanel)
     private
       FpHookClient : TmafHookClient;
       FnHookID : Integer;         // the HookID to load the frames
       FnSubHookID : Integer;      // to load a single frame by HookID / SubHookID combination instead of uID
       FnUniqueID : Integer;       // to call a SubHook by its uID when there is only 1 frame to show
       FControlOptions : TmafControlOptions;
       FState : TmafStates;
       FpFrameList : TList;
//       FpParameters : TmafParameters;
       FsDefaultFrame : String;    // name of the frame shown by default when loading (leave empty if handled by program)
       FAfterFrameLoad : TFrameLoad;
       FBeforeFrameLoad : TFrameDataLoad;
       FAfterChangeFrame : TFrameLoad;
       FOriginalAfterExecHook : TExecuteHook;
       function __GetCurrentFrameName: String;
       function __GetFrameCount: Integer;
       function __GetFrameByIndex(Index: Integer): TObject;
     protected
       FpCurrentFrame : TObject;   // frame currently on the panel
       FpNewFrame : TObject;       // frame to be shown next, when load command comes, used in animations
       procedure AEH_FrameList(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer); virtual;
       procedure BCR_FrameUnload(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer); virtual;
       procedure BCR_FrameLoad(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer); virtual;
//       procedure BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer); virtual;
       procedure LoadFrameList;
       procedure __DoChangeFrame; virtual; abstract;
       function __FindFrame(FrameName: String): PFrame;
     public
       constructor Create(AOwner : TComponent); override;
       destructor Destroy; override;

       procedure Load(FrameName: String = ''); virtual;
       procedure Unload(FrameName: String); virtual;
       procedure Loaded; override;

       property CurrentFrame : TObject read FpCurrentFrame write FpCurrentFrame;
       property NewFrame : TObject read FpNewFrame write FpNewFrame;
       property CurrentFrameName : String read __GetCurrentFrameName;
       property Count : Integer read __GetFrameCount;
       property Frame[Index: Integer] : TObject read __GetFrameByIndex;
     published
       property UniqueID : Integer read FnUniqueID write FnUniqueID;
       property HookID : Integer read FnHookID write FnHookID;
       property SubHookID : Integer read FnSubHookID write FnSubHookID;
       property HookClient : TmafHookClient read FpHookClient write FpHookClient;
       property ControlOptions : TmafControlOptions read FControlOptions write FControlOptions;
       property DefaultFrame : String read FsDefaultFrame write FsDefaultFrame;
       property AfterFrameLoad : TFrameLoad read FAfterFrameLoad write FAfterFrameLoad;
       property BeforeFrameLoad : TFrameDataLoad read FBeforeFrameLoad write FBeforeFrameLoad;
       property AfterChangeFrame : TFrameLoad read FAfterChangeFrame write FAfterChangeFrame;
     end;

     TmafFramePanel = class(TmafBasePanel)
     private
       FbLoadingFrame : Boolean;
       FpTimer : TTimer;
       FAfterScroll,
       FBeforeScroll : TOnScroll;
       FAnimation : TmafPanelAnimation;
       procedure __OnTimer(Sender: TObject);
     protected
//       procedure AEH(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
//       procedure BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
       procedure __DoChangeFrame; override;
       procedure __DoNoAnimation;
       procedure __DoScroll_RightToLeft;
     public
       procedure Load(FrameName: String = ''); override;
       procedure Unload(FrameName: String); override;

     published
       property Animation : TmafPanelAnimation read FAnimation write FAnimation;
       property AfterScroll : TOnScroll read FAfterScroll write FAfterScroll;
       property BeforeScroll : TOnScroll read FBeforeScroll write FBeforeScroll;
     end;

     TmafWizardPage = class(TComponent)
     private
       FnSubHookID : Integer;
       FpClient : TForm;
       FpController : TmafWindowController;
     public
       property SubHookID : Integer read FnSubHookID write FnSubHookID;
       property Client : TForm read FpClient write FpClient;
       property Controller : TmafWindowController read FpController write FpController;
     end;

     TmafWizardPanel = class(TPanel)
     private
       FpHookClient : TmafHookClient; // our beloved HookClient
       FpResClient : TmafResourceClient;   // and the well known ResClient :)
       FnHookID : Integer;         // the HookID to load the forms
       FOptions : TWizardPanelOptions;
       FpPageList : TList;
       FnLocalMode : Integer;
       FpNextButton : TWinControl;
       FpPrevButton : TWinControl;
       FnCurrentStep : Integer;
       FnDataID : Integer;
       FBeforeStep : TOnBeforeStep; // before a page is made invisible
       FAfterStep : TOnStep;        // after the page is made visible
       FOnLastPage,
       FOnFirstPage : TNotifyEvent;
       function __GetPageCount: Integer;
       function __GetPageByIndex(Index: Integer): TmafWizardPage;
     protected
       procedure MSG_BeforeFormClose(var Msg: TMessage); message MSG_BEFORE_FORM_CLOSE;
       procedure BEH(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
       procedure AEH(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
       procedure BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
       procedure __AddPage(nSubHookID : Integer);
       procedure __DeletePage(idx: Integer);
       function __EnableDisable_WizardPage(nPageID: Integer): Boolean;
     public
       constructor Create(AOwner : TComponent); override;
       destructor Destroy; override;
       procedure Loaded; override;
       procedure LoadPages;
       procedure Clear;
       procedure Next;
       procedure Previous;
       procedure First;
       procedure Last;
       procedure GotoPage(nPageID : Integer);

       property CurrentPageID : Integer read FnCurrentStep;
       property PageCount : Integer read __GetPageCount;
       property Pages[Index: Integer]: TmafWizardPage read __GetPageByIndex;
     published
       property HookID : Integer read FnHookID write FnHookID;
       property HookClient : TmafHookClient read FpHookClient write FpHookClient;
       property ResClient : TmafResourceClient read FpResClient write FpResClient;
       property Options : TWizardPanelOptions read FOptions write FOptions default [];
       property NextButton : TWinControl read FpNextButton write FpNextButton;
       property PrevButton : TWinControl read FpPrevButton write FpPrevButton;

       property BeforeStep : TOnBeforeStep read FBeforeStep write FBeforeStep;
       property AfterStep : TOnStep read FAfterStep write FAfterStep;
       property OnLastPage : TNotifyEvent read FOnLastPage write FOnLastPage;
       property OnFirstPage : TNotifyEvent read FOnFirstPage write FOnFirstPage;
     end;

implementation

{ TmafBasePanel }

procedure TmafBasePanel.AEH_FrameList(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
var i : Integer;
    PF: PFrame;
begin
  If Assigned(QHS^.pChildObj) Then begin
    If Not (msQuerying in FState) Then begin
      PF := New(PFrame);
      FillChar(PF^, SizeOf(RFrame), 0);
      PF^.ID := PFrameData(QHS^.pChildObj)^.FrameID;
      PF^.FrameName := PFrameData(QHS^.pChildObj)^.FrameName;
      PF^.uID := QHS^.uID;
      FpFrameList.Add(PF);

      // only in preload we load every frame
      If coPreloadFrames in FControlOptions Then begin
        Include(FState, msQuerying);
        FpHookClient.ExecuteUniqueID(QHS^.uID);
        Exclude(FState, msQuerying);
      end;
    end else begin
      For i := 0 To FpFrameList.Count - 1 Do
        If PFrame(FpFrameList.Items[i])^.uID = QHS^.uID Then begin
          PFrame(FpFrameList.Items[i])^.aObj := QHS^.pChildObj;
          If Assigned(FAfterFrameLoad) Then
            FAfterFrameLoad(Self, PFrame(FpFrameList.Items[i])^.aObj);
          Break;
        end;
    end;
  end;
  If coAfterExecHook in FControlOptions Then
    If Assigned(FOriginalAfterExecHook) Then
      FOriginalAfterExecHook(nCommand, QHS, pUserParam, ErrCode);
end;

procedure TmafBasePanel.BCR_FrameLoad(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin
  If msQuerying in FState Then
    QHS^.pParent := Self;
end;

procedure TmafBasePanel.BCR_FrameUnload(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin
  // setting both variables causes the ModuleController to unload the frame
  QHS^.pChildObj := FpHookClient.PopVar.AsPointer;
  QHS^.pParent := Self;
end;

constructor TmafBasePanel.Create(AOwner: TComponent);
begin
  inherited;
  FOriginalAfterExecHook := nil;
  FpFrameList := TList.Create;
  FState := [];
  FpCurrentFrame := nil;
  FpNewFrame := nil;
//  FpParameters := TmafParameters.Create(Self);
end;

destructor TmafBasePanel.Destroy;
begin
  While Not FpFrameList.Count = 0 Do
    Unload(PFrame(FpFrameList.Items[0])^.FrameName);
  FreeAndNil(FpFrameList);
//  FreeAndNil(FpParameters);
  inherited;
end;

procedure TmafBasePanel.Load(FrameName: String);
var i : Integer;
    aFrameRec : PFrame;
begin
  If (csDesigning in ComponentState) Then // only RunMode ! (although it should work in DesignMode too ;) )
    Exit;

  If FrameName = '' Then begin
    LoadFrameList;
    If FsDefaultFrame <> '' Then
      Load(FsDefaultFrame);
    Exit;
  end;

  If FrameName = CurrentFrameName Then
    Exit; // nothing to do

  If ((Assigned(FpHookClient)) And ((FnUniqueID > 0) Or (FnHookID > 0))) Then begin
    FpNewFrame := nil;
    // first we need to find the new frame
    aFrameRec := __FindFrame(FrameName);
    If Assigned(aFrameRec) Then
      FpNewFrame := aFrameRec^.aObj;

    // if we pre-loaded all frames, we just have to make them visible
    If coPreloadFrames in FControlOptions Then begin
      If FpNewFrame = nil Then
        Raise EComponentError.Create('Frame "' + FrameName + '" not found!');
      __DoChangeFrame;  // we can call it as we do have a a new frame to show
      Exit;
    end;

    // now if we don't have preloaded frames, we need to load the frame
    If FpNewFrame = nil Then begin
      Include(FState, msQuerying);
      FOriginalAfterExecHook := FpHookClient.AfterExecHook;
      FpHookClient.PushEvents;
      FpHookClient.AfterExecHook := AEH_FrameList;
      FpHookClient.BeforeCallRouter := BCR_FrameLoad;
      If FnUniqueID > 0 Then
        FpHookClient.ExecuteUniqueID(FnUniqueID)
      Else
        FpHookClient.ExecuteHook(FnHookID, FnSubHookID);
      FpHookClient.PopEvents;
      FOriginalAfterExecHook := nil;
      Exclude(FState, msQuerying);
      aFrameRec := __FindFrame(FrameName);
      If Assigned(aFrameRec) Then begin
        FpNewFrame := aFrameRec^.aObj;
        __DoChangeFrame;
      end;
    end;
  end;
end;

procedure TmafBasePanel.Loaded;
begin
  inherited;
  If coAutoLoad in FControlOptions Then begin
    LoadFrameList;
    If FsDefaultFrame <> '' Then
      Load(FsDefaultFrame);
  end;
end;

procedure TmafBasePanel.LoadFrameList;
var ErrCode : Integer;
begin
  If Not Assigned(FpHookClient) Then
    Exit;
  If ((FnHookID < 1) And (FnUniqueID < 1)) Then
    Exit;

  Include(FState, msListing);
  FOriginalAfterExecHook := FpHookClient.AfterExecHook;
  FpHookClient.PushEvents;
  FpHookClient.AfterExecHook := AEH_FrameList;
  FpHookClient.BeforeCallRouter := BCR_FrameLoad;
{  FpHookClient.PushVar.AsObject := FpHookClient.Parameter;
  FpHookClient.Parameter := FpParameters; }

  If FnUniqueID > 0 Then
    ErrCode := FpHookClient.ExecuteUniqueID(FnUniqueID)
  Else
    ErrCode := FpHookClient.ExecuteHook(FnHookID, FnSubHookID);
//  FpHookClient.Parameter := FpHookClient.PopVar.AsObject;
  FpHookClient.PopEvents;
  FOriginalAfterExecHook := nil;
  Exclude(FState, msListing);
end;

procedure TmafBasePanel.Unload(FrameName: String);
var aFrame : PFrame;
begin
  If FrameName = '' Then begin
    While FpFrameList.Count > 0 Do begin
      aFrame := PFrame(FpFrameList.Items[0]);
      FpHookClient.PushEvents;
      FpHookClient.BeforeCallRouter := BCR_FrameUnload;
      FpHookClient.PushVar.AsPointer := Pointer(aFrame^.aObj);
      FpHookClient.ExecuteUniqueID(aFrame^.uID);
      FpHookClient.PopEvents;
      FpFrameList.Delete(0);
      Dispose(aFrame);
    end;
    FpCurrentFrame := nil;
    FpNewFrame := nil;
    Exit;
  end;

  aFrame := __FindFrame(FrameName);
  If Assigned(aFrame) Then begin
    FpHookClient.PushEvents;
    FpHookClient.BeforeCallRouter := BCR_FrameUnload;
    FpHookClient.PushVar.AsObject := aFrame^.aObj;
    FpHookClient.ExecuteUniqueID(aFrame^.uID);
    FpHookClient.PopEvents;
    FpFrameList.Delete(FpFrameList.IndexOf(aFrame));
    Dispose(aFrame);
  end;
end;

function TmafBasePanel.__FindFrame(FrameName: String): PFrame;
var i : Integer;
begin
  Result := nil;
  For i := 0 To FpFrameList.Count - 1 Do
    If PFrame(FpFrameList.Items[i])^.FrameName = FrameName Then begin
      Result := PFrame(FpFrameList.Items[i]);
      Break;
    end;
end;

function TmafBasePanel.__GetCurrentFrameName: String;
var i : Integer;
begin
  Result := '';
  If FpCurrentFrame = nil Then
    Exit;

  For i := 0 To FpFrameList.Count - 1 Do
    If PFrame(FpFrameList.Items[i])^.aObj = FpCurrentFrame Then begin
      Result := PFrame(FpFrameList.Items[i])^.FrameName;
      Break;
    end;
end;

function TmafBasePanel.__GetFrameByIndex(Index: Integer): TObject;
begin
  Result := nil;
  If ((Index > -1) and (Index < FpFrameList.Count)) Then
    Result := PFrame(FpFrameList.Items[Index])^.aObj
  Else
    Raise EComponentError.Create('TmafBasePanel.__GetFrameByIndex: Index out of bounds!');
end;

function TmafBasePanel.__GetFrameCount: Integer;
begin
  Result := FpFrameList.Count;
end;

{ TmafFramePanel }

{procedure TmafFramePanel.AEH(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
var DoDiscard : Boolean;
begin
  If msUnloading in FState Then
    Exit;

  If msLoading in FState Then begin
    FpClient := TObject(QHS^.pChildObj);
    If Assigned(FAfterFrameLoad) Then
      FAfterFrameLoad(Self, FpClient);
    FnClient_uID := QHS^.uID;
  end;  //  --  If msLoading in FState Then

  If ((msQuerying in FState) Or (msListing in FState)) Then begin
    DoDiscard := True;
    If ((coFrameList in FControlOptions) And (Assigned(QHS^.pChildObj))) Then
      If msListing in FState Then
        FpFrameList.Add(PFrameData(QHS^.pChildObj)^.FrameName);
    If msQuerying in FState Then begin
      If FsFrameName <> '' Then
        DoDiscard := Not (PFrameData(QHS^.pChildObj)^.FrameName = FsFrameName)
      Else
        If Assigned(FBeforeFrameLoad) Then
          FBeforeFrameLoad(Self, PFrameData(QHS^.pChildObj), DoDiscard);
      If Not DoDiscard Then begin
        Include(FState, msLoading);
        FpHookClient.ExecuteUniqueID(QHS^.uID);
        Exclude(FState, msLoading);
      end;  //  --  If Not DoDiscard Then
    end;  //  --  If msQuerying in FState Then
  end;  //  --  If ((msQuerying in FState) Or (msListing in FState)) Then
end;

procedure TmafFramePanel.BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin
  If ((msLoading in FState) Or (msUnloading in FState)) Then begin
    QHS^.pParent := Self;
    QHS^.pChildObj := FpClient;
  end;
end;   }

// here we can implement different animation handler
procedure TmafFramePanel.Load(FrameName: String);
begin
  inherited;

end;

procedure TmafFramePanel.Unload(FrameName: String);
begin
  inherited;
end;

procedure TmafFramePanel.__DoChangeFrame;
begin
  Case FAnimation Of
    paNone : __DoNoAnimation;
    paScrollRightToLeft : If Assigned(FpCurrentFrame) Then
                            __DoScroll_RightToLeft
                          Else
                            __DoNoAnimation;
  End;
  If Assigned(FAfterChangeFrame) Then
    FAfterChangeFrame(Self, FpCurrentFrame);
end;

// basic change
procedure TmafFramePanel.__DoNoAnimation;
begin
  If Assigned(FpCurrentFrame) Then begin
    TFrame(FpCurrentFrame).Visible := False;
    TFrame(FpCurrentFrame).Parent := nil;
    TFrame(FpCurrentFrame).Align := alNone;
  end;

  TFrame(FpNewFrame).Parent := Self;
  TFrame(FpNewFrame).Align := alClient;
  TFrame(FpNewFrame).Visible := True;

  FpCurrentFrame := FpNewFrame;
  FpNewFrame := nil;
end;

procedure TmafFramePanel.__DoScroll_RightToLeft;
var i, j, mLeft1, mLeft2 : Integer;
begin
  If Assigned(FBeforeScroll) Then
    FBeforeScroll(Self, FpCurrentFrame, FpNewFrame);
  mLeft1 := Width - 20;
  mLeft2 := 20;
  If Assigned(FpCurrentFrame) Then begin
    TFrame(FpCurrentFrame).Width := Width;
    TFrame(FpCurrentFrame).Height := Height;
    TFrame(FpCurrentFrame).Align := alNone;
  end;
  If Assigned(FpNewFrame) Then begin
    TFrame(FpNewFrame).Visible := False;
    TFrame(FpNewFrame).Parent := Self;
    TFrame(FpNewFrame).ParentWindow := Self.Handle;
    TFrame(FpNewFrame).Align := alNone;
    TFrame(FpNewFrame).Width := Width;
    TFrame(FpNewFrame).Height := Height;
    TFrame(FpNewFrame).Left := Width;
    TFrame(FpNewFrame).Top := 0;
    TFrame(FpNewFrame).Visible := True;
  end;

  j := 10;
  For i := Width Downto mLeft1 Do begin
    If Assigned(FpCurrentFrame) Then
      TFrame(FpCurrentFrame).Left := TFrame(FpCurrentFrame).Left - 1;
    TFrame(FpNewFrame).Left := TFrame(FpNewFrame).Left - 1;
    Application.ProcessMessages;
    Sleep(j);
    If j > 1 Then
      Dec(j);
  end;

  i := mLeft1;
  repeat
    If Assigned(FpCurrentFrame) Then
      TFrame(FpCurrentFrame).Left := TFrame(FpCurrentFrame).Left - 10;
    TFrame(FpNewFrame).Left := TFrame(FpNewFrame).Left - 10;
    Application.ProcessMessages;
//    Refresh;
    Dec(i, 10);
  until (i <= mLeft2);
  For i := mLeft2 Downto 0 Do begin
    If Assigned(FpCurrentFrame) Then
      TFrame(FpCurrentFrame).Left := TFrame(FpCurrentFrame).Left - 1;
    TFrame(FpNewFrame).Left := TFrame(FpNewFrame).Left - 1;
    Sleep(j);
    Inc(j);
  end;
  If Assigned(FAfterScroll) Then
    FAfterScroll(Self, FpCurrentFrame, FpNewFrame);
  If Assigned(FpCurrentFrame) Then
    TFrame(FpCurrentFrame).Parent := nil;
  TFrame(FpNewFrame).Align := alClient;
  FpCurrentFrame := FpNewFrame;
  FpNewFrame := nil;
end;

procedure TmafFramePanel.__OnTimer(Sender: TObject);
var i, j : Integer;
begin
  FpTimer.Enabled := False;
{  j := 40;
  If (TScrollingWinControl(FpClient).Left > (Self.Left + 40)) Then begin
    If (FpTimer.Interval > 10) Then begin
      FpTimer.Interval := FpTimer.Interval - 10;
      j := 5;
    end;
    If ((FpTimer.Interval <= 10) And (FpTimer.Interval > 1)) Then begin
      FpTimer.Interval := FpTimer.Interval - 1;
      j := 10;
    end;
  end else begin
    If TScrollingWinControl(FpClient).Left > (Self.Left) Then begin
      FpTimer.Interval := FpTimer.Interval + 10;
      j := 10;
    end;
  end;


  For i := 0 To j Do begin
    TScrollingWinControl(FpOldClient).Left := TScrollingWinControl(FpOldClient).Left - 1;
    TScrollingWinControl(FpClient).Left := TScrollingWinControl(FpClient).Left - 1;
    If TScrollingWinControl(FpClient).Left = 0 Then
      Break;
  end;
  If TScrollingWinControl(FpClient).Left = Left Then begin
    If Assigned(FAfterScroll) Then
      FAfterScroll(Self, FpOldClient, FpClient);
    Unload;
  end;

  If TScrollingWinControl(FpClient).Left > Self.Left Then
    FpTimer.Enabled := True;     }
end;

{ TmafWizardPanel }

constructor TmafWizardPanel.Create(AOwner: TComponent);
begin
  inherited;
  FpPageList := TList.Create;
  FOptions := [];
  FnCurrentStep := -1;
end;

destructor TmafWizardPanel.Destroy;
begin
  Clear;
  FreeAndNil(FpPageList);
  inherited;
end;

procedure TmafWizardPanel.Loaded;
var aController : TmafWindowController;
begin
  inherited;

  If (csDesigning in ComponentState) Then
    Exit;                        // only in Runtime

  aController := GetController(Owner);
  If Assigned(aController) Then
    aController.AddToNotifyList(Self);

  If wpoAutoLoad in FOptions Then
    LoadPages;            // if designed, then we load our pages here
  If wpoShowFirstPage In FOptions Then
    First;
end;

procedure TmafWizardPanel.MSG_BeforeFormClose(var Msg: TMessage);
begin
  Clear;
end;

procedure TmafWizardPanel.AEH(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
var WP : TmafWizardPage;
begin
  Case FnLocalMode Of
    RES_GetWindow  : begin
                       If Assigned(QHS^.pChildObj) Then begin
                         WP := TmafWizardPage(QHS^.pParent);
                         WP.Name := 'mafWizardPage_' + IntToStr(QHS^.HookID) + '_' + IntToStr(QHS^.SubHookID);
                         WP.SubHookID := QHS^.SubHookID;
                         WP.Client := TForm(QHS^.pChildObj);
                         WP.Controller := GetController(WP.Client);
                         WP.Client.Parent := Self;
                         WP.Client.ParentWindow := Handle;
                         WP.Client.Visible := False;
                         FpPageList.Add(WP);
                         If IsClass(WP.Client, TForm) Then
                           WP.Client.BorderStyle := bsNone;
                         WP.Client.Align := alClient;
                       end;
                       QHS^.pChildObj := nil;
                     end;
    RES_FreeWindow : begin

                     end;
  end;
end;

procedure TmafWizardPanel.BCR(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer);
begin

end;

procedure TmafWizardPanel.BEH(nCommand: Integer; QHS: pQHS; var pUserParam: Pointer; ErrCode: Integer);
begin
  Case FnLocalMode Of
    RES_GetWindow  : QHS^.pParent := TmafWizardPage.Create(Self);
    RES_FreeWindow : QHS^.pChildObj := FpHookClient.PopVar.AsObject;
  End;
end;

procedure TmafWizardPanel.__AddPage(nSubHookID: Integer);
begin
  If (csDesigning in ComponentState) Then // only RunMode ! (although it should work in DesignMode too ;) )
    Exit;

  If ((Assigned(FpHookClient)) And (FnHookID > 0)) Then begin
    FpHookClient.PushEvents;               // saving the programs event handler
    FpHookClient.BeforeExecHook := BEH;    // setting our event handler
    FpHookClient.AfterExecHook := AEH;
    FpHookClient.BeforeCallRouter := BCR;
    FnLocalMode := RES_GetWindow;
    FpHookClient.ExecuteHook(FnHookID, nSubHookID);
    FpHookClient.PopEvents;  // restoring previous event handler
  end;
end;

procedure TmafWizardPanel.__DeletePage(idx: Integer);
begin
  If (csDesigning in ComponentState) Then // only RunMode ! (although it should work in DesignMode too ;) )
    Exit;

  If ((Assigned(FpHookClient)) And (FnHookID > 0)) Then begin
    FpHookClient.PushEvents;               // saving the programs event handler
    FpHookClient.BeforeExecHook := BEH;    // setting our event handler
    FnLocalMode := RES_FreeWindow;
    FpHookClient.PushVar.AsObject := TmafWizardPage(FpPageList[idx]).Client;
    FpHookClient.ExecuteHook(FnHookID, TmafWizardPage(FpPageList[idx]).FnSubHookID);
    FpHookClient.PopEvents;  // restoring previous event handler
    TmafWizardPage(FpPageList[idx]).FnSubHookID := 0;
    TmafWizardPage(FpPageList[idx]).Client := nil;
  end;
end;

function TmafWizardPanel.__GetPageByIndex(Index: Integer): TmafWizardPage;
begin
  Result := nil;
  If ((Index > -1) And (Index < FpPageList.Count)) Then
    Result := TmafWizardPage(FpPageList.Items[Index])
  Else
    Raise Exception.Create('TmafWizardPage: Index out of range!');
end;

function TmafWizardPanel.__GetPageCount: Integer;
begin
  Result := FpPageList.Count;
end;

procedure TmafWizardPanel.LoadPages;
begin
  __AddPage(0); // load all pages
end;

procedure TmafWizardPanel.Clear;
begin
  While FpPageList.Count > 0 Do begin
    If TmafWizardPage(FpPageList[0]).Client <> nil Then
      __DeletePage(0);
    TmafWizardPage(FpPageList[0]).Free;
    FpPageList.Delete(0);
  end;  //  --  While FpPageList.Count > 0 Do
end;

function TmafWizardPanel.__EnableDisable_WizardPage(nPageID: Integer): Boolean;
var pPage : TmafWizardPage;
    bCanChange : Boolean;
begin
  Result := False;
  If ((nPageID > -1) And (nPageID <= PageCount -1)) Then begin
    Result := True;
    pPage := TmafWizardPage(FpPageList.Items[nPageID]);
    If pPage.Client.Visible Then begin
      bCanChange := True;
      If Assigned(FBeforeStep) Then
        FBeforeStep(Self, nPageID, bCanChange);
      Result := bCanChange;
      If bCanChange Then begin
        pPage.Client.Visible := False;
        pPage.Client.ParentWindow := 0;
        pPage.Client.Parent := nil;
      end;
    end else begin
      pPage.Client.ParentWindow := Self.Handle;
      pPage.Client.Parent := Self;
      pPage.Client.Visible := True;
      If FnDataID > 0 Then
        pPage.Controller.DataID := FnDataID;
      If Assigned(FAfterStep) Then
        FAfterStep(Self, nPageID);
    end;  //  --  If pPage.FpClient.Visible Then
  end;  //  --  If ((nPageID > 0) And (nPageID < PageCount -1)) Then
  If FpPrevButton <> nil Then
    FpPrevButton.Enabled := (nPageID > 0);
  If (nPageID = PageCount - 1) Then
    If Assigned(FOnLastPage) Then
      FOnLastPage(Self);
  If (nPageID = 0) Then
    If Assigned(FOnFirstPage) Then
      FOnFirstPage(Self);
{  If FpNextButton <> nil Then
    FpNextButton.Enabled := (nPageID < PageCount - 1); }
end;

procedure TmafWizardPanel.Next;
begin
  If ((FnCurrentStep < PageCount - 2) And (FnCurrentStep > -1)) Then
    If __EnableDisable_WizardPage(FnCurrentStep) Then begin
      Inc(FnCurrentStep);
      __EnableDisable_WizardPage(FnCurrentStep);
    end;
end;

procedure TmafWizardPanel.Previous;
begin
  If FnCurrentStep > 0 Then
    If __EnableDisable_WizardPage(FnCurrentStep) Then begin
      Dec(FnCurrentStep);
      __EnableDisable_WizardPage(FnCurrentStep);
    end;
end;

procedure TmafWizardPanel.First;
begin
  If FnCurrentStep > -1 Then
    __EnableDisable_WizardPage(FnCurrentStep);
  FnCurrentStep := 0;
  __EnableDisable_WizardPage(FnCurrentStep);
end;

procedure TmafWizardPanel.Last;
begin
  If ((FnCurrentStep > -1) And (FnCurrentStep < PageCount -1)) Then
    __EnableDisable_WizardPage(FnCurrentStep);
  FnCurrentStep := PageCount - 1;
  __EnableDisable_WizardPage(FnCurrentStep);
end;

procedure TmafWizardPanel.GotoPage(nPageID: Integer);
begin
  If nPageID = FnCurrentStep Then      // nothing to do
    Exit;
  __EnableDisable_WizardPage(FnCurrentStep);
  If __EnableDisable_WizardPage(nPageID) Then
    FnCurrentStep := nPageID
end;

end.
