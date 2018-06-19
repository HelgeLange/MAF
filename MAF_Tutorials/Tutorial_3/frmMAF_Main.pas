unit frmMAF_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uMAF_ManagerLoader, uMAF_Core, StdCtrls, uMAF_Globals,
  uMAF_HookClient;

type
  TForm1 = class(TForm)
    mafManagerLoader1: TmafManagerLoader;
    Button1: TButton;
    Edit1: TEdit;
    mafHookClient1: TmafHookClient;
    procedure mafManagerLoader1DataLoad(Sender: TObject; Manager: string; var ManagerFileName: string);
    procedure Button1Click(Sender: TObject);
    procedure mafHookClient1AfterExecHook(nCommand: Integer; QHS: pQHS;
      var pUserParam: Pointer; ErrCode: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  mafHookClient1.ExecuteHook(12000); // that was the HookID we gave in the DFT Editor
end;

procedure TForm1.mafHookClient1AfterExecHook(nCommand: Integer; QHS: pQHS;
  var pUserParam: Pointer; ErrCode: Integer);
begin
  Case nCommand Of
    12000 : begin
              Edit1.Text := String(PChar(QHS^.pChildObj));
            end;
  end;
end;

procedure TForm1.mafManagerLoader1DataLoad(Sender: TObject; Manager: string;
  var ManagerFileName: string);
begin
  If Manager = 'HookManager' Then
    ManagerFileName := 'Router_FileDB.dll';
end;

end.
