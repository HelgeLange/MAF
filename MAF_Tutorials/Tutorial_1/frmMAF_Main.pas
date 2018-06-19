unit frmMAF_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uMAF_ManagerLoader, uMAF_Core, StdCtrls;

type
  TForm1 = class(TForm)
    mafManagerLoader1: TmafManagerLoader;
    procedure mafManagerLoader1DataLoad(Sender: TObject; Manager: string; var ManagerFileName: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.mafManagerLoader1DataLoad(Sender: TObject; Manager: string;
  var ManagerFileName: string);
begin
  If Manager = 'HookManager' Then
    ManagerFileName := 'Router_FileDB.dll';
end;

end.
