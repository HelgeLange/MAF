unit frmFrame1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, uMAF_ResourceClient, StdCtrls, uMAF_Core, uMAF_WindowController;

type
  TFrame1 = class(TFrame)
    mafWindowController1: TmafWindowController;
    Label1: TLabel;
    mafResourceClient1: TmafResourceClient;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
