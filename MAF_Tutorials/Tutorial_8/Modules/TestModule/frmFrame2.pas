unit frmFrame2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, uMAF_ResourceClient, uMAF_Core, uMAF_WindowController;

type
  TFrame2 = class(TFrame)
    mafWindowController1: TmafWindowController;
    mafResourceClient1: TmafResourceClient;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
