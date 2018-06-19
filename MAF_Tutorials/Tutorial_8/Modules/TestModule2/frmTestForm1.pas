unit frmTestForm1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uMAF_Core, uMAF_WindowController, StdCtrls, uMAF_HookClient,
  ComCtrls, uMAF_PageControl;

type
  TfTestForm1 = class(TForm)
    mafWindowController1: TmafWindowController;
    mafPageControl1: TmafPageControl;
    mafHookClient1: TmafHookClient;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fTestForm1: TfTestForm1;

implementation

{$R *.dfm}

end.
