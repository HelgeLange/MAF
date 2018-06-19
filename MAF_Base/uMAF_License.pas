unit uMAF_License;

interface

uses SysUtils, Windows, Classes;

Type TmafLicenseManager = class(TComponent)
     private
       FpHookManager : Pointer;
    procedure __SetHookManager(const Value: Pointer);
     protected
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;

       property HookManager : Pointer read FpHookManager write __SetHookManager;
     published
     end;

implementation

uses uMAF_HookManager;

{ TmafLicense }

constructor TmafLicenseManager.Create(AOwner: TComponent);
begin
  inherited;
  FpHookManager := nil;
end;

destructor TmafLicenseManager.Destroy;
begin

  inherited;
end;

procedure TmafLicenseManager.__SetHookManager(const Value: Pointer);
begin
  FpHookManager := Value;
  If FpHookManager <> nil Then
    TmafHookManager(FpHookManager).FreeNotification(Self);
end;

end.
