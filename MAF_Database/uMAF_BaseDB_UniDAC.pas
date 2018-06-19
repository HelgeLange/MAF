unit uMAF_BaseDB_UniDAC;

interface

uses Windows, Classes, SysUtils,

     uMAF_CustomBaseDB;

Type TmafBaseDB_UniDAC = class(TmafCustomProvider)
     private
     protected
     public
       class function GetProviderName: string; override;
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
     published
     end;

procedure Register;

implementation

{ TmafBaseDB_UniDAC }

constructor TmafBaseDB_UniDAC.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TmafBaseDB_UniDAC.Destroy;
begin

  inherited;
end;

class function TmafBaseDB_UniDAC.GetProviderName: string;
begin
  Result := 'DevArt UniDAC';
end;

procedure Register;
begin
  RegisterComponents('MAF Database', [TmafBaseDB_UniDAC]);
end;

initialization
//  RegisterClass(TmafBaseDB_UniDAC);
  mafProviders.RegisterProvider(TmafBaseDB_UniDAC);

finalization
//  UnRegisterClass(TmafBaseDB_UniDAC);

end.
