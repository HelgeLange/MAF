unit uMAF_BaseDB;

interface

uses SysUtils, Classes, Windows,
     uMAF_Globals, uMAF_CustomBaseDB, uMAF_TemplateStreamer;

Type TmafBaseDB = class(TmafCustomBaseDB)
     private
     protected
       procedure __ReadStreamData(Sender: TObject; ID: Integer); override;
       procedure __WriteStreamData(Sender: TObject; ID: Integer); override;
     public
     end;

implementation

{ TmafBaseDB }

procedure TmafBaseDB.__ReadStreamData(Sender: TObject; ID: Integer);
var pUser: PUserDataRec;
begin
  Case FpStreamer.StreamID Of
    6558 : begin
             pUser := PUserDataRec(FpStreamer.Data);
             FpStreamer.ReadString(pUser^.FirstName);
             FpStreamer.ReadString(pUser^.LastName);
             FpStreamer.ReadString(pUser^.Password);
             FpStreamer.ReadString(pUser^.Password2);
             FpStreamer.ReadByte(pUser^.SL);
             FpStreamer.ReadInteger(pUser^.Flags);
           end;
  end;
end;

procedure TmafBaseDB.__WriteStreamData(Sender: TObject; ID: Integer);
var pUser: PUserDataRec;
begin
  Case FpStreamer.StreamID Of
    6558 : begin
             pUser := PUserDataRec(FpStreamer.Data);
             FpStreamer.WriteString(pUser^.FirstName);
             FpStreamer.WriteString(pUser^.LastName);
             FpStreamer.WriteString(pUser^.Password);
             FpStreamer.WriteString(pUser^.Password2);
             FpStreamer.WriteByte(pUser^.SL);
             FpStreamer.WriteInteger(pUser^.Flags);
           end;
  end;
end;

end.
