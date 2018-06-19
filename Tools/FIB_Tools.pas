unit FIB_Tools;

interface

uses Classes, SysUtils, FIBDatabase, FIBQuery, FIBMiscellaneous, DB, ibase;

procedure ExecQuery(FQ: TFIBQuery; SQLText: String);
procedure DoQuery(FQ: TFIBQuery; SQLText: String);
procedure CQ(FQ : TFIBQuery);
function SaveBlob(FName : String; DB: TFIBDatabase; TA: TFIBTransaction) : TISC_QUAD;
procedure UpdateBlob(FName : String; var BlobID : TISC_QUAD; DB: TFIBDatabase; TA: TFIBTransaction);
procedure WriteBlob(FName : String; var BlobID : TISC_QUAD; DB: TFIBDatabase; TA: TFIBTransaction); overload;
procedure WriteBlob(var BlobID: TISC_QUAD; var S: String; DB: TFIBDatabase; TA: TFIBTransaction); overload;
procedure LoadBlob(BlobID: TISC_QUAD; var S : String; DB: TFIBDatabase; TA: TFIBTransaction); overload;
procedure LoadBlob(BlobID: TISC_QUAD; var Stream: TMemoryStream; DB: TFIBDatabase; TA: TFIBTransaction); overload;
procedure LoadTextBlob(BlobID: TISC_QUAD; var s : ShortString; DB: TFIBDatabase; TA: TFIBTransaction);
procedure SaveTextBlob(var BlobID: TISC_QUAD; var s : ShortString; DB: TFIBDatabase; TA: TFIBTransaction);
function EmptyBlobID: TISC_QUAD;
function IsEmptyBlob(ID: TISC_QUAD): Boolean;

const SelectAll : String = 'SELECT * FROM';

implementation

uses Dialogs;

function IsEmptyBlob(ID: TISC_QUAD): Boolean;
begin
  Result := ((ID.gds_quad_high = 0) And (ID.gds_quad_low = 0));
end;

function EmptyBlobID: TISC_QUAD;
begin
  Result.gds_quad_high := 0;
  Result.gds_quad_low := 0;
end;

procedure LoadBlob(BlobID: TISC_QUAD; var S : String; DB: TFIBDatabase; TA: TFIBTransaction);
var BlobStream  : TFIBBlobStream;
    List : TStringList;
begin
  S := '';
  If IsEmptyBlob(BlobID) Then
    Exit;
  List := TStringList.Create;
  BlobStream := TFIBBlobStream.Create; // Stream erstellen
  BlobStream.Database := DB;  // Datenbank-Verbindung zuweisen
  BlobStream.Transaction := TA; // Transaktion zuweisen
  BlobStream.Mode := bmRead; // wir wollen nur lesen
  BlobStream.BlobID := BlobID; // den Blob aus der DB lesen
  List.LoadFromStream(BlobStream);
  S := List.Text;
  BlobStream.Finalize; // und in den Blob speichern
  BlobStream.Free;
  FreeAndNil(List);
end;

procedure LoadBlob(BlobID: TISC_QUAD; var Stream: TMemoryStream; DB: TFIBDatabase; TA: TFIBTransaction);
var BlobStream  : TFIBBlobStream;
begin
  If not Assigned(Stream) Then
    Stream := TMemoryStream.Create;
  BlobStream := TFIBBlobStream.Create; // Stream erstellen
  BlobStream.Database := DB;  // Datenbank-Verbindung zuweisen
  BlobStream.Transaction := TA; // Transaktion zuweisen
  BlobStream.Mode := bmRead; // wir wollen nur lesen
  BlobStream.BlobID := BlobID; // den Blob aus der DB lesen
  BlobStream.SaveToStream(Stream);
  Stream.Position := 0; // an den Anfang des Streams zurückgehen
  If Stream.Size = 0 Then
    FreeAndNil(Stream);
  BlobStream.Finalize; // und in den Blob speichern
  BlobStream.Free;
end;

procedure LoadTextBlob(BlobID: TISC_QUAD; var s : ShortString; DB: TFIBDatabase; TA: TFIBTransaction);
var BlobStream  : TFIBBlobStream;
    List : TStringList;
begin
  List := TStringList.Create;
  BlobStream := TFIBBlobStream.Create; // Stream erstellen
  BlobStream.Database := DB;  // Datenbank-Verbindung zuweisen
  BlobStream.Transaction := TA; // Transaktion zuweisen
  BlobStream.Mode := bmRead; // wir wollen nur lesen
  BlobStream.BlobID := BlobID; // den Blob aus der DB lesen
  List.LoadFromStream(BlobStream);
  If List.Count > 0 Then s := List.Strings[0]
                    Else s := '';
  FreeAndNil(List);
  BlobStream.Finalize; // und in den Blob speichern
  BlobStream.Free;
end;

procedure SaveTextBlob(var BlobID: TISC_QUAD; var s : ShortString; DB: TFIBDatabase; TA: TFIBTransaction);
var BlobStream  : TFIBBlobStream;
    List : TStringList;
begin
  List := TStringList.Create;
  BlobStream := TFIBBlobStream.Create; // Stream erstellen
  BlobStream.Database := DB;  // Datenbank-Verbindung zuweisen
  BlobStream.Transaction := TA; // Transaktion zuweisen
  BlobStream.Mode := bmReadWrite; // wir wollen lesen und schreiben
  BlobStream.BlobID := BlobID; // den Blob aus der DB lesen
  List.Add(s);
  BlobStream.Position := 0;
  List.SaveToStream(BlobStream);
  FreeAndNil(List);
  BlobStream.Finalize; // und in den Blob speichern
  BlobID := BlobStream.BlobID;
  BlobStream.Free;
end;

procedure WriteBlob(FName : String; var BlobID : TISC_QUAD; DB: TFIBDatabase; TA: TFIBTransaction);
begin
  If ((BlobID.gds_quad_high = 0) And (BlobID.gds_quad_low = 0)) Then
    BlobID := SaveBlob(FName, DB, TA) // neuer Blob
  Else
    UpdateBlob(FName, BlobID, DB, TA); // Blob updaten
end;

procedure WriteBlob(var BlobID: TISC_QUAD; var S: String; DB: TFIBDatabase; TA: TFIBTransaction);
var BlobStream : TFIBBlobStream;
    pc         : PChar;
begin
  BlobStream := TFIBBlobStream.Create; // Stream erstellen
  BlobStream.Database := DB;  // Datenbank-Verbindung zuweisen
  BlobStream.Transaction := TA; // Transaktion zuweisen
  BlobStream.Mode := bmReadWrite; // wir wollen lesen und schreiben
  BlobStream.BlobID := BlobID; // den Blob aus der DB lesen
  pc := PChar(s);
  BlobStream.Write(pc^, Length(S));
  BlobStream.Finalize; // und in den Blob speichern
  BlobID := BlobStream.BlobID;
  BlobStream.Free;
end;

procedure UpdateBlob(FName : String; var BlobID : TISC_QUAD; DB: TFIBDatabase; TA: TFIBTransaction);
var BlobStream  : TFIBBlobStream;
begin
  If FileExists(FName) Then begin
    BlobStream := TFIBBlobStream.Create; // Stream erstellen
    BlobStream.Database := DB;  // Datenbank-Verbindung zuweisen
    BlobStream.Transaction := TA; // Transaktion zuweisen
    BlobStream.Mode := bmReadWrite; // wir wollen lesen und schreiben
    BlobStream.BlobID := BlobID; // den Blob aus der DB lesen
    BlobStream.LoadFromFile(FName); // den Stream mit den neuen Daten füllen
    BlobStream.Finalize; // und in den Blob speichern
    BlobID := BlobStream.BlobID;
    BlobStream.Free;
  end;
end;

function SaveBlob(FName : String; DB: TFIBDatabase; TA: TFIBTransaction) : TISC_QUAD;
var BlobStream  : TFIBBlobStream;
    EmptyBlobID : TISC_QUAD;
begin
  EmptyBlobID.gds_quad_high := 0;  // eine leere Blob-ID
  EmptyBlobID.gds_quad_low := 0;
  If FileExists(FName) Then begin
    BlobStream := TFIBBlobStream.Create; // Stream erstellen
    BlobStream.Database := DB;  // Datenbank-Verbindung zuweisen
    BlobStream.Transaction := TA; // Transaktion zuweisen
    BlobStream.Mode := bmWrite; // wir wollen nur schreiben
    BlobStream.LoadFromFile(FName); // den Stream füllen
    BlobStream.Finalize; // und in den Blob speichern
    Result := BlobStream.BlobID; // die ID des Blobs zurückgeben
    BlobStream.Free;
  end Else
    Result := EmptyBlobID; // wenn die Datei in "FName" nicht da ist, dann liefern wir ne leere BlobID
end;

procedure ExecQuery(FQ: TFIBQuery; SQLText: String);
begin
 FQ.Database.CheckActive;
 If FQ.Open Then FQ.Close;
 FQ.SQL.Clear;
 FQ.SQL.Add(SQLText);
 FQ.Prepare;
 FQ.ExecQuery;
end;

procedure DoQuery(FQ: TFIBQuery; SQLText: String);
begin
 FQ.Database.CheckActive;
 If FQ.Open Then FQ.Close;
 FQ.SQL.Clear;
 FQ.SQL.Add(SQLText);
 FQ.Prepare;
 FQ.ExecQuery;
end;

procedure CQ(FQ : TFIBQuery);
begin
 FQ.Close;
end;

end.
 