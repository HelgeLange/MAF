library PNGLoader;

uses
  SysUtils,
  Classes,
  Graphics,
  PNGImage;

{$R *.res}

procedure __Create_PNG(aStream: TMemoryStream; var AGraphic: TGraphic);
begin
  AGraphic := TPNGImage.Create;
  If Assigned(aStream) Then
    If aStream.Size > 0 Then
      AGraphic.LoadFromStream(aStream);
end;

exports
  __Create_PNG;

end.
