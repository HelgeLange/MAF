{*******************************************************************************
Name         : uMAF_XmlStorage.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2016 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 09.04.2016
Last Update  : 09.04.2016
Version      : 1.0.000
Purpose      :
Last Changes :

1.0.000 (09.04.2016) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_XmlStorage;

interface

uses SysUtils, Classes, Windows,
     Xml.Win.msxmldom, Xml.XMLDoc,
     uMAF_CustomBaseDB;

Type TmafXmlStorage = class(TmafCustomBaseDB)
     private
       FpXML: TXMLDocument;
       FsFileName: String;
     protected
       function __GetConnected: Boolean; override;
       procedure __SetConnected(const Value: Boolean); override;
     public
     published
       property FileName : String read FsFileName write FsFileName;
       property Connected;
     end;


implementation

{ TmafXmlStorage }

function TmafXmlStorage.__GetConnected: Boolean;
begin
  Result := FpXML.Active;
end;

procedure TmafXmlStorage.__SetConnected(const Value: Boolean);
begin
  If FileExists(FsFileName) Then begin
    FpXML.Active := False;
    FpXML.Active := True;
    FpXML.LoadFromFile(FsFileName);
  end;
end;

end.
