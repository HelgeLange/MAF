unit RegistryTools;

interface

procedure DelRegistryKey(Key, Value: String);
procedure SetRegistryStr(Key, Value, Wert: String);
procedure SetRegistryInt(Key, Value: String; Wert: Integer);
function GetRegistryStr(Key, Value: String) : String;
function GetRegistryInt(Key, Value: String) : Integer;

implementation

uses Registry;

var Reg : TRegistry;

procedure DelRegistryKey(Key, Value: String);
begin
 Reg         := TRegistry.Create;
 Reg.RootKey := HKEY_LOCAL_MACHINE;
 Reg.OpenKey(Key, True);
 Reg.DeleteValue(Value);
 Reg.Destroy;
 Reg := nil;
end;

procedure SetRegistryStr(Key, Value, Wert: String);
begin
 Reg         := TRegistry.Create;
 Reg.RootKey := HKEY_LOCAL_MACHINE;
 Reg.OpenKey(Key, True);
 Reg.WriteString(Value, Wert);
 Reg.Destroy;
 Reg := nil;
end;

procedure SetRegistryInt(Key, Value: String; Wert: Integer);
begin
 Reg         := TRegistry.Create;
 Reg.RootKey := HKEY_LOCAL_MACHINE;
 Reg.OpenKey(Key, True);
 Reg.WriteInteger(Value, Wert);
 Reg.Destroy;
 Reg := nil;
end;

function GetRegistryStr(Key, Value: String) : String;
begin
 Reg         := TRegistry.Create;
 Reg.RootKey := HKEY_LOCAL_MACHINE;
 Reg.OpenKey(Key, True);
 Try
  Result := Reg.ReadString(Value);
 Except
  on ERegistryException do Result := '';
 end;
 Reg.Destroy;
 Reg := nil;
end;

function GetRegistryInt(Key, Value: String) : Integer;
begin
 Reg         := TRegistry.Create;
 Reg.RootKey := HKEY_LOCAL_MACHINE;
 Reg.OpenKey(Key, True);
 Try
  Result := Reg.ReadInteger(Value);
 Except
  on ERegistryException do Result := 0;
 end;
 Reg.Destroy;
 Reg := nil;
end;


end.
