{*******************************************************************************
Name         : uMAF_Core.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2011-2015 by Helge Lange
Info         : HelgeLange@maf-components.com
Website      : http://www.maf-components.com
Date         : 28.09.2011
Last Update  : 29.07.2015
Version      : 1.0.003
Purpose      : base components for both manager and client components, that
               implement already basic needs to work within the MAF system
Last Changes :

1.0.003 (29.07.2015)------------------------------------------------------------
- [ADD] When adding pointers, callbacks and objects now the code checks if the
        parameter exists and only overwrites the value in that case
1.0.002 (16.07.2012)------------------------------------------------------------
- [ADD] when adding a new parameter in code with Add it now checks if the parameter
        already exists and in that case sets only the new value
- [ADD] the internally used funcion IndexOf now converts into UpperCase and
        compares in Uppercase
1.0.001 (05.11.2011)------------------------------------------------------------
- [ADD] Now supports pointer when saving the component to a stream. Pointer will
        be written as binary data and loaded when reading the component. Not much
        use in design time, but great when using TmafParameters to save to a
        stream and then save in Database, send as file or per tcp-ip to a server
- [CHG] when assigning a pointer ParameterItem to another, the memory contents
        will be copied into a newly created memory for the destination item,
        rather than just copying the pointer itself
1.0.000 (28.09.2011)------------------------------------------------------------
- initial version
*******************************************************************************}
unit uMAF_Parameters;

interface

uses Windows, Classes, SysUtils, Messages, Variants, Dialogs;

Type TParameterCollection = class;
     TParameterItem = class;

     TFreeParamPointer = procedure(Sender: TObject; Item: TParameterItem) Of Object;
     TParameterChangeEvent = procedure(Sender: TObject; Item: TParameterItem; var CanChange: Boolean) Of Object;

     TParamType = (ptVariant, ptPointer, ptObject, ptCallback);
     TVarParamType = (vptIn, vptOut, vptInOut);

     TParameterItem = class(TCollectionItem)
     private
       FVarType : TParamType;
       FVarParamType : TVarParamType;
       FsItemName : String;
       FpData : Pointer;
       FCallback : TNotifyEvent;          // only use for Callback functions
       FDefaultValue : Variant;
       FPtrSize : Integer;                // only used when FVarType is vtPointer
       FbEnabled : Boolean;               // used to enable/disable Parameters, so that receiver can distinguish between parameters active/inactive for this call
       FsTagString : String;
       FnTagInteger : Integer;
       FpParameterCollection : TParameterCollection;
       FOnChange : TParameterChangeEvent;
       procedure __SetVarType(const Value: TParamType);
       procedure __SetItemName(const Value: String);
       procedure __SetDefaultValue(const Value: Variant);
       function __GetIndex: Integer;
       procedure __SetPtrSize(const Value: Integer);
       function __GetAsInteger: Integer;
       function __GetAsString: String;
       procedure __SetAsInteger(const Value: Integer);
       procedure __SetAsString(const Value: String);
       function __GetDefaultValue: Variant;
       function __GetAsPointer: Pointer;
       procedure __SetAsPointer(const Value: Pointer);
       function __GetAsCallback: TNotifyEvent;
       procedure __SetAsCallback(const Value: TNotifyEvent);
       procedure ReadBinData(aStream: TStream);
       procedure WriteBinData(aStream: TStream);
     protected
       procedure DefineProperties(Filer: TFiler); override;
       function DoOnChange: Boolean;
     public
       {$IFDEF FPC}
       constructor Create(ACollection: TCollection); override;
       {$ELSE}
       constructor Create(Collection: TCollection); override;
       {$ENDIF}
       destructor Destroy; override;
       procedure Assign(Source: TPersistent); override;
       function GetDisplayName: String; override;
       procedure Clear;

       property AsString : String read __GetAsString write __SetAsString;
       property AsInteger : Integer read __GetAsInteger write __SetAsInteger;
       property AsPointer : Pointer read __GetAsPointer write __SetAsPointer;
       property AsCallback : TNotifyEvent read __GetAsCallback write __SetAsCallback;
     published
       property ItemName: String read FsItemName write __SetItemName;
       property DataType : TParamType read FVarType write __SetVarType default ptVariant;
       property Value : Variant read __GetDefaultValue write __SetDefaultValue;
       property VarParamType : TVarParamType read FVarParamType write FVarParamType;
       property Size : Integer read FPtrSize write __SetPtrSize;
       property Enabled : Boolean read FbEnabled write FbEnabled default True;
       property TagString : String read FsTagString write FsTagString;
       property TagInteger : Integer read FnTagInteger write FnTagInteger default 0;
       property Index: Integer read __GetIndex;
       property OnChange : TParameterChangeEvent read FOnChange write FOnChange;
     end;

     TParameterCollection = class(TOwnedCollection)
     private
       FFreeMemoryParams : Boolean;
       function GetItem(Index: Integer): TParameterItem;
       procedure SetItem(Index: Integer; const Value: TParameterItem);
     protected
       FOwner : TObject;  // the TmafParameters component
       FFreeParamPointer: TFreeParamPointer;
     public
       function Add: TParameterItem;
       function IndexOf(ItemName: String): Integer;
       procedure Assign(Source: TPersistent); override;
//       property Owner : TObject read FOwner write FOwner;

       property Items[Index: Integer] : TParameterItem read GetItem write SetItem;
     published
     end;

     TmafParameters = class(TComponent)
     private
       FpParameterCollection : TParameterCollection;
       function __GetCount: Integer;
       function __GetFreeParamPointer: TFreeParamPointer;
       procedure __SetFreeParamPointer(const Value: TFreeParamPointer);
       function __GetFreeMemoryParams: Boolean;
       procedure __SetFreeMemoryParams(const Value: Boolean);
       function __GetItemByName(AName: String): TParameterItem;
       function __GetItemByIndex(Index: Integer): TParameterItem;
     protected
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       function Add(AName: String; Value: Variant): Integer;
       function AddPointer(AName: String; Value: Pointer): Integer;
       function AddObject(AName: String; Value: TObject): Integer;
       function AddCallback(AName: String; ACallback: TNotifyEvent): Integer;
       procedure Delete(idx: Integer);
       property ParamByName[AName: String]: TParameterItem read __GetItemByName;
       property Param[Index: Integer]: TParameterItem read __GetItemByIndex;
     published
       property Count : Integer read __GetCount;
       property Data: TParameterCollection read FpParameterCollection write FpParameterCollection;
       property FreeMemoryParams: Boolean read __GetFreeMemoryParams write __SetFreeMemoryParams;
       property OnFreeParamPointer : TFreeParamPointer read __GetFreeParamPointer write __SetFreeParamPointer;
     end;

implementation

{ TParameterCollection }

function TParameterCollection.Add: TParameterItem;
begin
  Result := TParameterItem(inherited Add);
  Result.ItemName := 'Item_';// + IntToStr(NextID);
//  Result.DisplayName := 'Hossa_'
end;

procedure TParameterCollection.Assign(Source: TPersistent);
var i: Integer;
begin
  Clear;
  For i := 0 To TParameterCollection(Source).Count - 1 Do
    Add.Assign(TParameterCollection(Source).Items[i]);
end;

function TParameterCollection.GetItem(Index: Integer): TParameterItem;
begin
  Result := TParameterItem(inherited Items[Index]);
end;

function TParameterCollection.IndexOf(ItemName: String): Integer;
var i: Integer;
    S : String;
begin
  Result := -1;
  i := 0;
  S := UpperCase(ItemName);
  While (Result = -1) And (i < Count) Do begin
    If (CompareText(UpperCase(Items[i].ItemName), S) = 0) Then
      Result := i
    Else
      Inc(i);
   end;
end;

procedure TParameterCollection.SetItem(Index: Integer; const Value: TParameterItem);
begin
  inherited SetItem(Index, Value);
end;

{ TParameterItem }

procedure TParameterItem.Assign(Source: TPersistent);
begin
//  inherited;
  // first we clear existing data
  FDefaultValue := '';
  Case FVarType Of
    ptPointer  : begin
                   If Assigned(FpData) Then
                     If ((FpParameterCollection.FFreeMemoryParams) And (FPtrSize > 0)) Then begin
                       FreeMem(FpData, FPtrSize);
                       FpData := nil;
                       FPtrSize := 0;
                     end else
                       If Assigned(FpParameterCollection.FFreeParamPointer) Then
                         FpParameterCollection.FFreeParamPointer(FpParameterCollection, Self);
                 end;
    ptObject   : If Assigned(FpData) Then
                   If FpParameterCollection.FFreeMemoryParams Then
                     TObject(FpData).Free
                   Else
                     If Assigned(FpParameterCollection.FFreeParamPointer) Then
                       FpParameterCollection.FFreeParamPointer(FpParameterCollection, Self);
    ptCallback : FCallback := nil;
  end;
  FVarType := TParameterItem(Source).FVarType;
  FVarParamType := TParameterItem(Source).FVarParamType;
  FsItemName := TParameterItem(Source).FsItemName;
  If FVarType = ptPointer Then begin
    If ((TParameterItem(Source).FPtrSize > 0) And (TParameterItem(Source).FpData <> nil)) Then begin
      FPtrSize := TParameterItem(Source).FPtrSize;
      GetMem(FpData, FPtrSize);
      CopyMemory(FpData, TParameterItem(Source).FpData, FPtrSize);
    end else begin
      FpData := nil;
      FPtrSize := 0;
    end;
  end;
  // objects will just be copied, so be careful
  If FVarType = ptObject Then
    FpData := TParameterItem(Source).FpData;
    
  If FVarType = ptCallback Then
    FCallback := TParameterItem(Source).FCallback;
  If FVarType = ptVariant Then
    FDefaultValue := TParameterItem(Source).Value;
  FbEnabled := TParameterItem(Source).FbEnabled;
end;

{$IFDEF FPC}
constructor TParameterItem.Create(ACollection: TCollection);
{$ELSE}
constructor TParameterItem.Create(Collection: TCollection);
{$ENDIF}
begin
  inherited;
  {$IFDEF FPC}
  FpParameterCollection := TParameterCollection(ACollection);
  FsItemName := 'Item_' + IntToStr(Index);
  {$ELSE}
  FpParameterCollection := TParameterCollection(Collection);
  FsItemName := 'Item_' + IntToStr(FpParameterCollection.NextID);
  {$ENDIF}
  FpData := nil;
  FVarType := ptVariant;
  FsTagString := '';
  FnTagInteger := 0;
//  FsItemName := 'Item_' + IntToStr(FpParameterCollection.NextID);
  FbEnabled := True;
end;

procedure TParameterItem.ReadBinData(aStream: TStream);
begin
  TMemoryStream(aStream).Read(FPtrSize, SizeOf(Integer));
  If ((FVarType = ptPointer) And (FPtrSize > 0)) Then begin
    GetMem(FpData, FPtrSize);
    TMemoryStream(aStream).Read(FpData^, FPtrSize);
  end;
end;

procedure TParameterItem.WriteBinData(aStream: TStream);
begin
  TMemoryStream(aStream).Write(FPtrSize, SizeOf(Integer));
  If ((FVarType = ptPointer) And (FPtrSize > 0) And (FpData <> nil)) Then begin // here we write ONLY pointer
    TMemoryStream(aStream).Write(FpData^, FPtrSize);
  end;
end;

procedure TParameterItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('BinData', ReadBinData, WriteBinData, True);
end;

destructor TParameterItem.Destroy;
begin
  If ((FVarType = ptPointer) Or (FVarType = ptObject)) Then
    If Assigned(FpData) Then begin
      If FpParameterCollection.FFreeMemoryParams Then begin
        Case FVarType Of
          ptPointer : If FPtrSize > 0 Then
                        FreeMem(FpData, FPtrSize);
          ptObject  : TObject(FpData).Free;
        end;
        FpData := nil;
      end else begin
        If Assigned(FpParameterCollection.FFreeParamPointer) Then
          FpParameterCollection.FFreeParamPointer(FpParameterCollection.FOwner, Self);
        FpData := nil;
      end;
    end;
  inherited;
end;

function TParameterItem.DoOnChange: Boolean;
var CanChange: Boolean;
begin
  CanChange := True;
  If Assigned(FOnChange) Then
    FOnChange(FpParameterCollection.FOwner, Self, CanChange);
  Result := CanChange;
end;

function TParameterItem.GetDisplayName: String;
begin
  Result := FsItemName;
end;

function TParameterItem.__GetIndex: Integer;
begin
  Result := FpParameterCollection.IndexOf(FsItemName);
end;

procedure TParameterItem.Clear;
begin
  case FVarType of
    ptVariant  : FDefaultValue := Null;
    ptPointer  : begin
                   If Assigned(FpData) Then begin
                     FreeMem(FpData, Size);
                     FpData := nil;
                     Size := 0;
                   end;
                 end;
    ptObject   : If Assigned(FpData) Then
                   TObject(FpData).Free;
    ptCallback : FCallback := nil;
  end;
end;

function TParameterItem.__GetAsInteger: Integer;
begin
  Case FVarType Of
    ptVariant  : Result := Integer(FDefaultValue);
    ptPointer,
    ptObject   : Result :=  Integer(FpData);
    ptCallback : ;
  end;
end;

function TParameterItem.__GetAsCallback: TNotifyEvent;
begin
  If FVarType = ptCallback Then
    Result := FCallback;
end;

procedure TParameterItem.__SetAsCallback(const Value: TNotifyEvent);
var OldCallback : TNotifyEvent;
begin
  If FVarType = ptCallback Then begin
    OldCallback := FCallback;
    FCallback := Value;
    If Not DoOnChange Then
      FCallback := OldCallback;
  end;
end;

procedure TParameterItem.__SetAsInteger(const Value: Integer);
var OldValue : Variant;
    pData : Pointer;
begin
  Case FVarType Of
    ptVariant  : begin
                   OldValue := FDefaultValue;
                   FDefaultValue := Value;
                   If Not DoOnChange Then
                     FDefaultValue := OldValue;
                 end;
    ptPointer,
    ptObject   : begin
                   pData := FpData;
                   FpData := Pointer(Value);
                   If Not DoOnChange Then
                     FpData := pData;
                 end;
    ptCallback : ;
  end;
end;

function TParameterItem.__GetAsPointer: Pointer;
begin
  If ((FVarType = ptPointer) Or (FVarType = ptObject)) Then begin
    Result := Pointer(FpData);
  end else
    Raise EComponentError.Create('Cannot return a variant data type as pointer!');
end;

procedure TParameterItem.__SetAsPointer(const Value: Pointer);
var pData : Pointer;
begin
  If ((FVarType = ptPointer) Or (FVarType = ptObject)) Then begin
    pData := FpData;
    FpData := Value;
    If Not DoOnChange Then
      FpData := pData;
  end else
    Raise EComponentError.Create('Cannot set a pointer to a variant data type!');
end;

function TParameterItem.__GetAsString: String;
begin
  Case FVarType Of
    ptVariant  : Result := String(FDefaultValue);
    ptPointer,
    ptObject   : Result := IntToHex(Integer(FpData), 2);
    ptCallback : ;
  end;
end;

function TParameterItem.__GetDefaultValue: Variant;
begin
  Case FVarType Of
    ptVariant : Result := FDefaultValue;
    ptPointer : Result := Integer(FpData);
    ptObject  : Result := Integer(FpData);
    ptCallback: ;
  end;
end;

procedure TParameterItem.__SetAsString(const Value: String);
var OldValue : String;
begin
  OldValue := FDefaultValue;
  Case FVarType Of
    ptVariant  : FDefaultValue := Value;
    ptPointer,
    ptObject,
    ptCallback : Raise EComponentError.Create('Cannot assign Pointer, Objects or Callbacks as string!');
  end;
  If Not DoOnChange Then
    FDefaultValue := OldValue;
end;

procedure TParameterItem.__SetDefaultValue(const Value: Variant);
var OldValue : Variant;
begin
  OldValue := FDefaultValue;
  Case FVarType Of
    ptPointer,
    ptObject,
    ptCallback : FDefaultValue := 0;
    Else FDefaultValue := Value;
  end;
  If Not DoOnChange Then
    FDefaultValue := OldValue;
end;

procedure TParameterItem.__SetItemName(const Value: String);
begin
  FsItemName := Value;
  DisplayName := Value;
{  Exit;
  If Value <> FsItemName Then
    If FpParameterCollection.IndexOf(Value) = -1 Then begin
      FsItemName := Value;
      ShowMessage(FsItemName);
    end
    Else
      EComponentError.Create('TParameterItem.__SetItemName: The item name "' + Value + '" already exists!');   }
end;

// FPtrSize is only used for the data type vtPointer to free the memory when the 
// item is deleted
procedure TParameterItem.__SetPtrSize(const Value: Integer);
begin
  If FVarType = ptPointer Then
    FPtrSize := Value
  Else
    FPtrSize := 0;
end;

procedure TParameterItem.__SetVarType(const Value: TParamType);
begin
  Case FVarType Of
    ptPointer,
    ptObject,
    ptCallback : FDefaultValue := '';
  end;
  FVarType := Value;
end;

{ TmafParameters }

function TmafParameters.Add(AName: String; Value: Variant): Integer;
var pItem: TParameterItem;
    idx : Integer;
begin
  idx := Data.IndexOf(AName);
  If idx = -1 Then begin
    pItem := Data.Add;
    pItem.ItemName := AName;
    pItem.DataType := ptVariant;
  end else
    pItem := Data.Items[idx];
  pItem.Value := Value;
  Result := pItem.Index;
end;

function TmafParameters.AddCallback(AName: String; ACallback: TNotifyEvent): Integer;
var pItem: TParameterItem;
    idx : Integer;
begin
  idx := Data.IndexOf(AName);
  If idx = -1 Then begin
    pItem := Data.Add;
    pItem.ItemName := AName;
    pItem.DataType := ptCallback;
  end else
    pItem := Data.Items[idx];
  pItem.AsCallback := ACallback;
  Result := pItem.Index;
end;

function TmafParameters.AddObject(AName: String; Value: TObject): Integer;
var pItem: TParameterItem;
    idx : Integer;
begin
  idx := Data.IndexOf(AName);
  If idx = -1 Then begin
    pItem := Data.Add;
    pItem.ItemName := AName;
    pItem.DataType := ptObject;
  end else
    pItem := Data.Items[idx];
  pItem.AsPointer := Pointer(Value);
  Result := pItem.Index;
end;

function TmafParameters.AddPointer(AName: String; Value: Pointer): Integer;
var pItem: TParameterItem;
    idx : Integer;
begin
  idx := Data.IndexOf(AName);
  If idx = -1 Then begin
    pItem := Data.Add;
    pItem.ItemName := AName;
    pItem.DataType := ptPointer;
  end else
    pItem := Data.Items[idx];
  pItem.AsPointer := Value;
  Result := pItem.Index;
end;

constructor TmafParameters.Create(AOwner: TComponent);
begin
  inherited;
  FpParameterCollection := TParameterCollection.Create(Self, TParameterItem);
  FpParameterCollection.FOwner := Self;
end;

destructor TmafParameters.Destroy;
begin
  FpParameterCollection.Free;
  inherited;
end;

function TmafParameters.__GetCount: Integer;
begin
  Result := FpParameterCollection.Count;
end;

function TmafParameters.__GetFreeParamPointer: TFreeParamPointer;
begin
  Result := FpParameterCollection.FFreeParamPointer;
end;

function TmafParameters.__GetItemByIndex(Index: Integer): TParameterItem;
begin
  Result := nil;
  If ((Index > -1) And (Index < Data.Count)) Then
    Result := Data.Items[Index];
end;

function TmafParameters.__GetItemByName(AName: String): TParameterItem;
var idx: Integer;
begin
  Result := nil;
  idx := Data.IndexOf(AName);
  If idx > -1 Then
    Result := Data.Items[idx];
end;

procedure TmafParameters.__SetFreeParamPointer(const Value: TFreeParamPointer);
begin
  FpParameterCollection.FFreeParamPointer := Value;
end;

function TmafParameters.__GetFreeMemoryParams: Boolean;
begin
  Result := FpParameterCollection.FFreeMemoryParams;
end;

procedure TmafParameters.__SetFreeMemoryParams(const Value: Boolean);
begin
  FpParameterCollection.FFreeMemoryParams := Value;
end;

procedure TmafParameters.Delete(idx: Integer);
begin
  If ((idx > -1) And (idx < Data.Count)) Then
    Data.Delete(idx);
end;

end.
