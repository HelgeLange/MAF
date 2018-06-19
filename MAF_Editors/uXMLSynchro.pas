unit uXMLSynchro;

interface

uses SysUtils, Windows, Classes, xmldom, XMLIntf, XMLDoc,
     {$IFDEF VER230}
     VCL.Dialogs,
     {$ELSE}
     Dialogs,
     {$ENDIF}
     // Modular Application Framework Components units
     uMAF_Globals, uMAF_ModuleController_DataHelper, uMAF_HookManager_Helper;

procedure __EnumRegisteredModuleFunctions(XML: TXMLDocument; nModuleID: Integer; aList: TList);
procedure __EnumInstallModuleFunctions(XML: TXMLDocument; nModuleID: Integer; aList: TList);


implementation

uses uMAF_Tools;

procedure __EnumRegisteredModuleFunctions(XML: TXMLDocument; nModuleID: Integer; aList: TList);
var i, j : Integer;
    HooksRoot, HookNode, HookIDNode : IXMLNode;
    pToken : PDataRecord;
begin
  If Not XML.Active Then
    Exit;

  HooksRoot := XML.DocumentElement.ChildNodes.FindNode('Hooks');
  If HooksRoot <> nil Then begin
    For i := 0 To HooksRoot.ChildNodes.Count - 1 Do begin
      HookNode := HooksRoot.ChildNodes.Nodes[i];
      For j := 0 To HookNode.ChildNodes.Count - 1 do begin
        HookIDNode := HookNode.ChildNodes.Nodes[j];
        If HookIDNode.Attributes['ModuleID'] = nModuleID Then begin
          pToken := __Create_DataRecord;
          pToken^.uID := HookNode.Attributes['uID'];
          pToken^.HookID := HookNode.Attributes['HookID'];
          pToken^.SubHookID := HookIDNode.Attributes['SubHookID'];
          If HookIDNode.HasAttribute('SubHookDesc') Then
            pToken^.Description := HookIDNode.Attributes['SubHookDesc'];
          aList.Add(pToken);
        end;
      end;
    end;
  end;
  XML.Active := False;
end;

procedure __EnumInstallModuleFunctions(XML: TXMLDocument; nModuleID: Integer; aList: TList);
var i, j : Integer;
    HooksRoot, HookNode, HookIDNode : IXMLNode;
    pToken : PMAFInstallToken;
    S : String;
begin
  If Not XML.Active Then
    Exit;

  HooksRoot := XML.DocumentElement.ChildNodes.FindNode('Hooks');
  If HooksRoot <> nil Then begin
    For i := 0 To HooksRoot.ChildNodes.Count - 1 Do begin
      HookNode := HooksRoot.ChildNodes.Nodes[i];
      For j := 0 To HookNode.ChildNodes.Count - 1 do begin
        HookIDNode := HookNode.ChildNodes.Nodes[j];
        If HookIDNode.Attributes['ModuleID'] = nModuleID Then begin
          pToken := __Create_InstallToken(iaInsert);
          pToken^.uID := HookIDNode.Attributes['uID'];
          pToken^.nHookID := HookNode.Attributes['HookID'];
          pToken^.nSubHookID := HookIDNode.Attributes['SubHookID'];
          pToken^.nModuleID := nModuleID;
          pToken^.nCodeGroupID := HookIDNode.Attributes['CodeGroupID'];
          pToken^.bActive := HookIDNode.Attributes['bActive'];
          If HookIDNode.HasAttribute('SubHookDesc') Then begin
            S := HookIDNode.Attributes['SubHookDesc'];
            StrToPChar(S, pToken^.sDescription);
          end;
          aList.Add(pToken);
        end;
      end;
    end;
  end;
  XML.Active := False;
end;

end.
