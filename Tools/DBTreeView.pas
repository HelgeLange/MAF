unit DBTreeView;

interface

uses Windows, DB, ComCtrls, Classes, SysUtils, controls;

function TreeAddItem(Sender: TTreeView; ItemList: TStrings; Bookmark: Pointer; Resort: Boolean): TTreeNode;
function TreeFindItem(Sender: TTreeView; NodeItem: TTreeNode; Name: String): TTreeNode;

implementation

// In der ItemList sind in jedem String die Verweise auf die Bilder in der
// ImageList gespeichert. Nach der Nummer des Bildes folgt ein Leerzeichen
// und darauf der Text, welcher in der Liste erscheinen soll.
function TreeAddItem(Sender: TTreeView; ItemList: TStrings; Bookmark: Pointer; Resort: Boolean): TTreeNode;
var ThisNode, Node : TTreeNode;
    I, J           : Integer;
    S              : String;
begin
 Node := nil;
 //nil = level 0 Kein übergeordnetes Element = root
 //wird von TreeFindItem getestet
 for I := 0 to Itemlist.count -1 do begin
  S := ItemList[i];
  J := StrToInt(Copy(S, 1, Pos(' ', S)-1));
  Delete(S, 1, Pos(' ', S));
  ItemList[i] := S;
  ThisNode := TreeFindItem(Sender, node, Itemlist[i]);
  if ThisNode <> nil then Node := ThisNode else begin
   if I < Itemlist.count -1 then begin
    if I = 0 then Node := Sender.Items.Add(Node, Itemlist[i])
             else Node := Sender.Items.AddChild(Node, Itemlist[i]);
    Node.ImageIndex := J;
    Node.SelectedIndex := J;
   end else begin
    if I = 0 then Node := Sender.Items.AddObject(Node, Itemlist[i], Bookmark)
             else Node := Sender.Items.AddChildObject(Node, Itemlist[i], Bookmark);
    Node.ImageIndex := J;
    Node.SelectedIndex := J;
   end; // if I < Itemlist.count -1
    Node.StateIndex := Node.Level + 1;
    if Resort and (Node.Parent <> nil) then Node.Parent.AlphaSort;
  end; // if ThisNode <> nil
 end;  // for I := 0 to Itemlist.count -1
 Result := Node;
end;

function TreeFindItem(Sender: TTreeView; NodeItem: TTreeNode; Name: String): TTreeNode;
begin
 if NodeItem = nil Then NodeItem := Sender.Items.GetFirstNode
                   Else NodeItem := NodeItem.GetFirstChild;
 //NodeItem ist das 1. Item des gewählten Levels
 //ween dieses Level keine Items besitzt, ist NodeItem = nil
 if (NodeItem <> nil) and (NodeItem.Text <> Name) then
 repeat
  NodeItem := NodeItem.GetNextSibling;
 until (NodeItem = nil) or (NodeItem.Text = Name);
 Result := NodeItem;
end;

end.
