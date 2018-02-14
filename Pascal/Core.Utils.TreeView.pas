unit Core.Utils.TreeView;


interface

uses
  Classes,
  SysUtils,

  Core.Strings,


  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,

  ComCtrls;

  function Find(Data:pointer; Parent:TTreeNode):TTreeNode; overload;
  function Find(Data:pointer; TreeView:TTreeView):TTreeNode; overload;
  function Find(sPath:Core.Strings.VarString; TreeView:TTreeView):TTreeNode; overload;
  function Find(sName:Core.Strings.VarString; Parent:TTreeNode):TTreeNode; overload;
  function Find(var saPath:Core.Arrays.Types.VarString; TreeView:TTreeView):TTreeNode; overload;
  function Add(sPath:Core.Strings.VarString; TreeView:TTreeView; const Data:pointer=nil):TTreeNode; overload;
  procedure Clear(TreeView:TTreeView); overload;

implementation

procedure Clear(TreeView:TTreeView);
begin
  TreeView.BeginUpdate;
  Try
    TreeView.Items.Clear;
  finally
    TreeView.EndUpdate;
  end;
end;

function Find(sPath:Core.Strings.VarString; TreeView:TTreeView):TTreeNode;
var
  saPath:Core.Arrays.Types.VarString;
begin
  Core.Arrays.VarString.fromString(saPath,sPath,'/');
  Try
    Result:=Find(saPath,TreeView);
  finally
    Done(saPath);
  end;
end;

function Find(Data:pointer; Parent:TTreeNode):TTreeNode;
var
  iCt,iLcv:LongInt;
begin
  iCt:=Parent.Count; iLcv:=0; Result:=nil;
  while (iLcv<iCt) and (Result=nil) do begin
    if Parent.Items[iLcv].Data=Data then
      Result:=Parent.Items[iLcv]
    else
      Result:=Find(Data,Parent.Items[iLcv]);
    Inc(iLcv);
  end;
end;

function Find(sName:Core.Strings.VarString; Parent:TTreeNode):TTreeNode;
var
  iCt,iLcv:LongInt;
begin
  iCt:=Parent.Count; iLcv:=0; Result:=nil;
  while (iLcv<iCt) and (Result=nil) do begin
    if Parent.Items[iLcv].Text=sName then
      Result:=Parent.Items[iLcv];
    Inc(iLcv);
  end;
end;


function Find(Data:pointer; TreeView:TTreeView):TTreeNode;
var
  iCt,iLcv:LongInt;
begin
  iCt:=TreeView.Items.Count; iLcv:=0; Result:=nil;
  while (iLcv<iCt) and (Result=nil) do begin
    Result:=Find(Data,TreeView.Items[iLcv]);
    Inc(iLcv);
  end;
end;


function Find(var saPath:Core.Arrays.Types.VarString; TreeView:TTreeView):TTreeNode;
var
  iCount:LongInt;
  iLevel:LongInt;

  function SearchNode(Node:TTreeNode):TTreeNode;
  var
    iNodeLcv:LongInt;
    iNodeCount:LongInt;
  begin
    Result:=nil; iNodeLcv:=0; iNodeCount:=Node.Count;
    While (iNodeLcv<iNodeCount) and (Result=nil) do begin
      if SameText(saPath[iLevel],Node.Items[iNodeLcv].Text) then begin
        if ( (iLevel+1) >=iCount ) then
          Result:=Node.Items[iNodeLcv]
        else begin
          Inc(iLevel);
          Result:=SearchNode(Node.Items[iNodeLcv]);
        end;
      end;
      Inc(iNodeLcv);
    end;
  end;

  function SearchTreeView:TTreeNode;
  var
    iNodeLcv:LongInt;
    iNodeCount:LongInt;
  begin
    Result:=nil; iNodeLcv:=0; iNodeCount:=TreeView.Items.Count;
    While (iNodeLcv<iNodeCount) and (Result=nil) do begin
      if (TreeView.Items[iNodeLcv].Level=0) and SameText(saPath[iLevel],TreeView.Items[iNodeLcv].Text) then begin
         if ( (iLevel+1) >= iCount ) then
           Result:=TreeView.Items[iNodeLcv]
         else begin
           Inc(iLevel);
           Result:=SearchNode(TreeView.Items[iNodeLcv]);
         end;
      end;
      Inc(iNodeLcv);
    end;
  end;
begin
  Result:=nil;
  iLevel:=0; iCount:=System.Length(saPath);
  If iCount>0 then
    Result:=SearchTreeView;
end;

function Add(sPath:Core.Strings.VarString; TreeView:TTreeView; const Data:pointer=nil):TTreeNode;
var
  saPath:Core.Arrays.Types.VarString;
  iCount:LongInt;
  iLevel:LongInt;

  function AddNode(Node:TTreeNode):TTreeNode;
  var
    iNodeLcv:LongInt;
    iNodeCount:LongInt;
  begin
    Result:=nil; iNodeLcv:=0; iNodeCount:=Node.Count;
    While (iNodeLcv<iNodeCount) and (Result=nil) do begin
      if SameText(saPath[iLevel],Node.Items[iNodeLcv].Text) then begin
        // The node @ level was found
        if ( (iLevel+1) >=iCount ) then begin // Done evaluating
          Result:=Node.Items[iNodeLcv];
          Result.Data:=Data;
        end else begin
          // There is going to be a subnode here...
          Inc(iLevel);
          Result:=AddNode(Node.Items[iNodeLcv]);
        end;
      end;
      Inc(iNodeLcv);
    end;
    if (Result=nil) then begin
      if ( (iLevel+1) >=iCount) then begin// Done evaluating
        Result:=TreeView.Items.AddChild(Node,saPath[iLevel]);
        Result.Data:=Data;
      end else begin
        // There are going to be more subnodes here...
        Inc(iLevel);
        Result:=AddNode(TreeView.Items.AddChild(Node,saPath[iLevel-1]));
      end;
    end;
  end;

  function AddToTreeView:TTreeNode;
  var
    iNodeLcv:LongInt;
    iNodeCount:LongInt;
  begin
    Result:=nil; iNodeLcv:=0; iNodeCount:=TreeView.Items.Count;
    While (iNodeLcv<iNodeCount) and (Result=nil) do begin
      if (TreeView.Items[iNodeLcv].Level=0) and SameText(saPath[iLevel],TreeView.Items[iNodeLcv].Text) then begin
         if ( (iLevel+1) >= iCount ) then begin
           Result:=TreeView.Items[iNodeLcv];
           Result.Data:=Data;
         end else begin
           Inc(iLevel);
           Result:=AddNode(TreeView.Items[iNodeLcv]);
         end;
      end;
      Inc(iNodeLcv);
    end;
    If (Result=nil) then begin
      if ( (iLevel+1) >= iCount ) then begin
        Result:=TreeView.Items.Add(nil,saPath[iLevel]);
        Result.Data:=Data;
      end else begin
        Inc(iLevel);
        Result:=AddNode(TreeView.Items.Add(nil,saPath[iLevel-1]));
      end;
    end;
  end;
begin
  Result:=nil;
  Core.Arrays.VarString.fromString(saPath,sPath,'/',[soIgnoreDelimAtStart,soClearList]);
  Try
    Result:=Find(saPath,TreeView);
    if (Result=nil) then begin
      iLevel:=0; iCount:=System.Length(saPath);
      If iCount>0 then
        Result:=AddToTreeView;
    end;
  finally
    Done(saPath);
  end;
end;

end.

