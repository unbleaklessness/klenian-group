(* ::Package:: *)

BeginPackage @ "GroupAnalyzer`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

generateGroup::usage = "Generates a group. Takes a list of initial elements (matrices) as the first argument and modulus of the group as the second argument.";
removeEqualElements::usage = "Removes equal elements from the group. Takes list of group elements as the first argument.";
groupInfo::usage = "Prints out group elements and number of elements in the group. Takes group (list of matrices) as the first argument.";
generateCayleyTable::usage = "Generates Cayley table of the group. Takes list of all group elements as first argument, returns Cayley table (2D array of indexes).";
findCommutatives::usage = "Finds commutative elements in the group. Takes Cayley table of the group as the first argument.";
findCenter::usage = "Finds center of the group. Takes Cayley table of the group as the first argument.";

Begin @ "`Private`";

generateElements[initials_List, mod_Integer] := FixedPoint[Union[Flatten[Outer[Mod[#1 . #2, mod] &, #, #, 1], 1]] &, initials];
secondIdentity[dimension_Integer, mod_Integer] := (mod - 1) * IdentityMatrix @ dimension;
removeEqualElements[group_List, mod_Integer] := Fold[If[MemberQ[#1, Mod[secondIdentity[Length @ #2, mod] . #2, mod]], Delete[#1, Position[#1, #2][[1, 1]]], #1] &, group, group];
deleteColumn[table_List, n_Integer] := Delete[#, n] & /@ table;
removeEqualsFromCayley[cayley_List, mod_Integer] := Fold[If[MemberQ[#1[[1]], Mod[secondIdentity[Length @ #2, mod] . #2, mod]],
	deleteColumn[Delete[#1, Position[#1[[1]], #2][[1, 1]]], Position[#1[[1]], #2][[1, 1]]], #1] &, cayley, cayley[[1]]];

findCommutatives[cayley_List] := Sort @ # & /@ Flatten[
	Table[
		If[cayley[[i, j]] == cayley[[j, i]], {i, j}, Nothing],
	{i, Length @ cayley},
	{j, Length @ cayley}], 1] // DeleteDuplicates;

(*generateGroup[initials_List, mod_Integer] := removeEqualElements[generateElements[initials, mod], mod];*)
generateGroup[initials_List, mod_Integer] := generateElements[initials, mod];
gct1[group_, mod_] := (#[[1, 1]] & /@ # &) @ # & /@ Table[Position[group, Mod[group[[i]] . group[[j]], mod]], {i, Length @ group}, {j, Length @ group}];
gct11[table_] := Module[{}, i = 0; Prepend[#, i++] & /@ table];
gct2[group_, mod_] := gct11 @ Prepend[group, Range @ Length @ group];
generateCayleyTable[group_, mod_] := removeEqualsFromCayley[gct2[group, mod], mod];
groupInfo[group_List] := Print[{"Length", Length @ g}, "\n", {TableForm @ #} & /@ group];

findCenter[cayley_List] := #[[1, 1]] & /@ Select[Table[
	If[cayley[[i, j]] == cayley[[j, i]], {i, j}, Nothing],
		{i, Length @ cayley},
		{j, Length @ cayley}], Length @ # == Length @ cayley &];

End[];
EndPackage[];
