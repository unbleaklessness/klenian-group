(* ::Package:: *)

BeginPackage @ "GroupAnalyzer`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

generateGroup::usage = "Generates a group. Takes a list of initial elements (matrices) as the first argument and modulus of the group as the second argument.";
groupInfo::usage = "Prints out group elements and number of elements in the group. Takes group (list of matrices) as the first argument.";
(*(* TODO *) generateCayleyTable::usage = "Generates Cayley table of the group. Takes list of all group elements as first argument, returns Cayley table (2D array of indexes).";*)
findCommutatives::usage = "Finds commutative elements in the group. Takes Cayley table of the group as the first argument.";
findCenter::usage = "Finds center of the group. Takes Cayley table of the group as the first argument.";

Begin @ "`Private`";

generateElements[initials_List, mod_Integer] := FixedPoint[Union[Flatten[Outer[Mod[#1 . #2, mod] &, #, #, 1], 1]] &, initials];
secondIdentity[dimension_Integer, mod_Integer] := (mod - 1) * IdentityMatrix @ dimension;
removeEqualElements[group_List, mod_Integer] := Fold[If[MemberQ[#1, Mod[secondIdentity[Length @ #2, mod] . #2, mod]], Delete[#1, Position[#1, #2][[1, 1]]], #1] &, group, group];

findCommutatives[cayley_List] := Sort @ # & /@ Flatten[
	Table[
		If[cayley[[i, j]] == cayley[[j, i]], {i, j}, Nothing],
	{i, Length @ cayley},
	{j, Length @ cayley}], 1] // DeleteDuplicates;

generateGroup[initials_List, mod_Integer] := removeEqualElements[generateElements[initials, mod], mod];
(*generateCayleyTable[group_, mod] := uniteCayleyTables[cayleyForFirstBasis[group, mod], cayleyForSecondBasis[group, mod]];*)
groupInfo[group_List] := Print[{"Length", Length @ g}, "\n", {TableForm @ #} & /@ group];

(*findCenter[cayley_List] := Module[{check},
	check[element_] := Select[element, # =!= Null &];
	Take[#, 1][[1, 1]] & /@ Select[
		check @ # & /@ Table[
			If[cayley[[i, j]] == cayley[[j, i]], {i, j}]
		,
			{i, Length @ cayley}
		,
			{j, Length @ cayley}
	], Length @ # == Length @ cayley &]
];*)

findCenter[cayley_List] := #[[1, 1]] & /@ Select[Table[
	If[cayley[[i, j]] == cayley[[j, i]], {i, j}, Nothing],
		{i, Length @ cayley},
		{j, Length @ cayley}], Length @ # == Length @ cayley &];

End[];
EndPackage[];


m11 = {{0, 3}, {2, 4}};
m12 = {{0, 1}, {6, 0}};
m13 = {{3, 0}, {0, 5}};
m14 = {{1, 1}, {0, 1}};
group1 = {m11, m12, m13, m14};
