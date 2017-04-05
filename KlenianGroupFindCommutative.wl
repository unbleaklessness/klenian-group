(* ::Package:: *)

BeginPackage @ "KlenianGroupFindCommutative`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

findCommutatives::usage = "";
findCommutativesForOne::usage = "";
findCommutativesForGroup::usage = "";

Begin @ "`Private`";
Module[{},

	findCommutativesForOne[cayley_List, element_Integer] := Module[{},
		Select[Range @ Length @ cayley, cayley[[#, element]] == cayley[[element, #]] &]
	];
	
	findCommutatives[cayley_List] := Module[{},
		Select[
			Flatten[
				Table[
					If[cayley[[i, j]] == cayley[[j, i]], {i, j}]
				,
					{i, Length @ cayley}
				,
					{j, Length @ cayley}
		], 1], ListQ]
	];

	findCommutativesForGroup[cayley_List, group_List] := Module[{},
		Select[
			Flatten[
				Table[
					If[cayley[[group[[i]], j]] == cayley[[j, group[[i]]]], {group[[i]], j}]
				,
					{i, Length @ group}
				,
					{j, Length @ cayley}
		], 1], ListQ]
	];

];
End[];
EndPackage[];
