(* ::Package:: *)

BeginPackage @ "KlenianGroupFindInverses`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "KlenianGroupHelpers`";

findInverses::usage = "";

Begin @ "`Private`";
Module[{},

	findInverses[cayley_List, neutral_] := Module[{}, 
		Select[
			Flatten[
				Table[
					If[cayley[[i, j]] == neutral, {i, j}]
				,
					{i, Length @ cayley}
				,
					{j, Length @ cayley}
		], 1], ListQ]
	];

];
End[];
EndPackage[];
