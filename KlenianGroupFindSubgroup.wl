(* ::Package:: *)

BeginPackage @ "KlenianGroupFindSubgroup`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "KlenianGroupMultiply`";

powersSubgroup::usage = "";

Begin @ "`Private`";
Module[{powers, pos},

	powersSubgroup[element_, cayley_List] := {
		powers = FoldList[multiply[{#1, #2}, cayley] &, element, Table[element, Length @ cayley]];
		pos = Position[powers, element][[2, 1]] - 1;
		Take[powers, pos]
	};

];
End[];
EndPackage[];
