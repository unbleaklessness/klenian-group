(* ::Package:: *)

BeginPackage @ "KlenianGroupFindSubgroup`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "KlenianGroupMultiply`";

powersSubgroup::usage = "";

Begin @ "`Private`";
Module[{},

	powersSubgroup[cayley_List, element_, power_] :=
		FoldList[multiply[{#1, #2}, cayley] &, element, Table[element, power]];

];
End[];
EndPackage[];
