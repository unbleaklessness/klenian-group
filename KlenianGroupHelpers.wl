(* ::Package:: *)

BeginPackage @ "KlenianGroupHelpers`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

selectColumnFromTable::usage = "";
findPair::usage = "";
addTitles::usage = "";

Begin @ "`Private`";
Module[{fontSize, itemSize, toDigit},

	addTitles[table_List] := Module[{newTable},
		newTable = table;
		PrependTo[newTable, Range @ Length @ table];
		newTable = MapIndexed[Prepend[#1, #2[[1]] - 1] &, newTable];
		newTable 
	];

	(* Takes first occurence of {a, b} in the list and return b, where a is element_ *)
	findPair[list_, element_] := Module[{},
		Select[list, #[[1]] == element &][[1, 2]]
	];

	selectColumnFromTable[table_List, index_Integer] := Select[If[# =!= {}, #[[index]]] & /@ table, # =!= Null &];
	
];
End[];
EndPackage[];
