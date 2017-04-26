(* ::Package:: *)

BeginPackage @ "KlenianGroupFindOrder`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

findOrder::usage = "";

Begin @ "`Private`";
Module[{},

	findOrder[cayley_List, neutral_Integer] := Module[{},
		result = {};
		For[i = 1, i <= Length @ cayley[[1]], i++,
			AppendTo[result, {i, {}}];
			mult = cayley[[i, i]];
			For[k = 1, k <= Length @ cayley[[1]], k++,
				If[mult == neutral, AppendTo[result[[i, 2]], k]];
				mult = cayley[[mult, i]];
			];
		];
		result
	];
	
];
End[];
EndPackage[];