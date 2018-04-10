(* ::Package:: *)

BeginPackage @ "KlenianGroupFindOrder`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

findOrder::usage = "";

Begin @ "`Private`";
Module[{result, i, mult, k},

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
		For[i = 1, i <= Length @ result, i++,
			result[[i]] = Insert[result[[i]], Length @ result[[i, 2]], 2];
		];
		result
	];
	
];
End[];
EndPackage[];
