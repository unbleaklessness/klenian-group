(* ::Package:: *)

BeginPackage @ "KlenianGroupFindOrder`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "KlenianGroupHelpers`";

findOrder::usage = "";

Begin @ "`Private`";
Module[{result, i, mult, j},
	
	findOrder[cayley_List, neutral_Integer] := Module[{},
		result = {};
		For[i = 1, i <= Length @ cayley[[1]], i++,
			For[j = 1, j <= Length @ cayley[[1]], j++,
				If[pow[i, j, cayley] == neutral,
					AppendTo[result, {i, j}];
					Break[];
				];
			];
		];
		result
	];
	
];
End[];
EndPackage[];
