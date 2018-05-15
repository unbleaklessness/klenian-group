(* ::Package:: *)

BeginPackage @ "KlenianGroupFindNormalizer`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "KlenianGroupHelpers`";

findNormalizer::usage = "";

Begin @ "`Private`";
Module[{},

	findNormalizer[dihedral_, inverses_] := Module[{result, i, j},
		result = {};
		For[i = 1, i <= Length @ dihedral[[1]], i++,
			AppendTo[result, {}];
			For[j = 1, j <= Length @ dihedral[[1]], j++,
				AppendTo[result[[i]], 
					dihedral[[dihedral[[findPair[inverses, i], j]], i]]
				];
			];
		];
		result
	];

];
End[];
EndPackage[];
