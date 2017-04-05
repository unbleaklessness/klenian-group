(* ::Package:: *)

BeginPackage @ "KlenianGroupMultiply`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

multiply::usage = "";

Begin @ "`Private`";
Module[{},

	multiply[elements_List, cayley_List] :=
	Module[{result, i},
		For[i = 1, i <= Length @ elements, i++,
			If[i == 1,
				result = cayley[[elements[[i]], elements[[i + 1]]]];
				i++;
			,
				result = cayley[[result, elements[[i]]]];
			];
		];
		result
	];

];
End[];
EndPackage[];
