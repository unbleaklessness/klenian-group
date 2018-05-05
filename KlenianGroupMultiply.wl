(* ::Package:: *)

BeginPackage @ "KlenianGroupMultiply`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

multiply::usage = "";
multiply'::usage = "";
pow::usage = "";

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
	
	multiply'[first_Integer, second_Integer, cayley_List] := cayley[[first, second]];
	
	pow[element_Integer, power_Integer, cayley_List] := Fold[multiply'[#1, #2, cayley] &, element, Table[element, power - 1]];

];
End[];
EndPackage[];
