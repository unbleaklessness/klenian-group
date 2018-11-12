(* ::Package:: *)

BeginPackage @ "KlenianGroupGenerator`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

generateKlenianGroup::usage = "Generate Cayley table for Klenian group.
Returns list where the first element is Cayley table (2D list of indexes), second element is a list of numerical matrices (Klenian group elements) and third is list of symbolic ones.";

Begin @ "`Private`";
	
generateKlenianGroup[] :=
Module[{gamma, a, b, c, p, bagSymbolic, bagNumeric, oldBagLength, goOnQ, n, indexes, productNumeric, position, productSymbolic, cayleyTable}, 
	gamma = Exp[2 Pi I / 7];
	{a, b, c} = {gamma^5 - gamma^2, gamma^3 - gamma^4, gamma^6 - gamma} / Sqrt[-7];
	p[1] = {{gamma, 0, 0}, {0, gamma^4, 0}, {0, 0, gamma^2}};
	p[2] = {{a, b, c}, {b, c, a}, {c, a, b}};
		
	bagSymbolic = {p[1],p[2]};
	bagNumeric = N[{p[1], p[2]}, 30];
	
	oldBagLength = 0;
	While[goOnQ = oldBagLength < Length[bagNumeric]; oldBagLength = Length[bagNumeric]; goOnQ,
		n = Length[bagNumeric];
		Do[
			If[Head @ indexes[i, j] =!= Integer,
				productNumeric = bagNumeric[[j]] . bagNumeric[[i]];
				position = Position[bagNumeric, _? (# == productNumeric &), {1}, 1];
				If[position === {},
					productSymbolic = bagSymbolic[[j]] . bagSymbolic[[i]];
					AppendTo[bagNumeric, productNumeric];
					AppendTo[bagSymbolic, productSymbolic];
				,
					indexes[i, j] = position[[1, 1]];
				];
			];
		,
			{i, n}, {j, n}
		];
	];
		
	cayleyTable = {};
		
	For[i = 1, i <= 168, i++,
		AppendTo[cayleyTable, {}];
		For[j = 1, j <= 168, j++,
			AppendTo[cayleyTable[[i]], indexes[i, j]];
		];
	];
	
	{cayleyTable, bagNumeric, bagSymbolic}
];

End[];
EndPackage[];
