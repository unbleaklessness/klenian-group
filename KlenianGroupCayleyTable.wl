(* ::Package:: *)

BeginPackage @ "KlenianGroupCayleyTable`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

getCayleyTableAndBag::usage = "";

Begin @ "`Private`";
Module[{},
	
	getCayleyTableAndBag[] :=
	Module[{pDone, bag, oldBagLength, gamma, a, b, c, p, goOnQ, n, newProd, pos, cayley, i, j}, 
		gamma = Exp[2 Pi I / 7];
		{a, b, c} = {gamma^5 - gamma^2, gamma^3 - gamma^4, gamma^6 - gamma} / Sqrt[-7];

		p[1] = {{gamma, 0, 0}, {0, gamma^4, 0}, {0, 0, gamma^2}};
		p[2] = {{a, b, c}, {b, c, a}, {c, a, b}};
		
		bag = N[{p[1], p[2]}, 30];
		
		oldBagLength = 0;
		While[goOnQ = oldBagLength < Length[bag]; oldBagLength = Length[bag]; goOnQ,
			n = Length[bag];
			Do[
				If[Head @ pDone[i, j] =!= Integer,
					newProd = bag[[j]] . bag[[i]];
					pos = Position[bag, _? (# == newProd &), {1}, 1];
					If[pos === {},
						AppendTo[bag, newProd];
					,
						pDone[i, j] = pos[[1, 1]];
					];
				];
			,
				{i, n}, {j, n}
			];
		];
		
		cayley = {};
		
		For[i = 1, i <= 168, i++,
			AppendTo[cayley, {}];
			For[j = 1, j <= 168, j++,
				AppendTo[cayley[[i]], pDone[i, j]];
			];
		];
	
		{cayley, bag}
	];
	
];
End[];
EndPackage[];
