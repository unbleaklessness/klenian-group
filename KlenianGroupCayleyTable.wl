(* ::Package:: *)

BeginPackage @ "KlenianGroupCayleyTable`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

getCayleyTableAndBag::usage = "";

Begin @ "`Private`";
Module[{},
	
	getCayleyTableAndBag[] :=
	Module[{pDone, bag, oldBagLength, gamma, a, b, c, p, goOnQ, n, newProd, pos}, 
		gamma = Exp[2 Pi I / 7];
		{a, b, c} = {gamma^5 - gamma^2, gamma^3 - gamma^4, gamma^6 - gamma} / Sqrt[-7];

		p[1] = {{gamma, 0, 0}, {0, gamma^4, 0}, {0, 0, gamma^2}};
		p[2] = {{a, b, c}, {b, c, a}, {c, a, b}};
		
		bag = N[{p[1], p[2]}, 30];
		
		pDone = {};
		oldBagLength = 0;
		While[goOnQ = oldBagLength < Length[bag]; oldBagLength = Length[bag]; goOnQ,
			n = Length[bag];
			Do[
				AppendTo[pDone, {}];
				If[Quiet @ Not @ IntegerQ @ pDone[[i, j]],
					newProd = bag[[j]] . bag[[i]];
					pos = Position[bag, _? (# == newProd &), {1}, 1];
					If[pos === {},
						AppendTo[bag, newProd];
						,
						AppendTo[pDone[[i]], pos[[1, 1]]];
					];
				];
			,
				{i, n}, {j, n}
			];
		];
	
		{Select[pDone, # =!= {} &], bag}
	];
	
];
End[];
EndPackage[];
