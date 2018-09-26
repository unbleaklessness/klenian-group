(* ::Package:: *)

BeginPackage @ "KlenianGroupCayleyTable`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

getCayleyTableAndBag::usage = "";
getCayleyTableAndBag2::usage = "";
getCayleyTableAndBag3::usage = "";

Begin @ "`Private`";
Module[{},
	
	getCayleyTableAndBag[] :=
	Module[{pDone, bag, oldBagLength, gamma, a, b, c, p, goOnQ, n, newProd, pos, cayley, i, j, bag1, newProd1}, 
		(*gamma = Exp[2 Pi I / 7];
		{a, b, c} = {gamma^5 - gamma^2, gamma^3 - gamma^4, gamma^6 - gamma} / Sqrt[-7];

		p[1] = {{gamma, 0, 0}, {0, gamma^4, 0}, {0, 0, gamma^2}};
		p[2] = {{a, b, c}, {b, c, a}, {c, a, b}};*)
		
		z = Exp[2 Pi I / 13];
		p[1] = {
			{z^7, 0, 0, 0, 0, 0},
			{0, z^11, 0, 0, 0, 0},
			{0, 0, z^8, 0, 0, 0},
			{0, 0, 0, z^6, 0, 0},
			{0, 0, 0, 0, z^2, 0},
			{0, 0, 0, 0, 0, z^5}
		};
		p[2] = (-1 / Sqrt[13])*{
			{z^12 - z, z^10 - z^3, z^4 - z^9, z^5 - z^8, z^2 - z^11, z^6 - z^7},
			{z^10 - z^3, z^4 - z^9, z^12 - z, z^2 - z^11, z^6 - z^7, z^5 - z^8},
			{z^4 - z^9, z^12 - z, z^10 - z^3, z^6 - z^7, z^5 - z^8, z^2 - z^11},
			{z^5 - z^8, z^2 - z^11, z^6 - z^7, z - z^12, z^3 - z^10, z^9 - z^4},
			{z^2 - z^11, z^6 - z^7, z^5 - z^8, z^3 - z^10, z^9 - z^4, z - z^12},
			{z^6 - z^7, z^5 - z^8, z^2 - z^11, z^9 - z^4, z - z^12, z^3 - z^10}
		};
		
		bag = N[{p[1], p[2]}, 30];
		bag1 = {p[1],p[2]};
		
		oldBagLength = 0;
		While[goOnQ = oldBagLength < Length[bag]; oldBagLength = Length[bag]; goOnQ,
			n = Length[bag];
			Do[
				If[Head @ pDone[i, j] =!= Integer,
					newProd = bag[[j]] . bag[[i]];
					newProd1 = FullSimplify[ComplexExpand[bag1[[j]]] . ComplexExpand[bag1[[i]]]];
					pos = Position[bag, _? (# == newProd &), {1}, 1];
					If[pos === {},
						AppendTo[bag, newProd];
						AppendTo[bag1, newProd1];
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
	
		{cayley, bag, bag1}
	];
		
];
End[];
EndPackage[];


Position[{1,2,3}, 3][[1,1]]
