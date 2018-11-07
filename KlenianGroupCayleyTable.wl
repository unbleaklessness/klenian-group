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
		
		(*z = Exp[2 Pi I / 13];
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
		};*)
		
		(*p[1] = {
			{2, 0},
			{0, 7}
		};
		p[2] = {
			{0, 5},
			{5, 3}
		};
		p[3] = {
			{1, 1},
			{0, 1}
		};
		p[4] = {
			{0, 1},
			{12, 0}
		};
		p[5] = {
			{2, 2},
			{3, 10}
		};*)
		
		p[1] = {
			{0, 3},
			{2, 4}
		};
		p[2] = {
			{0, 1},
			{6, 0}
		};
		p[3] = {
			{3, 0},
			{0, 5}
		};
		p[4] = {
			{1, 1},
			{0, 1}
		};
		
		bag = N[{p[1], p[2], p[3], p[4]}, 30];
		bag1 = {p[1], p[2], p[3], p[4]};
		
		oldBagLength = 0;
		counter = 1;
		While[goOnQ = oldBagLength < Length[bag1]; oldBagLength = Length[bag1]; goOnQ,
			n = Length[bag1];
			Do[
				If[Head @ pDone[i, j] =!= Integer,
					(*If[bag1[[i]] \[Equal] 12 * IdentityMatrix[2],
						newProd1 = bag1[[j]];
					,
						newProd1 = Mod[bag1[[j]] . bag1[[i]], 13]
					];*)
					newProd1 = Mod[bag1[[j]] . bag1[[i]], 7];
					(*If[newProd1 \[Equal] 12 * IdentityMatrix @ 2, newProd1 = IdentityMatrix @ 2];*)
					pos = Position[bag1, _? (# == newProd1 &), {1}, 1];
					If[pos === {},
						(*If[counter < 50, TableForm @ # & /@ {bag1[[j]], bag1[[i]], newProd1} // Print];*)
						(*Print @ {counter, TableForm @ newProd1};*)
						If[Mod[counter, 99] == 0, Print @ counter];
						If[counter > 290, Print @ counter];
						If[counter == 2178, Break[]];
						counter = counter + 1;
						newProd = bag[[j]] . bag[[i]];
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
