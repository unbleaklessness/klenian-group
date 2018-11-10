(* ::Package:: *)

BeginPackage @ "KlenianGroupCayleyTable`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

getCayleyTableAndBag::usage = "";

Begin @ "`Private`";
Module[{},

	generateGroup[group_, modulus_] :=
	Module[{},
		FixedPoint[Union[Flatten[Outer[Mod[#1 . #2, modulus] &, #, #]]] &, group]]];
	];
	
	getCayleyTableAndBag[] :=
	Module[{}, 
		m1 = {
			{0, 3},
			{2, 4}
		};
		m2 = {
			{0, 1},
			{6, 0}
		};
		m3 = {
			{3, 0},
			{0, 5}
		};
		m4 = {
			{1, 1},
			{0, 1}
		};
		group = {m1, m2, m3, m4};
		
		
	
		{cayley, bag, bag1}
	];
		
];
End[];
EndPackage[];


(*m1 = {
			{0, 3},
			{2, 4}
		};
		m2 = {
			{0, 1},
			{6, 0}
		};
		m3 = {
			{3, 0},
			{0, 5}
		};
		m4 = {
			{1, 1},
			{0, 1}
		};*)
		
		m1 = {
			{2, 0},
			{0, 7}
		};
		m2 = {
			{0, 5},
			{5, 3}
		};
		m3 = {
			{1, 1},
			{0, 1}
		};
		m4 = {
			{0, 1},
			{12, 0}
		};
		m5 = {
			{2, 2},
			{3, 10}
		};
		
		group = {m1, m2, m3, m4, m5};


mod = 13;
c1[m1_, m2_] := Mod[m1 . m2, mod];
c2[m_] := If[m == (mod - 1) * IdentityMatrix[Length @ m], IdentityMatrix[Length @ m], m];
composition[m1_, m2_] := c2 @ c1[m1, m2];
generateGroup[group_, mod_] := FixedPoint[Union[Flatten[Outer[composition, #, #, 1], 1]] &, group];


g = generateGroup[group, 13];

For[i = Length @ g, i > 0, i--,
	If[MemberQ[g, Mod[((mod - 1) * IdentityMatrix @ 2) . g[[i]], mod]], g = Delete[g, i]];
];

{"Length:", g // Length}
{TableForm @ #} & /@ g



