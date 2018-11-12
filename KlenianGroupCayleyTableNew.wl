(* ::Package:: *)

BeginPackage @ "KlenianGroupCayleyTableNew`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

getCayleyTableAndBagNew::usage = "";

Begin @ "`Private`";

c1[m1_, m2_, mod_] := Mod[m1 . m2, mod];
c2[m_, mod_] := If[m == (mod - 1) * IdentityMatrix[Length @ m], IdentityMatrix[Length @ m], m];
composition[m1_, m2_, mod_] := c2[c1[m1, m2, mod], mod];
generateGroup[group_, mod_] := FixedPoint[Union[Flatten[Outer[composition[##, mod] &, #, #, 1], 1]] &, group];
	
getCayleyTableAndBagNew[] :=
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
	mod = 7;
		
	g = generateGroup[group, mod];

	For[i = Length @ g, i > 0, i--,
		If[MemberQ[g, Mod[((mod - 1) * IdentityMatrix @ Length @ g[[1]]) . g[[i]], mod]], g = Delete[g, i]];
	];
	
	{g, N[g, 30], g}
];

End[];
EndPackage[];
