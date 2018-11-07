(* ::Package:: *)

z = Exp[2 Pi I / 13];
m1 = {
	{z^7, 0, 0, 0, 0, 0},
	{0, z^11, 0, 0, 0, 0},
	{0, 0, z^8, 0, 0, 0},
	{0, 0, 0, z^6, 0, 0},
	{0, 0, 0, 0, z^2, 0},
	{0, 0, 0, 0, 0, z^5}
};
m2 = (-1 / Sqrt[13])*{
	{z^12 - z, z^10 - z^3, z^4 - z^9, z^5 - z^8, z^2 - z^11, z^6 - z^7},
	{z^10 - z^3, z^4 - z^9, z^12 - z, z^2 - z^11, z^6 - z^7, z^5 - z^8},
	{z^4 - z^9, z^12 - z, z^10 - z^3, z^6 - z^7, z^5 - z^8, z^2 - z^11},
	{z^5 - z^8, z^2 - z^11, z^6 - z^7, z - z^12, z^3 - z^10, z^9 - z^4},
	{z^2 - z^11, z^6 - z^7, z^5 - z^8, z^3 - z^10, z^9 - z^4, z - z^12},
	{z^6 - z^7, z^5 - z^8, z^2 - z^11, z^9 - z^4, z - z^12, z^3 - z^10}
};


(*
MatrixPower[m1, 13] \[Rule] Identity
MatrixPower[m2, 2] \[Rule] Identify
*)


(* Powers of matrix. *)
a1 = Map[FullSimplify @ MatrixPower[ComplexExpand @ m1, #1] &, Range @ 14];
MapIndexed[{#2, TableForm @ #1} &, a1]


(* Select identity matrices. *)
a2 = Select[a1, # == IdentityMatrix[6] || # == -IdentityMatrix[6] &];
Map[TableForm @ # &, a2]

(* Number of identify matrices. *)
a3 = {"Length", Length @ a2}



