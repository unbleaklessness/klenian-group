(* ::Package:: *)

m1 = {
	{2, 0},
	{0, 7}
};
m3 = {
	{1, 1},
	{0, 1}
};


i1 = (12 * IdentityMatrix @ 2)
i2 = IdentityMatrix @ 2
Mod[m1 . i1, 13]


c1 = MatrixPower[m1, 6];
Mod[c1, 13] // TableForm
c1 // TableForm


a1 = Mod[MatrixPower[m1, #], 13] & /@ Range @ 26;
a2 = If[# == 12 * IdentityMatrix @ 2, IdentityMatrix @ 2, #] & /@ a1;
i = 1; {i++, TableForm @ #} & /@ a1
i = 1; {i++, TableForm @ #} & /@ a2


a2 = Select[a1, # == IdentityMatrix[2] || # == -IdentityMatrix[2] &];
TableForm @ # & /@ a2
Print @ {"Length:", a2 // Length}


b1 = Mod[m1 . m3 . m1. m3 . m1 . m3 . m1 . m3, 13]
b2 = m1 . m3


12 * IdentityMatrix[2]


168 * 2
