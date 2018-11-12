(* ::Package:: *)

BeginPackage @ "GroupGenerator`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

generateGroup::usage = "Generates a group. Takes a list of initial elements (matrices) as the first argument and modulus of the group as second argument.";
groupInfo::usage = "Prints out group elements and number of elements in the group. Takes group (list of matrices) as the first argument.";

Begin @ "`Private`";

secondIdentity[dimension_, mod_] := (mod - 1) * IdentityMatrix @ dimension;
multiply[m1_, m2_, mod_] := Mod[m1 . m2, mod];
generate[initial_, mod_] := FixedPoint[Union[Flatten[Outer[multiply[##, mod] &, #, #, 1], 1]] &, initial];
removeDuplicates[group_, mod_] := Fold[If[MemberQ[#1, multiply[secondIdentity[Length @ #2, mod], #2, mod]], Delete[#1, Position[#1, #2][[1, 1]]], #1] &, group, group];
	
generateGroup[initial_List, mod_Integer] := removeDuplicates[generate[initial, mod], mod];
groupInfo[group_] := Print[{"Length", Length @ g}, "\n", {TableForm @ #} & /@ group];

End[];
EndPackage[];


m1 = {{0, 3}, {2, 4}};
m2 = {{0, 1}, {6, 0}};
m3 = {{3, 0}, {0, 5}};
m4 = {{1, 1}, {0, 1}};
group = {m1, m2, m3, m4};


g = generateGroup[group, 7];
groupInfo @ g;
