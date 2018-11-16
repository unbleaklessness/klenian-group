(* ::Package:: *)

BeginPackage @ "GroupGenerator`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

generateGroup::usage = "Generates a group. Takes a list of initial elements (matrices) as the first argument and modulus of the group as second argument.";
groupInfo::usage = "Prints out group elements and number of elements in the group. Takes group (list of matrices) as the first argument.";
generateCayleyTable::usage = "";
(* REMOVE *) cayleyForSecondBasis::usage = "";

Begin @ "`Private`";

multiply[m1_, m2_, mod_] := Mod[m1 . m2, mod];
generate[initials_, mod_] := FixedPoint[Union[Flatten[Outer[multiply[##, mod] &, #, #, 1], 1]] &, initials];
secondIdentity[dimension_, mod_] := (mod - 1) * IdentityMatrix @ dimension;
removeDuplicates[group_, mod_] := Fold[If[MemberQ[#1, multiply[secondIdentity[Length @ #2, mod], #2, mod]], Delete[#1, Position[#1, #2][[1, 1]]], #1] &, group, group];
(*cayleyForFirstBasis[group_, mod_] := Grid[Table[Position[group, Mod[group[[i]] . group[[j]], mod]], {i, Length @ group}, {j, Length @ group}]];*)
(*cayleyForSecondBasis[group_, mod_] := Grid[Table[Position[group, Mod[group[[i]] . group[[j]] . secondIdentity[Length @ group[[1]], mod], mod]], {i, Length @ group}, {j, Length @ group}]];*)

(*uct[s_, e_, i_] := MapIndexed[If[#1 == {}, s[[First @ i, First @ #2]], #1] &, e];
uniteCayleyTables[first_, second_] := #[[1]] & /@ MapIndexed[uct[second, #1, #2] &, first];*)

generateGroup[initials_List, mod_Integer] := removeDuplicates[generate[initials, mod], mod];
(*generateCayleyTable[group_, mod] := uniteCayleyTables[cayleyForFirstBasis[group, mod], cayleyForSecondBasis[group, mod]];*)
groupInfo[group_] := Print[{"Length", Length @ g}, "\n", {TableForm @ #} & /@ group];

End[];
EndPackage[];


Table[i * j, {i, 10}, {j, 10}]


m1 = {{0, 3}, {2, 4}};
m2 = {{0, 1}, {6, 0}};
m3 = {{3, 0}, {0, 5}};
m4 = {{1, 1}, {0, 1}};
group = {m1, m2, m3, m4};


g = generateGroup[group, 7];
groupInfo @ g;
cayleyForSecondBasis[g, 7]
(*c = generateCayleyTable[g, 7];
Print @ c*)


si[d_, m_] := (m - 1) * IdentityMatrix @ d;
r = Mod[g[[1]] . g[[8]] . si[2, 7], 7]
Position[g, r]


tl1 = {{1}, {}, {2}, {3}, {}, {4}};
tl2 = {{}, {4}, {}, {}, {2}, {}};
te[x_, y_] := MapIndexed[If[#1 == {}, tl2[[First @ #2]], #1] &, tl1];
tr[] := te[tl1, tl2];
tr[]


1 == 2
