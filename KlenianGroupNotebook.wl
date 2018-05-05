(* ::Package:: *)

Tr @ bag1[[g1[[1]]]] // FullSimplify
Tr @ bag1[[g1[[2]]]] // FullSimplify


trr[x_] := Map[FullSimplify @ Tr @ bag1[[#]] &, x];
fff[x_] := Map[trr @ # &, x] // Flatten // DeleteDuplicates;
straces = fff @ sgroups;


straces // Length


multiply[{13, 13}, cayley]


pow[13, 2, cayley]
