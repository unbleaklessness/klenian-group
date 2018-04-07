(* ::Package:: *)

(*
\:041f\:0440\:043e\:0433\:0440\:0430\:043c\:043c\:043c\:044b \:043e\:043f\:0438\:0441\:044b\:0432\:0430\:044e\:0442 \:0433\:0440\:0443\:043f\:043f\:0443 \:0432 \:0441\:043c\:044b\:0441\:043b\:0435 \:043f\:043e\:043b\:043d\:043e\:0433\:043e \:043f\:0435\:0440\:0435\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:044f \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432 \:0433\:0440\:0443\:043f\:043f\:044b, \:0441\:043e\:0437\:0434\:0430\:043d\:0438\:0435 \:0442\:0430\:0431\:043b\:0438\:0446\:044b \:041a\:0435\:043b\:0438 \:0434\:043b\:044f \:043d\:0435\:0451.
\:041d\:0430\:0445\:043e\:0436\:0434\:0435\:043d\:0438\:0435 \:043e\:0431\:0440\:0430\:0442\:043d\:044b\:0445 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432, \:043d\:0430\:0445\:043e\:0436\:0434\:0435\:043d\:0438\:044f \:0441\:043e\:043f\:0440\:044f\:0436\:0435\:043d\:043d\:044b\:0445 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432, \:043a\:043e\:043c\:043c\:0443\:0442\:0430\:0442\:043e\:0440\:043e\:0432. \:041d\:0430 \:043e\:0441\:043d\:043e\:0432\:0430\:043d\:0438\:0438 \:044d\:0442\:043e\:0433\:043e \:043d\:0430\:0445\:043e\:0436\:0434\:0435\:043d\:0438\:0435 \:043d\:043e\:0440\:043c\:0430\:043b\:044c\:043d\:043e\:0433\:043e \:0437\:0430\:043c\:044b\:043a\:0430\:043d\:0438\:044f, \:043d\:043e\:0440\:043c\:0430\:043b\:0438\:0437\:0430\:0442\:043e\:0440\:043e\:0432, \:0446\:0435\:043d\:0442\:0440\:0430\:043b\:0438\:0437\:0430\:0442\:043e\:0440\:043e\:0432,
\:043a\:043e\:043c\:043c\:0443\:0442\:0430\:043d\:0442\:0430, \:043f\:0440\:0438\:043c\:0435\:043d\:0435\:043d\:0438\:044f \:0434\:0430\:043d\:043d\:044b\:0445 \:043f\:0440\:043e\:0446\:0435\:0434\:0443\:0440 \:043a \:0433\:0440\:0443\:043f\:043f\:0430\:043c \:0414\:0438\:044d\:0434\:0440\:0430 \:0438 \:0433\:0440\:0443\:043f\:043f\:0435 \:041a\:043b\:0435\:0439\:043d\:0430 \:0438 \:043f\:0440\:043e\:0435\:043a\:0442\:0438\:0432\:043d\:044b\:043c \:043b\:0438\:043d\:0435\:0439\:043d\:044b\:043c \:0433\:0440\:0443\:043f\:043f\:0430\:043c PSL(2, 7) \:043a\:043e\:0442\:043e\:0440\:044b\:0435 \:043f\:043e\:044f\:0432\:043b\:044f\:044e\:0442\:0441\:044f \:043f\:0440\:0438 \:0438\:0441\:0441\:043b\:0435\:0434\:043e\:0432\:0430\:043d\:0438\:0438 \:0441\:0438\:043c\:043c\:0435\:0442\:0440\:0438\:0438 \:043a\:0432\:0430\:0440\:0442\:0438\:043a\:0438 \:041a\:043b\:0435\:0439\:043d\:0430.
*)


If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "KlenianGroupCayleyTable`";
Needs @ "KlenianGroupHelpers`";
Needs @ "KlenianGroupFindCommutative`";
Needs @ "KlenianGroupFindInverses`";
Needs @ "KlenianGroupFindCenter`";
Needs @ "KlenianGroupFindNormalizer`";
Needs @ "KlenianGroupFindOrder`";
Needs @ "KlenianGroupMultiply`";

Needs @ "KlenianGroupFindSubgroup`";

generated = getCayleyTableAndBag[];
cayley = generated[[1]];
bag = generated[[2]];
bag1 = generated[[3]];


part = Table[cayley[[i, j]], {i, 168}, {j, 168}];
partWithTitles = addTitles @ part;


(* The only commutative element is an unitray matrix. *)
neutralElement = Ceiling @ Select[bag, And @@ Table[bag[[i]] . # == # . bag[[i]], {i, Length @ bag}] &][[1]];
neutralElementId = Position[Ceiling @ bag, neutralElement][[1, 1]];


inverses = findInverses[cayley, neutralElementId];


center = findCenter @ cayley;


normalizer = findNormalizer[cayley, inverses];


commutatives = findCommutatives @ cayley;


CreateDocument @ {
	TextCell["\:0413\:0440\:0443\:043f\:043f\:0430 \:041a\:043b\:0435\:0439\:043d\:0430", "Title"],
	Grid[
		Table[
			If[i == 1,
				Item[partWithTitles[[i, j]], Background -> LightBlue, Frame -> {True, False, False, False}]
			,
				If[j == 1,
					Item[partWithTitles[[i, j]], Background -> LightBlue, Frame -> {False, False, False, True}]
				,
					partWithTitles[[i, j]]	
				]
			]
		,
			{i, Length @ partWithTitles}
		,
			{j, Length @ partWithTitles}
		], ItemSize -> {2.3, 1.3}, Frame -> True
	],
	TabView @ {
		"\:041e\:0431\:0440\:0430\:0442\:043d\:044b\:0435 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:044b" -> inverses,
		"\:041a\:043e\:043c\:043c\:0443\:0442\:0438\:0440\:0443\:044e\:0449\:0438\:0435 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:044b" -> commutatives,
		"\:0426\:0435\:043d\:0442\:0440 \:0433\:0440\:0443\:043f\:043f\:044b" -> center
	}
};


order = findOrder[cayley, neutralElementId];


X = Range @ 168;

w[t_] := multiply[{findPair[inverses, #], t, #}, cayley] & /@ Range @ 168 // DeleteDuplicates;

g1 = w[1]
g1 // Length

Y = X;
X = Complement[Y, g1];

g2 = w[X[[1]]]
g2 // Length

Y = X;
X = Complement[Y, g2];

g3 = w[X[[2]]]
g3 // Length

Y = X;
X = Complement[Y, g3];

g4 = w[X[[2]]]
g4 // Length

Y = X;
X = Complement[Y, g4];

g5 = w[X[[2]]]
g5 // Length

g6 = {6}
g6 // Length

sgroups = {g1, g2, g3, g4, g5, g6};


Tr @ bag1[[g1[[1]]]] // FullSimplify
Tr @ bag1[[g1[[2]]]] // FullSimplify


trr[x_] := Map[FullSimplify @ Tr @ bag1[[#]] &, x];
fff[x_] := Map[trr @ # &, x] // Flatten // DeleteDuplicates;
straces = fff @ sgroups;


straces // Length


sylovGroups[x_] := Select[x, PrimeQ[168 / Length[#]] &];
sylovGroups @ sgroups
