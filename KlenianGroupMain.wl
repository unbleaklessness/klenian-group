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


order = findOrder[cayley, neutralElementId]


(*
\:0421\:043f\:0435\:043a\:0442\:0440 \:0433\:0440\:0443\:043f\:043f\:044b. \:041a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432 \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0443\:044e\:0449\:0438\:0445 \:043f\:043e\:0440\:044f\:0434\:043a\:043e\:0432.
\:041d\:0430\:0431\:043e\:0440 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432 \:0441 \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0443\:044e\:0449\:0438\:043c \:0434\:043b\:044f \:043d\:0435\:0433\:043e \:043f\:043e\:0440\:044f\:0434\:043a\:043e\:043c.
\:041a\:043e\:043b\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432 \:043f\:043e\:0440\:044f\:0434\:043a\:0430 \:0432 \:0433\:0440\:0443\:043f\:043f\:0435...
*)
result = {};
For[i = 1, i <= Length @ cayley[[1]], i++,
	For[j = 1, j <= Length @ cayley[[1]], j++,
		If[pow[i, j, cayley] == neutralElementId,
			AppendTo[result, {i, j}];
			Break[];
		];
	];
];
result
result2 = {};
For[i = 1, i <= Length @ result, i++,
	AppendTo[result2, result[[i, 2]]];
];
result2;
unique = result2 // DeleteDuplicates;
count = {};
For[i = 1, i <= Length @ unique, i++,
	AppendTo[count, {}];
	AppendTo[count[[i]], unique[[i]]];
	AppendTo[count[[i]], Count[result2, unique[[i]]]];
];
count


(*
\:0421\:0442\:0440\:0443\:043a\:0442\:0443\:0440\:0430 \:0411\:0438\:043e\:0432\:0438\:043b\:043b\:044f.
*)
pairs = {{1, 2}};
(*For[i = 1, i \[LessEqual] Length @ cayley[[1]], i++,
	If[i \[Equal] 168, Break[]];
	AppendTo[pairs, {cayley[[1, i]], cayley[[1, i + 1]]}];
];
pairs*)
rows = {};
For[i = 1, i <= Length @ pairs, i++,
	AppendTo[rows, {}];
	AppendTo[rows[[i]], pairs[[i]]];
	AppendTo[rows[[i]], {}];
	For[j = 1, j <= Length @ cayley[[1]], j++,
		For[k = 1, k <= 168, k++,
			AppendTo[rows[[i, 2]],
				 {multiply[{j, pow[pairs[[i, 1]], k, cayley], findPair[cayley, j]}, cayley],
				 multiply[{j, pow[pairs[[i, 2]], k, cayley], findPair[cayley, j]}, cayley],
				 multiply[{j, pow[multiply[{pairs[[i, 1]], pairs[[i, 2]]}, cayley], k, cayley], findPair[cayley, j]}, cayley]}];
		];
	];
	rows[[i, 2]] = DeleteDuplicates @ rows[[i, 2]];
];
rows


(*
\:0421\:0442\:0440\:0443\:043a\:0442\:0443\:0440\:0430 \:0411\:0438\:043e\:0432\:0438\:043b\:043b\:044f.
*)
pairs2 = {{3, 4}};
(*For[i = 1, i \[LessEqual] Length @ cayley[[1]], i++,
	If[i \[Equal] 168, Break[]];
	AppendTo[pairs2, {cayley[[1, i]], cayley[[1, i + 1]]}];
];
pairs2*)
rows2 = {};
For[i = 1, i <= Length @ pairs2, i++,
	AppendTo[rows2, {}];
	AppendTo[rows2[[i]], pairs2[[i]]];
	AppendTo[rows2[[i]], {}];
	For[j = 1, j <= Length @ cayley[[1]], j++,
		For[k = 1, k <= 168, k++,
			AppendTo[rows2[[i, 2]],
				 {multiply[{j, pow[pairs2[[i, 1]], k, cayley], findPair[cayley, j]}, cayley],
				 multiply[{j, pow[pairs2[[i, 2]], k, cayley], findPair[cayley, j]}, cayley],
				 multiply[{j, pow[multiply[{pairs2[[i, 1]], pairs2[[i, 2]]}, cayley], k, cayley], findPair[cayley, j]}, cayley]}];
		];
	];
	rows2[[i, 2]] = DeleteDuplicates @ rows2[[i, 2]];
];
rows2


(*
\:0421\:0442\:0440\:0443\:043a\:0442\:0443\:0440\:0430 \:0411\:0438\:043e\:0432\:0438\:043b\:043b\:044f.
*)
pairs3 = {{1, 3}};
(*For[i = 1, i \[LessEqual] Length @ cayley[[1]], i++,
	If[i \[Equal] 168, Break[]];
	AppendTo[pairs3, {cayley[[1, i]], cayley[[1, i + 1]]}];
];
pairs3*)
rows3 = {};
For[i = 1, i <= Length @ pairs3, i++,
	AppendTo[rows3, {}];
	AppendTo[rows3[[i]], pairs3[[i]]];
	AppendTo[rows3[[i]], {}];
	For[j = 1, j <= Length @ cayley[[1]], j++,
		For[k = 1, k <= 168, k++,
			AppendTo[rows3[[i, 2]],
				 {multiply[{j, pow[pairs3[[i, 1]], k, cayley], findPair[cayley, j]}, cayley],
				 multiply[{j, pow[pairs3[[i, 2]], k, cayley], findPair[cayley, j]}, cayley],
				 multiply[{j, pow[multiply[{pairs3[[i, 1]], pairs3[[i, 2]]}, cayley], k, cayley], findPair[cayley, j]}, cayley]}];
		];
	];
	rows3[[i, 2]] = DeleteDuplicates @ rows3[[i, 2]];
];
rows3


(*
\:0421\:0442\:0440\:0443\:043a\:0442\:0443\:0440\:0430 \:0411\:0438\:043e\:0432\:0438\:043b\:043b\:044f.
*)
pairs4 = {{1, 4}};
(*For[i = 1, i \[LessEqual] Length @ cayley[[1]], i++,
	If[i \[Equal] 168, Break[]];
	AppendTo[pairs4, {cayley[[1, i]], cayley[[1, i + 1]]}];
];
pairs4*)
rows4 = {};
For[i = 1, i <= Length @ pairs4, i++,
	AppendTo[rows4, {}];
	AppendTo[rows4[[i]], pairs4[[i]]];
	AppendTo[rows4[[i]], {}];
	For[j = 1, j <= Length @ cayley[[1]], j++,
		For[k = 1, k <= 168, k++,
			AppendTo[rows4[[i, 2]],
				 {multiply[{j, pow[pairs4[[i, 1]], k, cayley], findPair[cayley, j]}, cayley],
				 multiply[{j, pow[pairs4[[i, 2]], k, cayley], findPair[cayley, j]}, cayley],
				 multiply[{j, pow[multiply[{pairs4[[i, 1]], pairs4[[i, 2]]}, cayley], k, cayley], findPair[cayley, j]}, cayley]}];
		];
	];
	rows4[[i, 2]] = DeleteDuplicates @ rows4[[i, 2]];
];
rows4


(*
\:0421\:0442\:0440\:0443\:043a\:0442\:0443\:0440\:0430 \:0411\:0438\:043e\:0432\:0438\:043b\:043b\:044f.
*)
pairs5 = {{1, 5}};
(*For[i = 1, i \[LessEqual] Length @ cayley[[1]], i++,
	If[i \[Equal] 168, Break[]];
	AppendTo[pairs5, {cayley[[1, i]], cayley[[1, i + 1]]}];
];
pairs5*)
rows5 = {};
For[i = 1, i <= Length @ pairs5, i++,
	AppendTo[rows5, {}];
	AppendTo[rows5[[i]], pairs5[[i]]];
	AppendTo[rows5[[i]], {}];
	For[j = 1, j <= Length @ cayley[[1]], j++,
		For[k = 1, k <= 168, k++,
			AppendTo[rows5[[i, 2]],
				 {multiply[{j, pow[pairs5[[i, 1]], k, cayley], findPair[cayley, j]}, cayley],
				 multiply[{j, pow[pairs5[[i, 2]], k, cayley], findPair[cayley, j]}, cayley],
				 multiply[{j, pow[multiply[{pairs5[[i, 1]], pairs5[[i, 2]]}, cayley], k, cayley], findPair[cayley, j]}, cayley]}];
		];
	];
	rows5[[i, 2]] = DeleteDuplicates @ rows5[[i, 2]];
];
rows5


rowsSet = rows[[1, 2]]
rows2Set = rows2[[1, 2]]
rows3Set = rows3[[1, 2]]
rows4Set = rows4[[1, 2]]
rows5Set = rows5[[1, 2]]


rows12SetInter = Intersection[rowsSet, rows2Set]


rows23SetInter = Intersection[rows2Set, rows3Set]


rows13SetInter = Intersection[rowsSet, rows3Set]


rows41SetInter = Intersection[rows4Set, rowsSet]


rows42SetInter = Intersection[rows4Set, rows2Set]


rows43SetInter = Intersection[rows4Set, rows3Set]


rows15SetInter = Intersection[rowsSet, rows5Set]


rows25SetInter = Intersection[rows2Set, rows5Set]


rows35SetInter = Intersection[rows3Set, rows5Set]


rows45SetInter = Intersection[rows4Set, rows5Set]


tempInter = Intersection[rows45SetInter, rows35SetInter]


rowsSetInter // Length


X = Range @ 168;

w[t_] := multiply[{findPair[inverses, #], t, #}, cayley] & /@ Range @ 168 // DeleteDuplicates;

g1 = w[1];
g1l = g1 // Length;

Y = X;
X = Complement[Y, g1];

g2 = w[X[[1]]];
g2l = g2 // Length;

Y = X;
X = Complement[Y, g2];

g3 = w[X[[2]]];
g3l = g3 // Length;

Y = X;
X = Complement[Y, g3];

g4 = w[X[[2]]];
g4l = g4 // Length;

Y = X;
X = Complement[Y, g4];

g5 = w[X[[2]]];
g5l = g5 // Length;

g6 = {6};
g6l = g6 // Length;

sgroups = {{g1l, g1}, {g2l, g2}, {g3l, g3}, {g4l, g4}, {g5l, g5}, {g6l, g6}};


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
		], ItemSize -> {2.3, 1.3},
		   Frame -> True,
		   ItemStyle -> Directive[FontSize -> 16],
		   Spacings -> {1, 1}
	],
	TabView @ {
		"\:041e\:0431\:0440\:0430\:0442\:043d\:044b\:0435 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:044b" -> Style[inverses, FontSize -> 16],
		"\:041a\:043e\:043c\:043c\:0443\:0442\:0438\:0440\:0443\:044e\:0449\:0438\:0435 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:044b" -> Style[commutatives, FontSize -> 16],
		"\:0426\:0435\:043d\:0442\:0440 \:0433\:0440\:0443\:043f\:043f\:044b" -> Style[center, 50],
		"\:041f\:043e\:0440\:044f\:0434\:043a\:0438 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432" -> Grid[Prepend[order, Style[#, FontWeight -> Bold, FontSize -> 23] & /@ {"\:041f\:043e\:0440\:044f\:0434\:043e\:043a", "\:041a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e", "\:042d\:043b\:0435\:043c\:0435\:043d\:0442\:044b"}],
						          Frame -> All,
						          ItemStyle -> Directive[FontSize -> 16],
						          FrameStyle -> Directive[Gray],
						          Spacings -> {1.5, 1.5}],
		"\:041a\:043b\:0430\:0441\:0441\:044b \:0441\:043e\:043f\:0440\:044f\:0436\:0435\:043d\:043d\:044b\:0445 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432" -> Grid[Prepend[sgroups, Style[#, FontWeight -> Bold, FontSize -> 23] & /@ {"\:041f\:043e\:0440\:044f\:0434\:043e\:043a \:043a\:043b\:0430\:0441\:0441\:043e\:0432", "\:041a\:043b\:0430\:0441\:0441\:044b"}],
		                                     Frame -> All,
		                                     ItemStyle -> Directive[FontSize -> 16],
		                                     FrameStyle -> Directive[Gray],
		                                     Spacings -> {1.5, 1.5}]
	}
};


Tr @ bag1[[g1[[1]]]] // FullSimplify
Tr @ bag1[[g1[[2]]]] // FullSimplify


trr[x_] := Map[FullSimplify @ Tr @ bag1[[#]] &, x];
fff[x_] := Map[trr @ # &, x] // Flatten // DeleteDuplicates;
straces = fff @ sgroups;


straces // Length


multiply[{13, 13}, cayley]


pow[13, 2, cayley]
