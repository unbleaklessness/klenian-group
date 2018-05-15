(* ::Package:: *)

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


(* order = findOrder[cayley, neutralElementId] *)


(*
\:0421\:043f\:0435\:043a\:0442\:0440 \:0433\:0440\:0443\:043f\:043f\:044b. \:041a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432 \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0443\:044e\:0449\:0438\:0445 \:043f\:043e\:0440\:044f\:0434\:043a\:043e\:0432.
\:041d\:0430\:0431\:043e\:0440 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432 \:0441 \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0443\:044e\:0449\:0438\:043c \:0434\:043b\:044f \:043d\:0435\:0433\:043e \:043f\:043e\:0440\:044f\:0434\:043a\:043e\:043c.
\:041a\:043e\:043b\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432 \:043f\:043e\:0440\:044f\:0434\:043a\:0430 \:0432 \:0433\:0440\:0443\:043f\:043f\:0435.
*)

order = {};
For[i = 1, i <= Length @ cayley[[1]], i++,
	For[j = 1, j <= Length @ cayley[[1]], j++,
		If[pow[i, j, cayley] == neutralElementId,
			AppendTo[order, {i, j}];
			Break[];
		];
	];
];
order;

orderElements = {};
For[i = 1, i <= Length @ order, i++,
	AppendTo[orderElements, order[[i, 2]]];
];
unique = orderElements // DeleteDuplicates;

orderCount = {};
For[i = 1, i <= Length @ unique, i++,
	AppendTo[orderCount, {}];
	AppendTo[orderCount[[i]], unique[[i]]];
	AppendTo[orderCount[[i]], Count[orderElements, unique[[i]]]];
];
orderCount;


(* \:041d\:0430\:0445\:043e\:0436\:0434\:0435\:043d\:0438\:0435 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432 \:043a\:0430\:043a\:043e\:0433\:043e-\:0442\:043e \:043f\:043e\:0440\:044f\:0434\:043a\:0430 *)
findOrders[x_] := #[[1]] & /@ Select[order, #[[2]] == x &];


order2 = findOrders[2];
order4 = findOrders[4];


(* \:0413\:0435\:043d\:0435\:0440\:0430\:0446\:0438\:044f \:043f\:0430\:0440, \:043f\:043e\:0440\:043e\:0436\:0434\:0430\:044e\:0449\:0438\:0445 D4 \:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f\:0443 *)
fmf[x_] := Select[x, # =!= {} &];
ryr := Flatten[fmf @ # & /@ Table[
If[multiply[{order4[[j]], order2[[i]]}, cayley] == multiply[{order2[[i]], findPair[inverses, order4[[j]]]}, cayley],
	{order2[[i]], order4[[j]]}, {}]
, {i, order2 // Length}
, {j, order4 // Length}], 1];


(* \:0421\:043e\:043f\:0440\:044f\:0436\:0435\:043d\:0438\:0435 \:043f\:0430\:0440, \:043f\:043e\:0440\:043e\:0436\:0434\:0430\:044e\:0449\:0438\:0445 D4 \:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f\:0443 *)
rer[x_] := Sort[{multiply[{findPair[inverses, #], x[[1]], #}, cayley], multiply[{findPair[inverses, #], x[[2]], #}, cayley]}] & /@ Range @ 168;
rur = Flatten[rer @ # & /@ ryr, 1] // DeleteDuplicates;


(* \:041d\:0430\:0445\:043e\:0436\:0434\:0435\:043d\:0438\:0435 \:0441\:0438\:043b\:043e\:0432\:0441\:043a\:043e\:0439 2-\:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f\:044b *)
olo[x_, n_] := FoldList[multiply[{#1, #2}, cayley] &, x, Table[x, n - 1]];
ala[x_, y_, n_] := multiply[{y, #}, cayley] & /@ olo[x, n];
wew[x_, y_, n_] := Join[olo[x, n], ala[x, y, n]] // Sort;
wewa[l_, p_] := wew[#[[2]], #[[1]], p] & /@ l // DeleteDuplicates;


sylov2 = wewa[ryr, 4] (* \:0421\:0438\:043b\:043e\:0432\:0441\:043a\:0438\:0435 2-\:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f\:044b *)


(* \:041d\:0430\:0445\:043e\:0436\:0434\:0435\:043d\:0438\:0435 2 \:0438 3 \:0441\:0438\:043b\:043e\:0432\:0441\:043a\:0438\:0445 \:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f *)
uuu[x_] := pow[x, #, cayley] & /@ Range @ findPair[order, x];
uru[x_, y_] := multiply[{findPair[inverses, x], #, x}, cayley] & /@ uuu[y] // Sort // DeleteDuplicates;
utu[x_] := uru[#, x] & /@ Range @ 168 // DeleteDuplicates;


sylov7 = utu[1] (* \:0421\:0438\:043b\:043e\:0432\:0441\:043a\:0438\:0435 7-\:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f\:044b *)


sylov3 = utu[5] (* \:0421\:0438\:043b\:043e\:0432\:0441\:043a\:0438\:0435 3-\:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f\:044b *)


(*
\:0413\:0440\:0443\:043f\:043f\:044b \:0441\:043e\:043f\:0440\:044f\:0436\:0435\:043d\:043d\:044b\:0445 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432.
*)

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


(* \:0414\:043e\:0431\:0430\:0432\:0438\:0442\:044c \:0441\:043b\:0435\:0434\:044b \:0434\:043b\:044f \:043a\:043b\:0430\:0441\:0441\:043e\:0432 \:0441\:043e\:043f\:0440\:044f\:0436\:0435\:043d\:043d\:044b\:0445 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432 *)
trace[x_] := Tr @ bag1[[x]] // FullSimplify;
sgroups = Append[#, trace @ #[[2, 1]]] & /@ sgroups;


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
		],
		ItemSize -> {2.3, 1.3},
		Frame -> True,
		ItemStyle -> Directive[FontSize -> 16],
		Spacings -> {1, 1}
	],
	TabView @ {
		"\:041e\:0431\:0440\:0430\:0442\:043d\:044b\:0435 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:044b" -> Style[inverses, FontSize -> 16],
		"\:041a\:043e\:043c\:043c\:0443\:0442\:0438\:0440\:0443\:044e\:0449\:0438\:0435 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:044b" -> Style[commutatives, FontSize -> 16],
		"\:0426\:0435\:043d\:0442\:0440 \:0433\:0440\:0443\:043f\:043f\:044b" -> Style[center, 50],
		"\:041f\:043e\:0440\:044f\:0434\:043a\:0438 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432" -> Grid[{{
			Grid[Prepend[order, Style[#, FontWeight -> Bold, FontSize -> 23] & /@ {"\:042d\:043b\:0435\:043c\:0435\:043d\:0442", "\:041f\:043e\:0440\:044f\:0434\:043e\:043a"}],
				Frame -> All,
				ItemStyle -> Directive[FontSize -> 16],
				FrameStyle -> Directive[Gray],
				Spacings -> {1.5, 1.5}],
			Grid[Prepend[orderCount, Style[#, FontWeight -> Bold, FontSize -> 23] & /@ {"\:041f\:043e\:0440\:044f\:0434\:043e\:043a", " \:041a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432 \:043f\:043e\:0440\:044f\:0434\:043a\:0430"}],
				Frame -> All,
				ItemStyle -> Directive[FontSize -> 16],
				FrameStyle -> Directive[Gray],
				Spacings -> {1.5, 1.5}]
		}}, Alignment -> Top],
		"\:041a\:043b\:0430\:0441\:0441\:044b \:0441\:043e\:043f\:0440\:044f\:0436\:0435\:043d\:043d\:044b\:0445 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432" -> Grid[Prepend[sgroups, Style[#, FontWeight -> Bold, FontSize -> 23] & /@ {"\:041f\:043e\:0440\:044f\:0434\:043e\:043a \:043a\:043b\:0430\:0441\:0441\:0430", "\:041a\:043b\:0430\:0441\:0441\:044b", "\:0421\:043b\:0435\:0434 \:043a\:043b\:0430\:0441\:0441\:0430"}],
			Frame -> All,
		    ItemStyle -> Directive[FontSize -> 16],
		    FrameStyle -> Directive[Gray],
		    Spacings -> {1.5, 1.5}],
		"\:0421\:0438\:043b\:043e\:0432\:0441\:043a\:0438\:0435 \:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f\:044b" -> Grid[{{
		Grid[Prepend[{#} & /@ sylov2, {"\:0421\:0438\:043b\:043e\:0432\:0441\:043a\:0438\:0435 2-\:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f\:044b"}],
			Frame -> All,
		    ItemStyle -> Directive[FontSize -> 16],
		    FrameStyle -> Directive[Gray],
		    Spacings -> {1.5, 1.5}],
		Grid[Prepend[{#} & /@ sylov3, {"\:0421\:0438\:043b\:043e\:0432\:0441\:043a\:0438\:0435 3-\:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f\:044b"}],
			Frame -> All,
		    ItemStyle -> Directive[FontSize -> 16],
		    FrameStyle -> Directive[Gray],
		    Spacings -> {1.5, 1.5}],
		Grid[Prepend[{#} & /@ sylov7, {"\:0421\:0438\:043b\:043e\:0432\:0441\:043a\:0438\:0435 7-\:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f\:044b"}],
			Frame -> All,
		    ItemStyle -> Directive[FontSize -> 16],
		    FrameStyle -> Directive[Gray],
		    Spacings -> {1.5, 1.5}],
		Grid[{{"\:041a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e 2-\:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f", Length @ sylov2}, {"\:041a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e 3-\:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f", Length @ sylov3}, {"\:041a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e 7-\:043f\:043e\:0434\:0433\:0440\:0443\:043f\:043f", Length @ sylov7}}, 
			Frame -> All,
		    ItemStyle -> Directive[FontSize -> 16],
		    FrameStyle -> Directive[Gray],
		    Spacings -> {1.5, 1.5}]
		}}, Alignment -> Top]
	}
};
