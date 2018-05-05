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
order

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
orderCount


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
			Grid[Prepend[order, Style[#, FontWeight -> Bold, FontSize -> 23] & /@ {"\:041f\:043e\:0440\:044f\:0434\:043e\:043a", "\:042d\:043b\:0435\:043c\:0435\:043d\:0442"}],
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
		"\:041a\:043b\:0430\:0441\:0441\:044b \:0441\:043e\:043f\:0440\:044f\:0436\:0435\:043d\:043d\:044b\:0445 \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:043e\:0432" -> Grid[Prepend[sgroups, Style[#, FontWeight -> Bold, FontSize -> 23] & /@ {"\:041f\:043e\:0440\:044f\:0434\:043e\:043a \:043a\:043b\:0430\:0441\:0441\:0430", "\:041a\:043b\:0430\:0441\:0441\:044b"}],
			Frame -> All,
		    ItemStyle -> Directive[FontSize -> 16],
		    FrameStyle -> Directive[Gray],
		    Spacings -> {1.5, 1.5}]
	}
};
