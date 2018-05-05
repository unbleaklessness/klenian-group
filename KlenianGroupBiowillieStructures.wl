(* ::Package:: *)

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
