(* ::Package:: *)

BeginPackage["YTYGraphTransforms`"]


YTYGraphTransforms::usage = "YTYGraphTransforms: Contains functions for performing Triangle-Wye and Wye-Triangle transforms on simple undirected graphs.
Functions include: TriangleWye, TriangleWyeChildren, TriangleWyeFamily, WyeTriangle, WyeTriangleParents, WyeTriangleFamily, WyeTriangleWyeFamily.";


TriangleWye::usage = "TriangleWye[g, {a,b,c}] performs a Triangle-Wye transform on the triangle given by vertices {a,b,c} in graph g \
by deleting any edges among the vertices {a,b,c} and adding a new vertex incident to each of {a,b,c}.";

TriangleWyeChildren::usage = "TriangleWyeChildren[g] returns every graph (distinct under isomorphism) \
that can be created by performing a Triangle-Wye transform on some triangle in g. \
Relies on function TriangleWye.";

TriangleWyeDescendants::usage = "TriangleWyeDescendants[g] returns a nested list of all (not necessarily distinct) graphs \
that can be created by performing a sequence of Triangle-Wye transforms on g. \
The nested lists keep track of the family heritage and look like {graph, TriangleWyeDescendants /@ TriangleWyeChildren[graph]}. \
Relies on function TriangleWyeChildren.

To get a flat list of the distinct Triangle-Wye Descendants of g, use DeleteDuplicates[Flatten[TriangleWyeDescendants[g]], IsomorphicGraphQ[#1, #2] &].";



WyeTriangle::usage ="WyeTriangle[g, v] performs a Wye-Triangle transform on vertex v in graph g \
by deleting v and forming a 3-cycle among the neighbors of v. \
This function fails if VertexDegree[g,v] != 3.";

WyeTriangleParents::usage = "WyeTriangleParents[g] returns every graph (distinct under isomorphism) \
that can be created by performing a Wye-Triangle transform on some degree 3 vertex in g. \
Relies on function WyeTriangle.
WyeTriangleParents[g, True] does the same thing using a stricter definition of a Wye-Triangle transform \
where the neighbors of the vertex to be deleted must not be neighbors of each other.";

WyeTriangleAncestors::usage = "WyeTriangleAncestors[g] returns a nested list of all (not necessarily distinct) graphs \
that can be created by performing a sequence of Wye-Triangle transforms on g. \
The nested lists keep track of the family heritage and look like {graph, WyeTriangleAncestors /@ WyeTriangleParents[graph]}. \
Relies on function WyeTriangleParents.
WyeTriangleAncestors[g, True] does the same thing using a stricter definition of a Wye-Triangle transform \
where the neighbors of the vertex to be deleted must not be neighbors of each other.

To get a flat list of the distinct Wye-Triangle Ancestors of g, use DeleteDuplicates[Flatten[WyeTriangleAncestors[g]], IsomorphicGraphQ[#1, #2] &].";



WyeTriangleWyeFamily::usage = "WyeTriangleWyeFamily[g] returns a flattened list of all distinct graphs \
that can be created by performing a sequence of Triangle-Wye and Wye-Triangle transforms on g, where g is either a single graph or a list graphs. \
Relies on functions WyeTriangleParents and TriangleWyeChildren.
WyeTriangleWyeFamily[g, Limit, ProgressFlag, LogFlag, Strict, Condition] returns a flattened list of all distinct graphs \
that can be created by performing a sequence of no more than Limit Triangle-Wye and Wye-Triangle transforms on g. \
Set ProgressFlag to True to print some output to indicate the progress of the algorithm. \
In this output, G is the starting set of graphs, and each row contains the list of (the sizes of) \
sets of graphs produced with the same edge count. \
Set LogFlag to True to produce a simple log file containing the edgelists of the graphs produced. \
Set Strict  to True to use a stricter definition of a Wye-Triangle transform \
where the neighbors of the vertex to be deleted must not be neighbors of each other. \
Condition should be a function that takes a single graph object and returns True or False. \
If Condition is defined, all graphs (except those of g) that don't \
satisfy Condition will be immediately discarded from consideration.";
WyeTriangleWyeFamily::badG = "A Graph object or Flattened List of Graph objects was expected.";
WyeTriangleWyeFamily::badCondition = "A function that takes a graph and returns True or False was expected.";
WyeTriangleWyeFamily::badProgressFlag = "A boolean value was expected.";
WyeTriangleWyeFamily::badLoggFlag = "A boolean value was expected.";
WyeTriangleWyeFamily::badStrictFlag = "A boolean value was expected.";



Begin["`Private`"]


DeleteGraphDuplicates[GL_List] := Module[{},
  Return[DeleteDuplicates[GL, IsomorphicGraphQ[#1, #2] &]];
];



TriangleWye[G_Graph, Triangle_List] := Module[{v, deledges, addedges},
  If[Length[Triangle] != 3, Return[$Failed];];

  v = Min[Complement[Range[VertexCount[G] + 1], VertexList[G]]];
  deledges = Intersection[Flatten[Table[{UndirectedEdge[e[[1]], e[[2]]], UndirectedEdge[e[[2]], e[[1]]]}, {e, Subsets[Triangle, {2}]}], 1], EdgeList[G]];
  If[Length[deledges] != 3, Return[$Failed];];
  addedges = UndirectedEdge[Min[v, #], Max[v, #]] & /@ Triangle;
  Return[EdgeAdd[VertexAdd[EdgeDelete[G, deledges], v],addedges]];
];



TriangleWyeChildren[G_Graph] := Module[  {triangles = {}},
  triangles = Union[Sort /@ Flatten[Table[List[e[[1]], e[[2]], #] & /@ Intersection[AdjacencyList[G, e[[1]]], AdjacencyList[G, e[[2]]]], {e, EdgeList[G]}], 1]];
  Return[DeleteGraphDuplicates[Table[TriangleWye[G, t], {t, triangles}]]];
];



TriangleWyeDescendants[G_Graph] := Module[{},
  Return[List[G, TriangleWyeDescendants /@ TriangleWyeChildren[G]]];
];



WyeTriangle[G_Graph, V_] := Module[{addedges = {}, edges = EdgeList[G]},
  If[\[Not] MemberQ[VertexList[G], V] \[Or] VertexDegree[G, V] != 3, Return[$Failed];];

  addedges = Table[UndirectedEdge[Min[e[[1]], e[[2]]], Max[e[[1]], e[[2]]]], {e, Subsets[AdjacencyList[G, V], {2}]}];
  addedges = DeleteCases[addedges, e_ /; Intersection[{e, UndirectedEdge[e[[2]], e[[1]]]}, EdgeList[G]] != {}];
  Return[EdgeAdd[VertexDelete[G, V], addedges]];
];



WyeTriangleParents[G_Graph, Strict_Symbol: False] := Module[  {wyes = {}},
  wyes = Cases[VertexList[G], v_ /; VertexDegree[G, v] == 3];
  If[Strict, wyes = DeleteCases[wyes, v_ /; Intersection[UndirectedEdge[#[[1]], #[[2]]] & /@ Permutations[AdjacencyList[G, v], {2}], EdgeList[G]] != {}]];
  Return[DeleteGraphDuplicates[Table[WyeTriangle[G, v], {v, wyes}]]];
];



WyeTriangleFamily[G_Graph, Strict_Symbol: False] := Module[{},
  Return[List[G, WyeTriangleFamily[#, Strict] & /@ WyeTriangleParents[G, Strict]]];
];



WyeTriangleWyeFamily[G_, Lim_Integer: -1, Prog_Symbol: False, Logg_Symbol: False, Strict_Symbol: False, F_Symbol: GraphQ] := Module[
  {ytyarray, queue = {}, position, current, n, queuetotal, ytycount, ytytotal, tmp = Table[Null, {6}], logstream},

  (*  Error Checking  *)
  If[Head@G =!= Graph && !(Head@G === List && Union[Head/@G] === {Graph}), Message[WyeTriangleWyeFamily::badG]; Return[$Failed]];
  If[FreeQ[{True, False}, F[CompleteGraph[9]]] || FreeQ[{True, False}, F[CompleteGraph[{2, 9}]]], Message[WyeTriangleWyeFamily::badCondition]; Return[$Failed]];
  If[FreeQ[{True, False}, Prog], Message[WyeTriangleWyeFamily::badProgressFlag]; Return[$Failed]];
  If[FreeQ[{True, False}, Logg], Message[WyeTriangleWyeFamily::badLoggFlag]; Return[$Failed]];
  If[FreeQ[{True, False}, Strict], Message[WyeTriangleWyeFamily::badStrictFlag]; Return[$Failed]];

  (*  Dynamic Progress Indicators  *)
  If[Prog, tmp[[1]] = PrintTemporary["       Total Number of Graphs in YTY Family So Far: ", Dynamic[ytytotal]]];
  If[Prog, tmp[[2]] = PrintTemporary["       Total Number of Graphs yet to be Considered: ", Dynamic[queuetotal]]];
  If[Prog, tmp[[3]] = PrintTemporary["   Total Number of Graph-Sets yet to be Considered: ", Dynamic[Length@queue]]];
  If[Prog, tmp[[4]] = PrintTemporary["  Size of the Graph-Set Currently Being Considered: ", Dynamic[Length@current]]];
  If[Prog, tmp[[5]] = PrintTemporary["Current Number of YTY Moves from Starting Graph(s): ", Dynamic[n]]];
  If[Prog, tmp[[6]] = PrintTemporary[Dynamic[Text[Grid[ytycount, {Spacings -> {Automatic, .5}, ItemStyle -> Directive[FontSize -> 12]}]]]]];

  (*  Initialize ytyarray, queue, queuetotal, ytycount, ytytotal, and logstream  *)
  If[Head@G === Graph,
    ytyarray = {{{G}}}; queue={{{1,1},1}}; queuetotal = 1; ytycount = {{Text["G"]}}; ytytotal = 1,
    (*ELSE*)
	ytyarray = {{}}& /@ Range[Min[VertexCount /@ G],Max[VertexCount /@ G]];
	Function[tempura, AppendTo[ytyarray[[VertexCount@# - tempura, 1]], #]& /@ G][Min[VertexCount /@ G]-1];
    If[# == {{}}, {}, #]& /@ ytyarray;
    Table[If[ytyarray[[i]] != {}, AppendTo[queue, {{i,1},1}]; ], {i,Length@ytyarray}];
    queuetotal = Length@queue;
    ytycount = Table[If[l == {}, {}, {Length@First@l}], {l, ytyarray}];
    ytytotal = Length@Flatten@ytyarray;
  ];
  If[Logg,
    logstream = OpenWrite["YTYFamily-"<>DateString[{"Month",".","Day",".","Year","-","Hour",".","Minute"}]<>".txt", FormatType -> StandardForm];
    WriteString[logstream, "\n(*** WyeTriangleWyeFamily[g, "<>ToString[Lim]<>", "<>ToString[Prog]<>", "<>ToString[Logg]<>", "<>ToString[Strict]<>", "<>ToString[F]<>"] ***)\n"];
    WriteString[logstream, "(*** "<>DateString[]<>" ***)\n\n\n\n"];
    WriteString[logstream, "(*** Graph(s) Used as Input: ***)\n\n"];
    If[Head@G === Graph, 
      (*THEN*)
      WriteString[logstream, StringReplace[ToString[List@@@EdgeList@G]<>"\n", " " -> ""]],
      (*ELSE*)
      WriteString[logstream, StringReplace[ToString[List@@@EdgeList@#]<>"\n", " " -> ""]]& /@ G;
     ];
    WriteString[logstream, "\n(*** Resulting Wye-Triangle-Wye Family: ***)\n\n"];
  ];

  (*  Main Loop  *)
  While[queue != {},
	position = First@First@queue;
    n = Last@First@queue;

    (*  If we are sitting at the top or bottom of our ytyarray, expand it.  *)
    If[First@position == 1, PrependTo[ytyarray, {}]; PrependTo[ytycount, {}]; queue = {{First@First@#+1, Last@First@#}, Last@#}& /@ queue; position = First@First@queue];
    If[First@position == Length[ytyarray], AppendTo[ytyarray, {}]; AppendTo[ytycount, {}]];
    queue = Rest@queue;
    queuetotal = queuetotal - Length@ytyarray[[First@position, Last@position]];

    (*  Build the TriangleWyeChildren of our current cell.  *)
    (*  Apply condition F, delete duplicates, and delete any graphs that are in the same ytyarray row.  *)
    (*  If there are graphs left, add them to ytyarray and update ytycount and ytytotal.  *)
    (*  If we have not reached Lim, append the new graphs to the queue and update queuetotal.  *)
    current = ytyarray[[First@position, Last@position]];
    current = Flatten[TriangleWyeChildren /@ current];
    If[F =!= GraphQ, current = Cases[current, g_ /; F[g]]];
    current = DeleteGraphDuplicates@current;
    current = DeleteCases[current, g_ /; MemberQ[Flatten[ytyarray[[First@position+1]]], h_ /; IsomorphicGraphQ[g, h]]];
    If[current != {},
      AppendTo[ytyarray[[First@position+1]], current];
      If[Logg, WriteString[logstream, StringReplace[ToString[List@@@EdgeList@#]<>"\n", " " -> ""]]& /@ current];
      AppendTo[ytycount[[First@position+1]], Length@current];
      ytytotal = ytytotal + Length@current;
      If[n != Lim, 
        AppendTo[queue, {{First@position+1, Length@ytycount[[First@position+1]]}, n+1}];
        queuetotal = queuetotal + Length@current;
      ];
    ];

    (*  Build the WyeTriangleParents of our current cell.  *)
    (*  Apply condition F, delete duplicates, and delete any graphs that are in the same ytyarray row.  *)
    (*  If there are graphs left, add them to ytyarray and update ytycount and ytytotal.  *)
    (*  If we have not reached Lim, append the new graphs to the queue and update queuetotal.  *)
    current = ytyarray[[First@position, Last@position]];
    current = Flatten[WyeTriangleParents[#, Strict]& /@ current];
    If[F =!= GraphQ, current = Cases[current, g_ /; F[g]]];
    current = DeleteGraphDuplicates@current;
    current = DeleteCases[current, g_ /; MemberQ[Flatten[ytyarray[[First@position-1]]], h_ /; IsomorphicGraphQ[g, h]]];
    If[current != {},
      AppendTo[ytyarray[[First@position-1]], current];
      If[Logg, WriteString[logstream, StringReplace[ToString[List@@@EdgeList@#]<>"\n", " " -> ""]]& /@ current];
      AppendTo[ytycount[[First@position-1]], Length@current];
      ytytotal = ytytotal + Length@current;
      If[n != Lim,
        AppendTo[queue, {{First@position-1, Length@ytycount[[First@position-1]]}, n+1}];
        queuetotal = queuetotal + Length@current;
      ];
    ]; 
  ];
  
  (* Cleanup *)  
  If[Prog, Do[NotebookDelete[i],{i,tmp}]];
  If[Prog, Print["Total Number of Graphs in YTY-Family: ", ytytotal]];
  If[Prog, Print["  Current Number of YTY Moves from G: ", n]];
  If[Prog, Print[Text[Grid[ytycount, {Spacings -> {Automatic, .5}, ItemStyle -> Directive[FontSize -> 12]}]]]];
  If[Logg, Close@logstream];
  Return[DeleteGraphDuplicates@Flatten@ytyarray];
];



End[ ]
EndPackage[ ]



