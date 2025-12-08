(* ::Package:: *)

(* \:83b7\:53d6\:811a\:672c\:6240\:5728\:7684\:7edd\:5bf9\:76ee\:5f55 *)

scriptDir = DirectoryName[$InputFileName];

(* \:5c06\:5f53\:524d\:5de5\:4f5c\:76ee\:5f55\:8bbe\:7f6e\:4e3a\:811a\:672c\:6240\:5728\:76ee\:5f55 *)

SetDirectory[scriptDir];

createChebNodes2D[m_] :=
    Module[{A, half, ret, p, q, r, dx1, dx2, dx3, tx1, tx2, tx3, temp,
         x1, x2, x3, L1, L2, L3, x, y, ret1},
        A = {{-1 / 2, 1, -1 / 2}, {-Sqrt[3] / 2, 0, Sqrt[3] / 2}};
    (* 
   \:4e09\:89d2\:5f62\:7684\:8282\:70b9\:5750\:6807\:ff0c{{xi,xj,xm}\:ff0c{yi,yj,ym}} *)
        (*m=10;*)(* m = \:4e09\:89d2\:5f62\:8fb9\:4e0a\:7684\:8282\:70b9\:4e2a\:6570 - 1 *)
        half = 0.5;
        ret = {};
        For[p = 0, p <= m, p++,
            For[q = 0, q <= m - p, q++,
                r = m - p - q;
                dx1 = Pi * p / (2 m);
                dx2 = Pi * q / (2 m);
                dx3 = Pi * r / (2 m);
                tx1 = Tan[half * dx1];
                tx2 = Tan[half * dx2];
                tx3 = Tan[half * dx3];
                temp = tx1 + tx2 + tx3 - 3 * tx1 * tx2 * tx3;
                x1 = tx1 * (1 + tx2) (1 + tx3) / temp;
                x2 = tx2 * (1 + tx1) (1 + tx3) / temp;
                x3 = tx3 * (1 + tx1) (1 + tx2) / temp;
                L1 = x1^2;
                L2 = x2^2;
                L3 = x3^2;
                {x, y} = A . {L1, L2, L3};
                AppendTo[ret, {x, y}];
            ];
        ];
        ret1 = Table[{{0, -(2 / Sqrt[3])}, {2 / Sqrt[3], 0}} . ret[[k
            ]], {k, 1, Length[ret]}];(* \:4eff\:5c04\:53d8\:6362\:5230\:9ad8\:65af\:79ef\:5206\:533a\:57df *)
        Return[ret1]
    ];

createMesh[ret1_, mesh_, node_] :=
    Module[
        {iP, extendMesh, extendNodes, n, tri, iel, T, abcd, ef, extendTri
            }
        ,
   (* \:628a1\:9636\:4e09\:89d2\:5f62\:7f51\:683c\:6269\:5c55\:4e3a\:9ad8\:9636\:7f51\:683c;
   extendMesh\:4e0eextendNodes\:90fd\:6ca1\:6709\:8003\:8651\:5355\:5143\:4e4b\:95f4\:7684\:8026\:5408\:ff0c\:9700\:8981\:7f51\:683c\:5408\:5e76\:7b97\:6cd5\:7ee7\:7eed\:5408\:5e76
   *)
        iP = {{0, 1 / Sqrt[3], 1/3}, {-(1/2), -(1 / (2 Sqrt[3])), 1/3
            }, {1/2, -(1 / (2 Sqrt[3])), 1/3}};
        extendNodes = {};
        extendMesh = {};
        n = Length[ret1];
        For[iel = 1, iel <= Length[mesh], iel++,
            tri = node[[mesh[[iel]]]];
            T = Append[tri\[Transpose], {1, 1, 1}] . iP;
            abcd = T[[1 ;; 2, 1 ;; 2]];
            ef = T[[1 ;; 2, 3]];
            extendTri = Table[abcd . ret1[[i]] + ef, {i, 1, n}];
            extendNodes = Join[extendNodes, extendTri];
            AppendTo[extendMesh, Range[1, n] + (iel - 1) * n];
        ];
        {extendMesh, extendNodes}
    ];

findDuplicateCoordinatesFinal[coords_, tolerance_ : 10 ^ -8] :=
    Module[
        {rounded, groups}
        ,(*\:5904\:7406\:6d6e\:70b9\:6570\:7cbe\:5ea6\:95ee\:9898*)
        rounded =
            If[tolerance > 0,
                Round[coords / tolerance] * tolerance
                ,
                coords
            (*\:5982\:679c\:5bb9\:5dee\:4e3a0\:ff0c\:4f7f\:7528\:7cbe\:786e\:5339\:914d*)];
        (*\:5206\:7ec4\:5e76\:7b5b\:9009\:91cd\:590d*)
        groups = Values @ GroupBy[Range[Length[coords]], rounded[[#]]&
            ];
        (*\:8fd4\:56de\:6709\:91cd\:590d\:7684\:7ec4\:ff0c\:6309\:987a\:5e8f\:6392\:5e8f*)Sort /@ Select[groups, Length[#] > 1&]
    ];

(*\:5e26\:6709\:8be6\:7ec6\:4fe1\:606f\:7684\:7248\:672c*)

findDuplicateCoordinatesDetailed[coords_, tolerance_ : 10 ^ -8] :=
    Module[{duplicateGroups, details},
        duplicateGroups = findDuplicateCoordinatesFinal[coords, tolerance
            ];
        details = Table[<|"Coordinate" -> coords[[First[group]]], "Indices"
             -> group, "Count" -> Length[group], "AllCoordinates" -> coords[[group
            ]]|>, {group, duplicateGroups}];
        <|"DuplicateGroups" -> duplicateGroups, "Details" -> details,
             "Summary" -> <|"TotalDuplicateGroups" -> Length[duplicateGroups], "TotalDuplicatePoints"
             -> Total[Length[#] - 1& /@ duplicateGroups], "UniqueCoordinatesCount"
             -> Length[coords] - Total[Length[#] - 1& /@ duplicateGroups]|>|>
    ];

normalizeNodeNumbers[mesh_] :=
    Module[
        {nodes, uniqueNodes, sortedNodes, mapping, maxNode}
        , (*\:63d0\:53d6\:7f51\:683c\:4e2d\:7684\:6240\:6709\:8282\:70b9\:7f16\:53f7*)nodes = Flatten[mesh];
        uniqueNodes = DeleteDuplicates[nodes] // Sort;
        maxNode = Max[nodes];
        (*\:521b\:5efa\:6620\:5c04\:89c4\:5219\:ff1a\:5c06\:73b0\:6709\:8282\:70b9\:7f16\:53f7\:6620\:5c04\:5230\:8fde\:7eed\:7f16\:53f7*)
        mapping = Thread[uniqueNodes -> Range[Length[uniqueNodes]]];
        (*\:5e94\:7528\:6620\:5c04\:89c4\:5219\:5230\:7f51\:683c*) {mesh /. mapping, mapping}
    ];

mergeMesh[meshElements_, nodeCoordinates_] :=
    Module[{dups, idu, mapping, newmesh, newnodeCoordinates, mesh, comm,
         j},
        dups = findDuplicateCoordinatesFinal[nodeCoordinates];
        mesh = meshElements;
        For[idu = 1, idu <= Length[dups], idu++,
            comm = dups[[idu, 1]];
            For[j = 2, j <= Length[dups[[idu]]], j++,
                mesh = mesh /. {dups[[idu, j]] -> comm};
            ];
        ];
        {newmesh, mapping} = normalizeNodeNumbers[mesh];
        newnodeCoordinates = nodeCoordinates[[mapping[[;;, 1]]]];
        {newmesh, newnodeCoordinates}
    ];

(* \:7528\:4e8e\:53ef\:89c6\:5316\:4e00\:9636\:5355\:5143\:6784\:6210\:7684\:7f51\:683c *)

visualizeMesh[meshElements_, nodeCoordinates_] :=
    Graphics[{EdgeForm[Black], FaceForm[{Cyan, Opacity[0.0]}], Polygon[nodeCoordinates
        [[#]]]& /@ meshElements, Red, PointSize[0.01], Point[nodeCoordinates
        ], Black, Table[Text[Style[ToString[i], FontSize -> 0], nodeCoordinates
        [[i]], {0, -0.1}], {i, Length[nodeCoordinates]}]}, ImageSize -> 700]

(* \:7528\:4e8e\:53ef\:89c6\:5316order\:9636\:7f51\:683c *)

showMesh[elements_, nodes_, order_, nodeMarkSize_:0.01, nodeLabelSize_
    :0, elementLabelSize_:0, imagesize_:700] :=
    Module[{figs, nodeLabels, elementLabels, nodeMarks},
        figs = Table[Graphics[{EdgeForm[{Thickness[0.0005], Purple}],
             FaceForm[{Cyan, Opacity[0.0]}], Polygon[nodes[[elements[[i, {1, 1 + 
            order, -1}]]]]]}], {i, 1, Length[elements]}];
        nodeLabels = Table[Graphics[Text[i, nodes[[i]], {1, 1}, Background
             -> None, BaseStyle -> {FontFamily -> "Arial", FontSize -> nodeLabelSize,
             FontColor -> Black}]], {i, 1, Length[nodes]}];
        elementLabels = Table[Graphics[Text[i, Mean[nodes[[elements[[
            i]]]]], {0, 0}, Background -> None, BaseStyle -> {FontFamily -> "Arial",
             FontSize -> elementLabelSize, FontColor -> Black}]], {i, 1, Length[elements
            ]}];
        nodeMarks = Table[Graphics[{Black, Rectangle[nodes[[i]] - nodeMarkSize,
             nodes[[i]] + nodeMarkSize]}], {i, 1, Length[nodes]}];
        AppendTo[figs, nodeMarks];
        AppendTo[figs, nodeLabels];
        AppendTo[figs, elementLabels];
        Show[figs, ImageSize -> imagesize]
    ];


(*Mathematica\:6570\:636e\:5904\:7406\:811a\:672c*)
args=$CommandLine;
If[Length[args]<8,Print["Usage: math -script process.m order <input.xlsx> <output.xlsx> <output0.svg> <output1.svg>"];
Exit[];];

m = ToExpression[args[[4]]];
inputFile=args[[5]];
outputFile=args[[6]];
outputFig1=args[[7]];
outputFig2=args[[8]];

(*\:8bfb\:53d6Excel\:6587\:4ef6*)
data = Import[inputFile,"XLSX"];
node = data[[1]];
mesh = data[[2]];
Export[outputFig1,visualizeMesh[mesh, node]];

Print["Processing data with mergeMesh..."];
ret1=createChebNodes2D[m];

{extendMesh,extendNodes}=createMesh[ret1,mesh,node];
{newmesh,newnodeCoordinates}=mergeMesh[extendMesh,extendNodes];

Print["Nodes: ",Length[newnodeCoordinates],", Elements: ",Length[newmesh]];

(*\:51c6\:5907\:5bfc\:51fa\:6570\:636e*)
nodeExport=Prepend[MapIndexed[Prepend[#1,First[#2]]&,newnodeCoordinates],{"\:8282\:70b9ID","X\:5750\:6807","Y\:5750\:6807"}];
elementExport=Prepend[MapIndexed[Prepend[#1,First[#2]]&,newmesh],Join[{"\:5355\:5143ID"},Table["\:8282\:70b9"<>ToString[i],{i,1,(m+1)*(m+2)/2}]]];
Export[outputFig2,showMesh[newmesh, newnodeCoordinates, m]];

Print["Exporting results to: ",outputFile];
(*\:5bfc\:51fa\:5230Excel*)
Export[outputFile,{"\:8282\:70b9\:6570\:636e"->nodeExport,"\:5355\:5143\:6570\:636e"->elementExport}];

showMesh[elements_, nodes_, order_, nodeMarkSize_:0.05, nodeLabelSize_:12, elementLabelSize_:12, imagesize_:700]


Print["Processing completed successfully!"];
