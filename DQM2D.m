(* ::Package:: *)

(* \:83b7\:53d6\:811a\:672c\:6240\:5728\:7684\:7edd\:5bf9\:76ee\:5f55 *)
scriptDir = DirectoryName[$InputFileName];
(* \:5c06\:5f53\:524d\:5de5\:4f5c\:76ee\:5f55\:8bbe\:7f6e\:4e3a\:811a\:672c\:6240\:5728\:76ee\:5f55 *)
SetDirectory[scriptDir];
gaussData=Import["triasymq.xlsx"];
createChebNodes2D[m_]:=Module[{A,half,ret,p,q,r,dx1,dx2,dx3,tx1,tx2,tx3,temp,x1,x2,x3,L1,L2,L3,x,y,ret1},
A={{-1/2,1,-1/2},{-Sqrt[3]/2,0,Sqrt[3]/2}}; (* \:4e09\:89d2\:5f62\:7684\:8282\:70b9\:5750\:6807\:ff0c{{xi,xj,xm}\:ff0c{yi,yj,ym}} *)
(*m=10;*)(* m = \:4e09\:89d2\:5f62\:8fb9\:4e0a\:7684\:8282\:70b9\:4e2a\:6570 - 1 *)
half=0.5;
ret={};
For[p=0,p<=m,p++,
For[q=0,q<=m-p,q++,
r=m-p-q;
dx1=Pi*p/(2m);
dx2=Pi*q/(2m);
dx3=Pi*r/(2m);
tx1=Tan[half*dx1];
tx2=Tan[half*dx2];
tx3=Tan[half*dx3];
temp=tx1+tx2+tx3-3*tx1*tx2*tx3;
x1=tx1*(1+tx2)(1+tx3)/temp;
x2=tx2*(1+tx1)(1+tx3)/temp;
x3=tx3*(1+tx1)(1+tx2)/temp;
L1=x1^2;
L2=x2^2;
L3=x3^2;
{x,y}=A . {L1,L2,L3};
AppendTo[ret,{x,y}];
];
];
ret1=Table[{{0,-(2/Sqrt[3])},{2/Sqrt[3],0}} . ret[[k]],{k,1,Length[ret]}];(* \:4eff\:5c04\:53d8\:6362\:5230\:9ad8\:65af\:79ef\:5206\:533a\:57df *)
Return[ret1]
];



args=$CommandLine;
If[Length[args]<5,Print["Usage: math -script DQM2D.m m algebraPrecision"];
Exit[];];
m = ToExpression[args[[4]]];(* \:7f51\:683c\:9636\:6b21 *)
algebraPrecision=ToExpression[args[[5]]]; (* \:9ad8\:65af\:79ef\:5206\:4ee3\:6570\:7cbe\:5ea6 *)
gauss2D=gaussData[[algebraPrecision]];
ret1=createChebNodes2D[m];
\[CapitalPhi][\[Xi]_,\[Eta]_]=Block[{n=m},Flatten[Table[\[Xi]^k*\[Eta]^(m-k),{m,0,n},{k,0,m}]]];
shapes[\[Xi]_,\[Eta]_]=\[CapitalPhi][\[Xi],\[Eta]] . Inverse[Table[\[CapitalPhi][ret1[[i,1]],ret1[[i,2]]],{i,1,Length[ret1]}]];
shapes10[\[Xi]_,\[Eta]_]=D[shapes[\[Xi],\[Eta]],\[Xi]];
shapes01[\[Xi]_,\[Eta]_]=D[shapes[\[Xi],\[Eta]],\[Eta]];
c10=Table[shapes10[ret1[[i,1]],ret1[[i,2]]],{i,1,1Length[ret1]}]//Chop;
c01=Table[shapes01[ret1[[i,1]],ret1[[i,2]]],{i,1,1Length[ret1]}]//Chop;
cL=Table[shapes[gauss2D[[i,1]],gauss2D[[i,2]]],{i,1,Length[gauss2D]}];
Export["c10_c01_order="<>ToString[m]<>".xlsx",{"c10"->c10,"c01"->c01},"Sheets"]
Export["cL_algebraPrecision="<>ToString[algebraPrecision]<>".xlsx",{"cL"->cL},"Sheets"]
