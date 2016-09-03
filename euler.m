(* ::Package:: *)

BeginPackage["euler`"]


eulerMethodS::usage = "eulerMethodS[{x,y},h,slope,steps]"

eulerMethod2::usage = "eulerMethod2[equation,initial conditions,h,steps]"

rungaRunga::usage = "rungaRunga[{t0, \[Theta]0, V0}, slope\[Theta], slopeV, h, steps] generates points using the midpoint method"


Print["welcome to outWorld...\n\n"]
(*rungaRunga[{0,0,1.99},Function[{x,y,z},z],Function[{x,y,z},-Sin[y]],.001,100000]//ListPlot*)


Begin["`Private`"]



eulerStep[{x_,y_},h_,slope_]:=Module[{k1 =slope[x,y]},{x+h,y+k1 h}]


eulerMethodS[{x_,y_},h_,slope_,steps_]:=NestList[
										  eulerStep[#,h,slope]&,
											{x,y},
												steps]


Clear[eulerMethod2]
eulerMethod2[_[_]==function_,_[xx_]==yy_,h_,steps_]:=Module[{},

fff=Function[{x,y},Evaluate@(function

/.(Thread[Level[function,{-1}]->x])/.{Y[_]->y,\[Theta][_]->y,V[_]->y}
)

];
eulerMethodS[{xx,yy},h,Function[{xxxx,yyyy},fff[xxxx,yyyy]],steps]
]


rk2Step[{t0_,\[Theta]0_,V0_},slope\[Theta]_,slopeV_,h_]:=Module[{},
k1\[Theta]=slope\[Theta][t0,\[Theta]0,V0];
k1V =slopeV[t0,\[Theta]0,V0];

k2\[Theta]=slope\[Theta][t0+h/2,\[Theta]0+h/2 k1\[Theta],V0+h/2 k1V];
k2V=slopeV[t0+h/2,\[Theta]0+h/2 k1\[Theta],V0+h/2 k1V];

{t0+h,\[Theta]0+h k2\[Theta],V0+h k2V}

]


rungaRunga[{t0_,\[Theta]0_,V0_},slope\[Theta]_,slopeV_,h_,steps_]:={#[[1]],#[[2]]}&/@NestList[
rk2Step[#,slope\[Theta],slopeV,h]&,
{t0,\[Theta]0,V0},
steps
];


End[]
EndPackage[]


rungaRunga[{0,0,1.99},Function[{x,y,z},z],Function[{x,y,z},-Sin[y]],.001,100000]//ListPlot


?euler`*
