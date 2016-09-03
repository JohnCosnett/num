(* ::Package:: *)

BeginPackage["c`",{"CCompilerDriver`"}]


cc::usage = "cc[program] returns the output of a 
c program as a string" 


cc[program_, name_:"hello"]:=(hey=CreateExecutable["#include <stdio.h>
"<>program
,name];
Import["!"<>QuoteFile[hey],"Text"])


Begin["`Private`"]


End[]
EndPackage[]
