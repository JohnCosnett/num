(* :Title: conduct *)
(* :Context: conduct` *)
(* :Author: johncosnett *)
(* :Date: 2016-09-03 *)


BeginPackage["conduct`"]

surge::usage = "surge[ matrix ] updates after charge has percolated to nearby cells";

breakApart::usage = "breakApart[ matrix ] partitions matrix into a tensor of submatrices one for each cell";

putBackTogether::usage = "putBackTogether[ matrix ] inverse operation of breakApart[]";

planckSecond::usage = "planckSecond[ matrix ] a single cycle of putBackTogether[ surge[ breakApart[ ]]]";

percolT::usage = "percolT example matrix 3\[Times]3";

simulate::usage = "simulate[ size, probability ]";

trumpMatrix::usage = "trumpMatrix[ probability, size ] creates matrix with a layer of charge 2's across the top";

gifMakerC::usage = "gifMakerC[{color0, color1, color2}, size, probability, fileNameString, DisplayDurationOption,meshBool ]";

gifMakerB::usage = "gifMakerB[{color0, color1, color2}, size, probability, fileNameString, DisplayDurationOption,meshBool ]";


gifMaker::usage = "gifMaker[ size, probability, fileNameString, DisplayDuration]";

s::usage = "s[ matrix ]";

Print["welcome to the percolator..."];


Begin["`Private`"]


gifMakerC[{color0_, color1_, color2_}, size_, probability_,
   fileNameString_, frameDuration_: Nothing, meshBool_:True] :=

  Export[fileNameString <> ".GIF",
   MatrixPlot[#,
      ColorRules -> {0 -> color0, 1 -> color1, 2 -> color2},Mesh -> meshBool,FrameTicks -> None] & /@
    FixedPointList[planckSecond, trumpMatrix[probability, size]],
   frameDuration];



gifMakerB[{color0_, color1_, color2_}, size_, probability_,
   fileNameString_, frameDuration_: Nothing, meshBool_:True] :=

  Export[fileNameString <> ".GIF",
   MatrixPlot[#,
      ColorRules -> {0 -> color0, 1 -> color1, 2 -> color2},Mesh -> meshBool,FrameTicks -> None] & /@
    FixedPointList[planckSecond, bumpMatrix[probability, size]],
   frameDuration];




gifMaker[size_, probability_, fileNameString_,
   frameDuration_: Nothing] :=

  Export[fileNameString <> ".GIF",
   MatrixPlot[#,
      ColorRules -> {0 -> LightBlue, 1 -> Green, 2 -> Red},Mesh -> All] & /@
    FixedPointList[planckSecond, trumpMatrix[probability, size]],
   frameDuration];


simulate[ size_, probability_]:= s[ trumpMatrix[probability, size] ]

s[ matrix_] :=
    ListAnimate[
         MatrixPlot[#,ColorRules -> {0 -> Black, 1 -> Green, 2->Red}, FrameTicks->None]&

             /@ FixedPointList[planckSecond, matrix]
    ];

bumpMatrix[probability_ ,size_] := ReplacePart[
  (RandomChoice[{probability, 1 - probability} -> {1, 0}, {size - 1, size}]),Table[{50+i,50+i},{i,-20,20,1}]~Join~Table[{50-i,50+i},{i,-20,20,1}]->2];

trumpMatrix[probability_ ,size_] := {Table[2, size]}~Join~(RandomChoice[{probability, 1 - probability} -> {1, 0}, {size - 1, size}])

surge[ matrix_ ]:=

    matrix //. {

          {a_Integer, b_, c_},
          {d_,        e_, f_},
          {g_,        h_, i_}

        } /; ( e == 1 && (b ==2 || d == 2 || f == 2|| h== 2)):>
            {
              {a,b,c},
              {d,2,f},
              {g,h,i}
            };


breakApart[ matrix_ ]:= Partition[ArrayPad[matrix, 1], {3, 3}, {1, 1}]

putBackTogether[ matrix_ ]:=

    (matrix /.
          {

            {a_Integer, b_, c_},
            {d_       , e_, f_},
            {g_       , h_, i_}

          } :> e

    )

planckSecond[ matrix_ ]:= putBackTogether[ surge[ breakApart[ matrix]]]

percolT = ( {

    { 0, 2, 2},
    { 1, 1, 1},
    { 1, 0, 0}

   } );




End[]

EndPackage[]