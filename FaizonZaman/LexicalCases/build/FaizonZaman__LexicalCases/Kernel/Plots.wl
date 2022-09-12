BeginPackage["FaizonZaman`LexicalCases`Plots`"]
Begin["`Private`"]
Options[FaizonZaman`LexicalCases`LexicalDispersionPlot] = {
	AspectRatio -> 1/GoldenRatio,
	ImageSize -> Automatic,
	PlotRange -> All,
	PlotTheme -> "Scientific",
	PlotLabel -> Automatic,
	FaizonZaman`LexicalCases`HideMissing -> False,
	FaizonZaman`LexicalCases`DataJoin -> False,
	FaizonZaman`LexicalCases`DispersionPlotFunction -> Automatic
};

FaizonZaman`LexicalCases`$DispersionPlotFunctions = {"MatrixPlot", "SmoothHistogram"}

GetLexicalEvents[text_, tokens_] := Association@Map[
  (# -> StringPosition[text, FaizonZaman`LexicalCases`LexicalPattern[#]][[All, 1]]) &,
  tokens
  ]

FaizonZaman`LexicalCases`LexicalDispersionPlot[texts:{__String}, token_, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot}]] := iMultiTextLDP[OptionValue[FaizonZaman`LexicalCases`DataJoin], texts, token, opts]
FaizonZaman`LexicalCases`LexicalDispersionPlot[text_String, token_, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot}]] := FaizonZaman`LexicalCases`LexicalDispersionPlot[text, {token}, opts]
FaizonZaman`LexicalCases`LexicalDispersionPlot[text_String, tokens_List, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot}]] /; AllTrue[tokens, FaizonZaman`LexicalCases`LexicalPatternQ] := Module[
	{
		events,
		plotfn = Replace[OptionValue[FaizonZaman`LexicalCases`DispersionPlotFunction], Automatic -> "MatrixPlot"]
		},
		events = GetLexicalEvents[text, tokens];
		If[
			OptionValue[HideMissing],
			events = DeleteCases[{}][events]
			];
		plot[plotfn, text, events, Keys[events], opts]
		]

FaizonZaman`LexicalCases`LexicalDispersionPlot[text_String, ___] := $Failed

plot["MatrixPlot", text_String, tokenevents_Association, ielm_, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot}]] := Block[
	{
		label = Replace[OptionValue[PlotLabel], Automatic -> "Lexical Dispersion Plot"],
		length = StringLength[text],
		rowindex, spardat, sparr, ticks
		},
	rowindex = AssociationThread[ielm -> Range[Length[ielm]]];
	spardat = tokenevents // KeyValueMap[Thread[Thread[{rowindex[#1], #2}] -> 1] &] /* Flatten;
	sparr = SparseArray[spardat, {Length[ielm], length}];
		
	ticks = KeyValueMap[{#2, #1}&, rowindex];
	MatrixPlot[
		sparr,
		FrameTicks -> {{ticks, None}, {Automatic, None}},
		AspectRatio -> OptionValue[AspectRatio],
		ImageSize -> OptionValue[ImageSize],
		PlotRange -> OptionValue[PlotRange],
		PlotTheme -> OptionValue[PlotTheme],
		PlotLabel -> label
		]
	];
plot["SmoothHistogram", text_, tokenevents_, ielm_, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot}]] := Block[
	{label = Replace[OptionValue[PlotLabel], Automatic -> "Smooth Lexical Histogram"]},
		SmoothHistogram[
 			tokenevents,
 			PlotLegends -> ielm,
 			AspectRatio -> OptionValue[AspectRatio],
 			ImageSize -> OptionValue[ImageSize],
 			PlotRange -> OptionValue[PlotRange],
 			PlotTheme -> OptionValue[PlotTheme],
 			PlotLabel -> label,
			Frame -> None
 		]
	];
(* TODO: Improve plot labeling for these multitext cases *)
iMultiTextLDP[True, texts_, tokens_, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot}]] := Block[
	{
		crosstextevents = Merge[Map[GetLexicalEvents[#, tokens]&, texts], Flatten],
		plotfn = Replace[OptionValue[FaizonZaman`LexicalCases`DispersionPlotFunction], Automatic -> "MatrixPlot"]
		},
		
		If[
			OptionValue[HideMissing],
			crosstextevents = DeleteCases[{}][crosstextevents]
			];

		plot[plotfn, First@MaximalBy[texts, StringLength, 1], crosstextevents, Keys[crosstextevents]]
	];
iMultiTextLDP[False, texts_, tokens_, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot}]] := Map[FaizonZaman`LexicalCases`LexicalDispersionPlot[#, tokens, opts]&, texts];

End[]
EndPackage[]