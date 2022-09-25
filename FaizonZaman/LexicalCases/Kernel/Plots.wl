BeginPackage["FaizonZaman`LexicalCases`Plots`"]
Begin["`Private`"]
Options[FaizonZaman`LexicalCases`LexicalDispersionPlot] = {
	AspectRatio -> 1/GoldenRatio,
	ImageSize -> Automatic,
	PlotRange -> All,
	PlotTheme -> Automatic,
	PlotLabel -> Automatic,
	FaizonZaman`LexicalCases`HideMissing -> False,
	FaizonZaman`LexicalCases`DataJoin -> False,
	FaizonZaman`LexicalCases`DispersionPlotFunction -> Automatic
};

FaizonZaman`LexicalCases`$DispersionPlotFunctions = {"MatrixPlot", "SmoothHistogram"}

GetLexicalEvents[text_String, tokens_] := Association@Map[
  (# -> StringPosition[text, FaizonZaman`LexicalCases`LexicalPattern[#]][[All, 1]]) &,
  tokens
  ]

GetLexicalEvents[ds_Dataset, tokens_] := Association@Map[#Match -> Extract[{All, 1}][#Position] &, Normal@ds]

FaizonZaman`LexicalCases`LexicalDispersionPlot[texts:{__String}, token_, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot, MatrixPlot, SmoothHistogram}]] := iMultiTextLDP[OptionValue[FaizonZaman`LexicalCases`DataJoin], texts, token, opts]
FaizonZaman`LexicalCases`LexicalDispersionPlot[text_String, token_, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot, MatrixPlot, SmoothHistogram}]] := FaizonZaman`LexicalCases`LexicalDispersionPlot[text, {token}, opts]
FaizonZaman`LexicalCases`LexicalDispersionPlot[text_String, tokens_List, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot, MatrixPlot, SmoothHistogram}]] /; AllTrue[tokens, FaizonZaman`LexicalCases`LexicalPatternQ] := Module[
	{
		events,
		plotfn = Replace[OptionValue[FaizonZaman`LexicalCases`DispersionPlotFunction], Automatic -> "MatrixPlot"]
		},
		(* Echo[{opts}, "LDP Options"]; *)
		events = GetLexicalEvents[text, tokens];
		If[
			OptionValue[HideMissing],
			events = DeleteCases[{}][events]
			];
		plot[plotfn, text, events, Keys[events], opts]
		]

FaizonZaman`LexicalCases`LexicalDispersionPlot[text_String, ___] := $Failed


FaizonZaman`LexicalCases`LexicalDispersionPlot[text_String, ds_Dataset, token_, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot, MatrixPlot, SmoothHistogram}]] := FaizonZaman`LexicalCases`LexicalDispersionPlot[text, ds, {token}, opts]
FaizonZaman`LexicalCases`LexicalDispersionPlot[text_String, ds_Dataset, tokens_List, opts:OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot, MatrixPlot, SmoothHistogram}]] /; AllTrue[tokens, FaizonZaman`LexicalCases`LexicalPatternQ] := Module[
	{
		events,
		plotfn = Replace[OptionValue[FaizonZaman`LexicalCases`DispersionPlotFunction], Automatic -> "MatrixPlot"]
		},
		events = GetLexicalEvents[ds, tokens];
		If[
			OptionValue[HideMissing],
			events = DeleteCases[{}][events]
			];
		plot[plotfn, text, events, Keys[events], opts]
]

FaizonZaman`LexicalCases`LexicalDispersionPlot[ds_Dataset, ___] := $Failed

Options[plot] = Options[FaizonZaman`LexicalCases`LexicalDispersionPlot];

plot["MatrixPlot", text_String, tokenevents_Association, ielm_, opts:OptionsPattern[]] := Block[
	{
		label = Replace[OptionValue[PlotLabel], Automatic -> "Lexical Dispersion Plot"],
		theme = Replace[OptionValue[PlotTheme], Automatic -> "Scientific"],
		length = StringLength[text],
		rowindex, spardat, sparr, ticks
		},
	(* Echo[{opts}, "MPlot Options"]; *)
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
		PlotTheme -> theme,
		PlotLabel -> label
		]
	];
plot["SmoothHistogram", text_, tokenevents_, ielm_, opts:OptionsPattern[]] := Block[
	{
		label = Replace[OptionValue[PlotLabel], Automatic -> "Smooth Lexical Histogram"],
		theme = Replace[OptionValue[PlotTheme], Automatic -> "Scientific"]
		},
		SmoothHistogram[
 			tokenevents,
 			PlotLegends -> ielm,
 			AspectRatio -> OptionValue[AspectRatio],
 			ImageSize -> OptionValue[ImageSize],
 			PlotRange -> OptionValue[PlotRange],
 			PlotTheme -> theme,
 			PlotLabel -> label,
			Frame -> None
 		]
	];

Options[iMultiTextLDP] = Options[FaizonZaman`LexicalCases`LexicalDispersionPlot];
(* TODO: Improve plot labeling for these multitext cases *)
iMultiTextLDP[True, texts_, tokens_, opts:OptionsPattern[]] := Block[
	{
		crosstextevents = Merge[Map[GetLexicalEvents[#, tokens]&, texts], Flatten],
		plotfn = Replace[OptionValue[FaizonZaman`LexicalCases`DispersionPlotFunction], Automatic -> "MatrixPlot"]
		},
		If[
			OptionValue[HideMissing],
			crosstextevents = DeleteCases[{}][crosstextevents]
			];

		plot[plotfn, First@MaximalBy[texts, StringLength, 1], crosstextevents, Keys[crosstextevents], opts]
	];
iMultiTextLDP[False, texts_, tokens_, opts:OptionsPattern[]] := Map[FaizonZaman`LexicalCases`LexicalDispersionPlot[#, tokens, opts]&, texts];

End[]
EndPackage[]