BeginPackage["FaizonZaman`LexicalCases`Plots`"]
Begin["`Private`"]
Options[FaizonZaman`LexicalCases`LexicalDispersionPlot] = {
	AspectRatio -> 1/GoldenRatio,
	ImageSize -> Automatic,
	PlotRange -> All,
	PlotTheme -> Automatic,
	PlotLabel -> Automatic,
	FaizonZaman`LexicalCases`HideMissing -> False,
	FaizonZaman`LexicalCases`DataJoin -> False
};

Options[FaizonZaman`LexicalCases`LexicalDispersionSmoothHistogram] = Options[FaizonZaman`LexicalCases`LexicalDispersionPlot];

dispersionOpts = OptionsPattern[{FaizonZaman`LexicalCases`LexicalDispersionPlot, MatrixPlot, SmoothHistogram}];

GetLexicalEvents[text_String, tokens_] := Association@Map[
  (# -> StringPosition[text, FaizonZaman`LexicalCases`LexicalPattern[#]][[All, 1]]) &,
  tokens
  ]

GetLexicalEvents[ds_Dataset] := Association@Map[#Match -> Extract[{All, 1}][#Position] &, Normal@ds]
GetLexicalEvents[ds_Dataset, _] := Association@Map[#Match -> Extract[{All, 1}][#Position] &, Normal@ds]
GetLexicalEvents[ds:List[__Association], _] := Association@Map[#Match -> Extract[{All, 1}][#Position] &, ds]

FaizonZaman`LexicalCases`LexicalDispersionPlot[args__, opts:dispersionOpts]:= iLexicalDispersion["MatrixPlot", args, opts];
FaizonZaman`LexicalCases`LexicalDispersionSmoothHistogram[args__, opts:dispersionOpts]:= iLexicalDispersion["SmoothHistogram", args, opts];

iLexicalDispersion[plotfn_String, texts:{__String}, token_, opts:dispersionOpts] := 
	iMultiTextLDP[plotfn, OptionValue[FaizonZaman`LexicalCases`DataJoin], texts, token, opts]

iLexicalDispersion[plotfn_String, texts:{__String}, ds_Dataset, token_, opts:dispersionOpts] := 
	iMultiTextLDP[plotfn, OptionValue[FaizonZaman`LexicalCases`DataJoin], ds, texts, token, opts]

iLexicalDispersion[plotfn_String, text_String, ds_Dataset, token_, opts:dispersionOpts] := 
	plot[plotfn, text, GetLexicalEvents[ds], token, opts]

iLexicalDispersion[plotfn_String, text_String, token_, opts:dispersionOpts] := 
	iLexicalDispersion[plotfn, text, {token}, opts]

iLexicalDispersion[plotfn_String, text_String, tokens_List, opts:dispersionOpts] /; AllTrue[tokens, FaizonZaman`LexicalCases`LexicalPatternQ] := Module[
	{
		events
		},
		events = GetLexicalEvents[text, tokens];
		If[
			OptionValue[HideMissing],
			events = DeleteCases[{}][events]
			];
		plot[plotfn, text, events, Keys[events], opts]
		]

iLexicalDispersion[plotfn_String, smry_FaizonZaman`LexicalCases`LexicalSummary, opts:dispersionOpts] := 
	Block[
		{ucmp = Uncompress[smry["SourceData"]], res = smry["Dataset"] // ReverseSortBy[Length[#Position] &], keys},
		keys = res[All,#Match&] // Normal // DeleteDuplicates;
		Switch[smry["Source"],
			"Wikipedia", iLexicalDispersion[plotfn, ucmp, res, keys, FaizonZaman`LexicalCases`DataJoin -> True],
			_, iLexicalDispersion[plotfn, ucmp, res, keys]
		]
	]

iLexicalDispersion[args___] := (EchoLabel["Inputs"]@Map[Head][{args}];$Failed)

Options[plot] = Options[FaizonZaman`LexicalCases`LexicalDispersionPlot];

plot["MatrixPlot", text_String, tokenevents_Association, ielm_, opts:OptionsPattern[]] := Block[
	{
		label = Replace[OptionValue[PlotLabel], Automatic -> "Lexical Dispersion Plot"],
		theme = Replace[OptionValue[PlotTheme], Automatic -> "Scientific"],
		isize = Replace[OptionValue[ImageSize], Automatic -> Full],
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
		ImageSize -> isize,
		PlotRange -> OptionValue[PlotRange],
		PlotTheme -> theme,
		PlotLabel -> label
		]
	];

plot["SmoothHistogram", text_, tokenevents_Association, ielm_, opts:OptionsPattern[]] := plot["SmoothHistogram", text, Values[tokenevents], ielm, opts] 
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
			Frame -> None,
			Filling -> Axis
 		]
	];

Options[iMultiTextLDP] = Options[FaizonZaman`LexicalCases`LexicalDispersionPlot];
(* TODO: Improve plot labeling for these multitext cases *)
iMultiTextLDP[plotfn_, True, texts_, tokens_, opts:OptionsPattern[]] := Block[
	{
		crosstextevents = Merge[Map[GetLexicalEvents[#, tokens]&, texts], Flatten]
		},
		If[
			OptionValue[HideMissing],
			crosstextevents = DeleteCases[{}][crosstextevents]
			];

		plot[plotfn, First@MaximalBy[texts, StringLength, 1], crosstextevents, Keys[crosstextevents], opts]
	];
iMultiTextLDP[plotfn_, True, ds_Dataset, texts_, tokens_, opts:OptionsPattern[]] := Block[
	{
		crosstextevents = Merge[Map[GetLexicalEvents[#, tokens]&, (Normal[ds] // SplitBy[#, #Article&]&)], Flatten]
		},
		If[
			OptionValue[HideMissing],
			crosstextevents = DeleteCases[{}][crosstextevents]
			];

		plot[plotfn, First@MaximalBy[texts, StringLength, 1], crosstextevents, Keys[crosstextevents], opts]
	];
iMultiTextLDP[plotfn_String, False, texts_, tokens_, opts:OptionsPattern[]] := Map[iLexicalDispersion[plotfn, #, tokens, opts]&, texts];
iMultiTextLDP[plotfn_String, False, ds_Dataset, texts_, tokens_, opts:OptionsPattern[]] := Map[iLexicalDispersion[plotfn, ds, #, tokens, opts]&, texts];

End[]
EndPackage[]