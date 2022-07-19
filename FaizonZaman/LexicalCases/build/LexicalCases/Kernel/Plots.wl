BeginPackage["LexicalCases`Plots`"]
Begin["`Private`"]
Options[LexicalCases`LexicalDispersionPlot] = {
	AspectRatio -> 1/5,
	ImageSize -> Large,
	PlotRange -> All,
	PlotTheme -> "Scientific",
	PlotLabel -> "Lexical Dispersion Plot"
};
LexicalCases`LexicalDispersionPlot[text_String, word_String, opts:OptionsPattern[{LexicalCases`LexicalDispersionPlot}]] := LexicalCases`LexicalDispersionPlot[text, {word}, opts]
LexicalCases`LexicalDispersionPlot[text_String, words:{__String}, opts:OptionsPattern[{LexicalCases`LexicalDispersionPlot}]] := Module[
	{
		tokens = Monitor[
			(* TextWords is slow for what I want it to do, using StringCases instead *)
			(*TextWords[text]*)
			StringCases[text, RegularExpression["\\b\\w+\\b"]],
			Row[{"Tokenizing text",ProgressIndicator[Appearance->"Ellipsis"]}]
			], textevents, events,
		rowIndex = AssociationThread[words -> Range[Length[words]]],
		sparr, ticks
		},
		(* 1 \[LongDash] Generate discrete indices for the tokenized text *)
		textevents = Monitor[PositionIndex[tokens], Row[{"Indexing tokens", ProgressIndicator[Appearance->"Ellipsis"]}]];
		events = Monitor[
			KeyTake[words][textevents] // KeyValueMap[Thread[Thread[{rowIndex[#1], #2}] -> 1] &] /* Flatten,
			Row[{"Generating sparse elements", ProgressIndicator[Appearance->"Ellipsis"]}]
			];
		sparr = SparseArray[events, {Length[words], Length[tokens]}];
		
		ticks = KeyValueMap[{#2, #1}&, rowIndex];
		MatrixPlot[
			sparr,
			FrameTicks -> {{ticks, None}, {Automatic, None}},
			AspectRatio -> OptionValue[AspectRatio],
			ImageSize -> OptionValue[ImageSize],
			PlotRange -> OptionValue[PlotRange],
			PlotTheme -> OptionValue[PlotTheme],
			PlotLabel -> OptionValue[PlotLabel]
			]
		]
End[]
EndPackage[]