BeginTestSection["Utilities"]
(* ExtractHeads *)

VerificationTest[
    FaizonZaman`LexicalCases`Utilities`ExtractHeads[TextType["Adverb"] ~~ TextType["Adjective"] ~~ BoundToken["specie" | "species"]],
    {TextType, TextType, Alternatives, BoundToken, StringExpression},
    "TestID" -> "Utilities-ExtractHeads-Test1"
]

VerificationTest[
    FaizonZaman`LexicalCases`Utilities`ExtractHeads[TextType["Adjective" | "Determiner"] ~~ "screen"],
    {Alternatives, TextType, StringExpression},
    "TestID" -> "Utilities-ExtractHeads-Test2"
]

VerificationTest[
    FaizonZaman`LexicalCases`Utilities`ExtractHeads[$SampleStringExpression],
    {TextType, StringExpression},
    "TestID" -> "Utilities-ExtractHeads-Test3"
]

(* GetFileExtension *)

VerificationTest[
	FaizonZaman`LexicalCases`Utilities`GetFileExtension[File[FileNameJoin[{$TopDirectory, "SystemFiles", "Components", "TextSearch", "ExampleData", "Text", "AliceInWonderland.txt"}]]],
	"txt",
	"TestID" -> "Utilities-GetFileExtension-Test1"
]

(* MatchTrim *)

VerificationTest[
	FaizonZaman`LexicalCases`Utilities`MatchTrim[True,
		{
			<|"Match" -> " past", "Position" -> {{49, 53}}|>,
			<|"Match" -> " few", "Position" -> {{54, 57}}|>,
			<|"Match" -> " blank", "Position" -> {{145, 150}, {180, 185}, {505, 510}}|>,
			<|"Match" -> " single", "Position" -> {{304, 310}}|>
   		}
   	],
   {
		<|"Match" -> "past", "Position" -> {{50, 53}}|>,
		<|"Match" -> "few", "Position" -> {{55, 57}}|>,
		<|"Match" -> "blank", "Position" -> {{146, 150}, {181, 185}, {506, 510}}|>,
		<|"Match" -> "single", "Position" -> {{305, 310}}|>
		},
	"TestID" -> "Utilities-MatchTrim-Test1"
]

(* OptionsJoin *)

VerificationTest[
	FaizonZaman`LexicalCases`Utilities`OptionsJoin[LexicalCases, LexicalDispersionPlot],
	{
		"Service" -> "Wikipedia",
		"StringTrim" -> True,
		IgnoreCase -> False,
		Overlaps -> False,
		MaxItems -> 50,
		MaxCategories -> 5, 
		Language -> "English",
		AspectRatio -> 1/GoldenRatio,
		ImageSize -> Automatic, 
		PlotRange -> All,
		PlotTheme -> Automatic, 
		PlotLabel -> Automatic,
		HideMissing -> False,
		DataJoin -> False,
		DispersionPlotFunction -> Automatic
	},
	"TestID" -> "Utilities-OptionsJoin-Test1"
]
(* ReplaceEmptyListWithMissing *)

VerificationTest[
	FaizonZaman`LexicalCases`Utilities`ReplaceEmptyListWithMissing[{{}}],
	{Missing["NoMatches"]},
	"TestID" -> "Utilities-ReplaceEmptyListWithMissing-Test1"
]

(* SupportedFileQ *)

VerificationTest[
	FaizonZaman`LexicalCases`Utilities`SupportedFileQ@File[FileNameJoin[{$TopDirectory, "SystemFiles", "Components", "TextSearch", "TextSearch.m"}]],
	False,
	"TestID" -> "Utilities-SupportedFileQ-Test1"
]

(* UnwrapAlternatives *)

VerificationTest[
	FaizonZaman`LexicalCases`Utilities`UnwrapAlternatives[{"A" | "B"}],
	("A"|"B"),
	"TestID" -> "Utilities-UnwrapAlternatives-Test1"
]

(* WrapAlternatives *)

VerificationTest[
	FaizonZaman`LexicalCases`Utilities`WrapAlternatives["A" | "B"],
	{"A" | "B"},
	"TestID" -> "Utilities-WrapAlternatives-Test1"
]

(* StopWordQ *)

VerificationTest[
	StopWordQ["the"],
	True,
	"TestID" -> "Utilities-StopWordQ-Test1"
]

VerificationTest[
	StopWordQ["math"],
	False,
	"TestID" -> "Utilities-StopWordQ-Test2"
]

(* ExpandPattern *)

VerificationTest[
	ExpandPattern[$SampleParagraph, Sandwich[TextType["Adjective" | "Noun"], "computer"]],
	{WordBoundary, "past" | "few" | "blank" | "single", 
   WordBoundary} | {WordBoundary, 
   "words" | "fingers" | "weeks" | "writer" | "block" | "screen" | 
    "front" | "day" | "mind" | "type" | "word" | "process" | "hours" |
     "computer" | "today" | "blank", 
   WordBoundary} ~~ " computer " ~~ (WordBoundary ~~ 
    "past" | "few" | "blank" | "single" ~~ 
    WordBoundary) | (WordBoundary ~~ 
    "words" | "fingers" | "weeks" | "writer" | "block" | "screen" | 
     "front" | "day" | "mind" | "type" | "word" | "process" | 
     "hours" | "computer" | "today" | "blank" ~~ WordBoundary),
    "TestID" -> "Utilities-ExpandPattern-Test1"
]

VerificationTest[
	ExpandPattern["this is the best music ever.", TextType["Adjective"] ~~ "music"],
	WordBoundary ~~ Alternatives["best"] ~~ WordBoundary ~~ " music",
    "TestID" -> "Utilities-ExpandPattern-Test2"
]

(* LexicalPattern *)

VerificationTest[
	StringCases[$SampleParagraph, LexicalPattern[TextType["Adjective" | "Determiner"] ~~ "screen"]],
	{"blank screen", "blank screen", "the screen"},
    "TestID" -> "Utilities-LexicalPattern-Test1"
]

VerificationTest[
	StringCases[$SampleParagraph, LexicalPattern[TextType["Adjective" | "Determiner"] ~~ "screen"]],
	{"blank screen", "blank screen", "the screen"},
    "TestID" -> "Utilities-LexicalPattern-Test2"
]

VerificationTest[
	StringPosition[$SampleParagraph, LexicalPattern[TextType["Determiner"] ~~ TextType["Adjective"] ~~ "screen" ~~ TextType["Preposition" | "Verb"]]],
	{{144, 160}, {176, 201}},
    "TestID" -> "Utilities-LexicalPattern-Test3"
]

VerificationTest[
	StringMatchQ["Alice walked quickly", LexicalPattern["Alice" ~~ TextType["Verb"] ~~ TextType["Adverb"]]],
	True,
    "TestID" -> "Utilities-LexicalPattern-Test4"
]

EndTestSection[]