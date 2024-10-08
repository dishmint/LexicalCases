BeginTestSection["Utilities"]
(* ExtractHeads *)

TestCreate[
    FaizonZaman`LexicalCases`Utilities`ExtractHeads[TypeToken["Adverb"] ~~ TypeToken["Adjective"] ~~ BoundToken["specie" | "species"]],
    {TypeToken, TypeToken, Alternatives, BoundToken, StringExpression},
    "TestID" -> "Utilities-ExtractHeads-Test1"
]

TestCreate[
    FaizonZaman`LexicalCases`Utilities`ExtractHeads[TypeToken["Adjective" | "Determiner"] ~~ "screen"],
    {Alternatives, TypeToken, StringExpression},
    "TestID" -> "Utilities-ExtractHeads-Test2"
]

TestCreate[
    FaizonZaman`LexicalCases`Utilities`ExtractHeads[$SampleStringExpression],
    {TypeToken, StringExpression},
    "TestID" -> "Utilities-ExtractHeads-Test3"
]

(* GetFileExtension *)

TestCreate[
	FaizonZaman`LexicalCases`Utilities`GetFileExtension[File[FileNameJoin[{$TopDirectory, "SystemFiles", "Components", "TextSearch", "ExampleData", "Text", "AliceInWonderland.txt"}]]],
	"txt",
	"TestID" -> "Utilities-GetFileExtension-Test1"
]

(* MatchTrim *)

TestCreate[
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

TestCreate[
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
		DataJoin -> False
	},
	"TestID" -> "Utilities-OptionsJoin-Test1"
]
(* ReplaceEmptyListWithMissing *)

TestCreate[
	FaizonZaman`LexicalCases`Utilities`ReplaceEmptyListWithMissing[{{}}],
	{Missing["NoMatches"]},
	"TestID" -> "Utilities-ReplaceEmptyListWithMissing-Test1"
]

(* SupportedFileQ *)

TestCreate[
	FaizonZaman`LexicalCases`Utilities`SupportedFileQ@File[FileNameJoin[{$TopDirectory, "SystemFiles", "Components", "TextSearch", "TextSearch.m"}]],
	False,
	"TestID" -> "Utilities-SupportedFileQ-Test1"
]

(* UnwrapAlternatives *)

TestCreate[
	FaizonZaman`LexicalCases`Utilities`UnwrapAlternatives[{"A" | "B"}],
	("A"|"B"),
	"TestID" -> "Utilities-UnwrapAlternatives-Test1"
]

(* WrapAlternatives *)

TestCreate[
	FaizonZaman`LexicalCases`Utilities`WrapAlternatives["A" | "B"],
	{"A" | "B"},
	"TestID" -> "Utilities-WrapAlternatives-Test1"
]

(* StopWordQ *)

TestCreate[
	StopWordQ["the"],
	True,
	"TestID" -> "Utilities-StopWordQ-Test1"
]

TestCreate[
	StopWordQ["math"],
	False,
	"TestID" -> "Utilities-StopWordQ-Test2"
]

(* ExpandPattern *)

TestCreate[
	ExpandPattern[$SampleParagraph, BoundToken[TypeToken["Adjective" | "Noun"], " computer "]],
	("past" | "few" | "blank" | "single") | ("words" | "fingers" | 
    "weeks" | "writer" | "block" | "screen" | "front" | "day" | 
    "mind" | "type" | "word" | "process" | "hours" | "computer" | 
    "today" | "blank") ~~ " computer " ~~ ("past" | "few" | "blank" | 
    "single") | ("words" | "fingers" | "weeks" | "writer" | "block" | 
    "screen" | "front" | "day" | "mind" | "type" | "word" | 
    "process" | "hours" | "computer" | "today" | "blank"),
    "TestID" -> "Utilities-ExpandPattern-Test1"
]

TestCreate[
	ExpandPattern["this is the best music ever.", TypeToken["Adjective"] ~~ " music"],
	"best music",
    "TestID" -> "Utilities-ExpandPattern-Test2"
]

TestCreate[
	ExpandPattern["to be.", WordToken[1] ~~ TypeToken["Verb"]],
	WordBoundary ~~ WordCharacter .. ~~ WordBoundary ~~ "be",
    "TestID" -> "Utilities-ExpandPattern-Test3"
]

(* LexicalPattern *)

TestCreate[
	StringCases[$SampleParagraph, LexicalPattern[TypeToken["Adjective" | "Determiner"] ~~ " screen"]],
	{"blank screen", "blank screen", "the screen"},
    "TestID" -> "Utilities-LexicalPattern-Test1"
]

TestCreate[
	StringCases[$SampleParagraph, LexicalPattern[TypeToken["Adjective" | "Determiner"] ~~ " screen"]],
	{"blank screen", "blank screen", "the screen"},
    "TestID" -> "Utilities-LexicalPattern-Test2"
]

TestCreate[
	StringPosition[$SampleParagraph, LexicalPattern[TypeToken["Determiner"] ~~ " " ~~ TypeToken["Adjective"] ~~ " screen " ~~ TypeToken["Preposition" | "Verb"]]],
	{{144, 160}, {176, 201}},
    "TestID" -> "Utilities-LexicalPattern-Test3"
]

TestCreate[
	StringMatchQ["Alice walked quickly", LexicalPattern["Alice " ~~ TypeToken["Verb"] ~~ " " ~~ TypeToken["Adverb"]]],
	True,
    "TestID" -> "Utilities-LexicalPattern-Test4"
]

EndTestSection[]