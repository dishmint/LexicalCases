BeginTestSection["LexicalCases"]

(* STRINGS *)
(* Short Strings *)
TestCreate[
	LexicalCases[$SampleSentence, $SampleStringExpression]["Data"],
	{<|"Match" -> "best key lime pie", "Position" -> {{5, 21}}|>},
	"TestID" -> "ShortStringTest1"
	]

(* Documentation examples - BoundToken *)
TestCreate[
	LexicalCases["The great machine whirs. The weak machines sputter.", "great " ~~ BoundToken["machine"]]["Data"],
	{<|"Match" -> "great machine", "Position" -> {{5, 17}}|>},
	"TestID" -> "LexicalCases-DocExamples-BoundToken-Test1"
	]

TestCreate[
	LexicalCases["The great machine whirs. The weak machines sputter.", "great " ~~ BoundToken["machine"]]["Data"],
	{<|"Match" -> "great machine", "Position" -> {{5, 17}}|>},
	"TestID" -> "LexicalCases-DocExamples-BoundToken-Test1"
	]

TestCreate[
	LexicalCases["The great machine whirs. The weak machines sputter.", "weak " ~~ BoundToken["machines"]]["Data"],
	{<|"Match" -> "weak machines", "Position" -> {{30, 42}}|>},
	"TestID" -> "LexicalCases-DocExamples-BoundToken-Test2"
	]

TestCreate[
	LexicalCases["The great machine whirs. The weak machines sputter.", "weak " ~~ BoundToken[RegularExpression["\\w+"]]]["Data"],
	{<|"Match" -> "weak machines", "Position" -> {{30, 42}}|>},
	"TestID" -> "LexicalCases-DocExamples-BoundToken-Test3"
	]

TestCreate[
	LexicalCases["He was number 1!", "number " ~~ BoundToken[DigitCharacter]]["Data"],
	{<|"Match" -> "number 1", "Position" -> {{8, 15}}|>},
	"TestID" -> "LexicalCases-DocExamples-BoundToken-Test4"
	]

TestCreate[
	LexicalCases["The great machine whirs. The weak machines sputter.", ("great" | "weak") ~~ " " ~~ BoundToken["machine" | "machines"]]["Data"],
	{
		<|"Match" -> "great machine", "Position" -> {{5, 17}}|>,
		<|"Match" -> "weak machines", "Position" -> {{30, 42}}|>
		},
	"TestID" -> "LexicalCases-DocExamples-BoundToken-Test5"
	]

(* Documentation examples - WordToken *)

TestCreate[
	LexicalCases[$SampleParagraph, WordToken[2] ~~ " screen"]["Data"],
	{
		<|"Match" -> "a blank screen", "Position" -> {{144, 157}}|>,
		<|"Match" -> "That blank screen", "Position" -> {{176, 192}}|>,
		<|"Match" -> "with the screen", "Position" -> {{480, 494}}|>
		},
	"TestID" -> "LexicalCases-DocExamples-WordToken-Test1"
	]

TestCreate[
	LexicalCases[$SampleParagraph, WordToken[2, 4] ~~ " screen"]["Data"],
	{
		<|"Match" -> "sat with a blank screen", "Position" -> {{135, 157}}|>,
		<|"Match" -> "That blank screen", "Position" -> {{176, 192}}|>,
		<|"Match" -> "would end with the screen", "Position" -> {{470, 494}}|>
		},
	"TestID" -> "LexicalCases-DocExamples-WordToken-Test2"
	]

TestCreate[
	Length[
		Join@*Flatten@{
			LexicalCases[$SampleParagraph, WordToken[2] ~~ " screen"]["Data"],
			LexicalCases[$SampleParagraph, WordToken[3] ~~ " screen"]["Data"],
			LexicalCases[$SampleParagraph, WordToken[4] ~~ " screen"]["Data"]
			}
		] === (
			DeleteDuplicates/*Length@LexicalCases[$SampleParagraph, WordToken[2, 4] ~~ " screen", Overlaps -> True]["Data"]
		),
		True,
	"TestID" -> "LexicalCases-DocExamples-WordToken-Test3"
	]

(* Documentation examples - OptionalToken *)


TestCreate[
	LexicalCases["this is a cool string. this is a string.", "this is a" ~~ OptionalToken["cool"] ~~ "string"]["Data"],
	{
		<|"Match" -> "this is a cool string", "Position" -> {{1, 21}}|>,
		<|"Match" -> "this is a string", "Position" -> {{24, 39}}|>
		},
	"TestID" -> "LexicalCases-DocExamples-OptionalToken-Test1"
	]

(* Documentation examples - Sandwich *)
TestCreate[
	LexicalCases[$SampleParagraph, Sandwich[WordToken[2, "KeepContractions"], " he "]]["Data"],
	{
  		<|"Match" -> "but here he sat with", "Position" -> {{123, 142}}|>,
  		<|"Match" -> "understand why he couldn't even", "Position" -> {{266, 296}}|>,
  		<|"Match" -> "eight hours he was prepared", "Position" -> {{404, 430}}|>
   		},
	"TestID" -> "LexicalCases-DocExamples-Sandwich-Test1"
	]

TestCreate[
	LexicalCases["a nice car is good.", Sandwich[w : WordToken[1], BoundToken["car"]] :> w],
	LexicalCases["a nice car is good.", Sandwich[w : WordToken[1], BoundToken["car"]] :> w],
	"TestID" -> "LexicalCases-DocExamples-Sandwich-Test2"
	]

(* Documentation examples - TextType *)
TestCreate[
	LexicalCases[$SampleParagraph, adjective : TextType["Adjective"] ~~ " screen" :> adjective]["Data"],
	{<|"Match" -> "blank", "Position" -> {{146, 157}, {181, 192}}|>},
	"TestID" -> "LexicalCases-DocExamples-TextType-Test1"
	]

TestCreate[
	LexicalCases[$SampleParagraph, adjectiveOrDeterminer : TextType["Adjective" | "Determiner"] ~~ " screen" :> adjectiveOrDeterminer]["Data"],
	{
		<|"Match" -> "blank", "Position" -> {{146, 157}, {181, 192}}|>,
		<|"Match" -> "the", "Position" -> {{485, 494}}|>
		},
	"TestID" -> "LexicalCases-DocExamples-TextType-Test2"
	]

(* FILES *)

(* TestCreate[
	LexicalCases[$SampleFile, $SampleStringExpression]["Data"],
	[[RESULT]],
	"TestID" -> "ShortStringTest1"
	] *)

(* SearchIndexObjects *)


(* TestCreate[
	LexicalCases[$SampleSearchIndexObject, $SampleStringExpression]["Data"],
	[[RESULT]],
	"TestID" -> "ShortStringTest1"
	] *)


(* WIKIPEDIA - should wiki tests be in a separate file?  *)


(* TestCreate[
	LexicalCases[$SampleWikiQuery, $SampleWikiExpression]["Data"],
	[[RESULT]],
	"TestID" -> "ShortStringTest1"
	] *)

EndTestSection[]
