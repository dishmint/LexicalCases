(* Test if a pattern is a lexical pattern *)

BeginTestSection["Validation"]

(* LexicalPattern Validation *)
TestCreate[
	LexicalPatternQ[$SampleStringExpression],
	True,
	"TestID" -> "LexicalPatternQTest1"
]

(* LexicalPatternQ unvalued symbols *)
TestCreate[
	FailureQ[LexicalPatternQ["Alice" ~~ xxx ~~ TextType["Adverb"]]],
	True,
	"TestID" -> "LexicalPatternQ-UnvaluedSymbols-Test1"
]

(* BoundToken *)
TestCreate[
	LexicalPatternQ[("great" | "weak") ~~ BoundToken["machine" | "machines"]],
	True
,
"TestID" -> "LexicalPatternQ-BoundToken-Test1"
]

TestCreate[
	LexicalPatternQ["weak" ~~ BoundToken["machines"]],
	True
,
"TestID" -> "LexicalPatternQ-BoundToken-Test2"
]

TestCreate[
	LexicalPatternQ["weak" ~~ BoundToken[RegularExpression["\\w+"]]],
	True
,
"TestID" -> "LexicalPatternQ-BoundToken-Test3"
]

TestCreate[
	LexicalPatternQ["number" ~~ BoundToken[DigitCharacter]],
	True
,
"TestID" -> "LexicalPatternQ-BoundToken-Test4"
]

TestCreate[
	BoundToken["machine"],
	BoundToken["machine"]
,
"TestID" -> "BoundToken-Test1"
]

TestCreate[
	BoundToken["machine"|"machines"],
	BoundToken["machine"|"machines"]
,
"TestID" -> "BoundToken-Test2"
]

TestCreate[
	BoundToken["A" | WordToken[1]],
	BoundToken["A" | WordToken[1]]
,
"TestID" -> "BoundToken-Test3"
]

(* WordToken *)
TestCreate[
	LexicalPatternQ["pattern" ~~ WordToken[1]],
    "TestID" -> "LexicalPatternQ-WordToken-Test1"
]

TestCreate[
	LexicalPatternQ["number" ~~ WordToken[1,2]],
    "TestID" -> "LexicalPatternQ-WordToken-Test2"
]

TestCreate[
	LexicalPatternQ["this" ~~ "is" ~~ WordToken[1, "KeepContractions"] ~~ "place"],
    "TestID" -> "LexicalPatternQ-WordToken-Test3"
]

(* OptionalToken *)
TestCreate[
	LexicalPatternQ["this is a" ~~ OptionalToken[TextType["Adjective"]] ~~ "string"],
    "TestID" -> "LexicalPatternQ-OptionalToken-Test1"
]

TestCreate[
	LexicalPatternQ["cool" ~~ OptionalToken["crazy"] ~~ "computer"],
    "TestID" -> "LexicalPatternQ-OptionalToken-Test2"
]

TestCreate[
	LexicalPatternQ["this" ~~ OptionalToken[WordToken[1] ~~ TextType["Adjective"]] ~~ "place"],
    "TestID" -> "LexicalPatternQ-OptionalToken-Test3"
]

(* Sandwich *)
TestCreate[
	LexicalPatternQ[Sandwich[WordToken[1], BoundToken["car"]]],
    "TestID" -> "LexicalPatternQ-Sandwich-Test1"
]

(* TextType *)
TestCreate[
	LexicalPatternQ[adjective : TextType["Adjective"] ~~ "sentence" :> adjective],
    "TestID" -> "LexicalPatternQ-TextType-Test1"
]

EndTestSection[]