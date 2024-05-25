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
	FailureQ[LexicalPatternQ["Alice" ~~ xxx ~~ TypeToken["Adverb"]]],
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
	LexicalPatternQ[BoundToken["outer", "inner"]],
	True
,
"TestID" -> "LexicalPatternQ-BoundToken-Test5"
]

TestCreate[
	LexicalPatternQ[BoundToken[BoundToken["outer"], "inner"]],
	True
,
"TestID" -> "LexicalPatternQ-BoundToken-Test6"
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

TestCreate[
	BoundToken["outer", "inner"],
	BoundToken["outer", "inner"]
,
"TestID" -> "BoundToken-Test4"
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
	LexicalPatternQ["this is a" ~~ OptionalToken[TypeToken["Adjective"]] ~~ "string"],
    "TestID" -> "LexicalPatternQ-OptionalToken-Test1"
]

TestCreate[
	LexicalPatternQ["cool" ~~ OptionalToken["crazy"] ~~ "computer"],
    "TestID" -> "LexicalPatternQ-OptionalToken-Test2"
]

TestCreate[
	LexicalPatternQ["this" ~~ OptionalToken[WordToken[1] ~~ TypeToken["Adjective"]] ~~ "place"],
    "TestID" -> "LexicalPatternQ-OptionalToken-Test3"
]

(* BoundToken *)
TestCreate[
	LexicalPatternQ[BoundToken[WordToken[1], BoundToken["car"]]],
    "TestID" -> "LexicalPatternQ-BoundToken-Test1"
]

(* TypeToken *)
TestCreate[
	LexicalPatternQ[adjective : TypeToken["Adjective"] ~~ "sentence" :> adjective],
    "TestID" -> "LexicalPatternQ-TypeToken-Test1"
]

(* SynonymToken *)
TestCreate[
	LexicalPatternQ[SynonymToken["cool"] ~~ " sentence"],
    "TestID" -> "LexicalPatternQ-SynonymToken-Test1"
]

EndTestSection[]