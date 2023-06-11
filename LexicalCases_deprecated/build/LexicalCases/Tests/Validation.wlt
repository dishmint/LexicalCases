(* Test if a pattern is a lexical pattern *)

BeginTestSection["Validation"]

(* LexicalPattern Validation *)
VerificationTest[
	LexicalPatternQ[$SampleStringExpression],
	True,
	"TestID" -> "LexicalPatternQTest1"
]

(* LexicalPatternQ unvalued symbols *)
VerificationTest[
	LexicalPatternQ["Alice" ~~ xxx ~~ TextType["Adverb"]],
	Failure["ConfirmationFailed",
		<|
			"MessageTemplate" -> "``[``] did not return True.", 
			"MessageParameters" -> {GeneralUtilities`StringPatternQ, StringExpression["Alice", xxx, " "]},
			"ConfirmationType" -> "ConfirmBy",
			"Expression" -> StringExpression["Alice", xxx, " "],
			"Function" -> GeneralUtilities`StringPatternQ,
			"Information" -> StringForm["`1` contains invalid string patterns", StringExpression["Alice", xxx, " "]]
			|>
		],
	"TestID" -> "LexicalPatternQ-UnvaluedSymbols-Test1"
]

(* BoundToken *)
VerificationTest[
	LexicalPatternQ[("great" | "weak") ~~ BoundToken["machine" | "machines"]],
	True
,
"TestID" -> "LexicalPatternQ-BoundToken-Test1"
]

VerificationTest[
	LexicalPatternQ["weak" ~~ BoundToken["machines"]],
	True
,
"TestID" -> "LexicalPatternQ-BoundToken-Test2"
]

VerificationTest[
	LexicalPatternQ["weak" ~~ BoundToken[RegularExpression["\\w+"]]],
	True
,
"TestID" -> "LexicalPatternQ-BoundToken-Test3"
]

VerificationTest[
	LexicalPatternQ["number" ~~ BoundToken[DigitCharacter]],
	True
,
"TestID" -> "LexicalPatternQ-BoundToken-Test4"
]

VerificationTest[
	BoundToken["machine"],
	BoundToken["machine"]
,
"TestID" -> "BoundToken-Test1"
]

VerificationTest[
	BoundToken["machine"|"machines"],
	BoundToken["machine"|"machines"]
,
"TestID" -> "BoundToken-Test2"
]

VerificationTest[
	BoundToken["A" | WordToken[1]],
	BoundToken["A" | WordToken[1]]
,
"TestID" -> "BoundToken-Test3"
]

(* WordToken *)
VerificationTest[
	LexicalPatternQ["pattern" ~~ WordToken[1]],
    "TestID" -> "LexicalPatternQ-WordToken-Test1"
]

VerificationTest[
	LexicalPatternQ["number" ~~ WordToken[1,2]],
    "TestID" -> "LexicalPatternQ-WordToken-Test2"
]

VerificationTest[
	LexicalPatternQ["this" ~~ "is" ~~ WordToken[1, "KeepContractions"] ~~ "place"],
    "TestID" -> "LexicalPatternQ-WordToken-Test3"
]

(* OptionalToken *)
VerificationTest[
	LexicalPatternQ["this is a" ~~ OptionalToken[TextType["Adjective"]] ~~ "string"],
    "TestID" -> "LexicalPatternQ-OptionalToken-Test1"
]

VerificationTest[
	LexicalPatternQ["cool" ~~ OptionalToken["crazy"] ~~ "computer"],
    "TestID" -> "LexicalPatternQ-OptionalToken-Test2"
]

VerificationTest[
	LexicalPatternQ["this" ~~ OptionalToken[WordToken[1] ~~ TextType["Adjective"]] ~~ "place"],
    "TestID" -> "LexicalPatternQ-OptionalToken-Test3"
]

(* Sandwich *)
VerificationTest[
	LexicalPatternQ[Sandwich[WordToken[1], BoundToken["car"]]],
    "TestID" -> "LexicalPatternQ-Sandwich-Test1"
]

(* TextType *)
VerificationTest[
	LexicalPatternQ[adjective : TextType["Adjective"] ~~ "sentence" :> adjective],
    "TestID" -> "LexicalPatternQ-TextType-Test1"
]

EndTestSection[]