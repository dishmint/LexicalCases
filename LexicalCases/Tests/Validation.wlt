BeginTestSection["Validation"]
(* LexicalPattern Validation *)
VerificationTest[
	LexicalPatternQ[$SampleStringExpression],
	True
,
"TestID" -> "LexicalPatternQTest1"
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
	LexicalPatternQ["pattern" ~~ OptionalToken["matching"]],
    "TestID" -> "LexicalPatternQ-WordToken-Test1"
]

VerificationTest[
	LexicalPatternQ["cool" ~~ OptionalToken["crazy"] ~~ "computer"],
    "TestID" -> "LexicalPatternQ-WordToken-Test2"
]

VerificationTest[
	LexicalPatternQ["this" ~~ "is" ~~ WordToken[1, "KeepContractions"] ~~ "place"],
    "TestID" -> "LexicalPatternQ-WordToken-Test3"
]



EndTestSection[]