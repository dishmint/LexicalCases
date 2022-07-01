BeginTestSection["Validation"]
(* LexicalPattern Validation *)
VerificationTest[
	LexicalPatternQ[$SampleStringExpression],
	True
,
"TestID" -> "LexicalPatternQTest1"
]

VerificationTest[
	LexicalPatternQ[("great" | "weak") ~~ BoundToken["machine" | "machines"]],
	True
,
"TestID" -> "LexicalPatternQTest2"
]

VerificationTest[
	LexicalPatternQ["weak" ~~ BoundToken["machines"]],
	True
,
"TestID" -> "LexicalPatternQTest3"
]

VerificationTest[
	LexicalPatternQ["weak" ~~ BoundToken[RegularExpression["\\w+"]]],
	True
,
"TestID" -> "LexicalPatternQTest4"
]

VerificationTest[
	LexicalPatternQ["number" ~~ BoundToken[DigitCharacter]],
	True
,
"TestID" -> "LexicalPatternQTest5"
]

EndTestSection[]