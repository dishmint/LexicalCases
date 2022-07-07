(* Test Pattern Object behavior *)

BeginTestSection["Pattrerns"]

(* BoundToken *)
VerificationTest[
	BoundToken["machine"],
	BoundToken["machine"],
    "TestID" -> "BoundToken-Test1"
]

VerificationTest[
	BoundToken["machine"|"machines"],
	BoundToken["machine"|"machines"],
    "TestID" -> "BoundToken-Test2"
]

(* WordToken *)

VerificationTest[
	WordToken[3],
	WordToken[3],
    "TestID" -> "WordToken-Test1"
]

VerificationTest[
	WordToken[1,4],
	WordToken[1,4],
    "TestID" -> "WordToken-Test2"
]

(* OptionalToken *)

VerificationTest[
	OptionalToken[TextType["Adjective"]],
	OptionalToken[TextType["Adjective"]],
    "TestID" -> "OptionalToken-Test1"
]

VerificationTest[
	OptionalToken[adjective : TextType["Adjective"] ~~ "sentence"],
    "TestID" -> "OptionalToken-Test2"
]

(* TextType *)

VerificationTest[
	TextType["Adjective"],
	TextType["Adjective"],
    "TestID" -> "TextType-Test1"
]

VerificationTest[
	TextType["Adjective"|"Noun"],
	TextType["Adjective"|"Noun"],
    "TestID" -> "TextType-Test2"
]

(* Sandwich *)

VerificationTest[
	Sandwich[TextType["Adjective"|"Noun"], "computer"],
	(TextType["Adjective" | "Noun"] ~~ "computer" ~~ TextType["Adjective" | "Noun"]),
    "TestID" -> "Sandwich-Test1"
]

VerificationTest[
	Sandwich[OptionalToken[TextType["Adjective"|"Noun"]], "computer"],
	(OptionalToken["Adjective" | "Noun"] ~~ "computer" ~~ OptionalToken["Adjective" | "Noun"]),
    "TestID" -> "Sandwich-Test2"
]


(* LexicalPattern *)

VerificationTest[
	LexicalPattern["Alice" ~~ TextType["Verb"] ~~ TextType["Adverb"]],
	LexicalPattern["Alice" ~~ TextType["Verb"] ~~ TextType["Adverb"]],
    "TestID" -> "LexicalPattern-Test1"
]

EndTestSection[]