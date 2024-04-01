(* Test Pattern Object behavior *)

BeginTestSection["Pattrerns"]

(* BoundToken *)
TestCreate[
	BoundToken["machine"],
	BoundToken["machine"],
    "TestID" -> "BoundToken-Test1"
]

TestCreate[
	BoundToken["machine"|"machines"],
	BoundToken["machine"|"machines"],
    "TestID" -> "BoundToken-Test2"
]

(* WordToken *)

TestCreate[
	WordToken[3],
	WordToken[3],
    "TestID" -> "WordToken-Test1"
]

TestCreate[
	WordToken[1,4],
	WordToken[1,4],
    "TestID" -> "WordToken-Test2"
]

(* OptionalToken *)

TestCreate[
	OptionalToken[TextType["Adjective"]],
	OptionalToken[TextType["Adjective"]],
    "TestID" -> "OptionalToken-Test1"
]

TestCreate[
	OptionalToken[adjective : TextType["Adjective"] ~~ "sentence"],
	OptionalToken[adjective : TextType["Adjective"] ~~ "sentence"],
    "TestID" -> "OptionalToken-Test2"
]

(* TextType *)

TestCreate[
	TextType["Adjective"],
	TextType["Adjective"],
    "TestID" -> "TextType-Test1"
]

TestCreate[
	TextType["Adjective"|"Noun"],
	TextType["Adjective"|"Noun"],
    "TestID" -> "TextType-Test2"
]

(* LexicalPattern *)

TestCreate[
	LexicalPattern["Alice " ~~ TextType["Verb"] ~~ TextType["Adverb"]],
	LexicalPattern["Alice " ~~ TextType["Verb"] ~~ TextType["Adverb"]],
    "TestID" -> "LexicalPattern-Test1"
]

(* ExpandPattern *)

TestCreate[
	ExpandPattern["", BoundToken[BoundToken["outer"], "inner"]],
	WordBoundary~~"outer"~~WordBoundary~~"inner"~~WordBoundary~~"outer"~~WordBoundary,
	"TestID" -> "ExpandPattern-BoundToken-Test1"
]

TestCreate[
	ExpandPattern["nice computer big", BoundToken[TextType["Adjective"|"Noun"]," computer "]],
	 (WordBoundary~~"nice"|"big"~~WordBoundary)|(WordBoundary~~Alternatives["computer"]~~WordBoundary)~~ " computer " ~~(WordBoundary~~"nice"|"big"~~WordBoundary)|(WordBoundary~~Alternatives["computer"]~~WordBoundary),
	"TestID" -> "ExpandPattern-BoundToken-Test2"
]

EndTestSection[]