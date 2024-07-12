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
	OptionalToken[TypeToken["Adjective"]],
	OptionalToken[TypeToken["Adjective"]],
    "TestID" -> "OptionalToken-Test1"
]

TestCreate[
	OptionalToken[adjective : TypeToken["Adjective"] ~~ "sentence"],
	OptionalToken[adjective : TypeToken["Adjective"] ~~ "sentence"],
    "TestID" -> "OptionalToken-Test2"
]

(* TypeToken *)

TestCreate[
	TypeToken["Adjective"],
	TypeToken["Adjective"],
    "TestID" -> "TypeToken-Test1"
]

TestCreate[
	TypeToken["Adjective"|"Noun"],
	TypeToken["Adjective"|"Noun"],
    "TestID" -> "TypeToken-Test2"
]

(* SynonymToken *)

TestCreate[
	SynonymToken["good"],
	SynonymToken["good"],
    "TestID" -> "SynonymToken-Test1"
]

TestCreate[
	SynonymToken["good"|"bad"],
	SynonymToken["good"|"bad"],
    "TestID" -> "SynonymToken-Test2"
]

(* LexicalPattern *)

TestCreate[
	LexicalPattern["Alice " ~~ TypeToken["Verb"] ~~ TypeToken["Adverb"]],
	LexicalPattern["Alice " ~~ TypeToken["Verb"] ~~ TypeToken["Adverb"]],
    "TestID" -> "LexicalPattern-Test1"
]

(* ExpandPattern *)

TestCreate[
	ExpandPattern["", BoundToken[BoundToken["outer"], "inner"]],
	WordBoundary~~"outer"~~WordBoundary~~"inner"~~WordBoundary~~"outer"~~WordBoundary,
	"TestID" -> "ExpandPattern-BoundToken-Test1"
]

TestCreate[
	ExpandPattern["nice computer big", BoundToken[TypeToken["Adjective"|"Noun"]," computer "]],
	("nice" | "big") | "computer" ~~ " computer " ~~ ("nice" | "big") | "computer",
	"TestID" -> "ExpandPattern-BoundToken-Test2"
]

EndTestSection[]