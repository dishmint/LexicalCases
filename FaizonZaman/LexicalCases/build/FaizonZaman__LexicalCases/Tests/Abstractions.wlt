(* Test if a pattern is a lexical pattern *)

BeginTestSection["Abstractions"]

(* LexicalMap *)
TestCreate[
	LexicalMap[ToUpperCase,"This is cool", TextType["Adjective"|"Verb"]],
	"This IS COOL",
	"TestID" -> "LexicalMapTest1"
]

TestCreate[
	LexicalMap[StringReverse/*ToUpperCase, "This is cool", TextType["Adjective" | "Verb"]],
	"This SI LOOC",
	"TestID" -> "LexicalMapTest2"
]

TestCreate[
	LexicalMap["**" <> # <> "**" &, "This is cool", TextType["Adjective" | "Verb"]],
	"This **is** **cool**",
	"TestID" -> "LexicalMapTest3"
]

TestCreate[ (* Wrapping with BoundToken breaks the above example  *)
	LexicalMap["**" <> # <> "**" &, "This is cool", BoundToken[TextType["Adjective" | "Verb"]]],
	"This **is** **cool**",
	"TestID" -> "LexicalMapTest4"
]

(* LexigramCount *)
TestCreate[
	LexigramCount["Alice " ~~ TextType["Verb"] ~~ WordToken[1]],
	3,
	"TestID" -> "LexigramCountTest1"
]

TestCreate[
	LexigramCount["Alice " ~~ TextType["Verb"] ~~ OptionalToken["away"] ~~ WordToken[1]],
	Interval[{3,4}],
	"TestID" -> "LexigramCountTest2"
]

TestCreate[
	LexigramCount[TextType["Adjective"] ~~ "hello" ~~ OptionalToken[TextType["Verb"]]],
	Interval[{2,3}],
	"TestID" -> "LexigramCountTest3"
]

TestCreate[
	LexigramCount[TextType["Adjective"] ~~ "hello" ~~ OptionalToken[TextType["Verb"] ~~ OptionalToken["there"]]],
	Interval[{2,4}],
	"TestID" -> "LexigramCountTest4"
]

EndTestSection[]