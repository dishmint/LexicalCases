BeginTestSection["Utilities"]
(* ExtractHeads *)

VerificationTest[
    LexicalCases`Utilities`ExtractHeads[TextType["Adverb"] ~~ TextType["Adjective"] ~~ BoundToken["specie" | "species"]],
    {TextType, TextType, Alternatives, BoundToken, StringExpression},
    "TestID" -> "Utilities-ExtractHeads-Test1"
]

VerificationTest[
    LexicalCases`Utilities`ExtractHeads[TextType["Adjective" | "Determiner"] ~~ "screen"],
    {Alternatives, TextType, StringExpression},
    "TestID" -> "Utilities-ExtractHeads-Test2"
]

VerificationTest[
    LexicalCases`Utilities`ExtractHeads[$SampleStringExpression],
    {TextType, StringExpression},
    "TestID" -> "Utilities-ExtractHeads-Test3"
]

(* GetFileExtension *)
(* MatchTrim *)
(* OptionsJoin *)
(* ReplaceEmptyListWithMissing *)
(* SupportedFileQ *)
(* UnwrapAlternatives *)
(* WrapAlternatives *)
(* StopWordQ *)
(* ExpandPattern *)
(* ExpandPattern - Doc Examples *)

VerificationTest[
	ExpandPattern[$SampleParagraph, Sandwich[TextType["Adjective" | "Noun"], "computer"]],
	(
		(
			(WordBoundary | " " | StartOfString | StartOfLine)
			~~ ("past" | "few" | "blank" | "single")
			~~ ((WordBoundary | " " | EndOfString | EndOfLine) | (WordBoundary | " " | StartOfString | StartOfLine))
			~~ ("words" | "fingers" | "weeks" | "writer" | "block" | "screen" | "front" | "day" | "mind" | "type" | "word" | "process" | "hours" | "computer" | "today" | "blank")
			~~ (WordBoundary | " " | EndOfString | EndOfLine)
	 	)
		~~ "computer"
		~~ (
            (WordBoundary | " " | StartOfString |StartOfLine) ~~ ("past" | "few" | "blank" | "single")
		~~ ((WordBoundary | " " | EndOfString | EndOfLine) | (WordBoundary | " " | StartOfString | StartOfLine))
		~~ ("words" | "fingers" | "weeks" | "writer" | "block" | "screen" | "front" | "day" | "mind" | "type" | "word" | "process" | "hours" | "computer" | "today" | "blank")
		~~ (WordBoundary | " " | EndOfString | EndOfLine)
        )
	),
    "TestID" -> "Utlities-ExpandPattern-Test1"
]

VerificationTest[
	ExpandPattern["this is the best music ever.", TextType["Adjective"] ~~ "music"],
	(WordBoundary | " " | StartOfString | StartOfLine) ~~ Alternatives["best"] ~~ (WordBoundary | " " | EndOfString | EndOfLine) ~~ "music",
    "TestID" -> "Utlities-ExpandPattern-Test2"
]

(* LexicalPattern *)

VerificationTest[
	StringCases[$SampleParagraph, LexicalPattern[TextType["Adjective" | "Determiner"] ~~ "screen"]],
	{" blank screen", " blank screen", " the screen"},
    "TestID" -> "Utlities-LexicalPattern-Test1"
]

VerificationTest[
	StringCases[$SampleParagraph, LexicalPattern[TextType["Adjective" | "Determiner"] ~~ "screen"]],
	{" blank screen", " blank screen", " the screen"},
    "TestID" -> "Utlities-LexicalPattern-Test2"
]

VerificationTest[
	StringPosition[$SampleParagraph, LexicalPattern[TextType["Determiner"] ~~ TextType["Adjective"] ~~ "screen" ~~ TextType["Preposition" | "Verb"]]],
	{{143, 160}, {144, 160}, {175, 201}, {176, 201}},
    "TestID" -> "Utlities-LexicalPattern-Test3"
]

VerificationTest[
	StringMatchQ["Alice walked quickly", LexicalPattern["Alice" ~~ TextType["Verb"] ~~ TextType["Adverb"]]],
	True,
    "TestID" -> "Utlities-LexicalPattern-Test4"
]

EndTestSection[]