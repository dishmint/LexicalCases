BeginTestSection[]

(* LexicalPatternStructure *)
VerificationTest[
	LexicalPatternStructure[LexicalPattern["computer" | "computers", TextType["Verb"]]],
	TextElement[
		List[
			TextElement[
				List[
					Alternatives[TextElement[List["computer"],Association[Rule["GrammaticalUnit","Text"]]],TextElement[List["computers"],Association[Rule["GrammaticalUnit","Text"]]]]
					],
				Association[Rule["GrammaticalUnit","Alternatives"]]
				],
			TextElement[List["Verb"],Association[Rule["GrammaticalUnit","TextType"]]]
			],
		Association[Rule["GrammaticalUnit","LexicalPattern"]]
	]
,
"TestID" -> "LexicalPatternStructureTest1"
]


EndTestSection[]
