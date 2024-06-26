BeginTestSection["LexicalStructure"]
(* LexicalStructure *)
TestCreate[
	LexicalStructure[StringExpression["computer" | "computers", TypeToken["Verb"]]],
	TextElement[
		List[
			TextElement[
				List[
					Alternatives[TextElement[List["computer"],Association[Rule["GrammaticalUnit","Text"]]],TextElement[List["computers"],Association[Rule["GrammaticalUnit","Text"]]]]
					],
				Association[Rule["GrammaticalUnit","Alternatives"]]
				],
			TextElement[List["Verb"],Association[Rule["GrammaticalUnit","TypeToken"]]]
			],
		Association[Rule["GrammaticalUnit","StringExpression"]]
	]
,
"TestID" -> "LexicalStructureTest1"
]
EndTestSection[]
