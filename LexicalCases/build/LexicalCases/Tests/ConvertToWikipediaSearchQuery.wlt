BeginTestSection["ConvertToWikipediaSearchQuery"]

VerificationTest[(* 1 *)
	ConvertToWikipediaSearchQuery[$SampleLexicalPattern]
	,
	"key lime pie"
	,
	"TestID" -> "ConvertToWikipediaSearchQueryTest1"
]

VerificationTest[
	ConvertToWikipediaSearchQuery[LexicalPattern[TextType["Determiner"], "king" | "queen"]],
	{"king", "queen"},
	"TestID" -> "ConvertToWikipediaSearchQueryTest2"
]

EndTestSection[]
