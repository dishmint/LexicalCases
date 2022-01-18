BeginTestSection["ConvertToWikipediaSearchQuery"]

VerificationTest[(* 1 *)
	ConvertToWikipediaSearchQuery[$SampleStringExpression]
	,
	"key lime pie"
	,
	"TestID" -> "ConvertToWikipediaSearchQueryTest1"
]

VerificationTest[
	ConvertToWikipediaSearchQuery[StringExpression[TextType["Determiner"], "king" | "queen"]],
	{"king", "queen"},
	"TestID" -> "ConvertToWikipediaSearchQueryTest2"
]

EndTestSection[]
