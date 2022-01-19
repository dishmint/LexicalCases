BeginTestSection["ToWikipediaSearchQuery"]
VerificationTest[(* 1 *)
	ToWikipediaSearchQuery[$SampleStringExpression]
	,
	"key lime pie"
	,
	"TestID" -> "ToWikipediaSearchQueryTest1"
]

VerificationTest[
	ToWikipediaSearchQuery[StringExpression[TextType["Determiner"], "king" | "queen"]],
	{"king", "queen"},
	"TestID" -> "ToWikipediaSearchQueryTest2"
]

EndTestSection[]
