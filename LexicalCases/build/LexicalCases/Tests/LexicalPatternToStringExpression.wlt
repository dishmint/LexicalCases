BeginTestSection["LexicalPatternToStringExpression"]
(* LexicalPatternToStringExpression *)
VerificationTest[
	LexicalPatternToStringExpression[$SampleStringLong, LexicalPattern["computer" | "computers", " ", TextType["Verb"]]],
		StringExpression[
			Alternatives["computer","computers"]," ",
			Except[WordCharacter,Alternatives[WordBoundary," ",StartOfString,StartOfLine]],
			Alternatives[
				"is","can","be","programmed","carry","perform","known","enable","includes","operating","needed","used","may","refer","are","linked","function","use","included","links","were","meant","have",
				"aided","doing","built","automate","guiding","did","specialized","calculating","developed","followed","integrated","leading","been","increasing","counts","predicted","consists",
				"carries","change","stored","include","allow","retrieved","saved"
			],
			Except[WordCharacter,Alternatives[WordBoundary," ",EndOfString,EndOfLine]]
			],
	"TestID" -> "LexicalPatternToStringExpressionTest1"
]
EndTestSection[]
