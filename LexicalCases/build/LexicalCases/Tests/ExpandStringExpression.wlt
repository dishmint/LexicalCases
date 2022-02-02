BeginTestSection["ExpandStringExpression"]
(* ExpandStringExpression *)
VerificationTest[
	ExpandStringExpression[$SampleStringLong, StringExpression[Bounded["computer" | "computers"], TextType["Verb"]]],
		StringExpression[
			Alternatives[WordBoundary," ",StartOfString,StartOfLine],Alternatives["computer","computers"], Alternatives[WordBoundary," ",EndOfString,EndOfLine],
			Alternatives[WordBoundary," ",StartOfString,StartOfLine],
			Alternatives[
				"is","can","be","programmed","carry","perform","known","enable",
				"includes","operating","needed","used","may","refer","are","linked",
				"function","use","included","links","were","meant","have","aided",
				"doing","built","automate","guiding","did","specialized","calculating",
				"developed","followed","integrated","leading","been","increasing",
				"counts","predicted","consists","carries","change","stored","include",
				"allow","retrieved","saved"
				],
			Alternatives[WordBoundary," ",EndOfString,EndOfLine]
			],
	"TestID" -> "ExpandStringExpressionTest1"
]
EndTestSection[]
