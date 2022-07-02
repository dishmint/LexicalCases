BeginTestSection["LexicalCases"]

(* STRINGS *)
(* Short Strings *)
VerificationTest[
	LexicalCases[$SampleSentence, $SampleStringExpression]["Data"],
	{<|"Match" -> "best key lime pie", "Position" -> {{5, 21}}|>},
	"TestID" -> "ShortStringTest1"
	]

(* FILES *)
(* SearchIndexObjects *)
(* WIKIPEDIA - should wiki tests be in a separate file?  *)
EndTestSection[]
