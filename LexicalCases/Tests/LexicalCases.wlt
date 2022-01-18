BeginTestSection["LexicalCases"]
(* Short Strings *)
VerificationTest[
	LexicalCases[$SampleStringShort, $SampleStringExpression]["Data"],
	{<|"Match" -> "best key lime pie", "Position" -> {{5, 21}}|>},
	"TestID" -> "ShortStringTest1"
	]
EndTestSection[]
