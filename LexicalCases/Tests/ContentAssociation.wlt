Needs["LexicalSamples`"]

BeginTestSection[]

	(* ContentAssociation *)
	VerificationTest[
		ContentAssociation[$SampleStringShort, $SampleLexicalPattern],
		Association["Adjective" \[Rule] "best" | "key"],
		"TestID" -> "ContentAssociationTest1"
	]

EndTestSection[]
