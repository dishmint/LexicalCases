BeginTestSection["ContentAssociation"]

VerificationTest[(* 1 *)
	ContentAssociation[$SampleStringShort, $SampleLexicalPattern]
	,
	Association["Adjective"->"best"|"key"]
	,
	TestID->"ContentAssociationTest1"
]

EndTestSection[]
