BeginTestSection["ContentAssociation"]
VerificationTest[(* 1 *)
	ContentAssociation[$SampleStringShort, $SampleStringExpression]
	,
	Association["Adjective"->"best"|"key"]
	,
	TestID->"ContentAssociationTest1"
]

EndTestSection[]
