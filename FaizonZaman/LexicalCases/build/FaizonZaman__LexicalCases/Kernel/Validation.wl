BeginPackage["FaizonZaman`LexicalCases`Validation`"]

Begin["`Private`"]

$ValidLexicalTokens = (_FaizonZaman`LexicalCases`TextType|_FaizonZaman`LexicalCases`OptionalToken|_FaizonZaman`LexicalCases`BoundToken|_FaizonZaman`LexicalCases`WordToken|_FaizonZaman`LexicalCases`SynonymToken)
extractLexicalTokens[expr_] := Cases[expr, $ValidLexicalTokens, {0, Infinity}];

ValidateLexicalToken[FaizonZaman`LexicalCases`TextType[_String]] = True
ValidateLexicalToken[FaizonZaman`LexicalCases`TextType[a_Alternatives]] := AllTrue[StringQ][List @@ a]
ValidateLexicalToken[FaizonZaman`LexicalCases`OptionalToken[a_Alternatives]] := AllTrue[FaizonZaman`LexicalCases`LexicalPatternQ][List @@ a]
ValidateLexicalToken[FaizonZaman`LexicalCases`OptionalToken[opt_]] := FaizonZaman`LexicalCases`LexicalPatternQ[opt]
ValidateLexicalToken[FaizonZaman`LexicalCases`BoundToken[a_Alternatives]] := AllTrue[FaizonZaman`LexicalCases`LexicalPatternQ][List @@ a]
ValidateLexicalToken[FaizonZaman`LexicalCases`BoundToken[e:Except[_Alternatives]]] := FaizonZaman`LexicalCases`LexicalPatternQ[e]
ValidateLexicalToken[FaizonZaman`LexicalCases`BoundToken[outer_, inner_]] := AllTrue[FaizonZaman`LexicalCases`LexicalPatternQ][{outer, inner}]
ValidateLexicalToken[FaizonZaman`LexicalCases`WordToken[_Integer]] = True
ValidateLexicalToken[FaizonZaman`LexicalCases`WordToken[_Integer, _Integer]] = True
ValidateLexicalToken[FaizonZaman`LexicalCases`WordToken[_Integer,"KeepContractions"]] = True
ValidateLexicalToken[FaizonZaman`LexicalCases`WordToken[_Integer, _Integer, "KeepContractions"]] = True
ValidateLexicalToken[FaizonZaman`LexicalCases`SynonymToken[_String]] = True
ValidateLexicalToken[FaizonZaman`LexicalCases`SynonymToken[a_Alternatives]] := AllTrue[StringQ][List @@ a]
ValidateLexicalToken[expr_] := False

FaizonZaman`LexicalCases`LexicalCases::snvld = "`1` contains invalid string patterns"
FaizonZaman`LexicalCases`LexicalCases::pattc = "`1` contains multiple patterns witih the same name"
FaizonZaman`LexicalCases`LexicalCases::invld = "`1` contains invalid lexical patterns"
FaizonZaman`LexicalCases`LexicalCases::uvsym = "Symbols `1` are unvalued"

SetAttributes[FaizonZaman`LexicalCases`LexicalPatternQ, HoldAll]

FaizonZaman`LexicalCases`LexicalPatternQ[expr_]:= Module[
	{
		se = Replace[expr, $ValidLexicalTokens :> " ", {0, Infinity}],
		lt = extractLexicalTokens[expr]
	},
	Enclose[
		ConfirmBy[
			se,
			(Cases[#, p_Pattern :> First[p]] // Counts // Max // LessEqualThan[1])&,
			StringForm[FaizonZaman`LexicalCases`LexicalCases::pattc, se]
			];
		ConfirmBy[se, GeneralUtilities`StringPatternQ, StringForm[FaizonZaman`LexicalCases`LexicalCases::snvld, se]];
		ConfirmBy[lt, AllTrue[ValidateLexicalToken], StringForm[FaizonZaman`LexicalCases`LexicalCases::invld, expr]];
		True
	]
]
FaizonZaman`LexicalCases`LexicalPatternQ[Rule[expr_?FaizonZaman`LexicalCases`LexicalPatternQ,_]]:= True;
FaizonZaman`LexicalCases`LexicalPatternQ[RuleDelayed[expr_?FaizonZaman`LexicalCases`LexicalPatternQ,_]]:= True;

End[]

EndPackage[]