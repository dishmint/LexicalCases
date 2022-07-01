BeginPackage["LexicalCases`Validation`"]

Begin["`Private`"]

$ValidLexicalTokens = (_TextType|_OptionalToken|_BoundToken|_WordToken)
extractLexicalTokens[expr_] := Cases[expr, $ValidLexicalTokens, {0, Infinity}];

ValidateLexicalToken[TextType[_String]] := True
ValidateLexicalToken[TextType[a_Alternatives]] := AllTrue[StringQ][List @@ a]
ValidateLexicalToken[OptionalToken[a_Alternatives]] := AllTrue[LexicalCases`LexicalPatternQ][List @@ a]
ValidateLexicalToken[OptionalToken[opt_]] := LexicalCases`LexicalPatternQ[opt]
ValidateLexicalToken[BoundToken[a_Alternatives]] := AllTrue[LexicalCases`LexicalPatternQ][List @@ a]
ValidateLexicalToken[BoundToken[e:Except[_Alternatives]]] := LexicalCases`LexicalPatternQ[e]
ValidateLexicalToken[WordToken[n_Integer]] := True
ValidateLexicalToken[WordToken[m_Integer, n_Integer]] := True
ValidateLexicalToken[expr_] := (Message[LexicalCases`LexicalCases::invld, expr];False)

LexicalCases`LexicalCases::invld = "`1` is not a valid lexical token"

SetAttributes[LexicalCases`LexicalPatternQ, HoldAll]

LexicalCases`LexicalPatternQ[expr_]:= Module[
	{
		se = Replace[expr, $ValidLexicalTokens :> " ", Infinity],
		lt = extractLexicalTokens[expr]
		},
	Check[GeneralUtilities`StringPatternQ[se] \[Or] AllTrue[ValidateLexicalToken, lt], $Failed]
		];
LexicalCases`LexicalPatternQ[Rule[expr_?LexicalCases`LexicalPatternQ,_]]:= True;
LexicalCases`LexicalPatternQ[RuleDelayed[expr_?LexicalCases`LexicalPatternQ,_]]:= True;
LexicalCases`LexicalPatternQ[expr_?GeneralUtilities`StringPatternQ]:= True;

End[]

EndPackage[]