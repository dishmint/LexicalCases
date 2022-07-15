BeginPackage["LexicalCases`Validation`"]

Begin["`Private`"]

$ValidLexicalTokens = (_LexicalCases`TextType|_LexicalCases`OptionalToken|_LexicalCases`BoundToken|_LexicalCases`WordToken)
extractLexicalTokens[expr_] := Cases[expr, $ValidLexicalTokens, {0, Infinity}];

ValidateLexicalToken[LexicalCases`TextType[_String]] := True
ValidateLexicalToken[LexicalCases`TextType[a_Alternatives]] := AllTrue[StringQ][List @@ a]
ValidateLexicalToken[LexicalCases`OptionalToken[a_Alternatives]] := AllTrue[LexicalCases`LexicalPatternQ][List @@ a]
ValidateLexicalToken[LexicalCases`OptionalToken[opt_]] := LexicalCases`LexicalPatternQ[opt]
ValidateLexicalToken[LexicalCases`BoundToken[a_Alternatives]] := AllTrue[LexicalCases`LexicalPatternQ][List @@ a]
ValidateLexicalToken[LexicalCases`BoundToken[e:Except[_Alternatives]]] := LexicalCases`LexicalPatternQ[e]
ValidateLexicalToken[LexicalCases`WordToken[n_Integer]] := True
ValidateLexicalToken[LexicalCases`WordToken[m_Integer, n_Integer]] := True
ValidateLexicalToken[LexicalCases`WordToken[n_Integer,"KeepContractions"]] := True
ValidateLexicalToken[LexicalCases`WordToken[m_Integer, n_Integer,"KeepContractions"]] := True
ValidateLexicalToken[expr_] := False

LexicalCases`LexicalCases::snvld = "`1` contains invalid string patterns"
LexicalCases`LexicalCases::invld = "`1` contains invalid lexical patterns"
LexicalCases`LexicalCases::uvsym = "Symbols `1` are unvalued"

SetAttributes[LexicalCases`LexicalPatternQ, HoldAll]

(* getUVS[expr_] := Cases[expr, s_Symbol /; ! ValueQ[s]]

$uvs = {}

LexicalCases`UnvaluedSymbolsFreeQ[expr_] := iuvsq[$uvs = getUVS[expr]]

iuvsq[{}] := True
iuvsq[_] := False *)

LexicalCases`LexicalPatternQ[expr_]:= Module[
	{
		se = Replace[expr, $ValidLexicalTokens :> " ", {0, Infinity}],
		lt = extractLexicalTokens[expr]
		},
		Enclose[
			ConfirmBy[se, GeneralUtilities`StringPatternQ, StringForm[LexicalCases`LexicalCases::snvld, se]];
			ConfirmBy[lt, AllTrue[ValidateLexicalToken], StringForm[LexicalCases`LexicalCases::invld, expr]];
			True
		]
		]
LexicalCases`LexicalPatternQ[Rule[expr_?LexicalCases`LexicalPatternQ,_]]:= True;
LexicalCases`LexicalPatternQ[RuleDelayed[expr_?LexicalCases`LexicalPatternQ,_]]:= True;
(* LexicalCases`LexicalPatternQ[expr_?GeneralUtilities`StringPatternQ]:= True; *)

End[]

EndPackage[]