BeginPackage["FaizonZaman`LexicalCases`Abstractions`"]
Begin["`Private`"]

FaizonZaman`LexicalCases`LexicalMap::inva = "Lexical pattern expected instead of ``"
FaizonZaman`LexicalCases`LexicalMap[f_, source_String, lp_?LexicalPatternQ] := StringReplace[source, lp :> f[lp]]
FaizonZaman`LexicalCases`LexicalMap[f_, source_String, any_] := Message[LexicalMap::inva, any]

End[]
EndPackage[]