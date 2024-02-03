BeginPackage["FaizonZaman`LexicalCases`Abstractions`"]
Begin["`Private`"]

FaizonZaman`LexicalCases`LexicalMap::inva = "Lexical pattern expected instead of `1`"
FaizonZaman`LexicalCases`LexicalMap[f_, source_String, lp_?FaizonZaman`LexicalCases`LexicalPatternQ] := 
    StringReplace[source, FaizonZaman`LexicalCases`LexicalPattern[match:lp :> f[match]]]

FaizonZaman`LexicalCases`LexicalMap[f_, source_String, any_] := 
    Failure[
        "InvalidArgument",
        <|
            "MessageTemplate" -> FaizonZaman`LexicalCases`LexicalMap::inva,
            "MessageParameters" -> {any}
        |>
    ]

End[]
EndPackage[]