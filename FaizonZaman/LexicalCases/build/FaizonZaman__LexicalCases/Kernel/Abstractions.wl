BeginPackage["FaizonZaman`LexicalCases`Abstractions`"]
Begin["`Private`"]

(* ------------------------------- LEXICALMAP ------------------------------- *)
FaizonZaman`LexicalCases`LexicalMap::inva = "Lexical pattern expected instead of `1`"
FaizonZaman`LexicalCases`LexicalMap[f_, source_String, lp_?FaizonZaman`LexicalCases`LexicalPatternQ] := 
    StringReplace[source, FaizonZaman`LexicalCases`LexicalPattern[(match:lp) :> f[match]]]

FaizonZaman`LexicalCases`LexicalMap[f_, source_String, any_] := 
    Failure[
        "InvalidArgument",
        <|
            "MessageTemplate" -> FaizonZaman`LexicalCases`LexicalMap::inva,
            "MessageParameters" -> {any}
        |>
    ]

(* ------------------------------ LEXIGRAMCOUNT ----------------------------- *)
optmax[i_Interval] := Max[i]
optmax[i_Integer] := i
countres[i_Interval] := i
countres[expr_]:= List/*Total@@expr
FaizonZaman`LexicalCases`LexigramCount::inva = "Lexical pattern expected instead of `1`"
FaizonZaman`LexicalCases`LexigramCount[lexicalPattern_?FaizonZaman`LexicalCases`LexicalPatternQ] := 
    ReplaceAll[
        lexicalPattern,
        {
        t : FaizonZaman`LexicalCases`TypeToken[_String] :> 1,
        FaizonZaman`LexicalCases`TypeToken[a_Alternatives] :> Length[a],
        FaizonZaman`LexicalCases`OptionalToken[args_] :> Interval[{0, FaizonZaman`LexicalCases`LexigramCount[args] // optmax}],
        FaizonZaman`LexicalCases`BoundToken[args_] :> FaizonZaman`LexicalCases`LexigramCount[args],
        FaizonZaman`LexicalCases`BoundToken[a_Alternatives] :> FaizonZaman`LexicalCases`LexigramCount[a],
        FaizonZaman`LexicalCases`WordToken[n_Integer] :> n,
        FaizonZaman`LexicalCases`WordToken[min_Integer, max_Integer] :> Interval[{min, max}],
        s_String :> StringSplit/*Length@s
        }
    ] // countres

FaizonZaman`LexicalCases`LexigramCount[any_] := 
    Failure[
        "InvalidArgument",
        <|
            "MessageTemplate" -> FaizonZaman`LexicalCases`LexigramCount::inva,
            "MessageParameters" -> {any}
        |>
    ]

End[]
EndPackage[]