(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Utility functions*)


BeginPackage["LexicalCases`Utilities`"]

OptionsJoin::usage = "OptionsJoin[sym_1, ..., sym_i] joins the Options of the sym_i"
ExtractHeads::usage = "ExtractHeads[expr] extracts all heads found in expr"
UnwrapAlternatives::usage = "UnwrapAlternatives[expr] if expr is a list of alternatives return the alternatives"
WrapAlternatives::usage = "WrapAlternatives[expr] if expr is an alternatives retun expr in a list"

GetFileExtension::usage = "GetFileExtension[file] extract \"FileExtension\" from Information[file]"
SupportedFileQ::usage = "SupportedFileQ[file] returns True if file is a plain text document"

MatchTrim::usage = "MatchTrim[boole, matches] trims white space from each match and updates the match positions if boole is True"

ReplaceEmptyListWithMissing::usage = "ReplaceEmptyListWithMissing[result] Replaces empty lists in result with Missing[\"NoMatches\"]"

Begin["`Private`"]

OptionsJoin[sym__Symbol]:=(Map[Options]/*Apply[Join])[{sym}]

(* expr operations *)

ExtractHeads[expr_] := Cases[expr, h_[___] :> h, {0, Infinity}]

UnwrapAlternatives[{expr_Alternatives}] := expr
UnwrapAlternatives[else_] := else

WrapAlternatives[expr_Alternatives] := {expr}
WrapAlternatives[else_] := else

GetFileExtension[file_File] := Information[file, "FileExtension"]
SupportedFileQ[file_File] := MemberQ[{"txt", "md", "csv", "tsv"}, GetFileExtension[file]]


(* match operations *)
startTrim[True] := 1
startTrim[False] := 0
endTrim[True] := -1
endTrim[False] := 0

TrimMatchPositions[m_String, psns : {{_, _} ..}] := Map[p |-> Through[{startTrim@*StringStartsQ[" "], endTrim@*StringEndsQ[" "]}[m]] +p][psns]

TrimMatchPositions[_,p_]:= p

consolidateMatches = Query[
	GroupBy[#Match &]/*(KeyValueMap[<|"Match" -> #1,"Position" -> #2|> &]),
	KeyDrop["Match"]/*Values/*(Flatten[#, 2] &)
	]

trimMatches = Query[
		All,
		<|"Match" -> StringTrim[#Match],"Position" -> (TrimMatchPositions[#Match, #Position])|> &
		]

MatchTrim[True, matches_List]:= consolidateMatches@trimMatches[matches]

MatchTrim[False, matches_List]:= matches

MatchTrim[boole:(True|False)][matches_List] := MatchTrim[boole,matches]

ReplaceEmptyListWithMissing[result_]:= Replace[result, {} -> Missing["NoMatches"], 1]

stopwords = Alternatives@@WordList["Stopwords"]

LexicalCases`StopWordQ[s_String] := StringMatchQ[stopwords][ToLowerCase[s]]

(* ToLexicalPattern *)

LexicalCases`ToLexicalPattern[string_String]:= Block[
	{structure, components},
	structure = Normal@TextStructure[string, "PartsOfSpeech"];
	components = Cases[structure, _[_String, {"GrammaticalUnit" -> Entity["GrammaticalUnit", gu_]}] :> LexicalCases`TextType[gu], Infinity];
	StringExpression @@ components
  ]

End[]
EndPackage[]
