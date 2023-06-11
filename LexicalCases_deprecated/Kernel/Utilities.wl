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

DebugTiming::usage = "DebugTiming[label][expr] applies EchoTiming to expr prepending label."

Begin["`Private`"]

DebugTiming[label_String][expr_]:= EchoTiming[expr,label]

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

DualSubsetMap[f1_, f2_, list_, pos_] := Module[
  {apos, l1},
  apos = DeleteCases[Range[Length[list]], Alternatives @@ pos];
  l1 = SubsetMap[Map[f1], list, pos];
  SubsetMap[Map[f2], l1, apos]
  ]

LexicalCases`ToLexicalPattern[string_String]:= LexicalCases`ToLexicalPattern[string] = Block[
	{structure, components},
	structure = Normal@TextStructure[string, "PartsOfSpeech"];
	components = Cases[structure, _[_String, {"GrammaticalUnit" -> Entity["GrammaticalUnit", gu_]}] :> LexicalCases`TextType[gu], Infinity];
	StringExpression @@ components
  ]

LexicalCases`ToLexicalPattern[string_String, pos_List]:= LexicalCases`ToLexicalPattern[string, pos] = Block[
  {
   structure = Normal[TextStructure[string, "PartsOfSpeech"]],
   components,
   keypos, newpos, mapped
   },
  components = Cases[structure, TextElement[s_String, {"GrammaticalUnit" -> e : Entity["GrammaticalUnit", _]}] :> {s -> e["TagName"]}, Infinity];
  keypos = Position[components, Alternatives @@ Cases[pos, _String]] // Extract[{All, 1}];
  newpos = Union[Cases[pos, _Integer]~Join~keypos];
  mapped =  DualSubsetMap[First/*Keys, First/*Values/*LexicalCases`TextType, components, newpos];
  StringExpression@@mapped
  ]

End[]
EndPackage[]
