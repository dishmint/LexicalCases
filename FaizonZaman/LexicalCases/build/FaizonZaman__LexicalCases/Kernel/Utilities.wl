(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Utility functions*)


BeginPackage["FaizonZaman`LexicalCases`Utilities`"]

OptionsJoin::usage = "OptionsJoin[sym_1, ..., sym_i] joins the Options of the sym_i"
ExtractHeads::usage = "ExtractHeads[expr] extracts all heads found in expr"
UnwrapAlternatives::usage = "UnwrapAlternatives[expr] if expr is a list of alternatives return the alternatives"
WrapAlternatives::usage = "WrapAlternatives[expr] if expr is an alternatives retun expr in a list"

StringExpressionToList::usage = "StringExpressionToList[expr] replaces heads StringExpression with List in expr"
ListToStringExpression::usage = "ListToStringExpression[expr] replaces heads List with StringExpression in expr"

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

TrimMatchPositions[m_String, psns : {{_, _} ..}] := Map[pos |-> Comap[{startTrim@*StringStartsQ[" "], endTrim@*StringEndsQ[" "]}][m] + pos][psns]

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

FaizonZaman`LexicalCases`StopWordQ[s_String] := StringMatchQ[stopwords][ToLowerCase[s]]

(* ToLexicalPattern *)

DualSubsetMap[f1_, f2_, list_, pos_] := Module[
  {apos, l1},
  apos = DeleteCases[Range[Length[list]], Alternatives @@ pos];
  l1 = SubsetMap[Map[f1], list, pos];
  SubsetMap[Map[f2], l1, apos]
  ]

riffleStringAndTypeToken[patt_] := FixedPoint[
	SequenceReplace[#, 
		{before_FaizonZaman`LexicalCases`TypeToken, s_String, after_FaizonZaman`LexicalCases`TypeToken} :> 
			Splice[{before, " ", s, " ", after}]
	] &,
	patt
]

riffleStrings[patt_] := FixedPoint[
	SequenceReplace[patt, 
	{before : Except[" "], s_String, after : Except[" "]} :> 
		Splice[Riffle[{before, s, after}, " "]]
	]&,
	patt
]

riffleTypes[patt_] := FixedPoint[
	SequenceReplace[#,
		{types__FaizonZaman`LexicalCases`TypeToken} :> 
			Splice[Riffle[{types}, " "]]
    ] &,
	patt
]


iToLexicalPattern[structure_, string_, None] := 
	StringExpression @@ ReplaceTokens[structure, string, All]
iToLexicalPattern[structure_, string_, spec_List] := 
	StringExpression @@ ReplaceTokens[structure, string, spec]

IndexTokens[s_String] := Block[
	{splits = StringSplit[s, {" " -> " ", p : PunctuationCharacter :> p}]},
	splits //= PositionIndex/*KeyValueMap[Splice[Thread[#2 -> #1]] &]/*KeySort
  ]

FaizonZaman`LexicalCases`ToLexicalPattern[string_String, preservationSpec_ : None] :=
	With[
		{structure = Normal[TextStructure[string, "PartsOfSpeech"]]},

		Block[
			{
				indexed = IndexTokens[string],
				rules = 
					Cases[
						structure, 
						TextElement[str_String, {"GrammaticalUnit" -> e : Entity["GrammaticalUnit", _]}] :>
							{str -> FaizonZaman`LexicalCases`TypeToken[StringDelete[e["TagName"], " "]]},
						Infinity
					] // Flatten
			},

			iToLexicalPattern[rules, indexed, preservationSpec]
		]
]

ReplaceTokens[struct_, indexed_Association, All] := indexed // ReplaceAll[struct]

TokenPattern[token_String] := FaizonZaman`LexicalCases`TypeToken[token]
TokenPattern[{token_String}] := FaizonZaman`LexicalCases`TypeToken[token]
TokenPattern[tokens_List] := FaizonZaman`LexicalCases`TypeToken[Alternatives @@ tokens]

ClearAll[replace]
replace[True, s_String, _] := s;
replace[False, " ", _] = " ";
replace[False, s_String, rules_] := s /. rules;

ReplaceTokens[struct_, indexed_Association, spec_List] := Block[
	{intpos, typepos},
	{intpos, typepos} = Lookup[{True,False}]@Merge[{GroupBy[spec, IntegerQ], <|True -> {}, False -> {}|>}, Flatten];

	indexed // KeyValueMap[replace[ MemberQ[#1][intpos] \[Or] MemberQ[ReplaceAll[struct]/*ReplaceAll[" "->{{}}]/*First@#2][typepos], #2, struct ]&]
]


StringExpressionToList[expr_StringExpression] := Replace[expr, StringExpression[args___] :> List[args], {0, Infinity}]
StringExpressionToList[expr_] := {expr}
ListToStringExpression[expr_] /; Not@*FreeQ[List]@expr := Replace[expr, List[args___] :> StringExpression[args], {0, Infinity}]

End[]
EndPackage[]
