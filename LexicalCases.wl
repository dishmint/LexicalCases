(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Extract and analyze text type sequences with the Wolfram Language.*)


BeginPackage["LexicalCases`"]
(* Main *)
LexicalCases::usage="LexicalCases[source, texlpatt] gives the text sequences in source that match the text pattern texlpatt."

(* LexicalPatterns *)
LexicalPattern::usage="LexicalPattern[t1, t2, ...] is a LexicalPattern object matching (t1, t2, ...) in the fixed order given."
OptionalLexicalPattern::usage="OptionalLexicalPattern[lp] matches 0 or 1 instances of lp"
LexicalPatternToStringExpression::usage="LexicalPatternToStringExpression[source, lp] Converts lexical pattern lp to a StringExpression"
TextType::usage="TextType[type] is a LexicalPattern object representing a type of text content."

ValidLexicalPatternQ::usage="ValidLexicalPatternQ[input] Returns True if input is a valid LexicalPattern"
ToTextElementStructure::usage="ToTextElementStructure[lp] renders a LexicalPattern using TextElements"

ExpandLexicalPattern::usage="ExpandLexicalPattern[t] Generate a RegularExpression template from a LexicalPattern"
ContentAssociation::usage="ContentAssociation[source, t] generates an association where a text contetype is the key, and examples from the source text of the content type are the values."
LexicalSummary::usage ="Represents the results of LexicalCases. Use the \"Properties\" subvalue for a list of properties."
ConvertToWikipediaSearchQuery::usage="For development purposes only, convert LexicalPattern to WikipediaSearch query strings"
$LexicalCasesSupportedServices::usage="List of supported services"
$LexicalPatternValidHeads::usage="List of symbols LexicalPattern supports"
Begin["Private`"]

(* Utility *)
OptionsJoin[sym__Symbol]:=(Map[Options]/*Apply[Join])[{sym}]

Listify[x_List] := x;
Listify[x_] := {x};

AlternativesToList[s_String] := {s};
AlternativesToList[a_Alternatives] := List@@a;

PrependArticleKey[{article_String, data_List}] := Map[Apply[Prepend]]@Thread[{data, "Article" -> article}]
PrependArticleKey[{article_String, data_Missing}] := <|"Article" -> article, "Match" -> data|>

ReplaceEmptyListWithMissing[result_]:=Replace[result, {} -> Missing["MatchNotFound"], Infinity];

(* Validate LexicalPattern Objects *)
ExtractHeads[expr_] := Cases[expr, h_[___] :> h, {0, Infinity}]
$LexicalPatternValidHeads = {
	LexicalPattern, LexicalPatternSequence, OptionalLexicalPattern, OrderlessLexicalPattern, TextType,
	Pattern, Except, Repeated, RepeatedNull,Blank, BlankSequence, BlankNullSequence,
	Alternatives, Rule, RuleDelayed, RegularExpression,
	LetterCharacter, WordCharacter, PunctuationCharacter, WhitespaceCharacter, DigitCharacter, HexadecimalCharacter,
	NumberString, StartOfString, EndOfString, WordBoundary,
	StartOfLine, EndOfLine
 }

ValidLexicalPatternQ[input_LexicalPattern]:= With[{heads = DeleteDuplicates@ExtractHeads[input]}, ContainsOnly[heads, $LexicalPatternValidHeads]]
ValidLexicalPatternQ[Rule[input_LexicalPattern,_]]:= ValidLexicalPatternQ[input]
ValidLexicalPatternQ[RuleDelayed[input_LexicalPattern,_]]:= ValidLexicalPatternQ[input]

(* Format LexicalPatterns for strings *)
TextElementFormat[s_String] := s
TextElementFormat[re_RegularExpression] := ToString[re]
TextElementFormat[a_Alternatives] := TextElement[ToString[a], <|"GrammaticalUnit" -> "Alternatives"|>]
TextElementFormat[LexicalPattern[args__]] :=TextElement[Map[TextElementFormat]@{args}, <|"GrammaticalUnit" -> "LexicalPattern"|>];
TextElementFormat[LexicalPatternSequence[args__]] :=TextElement[Map[TextElementFormat]@{args}, <|"GrammaticalUnit" -> "Sequence"|>];
TextElementFormat[OrderlessLexicalPattern[args__]] :=TextElement[Map[TextElementFormat]@{args}, <|"GrammaticalUnit" -> "Orderless"|>];
TextElementFormat[OptionalLexicalPattern[args__]] :=TextElement[Map[TextElementFormat]@{args}, <|"GrammaticalUnit" -> "Optional"|>];
TextElementFormat[TextType[type_String]] :=TextElement[type, <|"GrammaticalUnit" -> "TextType"|>];
TextElementFormat[TextType[type_RegularExpression]] :=TextElement[ToString[type], <|"GrammaticalUnit" -> "TextType"|>];
TextElementFormat[x_[arg1_,args___]] := TextElement[Map[TextElementFormat]@{arg1}, <|"GrammaticalUnit" -> ToString[x]|>]
TextElementFormat[sym_Symbol] := ToString[sym]
TextElementFormat[x_] := x

ToTextElementStructure[lp_LexicalPattern] := TextElementFormat[lp];
ToTextElementStructure[(Rule|RuleDelayed)[lp_LexicalPattern,_]] := TextElementFormat[StripNamedPattern@lp];

ExpandLexicalPattern[lp_LexicalPattern] := ReplaceAll[lp, {
	LexicalPattern -> StringExpression,
	OrderlessLexicalPattern -> Function[Alternatives@@Map[Apply[PatternSequence]][Permutations[{##}]]],
	OptionalLexicalPattern[opt_Alternatives] :> (opt~Join~Alternatives[""]),
	OptionalLexicalPattern[opt_] :> (Alternatives[opt]~Join~Alternatives[""])
	}]

TextContentGroup[List[content_String]] := content;
TextContentGroup[content_List] := Alternatives@@content;
ExtractContentTypes[lp_LexicalPattern] := Cases[lp, TextType[type_] :> type, Infinity];
ContentAssociation[sourcetext_String, lp_LexicalPattern] := Map[DeleteDuplicates /* TextContentGroup][TextCases[sourcetext, ExtractContentTypes[lp]]]
EscapePunctuation[s_String] := StringReplace[s, pc : PunctuationCharacter :> "\\" <> pc]

ContainsPatternHeadsQ[lp_] := ContainsAny[ExtractHeads[lp], {Pattern}]
StripNamedPattern[lp_] := StripNames[ContainsPatternHeadsQ[lp], lp]
StripNames[True, lp_LexicalPattern] := Replace[lp, p_Pattern :> Extract[2][p], Infinity]
StripNames[True, (Rule|RuleDelayed)[lp_LexicalPattern,_]] := Replace[lp, p_Pattern :> Extract[2][p], Infinity]
StripNames[False, lp_]:= lp

LexicalPatternToStringExpression[sourcetext_String, lp_LexicalPattern] :=
	Module[{TRX, CA},
		TRX = ExpandLexicalPattern[lp];
		CA  = ContentAssociation[sourcetext, lp];
		Replace[TRX, TextType[type_] :> CA[type], Infinity]
		]

LexicalPatternToStringExpression[sourcetext_String, Rule[lp_LexicalPattern, expr_]] := Rule[LexicalPatternToStringExpression[sourcetext, lp], expr]
LexicalPatternToStringExpression[sourcetext_String, RuleDelayed[lp_LexicalPattern, expr_]] := RuleDelayed[LexicalPatternToStringExpression[sourcetext, lp], expr]


WikipediaSearchQuery[List[], lp_LexicalPattern] := Message[ConvertToWikipediaSearchQuery::novq, lp]
WikipediaSearchQuery[wsq:List[__String], lp_LexicalPattern] := StringRiffle[wsq]
WikipediaSearchQuery[wsq:List[List[__String]], lp_LexicalPattern] := Flatten[wsq]
WikipediaSearchQuery[wsq_List, lp_LexicalPattern] := Cases[List[(_List|_String)..]][wsq] // Map[StringRiffle]

ConvertToWikipediaSearchQuery[lp_LexicalPattern]:= Module[
	{cleanLexicalPattern, stage1},
	cleanLexicalPattern = DeleteCases[List@@lp, (_TextType | _OptionalLexicalPattern | _OrderlessLexicalPattern), All];
	stage1 = ReplaceAll[cleanLexicalPattern, {Alternatives -> List}]//DeleteCases[" "];
	Check[WikipediaSearchQuery[stage1, lp] // StringReplace[(" " ..) -> " "] // StringTrim, Return[$Failed, Module]]
	]

ConvertToWikipediaSearchQuery[Rule[lp_LexicalPattern,_]] := ConvertToWikipediaSearchQuery[StripNamedPattern[lp]]
ConvertToWikipediaSearchQuery[RuleDelayed[lp_LexicalPattern,_]] := ConvertToWikipediaSearchQuery[StripNamedPattern[lp]]


ConvertToWikipediaSearchQuery::novq = "Keyword formulation not supported for the pattern ``. Consider using the \"Content\" option to supply keyaords, or trying a different LexicalPattern."


(* Input Handlers *)
Options[LexicalCases]={
	"Service" -> "Wikipedia"
};
Options[LexicalCasesWikipedia] = {
	MaxItems -> 50,
	Language -> "English"
};

$LexicalCasesSupportedServices = {"Wikipedia"}
(* SourceText and LexicalPattern Input *)
LexicalCases[sourcetext_String, lpatt_?ValidLexicalPatternQ]:= Module[
	{lpC},
	(* Find Matches *)
	lpC = Monitor[
		LexicalCasesOnString[sourcetext, lpatt],
		Row[{Style["Searching", Bold], ProgressIndicator[Appearance->"Ellipsis"], "\n", ToTextElementStructure[StripNamedPattern@lpatt]}]
		];
	(* Generate Summary Object *)
	Monitor[
		generateLexicalSummary[lpC, "Text", lpatt],
		Row[{"Generating LexicalSummary", ProgressIndicator[Appearance->"Necklace"]}]
	]
	]

(* SourceText is a string *)
LexicalCasesOnString[source_String, lp_?ValidLexicalPatternQ]:=Module[
	{RX, S = EscapePunctuation[source]},
	RX = LexicalPatternToStringExpression[S, lp];
	Map[AssociationThread[{"Match", "Position"} -> #] &]@With[
		{cases = DeleteDuplicates@StringCases[source, RX]},
		Thread[{cases, Map[StringPosition[source, #] &][cases]}]
		]
	]

(* LexicalPattern on Service *)
LexicalCases[lpatt_?ValidLexicalPatternQ, opts:OptionsPattern[{LexicalCases, LexicalCasesWikipedia}]]:= LexicalCasesFromService[OptionValue["Service"], lpatt, FilterRules[{opts}, Options[LexicalCasesWikipedia]]]

(* WikiQueryRyle and LexicalPattern Input *)
LexicalCases[query_Rule, lpatt_?ValidLexicalPatternQ, opts:OptionsPattern[{LexicalCases, LexicalCasesWikipedia}]]:= LexicalCasesFromService[OptionValue["Service"], query, lpatt, FilterRules[{opts}, Options[LexicalCasesWikipedia]]]

Options[LexicalCasesFromService]={

};

(*
TODO: Add relevant services from the list below
	{"ArXiv","CrossRef","Dropbox","Facebook","GoogleCustomSearch","Instagram","OpenLibrary","PubMed""Reddit""SurveyMonkey","Twilio","Twitter","Wikipedia"}
	*)

GetArticlesFromService["Wikipedia", query_, opts___] := Module[
	{ART, ARC, MTL},
	ART = Monitor[
		WikipediaArticlesFromRule[query, FilterRules[{opts}, OptionsJoin[WikipediaSearch,LexicalCasesWikipedia]]],
		Row[{"Searching Wikipedia: ", StringRiffle[Listify[Values[query]], ", "], ProgressIndicator[Appearance->"Ellipsis"]}]
		];
	ARC = Length[ART];
	MTL = First@TakeLargestBy[StringLength,1][ART->"Value"];
	<|"Articles" -> ART, "ArticleCount" -> ARC, "ArticleCountString" -> ToString[ARC], "MaxTitleLength" -> MTL|>
	]

GetTextFromArticles["Wikipedia", articles_List, articleCount_Integer, articleCountString_String, maxTitleLength_Integer] := Module[
	{texts},
		SetSharedVariable[ArticleIndex];
		ArticleIndex=0;
		Monitor[
			ParallelMap[(++ArticleIndex;WikipediaData[#])&,articles],
			Row[{
				"Gathering text from "<>articleCountString<>" articles:\n",
				ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",
				Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]],
				Dynamic[If[ArticleIndex <= articleCount-1," Getting article "<>StringPadRight["\""<>articles[[ArticleIndex+1]]<>"\"",maxTitleLength],""]]
				}
				]
			]
	]

SearchForLexicalPattern[texts_List, lp_?ValidLexicalPatternQ, slp_?ValidLexicalPatternQ, articles_List, articleCount_Integer, maxTitleLength_Integer] := Module[
	{T = texts},
		ArticleIndex=0;
		SetSharedVariable[lp];
		Monitor[
			ParallelMap[(++ArticleIndex;LexicalCasesOnString[#, lp])&, T],
			Row[{
				Style["Searching for LexicalPattern:\n", Bold],
				ToTextElementStructure[slp], "\n",
				Dynamic[If[ArticleIndex <= articleCount-1, StringPadRight["\""<>articles[[ArticleIndex+1]]<>"\" ",maxTitleLength]," "]],"\n",
				ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]
				}]
			]
	
	]

PackageResults[matches_, articles_List, articleCount_Integer, maxTitleLength_Integer] := Module[
	{AMT, MAC},
		AMT = Monitor[Thread[{articles, ReplaceEmptyListWithMissing[matches]}], Row[{"Threading Articles with Matches ", ProgressIndicator[Appearance->"Ellipsis"]}]];
		
		SetSharedVariable[AMT];
		ArticleIndex=0;
		MAC = Monitor[
			ParallelMap[(++ArticleIndex;PrependArticleKey[#])&, AMT],
			Row[{
				"Generating Association for ",
				Dynamic[If[ArticleIndex <= articleCount-1, StringPadRight["\""<>articles[[ArticleIndex+1]]<>"\" ",maxTitleLength]," "]],"\n",
				ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]
				}]];
		Clear[ArticleIndex];
		Flatten[MAC]
	]

(* Search wikipedia when only a pattern is given *)
LexicalCasesFromService["Wikipedia", lp_?ValidLexicalPatternQ, opts:OptionsPattern[{LexicalCasesWikipedia}]]:= With[
	{wikiquery = StringTrim@ConvertToWikipediaSearchQuery[lp]},
	ProcessWikiQuery[wikiquery,lp,opts]
	]

LexicalCasesFromService["Wikipedia", query_Rule, lp_?ValidLexicalPatternQ, opts:OptionsPattern[{LexicalCasesWikipedia}]]:= ProcessWikiQuery[query,lp,opts]

ProcessWikiQuery[query_?FailureQ,___] := Return[query, With]
ProcessWikiQuery[query_Rule,lp_?ValidLexicalPatternQ, opts___] := Module[
	{data = LexicalCasesWikipedia[query, lp, FilterRules[{opts}, Options[LexicalCasesWikipedia]]]},
	generateLexicalSummary[data, "Wikipedia", lp]
	]

ProcessWikiQuery[query:(_String|_List),lp_?ValidLexicalPatternQ, opts___] := Module[
	{data = LexicalCasesWikipedia["Content" -> query, lp, FilterRules[{opts}, Options[LexicalCasesWikipedia]]]},
	generateLexicalSummary[data, "Wikipedia", lp]
	]

(* SourceText is a WikipediaSearch Query *)
LexicalCasesWikipedia[wikiquery_Rule, lp_?ValidLexicalPatternQ, opts:OptionsPattern[]]:= Module[
	{LP = lp, SLP = StripNamedPattern[lp], ard, art, arc, acs, mtl, src, mtc, matchesassoc, articlematchthread},
	(* 1 - Get Wikipedia Articles *)
	ard = GetArticlesFromService["Wikipedia", wikiquery, opts];
	
	art = ard["Articles"];
	arc = ard["ArticleCount"];
	acs = ard["ArticleCountString"];
	mtl = ard["MaxTitleLength"];
	
	(* 2 - Get Wikipedia article text *)
	src = GetTextFromArticles["Wikipedia", art, arc, acs, mtl];
	
	(* 3 - Search for LexicalPattern *)
	mtc = SearchForLexicalPattern[src, LP, SLP, art, arc, mtl];
	PackageResults[mtc, art, arc, mtl]
	]


WikipediaArticlesFromRule[rule:("Content" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule, Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,LexicalCasesWikipedia]]]
WikipediaArticlesFromRule[rule:("Category" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule,"CategoryArticles", Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,LexicalCasesWikipedia]]]



(* LexicalSummary *)
LexicalSummary /: MakeBoxes[obj : LexicalSummary[asc_?LexicalSummaryAscQ], form : (StandardForm | TraditionalForm)] :=
Module[{above, below},
	above = {(*example grid*)
		{BoxForm`SummaryItem[{"Source: ", asc["Source"]}]},
		{BoxForm`SummaryItem[{"MatchCount: ",asc["TotalMatchCount"]}]}
	};
	below = {
	};
	BoxForm`ArrangeSummaryBox[
		LexicalSummary,(*head*)
		obj,(*interpretation*)
		None,(*icon,use None if not needed*)
		above,(*always shown content*)
		below,(*expandable content*)
		form,
		"Interpretable" -> Automatic]
		];
LexicalSummaryAscQ[asc_?AssociationQ] := AllTrue[{"Data", "Source", "TotalMatchCount", "TextElementStructure"}, KeyExistsQ[asc, #] &]
LexicalSummaryAscQ[_] = False;

(* Direct Properties *)
LexicalSummary[asc_?LexicalSummaryAscQ]["Data"] := asc["Data"]
LexicalSummary[asc_?LexicalSummaryAscQ]["Source"] := asc["Source"]
LexicalSummary[asc_?LexicalSummaryAscQ]["Dataset"] := Dataset[asc["Data"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["TotalMatchCount"] := asc["TotalMatchCount"]
LexicalSummary[asc_?LexicalSummaryAscQ]["TextElementStructure"] := asc["TextElementStructure"]

(* Dataset Properties *)
LexicalSummary[asc_?LexicalSummaryAscQ]["PercentDataset"] := PercentDataset[LexicalSummary[asc]["MatchCountGroups"], asc["TotalMatchCount"]]
(* Dataset Properties Filtered *)
LexicalSummary[asc_?LexicalSummaryAscQ]["PercentDataset", n_Integer] := LexicalSummary[asc]["PercentDataset"][;;UpTo[n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PercentDataset", DeleteStopwords] := LexicalSummary[asc]["PercentDataset"][Select[Not@StopWordQ[#Matches]&]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PercentDataset", n_Integer, DeleteStopwords] := LexicalSummary[asc]["PercentDataset", n][Select[Not@StopWordQ[#Matches]&]]

(* Count Properties *)
LexicalSummary[asc_?LexicalSummaryAscQ]["Counts"] := GetDatasetCounts[LexicalSummary[asc]["Dataset"], asc["Source"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups"] := (LexicalSummary[asc]["Counts"] // CountGroups)
LexicalSummary[asc_?LexicalSummaryAscQ]["MatchCounts"] := (LexicalSummary[asc]["Counts"] // DeleteMissing[#, 1, 1] &)
LexicalSummary[asc_?LexicalSummaryAscQ]["MatchCountGroups"] := (LexicalSummary[asc]["MatchCounts"] // CountGroups)
(* Count Properties Filtered *)
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups", n_Integer] := (LexicalSummary[asc]["CountGroups"][;;UpTo[n]])
LexicalSummary[asc_?LexicalSummaryAscQ]["MatchCountGroups", n_Integer] := (LexicalSummary[asc]["MatchCountGroups"][;;UpTo[n]])
LexicalSummary[asc_?LexicalSummaryAscQ]["MatchCountGroups", DeleteStopwords] := (LexicalSummary[asc]["MatchCounts"] // FilterOutStopwordRows // CountGroups)
LexicalSummary[asc_?LexicalSummaryAscQ]["MatchCountGroups", n_Integer, DeleteStopwords] := (LexicalSummary[asc]["MatchCountGroups", DeleteStopwords][;;UpTo[n]])


LexicalSummary[asc_?LexicalSummaryAscQ]["Survey"] := GenerateDashboard[LexicalSummary[asc]]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", n_Integer] := GenerateDashboard[LexicalSummary[asc], n]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", DeleteStopwords] := GenerateDashboard[LexicalSummary[asc], DeleteStopwords]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", n_Integer, DeleteStopwords] := GenerateDashboard[LexicalSummary[asc], n, DeleteStopwords]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups"] := PartOfSpeechGroups[LexicalSummary[asc]["MatchCountGroups"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", n_Integer] := PartOfSpeechGroups[LexicalSummary[asc]["MatchCountGroups", n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", DeleteStopwords] := PartOfSpeechGroups[LexicalSummary[asc]["MatchCountGroups", DeleteStopwords]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", n_Integer,  DeleteStopwords] := PartOfSpeechGroups[LexicalSummary[asc]["MatchCountGroups", n, DeleteStopwords]]

LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups"] := WordStemGroups[LexicalSummary[asc]["MatchCountGroups"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups", n_Integer] := WordStemGroups[LexicalSummary[asc]["MatchCountGroups", n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups", DeleteStopwords] := WordStemGroups[LexicalSummary[asc]["MatchCountGroups", DeleteStopwords]]
LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups", n_Integer,  DeleteStopwords] := WordStemGroups[LexicalSummary[asc]["MatchCountGroups", n, DeleteStopwords]]

LexicalSummary[asc_?LexicalSummaryAscQ][invalidkey_] := asc[invalidkey]
LexicalSummary[asc_?LexicalSummaryAscQ]["Properties"] := {"Data","Dataset","Counts","CountGroups", "MatchCounts", "MatchCountGroups","PercentDataset","PartOfSpeechGroups", "WordStemCountGroups", "Source","TotalMatchCount","TextElementStructure", "Survey"}

generateLexicalSummary[data_?FailureQ, ___] := data
generateLexicalSummary[data_, sourceType_String, LexicalPattern_] := Module[
	{matchcount},
	matchcount = DeleteMissing[GetDatasetCounts[Dataset[data], sourceType], 1, 1][Total, "Count"];
	LexicalSummary[<|"Data" -> data, "Source" -> sourceType, "TotalMatchCount" -> matchcount, "TextElementStructure" -> ToTextElementStructure[StripNamedPattern@LexicalPattern] |>]
]

StopWordQ[s_String] := StringMatchQ[Alternatives @@ WordList["Stopwords"]][s]
StopWordQ[l : List[__String]] := AnyTrue[l, StopWordQ]

PercentDatasetQ[ds_Dataset] := (First /* Normal /* Keys /* ContainsAny[{"Matches"}])[ds]
FilterOutStopwordRows[ds_Dataset] := Select[\[Not]StopWordQ[#Match] &][ds]

PartOfSpeechKey[word_String] := ProcessWordData[WordData[word, "PartsOfSpeech"]]
ProcessWordData[w_WordData] := None
ProcessWordData[w_] := w

CountGroups[ds_Dataset] := (ds // Query[GroupBy[#Count&], KeyDrop["Count"] /* Values /* (Flatten[#,1]&)] // KeyValueMap[<|"CountGroup" -> #1, "Matches" -> #2|> &]//ReverseSortBy["CountGroup"])
GetDatasetCounts[ds_Dataset,"Text"] := ds[All, <|"Match" -> "Match", "Count" -> "Position" /* Length|>]
GetDatasetCounts[ds_Dataset, "Wikipedia"] := ds[GroupBy["Match"], KeyDrop[{"Article", "Match"}] /* Values] // KeyValueMap[<|"Match" -> #1, "Count" -> Length[#2]|> &]

PartOfSpeechGroups[ds_Dataset] := (ds[TextWords /* ToLowerCase /* Flatten /* DeleteStopwords /* DeleteDuplicates, "Matches"][AlphabeticSort][GroupBy[PartOfSpeechKey]][KeyDrop[None]] // KeySort) // KeyValueMap[<|"PartOfSpeech" -> #1, "Words" -> #2|>&]

GetWordStemCounts[ds_Dataset] := (ds[All, "Matches"] // Normal // Flatten // (StringSplit[#, WordBoundary]&) /* Flatten // DeleteCases[" "] // DeleteStopwords // ToLowerCase // WordStem // Counts)

WordStemGroups[ds_Dataset] := (GetWordStemCounts[ds] // KeyValueMap[<|"Stem" -> #1, "Count" -> #2|> &] // Dataset // ReverseSortBy[#Count &]) // Query[GroupBy[#Count &], KeyDrop["Count"] /* Values /* (Flatten[#, 1]&)] // KeyValueMap[<|"CountGroup" -> #1, "WordStem" -> #2|> &]

PercentDataset[ds_Dataset, matchcount_Integer] := (ds[All, <|"Percentage" ->Interpreter["Percent"]@*ToString@*PercentForm@*N@((#CountGroup Length[#Matches])/matchcount),"Matches" -> #Matches|> &][ReverseSortBy["Percentage"]])

GenerateDashboard[lps_LexicalSummary, params___] := With[
	{
		pdat = lps["PercentDataset",params] ,
		posg = lps["PartOfSpeechGroups", params],
		wdsg = lps["WordStemCountGroups", params]
		},
		DynamicModule[
			{tab="Dataset"},
				Column[{
					Panel[SetterBar[Dynamic[tab],{"Dataset","PartOfSpeechGroups", "WordStemCountGroups"}]],
					Dynamic[
						Switch[tab,
							"Dataset",pdat,
							"PartOfSpeechGroups", posg,
							"WordStemCountGroups", wdsg
							]]
					}]
			]
		]
		
End[]
EndPackage[]
