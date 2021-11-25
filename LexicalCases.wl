(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Extract and analyze text type sequences with the Wolfram Language.*)


BeginPackage["LexicalCases`"]
(* Main *)
LexicalCases::usage="LexicalCases[source, textpatt] gives the text sequences in source that match the text pattern textpatt."

(* TextPatterns *)
TextPattern::usage="TextPattern[t1, t2, ...] is a TextPattern object matching (t1, t2, ...) in the fixed order given."
TextPatternSequence::usage="TextPatternSequence[t1, t2, ...] is a TextPattern object representing a sequence of TextPattern objects matching (t1, t2, ...)"
OrderlessTextPattern::usage="OrderlessTextPattern[t1, t2, ...] is a TextPattern object representing a pattern of text matching (t1, t2, ...) in any order."
OptionalTextPattern::usage="OptionalTextPattern[t] is a TextPattern object that represents 0 or 1 instances of t"
TextType::usage="TextType[type] is a TextPattern object representing a type of text content."
ConvertToWikipediaSearchQuery::usage="For development purposes only, convert TextPattern to WikipediaSearch query strings"
TextPatternToRegularExpression::usage="TextPatternToRegularExpression[source, t] Convert a TextPattern to a RegularExpression"
GenerateRegularExpressionTemplate::usage="GenerateRegularExpressionTemplate[t] Generate a RegularExpression template from a TextPattern"
ContentAssociation::usage="ContentAssociation[source, t] generates an association where a text contetype is the key, and examples from the source text of the content type are the values."
(* EscapePunctuation::usage = "EscapePunctuation[s] add escape characters before punctuation in the source text so they're not considered as patterns in RegularExpressions" *)
LexicalSummary::usage ="Represents the results of LexicalCases. Use the \"Properties\" subvalue for a list of properties."
ValidTextPatternQ::usage="ValidTextPatternQ[input] Returns True if input is a valid TextPattern"
ToTextElementStructure::usage="ToTextElementStructure[tp] renders a TextPattern using TextElements"
$LexicalCasesSupportedServices::usage="List of supported services"
(* FilterOutStopwordRows::usage="FilterOutStopwordRows[data] filters out rows with stopwords" *)
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

(* Validate TextPattern Objects *)
ExtractHeads[expr_] := Cases[expr, h_[___] :> h, {0, Infinity}]
$TextPatternValidHeads = {TextPattern, TextPatternSequence, OptionalTextPattern, OrderlessTextPattern, TextType, Pattern, Alternatives, Rule, RuleDelayed, RegularExpression}

ValidTextPatternQ[input_TextPattern]:= With[{heads = DeleteDuplicates@ExtractHeads[input]}, ContainsOnly[heads, $TextPatternValidHeads]]
ValidTextPatternQ[Rule[input_TextPattern,_]]:= ValidTextPatternQ[input]
ValidTextPatternQ[RuleDelayed[input_TextPattern,_]]:= ValidTextPatternQ[input]

(* Format TextPatterns for strings *)
TextElementFormat[s_String] := s
TextElementFormat[re_RegularExpression] := ToString[re]
TextElementFormat[a_Alternatives] :=TextElement[ToString[a], <|"GrammaticalUnit" -> "Alternatives"|>]
TextElementFormat[TextPattern[args__]] :=TextElement[Map[TextElementFormat]@{args}, <|"GrammaticalUnit" -> "TextPattern"|>];
TextElementFormat[TextPatternSequence[args__]] :=TextElement[Map[TextElementFormat]@{args}, <|"GrammaticalUnit" -> "Sequence"|>];
TextElementFormat[OrderlessTextPattern[args__]] :=TextElement[Map[TextElementFormat]@{args}, <|"GrammaticalUnit" -> "Orderless"|>];
TextElementFormat[OptionalTextPattern[args__]] :=TextElement[Map[TextElementFormat]@{args}, <|"GrammaticalUnit" -> "Optional"|>];
TextElementFormat[TextType[type_String]] :=TextElement[type, <|"GrammaticalUnit" -> "TextType"|>];
TextElementFormat[TextType[type_RegularExpression]] :=TextElement[ToString[type], <|"GrammaticalUnit" -> "TextType"|>];

ToTextElementStructure[tp_TextPattern] := TextElementFormat[tp];

GenerateRegularExpressionTemplate[tp_TextPattern] := Module[
	{p1,p2},
	p1 = ReplaceAll[tp, {
		TextPattern -> StringExpression,
		TextPatternSequence -> PatternSequence,
		OrderlessTextPattern -> Function[Alternatives@@Map[Apply[PatternSequence]][Permutations[{##}]]],
		OptionalTextPattern[opt__] :> ("(" <> StringRiffle[AlternativesToList[opt], "|"] <>")?"),
		TextType[type_] :> "`[:" <> type <> ":]`"
		}];
	p2 = ReplaceAll[p1, a_Alternatives :> ("(" <>StringRiffle[AlternativesToList[a], "|"] <> ")")]
		]

TextContentGroup[List[content_String]] := content;
TextContentGroup[content_List] := "(" <> StringRiffle[content, "|"] <> ")";
ExtractContentTypes[tp_TextPattern] := Cases[tp, TextType[type_] :> type, Infinity];
ContentAssociation[sourcetext_String, tp_TextPattern] := KeyMap["[:" <> # <> ":]" &][Map[DeleteDuplicates /* TextContentGroup][TextCases[sourcetext, ExtractContentTypes[tp]]]]
EscapePunctuation[s_String] := StringReplace[s, pc : PunctuationCharacter :> "\\" <> pc]

ContainsPatternHeadsQ[tp_] := ContainsAny[ExtractHeads[tp], {Pattern}]
StripNamedPattern[tp_] := StripNames[ContainsPatternHeadsQ[tp], tp]
StripNames[True, tp_TextPattern] := Replace[tp, p_Pattern :> Extract[2][p], Infinity]
StripNames[True, (Rule|RuleDelayed)[tp_TextPattern,_]] := Replace[tp, p_Pattern :> Extract[2][p], Infinity]
StripNames[False, tp_]:= tp

TextPatternToRegularExpression[sourcetext_String, tp_TextPattern] :=
	Module[{TRX, CA},
		TRX = GenerateRegularExpressionTemplate[tp];
		CA  = ContentAssociation[sourcetext, tp];
		RegularExpression[StringTemplate[TRX][CA]]
		]

TextPatternToRegularExpression[sourcetext_String, rule:((Rule|RuleDelayed)[tp_TextPattern, expr_])] :=
	Module[{TP = StripNamedPattern[tp], TRX, CA, REX, PTC = 0, CGR},
		(* Process LHS *)
		TRX = GenerateRegularExpressionTemplate[TP];
		CA  = ContentAssociation[sourcetext, TP];
		REX = RegularExpression[StringTemplate[TRX][CA]];
		(* Process RHS *)
		CGR = Cases[rule, p_Pattern :> (Extract[1][p] ->"$" <> ToString[++PTC]), Infinity];
		Replace[REX :> expr, CGR, Infinity]
		]

WikipediaSearchQuery[List[], tp_TextPattern] := Message[ConvertToWikipediaSearchQuery::novq, tp]
WikipediaSearchQuery[wsq:List[__String], tp_TextPattern] := StringRiffle[wsq]
WikipediaSearchQuery[wsq:List[List[__String]], tp_TextPattern] := Flatten[wsq]
WikipediaSearchQuery[wsq_List, tp_TextPattern] := Cases[List[(_List|_String)..]][wsq] // Map[StringRiffle]

ConvertToWikipediaSearchQuery[tp_TextPattern]:= Module[
	{cleanTextPattern, stage1},
	cleanTextPattern = DeleteCases[List@@tp, (_TextType | _OptionalTextPattern | _OrderlessTextPattern), All];
	stage1 = ReplaceAll[cleanTextPattern, {Alternatives -> List}]//DeleteCases[" "];
	Check[WikipediaSearchQuery[stage1, tp] // StringReplace[(" " ..) -> " "] // StringTrim, Return[$Failed, Module]]
	]

ConvertToWikipediaSearchQuery[Rule[tp_TextPattern,_]] := ConvertToWikipediaSearchQuery[StripNamedPattern[tp]]
ConvertToWikipediaSearchQuery[RuleDelayed[tp_TextPattern,_]] := ConvertToWikipediaSearchQuery[StripNamedPattern[tp]]


ConvertToWikipediaSearchQuery::novq = "Keyword formulation not supported for the pattern ``. Consider using the \"Content\" option to supply keyaords, or trying a different TextPattern."


(* Input Handlers *)
Options[LexicalCases]={
	"Service" -> "Wikipedia"
};
Options[LexicalCasesWikipedia] = {
	MaxItems -> 50,
	Language -> "English"
};

$LexicalCasesSupportedServices = {"Wikipedia"}
(* SourceText and TextPattern Input *)
LexicalCases[sourcetext_String, tpatt_?ValidTextPatternQ]:= Module[
	{TPC},
	(* Find Matches *)
	TPC = Monitor[
		LexicalCasesOnString[sourcetext, tpatt],
		Row[{Style["Searching", Bold], ProgressIndicator[Appearance->"Ellipsis"], "\n", ToTextElementStructure[StripNamedPattern@tpatt]}]
		];
	(* Generate Summary Object *)
	Monitor[
		generateLexicalSummary[TPC, "Text", tpatt],
		Row[{"Generating LexicalSummary", ProgressIndicator[Appearance->"Necklace"]}]
	]
	]

(* SourceText is a string *)
LexicalCasesOnString[source_String, tp_?ValidTextPatternQ]:=Module[
	{RX, S = EscapePunctuation[source]},
	RX = TextPatternToRegularExpression[S, tp];
	Map[AssociationThread[{"Match", "Position"} -> #] &]@With[
		{cases = DeleteDuplicates@StringCases[source, RX]},
		Thread[{cases, Map[StringPosition[source, #] &][cases]}]
		]
	]

(* TextPattern on Service *)
LexicalCases[tpatt_?ValidTextPatternQ, opts:OptionsPattern[{LexicalCases, LexicalCasesWikipedia}]]:= LexicalCasesFromService[OptionValue["Service"], tpatt, FilterRules[{opts}, Options[LexicalCasesWikipedia]]]

(* WikiQueryRyle and TextPattern Input *)
LexicalCases[query_Rule, tpatt_?ValidTextPatternQ, opts:OptionsPattern[{LexicalCases, LexicalCasesWikipedia}]]:= LexicalCasesFromService[OptionValue["Service"], query, tpatt, FilterRules[{opts}, Options[LexicalCasesWikipedia]]]

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

SearchForTextPattern[texts_List, tp_?ValidTextPatternQ, stp_?ValidTextPatternQ, articles_List, articleCount_Integer, maxTitleLength_Integer] := Module[
	{T = texts},
		ArticleIndex=0;
		SetSharedVariable[tp];
		Monitor[
			ParallelMap[(++ArticleIndex;LexicalCasesOnString[#, tp])&, T],
			Row[{
				Style["Searching for TextPattern:\n", Bold],
				ToTextElementStructure[stp], "\n",
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
LexicalCasesFromService["Wikipedia", tp_?ValidTextPatternQ, opts:OptionsPattern[{LexicalCasesWikipedia}]]:= With[
	{wikiquery = StringTrim@ConvertToWikipediaSearchQuery[tp]},
	ProcessWikiQuery[wikiquery,tp,opts]
	]

LexicalCasesFromService["Wikipedia", query_Rule, tp_?ValidTextPatternQ, opts:OptionsPattern[{LexicalCasesWikipedia}]]:= ProcessWikiQuery[query,tp,opts]

ProcessWikiQuery[query_?FailureQ,___] := Return[query, With]
ProcessWikiQuery[query_Rule,tp_?ValidTextPatternQ, opts___] := Module[
	{data = LexicalCasesWikipedia[query, tp, FilterRules[{opts}, Options[LexicalCasesWikipedia]]]},
	generateLexicalSummary[data, "Wikipedia", tp]
	]

ProcessWikiQuery[query:(_String|_List),tp_?ValidTextPatternQ, opts___] := Module[
	{data = LexicalCasesWikipedia["Content" -> query, tp, FilterRules[{opts}, Options[LexicalCasesWikipedia]]]},
	generateLexicalSummary[data, "Wikipedia", tp]
	]

(* SourceText is a WikipediaSearch Query *)
LexicalCasesWikipedia[wikiquery_Rule, tp_?ValidTextPatternQ, opts:OptionsPattern[]]:= Module[
	{TP = tp, STP = StripNamedPattern[tp], ard, art, arc, acs, mtl, src, mtc, matchesassoc, articlematchthread},
	(* 1 - Get Wikipedia Articles *)
	ard = GetArticlesFromService["Wikipedia", wikiquery, opts];
	
	art = ard["Articles"];
	arc = ard["ArticleCount"];
	acs = ard["ArticleCountString"];
	mtl = ard["MaxTitleLength"];
	
	(* 2 - Get Wikipedia article text *)
	src = GetTextFromArticles["Wikipedia", art, arc, acs, mtl];
	
	(* 3 - Search for TextPattern *)
	mtc = SearchForTextPattern[src, TP, STP, art, arc, mtl];
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


LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", n_Integer] := GenerateDashboard[LexicalSummary[asc], n]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", DeleteStopwords] := GenerateDashboard[LexicalSummary[asc], DeleteStopwords]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", n_Integer, DeleteStopwords] := GenerateDashboard[LexicalSummary[asc], n, DeleteStopwords]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups"] := PartOfSpeechGroups[LexicalSummary[asc]["MatchCountGroups"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", n_Integer] := PartOfSpeechGroups[LexicalSummary[asc]["MatchCountGroups", n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", DeleteStopwords] := PartOfSpeechGroups[LexicalSummary[asc]["MatchCountGroups", DeleteStopwords]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", n_Integer,  DeleteStopwords] := PartOfSpeechGroups[LexicalSummary[asc]["MatchCountGroups", n, DeleteStopwords]]

LexicalSummary[asc_?LexicalSummaryAscQ][invalidkey_] := asc[invalidkey]
LexicalSummary[asc_?LexicalSummaryAscQ]["Properties"] := {"Data","Dataset","Counts","CountGroups", "MatchCounts", "MatchCountGroups","PercentDataset","PartOfSpeechGroups", "Source","TotalMatchCount","TextElementStructure", "Survey"}

generateLexicalSummary[data_?FailureQ, ___] := data
generateLexicalSummary[data_, sourceType_String, textpattern_] := Module[
	{matchcount},
	matchcount = DeleteMissing[GetDatasetCounts[Dataset[data], sourceType], 1, 1][Total, "Count"];
	LexicalSummary[<|"Data" -> data, "Source" -> sourceType, "TotalMatchCount" -> matchcount, "TextElementStructure" -> ToTextElementStructure[StripNamedPattern@textpattern] |>]
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

PartOfSpeechGroups[ds_Dataset] := (ds[TextWords /* ToLowerCase /* Flatten /* DeleteStopwords /* DeleteDuplicates, "Matches"][AlphabeticSort][GroupBy[PartOfSpeechKey]][KeyDrop[None]] // KeySort)

PercentDataset[ds_Dataset, matchcount_Integer] := (ds[All, <|"Percentage" ->Interpreter["Percent"]@*ToString@*PercentForm@*N@((#CountGroup Length[#Matches])/matchcount),"Matches" -> #Matches|> &][ReverseSortBy["Percentage"]])

GenerateDashboard[tps_LexicalSummary, params___] := With[
	{
		pdat = tps["PercentDataset",params] ,
		posg = tps["PartOfSpeechGroups", params]
		},
		DynamicModule[
			{tab="Dataset"},
				Column[{
					Panel[SetterBar[Dynamic[tab],{"Dataset","PartOfSpeechGroups"}]],
					Dynamic[
						Switch[tab,
							"Dataset",pdat,
							"PartOfSpeechGroups", posg
							]]
					}]
			]
		]
		
End[]
EndPackage[]
