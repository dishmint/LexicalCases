(* ::Package:: *)

(* ::Title:: *)
(*TextPatternCases*)


(* ::Abstract:: *)
(*Extract and analyze text type sequences with the Wolfram Language.*)


BeginPackage["TextPatternCases`"]
(* Main *)
TextPatternCases::usage="TextPatternCases[source, textpatt] gives the text sequences in source that match the text pattern textpatt."

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
TextPatternSummary::usage ="Represents the results of TextPatternCases. Use the \"Properties\" subvalue for a list of properties."
ValidTextPatternQ::usage="ValidTextPatternQ[input] Returns True if input is a valid TextPattern"
ToTextElementStructure::usage="ToTextElementStructure[tp] renders a TextPattern using TextElements"
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
$TextPatternValidHeads = {TextPattern, TextPatternSequence, OptionalTextPattern, OrderlessTextPattern, TextType, Pattern, Alternatives, Rule, RuleDelayed, RegularExpression}

ValidTextPatternQ[input_TextPattern]:= With[{heads = DeleteDuplicates@Cases[input, h_[___] :> h, {0, Infinity}] }, ContainsOnly[heads, $TextPatternValidHeads]]
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

StripNamedPattern[tp_TextPattern] := tp
StripNamedPattern[(Rule|RuleDelayed)[tp_TextPattern,_]] := Replace[tp, p_Pattern :> Extract[2][p], Infinity]

TextPatternToRegularExpression[sourcetext_String, tp_TextPattern] :=
	Module[{TRX, CA},
		TRX = GenerateRegularExpressionTemplate[tp];
		CA  = ContentAssociation[sourcetext, tp];
		RegularExpression[StringTemplate[TRX][CA]]
		]

TextPatternToRegularExpression[sourcetext_String, rule:(Rule[tp_TextPattern, expr_]|RuleDelayed[tp_TextPattern, expr_])] :=
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
	stage1 = ReplaceAll[cleanTextPattern, {Alternatives -> List}];
	Check[WikipediaSearchQuery[stage1, tp] // StringReplace[(" " ..) -> " "], Return[$Failed, Module]]
	]

ConvertToWikipediaSearchQuery[Rule[tp_TextPattern,_]] := ConvertToWikipediaSearchQuery[StripNamedPattern[tp]]
ConvertToWikipediaSearchQuery[RuleDelayed[tp_TextPattern,_]] := ConvertToWikipediaSearchQuery[StripNamedPattern[tp]]


ConvertToWikipediaSearchQuery::novq = "Keyword formulation not supported for the pattern ``. Consider using the \"Content\" option to supply keyaords, or trying a different TextPattern."


(* Input Handlers *)
Options[TextPatternCases]={
	"Service" -> "Wikipedia"
};
Options[TextPatternCasesWikipedia] = {
	MaxItems -> 50,
	Language -> "English"
};


(* SourceText and TextPattern Input *)
TextPatternCases[sourcetext_String, tpatt_?ValidTextPatternQ]:= generateTextPatternSummary[TextPatternCasesOnString[sourcetext, tpatt], "Text", tpatt]

(* TextPattern on Service *)
TextPatternCases[tpatt_?ValidTextPatternQ, opts:OptionsPattern[{TextPatternCases, TextPatternCasesWikipedia}]]:= TextPatternCasesFromService[OptionValue["Service"], tpatt, FilterRules[{opts}, Options[TextPatternCasesWikipedia]]]

(* WikiQueryRyle and TextPattern Input *)
TextPatternCases[wikiquery_Rule, tpatt_?ValidTextPatternQ, opts:OptionsPattern[{TextPatternCases, TextPatternCasesWikipedia}]]:= generateTextPatternSummary[
	TextPatternCasesWikipedia[wikiquery, tpatt, FilterRules[{opts}, Options[TextPatternCasesWikipedia]]],
	"Wikipedia",
	tpatt
]

(* No SourceText specified *)
Options[TextPatternCasesFromService]={

};

(*
TODO: Add relevant services from the list below
	{"ArXiv","AWS","BingSearch","CharityEngine","ChemSpider","CrossRef","Dropbox","Facebook","Factual","FederalReserveEconomicData","Fitbit","Flickr","GoogleAnalytics","GoogleCalendar","GoogleContacts","GoogleCustomSearch","GooglePlus","GoogleTranslate","Instagram","IPFS","LinkedIn","MailChimp","MicrosoftTranslator","Mixpanel","MusicBrainz","OpenLibrary","OpenPHACTS","PubChem","PubMed","Pushbullet","Reddit","RunKeeper","SeatGeek","SurveyMonkey","Twilio","Twitter","Wikipedia","Yelp"}
	*)
TextPatternCasesFromService["Wikipedia", tp_?ValidTextPatternQ, opts:OptionsPattern[{TextPatternCasesWikipedia}]]:= With[
	{wikiquery = ConvertToWikipediaSearchQuery[tp]},
	ProcessWikiQuery[wikiquery,tp,opts]
	]

ProcessWikiQuery[query_?FailureQ,___] := Return[query, With]
ProcessWikiQuery[query_,tp_?ValidTextPatternQ, opts___] := Module[
	{data = TextPatternCasesWikipedia["Content" -> query, tp, FilterRules[{opts}, Options[TextPatternCasesWikipedia]]]},
	generateTextPatternSummary[data, "Wikipedia", tp]
	]

(* SourceText is a string *)
TextPatternCasesOnString[source_String, tp_?ValidTextPatternQ]:=Module[
	{RX, S = EscapePunctuation[source]},
	RX = TextPatternToRegularExpression[S, tp];
	Map[AssociationThread[{"Match", "Position"} -> #] &]@With[
		{cases = DeleteDuplicates@StringCases[source, RX]},
		Thread[{cases, Map[StringPosition[source, #] &][cases]}]
		]
	]

(* SourceText is a WikipediaSearch Query *)
TextPatternCasesWikipedia[wikiquery_Rule, tp_?ValidTextPatternQ, opts:OptionsPattern[]]:=Module[
	{TP = StripNamedPattern[tp], articles, sourcetexts, matches, matchesassoc, articlematchthread},
	(* 1 \[LongDash] Get Wikipedia Articles *)
	articles = Monitor[
		WikipediaArticlesFromRule[wikiquery, FilterRules[{opts}, OptionsJoin[WikipediaSearch,TextPatternCasesWikipedia]]],
		Row[{"Searching Wikipedia: ", StringRiffle[Listify[Values[wikiquery]], ", "], ProgressIndicator[Appearance->"Ellipsis"]}]
		];
	articleCount = Length[articles];
	articleCountString = ToString[articleCount];
	maxTitleLength = First@TakeLargestBy[StringLength,1][articles->"Value"];
	
	(* 2 \[LongDash] Get Wikipedia article text *)
	SetSharedVariable[ArticleIndex];
	ArticleIndex=0;
	sourcetexts = Monitor[
		ParallelMap[(++ArticleIndex;WikipediaData[#])&,articles],
		Row[{
			"Gathering text from "<>articleCountString<>" articles:\n",
			ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",
			Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]],
			Dynamic[If[ArticleIndex <= articleCount-1," Getting article "<>StringPadRight["\""<>articles[[ArticleIndex+1]]<>"\"",maxTitleLength],""]]
			}
			]
		];
	
	(* 4 \[LongDash] Search *)
	ArticleIndex=0;
	SetSharedVariable[tp, articles];
	matches = Monitor[
		ParallelMap[
		(++ArticleIndex;TextPatternCasesOnString[#, tp])&,
		sourcetexts
		],
		Row[{
			Style["Searching for TextPattern:\n", Bold],
			ToTextElementStructure[TP],
			Dynamic[If[ArticleIndex <= articleCount-1, StringPadRight["\""<>articles[[ArticleIndex+1]]<>"\" ",maxTitleLength]," "]],"\n",
			ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]
			}]];
	
	articlematchthread = Monitor[Thread[{articles, ReplaceEmptyListWithMissing[matches]}], Row[{"Threading Articles with Matches ", ProgressIndicator[Appearance->"Ellipsis"]}]];
	
	SetSharedVariable[articlematchthread];
	ArticleIndex=0;
	matchesassoc = Monitor[
		ParallelMap[(++ArticleIndex;PrependArticleKey[#])&, articlematchthread],
		Row[{
			"Generating Association for ",
			Dynamic[If[ArticleIndex <= articleCount-1, StringPadRight["\""<>articles[[ArticleIndex+1]]<>"\" ",maxTitleLength]," "]],"\n",
			ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]
			}]];
	
	Clear[ArticleIndex];
	
	(* articlematchthread *)
	Flatten[matchesassoc]
	]


WikipediaArticlesFromRule[rule:("Content" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule, Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,TextPatternCasesWikipedia]]]
WikipediaArticlesFromRule[rule:("Category" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule,"CategoryArticles", Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,TextPatternCasesWikipedia]]]



(* TextPatternSummary *)
TextPatternSummary /: MakeBoxes[obj : TextPatternSummary[asc_?TextPatternSummaryAscQ], form : (StandardForm | TraditionalForm)] :=
Module[{above, below},
	above = {(*example grid*)
		{BoxForm`SummaryItem[{"Source: ", asc["Source"]}]},
		{BoxForm`SummaryItem[{"MatchCount: ",asc["TotalMatchCount"]}]}
	};
	below = {
	};
	BoxForm`ArrangeSummaryBox[
		TextPatternSummary,(*head*)
		obj,(*interpretation*)
		None,(*icon,use None if not needed*)
		above,(*always shown content*)
		below,(*expandable content*)
		form,
		"Interpretable" -> Automatic]
		];
TextPatternSummaryAscQ[asc_?AssociationQ] := AllTrue[{"Data", "Source", "TotalMatchCount", "TextElementStructure"}, KeyExistsQ[asc, #] &]
TextPatternSummaryAscQ[_] = False;

TextPatternSummary[asc_?TextPatternSummaryAscQ]["Data"] := asc["Data"]
TextPatternSummary[asc_?TextPatternSummaryAscQ]["Dataset"] := Dataset[asc["Data"]]
TextPatternSummary[asc_?TextPatternSummaryAscQ]["Counts"] := GetDatasetCounts[TextPatternSummary[asc]["Dataset"], asc["Source"]]
TextPatternSummary[asc_?TextPatternSummaryAscQ]["Counts", "Ascending"] := (TextPatternSummary[asc]["Counts"][SortBy["Count"]])
TextPatternSummary[asc_?TextPatternSummaryAscQ]["Counts", "Descending"] := (TextPatternSummary[asc]["Counts"][ReverseSortBy["Count"]])
TextPatternSummary[asc_?TextPatternSummaryAscQ]["CountGroups"] := (TextPatternSummary[asc]["Counts", "Descending"] // CountGroups)
TextPatternSummary[asc_?TextPatternSummaryAscQ]["CountGroups", n_Integer] := (TextPatternSummary[asc]["CountGroups"][;;UpTo[n]])
TextPatternSummary[asc_?TextPatternSummaryAscQ]["MatchCounts"] := (TextPatternSummary[asc]["Counts"] // DeleteMissing[#, 1, 1] &)
TextPatternSummary[asc_?TextPatternSummaryAscQ]["MatchCounts", "Ascending"] := (TextPatternSummary[asc]["Counts", "Ascending"] // DeleteMissing[#, 1, 1] &)
TextPatternSummary[asc_?TextPatternSummaryAscQ]["MatchCounts", "Descending"] := (TextPatternSummary[asc]["Counts", "Descending"] // DeleteMissing[#, 1, 1] &)
TextPatternSummary[asc_?TextPatternSummaryAscQ]["MatchCountGroups"] := (TextPatternSummary[asc]["MatchCounts", "Descending"] // CountGroups)
TextPatternSummary[asc_?TextPatternSummaryAscQ]["MatchCountGroups", n_Integer] := (TextPatternSummary[asc]["MatchCountGroups"][;;UpTo[n]])
TextPatternSummary[asc_?TextPatternSummaryAscQ]["Source"] := asc["Source"]
TextPatternSummary[asc_?TextPatternSummaryAscQ]["TotalMatchCount"] := asc["TotalMatchCount"]
TextPatternSummary[asc_?TextPatternSummaryAscQ]["TextElementStructure"] := asc["TextElementStructure"]
TextPatternSummary[asc_?TextPatternSummaryAscQ]["Dashboard"] := GenerateDashboard[TextPatternSummary[asc]["MatchCountGroups"], TextPatternSummary[asc]["TotalMatchCount"]]
TextPatternSummary[asc_?TextPatternSummaryAscQ]["Dashboard", n_Integer] := GenerateDashboard[TextPatternSummary[asc]["MatchCountGroups", n]]
TextPatternSummary[asc_?TextPatternSummaryAscQ][invalidkey_] := asc[invalidkey]
TextPatternSummary[asc_?TextPatternSummaryAscQ]["Properties"] := {"Data","Dataset","Counts","CountGroups", "MatchCounts", "MatchCountGroups", "Source","TotalMatchCount","TextElementStructure", "Dashboard"}

generateTextPatternSummary[data_?FailureQ, ___] := data
generateTextPatternSummary[data_, sourceType_String, textpattern_] := Module[
	{matchcount},
	matchcount = DeleteMissing[GetDatasetCounts[Dataset[data], sourceType], 1, 1][Total, "Count"];
	TextPatternSummary[<|"Data" -> data, "Source" -> sourceType, "TotalMatchCount" -> matchcount, "TextElementStructure" -> ToTextElementStructure[Extract[1] /* StripNamedPattern@textpattern] |>]
]

PartOfSpeechKey[word_String] := ProcessWordData[WordData[word, "PartsOfSpeech"]]
ProcessWordData[w_WordData] := None
ProcessWordData[w_] := w

CountGroups[ds_Dataset] := (ds // Query[GroupBy[#Count&], KeyDrop["Count"] /* Values /* (Flatten[#,1]&)] // KeyValueMap[<|"CountGroup" -> #1, "Matches" -> #2|> &])
GetDatasetCounts[ds_Dataset,"Text"] := ds[All, <|"Match" -> "Match", "Count" -> "Position" /* Length|>]
GetDatasetCounts[ds_Dataset, "Wikipedia"] := ds[GroupBy["Match"], KeyDrop[{"Article", "Match"}] /* Values] // KeyValueMap[<|"Match" -> #1, "Count" -> Length[#2]|> &]
PartOfSpeechGroups[ds_Dataset] := (ds[TextWords /* ToLowerCase /* Flatten /* DeleteStopwords /* DeleteDuplicates, "Matches"][AlphabeticSort][GroupBy[PartOfSpeechKey]][KeyDrop[None]] // KeySort)

GenerateDashboard[ds_Dataset,matchcount_Integer] := With[
	{
		dataset=ds[All, <|"Percentage" ->Interpreter["Percent"]@*ToString@*PercentForm@*N@((#CountGroup Length[#Matches])/matchcount),"Matches" -> #Matches|> &][ReverseSortBy["Percentage"]],
		posg = PartOfSpeechGroups[ds]
		},
			DynamicModule[{plot="Dataset"},
				Column[{
					Panel[SetterBar[Dynamic[plot],{"Dataset","PartOfSpeechGroups"}]],
					Dynamic[
						Switch[plot,
							"Dataset",dataset,
							"PartOfSpeechGroups", posg
							]]
					}]
			]
		]

End[]
EndPackage[]
