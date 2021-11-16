(* ::Package:: *)

(* ::Title:: *)
(*TextSequenceCases*)


(* ::Abstract:: *)
(*Extract and analyze text type sequences with the Wolfram Language.*)


BeginPackage["TextSequenceCases`"]
(* Main *)
TextSequenceCases::usage="TextSequenceCases[source, textpatt] gives the text sequences in source that match the text pattern textpatt."

(* TextPatterns *)
TextPattern::usage="TextPattern[t1, t2, ...] is a TextPattern object matching (t1, t2, ...) in the fixed order given."
TextPatternSequence::usage="TextPatternSequence[t1, t2, ...] is a TextPattern object representing a sequence of TextPattern objects arguments matching (t1, t2, ...)"
OrderlessTextPattern::usage="OrderlessTextPattern[t1, t2, ...] is a TextPattern object representing a pattern of text matching (t1, t2, ...) in any order."
OptionalTextPattern::usage="OptionalTextPattern[t] is a TextPattern object that represents 0 or 1 instances of t"
TextType::usage="TextType[type] is a TextPattern object representing a type of text content."
ConvertToWikipediaSearchQuery::usage="For development purposes only, convert TextPattern to WikipediaSearch query strings"
TextPatternToRegularExpression::usage="TextPatternToRegularExpression[source, t] Convert a TextPattern to a RegularExpression"
GenerateRegularExpressionTemplate::usage="GenerateRegularExpressionTemplate[t] Generate a RegularExpression template from a TextPattern"
ContentAssociation::usage="ContentAssociation[source, t]"

Begin["Private`"]

(* Utility *)
OptionsJoin[sym__Symbol]:=(Map[Options]/*Apply[Join])[{sym}]

Listify[x_List] := x;
Listify[x_] := {x};

ReplaceEmptyListWithMissing[result_]:=Replace[result, {} -> Missing["NoMatchesFound"], Infinity];

(* Format TextPatterns for strings *)
TextPatternFormat[s_String] := s
TextPatternFormat[re_RegularExpression] := ToString[re]
TextPatternFormat[a_Alternatives] := (Apply[Riffle[Map[TextPatternFormat]@{##}, "|"] &])[a]
TextPatternFormat[TextPattern[args__]] := {"(>",Map[TextPatternFormat]@{args}, "<)"}
TextPatternFormat[TextPatternSequence[args__]] := {"(",Map[TextPatternFormat]@{args}, ")"}
TextPatternFormat[OrderlessTextPattern[args__]] := {"(~",Map[TextPatternFormat]@{args}, "~)"}
TextPatternFormat[OptionalTextPattern[args__]] := {"(",Map[TextPatternFormat]@{args}, ")?"}
TextPatternFormat[TextType[args:(_String|_RegularExpression)]] := {"(:",Map[TextPatternFormat]@{args}, ":)"}

ConvertToPatternString[tp_TextPattern]:= (Flatten /* StringRiffle)@TextPatternFormat[tp];

ExpressionRiffle[h_[args___], sep_] := h@@Riffle[{args}, sep];

AlternativesToList[s_String] := {s};
AlternativesToList[a_Alternatives] := List@@a;

GenerateRegularExpressionTemplate[tp_TextPattern] := Module[
	{spacefluffed = ExpressionRiffle[tp, " "]},
	ReplaceAll[spacefluffed, {
		TextPattern -> StringExpression,
		TextPatternSequence -> PatternSequence,
		OrderlessTextPattern -> Function[Alternatives@@Map[Apply[PatternSequence]][Permutations[{##}]]],
		OptionalTextPattern[opt__] :> ("(" <> StringRiffle[AlternativesToList[opt], "|"] <>")?"),
		TextType[type_] :> "`[:" <> type <> ":]`",
		a_Alternatives :> ("(" <>StringRiffle[AlternativesToList[a], "|"] <> ")")
		}]
		]

TextContentGroup[content_List] := "(" <> StringRiffle[content, "|"] <> ")";
ExtractContentTypes[tp_TextPattern] := Cases[tp, TextType[type_] :> type, Infinity];
ContentAssociation[sourcetext_String, tp_TextPattern] := KeyMap["[:" <> # <> ":]" &][Map[DeleteDuplicates /* TextContentGroup][TextCases[sourcetext, ExtractContentTypes[tp]]]]

TextPatternToRegularExpression[sourcetext_String, tp_TextPattern] :=
	Module[{TRX, CA},
		TRX = GenerateRegularExpressionTemplate[tp];
		CA  = ContentAssociation[sourcetext, tp];
		StringTemplate[TRX][CA]
		]

WikipediaSearchQuery[List[], tp_TextPattern] := Message[ConvertToWikipediaSearchQuery::novq, tp]
WikipediaSearchQuery[wsq:List[__String], tp_TextPattern] := StringRiffle[wsq]
WikipediaSearchQuery[wsq:List[List[__String]], tp_TextPattern] := Flatten[wsq]
WikipediaSearchQuery[wsq_List, tp_TextPattern] := Cases[List[(_List|_String)..]][wsq] // Map[StringRiffle]

ConvertToWikipediaSearchQuery[tp_TextPattern]:= Module[
	{cleanTextPattern, stage1},
	cleanTextPattern = DeleteCases[List@@tp, (_TextType | _OptionalTextPattern | _OrderlessTextPattern), All];
	stage1 = ReplaceAll[cleanTextPattern, {Alternatives -> List}];
	Check[WikipediaSearchQuery[stage1, tp], Return[$Failed, Module]]
	]

ConvertToWikipediaSearchQuery::novq = "Keyword formulation not supported for the pattern ``. Consider using the \"Content\" option to supply keyaords, or trying a different TextPattern."

(* Validate TextPattern Objects *)
ContainsOnlyTextPatternSymbols[heads_List]:= ContainsOnly[heads, {Symbol, String, Alternatives, RegularExpression, TextPattern, TextPatternSequence, OrderlessTextPattern, OptionalTextPattern, TextType}]

SymbolsTextPatternSymbolsQ[heads_List]:= With[{hcounts = KeyDrop[String][Counts[heads]]}, hcounts[Symbol]===(KeyDrop[Symbol]/*Total@hcounts)]

ValidTextPatternQ[tp_TextPattern]:=Module[{heads},
	heads=Cases[tp, x_:>Head[x],{0,Infinity},Heads->True];
	Through[(ContainsOnlyTextPatternSymbols\[And]SymbolsTextPatternSymbolsQ)[heads]]
	]

(* Input Handlers *)
Options[TextSequenceCases]={
	"Service" -> "Wikipedia"
};
Options[TextSequenceCasesWikipedia] = {
	MaxItems -> 50,
	Language -> "English"
};

TextSequenceCases[tpatt_?ValidTextPatternQ, opts:OptionsPattern[{TextSequenceCases, TextSequenceCasesWikipedia}]]:=
	TextSequenceCasesFromService[OptionValue["Service"], tpatt, FilterRules[{opts}, Options[TextSequenceCasesWikipedia]]]
	
TextSequenceCases[sourcetext_String, tpatt_?ValidTextPatternQ]:= TextSequenceCasesOnString[sourcetext, tpatt]
	
TextSequenceCases[wikiquery_Rule, tpatt_?ValidTextPatternQ, opts:OptionsPattern[{TextSequenceCases, TextSequenceCasesWikipedia}]]:= TextSequenceCasesWikipedia[wikiquery, tpatt, FilterRules[{opts}, Options[TextSequenceCasesWikipedia]]]

(* No SourceText specified *)
Options[TextSequenceCasesFromService]={

};

(*
TODO: Add relevant services from the list below
	{"ArXiv","AWS","BingSearch","CharityEngine","ChemSpider","CrossRef","Dropbox","Facebook","Factual","FederalReserveEconomicData","Fitbit","Flickr","GoogleAnalytics","GoogleCalendar","GoogleContacts","GoogleCustomSearch","GooglePlus","GoogleTranslate","Instagram","IPFS","LinkedIn","MailChimp","MicrosoftTranslator","Mixpanel","MusicBrainz","OpenLibrary","OpenPHACTS","PubChem","PubMed","Pushbullet","Reddit","RunKeeper","SeatGeek","SurveyMonkey","Twilio","Twitter","Wikipedia","Yelp"}
	*)
TextSequenceCasesFromService["Wikipedia", tp_TextPattern, opts:OptionsPattern[{TextSequenceCasesWikipedia}]]:= With[
	{wikiquery = ConvertToWikipediaSearchQuery[tp]},
	If[FailureQ[wikiquery],
	wikiquery,
	TextSequenceCasesWikipedia["Content" -> wikiquery, tp, FilterRules[{opts}, Options[TextSequenceCasesWikipedia]]]
	]
	]

(* SourceText is a string *)
TextSequenceCasesOnString[source_String, tp_TextPattern]:=Module[
	{RX = RegularExpression[TextPatternToRegularExpression[source, tp]]},
	MapThread[<|"Match" -> #1, "Position" -> #2|>&, Through[{Apply[StringCases], Apply[StringPosition]}[{source,RX}]]]
	]

(* SourceText is a WikipediaSearch Query *)
TextSequenceCasesWikipedia[wikiquery_Rule, tp_TextPattern, opts:OptionsPattern[]]:=Module[
	{RX = RegularExpression[TextPatternToRegularExpression[source, tp]], articles, sourcetexts, matches, matchesassoc, articlematchthread},

	(* 1 \[LongDash] Get Wikipedia Articles *)
	articles = Monitor[
		WikipediaArticlesFromRule[wikiquery, FilterRules[{opts}, OptionsJoin[WikipediaSearch,TextSequenceCasesWikipedia]]],
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
	SetSharedVariable[RX, articles];
	matches = Monitor[
		ParallelMap[
		(++ArticleIndex;TextSequenceCasesOnString[#, RX])&,
		sourcetexts
		],
		Row[{
			"Searching for "<>ConvertToPatternString[tp]<>" sequences ",
			Dynamic[If[ArticleIndex <= articleCount-1, StringPadRight["\""<>articles[[ArticleIndex+1]]<>"\" ",maxTitleLength]," "]],"\n",
			ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]
			}]];
	
	articlematchthread = Monitor[Thread[{articles, ReplaceEmptyListWithMissing[matches]}], Row[{"Threading Articles with Matches ", ProgressIndicator[Appearance->"Ellipsis"]}]];
	
	SetSharedVariable[articlematchthread];
	ArticleIndex=0;
	matchesassoc = Monitor[
		ParallelMap[(++ArticleIndex;Map[Apply[Prepend]]@Thread[{#2,"Article" -> #1}])&, articlematchthread],
		Row[{
			"Generating Association for ",
			Dynamic[If[ArticleIndex <= articleCount-1, StringPadRight["\""<>articles[[ArticleIndex+1]]<>"\" ",maxTitleLength]," "]],"\n",
			ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]
			}]];
	
	Clear[ArticleIndex];
	
	matchesassoc
	]


WikipediaArticlesFromRule[rule:("Content" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule, Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,TextSequenceCasesWikipedia]]]
WikipediaArticlesFromRule[rule:("Category" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule,"CategoryArticles", Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,TextSequenceCasesWikipedia]]]

End[]
EndPackage[]
