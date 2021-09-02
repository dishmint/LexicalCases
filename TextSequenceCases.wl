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
(* ConvertToSequencePattern::usage="For development purposes only, convert TextPattern objects to Pattern objects" *)

Begin["Private`"]

(* Utility *)
OptionsJoin[sym__Symbol]:=(Map[Options]/*Apply[Join])[{sym}]

Listify[x_List] := x
Listify[x_] := {x}

ReplaceEmptyListWithMissing[list:List[__List]]:=Replace[list, {} -> Missing["NoMatchesFound"], Infinity]

MonitorTextTypeInsertion[expr_]:= Monitor[expr, Row[{"Performing TextType Insertion", ProgressIndicator[Appearance -> "Ellipsis"]}]]
SetAttributes[MonitorTextTypeInsertion, HoldAll]

MonitorSequenceSearch[searchexpr_, article_]:= Monitor[searchexpr, Row[{article, ProgressIndicator[Appearance -> "Ellipsis"]}]]
SetAttributes[MonitorSequenceSearch, HoldAll]

(* Format TextPatterns for strings *)
TextPatternFormat[s_String] := s
TextPatternFormat[re_RegularExpression] := ToString[re]
TextPatternFormat[a_Alternatives] := (Apply[Riffle[Map[TextPatternFormat]@{##}, "|"] &])[a]
TextPatternFormat[TextPattern[args__]] := {"(>",Map[TextPatternFormat]@{args}, "<)"}
TextPatternFormat[TextPatternSequence[args__]] := {"(",Map[TextPatternFormat]@{args}, ")"}
TextPatternFormat[OrderlessTextPattern[args__]] := {"(~",Map[TextPatternFormat]@{args}, "~)"}
TextPatternFormat[OptionalTextPattern[args__]] := {"(",Map[TextPatternFormat]@{args}, ")?"}
TextPatternFormat[TextType[args:(_String|_RegularExpression)]] := {"(:",Map[TextPatternFormat]@{args}, ":)"}

ConvertToPatternString[tp_TextPattern]:= (Flatten /* StringRiffle)@TextPatternFormat[tp]

(* Pattern Behaviors and Processors *)
PatternizeTextCase[{x_}]:=x
PatternizeTextCase[x:{__}]:=Apply[PatternSequence][x]

InsertTextTypeCases[text_,tp_List]:=With[
	{cases=TextCases[text,Cases[tp,TextType[type_]:>type,Infinity]]},
	List@@Replace[
	tp,
	TextType[type_]:>Alternatives@@Map[PatternizeTextCase][StringSplit[cases[type]]],
	Infinity
	]
]
ConvertToSequencePattern[tp_TextPattern]:=ReplaceAll[
	tp,
	{
	TextPattern -> List,
	TextPatternSequence -> PatternSequence,
	OrderlessTextPattern -> Function[Alternatives@@Map[Apply[PatternSequence]][Permutations[{##}]]],
	OptionalTextPattern -> (Function[Flatten@Alternatives[#,PatternSequence[]]])
	}
	]

ConvertToSequencePattern[tp_List]:=ReplaceAll[
	tp,
	{
	TextPattern -> List,
	TextPatternSequence -> PatternSequence,
	OrderlessTextPattern -> Function[Alternatives@@Map[Apply[PatternSequence]][Permutations[{##}]]],
	OptionalTextPattern -> (Function[Flatten@Alternatives[#,PatternSequence[]]])
	}
	]


ConvertToWikipediaSearchQuery[tp_TextPattern]:= Module[
	{clean, alt2list},
	(* TODO: Rather than delete OrderlessTextPattern object it should be tupled; Add ExpandTextPattern function? *)
	clean = DeleteCases[tp, _TextType | _OptionalTextPattern | _OrderlessTextPattern];
	alt2list = ReplaceAll[clean, Alternatives -> List];
	result = (alt2list // Apply[List] /* Map[Listify] // Tuples // Map[StringRiffle])
	]

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
	TextSequenceCasesWikipedia["Content" -> wikiquery, tp, FilterRules[{opts}, Options[TextSequenceCasesWikipedia]]]
	]

(* SourceText is a string *)
TextSequenceCasesOnString[source_String, tp_TextPattern]:=Module[
	{
		s= TextWords[source],
		p= MonitorTextTypeInsertion@InsertTextTypeCases[source,ConvertToSequencePattern[tp]]
	},
	SequenceCases[s,p]
	]

(* SourceText is a list of strings (used in TextSequenceCasesWikipedia ) *)
TextSequenceCasesOnStringList[source:List[__String], tp_List]:=Module[
	{p= MonitorTextTypeInsertion@InsertTextTypeCases[StringRiffle[source], tp]},
	SequenceCases[source,p]
	]

(* SourceText is a WikipediaSearch Query *)
TextSequenceCasesWikipedia[wikiquery_Rule, tp_TextPattern, opts:OptionsPattern[]]:=Module[
	{p=ConvertToSequencePattern[tp],articles, sourcetexts, tokenizedsourcetexts, matches, matchesassoc, articlematchthread},

	(* 1 \[LongDash] Get Wikipedia Articles *)
	articles = Monitor[
		WikipediaArticlesFromRule[wikiquery, FilterRules[{opts}, OptionsJoin[WikipediaSearch,TextSequenceCasesWikipedia]]],
		Row[{"Searching Wikipedia: ", StringRiffle[Listify[Values[wikiquery]], ", "], ProgressIndicator[Appearance->"Ellipsis"]}]
		];
	articleCount = Length[articles];
	articleCountString = ToString[articleCount];
	maxTitleLength = First@TakeLargestBy[StringLength,1][articles->"Value"];
	
	(* 2 \[LongDash] Get Wikipedia article text *)
	SetSharedVariable[TextSequenceCases`ArticleIndex];
	TextSequenceCases`ArticleIndex=0;
	sourcetexts = Monitor[
		ParallelMap[(++TextSequenceCases`ArticleIndex;WikipediaData[#])&,articles],
		Row[{
			"Gathering text from "<>articleCountString<>" articles:\n",
			ProgressIndicator[Dynamic[TextSequenceCases`ArticleIndex],{0,articleCount}]," ",
			Dynamic[NumberForm[PercentForm[N[TextSequenceCases`ArticleIndex/articleCount]],{3,2}]],
			Dynamic[If[TextSequenceCases`ArticleIndex <= articleCount-1," Getting article "<>StringPadRight["\""<>articles[[TextSequenceCases`ArticleIndex+1]]<>"\"",maxTitleLength],""]]
			}
			]
		];
	(* TODO: clean text here if necessary *)
	
	(* 3 \[LongDash] Tokenize source texts *)
	(* TODO: I'll need to rethink the source Tokenization because what if there is a text sequence that matches across a full stop? That should be avoided.  (refer to TextSequenceCasesOnStringList) *)
	
	TextSequenceCases`ArticleIndex=0;
	tokenizedsourcetexts = Monitor[
		ParallelMap[(++TextSequenceCases`ArticleIndex;TextWords[#])&, sourcetexts],
		Row[{
			"Preparing articles for sequence search ",
			Dynamic[If[TextSequenceCases`ArticleIndex <= articleCount-1,StringPadRight["\""<>articles[[TextSequenceCases`ArticleIndex+1]]<>"\" ",maxTitleLength],""]],"\n",
			ProgressIndicator[Dynamic[TextSequenceCases`ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[TextSequenceCases`ArticleIndex/articleCount]],{3,2}]]
		}
		]];
	
	(* 4 \[LongDash] Search *)
	TextSequenceCases`ArticleIndex=0;
	SetSharedVariable[p, articles];
	matches = Monitor[
		ParallelMap[(++TextSequenceCases`ArticleIndex;MonitorSequenceSearch[TextSequenceCasesOnStringList[#,p], articles[[TextSequenceCases`ArticleIndex]]])&, tokenizedsourcetexts],
		Row[{
			"Searching for "<>ConvertToPatternString[tp]<>" sequences ",
			Dynamic[If[TextSequenceCases`ArticleIndex <= articleCount-1, StringPadRight["\""<>articles[[TextSequenceCases`ArticleIndex+1]]<>"\" ",maxTitleLength]," "]],"\n",
			ProgressIndicator[Dynamic[TextSequenceCases`ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[TextSequenceCases`ArticleIndex/articleCount]],{3,2}]]
			}]];
	
	articlematchthread = Monitor[Thread[{articles, ReplaceEmptyListWithMissing[matches]}], Row[{"Threading Articles with Matches ", ProgressIndicator[Appearance->"Ellipsis"]}]];
	
	SetSharedVariable[articlematchthread];
	TextSequenceCases`ArticleIndex=0;
	matchesassoc = Monitor[
		ParallelMap[(++TextSequenceCases`ArticleIndex;AssociationThread[{"Article", "Matches"} -> #])&, articlematchthread],
		Row[{
			"Generating Association for ",
			Dynamic[If[TextSequenceCases`ArticleIndex <= articleCount-1, StringPadRight["\""<>articles[[TextSequenceCases`ArticleIndex+1]]<>"\" ",maxTitleLength]," "]],"\n",
			ProgressIndicator[Dynamic[TextSequenceCases`ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[TextSequenceCases`ArticleIndex/articleCount]],{3,2}]]
			}]];
	
	TextSequenceCases`ArticleIndex=.;
	
	matchesassoc
	]


WikipediaArticlesFromRule[rule:("Content" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule, Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,TextSequenceCasesWikipedia]]]
WikipediaArticlesFromRule[rule:("Category" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule,"CategoryArticles", Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,TextSequenceCasesWikipedia]]]

End[]
EndPackage[]
