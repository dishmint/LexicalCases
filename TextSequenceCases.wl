(* ::Package:: *)

(* ::Title:: *)
(*TextSequenceCases*)


(* ::Abstract:: *)
(*Extract and analyze text type sequences with the Wolfram Language.*)


BeginPackage["TextSequenceCases`"]
(* Main *)
TextSequenceCases::usage="TextSequenceCases[\!\(\*
StyleBox[\"source\", \"TI\"]\), \!\(\*
StyleBox[\"textpatt\", \"TI\"]\)] gives the text sequences in \!\(\*
StyleBox[\"source\", \"TI\"]\) that match the text pattern \!\(\*
StyleBox[\"textpatt\", \"TI\"]\)."

(* TextPatterns *)
TextPattern::usage="TextPattern[\!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(1\)\(,\)\)]\)\!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(2\)\(,\)\)]\)\[Ellipsis]] is a TextPattern object matching \!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(1\)\(,\)\)]\)\!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(2\)\(,\)\)]\)\!\(\*SubscriptBox[\(\[Ellipsis]\), \(,\)]\)in the fixed order given."
TextPatternSequence::usage="TextPatternSequence[\!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(1\)\(,\)\)]\)\!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(2\)\(,\)\)]\)\[Ellipsis]] is a TextPattern object representing a sequence of TextPattern objects arguments matching \!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(1\)\(,\)\)]\)\!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(2\)\(,\)\)]\)\[Ellipsis]"
OrderlessTextPattern::usage="OrderlessTextPattern[\!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(1\)\(,\)\)]\)\!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(2\)\(,\)\)]\)\[Ellipsis]] is a TextPattern object representing a pattern of text matching \!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(1\)\(,\)\)]\)\!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(2\)\(,\)\)]\)\!\(\*SubscriptBox[\(\[Ellipsis]\), \(,\)]\) in any order."
OptionalTextPattern::usage="OptionalTextPattern[\!\(\*
StyleBox[\"textpatt\", \"TI\"]\)] is a TextPattern object that represents 0 or 1 instances of \!\(\*
StyleBox[\"textpatt\", \"TI\"]\)"
TextType::usage="TextType[\!\(\*
StyleBox[\"type\", \"TI\"]\)] is a TextPattern object representing text content of type \!\(\*
StyleBox[\"type\", \"TI\"]\)"
ConvertToSequencePattern::usage="For development purposes only, convert TextPattern objects to Pattern objects"

Begin["Private`"]
$TextPatternHeads = ((_String|_OrderlessTextPattern|_OptionalTextPattern|_TextPattern|_TextType)..)

(* Pattern Behaviors and Processors *)
InsertTextTypeCases[text_,tp_List]:=With[
	{cases=TextCases[text,Cases[tp,TextType[type_]:>type,Infinity]]},
	List@@Replace[
	tp,
	TextType[type_]:>Alternatives@@cases[type],
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

(* Input Handlers *)
Options[TextSequenceCases]:={
	"Service"->"Wikipedia"
};
Options[TextSequenceCasesServiceQuery]={
	"Service"->"Wikipedia"
	(* TODO: List of services \[LongDash] not all of these should be supported I don't think.
	{"ArXiv","AWS","BingSearch","CharityEngine","ChemSpider","CrossRef","Dropbox","Facebook","Factual","FederalReserveEconomicData","Fitbit","Flickr","GoogleAnalytics","GoogleCalendar","GoogleContacts","GoogleCustomSearch","GooglePlus","GoogleTranslate","Instagram","IPFS","LinkedIn","MailChimp","MicrosoftTranslator","Mixpanel","MusicBrainz","OpenLibrary","OpenPHACTS","PubChem","PubMed","Pushbullet","Reddit","RunKeeper","SeatGeek","SurveyMonkey","Twilio","Twitter","Wikipedia","Yelp"}
	*)
};
Options[TextSequenceCasesOnString]={
};

OptionsGiven[sym_Symbol][o_]:=OptionsGiven[sym,o]
OptionsGiven[sym_Symbol, {}]:=Options[sym]
OptionsGiven[sym_Symbol, opts:{__}]:=FilterRules[{opts}, Options[sym]]

(* Validate TextPatternObject *)
ContainsOnlyTextPatternSymbols[heads_List]:=ContainsOnly[heads,{Symbol,String,Alternatives,TextPattern,OrderlessTextPattern,OptionalTextPattern,TextType}]
SymbolsTextPatternSymbolsQ[heads_List]:=With[
	{hcounts=KeyDrop[String][Counts[heads]]},
	hcounts[Symbol]===(KeyDrop[Symbol]/*Total@hcounts)
	]
ValidTextPatternObjectQ[tp_]:=Module[
	{heads},
	heads=Cases[tp,x_:>Head[x],{0,Infinity},Heads->True];
	Through[(ContainsOnlyTextPatternSymbols\[And]SymbolsTextPatternSymbolsQ)[heads]]
	]

TextSequenceCases[tpatt_?ValidTextPatternObjectQ, opts:OptionsPattern[]]:=
	TextSequenceCasesServiceQuery[tpatt, OptionsGiven[TextSequenceCasesServiceQuery,{opts}]]
	
TextSequenceCases[sourcetext_String,tpatt_?ValidTextPatternObjectQ, opts:OptionsPattern[]]:=
	TextSequenceCasesOnString[sourcetext, tpatt, OptionsGiven[TextSequenceCasesOnString,{opts}]]
	
TextSequenceCases[wikiquery_Rule,tpatt_?ValidTextPatternObjectQ, opts:OptionsPattern[]]:=
	TextSequenceCasesOnWikipediaSearchQueryResults[wikiquery, tpatt, OptionsGiven[TextSequenceCasesOnWikipediaSearchQueryResults,{opts}]]

(* No SourceText specified *)
TextSequenceCasesServiceQuery[tp_TextPattern, opts:OptionsPattern[]]:=Module[
		{p=ConvertToSequencePattern[tp]},
		(*with the text InsertTextTypeCases[text, p] *)
		EchoLabel["patt  :"][p];
		EchoLabel["optsg  :"][opts];
	]

(* SourceText is a string *)
TextSequenceCasesOnString[source_String, tp_TextPattern, opts:OptionsPattern[]]:=Module[
	{s=TextWords[source],p=InsertTextTypeCases[source,ConvertToSequencePattern[tp]]},
	EchoLabel["source:"][s];
	EchoLabel["tpatt :"][p];
	EchoLabel["optsg  :"][opts];
	]

(* SourceText is a WikipediaSearch Query *)
TextSequenceCasesOnWikipediaSearchQueryResults[wikipediaquery_Rule, tp_TextPattern, opts:OptionsPattern[]]:=Module[
	{res=wikipediaquery,p=ConvertToSequencePattern[tp]},
	(* with the text InsertTextTypeCases[text, p] *)
	EchoLabel["rule   :"][res];
	EchoLabel["tpatt  :"][p];
	EchoLabel["optsg  :"][opts];
	];

End[]
EndPackage[]
