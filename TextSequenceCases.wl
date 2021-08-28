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
StyleBox[\"t\", \"TI\"], \(\(2\)\(,\)\)]\)\[Ellipsis]] represents a pattern of text matching \!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(1\)\(,\)\)]\)\!\(\*SubscriptBox[
StyleBox[\"t\", \"TI\"], \(\(2\)\(,\)\)]\)\!\(\*SubscriptBox[\(\[Ellipsis]\), \(,\)]\)in the fixed order given."
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


Begin["Private`"]
$TextPatternHeads = ((_String|_OrderlessTextPattern|_OptionalTextPattern|_TextPattern|_TextType|_Repeated|_Pattern|_PatternSequence|_OrderlessPatternSequence|_Alternatives|_Blank|_BlankNull|_BlankNullSequence)..)


(* Pattern Behaviors and Processors *)
TextPattern[seq:$TextPatternHeads]:=PatternSequence[seq]
OrderlessTextPattern[seq:$TextPatternHeads]:=OrderlessPatternSequence[seq]
OptionalTextPattern[p:$TextPatternHeads]:=Repeated[p,{0,1}]
(* ^^ Default arg for Optional is not supported (at the moment) *)

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

TextSequenceCases[tpatt_PatternSequence, opts:OptionsPattern[]]:=
	TextSequenceCasesServiceQuery[tpatt, OptionsGiven[TextSequenceCasesServiceQuery,{opts}]]
	
TextSequenceCases[sourcetext_String,tpatt_PatternSequence, opts___]:=
	TextSequenceCasesOnString[sourcetext, tpatt, OptionsGiven[TextSequenceCasesOnString,{opts}]]
	
TextSequenceCases[wikiquery_Rule,tpatt_PatternSequence, opts___]:=
	TextSequenceCasesOnWikipediaSearchQueryResults[wikiquery, tpatt, OptionsGiven[TextSequenceCasesOnWikipediaSearchQueryResults,{opts}]]

(* No SourceText specified *)
TextSequenceCasesServiceQuery[tpatt_PatternSequence, opts:OptionsPattern[]]:=Module[
		{t=tpatt},
		EchoLabel["tpatt  :"][tpatt];
		EchoLabel["optsg  :"][opts];
	]

(* SourceText is a string *)
TextSequenceCasesOnString[source_String, tpatt_PatternSequence, opts:OptionsPattern[]]:=Module[
	{s=source,t=tpatt},
	EchoLabel["source:"][s];
	EchoLabel["tpatt :"][tpatt];
	EchoLabel["optsg  :"][opts];
	]

(* SourceText is a WikipediaSearch Query *)
TextSequenceCasesOnWikipediaSearchQueryResults[wikipediaquery_Rule, tpatt_PatternSequence, opts:OptionsPattern[]]:=Module[
	{res=wikipediaquery,t=tpatt},
	EchoLabel["rule   :"][res];
	EchoLabel["tpatt  :"][tpatt];
	EchoLabel["optsg  :"][opts];
	];

End[]
EndPackage[]
