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
TextPattern::uage="Represents a fixed order text pattern"
OrdelessTextPattern::uage="Represents an any order text pattern"
OptionalTextPattern::uage="Represents an optional text pattern"

Begin["Private`"]
(* Input Handlers *)
Options[TextSequenceCases]:={
	"Service"->"Wikipedia"
};
TextSequenceCases[tpatt_TextPattern, opts:OptionsPattern[]]:=
	TextSequenceCasesServiceQuery[tpatt,FilterRules[opts,Options[TextSequenceCasesServiceQuery]]]
TextSequenceCases[sourcetext_String,tpatt_TextPattern, opts:OptionsPattern[]]:=
	TextSequenceCasesOnString[sourcetext, tpatt, FilterRules[opts,Options[TextSequenceCasesOnString]]]
TextSequenceCases[wikiquery_Rule,tpatt_TextPattern, opts:OptionsPattern[]]:=
	TextSequenceCasesOnWikipediaSearchQueryResults[wikiquery, tpatt, FilterRules[opts,Options[TextSequenceCasesOnWikipediaSearchQueryResults]]]

(* No SourceText specified *)
Options[TextSequenceCasesServiceQuery]={
	"Service"->"Wikipedia"
	(* TODO: List of services \[LongDash] not all of these should be supported I don't think.
	{"ArXiv","AWS","BingSearch","CharityEngine","ChemSpider","CrossRef","Dropbox","Facebook","Factual","FederalReserveEconomicData","Fitbit","Flickr","GoogleAnalytics","GoogleCalendar","GoogleContacts","GoogleCustomSearch","GooglePlus","GoogleTranslate","Instagram","IPFS","LinkedIn","MailChimp","MicrosoftTranslator","Mixpanel","MusicBrainz","OpenLibrary","OpenPHACTS","PubChem","PubMed","Pushbullet","Reddit","RunKeeper","SeatGeek","SurveyMonkey","Twilio","Twitter","Wikipedia","Yelp"}
	*)
};
TextSequenceCasesServiceQuery[tpatt_TextPattern,opts:OptionsPattern[]]:=Module[
		{t=tpatt},
		EchoLabel["tpatt :"][tpatt];
		EchoLabel["opts  :"][opts];
	]

(* SourceText is a string *)
Options[TextSequenceCasesOnString]={
};
TextSequenceCasesOnString[source_String, tpatt_TextPattern, opts:OptionsPattern[]]:=Module[
	{s=source,t=tpatt},
	EchoLabel["source:"][s];
	EchoLabel["tpatt :"][tpatt];
	EchoLabel["opts  :"][opts];
	]

(* SourceText is a WikipediaSearch Query *)
TextSequenceCasesOnWikipediaSearchQueryResults[wikipediaquery_Rule, tpatt_TextPattern, opts:OptionsPattern[]]:=Module[
	{res=wikipediaquery,t=tpatt},
	EchoLabel["source:"][res];
	EchoLabel["tpatt :"][tpatt];
	EchoLabel["opts  :"][opts];
	];

End[]
EndPackage[]
