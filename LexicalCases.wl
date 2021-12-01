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
OrderlessLexicalPattern::usage="OrderlessLexicalPattern[p1, p2, ...] matches p's in any order"
LexicalPatternSequence::usage="LexicalPatternSequence[p1, p2, ...] represents a sequence of p's"
LexicalPatternToStringExpression::usage="LexicalPatternToStringExpression[source, lp] Converts lexical pattern lp to a StringExpression"
TextType::usage="TextType[type] is a LexicalPattern object representing a type of text content."

ValidLexicalPatternQ::usage="ValidLexicalPatternQ[input] Returns True if input is a valid LexicalPattern"
ToTextElementStructure::usage="ToTextElementStructure[lp] renders a LexicalPattern using TextElements"

ExpandLexicalPattern::usage="ExpandLexicalPattern[lp] expands a lexical pattern into constructs suitable for StringExpression"
ContentAssociation::usage="ContentAssociation[source, t] generates an association where a text contetype is the key, and examples from the source text of the content type are the values."
ExtractContentTypes::usage="ExtractContentTypes[lp] extract content types from a lexical pattern"
LexicalSummary::usage ="Represents the results of LexicalCases. Use the \"Properties\" subvalue for a list of properties."
ConvertToWikipediaSearchQuery::usage="For development purposes only, convert LexicalPattern to WikipediaSearch query strings"
$LexicalCasesSupportedServices::usage="List of supported services"
$LexicalPatternValidHeads::usage="List of symbols LexicalPattern supports"
TextElementFormat::usage="TextElementFormat[x] formats x as a TextElement"
CountSummaryLowercase::usage="CountSummaryLowercase[ds] Makes matches lowercase and merges rows that now have the same match."
WordListLookup::usage="WordListLookup[type] returns the WordList[type]"
Begin["Private`"]

$PartsOfSpeech = {"Noun", "Verb", "Adjective", "Adverb", "Pronoun", "Preposition", "Conjunction", "Determiner", "Interjection"};
$PartsOfSpeechAlternatives = Alternatives@@$PartsOfSpeech;
WordLists = Monitor[Map[<|# -> WordList[#]|> &, $PartsOfSpeech], Row[{"Getting PartOfSpeech words",ProgressIndicator[Appearance->"Ellipsis"]}]];
WordListLookup[pos:$PartsOfSpeechAlternatives] := WordLists // Lookup[pos] // DeleteMissing // Flatten
WordListLookup[s_String] := {}
(* Utility *)
OptionsJoin[sym__Symbol]:=(Map[Options]/*Apply[Join])[{sym}]

Listify[x_List] := x;
Listify[x_] := {x};

InsertAnd[l:List[_]] := l;
InsertAnd[x_List] := Insert[x, "and", -2];

KeyWordString[{s_String}]:= "\""<>s<>"\"";
KeyWordString[l:{_String,_String}]:= StringRiffle[Map[KeyWordString][l], ", "];
KeyWordString[x_List] := StringRiffle[InsertAnd[Map[KeyWordString][x]], ", "];
KeyWordString[x_Alternatives] := ToString[Map[KeyWordString][x]]
KeyWordString[x_] := "\""<>x<>"\"";

AlternativesToList[s_String] := {s};
AlternativesToList[a_Alternatives] := List@@a;

PrependArticleKey[{article_String, data_List}] := Map[Apply[Prepend]]@Thread[{data, "Article" -> article}]
PrependArticleKey[{article_String, data_Missing}] := <|"Article" -> article, "Match" -> data|>

ReplaceEmptyListWithMissing[result_]:= Replace[result, {} -> Missing["NoMatches"], 1];

(* Validate LexicalPattern Objects *)
ExtractHeads[expr_] := Cases[expr, h_[___] :> h, {0, Infinity}]
$LexicalPatternValidHeads = {
	LexicalPattern, LexicalPatternSequence, OptionalLexicalPattern, OrderlessLexicalPattern, TextType,
	Pattern, PatternSequence, Except, Repeated, RepeatedNull,Blank, BlankSequence, BlankNullSequence,
	Alternatives, Rule, RuleDelayed, RegularExpression, StringExpression,
	LetterCharacter, WordCharacter, PunctuationCharacter, WhitespaceCharacter, DigitCharacter, HexadecimalCharacter,
	NumberString, StartOfString, EndOfString, WordBoundary,
	StartOfLine, EndOfLine
 }

ValidLexicalPatternQ[input_LexicalPattern]:= With[{heads = DeleteDuplicates@ExtractHeads[input]}, ContainsOnly[heads, $LexicalPatternValidHeads]]
ValidLexicalPatternQ[Rule[input_LexicalPattern,_]]:= ValidLexicalPatternQ[input]
ValidLexicalPatternQ[RuleDelayed[input_LexicalPattern,_]]:= ValidLexicalPatternQ[input]

(* Format LexicalPatterns for strings *)
PostProcessAlternatives[alts_Alternatives] := {alts}
PostProcessAlternatives[te_] := te

SetAttributes[TextElementFormat, HoldAll]
TextElementFormat[LexicalPattern[args___]] := TextElementFormat[LexicalPattern,args];
TextElementFormat[TextType[args_String]] := TextElementFormat[TextType,args];
TextElementFormat[TextType[args_Alternatives]] := TextElement[PostProcessAlternatives[Map[TextElementFormat][ExpandAlternativeTextTypes[args]]], <|"GrammaticalUnit" -> "Alternatives"|>];
TextElementFormat[OptionalLexicalPattern[args__]] := TextElement[Map[TextElementFormat][{args}], <|"GrammaticalUnit" -> "Optional"|>];
TextElementFormat[LexicalPatternSequence[args__]] := TextElement[Map[TextElementFormat][{args}], <|"GrammaticalUnit" -> "Sequence"|>];
TextElementFormat[a_Alternatives] := TextElement[PostProcessAlternatives[Map[TextElementFormat][a]], <|"GrammaticalUnit" -> "Alternatives"|>];
TextElementFormat[s_String] := s;
TextElementFormat[s_Symbol] := s;
TextElementFormat[h_] := TextElementFormat[Extract[0][h],Extract[1][h]];
TextElementFormat[TextType, s_String] := TextElement[s, <|"GrammaticalUnit" -> "TextType"|>];
TextElementFormat[h_, args__] := TextElement[Map[TextElementFormat][{args}], <|"GrammaticalUnit" -> ToString[h]|>];

ToTextElementStructure[lp_LexicalPattern] := TextElementFormat[lp];
ToTextElementStructure[(Rule|RuleDelayed)[lp_LexicalPattern,_]] := Construct[TextElementFormat, StripNamedPattern[lp]];

ExpandAlternativeTextTypes[alts_Alternatives] := (Apply[Alternatives]@*Map[TextType]@*Apply[List])[alts]

ExpandLexicalPattern[lp_LexicalPattern] := ReplaceAll[lp, {
	LexicalPattern -> StringExpression,
	LexicalPatternSequence -> StringExpression,
	OrderlessLexicalPattern -> Function[Alternatives@@Map[Apply[StringExpression]][Permutations[{##}]]],
	OptionalLexicalPattern[opt_Alternatives] :> (opt~Join~Alternatives[""]),
	OptionalLexicalPattern[opt_] :> (Alternatives[opt]~Join~Alternatives[""]),
	TextType[alts_Alternatives] :> ExpandAlternativeTextTypes[alts]
	}]

ExtractStringContentTypes[lp_LexicalPattern] := Splice[Cases[lp, TextType[type_String] :> type, Infinity]];
ExtractAlternativeContentTypes[lp_LexicalPattern] := Splice[Cases[lp, TextType[type_Alternatives] :> Splice[List@@type], Infinity]];
ExtractContentTypes[lp_LexicalPattern] := Through[{ExtractStringContentTypes, ExtractAlternativeContentTypes}[lp]]
ContentAssociation[sourcetext_String, lp_LexicalPattern] := Module[
	{WORDS = TextWords[sourcetext]},
	Merge[Identity]@KeyValueMap[<|#1 -> Alternatives@@DeleteDuplicates@((WORDS~Intersection~WordListLookup[#1])~Join~#2)|> &][TextCases[sourcetext, ExtractContentTypes[lp]]]
	]
EscapePunctuation[s_String] := StringReplace[s, pc : PunctuationCharacter :> "\\" <> pc]

ContainsPatternHeadsQ[lp_] := ContainsAny[ExtractHeads[lp], {Pattern}]
StripNamedPattern[lp_] := StripNames[ContainsPatternHeadsQ[lp], lp]
StripNames[True, lp_LexicalPattern] := Replace[lp, p_Pattern :> Extract[2][p], Infinity]
StripNames[True, (Rule|RuleDelayed)[lp_LexicalPattern,_]] := Replace[lp, p_Pattern :> Extract[2][p], Infinity]
StripNames[False, lp_]:= lp

ContentAlts[List[a_Alternatives]] := a
ContentAlts[a_Alternatives] := a
LexicalPatternToStringExpression[sourcetext_String, lp_LexicalPattern] :=
	Module[{TRX, CA},
		TRX = ExpandLexicalPattern[lp];
		CA  = ContentAssociation[sourcetext, lp];
		Replace[TRX, TextType[type_] :> ContentAlts[CA[type]], Infinity]
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
SetAttributes[LexicalCases, Listable]
LexicalCases::unsupobj=Import::unsupobj;
GetFileExtension[file_File] := Information[file, "FileExtension"]
SupportedFileQ[file_File] := MemberQ[{"txt", "md", "csv", "tsv"}, GetFileExtension[file]]
LexicalCases[file_File, args___] /; SupportedFileQ[file] := Module[{data = Import[file]}, LexicalCases[data, args]]
LexicalCases[file_File, args___] := Message[LexicalCases::unsupobj, GetFileExtension[file]]

(* SourceText and LexicalPattern Input *)
LexicalCases[sourcetext_String, lpatt_?ValidLexicalPatternQ]:= Module[
	{LPC,RES},
	ArticleIndex=0;
	(* Find Matches *)
	LPC = Monitor[
		LexicalCasesOnString[sourcetext, lpatt],
		Row[{"Searching", ProgressIndicator[Appearance->"Ellipsis"]}]
		];
	(* Generate Summary Object *)
	RES = Monitor[
		GenerateLexicalSummary[LPC, "Text", lpatt],
		Row[{"Generating LexicalSummary", ProgressIndicator[Appearance->"Necklace"]}]
	];
	ArticleIndex=.;
	RES
	]

(* SourceText is a string *)
LexicalCasesOnString[source_String, lp_?ValidLexicalPatternQ]:=Module[
	{RX, S = EscapePunctuation[source]},
	++ArticleIndex;
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
	{ART, ARC, MTL, SQR = KeyWordString[Values[query]]},
	ART = Monitor[
		WikipediaArticlesFromRule[query, FilterRules[{opts}, OptionsJoin[WikipediaSearch,LexicalCasesWikipedia]]],
		Row[{"Searching Wikipedia for ", SQR, ProgressIndicator[Appearance->"Ellipsis"]}]
		];
	ARC = Length[ART];
	MTL = First@TakeLargestBy[StringLength,1][ART->"Value"];
	<|"Articles" -> ART, "ArticleCount" -> ARC, "ArticleCountString" -> ToString[ARC], "MaxTitleLength" -> MTL|>
	]

GetArticleText[title_String] := Module[{TXT},
	TXT = WikipediaData[title];
	++ArticleIndex;
	TXT
	]

GetTextFromArticles["Wikipedia", articles_List, articleCount_Integer, articleCountString_String, maxTitleLength_Integer] := Module[
	{TXT},
		SetSharedVariable[ArticleIndex];
		ArticleIndex=0;
		TXT=Monitor[
			(* Progress appears to be fast *)
			ParallelMap[(++ArticleIndex;WikipediaData[#])&, articles],
			(* Progress appears to be slow  *)
			(* ParallelMap[GetArticleText[#]&, articles], *)
			Row[{
				"Gathering text from "<>articleCountString<>" articles:\n",
				Dynamic[If[ArticleIndex <= articleCount-1,StringPadRight["\""<>articles[[ArticleIndex+1]]<>"\"",maxTitleLength],""]], "\n",
				ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]
				}
				]
			];
			ArticleIndex=.;
			TXT
	]

LexicalPatternSearch[text_,pattern_] := Module[
	{RES = LexicalCasesOnString[text, pattern]},
		MatchCount+=Length[Replace[_Missing -> {}]@Flatten[Lookup[RES,"Position"], 1]];
		RES
		]

SearchArticles[texts_List, lp_?ValidLexicalPatternQ, slp_?ValidLexicalPatternQ, articles_List, articleCount_Integer, maxTitleLength_Integer] := Module[
	{MAT},
		ArticleIndex=0;
		MatchCount=0;
		SetSharedVariable[lp,MatchCount];
		MAT = Monitor[
			ParallelMap[(LexicalPatternSearch[#,lp])&, texts],
			Row[{
				"Searching ", Dynamic[If[ArticleIndex <= articleCount-1, "\""<>articles[[ArticleIndex+1]]<>"\" ", " "]], "\n",
				"Found: ", Dynamic[MatchCount], "\n",
				ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]
				}]
			];
		MatchCount=.;
		MAT
	]

PackageResults[matches_, articles_List, articleCount_Integer, maxTitleLength_Integer] := Module[
	{AMT, MAC},
		AMT = Thread[{articles, ReplaceEmptyListWithMissing[matches]}];
		SetSharedVariable[AMT];
		ArticleIndex=0;
		MAC = Monitor[
			ParallelMap[(++ArticleIndex;PrependArticleKey[#])&, AMT],
			Row[{
				"Packaging results",ProgressIndicator[Appearance->"Ellipsis"], "\n",
				ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",
				Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]}]
			];
		ArticleIndex=.;
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
	GenerateLexicalSummary[data, "Wikipedia", lp]
	]

ProcessWikiQuery[query:(_String|_List),lp_?ValidLexicalPatternQ, opts___] := Module[
	{data = LexicalCasesWikipedia["Content" -> query, lp, FilterRules[{opts}, Options[LexicalCasesWikipedia]]]},
	GenerateLexicalSummary[data, "Wikipedia", lp]
	]

(* SourceText is a WikipediaSearch Query *)
LexicalCasesWikipedia[wikiquery_Rule, lp_?ValidLexicalPatternQ, opts:OptionsPattern[]]:= Module[
	{LP = lp, SLP = StripNamedPattern[lp], ard, art, arc, acs, mtl, src, mtc},
	(* 1 - Get Wikipedia Articles *)
	ard = GetArticlesFromService["Wikipedia", wikiquery, opts];
	
	art = ard["Articles"];
	arc = ard["ArticleCount"];
	acs = ard["ArticleCountString"];
	mtl = ard["MaxTitleLength"];
	
	(* 2 - Get Wikipedia article text *)
	src = GetTextFromArticles["Wikipedia", art, arc, acs, mtl];
	
	(* 3 - Search for LexicalPattern *)
	mtc = SearchArticles[src, LP, SLP, art, arc, mtl];
	PackageResults[mtc, art, arc, mtl]
	]

Options[WikipediaArticlesFromRule] = {
	MaxItems -> 50
};

WikipediaArticlesFromRule["Content" -> a_Alternatives, opts:OptionsPattern[]]:= Module[
	{KWL = Apply[List][a], RULES},
	RULES = Thread["Content"-> KWL];
	Flatten@Map[r |-> WikipediaArticlesFromRule[r, MaxItems -> (Ceiling[OptionValue[MaxItems]/Length[KWL]]), FilterRules[opts, Except[MaxItems]]]][RULES]
]
WikipediaArticlesFromRule[rule:("Content" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule, Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,LexicalCasesWikipedia]]]
WikipediaArticlesFromRule[rule:("Category" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule,"CategoryArticles", Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,LexicalCasesWikipedia]]]

ArticleWithMatchCount["Text", _] := ""
ArticleWithMatchCount[_String, data_] := (data // DeleteMissing[#, 1, 1]& // Lookup["Article"] // DeleteDuplicates // Length)

DisplayArticlesWithMatch["Text",_]:= Nothing
DisplayArticlesWithMatch[source_,data_]:= {BoxForm`SummaryItem[{"Articles: ", ArticleWithMatchCount[source, data]}]}

(* LexicalSummary *)
LexicalSummary /: MakeBoxes[obj : LexicalSummary[asc_?LexicalSummaryAscQ], form : (StandardForm | TraditionalForm)] :=
Module[{above, below},
	above = {(*example grid*)
		{BoxForm`SummaryItem[{"Source: ", asc["Source"]}]},
		DisplayArticlesWithMatch[asc["Source"], asc["Data"]],
		{BoxForm`SummaryItem[{"Matches: ",asc["TotalMatchCount"]}]}
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
LexicalSummaryAscQ[asc_?AssociationQ] := AllTrue[{"Data", "Dataset", "Source", "TotalMatchCount", "TextElementStructure"}, KeyExistsQ[asc, #] &]
LexicalSummaryAscQ[_] = False;

(* Direct Properties *)
LexicalSummary[asc_?LexicalSummaryAscQ]["Data"] := asc["Data"]
LexicalSummary[asc_?LexicalSummaryAscQ]["Source"] := asc["Source"]
LexicalSummary[asc_?LexicalSummaryAscQ]["Dataset"] := asc["Dataset"]
LexicalSummary[asc_?LexicalSummaryAscQ]["TotalMatchCount"] := asc["TotalMatchCount"]
LexicalSummary[asc_?LexicalSummaryAscQ]["TextElementStructure"] := asc["TextElementStructure"]

(* Dataset Properties *)
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroupPercentages"] := PercentDataset[LexicalSummary[asc]["CountGroups"], asc["TotalMatchCount"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["LowercaseCountGroupPercentages"] := PercentDataset[LexicalSummary[asc]["CountGroups"] // CountSummaryLowercase, asc["TotalMatchCount"]]
(* Dataset Properties Filtered *)
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroupPercentages", n_Integer] := LexicalSummary[asc]["CountGroupPercentages"][;;UpTo[n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroupPercentages", DeleteStopwords] := LexicalSummary[asc]["CountGroupPercentages"][Select[Not@StopWordQ[#Matches]&]]
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroupPercentages", n_Integer, DeleteStopwords] := LexicalSummary[asc]["CountGroupPercentages", n][Select[Not@StopWordQ[#Matches]&]]

LexicalSummary[asc_?LexicalSummaryAscQ]["LowercaseCountGroupPercentages", n_Integer] := LexicalSummary[asc]["LowercaseCountGroupPercentages"][;;UpTo[n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["LowercaseCountGroupPercentages", DeleteStopwords] := LexicalSummary[asc]["LowercaseCountGroupPercentages"][Select[Not@StopWordQ[#Matches]&]]
LexicalSummary[asc_?LexicalSummaryAscQ]["LowercaseCountGroupPercentages", n_Integer, DeleteStopwords] := LexicalSummary[asc]["LowercaseCountGroupPercentages", n][Select[Not@StopWordQ[#Matches]&]]

(* Count Properties *)
LexicalSummary[asc_?LexicalSummaryAscQ]["Counts"] := GetDatasetCounts[LexicalSummary[asc]["Dataset"], asc["Source"]] // DeleteMissing[#, 1, 1] & // ReverseSortBy[#Count&]
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups"] := (LexicalSummary[asc]["Counts"] // CountGroups)
(* Count Properties Filtered *)
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups", n_Integer] := (LexicalSummary[asc]["CountGroups"][;;UpTo[n]])
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups", DeleteStopwords] := (LexicalSummary[asc]["Counts"] // FilterOutStopwordRows // CountGroups)
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups", n_Integer, DeleteStopwords] := (LexicalSummary[asc]["CountGroups", DeleteStopwords][;;UpTo[n]])


LexicalSummary[asc_?LexicalSummaryAscQ]["Survey"] := GenerateDashboard[LexicalSummary[asc]]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", n_Integer] := GenerateDashboard[LexicalSummary[asc], n]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", DeleteStopwords] := GenerateDashboard[LexicalSummary[asc], DeleteStopwords]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", n_Integer, DeleteStopwords] := GenerateDashboard[LexicalSummary[asc], n, DeleteStopwords]

LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups"] := PartOfSpeechGroups[LexicalSummary[asc]["CountGroups"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", n_Integer] := PartOfSpeechGroups[LexicalSummary[asc]["CountGroups", n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", DeleteStopwords] := PartOfSpeechGroups[LexicalSummary[asc]["CountGroups", DeleteStopwords]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", n_Integer,  DeleteStopwords] := PartOfSpeechGroups[LexicalSummary[asc]["CountGroups", n, DeleteStopwords]]

LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups"] := WordStemGroups[LexicalSummary[asc]["CountGroups"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups", n_Integer] := WordStemGroups[LexicalSummary[asc]["CountGroups", n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups", DeleteStopwords] := WordStemGroups[LexicalSummary[asc]["CountGroups", DeleteStopwords]]
LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups", n_Integer,  DeleteStopwords] := WordStemGroups[LexicalSummary[asc]["CountGroups", n, DeleteStopwords]]

LexicalSummary[asc_?LexicalSummaryAscQ][invalidkey_] := asc[invalidkey]
LexicalSummary[asc_?LexicalSummaryAscQ]["Properties"] := {"Data","Dataset","Counts","CountGroups","CountGroupPercentages", "LowercaseCountGroupPercentages","PartOfSpeechGroups", "WordStemCountGroups", "Source","TotalMatchCount","TextElementStructure", "Survey"}

GenerateLexicalSummary[data_?FailureQ, ___] := data
GenerateLexicalSummary[data_, sourceType_String, lexpatt_] := Monitor[
	iGenerateLexicalSummary[data, sourceType, lexpatt],
	Row[{"Generating Summary", ProgressIndicator[Appearance->"Ellipsis"]}]
	]

iGenerateLexicalSummary[data_, sourceType_String, lexpatt_] := Module[
	{MTC, DS = Dataset[data]},
	MTC = DeleteMissing[GetDatasetCounts[DS, sourceType], 1, 1][Total, "Count"];
	LexicalSummary[<|"Data" -> data, "Dataset" -> DS, "Source" -> sourceType, "TotalMatchCount" -> MTC, "TextElementStructure" -> ToTextElementStructure[StripNamedPattern@lexpatt] |>]
]

GetDatasetCounts[ds_Dataset,"Text"] := ds[All, <|"Match" -> "Match", "Count" -> "Position" /* Length|>]
GetDatasetCounts[ds_Dataset, "Wikipedia"] := ds[GroupBy["Match"], Map[KeyDrop[{"Article", "Match"}]]][All, Total@*Map[Length], "Position"] // KeyValueMap[<|"Match" -> #1, "Count" -> #2|> &]

CountGroups[ds_Dataset] := Query[
		GroupBy[#Count&] /* KeyValueMap[<|"Matches" -> #2, "CountGroup" -> #1|> &],
		KeyDrop["Count"] /* Values /* (Flatten[#,1]&)
	][ds]

CountGroupDSQ[ds_Dataset] :=Normal /* First /* KeyMemberQ["CountGroup"]@ds
CountDSQ[ds_Dataset] := Normal /* First /* KeyMemberQ["Count"]@ds

CountSummaryLowercase[ds_Dataset] /; CountDSQ[ds] := ReverseSortBy[#Count&]@Query[
	GroupBy[ToLowerCase[#Match] &] /* KeyValueMap[<|"Match" -> #1, #2|> &],
	Total /* KeyDrop["Match"]
	][ds]

ThreadMatchesWithCount[asc_Association] := Apply[Sequence][Map[<|"Matches" -> ToLowerCase[#], "Count" -> asc["CountGroup"]|> &][asc["Matches"]]]


CountSummaryLowercase[ds_Dataset] /; CountGroupDSQ[ds] := ReverseSortBy[#CountGroup&]@Query[
	GroupBy[#Count &] /* KeyValueMap[<|"Matches" -> #2, "CountGroup" -> #1|> &],
	KeyDrop["Count"] /* Values /* Flatten
	][
	Query[
		GroupBy[#Matches &] /* KeyValueMap[<|"Matches" -> #1, #2|> &],
		Total /* KeyDrop["Matches"]
		][ds[All, ThreadMatchesWithCount]]
	]

StopWordQ[s_String] := StringMatchQ[Alternatives @@ WordList["Stopwords"]][s]
StopWordQ[l : List[__String]] := AnyTrue[l, StopWordQ]

FilterOutStopwordRows[ds_Dataset] := Select[\[Not]StopWordQ[#Match] &][ds]

PartOfSpeechKey[word_String] := ProcessWordData[WordData[word, "PartsOfSpeech"]]
ProcessWordData[w_WordData] := None
ProcessWordData[w_] := w

(* TODO: module-ize these functions for clarity *)
PartOfSpeechGroups[ds_Dataset] := (ds[TextWords /* ToLowerCase /* Flatten /* DeleteStopwords /* DeleteDuplicates, "Matches"][AlphabeticSort][GroupBy[PartOfSpeechKey]][KeyDrop[None]] // KeySort) // KeyValueMap[<|"Words" -> #2, "PartOfSpeech" -> #1|>&]

GetWordStemCounts[ds_Dataset] := (ds[All, "Matches"] // Normal // Flatten // (StringSplit[#, WordBoundary]&) /* Flatten /* DeleteCases[" "|"\n"] // DeleteStopwords // ToLowerCase // WordStem // Counts)

WordStemGroups[ds_Dataset] := (GetWordStemCounts[ds] // KeyValueMap[<|"Stem" -> #1, "Count" -> #2|> &] // Dataset // ReverseSortBy[#Count &]) // Query[GroupBy[#Count &], KeyDrop["Count"] /* Values /* (Flatten[#, 1]&)] // KeyValueMap[<|"WordStem" -> #2, "CountGroup" -> #1|> &]

PercentDataset[ds_Dataset, matchcount_Integer] := (ds[
	All, <|"Matches" -> #Matches, "Percentage" ->Interpreter["Percent"]@*ToString@*PercentForm@*N@((#CountGroup Length[#Matches])/matchcount)|> &][ReverseSortBy["Percentage"]])

GenerateDashboard[lps_LexicalSummary, params___] := With[
	{
		cogp = lps["CountGroupPercentages",params],
		lccp = lps["LowercaseCountGroupPercentages",params],
		posg = lps["PartOfSpeechGroups", params],
		wdsg = lps["WordStemCountGroups", params]
		},
		DynamicModule[
			{tab="GroupPercentages"},
				Column[{
					Panel[SetterBar[Dynamic[tab],{"GroupPercentages","LowercaseGroupPercentages","PartOfSpeechGroups", "WordStemCountGroups"}]],
					Dynamic[
						Switch[tab,
							"GroupPercentages",cogp,
							"LowercaseGroupPercentages",lccp,
							"PartOfSpeechGroups", posg,
							"WordStemCountGroups", wdsg
							]]
					}]
			]
		]
		
End[]
EndPackage[]
