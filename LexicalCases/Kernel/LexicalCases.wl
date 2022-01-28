(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Extract and analyze text type sequences with the Wolfram Language.*)


BeginPackage["LexicalCases`"]
(* Main *)
LexicalCases::usage = "LexicalCases[source, se] extract cases of LexicalPattern se from Text source"
ValidSeQ::usage = "ValidSeQ[expr] returns True if expr is a String, StringExpression, or a StringExpression is the lhs of Rule/RuleDelayed"
(* Samples *)
$SampleStringShort::usage="A short example string"
$SampleStringLong::usage="A long example string"
$SampleStringExpression::usage="A sample text pattern used for testing"

(* Summary *)
LexicalSummary::usage = "A summary of LexicalCases results. Run LexicalSummary[<>][\"Properties\"] for a list or properties"
CountSummaryLowercase::usage = "CountSummaryLowercase[LexicalSummary[<>][\"Counts\"]] Converts matches to LowerCase and consolidates results\nCountSummaryLowercase[LexicalSummary[<>][\"CountGroups\"]] Converts matches to LowerCase and consolidates results"

(* Patterns *)
ExpandPattern::usage = "ExpandPattern[se] expands top level pattern objects in StringExpression se"

ExpandStringExpression::usage = "ExpandStringExpression[se] expands symbols and patterns into final form for searching"

ContentAssociation::usage = "ContentAssociation[source, se] extracts text-types from se and returns and returns an association of the form \"Type\" \[Rule] example1 | \[Ellipsis] | example$$i"
ExtractContentTypes::usage = "ExtractContentTypes[se] extracts text content type specifications from se"

Opt::usage = "Opt[se] matches se, \" \", or \"\""
TextType::usage = "TextType[type] a symbolic wrapper for TextContentTypes"
BoundedString::usage = "BoundedString[s] represents the string WordBoundary~~s~~WordBoundary\nBoundedString[s1|\[Ellipsis]|si] represents the strings WordBoundary~~s1|\[Ellipsis]|si~~WordBoundary"
Words::usage = "Words[n] represents n words separated by spaces"
Sandwich::usage = "Sandwich[bread, expr] sandwiches expr between bread like so: bread~~expr~~bread"

(* Format *)
Structure::usage="Structure[se] Visualize the structure of the StringExpression"

(* Services *)
$LexicalCasesServices::usage = "List of supported services"
ToWikipediaSearchQuery::usage = "ConvertToWikipediaSearchQuery[se] converts se to a form suitable for WikipediaSearch"

Begin["Private`"]

(* Samples *)
$SampleStringExpression = StringExpression[TextType["Adjective"], " key lime pie"];
(* From https://randomwordgenerator.com/sentence.php *)
$SampleStringShort = "The best key lime pie is still up for debate."
(* From TextCases[WikipediaData["computer"], "Paragraph"] // First *)
$SampleStringLong = "A computer is a machine that can be programmed to carry out \
sequences of arithmetic or logical operations automatically. Modern \
computers can perform generic sets of operations known as programs. \
These programs enable computers to perform a wide range of tasks. A \
computer system is a \"complete\" computer that includes the \
hardware, operating system (main software), and peripheral equipment \
needed and used for \"full\" operation. This term may also refer to a \
group of computers that are linked and function together, such as a \
computer network or computer cluster.
A broad range of industrial and consumer products use computers as \
control systems. Simple special-purpose devices like microwave ovens \
and remote controls are included, as are factory devices like \
industrial robots and computer-aided design, as well as \
general-purpose devices like personal computers and mobile devices \
like smartphones. Computers power the Internet, which links hundreds \
of millions of other computers and users.
Early computers were meant to be used only for calculations. Simple \
manual instruments like the abacus have aided people in doing \
calculations since ancient times. Early in the Industrial Revolution, \
some mechanical devices were built to automate long tedious tasks, \
such as guiding patterns for looms. More sophisticated electrical \
machines did specialized analog calculations in the early 20th \
century. The first digital electronic calculating machines were \
developed during World War II. The first semiconductor transistors in \
the late 1940s were followed by the silicon-based MOSFET (MOS \
transistor) and monolithic integrated circuit (IC) chip technologies \
in the late 1950s, leading to the microprocessor and the \
microcomputer revolution in the 1970s. The speed, power and \
versatility of computers have been increasing dramatically ever since \
then, with transistor counts increasing at a rapid pace (as predicted \
by Moore's law), leading to the Digital Revolution during the late \
20th to early 21st centuries.
Conventionally, a modern computer consists of at least one processing \
element, typically a central processing unit (CPU) in the form of a \
microprocessor, along with some type of computer memory, typically \
semiconductor memory chips. The processing element carries out \
arithmetic and logical operations, and a sequencing and control unit \
can change the order of operations in response to stored information. \
Peripheral devices include input devices (keyboards, mice, joystick, \
etc.), output devices (monitor screens, printers, etc.), and \
input/output devices that perform both functions (e.g., the 2000s-era \
touchscreen). Peripheral devices allow information to be retrieved \
from an external source and they enable the result of operations to \
be saved and retrieved."


(* Utils *)
optionsJoin[sym__Symbol]:=(Map[Options]/*Apply[Join])[{sym}]


(* Expressions *)
extractHeads[expr_] := Cases[expr, h_[___] :> h, {0, Infinity}]

SetAttributes[ValidSeQ, HoldAll]
ValidSeQ[expr_StringExpression]:= True
ValidSeQ[Rule[expr_?ValidSeQ,_]]:= True
ValidSeQ[RuleDelayed[expr_?ValidSeQ,_]]:= True
ValidSeQ[expr_String]:= True
ValidSeQ[_]:= False


ContainsPatternHeadsQ[se_?ValidSeQ] := ContainsAny[extractHeads[se], {Pattern}]
StripNamedPattern[se_?ValidSeQ] := StripNames[ContainsPatternHeadsQ[se], se]
StripNames[True, se_StringExpression] := Replace[se, p_Pattern :> Extract[2][p], Infinity]
StripNames[True, (Rule|RuleDelayed)[se_StringExpression,_]] := Replace[se, p_Pattern :> Extract[2][p], Infinity]
StripNames[False,se_?ValidSeQ]:= se

ExtractAlternatives[List[a_Alternatives]] := a
ExtractAlternatives[a_] := a

PostProcessAlternatives[alts_Alternatives] := {alts}
PostProcessAlternatives[te_] := te

PrependArticleKey[{article_String, data_List}] := Map[Apply[Prepend]]@Thread[{data, "Article" -> article}]
PrependArticleKey[{article_String, data_Missing}] := <|"Article" -> article, "Match" -> data|>

PrependArticleKey[{article_Integer, data_List}] := Map[Apply[Prepend]]@Thread[{data, "Article" -> article}]
PrependArticleKey[{article_Integer, data_Missing}] := <|"Article" -> article, "Match" -> data|>

(* Text Cleanup *)
EscapePunctuation[s_String] := StringReplace[s, pc : PunctuationCharacter :> "\\" <> pc]

articlePluralize::noa = "No articles found"
articlePluralize[0] := Message[articlePluralize::noa]
articlePluralize[1] := "article"
articlePluralize[_Integer?Positive] := "articles"

(* Format *)
SetAttributes[TextElementFormat, HoldAll]
TextElementFormat[StringExpression[args___]] := TextElementFormat[StringExpression, args];
TextElementFormat[TextType[type_String]] := TextElement[{type}, <|"GrammaticalUnit" -> "TextType"|>];
TextElementFormat[TextType[types_Alternatives]] := TextElement[PostProcessAlternatives[Map[TextElementFormat][ExpandAlternativeTextTypes[types]]], <|"GrammaticalUnit" -> "Alternatives"|>];
TextElementFormat[Opt[args__]] := TextElement[Map[TextElementFormat][{args}], <|"GrammaticalUnit" -> "Optional"|>];
TextElementFormat[AnyOrder[args__]] := TextElement[Map[TextElementFormat][{args}], <|"GrammaticalUnit" -> "AnyOrder"|>]
TextElementFormat[FixedOrder[args__]] := TextElement[Map[TextElementFormat][{args}], <|"GrammaticalUnit" -> "FixedOrder"|>]
TextElementFormat[a_Alternatives] := TextElement[PostProcessAlternatives[Map[TextElementFormat][a]], <|"GrammaticalUnit" -> "Alternatives"|>];
TextElementFormat[s_String] := TextElement[{s}, <|"GrammaticalUnit" -> "Text"|>];
TextElementFormat[s_Symbol] := s;
TextElementFormat[h_] := TextElementFormat[Extract[0][h],Extract[1][h]];
TextElementFormat[TextType, s_String] := TextElement[s, <|"GrammaticalUnit" -> "TextType"|>];
TextElementFormat[h_, args__] := TextElement[Map[TextElementFormat][{args}], <|"GrammaticalUnit" -> ToString[h]|>];

Structure[se_StringExpression] := TextElementFormat[se];
Structure[(Rule|RuleDelayed)[se_StringExpression,_]] := Construct[TextElementFormat, StripNamedPattern[se]];

(* Service Utils *)
$LexicalCasesServices = {"Wikipedia"}

ArticleIndex = 0;

ArticleSearchIndicator[service_String, query_] := Row[{
	"Searching "<>service<>" for ", query, ProgressIndicator[Appearance->"Ellipsis"]
	}]

(* Match Utils *)
startTrim[True] := 1
startTrim[False] := 0
endTrim[True] := -1
endTrim[False] := 0

mTrimPositions[m_String, psns : {{_, _} ..}] := Map[p |-> Through[{startTrim@*StringStartsQ[" "], endTrim@*StringEndsQ[" "]}[m]] +p][psns]

mTrimPositions[_,p_]:= p

MatchTrim[True, matches_List]:= Query[
	All,
	<|"Match" -> StringTrim[#Match],"Position" -> (mTrimPositions[#Match, #Position])|> &
	][matches]
		
MatchTrim[False, matches_List]:= matches

MatchTrim[boole:(True|False)][matches_List] := MatchTrim[boole,matches]

ReplaceEmptyListWithMissing[result_]:= Replace[result, {} -> Missing["NoMatches"], 1];

(* Patterns *)
MatchBoundary[patt_] := Except[WordCharacter,WordBoundary|" "|StartOfString|StartOfLine]~~patt~~Except[WordCharacter,WordBoundary|" "|EndOfString|EndOfLine]

Words[n_Integer] := RegularExpression["(\\s?\\b\\w+\\b\\s?){" <> ToString[n] <> "}"]
Words[m_Integer, n_Integer] := RegularExpression["(\\s?\\b\\w+\\b\\s?){" <> ToString[m] <> "," <> ToString[n] <> "}"]

Sandwich[bread_, expr_]:= bread~~expr~~bread
Sandwich[bread_][expr_] := Sandwich[bread, expr]

ExpandAlternativeTextTypes[alts_Alternatives] := (Apply[Alternatives]@*Map[TextType]@*Apply[List])[alts]

iExpandPattern[expr_]:= ReplaceAll[expr, {
	Opt[opt_Alternatives] :> (Map[MatchBoundary][opt]~Join~Alternatives[" ",""]),
	Opt[opt_] :> (Alternatives[MatchBoundary[opt]]~Join~Alternatives[" ",""]),
	TextType[alts_Alternatives] :> ExpandAlternativeTextTypes[alts],
	BoundedString[s:(_String|_FixedOrder|_StringExpression)] :> (WordBoundary|StartOfString|StartOfLine ~~ iExpandPattern[s] ~~ WordBoundary|EndOfString|EndOfLine),
	BoundedString[a_Alternatives] :> (WordBoundary ~~ Map[iExpandPattern][a] ~~ WordBoundary)
	}]

ExpandPattern[se_?ValidSeQ] := iExpandPattern[se]

ExtractStringContentTypes[se_StringExpression] := Splice[Cases[se, TextType[type_String] :> type, Infinity]];
ExtractAlternativeContentTypes[se_StringExpression] := Splice[Cases[se, TextType[type_Alternatives] :> Splice[List@@type], Infinity]];

ExtractContentTypes[se_StringExpression] := Through[{ExtractStringContentTypes, ExtractAlternativeContentTypes}[se]]

ContentAssociation[st_String, (Rule|RuleDelayed)[se_StringExpression,_]] := ContentAssociation[st, se]
ContentAssociation[sourcetext_String, se_StringExpression] := Map[ExtractAlternatives]@Merge[Identity]@KeyValueMap[<|#1 -> Alternatives@@DeleteDuplicates@#2|> &][TextCases[sourcetext, ExtractContentTypes[se]]]


ContentAlts[List[a_Alternatives]] := a
ContentAlts[a_Alternatives] := a

ExpandStringExpression[sourcetext_String, se_?ValidSeQ] :=
	Module[{TRX, CA},
		TRX = ExpandPattern[se];
		CA  = ContentAssociation[sourcetext, se];
		Replace[TRX, TextType[type_] :> MatchBoundary[ContentAlts[CA[type]]], Infinity]
		]

ExpandStringExpression[sourcetext_String, Rule[se_StringExpression, expr_]] := Rule[ExpandStringExpression[sourcetext, se], expr]
ExpandStringExpression[sourcetext_String, RuleDelayed[se_StringExpression, expr_]] := RuleDelayed[ExpandStringExpression[sourcetext, se], expr]

(* Wikipedia *)
InsertAnd[l:List[_]] := l;
InsertAnd[x_List] := Insert[x, "and", -2];

WikipediaKeywordString[{s_String}]:= "\""<>s<>"\"";
WikipediaKeywordString[l:{_String,_String}]:= StringRiffle[Map[WikipediaKeywordString][l], ", "];
WikipediaKeywordString[x_List] := StringRiffle[InsertAnd[Map[WikipediaKeywordString][x]], ", "];
WikipediaKeywordString[x_Alternatives] := ToString[Map[WikipediaKeywordString][x]]
WikipediaKeywordString[x_] := "\""<>x<>"\"";


WikipediaSearchQuery[List[],se_StringExpression] := Message[ToWikipediaSearchQuery::novq, se]
WikipediaSearchQuery[wsq:List[__String],se_StringExpression] := StringRiffle[wsq]
WikipediaSearchQuery[wsq:List[List[__String]],se_StringExpression] := Flatten[wsq]
WikipediaSearchQuery[wsq_List,se_StringExpression] := Cases[List[(_List|_String)..]][wsq] // Map[StringRiffle]

ToWikipediaSearchQuery[se_StringExpression]:= Module[
	{cleanLexicalPattern, stage1},
	cleanLexicalPattern = DeleteCases[List@@se, (_TextType | _Opt | _AnyOrder), All];
	stage1 = ReplaceAll[cleanLexicalPattern, {BoundedString[s_] :> s, Alternatives -> List}] // DeleteCases[" "];
	Check[WikipediaSearchQuery[stage1, se] // StringReplace[(" " ..) -> " "] // StringTrim, Return[$Failed, Module]]
	]

ToWikipediaSearchQuery[Rule[se_StringExpression,_]] := ToWikipediaSearchQuery[StripNamedPattern[se]]
ToWikipediaSearchQuery[RuleDelayed[se_StringExpression,_]] := ToWikipediaSearchQuery[StripNamedPattern[se]]

ToWikipediaSearchQuery::novq = "Keyword formulation not supported for the StringExpression ``. Consider using the \"Content\" option to supply keywords, or trying a different StringExpression."
Options[WikipediaArticlesFromRule] = {
	MaxItems -> 50
};

WikipediaArticlesFromRule["Content" -> a_Alternatives, opts:OptionsPattern[]]:= Module[
	{KWL = Apply[List][a], RULES},
	RULES = Thread["Content"-> KWL];
	Flatten@Map[r |-> WikipediaArticlesFromRule[r, MaxItems -> (Ceiling[OptionValue[MaxItems]/Length[KWL]]), FilterRules[opts, Except[MaxItems]]]][RULES]
]
WikipediaArticlesFromRule[rule:("Content" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule, Sequence@@FilterRules[{opts}, optionsJoin[WikipediaSearch,iSearchWikipedia]]]


iGetCategoryArticles[categories_List, n_Integer] := (Take[#, n]&)@*DeleteMissing@*DeleteDuplicates@*Flatten@*Map[WikipediaData["Category" -> #, "CategoryArticles"]&]@categories

WikipediaArticlesFromRule[rule:("Category" -> _), opts:OptionsPattern[{WikipediaArticlesFromRule, WikipediaSearch, iSearchWikipedia}]]:= iGetCategoryArticles[
	WikipediaSearch[rule, Sequence@@FilterRules[{opts}, optionsJoin[WikipediaSearch,iSearchWikipedia]]],
	OptionValue[MaxItems]
	]


iGetWikipediaArticles[query_Rule, opts___] := Module[
	{ART, ARC, MTL, SQR = WikipediaKeywordString[Values[query]], TXT},
	ART = Monitor[
		WikipediaArticlesFromRule[query, FilterRules[{opts}, optionsJoin[WikipediaSearch,iSearchWikipedia]]],
		ArticleSearchIndicator["Wikipedia", SQR]
		];
	ARC = Length[ART];
	MTL = First@TakeLargestBy[StringLength,1][ART->"Value"];
	ASC = <|"Articles" -> ART, "ArticleCount" -> ARC, "ArticleCountString" -> ToString[ARC], "MaxTitleLength" -> MTL|>;
	TXT = iGetWikipediaArticleText[ASC["Articles"], ASC["ArticleCount"], ASC["ArticleCountString"], ASC["MaxTitleLength"]];
	<|"Text" -> TXT|>~Join~ASC
	]

iGetWikipediaArticleText[articles_List, articleCount_Integer, articleCountString_String, maxTitleLength_Integer] := Module[
	{TXT},
		SetSharedVariable[ArticleIndex];
		ArticleIndex=0;
		TXT=Monitor[
			ParallelMap[(++ArticleIndex;WikipediaData[#])&, articles],
			Row[{
				"Gathering text from "<>articleCountString<>" "<>articlePluralize[articleCount]<>":\n",
				Dynamic[
					Which[
					(ArticleIndex <= articleCount-1),StringPadRight["\""<>articles[[ArticleIndex+1]]<>"\"",maxTitleLength],
					(ArticleIndex === 1), "\""<>articles[[1]]<>"\" ",
					True, ""
					]
				],
				"\n",
				ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]
				}
				]
			];
			ArticleIndex=.;
			TXT
	]



(* LexicalCases *)
Options[LexicalCases]={
	"Service" -> "Wikipedia",
	"StringTrim" -> True,
	IgnoreCase -> False,
	Overlaps -> False
};

LexicalCases::unsupobj=Import::unsupobj;
GetFileExtension[file_File] := Information[file, "FileExtension"]
SupportedFileQ[file_File] := MemberQ[{"txt", "md", "csv", "tsv"}, GetFileExtension[file]]
LexicalCases[file_File, args___] /; SupportedFileQ[file] := Module[{data = Import[file]}, LexicalCases[data, args]]
LexicalCases[file_File, args___] := Message[LexicalCases::unsupobj, GetFileExtension[file]]

LexicalCases[input:List[__String],se_?ValidSeQ, opts:OptionsPattern[LexicalCases]] /; AllTrue[DirectoryQ \[Or] FileExistsQ][input] := Module[
	{files = Map[File][input]},
	iLexicalCases[files, se, opts]
	]

LexicalCases[input:(List[__File]|List[__String]),se_?ValidSeQ, opts:OptionsPattern[LexicalCases]] := iLexicalCases[input, se, opts]

LexicalCases[Rule[index_SearchIndexObject, query_], se_?ValidSeQ, opts:OptionsPattern[LexicalCases]] := Module[
	{files = Map[File][TextSearch[index, query][All, "Location"]]},
	iLexicalCases[files, se, opts]
	]

(* SourceText and LexicalPattern Input *)
LexicalCases[sourcetext_String, se_?ValidSeQ, opts:OptionsPattern[LexicalCases]]:= Module[
	{LPC,RES},
	ArticleIndex=0;
	(* Find Matches *)
	LPC = Monitor[
		LexicalCasesOnString[sourcetext, se, opts],
		Row[{"Searching", ProgressIndicator[Appearance->"Ellipsis"]}]
		];
	(* Generate Summary Object *)
	RES = Monitor[
		GenerateLexicalSummary[LPC, "Text", se],
		Row[{"Generating LexicalSummary", ProgressIndicator[Appearance->"Necklace"]}]
	];
	ArticleIndex=.;
	RES
	]

(* SourceText is a string *)
LexicalCasesOnString[source_String, se_?ValidSeQ, opts:OptionsPattern[LexicalCases]]:=Module[
	{RX, S = EscapePunctuation[source]},
	++ArticleIndex;
	RX = ExpandStringExpression[S, se];
	
	MatchTrim[OptionValue["StringTrim"]]@
	Query[
		GroupBy[#Match &] /* (KeyValueMap[<|"Match" -> #1, "Position" -> #2|> &]),
		KeyDrop["Match"] /* Values /* (Flatten[#, 1] &)
		]@
	Map[AssociationThread[{"Match", "Position"} -> #] &]@
	Transpose@{
		StringCases[source,RX, IgnoreCase -> OptionValue[IgnoreCase], Overlaps -> OptionValue[Overlaps]],
		StringPosition[source, StripNamedPattern[RX], IgnoreCase -> OptionValue[IgnoreCase], Overlaps -> OptionValue[Overlaps]]
	}
	]

(* LexicalPattern on Service *)
LexicalCases[se_?ValidSeQ, opts:OptionsPattern[{LexicalCases, iSearchWikipedia}]]:= LexicalCasesFromService[OptionValue["Service"], se, FilterRules[{opts}, Options[iSearchWikipedia]]]

(* WikiQueryRyle and LexicalPattern Input *)
LexicalCases[query_Rule, se_?ValidSeQ, opts:OptionsPattern[{LexicalCases, iSearchWikipedia}]]:= LexicalCasesFromService[OptionValue["Service"], query, se, FilterRules[{opts}, Options[iSearchWikipedia]]]

Options[LexicalCasesFromService]={

};

StringSearch[text_,pattern_] := Module[
	{RES = LexicalCasesOnString[text, pattern]},
		MatchCount+=Length[Replace[_Missing -> {}]@Flatten[Lookup[RES,"Position"], 1]];
		RES
		]

SearchArticles[texts_List, se_?ValidSeQ, articles_List, articleCount_Integer, maxTitleLength_Integer] := Module[
	{MAT},
		ArticleIndex=0;
		MatchCount=0;
		SetSharedVariable[se,MatchCount];
		MAT = Monitor[
			ParallelMap[(StringSearch[#,se])&, texts],
			Row[{
				"Searching ", Dynamic[
					Which[
						(ArticleIndex <= articleCount-1), "\""<>articles[[ArticleIndex+1]]<>"\" ",
						(ArticleIndex === 1), "\""<>articles[[1]]<>"\" ",
						True," "
						]
				],
				"\n",
				"Found: ", Dynamic[MatchCount], "\n",
				ProgressIndicator[Dynamic[ArticleIndex],{0,articleCount}]," ",Dynamic[NumberForm[PercentForm[N[ArticleIndex/articleCount]],{3,2}]]
				}]
			];
		ArticleIndex=.;
		MatchCount=.;
		MAT
	]

PackageResults[matches_, articles_List, articleCount_Integer, maxTitleLength_Integer] := Module[
	{AMT, MAC},
		AMT = Thread[{articles, ReplaceEmptyListWithMissing[matches]}];
		SetSharedVariable[AMT,ArticleIndex];
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


Options[iSearchWikipedia] = {
	MaxItems -> 50,
	Language -> "English"
};

iSearchWikipedia[se_?ValidSeQ, opts:OptionsPattern[{iSearchWikipedia}]] := With[
	{wikiquery = StringTrim@ToWikipediaSearchQuery[se]},
	iSearchWikipedia[wikiquery,se,opts]
	]

iSearchWikipedia[query_?FailureQ,___] := Return[query, With]

iSearchWikipedia[query_Rule, se_?ValidSeQ, opts:OptionsPattern[{iSearchWikipedia}]] := iLexicalCases[query, se, opts]
		
iSearchWikipedia[query:(_String|_List), se_?ValidSeQ, opts:OptionsPattern[{iSearchWikipedia}]] := iLexicalCases["Content" -> query, se, opts]

LexicalCasesFromService["Wikipedia", se_?ValidSeQ, opts:OptionsPattern[]] := GenerateLexicalSummary[iSearchWikipedia[se, opts], "Wikipedia", se]
LexicalCasesFromService["Wikipedia", query:(_Rule|_String|_List), se_?ValidSeQ, opts:OptionsPattern[]]:= GenerateLexicalSummary[iSearchWikipedia[query, se, opts], "Wikipedia", se]

GetText[input:List[__string], opts:OptionsPattern[]] := Module[
	{SE = se, LEN = Length[texts], REN, ard, art, arc, acs, mtl, mtc},
	REN = Range[LEN];
	<|"Text" -> input, "Articles" -> Map[ToString][REN], "ArticleCount" -> LEN, "ArticleCountString" -> ToString[LEN], "MaxTitleLength" -> (First@TakeLargestBy[REN -> "Value", IntegerDigits /* Length,1])|>
	]

GetText[input:List[__File], opts:OptionsPattern[]] := Module[
	{SE = se, LEN = Length[input], REN = Map[FileNameTake][input], ard, art, arc, acs, mtl, mtc},
	<|"Text" -> Map[Import[#, "Text"]&][input], "Articles" -> REN, "ArticleCount" -> LEN, "ArticleCountString" -> ToString[LEN], "MaxTitleLength" -> (First@TakeLargestBy[REN -> "Value", IntegerDigits /* Length,1])|>
	]

	(*
	TODO: Add the following services
		{"ArXiv","CrossRef","Dropbox","Facebook","GoogleCustomSearch","Instagram","OpenLibrary","PubMed","Reddit","SurveyMonkey","Twilio","Twitter","Wikipedia"}
		*)
GetText[input_Rule, opts:OptionsPattern[]] := iGetWikipediaArticles[input, opts]

iLexicalCases[input:(List[__String]|List[__File]|_Rule), se_?ValidSeQ, opts:OptionsPattern[]] := Module[
	{ard, src, art, arc, acs, mtl, mtc},
		ard = GetText[input, opts];
		
		src = ard["Text"];
		art = ard["Articles"];
		arc = ard["ArticleCount"];
		acs = ard["ArticleCountString"];
		mtl = ard["MaxTitleLength"];
		
		(* 3 - Search for LexicalPattern *)
		mtc = SearchArticles[src, se, art, arc, mtl];
		PackageResults[mtc, art, arc, mtl]
		]


(* LexicalSummary *)
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
LexicalSummaryAscQ[asc_?AssociationQ] := AllTrue[{"Data", "Dataset", "Source", "TotalMatchCount", "Structure"}, KeyExistsQ[asc, #] &]
LexicalSummaryAscQ[_] = False;

(* Direct Properties *)
LexicalSummary[asc_?LexicalSummaryAscQ]["Data"] := asc["Data"]
LexicalSummary[asc_?LexicalSummaryAscQ]["Source"] := asc["Source"]
LexicalSummary[asc_?LexicalSummaryAscQ]["Dataset"] := asc["Dataset"]
LexicalSummary[asc_?LexicalSummaryAscQ]["TotalMatchCount"] := asc["TotalMatchCount"]
LexicalSummary[asc_?LexicalSummaryAscQ]["Structure"] := asc["Structure"]

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
LexicalSummary[asc_?LexicalSummaryAscQ]["Properties"] := {"Data","Dataset","Counts","CountGroups","CountGroupPercentages", "LowercaseCountGroupPercentages","PartOfSpeechGroups", "WordStemCountGroups", "Source","TotalMatchCount","Structure", "Survey"}

GenerateLexicalSummary[data_?FailureQ, ___] := data
GenerateLexicalSummary[data_, sourceType_String, se_?ValidSeQ] := Monitor[
	iGenerateLexicalSummary[data, sourceType, se],
	Row[{"Generating Summary", ProgressIndicator[Appearance->"Ellipsis"]}]
	]

iGenerateLexicalSummary[data_, sourceType_String, se_?ValidSeQ] := Module[
	{MTC, DS = Dataset[data]},
	MTC = DeleteMissing[GetDatasetCounts[DS, sourceType], 1, 1][Total, "Count"];
	LexicalSummary[<|"Data" -> data, "Dataset" -> DS, "Source" -> sourceType, "TotalMatchCount" -> MTC, "Structure" -> Structure[StripNamedPattern@se] |>]
]

(* Summary Utils *)
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
	KeyDrop["Count"] /* Values /* (Flatten[#,1]&)
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
