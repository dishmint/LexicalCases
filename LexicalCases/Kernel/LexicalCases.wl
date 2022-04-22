(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Extract and analyze text type sequences with the Wolfram Language.*)


BeginPackage["LexicalCases`"]
(* Main *)
LexicalCases::usage = "LexicalCases[source, se] extract cases of LexicalPattern se from Text source"
LexicalPatternQ::usage = "LexicalPatternQ[expr] returns True if expr is valid input for LexicalCases"
(* Samples *)
$SampleStringShort::usage="A short example string"
$SampleStringLong::usage="A long example string"
$SampleStringExpression::usage="A sample text pattern used for testing"

(* Summary *)
LexicalSummary::usage = "A summary of LexicalCases results. Run LexicalSummary[<>][\"Properties\"] for a list or properties"
CountSummaryLowercase::usage = "CountSummaryLowercase[LexicalSummary[<>][\"Counts\"]] Converts matches to LowerCase and consolidates results\nCountSummaryLowercase[LexicalSummary[<>][\"CountGroups\"]] Converts matches to LowerCase and consolidates results"
StopWordQ::usage = "StopWordQ[s] returns True if s is a stop word."
(* Patterns *)

TextType::usage = "TextType[type] a symbolic wrapper for TextContentTypes"
OptionalToken::usage = "OptionalToken[se] matches se, \" \", or \"\""
BoundToken::usage = "BoundToken[expr] sandwiches expr with boundaries\nBounded[s1|\[Ellipsis]|si] sandwiches the set of si with boundaries"
WordToken::usage = "WordToken[n] represents n words separated by spaces\nWordToken[m,n] represents m to n words separated by spaces"
Sandwich::usage = "Sandwich[outer, inner] sandwiches inner between outer"

ExpandPattern::usage = "ExpandPattern[patt] expands patt into a valid StringExpression"

LexicalPattern::usage = "LexicalPattern[se] A wrapper for using lexical patterns in string functions"

(* Format *)
LexicalStructure::usage="LexicalStructure[se] Visualize the structure of the StringExpression"

(* Services *)
$LexicalCasesServices::usage = "List of supported services"

(* Analysis / Visualization *)
LexicalDispersionPlot::usage = "LexicalDispersionPlot[text, w] plots the dispersion of word w across text\nLexicalDispersionPlot[text, {w$$1, $$, w$$i}] plots the dispersion of the w$$i across text"

Begin["`Private`"]
Needs["LexicalCases`Samples`"]
Needs["LexicalCases`Utilities`"]
Needs["LexicalCases`LexicalPattern`"]

(* Validation *)
$ValidLexicalTokens = (_TextType|_OptionalToken|_BoundToken|_WordToken)
extractLexicalTokens[expr_] := Cases[expr, $ValidLexicalTokens, {0, Infinity}];

ValidateLexicalToken[TextType[_String]] := True
ValidateLexicalToken[TextType[a_Alternatives]] := AllTrue[StringQ][List @@ a]
ValidateLexicalToken[OptionalToken[a_Alternatives]] := AllTrue[LexicalPatternQ][List @@ a]
ValidateLexicalToken[OptionalToken[opt_]] := LexicalPatternQ[opt]
ValidateLexicalToken[BoundToken[a_Alternatives]] := AllTrue[LexicalPatternQ][List @@ a]
ValidateLexicalToken[BoundToken[e:Except[_Alternatives]]] := LexicalPatternQ[e]
ValidateLexicalToken[WordToken[n_Integer]] := True
ValidateLexicalToken[WordToken[m_Integer, n_Integer]] := True
ValidateLexicalToken[expr_] := Message[LexicalCases::invld, expr];False

LexicalCases::invld = "`1` is not a valid lexical token"

SetAttributes[LexicalPatternQ, HoldAll]

LexicalPatternQ[expr_]:= Module[
	{
		se = Replace[expr, $ValidLexicalTokens :> " ", Infinity],
		lt = extractLexicalTokens[expr]
		},
	Check[GeneralUtilities`StringPatternQ[se] \[Or] AllTrue[ValidateLexicalToken, lt], $Failed]
		];
LexicalPatternQ[Rule[expr_?LexicalPatternQ,_]]:= True;
LexicalPatternQ[RuleDelayed[expr_?LexicalPatternQ,_]]:= True;
LexicalPatternQ[expr_?GeneralUtilities`StringPatternQ]:= True;


ContainsPatternHeadsQ[se_?LexicalPatternQ] := ContainsAny[ExtractHeads[se], {Pattern}]

StripNamedPattern[se_?LexicalPatternQ] := StripNames[ContainsPatternHeadsQ[se], se]
StripNames[True, HoldPattern[(Rule|RuleDelayed)[se_?LexicalPatternQ,_]]] := Replace[se, p_Pattern :> Extract[2][p], Infinity]
StripNames[True, c_Condition] := c
StripNames[True, se_?LexicalPatternQ] := Replace[se, p_Pattern :> Extract[2][p], Infinity]
StripNames[False,se_?LexicalPatternQ]:= se

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
FormatToken[StringExpression[args___]] := FormatToken[StringExpression, args];
FormatToken[TextType[type_String]] := TextElement[{type}, <|"GrammaticalUnit" -> "TextType"|>];
FormatToken[TextType[Containing[outer_,inner_]]] := TextElement[{inner}, <|"GrammaticalUnit" -> outer|>];
FormatToken[TextType[types_Alternatives]] := TextElement[WrapAlternatives[Map[FormatToken][ExpandAlternativeTextTypes[types]]], <|"GrammaticalUnit" -> "Alternatives"|>];
FormatToken[OptionalToken[args__]] := TextElement[Map[FormatToken][{args}], <|"GrammaticalUnit" -> "Optional"|>];
FormatToken[AnyOrder[args__]] := TextElement[Map[FormatToken][{args}], <|"GrammaticalUnit" -> "AnyOrder"|>]
FormatToken[FixedOrder[args__]] := TextElement[Map[FormatToken][{args}], <|"GrammaticalUnit" -> "FixedOrder"|>]
FormatToken[HoldPattern[WordToken[1]]] := TextElement[{1},<|"GrammaticalUnit" -> "Word"|>];
FormatToken[HoldPattern[WordToken[n_Integer]]] := TextElement[{n},<|"GrammaticalUnit" -> "Words"|>];
FormatToken[WordToken[args__Integer]] := TextElement[{Span[args]},<|"GrammaticalUnit" -> "Words"|>];
FormatToken[HoldPattern[_]] := TextElement[{"_"}, <|"GrammaticalUnit" -> "Blank"|>];
FormatToken[HoldPattern[__]] := TextElement[{"__"}, <|"GrammaticalUnit" -> "BlankSequence"|>];
FormatToken[HoldPattern[___]] := TextElement[{"___"}, <|"GrammaticalUnit" -> "BlankNullSequence"|>];
FormatToken[HoldPattern[h:(Blank|BlankSequence|BlankNullSequence)[type_]]] := TextElement[{type}, <|"GrammaticalUnit" -> ToString[h]|>];
FormatToken[a_Alternatives] := TextElement[WrapAlternatives[Map[FormatToken][a]], <|"GrammaticalUnit" -> "Alternatives"|>];
FormatToken[s_String] := TextElement[{s}, <|"GrammaticalUnit" -> "Text"|>];
FormatToken[s_Symbol] := s;
FormatToken[atom_?AtomQ] := atom;
FormatToken[h:Except[_Blank|_BlankSequence|_BlankNullSequence|_Words]] := With[{head = Head[h], arg = Check[Extract[1][h], $Failed, {Extract::partw, Extract::partd}]}, FormatToken[head, arg]];
FormatToken[TextType, s_String] := TextElement[s, <|"GrammaticalUnit" -> "TextType"|>];
FormatToken[t_, $Failed] := Confirm[$Failed, Message[FormatToken::nvld, t], "InvalidToken"]
FormatToken[h_, args__] := TextElement[Map[FormatToken][{args}], <|"GrammaticalUnit" -> ToString[h]|>];

FormatToken::nvld = "`1` is not supported in FormatToken"

SetAttributes[LexicalStructure, HoldAll]
LexicalStructure[expr_?LexicalPatternQ] := Enclose[FormatToken[expr], Identity, "InvalidToken"];
LexicalStructure[(Rule|RuleDelayed)[expr_?LexicalPatternQ,_]] := Enclose[Construct[FormatToken, StripNamedPattern[expr]], Identity, "FormatToken`InvalidToken"];

(* Service Utils *)
$LexicalCasesServices = {"Wikipedia"}

ArticleIndex = 0;

ArticleSearchIndicator[service_String, query_] := Row[{
	"Searching "<>service<>" for ", query, ProgressIndicator[Appearance->"Ellipsis"]
	}]


(* Patterns *)

Sandwich[bread_, expr_]:= bread~~expr~~bread
Sandwich[bread_][expr_] := Sandwich[bread, expr]

ExpandAlternativeTextTypes[alts_Alternatives] := (Apply[Alternatives]@*Map[TextType]@*Apply[List])[alts]

$StartTokenBoundary = (WordBoundary | " " | StartOfString | StartOfLine)
$EndTokenBoundary = (WordBoundary | " " | EndOfString | EndOfLine)

ApplyTokenBoundary[expr_] := $StartTokenBoundary~~expr~~$EndTokenBoundary

$WordAndContractionToken = ApplyTokenBoundary[(WordCharacter | "'")..]
$WordToken = ApplyTokenBoundary[WordCharacter..]

iExpand[expr_]:= ReplaceAll[expr, {
	OptionalToken[opt_Alternatives] :> (Map[iExpand /* ApplyTokenBoundary][opt]~Join~Alternatives[" ",""]),
	OptionalToken[opt_] :> (Alternatives[ApplyTokenBoundary[iExpand[opt]]]~Join~Alternatives[" ",""]),
	TextType[alts_Alternatives] :> ExpandAlternativeTextTypes[alts],
	BoundToken[s:Except[_Alternatives]] :> (ApplyTokenBoundary[iExpand[s]]),
	BoundToken[a_Alternatives] :> ApplyTokenBoundary[Map[iExpand, a]],
	WordToken[1] :> $WordToken,
	WordToken[n_Integer] :> StringExpression[$WordToken, Sequence@@ConstantArray[$WordToken, n-1]],
	WordToken[m_Integer, n_Integer] :> Alternatives@@Array[iExpand@*WordToken, n - 1, m],
	WordToken[1, "KeepContractions"] :> $WordAndContractionToken,
	WordToken[n_Integer, "KeepContractions"] :> StringExpression[$WordAndContractionToken, Sequence@@ConstantArray[$WordAndContractionToken, n-1]],
	WordToken[m_Integer, n_Integer, "KeepContractions"] :> Alternatives@@Array[iExpand@*(WordToken[#, "KeepContractions"]&), n - 1, m]
	}]

iExpandPattern[se_?LexicalPatternQ] := iExpand[se]

ExtractStringContentTypes[se_] := Splice[Cases[se, TextType[type_String] :> type, {0, Infinity}]];
ExtractAlternativeContentTypes[se_] := Splice[Cases[se, TextType[type_Alternatives] :> Splice[List@@type], {0, Infinity}]];
ExtractContainingContentTypes[se_] := Splice[Cases[se, TextType[type_Containing] :> type, {0, Infinity}]];

ExtractContentTypes[se_] := Through[{ExtractStringContentTypes, ExtractContainingContentTypes, ExtractAlternativeContentTypes}[se]]

ContentAssociation[st_String, (Rule|RuleDelayed)[se_,_]] := ContentAssociation[st, se]
ContentAssociation[sourcetext_String, se_] := Map[UnwrapAlternatives]@Merge[Identity]@KeyValueMap[<|#1 -> Alternatives@@DeleteDuplicates@#2|> &][TextCases[sourcetext, ExtractContentTypes[se]]]

ExpandPattern[sourcetext_String, se_?LexicalPatternQ] :=
	Module[{TRX, CA},
		TRX = iExpandPattern[se];
		CA  = ContentAssociation[sourcetext, se];
		Replace[TRX, TextType[type:(_String|_Containing)] :> ApplyTokenBoundary[UnwrapAlternatives[CA[type]]], {0, Infinity}]
		]

ExpandPattern[sourcetext_String, Rule[se_?LexicalPatternQ, expr_]] := Rule[ExpandPattern[sourcetext, se], expr]
ExpandPattern[sourcetext_String, RuleDelayed[se_?LexicalPatternQ, expr_]] := RuleDelayed[ExpandPattern[sourcetext, se], expr]

(* Naive solution for list inputs to ExpandPattern *)
ExpandPattern[sources:List[__String], se_?LexicalPatternQ] := Map[ExpandPattern[#, se]&][sources]

(* Wikipedia *)
InsertAnd[l:List[_]] := l;
InsertAnd[x_List] := Insert[x, "and", -2];

WikipediaKeywordString[{s_String}]:= "\""<>s<>"\"";
WikipediaKeywordString[l:{_String,_String}]:= StringRiffle[Map[WikipediaKeywordString][l], ", "];
WikipediaKeywordString[x_List] := StringRiffle[InsertAnd[Map[WikipediaKeywordString][x]], ", "];
WikipediaKeywordString[x_Alternatives] := ToString[Map[WikipediaKeywordString][x]]
WikipediaKeywordString[x_] := "\""<>x<>"\"";


WikipediaSearchQuery[List[],lp_] := $Failed
WikipediaSearchQuery[wsq:List[__String], _] := StringRiffle[wsq]
WikipediaSearchQuery[wsq:List[List[__String]], _] := Flatten[wsq]
WikipediaSearchQuery[wsq_List,_] := Cases[List[(_List|_String)..]][wsq] // Map[StringRiffle]

twsqErrorInfo[expr_TextType] := StringForm["TextType's do not produce keywords. A lexical pattern with one or more word/phrase strings in it should work", expr]
twsqErrorInfo[expr_] := StringForm["`` did not produce any keywords. A lexical pattern with one or more word/phrase strings in it should work", expr]

ToWikipediaSearchQuery[lp_?LexicalPatternQ] := Enclose[
	Confirm[
		iToWikipediaSearchQuery[lp],
		Message[ToWikipediaSearchQuery::novq, lp];twsqErrorInfo[lp]
		]
	]

iToWikipediaSearchQuery[Rule[lp_,_]] := iToWikipediaSearchQuery[StripNamedPattern[lp]]
iToWikipediaSearchQuery[RuleDelayed[lp_,_]] := iToWikipediaSearchQuery[StripNamedPattern[lp]]

iToWikipediaSearchQuery[lp:Except[_TextType|_WordToken]]:= Enclose[
	Module[
		{CLP, WSQ},
		CLP = Cases[List @@lp, (h : Except[TextType | WordToken])[args__] :> Cases[{args}, _String, {0, Infinity}]];
		WSQ = Confirm[WikipediaSearchQuery[CLP, lp]];
		WSQ // StringReplace[(" " ..) -> " "] // StringTrim
		]
	]

iToWikipediaSearchQuery[lp_] := $Failed

ToWikipediaSearchQuery::novq = "`` did not produce any keywords."

WikipediaArticlesFromRule["Content" -> a_Alternatives, opts:OptionsPattern[{LexicalCases}]]:= Module[
	{KWL = Apply[List][a], RULES},
	RULES = Thread["Content"-> KWL];
	Flatten@Map[r |-> WikipediaArticlesFromRule[r, MaxItems -> (Ceiling[OptionValue[MaxItems]/Length[KWL]]), FilterRules[{opts}, Except[MaxItems]]]][RULES]
]
WikipediaArticlesFromRule[rule:("Content" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule, Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,iSearchWikipedia]]]


iGetCategoryArticles[categories_List, n_Integer] := (Take[#, UpTo[n]]&)@*DeleteMissing@*DeleteDuplicates@*Flatten@ParallelMap[WikipediaData["Category" -> #, "CategoryArticles"]&, categories]

WikipediaArticlesFromRule[rule:("Category" -> _), opts:OptionsPattern[{LexicalCases}]]:= iGetCategoryArticles[
	WikipediaSearch[rule, MaxItems -> OptionValue[MaxCategories], Language -> OptionValue[Language]],
	OptionValue[MaxItems]
	]


iGetWikipediaArticles[query_Rule, opts___] := Module[
	{ART, ARC, MTL, SQR = WikipediaKeywordString[Values[query]], TXT},
	ART = Monitor[
		WikipediaArticlesFromRule[query, Sequence@@FilterRules[{opts}, OptionsJoin[WikipediaSearch,iSearchWikipedia]]],
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
	Overlaps -> False,
	MaxItems -> 50,
	MaxCategories -> 5,
	Language -> "English"
};

LexicalCases::unsupfmt="`` is not a supported format. Valid formats are: .txt, .md, .csv and .tsv";
LexicalCases[file_File, args___] := Enclose[
	ConfirmAssert[SupportedFileQ[file], Message[LexicalCases::unsupfmt, GetFileExtension[file]]];
	Module[{data = Import[file]}, LexicalCases[data, args]]
]

LexicalCases[input:List[__String], se_?LexicalPatternQ, opts:OptionsPattern[LexicalCases]] /; AllTrue[DirectoryQ \[Or] FileExistsQ][input] := Enclose[
	ConfirmAssert[CheckArguments[LexicalCases[input, se, opts], 2]];
		Module[
			{files = Map[File][input], LPC},
			LPC = iLexicalCases[files, se, opts];
			GenerateLexicalSummary[LPC, "File", se]
			]
		]

oSourceType[List[__File]] := "File"
oSourceType[List[__String]] := "Text"

LexicalCases[input:(List[__File]|List[__String]),se_?LexicalPatternQ, opts:OptionsPattern[LexicalCases]] := Enclose[
	ConfirmAssert[CheckArguments[LexicalCases[input, se, opts], 2]];
	Module[
		{LPC, OST},
		LPC = iLexicalCases[input, se, opts];
		OST = oSourceType[input];
		GenerateLexicalSummary[LPC, OST, se]
		]
	]

exprMsg[expr_String]:= "\""<> expr <> "\""
exprMsg[expr:Except[_String]]:= expr

LexicalCases::nofl = "No files found with query `1`"


LexicalCases::nvsi = "`` has no value. It should be a SearchIndexObject."
LexicalCases[Rule[x_Symbol?(Not@*ValueQ),_],___] := (Message[LexicalCases::nvsi, x];$Failed)

LexicalCases[input:Rule[index_SearchIndexObject, query_], se_?LexicalPatternQ, opts:OptionsPattern[LexicalCases]] := Enclose[
	ConfirmAssert[CheckArguments[LexicalCases[input, se, opts], 2]];
		Enclose[
			Module[
				{
					files = Map[File][TextSearch[index, query][All, "Location"]],
					LPC
					},
				ConfirmAssert[Not@*MatchQ[{}]@files];
				LPC = iLexicalCases[files, se, opts];
				GenerateLexicalSummary[LPC, "SearchIndex", se]
				],
				Failure[
					"NoFilesFound",
					<|
						"MessageTemplate" -> StringTemplate["No files found using `Query`."],
						"MessageParameters" -> <|"Query" -> exprMsg[query]|>,
						"Tag" -> #["Tag"],
						"ConfirmationType" -> #["ConfirmationType"],
						"HeldTest" -> #["HeldTest"],
						"Information" -> "Try another string or specify a query with SearchQueryString."
					|>
				
				]&
			]
		]

$stage = "";
lcStageMonitor[0] := ($stage = "")
lcStageMonitor[1] := ($stage = "Expanding Pattern")
lcStageMonitor[2] := ($stage = "Searching")

(* SourceText and LexicalPattern Input *)
LexicalCases[sourcetext_String, se_?LexicalPatternQ, opts:OptionsPattern[LexicalCases]]:= Enclose[
	ConfirmAssert[CheckArguments[LexicalCases[sourcetext, se, opts], 2]];
		Module[
			{LPC,RES},
			ArticleIndex=0;
			(* Find Matches *)
			LPC = Monitor[
				Confirm[LexicalCasesOnString[sourcetext, se, opts]],
				Row[{$stage, ProgressIndicator[Appearance->"Ellipsis"]}]
				];
			(* Generate Summary Object *)
			RES = Monitor[
				GenerateLexicalSummary[LPC, "Text", se],
				Row[{"Generating LexicalSummary", ProgressIndicator[Appearance->"Necklace"]}]
				];
			ArticleIndex=.;
			RES
			]
	]

(* SourceText is a string *)
LexicalCasesOnString[source_String, se_?LexicalPatternQ, opts:OptionsPattern[LexicalCases]]:=Enclose[
	Module[
		{RX, S = source, RES},
		++ArticleIndex;
		lcStageMonitor[1];
		RX = ConfirmQuiet[ExpandPattern[S, se], {Java::excptn, JavaNew::fail}];
		lcStageMonitor[2];
		RES = MatchTrim[OptionValue["StringTrim"]]@
			Query[
				GroupBy[#Match &] /* (KeyValueMap[<|"Match" -> #1, "Position" -> #2|> &]),
				KeyDrop["Match"] /* Values /* (Flatten[#, 1] &)
				]@
			Map[AssociationThread[{"Match", "Position"} -> #] &]@
			Transpose@{
				StringCases[S,RX, IgnoreCase -> OptionValue[IgnoreCase], Overlaps -> OptionValue[Overlaps]],
				StringPosition[S, StripNamedPattern[RX], IgnoreCase -> OptionValue[IgnoreCase], Overlaps -> OptionValue[Overlaps]]
			};
		lcStageMonitor[0];
		RES
		]
	]

(* LexicalPattern on Service *)
LexicalCases[se_?LexicalPatternQ, opts:OptionsPattern[{LexicalCases, iSearchWikipedia}]]:= Enclose[
	ConfirmAssert[CheckArguments[LexicalCases[se, opts], 1]];
	LexicalCasesFromService[OptionValue["Service"], se, FilterRules[{opts}, Options[iSearchWikipedia]]]
	]

(* WikiQueryRyle and LexicalPattern Input *)
LexicalCases[query_Rule, se_?LexicalPatternQ, opts:OptionsPattern[{LexicalCases, iSearchWikipedia}]]:= Enclose[
	ConfirmAssert[CheckArguments[LexicalCases[query, se, opts], 2]];
	LexicalCasesFromService[OptionValue["Service"], query, se, FilterRules[{opts}, Options[iSearchWikipedia]]]
	]

Options[LexicalCasesFromService]={

};

StringSearch[text_,pattern_] := Module[
	{RES = LexicalCasesOnString[text, pattern]},
		CriticalSection[lclock, MatchCount+=Length[Replace[_Missing -> {}]@Flatten[Lookup[RES,"Position"], 1]]];
		RES
		]

SearchArticles[texts_List, se_?LexicalPatternQ, articles_List, articleCount_Integer, maxTitleLength_Integer] := Module[
	{MAT},
		ArticleIndex=0;
		MatchCount=0;
		SetSharedVariable[se,MatchCount];
		SetSharedFunction[StringSearch];
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
	MaxCategories -> 5,
	Language -> "English"
};

iSearchWikipedia[se_?LexicalPatternQ, opts:OptionsPattern[{LexicalCases}]] := Enclose[
	With[
		{wikiquery = StringTrim@Confirm[ToWikipediaSearchQuery[se]]},
		iSearchWikipedia[wikiquery,se, opts]
		]
	]

iSearchWikipedia[query_?FailureQ,___] := Return[query, With]

iSearchWikipedia[query_Rule, se_?LexicalPatternQ, opts:OptionsPattern[{LexicalCases}]] := iLexicalCases[query, se, opts]
		
iSearchWikipedia[query:(_String|_List), se_?LexicalPatternQ, opts:OptionsPattern[{iSearchWikipedia, LexicalCases}]] := iLexicalCases["Content" -> query, se, opts]

LexicalCasesFromService["Wikipedia", se_?LexicalPatternQ, opts:OptionsPattern[]] := GenerateLexicalSummary[iSearchWikipedia[se, opts], "Wikipedia", se]
LexicalCasesFromService["Wikipedia", query:(_Rule|_String|_List), se_?LexicalPatternQ, opts:OptionsPattern[]]:= GenerateLexicalSummary[iSearchWikipedia[query, se, opts], "Wikipedia", se]

GetText[input:List[__String], opts:OptionsPattern[]] := Module[
	{LEN = Length[input], REN},
	REN = Range[LEN];
	<|"Text" -> input, "Articles" -> Map[ToString][REN], "ArticleCount" -> LEN, "ArticleCountString" -> ToString[LEN], "MaxTitleLength" -> (First@TakeLargestBy[REN -> "Value", IntegerDigits /* Length,1])|>
	]

GetText[input:List[__File], opts:OptionsPattern[]] := Module[
	{LEN = Length[input], REN = Map[FileNameTake][input]},
	<|"Text" -> Map[Import[#, "Text"]&][input], "Articles" -> REN, "ArticleCount" -> LEN, "ArticleCountString" -> ToString[LEN], "MaxTitleLength" -> (First@TakeLargestBy[REN -> "Value", IntegerDigits /* Length,1])|>
	]

GetText[input_Rule, opts:OptionsPattern[]] := iGetWikipediaArticles[input, opts]

iLexicalCases[input:(List[__String]|List[__File]|_Rule), se_?LexicalPatternQ, opts:OptionsPattern[{LexicalCases}]] := Module[
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

GetArticleCount[{}] := 0
GetArticleCount[data_] := (Lookup["Article"] /* DeleteDuplicates /* Length)[data]
ArticleWithMatchCount[_String, data_] := (data // DeleteMissing[#, 1, 1]& // GetArticleCount)

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
LexicalSummaryAscQ[asc_?AssociationQ] := AllTrue[{"Data", "Dataset", "Source", "TotalMatchCount", "LexicalStructure"}, KeyExistsQ[asc, #] &]
LexicalSummaryAscQ[_] = False;

(* Direct Properties *)
LexicalSummary[asc_?LexicalSummaryAscQ]["Data"] := asc["Data"]
LexicalSummary[asc_?LexicalSummaryAscQ]["Source"] := asc["Source"]
LexicalSummary[asc_?LexicalSummaryAscQ]["Dataset"] := asc["Dataset"]
LexicalSummary[asc_?LexicalSummaryAscQ]["TotalMatchCount"] := asc["TotalMatchCount"]
LexicalSummary[asc_?LexicalSummaryAscQ]["LexicalStructure"] := asc["LexicalStructure"]

(* Dataset Properties *)
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroupPercentages"] := PercentDataset[LexicalSummary[asc]["CountGroups"], asc["TotalMatchCount"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["LowercaseCountGroupPercentages"] := PercentDataset[LexicalSummary[asc]["CountGroups"] // CountSummaryLowercase, asc["TotalMatchCount"]]
(* Dataset Properties Filtered *)
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroupPercentages", n_Integer] := LexicalSummary[asc]["CountGroupPercentages"][;;UpTo[n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroupPercentages", DeleteStopwords] := FilterOutStopWordRows@LexicalSummary[asc]["CountGroupPercentages"]
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroupPercentages", n_Integer, DeleteStopwords] := FilterOutStopWordRows@LexicalSummary[asc]["CountGroupPercentages", n]

LexicalSummary[asc_?LexicalSummaryAscQ]["LowercaseCountGroupPercentages", n_Integer] := LexicalSummary[asc]["LowercaseCountGroupPercentages"][;;UpTo[n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["LowercaseCountGroupPercentages", DeleteStopwords] := FilterOutStopWordRows@LexicalSummary[asc]["LowercaseCountGroupPercentages"]
LexicalSummary[asc_?LexicalSummaryAscQ]["LowercaseCountGroupPercentages", n_Integer, DeleteStopwords] := FilterOutStopWordRows@LexicalSummary[asc]["LowercaseCountGroupPercentages", n]

(* Count Properties *)
LexicalSummary[asc_?LexicalSummaryAscQ]["Counts"] := GetDatasetCounts[LexicalSummary[asc]["Dataset"], asc["Source"]] // DeleteMissing[#, 1, 1] & // ReverseSortBy[#Count&]
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups"] := (LexicalSummary[asc]["Counts"] // CountGroups)
(* Count Properties Filtered *)
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups", n_Integer] := (LexicalSummary[asc]["CountGroups"][;;UpTo[n]])
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups", DeleteStopwords] := (LexicalSummary[asc]["Counts"] // CountGroups // FilterOutStopWordRows)
LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups", n_Integer, DeleteStopwords] := (LexicalSummary[asc]["CountGroups", DeleteStopwords][;;UpTo[n]])


LexicalSummary[asc_?LexicalSummaryAscQ]["Survey"] := GenerateDashboard[LexicalSummary[asc]]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", n_Integer] := GenerateDashboard[LexicalSummary[asc], n]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", DeleteStopwords] := GenerateDashboard[LexicalSummary[asc], DeleteStopwords]
LexicalSummary[asc_?LexicalSummaryAscQ]["Survey", n_Integer, DeleteStopwords] := GenerateDashboard[LexicalSummary[asc], n, DeleteStopwords]

LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups"] := PartOfSpeechGroups[LexicalSummary[asc]["CountGroups"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", n_Integer] := PartOfSpeechGroups[LexicalSummary[asc]["CountGroups"]][;;UpTo[n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", DeleteStopwords] := PartOfSpeechGroups[LexicalSummary[asc]["CountGroups", DeleteStopwords]]
LexicalSummary[asc_?LexicalSummaryAscQ]["PartOfSpeechGroups", n_Integer,  DeleteStopwords] := PartOfSpeechGroups[LexicalSummary[asc]["CountGroups", DeleteStopwords]][;;UpTo[n]]

LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups"] := WordStemGroups[LexicalSummary[asc]["CountGroups"]]
LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups", n_Integer] := WordStemGroups[LexicalSummary[asc]["CountGroups"]][;;UpTo[n]]
LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups", DeleteStopwords] := WordStemGroups[LexicalSummary[asc]["CountGroups", DeleteStopwords]]
LexicalSummary[asc_?LexicalSummaryAscQ]["WordStemCountGroups", n_Integer,  DeleteStopwords] := WordStemGroups[LexicalSummary[asc]["CountGroups", DeleteStopwords]][;;UpTo[n]]

LexicalSummary[asc_?LexicalSummaryAscQ][invalidkey_] := asc[invalidkey]
LexicalSummary[asc_?LexicalSummaryAscQ]["Properties"] := {"Data","Dataset","Counts","CountGroups","CountGroupPercentages", "LowercaseCountGroupPercentages","PartOfSpeechGroups", "WordStemCountGroups", "Source","TotalMatchCount","LexicalStructure", "Survey"}

GenerateLexicalSummary[data_?FailureQ, ___] := data
GenerateLexicalSummary[data_, sourceType_String, se_?LexicalPatternQ] := Enclose[
	Monitor[
		iGenerateLexicalSummary[data, sourceType, se],
		Row[{"Generating Summary", ProgressIndicator[Appearance->"Ellipsis"]}]
	],
	Identity,
	"LexicalSummaryFailed"
	]

iGenerateLexicalSummary[data_, sourceType_String, se_?LexicalPatternQ] := Module[
	{MTC, DS = Dataset[data], cse = StripNamedPattern@se, STC},
	STC = LexicalStructure[cse];
	MTC = DeleteMissing[GetDatasetCounts[DS, sourceType], 1, 1][Total, "Count"];
	Confirm[LexicalSummary[<|"Data" -> data, "Dataset" -> DS, "Source" -> sourceType, "TotalMatchCount" -> MTC, "LexicalStructure" -> STC |>], Null, "LexicalSummaryFailed"]
]

(* Summary Utils *)
GetDatasetCounts[ds_Dataset,"Text"] := ds[All, <|"Match" -> "Match", "Count" -> "Position" /* Length|>]
GetDatasetCounts[ds_Dataset, "Wikipedia"|"SearchIndex"] := ds[GroupBy["Match"], Map[KeyDrop[{"Article", "Match"}]]][All, Total@*Map[Length], "Position"] // KeyValueMap[<|"Match" -> #1, "Count" -> #2|> &]

CountGroups[ds_Dataset] := Query[
		GroupBy[#Count&] /* KeyValueMap[<|"Matches" -> #2, "CountGroup" -> #1|> &],
		KeyDrop["Count"] /* Values /* (Flatten[#,1]&)
	][ds]

CountGroupDSQ[ds_Dataset] /; Normal /* Length /* GreaterThan[0]@ds := Normal /* First /* KeyMemberQ["CountGroup"]@ds
CountDSQ[ds_Dataset] /; Normal /* Length /* GreaterThan[0]@ds := Normal /* First /* KeyMemberQ["Count"]@ds

CountGroupDSQ[_] := False
CountDSQ[_] := False

CountSummaryLowercase[ds_Dataset] /; CountDSQ[ds] := ReverseSortBy[#Count&]@Query[
	GroupBy[ToLowerCase[#Match] &] /* KeyValueMap[<|"Match" -> #1, #2|> &],
	Total /* KeyDrop["Match"]
	][ds]

CountSummaryLowercase[ds_Dataset] := ds

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

stopwords = Monitor[
	Alternatives@@WordList["Stopwords"],
	Row[{"Getting stopwords",ProgressIndicator[Appearance->"Ellipsis"]}]
]


StopWordQ[s_String] := StringMatchQ[stopwords][ToLowerCase[s]]


FilterOutStopWordRows[ds_Dataset] := ds[(DeleteCases[#, _String?StopWordQ, 3] &) /* Select[Not@SameQ[{}, #Matches] &]]

PartOfSpeechKey[word_String] := ProcessWordData[WordData[word, "PartsOfSpeech"]]
ProcessWordData[w_WordData] := None
ProcessWordData[w_] := w

(* TODO: module-ize these functions for clarity *)
PartOfSpeechGroups[ds_Dataset] := (ds[TextWords /* ToLowerCase /* Flatten /* DeleteStopwords /* DeleteDuplicates, "Matches"][AlphabeticSort][GroupBy[PartOfSpeechKey]][KeyDrop[None]] // KeySort) // KeyValueMap[<|"Words" -> #2, "PartOfSpeech" -> #1|>&]

GetWordStemCounts[ds_Dataset] := (ds[All, "Matches"] // Normal // Flatten // (StringSplit[#, WordBoundary]&) /* Flatten /* DeleteCases[" "|"\n"] // DeleteStopwords // ToLowerCase // WordStem // Counts)

WordStemGroups[ds_Dataset] := (GetWordStemCounts[ds] // KeyValueMap[<|"Stem" -> #1, "Count" -> #2|> &] // Dataset // ReverseSortBy[#Count &]) // Query[GroupBy[#Count &], KeyDrop["Count"] /* Values /* (Flatten[#, 1]&)] // KeyValueMap[<|"WordStem" -> #2, "CountGroup" -> #1|> &]

PercentDataset[ds_Dataset, matchcount_Integer] := (ds[
	All,
	<|"Matches" -> #Matches, "Percentage" -> Quantity[100. N[((#CountGroup Length[#Matches])/matchcount)], "Percent"]|> &][ReverseSortBy["Percentage"]])

Options[LexicalDispersionPlot] = {
	AspectRatio -> 1/5,
	ImageSize -> Large,
	PlotRange -> All,
	PlotTheme -> "Scientific",
	PlotLabel -> "Lexical Dispersion Plot"
};
LexicalDispersionPlot[text_String, word_String, opts:OptionsPattern[{LexicalDispersionPlot}]] := LexicalDispersionPlot[text, {word}, opts]
LexicalDispersionPlot[text_String, words:{__String}, opts:OptionsPattern[{LexicalDispersionPlot}]] := Module[
	{
		tokens = Monitor[
			(* TextWords is slow for what I want it to do, using StringCases instead *)
			(*TextWords[text]*)
			StringCases[text, RegularExpression["\\b\\w+\\b"]],
			Row[{"Tokenizing text",ProgressIndicator[Appearance->"Ellipsis"]}]
			], textevents, events,
		rowIndex = AssociationThread[words -> Range[Length[words]]],
		sparr, ticks
		},
		(* 1 \[LongDash] Generate discrete indices for the tokenized text *)
		textevents = Monitor[PositionIndex[tokens], Row[{"Indexing tokens", ProgressIndicator[Appearance->"Ellipsis"]}]];
		events = Monitor[
			KeyTake[words][textevents] // KeyValueMap[Thread[Thread[{rowIndex[#1], #2}] -> 1] &] /* Flatten,
			Row[{"Generating sparse elements", ProgressIndicator[Appearance->"Ellipsis"]}]
			];
		sparr = SparseArray[events, {Length[words], Length[tokens]}];
		
		ticks = KeyValueMap[{#2, #1}&, rowIndex];
		MatrixPlot[
			sparr,
			FrameTicks -> {{ticks, None}, {Automatic, None}},
			AspectRatio -> OptionValue[AspectRatio],
			ImageSize -> OptionValue[ImageSize],
			PlotRange -> OptionValue[PlotRange],
			PlotTheme -> OptionValue[PlotTheme],
			PlotLabel -> OptionValue[PlotLabel]
			]
		]

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
