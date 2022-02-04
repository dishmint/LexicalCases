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

(* Patterns *)

TextType::usage = "TextType[type] a symbolic wrapper for TextContentTypes"
OptionalToken::usage = "OptionalToken[se] matches se, \" \", or \"\""
BoundToken::usage = "BoundToken[expr] sandwiches expr with boundaries\nBounded[s1|\[Ellipsis]|si] sandwiches the set of si with boundaries"
WordToken::usage = "WordToken[n] represents n words separated by spaces\nWordToken[m,n] represents m to n words separated by spaces"
Sandwich::usage = "Sandwich[outer, inner] sandwiches inner between outer"

ExpandPattern::usage = "ExpandPattern[patt] expands patt into a valid StringExpression"

(* Format *)
LexicalStructure::usage="LexicalStructure[se] Visualize the structure of the StringExpression"

(* Services *)
$LexicalCasesServices::usage = "List of supported services"

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

$ValidLexicalTokens = (_TextType|_Opt|_Bounded|_Words)
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
	Check[StringPattern`StringPatternQ[se] \[Or] AllTrue[ValidateLexicalToken, lt], $Failed]
		];
LexicalPatternQ[Rule[expr_?LexicalPatternQ,_]]:= True;
LexicalPatternQ[RuleDelayed[expr_?LexicalPatternQ,_]]:= True;
LexicalPatternQ[expr_?StringPattern`StringPatternQ]:= True;


ContainsPatternHeadsQ[se_?LexicalPatternQ] := ContainsAny[extractHeads[se], {Pattern}]

StripNamedPattern[se_?LexicalPatternQ] := StripNames[ContainsPatternHeadsQ[se], se]
StripNames[True, se_?LexicalPatternQ] := Replace[se, p_Pattern :> Extract[2][p], Infinity]
StripNames[True, HoldPattern[(Rule|RuleDelayed)[se_?LexicalPatternQ,_]]] := Replace[se, p_Pattern :> Extract[2][p], Infinity]
StripNames[False,se_?LexicalPatternQ]:= se

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
FormatToken[StringExpression[args___]] := FormatToken[StringExpression, args];
FormatToken[TextType[type_String]] := TextElement[{type}, <|"GrammaticalUnit" -> "TextType"|>];
FormatToken[TextType[types_Alternatives]] := TextElement[PostProcessAlternatives[Map[FormatToken][ExpandAlternativeTextTypes[types]]], <|"GrammaticalUnit" -> "Alternatives"|>];
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
FormatToken[a_Alternatives] := TextElement[PostProcessAlternatives[Map[FormatToken][a]], <|"GrammaticalUnit" -> "Alternatives"|>];
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

ExtractContentTypes[se_] := Through[{ExtractStringContentTypes, ExtractAlternativeContentTypes}[se]]

ContentAssociation[st_String, (Rule|RuleDelayed)[se_,_]] := ContentAssociation[st, se]
ContentAssociation[sourcetext_String, se_] := Map[ExtractAlternatives]@Merge[Identity]@KeyValueMap[<|#1 -> Alternatives@@DeleteDuplicates@#2|> &][TextCases[sourcetext, ExtractContentTypes[se]]]


ContentAlts[List[a_Alternatives]] := a
ContentAlts[a_Alternatives] := a

ExpandPattern[sourcetext_String, se_?LexicalPatternQ] :=
	Module[{TRX, CA},
		TRX = iExpandPattern[se];
		CA  = ContentAssociation[sourcetext, se];
		Replace[TRX, TextType[type_String] :> ApplyTokenBoundary[ContentAlts[CA[type]]], Infinity]
		]

ExpandPattern[sourcetext_String, Rule[se_?LexicalPatternQ, expr_]] := Rule[ExpandPattern[sourcetext, se], expr]
ExpandPattern[sourcetext_String, RuleDelayed[se_?LexicalPatternQ, expr_]] := RuleDelayed[ExpandPattern[sourcetext, se], expr]

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
	stage1 = ReplaceAll[cleanLexicalPattern, {BoundToken[s_] :> s, Alternatives -> List}] // DeleteCases[" "];
	Check[WikipediaSearchQuery[stage1, se] // StringReplace[(" " ..) -> " "] // StringTrim, Return[$Failed, Module]]
	]

ToWikipediaSearchQuery[Rule[se_StringExpression,_]] := ToWikipediaSearchQuery[StripNamedPattern[se]]
ToWikipediaSearchQuery[RuleDelayed[se_StringExpression,_]] := ToWikipediaSearchQuery[StripNamedPattern[se]]

ToWikipediaSearchQuery::novq = "Keyword formulation not supported for the StringExpression ``. Consider using the \"Content\" option to supply keywords, or trying a different StringExpression."

WikipediaArticlesFromRule["Content" -> a_Alternatives, opts:OptionsPattern[{LexicalCases}]]:= Module[
	{KWL = Apply[List][a], RULES},
	RULES = Thread["Content"-> KWL];
	Flatten@Map[r |-> WikipediaArticlesFromRule[r, MaxItems -> (Ceiling[OptionValue[MaxItems]/Length[KWL]]), FilterRules[opts, Except[MaxItems]]]][RULES]
]
WikipediaArticlesFromRule[rule:("Content" -> _), opts:OptionsPattern[]]:= WikipediaSearch[rule, Sequence@@FilterRules[{opts}, optionsJoin[WikipediaSearch,iSearchWikipedia]]]


iGetCategoryArticles[categories_List, n_Integer] := (Take[#, UpTo[n]]&)@*DeleteMissing@*DeleteDuplicates@*Flatten@ParallelMap[WikipediaData["Category" -> #, "CategoryArticles"]&, categories]

WikipediaArticlesFromRule[rule:("Category" -> _), opts:OptionsPattern[{LexicalCases}]]:= iGetCategoryArticles[
	WikipediaSearch[rule, MaxItems -> OptionValue[MaxCategories], Language -> OptionValue[Language]],
	OptionValue[MaxItems]
	]


iGetWikipediaArticles[query_Rule, opts___] := Module[
	{ART, ARC, MTL, SQR = WikipediaKeywordString[Values[query]], TXT},
	ART = Monitor[
		WikipediaArticlesFromRule[query, Sequence@@FilterRules[{opts}, optionsJoin[WikipediaSearch,iSearchWikipedia]]],
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

LexicalCases::unsupobj=Import::unsupobj;
GetFileExtension[file_File] := Information[file, "FileExtension"]
SupportedFileQ[file_File] := MemberQ[{"txt", "md", "csv", "tsv"}, GetFileExtension[file]]
LexicalCases[file_File, args___] := Enclose[
	ConfirmAssert[SupportedFileQ[file], Message[LexicalCases::unsupobj, GetFileExtension[file]]];
	Module[{data = Import[file]}, LexicalCases[data, args]]
]

LexicalCases[input:List[__String],se_?LexicalPatternQ, opts:OptionsPattern[LexicalCases]] /; AllTrue[DirectoryQ \[Or] FileExistsQ][input] := Enclose[
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
		{LPC = iLexicalCases[input, se, opts], sourcetype = oSourceType[input]},
		GenerateLexicalSummary[LPC, sourcetype, se]
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
		{RX, S = EscapePunctuation[source], RES},
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
				StringCases[source,RX, IgnoreCase -> OptionValue[IgnoreCase], Overlaps -> OptionValue[Overlaps]],
				StringPosition[source, StripNamedPattern[RX], IgnoreCase -> OptionValue[IgnoreCase], Overlaps -> OptionValue[Overlaps]]
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

iSearchWikipedia[se_?LexicalPatternQ, opts:OptionsPattern[{LexicalCases}]] := With[
	{wikiquery = StringTrim@ToWikipediaSearchQuery[se]},
	iSearchWikipedia[wikiquery,se, opts]
	]

iSearchWikipedia[query_?FailureQ,___] := Return[query, With]

iSearchWikipedia[query_Rule, se_?LexicalPatternQ, opts:OptionsPattern[{LexicalCases}]] := iLexicalCases[query, se, opts]
		
iSearchWikipedia[query:(_String|_List), se_?LexicalPatternQ, opts:OptionsPattern[{iSearchWikipedia, LexicalCases}]] := iLexicalCases["Content" -> query, se, opts]

LexicalCasesFromService["Wikipedia", se_?LexicalPatternQ, opts:OptionsPattern[]] := GenerateLexicalSummary[iSearchWikipedia[se, opts], "Wikipedia", se]
LexicalCasesFromService["Wikipedia", query:(_Rule|_String|_List), se_?LexicalPatternQ, opts:OptionsPattern[]]:= GenerateLexicalSummary[iSearchWikipedia[query, se, opts], "Wikipedia", se]

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

iLexicalCases[input:(List[__String]|List[__File]|_Rule), se_?LexicalPatternQ, opts:OptionsPattern[]] := Module[
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
	All,
	<|"Matches" -> #Matches, "Percentage" -> Quantity[100. N[((#CountGroup Length[#Matches])/matchcount)], "Percent"]|> &][ReverseSortBy["Percentage"]])

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
