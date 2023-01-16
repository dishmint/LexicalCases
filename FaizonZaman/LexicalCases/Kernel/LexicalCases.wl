(* ::Package:: *)

(* ::Title:: *)

(*LexicalCases*)

(* ::Abstract:: *)

(*Extract and analyze text type sequences with the Wolfram Language.*)

BeginPackage["FaizonZaman`LexicalCases`"]

(* Main *)

LexicalCases::usage = "LexicalCases[source, lp] extract cases of LexicalPattern lp from Text source."

LexicalPatternQ::usage = "LexicalPatternQ[expr] returns True if expr is a valid lexical pattern."

ToLexicalPattern::usage = "ToLexicalPattern[s] converts string s to a lexical pattern."

(* Samples *)

$SampleSentence::usage = "A short example string."

$SampleParagraph::usage = "A long example string."

$SampleStringExpression::usage = "A sample pattern used for testing."

(* Summary *)

LexicalSummary::usage = "A summary of LexicalCases results. Run LexicalSummary[<>][\"Properties\"] for a list or properties"

CountSummaryLowercase::usage = "CountSummaryLowercase[ds] lowercase matches in ds and merge rows with the same match."

StopWordQ::usage = "StopWordQ[s] returns True if s is a stop word."

$FilterableProperties::usage = "List of filterable summary properties"
(* Patterns *)

TextType::usage = "TextType[type] a symbolic wrapper for TextContentTypes"

OptionalToken::usage = "OptionalToken[lp] matches the lexical pattern lp, whitespace \" \", or an empty string \"\"."

BoundToken::usage = "BoundToken[lp] represents a bounded form of the lexical pattern lp\nBounded[lp1|\[Ellipsis]|lpi] represents a bounded form of the alternatives lpi."

WordToken::usage = "WordToken[n] represents n words separated by spaces\nWordToken[m,n] represents m to n words separated by spaces."

Sandwich::usage = "Sandwich[outer, inner] sandwiches inner between outer."

ExpandPattern::usage = "ExpandPattern[source, lp] expands lexical pattern lp into a valid StringExpression with content from source."

LexicalPattern::usage = "LexicalPattern[patt] A wrapper for using lexical patterns in string functions"

(* Format *)

LexicalStructure::usage = "LexicalStructure[lp] Visualize lexical pattern structure"

(* Services *)

$LexicalCasesServices::usage = "List of supported services"

(* Analysis / Visualization *)

LexicalDispersionPlot::usage = "LexicalDispersionPlot[text, w] plots the dispersion of word w across text\nLexicalDispersionPlot[text, {w1, \[Ellipsis], wi}] plots the dispersion of the wi across text"
HideMissing::usage = "HideMissing is an option to LexicalDispersionPlot for hiding keywords from the plot if they were not found in the source text"
DataJoin::usage = "DataJoin is an option to LexicalDispersionPlot specifying whether to combine results when given multiple source texts"
DispersionPlotFunction::usage = "DispersionPlotFunction is an option to LexicalDispersionPlot specifying which kind of plot to use. Evaluate $DispersionPlotFunctions for a list of supported values"
$DispersionPlotFunctions::usage = "$DispersionPlotFunctions returns a list of supported option values for DispersionPlotFunction"

(* Options *)

MaxCategories::usage = "MaxCategories is an option to LexicalCases restricting the number of Wikipedia categories to get articles from"

Begin["`Private`"]

Needs["FaizonZaman`LexicalCases`Samples`"]

Needs["FaizonZaman`LexicalCases`Utilities`"]

Needs["FaizonZaman`LexicalCases`LexicalPattern`"]

Needs["FaizonZaman`LexicalCases`Validation`"]

Needs["FaizonZaman`LexicalCases`Plots`"]

ContainsPatternHeadsQ[se_?LexicalPatternQ] := ContainsAny[ExtractHeads[se], {Pattern}]

StripNamedPattern[se_?LexicalPatternQ] := StripNames[ContainsPatternHeadsQ[se], se]
StripNames[True, HoldPattern[(Rule|RuleDelayed)[se_?LexicalPatternQ,_]]] := Replace[se, p_Pattern :> Extract[2][p], Infinity]
StripNames[True, c_Condition] := c
StripNames[True, se_?LexicalPatternQ] := Replace[se, p_Pattern :> Extract[2][p], Infinity]
StripNames[False,se_?LexicalPatternQ]:= se

PrependArticleKey[{article_String, data_List}] := Map[Apply[Prepend]]@Thread[{data, "Article" -> article}]
PrependArticleKey[{article_String, data_Missing}] := <|"Article" -> article, "Match" -> data, "Position" -> {}|>

PrependArticleKey[{article_Integer, data_List}] := Map[Apply[Prepend]]@Thread[{data, "Article" -> article}]
PrependArticleKey[{article_Integer, data_Missing}] := <|"Article" -> article, "Match" -> data, "Position" -> {}|>

(* Text Cleanup *)

EscapePunctuation[s_String] := StringReplace[s, pc:PunctuationCharacter :> "\\" <> pc]

articlePluralize::noa = "No articles found"
articlePluralize[0] := Message[articlePluralize::noa]
articlePluralize[1] := "article"
articlePluralize[_Integer?Positive] := "articles"

(* Format *)

FormatToken[StringExpression[args___]] :=
	FormatToken[StringExpression, args];

FormatToken[TextType[type_String]] :=
	TextElement[{type}, <|"GrammaticalUnit" -> "TextType"|>];

FormatToken[TextType[Containing[outer_, inner_]]] :=
	TextElement[{inner}, <|"GrammaticalUnit" -> outer|>];

FormatToken[TextType[types_Alternatives]] :=
	TextElement[WrapAlternatives[Map[FormatToken][ExpandAlternativeTextTypes[
		types]]], <|"GrammaticalUnit" -> "Alternatives"|>];

FormatToken[OptionalToken[args__]] :=
	TextElement[Map[FormatToken][{args}], <|"GrammaticalUnit" -> "Optional"
		|>];

FormatToken[AnyOrder[args__]] :=
	TextElement[Map[FormatToken][{args}], <|"GrammaticalUnit" -> "AnyOrder"
		|>]

FormatToken[FixedOrder[args__]] :=
	TextElement[Map[FormatToken][{args}], <|"GrammaticalUnit" -> "FixedOrder"
		|>]

FormatToken[HoldPattern[WordToken[1]]] :=
	TextElement[{1}, <|"GrammaticalUnit" -> "Word"|>];

FormatToken[HoldPattern[WordToken[n_Integer]]] :=
	TextElement[{n}, <|"GrammaticalUnit" -> "Words"|>];

FormatToken[WordToken[args__Integer]] :=
	TextElement[{Span[args]}, <|"GrammaticalUnit" -> "Words"|>];

FormatToken[HoldPattern[_]] :=
	TextElement[{"_"}, <|"GrammaticalUnit" -> "Blank"|>];

FormatToken[HoldPattern[__]] :=
	TextElement[{"__"}, <|"GrammaticalUnit" -> "BlankSequence"|>];

FormatToken[HoldPattern[___]] :=
	TextElement[{"___"}, <|"GrammaticalUnit" -> "BlankNullSequence"|>];

FormatToken[HoldPattern[h : (Blank | BlankSequence | BlankNullSequence
	)[type_]]] :=
	TextElement[{type}, <|"GrammaticalUnit" -> ToString[h]|>];

FormatToken[a_Alternatives] :=
	TextElement[WrapAlternatives[Map[FormatToken][a]], <|"GrammaticalUnit"-> "Alternatives"|>];

FormatToken[s_String] :=
	TextElement[{s}, <|"GrammaticalUnit" -> "Text"|>];

FormatToken[s_Symbol] :=
	s;

FormatToken[atom_?AtomQ] :=
	atom;

FormatToken[h : Except[_Blank | _BlankSequence | _BlankNullSequence |
	 _Words]] :=
	With[{head = Head[h], arg = Check[Extract[1][h], $Failed, {Extract::partw,
		 Extract::partd}]},
		FormatToken[head, arg]
	];

FormatToken[TextType, s_String] :=
	TextElement[s, <|"GrammaticalUnit" -> "TextType"|>];

FormatToken[t_, $Failed] :=
	Enclose[Confirm[$Failed, Message[FormatToken::nvld, t], "InvalidToken"]]

FormatToken[h_, args__] :=
	TextElement[Map[FormatToken][{args}], <|"GrammaticalUnit" -> ToString[
		h]|>];

FormatToken::nvld = "`1` is not supported in FormatToken"

SetAttributes[LexicalStructure, HoldAll]

LexicalStructure[expr_?LexicalPatternQ] :=
	Enclose[FormatToken[expr], Identity, "InvalidToken"];

LexicalStructure[(Rule | RuleDelayed)[expr_?LexicalPatternQ, _]] :=
	Enclose[Construct[FormatToken, StripNamedPattern[expr]], Identity, "FormatToken`InvalidToken"
		];

(* Service Utils *)

$LexicalCasesServices = {"Wikipedia"}

ArticleIndex = 0;

ArticleSearchIndicator[service_String, query_] :=
	Row[{"Searching " <> service <> " for ", query, ProgressIndicator[Appearance -> "Ellipsis"]}]

(* Patterns *)

Sandwich[bread_, expr_] :=
	bread ~~ expr ~~ bread

Sandwich[bread_][expr_] :=
	Sandwich[bread, expr]

ExpandAlternativeTextTypes[alts_Alternatives] := (Apply[Alternatives] @* Map[TextType] @* Apply[List])[alts]

$StartTokenBoundary = (WordBoundary | " " | StartOfString | StartOfLine)
$EndTokenBoundary = (WordBoundary | " " | EndOfString | EndOfLine)

ApplyTokenBoundary[expr_] := $StartTokenBoundary~~expr~~$EndTokenBoundary

$WordAndContractionToken = ApplyTokenBoundary[(WordCharacter | "'")..]
$WordToken = ApplyTokenBoundary[WordCharacter..]

iExpand[expr_] :=
	ReplaceAll[expr,
	{
		OptionalToken[opt_Alternatives] :> (Map[iExpand /* ApplyTokenBoundary][opt] ~ Join ~ Alternatives[" ", ""]),

		OptionalToken[opt_] :> (Alternatives[ApplyTokenBoundary[iExpand[opt]]] ~ Join ~ Alternatives[" ", ""]),
		
		TextType[alts_Alternatives] :> ExpandAlternativeTextTypes[alts],
		
		BoundToken[s : Except[_Alternatives]] :> (ApplyTokenBoundary[iExpand[s]]),
		
		BoundToken[a_Alternatives] :> ApplyTokenBoundary[Map[iExpand, a]],
		
		WordToken[1] :> $WordToken,
		
		WordToken[n_Integer] :> StringExpression[$WordToken, Sequence @@ ConstantArray[$WordToken, n - 1]],
		
		WordToken[m_Integer, n_Integer] :> Alternatives @@ Array[iExpand @* WordToken, n - 1, m],
		
		WordToken[1, "KeepContractions"] :> $WordAndContractionToken,
		
		WordToken[n_Integer, "KeepContractions"] :> StringExpression[$WordAndContractionToken, Sequence @@ ConstantArray[$WordAndContractionToken, n - 1]],
		
		WordToken[m_Integer, n_Integer, "KeepContractions"] :> Alternatives @@ Array[iExpand@* (WordToken[#, "KeepContractions"]&), n - 1, m]
		}
	]

iExpandPattern[se_?LexicalPatternQ] :=
	iExpand[se]

ExtractStringContentTypes[se_] :=
	Splice[Cases[se, TextType[type_String] :> type, {0, Infinity}]];

ExtractAlternativeContentTypes[se_] :=
	Splice[Cases[se, TextType[type_Alternatives] :> Splice[List @@ type],
		 {0, Infinity}]];

ExtractContainingContentTypes[se_] :=
	Splice[Cases[se, TextType[type_Containing] :> type, {0, Infinity}]];

ExtractContentTypes[se_] :=
	Through[{ExtractStringContentTypes, ExtractContainingContentTypes, ExtractAlternativeContentTypes}[se]]

ContentAssociation[st_String, (Rule | RuleDelayed)[se_, _]] :=
	ContentAssociation[st, se]

ContentAssociation[sourcetext_String, se_] :=
	Block[
		{typecontent = TextCases[sourcetext, ExtractContentTypes[se]], valuealts, mergeonkey},
		valuealts = KeyValueMap[<|#1 -> Alternatives@@ DeleteDuplicates @ #2|>&][typecontent];
		mergeonkey = Merge[Identity] @ valuealts;
		UnwrapAlternatives /@ mergeonkey
		]

ExpandPattern[sourcetext_String, se_?LexicalPatternQ] :=
	Module[{trx, ca},
		trx = iExpandPattern[se];
		ca = ContentAssociation[sourcetext, se];
		Replace[trx, TextType[type : (_String | _Containing)] :> ApplyTokenBoundary[UnwrapAlternatives[ca[type]]], {0, Infinity}]
	]

ExpandPattern[sourcetext_String, Rule[se_?LexicalPatternQ, expr_]] :=
	Rule[ExpandPattern[sourcetext, se], expr]

ExpandPattern[sourcetext_String, RuleDelayed[se_?LexicalPatternQ, expr_]] :=
	RuleDelayed[ExpandPattern[sourcetext, se], expr]

(* Naive solution for list inputs to ExpandPattern *)

ExpandPattern[sources : List[__String], se_?LexicalPatternQ] :=
	Map[ExpandPattern[#, se]&][sources]

(* Wikipedia *)

InsertAnd[l : List[_]] := l;

InsertAnd[x_List] := Insert[x, "and", -2];

WikipediaKeywordString[{s_String}] := "\"" <> s <> "\"";

WikipediaKeywordString[l : {_String, _String}] :=
	StringRiffle[Map[WikipediaKeywordString][l], ", "];

WikipediaKeywordString[x_List] :=
	StringRiffle[InsertAnd[Map[WikipediaKeywordString][x]], ", "];

WikipediaKeywordString[x_Alternatives] :=
	ToString[Map[WikipediaKeywordString][x]]

WikipediaKeywordString[x_] :=
	"\"" <> x <> "\"";

WikipediaSearchQuery[List[], lp_] :=
	$Failed

WikipediaSearchQuery[wsq : List[__String], _] :=
	StringRiffle[wsq]

WikipediaSearchQuery[wsq : List[List[__String]], _] :=
	Flatten[wsq]

WikipediaSearchQuery[wsq_List, _] :=
	Cases[List[(_List | _String)..]][wsq] // Map[StringRiffle]

twsqErrorInfo[expr_TextType] :=
	StringForm["TextType's do not produce keywords. A lexical pattern with one or more word/phrase strings in it should work",
		 expr]

twsqErrorInfo[expr_] :=
	StringForm["`` did not produce any keywords. A lexical pattern with one or more word/phrase strings in it should work",
		 expr]

ToWikipediaSearchQuery[lp_?LexicalPatternQ] :=
	Enclose[
		Confirm[
			iToWikipediaSearchQuery[lp]
			,
			Message[ToWikipediaSearchQuery::novq, lp];
			twsqErrorInfo[lp]
		]
	]

iToWikipediaSearchQuery[Rule[lp_, _]] :=
	iToWikipediaSearchQuery[StripNamedPattern[lp]]

iToWikipediaSearchQuery[RuleDelayed[lp_, _]] :=
	iToWikipediaSearchQuery[StripNamedPattern[lp]]

iToWikipediaSearchQuery[lp : Except[_TextType | _WordToken]] :=
	Enclose[
		Module[{clp, wsq},
			clp = Cases[List @@ lp, (h : Except[TextType | WordToken])[args__] :> Cases[{args}, _String, {0, Infinity}]];
			wsq = Confirm[WikipediaSearchQuery[clp, lp]];
			
			wsq // StringReplace[(" "..) -> " "] // StringTrim
		]
	]

iToWikipediaSearchQuery[lp_] := $Failed

ToWikipediaSearchQuery::novq = "`` did not produce any keywords."

WikipediaArticlesFromRule["Content" -> a_Alternatives, opts : OptionsPattern[{LexicalCases}]] :=
	Module[
		{kwl = Apply[List][a], rules, articlesfromrule},
		rules = Thread["Content" -> kwl];
		articlesfromrule = Map[rule |-> WikipediaArticlesFromRule[rule, MaxItems -> (Ceiling[OptionValue[MaxItems] / Length[kwl]]), FilterRules[{opts}, Except[MaxItems]]]][rules];
		Flatten @ articlesfromrule
	]

WikipediaArticlesFromRule[rule : ("Content" -> _), opts : OptionsPattern[
	]] :=
	WikipediaSearch[rule, Sequence @@ FilterRules[{opts}, OptionsJoin[WikipediaSearch,
		 iSearchWikipedia]]]

iGetCategoryArticles[categories_List, n_Integer] :=
	(Take[#, UpTo[n]]&) @* DeleteMissing @* DeleteDuplicates @* Flatten @
		 ParallelMap[WikipediaData["Category" -> #, "CategoryArticles"]&, categories
		]

WikipediaArticlesFromRule[rule : ("Category" -> _), opts : OptionsPattern[{LexicalCases}]] :=
	iGetCategoryArticles[WikipediaSearch[rule, MaxItems -> OptionValue[MaxCategories], Language -> OptionValue[Language]], OptionValue[MaxItems]]

iGetWikipediaArticles[query_Rule, opts___] :=
	Module[
		{art, arc, mtl, asc, sqr = WikipediaKeywordString[Values[query]], txt},
		art =
			Monitor[
				WikipediaArticlesFromRule[query, Sequence @@ FilterRules[{opts}, OptionsJoin[WikipediaSearch, iSearchWikipedia]]],
				ArticleSearchIndicator["Wikipedia", sqr]
			];
		arc = Length[art];
		mtl = First @ TakeLargestBy[StringLength, 1][art -> "Value"];
		asc = <|"Articles" -> art, "ArticleCount" -> arc, "ArticleCountString" -> ToString[arc], "MaxTitleLength" -> mtl|>;
		txt = iGetWikipediaArticleText[asc["Articles"], asc["ArticleCount"], asc["ArticleCountString"], asc["MaxTitleLength"]];
		<|"Text" -> txt|> ~ Join ~ asc
	]

iGetWikipediaArticleText[articles_List, articleCount_Integer, articleCountString_String, maxTitleLength_Integer] :=
	Module[
		{txt},
		SetSharedVariable[ArticleIndex];
		ArticleIndex = 0;
		txt =
			Monitor[
				ParallelMap[(++ArticleIndex;WikipediaData[#])&, articles]
				,
				Row[
					{
						"Gathering text from " <> articleCountString <> " " <> articlePluralize[
							articleCount] <> ":\n"
						,
						Dynamic[
							Which[
								(ArticleIndex <= articleCount - 1),
									StringPadRight["\"" <> articles[[ArticleIndex + 1]] <> "\"",
										 maxTitleLength]
								,
								(ArticleIndex === 1),
									"\"" <> articles[[1]] <> "\" "
								,
								True,
									""
							]
						]
						,
						"\n"
						,
						ProgressIndicator[Dynamic[ArticleIndex], {0, articleCount}]
						,
						" "
						,
						Dynamic[NumberForm[PercentForm[N[ArticleIndex / articleCount]],
							 {3, 2}]]
					}
				]
			];
		ArticleIndex =.;
		txt
	]

(* LexicalCases *)

Options[LexicalCases] = {
	"Service" -> "Wikipedia",
	"StringTrim" -> True,
	IgnoreCase -> False,
	Overlaps -> False,
	MaxItems -> 50,
	MaxCategories -> 5,
	Language -> "English"
	};

LexicalCases::unsupfmt = "`` is not a supported format. Valid formats are: .txt, .md, .csv and .tsv";

LexicalCases[file_File, args___] :=
	Enclose[
		ConfirmAssert[SupportedFileQ[file], Message[LexicalCases::unsupfmt, GetFileExtension[file]]];
		Module[{data = Import[file]}, LexicalCases[data, args]]
	]

LexicalCases[input : List[__String], se_?LexicalPatternQ, opts : OptionsPattern[LexicalCases]] /; AllTrue[DirectoryQ \[Or] FileExistsQ][input] :=
	Enclose[
		ConfirmAssert[CheckArguments[LexicalCases[input, se, opts], 2]];
		Module[{files = Map[File][input], lpc},
			lpc = iLexicalCases[files, se, opts];
			GenerateLexicalSummary[lpc["Results"], "File", lpc["Text"], se]
		]
	]

oSourceType[List[__File]] :=
	"File"

oSourceType[List[__String]] :=
	"Text"

LexicalCases[input : (List[__File] | List[__String]), se_?LexicalPatternQ, opts : OptionsPattern[LexicalCases]] :=
	Enclose[
		ConfirmAssert[CheckArguments[LexicalCases[input, se, opts], 2]];
		Module[{lpc, ost},
			lpc = iLexicalCases[input, se, opts];
			ost = oSourceType[input];
			GenerateLexicalSummary[lpc["Results"], ost, lpc["Text"], se]
		]
	]

exprMsg[expr_String] := "\"" <> expr <> "\""

exprMsg[expr : Except[_String]] := expr

LexicalCases::nofl = "No files found with query `1`"

LexicalCases::nvsi = "`` has no value. It should be a SearchIndexObject."

LexicalCases[Rule[x_Symbol ? (Not @* ValueQ), _], ___] :=
	(
		Message[LexicalCases::nvsi, x];
		$Failed
	)

LexicalCases[input : Rule[index_SearchIndexObject, query_], se_?LexicalPatternQ, opts : OptionsPattern[LexicalCases]] :=
	Enclose[
		ConfirmAssert[CheckArguments[LexicalCases[input, se, opts], 2]];
		Enclose[
			Module[{
				files,
				f1 = DeleteMissing[Dataset[TextSearch[index, query][All, {"Text","Description"}]] // Normal, 2] // Flatten,
				f2 = Map[File]@DeleteMissing[TextSearch[index, query][All, "Location"]],
				lpc
				},
				If[Length@f1!=0,files = f1,files=f2];
				ConfirmAssert[Not @* MatchQ[{}] @ files];
				lpc = iLexicalCases[files, se, opts];
				GenerateLexicalSummary[lpc["Results"], "SearchIndex", Compress[lpc["Text"]], se]
			]
			,
			Failure["NoFilesFound",
				<|
					"MessageTemplate" -> StringTemplate["No files found using `Query`."],
					"MessageParameters" -> <|"Query" -> exprMsg[query]|>, "Tag" -> #["Tag"],
					"ConfirmationType" -> #["ConfirmationType"], "HeldTest" -> #["HeldTest"],
					"Information" -> "Try another string or specify a query with SearchQueryString."
				|>]&
		]
	]

(* SourceText and LexicalPattern Input *)

LexicalCases[sourcetext_String, se_, opts: OptionsPattern[LexicalCases]] :=
	Enclose[
		ConfirmAssert[CheckArguments[LexicalCases[sourcetext, se, opts], 2]];
		ConfirmBy[se,LexicalPatternQ];
		Module[{lpc, src = Compress[sourcetext], res},
			ArticleIndex = 0;
			(* Find Matches *)
			lpc = Confirm[LexicalCasesOnString[sourcetext, se, opts]];
			(* Generate Summary Object *)
			res = GenerateLexicalSummary[lpc, "Text", src, se];
			(* Clear Article Index *)
			ArticleIndex =.;
			res
		]
	]

(* SourceText is a string *)

LexicalCasesOnString[source_String, se_?LexicalPatternQ, opts : OptionsPattern[LexicalCases]] :=
	Enclose[
		Module[{rx, s = source, res},
			++ArticleIndex;
			rx = ConfirmQuiet[ExpandPattern[s, se], {Java::excptn, JavaNew::fail}];
			res = MatchTrim[OptionValue["StringTrim"]] @ Query[
				GroupBy[#Match&] /* (KeyValueMap[<|"Match" -> #1, "Position" -> #2|>&]),
				KeyDrop["Match"] /* Values /* (Flatten[#, 1]&)
				] @ Map[AssociationThread[{"Match", "Position"} -> #]&] @ Transpose @ {
					ConfirmQuiet[
						StringCases[s, rx, IgnoreCase -> OptionValue[IgnoreCase], Overlaps -> OptionValue[Overlaps]],
						{StringExpression::cond},
						"Message issued while calling StringCases"
						],
					StringPosition[s, StripNamedPattern[rx], IgnoreCase -> OptionValue[IgnoreCase], Overlaps -> OptionValue[Overlaps]]
					};
			res
		]
	]

(* LexicalPattern on Service *)

LexicalCases[se_?LexicalPatternQ, opts : OptionsPattern[{LexicalCases, iSearchWikipedia}]] :=
	Enclose[
		ConfirmAssert[CheckArguments[LexicalCases[se, opts], 1]];
		LexicalCasesFromService[OptionValue["Service"], se, FilterRules[{opts}, Options[iSearchWikipedia]]]
	]

(* WikiQueryRyle and LexicalPattern Input *)

LexicalCases[query_Rule, se_?LexicalPatternQ, opts : OptionsPattern[{LexicalCases, iSearchWikipedia}]] :=
	Enclose[
		ConfirmAssert[CheckArguments[LexicalCases[query, se, opts], 2]];
		LexicalCasesFromService[OptionValue["Service"], query, se, FilterRules[{opts}, Options[iSearchWikipedia]]]
	]

Options[LexicalCasesFromService] = {};

StringSearch[text_, pattern_] :=
	Module[{res = LexicalCasesOnString[text, pattern]},
		CriticalSection[lclock, MatchCount += Length[Replace[_Missing -> {}] @ Flatten[Lookup[res, "Position"], 1]]];
		res
	]

SearchArticles[texts_List, se_?LexicalPatternQ, articles_List, articleCount_Integer, maxTitleLength_Integer] :=
	Module[{mat},
		ArticleIndex = 0;
		MatchCount = 0;
		SetSharedVariable[se, MatchCount];
		SetSharedFunction[StringSearch];
		mat = Monitor[
			ParallelMap[(StringSearch[#,se])&, texts],
			Row[
				{
					"Searching ",
					Dynamic[
						Which[
							(ArticleIndex <= articleCount - 1), "\"" <> articles[[ArticleIndex + 1]] <> "\" ",
							(ArticleIndex === 1), "\"" <> articles[[1]] <> "\" ",
							True," "
							]
						],
					"\n", "Found: ",
					Dynamic[MatchCount], "\n", ProgressIndicator[Dynamic[ArticleIndex], {0, articleCount}], " ",
					Dynamic[NumberForm[PercentForm[N[ArticleIndex / articleCount]], {3, 2}]]
					}
				]
			];
		ArticleIndex =.;
		MatchCount =.;
		mat
	]

PackageResults[matches_, articles_List, articleCount_Integer, maxTitleLength_Integer] :=
	Module[{amt, mac},
		amt = Thread[{articles, ReplaceEmptyListWithMissing[matches]}];
		SetSharedVariable[amt, ArticleIndex];
		ArticleIndex = 0;
		mac =
			Monitor[
				ParallelMap[(++ArticleIndex;PrependArticleKey[#])&,amt],
				Row[
					{
						"Packaging results",
						ProgressIndicator[Appearance -> "Ellipsis"], "\n", ProgressIndicator[Dynamic[ArticleIndex], {0, articleCount}],
					 " ", Dynamic[NumberForm[PercentForm[N[ArticleIndex / articleCount]],{3, 2}]]
					}
					]
			];
		ArticleIndex =.;
		Flatten[mac]
	]

Options[iSearchWikipedia] = {MaxItems -> 50, MaxCategories -> 5, Language -> "English"};

iSearchWikipedia[se_?LexicalPatternQ, opts : OptionsPattern[{LexicalCases}]] :=
	Enclose[
		With[{wikiquery = StringTrim @ Confirm[ToWikipediaSearchQuery[se]]},
			iSearchWikipedia[wikiquery, se, opts]
		]
	]

iSearchWikipedia[query_?FailureQ, ___] := Return[query, With]

iSearchWikipedia[query_Rule, se_?LexicalPatternQ, opts : OptionsPattern[{LexicalCases}]] :=
	iLexicalCases[query, se, opts]

iSearchWikipedia[query : (_String | _List), se_?LexicalPatternQ, opts: OptionsPattern[{iSearchWikipedia, LexicalCases}]] :=
	iLexicalCases["Content" -> query, se, opts]

LexicalCasesFromService["Wikipedia", se_?LexicalPatternQ, opts : OptionsPattern[]] :=
	Module[
		{data = iSearchWikipedia[se, opts]},
		GenerateLexicalSummary[data["Results"], "Wikipedia", Compress[data["Text"]], se]
	]

LexicalCasesFromService["Wikipedia", query : (_Rule | _String | _List), se_?LexicalPatternQ, opts : OptionsPattern[]] :=
	Module[
		{data = iSearchWikipedia[query, se, opts]},	
		GenerateLexicalSummary[data["Results"], "Wikipedia", Compress[data["Text"]], se]
	]

GetText[input : List[__String], opts : OptionsPattern[]] :=
	Module[
		{len = Length[input], ren},
		ren = Range[len];
		<|
			"Text" -> input,
			"Articles" -> Map[ToString][ren],
			"ArticleCount" -> len,
			"ArticleCountString" -> ToString[len],
			"MaxTitleLength" -> (First @ TakeLargestBy[ren -> "Value", IntegerDigits /* Length, 1])
			|>
	]

GetText[input : List[__File], opts : OptionsPattern[]] :=
	Module[
		{len = Length[input], ren = Map[FileNameTake][input]},
		<|
			"Text" -> Map[Import[#, "Text"]&][input],
			"Articles" -> ren,
			"ArticleCount" -> len,
			"ArticleCountString" -> ToString[len],
			"MaxTitleLength" -> (First @ TakeLargestBy[ren -> "Value", IntegerDigits /* Length, 1])
			|>
	]

GetText[input_Rule, opts : OptionsPattern[]] :=
	iGetWikipediaArticles[input, opts]

iLexicalCases[input : (List[__String] | List[__File] | _Rule), se_?LexicalPatternQ, opts : OptionsPattern[{LexicalCases}]] :=
	Module[{ard, src, art, arc, acs, mtl, mtc},
		ard = GetText[input, opts];
		src = ard["Text"];
		art = ard["Articles"];
		arc = ard["ArticleCount"];
		acs = ard["ArticleCountString"];
		mtl = ard["MaxTitleLength"];
		(* 3 - Search for LexicalPattern *)
		mtc = SearchArticles[src, se, art, arc, mtl];
		<|"Results"->PackageResults[mtc, art, arc, mtl], "Text" -> src|>
	]

(* LexicalSummary *)

ArticleWithMatchCount["Text", _] := ""

GetArticleCount[{}] := 0

GetArticleCount[data_] := (Lookup["Article"] /* DeleteDuplicates /* Length)[data]

ArticleWithMatchCount[_String, data_] :=
	(
		data //
		DeleteMissing[#, 1, 1]& //
		GetArticleCount
	)

DisplayArticlesWithMatch["Text", _] := Nothing

DisplayArticlesWithMatch[source_, data_] := 
	{BoxForm`SummaryItem[{"Articles: ", ArticleWithMatchCount[source, data]}]}

(* LexicalSummary *)
Options[LexicalSummary] = {
	MaxItems -> All,
	DeleteStopwords -> False,
	ImageSize -> Automatic
};

LexicalSummary /: MakeBoxes[obj : LexicalSummary[asc_?LexicalSummaryAscQ
	], form : (StandardForm | TraditionalForm)] :=
	Module[{above, below},
		above =
			{(*example grid*)
			{BoxForm`SummaryItem[{"Source: ", asc["Source"]}]},
			DisplayArticlesWithMatch[asc["Source"], asc["Data"]],
			{BoxForm`SummaryItem[{"Matches: ", asc["TotalMatchCount"]}]}
			};
		below = {};
		BoxForm`ArrangeSummaryBox[
			LexicalSummary
			,(*head*)
			obj
			,(*interpretation*)
			None
			,(*icon,use None if not needed*)
			above
			,(*always shown content*)
			below
			, (*expandable content*)
			form
			,
			"Interpretable" -> Automatic
		]
	];

LexicalSummaryAscQ[asc_?AssociationQ] :=
	AllTrue[{"Data", "Dataset", "Source", "SourceData", "TotalMatchCount", "LexicalStructure"}, KeyExistsQ[asc, #]&]

LexicalSummaryAscQ[_] = False;

(* Direct Properties *)

LexicalSummary[asc_?LexicalSummaryAscQ]["Data"] :=
	asc["Data"]

LexicalSummary[asc_?LexicalSummaryAscQ]["Source"] :=
	asc["Source"]

LexicalSummary[asc_?LexicalSummaryAscQ]["SourceData"] :=
	asc["SourceData"]

LexicalSummary[asc_?LexicalSummaryAscQ]["Dataset"] :=
	asc["Dataset"]

LexicalSummary[asc_?LexicalSummaryAscQ]["TotalMatchCount"] :=
	asc["TotalMatchCount"]

LexicalSummary[asc_?LexicalSummaryAscQ]["LexicalStructure"] :=
	asc["LexicalStructure"]

(* Dataset Properties *)

LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroupPercentages"] :=
	PercentDataset[LexicalSummary[asc]["CountGroups"], asc["TotalMatchCount"]]

LexicalSummary[asc_?LexicalSummaryAscQ]["LowercaseCountGroupPercentages"] :=
	PercentDataset[LexicalSummary[asc]["CountGroups"] // CountSummaryLowercase, asc["TotalMatchCount"]]

(* Dataset Properties Filtered *)
$FilterableProperties = {"Data", "Dataset", "CountGroupPercentages", "LowercaseCountGroupPercentages", "CountGroups", "Counts", "WordCloud", "Survey", "PartOfSpeechGroups", "WordStemCountGroups", "LexicalDispersion"(* , "SmoothLexicalHistogram" *)};
LexicalSummary[asc_?LexicalSummaryAscQ][prop_String, opts:OptionsPattern[]] := Enclose[
	ConfirmAssert[MemberQ[$FilterableProperties, prop], "Key [" <> prop <> "] is either an invalid LexicalSummary property or it does not support MaxItems or DeleteStopwords options"];
	Module[
		{
			maxitems  = Replace[OptionValue[LexicalSummary, {opts}, MaxItems], All -> Infinity],
			excludestopwords = OptionValue[LexicalSummary, {opts}, DeleteStopwords],
			imgsize = OptionValue[LexicalSummary, {opts}, ImageSize],
			res
			},
		Switch[prop,
			"WordCloud",
			(
				res = asc["Dataset"] // ReverseSortBy[Length[#Position] &];
				res = If[excludestopwords, FilterOutStopWordRows["WordCloud", res], res];
				res[ ;; UpTo[maxitems]][All, Table[#Match, Length[#Position]] &] // Normal/*Flatten/*WordCloud
			),
			"Counts",
			(
				res = LexicalSummary[asc]["Counts"];
				res = If[excludestopwords, FilterOutStopWordRows["Counts", res], res];
				res[ ;; UpTo[maxitems]]
			),
			"Survey",
			(
				GenerateDashboard[LexicalSummary[asc], MaxItems -> maxitems, DeleteStopwords -> excludestopwords, ImageSize -> imgsize]
			),
			"LexicalDispersion",
			(
				res = asc["Dataset"] // ReverseSortBy[Length[#Position] &];
				res = If[excludestopwords, FilterOutStopWordRows["Dataset", res], res];
				res = res[ ;; UpTo[maxitems]];

				(* NOTE: Plotting each match might take a while.. *)
				Block[
					{
						keys = res[All,#Match&] // Normal // DeleteDuplicates,
						ucmp = Uncompress[asc["SourceData"]]
						},
					Switch[asc["Source"],
						"Wikipedia", LexicalDispersionPlot[ucmp, res, keys, DataJoin -> True],
						_, LexicalDispersionPlot[ucmp, res, keys]
					]
				]
			),
(* 			"SmoothLexicalHistogram",
			(
				res = asc["Dataset"] // ReverseSortBy[Length[#Position] &];
				res = If[excludestopwords, FilterOutStopWordRows["Dataset", res], res];
				res = res[ ;; UpTo[maxitems]];

				(* NOTE: Plotting each match might take a while.. *)
				Block[
					{
						keys = res[All,#Match&] // Normal // DeleteDuplicates,
						ucmp = Uncompress[asc["SourceData"]]
						},
					Switch[asc["Source"],
						"Wikipedia", LexicalDispersionPlot[ucmp, res, keys, DispersionPlotFunction -> "SmoothHistogram", DataJoin -> True],
						_, LexicalDispersionPlot[ucmp, res, keys, DispersionPlotFunction -> "SmoothHistogram"]
						]
				]
			), *)
			"Data"|"Dataset", (
				res = asc["Dataset"];
				res = If[excludestopwords, FilterOutStopWordRows[prop, res], res];
				res = res[;; UpTo[maxitems]];
				If[prop==="Data", Normal[res], res]
			),
			_, (
				res = LexicalSummary[asc][prop];
				res = If[excludestopwords, FilterOutStopWordRows[prop, res], res];
				res[ ;; UpTo[maxitems]]
			)
		]
		
		]
	]

(* Count Properties *)

LexicalSummary[asc_?LexicalSummaryAscQ]["Counts"] :=
	GetDatasetCounts[LexicalSummary[asc]["Dataset"], asc["Source"]] //
	DeleteMissing[#, 1, 1]& //
	ReverseSortBy[#Count&]

LexicalSummary[asc_?LexicalSummaryAscQ]["CountGroups"] :=
	(LexicalSummary[asc]["Counts"] // CountGroups)

LexicalSummary[asc_?LexicalSummaryAscQ]["WordCloud"] := asc["Dataset"][All, Table[#Match, Length[#Position]] &] // Normal/*Flatten/*WordCloud

LexicalSummary[asc_?LexicalSummaryAscQ][invalidkey_] :=
	asc[invalidkey]

LexicalSummary[asc_?LexicalSummaryAscQ]["Properties"] :=
	{
		"Data", "Dataset", "Counts", "CountGroups", "CountGroupPercentages",
		"LowercaseCountGroupPercentages", "PartOfSpeechGroups", "WordStemCountGroups",
		"Source", "TotalMatchCount", "LexicalStructure", "Survey"}

GenerateLexicalSummary[data_?FailureQ, ___] := data

GenerateLexicalSummary[data_, sourceType_String, se_?LexicalPatternQ] :=
	Enclose[
		iGenerateLexicalSummary[data, sourceType, se],
		Identity,
		"LexicalSummaryFailed"
		]

iGenerateLexicalSummary[data_, source_String, se_?LexicalPatternQ] := Enclose[
	Module[{mtc, ds = Dataset[data], cse = StripNamedPattern @ se, stc},
		stc = LexicalStructure[cse];
		mtc = DeleteMissing[GetDatasetCounts[ds, source], 1, 1][Total, "Count"];
		Confirm[
			LexicalSummary[<|"Data" -> data, "Dataset" -> ds, "Source" -> source, "TotalMatchCount" -> mtc, "LexicalStructure" -> stc|>],
			Null,
			"LexicalSummaryFailed"
			]
		]
	]

GenerateLexicalSummary[data_, sourceType_String, sourcedata:(List[__String]|_String), se_?LexicalPatternQ] :=
	Enclose[
		iGenerateLexicalSummary[data, sourceType, sourcedata, se],
		Identity,
		"LexicalSummaryFailed"
		]


iGenerateLexicalSummary[data_, sourceType_String, sourcedata:(List[__String]|_String), se_?LexicalPatternQ] :=
	Enclose[
		Module[
			{mtc, ds = Dataset[data], cse = StripNamedPattern @ se, stc},
			stc = LexicalStructure[cse];
			mtc = DeleteMissing[GetDatasetCounts[ds, sourceType], 1, 1][Total, "Count"];
			Confirm[
				LexicalSummary[<|"Data" -> data, "Dataset" -> ds, "Source" -> sourceType, "SourceData" -> sourcedata, "TotalMatchCount" -> mtc, "LexicalStructure" -> stc|>],
				Null,
				"LexicalSummaryFailed"
				]
		]
	]

(* Summary Utils *)

GetDatasetCounts[ds_Dataset, "Text"] :=
	ds[All, <|"Match" -> "Match", "Count" -> "Position" /* Length|>]

GetDatasetCounts[ds_Dataset, "Wikipedia" | "SearchIndex"] :=
	ds[GroupBy["Match"], Map[KeyDrop[{"Article", "Match"}]]][All, Total @* Map[Length], "Position"] // KeyValueMap[<|"Match" -> #1, "Count" -> #2|>&]

CountGroups[ds_Dataset] :=
	Query[
		GroupBy[#Count&] /* KeyValueMap[<|"Matches" -> #2, "CountGroup" -> #1|>&],
		KeyDrop["Count"] /* Values /* (Flatten[#, 1]&)
		][ds]

CountGroupDSQ[ds_Dataset] /; Normal /* Length /* GreaterThan[0] @ ds :=
	Normal /* First /* KeyMemberQ["CountGroup"] @ ds

CountDSQ[ds_Dataset] /; Normal /* Length /* GreaterThan[0] @ ds :=
	Normal /* First /* KeyMemberQ["Count"] @ ds

CountGroupDSQ[_] := False

CountDSQ[_] := False

CountSummaryLowercase[ds_Dataset] /; CountDSQ[ds] :=
	ReverseSortBy[#Count&] @ Query[GroupBy[ToLowerCase[#Match]&] /* KeyValueMap[
		<|"Match" -> #1, #2|>&], Total /* KeyDrop["Match"]][ds]

CountSummaryLowercase[ds_Dataset] :=
	ds

ThreadMatchesWithCount[asc_Association] :=
	Apply[Sequence][Map[<|"Matches" -> ToLowerCase[#], "Count" -> asc["CountGroup"
		]|>&][asc["Matches"]]]

CountSummaryLowercase[ds_Dataset] /; CountGroupDSQ[ds] :=
	ReverseSortBy[#CountGroup&] @ Query[
			GroupBy[#Count&] /* KeyValueMap[<|"Matches" -> #2, "CountGroup" -> #1|>&],
			KeyDrop["Count"] /* Values /* (Flatten[#, 1]&)][
				Query[
					GroupBy[#Matches&] /* KeyValueMap[<|"Matches" -> #1, #2|>&],
					Total /* KeyDrop["Matches"]
					][ds[All, ThreadMatchesWithCount]]
					]

FilterOutStopWordRows["CountGroups"|"CountGroupPercentages"|"LowercaseCountGroupPercentages"|"WordStemCountGroups"|"PartOfSpeecGroups", ds_Dataset] :=
	ds[(DeleteCases[#, _String?StopWordQ, 3]&) /* Select[Not @ SameQ[{}, #Matches]&]]
FilterOutStopWordRows["Counts", ds_Dataset] :=
	ds[All, Replace[#, KeyValuePattern[{"Match" -> s_String?StopWordQ}] :> <|"Match" -> Missing["StopWord"], "Count" -> #Count|>] &] // DeleteMissing[#, 1, 1] &
FilterOutStopWordRows["WordCloud", ds_Dataset] :=
	ds[All, Replace[#, KeyValuePattern[{"Match" -> s_String?StopWordQ}] :> <|"Match" -> Missing["StopWord"], "Position" -> #Position|>] &] // DeleteMissing[#, 1, 1] &
FilterOutStopWordRows["Dataset", ds_Dataset] :=
	ds[All, Replace[#, KeyValuePattern[{"Match" -> s_String?StopWordQ}] :> <|"Match" -> Missing["StopWord"], "Position" -> #Position|>] &] // DeleteMissing[#, 1, 1] &

PartOfSpeechKey[word_String] :=
	ProcessWordData[WordData[word, "PartsOfSpeech"]]

ProcessWordData[w_WordData] := None

ProcessWordData[w_] := w

(* TODO: module-ize these functions for clarity *)

PartOfSpeechGroups[ds_Dataset] :=
	(ds[TextWords /* ToLowerCase /* Flatten /* DeleteStopwords /* DeleteDuplicates, "Matches"][AlphabeticSort][GroupBy[PartOfSpeechKey]][KeyDrop[None]] // KeySort) // KeyValueMap[<|"Words" -> #2, "PartOfSpeech" -> #1|>&]

GetWordStemCounts[ds_Dataset] :=
	(
		ds[All, "Matches"] //
		Normal //
		Flatten //
		(StringSplit[#, WordBoundary]&) /* Flatten /* DeleteCases[" " | "\n"] //
		DeleteStopwords //
		ToLowerCase //
		WordStem //
		Counts
	)

WordStemGroups[ds_Dataset] :=
	(
		GetWordStemCounts[ds] //
		KeyValueMap[<|"Stem" -> #1, "Count" -> #2|>&] //
		Dataset //
		ReverseSortBy[#Count&]
	) //
	Query[GroupBy[#Count&], KeyDrop["Count"] /* Values /* (Flatten[#, 1]&
		)] //
	KeyValueMap[<|"WordStem" -> #2, "CountGroup" -> #1|>&]

PercentDataset[ds_Dataset, matchcount_Integer] :=
	(ds[All, <|"Matches" -> #Matches, "Percentage" -> Quantity[100. N[((
		#CountGroup Length[#Matches]) / matchcount)], "Percent"]|>&][ReverseSortBy[
		"Percentage"]])

(* TODO: Dashboard should only present plots  *)
GenerateDashboard[lps_LexicalSummary, opts___Rule] :=
	With[
		{
			wcld = lps["WordCloud", opts],
			ldpt = lps["LexicalDispersion", opts]
			(* slhp = lps["SmoothLexicalHistogram", opts] *)
			},
		DynamicModule[
			{tab = "LexicalDispersion"},
			Column[
				{
					Panel[SetterBar[Dynamic[tab], {"LexicalDispersion"(* , "SmoothLexicalHistogram" *), "WordCloud"}]],
					Dynamic[
						Switch[tab,
							"LexicalDispersion",
								ldpt
							,
(* 							"SmoothLexicalHistogram",
								slhp
							, *)
							"WordCloud",
								wcld
						]
					]
				}
			]
		]
	]

End[]

EndPackage[]
