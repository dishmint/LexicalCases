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

(* Abstractions *)

LexicalMap::usage = "LexicalMap[f, source, lp] maps f over each match of lp in source."
LexigramCount::usage = "LexigramCount[lp] returns the number of lexigrams in the lexical pattern."

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

TypeToken::usage = "TypeToken[type] a symbolic wrapper for TextContentTypes"
OptionalToken::usage = "OptionalToken[lp] matches the lexical pattern lp, whitespace \" \", or an empty string \"\"."
BoundToken::usage = "BoundToken[lp] represents a bounded form of the lexical pattern lp\nBounded[lp1|\[Ellipsis]|lpi] represents a bounded form of the alternatives lpi.\nBoundToken[outer, inner] places inner between two outer's."
WordToken::usage = "WordToken[n] represents n words separated by spaces\nWordToken[m,n] represents m to n words separated by spaces."
SynonymToken::usage = "SynonymToken[word] represents a curated set of synonyms for word."
ExpandPattern::usage = "ExpandPattern[source, lp] expands lexical pattern lp into a valid StringExpression with content from source."
LexicalPattern::usage = "LexicalPattern[patt] A wrapper for using lexical patterns in string functions"

(* Format *)

LexicalStructure::usage = "LexicalStructure[lp] Visualize lexical pattern structure"
FormatLexicalPattern::usage = "FormatLexicalPattern[lp] formats lexical pattern lp for labeling"

(* Services *)

$LexicalCasesServices::usage = "List of supported services"

(* Analysis / Visualization *)

LexicalDispersionPlot::usage = "LexicalDispersionPlot[text, w] plots the dispersion of word w across text\nLexicalDispersionPlot[text, {w1, \[Ellipsis], wi}] plots the dispersion of the wi across text"
LexicalDispersionSmoothHistogram::usage = "LexicalDispersionSmoothHistogram[text, w] plots the dispersion of word w across text as a smooth histogram.\nLexicalDispersionSmoothHistogram[text, {w1, \[Ellipsis], wi}] plots the dispersion of the wi across text as a smooth histogram."
HideMissing::usage = "HideMissing is an option to LexicalDispersionPlot for hiding keywords from the plot if they were not found in the source text"
DataJoin::usage = "DataJoin is an option to LexicalDispersionPlot specifying whether to combine results when given multiple source texts"

(* Options *)

MaxCategories::usage = "MaxCategories is an option to LexicalCases restricting the number of Wikipedia categories to get articles from"

Begin["`Private`"]

Needs["FaizonZaman`LexicalCases`Samples`"]
Needs["FaizonZaman`LexicalCases`Utilities`"]
Needs["FaizonZaman`LexicalCases`LexicalPattern`"]
Needs["FaizonZaman`LexicalCases`Validation`"]
Needs["FaizonZaman`LexicalCases`Abstractions`"]
Needs["FaizonZaman`LexicalCases`Plots`"]

ContainsPatternHeadsQ::usage = "ContainsPatternHeadsQ[expr] returns True if expr contains the head Pattern"
ContainsPatternHeadsQ[se_?LexicalPatternQ] :=
	ContainsAny[ExtractHeads[se], {Pattern}]
	(* [^] Why isn't this using FreeQ? Then I don't need to ExtractHeads *)

StripNamedPattern[se_?LexicalPatternQ] :=
	StripNames[ContainsPatternHeadsQ[se], se]
	(* [^] Strip names from named patterns (name: patt); otherwise causes pattern issues later on (nmi) *)
StripNames[True, HoldPattern[(Rule|RuleDelayed)[se_?LexicalPatternQ,_]]] :=
	Replace[se, p_Pattern :> Extract[2][p], Infinity]
	(* [^] Replace Pattern[name, patt] with patt *)
StripNames[True, c_Condition] := c
(* [^] A condition stays the same *)
StripNames[True, se_?LexicalPatternQ] :=
	Replace[se, p_Pattern :> Extract[2][p], Infinity]
	(* [^] Replace Pattern[name, patt] with patt *)
StripNames[False, se_?LexicalPatternQ]:= se
(* [^]  If the LexicalPattern doesn't contain Pattern objects, return the LexicalPattern *)

PrependArticleKey[{article_String, data_List}] :=
	Prepend@@@Thread[{data, "Article" -> article}]
	(* [^] Prepend the article title to each result  *)
PrependArticleKey[{article_String, data_Missing}] :=
	<|"Article" -> article, "Match" -> data, "Position" -> {}|>
	(* [^] If data is Missing then set Position to {}  *)

PrependArticleKey[{article_Integer, data_List}] :=
	Prepend@@@Thread[{data, "Article" -> article}]
	(* [^] Prepend the article id (int) to each result *)
PrependArticleKey[{article_Integer, data_Missing}] :=
	<|"Article" -> article, "Match" -> data, "Position" -> {}|>
	(* [^] If data is Missing then set Position to {} *)

(* Text Cleanup *)

EscapePunctuation[s_String] :=
	StringReplace[s, pc:PunctuationCharacter :> "\\" <> pc]
	(* [^] Ensure punctuation is escaped as it affects searching downstream (nmi) *)

articlePluralize::noa = "No articles found"
articlePluralize[0] := Message[articlePluralize::noa]
(* [^] Message the user if no articles are found *)
articlePluralize[1] := "article"
(* [^] Use singular for one article *)
articlePluralize[_Integer?Positive] := "articles"
(* [^] Use plural for multiple articles *)

(* Format *)

FormatToken::usage = "FormatToken[lp] formats lexical pattern lp for use in LexicalStructure"

FormatToken[StringExpression[args___]] :=
	FormatToken[StringExpression, args]
	(* [^] Format the args of StringExpression *)

FormatToken[TypeToken[type_String]] :=
	TextElement[{type}, <|"GrammaticalUnit" -> "TypeToken"|>]
	(* [^] Format type as a TypeToken GU *)

FormatToken[TypeToken[Containing[outer_, inner_]]] :=
	TextElement[{inner}, <|"GrammaticalUnit" -> outer|>]
	(* [^] Format inner as a outer GU *)

FormatToken[TypeToken[types_Alternatives]] :=
	TextElement[
		WrapAlternatives[Map[FormatToken][ExpandAlternativeTypeTokens[types]]],
		<|"GrammaticalUnit" -> "Alternatives"|>
	]
	(* [^] Expand and format a set of TypeToken's as an Alternatives GU   *)

FormatToken[SynonymToken[words_Alternatives]] :=
	TextElement[
		WrapAlternatives[Map[FormatToken][ExpandAlternativeSynonyms[words]]],
		<|"GrammaticalUnit" -> "Alternatives"|>
	]
	(* [^] Expand and format a set of SynonymToken's as an Alternatives GU   *)

FormatToken[OptionalToken[args__]] :=
	TextElement[
		Map[FormatToken][{args}],
		<|"GrammaticalUnit" -> "Optional"|>
	]
	(* [^] Format args as an Optional GU *)

FormatToken[AnyOrder[args__]] :=
	TextElement[
		Map[FormatToken][{args}],
		<|"GrammaticalUnit" -> "AnyOrder"|>
	]
	(* [^] Format args as an AnyOrder GU *)

FormatToken[FixedOrder[args__]] := TextElement[
	Map[FormatToken][{args}],
	<|"GrammaticalUnit" -> "FixedOrder"|>
]
(* [^] Format args an a FixedOrder GU *)

FormatToken[HoldPattern[WordToken[1]]] := TextElement[{1}, <|"GrammaticalUnit" -> "Word"|>];
(* [^] Format Word Tokens *)
FormatToken[HoldPattern[WordToken[n_Integer]]] := TextElement[{n}, <|"GrammaticalUnit" -> "Words"|>];
(* [^] Format Word Sequence Tokens *)
FormatToken[WordToken[args__Integer]] := TextElement[{Span[args]}, <|"GrammaticalUnit" -> "Words"|>];
(* [^] Format Word Interval Tokens *)
FormatToken[SynonymToken[s_String]] := TextElement[{s}, <|"GrammaticalUnit" -> "Synonyms"|>];
(* [^] Format Synonym Tokens *)
FormatToken[HoldPattern[_]] := TextElement[{"_"}, <|"GrammaticalUnit" -> "Blank"|>];
(* [^] Format Blank *)
FormatToken[HoldPattern[__]] := TextElement[{"__"}, <|"GrammaticalUnit" -> "BlankSequence"|>];
(* [^] Format BlankSequence *)
FormatToken[HoldPattern[___]] := TextElement[{"___"}, <|"GrammaticalUnit" -> "BlankNullSequence"|>];
(* [^] Format BlankNullSequence *)
FormatToken[HoldPattern[h : (Blank | BlankSequence | BlankNullSequence)[type_]]] := TextElement[{type}, <|"GrammaticalUnit" -> ToString[h]|>];
(* [^] Format named Blank's *)
FormatToken[a_Alternatives] := TextElement[WrapAlternatives[Map[FormatToken][a]], <|"GrammaticalUnit"-> "Alternatives"|>];
(* [^] Format Alternatives *)
FormatToken[s_String] := TextElement[{s}, <|"GrammaticalUnit" -> "Text"|>];
(* [^] Format plain text *)
FormatToken[s_Symbol] := s;
(* [^] Format a symbol *)
FormatToken[atom_?AtomQ] := atom;
(* [^] Format an atomic head *)
FormatToken[h : Except[_Blank | _BlankSequence | _BlankNullSequence | _Words]] :=
	With[
		{
			head = Head[h],
			arg = Check[Extract[1][h], $Failed, {Extract::partw,Extract::partd}]
			},
		FormatToken[head, arg]
	]
(* [^] Intermediate check for patterns without args *)
FormatToken[TypeToken, s_String] := TextElement[s, <|"GrammaticalUnit" -> "TypeToken"|>];
(* [^] Format TypeToken's of type s *)
FormatToken[t_, $Failed] := Enclose[Confirm[$Failed, Message[FormatToken::nvld, t], "InvalidToken"]]
(* [^] Throw Failure if token t is not supported *)
FormatToken[h_, args__] := TextElement[Map[FormatToken][{args}], <|"GrammaticalUnit" -> ToString[h]|>];
(* [^] Format nested tokens *)
FormatToken::nvld = "`1` is not supported in FormatToken"
(* [^] FormatToken Message *)

(* ---------------------------- LexicalStructure ---------------------------- *)
SetAttributes[LexicalStructure, HoldAll]

LexicalStructure[expr_?LexicalPatternQ] :=
	Enclose[FormatToken[expr], Identity, "InvalidToken"]
	(* [^] Run FormatToken on expr; Returns a Failure if FormatToken::nvld issues *)

LexicalStructure[(Rule | RuleDelayed)[expr_?LexicalPatternQ, _]] :=
	Enclose[
		Construct[FormatToken, StripNamedPattern[expr]],
		Identity,
		"FormatToken`InvalidToken"
	]
	(* [^] Run FormatToken on expr, removing named patterns before formatting *)

(* ------------------ Formatting LexicalPattern for Labels ------------------ *)
(* TypeToken *)
FormatLexicalPattern[TypeToken[a_String]] := ToUpperCase[a]
(* [^] TypeToken["Adjective"] formats as `ADJECTIVE` *)
FormatLexicalPattern[TypeToken[a_Alternatives]] := Riffle[Map[ToUpperCase][a] // Apply[List], "|"] // Row
(* [^] TypeToken["Adjective"|"Noun"] formats as `ADJECTIVE|NOUN` *)

(* BoundToken *)
FormatLexicalPattern[BoundToken[a_]] := Row[{"[", FormatLexicalPattern[a], "]"}]
(* [^] BoundToken["hello"] formats as `[hello]` *)

(* WordToken *)
FormatLexicalPattern[WordToken[n_]] := StringTemplate["\!\(\*SubscriptBox[\(WORD\), \(``\)]\)"][n]
(* [^] WordToken[3] formats as `WORD` with subscript `3` *)
FormatLexicalPattern[WordToken[n_, m_]] := StringTemplate["\!\(\*SubscriptBox[\(WORD\), \(`` - ``\)]\)"][n, m]
(* [^] WordToken[2, 4] formats as `WORD` with subscript `2-4` *)

(* OptionalToken *)
FormatLexicalPattern[OptionalToken[a_]] := Style[Row[{"(", FormatLexicalPattern[a], ")"}], Gray]
(* [^] OptionalToken["hello"] formats as `(hello)` *)

(* SynonymToken *)
FormatLexicalPattern[SynonymToken[s_String]] := Style[StringTemplate["\!\(\*SubscriptBox[\(``\), \(syn\)]\)"][s], Darker[Blue]]
(* [^] SynonymToken["hello"] formats as `hello` with subscript `syn` *)

(* Alternatives *)
FormatLexicalPattern[a_Alternatives?LexicalPatternQ] := Riffle[Map[FormatLexicalPattern, a] // Apply[List], "|"] // Row
(* [^] Formats an Alternatives of LexicalPattern's *)

(* LexicalPattern *)
FormatLexicalPattern[lp_?LexicalPatternQ] := Map[FormatLexicalPattern, lp] // Apply[List/*Row]
(* [^] Formats a LexicalPattern *)

(* Service Utils *)

$LexicalCasesServices = {"Wikipedia"}
(* [^] List of supported services *)

ArticleIndex = 0;
(* [^] Index of processed article / piece of text *)

ArticleSearchIndicator[service_String, query_] :=
	Row[{"Searching " <> service <> " for ", query, ProgressIndicator[Appearance -> "Ellipsis"]}]
(* [^] Indicates to user the query is being searched *)

(* Patterns *)

ExpandAlternativeTypeTokens[alts_Alternatives] :=
	(Apply[Alternatives] @* Map[TypeToken] @* Apply[List])[alts]
	(* [^] TypeToken[A|B|C] -> TypeToken[A]|TypeToken[B]|TypeToken[C] *)
ExpandAlternativeSynonyms[alts_Alternatives] :=
	(Apply[Alternatives] @* Map[SynonymToken] @* Apply[List])[alts]
	(* [^] SynonymToken[A|B|C] -> SynonymToken[A]|SynonymToken[B]|SynonymToken[C] *)

BoundedToken[outer_, inner_] :=
	outer ~~ inner ~~ outer
	(* [^] inner pattern surrounded by outer pattern *)

BoundToken[outer_][inner_] :=
	BoundToken[outer, inner]
	(* [^] BoundToken is an inert wrapper, BoundToken[o, i] will expand into a call of BoundedToken *)

BoundedWord[expr_] :=
	WordBoundary~~expr~~WordBoundary
	(* [^] expr surrounded by WordBoundary to prevent subword matches *)

$WordAndContractionToken = BoundedWord[(WordCharacter | "'")..]
(* [^] Maintains contractions as a single word *)
$WordToken = BoundedWord[WordCharacter..]
(* [^] A set of word characters bound by WordBoundary *)

Unpattern[expr_, f_ : Identity] :=
	MapAt[Comap[unpatterned[First, Last/*f]], expr, Position[expr, _Pattern]]
	(* [^] Replace head Pattern with the inert head unpatterned for processing *)
Repattern[expr_] :=
	expr /. unpatterned -> Pattern
	(* [^] Replace the inert head unpatterned with Pattern to reinstate original expr *)
$UnpatternPattern[tok_] :=
	(tok|unpatterned[uname_, tok])
	(* [^] A pattern containing a token or an unpatterened token *)

(* Generic Whitespace wrapping *)
(* iWrapSpace[tk_, String] := BoundedToken[" ", tk] 
iWrapSpace[tk_, StartOfString] := (tk ~~ " ")
iWrapSpace[tk_, EndOfString] := (" " ~~ tk)
iWrapSpace[tk_, ContainsOnly] := tk *)
(* [^] These may be unused *)

ExpandToken[tok_TypeToken, position_ : String, content_] :=
	iExpandToken[tok, position, content]
ExpandToken[utok:unpatterned[name_, tok_TypeToken], position_ : String, content_] :=
	iExpandToken[utok, position, content]
ExpandToken[tok_, position_ : String] :=
	iExpandToken[tok, position]
ExpandToken[utok:unpatterned[name_, tok_], position_ : String] :=
	iExpandToken[utok, position]
(* [^] Top level ExpandToken signatures *)

$UOpt[ot_] := ot /. (OptionalToken[alt_] :> alt)
(* [^] Shucks any OptionalToken's in an expression; Used for pattern hack representing optional as " " or " "~~opt~~" " *)
iExpandToken[opt:$UnpatternPattern[_OptionalToken], String] :=
	With[{o = $UOpt[opt]}, BoundedToken[" ", o] | " "]
	(* [^] the optional token o surrounded by spaces OR just the space *)
iExpandToken[opt:$UnpatternPattern[_OptionalToken], StartOfString] :=
	With[{o = $UOpt[opt]}, (o ~~ " ") | ""]
	(* [^] the optional token o followed by a space OR just the empty string *)
iExpandToken[opt:$UnpatternPattern[_OptionalToken], EndOfString] :=
	With[{o = $UOpt[opt]}, (" " ~~ o) | ""]
	(* [^] the optional token o preceded by a space OR just the empty string *)
iExpandToken[opt:$UnpatternPattern[_OptionalToken], ContainsOnly] :=
	With[{o = $UOpt[opt]}, o | ""]
	(* [^] the optional token o OR just the empty string *)

$WordTokenRules = {
		WordToken[1] :>
			$WordToken,
			(* [^] A single word *)
		
		WordToken[n_Integer] :>
			StringExpression[
				$WordToken, " ",
				(* [^] First word followed by a space *)
				Splice[
					Riffle[ConstantArray[$WordToken, n - 1], " "]
				]
				(* [^] Rest of the words Riffle'd with a space *)
			],
			(* [^] A set of n words *)
		
		WordToken[m_Integer, n_Integer] :>
			Alternatives @@ Array[ReplaceAll[$WordTokenRules] @* WordToken, n - 1, m],
			(* [^] A set of m to n words *)
		
		WordToken[1, "KeepContractions"] :>
			$WordAndContractionToken,
			(* [^] A single word with a contraction, ex: don't *)
		
		WordToken[n_Integer, "KeepContractions"] :>
			StringExpression[
				$WordAndContractionToken, " ",
				Sequence @@ Riffle[ConstantArray[$WordAndContractionToken, n - 1], " "]
			],
			(* [^] A set of n words with a contraction *)
		
		WordToken[m_Integer, n_Integer, "KeepContractions"] :>
			Alternatives @@ Array[ReplaceAll[$WordTokenRules] @* (WordToken[#, "KeepContractions"]&), n - 1, m]
			(* [^] A set of m to n words with contractions *)
};

(* Expanding WordToken's *)
iExpandToken[word:$UnpatternPattern[_WordToken], pos_] :=
	iExpandWord[word, pos] /. $WordTokenRules
	(* [^] ReplaceAll occurrences of $WordTokenRules *)
iExpandWord[word:$UnpatternPattern[_], String] :=
	BoundedToken[" ", word] 
	(* [^] Expand word into word surrounded by spaces  *)
iExpandWord[word:$UnpatternPattern[_], StartOfString] :=
	(word ~~ " ")
	(* [^] Expand word by appending a space after it  *)
iExpandWord[word:$UnpatternPattern[_], EndOfString] :=
	(" " ~~ word)
	(* [^] Expand word by prepending a space before it  *)
iExpandWord[word:$UnpatternPattern[_], ContainsOnly] :=
	word
	(* [^] Leave word as is  *)

(* ----------------------------- Hydrate Tokens ----------------------------- *)
MassTypeToken[ca_Association][expr_] :=
	MassTypeToken[expr, ca]
MassTypeToken[expr_, ca_] :=
	With[
		{
			(* [1] -- Get Expanded TypeTokens *)
			expandedTypeTokenAlts = Replace[expr, TypeToken[alts_Alternatives] :> ExpandAlternativeTypeTokens[alts], {0, Infinity}]
			(* [^] Expand TypeToken[A|B|C] -> TypeToken[A]|... *)
		},
		FixedPoint[
			(* Replace[#, TypeToken[type : (_String | _Containing)] :> BoundedWord[UnwrapAlternatives[ca[type]]], {0, Infinity}]&, *)
			Function[Replace[#, TypeToken[type : (_String | _Containing)] :> UnwrapAlternatives[ca[type]], {0, Infinity}]],
			(* [^]  Replace TypeTokens with content via ca lookup on type *)
			expandedTypeTokenAlts
		]
	]

MassWordToken[expr_] :=
	expr /. $WordTokenRules
	(* [^] Expand word tokens in expr *)

MapAtOptionalToken[func_, expr_] :=
	MapAt[func, expr, Position[expr, _OptionalToken]]
	(* [^] Map func over all OptionalToken's in expr *)

iExpandOptional[opt:anchor[__], String] :=
	MapAtOptionalToken[With[{o = $UOpt[#]}, BoundedToken[" ", o] | " "]&, opt]
	(* [^] Expand OptionalToken o into o surrounded by spaces or just a space *)
iExpandOptional[opt:anchor[__], StartOfString] :=
	MapAtOptionalToken[With[{o = $UOpt[#]}, (o ~~ " ") | ""]&, opt]
	(* [^] Expand optional token o into o followed by a space or just the empty string *)
iExpandOptional[opt:anchor[__], EndOfString] :=
	MapAtOptionalToken[With[{o = $UOpt[#]}, (" " ~~ o) | ""]&, opt]
	(* [^] Expand optional token o into o preceded by a space or just the empty string *)
iExpandOptional[opt:anchor[__], ContainsOnly] :=
	MapAtOptionalToken[With[{o = $UOpt[#]}, o | ""]&, opt]
	(* [^] Expand optional token o into o or just the empty string *)

MassOptionalToken[expr_] /; Not@*FreeQ[OptionalToken]@expr :=
	With[
		{list = {$LPS, Splice@StringExpressionToList@expr, $LPE}},
		Block[
			{res},
			res = SequenceReplace[
				list,
				{
					{$LPS, seq_anchor, after__, $LPE} :>
						StringExpression[iExpandOptional[seq, StartOfString], after],
					(* [^] Expand seq appearing at the start of the pattern *)
					{$LPS, before__, seq_anchor, $LPE} :>
						StringExpression[before, iExpandOptional[seq, EndOfString]],
					(* [^] Expand seq appearing at the end of the pattern *)
					{$LPS, before__, seq_anchor, after__, $LPE} :>
						StringExpression[before, iExpandOptional[seq, String], after],
					(* [^] Expand seq appearing in the middle of the pattern *)
					{$LPS, seq_anchor, $LPE} :>
						StringExpression[iExpandOptional[seq, ContainsOnly]]
					(* [^] Expand seq when it's the only pattern *)
				}
			];
			res //= ReplaceAll[anchor[s__]:> s];
			(* [^] shuck anchor heads *)
			res //= DeleteCases[$LPS|$LPE];
			(* [^] Remove the start and end delimiters *)
			res //= First;
			res
		]
	]

MassOptionalToken[expr_] :=
	expr // ReplaceAll[anchor[s__] :> s]
	(* [^] Default action for expr; shuck anchor heads *)

genSyn[word_] :=
	(word|Splice[Synonyms[word], Alternatives])
	(* [^] generate synonym pattern *)

$SynonymTokenRules = {
	SynonymToken[word_String] :>
		genSyn[word],
	(* [^] generate synonyms for a single word *)
	SynonymToken[words_Alternatives] :>
		Map[genSyn, words]
	(* [^] generate synonyms for a set of words *)
};

MassSynonymToken[expr_] :=
	expr /. $SynonymTokenRules
	(* [^] Expand expr's synonym tokens *)

MassTokens[content_Association] :=
	(MassSynonymToken@*MassWordToken@*MassTypeToken[content])
	(* [^] Operator function to mass tokens in a lexical pattern *)

StartContext[content_][a:anchor[seq__], after__] :=
	MassSynonymToken@MassWordToken@MassOptionalToken@StringExpression[MassTokens[content]@a, after]
	(* [^] Expand pattern-start's tokens *)
MiddleContext[content_][before__, a:anchor[seq__], after__] :=
	MassSynonymToken@MassWordToken@MassOptionalToken@StringExpression[before, MassTokens[content]@a, after]
	(* [^] Expand pattern-middle's tokens *)
EndContext[content_][before__, a:anchor[seq__]] :=
	MassSynonymToken@MassWordToken@MassOptionalToken@StringExpression[before, MassTokens[content]@a]
	(* [^] Expand pattern-end's tokens *)
SingletonContext[content_][a:anchor[seq__]] :=
	MassSynonymToken@MassWordToken@MassOptionalToken@StringExpression[MassTokens[content]@a]
	(* [^] Expand pattern's tokens *)

TokenPostProcess[expr_] :=
	With[
		{list = StringExpressionToList@expr},
		Block[
			{res},
			res = SequenceReplace[
				list, 
				rwb : {Repeated[WordBoundary, {2, Infinity}]} :> 
					Splice[Riffle[rwb, " "]]
			]; 
			(* [^] inject a space between WordBoundary's *)
			res //= ListToStringExpression;
			(* [^] Convert the list to a string expression *)
			res //= ReplaceAll[HoldPattern[Verbatim[Alternatives][s_String]] :> s];
			(* [^] shuck singleton alternatives *)
			res
		]
	]

$LPS = LexicalPatternDelimiter["Start"]
(* [^] Represents the start of a lexical pattern *)
$LPE = LexicalPatternDelimiter["End"]
(* [^] Represents the end of a lexical pattern *)
ReformTokens[expr_, content_] /; ContainsPatternHeadsQ[expr] :=
	(* [^] If expr contains named patterns, name: patt, then:  *)
	Block[
		{res},
		res = ReformTokens[Unpattern[expr], content];
		(* [^] Call ReformTokens on expr with named patterns Pattern made inert *)
		res //= Repattern;
		(* [^] Reinstate Pattern heads *)
		res //= TokenPostProcess;
		(* [^] Handle repeated WordBoundary's, List to StringExpressioning and Shucking singleton alts *)
		res
	]

ReformTokens[expr_, content_] :=
	Block[
		{res},
		res = FixedPoint[iReformToken[#, content]&, expr];
		(* [^] Repeatedly apply iReformToken to expr until its fixed-point *)
		res //= TokenPostProcess;
		(* [^] Clean up the rest of the tokens *)
		res
	]

iReformToken[expr_, content_] /; Not@*FreeQ[TypeToken|WordToken|OptionalToken|BoundToken|SynonymToken]@expr := 
	With[
		{list = {$LPS, Splice@StringExpressionToList@expr, $LPE}},
		Block[
			{res},
			res = SequenceReplace[
				list,
				{
					{$LPS, seq:Longest[Repeated[$UnpatternPattern[(TypeToken|WordToken|OptionalToken|BoundToken|SynonymToken)[__]]]], after__, $LPE} :>
						StartContext[content][anchor[seq], after],
						(* [^] Reform tokens appearing at the begining of the lexical pattern *)
					{$LPS, before__, seq:Longest[Repeated[$UnpatternPattern[(TypeToken|WordToken|OptionalToken|BoundToken|SynonymToken)[__]]]], $LPE} :>
						EndContext[content][before, anchor[seq]],
						(* [^] Reform tokens appearing at the end of the lexical pattern *)
					{$LPS, before__, seq:Longest[Repeated[$UnpatternPattern[(TypeToken|WordToken|OptionalToken|BoundToken|SynonymToken)[__]]]], after__, $LPE} :>
						MiddleContext[content][before, anchor[seq], after],
						(* [^] Reform tokens appearing in the middle of the lexical pattern *)
					{$LPS, seq:Longest[Repeated[$UnpatternPattern[(TypeToken|WordToken|OptionalToken|BoundToken|SynonymToken)[__]]]], $LPE} :>
						SingletonContext[content][anchor[seq]]
						(* [^] Reform tokens appearing on their own in the lexical pattern *)
				}
			];
			res //= First;
			res //= DeleteCases[$LPS|$LPE];
			res
		]
	]
iReformToken[expr_, _] :=
	expr
	(* [^] default, just return expr *)

iExpand[expr_] :=
	ReplaceAll[expr,
	{		
		BoundToken[outer_, inner_] :>
			BoundedToken[iExpand[outer],iExpand[inner]],
		BoundToken[s : Except[_Alternatives]] :>
			(BoundedWord[iExpand[s]]),
		BoundToken[a_Alternatives] :>
			BoundedWord[Map[iExpand, a]]
		}
	]

iExpandPattern[se_?LexicalPatternQ] :=
	iExpand[se]
	(* [*] this seems redundant (it's trying to be an entrypoint to iExpand, but maybe doesn't need to exist) *)

ExtractStringContentTypes[se_] :=
	Splice[Cases[se, TypeToken[type_String] :> type, {0, Infinity}]];

ExtractAlternativeContentTypes[se_] :=
	Splice[
		Cases[se, 
			TypeToken[type_Alternatives] :> Splice[List @@ type],
			{0, Infinity}
		]
	];

ExtractContainingContentTypes[se_] :=
	Splice[Cases[se, TypeToken[type_Containing] :> type, {0, Infinity}]];

ExtractContentTypes[se_] :=
	Comap[{ExtractStringContentTypes, ExtractContainingContentTypes, ExtractAlternativeContentTypes}][se]

ContentAssociation[st_String, (Rule | RuleDelayed)[se_, _]] :=
	ContentAssociation[st, se]

ContentAssociation[sourcetext_String, se_] :=
	Block[
		{typecontent, valuealts, mergeonkey},
		(* [1] -- Extract text-type instances *)
		typecontent = TextCases[sourcetext, ExtractContentTypes[se]];
		(* [^] Get all the instances of text types from the source text *)
		(* [2] -- Prune and sibling the content *)
		valuealts = KeyValueMap[<|#1 -> Apply[Alternatives] @* DeleteDuplicates @ #2|>&][typecontent];
		(* [^] Map text-type to their instance-alternatives: Adjective -> cool|scary|fun *)
		(* [3] -- Merge contents with the same key *)
		mergeonkey = Merge[Identity] @ valuealts;
		(* [4] Unwrap the alts *)
		Map[UnwrapAlternatives] @ mergeonkey
		]

ExpandPattern[sourcetext_String, Rule[se_?LexicalPatternQ, expr_]] :=
	Rule[ExpandPattern[sourcetext, se], expr]
	(* [^] Handling LP as rule-key *)

ExpandPattern[sourcetext_String, RuleDelayed[se_?LexicalPatternQ, expr_]] :=
	RuleDelayed[ExpandPattern[sourcetext, se], expr]
	(* [^] Handling LP as rule-delayed-key *)

(* Naive solution for list inputs to ExpandPattern *)
ExpandPattern[sources : List[__String], se_?LexicalPatternQ] :=
	Map[ExpandPattern[#, se]&][sources]
	(* [^] ExpandPatterns by all sources *)

ExpandPattern[sourcetext_String, se_?LexicalPatternQ] :=
	Module[
		{rft, trx, cas},
		(* [1] -- Extract instances of content types *)
		cas = ContentAssociation[sourcetext, se];

		(* [2] -- Replace tokens with patterns containing their instances *)
		rft = ReformTokens[se, cas];
		(* [^] Recursively expands `se` until reaching a fixed point *)

		(* [3] -- Expand BoundToken's *)
		trx = iExpandPattern[rft]
	]

(* Wikipedia *)

InsertAnd[l : List[_]] := l;

InsertAnd[x_List] := Insert[x, "and", -2];

WikipediaKeywordString[{s_String}] := "\"" <> s <> "\"";

WikipediaKeywordString[l : {_String, _String}] := StringRiffle[Map[WikipediaKeywordString][l], ", "];

WikipediaKeywordString[x_List] := StringRiffle[InsertAnd[Map[WikipediaKeywordString][x]], ", "];

WikipediaKeywordString[x_Alternatives] := ToString[Map[WikipediaKeywordString][x]]

WikipediaKeywordString[x_] := "\"" <> x <> "\"";

WikipediaSearchQuery[List[], lp_] := $Failed

WikipediaSearchQuery[wsq : List[__String], _] := StringRiffle[wsq]

WikipediaSearchQuery[wsq : List[List[__String]], _] := Flatten[wsq]

WikipediaSearchQuery[wsq_List, _] := Cases[List[(_List | _String)..]][wsq] // Map[StringRiffle]

twsqErrorInfo[expr_TypeToken] := StringForm["TypeToken's do not produce keywords. A lexical pattern with one or more word/phrase strings in it should work", expr]

twsqErrorInfo[expr_] := StringForm["`` did not produce any keywords. A lexical pattern with one or more word/phrase strings in it should work", expr]

ToWikipediaSearchQuery[lp_?LexicalPatternQ] :=
	Enclose[
		Confirm[
			iToWikipediaSearchQuery[lp],
			Message[ToWikipediaSearchQuery::novq, lp];
			twsqErrorInfo[lp]
		]
	]

iToWikipediaSearchQuery[Rule[lp_, _]] :=
	iToWikipediaSearchQuery[StripNamedPattern[lp]]

iToWikipediaSearchQuery[RuleDelayed[lp_, _]] :=
	iToWikipediaSearchQuery[StripNamedPattern[lp]]

iToWikipediaSearchQuery[lp : Except[_TypeToken | _WordToken]] :=
	Enclose[
		Module[{clp, wsq},
			clp = Cases[List @@ lp, (h : Except[TypeToken | WordToken])[args__] :> Cases[{args}, _String, {0, Infinity}]];
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
						"Gathering text from " <> articleCountString <> " " <> articlePluralize[articleCount] <> ":\n"
						,
						Dynamic[
							Which[
								(ArticleIndex <= articleCount - 1), StringPadRight["\"" <> articles[[ArticleIndex + 1]] <> "\"", maxTitleLength],
								(ArticleIndex === 1), "\"" <> articles[[1]] <> "\" ",
								True,""
							]
						],
						"\n", ProgressIndicator[Dynamic[ArticleIndex], {0, articleCount}], " ", Dynamic[NumberForm[PercentForm[N[ArticleIndex / articleCount]], {3, 2}]]
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

(* If a supported `file` is given, import it and run LexicalCases on it with `args` *)
LexicalCases[file_File, args___] :=
	Enclose[
		ConfirmAssert[SupportedFileQ[file], Message[LexicalCases::unsupfmt, GetFileExtension[file]]];
		Block[{data = Import[file]}, LexicalCases[data, args]]
	]

(* If a list of `files` or `strings` is given *)
LexicalCases[input : (List[__File] | List[__String]), se_?LexicalPatternQ, opts : OptionsPattern[LexicalCases]] :=
	Enclose[
		ConfirmAssert[CheckArguments[LexicalCases[input, se, opts], 2]];
		Block[
			{lpc, ost},
			lpc = iLexicalCases[input, se, opts];
			ost = (ToString@*Head@*First@input) /. "String" -> "Text";
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
			Module[
				{
					files,
					f1 = DeleteMissing[Dataset[TextSearch[index, query][All, {"Text","Description"}]] // Normal, 2] // Flatten,
					f2 = Map[File]@DeleteMissing[TextSearch[index, query][All, "Location"]],
					lpc
				},
				If[Length@f1!=0,files = f1,files=f2];
				ConfirmAssert[Not @* MatchQ[{}] @ files];
				lpc = iLexicalCases[files, se, opts];
				GenerateLexicalSummary[lpc["Results"], "SearchIndex", Compress[lpc["Text"]], se]
			],
			Failure["NoFilesFound",
				<|
					"MessageTemplate" -> StringTemplate["No files found using `Query`."],
					"MessageParameters" -> <|"Query" -> exprMsg[query]|>, "Tag" -> #["Tag"],
					"ConfirmationType" -> #["ConfirmationType"], "HeldTest" -> #["HeldTest"],
					"Information" -> "Try another string or specify a query with SearchQueryString."
				|>
			]&
		]
	]

(* SourceText and LexicalPattern Input *)

LexicalCases[sourcetext_String, se_?LexicalPatternQ, opts: OptionsPattern[LexicalCases]] :=
	Enclose[
		ConfirmAssert[CheckArguments[LexicalCases[sourcetext, se, opts], 2]];
		Block[
			{lpc, src = Compress[sourcetext], res},
			ArticleIndex = 0;
			(* [1] -- Find Matches *)
			lpc = Confirm[LexicalCasesOnString[sourcetext, se, opts]];
			(* [2] -- Generate Summary Object *)
			res = GenerateLexicalSummary[lpc, "Text", src, se];
			(* [3] -- Clear Article Index *)
			ArticleIndex =.;
			res
		]
	]

(* SourceText is a string *)
Options[StringScan] = Options[LexicalCases];
StringScan[s_, rx_, opts:OptionsPattern[StringScan]] :=
	Enclose[
		With[
			{options = {IgnoreCase -> OptionValue[IgnoreCase], Overlaps -> OptionValue[Overlaps]}},
			ComapApply[
				{
					Function[
						ConfirmQuiet[
							StringCases[#1, #2, Sequence@@options],
							{StringExpression::cond},
							"Message issued while calling StringCases"
						]
					],
					Function[StringPosition[#1, StripNamedPattern[#2], Sequence@@options]]
				}
			][
				{s, rx}
			]
		]
	]


LexicalCasesOnString[source_String, se_?LexicalPatternQ, opts : OptionsPattern[LexicalCases]] :=
	Enclose[
		ConfirmAssert[CheckArguments[LexicalCases[source, se, opts], 2]];
		Block[
			{rx, s, res},
			s = source;
			++ArticleIndex;
			(* [1] -- Expand the lexical pattern *)
			rx = ConfirmQuiet[ExpandPattern[s, se], {Java::excptn, JavaNew::fail}];
			(* [2] -- Scan the string for [1] *)
			res = Confirm[StringScan[s, rx, opts]];
			(* [3] -- Transpose the results from {{matches,...}, {positions,...}} to {{match, position}, ...} *)
			res //= Transpose;
			(* [4] -- Key the pairs *)
			res //= Map[Function[AssociationThread[{"Match", "Position"} -> #]]];
			(* [5] -- Group positions with the same match *)
			res //= Query[
				GroupBy[#Match&] /* (KeyValueMap[<|"Match" -> #1, "Position" -> #2|>&]),
				KeyDrop["Match"] /* Values /* (Flatten[#, 1]&)
			];
			(* [6] -- Trim the strings *)
			res //= MatchTrim[OptionValue["StringTrim"]];
			res
		]
	]

(* LexicalPattern on Service *)

LexicalCases[se_?LexicalPatternQ, opts : OptionsPattern[{LexicalCases, iSearchWikipedia}]] :=
	Enclose[
		(* [1] -- Verify single argument with valid options *)
		ConfirmAssert[CheckArguments[LexicalCases[se, opts], 1]];
		(* [2] -- Pass pattern to selected service (default is Wikipedia) *)
		LexicalCasesFromService[OptionValue["Service"], se, FilterRules[{opts}, Options[iSearchWikipedia]]]
	]

(* WikiQueryRule and LexicalPattern Input *)

LexicalCases[query_Rule, se_?LexicalPatternQ, opts : OptionsPattern[{LexicalCases, iSearchWikipedia}]] :=
	Enclose[
		(* [1] -- Verify search query and lexical pattern arguments with valid options *)
		ConfirmAssert[CheckArguments[LexicalCases[query, se, opts], 2]];
		(* [2] -- Pass service search query and lexical pattern to selected service *)
		LexicalCasesFromService[OptionValue["Service"], query, se, FilterRules[{opts}, Options[iSearchWikipedia]]]
	]

Options[LexicalCasesFromService] = {};

StringSearch[text_, pattern_] :=
	Block[
		{res},
		(* [1] -- search for pattern in the given text *)
		res = LexicalCasesOnString[text, pattern];
		(* [2] -- Increment MatchCount as matches are found *)
		CriticalSection[lclock, MatchCount += Length[Replace[_Missing -> {}] @ Flatten[Lookup[res, "Position"], 1]]];
		(* [^] CriticalSection is used to make sure the parallelization doesn't mess up the summing of the MatchCount (fwiw, not clear to me atm) *)
		(* [3] -- Return the result *)
		res
	]

SearchArticles[texts_List, se_?LexicalPatternQ, articles_List, articleCount_Integer, maxTitleLength_Integer] :=
	Block[
		{mat},
		(* [1] --  Initialize counts and prepare for parallelization *)
		ArticleIndex = 0;
		MatchCount = 0;
		SetSharedVariable[se, MatchCount];
		SetSharedFunction[StringSearch];
		(* [2] -- Apply search for each article *)
		mat = Monitor[
			ParallelMap[Function[StringSearch[#,se]], texts],
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
		(* [3] -- Reset index and count *)
		ArticleIndex =.;
		MatchCount =.;
		(* [4] -- Return the matches *)
		mat
	]

PackageResults[matches_, articles_List, articleCount_Integer, maxTitleLength_Integer] :=
	Block[{amt, mac},
		(* [1] -- Thread article titles with their matches *)
		amt = Thread[{articles, ReplaceEmptyListWithMissing[matches]}];
		(* [2] -- Make `amt` available for parallelization  *)
		SetSharedVariable[amt, ArticleIndex];
		(* [3] -- Initialize index *)
		ArticleIndex = 0;
		(* [4] -- Associate article key with article matches *)
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
		(* [5] -- Reset index *)
		ArticleIndex =.;
		(* [6] -- Flatten across articles *)
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
	Block[
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
	Block[
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
	Block[
		{ard, src, art, arc, acs, mtl, mtc},
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
	Block[
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
			"SmoothLexicalHistogram",
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
						"Wikipedia", LexicalDispersionSmoothHistogram[ucmp, res, keys, DataJoin -> True],
						_, LexicalDispersionSmoothHistogram[ucmp, res, keys]
						]
				]
			),
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
	Apply[Sequence][
		Map[<|"Matches" -> ToLowerCase[#], "Count" -> asc["CountGroup"]|>& ][ asc["Matches"] ]
		]

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
