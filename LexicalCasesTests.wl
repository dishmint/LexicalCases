(* ::Package:: *)

(* ::Title:: *)
(*LexicalPatternCases*)


(* ::Abstract:: *)
(*Extract and analyze text type sequences with the Wolfram Language.*)

BeginPackage["LexicalCasesTests`"]

Needs["LexicalCases`"]
(* Main *)
LexicalCasesTestReport::usage="LexicalPatternTestReport returns a TestReport for LexicalPattern components."
$LexicalCasesTests::usage="A list of VerificationTests for LexicalPattern components"
$SampleStringShort::usage="A short example string"
$SampleStringLong::usage="A long example string"
$SampleLexicalPattern::usage="A sample text pattern used for testing"
Begin["Private`"]
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
$SampleLexicalPattern = LexicalPattern[TextType["Adjective"], " key lime pie"];
$LexicalCasesTests := {
	(* ContentAssociation *)
	VerificationTest[
		ContentAssociation[$SampleStringShort, $SampleLexicalPattern],
		Association["Adjective" -> {"best" | "key" | "still" | "up"}],
		"TestID" -> "ContentAssociationTest1"
	],
	(* ConvertToWikipediaSearchQuery *)
	VerificationTest[
		ConvertToWikipediaSearchQuery[$SampleLexicalPattern],
		"key lime pie",
		"TestID" -> "ConvertToWikipediaSearchQueryTest1"
	],
	VerificationTest[
		ConvertToWikipediaSearchQuery[LexicalPattern[TextType["Determiner"], " ", "king" | "queen"]],
		{"king", "queen"},
		"TestID" -> "ConvertToWikipediaSearchQueryTest2"
	],
	(* LexicalPatternToStringExpression *)
	VerificationTest[
		LexicalPatternToStringExpression[$SampleStringLong, LexicalPattern["computer" | "computers", " ", TextType["Verb"]]],
			StringExpression[
				Alternatives["computer","computers"]," ",
				Except[WordCharacter,Alternatives[WordBoundary," "]],
				Alternatives[
				"allow","automate","be","can","carry","change","chip","circuit","cluster","complete","control","design","enable","form","full","function","group","have","include","input","like",
				"long","machine","microwave","monitor","network","order","out","output","pace","people","perform","power","range","refer","result","source","speed","term","type","use","well","is",
				"programmed","known","includes","operating","needed","used","may","are","linked","included","links","were","meant","aided","doing","built","guiding","did","specialized",
				"calculating","developed","followed","integrated","leading","been","increasing","counts","predicted","consists","carries","stored","retrieved","saved"
				],
				Except[WordCharacter,Alternatives[WordBoundary," "]]
				],
		"TestID" -> "LexicalPatternToStringExpressionTest1"
	],
	(* ToTextElementStructure *)
	VerificationTest[
		ToTextElementStructure[LexicalPattern["computer" | "computers", " ", TextType["Verb"]]],
		TextElement[
			{
				TextElement[{Alternatives["computer", "computers"]}, Association["GrammaticalUnit" -> "Alternatives"]],
				" ",
				TextElement["Verb",Association["GrammaticalUnit" -> "TextType"]]
			},
		Association["GrammaticalUnit" -> "LexicalPattern"]
		]
	,
	"TestID" -> "ToTextElementStructureTest1"
	],
	(* Short Strings *)
	VerificationTest[
		LexicalCases[$SampleStringShort, $SampleLexicalPattern, "StringTrim" -> True]["Data"],
		{<|"Match" -> "best key lime pie", "Position" -> {{5, 21}}|>},
		"TestID" -> "ShortStringTest1"
		]
	(* Long Strings *)
};

LexicalCasesTestReport := TestReport[$LexicalCasesTests]
End[]
EndPackage[]
