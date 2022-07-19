(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*LexicalPattern UpValues*)


BeginPackage["FaizonZaman`LexicalCases`LexicalPattern`"]

Begin["`Private`"]

(* StringCases *)

FaizonZaman`LexicalCases`LexicalPattern /: StringCases[source_, FaizonZaman`LexicalCases`LexicalPattern[se_]] := Module[
	{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
	StringCases[source, P]
	]

FaizonZaman`LexicalCases`LexicalPattern /: StringCases[lp:FaizonZaman`LexicalCases`LexicalPattern[_]] := Function[StringCases[#, lp]]

(* StringMatchQ *)
FaizonZaman`LexicalCases`LexicalPattern /: StringMatchQ[source_, FaizonZaman`LexicalCases`LexicalPattern[se_]] := Module[
	{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
	StringMatchQ[source, P]
	]

FaizonZaman`LexicalCases`LexicalPattern /: StringMatchQ[lp:FaizonZaman`LexicalCases`LexicalPattern[_]] := Function[StringMatchQ[#, lp]]

(* StringPosition *)
FaizonZaman`LexicalCases`LexicalPattern /: StringPosition[source_, FaizonZaman`LexicalCases`LexicalPattern[se_]] := Module[
	{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
	StringPosition[source, P]
	]

FaizonZaman`LexicalCases`LexicalPattern /: StringPosition[lp:FaizonZaman`LexicalCases`LexicalPattern[_]] := Function[StringPosition[#, lp]]

(* StringCount *)
FaizonZaman`LexicalCases`LexicalPattern /: StringCount[source_, FaizonZaman`LexicalCases`LexicalPattern[se_]] := Module[
	{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
	StringCount[source, P]
	]

FaizonZaman`LexicalCases`LexicalPattern /: StringCount[lp:FaizonZaman`LexicalCases`LexicalPattern[_]] := Function[StringCount[#, lp]]
(* StringDelete *)
FaizonZaman`LexicalCases`LexicalPattern /: StringDelete[source_, FaizonZaman`LexicalCases`LexicalPattern[se_]] := Module[
{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
StringDelete[source, P]
]

FaizonZaman`LexicalCases`LexicalPattern /: StringDelete[lp:FaizonZaman`LexicalCases`LexicalPattern[_]] := Function[StringDelete[#, lp]]
(* StringStartsQ *)
FaizonZaman`LexicalCases`LexicalPattern /: StringStartsQ[source_, FaizonZaman`LexicalCases`LexicalPattern[se_]] := Module[
{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
StringStartsQ[source, P]
]

FaizonZaman`LexicalCases`LexicalPattern /: StringStartsQ[lp:FaizonZaman`LexicalCases`LexicalPattern[_]] := Function[StringStartsQ[#, lp]]
(* StringEndsQ *)
FaizonZaman`LexicalCases`LexicalPattern /: StringEndsQ[source_, FaizonZaman`LexicalCases`LexicalPattern[se_]] := Module[
{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
StringEndsQ[source, P]
]

FaizonZaman`LexicalCases`LexicalPattern /: StringEndsQ[lp:FaizonZaman`LexicalCases`LexicalPattern[_]] := Function[StringEndsQ[#, lp]]
(* StringFreeQ *)
FaizonZaman`LexicalCases`LexicalPattern /: StringFreeQ[source_, FaizonZaman`LexicalCases`LexicalPattern[se_]] := Module[
{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
StringFreeQ[source, P]
]

FaizonZaman`LexicalCases`LexicalPattern /: StringFreeQ[lp:FaizonZaman`LexicalCases`LexicalPattern[_]] := Function[StringFreeQ[#, lp]]
(* StringSplit *)
FaizonZaman`LexicalCases`LexicalPattern /: StringSplit[source_, FaizonZaman`LexicalCases`LexicalPattern[se_]] := Module[
{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
StringSplit[source, P]
]

FaizonZaman`LexicalCases`LexicalPattern /: StringSplit[lp:FaizonZaman`LexicalCases`LexicalPattern[_]] := Function[StringSplit[#, lp]]
(* StringTrim *)
FaizonZaman`LexicalCases`LexicalPattern /: StringTrim[source_, FaizonZaman`LexicalCases`LexicalPattern[se_]] := Module[
{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
StringTrim[source, P]
]

FaizonZaman`LexicalCases`LexicalPattern /: StringTrim[lp:FaizonZaman`LexicalCases`LexicalPattern[_]] := Function[StringTrim[#, lp]]
(* StringReplace *)
FaizonZaman`LexicalCases`LexicalPattern /: StringReplace[source_, FaizonZaman`LexicalCases`LexicalPattern[se:(_Rule|_RuleDelayed)]] := Module[
{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
StringReplace[source, P]
]

FaizonZaman`LexicalCases`LexicalPattern /: StringReplace[lp:FaizonZaman`LexicalCases`LexicalPattern[(_Rule|_RuleDelayed)]] := Function[StringReplace[#, lp]]
(* StringReplaceList *)
FaizonZaman`LexicalCases`LexicalPattern /: StringReplaceList[source_, FaizonZaman`LexicalCases`LexicalPattern[se:(_Rule|_RuleDelayed)]] := Module[
{P = FaizonZaman`LexicalCases`ExpandPattern[source, se]},
StringReplaceList[source, P]
]

FaizonZaman`LexicalCases`LexicalPattern /: StringReplaceList[lp:FaizonZaman`LexicalCases`LexicalPattern[(_Rule|_RuleDelayed)]] := Function[StringReplaceList[#, lp]]

End[]
EndPackage[]
