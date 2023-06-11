(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*LexicalPattern UpValues*)


BeginPackage["LexicalCases`LexicalPattern`"]

Begin["`Private`"]

(* StringCases *)

LexicalCases`LexicalPattern /: StringCases[source_, LexicalCases`LexicalPattern[se_]] := Module[
	{P = LexicalCases`ExpandPattern[source, se]},
	StringCases[source, P]
	]

LexicalCases`LexicalPattern /: StringCases[lp:LexicalCases`LexicalPattern[_]] := Function[StringCases[#, lp]]

(* StringMatchQ *)
LexicalCases`LexicalPattern /: StringMatchQ[source_, LexicalCases`LexicalPattern[se_]] := Module[
	{P = LexicalCases`ExpandPattern[source, se]},
	StringMatchQ[source, P]
	]

LexicalCases`LexicalPattern /: StringMatchQ[lp:LexicalCases`LexicalPattern[_]] := Function[StringMatchQ[#, lp]]

(* StringPosition *)
LexicalCases`LexicalPattern /: StringPosition[source_, LexicalCases`LexicalPattern[se_]] := Module[
	{P = LexicalCases`ExpandPattern[source, se]},
	StringPosition[source, P]
	]

LexicalCases`LexicalPattern /: StringPosition[lp:LexicalCases`LexicalPattern[_]] := Function[StringPosition[#, lp]]

(* StringCount *)
LexicalCases`LexicalPattern /: StringCount[source_, LexicalCases`LexicalPattern[se_]] := Module[
	{P = LexicalCases`ExpandPattern[source, se]},
	StringCount[source, P]
	]

LexicalCases`LexicalPattern /: StringCount[lp:LexicalCases`LexicalPattern[_]] := Function[StringCount[#, lp]]
(* StringDelete *)
LexicalCases`LexicalPattern /: StringDelete[source_, LexicalCases`LexicalPattern[se_]] := Module[
{P = LexicalCases`ExpandPattern[source, se]},
StringDelete[source, P]
]

LexicalCases`LexicalPattern /: StringDelete[lp:LexicalCases`LexicalPattern[_]] := Function[StringDelete[#, lp]]
(* StringStartsQ *)
LexicalCases`LexicalPattern /: StringStartsQ[source_, LexicalCases`LexicalPattern[se_]] := Module[
{P = LexicalCases`ExpandPattern[source, se]},
StringStartsQ[source, P]
]

LexicalCases`LexicalPattern /: StringStartsQ[lp:LexicalCases`LexicalPattern[_]] := Function[StringStartsQ[#, lp]]
(* StringEndsQ *)
LexicalCases`LexicalPattern /: StringEndsQ[source_, LexicalCases`LexicalPattern[se_]] := Module[
{P = LexicalCases`ExpandPattern[source, se]},
StringEndsQ[source, P]
]

LexicalCases`LexicalPattern /: StringEndsQ[lp:LexicalCases`LexicalPattern[_]] := Function[StringEndsQ[#, lp]]
(* StringFreeQ *)
LexicalCases`LexicalPattern /: StringFreeQ[source_, LexicalCases`LexicalPattern[se_]] := Module[
{P = LexicalCases`ExpandPattern[source, se]},
StringFreeQ[source, P]
]

LexicalCases`LexicalPattern /: StringFreeQ[lp:LexicalCases`LexicalPattern[_]] := Function[StringFreeQ[#, lp]]
(* StringSplit *)
LexicalCases`LexicalPattern /: StringSplit[source_, LexicalCases`LexicalPattern[se_]] := Module[
{P = LexicalCases`ExpandPattern[source, se]},
StringSplit[source, P]
]

LexicalCases`LexicalPattern /: StringSplit[lp:LexicalCases`LexicalPattern[_]] := Function[StringSplit[#, lp]]
(* StringTrim *)
LexicalCases`LexicalPattern /: StringTrim[source_, LexicalCases`LexicalPattern[se_]] := Module[
{P = LexicalCases`ExpandPattern[source, se]},
StringTrim[source, P]
]

LexicalCases`LexicalPattern /: StringTrim[lp:LexicalCases`LexicalPattern[_]] := Function[StringTrim[#, lp]]
(* StringReplace *)
LexicalCases`LexicalPattern /: StringReplace[source_, LexicalCases`LexicalPattern[se:(_Rule|_RuleDelayed)]] := Module[
{P = LexicalCases`ExpandPattern[source, se]},
StringReplace[source, P]
]

LexicalCases`LexicalPattern /: StringReplace[lp:LexicalCases`LexicalPattern[(_Rule|_RuleDelayed)]] := Function[StringReplace[#, lp]]
(* StringReplaceList *)
LexicalCases`LexicalPattern /: StringReplaceList[source_, LexicalCases`LexicalPattern[se:(_Rule|_RuleDelayed)]] := Module[
{P = LexicalCases`ExpandPattern[source, se]},
StringReplaceList[source, P]
]

LexicalCases`LexicalPattern /: StringReplaceList[lp:LexicalCases`LexicalPattern[(_Rule|_RuleDelayed)]] := Function[StringReplaceList[#, lp]]

End[]
EndPackage[]
