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
(* StringStartsQ *)
(* StringFreeQ *)
(* StringSplit *)
(* StringTrim *)
(* StringReplace *)
(* StringReplaceList *)
(* StringReplacePart *)

End[]
EndPackage[]
