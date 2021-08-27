(* ::Package:: *)

(* ::Title:: *)
(*TextSequenceCases*)


(* ::Abstract:: *)
(*Extract and analyze text type sequences with the Wolfram Language.*)


BeginPackage["TextSequenceCases`"]
(* Main *)
TextSequenceCases::usage="TextSequenceCases[\!\(\*
StyleBox[\"source\", \"TI\"]\), \!\(\*
StyleBox[\"textpatt\", \"TI\"]\)] gives the text sequences in \!\(\*
StyleBox[\"source\", \"TI\"]\) that match the text pattern \!\(\*
StyleBox[\"textpatt\", \"TI\"]\)."

(* TextPatterns *)
TextPattern::uage="Represents a fixed order text pattern"
OrdelessTextPattern::uage="Represents an any order text pattern"
OptionalTextPattern::uage="Represents an optional text pattern"

Begin["Private`"]

End[]
EndPackage[]
