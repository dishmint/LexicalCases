(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Utility functions*)


BeginPackage["LexicalCases`Utilities`"]

OptionsJoin::usage = "OptionsJoin[sym_1, ..., sym_i] joins the Options of the sym_i"
ExtractHeads::usage = "ExtractHeads[expr] extracts all heads found in expr"
ExtractAlternatives::usage = "ExtractAlternatives[expr] if expr is a list of alternatives return the alternatives"

Begin["`Private`"]

OptionsJoin[sym__Symbol]:=(Map[Options]/*Apply[Join])[{sym}]

(* expr operations *)

ExtractHeads[expr_] := Cases[expr, h_[___] :> h, {0, Infinity}]

ExtractAlternatives[List[a_Alternatives]] := a
ExtractAlternatives[a_] := a

End[]
EndPackage[]
