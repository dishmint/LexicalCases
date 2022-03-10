(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Utility functions*)


BeginPackage["LexicalCases`Utilities`"]

OptionsJoin::usage = "OptionsJoin[sym_1, ..., sym_i] joins the Options of the sym_i"
ExtractHeads::usage = "ExtractHeads[expr] extracts all heads found in expr"
ExtractAlternatives::usage = "ExtractAlternatives[expr] if expr is a list of alternatives return the alternatives"
PostProcessAlternatives::usage = "PostProcessAlternatives[expr] if expr is an alternatives retun expr in a list"

Begin["`Private`"]

OptionsJoin[sym__Symbol]:=(Map[Options]/*Apply[Join])[{sym}]

(* expr operations *)

ExtractHeads[expr_] := Cases[expr, h_[___] :> h, {0, Infinity}]

ExtractAlternatives[{expr_Alternatives}] := expr
ExtractAlternatives[else_] := else

PostProcessAlternatives[expr_Alternatives] := {expr}
PostProcessAlternatives[else_] := else

End[]
EndPackage[]
